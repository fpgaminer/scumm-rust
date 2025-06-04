use std::{
	cell::RefCell,
	collections::{HashMap, HashSet, VecDeque},
	pin::Pin,
	rc::Rc,
};

use crate::ast::*;
use futures::{FutureExt, future::LocalBoxFuture};
use indexmap::IndexMap;
use log::{debug, error, info, warn};
use wasm_bindgen::prelude::*;
#[cfg(target_arch = "wasm32")]
use web_sys::{Document, Element, HtmlImageElement};


pub enum Declaration {
	Script(Block),
	Object(ObjectDef),
	Class(ObjectDef),
	Room(RoomDef),
}


// ---------------------------------------------------------------------------
// Runtime object definition (built from the AST once at start-up)
// ---------------------------------------------------------------------------
#[derive(Clone)]
pub struct ObjectDef {
	pub name: String,                  // human readable name
	pub classes: Vec<String>,          // List of classes this object inherits
	pub verbs: HashMap<String, Block>, // vLook, vOpen, …
	pub room: u32,                     // 0 = nowhere / inventory only
	pub state: u32,
	pub states: Vec<String>, // List of image paths for each state (starting at state 1)
	pub x: i32,              // x position in pixels
	pub y: i32,              // y position in pixels
	pub width: u32,          // width in pixels
	pub height: u32,         // height in pixels
}

impl Default for ObjectDef {
	fn default() -> Self {
		ObjectDef {
			name: String::new(),
			classes: Vec::new(),
			verbs: HashMap::new(),
			room: 0,
			state: 1,
			states: Vec::new(),
			x: 0,
			y: 0,
			width: 0,
			height: 0,
		}
	}
}


#[derive(Default)]
pub struct RoomDef {
	pub name: String,
	pub entry_script: Option<Block>, // Script to run when entering this room
	pub image: Option<String>,       // Background image for the room
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct ObjectSnapshot {
	state: u32,
	x: i32,
	y: i32,
	width: u32,
	height: u32,
}

impl From<&ObjectDef> for ObjectSnapshot {
	fn from(obj: &ObjectDef) -> Self {
		ObjectSnapshot {
			state: obj.state,
			x: obj.x,
			y: obj.y,
			width: obj.width,
			height: obj.height,
		}
	}
}


/// SCUMM VM task context
#[derive(Clone)]
pub struct Ctx {
	pub vars: Rc<RefCell<HashMap<String, Value>>>,
	pub delay: Rc<RefCell<u32>>, // delay for blocking calls like wait()
	pub interpreter: Interpreter,
}


// ---------------------------------------------------------------------------
// Web interface for managing DOM
// ---------------------------------------------------------------------------
#[cfg(not(target_arch = "wasm32"))]
#[derive(Clone, Default)]
struct WebInterface {
	room_background: Rc<RefCell<Option<String>>>,
	objects: Rc<RefCell<HashMap<u32, ObjectDef>>>,
	inventory: Rc<RefCell<HashSet<u32>>>,
	#[allow(dead_code)]
	dragging_object: Rc<RefCell<Option<u32>>>,
}

#[cfg(not(target_arch = "wasm32"))]
impl WebInterface {
	fn new() -> Result<Self, JsValue> {
		Ok(WebInterface {
			room_background: Rc::new(RefCell::new(None)),
			objects: Rc::new(RefCell::new(HashMap::new())),
			inventory: Rc::new(RefCell::new(HashSet::new())),
			dragging_object: Rc::new(RefCell::new(None)),
		})
	}

	#[allow(dead_code)]
	fn display_message(&self, _message: &str) -> Result<(), JsValue> {
		Ok(())
	}

	#[allow(dead_code)]
	fn show_object_name(&self, _name: &str, _x: i32, _y: i32) -> Result<(), JsValue> {
		Ok(())
	}

	#[allow(dead_code)]
	fn hide_object_name(&self) -> Result<(), JsValue> {
		Ok(())
	}

	#[allow(dead_code)]
	fn show_use_hint(&self, _text: &str, _x: i32, _y: i32) -> Result<(), JsValue> {
		Ok(())
	}

	#[allow(dead_code)]
	fn hide_use_hint(&self) -> Result<(), JsValue> {
		Ok(())
	}

	fn set_room_background(&self, image: Option<&str>) -> Result<(), JsValue> {
		*self.room_background.borrow_mut() = image.map(|s| s.to_string());
		Ok(())
	}

	fn clear_room(&self) {
		self.objects.borrow_mut().clear();
	}

	fn remove_object(&self, id: u32) {
		self.objects.borrow_mut().remove(&id);
	}

	fn render_object(&self, _interp: Interpreter, id: u32, obj: &ObjectDef) -> Result<(), JsValue> {
		self.objects.borrow_mut().insert(id, obj.clone());
		Ok(())
	}

	fn set_inventory(&self, items: &HashSet<u32>) {
		*self.inventory.borrow_mut() = items.clone();
	}

	#[allow(dead_code)]
	fn get_inventory(&self) -> HashSet<u32> {
		self.inventory.borrow().clone()
	}

	fn update_inventory(&self, _interp: Interpreter, items: &HashSet<u32>) -> Result<(), JsValue> {
		self.set_inventory(items);
		Ok(())
	}

	#[allow(dead_code)]
	fn get_background(&self) -> Option<String> {
		self.room_background.borrow().clone()
	}

	#[allow(dead_code)]
	fn get_objects(&self) -> HashMap<u32, ObjectDef> {
		self.objects.borrow().clone()
	}
}

#[cfg(target_arch = "wasm32")]
#[derive(Clone)]
struct WebInterface {
	document: Document,
	game_container: Element,
	room_container: Element,
	inventory_container: Element,
	container_width: f64,
	container_height: f64,
	current_room_image: Rc<RefCell<Option<String>>>,
	scale_x: Rc<RefCell<f64>>,
	scale_y: Rc<RefCell<f64>>,
	dragging_object: Rc<RefCell<Option<u32>>>,
}

#[cfg(target_arch = "wasm32")]
impl WebInterface {
	fn new() -> Result<Self, JsValue> {
		let window = web_sys::window().ok_or("No global `window` exists")?;
		let document = window.document().ok_or("Should have a document on window")?;

		// Define container dimensions
		let container_width = 720.0;
		let container_height = 480.0;

		// Create main game container
		let game_container = document.create_element("div")?;
		game_container.set_attribute("id", "game-container")?;
		game_container.set_attribute(
			"style",
			&format!(
				"position: relative; width: {}px; height: {}px; background: #000; margin: 0 auto; border: 2px solid #333;",
				container_width, container_height
			),
		)?;

		// Create room container for objects
		let room_container = document.create_element("div")?;
		room_container.set_attribute("id", "room-container")?;
		room_container.set_attribute(
			"style",
			"position: relative; width: 100%; height: 100%; transform-origin: top left; transform: scale(1, 1);",
		)?;

		game_container.append_child(&room_container)?;

		let inventory_container = document.create_element("div")?;
		inventory_container.set_attribute("id", "inventory-container")?;
		inventory_container.set_attribute(
			"style",
			&format!(
				"width: {}px; min-height: 40px; margin: 4px auto; display: flex; gap: 4px; background: #222; padding: 4px; border: 1px solid #666; box-sizing: border-box;",
				container_width
			),
		)?;

		// Append to body or app div
		if let Some(app) = document.get_element_by_id("app") {
			app.append_child(&game_container)?;
			app.append_child(&inventory_container)?;
		} else {
			document.body().unwrap().append_child(&game_container)?;
			document.body().unwrap().append_child(&inventory_container)?;
		}

		let web_interface = WebInterface {
			document,
			game_container,
			room_container,
			inventory_container,
			container_width,
			container_height,
			current_room_image: Rc::new(RefCell::new(None)),
			scale_x: Rc::new(RefCell::new(1.0)),
			scale_y: Rc::new(RefCell::new(1.0)),
			dragging_object: Rc::new(RefCell::new(None)),
		};

		// Ensure the message box exists so it's visible immediately
		web_interface.display_message("")?;

		Ok(web_interface)
	}

	fn set_room_background(&self, image: Option<&str>) -> Result<(), JsValue> {
		let style = if let Some(img) = image {
			// Store the current room image path
			*self.current_room_image.borrow_mut() = Some(img.to_string());

			// Load the image to get its natural dimensions
			let image_element = HtmlImageElement::new()?;
			let scale_x = self.scale_x.clone();
			let scale_y = self.scale_y.clone();
			let room_container = self.room_container.clone();
			let container_width = self.container_width;
			let container_height = self.container_height;
			let image_element_clone = image_element.clone();

			// Create a closure to handle image load
			let onload = wasm_bindgen::closure::Closure::wrap(Box::new(move || {
				let width = image_element_clone.natural_width() as f64;
				let height = image_element_clone.natural_height() as f64;
				let container_ratio = container_width / container_height;
				let room_ratio = width / height;
				let (sx, sy) = if container_ratio > room_ratio {
					let s = container_height / height;
					(s, s)
				} else {
					let s = container_width / width;
					(s, s)
				};
				*scale_x.borrow_mut() = sx;
				*scale_y.borrow_mut() = sy;
				let _ = room_container.set_attribute(
					"style",
					&format!(
						"position: relative; width: 100%; height: 100%; transform-origin: top left; transform: scale({}, {});",
						sx, sy
					),
				);
				web_sys::console::log_1(&format!("Room image loaded: {}x{} scale {} {}", width, height, sx, sy).into());
			}) as Box<dyn Fn()>);

			image_element.set_onload(Some(onload.as_ref().unchecked_ref()));
			onload.forget(); // Keep the closure alive
			image_element.set_src(img);

			format!(
				"position: relative; width: {}px; height: {}px; background: #000 url('{}') no-repeat center/cover; margin: 0 auto; border: 2px solid #333;",
				self.container_width, self.container_height, img
			)
		} else {
			*self.current_room_image.borrow_mut() = None;
			*self.scale_x.borrow_mut() = 1.0;
			*self.scale_y.borrow_mut() = 1.0;
			self.room_container.set_attribute(
				"style",
				"position: relative; width: 100%; height: 100%; transform-origin: top left; transform: scale(1, 1);",
			)?;
			format!(
				"position: relative; width: {}px; height: {}px; background: #000; margin: 0 auto; border: 2px solid #333;",
				self.container_width, self.container_height
			)
		};
		self.game_container.set_attribute("style", &style)
	}

	fn clear_room(&self) {
		while let Some(child) = self.room_container.first_child() {
			let _ = self.room_container.remove_child(&child);
		}
	}

	fn remove_object(&self, id: u32) {
		if let Some(el) = self.document.get_element_by_id(&format!("object-{}", id)) {
			let _ = el.remove();
		}
	}

	fn render_object(&self, interp: Interpreter, id: u32, obj: &ObjectDef) -> Result<(), JsValue> {
		let element = match self.document.get_element_by_id(&format!("object-{}", id)) {
			Some(el) => el,
			None => {
				let div = self.document.create_element("div")?;
				div.set_attribute("id", &format!("object-{}", id))?;
				div.set_attribute("class", "game-object")?;
				let name = obj.name.clone();
				let self_clone = self.clone();
				let mouse_enter = wasm_bindgen::closure::Closure::wrap(Box::new(move |e: web_sys::MouseEvent| {
					let _ = self_clone.show_object_name(&name, e.client_x(), e.client_y());
				}) as Box<dyn FnMut(_)>);
				div.add_event_listener_with_callback("mouseenter", mouse_enter.as_ref().unchecked_ref())?;
				mouse_enter.forget();

				let name = obj.name.clone();
				let self_clone = self.clone();
				let mouse_move = wasm_bindgen::closure::Closure::wrap(Box::new(move |e: web_sys::MouseEvent| {
					let _ = self_clone.show_object_name(&name, e.client_x(), e.client_y());
				}) as Box<dyn FnMut(_)>);
				div.add_event_listener_with_callback("mousemove", mouse_move.as_ref().unchecked_ref())?;
				mouse_move.forget();

				let self_clone = self.clone();
				let leave = wasm_bindgen::closure::Closure::wrap(Box::new(move |_e: web_sys::MouseEvent| {
					let _ = self_clone.hide_object_name();
				}) as Box<dyn FnMut(_)>);
				div.add_event_listener_with_callback("mouseleave", leave.as_ref().unchecked_ref())?;
				leave.forget();

				let self_clone = self.clone();
				let interp_clone = interp.clone();
				let context_closure = wasm_bindgen::closure::Closure::wrap(Box::new(move |e: web_sys::MouseEvent| {
					e.prevent_default();
					e.stop_propagation();
					let verbs = interp_clone.verbs_for_object(id);
					let _ = self_clone.show_context_menu(id, e.client_x(), e.client_y(), verbs, interp_clone.clone());
				}) as Box<dyn FnMut(_)>);
				div.add_event_listener_with_callback("click", context_closure.as_ref().unchecked_ref())?;
				context_closure.forget();

				// Allow dropping inventory items onto this object
				let over_self = self.clone();
				let over_interp = interp.clone();
				let over_id = id;
				let over = wasm_bindgen::closure::Closure::wrap(Box::new(move |e: web_sys::DragEvent| {
					if let Some(src) = *over_self.dragging_object.borrow() {
						let src_has = over_interp.verbs_for_object(src).contains(&"vUse".to_string());
						let tgt_has = over_interp.verbs_for_object(over_id).contains(&"vUse".to_string());
						if src_has || tgt_has {
							e.prevent_default();
							if let (Some(src_name), Some(tgt_name)) = (
								over_interp.with_object_by_id(src, |o| o.name.clone()),
								over_interp.with_object_by_id(over_id, |o| o.name.clone()),
							) {
								let text = format!("Use {} on {}", src_name, tgt_name);
								let _ = over_self.show_use_hint(&text, e.client_x(), e.client_y());
							}
						}
					}
				}) as Box<dyn FnMut(_)>);
				div.add_event_listener_with_callback("dragover", over.as_ref().unchecked_ref())?;
				over.forget();

				let leave_self = self.clone();
				let leave = wasm_bindgen::closure::Closure::wrap(Box::new(move |_e: web_sys::DragEvent| {
					let _ = leave_self.hide_use_hint();
				}) as Box<dyn FnMut(_)>);
				div.add_event_listener_with_callback("dragleave", leave.as_ref().unchecked_ref())?;
				leave.forget();

				let drop_self = self.clone();
				let drop_interp = interp.clone();
				let drop_id = id;
				let drop = wasm_bindgen::closure::Closure::wrap(Box::new(move |e: web_sys::DragEvent| {
					e.prevent_default();
					if let Some(src) = *drop_self.dragging_object.borrow() {
						let src_has = drop_interp.verbs_for_object(src).contains(&"vUse".to_string());
						let tgt_has = drop_interp.verbs_for_object(drop_id).contains(&"vUse".to_string());
						if src_has {
							drop_interp.run_verb(src, "vUse", Some(drop_id));
						}
						if tgt_has {
							drop_interp.run_verb(drop_id, "vUse", Some(src));
						}
					}
					let _ = drop_self.hide_use_hint();
				}) as Box<dyn FnMut(_)>);
				div.add_event_listener_with_callback("drop", drop.as_ref().unchecked_ref())?;
				drop.forget();

				self.room_container.append_child(&div)?;
				div
			},
		};

		let mut style = format!(
			"position: absolute; left: {}px; top: {}px; width: {}px; height: {}px;",
			obj.x, obj.y, obj.width, obj.height
		);
		if obj.state > 0 {
			if let Some(img) = obj.states.get(obj.state as usize - 1) {
				if !img.is_empty() {
					style.push_str(&format!(" background: url('{}') no-repeat; background-size: contain;", img));
				}
			}
		}
		element.set_attribute("style", &style)?;
		Ok(())
	}

	//fn create_object_div(&self, object_def: &ObjectDef, _room_id: i32) -> Result<Element, JsValue> {
	//	let div = self.document.create_element("div")?;
	//div.set_attribute("id", &format!("object-{}", object_def.id))?;
	//	div.set_attribute("class", "game-object")?;
	//div.set_attribute("data-object-id", &object_def.id.to_string())?;

	// Basic positioning and styling
	//	let style = "position: absolute; cursor: pointer; border: 1px solid #666; padding: 5px; background: #333; color: white; font-family: monospace; font-size: 12px;".to_string();

	// Set costume-based appearance
	/*if let Some(costume) = object_def.costume {
		// For now, use costume number as a simple visual indicator
		// Later this would load actual image files
		style.push_str(&format!(
			" background-image: url('obj{:03}.png'); background-size: contain; background-repeat: no-repeat; width: 32px; height: 32px;",
			costume
		));
		div.set_inner_html(&format!("<span style='display:none'>{}</span>", object_def.name));
	} else {
		// No costume - show as text
		div.set_inner_html(&object_def.name);
		style.push_str(" min-width: 60px; text-align: center;");
	}*/

	// Random positioning for now (later would be specified in the script)
	//let x = (object_def.id * 37) % 500; // Simple pseudo-random positioning
	//let y = (object_def.id * 73) % 350;
	//style.push_str(&format!(" left: {}px; top: {}px;", x, y));

	//	div.set_attribute("style", &style)?;
	//	div.set_attribute("title", &object_def.name)?;

	//	Ok(div)
	//}

	/*fn show_object(&self, object_id: i32, _room_id: i32) -> Result<(), JsValue> {
		if let Some(div) = self.document.get_element_by_id(&format!("object-{}", object_id)) {
			div.set_attribute("style", &div.get_attribute("style").unwrap_or_default().replace("display: none;", ""))?;
		}
		Ok(())
	}

	fn hide_object(&self, object_id: i32) -> Result<(), JsValue> {
		if let Some(div) = self.document.get_element_by_id(&format!("object-{}", object_id)) {
			let current_style = div.get_attribute("style").unwrap_or_default();
			div.set_attribute("style", &format!("{}; display: none;", current_style))?;
		}
		Ok(())
	}*/

	fn display_message(&self, message: &str) -> Result<(), JsValue> {
		// Create or update message area
		let message_div = if let Some(existing) = self.document.get_element_by_id("game-message") {
			existing
		} else {
			let div = self.document.create_element("div")?;
			div.set_attribute("id", "game-message")?;
			div.set_attribute("style", "position: absolute; bottom: 10px; left: 10px; right: 10px; background: rgba(0,0,0,0.8); color: white; padding: 10px; font-family: monospace; border: 1px solid #666;")?;
			self.game_container.append_child(&div)?;
			div
		};

		message_div.set_inner_html(message);
		Ok(())
	}

	fn show_object_name(&self, name: &str, x: i32, y: i32) -> Result<(), JsValue> {
		let label = if let Some(existing) = self.document.get_element_by_id("object-name") {
			existing
		} else {
			let div = self.document.create_element("div")?;
			div.set_attribute("id", "object-name")?;
			div.set_attribute("style", "position:absolute; pointer-events:none; background: rgba(0,0,0,0.8); color:white; padding:2px 4px; font-family:monospace; font-size:12px; border:1px solid #666; border-radius:4px;")?;
			self.game_container.append_child(&div)?;
			div
		};

		label.set_inner_html(name);
		let rect = self.game_container.get_bounding_client_rect();
		let adj_x = x as f64 - rect.left() + 10.0;
		let adj_y = y as f64 - rect.top() + 10.0;
		label.set_attribute(
                        "style",
                        &format!(
                                "position:absolute; pointer-events:none; background: rgba(0,0,0,0.8); color:white; padding:2px 4px; font-family:monospace; font-size:12px; border:1px solid #666; border-radius:4px; left:{}px; top:{}px; display:block;",
                                adj_x,
                                adj_y
                        ),
                )?;
		Ok(())
	}

	fn hide_object_name(&self) -> Result<(), JsValue> {
		if let Some(label) = self.document.get_element_by_id("object-name") {
			label.set_attribute("style", "display:none;")?;
		}
		Ok(())
	}

	fn show_use_hint(&self, text: &str, x: i32, y: i32) -> Result<(), JsValue> {
		let label = if let Some(existing) = self.document.get_element_by_id("use-hint") {
			existing
		} else {
			let div = self.document.create_element("div")?;
			div.set_attribute("id", "use-hint")?;
			div.set_attribute("style", "position:absolute; pointer-events:none; background: rgba(0,0,0,0.8); color:white; padding:2px 4px; font-family:monospace; font-size:12px; border:1px solid #666; border-radius:4px;")?;
			self.game_container.append_child(&div)?;
			div
		};

		label.set_inner_html(text);
		let rect = self.game_container.get_bounding_client_rect();
		let adj_x = x as f64 - rect.left() + 10.0;
		let adj_y = y as f64 - rect.top() + 10.0;
		label.set_attribute(
                        "style",
                        &format!(
                                "position:absolute; pointer-events:none; background: rgba(0,0,0,0.8); color:white; padding:2px 4px; font-family:monospace; font-size:12px; border:1px solid #666; border-radius:4px; left:{}px; top:{}px; display:block;",
                                adj_x,
                                adj_y
                        ),
                )?;
		Ok(())
	}

	fn hide_use_hint(&self) -> Result<(), JsValue> {
		if let Some(label) = self.document.get_element_by_id("use-hint") {
			label.set_attribute("style", "display:none;")?;
		}
		Ok(())
	}

	fn show_context_menu(&self, obj_id: u32, x: i32, y: i32, verbs: Vec<String>, interp: Interpreter) -> Result<(), JsValue> {
		let menu = if let Some(existing) = self.document.get_element_by_id("context-menu") {
			existing
		} else {
			let div = self.document.create_element("div")?;
			div.set_attribute("id", "context-menu")?;
			div.set_attribute("style", "position:absolute; display:none;")?;
			self.document.body().unwrap().append_child(&div)?;
			let menu_clone = div.clone();
			let hide = wasm_bindgen::closure::Closure::wrap(Box::new(move |_e: web_sys::MouseEvent| {
				let _ = menu_clone.set_attribute("style", "display:none;");
			}) as Box<dyn FnMut(_)>);
			self.document.add_event_listener_with_callback("click", hide.as_ref().unchecked_ref())?;
			hide.forget();
			div
		};

		menu.set_inner_html("");
		for verb in verbs {
			let item = self.document.create_element("div")?;
			let display = scumm_verb_to_user(&verb);
			item.set_inner_html(&display);
			item.set_attribute("class", "menu-item")?;
			let menu_clone = menu.clone();
			let verb_clone = verb.clone();
			let interp_clone = interp.clone();
			let click = wasm_bindgen::closure::Closure::wrap(Box::new(move |_e: web_sys::MouseEvent| {
				interp_clone.run_verb(obj_id, &verb_clone, None);
				let _ = menu_clone.set_attribute("style", "display:none;");
			}) as Box<dyn FnMut(_)>);
			item.add_event_listener_with_callback("click", click.as_ref().unchecked_ref())?;
			click.forget();
			menu.append_child(&item)?;
		}
		menu.set_attribute("style", &format!("position:absolute; left:{}px; top:{}px; display:block;", x, y))?;
		Ok(())
	}

	fn update_inventory(&self, interp: Interpreter, items: &HashSet<u32>) -> Result<(), JsValue> {
		while let Some(child) = self.inventory_container.first_child() {
			let _ = self.inventory_container.remove_child(&child);
		}
		for id in items {
			if let Some(obj) = interp.with_object_by_id(*id, |o| o.clone()) {
				let div = self.document.create_element("div")?;
				div.set_attribute("class", "inventory-item")?;
				let mut style = "width:32px;height:32px;margin:2px;border:1px solid #555;background:#111;color:#fff;font-size:12px;display:flex;align-items:center;justify-content:center;".to_string();
				if obj.state > 0 {
					if let Some(img) = obj.states.get(obj.state as usize - 1) {
						if !img.is_empty() {
							style.push_str(&format!(" background:url('{}') no-repeat center/contain;", img));
							div.set_attribute("title", &obj.name)?;
						} else {
							div.set_inner_html(&obj.name);
						}
					} else {
						div.set_inner_html(&obj.name);
					}
				} else {
					div.set_inner_html(&obj.name);
				}
				div.set_attribute("style", &style)?;
				div.set_attribute("draggable", "true")?;

				// Drag start to initiate use
				let drag_self = self.clone();
				let drag_id = *id;
				let dragstart = wasm_bindgen::closure::Closure::wrap(Box::new(move |_e: web_sys::DragEvent| {
					*drag_self.dragging_object.borrow_mut() = Some(drag_id);
				}) as Box<dyn FnMut(_)>);
				div.add_event_listener_with_callback("dragstart", dragstart.as_ref().unchecked_ref())?;
				dragstart.forget();

				let drag_self = self.clone();
				let dragend = wasm_bindgen::closure::Closure::wrap(Box::new(move |_e: web_sys::DragEvent| {
					*drag_self.dragging_object.borrow_mut() = None;
					let _ = drag_self.hide_use_hint();
				}) as Box<dyn FnMut(_)>);
				div.add_event_listener_with_callback("dragend", dragend.as_ref().unchecked_ref())?;
				dragend.forget();

				// Show object name when hovering
				let name = obj.name.clone();
				let self_clone = self.clone();
				let mouse_enter = wasm_bindgen::closure::Closure::wrap(Box::new(move |e: web_sys::MouseEvent| {
					let _ = self_clone.show_object_name(&name, e.client_x(), e.client_y());
				}) as Box<dyn FnMut(_)>);
				div.add_event_listener_with_callback("mouseenter", mouse_enter.as_ref().unchecked_ref())?;
				mouse_enter.forget();

				let name = obj.name.clone();
				let self_clone = self.clone();
				let mouse_move = wasm_bindgen::closure::Closure::wrap(Box::new(move |e: web_sys::MouseEvent| {
					let _ = self_clone.show_object_name(&name, e.client_x(), e.client_y());
				}) as Box<dyn FnMut(_)>);
				div.add_event_listener_with_callback("mousemove", mouse_move.as_ref().unchecked_ref())?;
				mouse_move.forget();

				let self_clone = self.clone();
				let leave = wasm_bindgen::closure::Closure::wrap(Box::new(move |_e: web_sys::MouseEvent| {
					let _ = self_clone.hide_object_name();
				}) as Box<dyn FnMut(_)>);
				div.add_event_listener_with_callback("mouseleave", leave.as_ref().unchecked_ref())?;
				leave.forget();

				// Allow interacting with inventory objects
				let self_clone = self.clone();
				let interp_clone = interp.clone();
				let id_clone = *id;
				let context_closure = wasm_bindgen::closure::Closure::wrap(Box::new(move |e: web_sys::MouseEvent| {
					e.prevent_default();
					e.stop_propagation();
					let mut verbs = interp_clone.verbs_for_object(id_clone);
					verbs.retain(|v| v != "vPickUp");
					let _ = self_clone.show_context_menu(id_clone, e.client_x(), e.client_y(), verbs, interp_clone.clone());
				}) as Box<dyn FnMut(_)>);
				div.add_event_listener_with_callback("click", context_closure.as_ref().unchecked_ref())?;
				context_closure.forget();

				// Allow dropping other items onto this inventory item
				let self_over = self.clone();
				let interp_over = interp.clone();
				let id_over = *id;
				let over = wasm_bindgen::closure::Closure::wrap(Box::new(move |e: web_sys::DragEvent| {
					if let Some(src) = *self_over.dragging_object.borrow() {
						let src_has = interp_over.verbs_for_object(src).contains(&"vUse".to_string());
						let tgt_has = interp_over.verbs_for_object(id_over).contains(&"vUse".to_string());
						if src_has || tgt_has {
							e.prevent_default();
							if let (Some(src_name), Some(tgt_name)) = (
								interp_over.with_object_by_id(src, |o| o.name.clone()),
								interp_over.with_object_by_id(id_over, |o| o.name.clone()),
							) {
								let text = format!("Use {} on {}", src_name, tgt_name);
								let _ = self_over.show_use_hint(&text, e.client_x(), e.client_y());
							}
						}
					}
				}) as Box<dyn FnMut(_)>);
				div.add_event_listener_with_callback("dragover", over.as_ref().unchecked_ref())?;
				over.forget();

				let self_leave = self.clone();
				let leave = wasm_bindgen::closure::Closure::wrap(Box::new(move |_e: web_sys::DragEvent| {
					let _ = self_leave.hide_use_hint();
				}) as Box<dyn FnMut(_)>);
				div.add_event_listener_with_callback("dragleave", leave.as_ref().unchecked_ref())?;
				leave.forget();

				let self_drop = self.clone();
				let interp_drop = interp.clone();
				let id_drop = *id;
				let drop = wasm_bindgen::closure::Closure::wrap(Box::new(move |e: web_sys::DragEvent| {
					e.prevent_default();
					if let Some(src) = *self_drop.dragging_object.borrow() {
						let src_has = interp_drop.verbs_for_object(src).contains(&"vUse".to_string());
						let tgt_has = interp_drop.verbs_for_object(id_drop).contains(&"vUse".to_string());
						if src_has {
							interp_drop.run_verb(src, "vUse", Some(id_drop));
						}
						if tgt_has {
							interp_drop.run_verb(id_drop, "vUse", Some(src));
						}
					}
					let _ = self_drop.hide_use_hint();
				}) as Box<dyn FnMut(_)>);
				div.add_event_listener_with_callback("drop", drop.as_ref().unchecked_ref())?;
				drop.forget();

				self.inventory_container.append_child(&div)?;
			}
		}
		Ok(())
	}
}


#[derive(Clone, Debug, PartialEq)]
pub enum Value {
	Number(i32),
	Bool(bool),
	Str(String),
	Null,
}

impl Value {
	pub fn truthy(&self) -> bool {
		match self {
			Value::Bool(b) => *b,
			Value::Number(n) => *n != 0,
			Value::Str(s) => !s.is_empty(),
			Value::Null => false,
		}
	}
	pub fn as_number(&self) -> i32 {
		match self {
			Value::Number(n) => *n,
			Value::Bool(b) => {
				if *b {
					1
				} else {
					0
				}
			},
			_ => 0,
		}
	}
	pub fn as_string(&self) -> String {
		match self {
			Value::Str(s) => s.clone(),
			Value::Number(n) => n.to_string(),
			Value::Bool(b) => b.to_string(),
			Value::Null => "<null>".into(),
		}
	}
}


// ---------------------------------------------------------------------------
// World model (super‑simple!)
// ---------------------------------------------------------------------------
#[derive(Default)]
struct World {
	inventory: HashSet<u32>, // OBJ currently held by player
	current_room: u32,
}


type BuiltinFn = dyn for<'a> Fn(Vec<Value>, &'a Ctx) -> LocalBoxFuture<'a, Value>;
type TaskFuture = Pin<Box<dyn Future<Output = ()> + 'static>>;

/// A running instance of a script
struct Task {
	fut: TaskFuture,
	ctx: Ctx,
}


#[derive(Clone)]
pub struct Interpreter {
	//tasks: VecDeque<Task>, // RUNNABLE queue
	pub builtins: Rc<HashMap<String, Rc<BuiltinFn>>>,
	//scripts: Rc<HashMap<String, Script>>,
	//pub consts: Rc<HashMap<String, Value>>,
	world: Rc<RefCell<World>>,
	//objects: Rc<RefCell<HashMap<i32, ObjectDef>>>, // id → def
	//#[allow(dead_code)]
	//object_names: Rc<HashMap<String, i32>>, // lowercase name → id
	web_interface: WebInterface, // Web DOM interface
	run_queue: Rc<RefCell<VecDeque<Task>>>,
	pub declarations: Rc<RefCell<IndexMap<String, Declaration>>>,
	last_room: Rc<RefCell<u32>>,
	last_objects: Rc<RefCell<HashMap<u32, Option<ObjectSnapshot>>>>,
	last_inventory: Rc<RefCell<HashSet<u32>>>,
}

impl Interpreter {
	pub fn new(ast: &[TopLevel]) -> Self {
		// Initialize the interpreter
		let this = Interpreter {
			builtins: Rc::new(build_builtins()),
			world: Rc::new(RefCell::new(World::default())),
			web_interface: WebInterface::new().unwrap(),
			run_queue: Rc::new(RefCell::new(VecDeque::new())),
			declarations: Rc::new(RefCell::new(IndexMap::new())),
			last_room: Rc::new(RefCell::new(0)),
			last_objects: Rc::new(RefCell::new(HashMap::new())),
			last_inventory: Rc::new(RefCell::new(HashSet::new())),
		};

		// Execute the AST to build up all the declarations
		for tl in ast {
			match tl.exec(&this) {
				Ok(_) => {},
				Err(err) => {
					error!("Error executing top-level statement: {:?}", err);
				},
			}
		}

		// Start the main script
		this.spawn_script("main");

		this
	}

	// --------------------------------------------------
	// Script spawning helpers
	// --------------------------------------------------
	fn spawn_script_value(&self, v: Value) {
		match v {
			Value::Number(n) => self.spawn_script_id(n as u32),
			Value::Str(s) => self.spawn_script(&s),
			_ => {},
		}
	}

	fn spawn_script_id(&self, id: u32) {
		if let Some((name, decl)) = self.declarations.borrow().get_index(id as usize) {
			if matches!(decl, Declaration::Script(_)) {
				self.spawn_script(name);
			} else {
				error!("Unknown script id: {}", id);
			}
		} else {
			error!("Unknown script id: {}", id);
		}
	}

	fn spawn_script(&self, key: &str) {
		let script = self.declarations.borrow().get(key).and_then(|decl| match decl {
			Declaration::Script(script) => Some(script.clone()),
			_ => None,
		});
		let script = match script {
			Some(script) => script,
			None => {
				error!("Unknown script: {}", key);
				return;
			},
		};

		info!("startScript -> {}", key);
		self.spawn_block_named(key, script);
	}

	fn spawn_block_named(&self, name: &str, block: Block) {
		let ctx = Ctx {
			vars: Rc::new(RefCell::new(HashMap::new())),
			delay: Rc::new(RefCell::new(0)),
			interpreter: self.clone(),
		};
		let key = name.to_string();
		let cloned_ctx = ctx.clone();
		let task = Task {
			fut: Box::pin(async move {
				if let Err(err) = block.exec(&ctx).await {
					error!("Error executing block {}: {:?}", key, err);
				} else {
					debug!("Block {} executed successfully", key);
				}
			}),
			ctx: cloned_ctx,
		};
		self.run_queue.borrow_mut().push_back(task);
	}

	#[allow(dead_code)]
	fn spawn_block(&self, block: Block) {
		self.spawn_block_named("<anon>", block);
	}

	fn to_id(&self, v: &Value) -> u32 {
		match v {
			Value::Number(n) => (*n).try_into().unwrap_or(0), // Convert i32 to u32, default to 0 if negative
			Value::Str(s) => {
				self.declarations.borrow().get_index_of(s).map(|id| id as u32).unwrap_or(0) // 0 = unknown
			},
			_ => 0,
		}
	}

	// --------------------------------------------------
	// Web-specific tick method
	// Performs a single tick of the interpreter
	// This is intended to be called once per animation frame
	// --------------------------------------------------
	pub fn tick_web(&self) {
		// The entry script may have changed the room already
		if self.world.borrow().current_room == 0 {
			self.world.borrow_mut().current_room = 1;
		}

		//---------------------------------------------------------------
		// Run each runnable task *once* (co-operative multitasking)
		//---------------------------------------------------------------
		let mut n = self.run_queue.borrow().len();

		while n > 0 {
			let mut task = self.run_queue.borrow_mut().pop_front().unwrap();
			//let mut task = self.tasks.pop_front().unwrap();
			n -= 1;

			// Check if the task has a delay
			if *task.ctx.delay.borrow() > 0 {
				*task.ctx.delay.borrow_mut() -= 1;
				self.run_queue.borrow_mut().push_back(task);
				continue;
			}

			// Continue the task
			match task.fut.as_mut().poll(&mut std::task::Context::from_waker(futures::task::noop_waker_ref())) {
				std::task::Poll::Ready(()) => {
					debug!("Task completed");
				},
				std::task::Poll::Pending => {
					self.run_queue.borrow_mut().push_back(task);
					continue; // Still running, continue to next task
				},
			}
		}

		// Update DOM after script execution
		if let Err(e) = self.sync_web() {
			error!("Web sync error: {:?}", e);
		}
	}

	pub fn add_declaration<K: Into<String>>(&self, name: K, decl: Declaration) -> Result<u32, anyhow::Error> {
		match self.declarations.borrow_mut().entry(name.into()) {
			indexmap::map::Entry::Occupied(entry) => {
				// Declaration already exists, return an error
				anyhow::bail!("Declaration for '{}' already exists", entry.key());
			},
			indexmap::map::Entry::Vacant(entry) => {
				// Insert the new declaration
				let id = entry.index() as u32;
				entry.insert(decl);
				Ok(id)
			},
		}
	}

	pub fn with_object_by_name<F, R>(&self, name: &str, f: F) -> Option<R>
	where
		F: FnOnce(&ObjectDef) -> R,
	{
		self.declarations.borrow().get(name).and_then(|decl| match decl {
			Declaration::Object(obj) => Some(f(obj)),
			_ => None,
		})
	}

	pub fn with_object_by_id<F, R>(&self, id: u32, f: F) -> Option<R>
	where
		F: FnOnce(&ObjectDef) -> R,
	{
		self.declarations.borrow().get_index(id as usize).and_then(|(_, decl)| match decl {
			Declaration::Object(obj) => Some(f(obj)),
			_ => None,
		})
	}

	pub fn with_object_by_id_mut<F, R>(&self, id: u32, f: F) -> Option<R>
	where
		F: FnOnce(&mut ObjectDef) -> R,
	{
		self.declarations.borrow_mut().get_index_mut(id as usize).and_then(|(_, decl)| match decl {
			Declaration::Object(obj) => Some(f(obj)),
			_ => None,
		})
	}

	pub fn with_room_by_id<F, R>(&self, id: u32, f: F) -> Option<R>
	where
		F: FnOnce(&RoomDef) -> R,
	{
		self.declarations.borrow().get_index(id as usize).and_then(|(_, decl)| match decl {
			Declaration::Room(room) => Some(f(room)),
			_ => None,
		})
	}

	fn sync_web(&self) -> Result<(), JsValue> {
		let room_id = self.world.borrow().current_room;
		if room_id != *self.last_room.borrow() {
			self.render_room(room_id)?;
			*self.last_room.borrow_mut() = room_id;
		} else {
			self.update_objects(room_id)?;
		}
		self.update_inventory()?;
		Ok(())
	}


	fn render_room(&self, room_id: u32) -> Result<(), JsValue> {
		self.web_interface.clear_room();

		let image = self.with_room_by_id(room_id, |r| r.image.clone()).flatten();
		self.web_interface.set_room_background(image.as_deref())?;

		let mut last_objects = self.last_objects.borrow_mut();

		for (i, (_, decl)) in self.declarations.borrow().iter().enumerate() {
			if let Declaration::Object(obj) = decl {
				if obj.room == room_id && obj.state > 0 {
					self.web_interface.render_object(self.clone(), i as u32, obj)?;
					last_objects.insert(i as u32, Some(ObjectSnapshot::from(obj)));
				} else {
					last_objects.insert(i as u32, None);
				}
			}
		}
		Ok(())
	}

	fn update_objects(&self, room_id: u32) -> Result<(), JsValue> {
		let mut last_objects = self.last_objects.borrow_mut();
		for (i, (_, decl)) in self.declarations.borrow().iter().enumerate() {
			if let Declaration::Object(obj) = decl {
				let id = i as u32;
				let visible = obj.room == room_id && obj.state > 0;
				match (last_objects.get(&id), visible) {
					(Some(Some(snapshot)), true) => {
						let current = ObjectSnapshot::from(obj);
						if *snapshot != current {
							self.web_interface.render_object(self.clone(), id, obj)?;
							last_objects.insert(id, Some(current));
						}
					},
					(Some(Some(_)), false) => {
						self.web_interface.remove_object(id);
						last_objects.insert(id, None);
					},
					(Some(None), true) => {
						self.web_interface.render_object(self.clone(), id, obj)?;
						last_objects.insert(id, Some(ObjectSnapshot::from(obj)));
					},
					(Some(None), false) => {
						// remain hidden
					},
					(None, true) => {
						self.web_interface.render_object(self.clone(), id, obj)?;
						last_objects.insert(id, Some(ObjectSnapshot::from(obj)));
					},
					(None, false) => {
						last_objects.insert(id, None);
					},
				}
			}
		}
		Ok(())
	}

	fn update_inventory(&self) -> Result<(), JsValue> {
		let items = self.world.borrow().inventory.clone();
		if items != *self.last_inventory.borrow() {
			self.web_interface.update_inventory(self.clone(), &items)?;
			*self.last_inventory.borrow_mut() = items;
		}
		Ok(())
	}

	pub fn verbs_for_object(&self, id: u32) -> Vec<String> {
		let decls = self.declarations.borrow();
		let (_, decl) = match decls.get_index(id as usize) {
			Some(v) => v,
			None => return Vec::new(),
		};
		let Declaration::Object(obj) = decl else { return Vec::new() };

		let mut verbs: Vec<String> = obj.verbs.keys().cloned().collect();
		for class_name in &obj.classes {
			if let Some(Declaration::Class(class_def)) = decls.get(class_name) {
				for k in class_def.verbs.keys() {
					if !verbs.contains(k) {
						verbs.push(k.clone());
					}
				}
			}
		}
		verbs
	}

	fn find_verb_block(&self, id: u32, verb: &str) -> Option<Block> {
		let decls = self.declarations.borrow();
		let (_, decl) = decls.get_index(id as usize)?;
		let Declaration::Object(obj) = decl else { return None };

		if let Some(b) = obj.verbs.get(verb) {
			return Some(b.clone());
		}
		for class_name in &obj.classes {
			if let Some(Declaration::Class(class_def)) = decls.get(class_name) {
				if let Some(b) = class_def.verbs.get(verb) {
					return Some(b.clone());
				}
			}
		}
		None
	}

	pub fn run_verb(&self, obj_id: u32, verb: &str, that: Option<u32>) -> bool {
		let Some(block) = self.find_verb_block(obj_id, verb) else { return false };

		let ctx = Ctx {
			vars: Rc::new(RefCell::new(HashMap::new())),
			delay: Rc::new(RefCell::new(0)),
			interpreter: self.clone(),
		};
		ctx.vars.borrow_mut().insert("this".into(), Value::Number(obj_id as i32));
		ctx.vars.borrow_mut().insert("that".into(), Value::Number(that.unwrap_or(0) as i32));
		let key = format!("{}::{}", obj_id, verb);
		let cloned_ctx = ctx.clone();
		let task = Task {
			fut: Box::pin(async move {
				if let Err(err) = block.exec(&ctx).await {
					error!("Error executing block {}: {:?}", key, err);
				}
			}),
			ctx: cloned_ctx,
		};
		self.run_queue.borrow_mut().push_back(task);
		true
	}
}


// --------------------------------------------------
// Built‑ins
// --------------------------------------------------
pub fn builtin_async<F>(f: F) -> Rc<BuiltinFn>
where
	F: 'static + for<'a> Fn(Vec<Value>, &'a Ctx) -> LocalBoxFuture<'a, Value>,
{
	Rc::new(move |args, ctx| f(args, ctx))
}


fn build_builtins() -> HashMap<String, Rc<BuiltinFn>> {
	use Value::*;
	let mut builtins: HashMap<String, Rc<BuiltinFn>> = HashMap::new();

	builtins.insert(
		"print".into(),
		builtin_async(|args, _| {
			async move {
				let output = args.iter().map(|v| v.as_string()).collect::<Vec<_>>().join(" ");
				info!("{}", output);
				Null
			}
			.boxed_local()
		}),
	);

	builtins.insert(
		"wait".into(),
		builtin_async(|mut args, ctx| {
			async move {
				let ticks = args.pop().unwrap_or(Number(1)).as_number();
				*ctx.delay.borrow_mut() = ticks as u32;
				yield_now().await; // yield
				Null
			}
			.boxed_local()
		}),
	);

	builtins.insert(
		"breakScript".into(),
		builtin_async(|_, _ctx| {
			async move {
				yield_now().await; // stop right here, resume next tick
				Value::Null
			}
			.boxed_local()
		}),
	);

	builtins.insert(
		"sayLine".into(),
		builtin_async(|args, ctx| {
			async move {
				if args.len() >= 2 {
					let txt = args[1].as_string();
					let message = txt.trim_matches('"');

					ctx.interpreter.web_interface.display_message(message).unwrap_or_else(|_| {
						info!("{}", message);
					});
				}
				Null
			}
			.boxed_local()
		}),
	);

	builtins.insert(
		"egoSay".into(),
		builtin_async(|args, ctx| {
			async move {
				if let Some(arg) = args.first() {
					let txt = arg.as_string();
					let message = txt.trim_matches('"');

					ctx.interpreter.web_interface.display_message(message).unwrap_or_else(|_| {
						info!("{}", message);
					});
				}
				Null
			}
			.boxed_local()
		}),
	);

	builtins.insert(
		"prompt".into(),
		builtin_async(|args, _| {
			async move {
				let msg = args.first().map(|v| v.as_string()).unwrap_or_default();
				let prompt_text = msg.trim_matches('"');

				// Use web prompt API
				let window = web_sys::window().unwrap();
				if let Ok(Some(result)) = window.prompt_with_message(prompt_text) {
					Value::Str(result)
				} else {
					Value::Str(String::new())
				}
			}
			.boxed_local()
		}),
	);

	builtins.insert(
		"startScript".into(),
		builtin_async(|mut args, ctx| {
			async move {
				if let Some(id) = args.pop() {
					ctx.interpreter.spawn_script_value(id);
				}
				Null
			}
			.boxed_local()
		}),
	);

	builtins.insert(
		"startRoom".into(),
		builtin_async(|args, ctx| {
			async move {
				if args.len() != 1 {
					error!("startRoom requires exactly 1 argument");
					return Null;
				}

				let id = ctx.interpreter.to_id(&args[0]);

				let room_name = match ctx.interpreter.with_room_by_id(id, |r| r.name.clone()) {
					Some(name) => name,
					None => {
						error!("startRoom called with non-room id {}", id);
						return Null;
					},
				};

				ctx.interpreter.world.borrow_mut().current_room = id;
				if let Some(entry) = ctx.interpreter.with_room_by_id(id, |r| r.entry_script.clone()).flatten() {
					ctx.interpreter.spawn_block_named(&format!("{}::entry", room_name), entry);
				}
				Null
			}
			.boxed_local()
		}),
	);

	// ---- World manipulation stubs ----
	// putActorAt(actor, x, y, room)
	builtins.insert(
		"putActorAt".into(),
		builtin_async(|args, ctx| {
			async move {
				if args.len() != 4 {
					error!("putActorAt requires exactly 4 arguments: actor, x, y, room");
					return Null;
				}

				let actor = ctx.interpreter.to_id(&args[0]);
				let room = ctx.interpreter.to_id(&args[3]);

				// NOT FULLY IMPLEMENTED YET
				// Assumes the target actor is the player
				// No position is tracked, so just updates the current room
				ctx.interpreter.world.borrow_mut().current_room = room;

				info!("[actor {actor}] appears in room {room}");
				Null
			}
			.boxed_local()
		}),
	);

	builtins.insert(
		"setCameraAt".into(),
		builtin_async(|args, _| {
			async move {
				if args.len() >= 2 {
					debug!("[camera] room {} target x {}", args[0].as_string(), args[1].as_string());
				}
				Null
			}
			.boxed_local()
		}),
	);
	builtins.insert(
		"walkActorTo".into(),
		builtin_async(|args, _| {
			async move {
				if args.len() >= 3 {
					debug!("[actor {}] walks to {},{}", args[0].as_string(), args[1].as_string(), args[2].as_string());
				}
				Null
			}
			.boxed_local()
		}),
	);
	builtins.insert(
		"faceActor".into(),
		builtin_async(|args, _| {
			async move {
				if args.len() >= 2 {
					debug!("[actor {}] faces dir {}", args[0].as_string(), args[1].as_string());
				}
				Null
			}
			.boxed_local()
		}),
	);

	builtins.insert(
		"getState".into(),
		builtin_async(|args, ctx| {
			async move {
				if args.len() != 1 {
					error!("getState requires exactly 1 argument: object");
					return Null;
				}

				let id = ctx.interpreter.to_id(&args[0]);

				if let Some(state) = ctx.interpreter.with_object_by_id(id, |object| object.state) {
					Value::Number(state as i32)
				} else {
					warn!("[object {id}] not found");
					Null
				}
			}
			.boxed_local()
		}),
	);

	builtins.insert(
		"setState".into(),
		builtin_async(|args, ctx| {
			async move {
				if args.len() != 2 {
					error!("setState requires exactly 2 arguments: object, state");
					return Null;
				}

				let id = ctx.interpreter.to_id(&args[0]);
				let state_value = args[1].as_number() as u32;

				match ctx.interpreter.with_object_by_id_mut(id, |object| {
					object.state = state_value;
					debug!("[object {id}] state set to {state_value}");
				}) {
					Some(_) => {},
					None => {
						warn!("[object {id}] not found");
					},
				}

				Null
			}
			.boxed_local()
		}),
	);

	builtins.insert(
		"objectInHand".into(),
		builtin_async(|args, ctx| {
			async move {
				if let Some(obj) = args.first() {
					let id = ctx.interpreter.to_id(obj);
					Value::Bool(ctx.interpreter.world.borrow().inventory.contains(&id))
				} else {
					Value::Bool(false)
				}
			}
			.boxed_local()
		}),
	);

	builtins.insert(
		"addToInventory".into(),
		builtin_async(|args, ctx| {
			async move {
				if let Some(obj) = args.first() {
					let id = ctx.interpreter.to_id(obj);
					ctx.interpreter.world.borrow_mut().inventory.insert(id);
					ctx.interpreter.with_object_by_id_mut(id, |o| o.room = 0);
					debug!("[inventory] added object {id}");
				}
				Null
			}
			.boxed_local()
		}),
	);

	builtins.insert(
		"pickupObject".into(),
		builtin_async(|args, ctx| {
			async move {
				if let Some(obj) = args.first() {
					let id = ctx.interpreter.to_id(obj);
					ctx.interpreter.world.borrow_mut().inventory.insert(id);
					ctx.interpreter.with_object_by_id_mut(id, |o| o.room = 0);
					debug!("[inventory] picked up object {id}");
				}
				Null
			}
			.boxed_local()
		}),
	);

	builtins.insert(
		"animateObject".into(),
		builtin_async(|args, ctx| {
			async move {
				if args.len() >= 2 {
					let obj_id = ctx.interpreter.to_id(&args[0]);
					let anim_id = args[1].as_number();

					// Log animation and potentially trigger visual effect
					debug!("[object {obj_id}] plays anim {anim_id}");

					// Future enhancement: add CSS animation or transform to the object div
					/*if let Some(obj_div) = ctx.interpreter.web_interface.document.get_element_by_id(&format!("object-{}", obj_id)) {
						// Simple bounce animation for now
						obj_div
							.set_attribute(
								"style",
								&format!("{}; animation: bounce 0.5s ease-in-out;", obj_div.get_attribute("style").unwrap_or_default()),
							)
							.unwrap_or_else(|e| {
								error!("Error animating object: {:?}", e);
							});
					}*/
				}
				Null
			}
			.boxed_local()
		}),
	);

	builtins.insert(
		"loadRoom".into(),
		builtin_async(|args, ctx| {
			async move {
				if let Some(room) = args.first() {
					let id = ctx.interpreter.to_id(room);
					info!("[game] loading room {id} – thanks for playing!");
					ctx.interpreter.run_queue.borrow_mut().clear(); // stop
				}
				Null
			}
			.boxed_local()
		}),
	);

	builtins
}


#[derive(Debug)]
#[must_use = "must await or poll this future"]
pub struct YieldNow(bool);

impl Future for YieldNow {
	type Output = ();

	fn poll(mut self: Pin<&mut Self>, cx: &mut std::task::Context<'_>) -> std::task::Poll<Self::Output> {
		if !self.0 {
			self.0 = true;
			cx.waker().wake_by_ref();
			std::task::Poll::Pending
		} else {
			std::task::Poll::Ready(())
		}
	}
}

pub fn yield_now() -> YieldNow {
	YieldNow(false)
}


// ---------------------------------------------------------------------------
// Convenience wrapper
// ---------------------------------------------------------------------------

#[cfg(target_arch = "wasm32")]
fn scumm_verb_to_user(verb: &str) -> String {
	let trimmed = verb.strip_prefix('v').unwrap_or(verb);
	let mut out = String::new();
	for (i, ch) in trimmed.chars().enumerate() {
		if i == 0 {
			out.extend(ch.to_uppercase());
		} else if ch.is_uppercase() {
			out.push(' ');
			out.extend(ch.to_lowercase());
		} else {
			out.push(ch);
		}
	}
	out
}


#[cfg(test)]
mod tests {
	use super::*;
	use std::collections::HashMap;

	fn run_script(src: &str) -> HashMap<String, Value> {
		let ast = crate::parse_str(src).expect("parse failed");

		// Use the real constructor so that interpreter state is initialised
		let interp = Interpreter::new(&ast);

		// Grab the context of the spawned `main` script so we can inspect its vars
		let ctx = interp.run_queue.borrow().front().expect("no script spawned").ctx.clone();

		// Tick until all tasks have completed.
		// Limit to 128 ticks to avoid infinite loops in tests.
		for _ in 0..128 {
			if interp.run_queue.borrow().is_empty() {
				break;
			}
			interp.tick_web();
		}

		ctx.vars.borrow().clone()
	}

	#[test]
	fn math_precedence() {
		let vars = run_script("script main() {\n    int a = 3 + 4 * 5;\n    int b = (3 + 4) * 5;\n}");
		assert_eq!(vars.get("a"), Some(&Value::Number(23)));
		assert_eq!(vars.get("b"), Some(&Value::Number(35)));
	}

	#[test]
	fn variable_reassignment() {
		let vars = run_script("script main() {\n    int x = 1;\n    x = x + 4 * 2;\n}");
		assert_eq!(vars.get("x"), Some(&Value::Number(9)));
	}

	#[test]
	fn while_loop_executes() {
		let vars = run_script("script main() {\n    int i = 0;\n    while (i < 3) { i = i + 1; }\n}");
		assert_eq!(vars.get("i"), Some(&Value::Number(3)));
	}

	#[test]
	fn if_else_branches() {
		let vars = run_script("script main() {\n    int x = 0;\n    if (1 < 2) { x = 1; } else { x = 2; }\n}");
		assert_eq!(vars.get("x"), Some(&Value::Number(1)));
	}

	#[test]
	fn if_elseif_chain_executes_correct_branch() {
		let src = r#"object Safe { state = 2; }
script main() {
    string msg = "";
    if (getState(Safe) == 1 && 0 == 1) {
        msg = "first";
    } else if (getState(Safe) == 1) {
        msg = "second";
    } else if (getState(Safe) == 2) {
        msg = "third";
    }
}"#;
		let vars = run_script(src);
		assert_eq!(vars.get("msg"), Some(&Value::Str("third".to_string())));
	}

	#[test]
	fn verb_uses_correct_else_if_branch() {
		let src = r#"object Key {}
object Safe {
    state = 2;
    verb vUse(int this, int that) {
        if (getState(this) == 1 && that == Key) {
            setState(this, 5);
        } else if (getState(this) == 1) {
            setState(this, 4);
        } else if (getState(this) == 2) {
            setState(this, 3);
        }
    }
}
script main() {}
"#;

		let ast = crate::parse_str(src).expect("parse failed");
		let interp = Interpreter::new(&ast);

		let safe_id = interp.declarations.borrow().get_index_of("Safe").unwrap() as u32;
		let key_id = interp.declarations.borrow().get_index_of("Key").unwrap() as u32;

		interp.run_verb(safe_id, "vUse", Some(key_id));
		for _ in 0..32 {
			if interp.run_queue.borrow().is_empty() {
				break;
			}
			interp.tick_web();
		}

		let state = interp.with_object_by_id(safe_id, |o| o.state).unwrap();
		assert_eq!(state, 3);
	}

	#[test]
	fn builtin_setstate_getstate() {
		let src = "object O { state = 0; }\nscript main() {\n    int before = getState(O);\n    setState(O, 2);\n    int after = getState(O);\n}";
		let vars = run_script(src);
		assert_eq!(vars.get("before"), Some(&Value::Number(0)));
		assert_eq!(vars.get("after"), Some(&Value::Number(2)));
	}

	#[test]
	fn builtin_inventory() {
		let src = "object O {}\nscript main() {\n    addToInventory(O);\n    bool has = objectInHand(O);\n}";
		let vars = run_script(src);
		assert_eq!(vars.get("has"), Some(&Value::Bool(true)));
	}

	#[test]
	fn pickup_moves_object_to_inventory() {
		let src = r#"room R {
    object O { state = 1; }
}

script main() { pickupObject(O); }"#;
		let ast = crate::parse_str(src).expect("parse failed");
		let interp = Interpreter::new(&ast);

		for _ in 0..32 {
			if interp.run_queue.borrow().is_empty() {
				break;
			}
			interp.tick_web();
		}

		let obj_id = interp.declarations.borrow().get_index_of("O").unwrap() as u32;
		assert!(interp.world.borrow().inventory.contains(&obj_id));
		let room = interp.with_object_by_id(obj_id, |o| o.room).unwrap();
		assert_eq!(room, 0);
	}

	#[test]
	fn room_with_objects_and_entry_script() {
		let src = r#"room Kitchen {
    image = "kitchen.png";
    object Key { state = 0; x = 10; y = 20; }
    object Door {
        state = 1;
        x = 30;
        y = 40;
    }
    script entry() { setState(Key, 2); }
}

script main() {}
"#;

		let ast = crate::parse_str(src).expect("parse failed");
		let interp = Interpreter::new(&ast);

		let decls = interp.declarations.borrow();

		// Verify room exists and has an entry script
		let room_id = decls.get_index_of("Kitchen").expect("room missing") as u32;
		let room_def = match decls.get("Kitchen").expect("room not found") {
			Declaration::Room(def) => def,
			_ => panic!("Kitchen is not a room"),
		};
		assert!(room_def.entry_script.is_some());

		// Objects should belong to the room and keep their properties
		let key_def = match decls.get("Key").expect("Key missing") {
			Declaration::Object(obj) => obj,
			_ => panic!("Key is not an object"),
		};
		assert_eq!(key_def.room, room_id);
		assert_eq!(key_def.state, 0);
		assert_eq!(key_def.x, 10);
		assert_eq!(key_def.y, 20);

		let door_def = match decls.get("Door").expect("Door missing") {
			Declaration::Object(obj) => obj,
			_ => panic!("Door is not an object"),
		};
		assert_eq!(door_def.room, room_id);
		assert_eq!(door_def.state, 1);
		assert_eq!(door_def.x, 30);
		assert_eq!(door_def.y, 40);

		// Entry scripts are stored in the room only, not as a global script
		assert!(decls.get("entry").is_none());
	}

	#[test]
	fn builtin_startroom_runs_entry() {
		let src = r#"room R {
    object O { state = 0; }
    script entry() { setState(O, 1); }
}

script main() { startRoom(R); }
"#;

		let ast = crate::parse_str(src).expect("parse failed");
		let interp = Interpreter::new(&ast);

		// Execute until all tasks have completed
		for _ in 0..32 {
			if interp.run_queue.borrow().is_empty() {
				break;
			}
			interp.tick_web();
		}

		let room_id = interp.declarations.borrow().get_index_of("R").unwrap() as u32;
		assert_eq!(interp.world.borrow().current_room, room_id);

		let obj_state = match interp.declarations.borrow().get("O").unwrap() {
			Declaration::Object(obj) => obj.state,
			_ => unreachable!(),
		};
		assert_eq!(obj_state, 1);
	}

	#[test]
	fn builtin_startscript_runs_script() {
		let src = "object O { state = 0; }\nscript S() { setState(O, 1); }\nscript main() { startScript(S); wait(1); int result = getState(O); }";
		let vars = run_script(src);
		assert_eq!(vars.get("result"), Some(&Value::Number(1)));
	}

	#[test]
	fn web_interface_updates_on_object_state_change() {
		let src = r#"room R {
    object Key {
        state = 1;
        states = { { 0, 0, "key1.png" }, { 0, 0, "key2.png" } };
    }
    object Door { state = 1; }
}

script change() { setState(Key, 2); }
script main() { startRoom(R); }
"#;

		let ast = crate::parse_str(src).expect("parse failed");
		let interp = Interpreter::new(&ast);

		for _ in 0..32 {
			if interp.run_queue.borrow().is_empty() {
				break;
			}
			interp.tick_web();
		}

		let key_id = interp.declarations.borrow().get_index_of("Key").unwrap() as u32;
		let door_id = interp.declarations.borrow().get_index_of("Door").unwrap() as u32;

		let objects = interp.web_interface.get_objects();
		assert!(objects.contains_key(&key_id));
		assert!(objects.contains_key(&door_id));
		assert_eq!(objects[&key_id].state, 1);

		interp.spawn_script("change");
		for _ in 0..32 {
			if interp.run_queue.borrow().is_empty() {
				break;
			}
			interp.tick_web();
		}

		let objects = interp.web_interface.get_objects();
		assert_eq!(objects[&key_id].state, 2);
	}
}
