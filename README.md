## Compilation and Running

To compile the wasm module, run `wasm-pack build`.
To run, use `npm run dev` which starts a local server.



## TODO

[ ] Lifetime handling is a mess; most every using clones, Rc, RefCell, etc. Need to go through and optimize.



## ScummC Grammar (variant)

This uses a variant of the ScummC grammar.
Execution always starts with the script named `main`.
Objects, scripts, and rooms are assigned a unique ID by the interpreter and are referenced by that ID during runtime.
The default state for objects is state 0.

## Syntax Overview

### Comments
```c
// Single line comment
/* Multi-line 
   comment */
```

### Script Definitions
Scripts are the main execution units. Parameters are declared with type and name:
```c
script main() {
    startRoom(MyRoom);
    startScript(IdleLoop);
}

script processInput(int objectId, string input) {
    // Script body
}
```

### Room Definitions
Rooms define game areas and can contain objects and entry scripts:
```c
room Kitchen {
    image = "kitchen.png";  // Background image
    
    script entry() {
        // Called when entering the room
        putActorAt(player, 100, 200, Kitchen);
    }
    
    object Refrigerator { /* ... */ }
}
```

### Object Definitions
Objects have properties, states, and verbs (interactive behaviors):
```c
object Door {
    x = 100;           // Position
    y = 150;
    w = 80;            // Dimensions  
    h = 120;
    name = "Door";     // Display name
    state = 1;         // Current state (0 = invisible/non-interactive)
    
    states = {
        { 0, 0, "door_closed.png" },  // State 1
        { 0, 0, "door_open.png" }     // State 2
    };
    
    class = { Interactive };  // Inherit from class
    
    verb vLook(int this, int that) {
        egoSay("A wooden door.");
    }
    
    verb vOpen(int this, int that) {
        if (getState(this) == 1) {
            setState(this, 2);
            egoSay("The door opens.");
        }
    }
}
```

### Class Definitions
Classes define reusable behaviors that objects can inherit:
```c
class Interactive {
    verb vPickUp(int this, int that) {
        pickupObject(this);
        egoSay("Got it!");
    }
}
```

### Variables and Types
Variables must be declared with a type:
```c
script example() {
    int counter = 0;
    string message = "Hello";
    bool found = false;
}
```

### Control Flow
Standard control structures are supported:
```c
// If-else statements
if (condition) {
    // then block
} else if (otherCondition) {
    // else if block  
} else {
    // else block
}

// While loops
while (counter < 10) {
    counter = counter + 1;
    wait(1);
}
```

### Expressions and Operators
Full expression support with proper precedence:
```c
// Arithmetic: +, -, *, /
result = 3 + 4 * 5;  // = 23

// Comparison: <, >, <=, >=, ==, !=
if (score >= 100) { /* ... */ }

// Logical: &&, ||, !
if (hasKey && !doorLocked) { /* ... */ }

// Assignment
variable = expression;
```

### Object Properties
Objects support these standard properties:
- `x`, `y`: Position coordinates
- `w`, `h`: Width and height dimensions  
- `name`: Display name (string)
- `state`: Current state number (0 = invisible/non-interactive)
- `states`: Array of state definitions with graphics
- `class`: Array of inherited class names

### Object States
States control object visibility and graphics:
```c
states = {
    { x_offset, y_offset, "image1.png" },  // State 1
    { x_offset, y_offset, "image2.png" },  // State 2
    { x_offset, y_offset, "" },            // State 3 - interactive but no graphics
};
```
State 0 means the object is invisible and non-interactive.
Using an empty string `""` for the image makes the object interactive and visible but renders without any state-specific graphics.

### Verb Definitions
Verbs define how objects respond to player interactions. All verbs take two parameters:
- `this`: The object ID that owns the verb
- `that`: The other object ID (for two-object interactions like "use key on door")

Standard verbs:
- `vLook`: Called when examining the object
- `vUse`: Called when using the object (alone or with another)
- `vPickUp`: Called when taking the object
- `vRead`: Called when reading the object
- `vOpen`: Called when opening the object

### Built-in Functions

#### Core Game Functions
- `startScript(script)`: Starts execution of a script
- `breakScript()`: Yields execution for one tick (for cooperative multitasking)
- `wait(ticks)`: Pauses execution for the specified number of ticks

#### Object Manipulation
- `setState(obj, state)`: Sets object's state (controls visibility/graphics)
- `getState(obj) -> int`: Returns object's current state
- `objectInHand(obj) -> bool`: Checks if object is in inventory
- `pickupObject(obj)`: Adds object to inventory
- `addToInventory(obj)`: Alias for pickupObject
- `animateObject(obj, animId)`: Plays animation on object

#### Player/Actor Functions  
- `putActorAt(actor, x, y, room)`: Places actor at position in room
- `walkActorTo(actor, x, y)`: Makes actor walk to position
- `faceActor(actor, direction)`: Changes actor facing direction

#### UI/Interaction Functions
- `egoSay(message)`: Makes player character speak
- `prompt(question) -> string`: Shows input dialog and returns user input
- `print(values...)`: Outputs to console (for debugging)

#### Room/Scene Functions
- `startRoom(room)`: Changes to specified room
- `setCameraAt(room, x)`: Sets camera position

### Function Parameters
Functions can be called with:
- Object names (identifiers): `setState(Door, 2)` 
- String literals: `egoSay("Hello world")`
- Expressions: `setState(target, getState(source) + 1)`

### Example Complete Script
```c
script main() {
    startRoom(Bedroom);
    startScript(GameLoop);
}

room Bedroom {
    image = "bedroom.png";
    
    object Key {
        x = 200; y = 300; w = 16; h = 16;
        name = "Brass Key";
        class = { Takeable };
        
        verb vLook(int this, int that) {
            egoSay("An old brass key.");
        }
    }
}

class Takeable {
    verb vPickUp(int this, int that) {
        pickupObject(this);
        egoSay("Taken.");
    }
}

script GameLoop() {
    while (1) {
        breakScript();  // Keep running
    }
}
```