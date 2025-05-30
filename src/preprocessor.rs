use std::{
	collections::HashSet,
	fs,
	path::{Path, PathBuf},
};

use anyhow::{Context, Result, anyhow};

/// Flatten an entry source file by inlining all `#include` directives.
pub fn preprocess(entry: &Path) -> Result<String> {
	let mut visited = HashSet::<PathBuf>::new();
	inline_file(entry, entry.parent().unwrap_or_else(|| Path::new(".")), &mut visited)
}

fn inline_file(path: &Path, search_dir: &Path, visited: &mut HashSet<PathBuf>) -> Result<String> {
	let canon = fs::canonicalize(path).with_context(|| format!("Unable to resolve path {path:?}"))?;
	if !visited.insert(canon.clone()) {
		// Already processed – prevents cyclic includes.
		return Ok(String::new());
	}

	let src = fs::read_to_string(&canon).with_context(|| format!("Failed to read source file {canon:?}"))?;

	let mut out = String::new();
	for (lineno, line) in src.lines().enumerate() {
		let trimmed = line.trim_start();
		if let Some(rest) = trimmed.strip_prefix("#include") {
			// Extract quoted path even when a trailing comment is present.
			let rest = rest.trim();
			let first_q = rest.find('"').ok_or_else(|| malformed(lineno, line))?;
			let after = &rest[first_q + 1..];
			let second_q = after.find('"').ok_or_else(|| malformed(lineno, line))?;
			let include_name = &after[..second_q];

			let include_path = search_dir.join(include_name);
			let flattened = inline_file(&include_path, include_path.parent().unwrap_or(search_dir), visited)?;
			out.push_str(&flattened);
		} else {
			out.push_str(line);
			out.push('\n');
		}
	}

	Ok(out)
}

fn malformed(lineno: usize, line: &str) -> anyhow::Error {
	anyhow!(format!("Malformed #include on line {}: {}", lineno + 1, line))
}
