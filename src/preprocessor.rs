use std::{collections::HashSet, fs};

use anyhow::{Context, Result, anyhow};

/// Flatten an entry source file by inlining all `#include` directives.
pub fn preprocess(filename: &str, src: &str) -> Result<String> {
	let mut visited = HashSet::<String>::new();
	inline_file(filename, src, &mut visited)
}

fn inline_file(filename: &str, src: &str, visited: &mut HashSet<String>) -> Result<String> {
	if !visited.insert(filename.to_owned()) {
		// Already processed – prevents cyclic includes.
		return Ok(String::new());
	}

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

			let flattened = inline_file(
				include_name,
				&fs::read_to_string(include_name).with_context(|| format!("Failed to read included file: {}", include_name))?,
				visited,
			)
			.with_context(|| format!("Error processing include: {}", include_name))?;
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
