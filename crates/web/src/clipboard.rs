use std::fmt::Write;

use wasm_bindgen::JsValue;
use wasm_bindgen::closure::Closure;
use yew::Callback;

/// Write text to the browser clipboard, reporting a refusal to `on_error`.
pub fn copy(text: String, on_error: Callback<String>) {
    let Some(window) = web_sys::window() else {
        on_error.emit(String::from("Copying: no window"));
        return;
    };

    let promise = window.navigator().clipboard().write_text(&text);

    let failed = Closure::once(move |error: JsValue| {
        let error = error
            .as_string()
            .or_else(|| js_sys::Error::from(error).message().as_string())
            .unwrap_or_else(|| String::from("refused"));
        on_error.emit(format!("Copying: {error}"));
    });

    let _ = promise.catch(&failed);
    failed.forget();
}

/// Tab-separated rows, one per line.
pub fn text<'a>(rows: impl IntoIterator<Item = Vec<&'a str>>) -> String {
    let mut out = String::new();

    for row in rows {
        out.push_str(&row.join("\t"));
        out.push('\n');
    }

    out
}

/// A pretty-printed JSON array of objects with string or null fields.
pub fn json<'a>(rows: impl IntoIterator<Item = Vec<(&'static str, Option<&'a str>)>>) -> String {
    let mut out = String::from("[");
    let mut first = true;

    for row in rows {
        out.push_str(if first { "\n  {" } else { ",\n  {" });
        first = false;

        for (index, (key, value)) in row.into_iter().enumerate() {
            out.push_str(if index == 0 { "\n    " } else { ",\n    " });
            string(&mut out, key);
            out.push_str(": ");

            match value {
                Some(value) => string(&mut out, value),
                None => out.push_str("null"),
            }
        }

        out.push_str("\n  }");
    }

    out.push_str(if first { "]" } else { "\n]" });
    out
}

fn string(out: &mut String, value: &str) {
    out.push('"');

    for c in value.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if (c as u32) < 0x20 => {
                let _ = write!(out, "\\u{:04x}", c as u32);
            }
            c => out.push(c),
        }
    }

    out.push('"');
}
