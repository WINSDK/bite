use crate::TokenStream;
use config::CONFIG;

mod tests;

pub fn parse(s: &str) -> Option<TokenStream> {
    // macOS prefixes symbols with an extra underscore therefore '__S' is allowed
    let s = s.strip_prefix("ZN").or(s.strip_prefix("_ZN")).or(s.strip_prefix("__ZN"))?;

    // paths have to be ascii
    if !s.bytes().all(|c| c.is_ascii()) {
        return None;
    }

    let mut stream = TokenStream::new(s);
    let s = stream.inner();

    let mut unparsed = s;
    let mut in_first_part = true;

    loop {
        // it's not valid to not have a closing 'E' character
        unparsed.as_bytes().first()?;

        // break on finding closing character
        if let Some(b'E') = unparsed.as_bytes().first() {
            break;
        }

        // there must be a length
        if !unparsed.as_bytes().first()?.is_ascii_digit() {
            return None;
        }

        // length of path component
        let mut len = 0usize;
        while let Some(digit @ b'0'..=b'9') = unparsed.as_bytes().first() {
            len = len.checked_mul(10)?.checked_add((digit - b'0') as usize)?;
            unparsed = &unparsed[1..];
        }

        let mut part = unparsed.get(..len)?;
        unparsed = unparsed.get(len..)?;

        if is_rust_hash(part) {
            break;
        }

        if part.starts_with("_$") {
            part = &part[1..];
        }

        if !in_first_part {
            stream.push("::", CONFIG.colors.delimiter);
        }

        loop {
            if part.starts_with('.') {
                if part[1..].starts_with('.') {
                    stream.push("::", CONFIG.colors.delimiter);
                    part = &part[2..];
                } else {
                    stream.push(".", CONFIG.colors.comment);
                    part = &part[1..];
                }
            } else if part.starts_with('$') {
                let (escape, after_escape) = match part[1..].find('$') {
                    Some(end) => (&part[1..=end], &part[end + 2..]),
                    None => break,
                };

                // source: compiler/rustc_symbol_mangling/src/legacy.rs
                match escape {
                    "SP" => stream.push("@", CONFIG.colors.comment),
                    "BP" => stream.push("*", CONFIG.colors.asm.pointer),
                    "RF" => stream.push("&", CONFIG.colors.asm.pointer),
                    "LT" => stream.push("<", CONFIG.colors.asm.annotation),
                    "GT" => stream.push(">", CONFIG.colors.asm.annotation),
                    "LP" => stream.push("(", CONFIG.colors.asm.label),
                    "RP" => stream.push(")", CONFIG.colors.asm.label),
                    "C" => {
                        // if the next character is a space don't print one
                        //
                        // this is to allow for a space between comma separated items
                        if let Some(b"$u20$") = after_escape.as_bytes().get(..5) {
                            stream.push(",", CONFIG.colors.asm.expr);
                        } else {
                            stream.push(", ", CONFIG.colors.asm.expr);
                        }
                    }
                    _ => {
                        if let Some(stripped) = escape.strip_prefix('u') {
                            let digits = stripped;
                            let all_lower_hex =
                                digits.chars().all(|c| matches!(c, '0'..='9' | 'a'..='f'));

                            let chr = u32::from_str_radix(digits, 16).ok().and_then(char::from_u32);

                            if let (true, Some(chr)) = (all_lower_hex, chr) {
                                if !chr.is_control() {
                                    let color = CONFIG.colors.asm.component;
                                    stream.push_string(chr.to_string(), color);
                                    part = after_escape;
                                    continue;
                                }
                            }
                        }

                        break;
                    }
                }

                part = after_escape;
            } else if let Some(idx) = part.find(|c| c == '$' || c == '.') {
                let ident = &part[..idx];
                stream.push(ident, CONFIG.colors.asm.component);
                part = &part[idx..];
            } else {
                break;
            }
        }

        stream.push(part, CONFIG.colors.asm.component);
        in_first_part = false;
    }

    Some(stream)
}

fn is_rust_hash(s: &str) -> bool {
    s.starts_with('h') && s[1..].chars().all(|c| c.is_ascii_hexdigit())
}
