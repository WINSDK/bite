#[cfg(test)]
#[macro_export]
macro_rules! eq {
    ($mangled:literal => $demangled:literal) => {
        let symbol = super::parse($mangled)
            .expect(&format!("Formatting '{}' failed.", $mangled));

        assert_eq!(
            String::from_iter(symbol.tokens().iter().map(|t| &t.text[..])),
            $demangled
        )
    };
}
