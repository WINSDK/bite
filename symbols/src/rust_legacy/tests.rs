#![cfg(test)]

use super::*;

macro_rules! none {
    ($mangled:literal) => {
        if parse($mangled).is_some() {
            panic!("Formatting '{}' succeeded when it wasn't supposed to.", $mangled);
        }
    };
}

macro_rules! eq {
    ($mangled:literal => $demangled:literal) => {
        let symbol = parse($mangled).expect(&format!("Formatting '{}' failed.", $mangled));

        assert_eq!(
            String::from_iter(symbol.tokens().iter().map(|t| &t.text[..])),
            $demangled
        );
    };
}

// tests come from https://github.com/rust-lang/rustc-demangle/blob/main/src/legacy.rs

#[test]
fn basic() {
    none!("test");
    eq!("_ZN4testE" => "test");
    none!("_ZN4test");
    eq!("_ZN4test1a2bcE" => "test::a::bc");
}

#[test]
fn dollars() {
    eq!("_ZN4$RP$E" => ")");
    eq!("_ZN8$RF$testE" => "&test");
    eq!("_ZN8$BP$test4foobE" => "*test::foob");
    eq!("_ZN9$u20$test4foobE" => " test::foob");
    eq!("_ZN35Bar$LT$$u5b$u32$u3b$$u20$4$u5d$$GT$E" => "Bar<[u32; 4]>");
}

#[test]
fn many_dollars() {
    eq!("_ZN13test$u20$test4foobE" => "test test::foob");
    eq!("_ZN12test$BP$test4foobE" => "test*test::foob");
}

#[test]
fn osx() {
    eq!("__ZN5alloc9allocator6Layout9for_value17h02a996811f781011E" =>
        "alloc::allocator::Layout::for_value");

    eq!("__ZN38_$LT$core..option..Option$LT$T$GT$$GT$6unwrap18_MSG_FILE_LINE_COL17haf7cb8d5824ee659E" =>
        "<core::option::Option<T>>::unwrap::_MSG_FILE_LINE_COL");
    eq!("__ZN4core5slice89_$LT$impl$u20$core..iter..traits..IntoIterator$u20$for$u20$$RF$$u27$a$u20$$u5b$T$u5d$$GT$9into_iter17h450e234d27262170E" =>
        "core::slice::<impl core::iter::traits::IntoIterator for &'a [T]>::into_iter");
}

#[test]
fn windows() {
    eq!("ZN4testE" => "test");
    eq!("ZN13test$u20$test4foobE" => "test test::foob");
    eq!("ZN12test$RF$test4foobE" => "test&test::foob");
}

#[test]
fn elements_beginning_with_underscore() {
    eq!("_ZN13_$LT$test$GT$E" => "<test>");
    eq!("_ZN28_$u7b$$u7b$closure$u7d$$u7d$E" => "{{closure}}");
    eq!("_ZN15__STATIC_FMTSTRE" => "__STATIC_FMTSTR");
}

#[test]
fn trait_impls() {
    eq!("_ZN71_$LT$Test$u20$$u2b$$u20$$u27$static$u20$as$u20$foo..Bar$LT$Test$GT$$GT$3barE" =>
        "<Test + 'static as foo::Bar<Test>>::bar");
}

#[test]
fn without_hash() {
    eq!("_ZN3foo17h05af221e174051e9E" => "foo");
}

#[test]
fn without_hash_edgecases() {
    // One element, no hash.
    eq!("_ZN3fooE" => "foo");
    // Two elements, no hash.
    eq!("_ZN3foo3barE" => "foo::bar");
}

#[test]
fn thinlto() {
    // One element, no hash.
    eq!("_ZN3fooE.llvm.9D1C9369" => "foo");
    eq!("_ZN3fooE.llvm.9D1C9369@@16" => "foo");
}

#[test]
fn llvm_ir_branch_labels() {
    eq!("_ZN4core5slice77_$LT$impl$u20$core..ops..index..IndexMut$LT$I$GT$$u20$for$u20$$u5b$T$u5d$$GT$9index_mut17haf9727c2edfbc47bE.exit.i.i" =>
        "core::slice::<impl core::ops::index::IndexMut<I> for [T]>::index_mut");
}

#[test]
fn invalid_no_chop() {
    none!("_ZNfooE");
}

#[test]
fn handle_assoc_types() {
    eq!("_ZN151_$LT$alloc..boxed..Box$LT$alloc..boxed..FnBox$LT$A$C$$u20$Output$u3d$R$GT$$u20$$u2b$$u20$$u27$a$GT$$u20$as$u20$core..ops..function..FnOnce$LT$A$GT$$GT$9call_once17h69e8f44b3723e1caE" =>
        "<alloc::boxed::Box<alloc::boxed::FnBox<A, Output=R> + 'a> as core::ops::function::FnOnce<A>>::call_once");
}

#[test]
fn handle_bang() {
    eq!(
        "_ZN88_$LT$core..result..Result$LT$$u21$$C$$u20$E$GT$$u20$as$u20$std..process..Termination$GT$6report17hfc41d0da4a40b3e8E" =>
        "<core::result::Result<!, E> as std::process::Termination>::report"
    );
}
