#![cfg(test)]

use super::*;

macro_rules! eq {
    ($mangled:literal => $demangled:literal) => {
        let symbol = parse($mangled).expect(&format!("Formatting '{}' failed.", $mangled));

        assert_eq!(
            String::from_iter(symbol.tokens().iter().map(|t| &t.text[..])),
            $demangled
        );
    };
}

#[test]
fn crate_ident() {
    eq!("_RC8demangle" => "demangle");
}

#[test]
fn generics() {
    eq!("_RINvNvC3std3mem8align_ofjdE" => "std::mem::align_of::<usize, f64>");
    eq!("_RINvNtC3std3mem8align_ofINtC3wow6HolderpEE" =>
         "std::mem::align_of::<wow::Holder<_>>");
}

#[test]
fn namespaces() {
    eq!("_RNvC4bite6decode" => "bite::decode");
    eq!("_RNvNvC4bite6decode6x86_64" => "bite::decode::x86_64");
    eq!("_RINvNvC4bite6decode6x86_64NvC3lol4damnE" =>
         "bite::decode::x86_64::<lol::damn>");
}

#[test]
fn methods() {
    eq!("_RNvNvXs2_C7mycrateINtC7mycrate3FoopEINtNtC3std7convert4FrompE4from3MSG" =>
         "<mycrate::Foo<_> as std::convert::From<_>>::from::MSG");
}

#[test]
fn pointers() {
    eq!("_RINvC4bite6decodeRL_eE" => "bite::decode::<&str>");
    eq!("_RINvC4bite6decodeRL0_eE" => "bite::decode::<&'a str>");

    eq!("_RINvC4bite6decodeQL_eE" => "bite::decode::<&mut str>");
    eq!("_RINvC4bite6decodeQL0_eE" => "bite::decode::<&'a mut str>");

    eq!("_RINvC4bite6decodePeE" => "bite::decode::<*const str>");
    eq!("_RINvC4bite6decodeOeE" => "bite::decode::<*mut str>");
}

#[test]
fn arrays() {
    eq!("_RINvC4bite6decodeANtNvC3std5array5Arrayjf_E" =>
         "bite::decode::<[std::array::Array; _]>");
}

#[test]
fn tupples() {
    eq!("_RINvNtC3std3mem8align_ofjTddNvC4core3ptrEE" =>
         "std::mem::align_of::<usize, (f64, f64, core::ptr)>");
}

#[test]
fn backref() {
    eq!("_RNvMs1_NtNtCs9ltgdHTiPiY_4core3ptr8non_nullINtB5_7NonNullReE6as_ptrCslWKjbRFJPpS_3log" =>
         "<core::ptr::non_null::NonNull<&str>>::as_ptr");
}

#[test]
fn constants() {
    eq!("_RNvMNtCs9ltgdHTiPiY_4core5sliceSRe4iterCslWKjbRFJPpS_3log" => "<[&str]>::iter");
}

#[test]
fn fn_signature() {
    eq!("_RINvNtC3std3mem8align_ofFUdddEoE" =>
         "std::mem::align_of::<unsafe fn(f64, f64, f64) -> u128>");

    eq!("_RINvNtC3std3mem8align_ofFKCdddEoE" =>
         "std::mem::align_of::<extern \"C\" fn(f64, f64, f64) -> u128>");

    eq!("_RINvNtC3std3mem8align_ofFdddEoE" =>
         "std::mem::align_of::<fn(f64, f64, f64) -> u128>");
}

#[test]
fn dyn_traits() {
    eq!("_RINvNtC4core4simd3mulDNvNtC4core3mem4Readp4ItemReEL_E" =>
         "core::simd::mul::<dyn core::mem::Read<Item = &str>>");

    eq!("_RINvNtC4core4simd3mulDNvNtC4core3mem4ReadEL0_E" =>
         "core::simd::mul::<dyn core::mem::Read + 'a>");

    eq!("_RINvNtC4core4simd3mulDNvNtC4core3mem4ReadEL_E" =>
         "core::simd::mul::<dyn core::mem::Read>");
}

#[test]
fn closures() {
    eq!("_RNCNvC4bite6decode0" => "bite::decode::{closure}");
    eq!("_RNCNvC4bite6decodes_0" => "bite::decode::{closure#0}");
    eq!("_RNCNvC4bite6decodes0_3wow" => "bite::decode::{closure:wow#1}");
}

#[test]
fn complex() {
    eq!("_RNvXs5_NtCsd4VYFwevHkG_4bite6decodeINtB5_5ArrayNtNtB5_6x86_646PrefixKj4_EINtNtNtCs9ltgdHTiPiY_4core3ops5index8IndexMutjE9index_mutB7_" =>
        "<bite::decode::Array<bite::decode::x86_64::Prefix, _> as core::ops::index::IndexMut<usize>>::index_mut");
}

#[test]
#[should_panic]
fn too_many_arguements() {
    parse(
        "IC3stdbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbE",
    )
    .unwrap();
}
