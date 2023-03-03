pub fn parse(s: &str) -> Option<String> {
    todo!()
}

#[cfg(test)]
mod tests {
    use crate::symbols::Config;
    use once_cell::sync::Lazy;

    static CONFIG: Lazy<Config> =
        Lazy::new(|| Config::from_string(include_str!("../../.dumpfmt").to_string()));

    macro_rules! fmt {
        ($mangled:literal => $demangled:literal) => {
            match $crate::symbols::Symbol::parse($mangled) {
                Ok(sym) if sym.display() != $demangled => {
                    let repr = unsafe { std::str::from_utf8_unchecked(sym.source.buf) };

                    println!("{repr} => {:?}", sym.ast);
                    assert_eq!(sym.display(), $demangled, "left should match right");
                }
                Err(err) => panic!("{err:?}"),
                _ => {}
            }
        };

        ($mangled:literal => $demangled:literal, $cfg:expr) => {
            match $crate::symbols::Symbol::parse_with_config($mangled, &CONFIG) {
                Ok(sym) if sym.display() != $demangled => {
                    let repr = unsafe { std::str::from_utf8_unchecked(sym.source.buf) };

                    println!("{repr} => {:?}", sym.ast);
                    assert_eq!(sym.display(), $demangled, "left should match right");
                }
                Err(err) => panic!("{err:?}"),
                _ => {}
            }
        };
    }

    #[test]
    fn crate_ident() {
        fmt!("_RC8demangle" => "demangle");
    }

    #[test]
    fn generics() {
        fmt!("_RINvNvC3std3mem8align_ofjdE" => "std::mem::align_of::<usize, f64>");
        fmt!("_RINvNtC3std3mem8align_ofINtC3wow6HolderpEE" =>
             "std::mem::align_of::<wow::Holder<_>>");
    }

    #[test]
    fn namespaces() {
        fmt!("_RNvC4bite6decode" => "bite::decode");
        fmt!("_RNvNvC4bite6decode6x86_64" => "bite::decode::x86_64");
        fmt!("_RINvNvC4bite6decode6x86_64NvC3lol4damnE" =>
             "bite::decode::x86_64::<lol::damn>");
    }

    #[test]
    fn methods() {
        fmt!("_RNvNvXs2_C7mycrateINtC7mycrate3FoopEINtNtC3std7convert4FrompE4from3MSG" =>
             "<mycrate::Foo<_> as std::convert::From<_>>::from::MSG");
    }

    #[test]
    fn pointers() {
        fmt!("_RINvC4bite6decodeRL_eE" => "bite::decode::<&str>");
        fmt!("_RINvC4bite6decodeRL0_eE" => "bite::decode::<&'a str>");

        fmt!("_RINvC4bite6decodeQL_eE" => "bite::decode::<&mut str>");
        fmt!("_RINvC4bite6decodeQL0_eE" => "bite::decode::<&'a mut str>");

        fmt!("_RINvC4bite6decodePeE" => "bite::decode::<*const str>");
        fmt!("_RINvC4bite6decodeOeE" => "bite::decode::<*mut str>");
    }

    #[test]
    fn arrays() {
        fmt!("_RINvC4bite6decodeANtNvC3std5array5Arrayjf_E" =>
             "bite::decode::<[std::array::Array; 15]>");
    }

    #[test]
    fn tupples() {
        fmt!("_RINvNtC3std3mem8align_ofjTddNvC4core3ptrEE" =>
             "std::mem::align_of::<usize, (f64, f64, core::ptr)>");
    }

    #[test]
    fn constants() {
        fmt!("_RNvXs5_NtCsd4VYFwevHkG_4bite6decodeINtB5_5ArrayNtNtB5_6x86_646PrefixKj4_EINtNtNtCs9ltgdHTiPiY_4core3ops5index8IndexMutjE9index_mutB7_" =>
             "<bite::decode::Array<bite::decode::x86_64::Prefix, 4> as core::ops::index::IndexMut<usize>>::index_mut");

        fmt!("__RNvMNtCs9ltgdHTiPiY_4core5sliceSRe4iterCslWKjbRFJPpS_3log" => "<[&str]>::iter");

        fmt!("__RNvMs1_NtNtCs9ltgdHTiPiY_4core3ptr8non_nullINtB5_7NonNullReE6as_ptrCslWKjbRFJPpS_3log" =>
             "<core::ptr::non_null::NonNull<&str>>::as_ptr")
    }

    #[test]
    fn fn_signature() {
        fmt!("_RINvNtC3std3mem8align_ofFUKC3rundddEoE" =>
             "std::mem::align_of::<unsafe fn run(f64, f64, f64) -> u128>");

        fmt!("_RINvNtC3std3mem8align_ofFKC3rundddEoE" =>
             "std::mem::align_of::<fn run(f64, f64, f64) -> u128>");

        fmt!("_RINvNtC3std3mem8align_ofFdddEoE" =>
             "std::mem::align_of::<fn(f64, f64, f64) -> u128>");
    }

    #[test]
    fn dyn_traits() {
        fmt!("_RINvNtC4core4simd3mulDNvNtC4core3mem4Readp4ItemReEL_E" =>
             "core::simd::mul::<dyn core::mem::Read<Item = &str>>");

        fmt!("_RINvNtC4core4simd3mulDNvNtC4core3mem4ReadEL0_E" =>
             "core::simd::mul::<dyn core::mem::Read + 'a>");

        fmt!("_RINvNtC4core4simd3mulDNvNtC4core3mem4ReadEL_E" =>
             "core::simd::mul::<dyn core::mem::Read>");
    }

    #[test]
    fn type_compression() {
        fmt!("_RINvNtCs9ltgdHTiPiY_4core3ptr13drop_in_placeNtCs1GtwyVVVJ4z_6goblin6ObjectECsjO9TEQ1PNLx_4bite" =>
             "core::ptr::drop_in_place::<goblin::Object>");
    }

    #[test]
    fn closures() {
        fmt!("_RNCNvC4bite6decodes_0" => "bite::decode::{closure}");
        fmt!("_RNCNvC4bite6decodes0_" => "bite::decode::{closure#1}");
        fmt!("_RNCNvC4bite6decodes0_3wow" => "bite::decode::{closure:wow#1}");

        fmt!("_RINvMNtCs9ltgdHTiPiY_4core6optionINtB3_6OptionRhE3maphNCINvMs9_NtCsd4VYFwevHkG_4bite6decodeNtBZ_6Reader10consume_eqNCNvNtBZ_6x86_643asms_0Es0_0EB11_" =>
             "<core::option::Option<&u8>>::map::<u8, <bite::decode::Reader>::consume_eq::<bite::decode::x86_64::asm::{closure}>::{closure#1}>");
    }

    #[test]
    fn formatting() {
        fmt!("_RNvNvNvNtC4core4iter8adapters3map3Map" => "Map", &CONFIG);

        fmt!("_RNvNvNvNtC4core4iter6traits8iterator8Iterator"=> "Iterator", &CONFIG);

        fmt!("__RINvNtCs6sMkaBefFpu_4core3ptr13drop_in_placeINtNtCsiU9zNs5JoLw_5alloc7raw_vec6RawVecTjINtNtBL_3vec3VecTRejEEEEECsuo8w5Bdzp_4bite" =>
             "drop_in_place::<RawVec::<(usize, Vec::<(&str, usize)>)>>",
             &CONFIG
        );

        fmt!("__RINvNvMs_NtCsiU9zNs5JoLw_5alloc7raw_vecINtB7_6RawVecppE7reserve21do_reserve_and_handlehNtNtC3dem5alloc6GlobalECsuo8w5Bdzp_4bite" =>
             "<RawVec::<_, _>>::reserve::do_reserve_and_handle::<u8, dem::alloc::Global>",
             &CONFIG
        );
    }
}
