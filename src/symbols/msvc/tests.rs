//! Most tests cases are ported from Ghidra.
#![cfg(test)]

use super::*;

macro_rules! eq {
    ($mangled:literal => $demangled:literal) => {
        let symbol = parse($mangled).expect(&format!("Formatting '{}' failed.", $mangled));

        assert_eq!(
            String::from_iter(symbol.tokens().iter().map(|t| &t.text[..])),
            $demangled
        )
    };
}

#[test]
fn simple() {
    let mut parser = Context::new("?x@@YAXMH@Z");
    let mut backrefs = Backrefs::default();
    let tree = Symbol::parse(&mut parser, &mut backrefs).unwrap();

    assert_eq!(
        tree,
        Symbol {
            path: Path {
                name: UnqualifiedPath(
                    NestedPath::Literal(Literal::Borrowed { start: 1, end: 2 }),
                ),
                scope: Scope(vec![]),
            },
            tipe: Type::Function(
                Function {
                    calling_conv: CallingConv::Cdecl,
                    qualifiers: FunctionQualifiers(Modifiers::empty()),
                    return_type: Box::new(FunctionReturnType(Type::Void(Modifiers::empty()))),
                    params: FunctionParameters(Parameters(vec![
                        Type::Float(Modifiers::empty()),
                        Type::Int(Modifiers::empty()),
                    ])),
                }
            ),
        }
    );

    eq!("?x@@YAXMH@Z" => "void __cdecl x(float, int)");
}

#[test]
fn constructor() {
    eq!("??0klass@@QEAA@XZ" => "__cdecl klass::klass(void)");
}

#[test]
fn destructor() {
    eq!("??1klass@@QEAA@XZ" => "__cdecl klass::~klass(void)");
}

#[test]
fn triple_q0() {
    eq!("???__E??_7name0@name1@@6B@@@YMXXZ@?A0x647dec29@@$$FYMXXZ" =>
        "void __clrcall `dynamic initializer for 'const name1::name0::`vftable'''(void)");
}

#[test]
fn triple_q0_mod1() {
    eq!("???__E??_7name0@name1@@6Bx@xx@@y@yy@@@z@zz@@YMXXZ@?A0x647dec29@@$$FYMXXZ" =>
        "void __clrcall zz::z::`dynamic initializer for 'const name1::name0::`vftable'{for `xx::x's `yy::y'}''(void)");
}

#[test]
fn triple_q0_mod2() {
    eq!("???__E??_7name0@name1@@6B@z@@YMXXZ@?A0x647dec29@@$$FYMXXZ" =>
        "void __clrcall z::`dynamic initializer for 'const name1::name0::`vftable'''(void)");
}

#[test]
fn triple_q0_breakdown1() {
    eq!("?var@?A0x647dec29@@$$FYMXXZ" => "void __clrcall `anonymous namespace'::var(void)");
}

#[test]
fn triple_q0_breakdown2() {
    eq!("?var@@YMXXZ" => "void __clrcall var(void)");
}

#[test]
fn triple_q0_breakdown3() {
    eq!("??_7name0@name1@@6B@" => "const name1::name0::`vftable'");
}

#[test]
fn triple_q0_breakdown3a() {
    eq!("??_7name0@name1@@6B" => "const name1::name0::`vftable'{for ??}");
}

#[test]
fn triple_q0_breakdown3b() {
    eq!("??_7name0@name1@@6Baaa@@@" => "const name1::name0::`vftable'{for `aaa'}");
}

#[test]
fn triple_q0_breakdown4() {
    eq!("??__E?var@@6B@@@YMXXZ" => "void __clrcall `dynamic initializer for 'const var''(void)");
}

#[test]
fn triple_q0_breakdown5() {
    eq!("??__Evar@@YMXXZ" => "void __clrcall `dynamic initializer for 'var''(void)");
}

#[test]
fn triple_q1a() {
    eq!("???__Ename0@name1@@YMXXZ@?A0xd585d5fc@@$$FYMXXZ" =>
        "void __clrcall name1::`dynamic initializer for 'name0''(void)");
}

#[test]
fn triple_q1a_breakdown_analysis_000() {
    eq!("?name0@@$$FYMXXZ" => "void __clrcall name0(void)");
}

#[test]
fn triple_q2a() {
    eq!("???__E?name0@name1@<name2>@@$$Q2_NA@@YMXXZ@?A0x3d49b2d0@@$$FYMXXZ" =>
        "void __clrcall `dynamic initializer for 'public: static bool <name2>::name1::name0''(void)");
}

#[test]
fn triple_q2a_breakdown0() {
    eq!("?var@?A0x3d49b2d0@@$$FYMXXZ" =>
        "void __clrcall `anonymous namespace'::var(void)");
}

#[test]
fn triple_q2a_breakdown1() {
    eq!("?name0@name1@<name2>@@$$Q2_NA" => "public: static bool <name2>::name1::name0");
}

#[test]
fn triple_q2a_breakdown2() {
    eq!("??__E?var@@3HA@@YMXXZ" => "void __clrcall `dynamic initializer for 'int var''(void)");
}

#[test]
fn triple_q8a() {
    eq!("???__E??_7name0@@6B@@@YMXXZ@?A0xc2524ebc@@$$FYMXXZ" => "void __clrcall `dynamic initializer for 'const name0::`vftable'''(void)");
}

#[test]
fn triple_q8a1() {
    eq!("???__E??_7name0@@6B@name1@@YMXXZ@?A0xc2524ebc@@$$FYMXXZ" =>
        "void __clrcall name1::`dynamic initializer for 'const name0::`vftable'''(void)");
}

#[test]
fn triple_q3a() {
    eq!("???__E?name0@name1@@3HA@@YMXXZ@?A0x09343ef7@@$$FYMXXZ" =>
        "void __clrcall `dynamic initializer for 'int name1::name0''(void)");
}

#[test]
fn triple_q1() {
    eq!("???__Ename0@name1@@YMXXZ@?A0xd585d5fc@@$$FYMXXZ" =>
        "void __clrcall name1::`dynamic initializer for 'name0''(void)");
}

#[test]
fn triple_q2() {
    eq!("???__E?name0@name1@<name2>@@$$Q2_NA@@YMXXZ@?A0x3d49b2d0@@$$FYMXXZ" => "void __clrcall `dynamic initializer for 'public: static bool <name2>::name1::name0''(void)");
}

#[test]
fn triple_q4() {
    eq!("???__E?name0@name1@name2@@$$Q2W4name3@name4@2@A@@YMXXZ@?A0x3d49b2d0@@$$FYMXXZ" =>
        "void __clrcall `dynamic initializer for 'public: static enum name2::name4::name3 name2::name1::name0''(void)");
}

#[test]
fn triple_q5() {
    eq!("???__E?name0@name1@name2@@$$Q2HA@@YMXXZ@?A0x3d49b2d0@@$$FYMXXZ" =>
        "void __clrcall `dynamic initializer for 'public: static int name2::name1::name0''(void)");
}

#[test]
fn triple_q6() {
    eq!("???__E?name0@name1@name2@@$$Q2W4name3@name4@2@A@@YMXXZ@?A0x3d49b2d0@@$$FYMXXZ" =>
        "void __clrcall `dynamic initializer for 'public: static enum name2::name4::name3 name2::name1::name0''(void)");
}

#[test]
fn triple_q7() {
    eq!("???__E?name0@name1@name2@@$$Q2W4name3@name4@2@A@@YMXXZ@?A0x3d49b2d0@@$$FYMXXZ" =>
        "void __clrcall `dynamic initializer for 'public: static enum name2::name4::name3 name2::name1::name0''(void)");
}

#[test]
fn triple_q8() {
    eq!("???__E??_7name0@@6B@@@YMXXZ@?A0xc2524ebc@@$$FYMXXZ" =>
        "void __clrcall `dynamic initializer for 'const name0::`vftable'''(void)");
}

#[test]
fn white_space_formatting1() {
    // Example: Space after template parameter (cv modifier).
    eq!("??0?$name0@$$CBUname1@@@name2@@QEAA@XZ" =>
        "public: __cdecl name2::name0<struct name1 const >::name0<struct name1 const >(void)");
}

#[test]
fn white_space_formatting2() {
    eq!("?name0@name1@@MAEPAPAP6GJPAUname2@@IIJ@ZXZ" =>
        "protected: virtual long (__stdcall** * __thiscall name1::name0(void))(struct name2 *, unsigned int, unsigned int, long)");
}

#[test]
fn white_space_formatting3() {
    //Example: Has trailing white space.
    eq!("?VarName@@3P9ClassName@@DAHXZED" =>
        "int (__cdecl ClassName::*const volatile VarName)(void)const volatile ");
}

#[test]
fn function_pointer() {
    eq!("?fn@@3P6AHH@ZA" => "int (__cdecl *fn)(int)");
}

#[test]
fn function_pointer_named_function_pointer_with_anonymous_function_pointer_parameter() {
    eq!("?fun@@3P6KXP6KXH@Z@ZA" => "void (* fun)(void (*)(int))");
}

#[test]
#[should_panic]
fn function_pointer_emod_invalid() {
    eq!("?fn@@3PE6AHH@ZA" => "?fn@@3PE6AHH@ZA");
}

#[test]
#[should_panic]
fn function_pointer_dollar_amod_invalid() {
    eq!("?fn@@3P$A6AHH@ZA" => "?fn@@3P$A6AHH@ZA");
}

#[test]
#[should_panic]
fn function_pointer_dollar_bmod_invalid() {
    eq!("?fn@@3P$B6AHH@ZA" => "?fn@@3P$B6AHH@ZA");
}

#[test]
#[should_panic]
fn function_pointer_dollar_cmod_invalid() {
    eq!("?fn@@3P$C6AHH@ZA" => "?fn@@3P$C6AHH@ZA");
}

#[test]
fn function_reference() {
    eq!("?fn@@3A6AHH@ZA" => "int (__cdecl &fn)(int)");
}

#[test]
#[should_panic]
fn function_reference_emod_invalid() {
    eq!("?fn@@3AE6AHH@ZA" => "?fn@@3AE6AHH@ZA");
}

#[test]
#[should_panic]
fn function_reference_dollar_amod_invalid() {
    eq!("?fn@@3A$A6AHH@ZA" => "?fn@@3A$A6AHH@ZA");
}

#[test]
#[should_panic]
fn function_reference_dollar_bmod_invalid() {
    eq!("?fn@@3A$B6AHH@ZA" => "?fn@@3A$B6AHH@ZA");
}

#[test]
#[should_panic]
fn function_reference_dollar_cmod_invalid() {
    eq!("?fn@@3A$C6AHH@ZA" => "?fn@@3A$C6AHH@ZA");
}

#[test]
#[should_panic]
fn function_question_modifier_invalid() {
    eq!("?fn@@3?6AHH@ZA" => "?fn@@3?6AHH@ZA");
}

#[test]
fn pointer_to_function_pointer() {
    eq!("?fn@@3PAP6AHH@ZA" => "int (__cdecl **fn)(int)");
}

#[test]
fn pointer_to_pointer_to_function_pointer() {
    eq!("?fn@@3PAPAP6AHH@ZA" => "int (__cdecl** * fn)(int)");
}

#[test]
fn pointer_to_data() {
    eq!("?var@@3PBHC" => "int const volatile *var");
}

#[test]
fn reference_to_data() {
    eq!("?var@@3ABHC" => "int const volatile &var");
}

#[test]
fn question_to_data() {
    eq!("?var@@3?BHC" => "int const volatile var");
}

#[test]
fn pointer_to_pointer_to_data() {
    eq!("?var@@3PDPBHC" => "int const * const volatile * volatile var");
}

#[test]
fn pointer_to_reference_to_data() {
    eq!("?var@@3PDABHC" => "int const &const volatile *var");
}

#[test]
fn reference_to_pointer_to_data() {
    eq!("?var@@3ADPBHC" => "int const * const volatile & volatile var");
}

#[test]
fn question_to_pointer_to_data() {
    eq!("?var@@3?DPBHC" => "int const * const volatile volatile var");
}

#[test]
fn question_to_reference_to_data() {
    eq!("?var@@3?DABHC" => "int const & const volatile volatile var");
}

#[test]
fn extern_c_1() {
    eq!("?abort@@$$J0YAXXZ" => "extern \"C\" void __cdecl abort(void)");
}

#[test]
fn extern_c_2_j() {
    //Manufactured data
    eq!("?xyz@@$$J00HA" => "extern \"C\" private: static int xyz");
}

#[test]
fn extern_c_2_n() {
    //Manufactured data
    eq!("?xyz@@$$N00HA" => "extern \"C\" private: static int xyz");
}

#[test]
fn extern_c_2_o() {
    //Manufactured data
    eq!("?xyz@@$$O00HA" => "extern \"C\" private: static int xyz");
}

#[test]
fn extern_c_2_j_1() {
    //Manufactured data
    eq!("?xyz@@$$J110HA" => "extern \"C\" private: static int xyz");
}

#[test]
fn extern_c_2_j_2() {
    //Manufactured data
    eq!("?xyz@@$$J2220HA" => "extern \"C\" private: static int xyz");
}

#[test]
fn extern_c_2_j_3() {
    //Manufactured data
    eq!("?xyz@@$$J33330HA" => "extern \"C\" private: static int xyz");
}

#[test]
fn extern_c_2_j_4() {
    //Manufactured data
    eq!("?xyz@@$$J444440HA" => "extern \"C\" private: static int xyz");
}

#[test]
fn extern_c_2_j_5() {
    //Manufactured data
    eq!("?xyz@@$$J5555550HA" => "extern \"C\" private: static int xyz");
}

#[test]
fn extern_c_2_j_6() {
    //Manufactured data
    eq!("?xyz@@$$J66666660HA" => "extern \"C\" private: static int xyz");
}

#[test]
fn extern_c_2_j_7() {
    //Manufactured data
    eq!("?xyz@@$$J777777770HA" => "extern \"C\" private: static int xyz");
}

#[test]
fn extern_c_2_j_8() {
    //Manufactured data
    eq!("?xyz@@$$J8888888880HA" => "extern \"C\" private: static int xyz");
}

#[test]
fn extern_c_2_j_9() {
    //Manufactured data
    eq!("?xyz@@$$J99999999990HA" => "extern \"C\" private: static int xyz");
}

#[test]
fn extern_c_3() {
    eq!("?name0@@$$J0YMXP6MXPAX@Z0@Z" => "extern \"C\" void __clrcall name0(void (__clrcall*)(void *), void *)");
}

#[test]
fn file_one_sample_2() {
    eq!("??1?$name0@PEAUname1@@$0A@P6APEAXPEAX@Z$1?name2@@$$FYAPEAX0@ZP6AAEAPEAU1@AEAPEAU1@@Z$1?name3@?$name4@PEAUname1@@@@$$FSAAEAPEAU1@1@Z@@$$FMEAA@XZ" =>
        "protected: virtual __cdecl name0<struct name1 *, 0, void * (__cdecl*)(void * ), &void * __cdecl name2(void * ), struct name1 * & (__cdecl*)(struct name1 * & ), &public: static struct name1 * & __cdecl name4<struct name1 * >::name3(struct name1 * & )>::~name0<struct name1 *, 0, void * (__cdecl*)(void * ), &void * __cdecl name2(void * ), struct name1 * & (__cdecl*)(struct name1 * & ), &public: static struct name1 * & __cdecl name4<struct name1 * >::name3(struct name1 * & )>(void) ");
}

#[test]
fn cast_operator1() {
    eq!("??Bname0@@QEBAIXZ" => "public: __cdecl name0::operator unsigned int(void)const ");
}

#[test]
fn cast_operator2() {
    eq!("??Bname0@@QEBAVname1@@XZ" => "public: __cdecl name0::operator class name1(void)const ");
}

#[test]
fn cast_operator3() {
    eq!("??Bname0@@QEBAPEBVname1@@XZ" => "public: __cdecl name0::operator class name1 const * (void)const ");
}

#[test]
fn cast_operator4() {
    eq!("??Bname0@name1@@QEAAPEAVname2@name3@@XZ" => "public: __cdecl name1::name0::operator class name3::name2 * (void) ");
}

#[test]
fn cast_operator5() {
    eq!("??Bname0@@QEBAP6AP6AXXZXZXZ" => "public: __cdecl name0::operator void (__cdecl*(__cdecl*)(void))(void)(void)const ");
}

#[test]
fn cast_operator6() {
    eq!("??$?BPEAE@?$name0@PEAE@name1@@QEAA?AU?$name2@PEAE@1@XZ" =>
        "public: __cdecl name1::name0<unsigned char * >::operator<unsigned char * > struct name1::name2<unsigned char * >(void) ");
}

#[test]
fn cast_operator7() {
    eq!("??$?BPEAEU?$name0@U?$name1@Uname2@name3@@Uname4@2@Uname5@2@U?$name6@U?$name7@Uname8@name3@@@name3@@Uname9@name10@2@@2@U?$name11@$0A@@2@Uname12@2@@name3@@@name3@@@?$name13@PEAEU?$name0@U?$name1@Uname2@name3@@Uname4@2@Uname5@2@U?$name6@U?$name7@Uname8@name3@@@name3@@Uname9@name10@2@@2@U?$name11@$0A@@2@Uname12@2@@name3@@@name3@@@name3@@QEAA?AU?$name14@PEAEU?$name0@U?$name1@Uname2@name3@@Uname4@2@Uname5@2@U?$name6@U?$name7@Uname8@name3@@@name3@@Uname9@name10@2@@2@U?$name11@$0A@@2@Uname12@2@@name3@@@name3@@@1@XZ" =>
        "public: __cdecl name3::name13<unsigned char *, struct name3::name0<struct name3::name1<struct name3::name2, struct name3::name4, struct name3::name5, struct name3::name6<struct name3::name7<struct name3::name8>, struct name3::name10::name9>, struct name3::name11<0>, struct name3::name12> > >::operator<unsigned char *, struct name3::name0<struct name3::name1<struct name3::name2, struct name3::name4, struct name3::name5, struct name3::name6<struct name3::name7<struct name3::name8>, struct name3::name10::name9>, struct name3::name11<0>, struct name3::name12> > > struct name3::name14<unsigned char *, struct name3::name0<struct name3::name1<struct name3::name2, struct name3::name4, struct name3::name5, struct name3::name6<struct name3::name7<struct name3::name8>, struct name3::name10::name9>, struct name3::name11<0>, struct name3::name12> > >(void) ");
}

#[test]
fn array_o_1() {
    eq!("?name0@name1@@0_OBHB" => "private: static int const name1::name0[]");
}

#[test]
fn array_o_2() {
    eq!("?name0@@3_OAPEBUname1@@B" => "struct name1 const name0[]");
}

#[test]
fn array_o_2moda() {
    //Manufactured, added PEBPEB after (one PEB after--as in original
    eq!("?name0@@3_OAPEBPEBPEBUname1@@B" => "struct name1 const name0[]");
}

#[test]
fn array_o_2modb() {
    // manufactured, added PEBPEB prior (one PEB after--as in original
    eq!("?name0@@3PEBPEB_OAPEBUname1@@B" => "struct name1 const * const * const name0[]");
}

#[test]
fn array_o_2modc() {
    // manufactured, added PEBY01 prior (one PEB after--as in original
    eq!("?name0@@3PEBY01_OAPEBUname1@@B" => "struct name1 (const * const name0)[2][]");
}

#[test]
fn array_o_2modd() {
    // manufactured, added PEBYA01PEB prior (one PEB after--as in original
    eq!("?name0@@3PEBY01PEB_OAPEBUname1@@B" => "struct name1 const * (const * const name0)[2][]");
}

#[test]
fn array_o_4() {
    eq!("?name0@name1@@0_OBQEBGB" => "private: static unsigned short const name1::name0[]");
}

#[test]
fn array_o_5() {
    // manufactured: added pointers to pointers to function pointers, which gets stripped on emit()
    eq!("?name0@name1@@0_OBPEBPEBP6A?BHH@ZB" =>
        "private: static int const (__cdecl*const name1::name0)(int)[]");
}

#[test]
fn array_o_6() {
    // manufactured
    eq!("?name0@name1@@0_O6A?BHH@ZA" => "private: static int const (__cdecl name1::name0)(int)[]");
}

#[test]
fn interesting_array_array_o_a() {
    eq!("?Var@@0_OBY01QEBHB" => "private: static int const Var[][2]");
}

//parses, but ignores EIF, member, based, and const/volatile... will only output const or volatile, with const preference over volatile.
#[test]
fn interesting_array_array_o_parses_but_ignores_all_cveifmember_based_1() {
    //parses, but ignores EIF
    eq!("?Var@@0_OEIF5aaa@@2bbb@@Y01QEBHB" => "private: static int const Var[][2]");
}

//parses, but ignores EIF, member, based, and const/volatile... will only output const or volatile, with const preference over volatile.
#[test]
fn interesting_array_array_o_parses_but_ignores_all_cveifmember_based_2() {
    //parses, but ignores EIF
    eq!("?Var@@0_OEIF5aaa@@2bbb@@Y01QEIF5ccc@@2ddd@@HB" => "private: static int const Var[][2]");
}

//TODO: CREATE mstruth output (dispatcher)
#[test]
fn interesting_array_array_o_b() {
    eq!("?Var@@0_OBY00QEBY01HB" => "private: static int const Var[][1][][2]");
}

//TODO: CREATE mstruth output (dispatcher)
#[test]
fn interesting_array_array_o_c() {
    eq!("?Var@@0_OBY00QEBY01QEBY02HB" => "private: static int const Var[][1][][2][][3]");
}

#[test]
fn back_ref_x1() {
    eq!("??0name0@name1@name2@@QEAA@AEBV?$name3@_WU?$name4@_W@name5@@V?$name6@_W@2@Vname7@@@name5@@V?$name8@PEAXU?$name9@U?$name10@U?$name11@P6AHPEAX@Z$1?name12@@YAH0@Z@name13@@Uname14@2@Uname15@2@U?$name16@U?$name17@U?$name18@PEAX$0?0@name13@@@name13@@Uname19@name20@2@@2@U?$name21@$0A@@2@Uname22@2@@name13@@@name13@@@name13@@@Z" =>
        "public: __cdecl name2::name1::name0::name0(class name5::name3<wchar_t, struct name5::name4<wchar_t>, class name5::name6<wchar_t>, class name7> const &, class name13::name8<void *, struct name13::name9<struct name13::name10<struct name13::name11<int (__cdecl*)(void * ), &int __cdecl name12(void * )>, struct name13::name14, struct name13::name15, struct name13::name16<struct name13::name17<struct name13::name18<void *, -1> >, struct name13::name20::name19>, struct name13::name21<0>, struct name13::name22> > >)");
}

#[test]
fn back_ref_x2() {
    eq!("??0name0@?1??name1@name2@@UAEXXZ@QAE@V?$name3@GU?$name4@G@name5@@V?$name6@G@2@@name5@@0@Z" =>
        "public: __thiscall `public: virtual void __thiscall name2::name1(void)'::`2'::name0::name0(class name5::name3<unsigned short, struct name5::name4<unsigned short>, class name5::name6<unsigned short> >, class name5::name3<unsigned short, struct name5::name4<unsigned short>, class name5::name6<unsigned short> >)");
}

#[test]
fn special_f() {
    eq!("??__Fname0@?1??name1@name2@name3@name4@@CAXPEAUname5@@P84@EAAJPEAPEAG@ZW4name6@@PEAUname7@@@Z@YAXXZ" =>
        "void __cdecl `private: static void __cdecl name4::name3::name2::name1(struct name5 *, long (__cdecl name4::*)(unsigned short * * ), enum name6, struct name7 * )'::`2'::`dynamic atexit destructor for 'name0''(void)");
}

#[test]
fn nested_function_pointer1a() {
    eq!("?name0@@3P6AP6AXXZXZEA" => "void (__cdecl*(__cdecl* name0)(void))(void)");
}

#[test]
fn nested_function_pointer1a_with_publicstatic() {
    eq!("?name0@@2P6AP6AXXZXZEA" => "public: static void (__cdecl*(__cdecl* name0)(void))(void)");
}

#[test]
fn nested_function_reference1a_with_publicstatic() {
    eq!("?name0@@2A6AA6AXXZXZEA" => "public: static void (__cdecl&(__cdecl& name0)(void))(void)");
}

#[test]
fn nested_function_indirect1a_with_publicstatic() {
    eq!("?name0@@2$$A6A$$A6AXXZXZEA" => "public: static void (__cdecl(__cdecl name0)(void))(void)");
}

#[test]
fn nested_function_pointer1b() {
    eq!("?name0@@3_O6AP6AXXZXZEA" => "void (__cdecl*(__cdecl name0)(void))(void)[]");
}

#[test]
fn nested_function_pointer1b1() {
    eq!("?name0@@3_O6AP6AXXZXZA" => "void (__cdecl*(__cdecl name0)(void))(void)[]");
}

#[test]
fn nested_function_pointer1c() {
    eq!("?name0@@3_O6A_O6AXXZXZEA" => "void (__cdecl(__cdecl name0)(void))(void)[][]");
}

#[test]
fn nested_function_pointer2() {
    eq!("?name0@@3P6AP6AHPEAXIPEBG@ZP6AH0I1@ZK0@ZEA" =>
        "int (__cdecl*(__cdecl* name0)(int (__cdecl*)(void *, unsigned int, unsigned short const * ), unsigned long, void * ))(void *, unsigned int, unsigned short const * )");
}

#[test]
fn nested_function_pointer3() {
    eq!("?name0@name1@@0P6AP6AHPEAXIPEBG@ZP6AH0I1@ZK0@ZEA" =>
        "private: static int (__cdecl*(__cdecl* name1::name0)(int (__cdecl*)(void *, unsigned int, unsigned short const * ), unsigned long, void * ))(void *, unsigned int, unsigned short const * )");
}

#[test]
fn other_function_pointer_with_elipses() {
    //Demonstrates that varargs ("...") is not a complex type, as if it was,
    //  it would be put on the BackrefParameters list and change the backref used
    eq!("?name0@name1@@QEAAKP6AKPEAXKZZP6AKPEBGZZ1@Z" =>
        "public: unsigned long __cdecl name1::name0(unsigned long (__cdecl*)(void *, unsigned long, ...), unsigned long (__cdecl*)(unsigned short const *, ...), unsigned long (__cdecl*)(void *, unsigned long, ...)) ");
}

#[test]
fn function_pointer_rtti_r0() {
    eq!("??_R0P6AXPEAUname0@@@Z@8" => "void (__cdecl*)(struct name0 * ) `RTTI Type Descriptor'");
}

#[test]
fn normal_rtti_r0() {
    eq!("??_R0?P4Vname0@@@8" => "class name0 const volatile __based() `RTTI Type Descriptor'");
}

#[test]
fn derelict1() {
    eq!("??1?$name0@PEAXV?$name1@PEAX$1??$name2@PEAX@@YAXPEAX@Z$1?name3@@YAX0@Z$01@@$0?0$1??$name4@PEAX@@YAHPEAX0@Z$01@@QEAA@XZ" =>
        "public: __cdecl name0<void *, class name1<void *, &void __cdecl name2<void * >(void * ), &void __cdecl name3(void * ), 2>, -1, &int __cdecl name4<void * >(void *, void * ), 2>::~name0<void *, class name1<void *, &void __cdecl name2<void * >(void * ), &void __cdecl name3(void * ), 2>, -1, &int __cdecl name4<void * >(void *, void * ), 2>(void) ");
}

#[test]
fn scope_with_interface() {
    eq!("?name0@?Iname1@name2@@UEAA?AW4name3@@XZ" => "public: virtual enum name3 __cdecl name2[::name1]::name0(void) ");
}

#[test]
fn derelict3() {
    eq!("??$?8GU?$name0@G@name1@@V?$name2@G@1@@name1@@YA_NAEBV?$name3@GU?$name0@G@name1@@V?$name2@G@2@Vname4@@@0@0@Z" =>
        "bool __cdecl name1::operator==<unsigned short, struct name1::name0<unsigned short>, class name1::name2<unsigned short> >(class name1::name3<unsigned short, struct name1::name0<unsigned short>, class name1::name2<unsigned short>, class name4> const &, class name1::name3<unsigned short, struct name1::name0<unsigned short>, class name1::name2<unsigned short>, class name4> const & )");
}

#[test]
fn derelict4() {
    eq!("???__E?name0@name1@<name2>@@$$Q2_NA@@YMXXZ@?A0x3d49b2d0@@$$FYMXXZ" =>
        "void __clrcall `dynamic initializer for 'public: static bool <name2>::name1::name0''(void)");
}

#[test]
fn derelict4related() {
    eq!("???__E??_7name0@@6B@@@YMXXZ@?A0xc2524ebc@@$$FYMXXZ" =>
        "void __clrcall `dynamic initializer for 'const name0::`vftable'''(void)");
}

//Manufactured
#[test]
fn derelict4related_part1() {
    eq!("??__E??_7name0@@6B@@@YMXXZ" => "void __clrcall `dynamic initializer for 'const name0::`vftable'''(void)");
}

#[test]
fn derelict4related_diff1() {
    eq!("?name0@@3QAY01$$CBEA" => "unsigned char const (* name0)[2]");
}

#[test]
fn derelict4related_diff1other() {
    eq!("?name0@@3PEAY01EEA" => "unsigned char (* name0)[2]");
}

#[test]
fn derelict20140818() {
    eq!("??_7?$name0@V?$name1@PAVname2@name3@@@name4@@$0A@V?$name5@$1?name6@?$name7@PAVname2@name3@@@name8@name4@@SGPAUname9@4@XZ@2@@name4@@6Bname9@1@@" =>
        "const name4::name0<class name4::name1<class name3::name2 *>, 0, class name4::name5<&public: static struct name4::name9 * __stdcall name4::name8::name7<class name3::name2 *>::name6(void)> >::`vftable'{for `name4::name9'}");
}

//Manufactured (modification)
#[test]
fn std_nullptr_arg() {
    eq!("?fn@@YAH$$T@Z" => "int __cdecl fn(std::nullptr_t)");
}

//std::nullptr_t
#[test]
fn std_nullptr_arg_real() {
    eq!("??$?9$$A6A_NABW4name0@name1@@@Z@name2@@YA_NABV?$name3@$$A6A_NABW4name0@name1@@@Z@0@$$T@Z" =>
        "bool __cdecl name2::operator!=<bool __cdecl(enum name1::name0 const &)>(class name2::name3<bool __cdecl(enum name1::name0 const &)> const &, std::nullptr_t)");
}

#[test]
fn basic_types_c() {
    eq!("?Name@@3CA" => "signed char Name");
}

#[test]
fn basic_types_d() {
    eq!("?Name@@3DA" => "char Name");
}

#[test]
fn basic_types_e() {
    eq!("?Name@@3EA" => "unsigned char Name");
}

#[test]
fn basic_types_f() {
    eq!("?Name@@3FA" => "short Name");
}

#[test]
fn basic_types_g() {
    eq!("?Name@@3GA" => "unsigned short Name");
}

#[test]
fn basic_types_h() {
    eq!("?Name@@3HA" => "int Name");
}

#[test]
fn basic_types_i() {
    eq!("?Name@@3IA" => "unsigned int Name");
}

#[test]
fn basic_types_j() {
    eq!("?Name@@3JA" => "long Name");
}

#[test]
fn basic_types_k() {
    eq!("?Name@@3KA" => "unsigned long Name");
}

#[test]
fn basic_types_m() {
    eq!("?Name@@3MA" => "float Name");
}

#[test]
fn basic_types_n() {
    eq!("?Name@@3NA" => "double Name");
}

#[test]
fn basic_types_o() {
    eq!("?Name@@3OA" => "long double Name");
}

#[test]
fn extended_types_d() {
    eq!("?Name@@3_DA" => "__int8 Name");
}

#[test]
fn extended_types_e() {
    eq!("?Name@@3_EA" => "unsigned __int8 Name");
}

#[test]
fn extended_types_f() {
    eq!("?Name@@3_FA" => "__int16 Name");
}

#[test]
fn extended_types_g() {
    eq!("?Name@@3_GA" => "unsigned __int16 Name");
}

#[test]
fn extended_types_h() {
    eq!("?Name@@3_HA" => "__int32 Name");
}

#[test]
fn extended_types_i() {
    eq!("?Name@@3_IA" => "unsigned __int32 Name");
}

#[test]
fn extended_types_j() {
    eq!("?Name@@3_JA" => "__int64 Name");
}

#[test]
fn extended_types_k() {
    eq!("?Name@@3_KA" => "unsigned __int64 Name");
}

#[test]
fn extended_types_l() {
    eq!("?Name@@3_LA" => "__int128 Name");
}

#[test]
fn extended_types_m() {
    eq!("?Name@@3_MA" => "unsigned __int128 Name");
}

#[test]
fn extended_types_n() {
    eq!("?Name@@3_NA" => "bool Name");
}

#[test]
fn extended_types_p() {
    eq!("?Name@@3_PA" => "UNKNOWN Name");
}

#[test]
fn extended_types_q() {
    eq!("?Name@@3_QA" => "char8_t Name");
}

#[test]
fn extended_types_r() {
    eq!("?Name@@3_RA" => "<unknown> Name");
}

#[test]
fn extended_types_s() {
    eq!("?Name@@3_SA" => "char16_t Name");
}

#[test]
fn extended_types_t() {
    eq!("?Name@@3_TA" => "UNKNOWN Name");
}

#[test]
fn extended_types_u() {
    eq!("?Name@@3_UA" => "char32_t Name");
}

#[test]
fn extended_types_v() {
    eq!("?Name@@3_VA" => "UNKNOWN Name");
}

#[test]
fn extended_types_w() {
    eq!("?Name@@3_WA" => "wchar_t Name");
}

#[test]
fn _w64prefix1() {
    eq!("?Name@@3_$HA" => "__w64 int Name");
}

#[test]
fn _w64prefix2() {
    eq!("?Name@@3_$_$HA" => "__w64 __w64 int Name");
}

#[test]
fn _w64prefix3() {
    eq!("?Name@@3_$_$PEB_$HA" => "__w64 __w64 __w64 int const * Name");
}

#[test]
fn _w64prefix4() {
    eq!("?Name@@3_$_$PEBPEB_$HA" => "__w64 __w64 __w64 int const * const * Name");
}

#[test]
fn _w64prefix5() {
    eq!("?FnName@@YA_$PEB_$H_$_$PEB_$D@Z" =>
        "__w64 __w64 int const * __cdecl FnName(__w64 __w64 __w64 char const * )");
}

#[test]
fn complex_types_t() {
    eq!("?VarName@SpaceName@@3TTypeName@TypeSpace@@FEIA" =>
        "union TypeSpace::TypeName __unaligned __restrict SpaceName::VarName");
}

#[test]
fn complex_types_u() {
    eq!("?VarName@SpaceName@@3UTypeName@TypeSpace@@FEIA" =>
        "struct TypeSpace::TypeName __unaligned __restrict SpaceName::VarName");
}

#[test]
fn complex_types_v() {
    eq!("?VarName@SpaceName@@3VTypeName@TypeSpace@@FEIA" =>
        "class TypeSpace::TypeName __unaligned __restrict SpaceName::VarName");
}

#[test]
fn complex_types_l() {
    //Seems that L is a complex type
    eq!("?VarName@SpaceName@@3LTypeName@TypeSpace@@FEIA" =>
        "TypeSpace::TypeName __unaligned __restrict SpaceName::VarName");
}

//The coclass and cointerface symbols were all hand-mangled, which were then input to undname for truth
//Could not find any in the wild.  Also, have yet to create C source that would
// have coclass or cointerface types.

#[test]
fn complex_types_y() {
    eq!("?VarName@SpaceName@@3_YTypeName@TypeSpace@@FEIA" =>
        "cointerface TypeSpace::TypeName __unaligned __restrict SpaceName::VarName");
}

#[test]
fn complex_types_x() {
    eq!("?VarName@SpaceName@@3_XTypeName@TypeSpace@@FEIA" =>
        "coclass TypeSpace::TypeName __unaligned __restrict SpaceName::VarName");
}

#[test]
fn complex_types_yparam() {
    eq!("?FnName@@YAYRet@@YParam@@@Z" => "cointerface Ret __cdecl FnName(cointerface Param)");
}

#[test]
fn complex_types_xparam() {
    eq!("?FnName@@YA_XRet@@_XParam@@@Z" => "coclass Ret __cdecl FnName(coclass Param)");
}

#[test]
fn modifier_types_p() {
    eq!("?FnName@@YAXPAH@Z" => "void __cdecl FnName(int *)");
}

#[test]
fn modifier_types_q() {
    eq!("?FnName@@YAXQAH@Z" => "void __cdecl FnName(int * const)");
}

#[test]
fn modifier_types_r() {
    eq!("?FnName@@YAXRAH@Z" => "void __cdecl FnName(int * volatile)");
}

#[test]
fn modifier_types_s() {
    eq!("?FnName@@YAXSAH@Z" => "void __cdecl FnName(int * const volatile)");
}

#[test]
fn modifier_types_a() {
    eq!("?FnName@@YAXAAH@Z" => "void __cdecl FnName(int &)");
}

#[test]
fn modifier_types_b() {
    eq!("?FnName@@YAXBAH@Z" => "void __cdecl FnName(int & volatile)");
}

#[test]
fn modifier_types_p_with_modifier_d() {
    eq!("?FnName@@YAXPDH@Z" => "void __cdecl FnName(int const volatile *)");
}

#[test]
fn modifier_types_s_with_modifier_d() {
    eq!("?FnName@@YAXSDH@Z" => "void __cdecl FnName(int const volatile * const volatile)");
}

#[test]
fn modifier_types_b_with_modifier_d() {
    eq!("?FnName@@YAXBDH@Z" => "void __cdecl FnName(int const volatile & volatile)");
}

#[test]
fn cvmodifiers_b() {
    eq!("?VarName@@3HB" => "int const VarName");
}

#[test]
fn cvmodifiers_c() {
    eq!("?VarName@@3HC" => "int volatile VarName");
}

#[test]
fn cvmodifiers_d() {
    eq!("?VarName@@3HD" => "int const volatile VarName");
}

#[test]
fn cvmodifiers_b_on_p() {
    eq!("?VarName@@3PBHA" => "int const * VarName");
}

#[test]
fn cvmodifiers_b_on_p_to_b_on_h() {
    eq!("?VarName@@3PBHB" => "int const * const VarName");
}

#[test]
fn cvmodifiers_b_on_q() {
    eq!("?VarName@@3QBHA" => "int const * VarName");
}

#[test]
fn cvmodifiers_c_on_p_to_c_on_h() {
    eq!("?VarName@@3PCHC" => "int volatile * volatile VarName");
}

#[test]
fn cvmodifiers_d_on_p_to_d_on_h() {
    eq!("?VarName@@3PDHD" => "int const volatile * const volatile VarName");
}

#[test]
fn cvmodifiers_a_on_p_to_c_on_h() {
    eq!("?VarName@@3PCHA" => "int volatile * VarName");
}

#[test]
fn cvmodifiers_a_on_p_to_d_on_h() {
    eq!("?VarName@@3PDHA" => "int const volatile * VarName");
}

#[test]
fn cvmodifiers_c_on_p_to_a_on_h() {
    eq!("?VarName@@3PAHC" => "int * volatile VarName");
}

#[test]
fn cvmodifiers_d_on_p_to_a_on_h() {
    eq!("?VarName@@3PAHD" => "int * const volatile VarName");
}

#[test]
fn cvmodifiers_a_on_q_to_arrayof_a_on_h() {
    eq!("?VarName@@3QAY01HA" => "int (* VarName)[2]");
}

#[test]
fn cvmodifiers_modified_a() {
    //A
    eq!("?VarName@VarSpace@@3PEAHA" => "int * VarSpace::VarName");
}

//B, J block
#[test]
fn cvmodifiers_modified_b() {
    eq!("?VarName@VarSpace@@3PEBHA" => "int const * VarSpace::VarName");
}

#[test]
fn cvmodifiers_modified_j() {
    eq!("?VarName@VarSpace@@3PEJHA" => "int const * VarSpace::VarName");
}

//C, G, K block
#[test]
fn cvmodifiers_modified_c() {
    eq!("?VarName@VarSpace@@3PECHA" => "int volatile * VarSpace::VarName");
}

#[test]
fn cvmodifiers_modified_k() {
    eq!("?VarName@VarSpace@@3PEKHA" => "int volatile * VarSpace::VarName");
}

//D, H, L block
#[test]
fn cvmodifiers_modified_d() {
    eq!("?VarName@VarSpace@@3PEDHA" => "int const volatile * VarSpace::VarName");
}

#[test]
fn cvmodifiers_modified_l() {
    eq!("?VarName@VarSpace@@3PELHA" => "int const volatile * VarSpace::VarName");
}

//M, N, O, P block
#[test]
fn cvmodifiers_modified_m() {
    eq!("?VarName@VarSpace@@3PEM0HA" => "int __based(void) * VarSpace::VarName");
}

#[test]
fn cvmodifiers_modified_n() {
    eq!("?VarName@VarSpace@@3PEN0HA" => "int const __based(void) * VarSpace::VarName");
}

#[test]
fn cvmodifiers_modified_o() {
    eq!("?VarName@VarSpace@@3PEO0HA" => "int volatile __based(void) * VarSpace::VarName");
}

#[test]
fn cvmodifiers_modified_p() {
    eq!("?VarName@VarSpace@@3PEP0HA" => "int const volatile __based(void) * VarSpace::VarName");
}

//Q, U, Y block
#[test]
fn cvmodifiers_modified_q() {
    eq!("?VarName@VarSpace@@3PEQClassName@@HA" => "int ClassName::* VarSpace::VarName");
}

#[test]
fn cvmodifiers_modified_u() {
    eq!("?VarName@VarSpace@@3PEUClassName@@HA" => "int ClassName::* VarSpace::VarName");
}

#[test]
fn cvmodifiers_modified_y() {
    eq!("?VarName@VarSpace@@3PEYClassName@@HA" => "int ClassName::* VarSpace::VarName");
}

//R, V, Z block
#[test]
fn cvmodifiers_modified_r() {
    eq!("?VarName@VarSpace@@3PERClassName@@HA" => "int const ClassName::* VarSpace::VarName");
}

#[test]
fn cvmodifiers_modified_v() {
    eq!("?VarName@VarSpace@@3PEVClassName@@HA" => "int const ClassName::* VarSpace::VarName");
}

#[test]
fn cvmodifiers_modified_z() {
    eq!("?VarName@VarSpace@@3PEZClassName@@HA" => "int const ClassName::* VarSpace::VarName");
}

//S, W, 0 block
#[test]
fn cvmodifiers_modified_s() {
    eq!("?VarName@VarSpace@@3PESClassName@@HA" => "int volatile ClassName::* VarSpace::VarName");
}

#[test]
fn cvmodifiers_modified_w() {
    eq!("?VarName@VarSpace@@3PEWClassName@@HA" => "int volatile ClassName::* VarSpace::VarName");
}

#[test]
fn cvmodifiers_modified0() {
    eq!("?VarName@VarSpace@@3PE0ClassName@@HA" => "int volatile ClassName::* VarSpace::VarName");
}

//T, X, 1 block
#[test]
fn cvmodifiers_modified_t() {
    eq!("?VarName@VarSpace@@3PETClassName@@HA" => "int const volatile ClassName::* VarSpace::VarName");
}

#[test]
fn cvmodifiers_modified_x() {
    eq!("?VarName@VarSpace@@3PEXClassName@@HA" => "int const volatile ClassName::* VarSpace::VarName");
}

#[test]
fn cvmodifiers_modified1() {
    eq!("?VarName@VarSpace@@3PE1ClassName@@HA" => "int const volatile ClassName::* VarSpace::VarName");
}

//2, 3, 4 block
#[test]
fn cvmodifiers_modified2() {
    eq!("?VarName@VarSpace@@3PE2ClassName@@0HA" => "int __based(void) ClassName::* VarSpace::VarName");
}

#[test]
fn cvmodifiers_modified3() {
    eq!("?VarName@VarSpace@@3PE3ClassName@@0HA" => "int const __based(void) ClassName::* VarSpace::VarName");
}

#[test]
fn cvmodifiers_modified4() {
    eq!("?VarName@VarSpace@@3PE4ClassName@@0HA" => "int volatile __based(void) ClassName::* VarSpace::VarName");
}

//5 and variation 0 on __based()
#[test]
fn cvmodifiers_modified5_0() {
    eq!("?VarName@VarSpace@@3PE5ClassName@@0HA" => "int const volatile __based(void) ClassName::* VarSpace::VarName");
}

//5 and variation 1 on __based()
#[test]
fn cvmodifiers_modified5_1() {
    eq!("?VarName@VarSpace@@3PE5ClassName@@1HA" => "int const volatile __based() ClassName::* VarSpace::VarName");
}

//5 and variation 2 on __based()
#[test]
fn cvmodifiers_modified5_2() {
    eq!("?VarName@VarSpace@@3PE5ClassName@@2BasedPointer@BasedSpace@@HA" =>
        "int const volatile __based(BasedSpace::BasedPointer) ClassName::* VarSpace::VarName");
}

//5 and variation 3 on __based()
#[test]
fn cvmodifiers_modified5_3() {
    eq!("?VarName@VarSpace@@3PE5ClassName@@3HA" => "int const volatile __based() ClassName::* VarSpace::VarName");
}

//5 and variation 4 on __based()
#[test]
fn cvmodifiers_modified5_4() {
    eq!("?VarName@VarSpace@@3PE5ClassName@@4HA" => "int const volatile __based() ClassName::* VarSpace::VarName");
}

#[test]
fn cvmodifiers_modified5_5() {
    eq!("?VarName@VarSpace@@3PE5ClassName@@5HA" => "int");
}

//6, 7, 8, 9, _A, _B, _C, _D block
#[test]
fn cvmodifiers_modified6() {
    eq!("?VarName@@3P6AHH@ZEA" => "int (__cdecl* VarName)(int)");
}

#[test]
fn cvmodifiers_modified7() {
    eq!("?VarName@@3P7AHH@ZEA" => "int (__cdecl* VarName)(int)");
}

#[test]
fn cvmodifiers_modified8() {
    eq!("?VarName@@3P8ClassName@@EDAHXZED" =>
        "int (__cdecl ClassName::*const volatile VarName)(void)const volatile ");
}

#[test]
fn cvmodifiers_modified9_e() {
    eq!("?VarName@@3P9ClassName@@EDAHXZED" =>
        "int (__cdecl ClassName::*const volatile VarName)(void)const volatile ");
}

#[test]
fn cvmodifiers_modified9() {
    //Has trailing space
    eq!("?VarName@@3P9ClassName@@DAHXZED" =>
        "int (__cdecl ClassName::*const volatile VarName)(void)const volatile ");
}

#[test]
fn cvmodifiers_modified_aa() {
    eq!("?VarName@@3P_A0AHH@ZEA" => "int (__cdecl __based(void) * VarName)(int)");
}

#[test]
fn cvmodifiers_modified_ab() {
    eq!("?VarName@@3P_B0AHH@ZEA" => "int (__cdecl __based(void) * VarName)(int)");
}

#[test]
fn cvmodifiers_modified_ac() {
    eq!("?VarName@@3P_CClassName@@D0AHH@ZEA" =>
        "int (__cdecl __based(void) ClassName::* VarName)(int)const volatile ");
}

#[test]
fn cvmodifiers_modified_ad() {
    eq!("?VarName@@3P_DClassName@@D0AHH@ZEA" =>
        "int (__cdecl __based(void) ClassName::* VarName)(int)const volatile ");
}

#[test]
fn cvmodifiers_modified_d_ed() {
    eq!("?VarName@@3P_DClassName@@D0AHH@ZED" =>
        "int (__cdecl __based(void) ClassName::*const volatile VarName)(int)const volatile ");
}

//6 with const volatile 'ED'
#[test]
fn cvmodifiers_modified6_ed() {
    eq!("?VarName@@3P6AHH@ZED" => "int (__cdecl*const volatile VarName)(int)");
}

//unaligned, restrict, ptr64, const, volatile
#[test]
fn cvmodifiers() {
    eq!("?VarName@@3PEIFDHEIFD" =>
        "int const volatile __unaligned * __restrict const volatile __unaligned __restrict VarName");
}

//EI order matters
#[test]
fn cvmodifiers_modified_eiorder() {
    eq!("?VarName@@3PEIAHA" => "int * __restrict VarName");
}

//IE order matters
#[test]
fn cvmodifiers_modified_ieorder() {
    eq!("?VarName@@3PIEAHA" => "int * __restrict VarName");
}

//many Es and Is with order
#[test]
fn cvmodifiers_modified_eeeiii() {
    eq!("?VarName@@3PEEEIIIEEEAHA" => "int * __restrict __restrict __restrict VarName");
}

#[test]
fn cvmodifiers_more_based0() {
    eq!("?Var@@3PBHA" => "int const * Var");
}

#[test]
fn cvmodifiers_more_based1() {
    eq!("?Var@@3PAHN5" => "int * ");
}

#[test]
fn cvmodifiers_more_based2() {
    eq!("?Var@@3HN5" => "int ");
}

#[test]
fn cvmodifiers_more_based2a() {
    eq!("?Var@@3HN0" => "int const __based(void) Var");
}

#[test]
fn cvmodifiers_more_based3() {
    eq!("?Var@@3PP5HA" => "int");
}

#[test]
fn cvmodifiers_more_based3a() {
    eq!("?Var@@3PP0HA" => "int const volatile __based(void) * Var");
}

#[test]
fn cvmodifiers_more_based3b() {
    eq!("?Var@@3PP5Y01HA" => "int [2]");
}

#[test]
fn cvmodifiers_more_based4() {
    eq!("?Var@@3PP0Y01HP0" => "int (const volatile __based(void) * const volatile __based(void) Var)[2]");
}

#[test]
fn cvmodifiers_more_based5() {
    eq!("?Var@@3PP0Y01HP5" => "int (const volatile __based(void) * )[2]");
}

#[test]
fn cvmodifiers_more_based6() {
    eq!("?Var@@3PP5Y01HP0" => "int [2]");
}

#[test]
fn cvmodifiers_more_based7() {
    eq!("?Var@@3P_CClass@@D0AHD@ZEP5" =>
        "int (__cdecl __based(void) Class::*)(char)const volatile ");
}

#[test]
fn cvmodifiers_more_based8() {
    eq!("?fn@@YAHPEIFN5H@Z" => "int __cdecl fn(int)");
}

#[test]
fn cvmodifiers_more_based9() {
    eq!("?fn@@YAHSEIFN5H@Z" => "int __cdecl fn(int)");
}

// Test C-V Modifiers __based() variation.
// TODO:
// More investigation required.  For __based(), code 5 is supposed to remove the __based() property (according to standard document).
// For undname, the code 5 seems to work for the `RTTI Type Descriptor' below, but it also removes the const volatile.
// However, for undname, the __based() is not removed for the non-RTTI object.  This is just data gathering phase of the investigation.
// We have not yet implemented any code in MDMang to remove the __based() property, as we do not have enough understanding of the
// cases involved.
// Upon reading the bible document, I found more under "Function" talking about __based.  I've implemented a test of that below.  I was
// able to create a __based(void) function using the underscore (_) method.  I then changed the __based code to 5, and I see that
// it removed this: "__cdecl __based(void)"  The underscore code is not implemented--the "Function" section needs to be codified better.
// I tried the underscore on a simple data type below as well.  See the "truth" presented there.  The underscore seemed to have no effect,
// even on a pointer.
#[test]
fn cvmodifiers_based5_variation_aaa1() {
    eq!("?Var@@3_OBHN0" => "int const __based(void) Var[]");
}

#[test]
fn cvmodifiers_based5_variation_aaa2() {
    eq!("?Var@@3_OBHN5" => "int []");
}

#[test]
fn cvmodifiers_based5_variation_aa1() {
    eq!("?Var@@3PAHN0" => "int * const __based(void) Var");
}

#[test]
fn cvmodifiers_based5_variation_aa2() {
    eq!("?Var@@3PAHN5" => "int * ");
}

#[test]
fn cvmodifiers_based5_variation_ab() {
    eq!("?Var@@3HN5" => "int ");
}

#[test]
fn cvmodifiers_based5_variation_ac() {
    eq!("?Var@@3PP5HA" => "int");
}

#[test]
fn cvmodifiers_based5_variation_ad() {
    eq!("?Var@@3PEP5HEP0" => "int");
}

#[test]
fn cvmodifiers_based5_variation_ae() {
    eq!("?Var@@3QP0Y01HP0" => "int (const volatile __based(void) * const volatile __based(void) Var)[2]");
}

#[test]
fn cvmodifiers_based5_variation_af() {
    eq!("?Var@@3QP0Y01HP5" => "int (const volatile __based(void) * )[2]");
}

#[test]
fn cvmodifiers_based5_variation_ag() {
    eq!("?Var@@3QP5Y01HP0" => "int [2]");
}

#[test]
fn cvmodifiers_based5_variation_ah() {
    eq!("?Var@@3P_CClass@@D0AHD@ZEP0" =>
        "int (__cdecl __based(void) Class::*const volatile __based(void) Var)(char)const volatile ");
}

#[test]
fn cvmodifiers_based5_variation_ai() {
    eq!("?Var@@3P_CClass@@D0AHD@ZEP5" => "int (__cdecl __based(void) Class::*)(char)const volatile ");
}

//This test seems to dictate that a function pointer should be elaborated internal to CVMod, where the based5 will eliminate all of the function context.
//  It also seems to indicate that the "int" portion would be the referred-to type and the rest of the function spec would be part of the the function info.
//  Other information at one time, led me to believe that the return type of a function is special... need to rekinkdle those thoughts, but think related to nested
//  functions, such as function returning a function pointer..

#[test]
fn cvmodifiers_based5_variation_aj() {
    eq!("?Var@@3P_CClass@@D5AHD@ZEP0" => "int ");
}

#[test]
fn cvmodifiers_based5_variation_ak() {
    eq!("?Var@@3PEP0HEP0" => "int const volatile __based(void) * const volatile __based(void) Var");
}

#[test]
fn cvmodifiers_based5_variation_al() {
    eq!("?Var@@3PEP0HEP5" => "int const volatile __based(void) * ");
}

#[test]
fn cvmodifiers_based5_variation_am() {
    eq!("?Var@@3PEP5HEP0" => "int");
}

#[test]
fn cvmodifiers_based5_variation_an() {
    eq!("?Var@@3PEBHN5" => "int const * ");
}

#[test]
fn cvmodifiers_based5_variation_ao() {
    eq!("??_R0?PAVname0@@@8" => "class name0 const volatile __based() `RTTI Type Descriptor'");
}

#[test]
fn cvmodifiers_based5_variation_ap() {
    eq!("??_R0?P5Vname0@@@8" => "class name0 `RTTI Type Descriptor'");
}

//Manufactured.
#[test]
fn cvmodifiers_based5_variation_aq() {
    //20160615 correction
    eq!("?VarName@VarSpace@@3PE5ClassName@@5HA" => "int");
}

//Manufactured.  Added as counterpoint to the above test.
#[test]
fn cvmodifiers_based5_variation_aq_0() {
    eq!("?VarName@VarSpace@@3PE5ClassName@@0HA" =>
        "int const volatile __based(void) ClassName::* VarSpace::VarName");
}

#[test]
fn cvmodifiers_based5_variation_ar() {
    eq!("?FnName@@YAXPAH@Z" => "void __cdecl FnName(int *)");
}

#[test]
fn cvmodifiers_based5_variation_au() {
    eq!("?Var@@3HA" => "int Var");
}

#[test]
fn cvmodifiers_based5_variation_aw() {
    eq!("?Var@@3HN5" => "int ");
}

#[test]
fn cvmodifiers_based5_variation_ax() {
    eq!("?Var@@3PEBHN0" => "int const * const __based(void) Var");
}

#[test]
fn cvmodifiers_based5_variation_ay() {
    eq!("?Var@@3PEBHN5" => "int const * ");
}

#[test]
fn cvmodifiers_based5_variation_az() {
    eq!("?Var@@3PEP0HEP0" => "int const volatile __based(void) * const volatile __based(void) Var");
}

#[test]
fn cvmodifiers_based5_variation_ba() {
    eq!("?Var@@3PEP0HEP5" => "int const volatile __based(void) * ");
}

#[test]
fn cvmodifiers_based5_variation_bb() {
    eq!("?Var@@3PEP5HEP0" => "int");
}

#[test]
fn cvmodifiers_based5_variation_bc() {
    eq!("?Var@@3P_CClass@@D0AHD@ZEP0" =>
        "int (__cdecl __based(void) Class::*const volatile __based(void) Var)(char)const volatile ");
}

#[test]
fn cvmodifiers_based5_variation_bd() {
    eq!("?Var@@3P_CClass@@D0AHD@ZEP5" => "int (__cdecl __based(void) Class::*)(char)const volatile ");
}

#[test]
fn cvmodifiers_based5_variation_be() {
    eq!("?Var@@3P_CClass@@D5AHD@ZEP0" => "int ");
}

#[test]
fn enums_chartype() {
    eq!("?enumvar@@3W0enumname@enumspace@@A" => "enum char enumspace::enumname enumvar");
}

#[test]
fn enums_unsignedchartype() {
    eq!("?enumvar@@3W1enumname@enumspace@@A" => "enum unsigned char enumspace::enumname enumvar");
}

#[test]
fn enums_shorttype() {
    eq!("?enumvar@@3W2enumname@enumspace@@A" => "enum short enumspace::enumname enumvar");
}

#[test]
fn enums_unsignedshorttype() {
    eq!("?enumvar@@3W3enumname@enumspace@@A" => "enum unsigned short enumspace::enumname enumvar");
}

#[test]
fn enums_inttype() {
    eq!("?enumvar@@3W4enumname@enumspace@@A" => "enum enumspace::enumname enumvar");
}

#[test]
fn enums_unsignedinttype() {
    eq!("?enumvar@@3W5enumname@enumspace@@A" => "enum unsigned int enumspace::enumname enumvar");
}

#[test]
fn enums_longtype() {
    eq!("?enumvar@@3W6enumname@enumspace@@A" => "enum long enumspace::enumname enumvar");
}

#[test]
fn enums_unsignedlongtype() {
    eq!("?enumvar@@3W7enumname@enumspace@@A" => "enum unsigned long enumspace::enumname enumvar");
}

#[test]
fn access_levels_metatype8() {
    eq!("?Var@Namespace@@8" => "Namespace::Var");
}

#[test]
fn access_levels_guard5() {
    eq!("?Var@Namespace@@51" => "Namespace::Var{2}'");
}

#[test]
fn access_levels_1a() {
    eq!("?name0@name1@@1Uname2@@A" => "protected: static struct name2 name1::name0");
}

#[test]
fn access_levels_1b() {
    eq!("?name0@name1@@1PAUname2@1@A" => "protected: static struct name1::name2 * name1::name0");
}

// O, P (don't have a P yet) block
#[test]
fn access_levels_oa() {
    eq!("?name0@name1@@O7EAAKXZ" =>
        "[thunk]:protected: virtual unsigned long __cdecl name1::name0`adjustor{8}' (void) ");
}

#[test]
fn access_levels_ob() {
    eq!("?name0@name1@@OBA@EAAKXZ" =>
        "[thunk]:protected: virtual unsigned long __cdecl name1::name0`adjustor{16}' (void) ");
}

// W, X (don't have an X yet) block
#[test]
fn access_levels_wa() {
    eq!("?name0@name1@@W7EAAJAEBUname2@@@Z" =>
        "[thunk]:public: virtual long __cdecl name1::name0`adjustor{8}' (struct name2 const & ) ");
}

#[test]
fn access_levels_wb() {
    eq!("?name0@name1@@W7EAAJXZ" => "[thunk]:public: virtual long __cdecl name1::name0`adjustor{8}' (void) ");
}

// $0, $1 (don't have a $1 yet) block
#[test]
fn access_levels_dollar0() {
    eq!("?name0@name1@@$0PPPPPPPM@A@EAAKAEAKAEAPEAG@Z" =>
        "[thunk]:private: virtual unsigned long __cdecl name1::name0`vtordisp{4294967292, 0}' (unsigned long &, unsigned short * & ) ");
}

// $2, $3 (don't have a $3 yet) block
#[test]
fn access_levels_dollar2a() {
    eq!("?name0@name1@@$2PPPPPPPM@7EAAJXZ" =>
        "[thunk]:protected: virtual long __cdecl name1::name0`vtordisp{4294967292, 8}' (void) ");
}

#[test]
fn access_levels_dollar2b() {
    eq!("?name0@name1@@$2PPPPPPPM@BI@EAAJXZ" =>
        "[thunk]:protected: virtual long __cdecl name1::name0`vtordisp{4294967292, 24}' (void) ");
}

//$R2, real a
#[test]
fn access_levels_dollar_r2a() {
    eq!("?name0@name1@name2@@$R2BAA@7PPPPPPPM@BAI@EAAXXZ" =>
        "[thunk]:protected: virtual void __cdecl name2::name1::name0`vtordispex{256, 8, 4294967292, 264}' (void) ");
}

//$R2, real b
#[test]
fn access_levels_dollar_r2b() {
    eq!("?name0@name1@name2@@$R2BI@7PPPPPPPM@BAI@EAAXXZ" =>
        "[thunk]:protected: virtual void __cdecl name2::name1::name0`vtordispex{24, 8, 4294967292, 264}' (void) ");
}

//$R3, manufactured from $R2
#[test]
fn access_levels_dollar_r3() {
    eq!("?name0@name1@name2@@$R3BI@7PPPPPPPM@BAI@EAAXXZ" =>
        "[thunk]:protected: virtual void __cdecl name2::name1::name0`vtordispex{24, 8, 4294967292, 264}' (void) ");
}

//$R0, manufactured from $R2
#[test]
fn access_levels_dollar_r0() {
    eq!("?name0@name1@name2@@$R0BI@7PPPPPPPM@BAI@EAAXXZ" =>
        "[thunk]:private: virtual void __cdecl name2::name1::name0`vtordispex{24, 8, 4294967292, 264}' (void) ");
}

//$R1, manufactured from $R2
#[test]
fn access_levels_dollar_r1() {
    eq!("?name0@name1@name2@@$R1BI@7PPPPPPPM@BAI@EAAXXZ" =>
        "[thunk]:private: virtual void __cdecl name2::name1::name0`vtordispex{24, 8, 4294967292, 264}' (void) ");
}

//$R4, manufactured from $R2
#[test]
fn access_levels_dollar_r4() {
    eq!("?name0@name1@name2@@$R4BI@7PPPPPPPM@BAI@EAAXXZ" =>
        "[thunk]:public: virtual void __cdecl name2::name1::name0`vtordispex{24, 8, 4294967292, 264}' (void) ");
}

//$R5, manufactured from $R2
#[test]
fn access_levels_dollar_r5() {
    eq!("?name0@name1@name2@@$R5BI@7PPPPPPPM@BAI@EAAXXZ" =>
        "[thunk]:public: virtual void __cdecl name2::name1::name0`vtordispex{24, 8, 4294967292, 264}' (void) ");
}

//$B, real a
#[test]
fn access_levels_dollar_ba() {
    eq!("??_9name0@@$BBII@AA" => "[thunk]: __cdecl name0::`vcall'{392, {flat}}' }'");
}

//$B, real b
#[test]
fn access_levels_dollar_bb() {
    eq!("??_7?$name0@H$H??_9name1@@$BHI@AAA@@?$name2@Vname1@@@@6B@" =>
        "const name2<class name1>::name0<int, {[thunk]: __cdecl name1::`vcall'{120, {flat}}' }', 0}>::`vftable'");
}

//$B, real c
#[test]
fn access_levels_dollar_bc() {
    eq!("??1?$name0@Uname1@@P81@EAAJXZ$1??_91@$BCA@AA@@QEAA@XZ" =>
        "public: __cdecl name0<struct name1, long (__cdecl name1::*)(void), &[thunk]: __cdecl name1::`vcall'{32, {flat}}' }'>::~name0<struct name1, long (__cdecl name1::*)(void), &[thunk]: __cdecl name1::`vcall'{32, {flat}}' }'>(void) ");
}

//Manufactured from other.
#[test]
fn access_levels_underscore_based0_vtordisp() {
    eq!("?name0@name1@@_$40PPPPPPPM@A@EAAJUname2@@HPEBGPEAPEAGK2KK1PEAEKPEAVname3@@@Z" =>
        "[thunk]:public: virtual long __cdecl __based(void) name1::name0`vtordisp{4294967292, 0}' (struct name2, int, unsigned short const *, unsigned short * *, unsigned long, unsigned short * *, unsigned long, unsigned long, unsigned short const *, unsigned char *, unsigned long, class name3 * ) ");
}

//Manufactured from other.
#[test]
fn access_levels_underscore_based0_vtordispex() {
    eq!("??_9testAccessLevel@@_$R50A@B@C@D@AA@H@HH@" =>
        "[thunk]:public: virtual __cdecl __based(void) testAccessLevel::`vcall'`vtordispex{0, 1, 2, 3}' (int) throw(int, int)");
}

#[test]
fn access_levels_based0_guard() {
    eq!("?Var@Namespace@@51" => "Namespace::Var{2}'");
}

//Manufactured tests that teased out more details.
#[test]
fn access_levels_based0_vftable() {
    eq!("??_7a@b@@6B@" => "const b::a::`vftable'");
}

//Manufactured tests that teased out more details.
#[test]
fn access_levels_based0_vftable_fuzz_for_unprocessed_terminating_at() {
    eq!("??_7a@b@@6Bx@xx@@y@yy@@@" => "const b::a::`vftable'{for `xx::x's `yy::y'}");
}

//Manufactured.  Note that the following work:
//   C, D, K, L, S, T, which are all static member functions
#[test]
fn access_levels_based0_staticmember() {
    eq!("?FnName@@_C0AXPAH@Z" => "private: static void __cdecl __based(void) FnName(int *)");
}

//TODO 20170419: Problem is that I put MDBasedType inside of MDType, but then MDAdjustor extends
//  MDFunctionType, which parses super() (MDFunctionType) after parsing the Adjustor values.  Real
//  solution is likely that all MDType parsing/outputting needs to be in MDTypeInfo (at the opposite
//  end of the spectrum).
//Manufactured.  Note that the following work:
//   G, H, O, P, W, X, which are all adjustor functions (probably non-displaying static)
#[test]
fn access_levels_based_g() {
    eq!("?FnName@@_G0BA@EAAHXZ" => "[thunk]:private: virtual int __cdecl __based(void) FnName`adjustor{16}' (void) ");
}

#[test]
fn simple_template() {
    eq!("?Ti@@3V?$Tc@H@@A" => "class Tc<int> Ti");
}

//Manufactured to show that whole MDTemplateNameAndArgumentsList is a valid name backref.
#[test]
fn template_backref_of_whole_template_as_qual() {
    eq!("?Ti@@3V?$Tc@H@1@A" => "class Tc<int>::Tc<int> Ti");
}

#[test]
fn simple_template_main() {
    eq!("?$Tc@H" => "Tc<int>");
}

#[test]
fn simple_template_main_better() {
    eq!("?$Tc@HH" => "Tc<int, int>");
}

#[test]
fn template_as_template_parameter() {
    eq!("?Ti@@3V?$Tc@V?$Tb@H@@@@A" => "class Tc<class Tb<int> > Ti");
}

//Manufactured 20170512: to test MDTemplateNameAndArguments inside of MDReusableName and gets put into backreference names.
#[test]
fn template_in_reusable_in_qual() {
    eq!("?Var@?I?$templatename@H@1@3HA" => "int templatename<int>[::templatename<int>]::Var");
}

//real symbol: ?#
#[test]
fn special_template_parameters_questionnumber() {
    eq!("??0?$name0@?0Uname1@@@name2@@QEAA@XZ" =>
        "public: __cdecl name2::name0<`template-parameter-1', struct name1>::name0<`template-parameter-1', struct name1>(void) ");
}

//real symbol: $0
#[test]
fn special_template_parameters_dollar0() {
    eq!("??$?0V?$A@_NABW4B@C@@@D@E@@@?$F@V?$G@U?$H@Q6A_NABW4B@C@@@Z$0A@@D@E@@_NABW4B@C@@@D@E@@@E@@QAE@ABV?$F@V?$A@_NABW4B@C@@@D@E@@@1@@Z" =>
        "public: __thiscall E::F<class E::D::G<struct E::D::H<bool (__cdecl*const)(enum C::B const &), 0>, bool, enum C::B const &> >::F<class E::D::G<struct E::D::H<bool (__cdecl*const)(enum C::B const &), 0>, bool, enum C::B const &> ><class E::D::A<bool, enum C::B const &> >(class E::F<class E::D::A<bool, enum C::B const &> > const &)");
}

//real symbol: $1
#[test]
fn special_template_parameters_dollar1() {
    eq!("??0?$name0@Vname1@@$1?name2@@3Uname3@@B$1?name4@@3QBGB@@QEAA@XZ" =>
        "public: __cdecl name0<class name1, &struct name3 const name2, &unsigned short const * const name4>::name0<class name1, &struct name3 const name2, &unsigned short const * const name4>(void) ");
}

//manufactured symbol: $2 Blank (zero) exponent
#[test]
fn special_template_parameters_dollar2_blankzeroexp() {
    eq!("??$F@$2B@@@@QAE@@Z" => "public: __thiscall F<1.e0>()");
}

//manufactured symbol: $2 Simple mantissa and simple exponent
#[test]
fn special_template_parameters_dollar2_simplemant_simpleexp() {
    eq!("??$F@$2B@B@@@QAE@@Z" => "public: __thiscall F<1.e1>()");
}

//manufactured symbol: $2 Bigger more mantissa and simple exponent
#[test]
fn special_template_parameters_dollar2_complexmant_simpleexp() {
    eq!("??$F@$2BB@B@@@QAE@@Z" => "public: __thiscall F<1.7e1>()");
}

//manufactured symbol: $2 Bigger more mantissa and simple exponent
#[test]
fn special_template_parameters_dollar2_negcomplexmant_simpleexp() {
    eq!("??$F@$2?BB@B@@@QAE@@Z" => "public: __thiscall F<-1.7e1>()");
}

//manufactured symbol: $2 Simple mantissa and simple negative exponent
#[test]
fn special_template_parameters_dollar2_simplemant_negexp() {
    eq!("??$F@$2B@?B@@@QAE@@Z" => "public: __thiscall F<1.e-1>()");
}

//manufactured symbol: $2 Simple negative mantissa and simple exponent
#[test]
fn special_template_parameters_dollar2_negmant_simpleexp() {
    eq!("??$F@$2?B@B@@@QAE@@Z" => "public: __thiscall F<-1.e1>()");
}

//manufactured symbol: $2 Simple negative mantissa and simple negative exponent
#[test]
fn special_template_parameters_dollar2_negmant_negexp() {
    eq!("??$F@$2?B@?B@@@QAE@@Z" => "public: __thiscall F<-1.e-1>()");
}

//manufactured symbol: $2 Simple zero mantissa and simple zero exponent
#[test]
fn special_template_parameters_dollar2_simplezeromant_simplezeroexp() {
    eq!("??$F@$2A@A@@@QAE@@Z" => "public: __thiscall F<0.e0>()");
}

//manufactured symbol: $2 Simple zero mantissa and blank (zero) exponent
#[test]
fn special_template_parameters_dollar2_simplezeromant_blankzeroexp() {
    eq!("??$F@$2A@@@@QAE@@Z" => "public: __thiscall F<0.e0>()");
}

//manufactured symbol: $2 Blank (zero) mantissa and simple zero exponent
#[test]
fn special_template_parameters_dollar2_blankzeromant_simplezeroexp() {
    eq!("??$F@$2@A@@@QAE@@Z" => "public: __thiscall F<0.e0>()");
}

//manufactured symbol: $2 Blank (zero) mantissa and blank (zero) exponent
#[test]
fn special_template_parameters_dollar2_blankzeromant_blankzeroexp() {
    eq!("??$F@$2@@@@QAE@@Z" => "public: __thiscall F<0.e0>()");
}

//manufactured symbol: $2 Simple negative zero mantissa and simple negative zero exponent
#[test]
fn special_template_parameters_dollar2_simplenegzeromant_simplenegzeroexp() {
    eq!("??$F@$2?A@?A@@@QAE@@Z" => "public: __thiscall F<-0.e-0>()");
}

//manufactured symbol: $2 Simple negative blank (zero) mantissa and simple negative blank (zero) exponent
#[test]
fn special_template_parameters_dollar2_simplenegblankzeromant_simplenegblankzeroexp() {
    eq!("??$F@$2?@?@@@QAE@@Z" => "public: __thiscall F<-0.e-0>()");
}

//real symbol: $D
#[test]
fn special_template_parameters_dollar_d() {
    eq!("??0?$name0@$D0Uname1@@@name2@@QEAA@XZ" =>
        "public: __cdecl name2::name0<`template-parameter1', struct name1>::name0<`template-parameter1', struct name1>(void) ");
}

//real symbol: $E
#[test]
fn special_template_parameters_dollar_e() {
    eq!("??0?$name0@V?$name1@Vname2@@$E?name3@@3Uname4@@B@@@name5@@QEAA@PEAX@Z" =>
        "public: __cdecl name5::name0<class name1<class name2, struct name4 const name3> >::name0<class name1<class name2, struct name4 const name3> >(void * ) ");
}

//manufactured symbol: $F Simple zero and zero parameters
#[test]
fn special_template_parameters_dollar_f_zero_zero() {
    eq!("??$F@$FA@A@@@QAE@@Z" => "public: __thiscall F<{0, 0}>()");
}

//manufactured symbol: $F Simple negative zero and negative zero parameters
#[test]
fn special_template_parameters_dollar_f_negzero_negzero() {
    eq!("??$F@$F?A@?A@@@QAE@@Z" => "public: __thiscall F<{-0, -0}>()");
}

//manufactured symbol: $G Simple zero, zero, and zero parameters
#[test]
fn special_template_parameters_dollar_f_zero_zero_zero() {
    eq!("??$F@$GA@A@A@@@QAE@@Z" => "public: __thiscall F<{0, 0, 0}>()");
}

//manufactured symbol: $G Simple negative zero, negative zero, and negative zero parameters
#[test]
fn special_template_parameters_dollar_f_negzero_negzero_negzero() {
    eq!("??$F@$G?A@?A@?A@@@QAE@@Z" => "public: __thiscall F<{-0, -0, -0}>()");
}

//real symbol: $H
#[test]
fn special_template_parameters_dollar_h_zero() {
    eq!("??_7?$name0@H$H??_9name1@@$BHI@AA?B@@?$name2@Vname1@@@@6B@" =>
        "const name2<class name1>::name0<int, {[thunk]: __cdecl name1::`vcall'{120, {flat}}' }', -1}>::`vftable'");
}

//manufactured symbol: $H (from $H)
#[test]
fn special_template_parameters_dollar_h_one() {
    eq!("??_7?$name0@H$H??_9name1@@$BHI@AA?B@@?$name2@Vname1@@@@6B@" =>
        "const name2<class name1>::name0<int, {[thunk]: __cdecl name1::`vcall'{120, {flat}}' }', -1}>::`vftable'");
}

//manufactured symbol: $I (from $H)
#[test]
fn special_template_parameters_dollar_h_one_one() {
    eq!("??_7?$name0@H$I??_9name1@@$BHI@AAB@B@@?$name2@Vname1@@@@6B@" =>
        "const name2<class name1>::name0<int, {[thunk]: __cdecl name1::`vcall'{120, {flat}}' }', 1, 1}>::`vftable'");
}

//manufactured symbol: $I (from $H)
#[test]
fn special_template_parameters_dollar_h_negone_negone() {
    eq!("??_7?$name0@H$I??_9name1@@$BHI@AA?B@?B@@?$name2@Vname1@@@@6B@" =>
        "const name2<class name1>::name0<int, {[thunk]: __cdecl name1::`vcall'{120, {flat}}' }', -1, -1}>::`vftable'");
}

//manufactured symbol: $J (from $H)
#[test]
fn special_template_parameters_dollar_j() {
    eq!("??_7?$name0@H$J??_9name1@@$BHI@AAB@B@B@@?$name2@Vname1@@@@6B@" =>
        "const name2<class name1>::name0<int, {[thunk]: __cdecl name1::`vcall'{120, {flat}}' }', 1, 1, 1}>::`vftable'");
}

//manufactured symbol: $J (from $H)
#[test]
fn special_template_parameters_dollar_j_with_negs() {
    eq!("??_7?$name0@H$J??_9name1@@$BHI@AA?B@?B@?B@@?$name2@Vname1@@@@6B@" =>
        "const name2<class name1>::name0<int, {[thunk]: __cdecl name1::`vcall'{120, {flat}}' }', -1, -1, -1}>::`vftable'");

}

//TODO: Are there others, such as $K, $L, $M, $N, $O, $P ???

//real symbol: $Q
#[test]
fn special_template_parameters_dollar_q() {
    eq!("??0?$name0@$Q0Uname1@@@name2@@QEAA@XZ" =>
        "public: __cdecl name2::name0<`non-type-template-parameter1', struct name1>::name0<`non-type-template-parameter1', struct name1>(void) ");
}

//real symbol: $R
#[test]
fn special_template_parameters_dollar_r() {
    eq!("??0?$name0@$Rname1@EAAABAAB@@name2@name3@name4@name5@@$$FQE$AAM@AE$AAV01234@@Z" =>
        "public: __clrcall name5::name4::name3::name2::name0<name1>::name0<name1>(class name5::name4::name3::name2::name0<name1> % ) ");
}

//Manufactured; Keep.
#[test]
fn function_parameter_bqrs_direct_arg_modifiers() {
    //BQRS modifiers are only valid on direct arguments of functions and templates.
    eq!("?main@@YAHHPEAPEADQEAPEADREAPEADSEAPEADAEAPEADBEAPEAD@Z" =>
        "int __cdecl main(int, char * *, char * * const, char * * volatile, char * * const volatile, char * &, char * & volatile)");
}

//Manufactured; Keep.
#[test]
fn function_parameter_bqrs_non_direct_arg_modifiers() {
    //BQRS modifiers are only valid on direct arguments of functions and templates.
    // Cannot have "pointer to reference" or "reference to reference, " so cannot test these non-direct cases.
    eq!("?main@@YAHHPEAPEADPEAQEADPEAREADPEASEAD@Z" => "int __cdecl main(int, char * *, char * *, char * *, char * * )");
}

//Manufactured; Keep.
#[test]
fn template_parameter_bqrs_direct_arg_modifiers() {
    //BQRS modifiers are only valid on direct arguments of functions and templates.
    eq!("?Ti@@3V?$Tc@PEAPEADQEAPEADREAPEADSEAPEADAEAPEADBEAPEAD@@A" =>
        "class Tc<char * *, char * * const, char * * volatile, char * * const volatile, char * &, char * & volatile> Ti");
}

//Manufactured; Keep.
#[test]
fn template_parameter_bqrs_non_direct_arg_modifiers() {
    //BQRS modifiers are only valid on direct arguments of functions and templates.
    // Cannot have "pointer to reference" or "reference to reference, " so cannot test these non-direct cases.
    eq!("?Ti@@3V?$Tc@PEAPEADPEAQEADPEAREADPEASEAD@@A" => "class Tc<char * *, char * *, char * *, char * * > Ti");
}

#[test]
fn parameter_const_function_direct_arg() {
    //When can P, Q:const, R:volatile, S:const volatile be seen in arguments emission?
    // Seems that these are used and stored when Direct Argument (not a referred to type within an argument)
    //  of a function.  TODO: seems that for a modified type in a template, there is an issue--checking this 20140521
    // $$H
    eq!("?main@@$$HYAHHQEAPEAD@Z" => "int __cdecl main(int, char * * const)");
}

#[test]
fn parameter_const_template_direct_arg() {
    //When can P, Q:const, R:volatile, S:const volatile be seen in arguments emission?
    // Seems that these are used and stored when Direct Argument (not a referred to type within an argument)
    //  of a function.  TODO: seems that for a modified type in a template, there is an issue--checking this 20140521
    eq!("??0?$name0@Vname1@@$1?name2@@3Uname3@@B$1?name4@@3QBGB@@QEAA@XZ" =>
        "public: __cdecl name0<class name1, &struct name3 const name2, &unsigned short const * const name4>::name0<class name1, &struct name3 const name2, &unsigned short const * const name4>(void) ");
}

//Manufactured; Keep.
#[test]
fn template_parameter_void() {
    //The "void" argument can be the first in a template arguments list, and still needs an '@' terminator for the list.
    eq!("?Ti@@3V?$Tc@X@@A" => "class Tc<void> Ti");
}

//Manufactured; Keep.
#[test]
fn template_parameter_void_void() {
    //Testing "void" as the first and second arguments of a template.
    eq!("?Ti@@3V?$Tc@XX@@A" => "class Tc<void, void> Ti");
}

//A, B: __cdecl block
#[test]
fn function_calling_conventions_a_cdecl() {
    eq!("?fnii@@YAHH@Z" => "int __cdecl fnii(int)");
}

#[test]
fn function_calling_conventions_b_cdecl() {
    eq!("?fnii@@YBHH@Z" => "int __cdecl fnii(int)");
}

//C, D: __pascal block
#[test]
fn function_calling_conventions_c_pascal() {
    eq!("?fnii@@YCHH@Z" => "int __pascal fnii(int)");
}

#[test]
fn function_calling_conventions_d_pascal() {
    eq!("?fnii@@YDHH@Z" => "int __pascal fnii(int)");
}

//E, F: __thiscall block
#[test]
fn function_calling_conventions_e_thiscall() {
    eq!("?fnii@@YEHH@Z" => "int __thiscall fnii(int)");
}

#[test]
fn function_calling_conventions_f_thiscall() {
    eq!("?fnii@@YFHH@Z" => "int __thiscall fnii(int)");
}

//G, H: __stdcall block
#[test]
fn function_calling_conventions_g_stdcall() {
    eq!("?fnii@@YGHH@Z" => "int __stdcall fnii(int)");
}

#[test]
fn function_calling_conventions_h_stdcall() {
    eq!("?fnii@@YHHH@Z" => "int __stdcall fnii(int)");
}

//I, J: __fastcall block
#[test]
fn function_calling_conventions_i_fastcall() {
    eq!("?fnii@@YIHH@Z" => "int __fastcall fnii(int)");
}

#[test]
fn function_calling_conventions_j_fastcall() {
    eq!("?fnii@@YJHH@Z" => "int __fastcall fnii(int)");
}

//K, L: blank block
#[test]
fn function_calling_conventions_k() {
    eq!("?fnii@@YKHH@Z" => "int fnii(int)");
}

#[test]
fn function_calling_conventions_l() {
    eq!("?fnii@@YLHH@Z" => "int fnii(int)");
}

//M, N: __clrcall block
#[test]
fn function_calling_conventions_m_clrcall() {
    eq!("?fnii@@YMHH@Z" => "int __clrcall fnii(int)");
}

#[test]
fn function_calling_conventions_n_clrcall() {
    eq!("?fnii@@YNHH@Z" => "int __clrcall fnii(int)");
}

//O, P: __eabi block
#[test]
fn function_calling_conventions_o_eabi() {
    eq!("?fnii@@YOHH@Z" => "int __eabi fnii(int)");
}

#[test]
fn function_calling_conventions_p_eabi() {
    eq!("?fnii@@YPHH@Z" => "int __eabi fnii(int)");
}

//Q: __vectorcall block
#[test]
fn function_calling_conventions_q_vectorcall() {
    eq!("?fnii@@YQHH@Z" => "int __vectorcall fnii(int)");
}

#[test]
fn function_throw_a() {
    eq!("?fnii@@YAHH@@" => "int __cdecl fnii(int) throw()");
}

#[test]
fn function_throw_b() {
    eq!("?fnii@@YAHH@HH@" => "int __cdecl fnii(int) throw(int, int)");
}

#[test]
fn function_no_return_not_void() {
    eq!("?fnii@@YA@H@Z" => "__cdecl fnii(int)");
}

//Having Void (X) for the first argument terminates the list and an '@' terminator is an error--so we should not error here.
#[test]
fn function_arguments_void_only_no_list() {
    eq!("?fn@@YAHXZ" => "int __cdecl fn(void)");
}

//Having Void (X) after the first argument is allows, and it does not terminate the list.
#[test]
fn function_arguments_void_not_first_in_list() {
    eq!("?fn@@YAHHXH@Z" => "int __cdecl fn(int, void, int)");
}

//Manufactured; Keep.
#[test]
fn function_backref_args_0() {
    eq!("?fn@@YAHAAHBAHCDEFGHIJKLabc@@MNOPAHQAHRAHSAHTdef@@Ughi@@Vjkl@@0123456789@Z" =>
        "int __cdecl fn(int &, int & volatile, signed char, char, unsigned char, short, unsigned short, int, unsigned int, long, unsigned long, abc, float, double, long double, int *, int * const, int * volatile, int * const volatile, union def, struct ghi, class jkl, int &, int & volatile, abc, int *, int * const, int * volatile, int * const volatile, union def, struct ghi, class jkl)");
}

//Manufactured; Keep.
#[test]
fn function_backref_args_1() {
    eq!("?fn@@YAHW0mno@@XYpqr@@_$H_D_E_F_G_H_I_J0123456789@Z" =>
        "int __cdecl fn(enum char mno, void, cointerface pqr, __w64 int, __int8, unsigned __int8, __int16, unsigned __int16, __int32, unsigned __int32, __int64, enum char mno, cointerface pqr, __w64 int, __int8, unsigned __int8, __int16, unsigned __int16, __int32, unsigned __int32, __int64)");
}

//Manufactured; Keep.
#[test]
fn function_backref_args_2() {
    eq!("?fn@@YAH_K_L_M_N_OAH_W_Xstu@@_Yvwx@@01234567@Z" =>
        "int __cdecl fn(unsigned __int64, __int128, unsigned __int128, bool, int[], wchar_t, coclass stu, cointerface vwx, unsigned __int64, __int128, unsigned __int128, bool, int[], wchar_t, coclass stu, cointerface vwx)");
}

//Manufactured; demonstrates problem in TemplateBackrefArgs more succinctly.
#[test]
fn template_backref_args_comma_problem() {
    eq!("?Ti@@3V?$Tc@VAAA@@00@@A" => "class Tc<class AAAclass AAAclass AAA> Ti");
}

//Manufactured; Keep. Have not seen real examples, but undname works this way.
#[test]
fn template_backref_args_0() {
    eq!("?Ti@@3V?$Tc@AAHBAHCDEFGHIJKLabc@@MNOPAHQAHRAHSAHTdef@@Ughi@@Vjkl@@0123456789@@A" =>
        "class Tc<int &, int & volatile, signed char, char, unsigned char, short, unsigned short, int, unsigned int, long, unsigned long, abc, float, double, long double, int *, int * const, int * volatile, int * const volatile, union def, struct ghi, class jklint &int & volatileabcint *int * constint * volatileint * const volatileunion defstruct ghiclass jkl> Ti");
}

//Manufactured; Keep. Have not seen real examples, but undname works this way.
#[test]
fn template_backref_args_1() {
    eq!("?Ti@@3V?$Tc@W0mno@@XYpqr@@_$H_D_E_F_G_H_I_J0123456789@@A" =>
        "class Tc<enum char mno, void, cointerface pqr, __w64 int, __int8, unsigned __int8, __int16, unsigned __int16, __int32, unsigned __int32, __int64enum char mnocointerface pqr__w64 int__int8unsigned __int8__int16unsigned __int16__int32unsigned __int32__int64> Ti");
}

//Manufactured; Keep. Have not seen real examples, but undname works this way.
#[test]
fn template_backref_args_2() {
    eq!("?Ti@@3V?$Tc@H_K_L_M_N_OAH_W_Xstu@@_Yvwx@@01234567@@A" =>
        "class Tc<int, unsigned __int64, __int128, unsigned __int128, bool, int[], wchar_t, coclass stu, cointerface vwxunsigned __int64__int128unsigned __int128boolint[]wchar_tcoclass stucointerface vwx> Ti");
}

#[test]
fn class_operators_question0a() {
    eq!("??0Array@@$$FQAE@XZ" => "public: __thiscall Array::Array(void)");
}

#[test]
fn class_operators_question0b() {
    eq!("??0Array@@$$FQAE@ABVJunk@@@Z" =>
        "public: __thiscall Array::Array(class Junk const &)");
}

#[test]
fn class_operators_question1() {
    eq!("??1Array@@$$FQAE@XZ" => "public: __thiscall Array::~Array(void)");
}

#[test]
fn class_operators_question2a() {
    eq!("??2@$$FYAPAXI@Z" => "void * __cdecl operator new(unsigned int)");
}

#[test]
fn class_operators_question2b() {
    eq!("??2Array@@$$FSAPAXI@Z" =>
        "public: static void * __cdecl Array::operator new(unsigned int)");
}

#[test]
fn class_operators_question3a() {
    eq!("??3@$$FYAXPAX@Z" => "void __cdecl operator delete(void *)");
}

#[test]
fn class_operators_question3b() {
    eq!("??3Array@@$$FSAXPAX@Z" =>
        "public: static void __cdecl Array::operator delete(void *)");
}

#[test]
fn class_operators_question4() {
    eq!("??4Array@@$$FQAEAAV0@ABV0@@Z" =>
        "public: class Array & __thiscall Array::operator=(class Array const &)");
}

#[test]
fn class_operators_question5() {
    eq!("??5Array@@$$FQAEAAV0@ABV0@@Z" =>
        "public: class Array & __thiscall Array::operator>>(class Array const &)");
}

#[test]
fn class_operators_question6() {
    eq!("??6Array@@$$FQAEAAV0@ABV0@@Z" =>
        "public: class Array & __thiscall Array::operator<<(class Array const &)");
}

#[test]
fn class_operators_question7() {
    eq!("??7Array@@$$FQAEAAV0@XZ" =>
        "public: class Array & __thiscall Array::operator!(void)");
}

#[test]
fn class_operators_question8() {
    eq!("??8Array@@$$FQAEAAV0@ABV0@@Z" =>
        "public: class Array & __thiscall Array::operator==(class Array const &)");
}

#[test]
fn class_operators_question9() {
    eq!("??9Array@@$$FQAEAAV0@ABV0@@Z" =>
        "public: class Array & __thiscall Array::operator!=(class Array const &)");
}

#[test]
fn class_operators_question_a() {
    eq!("??AArray@@$$FQAEAAV0@ABV0@@Z" =>
        "public: class Array & __thiscall Array::operator[](class Array const &)");
}

#[test]
fn class_operators_question_b() {
    eq!("??BArray@@$$FQAE?AVJunk@@XZ" =>
        "public: __thiscall Array::operator class Junk(void)");
}

#[test]
fn class_operators_question_c() {
    eq!("??CArray@@$$FQAEPAVJunk@@XZ" =>
        "public: class Junk * __thiscall Array::operator->(void)");
}

#[test]
fn class_operators_question_d() {
    eq!("??DArray@@$$FQAEAAV0@ABV0@@Z" =>
        "public: class Array & __thiscall Array::operator*(class Array const &)");
}

#[test]
fn class_operators_question_e() {
    eq!("??EArray@@$$FQAEAAV0@H@Z" =>
        "public: class Array & __thiscall Array::operator++(int)");
}

#[test]
fn class_operators_question_f() {
    eq!("??FArray@@$$FQAEAAV0@H@Z" =>
        "public: class Array & __thiscall Array::operator--(int)");
}

#[test]
fn class_operators_question_g() {
    eq!("??GArray@@$$FQAEAAV0@ABV0@@Z" =>
        "public: class Array & __thiscall Array::operator-(class Array const &)");
}

#[test]
fn class_operators_question_h() {
    eq!("??HArray@@$$FQAEAAV0@ABV0@@Z" =>
        "public: class Array & __thiscall Array::operator+(class Array const &)");
}

#[test]
fn class_operators_question_i() {
    eq!("??IArray@@$$FQAEAAV0@ABV0@@Z" =>
        "public: class Array & __thiscall Array::operator&(class Array const &)");
}

#[test]
fn class_operators_question_j() {
    eq!("??JArray@@$$FQAEAAV0@ABV0@@Z" =>
        "public: class Array & __thiscall Array::operator->*(class Array const &)");
}

#[test]
fn class_operators_question_k() {
    eq!("??KArray@@$$FQAEAAV0@ABV0@@Z" =>
        "public: class Array & __thiscall Array::operator/(class Array const &)");
}

#[test]
fn class_operators_question_l() {
    eq!("??LArray@@$$FQAEAAV0@ABV0@@Z" =>
        "public: class Array & __thiscall Array::operator%(class Array const &)");
}

#[test]
fn class_operators_question_m() {
    eq!("??MArray@@$$FQAEAAV0@ABV0@@Z" =>
        "public: class Array & __thiscall Array::operator<(class Array const &)");
}

#[test]
fn class_operators_question_n() {
    eq!("??NArray@@$$FQAEAAV0@ABV0@@Z" =>
        "public: class Array & __thiscall Array::operator<=(class Array const &)");
}

#[test]
fn class_operators_question_o() {
    eq!("??OArray@@$$FQAEAAV0@ABV0@@Z" =>
        "public: class Array & __thiscall Array::operator>(class Array const &)");
}

#[test]
fn class_operators_question_p() {
    eq!("??PArray@@$$FQAEAAV0@ABV0@@Z" =>
        "public: class Array & __thiscall Array::operator>=(class Array const &)");
}

#[test]
fn class_operators_question_q() {
    eq!("??QArray@@$$FQAEAAV0@ABV0@@Z" =>
        "public: class Array & __thiscall Array::operator, (class Array const &)");
}

#[test]
fn class_operators_question_r() {
    eq!("??RArray@@$$FQAEAAV0@ABV0@@Z" =>
    "public: class Array & __thiscall Array::operator()(class Array const &)");
}

#[test]
fn class_operators_question_s() {
    eq!("??SArray@@$$FQAEAAV0@XZ" =>
        "public: class Array & __thiscall Array::operator~(void)");
}

#[test]
fn class_operators_question_t() {
    eq!("??TArray@@$$FQAEAAV0@ABV0@@Z" =>
        "public: class Array & __thiscall Array::operator^(class Array const &)");
}

#[test]
fn class_operators_question_u() {
    eq!("??UArray@@$$FQAEAAV0@ABV0@@Z" =>
        "public: class Array & __thiscall Array::operator|(class Array const &)");
}

#[test]
fn class_operators_question_v() {
    eq!("??VArray@@$$FQAEAAV0@ABV0@@Z" =>
        "public: class Array & __thiscall Array::operator&&(class Array const &)");
}

#[test]
fn class_operators_question_w() {
    eq!("??WArray@@$$FQAEAAV0@ABV0@@Z" =>
        "public: class Array & __thiscall Array::operator||(class Array const &)");
}

#[test]
fn class_operators_question_x() {
    eq!("??XArray@@$$FQAEAAV0@ABV0@@Z" =>
        "public: class Array & __thiscall Array::operator*=(class Array const &)");
}

#[test]
fn class_operators_question_y() {
    eq!("??YArray@@$$FQAEAAV0@ABV0@@Z" =>
        "public: class Array & __thiscall Array::operator+=(class Array const &)");
}

#[test]
fn class_operators_question_z() {
    eq!("??ZArray@@$$FQAEAAV0@ABV0@@Z" =>
        "public: class Array & __thiscall Array::operator-=(class Array const &)");
}

#[test]
fn class_operators_question_0() {
    eq!("??_0Array@@$$FQAEAAV0@ABV0@@Z" =>
        "public: class Array & __thiscall Array::operator/=(class Array const &)");
}

#[test]
fn class_operators_question_1() {
    eq!("??_1Array@@$$FQAEAAV0@ABV0@@Z" =>
        "public: class Array & __thiscall Array::operator%=(class Array const &)");
}

#[test]
fn class_operators_question_2() {
    eq!("??_2Array@@$$FQAEAAV0@ABV0@@Z" =>
        "public: class Array & __thiscall Array::operator>>=(class Array const &)");
}

#[test]
fn class_operators_question_3() {
    eq!("??_3Array@@$$FQAEAAV0@ABV0@@Z" =>
        "public: class Array & __thiscall Array::operator<<=(class Array const &)");
}

#[test]
fn class_operators_question_4() {
    eq!("??_4Array@@$$FQAEAAV0@ABV0@@Z" =>
        "public: class Array & __thiscall Array::operator&=(class Array const &)");
}

#[test]
fn class_operators_question_5() {
    eq!("??_5Array@@$$FQAEAAV0@ABV0@@Z" =>
        "public: class Array & __thiscall Array::operator|=(class Array const &)");
}

#[test]
fn class_operators_question_6() {
    eq!("??_6Array@@$$FQAEAAV0@ABV0@@Z" =>
        "public: class Array & __thiscall Array::operator^=(class Array const &)");
}

#[test]
fn char_string_nul_only() {
    //Has hex-coded nul char only
    eq!("??_C@_00CNPNBAHC@?$AA@" => "`string'");
}

//Has regular char
#[test]
fn char_string_reg_char() {
    eq!("??_C@_01ELNMCGJD@W?$AA@" => "`string'");
}

//Has special char
#[test]
fn char_string_special_char_a() {
    eq!("??_C@_01IHBHIGKO@?0?$AA@" => "`string'");
}

#[test]
fn char_string_special_char_b() {
    eq!("??_C@_01KMDKNFGN@?1?$AA@" => "`string'");
}

#[test]
fn char_string_special_char_c() {
    eq!("??_C@_01KICIPPFI@?2?$AA@" => "`string'");
}

#[test]
fn char_string_special_char_d() {
    eq!("??_C@_01JLIPDDHJ@?3?$AA@" => "`string'");
}

#[test]
fn char_string_special_char_e() {
    eq!("??_C@_01LFCBOECM@?4?$AA@" => "`string'");
}

#[test]
fn char_string_special_char_f() {
    eq!("??_C@_01CLKCMJKC@?5?$AA@" => "`string'");
}

#[test]
fn char_string_special_char_g() {
    eq!("??_C@_01EEMJAFIK@?6?$AA@" => "`string'");
}

#[test]
fn char_string_special_char_h() {
    eq!("??_C@_01GPOEFGEJ@?7?$AA@" => "`string'");
}

#[test]
fn char_string_special_char_i() {
    eq!("??_C@_01GEODFPGF@?8?$AA@" => "`string'");
}

#[test]
fn char_string_special_char_j() {
    eq!("??_C@_01JOAMLHOP@?9?$AA@" => "`string'");
}

#[test]
fn char_string_special_char_k() {
    eq!("??_C@_01CIIBJEOE@?h?$AA@" => "`string'");
}

#[test]
fn char_string_special_char_l() {
    eq!("??_C@_01FFPGGAKB@?m?$AA@" => "`string'");
}

#[test]
fn char_string_special_char_m() {
    eq!("??_C@_01KKJKAMLN@?p?$AA@" => "`string'");
}

#[test]
fn char_string_special_char_n() {
    eq!("??_C@_01JIKMGODP@?r?$AA@" => "`string'");
}

#[test]
fn char_string_special_char_o() {
    eq!("??_C@_01EANLCPLP@y?$AA@" => "`string'");
}

#[test]
fn char_string_special_char_p() {
    eq!("??_C@_02BDIFHNNP@?1?9?$AA@" => "`string'");
}

//Has hex-coded char
#[test]
fn char_string_hexcoded_char() {
    eq!("??_C@_01EOFPKCAF@?$EA?$AA@" => "`string'");
}

#[test]
fn char_string_with_unknown_special_address_there_a() {
    //Microsoft gets this one wrong--has additional information after the nul char.  What is this?
    //Putative address of: 0x5DE3E15C
    eq!("??_C@_00CNPNBAHC@?$AA@FNODOBFM@" => "`string'");
    //Simlar ones to the above:
    //mangled = "??_C@_00CNPNBAHC@?$AA@JKADOLAD@";
    //mangled = "??_C@_00CNPNBAHC@?$AA@LNCPHCLB@";
    //mangled = "??_C@_00CNPNBAHC@?$AA@NNGAKEGL@";
    //mangled = "??_C@_00CNPNBAHC@?$AA@OKHAJAOM@";
    //mangled = "??_C@_00CNPNBAHC@?$AA@OMFIFPKP@";
    //mangled = "??_C@_00CNPNBAHC@?$AA@PBOPGDP@";
}

//Win7 SP1, netw5v64.pdb (netw5v64.sys)
#[test]
fn char_string_with_unknown_special_address_samefile_gone_b() {
    //Without special address
    eq!("??_C@_07CONGLLKI@WPA_PSK?$AA@" => "`string'");
}

//Win7 SP1, netw5v64.pdb (netw5v64.sys)
#[test]
fn char_string_with_unknown_special_address_samefile_there_b() {
    //With special address
    eq!("??_C@_07CONGLLKI@WPA_PSK?$AA@FNODOBFM@" => "`string'");
}

//Win7 SP1, bcmwl664.pdb (bcmwl664.sys)
#[test]
fn char_string_with_unknown_special_address_samefile_gone_c() {
    //Without special address
    eq!("??_C@_07DAFDOJHI@macaddr?$AA@" => "`string'");
}

//Win7 SP1, bcmwl664.pdb (bcmwl664.sys)
#[test]
fn char_string_with_unknown_special_address_samefile_there_c() {
    //With special address
    eq!("??_C@_07DAFDOJHI@macaddr?$AA@FNODOBFM@" => "`string'");
}

//Win7 SP1, dicowan.pdb and dicowans.pdb
#[test]
fn char_string_with_unknown_special_address_samefile_gone_d() {
    //Without special address
    eq!("??_C@_07CBCILOAJ@FaxTask?$AA@" => "`string'");
}

//Win7 SP1, dicowan.pdb and dicowans.pdb
#[test]
fn char_string_with_unknown_special_address_samefile_there_d() {
    //With special address
    eq!("??_C@_07CBCILOAJ@FaxTask?$AA@FNODOBFM@" => "`string'");
}

#[test]
fn wchar_string_a() {
    eq!("??_C@_11LOCGONAA@?$AA?$AA@" => "`string'");
}

#[test]
fn wchar_string_b() {
    eq!("??_C@_13BDBHJCJN@u?3?$AA?$AA@" => "`string'");
}

#[test]
fn wchar_string_c() {
    eq!("??_C@_1BA@KFOBIOMM@?$AAT?$AAY?$AAP?$AAE?$AAL?$AAI?$AAB?$AA?$AA@" => "`string'");
}

#[test]
fn wchar_string_d() {
    eq!("??_C@_1EK@KFPEBLPK@?$AA0?$AA1?$AA2?$AA3?$AA4?$AA5?$AA6?$AA7?$AA8?$AA9?$AA0?$AA1?$AA2?$AA3?$AA4?$AA5?$AA6?$AA7?$AA8?$AA9?$AA0?$AA1?$AA2?$AA3?$AA4?$AA5?$AA6?$AA7?$AA8?$AA9?$AAA?$AAB@" =>
        "`string'");
}

//Manufactured tests that teased out more details.
#[test]
fn underscore7a() {
    eq!("??_7CAnalogAudioStream@@6BCUnknown@@CKsSupport@@@" => "const CAnalogAudioStream::`vftable'{for `CUnknown's `CKsSupport'}");
}

//Manufactured tests that teased out more details.
#[test]
fn underscore7b() {
    eq!("??_7a@b@@6B@" => "const b::a::`vftable'");
}

//Manufactured tests that teased out more details.
#[test]
fn underscore7c() {
    eq!("??_7a@b@@6Bc@d@@@" => "const b::a::`vftable'{for `d::c'}");
}

//Manufactured tests that teased out more details.
#[test]
fn underscore7d() {
    eq!("??_7a@b@@6Bc@d@@e@f@@@" => "const b::a::`vftable'{for `d::c's `f::e'}");
}

//Manufactured tests that teased out more details.
#[test]
fn underscore7e() {
    eq!("??_7a@b@@6Bc@d@e@@f@g@h@@i@j@k@@@" => "const b::a::`vftable'{for `e::d::c's `h::g::f's `k::j::i'}");
}

#[test]
fn special_names_r() {
    eq!("??_R0X@8" => "void `RTTI Type Descriptor'");
}

//real symbol
#[test]
fn special_names_7() {
    eq!("??_7testAccessLevel@@6B@" => "const testAccessLevel::`vftable'");
}

//manufactured symbol
#[test]
fn special_names_8() {
    eq!("??_8testAccessLevel@@$BA@AA" => "[thunk]: __cdecl testAccessLevel::`vbtable'{0, {flat}}' }'");
}

//real symbol
#[test]
fn special_names_9a() {
    eq!("??_9testAccessLevel@@$BA@AA" => "[thunk]: __cdecl testAccessLevel::`vcall'{0, {flat}}' }'");
}

#[test]
fn special_names_9b() {
    eq!("??_9testAccessLevel@@$R5A@B@C@D@AA@@@" => "[thunk]:public: virtual __cdecl testAccessLevel::`vcall'`vtordispex{0, 1, 2, 3}' () throw()");
}

//manufactured symbol
#[test]
fn special_names_9c() {
    eq!("??_9testAccessLevel@@$R5A@B@C@D@AA@H@HH@" => "[thunk]:public: virtual __cdecl testAccessLevel::`vcall'`vtordispex{0, 1, 2, 3}' (int) throw(int, int)");
}

//manufactured symbol
#[test]
fn special_names_a() {
    eq!("??_AtestAccessLevel@@$BA@AA" => "[thunk]: __cdecl testAccessLevel::`typeof'{0, {flat}}' }'");
}

//real symbol
#[test]
fn special_names_b() {
    eq!("??_B?1??name0@name1@name2@@KAHPEBGAEAG@Z@51" =>
        "`protected: static int __cdecl name2::name1::name0(unsigned short const *, unsigned short & )'::`2'::`local static guard'{2}'");
}

//real symbol
#[test]
fn special_names_c() {
    eq!("??_B?1??VTFromRegType@CRegParser@ATL@@KAHPEBGAEAG@Z@51" =>
        "`protected: static int __cdecl ATL::CRegParser::VTFromRegType(unsigned short const *, unsigned short & )'::`2'::`local static guard'{2}'");
}

//manufactured symbol
#[test]
fn special_names_d() {
    eq!("??_DArray@@$$FQAEPAXI@Z" => "public: void * __thiscall Array::`vbase destructor'(unsigned int)");
}

//real symbol
#[test]
fn special_names_e() {
    eq!("??_EArray@@$$FQAEPAXI@Z" => "public: void * __thiscall Array::`vector deleting destructor'(unsigned int)");
}

//manufactured symbol
#[test]
fn special_names_f() {
    eq!("??_FArray@@$$FQAEPAXI@Z" => "public: void * __thiscall Array::`default constructor closure'(unsigned int)");
}

//manufactured symbol
#[test]
fn special_names_ga() {
    eq!("??_GArray@@$$FQAEPAXI@Z" => "public: void * __thiscall Array::`scalar deleting destructor'(unsigned int)");
}

//real symbol
#[test]
fn special_names_gb() {
    eq!("??_Gname0@?1???$name1@W4name2@name3@@@name3@@YA?AW4name2@1@PAV?$name4@W4name2@name3@@@1@IPBV?$name5@$$A6A_NABW4name2@name3@@@Z@name6@name7@@@Z@UAEPAXI@Z" =>
        "public: virtual void * __thiscall `enum name3::name2 __cdecl name3::name1<enum name3::name2>(class name3::name4<enum name3::name2> *, unsigned int, class name7::name6::name5<bool __cdecl(enum name3::name2 const &)> const *)'::`2'::name0::`scalar deleting destructor'(unsigned int)");
}

//manufactured symbol
#[test]
fn special_names_h() {
    eq!("??_HArray@@$$FQAEPAXI@Z" =>
        "public: void * __thiscall Array::`vector constructor iterator'(unsigned int)");
}

//manufactured symbol
#[test]
fn special_names_i() {
    eq!("??_IArray@@$$FQAEPAXI@Z" =>
        "public: void * __thiscall Array::`vector destructor iterator'(unsigned int)");
}

//manufactured symbol
#[test]
fn special_names_j() {
    eq!("??_JArray@@$$FQAEPAXI@Z" =>
        "public: void * __thiscall Array::`vector vbase constructor iterator'(unsigned int)");
}

//manufactured symbol
#[test]
fn special_names_k() {
    eq!("??_KArray@@$$FQAEPAXI@Z" =>
        "public: void * __thiscall Array::`virtual displacement map'(unsigned int)");
}

//real symbol
#[test]
fn special_names_la() {
    eq!("??_L@$$FYMXPAXIHP6MX0@Z1@Z" =>
        "void __clrcall `eh vector constructor iterator'(void *, unsigned int, int, void (__clrcall*)(void *), void (__clrcall*)(void *))");
}

//real symbol
#[test]
fn special_names_lb() {
    eq!("??_L@YGXPAXIHP6EX0@Z1@Z" =>
        "void __stdcall `eh vector constructor iterator'(void *, unsigned int, int, void (__thiscall*)(void *), void (__thiscall*)(void *))");
}

//real symbol
#[test]
fn special_names_m() {
    eq!("??_M@$$FYMXPAXIHP6MX0@Z@Z" =>
        "void __clrcall `eh vector destructor iterator'(void *, unsigned int, int, void (__clrcall*)(void *))");
}

//manufactured symbol
#[test]
fn special_names_n() {
    eq!("??_NArray@@$$FQAEPAXI@Z" =>
        "public: void * __thiscall Array::`eh vector vbase constructor iterator'(unsigned int)");
}

//manufactured symbol
#[test]
fn special_names_o() {
    eq!("??_OArray@@$$FQAEPAXI@Z" =>
        "public: void * __thiscall Array::`copy constructor closure'(unsigned int)");
}

//manufactured symbol
//TODO: look into the "`EH'" possibility (found information somewhere before, but never had a good symbol)
#[test]
fn special_names_q() {
    //name = "`EH'" //must have more embedding as we haven't gotten undname to return yet.
    //manufactured and not sure if good example or not... needs to output "`EH'" ???
    //TODO: need to look closer... as this must be a function, I think (but so do all of those other operators above).
    eq!("??_QNamespace1@Namespace2@@$$FQAEPAXI@Z" =>
        "public: void * __thiscall Namespace2::Namespace1::(unsigned int)");
}

//modified real symbol
#[test]
fn special_names_r0a() {
    eq!("??_R0?PAVname0@name1@@@0HB" =>
        "private: static int const class name1::name0 const volatile __based() `RTTI Type Descriptor'");
}

#[test]
fn special_names_r1() {
    eq!("??_R1A@?0A@EA@testAccessLevel@@8" => "testAccessLevel::`RTTI Base Class Descriptor at (0, -1, 0, 64)'");
}

#[test]
fn special_names_r2() {
    eq!("??_R2testAccessLevel@@8" => "testAccessLevel::`RTTI Base Class Array'");
}

#[test]
fn special_names_r3() {
    eq!("??_R3testAccessLevel@@8" => "testAccessLevel::`RTTI Class Hierarchy Descriptor'");
}

#[test]
fn special_names_r4() {
    eq!("??_R4testAccessLevel@@6B@" => "const testAccessLevel::`RTTI Complete Object Locator'");
}

//manufactured symbol
#[test]
fn special_names_s() {
    eq!("??_SArray@@$$FQAEPAXI@Z" => "public: void * __thiscall Array::`local vftable'(unsigned int)");
}

//manufactured symbol
#[test]
fn special_names_t() {
    eq!("??_TArray@@$$FQAEPAXI@Z" =>
        "public: void * __thiscall Array::`local vftable constructor closure'(unsigned int)");
}

//manufactured symbol
#[test]
fn special_names_ua() {
    eq!("??_UArray@@$$FQAEPAXI@Z" =>
        "public: void * __thiscall Array::operator new[](unsigned int)");
}

//real symbol
#[test]
fn special_names_ub() {
    eq!("??_U@YAPEAX_K@Z" =>
        "void * __cdecl operator new[](unsigned __int64)");
}

//manufactured symbol
#[test]
fn special_names_v() {
    eq!("??_VArray@@$$FQAEPAXI@Z" =>
        "public: void * __thiscall Array::operator delete[](unsigned int)");
}

//manufactured symbol
#[test]
fn special_names_x() {
    eq!("??_XArray@@$$FQAEPAXI@Z" =>
        "public: void * __thiscall Array::`placement delete closure'(unsigned int)");
}

//manufactured symbol
#[test]
fn special_names_y() {
    eq!("??_YArray@@$$FQAEPAXI@Z" =>
        "public: void * __thiscall Array::`placement delete[] closure'(unsigned int)");
}

//manufactured symbol
#[test]
fn special_names_aa() {
    eq!("??__AtestAccessLevel@@$BA@AA" =>
        "[thunk]: __cdecl testAccessLevel::`managed vector constructor iterator'{0, {flat}}' }'");
}

//manufactured symbol
#[test]
fn special_names_bb() {
    eq!("??__BtestAccessLevel@@$BA@AA" =>
        "[thunk]: __cdecl testAccessLevel::`managed vector destructor iterator'{0, {flat}}' }'");
}

//manufactured symbol
#[test]
fn special_names_cc() {
    eq!("??__CtestAccessLevel@@$BA@AA" =>
        "[thunk]: __cdecl testAccessLevel::`eh vector copy constructor iterator'{0, {flat}}' }'");
}

//manufactured symbol
#[test]
fn special_names_dd() {
    eq!("??__DtestAccessLevel@@$BA@AA" =>
        "[thunk]: __cdecl testAccessLevel::`eh vector vbase copy constructor iterator'{0, {flat}}' }'");
}

//real symbol
#[test]
fn special_names_ff() {
    eq!("??__Fname0@?1??name1@name2@name3@name4@@CAXPEAUname5@@P84@EAAJPEAPEAG@ZW4name6@@PEAUname7@@@Z@YAXXZ" =>
        "void __cdecl `private: static void __cdecl name4::name3::name2::name1(struct name5 *, long (__cdecl name4::*)(unsigned short * * ), enum name6, struct name7 * )'::`2'::`dynamic atexit destructor for 'name0''(void)");
}

//manufactured symbol
#[test]
fn special_names_gg() {
    eq!("??__GtestAccessLevel@@$BA@AA" =>
        "[thunk]: __cdecl testAccessLevel::`vector copy constructor iterator'{0, {flat}}' }'");
}

//manufactured symbol
#[test]
fn special_names_hh() {
    eq!("??__HtestAccessLevel@@$BA@AA" =>
        "[thunk]: __cdecl testAccessLevel::`vector vbase copy constructor iterator'{0, {flat}}' }'");
}

//manufactured symbol
#[test]
fn special_names_ii() {
    eq!("??__ItestAccessLevel@@$BA@AA" =>
        "[thunk]: __cdecl testAccessLevel::`managed vector copy constructor iterator'{0, {flat}}' }'");
}

//manufactured symbol
#[test]
fn special_names_jj() {
    eq!("??__JtestAccessLevel@@$BA@AA" =>
        "[thunk]: __cdecl testAccessLevel::`local static thread guard'{0, {flat}}' }'");
}

//manufactured symbol
#[test]
fn special_names_kk() {
    eq!("??__Kabc@def@@3HA" => "int def::operator \"\" abc");
}

//manufactured symbol
#[test]
fn special_names_k_confirm_non_mdreusable_name() {
    eq!("??__Kabc@def@0@3HA" => "int def::def::operator \"\" abc");
}

#[test]
fn qualification_with_interface_namespace_a() {
    eq!("?var@?IInterfaceNamespace@Namespace@@3HA" =>
        "int Namespace[::InterfaceNamespace]::var");
}

#[test]
fn qualification_with_interface_namespace_b() {
    //Notice that MSFT does not include starting bracket
    eq!("?var@Namespace@?IInterfaceNamespace@@3HA" =>
        "int InterfaceNamespace]::Namespace::var)");
}

#[test]
fn qualification_with_interface_namespace_c() {
    //Notice that MSFT does not include starting bracket
    eq!("?var@Namespace@?IInterfaceNamespace1@?IInterfaceNamespace2@@3HA" =>
        "int InterfaceNamespace2][::InterfaceNamespace1]::Namespace::var)");
}

#[test]
fn qualification_with_interface_namespace_d() {
    //Notice that MSFT does not include starting bracket
    eq!("?var@?IInterfaceNamespace1@Namespace@?IInterfaceNamespace2@@3HA" =>
        "int InterfaceNamespace2]::Namespace[::InterfaceNamespace1]::var");
}

#[test]
fn qualification_with_interface_namespace_e() {
    eq!("?var@?IInterfaceNamespace1@?IInterfaceNamespace2@Namespace@@3HA" =>
        "int Namespace[::InterfaceNamespace2][::InterfaceNamespace1]::var");
}

#[test]
fn more_fun_a() {
    eq!("?fn@@3P6A?BHH@ZA" => "int const (__cdecl* fn)(int)");
}

#[test]
fn more_fun_b() {
    eq!("?foo@test1@@QAAXXZ" => "public: void __cdecl test1::foo(void)");
}

#[test]
fn messy_template() {
    eq!("??$?0V?$A@_NABW4B@C@@@D@E@@@?$F@V?$G@U?$H@Q6A_NABW4B@C@@@Z$0A@@D@E@@_NABW4B@C@@@D@E@@@E@@QAE@ABV?$F@V?$A@_NABW4B@C@@@D@E@@@1@@Z" =>
        "public: __thiscall E::F<class E::D::G<struct E::D::H<bool (__cdecl*const)(enum C::B const &), 0>, bool, enum C::B const &> >::F<class E::D::G<struct E::D::H<bool (__cdecl*const)(enum C::B const &), 0>, bool, enum C::B const &> ><class E::D::A<bool, enum C::B const &> >(class E::F<class E::D::A<bool, enum C::B const &> > const &)");
}

#[test]
fn rtti_r1_a() {
    eq!("??_R17?0A@EC@IUnknown@@8" => "IUnknown::`RTTI Base Class Descriptor at (8, -1, 0, 66)'");
}

#[test]
fn rtti_r1_b() {
    eq!("??_R1A@?0A@EA@testAccessLevel@@8" => "testAccessLevel::`RTTI Base Class Descriptor at (0, -1, 0, 64)'");
}

#[test]
fn rtti_r1_c() {
    eq!("??_R1BA@?0A@EA@B@@8" => "B::`RTTI Base Class Descriptor at (16, -1, 0, 64)'");
}

#[test]
fn rtti_r1_d() {
    eq!("??_R17?0A@EA@name0@name1@@8" => "name1::name0::`RTTI Base Class Descriptor at (8, -1, 0, 64)'");
}

#[test]
fn rtti_r1_e() {
    eq!("??_R17?0A@EA@name0@name1@@8" => "name1::name0::`RTTI Base Class Descriptor at (8, -1, 0, 64)'");
}

#[test]
fn rtti_r1_f() {
    eq!("??_R17?0A@EA@?$name0@Vname1@name2@@@name2@@8" => "name2::name0<class name2::name1>::`RTTI Base Class Descriptor at (8, -1, 0, 64)'");
}

#[test]
fn rtti_r1_g() {
    eq!("??_R17?0A@EA@name0@@8" => "name0::`RTTI Base Class Descriptor at (8, -1, 0, 64)'");
}

#[test]
fn rtti_r1_h() {
    eq!("??_R17?0A@EA@?$name0@Vname1@name2@@@name2@@8" => "name2::name0<class name2::name1>::`RTTI Base Class Descriptor at (8, -1, 0, 64)'");
}

#[test]
fn rtti_r1_i() {
    eq!("??_R17?0A@EA@name0@name1@@8" => "name1::name0::`RTTI Base Class Descriptor at (8, -1, 0, 64)'");
}

#[test]
fn rtti_r1_j() {
    eq!("??_R17?0A@EA@?$name0@Vname1@name2@@@name2@@8" => "name2::name0<class name2::name1>::`RTTI Base Class Descriptor at (8, -1, 0, 64)'");
}

#[test]
fn rtti_r1_k() {
    eq!("??_R17?0A@EA@name0@@8" => "name0::`RTTI Base Class Descriptor at (8, -1, 0, 64)'");
}

#[test]
fn rtti_r1_l() {
    eq!("??_R17?0A@EA@?$name0@Vname1@name2@@@name2@@8" => "name2::name0<class name2::name1>::`RTTI Base Class Descriptor at (8, -1, 0, 64)'");
}

#[test]
fn source8_s2a() {
    eq!("??$ft2@P6APEADP6APEAXPEAHPEAF@Z@ZP6APEAFP6APEAX11@Z@ZH@@$$FYAHP6APEADP6APEAXPEAHPEAF@Z@ZP6APEAFP6APEAX11@Z@Z@Z" =>
        "int __cdecl ft2<char * (__cdecl*)(void * (__cdecl*)(int *, short * )), short * (__cdecl*)(void * (__cdecl*)(short *, short * )), int>(char * (__cdecl*)(void * (__cdecl*)(int *, short * )), short * (__cdecl*)(void * (__cdecl*)(short *, short * )))");
}

#[test]
fn source8_s2b() {
    eq!("??$ft2@P6APEAXPEAHP6APEAX0@ZP6APEADP6APEAX0PEAF@Z@ZP6APEAX2@Z2P6APEAF3@ZP6APEAFP6APEAX22@Z@ZPEAD@ZP6APEAF7@ZH@@$$FYAHP6APEAXPEAHP6APEAX0@ZP6APEADP6APEAX0PEAF@Z@ZP6APEAX2@Z2P6APEAF3@ZP6APEAFP6APEAX22@Z@ZPEAD@Z8@Z" =>
    "int __cdecl ft2<void * (__cdecl*)(int *, void * (__cdecl*)(int * ), char * (__cdecl*)(void * (__cdecl*)(int *, short * )), void * (__cdecl*)(short * ), short *, short * (__cdecl*)(void * (__cdecl*)(int *, short * )), short * (__cdecl*)(void * (__cdecl*)(short *, short * )), char * ), short * (__cdecl*)(void * (__cdecl*)(short *, short * )), int>(void * (__cdecl*)(int *, void * (__cdecl*)(int * ), char * (__cdecl*)(void * (__cdecl*)(int *, short * )), void * (__cdecl*)(short * ), short *, short * (__cdecl*)(void * (__cdecl*)(int *, short * )), short * (__cdecl*)(void * (__cdecl*)(short *, short * )), char * ), short * (__cdecl*)(void * (__cdecl*)(short *, short * )))");
}

#[test]
fn source8_aa() {
    eq!("?fai@@3P6APEAXPEAHP6APEAX0@ZP6APEADP6APEAX0PEAF@Z@ZP6APEAX2@Z2P6APEAF3@ZP6APEAFP6APEAX22@Z@ZPEAD@ZEA" => "void * (__cdecl* fai)(int *, void * (__cdecl*)(int *), char * (__cdecl*)(void * (__cdecl*)(int *, short *)), void * (__cdecl*)(short *), short *, short * (__cdecl*)(void * (__cdecl*)(int *, short *)), short * (__cdecl*)(void * (__cdecl*)(short *, short *)), char *)");
}

#[test]
fn source8_ab() {
    eq!("??_7testAccessLevel@@6B@" => "const testAccessLevel::`vftable'");
}

#[test]
fn source8_ac() {
    eq!("??0?$AAA@VBBB@@VCCC@@@@QEAA@P8BBB@@EAAPEAVCCC@@XZ@Z" => "public: __cdecl AAA<class BBB, class CCC>::AAA<class BBB, class CCC>(class CCC * (__cdecl BBB::*)(void))");
}

#[test]
fn source8_ac_mod1() {
    eq!("??0?$AAA@VBBB@@VCCC@@@@QEAA@P8BBB@@EIFDAPEAVCCC@@XZ@Z" => "public: __cdecl AAA<class BBB, class CCC>::AAA<class BBB, class CCC>(class CCC * (__cdecl BBB::*)(void)const volatile __unaligned __restrict)");
}

#[test]
fn source8_ad() {
    eq!("?w1@@3HD" => "int const volatile w1");
}

#[test]
fn source8_ae() {
    eq!("?Tci2@@3V?$Tc@V?$Tb@H@@@@A" => "class Tc<class Tb<int> > Tci2");
}

#[test]
fn source8_af() {
    eq!("?Tci1@@3V?$Tc@H@@A" => "class Tc<int> Tci1");
}

#[test]
fn source8_ag() {
    eq!("?fncii@@YA?BHH@Z" => "int const __cdecl fncii(int)");
}

#[test]
fn source8_ai() {
    eq!("?fnii@@YAHH@Z" => "int __cdecl fnii(int)");
}

#[test]
fn source8_aj() {
    eq!("?fn1@BBB@@QEAAPEAVCCC@@XZ" => "public: class CCC * __cdecl BBB::fn1(void)");
}

#[test]
fn source8_ak() {
    eq!("?pfspro@@3P6AHH@ZEA" => "int (__cdecl* pfspro)(int)");
}

#[test]
fn source8_al() {
    eq!("?pfspub@@3P6AHH@ZEA" => "int (__cdecl* pfspub)(int)");
}

#[test]
fn source8_am() {
    eq!("?pfspri@@3P6AHH@ZEA" => "int (__cdecl* pfspri)(int)");
}

#[test]
fn source8_an() {
    eq!("?ttt@@3Vtest1@@A" => "class test1 ttt");
}

#[test]
fn source8_ao() {
    eq!("?s@@3P8BBB@@EAAPEAVCCC@@XZEQ1@" => "class CCC * (__cdecl BBB::* s)(void)");
}

#[test]
fn source8_ap() {
    eq!("?PBBBMbr@@3PEQBBB@@HEQ1@" => "int BBB::* PBBBMbr");
}

#[test]
fn source8_aq() {
    eq!("?PBBBMbr_r@@3PEIQBBB@@HEIQ1@" => "int BBB::* __restrict __restrict PBBBMbr_r");
}

#[test]
fn source8_ar() {
    eq!("?PBBBMbr_u@@3PEQBBB@@HEQ1@" => "int BBB::* PBBBMbr_u");
}

#[test]
fn source8_as() {
    eq!("?PBBBMbr_ru@@3PEIQBBB@@HEIQ1@" => "int BBB::* __restrict __restrict PBBBMbr_ru");
}

#[test]
fn source8_at() {
    eq!("?PBBBMbr_ur@@3PEIQBBB@@HEIQ1@" => "int BBB::* __restrict __restrict PBBBMbr_ur");
}

#[test]
fn source8_au() {
    eq!("?a@@3HA" => "int a");
}

#[test]
fn source8_av() {
    eq!("?pui@@3PEFAHEFA" => "int __unaligned * __unaligned pui");
}

#[test]
fn source8_aw() {
    eq!("?upui@@3PEFAHEFA" => "int __unaligned * __unaligned upui");
}

#[test]
fn source8_ax() {
    eq!("?rpi@@3PEIAHEIA" => "int * __restrict __restrict rpi");
}

#[test]
fn source8_ay() {
    eq!("?pur@@3PEIFAHEIFA" => "int __unaligned * __restrict __unaligned __restrict pur");
}

#[test]
fn source8_az() {
    eq!("?cpur@@3PEIFBHEIFB" => "int const __unaligned * __restrict const __unaligned __restrict cpur");
}

#[test]
fn source8_ba() {
    eq!("?cvpur@@3PEIFDHEIFD" => "int const volatile __unaligned * __restrict const volatile __unaligned __restrict cvpur");
}

#[test]
fn source8_bb() {
    eq!("?vpur@@3PEIFCHEIFC" => "int volatile __unaligned * __restrict volatile __unaligned __restrict vpur");
}

#[test]
fn source8_bc() {
    eq!("?pci@@3PEBHEB" => "int const * const pci");
}

#[test]
fn source8_bd() {
    eq!("?pvi@@3PECHEC" => "int volatile * volatile pvi");
}

#[test]
fn source8_be() {
    eq!("?pcvi@@3PEDHED" => "int const volatile * const volatile pcvi");
}

#[test]
fn source8_bf() {
    eq!("?cpci@@3QEBHEB" => "int const * const cpci");
}

#[test]
fn source8_bg() {
    eq!("?vpvi@@3RECHEC" => "int volatile * volatile vpvi");
}

#[test]
fn source8_bh() {
    eq!("?cpvi@@3QECHEC" => "int volatile * volatile cpvi");
}

#[test]
fn source8_bi() {
    eq!("?cvpcvi@@3SEDHED" => "int const volatile * const volatile cvpcvi");
}

#[test]
fn source8_bj() {
    eq!("?cpi@@3QEAHEA" => "int * cpi");
}

#[test]
fn source8_bk() {
    eq!("?xpci@@3REBHEB" => "int const * const xpci");
}

#[test]
fn source8_bl() {
    eq!("?vpi@@3REAHEA" => "int * vpi");
}

#[test]
fn source8_bm() {
    eq!("?cvpi@@3SEAHEA" => "int * cvpi");
}

#[test]
fn source8_bn() {
    eq!("?cpcpci@@3QEBQEBHEB" => "int const * const * const cpcpci");
}

#[test]
fn source8_bo() {
    eq!("?cpcpvi@@3QEBQECHEB" => "int volatile * const * const cpcpvi");
}

#[test]
fn source8_bp() {
    eq!("?vpvpvi@@3RECRECHEC" => "int volatile * volatile * volatile vpvpvi");
}

#[test]
fn source8_bq() {
    eq!("?cvpcvpcvi@@3SEDSEDHED" => "int const volatile * const volatile * const volatile cvpcvpcvi");
}

#[test]
fn source8_br() {
    eq!("?pcpci@@3PEBQEBHEB" => "int const * const * const pcpci");
}

#[test]
fn source8_bs() {
    eq!("?pfnii@@3P6AHH@ZEA" => "int (__cdecl* pfnii)(int)");
}

#[test]
fn source8_bt() {
    eq!("?pfncii@@3P6A?BHH@ZEA" => "int const (__cdecl* pfncii)(int)");
}

#[test]
fn source8_bu() {
    eq!("?cpfncii@@3Q6A?BHH@ZEA" => "int const (__cdecl* cpfncii)(int)");
}

#[test]
fn source8_bv() {
    eq!("?enI@@3W4enumI@enumspace@@A" => "enum enumspace::enumI enI");
}

#[test]
fn source8_bw() {
    eq!("?enUI@@3W4enumUI@enumspace@@A" => "enum enumspace::enumUI enUI");
}

#[test]
fn source8_bx() {
    eq!("?enC@@3W4enumC@enumspace@@A" => "enum enumspace::enumC enC");
}

#[test]
fn source8_by() {
    eq!("?enUC@@3W4enumUC@enumspace@@A" => "enum enumspace::enumUC enUC");
}

#[test]
fn source8_bz() {
    eq!("?enS@@3W4enumS@enumspace@@A" => "enum enumspace::enumS enS");
}

#[test]
fn source8_ca() {
    eq!("?enUS@@3W4enumUS@enumspace@@A" => "enum enumspace::enumUS enUS");
}

#[test]
fn source8_cb() {
    eq!("?enL@@3W4enumL@enumspace@@A" => "enum enumspace::enumL enL");
}

#[test]
fn source8_cc() {
    eq!("?enUL@@3W4enumUL@enumspace@@A" => "enum enumspace::enumUL enUL");
}

#[test]
fn source8_cd() {
    eq!("?void3@@3PEAXEA" => "void * void3");
}

#[test]
fn source8_ce() {
    eq!("?void4@@3PEAXEA" => "void * void4");
}

#[test]
fn source8_cf() {
    eq!("?void5@@3PEAXEA" => "void * void5");
}

#[test]
fn source8_cg() {
    eq!("?blah2@@YA?BHH@Z" => "int const __cdecl blah2(int)");
}

#[test]
fn source8_ch() {
    eq!("?use@@YAHPEAVB@@@Z" => "int __cdecl use(class B * )");
}

#[test]
fn source8_ci() {
    eq!("?fnx2@@3P6A?BHH@ZEA" => "int const (__cdecl* fnx2)(int)");
}

#[test]
fn source8_cj() {
    eq!("?foo@test1@@QEAAXXZ" => "public: void __cdecl test1::foo(void) ");
}

#[test]
fn source8_ck() {
    eq!("?fnx1@@3P6A?BHH@ZEA" => "int const (__cdecl* fnx1)(int)");
}

#[test]
fn source8_cl() {
    eq!("?blah1@test1@@SA?BHH@Z" => "public: static int const __cdecl test1::blah1(int)");
}

#[test]
fn source8_cm() {
    eq!("?doit@testAccessLevel@@QEAAXXZ" => "public: void __cdecl testAccessLevel::doit(void) ");
}

#[test]
fn source8_cn() {
    eq!("?fnpri@testAccessLevel@@AEAAHH@Z" => "private: int __cdecl testAccessLevel::fnpri(int) ");
}

#[test]
fn source8_co() {
    eq!("?fnpro@testAccessLevel@@IEAAHH@Z" => "protected: int __cdecl testAccessLevel::fnpro(int) ");
}

#[test]
fn source8_cp() {
    eq!("?fnpub@testAccessLevel@@QEAAHH@Z" => "public: int __cdecl testAccessLevel::fnpub(int) ");
}

#[test]
fn source8_cq() {
    eq!("?fspri@testAccessLevel@@CAHH@Z" => "private: static int __cdecl testAccessLevel::fspri(int)");
}

#[test]
fn source8_cr() {
    eq!("?fspro@testAccessLevel@@KAHH@Z" => "protected: static int __cdecl testAccessLevel::fspro(int)");
}

#[test]
fn source8_cs() {
    eq!("?fspub@testAccessLevel@@SAHH@Z" => "public: static int __cdecl testAccessLevel::fspub(int)");
}

#[test]
fn source8_ct() {
    eq!("??0testAccessLevel@@QEAA@XZ" => "public: __cdecl testAccessLevel::testAccessLevel(void)");
}

#[test]
fn source8_cu() {
    eq!("??_R4testAccessLevel@@6B@" => "const testAccessLevel::`RTTI Complete Object Locator'");
}

#[test]
fn source8_cv() {
    eq!("??_R0?AVtestAccessLevel@@@8" => "class testAccessLevel `RTTI Type Descriptor'");
}

#[test]
fn source8_cw() {
    eq!("??_7type_info@@6B@" => "const type_info::`vftable'");
}

#[test]
fn source8_cx() {
    eq!("??_R3testAccessLevel@@8" => "testAccessLevel::`RTTI Class Hierarchy Descriptor'");
}

#[test]
fn source8_cy() {
    eq!("??_R2testAccessLevel@@8" => "testAccessLevel::`RTTI Base Class Array'");
}

#[test]
fn source8_cz() {
    eq!("??_R1A@?0A@EA@testAccessLevel@@8" => "testAccessLevel::`RTTI Base Class Descriptor at (0, -1, 0, 64)'");
}

#[test]
fn source8_da() {
    eq!("?fvpub@testAccessLevel@@UEAAHH@Z" => "public: virtual int __cdecl testAccessLevel::fvpub(int)");
}

#[test]
fn source8_db() {
    eq!("?fvpro@testAccessLevel@@MEAAHH@Z" => "protected: virtual int __cdecl testAccessLevel::fvpro(int)");
}

#[test]
fn source8_dc() {
    eq!("?fvpri@testAccessLevel@@EEAAHH@Z" => "private: virtual int __cdecl testAccessLevel::fvpri(int)");
}

#[test]
fn source8_dd() {
    eq!("??_9testAccessLevel@@$BA@AA" => "[thunk]: __cdecl testAccessLevel::`vcall'{0, {flat}}' }'");
}

#[test]
fn source8_de() {
    eq!("??_9testAccessLevel@@$BBA@AA" => "[thunk]: __cdecl testAccessLevel::`vcall'{16, {flat}}' }'");
}

#[test]
fn source8_df() {
    eq!("??_9testAccessLevel@@$B7AA" => "[thunk]: __cdecl testAccessLevel::`vcall'{8, {flat}}' }'");
}

#[test]
fn source8_dg() {
    eq!("?acpi@@3QEAY01HEA" => "int (*acpi)[2]");
}

#[test]
fn source8_dh() {
    eq!("?arr@@3PEAY01HEA" => "int (*arr)[2]");
}

#[test]
fn source8_dj() {
    eq!("??_7C@@6BB@@@" => "const C::`vftable'{for `B'}");
}

#[test]
fn source8_dk() {
    eq!("??_7C@@6BA@@@" => "const C::`vftable'{for `A'}");
}

#[test]
fn source8_dl() {
    eq!("??_R4C@@6BA@@@" => "const C::`RTTI Complete Object Locator'{for `A'}");
}

#[test]
fn source8_dm() {
    eq!("??_R0?AVC@@@8" => "class C `RTTI Type Descriptor'");
}

#[test]
fn source8_dn() {
    eq!("??_R3C@@8" => "C::`RTTI Class Hierarchy Descriptor'");
}

#[test]
fn source8_do() {
    eq!("??_R2C@@8" => "C::`RTTI Base Class Array'");
}

#[test]
fn source8_dp() {
    eq!("??_R0?AVA@@@8" => "class A `RTTI Type Descriptor'");
}

#[test]
fn source8_dq() {
    eq!("??_R3A@@8" => "A::`RTTI Class Hierarchy Descriptor'");
}

#[test]
fn source8_dr() {
    eq!("??_R2A@@8" => "A::`RTTI Base Class Array'");
}

#[test]
fn source8_ds() {
    eq!("??_R1BA@?0A@EA@B@@8" => "B::`RTTI Base Class Descriptor at (16, -1, 0, 64)'");
}

#[test]
fn source8_dt() {
    eq!("??_R0?AVB@@@8" => "class B `RTTI Type Descriptor'");
}

#[test]
fn source8_du() {
    eq!("??_R3B@@8" => "B::`RTTI Class Hierarchy Descriptor'");
}

#[test]
fn source8_dv() {
    eq!("??_R2B@@8" => "B::`RTTI Base Class Array'");
}

#[test]
fn source8_dw() {
    eq!("??_R4C@@6BB@@@" => "const C::`RTTI Complete Object Locator'{for `B'}");
}

#[test]
fn source8_dx() {
    eq!("?access@C@@EEAAHXZ" => "private: virtual int __cdecl C::access(void) ");
}

#[test]
fn source8_dy() {
    eq!("??0A@@QEAA@XZ" => "public: __cdecl A::A(void) ");
}

#[test]
fn source8_dz() {
    eq!("??_7A@@6B@" => "const A::`vftable'");
}

#[test]
fn source8_ea() {
    eq!("??_R4A@@6B@" => "const A::`RTTI Complete Object Locator'");
}

#[test]
fn source8_ec() {
    eq!("??0B@@QEAA@XZ" => "public: __cdecl B::B(void)");
}

#[test]
fn source8_ed() {
    eq!("??_7B@@6B@" => "const B::`vftable'");
}

#[test]
fn source8_ee() {
    eq!("??_R4B@@6B@" => "const B::`RTTI Complete Object Locator'");
}

#[test]
fn source8_ef() {
    eq!("?access@B@@UEAAHXZ" => "public: virtual int __cdecl B::access(void)");
}

#[test]
fn source8_eg() {
    eq!("?access@C@@GBA@EAAHXZ" => "[thunk]:private: virtual int __cdecl C::access`adjustor{16}' (void)");
}

#[test]
fn source8_eh() {
    eq!("?cvi@@3HD" => "int const volatile cvi");
}

#[test]
fn source8_ei() {
    eq!("?ci@@3HB" => "int const ci");
}

#[test]
fn source8_ej() {
    eq!("?vi@@3HC" => "int volatile vi");
}

#[test]
fn source8_ek() {
    eq!("?c@@3VC@@A" => "class C c");
}

#[test]
fn source8undname_aa() {
    eq!("?extppfvprica@@3PEQtestAccessLevel@@Y01P81@EBAHH@ZEQ1@" =>
        "int (__cdecl testAccessLevel::*(testAccessLevel::* extppfvprica)[2])(int)const");
}

#[test]
fn source8undname_ab() {
    eq!("??0?$AAA@VBBB@@VCCC@@@@QEAA@P8BBB@@EAAPEAVCCC@@XZP81@EAAJPEAV2@@ZPEBGHZZ" =>
        "public: __cdecl AAA<class BBB, class CCC>::AAA<class BBB, class CCC>(class CCC * (__cdecl BBB::*)(void), long (__cdecl BBB::*)(class CCC *), unsigned short const *, int, ...)");
}

#[test]
fn source8undname_ac() {
    eq!("??0?$AAA@VBBB@@VCCC@@@@QEAA@P8BBB@@EAAPEAVCCC@@XZPEBGHZZ" =>
        "public: __cdecl AAA<class BBB, class CCC>::AAA<class BBB, class CCC>(class CCC * (__cdecl BBB::*)(void), unsigned short const *, int, ...)");
}

#[test]
fn source8undname_ad() {
    eq!("??0?$AAA@VBBB@@VCCC@@@@QEAA@P8BBB@@EAAPEAVCCC@@XZHZZ" =>
        "public: __cdecl AAA<class BBB, class CCC>::AAA<class BBB, class CCC>(class CCC * (__cdecl BBB::*)(void), int, ...)");
}

#[test]
fn source8undname_ae() {
    eq!("??0?$AAA@VBBB@@VCCC@@@@QEAA@P8BBB@@EAAPEAVCCC@@XZZZ" =>
        "public: __cdecl AAA<class BBB, class CCC>::AAA<class BBB, class CCC>(class CCC * (__cdecl BBB::*)(void), ...)");
}

#[test]
fn source8undname_af() {
    eq!("??0?$AAA@VCCC@@@@QEAA@P8BBB@@EAAPEAVCCC@@XZ@Z" =>
        "public: __cdecl AAA<class CCC>::AAA<class CCC>(class CCC * (__cdecl BBB::*)(void))");
}

#[test]
fn source8undname_ag() {
    eq!("??0?$AAA@@@QEAA@P8BBB@@EAAPEAVCCC@@XZ@Z" => "public: __cdecl AAA<>::AAA<>(class CCC * (__cdecl BBB::*)(void))");
}

#[test]
fn source8undname_ah() {
    eq!("??$AAA@@@QEAA@P8BBB@@EAAPEAVCCC@@XZ@Z" => "public: __cdecl AAA<>(class CCC * (__cdecl BBB::*)(void))");
}

#[test]
fn source8undname_aj() {
    eq!("?AAA@@QAA@P8BBB@@AAPAVCCC@@XZ@Z" => "public: __cdecl AAA(class CCC * (__cdecl BBB::*)(void))");
}

#[test]
fn source8undname_al() {
    eq!("?BBBMbr@@3PEQBBB@@HQ1@" => "int BBB::* BBBMbr");
}

#[test]
fn source8undname_am() {
    eq!("?BBBMbr@@3PEFQBBB@@HEQ1@" => "int BBB::__unaligned * BBBMbr");
}

#[test]
fn source8undname_an() {
    eq!("?BBBMbr@@3PEIQBBB@@HEQ1@" => "int BBB::* __restrict BBBMbr");
}

#[test]
fn source8undname_ao() {
    eq!("?BBBMbr@@3PEFIQBBB@@HEQ1@" => "int BBB::__unaligned * __restrict BBBMbr");
}

#[test]
fn source8undname_ap() {
    eq!("?BBBMbr@@3PFIEQBBB@@HEQ1@" => "int BBB::__unaligned * __restrict BBBMbr");
}

#[test]
fn source8undname_aq() {
    eq!("??0a@@3HA" => "int a::a");
}

#[test]
fn source8undname_ar() {
    eq!("?pci@@3PAHB" => "int * const pci");
}

#[test]
fn source8undname_as() {
    eq!("?xpci@@3PBHA" => "int const * xpci");
}

#[test]
fn source8undname_at() {
    eq!("?xaa@@3PBHA" => "int const * xaa");
}

#[test]
fn source8undname_au() {
    eq!("?xbb@@3QBHA" => "int const * xbb");
}

#[test]
fn source8undname_av() {
    eq!("?xcc@@3QAHA" => "int * xcc");
}

#[test]
fn source8undname_aw() {
    eq!("?xaaa@@3PAHB" => "int * const xaaa");
}

#[test]
fn source8undname_ax() {
    eq!("?xbbb@@3QBHB" => "int const * const xbbb");
}

#[test]
fn source8undname_ay() {
    eq!("?xbbbb@@3PBHB" => "int const * const xbbbb");
}

#[test]
fn source8undname_az() {
    eq!("?enC@@3W0enumC@@A" => "enum char enumC enC");
}

#[test]
fn source8undname_ba() {
    eq!("?enC@@3W1enumC@@A" => "enum unsigned char enumC enC");
}

#[test]
fn source8undname_bb() {
    eq!("?enC@@3W2enumC@@A" => "enum short enumC enC");
}

#[test]
fn source8undname_bc() {
    eq!("?enC@@3W3enumC@@A" => "enum unsigned short enumC enC");
}

#[test]
fn source8undname_bd() {
    eq!("?enC@@3W4enumC@@A" => "enum enumC enC");
}

#[test]
fn source8undname_be() {
    eq!("??$?0V?$A@_NAEBW4B@C@@@D@E@@@?$F@V?$G@U?$H@Q6A_NAEBW4I@J@@@Z$0A@@K@L@@_NAEBW4M@N@@@O@P@@@Q@@QEAA@AEBV?$R@V?$T@_NAEBW4U@V@@@W@X@@@1@@Z" =>
        "public: __cdecl Q::F<class P::O::G<struct L::K::H<bool (__cdecl*const)(enum J::I const &), 0>, bool, enum N::M const &> >::F<class P::O::G<struct L::K::H<bool (__cdecl*const)(enum J::I const &), 0>, bool, enum N::M const &> ><class E::D::A<bool, enum C::B const &> >(class Q::R<class X::W::T<bool, enum V::U const &> > const &)");
}

#[test]
fn source8undname_bf() {
    eq!("?Ti@@3V?$Tc@H@@A" => "class Tc<int> Ti");
}

#[test]
fn source8undname_bg() {
    eq!("?xb@@3QCHA" => "int volatile * xb");
}

#[test]
fn source8undname_bh() {
    eq!("?xb@@3PCHA" => "int volatile * xb");
}

#[test]
fn source8undname_bi() {
    eq!("?xb@@3HA" => "int xb");
}

#[test]
fn source8undname_bj() {
    eq!("?xb@@3HC" => "int volatile xb");
}

#[test]
fn source8undname_bk() {
    eq!("?xb@@3PBQCHA" => "int volatile * const * xb");
}

#[test]
fn source8undname_bl() {
    eq!("?xb@@3PBQCHB" => "int volatile * const * const xb");
}

#[test]
fn source8undname_bm() {
    eq!("?cpi@@3PBHA" => "int const * cpi");
}

#[test]
fn source8undname_bn() {
    eq!("?cpi@@3QBHA" => "int const * cpi");
}

#[test]
fn source8undname_bo() {
    eq!("??$?0V?$A@_NABW4B@C@@@D@E@@@?$F@V?$G@U?$H@Q6A_NABW4B@C@@@Z$0A@@D@E@@_NABW4B@C@@@D@E@@@E@@QAE@ABV?$F@V?$A@_NABW4B@C@@@D@E@@@1@@Z" =>
        "public: __thiscall E::F<class E::D::G<struct E::D::H<bool (__cdecl*const)(enum C::B const &), 0>, bool, enum C::B const &> >::F<class E::D::G<struct E::D::H<bool (__cdecl*const)(enum C::B const &), 0>, bool, enum C::B const &> ><class E::D::A<bool, enum C::B const &> >(class E::F<class E::D::A<bool, enum C::B const &> > const &)");
}

#[test]
fn source8undname_bp() {
    eq!("?void2@@3PEAXEA" => "void * void2");
}

#[test]
fn source8undname_bq() {
    eq!("?void1@@3PEAXEA" => "void * void1");
}

#[test]
fn source8undname_br() {
    eq!("?pb@@3PEM2pBased@@HEM21@" => "int __based(pBased) * __based(pBased) pb");
}

#[test]
fn source8undname_bs() {
    eq!("?pb@xyz@@3PEM2pBased@abc@@HEM223@" =>
        "int __based(abc::pBased) * __based(abc::pBased) xyz::pb");
}

#[test]
fn source8undname_bt() {
    // const volatile
    eq!("?pb@xyz@@3SEM2pBased@abc@@HEM223@" =>
        "int __based(abc::pBased) * __based(abc::pBased) xyz::pb");
}

#[test]
fn source8undname_bu() {
    eq!("?pb1@xyz@@3PE5BBB@@2pBased@abc@@HEP234@" =>
        "int const volatile __based(abc::pBased) BBB::* const volatile __based(abc::pBased) xyz::pb1");
}

#[test]
fn source6undname_aa() {
    eq!("?VVCPPP2@@3PEAPEAPEBXEA" => "void const * * * VVCPPP2");
}

#[test]
fn source6undname_ab() {
    eq!("?VVCPP2@@3PEAPEBXEA" => "void const * * VVCPP2");
}

#[test]
fn source6undname_ac() {
    eq!("?VVCP2@@3PEBXEB" => "void const * const VVCP2");
}

#[test]
fn source6undname_ad() {
    eq!("?VIC2@@3HB" => "int const VIC2");
}

#[test]
fn source6undname_ae() {
    eq!("?VVPPP1@@3PEAPEAPEAXEA" => "void * * * VVPPP1");
}

#[test]
fn source6undname_af() {
    eq!("?VVPPP2@@3PEAPEAPEAXEA" => "void * * * VVPPP2");
}

#[test]
fn source6undname_ag() {
    eq!("?VVPP1@@3PEAPEAXEA" => "void * * VVPP1");
}

#[test]
fn source6undname_ah() {
    eq!("?VVPP2@@3PEAPEAXEA" => "void * * VVPP2");
}

#[test]
fn source6undname_ai() {
    eq!("?VVP1@@3PEAXEA" => "void * VVP1");
}

#[test]
fn source6undname_aj() {
    eq!("?VVP2@@3PEAXEA" => "void * VVP2");
}

#[test]
fn source6undname_ak() {
    eq!("?VIP1@@3PEAHEA" => "int * VIP1");
}

#[test]
fn source6undname_al() {
    eq!("?VIP2@@3PEAHEA" => "int * VIP2");
}

#[test]
fn source6undname_am() {
    eq!("?VIR1@@3AEAHEA" => "int & VIR1");
}

#[test]
fn source6undname_an() {
    eq!("?VIR2@@3AEAHEA" => "int & VIR2");
}

#[test]
fn source6undname_ao() {
    eq!("?VUIUR@@3AEAHEA" => "int & VUIUR");
}

#[test]
fn source6undname_ap() {
    eq!("?VUIUP@@3PEIFAHEIFA" =>
        "int __unaligned * __restrict __unaligned __restrict VUIUP");
}

#[test]
fn source6undname_aq() {
    eq!("?VUIUPARR@@3PEIAY01$$CFAHEIA" =>
        "int __unaligned (* __restrict __restrict VUIUPARR)[2]");
}

#[test]
fn source6undname_ar() {
    eq!("?VB1@@3_NA" => "bool VB1");
}

#[test]
fn source6undname_as() {
    eq!("?VB2@@3_NA" => "bool VB2");
}

#[test]
fn source6undname_at() {
    eq!("?VLD1@@3OA" => "long double VLD1");
}

#[test]
fn source6undname_au() {
    eq!("?VLD2@@3OA" => "long double VLD2");
}

#[test]
fn source6undname_av() {
    eq!("?VD1@@3NA" => "double VD1");
}

#[test]
fn source6undname_aw() {
    eq!("?VD2@@3NA" => "double VD2");
}

#[test]
fn source6undname_ax() {
    eq!("?VF1@@3MA" => "float VF1");
}

#[test]
fn source6undname_ay() {
    eq!("?VF2@@3MA" => "float VF2");
}

#[test]
fn source6undname_az() {
    eq!("?VULL1@@3_KA" => "unsigned __int64 VULL1");
}

#[test]
fn source6undname_ba() {
    eq!("?VULL2@@3_KA" => "unsigned __int64 VULL2");
}

#[test]
fn source6undname_bb() {
    eq!("?VLL1@@3_JA" => "__int64 VLL1");
}

#[test]
fn source6undname_bc() {
    eq!("?VLL2@@3_JA" => "__int64 VLL2");
}

#[test]
fn source6undname_bd() {
    eq!("?VUL1@@3KA" => "unsigned long VUL1");
}

#[test]
fn source6undname_be() {
    eq!("?VUL2@@3KA" => "unsigned long VUL2");
}

#[test]
fn source6undname_bf() {
    eq!("?VL1@@3JA" => "long VL1");
}

#[test]
fn source6undname_bg() {
    eq!("?VL2@@3JA" => "long VL2");
}

#[test]
fn source6undname_bh() {
    eq!("?VUI1@@3IA" => "unsigned int VUI1");
}

#[test]
fn source6undname_bi() {
    eq!("?VUI2@@3IA" => "unsigned int VUI2");
}

#[test]
fn source6undname_bj() {
    eq!("?VI1@@3HA" => "int VI1");
}

#[test]
fn source6undname_bk() {
    eq!("?VI2@@3HA" => "int VI2");
}

#[test]
fn source6undname_bl() {
    eq!("?VUS1@@3GA" => "unsigned short VUS1");
}

#[test]
fn source6undname_bm() {
    eq!("?VUS2@@3GA" => "unsigned short VUS2");
}

#[test]
fn source6undname_bn() {
    eq!("?VS1@@3FA" => "short VS1");
}

#[test]
fn source6undname_bo() {
    eq!("?VS2@@3FA" => "short VS2");
}

#[test]
fn source6undname_bp() {
    eq!("?VUC1@@3EA" => "unsigned char VUC1");
}

#[test]
fn source6undname_bq() {
    eq!("?VUC2@@3EA" => "unsigned char VUC2");
}

#[test]
fn source6undname_br() {
    eq!("?VC1@@3DA" => "char VC1");
}

#[test]
fn source6undname_bs() {
    eq!("?VC2@@3DA" => "char VC2");
}

#[test]
fn orig_test_aa() {
    eq!("?name0@?1??name1@name2@name3@@KAHPEBGAEAG@Z@4QBUname4@?1??123@KAH01@Z@B" =>
        "struct `protected: static int __cdecl name3::name2::name1(unsigned short const *, unsigned short &)'::`2'::name4 const * const `protected: static int __cdecl name3::name2::name1(unsigned short const *, unsigned short &)'::`2'::name0");
}

#[test]
fn orig_test_ab() {
    eq!("??_L@YGXPAXIHP6EX0@Z1@Z" =>
        "void __stdcall `eh vector constructor iterator'(void *, unsigned int, int, void (__thiscall*)(void *), void (__thiscall*)(void *))");
}

#[test]
fn orig_test_ac() {
    eq!("?name0@?2??name1@name2@name3@3@KGPAUname4@@PAG@Z@4QBUname5@233@B" =>
        "struct name3::name3::name2::name5 const * const `protected: static struct name4 * __stdcall name3::name3::name2::name1(unsigned short *)'::`3'::name0");
}

#[test]
fn orig_test_ad() {
    eq!("?name0@?1??name1@name2@name3@@SGPBUname4@name5@@XZ@4QBU45@B" =>
        "struct name5::name4 const * const `public: static struct name5::name4 const * __stdcall name3::name2::name1(void)'::`2'::name0");
}

#[test]
fn orig_test_ae() {
    eq!("?name0@name1@@SAHD@Z" => "public: static int __cdecl name1::name0(char)");
}

#[test]
fn orig_test_af() {
    eq!("??_C@_1BA@KFOBIOMM@?$AAT?$AAY?$AAP?$AAE?$AAL?$AAI?$AAB?$AA?$AA@" => "`string'");
}

#[test]
fn orig_test_ag() {
    eq!("?name0@name1@@MAEPAP6GJPAUname2@@IIJ@ZXZ" =>
        "protected: virtual long (__stdcall** __thiscall name1::name0(void))(struct name2 *, unsigned int, unsigned int, long)");
}

#[test]
fn orig_test_ah() {
    eq!("??0name0@@AAE@PBQBD@Z" => "private: __thiscall name0::name0(char const * const *)");
}

#[test]
fn orig_test_ai() {
    eq!("??0name0@@QAE@ABQBD@Z" => "public: __thiscall name0::name0(char const * const &)");
}

#[test]
fn orig_test_aj() {
    eq!("??_U@YAPEAX_K@Z" => "void * __cdecl operator new[](unsigned __int64)");
}

#[test]
fn orig_test_ak() {
    eq!("?name0@name1@@QAEPAPAPAPAMXZ" => "public: float * * * * __thiscall name1::name0(void)");
}

#[test]
fn orig_test_al() {
    eq!("?name0@name1@name2@name3@@0PAV123@A" => "private: static class name3::name2::name1 * name3::name2::name1::name0");
}

#[test]
fn orig_test_am() {
    eq!("??_7name0@@6B@" => "const name0::`vftable'");
}

#[test]
fn orig_test_an() {
    eq!("?name0@@3PAY0IA@EA" => "unsigned char (* name0)[128]");
}

#[test]
fn orig_test_ao() {
    eq!("?name0@@3PAY11BAA@Uname1@@A" => "struct name1 (* name0)[2][256]");
}

#[test]
fn orig_test_ap() {
    eq!("?name0@@YAP6AXIPAUname1@@@ZP6AXI0@Z@Z" =>
        "void (__cdecl*__cdecl name0(void (__cdecl*)(unsigned int, struct name1 *)))(unsigned int, struct name1 *)");
}

#[test]
fn orig_test_aq() {
    eq!("??_R0?PAVname0@@@8" => "class name0 const volatile __based() `RTTI Type Descriptor'");
}

#[test]
fn orig_test_ar() {
    eq!("??$name0@_W@name1@@YAHPB_W000PBUname2@@@Z" =>
        "int __cdecl name1::name0<wchar_t>(wchar_t const *, wchar_t const *, wchar_t const *, wchar_t const *, struct name2 const *)");
}

#[test]
fn orig_test_as() {
    eq!("??$?0_W@?$name0@Uname1@name2@@@name2@@QAE@ABV?$name0@_W@1@@Z" =>
        "public: __thiscall name2::name0<struct name2::name1>::name0<struct name2::name1><wchar_t>(class name2::name0<wchar_t> const &)");
}

#[test]
fn orig_test_at() {
    eq!("??4?$name0@Uname1@@$1?name2@@3Uname3@@B@@QAEAAV0@PAUname1@@@Z" =>
        "public: class name0<struct name1, &struct name3 const name2> & __thiscall name0<struct name1, &struct name3 const name2>::operator=(struct name1 *)");
}

#[test]
fn orig_test_au() {
    eq!("??$name0@D@name1@@YAIPAD0PBD1PBUname2@@@Z" =>
        "unsigned int __cdecl name1::name0<char>(char *, char *, char const *, char const *, struct name2 const *)");
}

#[test]
fn orig_test_av() {
    eq!("??1?$name0@U?$name1@Vname2@?Aname3@name4@@$0A@@name5@name6@@XPAV?$name7@I@name4@@@name5@name6@@UAE@XZ" =>
        "public: virtual __thiscall name6::name5::name0<struct name6::name5::name1<class name4::`anonymous namespace'::name2, 0>, void, class name4::name7<unsigned int> *>::~name0<struct name6::name5::name1<class name4::`anonymous namespace'::name2, 0>, void, class name4::name7<unsigned int> *>(void)");
}

#[test]
fn orig_test_aw() {
    eq!("??_G?$name0@U?$name1@Vname2@?Aname3@name4@@$0A@@name5@name6@@XPAV?$name7@W4name8@name4@@@name4@@@name5@name6@@UAEPAXI@Z" =>
        "public: virtual void * __thiscall name6::name5::name0<struct name6::name5::name1<class name4::`anonymous namespace'::name2, 0>, void, class name4::name7<enum name4::name8> *>::`scalar deleting destructor'(unsigned int)");
}

#[test]
fn orig_test_ax() {
    eq!("??1name0@@UEAA@XZ" => "public: virtual __cdecl name0::~name0(void)");
}

#[test]
fn orig_test_ay() {
    eq!("??0name0@@AEAA@PEAUname1@@@Z" => "private: __cdecl name0::name0(struct name1 *)");
}

#[test]
fn orig_test_az() {
    eq!("?name0@name1@@$4PPPPPPPM@A@EAAJUname2@@HPEBGPEAPEAGK2KK1PEAEKPEAVname3@@@Z" =>
        "[thunk]:public: virtual long __cdecl name1::name0`vtordisp{4294967292, 0}' (struct name2, int, unsigned short const *, unsigned short * *, unsigned long, unsigned short * *, unsigned long, unsigned long, unsigned short const *, unsigned char *, unsigned long, class name3 *)");
}

#[test]
fn orig_test_ba() {
    eq!("?name0@@YAXP6AJPEAPEAVname1@@@ZP6AJPEAVname2@@PEAPEAUname3@@@ZP6AJ3@Z@Z" =>
        "void __cdecl name0(long (__cdecl*)(class name1 * *), long (__cdecl*)(class name2 *, struct name3 * *), long (__cdecl*)(struct name3 * *))");
}

#[test]
fn orig_test_bb() {
    eq!("?name0@name1@@$4PPPPPPPM@A@EAAJUname2@@PEBGW4name3@@11PEAEK3KPEAXPEAVname4@@@Z" =>
        "[thunk]:public: virtual long __cdecl name1::name0`vtordisp{4294967292, 0}' (struct name2, unsigned short const *, enum name3, unsigned short const *, unsigned short const *, unsigned char *, unsigned long, unsigned char *, unsigned long, void *, class name4 *)");
}

#[test]
fn orig_test_bc() {
    eq!("?name0@name1@@QEAAJPEAUname2@@PEAUname3@@@Z" =>
        "public: long __cdecl name1::name0(struct name2 *, struct name3 * ) ");
}

#[test]
fn orig_test_bd() {
    eq!("?name0@name1@@2Uname2@@B" => "public: static struct name2 const name1::name0");
}

#[test]
fn orig_test_be() {
    eq!("?name0@name1@@MAEPAVname2@@XZ" => "protected: virtual class name2 * __thiscall name1::name0(void)");
}

#[test]
fn orig_test_bf() {
    eq!("??_Gname0@?1???$name1@I@name2@@YA_NPAV?$name3@I@1@ABI@Z@UAEPAXI@Z" =>
        "public: virtual void * __thiscall `bool __cdecl name2::name1<unsigned int>(class name2::name3<unsigned int> *, unsigned int const &)'::`2'::name0::`scalar deleting destructor'(unsigned int)");
}

#[test]
fn orig_test_bg() {
    eq!("??$?0V?$name0@_NABW4name1@name2@@@name3@name4@@@?$name5@V?$name6@U?$name7@Q6A_NABW4name1@name2@@@Z$0A@@name3@name4@@_NABW4name1@name2@@@name3@name4@@@name4@@QAE@ABV?$name5@V?$name0@_NABW4name1@name2@@@name3@name4@@@1@@Z" =>
    "public: __thiscall name4::name5<class name4::name3::name6<struct name4::name3::name7<bool (__cdecl*const)(enum name2::name1 const &), 0>, bool, enum name2::name1 const &> >::name5<class name4::name3::name6<struct name4::name3::name7<bool (__cdecl*const)(enum name2::name1 const &), 0>, bool, enum name2::name1 const &> ><class name4::name3::name0<bool, enum name2::name1 const &> >(class name4::name5<class name4::name3::name0<bool, enum name2::name1 const &> > const &)");
}

#[test]
fn orig_test_bh() {
    eq!("??_Gname0@?1???$name1@W4name2@name3@@@name3@@YA?AW4name2@1@PAV?$name4@W4name2@name3@@@1@IPBV?$name5@$$A6A_NABW4name2@name3@@@Z@name6@name7@@@Z@UAEPAXI@Z" =>
        "public: virtual void * __thiscall `enum name3::name2 __cdecl name3::name1<enum name3::name2>(class name3::name4<enum name3::name2> *, unsigned int, class name7::name6::name5<bool __cdecl(enum name3::name2 const &)> const *)'::`2'::name0::`scalar deleting destructor'(unsigned int)");
}

#[test]
fn orig_test_bi() {
    eq!("??_B?1??name0@name1@name2@@KAHPEBGAEAG@Z@51" =>
        "`protected: static int __cdecl name2::name1::name0(unsigned short const *, unsigned short &)'::`2'::`local static guard'{2}'");
}

#[test]
fn orig_test_bj() {
    eq!("??$?0V?$name0@_NABW4name1@name2@@@name3@name4@@@?$name5@V?$name6@U?$name7@Q6A_NABW4name1@name2@@@Z$0A@@name3@name4@@_NABW4name1@name2@@@name3@name4@@@name4@@QAE@ABV?$name5@V?$name0@_NABW4name1@name2@@@name3@name4@@@1@@Z" =>
        "public: __thiscall name4::name5<class name4::name3::name6<struct name4::name3::name7<bool (__cdecl*const)(enum name2::name1 const &), 0>, bool, enum name2::name1 const &> >::name5<class name4::name3::name6<struct name4::name3::name7<bool (__cdecl*const)(enum name2::name1 const &), 0>, bool, enum name2::name1 const &> ><class name4::name3::name0<bool, enum name2::name1 const &> >(class name4::name5<class name4::name3::name0<bool, enum name2::name1 const &> > const &)");
}

#[test]
fn orig_test_bk() {
    eq!("??6?Aname0@name1@@YAAAVname2@1@AAV21@ABVname3@1@@Z" =>
        "class name1::name2 & __cdecl name1::`anonymous namespace'::operator<<(class name1::name2 &, class name1::name3 const &)");
}

#[test]
fn orig_test_bl() {
    eq!("??8@YAHAEBVname0@@0@Z" => "int __cdecl operator==(class name0 const &, class name0 const &)");
}

#[test]
fn orig_test_bm() {
    eq!("??$?9$$A6A_NABW4name0@name1@@@Z@name2@@YA_NABV?$name3@$$A6A_NABW4name0@name1@@@Z@0@$$T@Z" =>
        "bool __cdecl name2::operator!=<bool __cdecl(enum name1::name0 const &)>(class name2::name3<bool __cdecl(enum name1::name0 const &)> const &, std::nullptr_t)");
}

#[test]
fn orig_test_bn() {
    eq!("?name0@name1@@SGPAV1@PAUname2@@@Z" => "public: static class name1 * __stdcall name1::name0(struct name2 *)");
}

#[test]
fn orig_test_bo() {
    eq!("?name0@name1@@QEBAPEFBUname2@@AEBUname3@@K@Z" =>
        "public: struct name2 const __unaligned * __cdecl name1::name0(struct name3 const &, unsigned long) const");
}

#[test]
fn orig_test_bp() {
    eq!("??_R17?0A@EA@name0@name1@@8" => "name1::name0::`RTTI Base Class Descriptor at (8, -1, 0, 64)'");
}

#[test]
fn orig_test_br() {
    eq!("?name0@?1??name1@@9@4P6AHPEAUname2@@@ZEA" => "int (__cdecl* `name1'::`2'::name0)(struct name2 *)");
}

#[test]
fn orig_test_bs() {
    eq!("?name0@name1@name2@@0QAY0BAA@$$CBIA" =>
        "private: static unsigned int const (* name2::name1::name0)[256]");
}

#[test]
fn dollar_dollar_1() {
    // Example: Space after template parameter (cv modifier).
    //This is a DataType $$C Modifier
    eq!("??0?$name0@$$CBUname1@@@name2@@QEAA@XZ" =>
        "public: __cdecl name2::name0<struct name1 const >::name0<struct name1 const >(void)");
}

#[test]
fn dollar_dollar_2() {
    //This is a DataType $$C Modifier
    eq!("?name0@@3QAY01$$CBEA" => "unsigned char const (* name0)[2]");
}

#[test]
fn dollar_dollar_3() {
    //manufactured.
    //This is a DataType $$T Modifier
    eq!("?fn@@YAH$$T@Z" => "int __cdecl fn(std::nullptr_t)");
}

#[test]
fn dollar_dollar_5() {
    eq!("?name0@@3PEIAY01$$CFAHEIA" =>
        "int __unaligned (* __restrict __restrict name0)[2]");
}

#[test]
fn dollar_dollar_6() {
    //found elsewhere ($$C)
    eq!("??0?$name0@$$CBUname1@@@name2@@QEAA@XZ" =>
        "public: __cdecl name2::name0<struct name1 const >::name0<struct name1 const >(void)");
}

#[test]
fn dollar_dollar_7() {
    eq!("??0?$name0@$$CBUname1@@@name2@@QEAA@XZ" =>
        "public: __cdecl name2::name0<struct name1 const >::name0<struct name1 const >(void)");
}

#[test]
fn dollar_dollar_8() {
    eq!("?abort@@$$J0YAXXZ" => "extern \"C\" void __cdecl abort(void)");
}

#[test]
fn dollar_dollar_10() {
    eq!("??0Array@@$$FQAE@XZ" => "public: __thiscall Array::Array(void)");
}

#[test]
fn dollar_dollar_11() {
    //STILL A PROBLEM 20140430 and 20140515
    eq!("???__E?name0@name1@name2@@$$Q2_NA@@YMXXZ@?A0x3d49b2d0@@$$FYMXXZ" =>
        "void __clrcall `dynamic initializer for 'public: static bool name2::name1::name0''(void)");
}

#[test]
fn dollar_dollar_12() {
    eq!("???__E?name0@name1@<name2>@@$$Q2_NA@@YMXXZ@?A0x3d49b2d0@@$$FYMXXZ" =>
        "void __clrcall `dynamic initializer for 'public: static bool <name2>::name1::name0''(void)");
}

#[test]
fn dollar_dollar_15() {
    //From ~LINE 3473
    //mod of a later one
    eq!("?var@@3$$BY0C@HA" => "int ( var)[2]");
}

#[test]
fn dollar_dollar_16() {
    //hand-made $$A ($$A works for functions: 6, 7, 8, 9; but nothing yet for non-function modifiers)
    eq!("?FN@@QAAH$$A6AH@Z@Z" => "public: int __cdecl FN(int __cdecl())");
}

#[test]
fn dollar_dollar_17() {
    eq!("??0?$name0@$$BY0BAE@G@@QEAA@PEAY0BAE@G@Z" =>
        "public: __cdecl name0<unsigned short [260]>::name0<unsigned short [260]>(unsigned short (*)[260])");
}

#[test]
fn dollar_dollar_18() {
    //hand-made $$A ($$A works for functions: 6, 7, 8, 9; but nothing yet for non-function modifiers)
    eq!("?var@@3$$A6AH@ZA" => "int (__cdecl var)()");
}

#[test]
fn dollar_dollar_19() {
    //hand-made $$A but as template parameter (full FN property vs. FN pointer as in _18, above.)
    eq!("?T@@3V?$TC@$$A6AH@Z@@A" => "class TC<int __cdecl()> T");
}

#[test]
fn dollar_dollar_20() {
    //hand-made $$A ($$A works for functions: 6, 7, 8, 9; but nothing yet for non-function modifiers)
    eq!("?var@@3$$A8blah@@AAH@ZA" => "int (__cdecl blah:: var)()");
}

#[test]
fn dollar_dollar_21() {
    //hand-made $$A: Mod of one in CV testing (doing $$A instead of P)
    eq!("?VarName@@3$$A_DClassName@@D0AHH@ZEA" =>
        "int (__cdecl __based(void) ClassName:: VarName)(int)const volatile ");
}

//Manufactured; Keep--should not encapsulate function reference "__cdecl __based(void) ClassName::" in parentheses.
#[test]
fn dollar_dollar_22() {
    //hand-made $$A: Mod of one in CV testing (doing $$A instead of P) and mod of one above, changing to template parameter
    eq!("?VarName@@3V?$TC@$$A_DClassName@@D0AHH@Z@@EA" =>
        "class TC<int __cdecl __based(void) ClassName::(int)const volatile > VarName");
}

#[test]
fn dollar_dollar_22_mod1() {
    //hand-made $$A: Mod of one in CV testing (doing $$A instead of P) and mod of one above, changing to template parameter
    eq!("?VarName@@3V?$TC@P_DClassName@@D0AHH@Z@@EA" =>
        "class TC<int (__cdecl __based(void) ClassName::*)(int)const volatile > VarName");
}

#[test]
fn dollar_dollar_22_mod2() {
    //hand-made $$A: Mod of one in CV testing (doing $$A instead of P) and mod of one above, changing to template parameter
    eq!("?VarName@@3V?$TC@S_DClassName@@D0AHH@Z@@EA" =>
        "class TC<int (__cdecl __based(void) ClassName::*const volatile)(int)const volatile > VarName");
}

#[test]
fn dollar_dollar_22_mod3() {
    //hand-made $$A: Mod of one in CV testing (doing $$A instead of P) and mod of one above, changing to template parameter
    eq!("?VarName@@3V?$TC@A_DClassName@@D0AHH@Z@@EA" =>
        "class TC<int (__cdecl __based(void) ClassName::&)(int)const volatile > VarName");
}

#[test]
fn dollar_dollar_22_mod4() {
    //hand-made $$A: Mod of one in CV testing (doing $$A instead of P) and mod of one above, changing to template parameter
    eq!("?VarName@@3V?$TC@B_DClassName@@D0AHH@Z@@EA" =>
        "class TC<int (__cdecl __based(void) ClassName::&volatile)(int)const volatile > VarName");
}

#[test]
fn dollar_dollar_22_mod4a() {
    //hand-made $$A: Mod of one in CV testing (doing $$A instead of P) and mod of one above, changing to template parameter
    eq!("?VarName@@3V?$TC@R_DClassName@@D0AHH@Z@@EA" =>
        "class TC<int (__cdecl __based(void) ClassName::*volatile)(int)const volatile > VarName");
}

//Manufactured; Keep--should not encapsulate function reference "__cdecl __based(void) ClassName::" in parentheses.
#[test]
fn dollar_dollar_22_mod5() {
    //hand-made $$A: Mod of one in CV testing (doing $$A instead of P) and mod of one above, changing to template parameter
    eq!("?var@@3V?$TC@$$A6AHH@Z@@A" => "class TC<int __cdecl(int)> var");
}

#[test]
fn dollar_dollar_22a() {
    //hand-made A instead of $$A of last one
    eq!("?VarName@@3V?$TC@A_DClassName@@D0AHH@Z@@EA" =>
        "class TC<int (__cdecl __based(void) ClassName::&)(int)const volatile > VarName");
}

#[test]
fn dollar_dollar_23() {
    eq!("??_Gname0@?1???$name1@W4name2@name3@@@name3@@YA?AW4name2@1@PAV?$name4@W4name2@name3@@@1@IPBV?$name5@$$A6A_NABW4name2@name3@@@Z@name6@name7@@@Z@UAEPAXI@Z" =>
        "public: virtual void * __thiscall `enum name3::name2 __cdecl name3::name1<enum name3::name2>(class name3::name4<enum name3::name2> *, unsigned int, class name7::name6::name5<bool __cdecl(enum name3::name2 const &)> const *)'::`2'::name0::`scalar deleting destructor'(unsigned int)");
}

#[test]
fn dollar_dollar_24() {
    eq!("??$name0@H$$A6AJPEAUname1@@PEAVname2@name3@@@Z@?$name4@HP6AJPEAUname1@@PEAVname2@name3@@@ZV?$name5@H@@V?$name5@P6AJPEAUname1@@PEAVname2@name3@@@Z@@@@QEAAJAEBHA6AJPEAUname1@@PEAVname2@name3@@@ZPEAVname6@0@@Z" =>
    "public: long __cdecl name4<int, long (__cdecl*)(struct name1 *, class name3::name2 *), class name5<int>, class name5<long (__cdecl*)(struct name1 *, class name3::name2 *)> >::name0<int, long __cdecl(struct name1 *, class name3::name2 *)>(int const &, long (__cdecl&)(struct name1 *, class name3::name2 *), class name4<int, long (__cdecl*)(struct name1 *, class name3::name2 *), class name5<int>, class name5<long (__cdecl*)(struct name1 *, class name3::name2 *)> >::name6 *)");
}

#[test]
fn temp_bbbqual_blank_name_while_checking_dollar_dollar_a() {
    eq!("?PBBBMbr@@3PEQBBB@@HEQ1@" => "int BBB::*PBBBMbr");
}

#[test]
fn managed_properties_and_dollar_dollar_debug_in_progress_ad() {
    eq!("??0?$name0@$$BY02Uname1@@@name2@@QEAA@XZ" =>
        "public: __cdecl name2::name0<struct name1 [3]>::name0<struct name1 [3]>(void)");
}

#[test]
fn managed_properties_and_dollar_dollar_debug_in_progress_af() {
    //mod of a later one
    eq!("?var@@3$$BY0C@HA" => "int (var)[2]");
}

#[test]
fn managed_properties_and_dollar_dollar_debug_in_progress_ai() {
    //hand-made $$A ($$A works for functions: 6, 7, 8, 9; but nothing yet for non-function modifiers)
    eq!("?FN@@QAAH$$A6AH@Z@Z" => "public: int __cdecl FN(int __cdecl())");
}

#[test]
fn managed_properties_and_dollar_dollar_debug_in_progress_al() {
    //hand-made $$A ($$A works for functions: 6, 7, 8, 9; but nothing yet for non-function modifiers)
    eq!("?var@@3$$A6AH$$A6AH@Z@ZA" => "int (__cdecl var)(int __cdecl())");
}

#[test]
fn managed_properties_and_dollar_dollar_debug_in_progress_am() {
    //hand-made $$A ($$A works for functions: 6, 7, 8, 9; but nothing yet for non-function modifiers)
    eq!("?var@@3A6AH$$A6AH@Z@ZA" => "int (__cdecl& var)(int __cdecl())");
}

#[test]
fn managed_properties_and_dollar_dollar_debug_in_progress_an() {
    //hand-made $$A ($$A works for functions: 6, 7, 8, 9; but nothing yet for non-function modifiers)
    eq!("?var@@3P6AH$$A6AH@Z@ZA" => "int (__cdecl* var)(int __cdecl())");
}

#[test]
fn managed_properties_and_dollar_dollar_debug_in_progress_ao() {
    //hand-made $$A ($$A works for functions: 6, 7, 8, 9; but nothing yet for non-function modifiers)
    eq!("?var@@3A6AHA6AH@Z@ZA" => "int (__cdecl& var)(int (__cdecl&)())");
}

#[test]
fn managed_properties_and_dollar_dollar_debug_in_progress_ap() {
    //hand-made $$A ($$A works for functions: 6, 7, 8, 9; but nothing yet for non-function modifiers)
    eq!("?var@@3P6AHP6AH@Z@ZA" => "int (__cdecl* var)(int (__cdecl*)())");
}

#[test]
fn managed_properties_and_dollar_dollar_debug_in_progress_as() {
    //found elsewhere ($$A)
    eq!("??$name0@H$$A6AJPEAUname1@@PEAVname2@name3@@@Z@?$name4@HP6AJPEAUname1@@PEAVname2@name3@@@ZV?$name5@H@@V?$name5@P6AJPEAUname1@@PEAVname2@name3@@@Z@@@@QEAAJAEBHA6AJPEAUname1@@PEAVname2@name3@@@ZPEAVname6@0@@Z" =>
    "public: long __cdecl name4<int, long (__cdecl*)(struct name1 *, class name3::name2 *), class name5<int>, class name5<long (__cdecl*)(struct name1 *, class name3::name2 *)> >::name0<int, long __cdecl(struct name1 *, class name3::name2 *)>(int const &, long (__cdecl&)(struct name1 *, class name3::name2 *), class name4<int, long (__cdecl*)(struct name1 *, class name3::name2 *), class name5<int>, class name5<long (__cdecl*)(struct name1 *, class name3::name2 *)> >::name6 *)");
}

#[test]
fn managed_properties_and_dollar_dollar_debug_in_progress_ata() {
    //hand-made $$A ($$A works for functions: 6, 7, 8, 9; but nothing yet for non-function modifiers)
    eq!("?var@@3$$A8blah@@AAH@ZA" => "int (__cdecl blah::var)()");
}

#[test]
fn managed_properties_and_dollar_dollar_debug_in_progress_au() {
    //hand-made $$A ($$A works for functions: 6, 7, 8, 9; but nothing yet for non-function modifiers)
    //NOTE: in parentheses if varname is included (like with function pointers)
    eq!("?var@@3$$A6AH$$A6AH@Z@ZA" => "int (__cdecl var)(int __cdecl())");
}

#[test]
fn managed_properties_and_dollar_dollar_debug_in_progress_av() {
    //hand-made $$A ($$A works for functions: 6, 7, 8, 9; but nothing yet for non-function modifiers)
    //NOTE: in parentheses if varname is included (like with function pointers); this returns function type
    eq!("?var@@3$$A6AH$$A6A$$A6AH@Z@Z@ZA" => "int (__cdecl var)(int (__cdecl__cdecl())())");
}

#[test]
fn managed_properties_and_dollar_dollar_debug_in_progress_av_mod() {
    //hand-made $$A ($$A works for functions: 6, 7, 8, 9; but nothing yet for non-function modifiers)
    //NOTE: in parentheses if varname is included (like with function pointers); this returns function typ)e
    eq!("?var@@3P6AHP6AP6AH@Z@Z@ZA" => "int (__cdecl* var)(int (__cdecl*(__cdecl*)())()))");
}

#[test]
fn managed_properties_and_dollar_dollar_debug_in_progress_aw() {
    //hand-made $$A ($$A works for functions: 6, 7, 8, 9; but nothing yet for non-function modifiers)
    eq!("?var@@3$$A6A$$A6AH@Z$$A6AH@Z@ZA" => "int (__cdecl(__cdecl var)(int __cdecl()))())");
}

#[test]
fn managed_properties_and_dollar_dollar_debug_in_progress_az() {
    //found elsewhere ($$C)
    eq!("??0?$name0@$$CBUname1@@@name2@@QEAA@XZ" =>
        "public: __cdecl name2::name0<struct name1 const >::name0<struct name1 const >(void)");
}

#[test]
fn managed_properties_and_dollar_dollar_debug_in_progress_ba() {
    //mod of above (same as $$C, but no modifier)
    eq!("??0?$name0@$$BUname1@@@name2@@QEAA@XZ" =>
        "public: __cdecl name2::name0<struct name1>::name0<struct name1>(void)");
}

#[test]
fn managed_properties_and_dollar_dollar_debug_in_progress_bb() {
    //mod of above (same as $$C, but no modifier)
    eq!("??0?$name0@$$BY02Uname1@@@name2@@QEAA@XZ" =>
        "public: __cdecl name2::name0<struct name1 [3]>::name0<struct name1 [3]>(void)");
}

#[test]
fn managed_properties_and_dollar_dollar_debug_in_progress_bg() {
    //mod of a later one
    eq!("??$var@H$$BY0C@HH@@QEAA@@Z" => "public: __cdecl var<int, int [2], int>()");
}

#[test]
fn managed_properties_and_dollar_dollar_debug_in_progress_bh() {
    //mod of a later one
    eq!("??$var@H$$BY0C@HH@@QEAA@$$BY0C@H@Z" =>
        "public: __cdecl var<int, int [2], int>(int [2])");
}

#[test]
fn managed_properties_and_dollar_dollar_debug_in_progress_bi() {
    //mod of a later one
    eq!("?var@@QAA@$$BY0C@H@Z" => "public: __cdecl var(int [2])");
}

#[test]
fn managed_properties_and_dollar_dollar_debug_in_progress_bj() {
    //mod of a later one
    eq!("?var@@3$$BY0C@HA" => "int (var)[2]");
}

#[test]
fn managed_properties_and_dollar_dollar_debug_in_progress_bk() {
    //mod of a later one
    eq!("?var@@3$$BHA" => "int var");
}

#[test]
fn managed_properties_and_dollar_dollar_debug_in_progress_bl() {
    eq!("??0?$name0@$$BY0BAE@G@@QEAA@PEAY0BAE@G@Z" =>
        "public: __cdecl name0<unsigned short [260]>::name0<unsigned short [260]>(unsigned short (*)[260])");
}

#[test]
fn managed_properties_and_dollar_dollar_debug_in_progress_bo() {
    eq!("?FN@@QEAM@PE$02AVCL@@@Z" => "public: __clrcall FN(cli::array<class CL, 2>^)");
}

#[test]
fn managed_properties_and_dollar_dollar_debug_in_progress_bp() {
    // Example: Space after template parameter (cv modifier).
    eq!("??0?$name0@$$CBUname1@@@name2@@QEAA@XZ" =>
        "public: __cdecl name2::name0<struct name1 const >::name0<struct name1 const >(void)");
}

#[test]
fn managed_properties_and_dollar_dollar_debug_in_progress_bq() {
    eq!("?abort@@$$J0YAXXZ" => "extern \"C\" void __cdecl abort(void)");
}

#[test]
fn managed_properties_and_dollar_dollar_debug_in_progress_bs() {
    eq!("???__E?name0@name1@name2@@$$Q2_NA@@YMXXZ@?A0x3d49b2d0@@$$FYMXXZ" =>
        "void __clrcall `dynamic initializer for 'public: static bool name2::name1::name0''(void)");
}

#[test]
fn managed_properties_and_dollar_dollar_debug_in_progress_bt() {
    eq!("??_Gname0@?1???$name1@W4name2@name3@@@name3@@YA?AW4name2@1@PAV?$name4@W4name2@name3@@@1@IPBV?$name5@$$A6A_NABW4name2@name3@@@Z@name6@name7@@@Z@UAEPAXI@Z" =>
        "public: virtual void * __thiscall `enum name3::name2 __cdecl name3::name1<enum name3::name2>(class name3::name4<enum name3::name2> *, unsigned int, class name7::name6::name5<bool __cdecl(enum name3::name2 const &)> const *)'::`2'::name0::`scalar deleting destructor'(unsigned int)");
}

#[test]
fn cvmod_tail_modifier_xxxx1() {
    eq!("?fn@@UAAXXZ" => "public: virtual void __cdecl fn(void)");
}

#[test]
fn cvmod_tail_modifier_xxxx2() {
    eq!("?fn@@UEIFDAXXZ" => "public: virtual void __cdecl fn(void) const volatile __unaligned __restrict");
}

// Manufactured
#[test]
fn managed_properties_aa() {
    eq!("?FN@@QEAM@BE$AAVCL@@@Z" => "public: __clrcall FN(class CL % volatile)");
}

// Manufactured
#[test]
fn managed_properties_au() {
    eq!("?FN@@QEAM@PE$02AVCL@@@Z" => "public: __clrcall FN(cli::array<class CL, 2>^)");
}

// Manufactured
#[test]
fn managed_properties_av() {
    eq!("?FN@@QEAM@PE$01AVCL@@@Z" => "public: __clrcall FN(cli::array<class CL >^)");
}

// Manufactured
#[test]
fn managed_properties_aw() {
    eq!("?FN@@QEAM@QE$01AVCL@@@Z" => "public: __clrcall FN(cli::array<class CL >^)");
}

// Manufactured
#[test]
fn managed_properties_ax() {
    eq!("?FN@@QEAM@RE$01AVCL@@@Z" => "public: __clrcall FN(cli::array<class CL >^)");
}

// Manufactured
#[test]
fn managed_properties_ay() {
    eq!("?FN@@QEAM@SE$01AVCL@@@Z" => "public: __clrcall FN(cli::array<class CL >^)");
}

// Manufactured
#[test]
fn managed_properties_az() {
    eq!("?FN@@QEAM@BE$01AVCL@@@Z" => "public: __clrcall FN(cli::array<class CL >^)");
}

// Manufactured
#[test]
fn managed_properties_ba() {
    eq!("?FN@@QEAM@P$01AVCL@@@Z" => "public: __clrcall FN(cli::array<class CL >^)");
}

// Manufactured
#[test]
fn managed_properties_bb() {
    eq!("?FN@@QEAM@Q$01AVCL@@@Z" => "public: __clrcall FN(cli::array<class CL >^)");
}

// Manufactured
#[test]
fn managed_properties_bc() {
    eq!("?FN@@QEAM@R$01AVCL@@@Z" => "public: __clrcall FN(cli::array<class CL >^)");
}

// Manufactured
#[test]
fn managed_properties_bd() {
    eq!("?FN@@QEAM@S$01AVCL@@@Z" => "public: __clrcall FN(cli::array<class CL >^)");
}

// Manufactured
#[test]
fn managed_properties_be() {
    eq!("?FN@@QEAM@B$01AVCL@@@Z" => "public: __clrcall FN(cli::array<class CL >^)");
}

// Manufactured
#[test]
fn managed_properties_bf() {
    eq!("??1?$name0@PEAUname1@@V?$name2@PEAUname1@@$0A@P6APEAXPEAX@Z$1?name3@@$$FYAPEAX0@ZP6AAEAPEAU1@AEAPEAU1@@Z$1?name4@?$name5@PEAUname1@@@@$$FSAAEAPEAU1@1@Z@@@@$$FUEAA@XZ" =>
        "public: virtual __cdecl name0<struct name1 *, class name2<struct name1 *, 0, void * (__cdecl*)(void *), &void * __cdecl name3(void *), struct name1 * & (__cdecl*)(struct name1 * &), &public: static struct name1 * & __cdecl name5<struct name1 *>::name4(struct name1 * &)> >::~name0<struct name1 *, class name2<struct name1 *, 0, void * (__cdecl*)(void *), &void * __cdecl name3(void *), struct name1 * & (__cdecl*)(struct name1 * & ), &public: static struct name1 * & __cdecl name5<struct name1 *>::name4(struct name1 * &)> >(void)");
}

// Manufactured
#[test]
fn managed_properties_bg() {
    eq!("??0name0@name1@@QEAA@AEBVname2@1@U?$name3@$$A6AXU?$name4@Vname5@name1@@@name1@@@Z@1@AEBU?$name6@Vname7@name1@@@1@@Z" =>
        "public: __cdecl name1::name0::name0(class name1::name2 const &, struct name1::name3<void __cdecl(struct name1::name4<class name1::name5>)>, struct name1::name6<class name1::name7> const &");
}

// Manufactured
#[test]
fn managed_properties_bh() {
    //This is the only real $$H example we have.
    eq!("?wmain@@$$HYAHXZ" => "int __cdecl wmain(void)");
}

// Manufactured
#[test]
fn managed_properties_bi() {
    eq!("??0name0@name1@name2@name3@@$$FQE$AAM@XZ" => "public: __clrcall name3::name2::name1::name0::name0(void)");
}

#[test]
fn managed_extensions1_single1() {
    eq!("?get@C@@$$FQ$CAMHXZ" => "public: int __clrcall C::get(void)%");
}

#[test]
fn managed_extensions1_single2xx() {
    eq!("?get@C@@$$FQE$AE$AEI$CDMHXZ" =>
        "public: int __clrcall C::get(void)const volatile % __restrict");
}

#[test]
fn managed_extensions1_single2xxx() {
    eq!("?get@C@@$$FQEI$AE$AEIF$CDMHXZ" =>
        "public: int __clrcall C::get(void)const volatile __unaligned % __restrict __restrict");
}

#[test]
fn managed_extensions1_single3() {
    // $$H
    eq!("?main@@$$HYAHHQEAPEAD@Z" => "int __cdecl main(int, char * * const)");
}

#[test]
fn managed_extensions1_aa() {
    eq!("??0name0@name1@name2@name3@name4@@$$FIE$AAM@PE$AAVname5@name6@name7@name8@@Vname9@678@@Z" =>
        "protected: __clrcall name4::name3::name2::name1::name0::name0(class name8::name7::name6::name5 ^, class name8::name7::name6::name9)");
}

#[test]
fn managed_extensions1_ac() {
    eq!("?get@C@@$$FQE$AAMHXZ" => "public: int __clrcall C::get(void)");
}

#[test]
fn managed_extensions1_ad() {
    // manufactured (has $C)
    eq!("?get@C@@$$FQ$CEIAMHXZ" => "public: int __clrcall C::get(void)% __restrict");
}

#[test]
fn managed_extensions1_ae() {
    // manufactured
    eq!("?get@C@@$$FQEI$AAMHXZ" => "public: int __clrcall C::get(void) __restrict");
}

#[test]
fn managed_extensions1_ae_add_throw() {
    // manufactured
    eq!("?get@C@@$$FQEI$AAMHXHH@" => "public: int __clrcall C::get(void) __restrict throw(int, int)");
}

#[test]
fn managed_extensions1_af() {
    // manufactured
    eq!("?get@C@@$$FQ$AEIAMHXZ" => "public: int __clrcall C::get(void) __restrict");
}

#[test]
fn managed_extensions1_ag() {
    // manufactured
    eq!("?get@C@@$$FQIE$AAMHXZ" => "public: int __clrcall C::get(void) __restrict");
}

#[test]
fn managed_extensions1_ai() {
    // manufactured
    eq!("?get@C@@$$FQE$AIAMHXZ" => "public: int __clrcall C::get(void) __restrict");
}

#[test]
fn managed_extensions1_aj() {
    // manufactured
    eq!("?get@C@@$$FQI$AEAMHXZ" => "public: int __clrcall C::get(void) __restrict");
}

#[test]
fn managed_extensions1_ak() {
    // manufactured
    eq!("?get@C@@$$FQE$AEAMHXZ" => "public: int __clrcall C::get(void)");
}

#[test]
fn managed_extensions1_al() {
    eq!("?get@C@@$$FQI$AIAMHXZ" => "public: int __clrcall C::get(void) __restrict __restrict");
}

#[test]
fn managed_extensions1_am() {
    eq!("?main@@$$HYAHHQEAPEAD@Z" => "int __cdecl main(int, char * * const)");
}

#[test]
fn managed_extensions1_ao() {
    eq!("?get@B@@$$FQEAAHXZ" => "public: int __cdecl B::get(void)");
}

#[test]
fn managed_extensions1_ap() {
    eq!("?get@C@@$$FQE$AAMHXZ" => "public: int __clrcall C::get(void)");
}

#[test]
fn managed_extensions1_aq() {
    // manufactured
    eq!("?get@C@@$$FQEIF$AFIEFAMHXZ" =>
        "public: int __clrcall C::get(void)__unaligned __unaligned __unaligned __restrict __restrict");
}

#[test]
fn managed_extensions1_ar() {
    eq!("??0C@@$$FQE$AAM@XZ" => "public: __clrcall C::C(void)");
}

#[test]
fn managed_extensions1_as() {
    eq!("?main@@$$HYAHHQEAPEAD@Z" => "int __cdecl main(int, char * * const)");
}

#[test]
fn managed_extensions1_at() {
    eq!("?useMe@@$$FYAHAEAPE$CAVB@@@Z" => "int __cdecl useMe(class B % &)");
}

#[test]
fn managed_extensions1_au() {
    eq!("?useMe@@YAHAEAPE$CAVB@@@Z" => "int __cdecl useMe(class B % &)");
}

#[test]
fn managed_extensions1_av() {
    eq!("?useMe2@@$$FYAHAE$CAVB@@@Z" => "int __cdecl useMe2(class B %)");
}

#[test]
fn managed_extensions1_aw() {
    eq!("?useMe2@@YAHAE$CAVB@@@Z" =>
        "int __cdecl useMe2(class B %)");
}

#[test]
fn managed_extensions1_ax() {
    eq!("?GetStream@FileBase@@$$FUE$AAMPEAHXZ" =>
        "public: virtual int * __clrcall FileBase::GetStream(void)");
}

#[test]
fn managed_extensions1_ay() {
    eq!("??0FileBase@@$$FQE$AAM@XZ" => "public: __clrcall FileBase::FileBase(void)");
}

#[test]
fn managed_extensions1_az() {
    eq!("??0FileDerived@@$$FQE$AAM@XZ" => "public: __clrcall FileDerived::FileDerived(void)");
}

#[test]
fn spacing1() {
    eq!("?Var@@3PEAHN5" => "int * ");
}

#[test]
fn spacing2() {
    eq!("?Var@@3PEDHN5" => "int const volatile * ");
}

#[test]
fn spacing3() {
    eq!("?Var@@3PEDHEIFN5" => "int const volatile * ");
}

#[test]
fn spacing4() {
    eq!("?Var@@3PEN5HA" => "int");
}

#[test]
fn spacing5() {
    eq!("?Var@@3PEN5HD" => "int");
}

#[test]
fn spacing6() {
    eq!("?foo@@QEAAXXZ" => "public: void __cdecl foo(void)");
}

#[test]
fn anonymous_namespace_backreference_a() {
    eq!("?var@abc@?Axyz@1@3HA" => "int abc::`anonymous namespace'::abc::var");
}

#[test]
fn anonymous_namespace_backreference_b() {
    eq!("?var@abc@?Axyz@2@3HA" => "int Axyz::`anonymous namespace'::abc::var");
}


#[test]
fn ghidra_file_info_basic_basic_000() {
    eq!("??_7exception@@6B@" => "const exception::`vftable'");
}

#[test]
fn ghidra_file_info_basic_001() {
    eq!("??_7bad_typeid@@6B@" => "const bad_typeid::`vftable'");
}

#[test]
fn ghidra_file_info_basic_002() {
    eq!("??_7__non_rtti_object@@6B@" => "const __non_rtti_object::`vftable'");
}

#[test]
fn ghidra_file_info_basic_003() {
    eq!("??_7bad_cast@@6B@" => "const bad_cast::`vftable'");
}

#[test]
fn ghidra_file_info_basic_004() {
    eq!("??3@YAXPAX@Z" => "void __cdecl operator delete(void *)");
}

#[test]
fn ghidra_file_info_basic_005() {
    eq!("??2@YAPAXI@Z" => "void * __cdecl operator new(unsigned int)");
}

#[test]
fn ghidra_file_info_basic_006() {
    eq!("??_V@YAXPAX@Z" => "void __cdecl operator delete[](void *)");
}

#[test]
fn ghidra_file_info_basic_007() {
    eq!("??_U@YAPAXI@Z" => "void * __cdecl operator new[](unsigned int)");
}

#[test]
fn ghidra_file_info_basic_008() {
    eq!("?_set_se_translator@@YAP6AXIPAU_EXCEPTION_POINTERS@@@ZP6AXI0@Z@Z" =>
        "void (__cdecl*__cdecl _set_se_translator(void (__cdecl*)(unsigned int, struct _EXCEPTION_POINTERS *)))(unsigned int, struct _EXCEPTION_POINTERS *)");
}

#[test]
fn ghidra_file_info_basic_009() {
    eq!("??1exception@@UAE@XZ" => "public: virtual __thiscall exception::~exception(void)");
}

#[test]
fn ghidra_file_info_basic_010() {
    eq!("??0exception@@QAE@XZ" => "public: __thiscall exception::exception(void)");
}

#[test]
fn ghidra_file_info_basic_011() {
    eq!("??0exception@@QAE@ABQBD@Z" => "public: __thiscall exception::exception(char const * const &)");
}

#[test]
fn ghidra_file_info_basic_012() {
    eq!("?_set_new_handler@@YAP6AHI@ZP6AHI@Z@Z" =>
        "int (__cdecl*__cdecl _set_new_handler(int (__cdecl*)(unsigned int)))(unsigned int)");
}

#[test]
fn ghidra_file_info_basic_013() {
    eq!("?_set_new_mode@@YAHH@Z" => "int __cdecl _set_new_mode(int)");
}

#[test]
fn ghidra_file_info_basic_014() {
    eq!("?set_terminate@@YAP6AXXZP6AXXZ@Z" => "void (__cdecl*__cdecl set_terminate(void (__cdecl*)(void)))(void)");
}

#[test]
fn ghidra_file_info_basic_015() {
    eq!("??8type_info@@QBEHABV0@@Z" => "public: int __thiscall type_info::operator==(class type_info const &)const ");
}

#[test]
fn ghidra_file_info_basic_016() {
    eq!("?name@type_info@@QBEPBDXZ" => "public: char const * __thiscall type_info::name(void)const ");
}

#[test]
fn ghidra_file_info_basic_017() {
    eq!("??0exception@@QAE@ABQBDH@Z" => "public: __thiscall exception::exception(char const * const &, int)");
}

#[test]
fn ghidra_file_info_basic_018() {
    eq!("??0exception@@QAE@ABV0@@Z" => "public: __thiscall exception::exception(class exception const &)");
}

#[test]
fn ghidra_file_info_basic_019() {
    eq!("??4exception@@QAEAAV0@ABV0@@Z" =>
        "public: class exception & __thiscall exception::operator=(class exception const &)");
}

#[test]
fn ghidra_file_info_basic_020() {
    eq!("?what@exception@@UBEPBDXZ" => "public: virtual char const * __thiscall exception::what(void)const ");
}

#[test]
fn ghidra_file_info_basic_021() {
    eq!("??0bad_cast@@QAE@ABV0@@Z" => "public: __thiscall bad_cast::bad_cast(class bad_cast const &)");
}

#[test]
fn ghidra_file_info_basic_022() {
    eq!("??1bad_cast@@UAE@XZ" => "public: virtual __thiscall bad_cast::~bad_cast(void)");
}

#[test]
fn ghidra_file_info_basic_023() {
    eq!("??0bad_cast@@QAE@ABQBD@Z" => "public: __thiscall bad_cast::bad_cast(char const * const &)");
}

#[test]
fn ghidra_file_info_basic_024() {
    eq!("??0bad_cast@@AAE@PBQBD@Z" => "private: __thiscall bad_cast::bad_cast(char const * const *)");
}

#[test]
fn ghidra_file_info_basic_025() {
    eq!("??0bad_typeid@@QAE@PBD@Z" => "public: __thiscall bad_typeid::bad_typeid(char const *)");
}

#[test]
fn ghidra_file_info_basic_026() {
    eq!("??0bad_typeid@@QAE@ABV0@@Z" => "public: __thiscall bad_typeid::bad_typeid(class bad_typeid const &)");
}

#[test]
fn ghidra_file_info_basic_027() {
    eq!("??0__non_rtti_object@@QAE@PBD@Z" => "public: __thiscall __non_rtti_object::__non_rtti_object(char const *)");
}

#[test]
fn ghidra_file_info_basic_028() {
    eq!("??0__non_rtti_object@@QAE@ABV0@@Z" =>
        "public: __thiscall __non_rtti_object::__non_rtti_object(class __non_rtti_object const &)");
}

#[test]
fn ghidra_file_info_basic_029() {
    eq!("??1bad_typeid@@UAE@XZ" => "public: virtual __thiscall bad_typeid::~bad_typeid(void)");
}

#[test]
fn ghidra_file_info_basic_030() {
    eq!("??1__non_rtti_object@@UAE@XZ" => "public: virtual __thiscall __non_rtti_object::~__non_rtti_object(void)");
}

#[test]
fn ghidra_file_info_basic_031() {
    eq!("??_Gexception@@UAEPAXI@Z" =>
        "public: virtual void * __thiscall exception::`scalar deleting destructor'(unsigned int)");
}

#[test]
fn ghidra_file_info_basic_032() {
    eq!("??_Eexception@@UAEPAXI@Z" =>
        "public: virtual void * __thiscall exception::`vector deleting destructor'(unsigned int)");
}

#[test]
fn ghidra_file_info_basic_033() {
    eq!("??0bad_cast@@QAE@PBD@Z" => "public: __thiscall bad_cast::bad_cast(char const *)");
}

#[test]
fn ghidra_file_info_basic_034() {
    eq!("??4bad_typeid@@QAEAAV0@ABV0@@Z" =>
        "public: class bad_typeid & __thiscall bad_typeid::operator=(class bad_typeid const &)");
}

#[test]
fn ghidra_file_info_basic_035() {
    eq!("??4bad_cast@@QAEAAV0@ABV0@@Z" =>
        "public: class bad_cast & __thiscall bad_cast::operator=(class bad_cast const &)");
}

#[test]
fn ghidra_file_info_basic_036() {
    eq!("??_Fbad_cast@@QAEXXZ" => "public: void __thiscall bad_cast::`default constructor closure'(void)");
}

#[test]
fn ghidra_file_info_basic_037() {
    eq!("??_Gbad_cast@@UAEPAXI@Z" =>
        "public: virtual void * __thiscall bad_cast::`scalar deleting destructor'(unsigned int)");
}

#[test]
fn ghidra_file_info_basic_038() {
    eq!("??_Ebad_cast@@UAEPAXI@Z" =>
        "public: virtual void * __thiscall bad_cast::`vector deleting destructor'(unsigned int)");
}

#[test]
fn ghidra_file_info_basic_039() {
    eq!("??_Fbad_typeid@@QAEXXZ" => "public: void __thiscall bad_typeid::`default constructor closure'(void)");
}

#[test]
fn ghidra_file_info_basic_040() {
    eq!("??_Gbad_typeid@@UAEPAXI@Z" =>
        "public: virtual void * __thiscall bad_typeid::`scalar deleting destructor'(unsigned int)");
}

#[test]
fn ghidra_file_info_basic_041() {
    eq!("??_G__non_rtti_object@@UAEPAXI@Z" =>
        "public: virtual void * __thiscall __non_rtti_object::`scalar deleting destructor'(unsigned int)");
}

#[test]
fn ghidra_file_info_basic_042() {
    eq!("??_Ebad_typeid@@UAEPAXI@Z" =>
        "public: virtual void * __thiscall bad_typeid::`vector deleting destructor'(unsigned int)");
}

#[test]
fn ghidra_file_info_basic_043() {
    eq!("??_E__non_rtti_object@@UAEPAXI@Z" =>
        "public: virtual void * __thiscall __non_rtti_object::`vector deleting destructor'(unsigned int)");
}

#[test]
fn ghidra_file_info_basic_044() {
    eq!("??4__non_rtti_object@@QAEAAV0@ABV0@@Z" =>
        "public: class __non_rtti_object & __thiscall __non_rtti_object::operator=(class __non_rtti_object const &)");
}

#[test]
fn ghidra_file_info_basic_045() {
    eq!("?set_unexpected@@YAP6AXXZP6AXXZ@Z" => "void (__cdecl*__cdecl set_unexpected(void (__cdecl*)(void)))(void)");
}

#[test]
fn ghidra_file_info_basic_046() {
    eq!("?terminate@@YAXXZ" => "void __cdecl terminate(void)");
}

#[test]
fn ghidra_file_info_basic_047() {
    eq!("?unexpected@@YAXXZ" => "void __cdecl unexpected(void)");
}

#[test]
fn ghidra_file_info_basic_048() {
    eq!("??1type_info@@UAE@XZ" => "public: virtual __thiscall type_info::~type_info(void)");
}

#[test]
fn ghidra_file_info_basic_049() {
    eq!("??9type_info@@QBEHABV0@@Z" =>
        "public: int __thiscall type_info::operator!=(class type_info const &)const ");
}

#[test]
fn ghidra_file_info_basic_050() {
    eq!("?before@type_info@@QBEHABV1@@Z" =>
        "public: int __thiscall type_info::before(class type_info const &)const ");
}

#[test]
fn ghidra_file_info_basic_051() {
    eq!("?raw_name@type_info@@QBEPBDXZ" =>
        "public: char const * __thiscall type_info::raw_name(void)const ");
}

#[test]
fn ghidra_file_info_basic_052() {
    eq!("?_query_new_handler@@YAP6AHI@ZXZ" =>
        "int (__cdecl*__cdecl _query_new_handler(void))(unsigned int)");
}

#[test]
fn ghidra_file_info_basic_053() {
    eq!("?_query_new_mode@@YAHXZ" => "int __cdecl _query_new_mode(void)");
}

#[test]
fn ghidra_file_info_basic_054() {
    eq!("?set_new_handler@@YAP6AXXZP6AXXZ@Z" =>
        "void (__cdecl*__cdecl set_new_handler(void (__cdecl*)(void)))(void)");
}

#[test]
fn ghidra_file_info_basic_055() {
    eq!("??_U@YAPAXIHPBDH@Z" => "void * __cdecl operator new[](unsigned int, int, char const *, int)");
}

#[test]
fn ghidra_file_info_basic_056() {
    eq!("??2@YAPAXIHPBDH@Z" => "void * __cdecl operator new(unsigned int, int, char const *, int)");
}

#[test]
fn ghidra_file_info_global_operators_001() {
    eq!("??2@YAPAXI@Z" => "void * __cdecl operator new(unsigned int)");
}

#[test]
fn ghidra_file_info_global_operators_002() {
    eq!("??3@YAXPAX@Z" => "void __cdecl operator delete(void *)");
}

#[test]
fn ghidra_file_info_global_operators_003() {
    eq!("??_U@YAPEAX_K@Z" => "void * __cdecl operator new[](unsigned __int64)");
}

#[test]
fn ghidra_file_info_strings_001() {
    eq!("??_C@_08JCCMCCIL@HH?3mm?3ss?$AA@" => "`string'");
}

#[test]
fn ghidra_file_info_strings_002() {
    eq!("??_C@_08EDHMEBNP@December?$AA@" => "`string'");
}

#[test]
fn ghidra_file_info_strings_003() {
    eq!("??_C@_08HCHEGEOA@November?$AA@" => "`string'");
}

#[test]
fn ghidra_file_info_strings_004() {
    eq!("??_C@_04MIEPOIFP@July?$AA@" => "`string'");
}

#[test]
fn ghidra_file_info_strings_005() {
    eq!("??_C@_03LBGABGKK@Jul?$AA@" => "`string'");
}

#[test]
fn ghidra_file_info_strings_006() {
    eq!("??_C@_0M@IDPNJOFL@TlsGetValue?$AA@" => "`string'");
}

#[test]
fn ghidra_file_info_strings_007() {
    eq!("??_C@_1BA@KFOBIOMM@?$AAT?$AAY?$AAP?$AAE?$AAL?$AAI?$AAB?$AA?$AA@" => "`string'");
}

#[test]
fn ghidra_file_info_strings_008() {
    eq!("??_C@_1M@KANJNLFF@?$AAC?$AAL?$AAS?$AAI?$AAD?$AA?$AA@" => "`string'");
}

#[test]
fn ghidra_file_info_strings_009() {
    eq!("??_C@_1O@JDLOHAN@?$AAD?$AAe?$AAl?$AAe?$AAt?$AAe?$AA?$AA@" => "`string'");
}

#[test]
fn ghidra_file_info_overridden_operator_001() {
    eq!("??5@YGAAVCArchive@@AAV0@AAPAVCGWTelMenuData@@@Z" =>
        "class CArchive & __stdcall operator>>(class CArchive &, class CGWTelMenuData * &)");
}

#[test]
fn ghidra_file_info_overridden_operator_002() {
    eq!("??_ECGWISUPInformation@@UAEPAXI@Z" =>
        "public: virtual void * __thiscall CGWISUPInformation::`vector deleting destructor'(unsigned int)");
}

#[test]
fn ghidra_file_info_overridden_operator_003() {
    eq!("??0CMuLawCodec@@QAE@XZ" => "public: __thiscall CMuLawCodec::CMuLawCodec(void)");
}

#[test]
fn ghidra_file_info_overridden_operator_004() {
    eq!("??1CMuLawCodec@@UAE@XZ" => "public: virtual __thiscall CMuLawCodec::~CMuLawCodec(void)");
}

#[test]
fn ghidra_file_info_overridden_operator_005() {
    eq!("??_GCGWCodec@@UAEPAXI@Z" =>
        "public: virtual void * __thiscall CGWCodec::`scalar deleting destructor'(unsigned int)");
}

#[test]
fn ghidra_file_info_overridden_operator_006() {
    eq!("??HFoo@@QAE?AV0@V0@@Z" => "public: class Foo __thiscall Foo::operator+(class Foo)");
}

#[test]
fn ghidra_file_info_overridden_operator_007() {
    eq!("??HFoo@@QAE?AV0@H@Z" => "public: class Foo __thiscall Foo::operator+(int)");
}

#[test]
fn ghidra_file_info_overridden_operator_008() {
    eq!("??HFoo@@QAE?AV0@VBar@@@Z" => "public: class Foo __thiscall Foo::operator+(class Bar)");
}

#[test]
fn ghidra_file_info_overridden_operator_009() {
    eq!("??HFoo@@QAE?AV0@PAU_RECTANGLE@@@Z" => "public: class Foo __thiscall Foo::operator+(struct _RECTANGLE *)");
}

#[test]
fn ghidra_file_info_overridden_operator_010() {
    eq!("??HFoo@@QAE?AV0@U_RECTANGLE@@@Z" => "public: class Foo __thiscall Foo::operator+(struct _RECTANGLE)");
}

#[test]
fn ghidra_file_info_overridden_operator_011() {
    eq!("??GFoo@@QAE?AV0@V0@@Z" => "public: class Foo __thiscall Foo::operator-(class Foo)");
}

#[test]
fn ghidra_file_info_overridden_operator_012() {
    eq!("??GFoo@@QAE?AV0@H@Z" => "public: class Foo __thiscall Foo::operator-(int)");
}

#[test]
fn ghidra_file_info_overridden_operator_013() {
    eq!("??GFoo@@QAE?AV0@W4MYENUM@@@Z" => "public: class Foo __thiscall Foo::operator-(enum MYENUM)");
}

#[test]
fn ghidra_file_info_overridden_operator_014() {
    eq!("??KFoo@@QAE?AV0@V0@@Z" => "public: class Foo __thiscall Foo::operator/(class Foo)");
}

#[test]
fn ghidra_file_info_overridden_operator_015() {
    eq!("??KFoo@@QAE?AV0@H@Z" => "public: class Foo __thiscall Foo::operator/(int)");
}

#[test]
fn ghidra_file_info_overridden_operator_016() {
    eq!("??8Foo@@QAE_NV0@@Z" => "public: bool __thiscall Foo::operator==(class Foo)");
}

#[test]
fn ghidra_file_info_overridden_operator_017() {
    eq!("??4Foo@@QAE?AV0@H@Z" => "public: class Foo __thiscall Foo::operator=(int)");
}

#[test]
fn ghidra_file_info_overridden_operator_018() {
    eq!("??4Foo@@QAEAAV0@ABV0@@Z" => "public: class Foo & __thiscall Foo::operator=(class Foo const &)");
}

#[test]
fn ghidra_file_info_overridden_operator_019() {
    eq!("??4Bar@@QAEAAV0@ABV0@@Z" => "public: class Bar & __thiscall Bar::operator=(class Bar const &)");
}

#[test]
fn ghidra_file_info_overridden_operator_020() {
    eq!("??6Foo@@QAEHH@Z" => "public: int __thiscall Foo::operator<<(int)");
}

#[test]
fn ghidra_file_info_overridden_operator_021() {
    eq!("??6Foo@@QAEHV0@@Z" => "public: int __thiscall Foo::operator<<(class Foo)");
}

#[test]
fn ghidra_file_info_overridden_operator_022() {
    eq!("??0Foo@@QAE@H@Z" => "public: __thiscall Foo::Foo(int)");
}

#[test]
fn ghidra_file_info_overridden_operator_023() {
    eq!("??0CFileDialog@@QAE@HPBG0K0PAVCWnd@@@Z" =>
        "public: __thiscall CFileDialog::CFileDialog(int, unsigned short const *, unsigned short const *, unsigned long, unsigned short const *, class CWnd *)");
}

#[test]
fn ghidra_file_info_vfvb_000() {
    eq!("??_7CComClassFactory@ATL@@6B@" => "const ATL::CComClassFactory::`vftable'");
}

#[test]
fn ghidra_file_info_vfvb_001() {
    eq!("??_7CGWLineDirectorBase@@6B@" => "const CGWLineDirectorBase::`vftable'");
}

#[test]
fn ghidra_file_info_vfvb_002() {
    eq!("??_8CGWTelMenuData@@7BCGWTelSelectionData@@@" =>
        "const CGWTelMenuData::`vbtable'{for `CGWTelSelectionData'}");
}

#[test]
fn ghidra_file_info_vfvb_003() {
    eq!("??_8CWebDVDComp@@7B@" => "const CWebDVDComp::`vbtable'");
}

#[test]
fn ghidra_file_info_class_methods_000() {
    eq!("?getFoo@Foo@@QAE?AV1@XZ" => "public: class Foo __thiscall Foo::getFoo(void)");
}

#[test]
fn ghidra_file_info_class_methods_001() {
    eq!("?getBar@Foo@@QAE?AVBar@@XZ" => "public: class Bar __thiscall Foo::getBar(void)");
}

#[test]
fn ghidra_file_info_class_methods_002() {
    eq!("?getMyStruct@Foo@@QAE?AU_S@@XZ" => "public: struct _S __thiscall Foo::getMyStruct(void)");
}

#[test]
fn ghidra_file_info_class_methods_003() {
    eq!("?getPMyStruct@Foo@@QAEPAU_S@@XZ" => "public: struct _S * __thiscall Foo::getPMyStruct(void)");
}

#[test]
fn ghidra_file_info_class_methods_004() {
    eq!("?getMyEnum@Foo@@QAE?AW4MYENUM@@XZ" => "public: enum MYENUM __thiscall Foo::getMyEnum(void)");
}

#[test]
fn ghidra_file_info_class_methods_005() {
    eq!("?getPMyEnum@Foo@@QAEPAW4MYENUM@@XZ" => "public: enum MYENUM * __thiscall Foo::getPMyEnum(void)");
}

#[test]
fn ghidra_file_info_class_methods_006() {
    eq!("?getMyUnion@Foo@@QAE?AT_U@@XZ" => "public: union _U __thiscall Foo::getMyUnion(void)");
}

#[test]
fn ghidra_file_info_class_methods_007() {
    eq!("?getPMyUnion@Foo@@QAEPAT_U@@XZ" => "public: union _U * __thiscall Foo::getPMyUnion(void)");
}

#[test]
fn ghidra_file_info_class_methods_008() {
    eq!("?what@exception@@UBEPBDXZ" => "public: virtual char const * __thiscall exception::what(void)const ");
}

#[test]
fn ghidra_file_info_class_methods_009() {
    eq!("?Init@CMuLawCodec@@UAEHH@Z" => "public: virtual int __thiscall CMuLawCodec::Init(int)");
}

#[test]
fn ghidra_file_info_class_methods_010() {
    eq!("?GetSamplesPerFrame@CGWCodec@@SAHD@Z" => "public: static int __cdecl CGWCodec::GetSamplesPerFrame(char)");
}

#[test]
fn ghidra_file_info_class_methods_011() {
    eq!("?getFloater@MyClass@@QAEPAPAPAPAMXZ" => "public: float * * * * __thiscall MyClass::getFloater(void)");
}

#[test]
fn ghidra_file_info_class_methods_012() {
    eq!("?OnGetCheckPosition@CCheckListBox@@UAE?AVCRect@@V2@0@Z" =>
        "public: virtual class CRect __thiscall CCheckListBox::OnGetCheckPosition(class CRect, class CRect)");
}

#[test]
fn ghidra_file_info_other_000() {
    eq!("??1MESSAGE_PROCESSOR_ID@CClarentMessageProcessor@@QAE@XZ" =>
        "public: __thiscall CClarentMessageProcessor::MESSAGE_PROCESSOR_ID::~MESSAGE_PROCESSOR_ID(void)");
}

#[test]
fn ghidra_file_info_other_001() {
    eq!("?ProcessClarentVersionCheckerAcknowledge@CClarentMessageProcessor@@UAE_NPAVCClarentVersionCheckerMessage@@@Z" =>
        "public: virtual bool __thiscall CClarentMessageProcessor::ProcessClarentVersionCheckerAcknowledge(class CClarentVersionCheckerMessage *)");
}

#[test]
fn ghidra_file_info_other_002() {
    eq!("??0CEndpointDlg@@QAE@VMESSAGE_PROCESSOR_ID@CClarentMessageProcessor@@PAVCMSSRemoveDialogRecipient@@@Z" =>
        "public: __thiscall CEndpointDlg::CEndpointDlg(class CClarentMessageProcessor::MESSAGE_PROCESSOR_ID, class CMSSRemoveDialogRecipient *)");
}

#[test]
fn ghidra_file_info_static_functions_000() {
    eq!("?printError@@YAXXZ" => "void __cdecl printError(void)");
}

#[test]
fn ghidra_file_info_static_functions_001() {
    eq!("?SpAlloc@@YAXHPAK0@Z" => "void __cdecl SpAlloc(int, unsigned long *, unsigned long *)");
}

#[test]
fn ghidra_file_info_static_functions_002() {
    eq!("?SpClearHBreakpoint@@YAXIIKH@Z" =>
        "void __cdecl SpClearHBreakpoint(unsigned int, unsigned int, unsigned long, int)");
}

#[test]
fn ghidra_file_info_static_functions_003() {
    eq!("?SpClearSBreakpoint@@YAXIIKPAEHEE@Z" =>
        "void __cdecl SpClearSBreakpoint(unsigned int, unsigned int, unsigned long, unsigned char *, int, unsigned char, unsigned char)");
}

#[test]
fn ghidra_file_info_static_functions_004() {
    eq!("?SpClearSingleStep@@YAXIIPAI@Z" =>
        "void __cdecl SpClearSingleStep(unsigned int, unsigned int, unsigned int *)");
}

#[test]
fn ghidra_file_info_static_functions_005() {
    eq!("?SpCtrlSetFlags@@YAXII@Z" => "void __cdecl SpCtrlSetFlags(unsigned int, unsigned int)");
}

#[test]
fn ghidra_file_info_static_functions_006() {
    eq!("?SpDetachTarget@@YAJI@Z" => "long __cdecl SpDetachTarget(unsigned int)");
}

#[test]
fn ghidra_file_info_static_functions_007() {
    eq!("?SpFree@@YAXHPAK0@Z" => "void __cdecl SpFree(int, unsigned long *, unsigned long *)");
}

#[test]
fn ghidra_file_info_static_functions_008() {
    eq!("?SpGetContextThread@@YAXIIIPAU_CONTEXT@NT@@I@Z" => "void __cdecl SpGetContextThread(unsigned int, unsigned int, unsigned int, struct NT::_CONTEXT *, unsigned int)");
}

#[test]
fn ghidra_file_info_static_functions_009() {
    eq!("?SpGetProcesses@@YAPAU_SYSTEM_PROCESS_INFORMATION@NT@@PAH@Z" =>
        "struct NT::_SYSTEM_PROCESS_INFORMATION * __cdecl SpGetProcesses(int *)");
}

#[test]
fn ghidra_file_info_static_functions_010() {
    eq!("?SpGetProcessHandle@@YAXKPAPAX@Z" => "void __cdecl SpGetProcessHandle(unsigned long, void * *)");
}

#[test]
fn ghidra_file_info_static_functions_011() {
    eq!("?SpGetProcessName@@YAXKPAXPAE@Z" => "void __cdecl SpGetProcessName(unsigned long, void *, unsigned char *)");
}

#[test]
fn ghidra_file_info_static_functions_012() {
    eq!("?SpGetProcessNameHandle@@YAXPAU_SYSTEM_PROCESS_INFORMATION@NT@@PAGPAPAX@Z" =>
        "void __cdecl SpGetProcessNameHandle(struct NT::_SYSTEM_PROCESS_INFORMATION *, unsigned short *, void * *)");
}

#[test]
fn ghidra_file_info_static_functions_013() {
    eq!("?SpGetRegSys@@YAXIHPAT_LARGE_INTEGER@@I@Z" => "void __cdecl SpGetRegSys(unsigned int, int, union _LARGE_INTEGER *, unsigned int)");
}

#[test]
fn ghidra_file_info_static_functions_014() {
    eq!("?SpGetThreadHandle@@YAXKKPAPAX@Z" => "void __cdecl SpGetThreadHandle(unsigned long, unsigned long, void * *)");
}

#[test]
fn ghidra_file_info_static_functions_015() {
    eq!("?SpInitTargetAsDLL@@YAJI@Z" => "long __cdecl SpInitTargetAsDLL(unsigned int)");
}

#[test]
fn ghidra_file_info_static_functions_016() {
    eq!("?SpKillTarget@@YAJI@Z" => "long __cdecl SpKillTarget(unsigned int)");
}

#[test]
fn ghidra_file_info_static_functions_017() {
    eq!("?SpReadMemory@@YAHHKPAEH@Z" => "int __cdecl SpReadMemory(int, unsigned long, unsigned char *, int)");
}

#[test]
fn ghidra_file_info_static_functions_018() {
    eq!("?SpSetContextThread@@YAXIIIPAU_CONTEXT@NT@@I@Z" =>
        "void __cdecl SpSetContextThread(unsigned int, unsigned int, unsigned int, struct NT::_CONTEXT *, unsigned int)");
}

#[test]
fn ghidra_file_info_static_functions_019() {
    eq!("?SpSetHBreakpoint@@YAXIIKHHHH@Z" =>
        "void __cdecl SpSetHBreakpoint(unsigned int, unsigned int, unsigned long, int, int, int, int)");
}

#[test]
fn ghidra_file_info_static_functions_020() {
    eq!("?SpSetRegSys@@YAXIHPAT_LARGE_INTEGER@@I@Z" =>
        "void __cdecl SpSetRegSys(unsigned int, int, union _LARGE_INTEGER *, unsigned int)");
}

#[test]
fn ghidra_file_info_static_functions_021() {
    eq!("?SpSetSBreakpoint@@YAXIIKKPAE0HEE@Z" =>
        "void __cdecl SpSetSBreakpoint(unsigned int, unsigned int, unsigned long, unsigned long, unsigned char *, unsigned char *, int, unsigned char, unsigned char)");
}

#[test]
fn ghidra_file_info_static_functions_022() {
    eq!("?SpSetSingleStep@@YAXIIPAI@Z" => "void __cdecl SpSetSingleStep(unsigned int, unsigned int, unsigned int *)");
}

#[test]
fn ghidra_file_info_static_functions_023() {
    eq!("?SpWriteMemory@@YAHHKPAEH@Z" => "int __cdecl SpWriteMemory(int, unsigned long, unsigned char *, int)");
}

#[test]
fn ghidra_file_info_static_functions_024() {
    eq!("?SpGetProcessHandle@@YAXKPAPAX@Z" => "void __cdecl SpGetProcessHandle(unsigned long, void * *)");
}

#[test]
fn ghidra_file_info_static_functions_025() {
    eq!("?DftSetPid@@YAXH@Z" => "void __cdecl DftSetPid(int)");
}

#[test]
fn ghidra_file_info_static_functions_026() {
    eq!("?fseal@@YAKKKK@Z" =>
        "unsigned long __cdecl fseal(unsigned long, unsigned long, unsigned long)");
}

#[test]
fn ghidra_file_info_class_variables_000() {
    eq!("?_pModule@ATL@@3PAVCComModule@1@A" => "class ATL::CComModule * ATL::_pModule");
}

#[test]
fn ghidra_file_info_class_variables_001() {
    eq!("?wndTop@CWnd@@2V1@B" => "public: static class CWnd const CWnd::wndTop");
}

#[test]
fn ghidra_file_info_global_variables_000() {
    eq!("?gl@@1JA" => "protected: static long gl");
}

#[test]
fn ghidra_file_info_global_variables_001() {
    eq!("?foo@@3JA" => "long foo");
}

#[test]
fn ghidra_file_info_global_variables_002() {
    eq!("?gl@@3JA" => "long gl");
}

#[test]
fn ghidra_file_info_global_variables_003() {
    eq!("?bar@@3EA" => "unsigned char bar");
}

#[test]
fn ghidra_file_info_global_variables_004() {
    eq!("?roundconst@@3KA" => "unsigned long roundconst");
}

#[test]
fn ghidra_file_info_global_variables_005() {
    eq!("?weak_key_lsb@@3PAEA" => "unsigned char * weak_key_lsb");
}

#[test]
fn ghidra_file_info_global_variables_006() {
    eq!("?m_libid@CComModule@ATL@@2U_GUID@@A" => "public: static struct _GUID ATL::CComModule::m_libid");
}

#[test]
fn ghidra_file_info_ftti_000() {
    eq!("??_R0PAX@8" => "void * `RTTI Type Descriptor'");
}

#[test]
fn ghidra_file_info_rtti_001() {
    eq!("??_R0PAVCException@BOB@@@8" => "class BOB::CException * `RTTI Type Descriptor'");
}

#[test]
fn ghidra_file_info_rtti_002() {
    eq!("??_R0?PAVCOleException@@@8" =>
        "class COleException const volatile __based() `RTTI Type Descriptor'");
}

#[test]
fn ghidra_file_info_rtti_003() {
    eq!("??_R0?AVCToolBarCtrl@@@8" => "class CToolBarCtrl `RTTI Type Descriptor'");
}

#[test]
fn ghidra_file_info_rtti_004() {
    eq!("??_R1ABCP@?40A@A@_AFX_CTL3D_STATE@@8" =>
        "_AFX_CTL3D_STATE::A::`RTTI Base Class Descriptor at (303, -5, 1, 0)'");
}

#[test]
fn ghidra_file_info_rtti_005() {
    eq!("??_R1A@?0A@A@_AFX_CTL3D_STATE@@8" =>
        "_AFX_CTL3D_STATE::`RTTI Base Class Descriptor at (0, -1, 0, 0)'");
}

#[test]
fn ghidra_file_info_rtti_006() {
    eq!("??_R1ABCP@?0A@A@_AFX_CTL3D_STATE@@8" =>
        "_AFX_CTL3D_STATE::`RTTI Base Class Descriptor at (303, -1, 0, 0)'");
}

#[test]
fn ghidra_file_info_rtti_007() {
    eq!("??_R1ABCP@?0FGHJKL@A@_AFX_CTL3D_STATE@@8" =>
        "_AFX_CTL3D_STATE::`RTTI Base Class Descriptor at (303, -1, 5667243, 0)'");
}

#[test]
fn ghidra_file_info_rtti_008() {
    eq!("??_R1ABCP@?0FGHJKL@MNOP@_AFX_CTL3D_STATE@@8" =>
        "_AFX_CTL3D_STATE::`RTTI Base Class Descriptor at (303, -1, 5667243, 52719)'");
}

#[test]
fn ghidra_file_info_rtti_009() {
    eq!("??_R1A@?0A@A@CEnumUnknown@@8" => "CEnumUnknown::`RTTI Base Class Descriptor at (0, -1, 0, 0)'");
}

#[test]
fn ghidra_file_info_rtti_010() {
    eq!("??_R2CStatic@@8" => "CStatic::`RTTI Base Class Array'");
}

#[test]
fn ghidra_file_info_rtti_011() {
    eq!("??_R2CTabCtrl@@8" => "CTabCtrl::`RTTI Base Class Array'");
}

#[test]
fn ghidra_file_info_rtti_012() {
    eq!("??_R2CTreeCtrl@@8" => "CTreeCtrl::`RTTI Base Class Array'");
}

#[test]
fn ghidra_file_info_rtti_013() {
    eq!("??_R2XOleIPFrame@COleControlContainer@@8" =>
        "COleControlContainer::XOleIPFrame::`RTTI Base Class Array'");
}

#[test]
fn ghidra_file_info_rtti_014() {
    eq!("??_R2XRowsetNotify@COleControlSite@@8" =>
        "COleControlSite::XRowsetNotify::`RTTI Base Class Array'");
}

#[test]
fn ghidra_file_info_rtti_015() {
    eq!("??_R3_AFX_THREAD_STATE@@8" => "_AFX_THREAD_STATE::`RTTI Class Hierarchy Descriptor'");
}

#[test]
fn ghidra_file_info_rtti_016() {
    eq!("??_R3CClientDC@@8" => "CClientDC::`RTTI Class Hierarchy Descriptor'");
}

#[test]
fn ghidra_file_info_rtti_017() {
    eq!("??_R3CMenu@@8" => "CMenu::`RTTI Class Hierarchy Descriptor'");
}

#[test]
fn ghidra_file_info_rtti_019() {
    eq!("??_R3XOleClientSite@COleControlSite@@8" =>
        "COleControlSite::XOleClientSite::`RTTI Class Hierarchy Descriptor'");
}

#[test]
fn ghidra_file_info_rtti_020() {
    eq!("??_R4_AFX_CTL3D_THREAD@@6B@" => "const _AFX_CTL3D_THREAD::`RTTI Complete Object Locator'");
}

#[test]
fn ghidra_file_info_rtti_021() {
    eq!("??_R4CDocManager@@6B@" => "const CDocManager::`RTTI Complete Object Locator'");
}

#[test]
fn ghidra_file_info_rtti_022() {
    eq!("??_R4istream_withassign@@6B@" =>
        "const istream_withassign::`RTTI Complete Object Locator'");
}

#[test]
fn ghidra_file_info_rtti_023() {
    eq!("??_R4XAmbientProps@COleControlSite@@6B@" =>
        "const COleControlSite::XAmbientProps::`RTTI Complete Object Locator'");
}

#[test]
fn ghidra_file_info_rtti_024() {
    eq!("??_R4XNotifyDBEvents@COleControlSite@@6B@" =>
        "const COleControlSite::XNotifyDBEvents::`RTTI Complete Object Locator'");
}

#[test]
fn ghidra_file_info_function_pointers_000() {
    eq!("?_query_new_handler@@YAP6AHI@ZXZ" => "int (__cdecl*__cdecl _query_new_handler(void))(unsigned int)");
}

#[test]
fn ghidra_file_info_function_pointers_001() {
    eq!("?_pnhHeap@@3P6AHI@ZA" => "int (__cdecl* _pnhHeap)(unsigned int)");
}

#[test]
fn ghidra_file_info_function_pointers_002() {
    eq!("?__pInconsistency@@3P6AXXZA" => "void (__cdecl* __pInconsistency)(void)");
}

#[test]
fn ghidra_file_info_arrays_000() {
    eq!("?FirstRxPacket@@3PAY0IA@EA" => "unsigned char (* FirstRxPacket)[128]");
}

#[test]
fn ghidra_file_info_arrays_001() {
    eq!("?acTPSTNCallContextsArray@@3PAY11BAA@UacTPSTNCallContext@@A" => "struct acTPSTNCallContext (* acTPSTNCallContextsArray)[2][256]");
}

#[test]
fn ghidra_file_info_arrays_002() {
    eq!("??_L@YGXPAXIHP6EX0@Z1@Z" =>
        "void __stdcall `eh vector constructor iterator'(void *, unsigned int, int, void (__thiscall*)(void *), void (__thiscall*)(void *))");
}

#[test]
fn ghidra_file_info_arrays_003() {
    eq!("??_M@YGXPAXIHP6EX0@Z@Z" => "void __stdcall `eh vector destructor iterator'(void *, unsigned int, int, void (__thiscall*)(void *))");
}

#[test]
fn ghidra_file_info_arrays_004() {
    eq!("?GetSuperWndProcAddr@CWnd@@MAEPAP6GJPAUHWND__@@IIJ@ZXZ" =>
        "protected: virtual long (__stdcall** __thiscall CWnd::GetSuperWndProcAddr(void))(struct HWND__ *, unsigned int, unsigned int, long)");
}

#[test]
fn ghidra_file_info_function_pointer_parameter_000() {
    eq!("??0CWinThread@@QAE@P6AIPAX@Z0@Z" => "public: __thiscall CWinThread::CWinThread(unsigned int (__cdecl*)(void *), void *)");
}

#[test]
fn ghidra_file_info_function_pointer_parameter_001() {
    eq!("?register_callback@ios_base@std@@QAEXP6AXW4event@12@AAV12@H@ZH@Z" =>
        "public: void __thiscall std::ios_base::register_callback(void (__cdecl*)(enum std::ios_base::event, class std::ios_base &, int), int)");
}

#[test]
fn ghidra_file_info_function_pointer_parameter_002() {
    eq!("?__ArrayUnwind@@YGXPAXIHP6EX0@Z@Z" => "void __stdcall __ArrayUnwind(void *, unsigned int, int, void (__thiscall*)(void *))");
}

#[test]
fn ghidra_file_info_templates_000() {
    eq!("??4?$_CIP@UIBindHost@@$1?IID_IBindHost@@3U_GUID@@B@@QAEAAV0@PAUIBindHost@@@Z" =>
        "public: class _CIP<struct IBindHost, &struct _GUID const IID_IBindHost> & __thiscall _CIP<struct IBindHost, &struct _GUID const IID_IBindHost>::operator=(struct IBindHost *)");
}

#[test]
fn ghidra_file_info_templates_001() {
    eq!("?_Clocptr@_Locimp@locale@std@@0PAV123@A" => "private: static class std::locale::_Locimp * std::locale::_Locimp::_Clocptr");
}

#[test]
fn ghidra_file_info_templates_002() {
    eq!("??0_Locinfo@std@@QAE@ABV01@@Z" => "public: __thiscall std::_Locinfo::_Locinfo(class std::_Locinfo const &)");
}

#[test]
fn this_pointer_modifiers_a() {
    eq!("?fn@@AAAHH@Z" => "private: int __cdecl fn(int)");
}

#[test]
fn this_pointer_modifiers_b() {
    eq!("?fn@@ABAHH@Z" => "private: int __cdecl fn(int)const ");
}

#[test]
fn this_pointer_modifiers_c() {
    eq!("?fn@@ACAHH@Z" => "private: int __cdecl fn(int)volatile ");
}

#[test]
fn this_pointer_modifiers_d() {
    eq!("?fn@@ADAHH@Z" => "private: int __cdecl fn(int)const volatile ");
}

#[test]
fn this_pointer_modifiers_ed() {
    eq!("?fn@@AEDAHH@Z" => "private: int __cdecl fn(int)const volatile");
}

#[test]
fn this_pointer_modifiers_id() {
    eq!("?fn@@AFDAHH@Z" => "private: int __cdecl fn(int)const volatile __unaligned ");
}

#[test]
fn this_pointer_modifiers_fd() {
    eq!("?fn@@AIDAHH@Z" => "private: int __cdecl fn(int)const volatile __restrict");
}

#[test]
fn this_pointer_modifiers_efid() {
    eq!("?fn@@AEFIDAHH@Z" => "private: int __cdecl fn(int)const volatile __unaligned __restrict");
}

#[test]
fn this_pointer_modifiers_ga() {
    eq!("?fn@@AGAAHH@Z" => "private: int __cdecl fn(int)& ");
}

#[test]
fn this_pointer_modifiers_ha() {
    eq!("?fn@@AHAAHH@Z" => "private: int __cdecl fn(int)&& ");
}

#[test]
fn this_pointer_modifiers_gha() {
    eq!("?fn@@AGHAAHH@Z" => "private: int __cdecl fn(int)& && ");
}

#[test]
fn this_pointer_modifiers_efghid() {
    eq!("?fn@@AEFGHIDAHH@Z" => "private: int __cdecl fn(int)const volatile __unaligned __restrict& && ");
}

#[test]
fn this_pointer_modifiers_eeffgghhiid() {
    eq!("?fn@@AEEFFGGHHIIDAHH@Z" => "private: int __cdecl fn(int)const volatile __unaligned __unaligned __restrict __restrict& && ");
}

#[test]
fn this_pointer_modifiers_eeffgghhiidollar_cd() {
    eq!("?fn@@AEEFFGGHHII$CDAHH@Z" => "private: int __cdecl fn(int)const volatile __unaligned __unaligned %  __restrict __restrict& && ");
}

#[test]
fn _rtti0_datatype_string_char_pointer() {
    eq!(".PEAD" => "char *");
}

#[test]
fn _rtti0_datatype_string_int() {
    eq!(".H" => "int");
}

#[test]
fn _rtti0_datatype_string_class_pointer() {
    eq!(".PAVBugaboo@@" => "class Bugaboo *");
}

//Has "$$Q" data type (kind of like a reference)
#[test]
fn dollar_dollar_qwith_dollar_a() {
    eq!("?var@@3$$Q$AAHA" => "int % var");
}

//Has "$$Q" data type (kind of like a reference)
#[test]
fn dollar_dollar_qwith_dollar_b() {
    eq!("?var@@3$$Q$BAHA" => "cli::pin_ptr<int && var");
}

//Has "$$Q" data type (kind of like a reference)
#[test]
fn dollar_dollar_qwith_dollar_c() {
    eq!("?var@@3$$Q$CAHA" => "int % var");
}

//Has "$$Q" data type (kind of like a reference)
#[test]
fn dollar_dollar_qwith_function_type() {
    eq!("?fn@@3$$Q6AHH@ZA" => "int (__cdecl&& fn)(int)");
}

//Has "$$Q" MDDataReferenceType as regular type.
#[test]
fn dollar_dollar_qas_regular_type() {
    eq!("?var@@3$$QAHA" => "int && var");
}

//Has "$$R" MDDataReferenceType as regular type.
#[test]
fn dollar_dollar_ras_regular_type() {
    eq!("?var@@3$$RAHA" => "int && var");
}

//Has "$$Q" MDDataReferenceType as regular type.
#[test]
fn dollar_dollar_qas_regular_type_with_const_volatile_const_volatile() {
    eq!("?var@@3$$QDHD" => "int const volatile && const volatile var");
}

//Has "$$Q" MDDataReferenceType as function arg.
#[test]
fn dollar_dollar_qas_function_arg() {
    eq!("?fn@@YAH$$QAH@Z" => "int __cdecl fn(int &&)");
}

//Has "$$R" MDDataReferenceType as function arg.
#[test]
fn dollar_dollar_ras_function_arg() {
    eq!("?fn@@YAH$$RAH@Z" => "int __cdecl fn(int && volatile)");
}

//Has "$$Q" MDDataReferenceType as function arg.
#[test]
fn dollar_dollar_qas_function_arg_with_const_volatile() {
    eq!("?fn@@YAH$$QDH@Z" => "int __cdecl fn(int const volatile &&)");
}

//Has "$$R" MDDataReferenceType as function arg.
#[test]
fn dollar_dollar_ras_function_arg_with_const_volatile() {
    eq!("?fn@@YAH$$RDH@Z" => "int __cdecl fn(int const volatile && volatile)");
}

#[test]
fn pointerwith_dollar_a() {
    eq!("?var@@3P$AAHA" => "int ^ var");
}

#[test]
fn pointerwith_dollar_b() {
    eq!("?var@@3P$BAHA" => "cli::pin_ptr<int * var");
}

#[test]
fn pointerwith_dollar_c() {
    eq!("?var@@3P$CAHA" => "int % var");
}

#[test]
fn referencewith_dollar_a() {
    eq!("?var@@3A$AAHA" => "int % var");
}

#[test]
fn referencewith_dollar_b() {
    eq!("?var@@3A$BAHA" => "cli::pin_ptr<int & var");
}

#[test]
fn referencewith_dollar_c() {
    eq!("?var@@3A$CAHA" => "int % var");
}

#[test]
fn questionwith_dollar_a() {
    eq!("?var@@3?$AAHA" => "int var");
}

#[test]
fn questionwith_dollar_b() {
    eq!("?var@@3?$BAHA" => "int var");
}

#[test]
fn questionwith_dollar_c() {
    eq!("?var@@3?$CAHA" => "int % var");
}

#[test]
fn arraywith_dollar_a() {
    eq!("?var@@3_O$AAHA" => "int var[]");
}

#[test]
fn arraywith_dollar_c() {
    eq!("?var@@3_O$CAHA" => "int var[]");
}
#[test]
fn jan_gray_0() {
    eq!("__ehhandler$?test_except_f@@YAXH@Z" => "__ehhandler$?test_except_f@@YAXH@Z");
}

#[test]
fn jan_gray_0_breakdown1() {
    eq!("?test_except_f@@YAXH@Z" => "void __cdecl test_except_f(int)");
}

#[test]
fn win10_6798753_breakdown1() {
    eq!("?_Xlen@?$vector@X@std@@KAXXZ" => "protected: static void __cdecl std::vector<void>::_Xlen(void)");
}

#[test]
fn win10_6798753_breakdown2() {
    eq!("?_Xlen@?$vector@UVolumeWarning@@V?$allocator@UVolumeWarning@@@std@@@std@@KAXXZ" =>
        "protected: static void __cdecl std::vector<struct VolumeWarning, class std::allocator<struct VolumeWarning> >::_Xlen(void)");
}

#[test]
fn win10_6798753_breakdown3() {
    eq!("?$vector@UVolumeWarning@@" => "vector<struct VolumeWarning>");
}

#[test]
fn win10_6798753_breakdown4() {
    eq!("?$vector@UVolumeWarning@?BL@@" => "vector<struct `27'::VolumeWarning>");
}

#[test]
fn vcamp110msvc_1() {
    eq!("??$_Uninit_move@PEAU?$pair@PEAU_View_info@details@Concurrency@@_N@std@@PEAU12@V?$allocator@U?$pair@PEAU_View_info@details@Concurrency@@_N@std@@@2@U12@@std@@YAPEAU?$pair@PEAU_View_info@details@Concurrency@@_N@0@PEAU10@00AEAU?$_Wrap_alloc@V?$allocator@U?$pair@PEAU_View_info@details@Concurrency@@_N@std@@@std@@@0@0U_Nonscalar_ptr_iterator_tag@0@@Z" =>
        "struct std::pair<struct Concurrency::details::_View_info *, bool> * __cdecl std::_Uninit_move<struct std::pair<struct Concurrency::details::_View_info *, bool> *, struct std::pair<struct Concurrency::details::_View_info *, bool> *, class std::allocator<struct std::pair<struct Concurrency::details::_View_info *, bool> >, struct std::pair<struct Concurrency::details::_View_info *, bool> >(struct std::pair<struct Concurrency::details::_View_info *, bool> *, struct std::pair<struct Concurrency::details::_View_info *, bool> *, struct std::pair<struct Concurrency::details::_View_info *, bool> *, struct std::_Wrap_alloc<class std::allocator<struct std::pair<struct Concurrency::details::_View_info *, bool> > > &, struct std::pair<struct Concurrency::details::_View_info *, bool> *, struct std::_Nonscalar_ptr_iterator_tag)");
}

#[test]
fn wordpad_1_mod() {
    eq!("?CreateObject@?$CProcessLocal@V_AFX_EXTDLL_STATE@@@@SGPAVCNoTrackObject@@XZ" =>
        "public: static class CNoTrackObject * __stdcall CProcessLocal<class _AFX_EXTDLL_STATE>::CreateObject(void)");
}

#[test]
fn _cn2_1232_z7_1() {
    eq!("?fn4@Bar2@Foo2c@@QAE?AVBar1@2@XZ" => "public: class Foo2c::Bar1 __thiscall Foo2c::Bar2::fn4(void)");
}

#[test]
fn _cn2_1232_z7_2() {
    eq!("?fn2@Bar1@?6??Bar3@Foo6@@SAHXZ@SAHXZ" => "public: static int __cdecl `public: static int __cdecl Foo6::Bar3(void)'::`7'::Bar1::fn2(void)");
}

#[test]
fn _cn3_1232_z7_1() {
    eq!("?fn3@?2??Bar3@Foo2b@@SAHXZ@4HA" => "int `public: static int __cdecl Foo2b::Bar3(void)'::`3'::fn3");
}

#[test]
//This should be the nexted name for the previous test.
fn _cn3_1232_z7_2() {
    eq!("?Bar3@Foo2b@@SAHXZ" => "public: static int __cdecl Foo2b::Bar3(void)");
}

#[test]
fn templated_operator_0() {
    eq!("??$?6$0BE@@TextWriter@cxl@@QEAAAEAV01@AEAY0BE@$$CB_W@Z" => "public: class cxl::TextWriter & __cdecl cxl::TextWriter::operator<<<20>(wchar_t const (&)[20])");
}

#[test]
fn templated_operator_1() {
    eq!("??$?NDU?$char_traits@D@std@@V?$allocator@D@1@@std@@YA_NAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@0@0@Z" =>
        "bool __cdecl std::operator<=<char, struct std::char_traits<char>, class std::allocator<char>>(class std::basic_string<char, struct std::char_traits<char>, class std::allocator<char>> const &, class std::basic_string<char, struct std::char_traits<char>, class std::allocator<char> > const &)");
}

//From cn3.cpp
#[test]
fn templated_operator_2() {
    eq!("??$?6N@?$myContainer@H@@QAE_NN@Z" => "public: bool __thiscall myContainer<int>::operator<<<double>(double)");
}

#[test]
fn templated_operator_3() {
    eq!("??$?MV?$myContainer@H@@@@YA_NABV?$myContainer@H@@0@Z" =>
        "bool __cdecl operator<<class myContainer<int> >(class myContainer<int> const &, class myContainer<int> const &)");
}

// Following the model of MSFT Guard output strings even though the mangled form does not
// follow MSFT's scheme.  Change is that we are not outputting the extraneous tick as is seen
// in the middle of `local static guard'{2}', and we are not increasing the string value
// that is in braces by one from the coded "GuardNum" value.  Thus, we are outputting
// `thread safe static guard{0}' for "?$TSS0@".  We can reconsider this later.
#[test]
fn thread_safe_static_guard_1() {
    eq!("?$TSS0@?1??GetCategoryMap@CDynamicRegistrationInfoSource@XPerfAddIn@@SAPEBU_ATL_CATMAP_ENTRY@ATL@@XZ@4HA" =>
        "int `public: static struct ATL::_ATL_CATMAP_ENTRY const * __cdecl XPerfAddIn::CDynamicRegistrationInfoSource::GetCategoryMap(void)'::`2'::`thread safe static guard{0}'");
}

#[test]
fn simple_main_template_as_counterpoint_to_thread_safe_static_guard() {
    eq!("?$TSS0@HH" => "TSS0<int, int>");
}

// Manufactured by modifying "??_B?1??name0@name1@name2@@KAHPEBGAEAG@Z@51" which is a
//  `local static guard'{2}'.  We eliminated the closing 51 that makes it an MDGuard typeinfo
//  with value of 2 (1+1).  We are also eliminating the extraneous middle closing tick (single
//  quote) that MSFT has in their output.  We are not incrementing the value of ManglingNumber
//  that we are putting in braces (unlike other MSFT guard numbers). Thus, we will output
//  `nonvisible static guard{1}' for "?$S1@".  We can reconsider this later.  We also tacked
//  on the "4HA" as is done for the `thread safe static guard' so that it is an "int".
// TODO: Watch for real symbol of this type "?$S1@".
#[test]
fn nonvisible_static_guard() {
    eq!("?$S1@?1??name0@name1@name2@@KAHPEBGAEAG@Z@4HA" =>
        "int `protected: static int __cdecl name2::name1::name0(unsigned short const *, unsigned short &)'::`2'::`nonvisible static guard{1}'");
}

// Manufactured by modifying "??_B?1??name0@name1@name2@@KAHPEBGAEAG@Z@51" which is a
//  `local static guard'{2}'.  We eliminated the closing 51 that makes it an MDGuard typeinfo
//  with value of 2 (1+1).  We are also eliminating the extraneous middle closing tick (single
//  quote) that MSFT has in their output.  We are not incrementing the value of "number"
//  that we are putting in braces (unlike other MSFT guard numbers). Thus, we will output
//  `reference temporary{1}'" for the "?$RT1@".  We can reconsider this later.  We also tacked
//  on the "4HA" as is done for the `thread safe static guard' so that it is an "int".
// TODO: Watch for real symbol of this type "?$RTnum@".
#[test]
fn reference_temporary() {
    eq!("?$RT1@?1??name0@name1@name2@@KAHPEBGAEAG@Z@4HA" =>
        "int `protected: static int __cdecl name2::name1::name0(unsigned short const *, unsigned short &)'::`2'::`reference temporary{1}'");
}

#[test]
fn simple_main_template_as_counterpoint_to_reference_temporary() {
    eq!("?$RT1@HH" => "RT1<int, int>");
}

#[test]
fn hashed_symbol_components_longer_than4096_1() {
    eq!("??@f4873c94f485cd6716c2319fc51ac714@" => "`f4873c94f485cd6716c2319fc51ac714'");
}

#[test]
fn hashed_symbol_components_longer_than4096_2() {
    eq!("?catch$0@?0???@f4873c94f485cd6716c2319fc51ac714@@4HA" => "int ``f4873c94f485cd6716c2319fc51ac714''::`1'::catch$0");
}

// Contrived example to make sure that the nameModifier (pushed into MDBasicName) and
//  the recent addition, castTypeString (pushed to MDBasicName and below), play well
//  together.  It also shows, that they should probably both be considered separate
//  (i.e., do not use nameModifier to push in the castTypeString... we would have to
//  manage merging and multiple calls... does not make sense to even consider it).
// Note: the cast operator used to have the cast-to type emitted in MDFunctionType,
//  and me moved it to MDSpecialName.
#[test]
fn cast_operator_with_adjustor_modifier() {
    eq!("??Bname@@O7AAHXZ" => "[thunk]:protected: virtual __cdecl name::operator int`adjustor{8}' (void)");
}

// Contrived example.
#[test]
fn cast_operator_to_function_pointer() {
    eq!("??BClassName@@YAP6KXP6KXH@Z@ZXZ" => "__cdecl ClassName::operator void (*)(void (*)(int))(void)");
}

// Contrived example.
#[test]
fn reference_to_const_member_pointer_of_type_float_as_function_parameter() {
    eq!("?FnName@FnSpace@@YKXABPUClassName@@M@Z" => "void FnSpace::FnName(float ClassName::* const &)");
}

#[test]
fn function_indirect_with_blank_calling_convention() {
    eq!("?FN@@QAAH$$A6KH@Z@Z" => "public: int __cdecl FN(int ())");
}

#[test]
fn cppmanaged_ilmain_1() {
    eq!("?main@@$$HYAHXZ" => "int __cdecl main(void)");
}

// Not happy with this next thing.... might be CPPManagedILDLLImporData, but not yet sure.
#[test]
fn cppmanaged_ildllimport_data_1() {
    eq!("???__E?Initialized@CurrentDomain@<CrtImplementationDetails>@@$$Q2HA@@YMXXZ@?A0x1ed4f156@@$$FYMXXZ" =>
        "void __clrcall `dynamic initializer for 'public: static int <CrtImplementationDetails>::CurrentDomain::Initialized''(void)");
}


#[test]
fn cppmanaged_ilfunction_1() {
    eq!("?A0x1ed4f156.??__E?Initialized@CurrentDomain@<CrtImplementationDetails>@@$$Q2HA@@YMXXZ" => "unknown");
}

// Temporary counterpoint in trying to figure out the above.
#[test]
fn xxxcounterpoint2() {
    eq!(".?AV?$name1@Vname2@@Uname3@name4@@@name4@@" => "class name4::name1<class name2, struct name4::name3>");
}

// Temporary test for trying to fuzz solutions for the above.
#[test]
fn xxxfuzz() {
    eq!("??__E?Initialized@CurrentDomain@<CrtImplementationDetails>@@$$Q2HA@@YMXXZ" => "void __clrcall `dynamic initializer for 'public: static int <CrtImplementationDetails>::CurrentDomain::Initialized''(void)");
}
