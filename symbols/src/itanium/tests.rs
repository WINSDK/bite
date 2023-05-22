#![cfg(test)]

use super::ast::{
    ArrayType, BareFunctionType, BaseUnresolvedName, BuiltinType, CallOffset, ClassEnumType,
    ClosureTypeName, CtorDtorName, CvQualifiers, DataMemberPrefix, Decltype, DestructorName,
    Discriminator, Encoding, ExceptionSpec, ExprPrimary, Expression, FunctionParam, FunctionType,
    GlobalCtorDtor, Identifier, Initializer, LambdaSig, LocalName, MangledName, MemberName, Name,
    NestedName, NonSubstitution, Number, NvOffset, OperatorName, Parse, ParseContext,
    PointerToMemberType, Prefix, PrefixHandle, RefQualifier, ResourceName, SeqId, SimpleId,
    SimpleOperatorName, SourceName, SpecialName, StandardBuiltinType, SubobjectExpr, Substitution,
    TaggedName, TemplateArg, TemplateArgs, TemplateParam, TemplateTemplateParam,
    TemplateTemplateParamHandle, Type, TypeHandle, UnnamedTypeName, UnqualifiedName,
    UnresolvedName, UnresolvedQualifierLevel, UnresolvedType, UnresolvedTypeHandle, UnscopedName,
    UnscopedTemplateName, UnscopedTemplateNameHandle, VOffset, VectorType, WellKnownComponent,
};

use super::error::Error;
use super::index_str::IndexStr;
use super::subs::{Substitutable, SubstitutionTable};
use core::fmt::Debug;
use core::iter::FromIterator;

fn assert_parse_ok<P, S1, S2, I1, I2>(
    production: &'static str,
    subs: S1,
    input: I1,
    expected: P,
    expected_tail: I2,
    expected_new_subs: S2,
) where
    P: Debug + Parse + PartialEq,
    S1: AsRef<[Substitutable]>,
    S2: AsRef<[Substitutable]>,
    I1: AsRef<[u8]>,
    I2: AsRef<[u8]>,
{
    let ctx = ParseContext::new();
    let input = input.as_ref();
    let expected_tail = expected_tail.as_ref();

    let expected_subs = SubstitutionTable::from_iter(
        subs.as_ref().iter().cloned().chain(expected_new_subs.as_ref().iter().cloned()),
    );
    let mut subs = SubstitutionTable::from_iter(subs.as_ref().iter().cloned());

    match P::parse(&ctx, &mut subs, IndexStr::from(input)) {
        Err(error) => panic!(
            "Parsing {:?} as {} failed: {}",
            String::from_utf8_lossy(input),
            production,
            error
        ),
        Ok((value, tail)) => {
            if value != expected {
                panic!(
                    "Parsing {:?} as {} produced\n\n{:#?}\n\nbut we expected\n\n{:#?}",
                    String::from_utf8_lossy(input),
                    production,
                    value,
                    expected
                );
            }
            if tail != expected_tail {
                panic!(
                    "Parsing {:?} as {} left a tail of {:?}, expected {:?}",
                    String::from_utf8_lossy(input),
                    production,
                    tail,
                    String::from_utf8_lossy(expected_tail)
                );
            }
            if subs[..] != expected_subs[..] {
                panic!(
                    "Parsing {:?} as {} produced a substitutions table of\n\n\
                     {:#?}\n\n\
                     but we expected\n\n\
                     {:#?}",
                    String::from_utf8_lossy(input),
                    production,
                    subs,
                    expected_subs
                );
            }
        }
    }
}

fn simple_assert_parse_ok<P, I1, I2>(
    production: &'static str,
    input: I1,
    expected: P,
    expected_tail: I2,
) where
    P: Debug + Parse + PartialEq,
    I1: AsRef<[u8]>,
    I2: AsRef<[u8]>,
{
    assert_parse_ok::<P, _, _, _, _>(production, [], input, expected, expected_tail, []);
}

fn assert_parse_err<P, S, I>(production: &'static str, subs: S, input: I, expected_error: Error)
where
    P: Debug + Parse + PartialEq,
    S: AsRef<[Substitutable]>,
    I: AsRef<[u8]>,
{
    let input = input.as_ref();
    let ctx = ParseContext::new();
    let mut subs = SubstitutionTable::from_iter(subs.as_ref().iter().cloned());

    match P::parse(&ctx, &mut subs, IndexStr::from(input)) {
        Err(ref error) if *error == expected_error => {}
        Err(ref error) => {
            panic!(
                "Parsing {:?} as {} produced an error of kind {:?}, but we expected kind {:?}",
                String::from_utf8_lossy(input),
                production,
                error,
                expected_error
            );
        }
        Ok((value, tail)) => {
            panic!(
                "Parsing {:?} as {} produced value\
                 \n\n\
                 {:#?}\
                 \n\n\
                 and tail {:?}, but we expected error kind {:?}",
                String::from_utf8_lossy(input),
                production,
                value,
                tail,
                expected_error
            );
        }
    }
}

fn simple_assert_parse_err<P, I>(production: &'static str, input: I, expected_error: Error)
where
    P: Debug + Parse + PartialEq,
    I: AsRef<[u8]>,
{
    assert_parse_err::<P, _, _>(production, [], input, expected_error);
}

#[test]
fn recursion_limit() {
    // Build the mangled symbol for the type `*****char` where the "*****"
    // is 10,000 pointer indirections. This is a valid type symbol, but
    // something that would cause us to blow the stack.
    let mut mangled = String::new();
    for _ in 0..10_000 {
        mangled.push('P');
    }
    mangled += "c";

    simple_assert_parse_err::<TypeHandle, _>("TypeHandle", mangled, Error::TooMuchRecursion);
}

macro_rules! assert_parse {
    ( $production:ident {
        $( with subs $subs:expr => {
            Ok => {
                $( $input:expr => {
                    $expected:expr ,
                    $expected_tail:expr ,
                    $expected_new_subs:expr
                } )*
            }
            Err => {
                $( $error_input:expr => $error:expr , )*
            }
        } )*
    } ) => {
        $( $(
            assert_parse_ok::<$production, _, _, _, _>(stringify!($production),
                                                       $subs,
                                                       $input,
                                                       $expected,
                                                       $expected_tail,
                                                       $expected_new_subs);
        )* )*

        $( $(
            assert_parse_err::<$production, _, _>(stringify!($production),
                                                  $subs,
                                                  $error_input,
                                                  $error);
        )* )*
    };

    ( $production:ident {
        Ok => {
            $( $input:expr => {
                $expected:expr ,
                $expected_tail:expr
            } )*
        }
        Err => {
            $( $error_input:expr => $error:expr , )*
        }
    } ) => {
        $(
            simple_assert_parse_ok::<$production, _, _>(stringify!($production),
                                                        $input,
                                                        $expected,
                                                        $expected_tail);
        )*


        $(
            simple_assert_parse_err::<$production, _>(stringify!($production),
                                                      $error_input,
                                                      $error);
        )*
    };
}

#[test]
fn parse_mangled_name() {
    assert_parse!(MangledName {
        Ok => {
            b"_Z3foo..." => {
                MangledName::Encoding(
                    Encoding::Data(
                        Name::Unscoped(
                            UnscopedName::Unqualified(
                                UnqualifiedName::Source(
                                    SourceName(Identifier {
                                        start: 3,
                                        end: 6,
                                    }))))), vec![]),
                b"..."
            }
            b"_GLOBAL__I__Z3foo..." => {
                MangledName::GlobalCtorDtor(
                    GlobalCtorDtor::Ctor(
                        Box::new(
                            MangledName::Encoding(
                                Encoding::Data(
                                    Name::Unscoped(
                                        UnscopedName::Unqualified(
                                            UnqualifiedName::Source(
                                                SourceName(
                                                    Identifier {
                                                        start: 14,
                                                        end: 17,
                                                    }))))), vec![])))),
                b"..."
            }
        }
        Err => {
            b"_Y" => Error::UnexpectedText,
            b"_Z" => Error::UnexpectedEnd,
            b"_" => Error::UnexpectedEnd,
            b"" => Error::UnexpectedEnd,
            b"_GLOBAL_" => Error::UnexpectedEnd,
        }
    });
}

#[test]
fn parse_encoding() {
    assert_parse!(Encoding {
        with subs [] => {
            Ok => {
                b"3fooi..." => {
                    Encoding::Function(
                        Name::Unscoped(
                            UnscopedName::Unqualified(
                                UnqualifiedName::Source(
                                    SourceName(Identifier {
                                        start: 1,
                                        end: 4,
                                    })))),
                        BareFunctionType(vec![
                            TypeHandle::Builtin(BuiltinType::Standard(StandardBuiltinType::Int))
                        ])),
                    b"...",
                    []
                }
                b"3foo..." => {
                    Encoding::Data(
                        Name::Unscoped(
                            UnscopedName::Unqualified(
                                UnqualifiedName::Source(
                                    SourceName(Identifier {
                                        start: 1,
                                        end: 4,
                                    }))))),
                    b"...",
                    []
                }
                b"GV3abc..." => {
                    Encoding::Special(
                        SpecialName::Guard(
                            Name::Unscoped(
                                UnscopedName::Unqualified(
                                    UnqualifiedName::Source(
                                        SourceName(Identifier {
                                            start: 3,
                                            end: 6,
                                        })))))),
                    b"...",
                    []
                }
            }
            Err => {
                b"zzz" => Error::UnexpectedText,
                b"" => Error::UnexpectedEnd,
            }
        }
    });
}

#[test]
fn parse_global_ctor_dtor() {
    assert_parse!(GlobalCtorDtor {
        Ok => {
            b"_I__Z3foo..." => {
                GlobalCtorDtor::Ctor(
                    Box::new(
                        MangledName::Encoding(
                            Encoding::Data(
                                Name::Unscoped(
                                    UnscopedName::Unqualified(
                                        UnqualifiedName::Source(
                                            SourceName(
                                                Identifier {
                                                    start: 6,
                                                    end: 9,
                                                }))))), vec![]))),
                b"..."
            }
            b".I__Z3foo..." => {
                GlobalCtorDtor::Ctor(
                    Box::new(
                        MangledName::Encoding(
                            Encoding::Data(
                                Name::Unscoped(
                                    UnscopedName::Unqualified(
                                        UnqualifiedName::Source(
                                            SourceName(
                                                Identifier {
                                                    start: 6,
                                                    end: 9,
                                                }))))), vec![]))),
                b"..."
            }
            b"$I__Z3foo..." => {
                GlobalCtorDtor::Ctor(
                    Box::new(
                        MangledName::Encoding(
                            Encoding::Data(
                                Name::Unscoped(
                                    UnscopedName::Unqualified(
                                        UnqualifiedName::Source(
                                            SourceName(
                                                Identifier {
                                                    start: 6,
                                                    end: 9,
                                                }))))), vec![]))),
                b"..."
            }
            b"_D__Z3foo..." => {
                GlobalCtorDtor::Dtor(
                    Box::new(
                        MangledName::Encoding(
                            Encoding::Data(
                                Name::Unscoped(
                                    UnscopedName::Unqualified(
                                        UnqualifiedName::Source(
                                            SourceName(
                                                Identifier {
                                                    start: 6,
                                                    end: 9,
                                                }))))), vec![]))),
                b"..."
            }
            b".D__Z3foo..." => {
                GlobalCtorDtor::Dtor(
                    Box::new(
                        MangledName::Encoding(
                            Encoding::Data(
                                Name::Unscoped(
                                    UnscopedName::Unqualified(
                                        UnqualifiedName::Source(
                                            SourceName(
                                                Identifier {
                                                    start: 6,
                                                    end: 9,
                                                }))))), vec![]))),
                b"..."
            }
            b"$D__Z3foo..." => {
                GlobalCtorDtor::Dtor(
                    Box::new(
                        MangledName::Encoding(
                            Encoding::Data(
                                Name::Unscoped(
                                    UnscopedName::Unqualified(
                                        UnqualifiedName::Source(
                                            SourceName(
                                                Identifier {
                                                    start: 6,
                                                    end: 9,
                                                }))))), vec![]))),
                b"..."
            }
        }
        Err => {
            b"_I" => Error::UnexpectedEnd,
            b"_" => Error::UnexpectedEnd,
            b"" => Error::UnexpectedEnd,
            b"blag" => Error::UnexpectedText,
            b"_J" => Error::UnexpectedText,
            b"_IJ" => Error::UnexpectedText,
        }
    });
}

#[test]
fn parse_name() {
    assert_parse!(Name {
        with subs [
            Substitutable::Prefix(
                Prefix::Unqualified(
                    UnqualifiedName::Operator(OperatorName::Simple(SimpleOperatorName::New)))),
            Substitutable::Prefix(
                Prefix::Nested(PrefixHandle::BackReference(0),
                               UnqualifiedName::Operator(OperatorName::Simple(SimpleOperatorName::New)))),
        ] => {
            Ok => {
                b"NS0_3abcE..." => {
                    Name::Nested(NestedName::Unqualified(CvQualifiers::default(),
                                                         None,
                                                         PrefixHandle::BackReference(1),
                                                         UnqualifiedName::Source(SourceName(Identifier {
                                                             start: 5,
                                                             end: 8,
                                                         })))),
                    b"...",
                    []
                }
                b"3abc..." => {
                    Name::Unscoped(
                        UnscopedName::Unqualified(
                            UnqualifiedName::Source(
                                SourceName(Identifier {
                                    start: 1,
                                    end: 4,
                                })))),
                    b"...",
                    []
                }
                b"dlIcE..." => {
                    Name::UnscopedTemplate(
                        UnscopedTemplateNameHandle::BackReference(2),
                        TemplateArgs(vec![
                            TemplateArg::Type(
                                TypeHandle::Builtin(
                                    BuiltinType::Standard(StandardBuiltinType::Char)))
                        ])),
                    b"...",
                    [
                        Substitutable::UnscopedTemplateName(
                            UnscopedTemplateName(
                                UnscopedName::Unqualified(
                                    UnqualifiedName::Operator(
                                        OperatorName::Simple(
                                            SimpleOperatorName::Delete))))),
                    ]
                }
                b"Z3abcEs..." => {
                    Name::Local(
                        LocalName::Relative(
                            Box::new(Encoding::Data(
                                Name::Unscoped(
                                    UnscopedName::Unqualified(
                                        UnqualifiedName::Source(
                                            SourceName(Identifier {
                                                start: 2,
                                                end: 5,
                                            })))))),
                            None,
                            None)),
                    b"...",
                    []
                }
            }
            Err => {
                b"zzz" => Error::UnexpectedText,
                b"" => Error::UnexpectedEnd,
            }
        }
    });
}

#[test]
fn parse_unscoped_template_name_handle() {
    assert_parse!(UnscopedTemplateNameHandle {
        with subs [
            Substitutable::UnscopedTemplateName(
                UnscopedTemplateName(
                    UnscopedName::Unqualified(
                        UnqualifiedName::Operator(
                            OperatorName::Simple(
                                SimpleOperatorName::New))))),
        ] => {
            Ok => {
                b"S_..." => {
                    UnscopedTemplateNameHandle::BackReference(0),
                    b"...",
                    []
                }
                b"dl..." => {
                    UnscopedTemplateNameHandle::BackReference(1),
                    b"...",
                    [
                        Substitutable::UnscopedTemplateName(
                            UnscopedTemplateName(
                                UnscopedName::Unqualified(
                                    UnqualifiedName::Operator(
                                        OperatorName::Simple(
                                            SimpleOperatorName::Delete)))))
                    ]
                }
            }
            Err => {
                b"zzzz" => Error::UnexpectedText,
                b"" => Error::UnexpectedEnd,
            }
        }
    });
}

#[test]
fn parse_nested_name() {
    // <nested-name> ::= N [<CV-qualifiers>] [<ref-qualifier>] <prefix> <unqualified-name> E
    //               ::= N [<CV-qualifiers>] [<ref-qualifier>] <template-prefix> <template-args> E
    assert_parse!(NestedName {
        with subs [
            Substitutable::Prefix(
                Prefix::Unqualified(
                    UnqualifiedName::Operator(
                        OperatorName::Simple(
                            SimpleOperatorName::New)))),
        ] => {
            Ok => {
                b"NKOS_3abcE..." => {
                    NestedName::Unqualified(
                        CvQualifiers {
                            restrict: false,
                            volatile: false,
                            konst: true,
                        },
                        Some(RefQualifier::RValueRef),
                        PrefixHandle::BackReference(0),
                        UnqualifiedName::Source(
                            SourceName(Identifier {
                                start: 6,
                                end: 9,
                            }))),
                    b"...",
                    []
                }
                b"NOS_3abcE..." => {
                    NestedName::Unqualified(
                        CvQualifiers {
                            restrict: false,
                            volatile: false,
                            konst: false,
                        },
                        Some(RefQualifier::RValueRef),
                        PrefixHandle::BackReference(0),
                        UnqualifiedName::Source(
                            SourceName(Identifier {
                                start: 5,
                                end: 8,
                            }))),
                    b"...",
                    []
                }
                b"NS_3abcE..." => {
                    NestedName::Unqualified(
                        CvQualifiers {
                            restrict: false,
                            volatile: false,
                            konst: false,
                        },
                        None,
                        PrefixHandle::BackReference(0),
                        UnqualifiedName::Source(
                            SourceName(Identifier {
                                start: 4,
                                end: 7,
                            }))),
                    b"...",
                    []
                }
                b"NKOS_3abcIJEEE..." => {
                    NestedName::Template(
                        CvQualifiers {
                            restrict: false,
                            volatile: false,
                            konst: true,
                        },
                        Some(RefQualifier::RValueRef),
                        PrefixHandle::NonSubstitution(NonSubstitution(0))),
                    b"...",
                    [
                        Substitutable::Prefix(
                            Prefix::Nested(
                                PrefixHandle::BackReference(0),
                                UnqualifiedName::Source(
                                    SourceName(Identifier {
                                        start: 6,
                                        end: 9,
                                    })))),
                    ]
                }
                b"NOS_3abcIJEEE..." => {
                    NestedName::Template(
                        CvQualifiers {
                            restrict: false,
                            volatile: false,
                            konst: false,
                        },
                        Some(RefQualifier::RValueRef),
                        PrefixHandle::NonSubstitution(NonSubstitution(0))),
                    b"...",
                    [
                        Substitutable::Prefix(
                            Prefix::Nested(
                                PrefixHandle::BackReference(0),
                                UnqualifiedName::Source(
                                    SourceName(Identifier {
                                        start: 5,
                                        end: 8,
                                    })))),
                    ]
                }
                b"NS_3abcIJEEE..." => {
                    NestedName::Template(
                        CvQualifiers {
                            restrict: false,
                            volatile: false,
                            konst: false,
                        },
                        None,
                        PrefixHandle::NonSubstitution(NonSubstitution(0))),
                    b"...",
                    [
                        Substitutable::Prefix(
                            Prefix::Nested(
                                PrefixHandle::BackReference(0),
                                UnqualifiedName::Source(
                                    SourceName(Identifier {
                                        start: 4,
                                        end: 7,
                                    })))),
                    ]
                }
            }
            Err => {
                // Ends with a prefix that is not a name or template.
                b"NS_E..." => Error::UnexpectedText,
                b"NS_DttrEE..." => Error::UnexpectedText,

                b"zzz" => Error::UnexpectedText,
                b"Nzzz" => Error::UnexpectedText,
                b"NKzzz" => Error::UnexpectedText,
                b"NKOzzz" => Error::UnexpectedText,
                b"NKO3abczzz" => Error::UnexpectedText,
                b"NKO3abc3abczzz" => Error::UnexpectedText,
                b"" => Error::UnexpectedEnd,
                b"N" => Error::UnexpectedEnd,
                b"NK" => Error::UnexpectedEnd,
                b"NKO" => Error::UnexpectedEnd,
                b"NKO3abc" => Error::UnexpectedEnd,
                b"NKO3abc3abc" => Error::UnexpectedEnd,
            }
        }
    });
}

#[test]
fn parse_prefix_handle() {
    // <prefix> ::= <unqualified-name>
    //          ::= <prefix> <unqualified-name>
    //          ::= <template-prefix> <template-args>
    //          ::= <template-param>
    //          ::= <decltype>
    //          ::= <prefix> <data-member-prefix>
    //          ::= <substitution>
    assert_parse!(PrefixHandle {
        with subs [
            Substitutable::Prefix(
                Prefix::Unqualified(
                    UnqualifiedName::Operator(
                        OperatorName::Simple(
                            SimpleOperatorName::New)))),
        ] => {
            Ok => {
                b"3foo..." => {
                    PrefixHandle::BackReference(1),
                    b"...",
                    [
                        Substitutable::Prefix(
                            Prefix::Unqualified(
                                UnqualifiedName::Source(
                                    SourceName(Identifier {
                                        start: 1,
                                        end: 4,
                                    }))))
                    ]
                }
                b"3abc3def..." => {
                    PrefixHandle::BackReference(2),
                    b"...",
                    [
                        Substitutable::Prefix(
                            Prefix::Unqualified(
                                UnqualifiedName::Source(
                                    SourceName(Identifier {
                                        start: 1,
                                        end: 4,
                                    })))),
                        Substitutable::Prefix(
                            Prefix::Nested(
                                PrefixHandle::BackReference(1),
                                UnqualifiedName::Source(
                                    SourceName(Identifier {
                                        start: 5,
                                        end: 8,
                                    })))),
                    ]
                }
                b"3fooIJEE..." => {
                    PrefixHandle::BackReference(2),
                    b"...",
                    [
                        Substitutable::Prefix(
                            Prefix::Unqualified(
                                UnqualifiedName::Source(
                                    SourceName(Identifier {
                                        start: 1,
                                        end: 4,
                                    })))),
                        Substitutable::Prefix(
                            Prefix::Template(PrefixHandle::BackReference(1),
                                             TemplateArgs(vec![
                                                 TemplateArg::ArgPack(vec![]),
                                             ])))
                    ]
                }
                b"T_..." => {
                    PrefixHandle::BackReference(1),
                    b"...",
                    [
                        Substitutable::Prefix(Prefix::TemplateParam(TemplateParam(0))),
                    ]
                }
                b"DTtrE..." => {
                    PrefixHandle::BackReference(1),
                    b"...",
                    [
                        Substitutable::Prefix(
                            Prefix::Decltype(
                                Decltype::Expression(Expression::Rethrow))),
                    ]
                }
                b"3abc3defM..." => {
                    PrefixHandle::BackReference(2),
                    b"...",
                    [
                        Substitutable::Prefix(
                            Prefix::Unqualified(
                                UnqualifiedName::Source(
                                    SourceName(Identifier {
                                        start: 1,
                                        end: 4,
                                    })))),
                        Substitutable::Prefix(
                            Prefix::DataMember(
                                PrefixHandle::BackReference(1),
                                DataMemberPrefix(
                                    SourceName(Identifier {
                                        start: 5,
                                        end: 8,
                                    })))),
                    ]
                }
                b"S_..." => {
                    PrefixHandle::BackReference(0),
                    b"...",
                    []
                }
                // The trailing E and <nested-name> case...
                b"3abc3defE..." => {
                    PrefixHandle::NonSubstitution(NonSubstitution(0)),
                    b"E...",
                    [
                        Substitutable::Prefix(
                            Prefix::Unqualified(
                                UnqualifiedName::Source(
                                    SourceName(Identifier {
                                        start: 1,
                                        end: 4,
                                    })))),
                    ]
                }
            }
            Err => {
                b"zzz" => Error::UnexpectedText,
                b"" => Error::UnexpectedEnd,
            }
        }
    });
}

#[test]
fn parse_type_handle() {
    assert_parse!(TypeHandle {
        with subs [
            Substitutable::Type(
                Type::PointerTo(
                    TypeHandle::Builtin(BuiltinType::Standard(StandardBuiltinType::Char)))),
        ] => {
            Ok => {
                b"S_..." => {
                    TypeHandle::BackReference(0),
                    b"...",
                    []
                }
                b"c..." => {
                    TypeHandle::Builtin(BuiltinType::Standard(StandardBuiltinType::Char)),
                    b"...",
                    []
                }
                b"FS_E..." => {
                    TypeHandle::BackReference(1),
                    b"...",
                    [
                        Substitutable::Type(
                            Type::Function(FunctionType {
                                cv_qualifiers: CvQualifiers {
                                    restrict: false,
                                    volatile: false,
                                    konst: false,
                                },
                                exception_spec: None,
                                transaction_safe: false,
                                extern_c: false,
                                bare: BareFunctionType(vec![TypeHandle::BackReference(0)]),
                                ref_qualifier: None,
                            })),
                    ]
                }
                b"A_S_..." => {
                    TypeHandle::BackReference(1),
                    b"...",
                    [
                        Substitutable::Type(
                            Type::Array(ArrayType::NoDimension(TypeHandle::BackReference(0)))),
                    ]
                }
                b"MS_S_..." => {
                    TypeHandle::BackReference(1),
                    b"...",
                    [
                        Substitutable::Type(
                            Type::PointerToMember(
                                PointerToMemberType(TypeHandle::BackReference(0),
                                                    TypeHandle::BackReference(0)))),
                    ]
                }
                b"T_..." => {
                    TypeHandle::BackReference(1),
                    b"...",
                    [
                        Substitutable::Type(Type::TemplateParam(TemplateParam(0))),
                    ]
                }
                b"T_IS_E..." => {
                    TypeHandle::BackReference(2),
                    b"...",
                    [
                        Substitutable::TemplateTemplateParam(
                            TemplateTemplateParam(TemplateParam(0))),
                        Substitutable::Type(
                            Type::TemplateTemplate(
                                TemplateTemplateParamHandle::BackReference(1),
                                TemplateArgs(vec![
                                    TemplateArg::Type(TypeHandle::BackReference(0))
                                ]))),
                    ]
                }
                b"DTtrE..." => {
                    TypeHandle::BackReference(1),
                    b"...",
                    [
                        Substitutable::Type(
                            Type::Decltype(Decltype::Expression(Expression::Rethrow))),
                    ]
                }
                b"KS_..." => {
                    TypeHandle::BackReference(1),
                    b"...",
                    [
                        Substitutable::Type(Type::Qualified(CvQualifiers {
                            restrict: false,
                            volatile: false,
                            konst: true,
                        }, TypeHandle::BackReference(0)))
                    ]
                }
                b"PS_..." => {
                    TypeHandle::BackReference(1),
                    b"...",
                    [
                        Substitutable::Type(Type::PointerTo(TypeHandle::BackReference(0)))
                    ]
                }
                b"RS_..." => {
                    TypeHandle::BackReference(1),
                    b"...",
                    [
                        Substitutable::Type(Type::LvalueRef(TypeHandle::BackReference(0)))
                    ]
                }
                b"OS_..." => {
                    TypeHandle::BackReference(1),
                    b"...",
                    [
                        Substitutable::Type(Type::RvalueRef(TypeHandle::BackReference(0)))
                    ]
                }
                b"CS_..." => {
                    TypeHandle::BackReference(1),
                    b"...",
                    [
                        Substitutable::Type(Type::Complex(TypeHandle::BackReference(0)))
                    ]
                }
                b"GS_..." => {
                    TypeHandle::BackReference(1),
                    b"...",
                    [
                        Substitutable::Type(Type::Imaginary(TypeHandle::BackReference(0)))
                    ]
                }
                b"U3abcS_..." => {
                    TypeHandle::BackReference(1),
                    b"...",
                    [
                        Substitutable::Type(
                            Type::VendorExtension(
                                SourceName(Identifier {
                                    start: 2,
                                    end: 5,
                                }),
                                None,
                                TypeHandle::BackReference(0)))
                    ]
                }
                b"U3abcIS_ES_..." => {
                    TypeHandle::BackReference(1),
                    b"...",
                    [
                        Substitutable::Type(
                            Type::VendorExtension(
                                SourceName(Identifier {
                                    start: 2,
                                    end: 5,
                                }),
                                Some(TemplateArgs(vec![
                                    TemplateArg::Type(TypeHandle::BackReference(0))
                                ])),
                                TypeHandle::BackReference(0)))
                    ]
                }
                b"DpS_..." => {
                    TypeHandle::BackReference(1),
                    b"...",
                    [
                        Substitutable::Type(
                            Type::PackExpansion(TypeHandle::BackReference(0))),
                    ]
                }
                b"3abc..." => {
                    TypeHandle::BackReference(1),
                    b"...",
                    [
                        Substitutable::Type(
                            Type::ClassEnum(
                                ClassEnumType::Named(
                                    Name::Unscoped(
                                        UnscopedName::Unqualified(
                                            UnqualifiedName::Source(
                                                SourceName(Identifier {
                                                    start: 1,
                                                    end: 4,
                                                })))))))
                    ]
                }
            }
            Err => {
                b"P" => Error::UnexpectedEnd,
                b"R" => Error::UnexpectedEnd,
                b"O" => Error::UnexpectedEnd,
                b"C" => Error::UnexpectedEnd,
                b"G" => Error::UnexpectedEnd,
                b"Dp" => Error::UnexpectedEnd,
                b"D" => Error::UnexpectedEnd,
                b"P" => Error::UnexpectedEnd,
                b"UlvE_" => Error::UnexpectedText,
                b"" => Error::UnexpectedEnd,
            }
        }
    });
}

#[test]
fn parse_exception_spec() {
    assert_parse!(ExceptionSpec {
        Ok => {
            b"Do..." => {
                ExceptionSpec::NoExcept,
                b"..."
            }
            b"DOtrE..." => {
                ExceptionSpec::Computed(Expression::Rethrow),
                b"..."
            }
        }
        Err => {
            b"DOtre" => Error::UnexpectedText,
            b"DOE" => Error::UnexpectedText,
            b"D" => Error::UnexpectedEnd,
            b"" => Error::UnexpectedEnd,
        }
    });
}

#[test]
fn parse_function_type() {
    assert_parse!(FunctionType {
        with subs [
            Substitutable::Type(
                Type::PointerTo(
                    TypeHandle::Builtin(BuiltinType::Standard(StandardBuiltinType::Char)))),
        ] => {
            Ok => {
                b"KDxFYS_RE..." => {
                    FunctionType {
                        cv_qualifiers: CvQualifiers {
                            restrict: false,
                            volatile: false,
                            konst: true,
                        },
                        exception_spec: None,
                        transaction_safe: true,
                        extern_c: true,
                        bare: BareFunctionType(vec![TypeHandle::BackReference(0)]),
                        ref_qualifier: Some(RefQualifier::LValueRef),
                    },
                    b"...",
                    []
                }
                b"DxFYS_RE..." => {
                    FunctionType {
                        cv_qualifiers: CvQualifiers {
                            restrict: false,
                            volatile: false,
                            konst: false,
                        },
                        exception_spec: None,
                        transaction_safe: true,
                        extern_c: true,
                        bare: BareFunctionType(vec![TypeHandle::BackReference(0)]),
                        ref_qualifier: Some(RefQualifier::LValueRef),
                    },
                    b"...",
                    []
                }
                b"FYS_RE..." => {
                    FunctionType {
                        cv_qualifiers: CvQualifiers {
                            restrict: false,
                            volatile: false,
                            konst: false,
                        },
                        exception_spec: None,
                        transaction_safe: false,
                        extern_c: true,
                        bare: BareFunctionType(vec![TypeHandle::BackReference(0)]),
                        ref_qualifier: Some(RefQualifier::LValueRef),
                    },
                    b"...",
                    []
                }
                b"FS_RE..." => {
                    FunctionType {
                        cv_qualifiers: CvQualifiers {
                            restrict: false,
                            volatile: false,
                            konst: false,
                        },
                        exception_spec: None,
                        transaction_safe: false,
                        extern_c: false,
                        bare: BareFunctionType(vec![TypeHandle::BackReference(0)]),
                        ref_qualifier: Some(RefQualifier::LValueRef),
                    },
                    b"...",
                    []
                }
                b"FS_E..." => {
                    FunctionType {
                        cv_qualifiers: CvQualifiers {
                            restrict: false,
                            volatile: false,
                            konst: false,
                        },
                        exception_spec: None,
                        transaction_safe: false,
                        extern_c: false,
                        bare: BareFunctionType(vec![TypeHandle::BackReference(0)]),
                        ref_qualifier: None,
                    },
                    b"...",
                    []
                }
            }
            Err => {
                b"DFYS_E" => Error::UnexpectedText,
                b"KKFS_E" => Error::UnexpectedText,
                b"FYS_..." => Error::UnexpectedText,
                b"FYS_" => Error::UnexpectedEnd,
                b"F" => Error::UnexpectedEnd,
                b"" => Error::UnexpectedEnd,
            }
        }
    });
}

#[test]
fn parse_bare_function_type() {
    assert_parse!(BareFunctionType {
        with subs [
            Substitutable::Type(
                Type::PointerTo(
                    TypeHandle::Builtin(BuiltinType::Standard(StandardBuiltinType::Char)))),
        ] => {
            Ok => {
                b"S_S_..." => {
                    BareFunctionType(vec![
                        TypeHandle::BackReference(0),
                        TypeHandle::BackReference(0),
                    ]),
                    b"...",
                    []
                }
            }
            Err => {
                b"" => Error::UnexpectedEnd,
            }
        }
    });
}

#[test]
fn parse_decltype() {
    assert_parse!(Decltype {
        Ok => {
            b"DTtrE..." => {
                Decltype::Expression(Expression::Rethrow),
                b"..."
            }
            b"DttrE..." => {
                Decltype::IdExpression(Expression::Rethrow),
                b"..."
            }
        }
        Err => {
            b"Dtrtz" => Error::UnexpectedText,
            b"DTrtz" => Error::UnexpectedText,
            b"Dz" => Error::UnexpectedText,
            b"Dtrt" => Error::UnexpectedText,
            b"DTrt" => Error::UnexpectedText,
            b"Dt" => Error::UnexpectedEnd,
            b"DT" => Error::UnexpectedEnd,
            b"D" => Error::UnexpectedEnd,
            b"" => Error::UnexpectedEnd,
        }
    });
}

#[test]
fn parse_class_enum_type() {
    assert_parse!(ClassEnumType {
        Ok => {
            b"3abc..." => {
                ClassEnumType::Named(
                    Name::Unscoped(
                        UnscopedName::Unqualified(
                            UnqualifiedName::Source(
                                SourceName(Identifier {
                                    start: 1,
                                    end: 4,
                                }))))),
                b"..."
            }
            b"Ts3abc..." => {
                ClassEnumType::ElaboratedStruct(
                    Name::Unscoped(
                        UnscopedName::Unqualified(
                            UnqualifiedName::Source(
                                SourceName(Identifier {
                                    start: 3,
                                    end: 6,
                                }))))),
                b"..."
            }
            b"Tu3abc..." => {
                ClassEnumType::ElaboratedUnion(
                    Name::Unscoped(
                        UnscopedName::Unqualified(
                            UnqualifiedName::Source(
                                SourceName(Identifier {
                                    start: 3,
                                    end: 6,
                                }))))),
                b"..."
            }
            b"Te3abc..." => {
                ClassEnumType::ElaboratedEnum(
                    Name::Unscoped(
                        UnscopedName::Unqualified(
                            UnqualifiedName::Source(
                                SourceName(Identifier {
                                    start: 3,
                                    end: 6,
                                }))))),
                b"..."
            }
        }
        Err => {
            b"zzz" => Error::UnexpectedText,
            b"Tzzz" => Error::UnexpectedText,
            b"T" => Error::UnexpectedEnd,
            b"" => Error::UnexpectedEnd,
        }
    });
}

#[test]
fn parse_array_type() {
    assert_parse!(ArrayType {
        with subs [
            Substitutable::Type(Type::Decltype(Decltype::Expression(Expression::Rethrow)))
        ] => {
            Ok => {
                b"A10_S_..." => {
                    ArrayType::DimensionNumber(10, TypeHandle::BackReference(0)),
                    b"...",
                    []
                }
                b"A10_Sb..." => {
                    ArrayType::DimensionNumber(10,
                                               TypeHandle::WellKnown(
                                                   WellKnownComponent::StdString1)),
                    b"...",
                    []
                }
                b"Atr_S_..." => {
                    ArrayType::DimensionExpression(Expression::Rethrow,
                                                   TypeHandle::BackReference(0)),
                    b"...",
                    []
                }
                b"A_S_..." => {
                    ArrayType::NoDimension(TypeHandle::BackReference(0)),
                    b"...",
                    []
                }
            }
            Err => {
                b"A10_" => Error::UnexpectedEnd,
                b"A10" => Error::UnexpectedEnd,
                b"A" => Error::UnexpectedEnd,
                b"" => Error::UnexpectedEnd,
                b"A10_..." => Error::UnexpectedText,
                b"A10..." => Error::UnexpectedText,
                b"A..." => Error::UnexpectedText,
                b"..." => Error::UnexpectedText,
            }
        }
    });
}

#[test]
fn parse_vector_type() {
    assert_parse!(VectorType {
        with subs [
            Substitutable::Type(Type::Decltype(Decltype::Expression(Expression::Rethrow)))
        ] => {
            Ok => {
                b"Dv10_S_..." => {
                    VectorType::DimensionNumber(10, TypeHandle::BackReference(0)),
                    b"...",
                    []
                }
                b"Dv10_Sb..." => {
                    VectorType::DimensionNumber(10,
                                                TypeHandle::WellKnown(
                                                    WellKnownComponent::StdString1)),
                    b"...",
                    []
                }
                b"Dvtr_S_..." => {
                    VectorType::DimensionExpression(Expression::Rethrow,
                                                    TypeHandle::BackReference(0)),
                    b"...",
                    []
                }
            }
            Err => {
                b"Dq" => Error::UnexpectedText,
                b"Dv" => Error::UnexpectedEnd,
                b"Dv42_" => Error::UnexpectedEnd,
                b"Dv42_..." => Error::UnexpectedText,
                b"Dvtr_" => Error::UnexpectedEnd,
                b"Dvtr_..." => Error::UnexpectedText,
                b"" => Error::UnexpectedEnd,
                b"..." => Error::UnexpectedText,
            }
        }
    });
}

#[test]
fn parse_pointer_to_member_type() {
    assert_parse!(PointerToMemberType {
        with subs [
            Substitutable::Type(Type::Decltype(Decltype::Expression(Expression::Rethrow)))
        ] => {
            Ok => {
                b"MS_S_..." => {
                    PointerToMemberType(TypeHandle::BackReference(0),
                                        TypeHandle::BackReference(0)),
                    b"...",
                    []
                }
            }
            Err => {
                b"MS_S" => Error::UnexpectedEnd,
                b"MS_" => Error::UnexpectedEnd,
                b"MS" => Error::UnexpectedEnd,
                b"M" => Error::UnexpectedEnd,
                b"" => Error::UnexpectedEnd,
                b"MS_..." => Error::UnexpectedText,
                b"M..." => Error::UnexpectedText,
                b"..." => Error::UnexpectedText,
            }
        }
    });
}

#[test]
fn parse_template_template_param_handle() {
    assert_parse!(TemplateTemplateParamHandle {
        with subs [
            Substitutable::TemplateTemplateParam(TemplateTemplateParam(TemplateParam(0)))
        ] => {
            Ok => {
                b"S_..." => {
                    TemplateTemplateParamHandle::BackReference(0),
                    b"...",
                    []
                }
                b"T1_..." => {
                    TemplateTemplateParamHandle::BackReference(1),
                    b"...",
                    [
                        Substitutable::TemplateTemplateParam(TemplateTemplateParam(TemplateParam(2)))
                    ]
                }
            }
            Err => {
                b"S" => Error::UnexpectedText,
                b"T" => Error::UnexpectedEnd,
                b"" => Error::UnexpectedEnd,
                b"S..." => Error::UnexpectedText,
                b"T..." => Error::UnexpectedText,
                b"..." => Error::UnexpectedText,
            }
        }
    });
}

#[test]
fn parse_template_args() {
    assert_parse!(TemplateArgs {
        with subs [
            Substitutable::Type(Type::Decltype(Decltype::Expression(Expression::Rethrow)))
        ] => {
            Ok => {
                b"IS_E..." => {
                    TemplateArgs(vec![TemplateArg::Type(TypeHandle::BackReference(0))]),
                    b"...",
                    []
                }
                b"IS_S_S_S_E..." => {
                    TemplateArgs(vec![
                        TemplateArg::Type(TypeHandle::BackReference(0)),
                        TemplateArg::Type(TypeHandle::BackReference(0)),
                        TemplateArg::Type(TypeHandle::BackReference(0)),
                        TemplateArg::Type(TypeHandle::BackReference(0)),
                    ]),
                    b"...",
                    []
                }
            }
            Err => {
                b"zzz" => Error::UnexpectedText,
                b"IE" => Error::UnexpectedText,
                b"IS_" => Error::UnexpectedEnd,
                b"I" => Error::UnexpectedEnd,
                b"" => Error::UnexpectedEnd,
            }
        }
    });
}

#[test]
fn parse_template_arg() {
    assert_parse!(TemplateArg {
        with subs [
            Substitutable::Type(Type::Decltype(Decltype::Expression(Expression::Rethrow)))
        ] => {
            Ok => {
                b"S_..." => {
                    TemplateArg::Type(TypeHandle::BackReference(0)),
                    b"...",
                    []
                }
                b"XtrE..." => {
                    TemplateArg::Expression(Expression::Rethrow),
                    b"...",
                    []
                }
                b"XsrS_1QE..." => {
                    TemplateArg::Expression(
                        Expression::UnresolvedName(
                            UnresolvedName::Nested1(
                                UnresolvedTypeHandle::BackReference(0),
                                vec![],
                                BaseUnresolvedName::Name(
                                    SimpleId(
                                        SourceName(Identifier {
                                            start: 6,
                                            end: 7
                                        }),
                                        None
                                    )
                                )
                            )
                        )
                    ),
                    b"...",
                    []
                }
                b"XsrS_1QIlEE..." => {
                    TemplateArg::Expression(
                        Expression::UnresolvedName(
                            UnresolvedName::Nested1(
                                UnresolvedTypeHandle::BackReference(0),
                                vec![],
                                BaseUnresolvedName::Name(
                                    SimpleId(
                                        SourceName(Identifier {
                                            start: 6,
                                            end: 7
                                        }),
                                        Some(
                                            TemplateArgs(
                                                vec![
                                                    TemplateArg::Type(
                                                        TypeHandle::Builtin(
                                                            BuiltinType::Standard(
                                                                StandardBuiltinType::Long
                                                            )
                                                        )
                                                    )
                                                ]
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    ),
                    b"...",
                    []
                }
                b"LS_E..." => {
                    TemplateArg::SimpleExpression(
                        ExprPrimary::Literal(TypeHandle::BackReference(0), 3, 3)),
                    b"...",
                    []
                }
                b"JE..." => {
                    TemplateArg::ArgPack(vec![]),
                    b"...",
                    []
                }
                b"JS_XtrELS_EJEE..." => {
                    TemplateArg::ArgPack(vec![
                        TemplateArg::Type(TypeHandle::BackReference(0)),
                        TemplateArg::Expression(Expression::Rethrow),
                        TemplateArg::SimpleExpression(
                            ExprPrimary::Literal(TypeHandle::BackReference(0), 10, 10)),
                        TemplateArg::ArgPack(vec![]),
                    ]),
                    b"...",
                    []
                }
            }
            Err => {
                b"..." => Error::UnexpectedText,
                b"X..." => Error::UnexpectedText,
                b"J..." => Error::UnexpectedText,
                b"JS_..." => Error::UnexpectedText,
                b"JS_" => Error::UnexpectedEnd,
                b"X" => Error::UnexpectedEnd,
                b"J" => Error::UnexpectedEnd,
                b"" => Error::UnexpectedEnd,
            }
        }
    });
}

#[test]
fn parse_expression() {
    assert_parse!(Expression {
        with subs [
            Substitutable::Type(
                Type::PointerTo(TypeHandle::Builtin(
                    BuiltinType::Standard(StandardBuiltinType::Int)))),
        ] => {
            Ok => {
                b"psLS_1E..." => {
                    Expression::Unary(OperatorName::Simple(SimpleOperatorName::UnaryPlus),
                                      Box::new(Expression::Primary(
                                          ExprPrimary::Literal(
                                              TypeHandle::BackReference(0),
                                              5,
                                              6)))),
                    b"...",
                    []
                }
                b"rsLS_1ELS_1E..." => {
                    Expression::Binary(OperatorName::Simple(SimpleOperatorName::Shr),
                                       Box::new(Expression::Primary(
                                           ExprPrimary::Literal(
                                               TypeHandle::BackReference(0),
                                               5,
                                               6))),
                                       Box::new(Expression::Primary(
                                           ExprPrimary::Literal(
                                               TypeHandle::BackReference(0),
                                               10,
                                               11)))),
                    b"...",
                    []
                }
                b"quLS_1ELS_2ELS_3E..." => {
                    Expression::Ternary(OperatorName::Simple(SimpleOperatorName::Question),
                                        Box::new(Expression::Primary(
                                            ExprPrimary::Literal(
                                                TypeHandle::BackReference(0),
                                                5,
                                                6))),
                                        Box::new(Expression::Primary(
                                            ExprPrimary::Literal(
                                                TypeHandle::BackReference(0),
                                                10,
                                                11))),
                                        Box::new(Expression::Primary(
                                            ExprPrimary::Literal(
                                                TypeHandle::BackReference(0),
                                                15,
                                                16)))),
                    b"...",
                    []
                }
                b"pp_LS_1E..." => {
                    Expression::PrefixInc(
                        Box::new(Expression::Primary(
                            ExprPrimary::Literal(
                                TypeHandle::BackReference(0),
                                6,
                                7)))),
                    b"...",
                    []
                }
                b"mm_LS_1E..." => {
                    Expression::PrefixDec(
                        Box::new(Expression::Primary(
                            ExprPrimary::Literal(
                                TypeHandle::BackReference(0),
                                6,
                                7)))),
                    b"...",
                    []
                }
                b"clLS_1EE..." => {
                    Expression::Call(
                        Box::new(Expression::Primary(
                            ExprPrimary::Literal(
                                TypeHandle::BackReference(0),
                                5,
                                6))),
                        vec![]),
                    b"...",
                    []
                }
                //               ::= cv <type> <expression>                       # type (expression), conversion with one argument
                b"cvS_LS_1E..." => {
                    Expression::ConversionOne(
                        TypeHandle::BackReference(0),
                        Box::new(Expression::Primary(
                            ExprPrimary::Literal(
                                TypeHandle::BackReference(0),
                                7,
                                8)))),
                    b"...",
                    []
                }
                b"cvS__LS_1ELS_1EE..." => {
                    Expression::ConversionMany(
                        TypeHandle::BackReference(0),
                        vec![
                            Expression::Primary(
                                ExprPrimary::Literal(
                                    TypeHandle::BackReference(0),
                                    8,
                                    9)),
                            Expression::Primary(
                                ExprPrimary::Literal(
                                    TypeHandle::BackReference(0),
                                    13,
                                    14)),
                        ]),
                    b"...",
                    []
                }
                b"tlS_LS_1ELS_1EE..." => {
                    Expression::ConversionBraced(
                        TypeHandle::BackReference(0),
                        vec![
                            Expression::Primary(
                                ExprPrimary::Literal(
                                    TypeHandle::BackReference(0),
                                    7,
                                    8)),
                            Expression::Primary(
                                ExprPrimary::Literal(
                                    TypeHandle::BackReference(0),
                                    12,
                                    13)),
                        ]),
                    b"...",
                    []
                }
                b"ilLS_1EE..." => {
                    Expression::BracedInitList(
                        Box::new(Expression::Primary(
                            ExprPrimary::Literal(
                                TypeHandle::BackReference(0),
                                5,
                                6)))),
                    b"...",
                    []
                }
                b"gsnwLS_1E_S_E..." => {
                    Expression::GlobalNew(
                        vec![
                            Expression::Primary(
                                ExprPrimary::Literal(
                                    TypeHandle::BackReference(0),
                                    7,
                                    8))
                        ],
                        TypeHandle::BackReference(0),
                        None),
                    b"...",
                    []
                }
                b"nwLS_1E_S_E..." => {
                    Expression::New(
                        vec![
                            Expression::Primary(
                                ExprPrimary::Literal(
                                    TypeHandle::BackReference(0),
                                    5,
                                    6))
                        ],
                        TypeHandle::BackReference(0),
                        None),
                    b"...",
                    []
                }
                b"gsnwLS_1E_S_piE..." => {
                    Expression::GlobalNew(
                        vec![
                            Expression::Primary(
                                ExprPrimary::Literal(
                                    TypeHandle::BackReference(0),
                                    7,
                                    8))
                        ],
                        TypeHandle::BackReference(0),
                        Some(Initializer(vec![]))),
                    b"...",
                    []
                }
                b"nwLS_1E_S_piE..." => {
                    Expression::New(
                        vec![
                            Expression::Primary(
                                ExprPrimary::Literal(
                                    TypeHandle::BackReference(0),
                                    5,
                                    6))
                        ],
                        TypeHandle::BackReference(0),
                        Some(Initializer(vec![]))),
                    b"...",
                    []
                }
                b"gsnaLS_1E_S_E..." => {
                    Expression::GlobalNewArray(
                        vec![
                            Expression::Primary(
                                ExprPrimary::Literal(
                                    TypeHandle::BackReference(0),
                                    7,
                                    8))
                        ],
                        TypeHandle::BackReference(0),
                        None),
                    b"...",
                    []
                }
                b"naLS_1E_S_E..." => {
                    Expression::NewArray(
                        vec![
                            Expression::Primary(
                                ExprPrimary::Literal(
                                    TypeHandle::BackReference(0),
                                    5,
                                    6))
                        ],
                        TypeHandle::BackReference(0),
                        None),
                    b"...",
                    []
                }
                b"gsnaLS_1E_S_piE..." => {
                    Expression::GlobalNewArray(
                        vec![
                            Expression::Primary(
                                ExprPrimary::Literal(
                                    TypeHandle::BackReference(0),
                                    7,
                                    8))
                        ],
                        TypeHandle::BackReference(0),
                        Some(Initializer(vec![]))),
                    b"...",
                    []
                }
                b"naLS_1E_S_piE..." => {
                    Expression::NewArray(
                        vec![
                            Expression::Primary(
                                ExprPrimary::Literal(
                                    TypeHandle::BackReference(0),
                                    5,
                                    6))
                        ],
                        TypeHandle::BackReference(0),
                        Some(Initializer(vec![]))),
                    b"...",
                    []
                }
                b"gsdlLS_1E..." => {
                    Expression::GlobalDelete(
                        Box::new(Expression::Primary(
                            ExprPrimary::Literal(
                                TypeHandle::BackReference(0),
                                7,
                                8)))),
                    b"...",
                    []
                }
                b"dlLS_1E..." => {
                    Expression::Delete(
                        Box::new(Expression::Primary(
                            ExprPrimary::Literal(
                                TypeHandle::BackReference(0),
                                5,
                                6)))),
                    b"...",
                    []
                }
                //               ::= [gs] da <expression>                         # delete[] expression
                b"gsdaLS_1E..." => {
                    Expression::GlobalDeleteArray(
                        Box::new(Expression::Primary(
                            ExprPrimary::Literal(
                                TypeHandle::BackReference(0),
                                7,
                                8)))),
                    b"...",
                    []
                }
                b"daLS_1E..." => {
                    Expression::DeleteArray(
                        Box::new(Expression::Primary(
                            ExprPrimary::Literal(
                                TypeHandle::BackReference(0),
                                5,
                                6)))),
                    b"...",
                    []
                }
                b"dcS_LS_1E..." => {
                    Expression::DynamicCast(
                        TypeHandle::BackReference(0),
                        Box::new(Expression::Primary(
                            ExprPrimary::Literal(
                                TypeHandle::BackReference(0),
                                7,
                                8)))),
                    b"...",
                    []
                }
                b"scS_LS_1E..." => {
                    Expression::StaticCast(
                        TypeHandle::BackReference(0),
                        Box::new(Expression::Primary(
                            ExprPrimary::Literal(
                                TypeHandle::BackReference(0),
                                7,
                                8)))),
                    b"...",
                    []
                }
                b"ccS_LS_1E..." => {
                    Expression::ConstCast(
                        TypeHandle::BackReference(0),
                        Box::new(Expression::Primary(
                            ExprPrimary::Literal(
                                TypeHandle::BackReference(0),
                                7,
                                8)))),
                    b"...",
                    []
                }
                b"rcS_LS_1E..." => {
                    Expression::ReinterpretCast(
                        TypeHandle::BackReference(0),
                        Box::new(Expression::Primary(
                            ExprPrimary::Literal(
                                TypeHandle::BackReference(0),
                                7,
                                8)))),
                    b"...",
                    []
                }
                b"tiS_..." => {
                    Expression::TypeidType(TypeHandle::BackReference(0)),
                    b"...",
                    []
                }
                b"teLS_1E..." => {
                    Expression::TypeidExpr(
                        Box::new(Expression::Primary(
                            ExprPrimary::Literal(
                                TypeHandle::BackReference(0),
                                5,
                                6)))),
                    b"...",
                    []
                }
                b"stS_..." => {
                    Expression::SizeofType(TypeHandle::BackReference(0)),
                    b"...",
                    []
                }
                b"szLS_1E..." => {
                    Expression::SizeofExpr(
                        Box::new(Expression::Primary(
                            ExprPrimary::Literal(
                                TypeHandle::BackReference(0),
                                5,
                                6)))),
                    b"...",
                    []
                }
                b"atS_..." => {
                    Expression::AlignofType(TypeHandle::BackReference(0)),
                    b"...",
                    []
                }
                b"azLS_1E..." => {
                    Expression::AlignofExpr(
                        Box::new(Expression::Primary(
                            ExprPrimary::Literal(
                                TypeHandle::BackReference(0),
                                5,
                                6)))),
                    b"...",
                    []
                }
                b"nxLS_1E..." => {
                    Expression::Noexcept(
                        Box::new(Expression::Primary(
                            ExprPrimary::Literal(
                                TypeHandle::BackReference(0),
                                5,
                                6)))),
                    b"...",
                    []
                }
                b"T_..." => {
                    Expression::TemplateParam(TemplateParam(0)),
                    b"...",
                    []
                }
                b"fp_..." => {
                    Expression::FunctionParam(FunctionParam(0, CvQualifiers::default(), Some(0))),
                    b"...",
                    []
                }
                b"dtT_3abc..." => {
                    Expression::Member(
                        Box::new(Expression::TemplateParam(TemplateParam(0))),
                        MemberName(
                            Name::Unscoped(
                                UnscopedName::Unqualified(
                                    UnqualifiedName::Source(
                                        SourceName(
                                            Identifier {
                                                start: 5,
                                                end: 8,
                                            })))))),
                    b"...",
                    []
                }
                b"ptT_3abc..." => {
                    Expression::DerefMember(
                        Box::new(Expression::TemplateParam(TemplateParam(0))),
                        MemberName(
                            Name::Unscoped(
                                UnscopedName::Unqualified(
                                    UnqualifiedName::Source(
                                        SourceName(
                                            Identifier {
                                                start: 5,
                                                end: 8,
                                            })))))),
                    b"...",
                    []
                }
                b"dtfp_clI3abcE..." => {
                    Expression::Member(
                        Box::new(Expression::FunctionParam(FunctionParam(0, CvQualifiers::default(), Some(0)))),
                        MemberName(
                            Name::UnscopedTemplate(
                                UnscopedTemplateNameHandle::NonSubstitution(NonSubstitution(0)),
                                TemplateArgs(vec![
                                    TemplateArg::Type(
                                        TypeHandle::BackReference(1))])))),
                    b"...",
                    [
                        Substitutable::Type(
                            Type::ClassEnum(
                                ClassEnumType::Named(
                                    Name::Unscoped(
                                        UnscopedName::Unqualified(
                                            UnqualifiedName::Source(
                                                SourceName(
                                                    Identifier {
                                                        start: 9,
                                                        end: 12
                                                    })))))))
                    ]
                }
                //               ::= ds <expression> <expression>                 # expr.*expr
                b"dsT_T_..." => {
                    Expression::PointerToMember(
                        Box::new(Expression::TemplateParam(TemplateParam(0))),
                        Box::new(Expression::TemplateParam(TemplateParam(0)))),
                    b"...",
                    []
                }
                b"sZT_..." => {
                    Expression::SizeofTemplatePack(TemplateParam(0)),
                    b"...",
                    []
                }
                b"sZfp_..." => {
                    Expression::SizeofFunctionPack(
                        FunctionParam(0, CvQualifiers::default(), Some(0))),
                    b"...",
                    []
                }
                b"sPE..." => {
                    Expression::SizeofCapturedTemplatePack(vec![]),
                    b"...",
                    []
                }
                b"spT_..." => {
                    Expression::PackExpansion(
                        Box::new(Expression::TemplateParam(TemplateParam(0)))),
                    b"...",
                    []
                }
                b"twT_..." => {
                    Expression::Throw(Box::new(Expression::TemplateParam(TemplateParam(0)))),
                    b"...",
                    []
                }
                b"tr..." => {
                    Expression::Rethrow,
                    b"...",
                    []
                }
                b"3abc..." => {
                    Expression::UnresolvedName(
                        UnresolvedName::Name(
                            BaseUnresolvedName::Name(
                                SimpleId(
                                    SourceName(Identifier {
                                        start: 1,
                                        end: 4,
                                    }),
                                    None)))),
                    b"...",
                    []
                }
                b"L_Z3abcE..." => {
                    Expression::Primary(
                        ExprPrimary::External(
                            MangledName::Encoding(
                                Encoding::Data(
                                    Name::Unscoped(
                                        UnscopedName::Unqualified(
                                            UnqualifiedName::Source(
                                                SourceName(Identifier {
                                                    start: 4,
                                                    end: 7,
                                                }))))), vec![]))),
                    b"...",
                    []
                }
                // An expression where arity matters
                b"cldtdefpT4TypeadsrT_5EnterE..." => {
                    Expression::Call(
                        Box::new(Expression::Member(
                            Box::new(Expression::Unary(OperatorName::Simple(SimpleOperatorName::Deref),
                                                       Box::new(Expression::FunctionParam(
                                                           FunctionParam(0,
                                                                         CvQualifiers::default(),
                                                                         None)
                                                       ))
                            )),
                            MemberName(
                                Name::Unscoped(
                                    UnscopedName::Unqualified(
                                        UnqualifiedName::Source(
                                            SourceName(Identifier {
                                                start: 10,
                                                end: 14,
                                            })))
                                 )
                            )
                        )),
                        vec![Expression::Unary(OperatorName::Simple(SimpleOperatorName::AddressOf),
                                               Box::new(Expression::UnresolvedName(
                                                   UnresolvedName::Nested1(
                                                       UnresolvedTypeHandle::BackReference(1),
                                                       vec![],
                                                       BaseUnresolvedName::Name(
                                                           SimpleId(
                                                               SourceName(Identifier {
                                                                       start: 21,
                                                                       end: 26
                                                               }
                                                               ),
                                                               None
                                                           )
                                                       )
                                                   ))))]
                    ),
                    b"...",
                    [
                        Substitutable::UnresolvedType(UnresolvedType::Template(TemplateParam(0), None))
                    ]
                }
            }
            Err => {
                b"dtStfp_clI3abcE..." => Error::UnexpectedText,
            }
        }
    });
}

#[test]
fn parse_unresolved_name() {
    assert_parse!(UnresolvedName {
        with subs [
            Substitutable::UnresolvedType(
                UnresolvedType::Decltype(Decltype::Expression(Expression::Rethrow))),
        ] => {
            Ok => {
                b"gs3abc..." => {
                    UnresolvedName::Global(BaseUnresolvedName::Name(SimpleId(SourceName(Identifier {
                        start: 3,
                        end: 6,
                    }), None))),
                    b"...",
                    []
                }
                b"3abc..." => {
                    UnresolvedName::Name(BaseUnresolvedName::Name(SimpleId(SourceName(Identifier {
                        start: 1,
                        end: 4,
                    }), None))),
                    b"...",
                    []
                }
                b"srS_3abc..." => {
                    UnresolvedName::Nested1(UnresolvedTypeHandle::BackReference(0),
                                            vec![],
                                            BaseUnresolvedName::Name(SimpleId(SourceName(Identifier {
                                                start: 5,
                                                end: 8,
                                            }), None))),
                    b"...",
                    []
                }
                b"srNS_3abc3abcE3abc..." => {
                    UnresolvedName::Nested1(
                        UnresolvedTypeHandle::BackReference(0),
                        vec![
                            UnresolvedQualifierLevel(SimpleId(SourceName(Identifier {
                                start: 6,
                                end: 9,
                            }), None)),
                            UnresolvedQualifierLevel(SimpleId(SourceName(Identifier {
                                start: 10,
                                end: 13,
                            }), None)),
                        ],
                        BaseUnresolvedName::Name(SimpleId(SourceName(Identifier {
                            start: 15,
                            end: 18,
                        }), None))),
                    b"...",
                    []
                }
                b"gssr3abcE3abc..." => {
                    UnresolvedName::GlobalNested2(
                        vec![
                            UnresolvedQualifierLevel(SimpleId(SourceName(Identifier {
                                start: 5,
                                end: 8,
                            }), None)),
                        ],
                        BaseUnresolvedName::Name(SimpleId(SourceName(Identifier {
                            start: 10,
                            end: 13,
                        }), None))),
                    b"...",
                    []
                }
                b"sr3abcE3abc..." => {
                    UnresolvedName::Nested2(
                        vec![
                            UnresolvedQualifierLevel(SimpleId(SourceName(Identifier {
                                start: 3,
                                end: 6,
                            }), None)),
                        ],
                        BaseUnresolvedName::Name(SimpleId(SourceName(Identifier {
                            start: 8,
                            end: 11,
                        }), None))),
                    b"...",
                    []
                }
            }
            Err => {
                b"zzzzzz" => Error::UnexpectedText,
                b"gszzz" => Error::UnexpectedText,
                b"gssrzzz" => Error::UnexpectedText,
                b"srNzzz" => Error::UnexpectedText,
                b"srzzz" => Error::UnexpectedText,
                b"srN3abczzzz" => Error::UnexpectedText,
                b"srN3abcE" => Error::UnexpectedText,
                b"srN3abc" => Error::UnexpectedText,
                b"srN" => Error::UnexpectedEnd,
                b"sr" => Error::UnexpectedEnd,
                b"gssr" => Error::UnexpectedEnd,
                b"gs" => Error::UnexpectedEnd,
                b"" => Error::UnexpectedEnd,
            }
        }
    });
}

#[test]
fn parse_unresolved_type_handle() {
    assert_parse!(UnresolvedTypeHandle {
        with subs [
            Substitutable::UnresolvedType(
                UnresolvedType::Decltype(Decltype::Expression(Expression::Rethrow))),
        ] => {
            Ok => {
                b"S_..." => {
                    UnresolvedTypeHandle::BackReference(0),
                    b"...",
                    []
                }
                b"T_..." => {
                    UnresolvedTypeHandle::BackReference(1),
                    b"...",
                    [
                        Substitutable::UnresolvedType(
                            UnresolvedType::Template(TemplateParam(0), None)),
                    ]
                }
                b"T_IS_E..." => {
                    UnresolvedTypeHandle::BackReference(1),
                    b"...",
                    [
                        Substitutable::UnresolvedType(
                            UnresolvedType::Template(TemplateParam(0), Some(TemplateArgs(vec![
                                TemplateArg::Type(TypeHandle::BackReference(0))
                            ])))),
                    ]
                }
                b"DTtrE..." => {
                    UnresolvedTypeHandle::BackReference(1),
                    b"...",
                    [
                        Substitutable::UnresolvedType(
                            UnresolvedType::Decltype(Decltype::Expression(Expression::Rethrow)))
                    ]

                }
            }
            Err => {
                b"zzzzzzz" => Error::UnexpectedText,
                b"" => Error::UnexpectedEnd,
            }
        }
    });
}

#[test]
fn parse_unresolved_qualifier_level() {
    assert_parse!(UnresolvedQualifierLevel {
        with subs [
            Substitutable::Type(Type::Decltype(Decltype::Expression(Expression::Rethrow)))
        ] => {
            Ok => {
                b"3abc..." => {
                    UnresolvedQualifierLevel(SimpleId(SourceName(Identifier {
                        start: 1,
                        end: 4,
                    }), None)),
                    b"...",
                    []
                }
                b"3abcIS_E..." => {
                    UnresolvedQualifierLevel(SimpleId(SourceName(Identifier {
                        start: 1,
                        end: 4,
                    }), Some(TemplateArgs(vec![
                        TemplateArg::Type(TypeHandle::BackReference(0))
                    ])))),
                    b"...",
                    []
                }
            }
            Err => {
                b"zzz" => Error::UnexpectedText,
                b"" => Error::UnexpectedEnd,
            }
        }
    });
}

#[test]
fn parse_simple_id() {
    assert_parse!(SimpleId {
        with subs [
            Substitutable::Type(Type::Decltype(Decltype::Expression(Expression::Rethrow)))
        ] => {
            Ok => {
                b"3abc..." => {
                    SimpleId(SourceName(Identifier {
                        start: 1,
                        end: 4,
                    }), None),
                    b"...",
                    []
                }
                b"3abcIS_E..." => {
                    SimpleId(SourceName(Identifier {
                        start: 1,
                        end: 4,
                    }), Some(TemplateArgs(vec![
                        TemplateArg::Type(TypeHandle::BackReference(0))
                    ]))),
                    b"...",
                    []
                }
            }
            Err => {
                b"zzz" => Error::UnexpectedText,
                b"" => Error::UnexpectedEnd,
            }
        }
    });
}

#[test]
fn parse_base_unresolved_name() {
    assert_parse!(BaseUnresolvedName {
        with subs [
            Substitutable::Type(Type::Decltype(Decltype::Expression(Expression::Rethrow)))
        ] => {
            Ok => {
                b"3abc..." => {
                    BaseUnresolvedName::Name(SimpleId(SourceName(Identifier {
                        start: 1,
                        end: 4,
                    }), None)),
                    b"...",
                    []
                }
                b"onnw..." => {
                    BaseUnresolvedName::Operator(OperatorName::Simple(SimpleOperatorName::New), None),
                    b"...",
                    []
                }
                b"onnwIS_E..." => {
                    BaseUnresolvedName::Operator(OperatorName::Simple(SimpleOperatorName::New),
                                                 Some(TemplateArgs(vec![
                                                     TemplateArg::Type(TypeHandle::BackReference(0))
                                                 ]))),
                    b"...",
                    []
                }
                b"dn3abc..." => {
                    BaseUnresolvedName::Destructor(DestructorName::Name(SimpleId(SourceName(Identifier {
                        start: 3,
                        end: 6,
                    }), None))),
                    b"...",
                    []
                }
            }
            Err => {
                b"ozzz" => Error::UnexpectedText,
                b"dzzz" => Error::UnexpectedText,
                b"dn" => Error::UnexpectedEnd,
                b"on" => Error::UnexpectedEnd,
                b"" => Error::UnexpectedEnd,
            }
        }
    });
}

#[test]
fn parse_destructor_name() {
    assert_parse!(DestructorName {
        with subs [
            Substitutable::UnresolvedType(
                UnresolvedType::Decltype(Decltype::Expression(Expression::Rethrow))),
        ] => {
            Ok => {
                b"S_..." => {
                    DestructorName::Unresolved(UnresolvedTypeHandle::BackReference(0)),
                    b"...",
                    []
                }
                b"3abc..." => {
                    DestructorName::Name(SimpleId(SourceName(Identifier {
                        start: 1,
                        end: 4,
                    }), None)),
                    b"...",
                    []
                }
            }
            Err => {
                b"zzz" => Error::UnexpectedText,
                b"" => Error::UnexpectedEnd,
            }
        }
    });
}

#[test]
fn parse_expr_primary() {
    assert_parse!(ExprPrimary {
        with subs [
            Substitutable::Type(Type::Decltype(Decltype::Expression(Expression::Rethrow)))
        ] => {
            Ok => {
                b"LS_12345E..." => {
                    ExprPrimary::Literal(TypeHandle::BackReference(0), 3, 8),
                    b"...",
                    []
                }
                b"LS_E..." => {
                    ExprPrimary::Literal(TypeHandle::BackReference(0), 3, 3),
                    b"...",
                    []
                }
                b"L_Z3abcE..." => {
                    ExprPrimary::External(
                        MangledName::Encoding(
                            Encoding::Data(
                                Name::Unscoped(
                                    UnscopedName::Unqualified(
                                        UnqualifiedName::Source(
                                            SourceName(Identifier {
                                                start: 4,
                                                end: 7,
                                            }))))), vec![])),
                    b"...",
                    []
                }
            }
            Err => {
                b"zzz" => Error::UnexpectedText,
                b"LS_zzz" => Error::UnexpectedEnd,
                b"LS_12345" => Error::UnexpectedEnd,
                b"LS_" => Error::UnexpectedEnd,
                b"L" => Error::UnexpectedEnd,
                b"" => Error::UnexpectedEnd,
            }
        }
    });
}

#[test]
fn parse_initializer() {
    assert_parse!(Initializer {
        Ok => {
            b"piE..." => {
                Initializer(vec![]),
                b"..."
            }
            b"pitrtrtrE..." => {
                Initializer(vec![
                    Expression::Rethrow,
                    Expression::Rethrow,
                    Expression::Rethrow,
                ]),
                b"..."
            }
        }
        Err => {
            b"pirtrtrt..." => Error::UnexpectedText,
            b"pi..." => Error::UnexpectedText,
            b"..." => Error::UnexpectedText,
            b"pirt" => Error::UnexpectedText,
            b"pi" => Error::UnexpectedEnd,
            b"p" => Error::UnexpectedEnd,
            b"" => Error::UnexpectedEnd,
        }
    });
}

#[test]
fn parse_local_name() {
    assert_parse!(LocalName {
        Ok => {
            b"Z3abcE3def_0..." => {
                LocalName::Relative(
                    Box::new(Encoding::Data(
                        Name::Unscoped(
                            UnscopedName::Unqualified(
                                UnqualifiedName::Source(
                                    SourceName(Identifier {
                                        start: 2,
                                        end: 5,
                                    })))))),
                    Some(Box::new(Name::Unscoped(
                        UnscopedName::Unqualified(
                            UnqualifiedName::Source(
                                SourceName(Identifier {
                                    start: 7,
                                    end: 10,
                                })))))),
                    Some(Discriminator(0))),
                b"..."
            }
            b"Z3abcE3def..." => {
                LocalName::Relative(
                    Box::new(Encoding::Data(
                        Name::Unscoped(
                            UnscopedName::Unqualified(
                                UnqualifiedName::Source(
                                    SourceName(Identifier {
                                        start: 2,
                                        end: 5,
                                    })))))),
                    Some(Box::new(Name::Unscoped(
                        UnscopedName::Unqualified(
                            UnqualifiedName::Source(
                                SourceName(Identifier {
                                    start: 7,
                                    end: 10,
                                })))))),
                    None),
                b"..."
            }
            b"Z3abcEs_0..." => {
                LocalName::Relative(
                    Box::new(Encoding::Data(
                        Name::Unscoped(
                            UnscopedName::Unqualified(
                                UnqualifiedName::Source(
                                    SourceName(Identifier {
                                        start: 2,
                                        end: 5,
                                    })))))),
                    None,
                    Some(Discriminator(0))),
                b"..."
            }
            b"Z3abcEs..." => {
                LocalName::Relative(
                    Box::new(Encoding::Data(
                        Name::Unscoped(
                            UnscopedName::Unqualified(
                                UnqualifiedName::Source(
                                    SourceName(Identifier {
                                        start: 2,
                                        end: 5,
                                    })))))),
                    None,
                    None),
                b"..."
            }
            b"Z3abcEd1_3abc..." => {
                LocalName::Default(
                    Box::new(Encoding::Data(
                        Name::Unscoped(
                            UnscopedName::Unqualified(
                                UnqualifiedName::Source(
                                    SourceName(Identifier {
                                        start: 2,
                                        end: 5,
                                    })))))),
                    Some(1),
                    Box::new(Name::Unscoped(
                        UnscopedName::Unqualified(
                            UnqualifiedName::Source(
                                SourceName(Identifier {
                                    start: 10,
                                    end: 13,
                                })))))),
                b"..."
            }
            b"Z3abcEd_3abc..." => {
                LocalName::Default(
                    Box::new(Encoding::Data(
                        Name::Unscoped(
                            UnscopedName::Unqualified(
                                UnqualifiedName::Source(
                                    SourceName(Identifier {
                                        start: 2,
                                        end: 5,
                                    })))))),
                    None,
                    Box::new(Name::Unscoped(
                        UnscopedName::Unqualified(
                            UnqualifiedName::Source(
                                SourceName(Identifier {
                                    start: 9,
                                    end: 12,
                                })))))),
                b"..."
            }
        }
        Err => {
            b"A" => Error::UnexpectedText,
            b"Z1a" => Error::UnexpectedEnd,
            b"Z1aE" => Error::UnexpectedEnd,
            b"Z" => Error::UnexpectedEnd,
            b"" => Error::UnexpectedEnd,
        }
    });
}

#[test]
fn parse_closure_type_name() {
    assert_parse!(ClosureTypeName {
        Ok => {
            b"UlvE_..." => {
                ClosureTypeName(LambdaSig(vec![]), None),
                b"..."
            }
            b"UlvE36_..." => {
                ClosureTypeName(LambdaSig(vec![]), Some(36)),
                b"..."
            }
        }
        Err => {
            b"UlvE36zzz" => Error::UnexpectedText,
            b"UlvEzzz" => Error::UnexpectedText,
            b"Ulvzzz" => Error::UnexpectedText,
            b"zzz" => Error::UnexpectedText,
            b"UlvE10" => Error::UnexpectedEnd,
            b"UlvE" => Error::UnexpectedEnd,
            b"Ulv" => Error::UnexpectedEnd,
            b"Ul" => Error::UnexpectedEnd,
            b"U" => Error::UnexpectedEnd,
            b"" => Error::UnexpectedEnd,
        }
    });
}

#[test]
fn parse_lambda_sig() {
    assert_parse!(LambdaSig {
        with subs [
            Substitutable::Type(
                Type::PointerTo(
                    TypeHandle::Builtin(BuiltinType::Standard(StandardBuiltinType::Bool))))
        ] => {
            Ok => {
                b"v..." => {
                    LambdaSig(vec![]),
                    b"...",
                    []
                }
                b"S_S_S_..." => {
                    LambdaSig(vec![
                        TypeHandle::BackReference(0),
                        TypeHandle::BackReference(0),
                        TypeHandle::BackReference(0),
                    ]),
                    b"...",
                    []
                }
            }
            Err => {
                b"..." => Error::UnexpectedText,
                b"S" => Error::UnexpectedEnd,
                b"" => Error::UnexpectedEnd,
            }
        }
    });
}

#[test]
fn parse_substitution() {
    assert_parse!(Substitution {
        with subs [
            Substitutable::Type(Type::Decltype(Decltype::Expression(Expression::Rethrow))),
            Substitutable::Type(Type::Decltype(Decltype::Expression(Expression::Rethrow))),
            Substitutable::Type(Type::Decltype(Decltype::Expression(Expression::Rethrow)))
        ] => {
            Ok => {
                b"S_..." => {
                    Substitution::BackReference(0),
                    b"...",
                    []
                }
                b"S1_..." => {
                    Substitution::BackReference(2),
                    b"...",
                    []
                }
                b"St..." => {
                    Substitution::WellKnown(WellKnownComponent::Std),
                    b"...",
                    []
                }
                b"Sa..." => {
                    Substitution::WellKnown(WellKnownComponent::StdAllocator),
                    b"...",
                    []
                }
                b"Sb..." => {
                    Substitution::WellKnown(WellKnownComponent::StdString1),
                    b"...",
                    []
                }
                b"Ss..." => {
                    Substitution::WellKnown(WellKnownComponent::StdString2),
                    b"...",
                    []
                }
                b"Si..." => {
                    Substitution::WellKnown(WellKnownComponent::StdIstream),
                    b"...",
                    []
                }
                b"So..." => {
                    Substitution::WellKnown(WellKnownComponent::StdOstream),
                    b"...",
                    []
                }
                b"Sd..." => {
                    Substitution::WellKnown(WellKnownComponent::StdIostream),
                    b"...",
                    []
                }
            }
            Err => {
                b"S999_" => Error::BadBackReference,
                b"Sz" => Error::UnexpectedText,
                b"zzz" => Error::UnexpectedText,
                b"S1" => Error::UnexpectedEnd,
                b"S" => Error::UnexpectedEnd,
                b"" => Error::UnexpectedEnd,
            }
        }
    });
}

#[test]
fn parse_special_name() {
    assert_parse!(SpecialName {
        Ok => {
            b"TVi..." => {
                SpecialName::VirtualTable(TypeHandle::Builtin(BuiltinType::Standard(StandardBuiltinType::Int))),
                b"..."
            }
            b"TTi..." => {
                SpecialName::Vtt(TypeHandle::Builtin(BuiltinType::Standard(StandardBuiltinType::Int))),
                b"..."
            }
            b"TIi..." => {
                SpecialName::Typeinfo(TypeHandle::Builtin(BuiltinType::Standard(StandardBuiltinType::Int))),
                b"..."
            }
            b"TSi..." => {
                SpecialName::TypeinfoName(TypeHandle::Builtin(BuiltinType::Standard(StandardBuiltinType::Int))),
                b"..."
            }
            b"Tv42_36_3abc..." => {
                SpecialName::VirtualOverrideThunk(
                    CallOffset::Virtual(VOffset(42, 36)),
                    Box::new(Encoding::Data(
                        Name::Unscoped(
                            UnscopedName::Unqualified(
                                UnqualifiedName::Source(
                                    SourceName(Identifier {
                                        start: 9,
                                        end: 12,
                                    }))))))),
                b"..."
            }
            b"Tcv42_36_v42_36_3abc..." => {
                SpecialName::VirtualOverrideThunkCovariant(
                    CallOffset::Virtual(VOffset(42, 36)),
                    CallOffset::Virtual(VOffset(42, 36)),
                    Box::new(Encoding::Data(
                        Name::Unscoped(
                            UnscopedName::Unqualified(
                                UnqualifiedName::Source(
                                    SourceName(Identifier {
                                        start: 17,
                                        end: 20,
                                    }))))))),
                b"..."
            }
            b"GV3abc..." => {
                SpecialName::Guard(
                    Name::Unscoped(
                        UnscopedName::Unqualified(
                            UnqualifiedName::Source(
                                SourceName(Identifier {
                                    start: 3,
                                    end: 6,
                                }))))),
                b"..."
            }
            b"GR3abc_..." => {
                SpecialName::GuardTemporary(
                    Name::Unscoped(
                        UnscopedName::Unqualified(
                            UnqualifiedName::Source(
                                SourceName(Identifier {
                                    start: 3,
                                    end: 6,
                                })))),
                    0),
                b"..."
            }
            b"GR3abc0_..." => {
                SpecialName::GuardTemporary(
                    Name::Unscoped(
                        UnscopedName::Unqualified(
                            UnqualifiedName::Source(
                                SourceName(Identifier {
                                    start: 3,
                                    end: 6,
                                })))),
                    1),
                b"..."
            }
            b"Gr4_abc..." => {
                SpecialName::JavaResource(vec![ResourceName {
                    start: 4,
                    end: 7,
                }]),
                b"..."
            }
            b"TCc7_i..." => {
                SpecialName::ConstructionVtable(
                    TypeHandle::Builtin(BuiltinType::Standard(StandardBuiltinType::Char)),
                    7,
                    TypeHandle::Builtin(BuiltinType::Standard(StandardBuiltinType::Int))
                ),
                b"..."
            }
            b"TFi..." => {
                SpecialName::TypeinfoFunction(
                    TypeHandle::Builtin(BuiltinType::Standard(StandardBuiltinType::Int))),
                b"..."
            }
            b"TH4name..." => {
                SpecialName::TlsInit(
                    Name::Unscoped(
                        UnscopedName::Unqualified(
                            UnqualifiedName::Source(
                                SourceName(Identifier { start: 3, end: 7 }))))),
                b"..."
            }
            b"TW4name..." => {
                SpecialName::TlsWrapper(
                    Name::Unscoped(
                        UnscopedName::Unqualified(
                            UnqualifiedName::Source(
                                SourceName(Identifier { start: 3, end: 7 }))))),
                b"..."
            }
        }
        Err => {
            b"TZ" => Error::UnexpectedText,
            b"GZ" => Error::UnexpectedText,
            b"GR3abcz" => Error::UnexpectedText,
            b"GR3abc0z" => Error::UnexpectedText,
            b"T" => Error::UnexpectedEnd,
            b"G" => Error::UnexpectedEnd,
            b"" => Error::UnexpectedEnd,
            b"GR3abc" => Error::UnexpectedEnd,
            b"GR3abc0" => Error::UnexpectedEnd,
            // This number is not allowed to be negative.
            b"TCcn7_i..." => Error::UnexpectedText,
            b"Gr3abc0" => Error::UnexpectedText,
        }
    });
}

#[test]
fn parse_function_param() {
    assert_parse!(FunctionParam {
        Ok => {
            b"fpK_..." => {
                FunctionParam(0,
                              CvQualifiers {
                                  restrict: false,
                                  volatile: false,
                                  konst: true,
                              },
                              Some(0)),
                b"..."
            }
            b"fL1pK_..." => {
                FunctionParam(1,
                              CvQualifiers {
                                  restrict: false,
                                  volatile: false,
                                  konst: true,
                              },
                              Some(0)),
                b"..."
            }
            b"fpK3_..." => {
                FunctionParam(0,
                              CvQualifiers {
                                  restrict: false,
                                  volatile: false,
                                  konst: true,
                              },
                              Some(4)),
                b"..."
            }
            b"fL1pK4_..." => {
                FunctionParam(1,
                              CvQualifiers {
                                  restrict: false,
                                  volatile: false,
                                  konst: true,
                              },
                              Some(5)),
                b"..."
            }
        }
        Err => {
            b"fz" => Error::UnexpectedText,
            b"fLp_" => Error::UnexpectedText,
            b"fpL_" => Error::UnexpectedText,
            b"fL1pK4z" => Error::UnexpectedText,
            b"fL1pK4" => Error::UnexpectedEnd,
            b"fL1p" => Error::UnexpectedEnd,
            b"fL1" => Error::UnexpectedEnd,
            b"fL" => Error::UnexpectedEnd,
            b"f" => Error::UnexpectedEnd,
            b"" => Error::UnexpectedEnd,
        }
    });
}

#[test]
fn parse_discriminator() {
    assert_parse!(Discriminator {
        Ok => {
            b"_0..." => {
                Discriminator(0),
                b"..."
            }
            b"_9..." => {
                Discriminator(9),
                b"..."
            }
            b"__99_..." => {
                Discriminator(99),
                b"..."
            }
        }
        Err => {
            b"_n1" => Error::UnexpectedText,
            b"__99..." => Error::UnexpectedText,
            b"__99" => Error::UnexpectedEnd,
            b"..." => Error::UnexpectedText,
        }
    });
}

#[test]
fn parse_data_member_prefix() {
    assert_parse!(DataMemberPrefix {
        Ok => {
            b"3fooM..." => {
                DataMemberPrefix(SourceName(Identifier {
                    start: 1,
                    end: 4,
                })),
                b"..."
            }
        }
        Err => {
            b"zzz" => Error::UnexpectedText,
            b"1" => Error::UnexpectedEnd,
            b"" => Error::UnexpectedEnd,
        }
    });
}

#[test]
fn parse_ref_qualifier() {
    assert_parse!(RefQualifier {
        Ok => {
            b"R..." => {
                RefQualifier::LValueRef,
                b"..."
            }
            b"O..." => {
                RefQualifier::RValueRef,
                b"..."
            }
        }
        Err => {
            b"..." => Error::UnexpectedText,
            b"" => Error::UnexpectedEnd,
        }
    });
}

#[test]
fn parse_cv_qualifiers() {
    assert_parse!(CvQualifiers {
        Ok => {
            b"" => {
                CvQualifiers { restrict: false, volatile: false, konst: false },
                b""
            }
            b"..." => {
                CvQualifiers { restrict: false, volatile: false, konst: false },
                b"..."
            }
            b"r..." => {
                CvQualifiers { restrict: true, volatile: false, konst: false },
                b"..."
            }
            b"rV..." => {
                CvQualifiers { restrict: true, volatile: true, konst: false },
                b"..."
            }
            b"rVK..." => {
                CvQualifiers { restrict: true, volatile: true, konst: true },
                b"..."
            }
            b"V" => {
                CvQualifiers { restrict: false, volatile: true, konst: false },
                b""
            }
            b"VK" => {
                CvQualifiers { restrict: false, volatile: true, konst: true },
                b""
            }
            b"K..." => {
                CvQualifiers { restrict: false, volatile: false, konst: true },
                b"..."
            }
        }
        Err => {
            // None.
        }
    });
}

#[test]
fn parse_builtin_type() {
    assert_parse!(BuiltinType {
        Ok => {
            b"c..." => {
                BuiltinType::Standard(StandardBuiltinType::Char),
                b"..."
            }
            b"c" => {
                BuiltinType::Standard(StandardBuiltinType::Char),
                b""
            }
            b"u3abc..." => {
                BuiltinType::Extension(SourceName(Identifier {
                    start: 2,
                    end: 5,
                })),
                b"..."
            }
        }
        Err => {
            b"." => Error::UnexpectedText,
            b"" => Error::UnexpectedEnd,
        }
    });
}

#[test]
fn parse_template_param() {
    assert_parse!(TemplateParam {
        Ok => {
            b"T_..." => {
                TemplateParam(0),
                b"..."
            }
            b"T3_..." => {
                TemplateParam(4),
                b"..."
            }
        }
        Err => {
            b"wtf" => Error::UnexpectedText,
            b"Twtf" => Error::UnexpectedText,
            b"T3wtf" => Error::UnexpectedText,
            b"T" => Error::UnexpectedEnd,
            b"T3" => Error::UnexpectedEnd,
            b"" => Error::UnexpectedEnd,
        }
    });
}

#[test]
fn parse_unscoped_name() {
    assert_parse!(UnscopedName {
        Ok => {
            b"St5hello..." => {
                UnscopedName::Std(UnqualifiedName::Source(SourceName(Identifier {
                    start: 3,
                    end: 8,
                }))),
                b"..."
            }
            b"5hello..." => {
                UnscopedName::Unqualified(UnqualifiedName::Source(SourceName(Identifier {
                    start: 1,
                    end: 6,
                }))),
                b"..."
            }
        }
        Err => {
            b"St..." => Error::UnexpectedText,
            b"..." => Error::UnexpectedText,
            b"" => Error::UnexpectedEnd,
        }
    });
}

#[test]
fn parse_unqualified_name() {
    assert_parse!(UnqualifiedName {
        Ok => {
            b"qu.." => {
                UnqualifiedName::Operator(OperatorName::Simple(SimpleOperatorName::Question)),
                b".."
            }
            b"C1.." => {
                UnqualifiedName::CtorDtor(CtorDtorName::CompleteConstructor(None)),
                b".."
            }
            b"10abcdefghij..." => {
                UnqualifiedName::Source(SourceName(Identifier {
                    start: 2,
                    end: 12,
                })),
                b"..."
            }
            b"UllE_..." => {
                UnqualifiedName::ClosureType(
                    ClosureTypeName(
                        LambdaSig(vec![
                            TypeHandle::Builtin(
                                BuiltinType::Standard(
                                    StandardBuiltinType::Long))
                        ]),
                        None)),
                b"..."
            }
            b"Ut5_..." => {
                UnqualifiedName::UnnamedType(UnnamedTypeName(Some(5))),
                b"..."
            }
            b"B5cxx11..." => {
                UnqualifiedName::ABITag(TaggedName(SourceName(Identifier {
                    start: 2,
                    end: 7,
                }))),
                b"..."
            }
            b"L3foo_0..." => {
                UnqualifiedName::LocalSourceName(
                    SourceName(Identifier {
                        start: 2,
                        end: 5
                    }),
                    Some(Discriminator(0))
                ),
                "..."
            }
            b"L3foo..." => {
                UnqualifiedName::LocalSourceName(
                    SourceName(Identifier {
                        start: 2,
                        end: 5
                    }),
                    None
                ),
                "..."
            }
        }
        Err => {
            b"zzz" => Error::UnexpectedText,
            b"Uq" => Error::UnexpectedText,
            b"C" => Error::UnexpectedEnd,
            b"" => Error::UnexpectedEnd,
        }
    });
}

#[test]
fn parse_unnamed_type_name() {
    assert_parse!(UnnamedTypeName {
        Ok => {
            b"Ut_abc" => {
                UnnamedTypeName(None),
                b"abc"
            }
            b"Ut42_abc" => {
                UnnamedTypeName(Some(42)),
                b"abc"
            }
            b"Ut42_" => {
                UnnamedTypeName(Some(42)),
                b""
            }
        }
        Err => {
            b"ut_" => Error::UnexpectedText,
            b"u" => Error::UnexpectedEnd,
            b"Ut" => Error::UnexpectedEnd,
            b"Ut._" => Error::UnexpectedText,
            b"Ut42" => Error::UnexpectedEnd,
        }
    });
}

#[test]
fn parse_identifier() {
    assert_parse!(Identifier {
        Ok => {
            b"1abc" => {
                Identifier { start: 0, end: 4 },
                b""
            }
            b"_Az1\0\0\0" => {
                Identifier { start: 0, end: 4 },
                b"\0\0\0"
            }
            b"$_0\0\0\0" => {
                Identifier { start: 0, end: 3 },
                b"\0\0\0"
            }
        }
        Err => {
            b"\0\0\0" => Error::UnexpectedText,
            b"" => Error::UnexpectedEnd,
        }
    });
}

#[test]
fn parse_source_name() {
    assert_parse!(SourceName {
        Ok => {
            b"1abc" => {
                SourceName(Identifier { start: 1, end: 2 }),
                b"bc"
            }
            b"10abcdefghijklm" => {
                SourceName(Identifier { start: 2, end: 12 }),
                b"klm"
            }
        }
        Err => {
            b"0abc" => Error::UnexpectedText,
            b"n1abc" => Error::UnexpectedText,
            b"10abcdef" => Error::UnexpectedEnd,
            b"" => Error::UnexpectedEnd,
        }
    });
}

#[test]
fn parse_number() {
    assert_parse!(Number {
        Ok => {
            b"n2n3" => {
                -2,
                b"n3"
            }
            b"12345abcdef" => {
                12345,
                b"abcdef"
            }
            b"0abcdef" => {
                0,
                b"abcdef"
            }
            b"42" => {
                42,
                b""
            }
        }
        Err => {
            b"001" => Error::UnexpectedText,
            b"wutang" => Error::UnexpectedText,
            b"n" => Error::UnexpectedEnd,
            b"" => Error::UnexpectedEnd,
        }
    });
}

#[test]
fn parse_call_offset() {
    assert_parse!(CallOffset {
        Ok => {
            b"hn42_..." => {
                CallOffset::NonVirtual(NvOffset(-42)),
                b"..."
            }
            b"vn42_36_..." => {
                CallOffset::Virtual(VOffset(-42, 36)),
                b"..."
            }
        }
        Err => {
            b"h1..." => Error::UnexpectedText,
            b"v1_1..." => Error::UnexpectedText,
            b"hh" => Error::UnexpectedText,
            b"vv" => Error::UnexpectedText,
            b"z" => Error::UnexpectedText,
            b"" => Error::UnexpectedEnd,
        }
    });
}

#[test]
fn parse_v_offset() {
    assert_parse!(VOffset {
        Ok => {
            b"n2_n3abcdef" => {
                VOffset(-2, -3),
                b"abcdef"
            }
            b"12345_12345abcdef" => {
                VOffset(12345, 12345),
                b"abcdef"
            }
            b"0_0abcdef" => {
                VOffset(0, 0),
                b"abcdef"
            }
            b"42_n3" => {
                VOffset(42, -3),
                b""
            }
        }
        Err => {
            b"001" => Error::UnexpectedText,
            b"1_001" => Error::UnexpectedText,
            b"wutang" => Error::UnexpectedText,
            b"n_" => Error::UnexpectedText,
            b"1_n" => Error::UnexpectedEnd,
            b"1_" => Error::UnexpectedEnd,
            b"n" => Error::UnexpectedEnd,
            b"" => Error::UnexpectedEnd,
        }
    });
}

#[test]
fn parse_nv_offset() {
    assert_parse!(NvOffset {
        Ok => {
            b"n2n3" => {
                NvOffset(-2),
                b"n3"
            }
            b"12345abcdef" => {
                NvOffset(12345),
                b"abcdef"
            }
            b"0abcdef" => {
                NvOffset(0),
                b"abcdef"
            }
            b"42" => {
                NvOffset(42),
                b""
            }
        }
        Err => {
            b"001" => Error::UnexpectedText,
            b"wutang" => Error::UnexpectedText,
            b"" => Error::UnexpectedEnd,
        }
    });
}

#[test]
fn parse_seq_id() {
    assert_parse!(SeqId {
        Ok => {
            b"1_" => {
                SeqId(1),
                b"_"
            }
            b"42" => {
                SeqId(146),
                b""
            }
            b"ABCabc" => {
                SeqId(13368),
                b"abc"
            }
        }
        Err => {
            b"abc" => Error::UnexpectedText,
            b"001" => Error::UnexpectedText,
            b"wutang" => Error::UnexpectedText,
            b"" => Error::UnexpectedEnd,
        }
    });
}

#[test]
fn parse_ctor_dtor_name() {
    assert_parse!(CtorDtorName {
        Ok => {
            b"D0" => {
                CtorDtorName::DeletingDestructor,
                b""
            }
            b"C101" => {
                CtorDtorName::CompleteConstructor(None),
                b"01"
            }
        }
        Err => {
            b"gayagaya" => Error::UnexpectedText,
            b"C" => Error::UnexpectedEnd,
            b"" => Error::UnexpectedEnd,
        }
    });
}

#[test]
fn parse_operator_name() {
    assert_parse!(OperatorName {
        Ok => {
            b"qu..." => {
                OperatorName::Simple(SimpleOperatorName::Question),
                b"..."
            }
            b"cvi..." => {
                OperatorName::Conversion(
                    TypeHandle::Builtin(
                        BuiltinType::Standard(
                            StandardBuiltinType::Int))),
                b"..."
            }
            b"li3Foo..." => {
                OperatorName::Literal(SourceName(Identifier {
                    start: 3,
                    end: 6,
                })),
                b"..."
            }
            b"v33Foo..." => {
                OperatorName::VendorExtension(3, SourceName(Identifier {
                    start: 3,
                    end: 6
                })),
                b"..."
            }
        }
        Err => {
            b"cv" => Error::UnexpectedEnd,
            b"li3ab" => Error::UnexpectedEnd,
            b"li" => Error::UnexpectedEnd,
            b"v33ab" => Error::UnexpectedEnd,
            b"v3" => Error::UnexpectedEnd,
            b"v" => Error::UnexpectedEnd,
            b"" => Error::UnexpectedEnd,
            b"q" => Error::UnexpectedText,
            b"c" => Error::UnexpectedText,
            b"l" => Error::UnexpectedText,
            b"zzz" => Error::UnexpectedText,
        }
    });
}

#[test]
fn parse_simple_operator_name() {
    assert_parse!(SimpleOperatorName {
        Ok => {
            b"qu" => {
                SimpleOperatorName::Question,
                b""
            }
            b"quokka" => {
                SimpleOperatorName::Question,
                b"okka"
            }
        }
        Err => {
            b"bu-buuuu" => Error::UnexpectedText,
            b"q" => Error::UnexpectedEnd,
            b"" => Error::UnexpectedEnd,
        }
    });
}

#[test]
fn parse_subobject_expr() {
    assert_parse!(SubobjectExpr {
        with subs [] => {
            Ok => {
                "PKcL_Z3FooEE..." => {
                    SubobjectExpr {
                        ty: TypeHandle::BackReference(1),
                        expr: Box::new(Expression::Primary(
                            ExprPrimary::External(
                                MangledName::Encoding(
                                    Encoding::Data(
                                        Name::Unscoped(
                                            UnscopedName::Unqualified(
                                                UnqualifiedName::Source(
                                                    SourceName(
                                                        Identifier {
                                                            start: 7,
                                                            end: 10,
                                                        }
                                                    )
                                                )
                                            )
                                        )
                                    ),
                                    vec![]
                                )
                            )
                        )),
                        offset: 0,
                    },
                    b"...",
                    [
                        Substitutable::Type(
                            Type::Qualified(
                                CvQualifiers {
                                    restrict: false,
                                    volatile: false,
                                    konst: true,
                                },
                                TypeHandle::Builtin(
                                    BuiltinType::Standard(
                                        StandardBuiltinType::Char,
                                    ),
                                ),
                            )
                        ),
                        Substitutable::Type(
                            Type::PointerTo(
                                TypeHandle::BackReference(
                                    0,
                                ),
                            ),
                        )
                    ]
                }
            }
            Err => {
                "" => Error::UnexpectedEnd,
                "" => Error::UnexpectedEnd,
            }
        }
    });
}
