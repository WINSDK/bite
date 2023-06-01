//! Abstract syntax tree types for mangled symbols.

use super::error::{self, Result};
use super::index_str::IndexStr;
use super::subs::{Substitutable, SubstitutionTable};
use crate::{Colors, TokenStream};

use tokenizing::{Color, ColorScheme};

use std::cell::Cell;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::mem;
use std::ops;
use std::ptr;
use std::str;

/// Keeps track of recursion levels and early returns with an error if there
/// is too much recursion.
macro_rules! try_begin_parse {
    ($ctx:expr) => {
        let _auto_check_recursion = AutoParseRecursion::new($ctx)?;
    };
}

#[derive(Debug, Default, Clone, Copy)]
struct ParseContextState {
    // The current recursion level. Should always be less than or equal to the
    // maximum.
    recursion_level: u32,
    // Whether or not we are currently parsing a conversion operator.
    in_conversion: bool,
}

/// Common context needed when parsing.
#[derive(Debug, Clone)]
pub(crate) struct ParseContext {
    // Maximum amount of recursive parsing calls we will allow. If this is too
    // large, we can blow the stack.
    max_recursion: u32,
    // Mutable state within the `ParseContext`.
    state: Cell<ParseContextState>,
}

impl ParseContext {
    /// Construct a new `ParseContext`.
    pub(crate) fn new() -> ParseContext {
        ParseContext {
            max_recursion: 96,
            state: Cell::new(ParseContextState::default()),
        }
    }

    #[inline]
    fn enter_recursion(&self) -> error::Result<()> {
        let mut state = self.state.get();
        let new_recursion_level = state.recursion_level + 1;

        if new_recursion_level >= self.max_recursion {
            Err(error::Error::TooMuchRecursion)
        } else {
            state.recursion_level = new_recursion_level;
            self.state.set(state);
            Ok(())
        }
    }

    #[inline]
    fn exit_recursion(&self) {
        let mut state = self.state.get();
        debug_assert!(state.recursion_level >= 1);
        state.recursion_level -= 1;
        self.state.set(state);
    }

    #[inline]
    fn in_conversion(&self) -> bool {
        self.state.get().in_conversion
    }

    fn set_in_conversion(&self, in_conversion: bool) -> bool {
        let mut state = self.state.get();
        let previously_in_conversion = state.in_conversion;
        state.in_conversion = in_conversion;
        self.state.set(state);
        previously_in_conversion
    }
}

/// An RAII type to automatically check the recursion level against the
/// maximum. If the maximum has been crossed, return an error. Otherwise,
/// increment the level upon construction, and decrement it upon destruction.
struct AutoParseRecursion<'a>(&'a ParseContext);

impl<'a> AutoParseRecursion<'a> {
    #[inline]
    fn new(ctx: &'a ParseContext) -> error::Result<AutoParseRecursion<'a>> {
        ctx.enter_recursion()?;
        Ok(AutoParseRecursion(ctx))
    }
}

impl<'a> Drop for AutoParseRecursion<'a> {
    #[inline]
    fn drop(&mut self) {
        self.0.exit_recursion();
    }
}

/// A trait for anything that can be parsed from an `IndexStr` and return a
/// `Result` of the parsed `Self` value and the rest of the `IndexStr` input
/// that has not been consumed in parsing the `Self` value.
///
/// For AST types representing productions which have `<substitution>` as a
/// possible right hand side, do not implement this trait directly. Instead,
/// make a newtype over `usize`, parse either the `<substitution>` back
/// reference or "real" value, insert the "real" value into the substitution
/// table if needed, and *always* return the newtype index into the substitution
/// table.
#[doc(hidden)]
pub(crate) trait Parse: Sized {
    /// Parse the `Self` value from `input` and return it, updating the
    /// substitution table as needed.
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(Self, IndexStr<'b>)>;
}

/// Determine whether this AST node is an instantiated[*] template function, and
/// get its concrete template arguments.
///
/// [*] Note that we will never see an abstract, un-instantiated template
/// function, since they don't end up in object files and don't get mangled
/// names.
trait GetTemplateArgs {
    /// Returns `Some` if this is a template function, `None` otherwise.
    fn get_template_args<'a>(&'a self, subs: &'a SubstitutionTable) -> Option<&'a TemplateArgs>;
}

/// A leaf name is the part the name that describes some type or class without
/// any leading namespace qualifiers.
///
/// This is used when figuring out how to format constructors and destructors,
/// which are formatted as `gooble::dodo::Thing::~Thing()` but we don't have
/// direct access to `Thing` in the `CtorDtorName` AST.
#[derive(Debug)]
pub(crate) enum LeafName<'a> {
    SourceName(&'a SourceName),
    WellKnownComponent(&'a WellKnownComponent),
    Closure(&'a ClosureTypeName),
    UnnamedType(&'a UnnamedTypeName),
}

impl<'subs> DemangleAsLeaf<'subs> for LeafName<'subs> {
    fn demangle_as_leaf<'me, 'ctx>(&'me self, ctx: &'ctx mut DemangleContext<'subs>) {
        match *self {
            LeafName::SourceName(sn) => sn.demangle(ctx, None),
            LeafName::Closure(c) => c.demangle(ctx, None),
            LeafName::WellKnownComponent(wkc) => wkc.demangle_as_leaf(ctx),
            LeafName::UnnamedType(utn) => utn.demangle_as_leaf(ctx),
        }
    }
}

impl<'a> Default for LeafName<'a> {
    fn default() -> Self {
        LeafName::SourceName(&SourceName(Identifier { start: 0, end: 0 }))
    }
}

/// Determine whether this AST node is some kind (potentially namespaced) name
/// and if so get its leaf name.
pub(crate) trait GetLeafName<'a> {
    fn get_leaf_name(&'a self, subs: &'a SubstitutionTable) -> Option<LeafName<'a>>;
}

/// Determine whether this AST node is a constructor, destructor, or conversion
/// function.
pub(crate) trait IsCtorDtorConversion {
    fn is_ctor_dtor_conversion(&self, subs: &SubstitutionTable) -> bool;
}

/// When formatting a mangled symbol's parsed AST as a demangled symbol, we need
/// to resolve indirect references to template and function arguments with
/// direct `TemplateArg` and `Type` references respectively.
///
/// Note that which set of arguments are implicitly referenced change as we
/// enter and leave different functions' scope. One might usually use de Brujin
/// indices to keep arguments within scopes separated from each other, but the
/// Itanium C++ ABI does not allow us the luxury. AFAIK, when the ABI was first
/// drafted, C++ did not have lambdas, and the issue did not come up at all
/// since a function simply couldn't refer to the types of closed over
/// variables.
///
/// This trait is implemented by anything that can potentially resolve arguments
/// for us.
trait ArgScope<'me, 'ctx>: fmt::Debug {
    /// Get the current scope's leaf name.
    fn leaf_name(&'me self) -> Result<LeafName<'ctx>>;

    /// Get the current scope's `index`th template argument.
    fn get_template_arg(&'me self, index: usize)
        -> Result<(&'ctx TemplateArg, &'ctx TemplateArgs)>;

    /// Get the current scope's `index`th function argument's type.
    fn get_function_arg(&'me self, index: usize) -> Result<&'ctx Type>;
}

/// An `ArgScopeStack` represents the current function and template demangling
/// scope we are within. As we enter new demangling scopes, we construct new
/// `ArgScopeStack`s whose `prev` references point back to the old ones. These
/// `ArgScopeStack`s are kept on the native stack, and as functions return, they
/// go out of scope and we use the previous `ArgScopeStack`s again.
#[derive(Copy, Clone, Debug)]
pub(crate) struct ArgScopeStack<'prev, 'subs> {
    item: &'subs dyn ArgScope<'subs, 'subs>,
    in_arg: Option<(usize, &'subs TemplateArgs)>,
    prev: Option<&'prev ArgScopeStack<'prev, 'subs>>,
}

/// When we first begin demangling, we haven't entered any function or template
/// demangling scope and we don't have any useful `ArgScopeStack`. Therefore, we
/// are never actually dealing with `ArgScopeStack` directly in practice, but
/// always an `Option<ArgScopeStack>` instead. Nevertheless, we want to define
/// useful methods on `Option<ArgScopeStack>`.
///
/// A custom "extension" trait with exactly one implementor: Rust's principled
/// monkey patching!
trait ArgScopeStackExt<'prev, 'subs>: Copy {
    /// Push a new `ArgScope` onto this `ArgScopeStack` and return the new
    /// `ArgScopeStack` with the pushed resolver on top.
    fn push(
        &'prev self,
        item: &'subs dyn ArgScope<'subs, 'subs>,
    ) -> Option<ArgScopeStack<'prev, 'subs>>;
}

impl<'prev, 'subs> ArgScopeStackExt<'prev, 'subs> for Option<ArgScopeStack<'prev, 'subs>> {
    fn push(
        &'prev self,
        item: &'subs dyn ArgScope<'subs, 'subs>,
    ) -> Option<ArgScopeStack<'prev, 'subs>> {
        Some(ArgScopeStack {
            prev: self.as_ref(),
            in_arg: None,
            item,
        })
    }
}

/// A stack of `ArgScope`s is itself an `ArgScope`!
impl<'prev, 'subs> ArgScope<'prev, 'subs> for Option<ArgScopeStack<'prev, 'subs>> {
    fn leaf_name(&'prev self) -> Result<LeafName<'subs>> {
        let mut scope = self.as_ref();
        while let Some(s) = scope {
            if let Ok(c) = s.item.leaf_name() {
                return Ok(c);
            }
            scope = s.prev;
        }
        Err(error::Error::BadLeafNameReference)
    }

    fn get_template_arg(
        &'prev self,
        idx: usize,
    ) -> Result<(&'subs TemplateArg, &'subs TemplateArgs)> {
        let mut scope = self.as_ref();
        while let Some(s) = scope {
            if let Ok((arg, args)) = s.item.get_template_arg(idx) {
                if let Some((in_idx, in_args)) = s.in_arg {
                    if ptr::eq(args, in_args) && in_idx <= idx {
                        return Err(error::Error::ForwardTemplateArgReference);
                    }
                }
                return Ok((arg, args));
            }
            scope = s.prev;
        }

        Err(error::Error::BadTemplateArgReference)
    }

    fn get_function_arg(&'prev self, idx: usize) -> Result<&'subs Type> {
        let mut scope = self.as_ref();
        while let Some(s) = scope {
            if let Ok(arg) = s.item.get_function_arg(idx) {
                return Ok(arg);
            }
            scope = s.prev;
        }

        Err(error::Error::BadFunctionArgReference)
    }
}

/// Common state that is required when demangling a mangled symbol's parsed AST.
#[derive(Debug)]
pub(crate) struct DemangleContext<'a> {
    /// The substitution table built up when parsing the mangled symbol into an
    /// AST.
    subs: &'a SubstitutionTable,

    /// Sometimes an AST node needs to insert itself as an inner item within one
    /// of its children when demangling that child. For example, the AST
    ///
    /// ```text
    ///     (array 10 int)
    /// ```
    ///
    /// is demangled as `int[10]`, but if we were to demangle the AST
    ///
    /// ```text
    ///     "(lvalue-ref (array 10 int))"
    /// ```
    ///
    /// then we would want this demangled form: `int (&) [10]`, which requires
    /// the parent lvalue-ref to be passed into the child array's demangling
    /// method. This kind of thing also pops up with function pointers.
    ///
    /// The `inner` stack enables such behavior by allowing us to pass AST
    /// parents down to their children as inner items.
    inner: Vec<&'a dyn DemangleAsInner<'a>>,

    /// The original input string.
    input: &'a [u8],

    /// `Identifier`s will be placed here, so `UnnamedTypeName` can utilize and print
    /// out the Constructor/Destructor used.
    source_name: Option<&'a str>,

    /// What the demangled name is being written to.
    pub stream: TokenStream,

    /// The last char written to `out`, if any.
    last_char_written: Option<char>,

    /// We are currently demangling a lambda argument, so template substitution
    /// should be suppressed to match libiberty.
    is_lambda_arg: bool,

    /// We are currently demangling a template-prefix.
    is_template_prefix: bool,

    /// We are currently demangling a template-prefix in a nested-name.
    is_template_prefix_in_nested_name: bool,

    /// `PackExpansion`'s should only print '...', only when there is no template
    /// argument pack.
    is_template_argument_pack: bool,

    /// Whether to show types of expression literals.
    show_expression_literal_types: bool,
}

impl<'a> DemangleContext<'a> {
    /// Construct a new `DemangleContext`.
    pub(crate) fn new(subs: &'a SubstitutionTable, input: &'a str) -> Self {
        Self {
            subs,
            inner: vec![],
            input: input.as_bytes(),
            source_name: None,
            stream: TokenStream::new(input),
            last_char_written: None,
            is_lambda_arg: false,
            is_template_prefix: false,
            is_template_prefix_in_nested_name: false,
            is_template_argument_pack: false,
            show_expression_literal_types: false,
        }
    }

    #[inline]
    fn ensure(&mut self, ch: char) {
        if self.last_char_written != Some(ch) {
            self.push_owned(format!("{ch}"), Colors::item());
        }
    }

    #[inline]
    fn ensure_space(&mut self) {
        self.ensure(' ')
    }

    #[inline]
    fn push_inner(&mut self, item: &'a dyn DemangleAsInner<'a>) {
        self.inner.push(item);
    }

    #[inline]
    fn pop_inner(&mut self) -> Option<&'a dyn DemangleAsInner<'a>> {
        self.inner.pop()
    }

    #[inline]
    fn pop_inner_if(&mut self, inner: &'a dyn DemangleAsInner<'a>) -> bool {
        let last = match self.inner.last() {
            None => return false,
            Some(last) => *last,
        };

        // FIXME: this compares vtables instead of objects
        if ptr::eq(last, inner) {
            self.inner.pop();
            true
        } else {
            false
        }
    }

    fn push_owned(&mut self, text: String, color: &'static Color) {
        self.last_char_written = text.chars().last();
        self.stream.push_cow(std::borrow::Cow::Owned(text), color);
    }

    fn push(&mut self, text: &'static str, color: &'static Color) {
        self.last_char_written = text.chars().last();
        self.stream.push(text, color);
    }

    fn demangle_inner_prefixes<'prev>(&mut self, scope: Option<ArgScopeStack<'prev, 'a>>) {
        let mut new_inner = vec![];
        while let Some(inner) = self.pop_inner() {
            if inner.downcast_to_function_type().map_or(false, |f| !f.cv_qualifiers.is_empty()) {
                new_inner.push(inner);
            } else {
                inner.demangle_as_inner(self, scope);
            }
        }
        new_inner.reverse();
        self.inner = new_inner;
    }

    fn demangle_inners<'prev>(&mut self, scope: Option<ArgScopeStack<'prev, 'a>>) {
        while let Some(inner) = self.pop_inner() {
            inner.demangle_as_inner(self, scope);
        }
    }

    fn set_source_name(&mut self, start: usize, end: usize) {
        let ident = &self.input[start..end];
        self.source_name = str::from_utf8(ident).ok();
    }
}

#[doc(hidden)]
#[derive(Debug)]
pub(crate) struct AutoDemangleContextInnerBarrier<'ctx, 'a> {
    ctx: &'ctx mut DemangleContext<'a>,
    saved_inner: Vec<&'a dyn DemangleAsInner<'a>>,
}

impl<'ctx, 'a> AutoDemangleContextInnerBarrier<'ctx, 'a> {
    /// Set aside the current inner stack on the demangle context.
    pub(crate) fn new(ctx: &'ctx mut DemangleContext<'a>) -> Self {
        let mut saved_inner = vec![];
        mem::swap(&mut saved_inner, &mut ctx.inner);
        AutoDemangleContextInnerBarrier { ctx, saved_inner }
    }
}

impl<'ctx, 'a> ops::Deref for AutoDemangleContextInnerBarrier<'ctx, 'a> {
    type Target = DemangleContext<'a>;

    fn deref(&self) -> &Self::Target {
        self.ctx
    }
}

impl<'ctx, 'a> ops::DerefMut for AutoDemangleContextInnerBarrier<'ctx, 'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.ctx
    }
}

impl<'ctx, 'a> Drop for AutoDemangleContextInnerBarrier<'ctx, 'a> {
    fn drop(&mut self) {
        // NB: We cannot assert that the context's inner is empty here,
        // because if demangling failed we'll unwind the stack without
        // using everything that put on the inner.
        mem::swap(&mut self.saved_inner, &mut self.ctx.inner);
    }
}

/// The inner stack allows passing AST nodes down deeper into the tree so that
/// nodes that logically precede something (e.g. PointerRef) can show up after
/// that thing in the demangled output. What's on the stack may not always be
/// intended for the first node that looks at the stack to grab, though.
///
/// Consider a function with template arguments and parameters, f<T>(a).
/// The function parameters logically precede the template arguments in the AST,
/// but they must be reversed in the output. The parameters end up on the inner
/// stack before processing the template argument nodes. If we're not careful,
/// a node inside the template arguments might pick the function parameters
/// off of the inner stack!
///
/// To solve this, certain nodes act as "inner barriers". By using this macro,
/// they set the existing inner stack aside and replace it with an empty stack
/// while visiting their children. This allows these barrier nodes to have
/// completely self-contained children.
macro_rules! inner_barrier {
    ( $ctx:ident ) => {
        let mut _ctx = AutoDemangleContextInnerBarrier::new($ctx);
        let $ctx = &mut _ctx;
    };
}

/// Any AST node that can be printed in a demangled form.
#[doc(hidden)]
pub(crate) trait Demangle<'subs>: fmt::Debug {
    /// Write the demangled form of this AST node to the given context.
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    );
}

/// Any AST node that can be printed as an inner type.
///
/// See the comments surrounding `DemangleContext::inner` for details.
#[doc(hidden)]
pub(crate) trait DemangleAsInner<'subs>: Demangle<'subs> {
    /// Write the inner demangling form of this AST node to the given context.
    fn demangle_as_inner<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        self.demangle(ctx, scope)
    }

    /// Cast this `` to a `Type`.
    fn downcast_to_type(&self) -> Option<&Type> {
        None
    }

    /// Cast this `` to a `FunctionType`.
    fn downcast_to_function_type(&self) -> Option<&FunctionType> {
        None
    }

    /// Cast this `` to an `ArrayType`.
    fn downcast_to_array_type(&self) -> Option<&ArrayType> {
        None
    }

    /// Cast this `` to a `PointerToMember`.
    fn downcast_to_pointer_to_member(&self) -> Option<&PointerToMemberType> {
        None
    }

    fn is_qualified(&self) -> bool {
        false
    }
}

/// Demangle this thing in the leaf name position.
///
/// For most things this should be the same as its `Demangle`
/// implementation. For `WellKnownComponent`s we need to strip the embedded
/// `std::` namespace prefix.
pub(crate) trait DemangleAsLeaf<'subs> {
    fn demangle_as_leaf<'me, 'ctx>(&'me self, ctx: &'ctx mut DemangleContext<'subs>);
}

macro_rules! reference_newtype {
    ( $newtype_name:ident , $oldtype:ty ) => {
        #[derive(Debug)]
        struct $newtype_name($oldtype);

        impl $newtype_name {
            #[allow(clippy::ptr_arg)]
            #[allow(unsafe_code)]
            fn new(types: &$oldtype) -> &$newtype_name {
                unsafe {
                    // This is safe because we only create an immutable
                    // reference. We are not breaking unique mutable aliasing
                    // requirements. An immutable reference does not allow
                    // dropping the referent, so no worries about double-free
                    // (additionally, see the assertion inside `Drop` below).
                    &*(types as *const $oldtype as *const $newtype_name)
                }
            }
        }

        impl Drop for $newtype_name {
            fn drop(&mut self) {
                unreachable!(
                    "Dropping implies we dereferenced and took ownership, which \
                              is not safe for this newtype"
                );
            }
        }

        impl ops::Deref for $newtype_name {
            type Target = $oldtype;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }
    };
}

// We can't implement `` for newtypes of `[TypeHandle]` like we
// want to because it is unsized and we need to make trait objects out of
// `` for pushing onto the context's inner stack. Therefore, we
// have this inelegant newtyping of `Vec<TypeHandle>`.

// A set of function arguments.
reference_newtype!(FunctionArgList, Vec<TypeHandle>);

// A set of function arguments prefixed by a return type (which we want to
// ignore).
reference_newtype!(FunctionArgListAndReturnType, Vec<TypeHandle>);

// A newtype around a slice of type handles that we format as function
// arguments.
reference_newtype!(FunctionArgSlice, [TypeHandle]);

// Demangle a slice of TypeHandle as a function argument list.
impl<'subs> Demangle<'subs> for FunctionArgSlice {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        let mut saw_needs_paren = false;
        let (needs_space, needs_paren) = ctx
            .inner
            .iter()
            .rev()
            .map(|inner| {
                if inner.downcast_to_pointer_to_member().is_some() {
                    (true, true)
                } else {
                    match inner.downcast_to_type() {
                        Some(&Type::Qualified(..))
                        | Some(&Type::Complex(_))
                        | Some(&Type::Imaginary(_))
                        | Some(&Type::PointerToMember(_)) => (true, true),
                        Some(&Type::PointerTo(_))
                        | Some(&Type::LvalueRef(_))
                        | Some(&Type::RvalueRef(_)) => (false, true),
                        _ => (false, false),
                    }
                }
            })
            .take_while(|&(_, needs_paren)| {
                if saw_needs_paren {
                    false
                } else {
                    saw_needs_paren |= needs_paren;
                    true
                }
            })
            .fold(
                (false, false),
                |(space, paren), (next_space, next_paren)| {
                    (space || next_space, paren || next_paren)
                },
            );

        if needs_paren {
            let needs_space = needs_space | !matches!(ctx.last_char_written, Some('(') | Some('*'));

            if needs_space {
                ctx.ensure_space();
            }

            ctx.push("(", Colors::brackets());
        }

        ctx.demangle_inner_prefixes(scope);

        if needs_paren {
            ctx.push(")", Colors::brackets());
        }

        ctx.push("(", Colors::brackets());

        // To maintain compatibility with libiberty, print `()` instead of
        // `(void)` for functions that take no arguments.
        if self.len() == 1 && self[0].is_void() {
            ctx.push(")", Colors::brackets());
            return;
        }

        let mut need_comma = false;
        for arg in self.iter() {
            if need_comma {
                ctx.push(", ", Colors::delimiter());
            }
            arg.demangle(ctx, scope);
            need_comma = true;
        }

        ctx.push(")", Colors::delimiter());
        ctx.demangle_inners(scope)
    }
}

impl<'subs> Demangle<'subs> for FunctionArgList {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        FunctionArgSlice::new(&self.0[..]).demangle(ctx, scope)
    }
}

impl<'subs> DemangleAsInner<'subs> for FunctionArgList {}

impl<'subs> Demangle<'subs> for FunctionArgListAndReturnType {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        FunctionArgSlice::new(&self.0[1..]).demangle(ctx, scope)
    }
}

impl<'subs> DemangleAsInner<'subs> for FunctionArgListAndReturnType {}

/// Define a handle to a AST type that lives inside the substitution table. A
/// handle is always either an index into the substitution table, or it is a
/// reference to a "well-known" component.
///
/// This declares:
///
/// - The enum of either a back reference into the substitution table or a
///   reference to a "well-known" component
/// - a `Demangle` impl that proxies to the appropriate `Substitutable` in the
///   `SubstitutionTable`
macro_rules! define_handle {
    (
        $(#[$attr:meta])*
        pub(crate) enum $typename:ident
    ) => {
        define_handle! {
            $(#[$attr])*
            pub(crate) enum $typename {}
        }
    };

    (
        $(#[$attr:meta])*
        pub(crate) enum $typename:ident {
            $(
                $( #[$extra_attr:meta] )*
                extra $extra_variant:ident ( $extra_variant_ty:ty ),
            )*
        }
    ) => {
        $(#[$attr])*
        #[derive(Clone, Debug, PartialEq, Eq)]
        pub(crate) enum $typename {
            /// A reference to a "well-known" component.
            WellKnown(WellKnownComponent),

            /// A back-reference into the substitution table to a component we
            /// have already parsed.
            BackReference(usize),

            $(
                $( #[$extra_attr] )*
                $extra_variant( $extra_variant_ty ),
            )*
        }

        impl<'subs> Demangle<'subs> for $typename {
            #[inline]
            fn demangle<'prev, 'ctx>(&'subs self,
                                     ctx: &'ctx mut DemangleContext<'subs>,
                                     scope: Option<ArgScopeStack<'prev, 'subs>>)
                                     {
                match *self {
                    $typename::WellKnown(ref comp) => comp.demangle(ctx, scope),
                    $typename::BackReference(idx) => ctx.subs[idx].demangle(ctx, scope),
                    $(
                        $typename::$extra_variant(ref extra) => extra.demangle(ctx, scope),
                    )*
                }
            }
        }

        impl<'a> GetLeafName<'a> for $typename {
            fn get_leaf_name(&'a self, subs: &'a SubstitutionTable) -> Option<LeafName<'a>> {
                match *self {
                    $typename::WellKnown(ref wk) => wk.get_leaf_name(subs),
                    $typename::BackReference(idx) => {
                        subs.get(idx).and_then(|s| s.get_leaf_name(subs))
                    }
                    $(
                        $typename::$extra_variant(ref e) => e.get_leaf_name(subs),
                    )*
                }
            }
        }
    };
}

/// A handle to a component that is usually substitutable, and lives in the
/// substitutions table, but in this particular case does not qualify for
/// substitutions.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct NonSubstitution(pub usize);

impl<'subs> Demangle<'subs> for NonSubstitution {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        ctx.subs.non_substitution(self.0).demangle(ctx, scope)
    }
}

impl<'a> GetLeafName<'a> for NonSubstitution {
    fn get_leaf_name(&'a self, subs: &'a SubstitutionTable) -> Option<LeafName<'a>> {
        subs.get_non_substitution(self.0).and_then(|ns| ns.get_leaf_name(subs))
    }
}

/// Define a "vocabulary" nonterminal, something like `OperatorName` or
/// `CtorDtorName` that's basically a big list of constant strings.
///
/// This declares:
///
/// - the enum itself
/// - a `Parse` impl
/// - a `Demangle` impl
///
/// See the definition of `CTorDtorName` for an example of its use.
///
/// Optionally, a piece of user data can be attached to the definitions
/// and be returned by a generated accessor. See `SimpleOperatorName` for
/// an example.
macro_rules! define_vocabulary {
    ( $(#[$attr:meta])* pub(crate) enum $typename:ident {
        $($variant:ident ( $mangled:expr, $printable:expr )),*
    } ) => {

        $(#[$attr])*
        pub(crate) enum $typename {
            $(
                #[doc=$printable]
                $variant
            ),*
        }

        impl Parse for $typename {
            fn parse<'a, 'b>(ctx: &'a ParseContext,
                             _subs: &'a mut SubstitutionTable,
                             input: IndexStr<'b>)
                             -> Result<($typename, IndexStr<'b>)> {
                try_begin_parse!(ctx);

                let mut found_prefix = false;
                $(
                    if let Some((head, tail)) = input.try_split_at($mangled.len()) {
                        if head.as_ref() == $mangled {
                            return Ok(($typename::$variant, tail));
                        }
                    } else {
                        found_prefix |= 0 < input.len() &&
                            input.len() < $mangled.len() &&
                            input.as_ref() == &$mangled[..input.len()];
                    }
                )*

                if input.is_empty() || found_prefix {
                    Err(error::Error::UnexpectedEnd)
                } else {
                    Err(error::Error::UnexpectedText)
                }
            }
        }

        impl<'subs> Demangle<'subs> for $typename {
            fn demangle<'prev, 'ctx>(
                &'subs self,
                ctx: &'ctx mut DemangleContext<'subs>,
                _: Option<ArgScopeStack<'prev, 'subs>>
            ) {
                let text = format!("{}", match *self {
                    $($typename::$variant => $printable),*
                });

                ctx.push_owned(text, Colors::item());
            }
        }

        impl $typename {
            #[allow(dead_code)]
            #[inline]
            fn starts_with(byte: u8) -> bool {
                $(
                    if $mangled[0] == byte {
                        return true;
                    }
                )*

                false
            }
        }
    };
    ( $(#[$attr:meta])* pub(crate) enum $typename:ident {
        $($variant:ident ( $mangled:expr, $printable:expr, $userdata:expr)),*
    }

      impl $typename2:ident {
          fn $fn_name:ident(&self) -> $userdata_ty:ty;
    } ) => {
        define_vocabulary! {
            $(#[$attr])*
            pub(crate) enum $typename {
                $(
                    $variant ( $mangled, $printable )
                ),*
            }
        }

        impl $typename2 {
            fn $fn_name(&self) -> $userdata_ty {
                match *self {
                    $(
                        $typename2::$variant => $userdata,
                    )*
                }
            }
        }
    };
}

/// The root AST node, and starting production.
///
/// ```text
/// <mangled-name> ::= _Z <encoding> [<clone-suffix>]*
///                ::= ___Z <encoding> <block_invoke>
///                ::= <type>
///
/// <block_invoke> ::= _block_invoke
///                ::= _block_invoke<decimal-digit>+
///                ::= _block_invoke_<decimal-digit>+
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum MangledName {
    /// The encoding of the mangled symbol name.
    Encoding(Encoding),

    /// The encoding of the mangled symbol name.
    BlockInvoke(Encoding, Option<isize>),

    /// A top-level type. Technically not allowed by the standard, however in
    /// practice this can happen, and is tested for by libiberty.
    Type(TypeHandle),

    /// A global constructor or destructor. This is another de facto standard
    /// extension (I think originally from `g++`?) that is not actually part of
    /// the standard proper.
    GlobalCtorDtor(GlobalCtorDtor),
}

impl Parse for MangledName {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(MangledName, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        if let Ok(tail) = consume(b"_Z", input).or_else(|_| consume(b"__Z", input)) {
            let (encoding, tail) = Encoding::parse(ctx, subs, tail)?;
            let (_, tail) = zero_or_more::<CloneSuffix>(ctx, subs, tail)?;
            return Ok((MangledName::Encoding(encoding), tail));
        }

        if let Ok(tail) = consume(b"___Z", input).or_else(|_| consume(b"____Z", input)) {
            let (encoding, tail) = Encoding::parse(ctx, subs, tail)?;
            let tail = consume(b"_block_invoke", tail)?;

            let tail_opt = match consume(b"_", tail).or_else(|_| consume(b".", tail)) {
                Ok(tail) => Some(parse_number(10, false, tail)?),
                Err(_) => parse_number(10, false, tail).ok(),
            };

            let (digits, tail) = match tail_opt {
                Some((digits, tail)) => (Some(digits), tail),
                None => (None, tail),
            };

            return Ok((MangledName::BlockInvoke(encoding, digits), tail));
        }

        if let Ok(tail) = consume(b"_GLOBAL_", input) {
            let (global_ctor_dtor, tail) = GlobalCtorDtor::parse(ctx, subs, tail)?;
            return Ok((MangledName::GlobalCtorDtor(global_ctor_dtor), tail));
        }

        // The libiberty tests also specify that a type can be top level,
        // and they are not prefixed with "_Z".
        let (ty, tail) = TypeHandle::parse(ctx, subs, input)?;
        Ok((MangledName::Type(ty), tail))
    }
}

impl<'subs> Demangle<'subs> for MangledName {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        match *self {
            MangledName::Encoding(ref enc) => {
                enc.demangle(ctx, scope);
            }
            MangledName::BlockInvoke(ref enc, _) => {
                ctx.push("invocation function for block in ", Colors::comment());
                enc.demangle(ctx, scope);
            }
            MangledName::Type(ref ty) => ty.demangle(ctx, scope),
            MangledName::GlobalCtorDtor(ref gcd) => gcd.demangle(ctx, scope),
        }
    }
}

/// The `<encoding>` production.
///
/// ```text
/// <encoding> ::= <function name> <bare-function-type>
///            ::= <data name>
///            ::= <special-name>
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Encoding {
    /// An encoded function.
    Function(Name, BareFunctionType),

    /// An encoded static variable.
    Data(Name),

    /// A special encoding.
    Special(SpecialName),
}

impl Parse for Encoding {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(Encoding, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        if let Ok((name, tail)) = Name::parse(ctx, subs, input) {
            if let Ok((ty, tail)) = BareFunctionType::parse(ctx, subs, tail) {
                return Ok((Encoding::Function(name, ty), tail));
            } else {
                return Ok((Encoding::Data(name), tail));
            }
        }

        let (name, tail) = SpecialName::parse(ctx, subs, input)?;
        Ok((Encoding::Special(name), tail))
    }
}

impl<'subs> Demangle<'subs> for Encoding {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        inner_barrier!(ctx);

        match *self {
            Encoding::Function(ref name, ref fun_ty) => {
                // Even if this function takes no args and doesn't have a return
                // value (see below), it will have the void parameter.
                debug_assert!(!fun_ty.0.is_empty());

                let scope = if let Some(leaf) = name.get_leaf_name(ctx.subs) {
                    match leaf {
                        LeafName::SourceName(leaf) => scope.push(leaf),
                        LeafName::WellKnownComponent(leaf) => scope.push(leaf),
                        LeafName::Closure(leaf) => scope.push(leaf),
                        LeafName::UnnamedType(leaf) => scope.push(leaf),
                    }
                } else {
                    scope
                };

                // Whether the first type in the BareFunctionType is a return
                // type or parameter depends on the context in which it
                // appears.
                //
                // * Templates and functions in a type or parameter position
                // have return types, unless they are constructors, destructors,
                // or conversion operator functions.
                //
                // * Non-template functions that are not in a type or parameter
                // position do not have a return type.
                //
                // We know we are not printing a type, so we only need to check
                // whether this is a template.
                //
                // For the details, see
                // https://itanium-cxx-abi.github.io/cxx-abi/abi.html#mangle.function-type
                let scope = if let Some(template_args) = name.get_template_args(ctx.subs) {
                    let scope = scope.push(template_args);
                    if !name.is_ctor_dtor_conversion(ctx.subs) {
                        fun_ty.0[0].demangle(ctx, scope);
                        ctx.push(" ", Colors::spacing());
                    }

                    scope
                } else {
                    scope
                };

                name.demangle(ctx, scope);
            }
            Encoding::Data(ref name) => name.demangle(ctx, scope),
            Encoding::Special(ref name) => name.demangle(ctx, scope),
        }
    }
}

impl<'subs> DemangleAsInner<'subs> for Encoding {
    fn demangle_as_inner<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        if let Encoding::Function(ref name, ref fun_ty) = *self {
            let (scope, function_args) =
                if let Some(template_args) = name.get_template_args(ctx.subs) {
                    let scope = scope.push(template_args);
                    let function_args = FunctionArgListAndReturnType::new(&fun_ty.0);
                    (scope, function_args as &dyn DemangleAsInner)
                } else {
                    let function_args = FunctionArgList::new(&fun_ty.0);
                    (scope, function_args as &dyn DemangleAsInner)
                };
            function_args.demangle_as_inner(ctx, scope)
        } else {
            unreachable!("we only push Encoding::Function onto the inner stack");
        }
    }
}

/// <clone-suffix> ::= [ . <clone-type-identifier> ] [ . <nonnegative number> ]*

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct CloneSuffix(CloneTypeIdentifier, Vec<isize>);

impl Parse for CloneSuffix {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(CloneSuffix, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        let tail = consume(b".", input)?;
        let (identifier, mut tail) = CloneTypeIdentifier::parse(ctx, subs, tail)?;

        let mut numbers = Vec::with_capacity(1);
        while let Ok((n, t)) = consume(b".", tail).and_then(|t| parse_number(10, false, t)) {
            numbers.push(n);
            tail = t;
        }

        let clone_suffix = CloneSuffix(identifier, numbers);
        Ok((clone_suffix, tail))
    }
}

impl<'subs> Demangle<'subs> for CloneSuffix {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        ctx.push("[", Colors::brackets());
        ctx.push("clone", Colors::item());
        self.0.demangle(ctx, scope);
        for nonnegative in &self.1 {
            ctx.push(".", Colors::delimiter());
            ctx.push_owned(format!("{nonnegative}"), Colors::comment());
        }
        ctx.push("]", Colors::brackets());
    }
}

/// A global constructor or destructor.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum GlobalCtorDtor {
    /// A global constructor.
    Ctor(Box<MangledName>),
    /// A global destructor.
    Dtor(Box<MangledName>),
}

impl Parse for GlobalCtorDtor {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(GlobalCtorDtor, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        let tail = match input.next_or(error::Error::UnexpectedEnd)? {
            (b'_', t) | (b'.', t) | (b'$', t) => t,
            _ => return Err(error::Error::UnexpectedText),
        };

        match tail.next_or(error::Error::UnexpectedEnd)? {
            (b'I', tail) => {
                let tail = consume(b"_", tail)?;
                let (name, tail) = MangledName::parse(ctx, subs, tail)?;
                Ok((GlobalCtorDtor::Ctor(Box::new(name)), tail))
            }
            (b'D', tail) => {
                let tail = consume(b"_", tail)?;
                let (name, tail) = MangledName::parse(ctx, subs, tail)?;
                Ok((GlobalCtorDtor::Dtor(Box::new(name)), tail))
            }
            _ => Err(error::Error::UnexpectedText),
        }
    }
}

impl<'subs> Demangle<'subs> for GlobalCtorDtor {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        inner_barrier!(ctx);

        match *self {
            GlobalCtorDtor::Ctor(ref name) => {
                ctx.push("global constructors keyed to ", Colors::comment());
                name.demangle(ctx, scope)
            }
            GlobalCtorDtor::Dtor(ref name) => {
                ctx.push("global destructors keyed to ", Colors::comment());
                name.demangle(ctx, scope)
            }
        }
    }
}

/// The `<name>` production.
///
/// ```text
/// <name> ::= <nested-name>
///        ::= <unscoped-name>
///        ::= <unscoped-template-name> <template-args>
///        ::= <local-name>
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Name {
    /// A nested name
    Nested(NestedName),

    /// An unscoped name.
    Unscoped(UnscopedName),

    /// An unscoped template.
    UnscopedTemplate(UnscopedTemplateNameHandle, TemplateArgs),

    /// A local name.
    Local(LocalName),
}

impl Parse for Name {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(Name, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        if let Ok((name, tail)) = NestedName::parse(ctx, subs, input) {
            return Ok((Name::Nested(name), tail));
        }

        if let Ok((name, tail)) = UnscopedName::parse(ctx, subs, input) {
            if tail.peek() == Some(b'I') {
                let name = UnscopedTemplateName(name);
                let idx = subs.insert(Substitutable::UnscopedTemplateName(name));
                let handle = UnscopedTemplateNameHandle::BackReference(idx);

                let (args, tail) = TemplateArgs::parse(ctx, subs, tail)?;
                return Ok((Name::UnscopedTemplate(handle, args), tail));
            } else {
                return Ok((Name::Unscoped(name), tail));
            }
        }

        if let Ok((name, tail)) = UnscopedTemplateNameHandle::parse(ctx, subs, input) {
            let (args, tail) = TemplateArgs::parse(ctx, subs, tail)?;
            return Ok((Name::UnscopedTemplate(name, args), tail));
        }

        let (name, tail) = LocalName::parse(ctx, subs, input)?;
        Ok((Name::Local(name), tail))
    }
}

impl<'subs> Demangle<'subs> for Name {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        match *self {
            Name::Nested(ref nested) => nested.demangle(ctx, scope),
            Name::Unscoped(ref unscoped) => unscoped.demangle(ctx, scope),
            Name::UnscopedTemplate(ref template, ref args) => {
                template.demangle(ctx, scope.push(args));
                args.demangle(ctx, scope)
            }
            Name::Local(ref local) => local.demangle(ctx, scope),
        }
    }
}

impl GetTemplateArgs for Name {
    fn get_template_args<'a>(&'a self, subs: &'a SubstitutionTable) -> Option<&'a TemplateArgs> {
        match *self {
            Name::UnscopedTemplate(_, ref args) => Some(args),
            Name::Nested(ref nested) => nested.get_template_args(subs),
            Name::Local(ref local) => local.get_template_args(subs),
            Name::Unscoped(_) => None,
        }
    }
}

impl<'a> GetLeafName<'a> for Name {
    fn get_leaf_name(&'a self, subs: &'a SubstitutionTable) -> Option<LeafName<'a>> {
        match *self {
            Name::UnscopedTemplate(ref templ, _) => templ.get_leaf_name(subs),
            Name::Nested(ref nested) => nested.get_leaf_name(subs),
            Name::Unscoped(ref unscoped) => unscoped.get_leaf_name(subs),
            Name::Local(ref local) => local.get_leaf_name(subs),
        }
    }
}

impl IsCtorDtorConversion for Name {
    fn is_ctor_dtor_conversion(&self, subs: &SubstitutionTable) -> bool {
        match *self {
            Name::Unscoped(ref unscoped) => unscoped.is_ctor_dtor_conversion(subs),
            Name::Nested(ref nested) => nested.is_ctor_dtor_conversion(subs),
            Name::Local(_) | Name::UnscopedTemplate(..) => false,
        }
    }
}

/// The `<unscoped-name>` production.
///
/// ```text
/// <unscoped-name> ::= <unqualified-name>
///                 ::= St <unqualified-name>   # ::std::
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum UnscopedName {
    /// An unqualified name.
    Unqualified(UnqualifiedName),

    /// A name within the `std::` namespace.
    Std(UnqualifiedName),
}

impl Parse for UnscopedName {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(UnscopedName, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        if let Ok(tail) = consume(b"St", input) {
            let (name, tail) = UnqualifiedName::parse(ctx, subs, tail)?;
            return Ok((UnscopedName::Std(name), tail));
        }

        let (name, tail) = UnqualifiedName::parse(ctx, subs, input)?;
        Ok((UnscopedName::Unqualified(name), tail))
    }
}

impl<'subs> Demangle<'subs> for UnscopedName {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        match *self {
            UnscopedName::Unqualified(ref unqualified) => unqualified.demangle(ctx, scope),
            UnscopedName::Std(ref std) => {
                ctx.push("std", Colors::item());
                ctx.push("::", Colors::delimiter());
                std.demangle(ctx, scope)
            }
        }
    }
}

impl<'a> GetLeafName<'a> for UnscopedName {
    fn get_leaf_name(&'a self, subs: &'a SubstitutionTable) -> Option<LeafName<'a>> {
        match *self {
            UnscopedName::Unqualified(ref name) | UnscopedName::Std(ref name) => {
                name.get_leaf_name(subs)
            }
        }
    }
}

impl IsCtorDtorConversion for UnscopedName {
    fn is_ctor_dtor_conversion(&self, subs: &SubstitutionTable) -> bool {
        match *self {
            UnscopedName::Unqualified(ref name) | UnscopedName::Std(ref name) => {
                name.is_ctor_dtor_conversion(subs)
            }
        }
    }
}

/// The `<unscoped-template-name>` production.
///
/// ```text
/// <unscoped-template-name> ::= <unscoped-name>
///                          ::= <substitution>
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct UnscopedTemplateName(pub UnscopedName);

define_handle! {
    /// A handle to an `UnscopedTemplateName`.
    pub(crate) enum UnscopedTemplateNameHandle {
        /// A handle to some `<unscoped-name>` component that isn't by itself
        /// substitutable.
        extra NonSubstitution(NonSubstitution),
    }
}

impl Parse for UnscopedTemplateNameHandle {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(UnscopedTemplateNameHandle, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        if let Ok((name, tail)) = UnscopedName::parse(ctx, subs, input) {
            let name = UnscopedTemplateName(name);
            let idx = subs.insert(Substitutable::UnscopedTemplateName(name));
            let handle = UnscopedTemplateNameHandle::BackReference(idx);
            return Ok((handle, tail));
        }

        let (sub, tail) = Substitution::parse(ctx, subs, input)?;

        match sub {
            Substitution::WellKnown(component) => {
                Ok((UnscopedTemplateNameHandle::WellKnown(component), tail))
            }
            Substitution::BackReference(idx) => {
                // TODO: should this check/assert that subs[idx] is an
                // UnscopedTemplateName?
                Ok((UnscopedTemplateNameHandle::BackReference(idx), tail))
            }
        }
    }
}

impl<'subs> Demangle<'subs> for UnscopedTemplateName {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        self.0.demangle(ctx, scope)
    }
}

impl<'a> GetLeafName<'a> for UnscopedTemplateName {
    fn get_leaf_name(&'a self, subs: &'a SubstitutionTable) -> Option<LeafName<'a>> {
        self.0.get_leaf_name(subs)
    }
}

/// The `<nested-name>` production.
///
/// ```text
/// <nested-name> ::= N [<CV-qualifiers>] [<ref-qualifier>] <prefix> <unqualified-name> E
///               ::= N [<CV-qualifiers>] [<ref-qualifier>] <template-prefix> <template-args> E
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum NestedName {
    /// A nested name.
    Unqualified(
        CvQualifiers,
        Option<RefQualifier>,
        PrefixHandle,
        UnqualifiedName,
    ),

    /// A nested template name. The `<template-args>` are part of the `PrefixHandle`.
    Template(CvQualifiers, Option<RefQualifier>, PrefixHandle),
}

impl Parse for NestedName {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(NestedName, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        let tail = consume(b"N", input)?;

        let (cv_qualifiers, tail) = if let Ok((q, tail)) = CvQualifiers::parse(ctx, subs, tail) {
            (q, tail)
        } else {
            (Default::default(), tail)
        };

        let (ref_qualifier, tail) = if let Ok((r, tail)) = RefQualifier::parse(ctx, subs, tail) {
            (Some(r), tail)
        } else {
            (None, tail)
        };

        let (prefix, tail) = PrefixHandle::parse(ctx, subs, tail)?;
        let tail = consume(b"E", tail)?;

        let substitutable = match prefix {
            PrefixHandle::BackReference(idx) => subs.get(idx),
            PrefixHandle::NonSubstitution(NonSubstitution(idx)) => subs.get_non_substitution(idx),
            PrefixHandle::WellKnown(_) => None,
        };

        match substitutable {
            Some(&Substitutable::Prefix(Prefix::Nested(ref prefix, ref name))) => Ok((
                NestedName::Unqualified(cv_qualifiers, ref_qualifier, prefix.clone(), name.clone()),
                tail,
            )),
            Some(&Substitutable::Prefix(Prefix::Template(..))) => Ok((
                NestedName::Template(cv_qualifiers, ref_qualifier, prefix),
                tail,
            )),
            _ => Err(error::Error::UnexpectedText),
        }
    }
}

impl NestedName {
    /// Get the ref-qualifier for this name, if one exists.
    pub(crate) fn ref_qualifier(&self) -> Option<&RefQualifier> {
        match *self {
            NestedName::Unqualified(_, Some(ref r), ..)
            | NestedName::Template(_, Some(ref r), ..) => Some(r),
            _ => None,
        }
    }

    // Not public because the prefix means different things for different
    // variants, and for `::Template` it actually contains part of what
    // conceptually belongs to `<nested-name>`.
    fn prefix(&self) -> &PrefixHandle {
        match *self {
            NestedName::Unqualified(_, _, ref p, _) | NestedName::Template(_, _, ref p) => p,
        }
    }
}

impl<'subs> Demangle<'subs> for NestedName {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        match *self {
            NestedName::Unqualified(_, _, ref p, ref name) => {
                p.demangle(ctx, scope);
                if name.accepts_double_colon() {
                    ctx.push("::", Colors::delimiter());
                }
                name.demangle(ctx, scope);
            }
            NestedName::Template(_, _, ref p) => {
                ctx.is_template_prefix_in_nested_name = true;
                p.demangle(ctx, scope);
                ctx.is_template_prefix_in_nested_name = false;
            }
        }

        if let Some(inner) = ctx.pop_inner() {
            inner.demangle_as_inner(ctx, scope);
        }

        if let Some(refs) = self.ref_qualifier() {
            ctx.ensure_space();
            refs.demangle(ctx, scope);
        }
    }
}

impl GetTemplateArgs for NestedName {
    fn get_template_args<'a>(&'a self, subs: &'a SubstitutionTable) -> Option<&'a TemplateArgs> {
        match *self {
            NestedName::Template(_, _, ref prefix) => prefix.get_template_args(subs),
            _ => None,
        }
    }
}

impl<'a> GetLeafName<'a> for NestedName {
    fn get_leaf_name(&'a self, subs: &'a SubstitutionTable) -> Option<LeafName<'a>> {
        match *self {
            NestedName::Unqualified(_, _, ref prefix, ref name) => {
                name.get_leaf_name(subs).or_else(|| prefix.get_leaf_name(subs))
            }
            NestedName::Template(_, _, ref prefix) => prefix.get_leaf_name(subs),
        }
    }
}

impl IsCtorDtorConversion for NestedName {
    fn is_ctor_dtor_conversion(&self, subs: &SubstitutionTable) -> bool {
        self.prefix().is_ctor_dtor_conversion(subs)
    }
}

/// The `<prefix>` production.
///
/// ```text
/// <prefix> ::= <unqualified-name>
///          ::= <prefix> <unqualified-name>
///          ::= <template-prefix> <template-args>
///          ::= <template-param>
///          ::= <decltype>
///          ::= <prefix> <data-member-prefix>
///          ::= <substitution>
///
/// <template-prefix> ::= <template unqualified-name>
///                   ::= <prefix> <template unqualified-name>
///                   ::= <template-param>
///                   ::= <substitution>
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Prefix {
    /// An unqualified name.
    Unqualified(UnqualifiedName),

    /// Some nested name.
    Nested(PrefixHandle, UnqualifiedName),

    /// A prefix and template arguments.
    Template(PrefixHandle, TemplateArgs),

    /// A template parameter.
    TemplateParam(TemplateParam),

    /// A decltype.
    Decltype(Decltype),

    /// A prefix and data member.
    DataMember(PrefixHandle, DataMemberPrefix),
}

impl GetTemplateArgs for Prefix {
    fn get_template_args<'a>(&'a self, _: &'a SubstitutionTable) -> Option<&'a TemplateArgs> {
        match *self {
            Prefix::Template(_, ref args) => Some(args),
            Prefix::Unqualified(_)
            | Prefix::Nested(_, _)
            | Prefix::TemplateParam(_)
            | Prefix::Decltype(_)
            | Prefix::DataMember(_, _) => None,
        }
    }
}

define_handle! {
    /// A reference to a parsed `<prefix>` production.
    pub(crate) enum PrefixHandle {
        /// A handle to some `<prefix>` component that isn't by itself
        /// substitutable; instead, it's only substitutable *with* its parent
        /// component.
        extra NonSubstitution(NonSubstitution),
    }
}

impl Parse for PrefixHandle {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(PrefixHandle, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        #[inline]
        fn save(
            subs: &mut SubstitutionTable,
            prefix: Prefix,
            tail_tail: IndexStr<'_>,
        ) -> PrefixHandle {
            if let Some(b'E') = tail_tail.peek() {
                // An `E` means that we just finished parsing a `<nested-name>`
                // and this final set of prefixes isn't substitutable itself,
                // only as part of the whole `<nested-name>`. Since they are
                // effectively equivalent, it doesn't make sense to add entries
                // for both.
                let idx = subs.insert_non_substitution(Substitutable::Prefix(prefix));
                PrefixHandle::NonSubstitution(NonSubstitution(idx))
            } else {
                let idx = subs.insert(Substitutable::Prefix(prefix));
                PrefixHandle::BackReference(idx)
            }
        }

        let mut tail = input;
        let mut current = None;

        loop {
            try_begin_parse!(ctx);

            match tail.peek() {
                Some(b'E') | None => {
                    if let Some(handle) = current {
                        return Ok((handle, tail));
                    } else {
                        return Err(error::Error::UnexpectedEnd);
                    }
                }
                Some(b'S') => {
                    // <prefix> ::= <substitution>
                    let (sub, tail_tail) = Substitution::parse(ctx, subs, tail)?;
                    current = Some(match sub {
                        Substitution::WellKnown(component) => PrefixHandle::WellKnown(component),
                        Substitution::BackReference(idx) => {
                            // TODO: do we need to check that the idx actually points to
                            // a Prefix?
                            PrefixHandle::BackReference(idx)
                        }
                    });
                    tail = tail_tail;
                }
                Some(b'T') => {
                    // <prefix> ::= <template-param>
                    let (param, tail_tail) = TemplateParam::parse(ctx, subs, tail)?;
                    current = Some(save(subs, Prefix::TemplateParam(param), tail_tail));
                    tail = tail_tail;
                }
                Some(b'D') => {
                    // Either
                    //
                    //     <prefix> ::= <decltype>
                    //
                    // or
                    //
                    //     <prefix> ::= <unqualified-name> ::= <ctor-dtor-name>
                    if let Ok((decltype, tail_tail)) = Decltype::parse(ctx, subs, tail) {
                        current = Some(save(subs, Prefix::Decltype(decltype), tail_tail));
                        tail = tail_tail;
                    } else {
                        let (name, tail_tail) = UnqualifiedName::parse(ctx, subs, tail)?;
                        let prefix = match current {
                            None => Prefix::Unqualified(name),
                            Some(handle) => Prefix::Nested(handle, name),
                        };
                        current = Some(save(subs, prefix, tail_tail));
                        tail = tail_tail;
                    }
                }
                Some(b'I')
                    if current.is_some() && current.as_ref().unwrap().is_template_prefix() =>
                {
                    // <prefix> ::= <template-prefix> <template-args>
                    let (args, tail_tail) = TemplateArgs::parse(ctx, subs, tail)?;
                    let prefix = Prefix::Template(current.unwrap(), args);
                    current = Some(save(subs, prefix, tail_tail));
                    tail = tail_tail;
                }
                Some(c) if current.is_some() && SourceName::starts_with(c) => {
                    // Either
                    //
                    //     <prefix> ::= <unqualified-name> ::= <source-name>
                    //
                    // or
                    //
                    //     <prefix> ::= <data-member-prefix> ::= <prefix> <source-name> M
                    debug_assert!(SourceName::starts_with(c));
                    debug_assert!(DataMemberPrefix::starts_with(c));

                    let (name, tail_tail) = SourceName::parse(ctx, subs, tail)?;
                    if tail_tail.peek() == Some(b'M') {
                        let prefix = Prefix::DataMember(current.unwrap(), DataMemberPrefix(name));
                        current = Some(save(subs, prefix, tail_tail));
                        tail = consume(b"M", tail_tail).unwrap();
                    } else {
                        let name = UnqualifiedName::Source(name);
                        let prefix = match current {
                            None => Prefix::Unqualified(name),
                            Some(handle) => Prefix::Nested(handle, name),
                        };
                        current = Some(save(subs, prefix, tail_tail));
                        tail = tail_tail;
                    }
                }
                Some(c) if UnqualifiedName::starts_with(c, &tail) => {
                    // <prefix> ::= <unqualified-name>
                    let (name, tail_tail) = UnqualifiedName::parse(ctx, subs, tail)?;
                    let prefix = match current {
                        None => Prefix::Unqualified(name),
                        Some(handle) => Prefix::Nested(handle, name),
                    };
                    current = Some(save(subs, prefix, tail_tail));
                    tail = tail_tail;
                }
                Some(_) => {
                    if let Some(handle) = current {
                        return Ok((handle, tail));
                    } else if tail.is_empty() {
                        return Err(error::Error::UnexpectedEnd);
                    } else {
                        return Err(error::Error::UnexpectedText);
                    }
                }
            }
        }
    }
}

impl<'a> GetLeafName<'a> for Prefix {
    fn get_leaf_name(&'a self, subs: &'a SubstitutionTable) -> Option<LeafName<'a>> {
        match *self {
            Prefix::Nested(ref prefix, ref name) => {
                name.get_leaf_name(subs).or_else(|| prefix.get_leaf_name(subs))
            }
            Prefix::Unqualified(ref name) => name.get_leaf_name(subs),
            Prefix::Template(ref prefix, _) => prefix.get_leaf_name(subs),
            Prefix::DataMember(_, ref name) => name.get_leaf_name(subs),
            Prefix::TemplateParam(_) | Prefix::Decltype(_) => None,
        }
    }
}

impl GetTemplateArgs for PrefixHandle {
    // XXX: Not an impl GetTemplateArgs for PrefixHandle because the 'me
    // reference to self may not live long enough.
    fn get_template_args<'a>(&'a self, subs: &'a SubstitutionTable) -> Option<&'a TemplateArgs> {
        match *self {
            PrefixHandle::BackReference(idx) => {
                if let Some(Substitutable::Prefix(p)) = subs.get(idx) {
                    p.get_template_args(subs)
                } else {
                    None
                }
            }
            PrefixHandle::NonSubstitution(NonSubstitution(idx)) => {
                if let Some(Substitutable::Prefix(p)) = subs.get_non_substitution(idx) {
                    p.get_template_args(subs)
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

impl<'subs> Demangle<'subs> for Prefix {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        if ctx.is_template_prefix {
            ctx.is_template_prefix = false;
        } else if ctx.is_template_prefix_in_nested_name {
            ctx.is_template_prefix_in_nested_name = false;
        }

        match *self {
            Prefix::Unqualified(ref unqualified) => unqualified.demangle(ctx, scope),
            Prefix::Nested(ref prefix, ref unqualified) => {
                prefix.demangle(ctx, scope);
                if unqualified.accepts_double_colon() {
                    ctx.push("::", Colors::delimiter());
                }
                unqualified.demangle(ctx, scope)
            }
            Prefix::Template(ref prefix, ref args) => {
                ctx.is_template_prefix = true;
                prefix.demangle(ctx, scope);
                ctx.is_template_prefix = false;
                args.demangle(ctx, scope)
            }
            Prefix::TemplateParam(ref param) => param.demangle(ctx, scope),
            Prefix::Decltype(ref dt) => dt.demangle(ctx, scope),
            Prefix::DataMember(ref prefix, ref member) => {
                prefix.demangle(ctx, scope);
                ctx.push("::", Colors::delimiter());
                member.demangle(ctx, scope)
            }
        }
    }
}

impl IsCtorDtorConversion for Prefix {
    fn is_ctor_dtor_conversion(&self, subs: &SubstitutionTable) -> bool {
        match *self {
            Prefix::Unqualified(ref unqualified) | Prefix::Nested(_, ref unqualified) => {
                unqualified.is_ctor_dtor_conversion(subs)
            }
            Prefix::Template(ref prefix, _) => prefix.is_ctor_dtor_conversion(subs),
            _ => false,
        }
    }
}

impl IsCtorDtorConversion for PrefixHandle {
    fn is_ctor_dtor_conversion(&self, subs: &SubstitutionTable) -> bool {
        match *self {
            PrefixHandle::BackReference(idx) => {
                if let Some(sub) = subs.get(idx) {
                    sub.is_ctor_dtor_conversion(subs)
                } else {
                    false
                }
            }
            PrefixHandle::NonSubstitution(NonSubstitution(idx)) => {
                if let Some(sub) = subs.get_non_substitution(idx) {
                    sub.is_ctor_dtor_conversion(subs)
                } else {
                    false
                }
            }
            PrefixHandle::WellKnown(_) => false,
        }
    }
}

impl PrefixHandle {
    // Is this <prefix> also a valid <template-prefix> production? Not to be
    // confused with the `GetTemplateArgs` trait.
    fn is_template_prefix(&self) -> bool {
        match *self {
            PrefixHandle::BackReference(_) | PrefixHandle::WellKnown(_) => true,
            PrefixHandle::NonSubstitution(_) => false,
        }
    }
}

/// The `<unqualified-name>` production.
///
/// ```text
/// <unqualified-name> ::= <operator-name>
///                    ::= <ctor-dtor-name>
///                    ::= <source-name>
///                    ::= <local-source-name>
///                    ::= <unnamed-type-name>
///                    ::= <abi-tag>
///                    ::= <closure-type-name>
///
/// # I think this is from an older version of the standard. It isn't in the
/// # current version, but all the other demanglers support it, so we will too.
/// <local-source-name> ::= L <source-name> [<discriminator>]
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum UnqualifiedName {
    /// An operator name.
    Operator(OperatorName),
    /// A constructor or destructor name.
    CtorDtor(CtorDtorName),
    /// A source name.
    Source(SourceName),
    /// A local source name.
    LocalSourceName(SourceName, Option<Discriminator>),
    /// A generated name for an unnamed type.
    UnnamedType(UnnamedTypeName),
    /// An ABI tag.
    ABITag(TaggedName),
    /// A closure type name
    ClosureType(ClosureTypeName),
}

impl Parse for UnqualifiedName {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(UnqualifiedName, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        if let Ok((op, tail)) = OperatorName::parse(ctx, subs, input) {
            return Ok((UnqualifiedName::Operator(op), tail));
        }

        if let Ok((ctor_dtor, tail)) = CtorDtorName::parse(ctx, subs, input) {
            return Ok((UnqualifiedName::CtorDtor(ctor_dtor), tail));
        }

        if let Ok(tail) = consume(b"L", input) {
            let (name, tail) = SourceName::parse(ctx, subs, tail)?;
            let (discr, tail) = if let Ok((d, t)) = Discriminator::parse(ctx, subs, tail) {
                (Some(d), t)
            } else {
                (None, tail)
            };
            return Ok((UnqualifiedName::LocalSourceName(name, discr), tail));
        }

        if let Ok((source, tail)) = SourceName::parse(ctx, subs, input) {
            return Ok((UnqualifiedName::Source(source), tail));
        }

        if let Ok((tagged, tail)) = TaggedName::parse(ctx, subs, input) {
            return Ok((UnqualifiedName::ABITag(tagged), tail));
        }

        if let Ok((closure, tail)) = ClosureTypeName::parse(ctx, subs, input) {
            return Ok((UnqualifiedName::ClosureType(closure), tail));
        }

        UnnamedTypeName::parse(ctx, subs, input)
            .map(|(unnamed, tail)| (UnqualifiedName::UnnamedType(unnamed), tail))
    }
}

impl<'subs> Demangle<'subs> for UnqualifiedName {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        match *self {
            UnqualifiedName::Operator(ref op_name) => {
                ctx.push("operator", Colors::known());
                op_name.demangle(ctx, scope)
            }
            UnqualifiedName::CtorDtor(ref ctor_dtor) => ctor_dtor.demangle(ctx, scope),
            UnqualifiedName::Source(ref name) | UnqualifiedName::LocalSourceName(ref name, ..) => {
                name.demangle(ctx, scope)
            }
            UnqualifiedName::UnnamedType(ref unnamed) => unnamed.demangle(ctx, scope),
            UnqualifiedName::ABITag(ref tagged) => tagged.demangle(ctx, scope),
            UnqualifiedName::ClosureType(ref closure) => closure.demangle(ctx, scope),
        }
    }
}

impl<'a> GetLeafName<'a> for UnqualifiedName {
    fn get_leaf_name(&'a self, subs: &'a SubstitutionTable) -> Option<LeafName<'a>> {
        match *self {
            UnqualifiedName::ABITag(_)
            | UnqualifiedName::Operator(_)
            | UnqualifiedName::CtorDtor(_) => None,
            UnqualifiedName::UnnamedType(ref name) => Some(LeafName::UnnamedType(name)),
            UnqualifiedName::ClosureType(ref closure) => closure.get_leaf_name(subs),
            UnqualifiedName::Source(ref name) | UnqualifiedName::LocalSourceName(ref name, _) => {
                Some(LeafName::SourceName(name))
            }
        }
    }
}

impl IsCtorDtorConversion for UnqualifiedName {
    fn is_ctor_dtor_conversion(&self, _: &SubstitutionTable) -> bool {
        match *self {
            UnqualifiedName::CtorDtor(_)
            | UnqualifiedName::Operator(OperatorName::Conversion(_)) => true,
            UnqualifiedName::Operator(_)
            | UnqualifiedName::Source(_)
            | UnqualifiedName::LocalSourceName(..)
            | UnqualifiedName::UnnamedType(_)
            | UnqualifiedName::ClosureType(_)
            | UnqualifiedName::ABITag(_) => false,
        }
    }
}

impl UnqualifiedName {
    #[inline]
    fn starts_with(byte: u8, input: &IndexStr) -> bool {
        byte == b'L'
            || OperatorName::starts_with(byte)
            || CtorDtorName::starts_with(byte)
            || SourceName::starts_with(byte)
            || UnnamedTypeName::starts_with(byte)
            || TaggedName::starts_with(byte)
            || ClosureTypeName::starts_with(byte, input)
    }

    fn accepts_double_colon(&self) -> bool {
        match *self {
            UnqualifiedName::Operator(_)
            | UnqualifiedName::CtorDtor(_)
            | UnqualifiedName::Source(_)
            | UnqualifiedName::LocalSourceName(..)
            | UnqualifiedName::UnnamedType(_)
            | UnqualifiedName::ClosureType(_) => true,
            UnqualifiedName::ABITag(_) => false,
        }
    }
}

/// The `<source-name>` non-terminal.
///
/// ```text
/// <source-name> ::= <positive length number> <identifier>
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct SourceName(pub Identifier);

impl Parse for SourceName {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(SourceName, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        let (source_name_len, input) = parse_number(10, false, input)?;
        debug_assert!(source_name_len >= 0);
        if source_name_len == 0 {
            return Err(error::Error::UnexpectedText);
        }

        let (head, tail) = match input.try_split_at(source_name_len as _) {
            Some((head, tail)) => (head, tail),
            None => return Err(error::Error::UnexpectedEnd),
        };

        let (identifier, empty) = Identifier::parse(ctx, subs, head)?;
        if !empty.is_empty() {
            return Err(error::Error::UnexpectedText);
        }

        let source_name = SourceName(identifier);
        Ok((source_name, tail))
    }
}

impl<'subs> ArgScope<'subs, 'subs> for SourceName {
    fn leaf_name(&'subs self) -> Result<LeafName<'subs>> {
        Ok(LeafName::SourceName(self))
    }

    fn get_template_arg(
        &'subs self,
        _: usize,
    ) -> Result<(&'subs TemplateArg, &'subs TemplateArgs)> {
        Err(error::Error::BadTemplateArgReference)
    }

    fn get_function_arg(&'subs self, _: usize) -> Result<&'subs Type> {
        Err(error::Error::BadFunctionArgReference)
    }
}

impl SourceName {
    #[inline]
    fn starts_with(byte: u8) -> bool {
        byte.is_ascii_digit()
    }
}

impl<'subs> Demangle<'subs> for SourceName {
    #[inline]
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        self.0.demangle(ctx, scope)
    }
}

/// The `<tagged-name>` non-terminal.
///
/// ```text
/// <tagged-name> ::= <name> B <source-name>
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct TaggedName(pub SourceName);

impl Parse for TaggedName {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(TaggedName, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        let tail = consume(b"B", input)?;
        let (source_name, tail) = SourceName::parse(ctx, subs, tail)?;
        Ok((TaggedName(source_name), tail))
    }
}

impl<'subs> Demangle<'subs> for TaggedName {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        ctx.push("[", Colors::brackets());
        ctx.push("abi", Colors::annotation());
        ctx.push(":", Colors::delimiter());
        self.0.demangle(ctx, scope);
        ctx.push("]", Colors::brackets());
    }
}

impl TaggedName {
    #[inline]
    fn starts_with(byte: u8) -> bool {
        byte == b'B'
    }
}

/// The `<identifier>` pseudo-terminal.
///
/// ```text
/// <identifier> ::= <unqualified source code identifier>
/// ```
///
/// > `<identifier>` is a pseudo-terminal representing the characters in the
/// > unqualified identifier for the entity in the source code. This ABI does not
/// > yet specify a mangling for identifiers containing characters outside of
/// > `_A-Za-z0-9.`.
///
/// Mangled symbols' identifiers also have `$` characters in the wild.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct Identifier {
    pub start: usize,
    pub end: usize,
}

impl Parse for Identifier {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        _subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(Identifier, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        if input.is_empty() {
            return Err(error::Error::UnexpectedEnd);
        }

        let end = input
            .as_ref()
            .iter()
            .map(|&c| c as char)
            .take_while(|&c| c == '$' || c == '_' || c == '.' || c.is_digit(36))
            .count();

        if end == 0 {
            return Err(error::Error::UnexpectedText);
        }

        let tail = input.range_from(end..);

        let identifier = Identifier {
            start: input.index(),
            end: tail.index(),
        };

        Ok((identifier, tail))
    }
}

impl<'subs> Demangle<'subs> for Identifier {
    #[inline]
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        _: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        let ident = &ctx.input[self.start..self.end];

        // Handle GCC's anonymous namespace mangling.
        let anon_namespace_prefix = b"_GLOBAL_";
        if ident.starts_with(anon_namespace_prefix)
            && ident.len() >= anon_namespace_prefix.len() + 2
        {
            let first = ident[anon_namespace_prefix.len()];
            let second = ident[anon_namespace_prefix.len() + 1];

            match (first, second) {
                (b'.', b'N') | (b'_', b'N') | (b'$', b'N') => {
                    ctx.push("(", Colors::brackets());
                    ctx.push("anonymous namespace", Colors::comment());
                    ctx.push(")", Colors::brackets());
                    return;
                }
                _ => {
                    // Fall through.
                }
            }
        }

        let source_name = String::from_utf8_lossy(ident);
        ctx.set_source_name(self.start, self.end);
        ctx.push_owned(source_name.into_owned(), Colors::item());
    }
}

/// The `<clone-type-identifier>` pseudo-terminal.
///
/// ```text
/// <clone-type-identifier> ::= <unqualified source code identifier>
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct CloneTypeIdentifier {
    start: usize,
    end: usize,
}

impl Parse for CloneTypeIdentifier {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        _subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(CloneTypeIdentifier, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        if input.is_empty() {
            return Err(error::Error::UnexpectedEnd);
        }

        let end = input
            .as_ref()
            .iter()
            .map(|&c| c as char)
            .take_while(|&c| c == '$' || c == '_' || c.is_digit(36))
            .count();

        if end == 0 {
            return Err(error::Error::UnexpectedText);
        }

        let tail = input.range_from(end..);

        let identifier = CloneTypeIdentifier {
            start: input.index(),
            end: tail.index(),
        };

        Ok((identifier, tail))
    }
}

impl<'subs> Demangle<'subs> for CloneTypeIdentifier {
    #[inline]
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        _: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        let ident = &ctx.input[self.start..self.end];

        let source_name = String::from_utf8_lossy(ident);
        ctx.set_source_name(self.start, self.end);
        ctx.push(".", Colors::comment());
        ctx.push_owned(source_name.into_owned(), Colors::item());
    }
}

/// The `<number>` production.
///
/// ```text
/// <number> ::= [n] <non-negative decimal integer>
/// ```
pub type Number = isize;

impl Parse for Number {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        _subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(isize, IndexStr<'b>)> {
        try_begin_parse!(ctx);
        parse_number(10, true, input)
    }
}

/// A <seq-id> production encoding a base-36 positive number.
///
/// ```text
/// <seq-id> ::= <0-9A-Z>+
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct SeqId(pub usize);

impl Parse for SeqId {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        _subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(SeqId, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        parse_number(36, false, input).map(|(num, tail)| (SeqId(num as _), tail))
    }
}

/// The `<operator-name>` production.
///
/// ```text
/// <operator-name> ::= <simple-operator-name>
///                 ::= cv <type>               # (cast)
///                 ::= li <source-name>        # operator ""
///                 ::= v <digit> <source-name> # vendor extended operator
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum OperatorName {
    /// A simple operator name.
    Simple(SimpleOperatorName),

    /// A type cast.
    Cast(TypeHandle),

    /// A type conversion.
    Conversion(TypeHandle),

    /// Operator literal, ie `operator ""`.
    Literal(SourceName),

    /// A non-standard, vendor extension operator.
    VendorExtension(u8, SourceName),
}

impl OperatorName {
    fn starts_with(byte: u8) -> bool {
        byte == b'c' || byte == b'l' || byte == b'v' || SimpleOperatorName::starts_with(byte)
    }

    fn arity(&self) -> u8 {
        match self {
            OperatorName::Cast(_) | OperatorName::Conversion(_) | OperatorName::Literal(_) => 1,
            OperatorName::Simple(ref s) => s.arity(),
            OperatorName::VendorExtension(arity, _) => *arity,
        }
    }

    fn parse_from_expr<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(Expression, IndexStr<'b>)> {
        let (operator, tail) = OperatorName::parse_internal(ctx, subs, input, true)?;

        let arity = operator.arity();
        if arity == 1 {
            let (first, tail) = Expression::parse(ctx, subs, tail)?;
            let expr = Expression::Unary(operator, Box::new(first));
            Ok((expr, tail))
        } else if arity == 2 {
            let (first, tail) = Expression::parse(ctx, subs, tail)?;
            let (second, tail) = Expression::parse(ctx, subs, tail)?;
            let expr = Expression::Binary(operator, Box::new(first), Box::new(second));
            Ok((expr, tail))
        } else if arity == 3 {
            let (first, tail) = Expression::parse(ctx, subs, tail)?;
            let (second, tail) = Expression::parse(ctx, subs, tail)?;
            let (third, tail) = Expression::parse(ctx, subs, tail)?;
            let expr =
                Expression::Ternary(operator, Box::new(first), Box::new(second), Box::new(third));
            Ok((expr, tail))
        } else {
            Err(error::Error::UnexpectedText)
        }
    }

    fn parse_internal<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
        from_expr: bool,
    ) -> Result<(OperatorName, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        if let Ok((simple, tail)) = SimpleOperatorName::parse(ctx, subs, input) {
            return Ok((OperatorName::Simple(simple), tail));
        }

        if let Ok(tail) = consume(b"cv", input) {
            // If we came through the expression path, we're a cast. If not,
            // we're a conversion.
            let previously_in_conversion = ctx.set_in_conversion(!from_expr);
            let parse_result = TypeHandle::parse(ctx, subs, tail);
            ctx.set_in_conversion(previously_in_conversion);
            let (ty, tail) = parse_result?;
            if from_expr {
                return Ok((OperatorName::Cast(ty), tail));
            } else {
                return Ok((OperatorName::Conversion(ty), tail));
            }
        }

        if let Ok(tail) = consume(b"li", input) {
            let (name, tail) = SourceName::parse(ctx, subs, tail)?;
            return Ok((OperatorName::Literal(name), tail));
        }

        let tail = consume(b"v", input)?;
        let (arity, tail) = match tail.peek() {
            Some(c) if c.is_ascii_digit() => (c - b'0', tail.range_from(1..)),
            None => return Err(error::Error::UnexpectedEnd),
            _ => return Err(error::Error::UnexpectedText),
        };
        let (name, tail) = SourceName::parse(ctx, subs, tail)?;
        Ok((OperatorName::VendorExtension(arity, name), tail))
    }
}

impl Parse for OperatorName {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(OperatorName, IndexStr<'b>)> {
        OperatorName::parse_internal(ctx, subs, input, false)
    }
}

impl<'subs> Demangle<'subs> for OperatorName {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        match *self {
            OperatorName::Simple(ref simple) => {
                match *simple {
                    SimpleOperatorName::New
                    | SimpleOperatorName::NewArray
                    | SimpleOperatorName::Delete
                    | SimpleOperatorName::DeleteArray => {
                        ctx.ensure_space();
                    }
                    _ => {}
                }
                simple.demangle(ctx, scope)
            }
            OperatorName::Cast(ref ty) | OperatorName::Conversion(ref ty) => {
                ctx.ensure_space();

                // Cast operators can refer to template arguments before they
                // actually appear in the AST, so we go traverse down the tree
                // and fetch them if they exist.
                let scope = ty.get_template_args(ctx.subs).map_or(scope, |args| scope.push(args));

                ty.demangle(ctx, scope);
            }
            OperatorName::Literal(ref name) => {
                name.demangle(ctx, scope);
                ctx.push("::", Colors::delimiter());
                ctx.push("operator ", Colors::known());
                ctx.push("\"\"", Colors::comment());
            }
            OperatorName::VendorExtension(arity, ref name) => {
                // TODO: no idea how this should be demangled...
                name.demangle(ctx, scope);
                ctx.push("::", Colors::delimiter());
                ctx.push("operator ", Colors::known());
                ctx.push_owned(format!("{arity}"), Colors::known());
            }
        }
    }
}

define_vocabulary! {
    /// The `<simple-operator-name>` production.
    #[derive(Clone, Debug, PartialEq, Eq)]
    pub(crate) enum SimpleOperatorName {
        New              (b"nw",  "new",      3),
        NewArray         (b"na",  "new[]",    3),
        Delete           (b"dl",  "delete",   1),
        DeleteArray      (b"da",  "delete[]", 1),
        UnaryPlus        (b"ps",  "+",        1),
        Neg              (b"ng",  "-",        1),
        AddressOf        (b"ad",  "&",        1),
        Deref            (b"de",  "*",        1),
        BitNot           (b"co",  "~",        1),
        Add              (b"pl",  "+",        2),
        Sub              (b"mi",  "-",        2),
        Mul              (b"ml",  "*",        2),
        Div              (b"dv",  "/",        2),
        Rem              (b"rm",  "%",        2),
        BitAnd           (b"an",  "&",        2),
        BitOr            (b"or",  "|",        2),
        BitXor           (b"eo",  "^",        2),
        Assign           (b"aS",  "=",        2),
        AddAssign        (b"pL",  "+=",       2),
        SubAssign        (b"mI",  "-=",       2),
        MulAssign        (b"mL",  "*=",       2),
        DivAssign        (b"dV",  "/=",       2),
        RemAssign        (b"rM",  "%=",       2),
        BitAndAssign     (b"aN",  "&=",       2),
        BitOrAssign      (b"oR",  "|=",       2),
        BitXorAssign     (b"eO",  "^=",       2),
        Shl              (b"ls",  "<<",       2),
        Shr              (b"rs",  ">>",       2),
        ShlAssign        (b"lS",  "<<=",      2),
        ShrAssign        (b"rS",  ">>=",      2),
        Eq               (b"eq",  "==",       2),
        Ne               (b"ne",  "!=",       2),
        Less             (b"lt",  "<",        2),
        Greater          (b"gt",  ">",        2),
        LessEq           (b"le",  "<=",       2),
        GreaterEq        (b"ge",  ">=",       2),
        Not              (b"nt",  "!",        1),
        LogicalAnd       (b"aa",  "&&",       2),
        LogicalOr        (b"oo",  "||",       2),
        PostInc          (b"pp",  "++",       1), // (postfix in <expression> context)
        PostDec          (b"mm",  "--",       1), // (postfix in <expression> context)
        Comma            (b"cm",  ",",        2),
        DerefMemberPtr   (b"pm",  "->*",      2),
        DerefMember      (b"pt",  "->",       2),
        Call             (b"cl",  "()",       2),
        Index            (b"ix",  "[]",       2),
        Question         (b"qu",  "?:",       3),
        Spaceship        (b"ss",  "<=>",      2)
    }

    impl SimpleOperatorName {
        // Automatically implemented by define_vocabulary!
        fn arity(&self) -> u8;
    }
}

/// The `<call-offset>` production.
///
/// ```text
/// <call-offset> ::= h <nv-offset> _
///               ::= v <v-offset> _
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum CallOffset {
    /// A non-virtual offset.
    NonVirtual(NvOffset),
    /// A virtual offset.
    Virtual(VOffset),
}

impl Parse for CallOffset {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(CallOffset, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        if input.is_empty() {
            return Err(error::Error::UnexpectedEnd);
        }

        if let Ok(tail) = consume(b"h", input) {
            let (offset, tail) = NvOffset::parse(ctx, subs, tail)?;
            let tail = consume(b"_", tail)?;
            return Ok((CallOffset::NonVirtual(offset), tail));
        }

        if let Ok(tail) = consume(b"v", input) {
            let (offset, tail) = VOffset::parse(ctx, subs, tail)?;
            let tail = consume(b"_", tail)?;
            return Ok((CallOffset::Virtual(offset), tail));
        }

        Err(error::Error::UnexpectedText)
    }
}

impl<'subs> Demangle<'subs> for CallOffset {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        _: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        match *self {
            CallOffset::NonVirtual(NvOffset(off)) => {
                ctx.push_owned(format!("{{offset({off})}}"), Colors::comment());
            }
            CallOffset::Virtual(VOffset(vbase, vcall)) => {
                ctx.push_owned(
                    format!("{{virtual offset({vbase}, {vcall})}}"),
                    Colors::comment(),
                );
            }
        }
    }
}

/// A non-virtual offset, as described by the <nv-offset> production.
///
/// ```text
/// <nv-offset> ::= <offset number>
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct NvOffset(pub isize);

impl Parse for NvOffset {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(NvOffset, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        Number::parse(ctx, subs, input).map(|(num, tail)| (NvOffset(num), tail))
    }
}

/// A virtual offset, as described by the <v-offset> production.
///
/// ```text
/// <v-offset> ::= <offset number> _ <virtual offset number>
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct VOffset(pub isize, pub isize);

impl Parse for VOffset {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(VOffset, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        let (offset, tail) = Number::parse(ctx, subs, input)?;
        let tail = consume(b"_", tail)?;
        let (virtual_offset, tail) = Number::parse(ctx, subs, tail)?;
        Ok((VOffset(offset, virtual_offset), tail))
    }
}

/// The `<ctor-dtor-name>` production.
///
/// ```text
/// <ctor-dtor-name> ::= C1  # complete object constructor
///                  ::= C2  # base object constructor
///                  ::= C3  # complete object allocating constructor
///                  ::= D0  # deleting destructor
///                  ::= D1  # complete object destructor
///                  ::= D2  # base object destructor
/// ```
///
/// GCC also emits a C4 constructor under some conditions when building
/// an optimized binary. GCC's source says:
///
/// /* This is the old-style "[unified]" constructor.
///    In some cases, we may emit this function and call
///    it from the clones in order to share code and save space.  */
///
/// Based on the GCC source we'll call this the "maybe in-charge constructor".
/// Similarly, there is a D4 destructor, the "maybe in-charge destructor".
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum CtorDtorName {
    /// "C1", the "complete object constructor"
    CompleteConstructor(Option<Box<Name>>),
    /// "C2", the "base object constructor"
    BaseConstructor(Option<Box<Name>>),
    /// "C3", the "complete object allocating constructor"
    CompleteAllocatingConstructor(Option<Box<Name>>),
    /// "C4", the "maybe in-charge constructor"
    MaybeInChargeConstructor(Option<Box<Name>>),
    /// "D0", the "deleting destructor"
    DeletingDestructor,
    /// "D1", the "complete object destructor"
    CompleteDestructor,
    /// "D2", the "base object destructor"
    BaseDestructor,
    /// "D4", the "maybe in-charge destructor"
    MaybeInChargeDestructor,
}

impl CtorDtorName {
    fn inheriting_mut(&mut self) -> &mut Option<Box<Name>> {
        match self {
            CtorDtorName::CompleteConstructor(ref mut inheriting)
            | CtorDtorName::BaseConstructor(ref mut inheriting)
            | CtorDtorName::CompleteAllocatingConstructor(ref mut inheriting)
            | CtorDtorName::MaybeInChargeConstructor(ref mut inheriting) => inheriting,
            CtorDtorName::DeletingDestructor
            | CtorDtorName::CompleteDestructor
            | CtorDtorName::BaseDestructor
            | CtorDtorName::MaybeInChargeDestructor => unreachable!(),
        }
    }
}

impl Parse for CtorDtorName {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(CtorDtorName, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        match input.peek() {
            Some(b'C') => {
                let mut tail = consume(b"C", input)?;
                let inheriting = match tail.peek() {
                    Some(b'I') => {
                        tail = consume(b"I", tail)?;
                        true
                    }
                    _ => false,
                };

                let mut ctor_type: CtorDtorName =
                    match tail.try_split_at(1).as_ref().map(|&(ref h, t)| (h.as_ref(), t)) {
                        None => Err(error::Error::UnexpectedEnd),
                        Some((b"1", t)) => {
                            tail = t;
                            Ok(CtorDtorName::CompleteConstructor(None))
                        }
                        Some((b"2", t)) => {
                            tail = t;
                            Ok(CtorDtorName::BaseConstructor(None))
                        }
                        Some((b"3", t)) => {
                            tail = t;
                            Ok(CtorDtorName::CompleteAllocatingConstructor(None))
                        }
                        Some((b"4", t)) => {
                            tail = t;
                            Ok(CtorDtorName::MaybeInChargeConstructor(None))
                        }
                        _ => Err(error::Error::UnexpectedText),
                    }?;

                if inheriting {
                    let (ty, tail) = Name::parse(ctx, subs, tail)?;
                    *ctor_type.inheriting_mut() = Some(Box::new(ty));
                    Ok((ctor_type, tail))
                } else {
                    Ok((ctor_type, tail))
                }
            }
            Some(b'D') => match input.try_split_at(2).as_ref().map(|&(ref h, t)| (h.as_ref(), t)) {
                Some((b"D0", tail)) => Ok((CtorDtorName::DeletingDestructor, tail)),
                Some((b"D1", tail)) => Ok((CtorDtorName::CompleteDestructor, tail)),
                Some((b"D2", tail)) => Ok((CtorDtorName::BaseDestructor, tail)),
                Some((b"D4", tail)) => Ok((CtorDtorName::MaybeInChargeDestructor, tail)),
                _ => Err(error::Error::UnexpectedText),
            },
            None => Err(error::Error::UnexpectedEnd),
            _ => Err(error::Error::UnexpectedText),
        }
    }
}

impl<'subs> Demangle<'subs> for CtorDtorName {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        let leaf = scope.leaf_name().unwrap_or_default();

        match *self {
            CtorDtorName::CompleteConstructor(ref inheriting)
            | CtorDtorName::BaseConstructor(ref inheriting)
            | CtorDtorName::CompleteAllocatingConstructor(ref inheriting)
            | CtorDtorName::MaybeInChargeConstructor(ref inheriting) => match inheriting {
                Some(ty) => ty.get_leaf_name(ctx.subs).unwrap_or_default().demangle_as_leaf(ctx),
                None => leaf.demangle_as_leaf(ctx),
            },
            CtorDtorName::DeletingDestructor
            | CtorDtorName::CompleteDestructor
            | CtorDtorName::BaseDestructor
            | CtorDtorName::MaybeInChargeDestructor => {
                ctx.push("~", Colors::item());
                leaf.demangle_as_leaf(ctx)
            }
        }
    }
}

impl CtorDtorName {
    #[inline]
    fn starts_with(byte: u8) -> bool {
        byte == b'C' || byte == b'D'
    }
}

/// The `<type>` production.
///
/// ```text
/// <type> ::= <builtin-type>
///        ::= <function-type>
///        ::= <class-enum-type>
///        ::= <array-type>
///        ::= <vector-type>
///        ::= <pointer-to-member-type>
///        ::= <template-param>
///        ::= <template-template-param> <template-args>
///        ::= <decltype>
///        ::= <CV-qualifiers> <type>
///        ::= P <type>                                 # pointer-to
///        ::= R <type>                                 # reference-to
///        ::= O <type>                                 # rvalue reference-to (C++0x)
///        ::= C <type>                                 # complex pair (C 2000)
///        ::= G <type>                                 # imaginary (C 2000)
///        ::= U <source-name> [<template-args>] <type> # vendor extended type qualifier
///        ::= Dp <type>                                # pack expansion (C++0x)
///        ::= <substitution>
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
#[allow(clippy::large_enum_variant)]
pub(crate) enum Type {
    /// A function type.
    Function(FunctionType),

    /// A class, union, or enum type.
    ClassEnum(ClassEnumType),

    /// An array type.
    Array(ArrayType),

    /// A vector type.
    Vector(VectorType),

    /// A pointer-to-member type.
    PointerToMember(PointerToMemberType),

    /// A named template parameter type.
    TemplateParam(TemplateParam),

    /// A template template type.
    TemplateTemplate(TemplateTemplateParamHandle, TemplateArgs),

    /// A decltype.
    Decltype(Decltype),

    /// A const-, restrict-, and/or volatile-qualified type.
    Qualified(CvQualifiers, TypeHandle),

    /// A pointer to a type.
    PointerTo(TypeHandle),

    /// An lvalue reference to a type.
    LvalueRef(TypeHandle),

    /// An rvalue reference to a type.
    RvalueRef(TypeHandle),

    /// A complex pair of the given type.
    Complex(TypeHandle),

    /// An imaginary of the given type.
    Imaginary(TypeHandle),

    /// A vendor extended type qualifier.
    VendorExtension(SourceName, Option<TemplateArgs>, TypeHandle),

    /// A pack expansion.
    PackExpansion(TypeHandle),
}

define_handle! {
    /// A reference to a parsed `Type` production.
    pub(crate) enum TypeHandle {
        /// A builtin type. These don't end up in the substitutions table.
        extra Builtin(BuiltinType),
    }
}

impl TypeHandle {
    fn is_void(&self) -> bool {
        matches!(
            self,
            TypeHandle::Builtin(BuiltinType::Standard(StandardBuiltinType::Void))
        )
    }
}

impl Parse for TypeHandle {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(TypeHandle, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        /// Insert the given type into the substitution table, and return a
        /// handle referencing the index in the table where it ended up.
        fn insert_and_return_handle<'b>(
            ty: Type,
            subs: &mut SubstitutionTable,
            tail: IndexStr<'b>,
        ) -> Result<(TypeHandle, IndexStr<'b>)> {
            let ty = Substitutable::Type(ty);
            let idx = subs.insert(ty);
            let handle = TypeHandle::BackReference(idx);
            Ok((handle, tail))
        }

        if let Ok((builtin, tail)) = BuiltinType::parse(ctx, subs, input) {
            // Builtin types are one of two exceptions that do not end up in the
            // substitutions table.
            let handle = TypeHandle::Builtin(builtin);
            return Ok((handle, tail));
        }

        // ::= <qualified-type>
        // We don't have a separate type for the <qualified-type> production.
        // Process these all up front, so that any ambiguity that might exist
        // with later productions is handled correctly.

        // ::= <extended-qualifier>
        if let Ok(tail) = consume(b"U", input) {
            let (name, tail) = SourceName::parse(ctx, subs, tail)?;
            let (args, tail) = if let Ok((args, tail)) = TemplateArgs::parse(ctx, subs, tail) {
                (Some(args), tail)
            } else {
                (None, tail)
            };
            let (ty, tail) = TypeHandle::parse(ctx, subs, tail)?;
            let ty = Type::VendorExtension(name, args, ty);
            return insert_and_return_handle(ty, subs, tail);
        }

        // ::= <CV-qualifiers>
        if let Ok((qualifiers, tail)) = CvQualifiers::parse(ctx, subs, input) {
            // CvQualifiers can parse successfully without consuming any input,
            // but we don't want to recurse unless we know we did consume some
            // input, lest we go into an infinite loop and blow the stack.
            if tail.len() < input.len() {
                // If the following production is a <function-type>, we want to let
                // it pick up these <CV-qualifiers>.
                if !FunctionType::starts_with(&tail) {
                    let (ty, tail) = TypeHandle::parse(ctx, subs, tail)?;
                    let ty = Type::Qualified(qualifiers, ty);
                    return insert_and_return_handle(ty, subs, tail);
                }
            }
        }

        if let Ok((ty, tail)) = ClassEnumType::parse(ctx, subs, input) {
            let ty = Type::ClassEnum(ty);
            return insert_and_return_handle(ty, subs, tail);
        }

        if let Ok((sub, tail)) = Substitution::parse(ctx, subs, input) {
            // If we see an 'I', then this is actually a substitution for a
            // <template-template-param>, and the template args are what
            // follows. Throw away what we just parsed, and re-parse it in
            // `TemplateTemplateParamHandle::parse` for now, but it would be
            // nice not to duplicate work we've already done.
            if tail.peek() != Some(b'I') {
                match sub {
                    Substitution::WellKnown(component) => {
                        return Ok((TypeHandle::WellKnown(component), tail));
                    }
                    Substitution::BackReference(idx) => {
                        // TODO: should this check if the back reference actually points
                        // to a <type>?
                        return Ok((TypeHandle::BackReference(idx), tail));
                    }
                }
            }
        }

        if let Ok((funty, tail)) = FunctionType::parse(ctx, subs, input) {
            let ty = Type::Function(funty);
            return insert_and_return_handle(ty, subs, tail);
        }

        if let Ok((ty, tail)) = ArrayType::parse(ctx, subs, input) {
            let ty = Type::Array(ty);
            return insert_and_return_handle(ty, subs, tail);
        }

        if let Ok((ty, tail)) = VectorType::parse(ctx, subs, input) {
            let ty = Type::Vector(ty);
            return insert_and_return_handle(ty, subs, tail);
        }

        if let Ok((ty, tail)) = PointerToMemberType::parse(ctx, subs, input) {
            let ty = Type::PointerToMember(ty);
            return insert_and_return_handle(ty, subs, tail);
        }

        if let Ok((param, tail)) = TemplateParam::parse(ctx, subs, input) {
            // Same situation as with `Substitution::parse` at the top of this
            // function: this is actually a <template-template-param> and
            // <template-args>.
            if tail.peek() != Some(b'I') {
                let ty = Type::TemplateParam(param);
                return insert_and_return_handle(ty, subs, tail);
            } else if ctx.in_conversion() {
                // This may be <template-template-param> <template-args>.
                // But if we're here for a conversion operator, that's only
                // possible if the grammar looks like:
                //
                // <nested-name>
                // -> <source-name> cv <template-template-param> <template-args> <template-args>
                //
                // That is, there must be *another* <template-args> production after ours.
                // If there isn't one, then this really is a <template-param>.
                //
                // NB: Parsing a <template-args> production may modify the substitutions
                // table, so we need to avoid contaminating the official copy.
                let mut tmp_subs = subs.clone();
                if let Ok((_, new_tail)) = TemplateArgs::parse(ctx, &mut tmp_subs, tail) {
                    if new_tail.peek() != Some(b'I') {
                        // Don't consume the TemplateArgs.
                        let ty = Type::TemplateParam(param);
                        return insert_and_return_handle(ty, subs, tail);
                    }
                    // We really do have a <template-template-param>. Fall through.
                    // NB: We can't use the arguments we just parsed because a
                    // TemplateTemplateParam is substitutable, and if we use it
                    // any substitutions in the arguments will come *before* it,
                    // putting the substitution table out of order.
                }
            }
        }

        if let Ok((ttp, tail)) = TemplateTemplateParamHandle::parse(ctx, subs, input) {
            let (args, tail) = TemplateArgs::parse(ctx, subs, tail)?;
            let ty = Type::TemplateTemplate(ttp, args);
            return insert_and_return_handle(ty, subs, tail);
        }

        if let Ok((param, tail)) = Decltype::parse(ctx, subs, input) {
            let ty = Type::Decltype(param);
            return insert_and_return_handle(ty, subs, tail);
        }

        if let Ok(tail) = consume(b"P", input) {
            let (ty, tail) = TypeHandle::parse(ctx, subs, tail)?;
            let ty = Type::PointerTo(ty);
            return insert_and_return_handle(ty, subs, tail);
        }

        if let Ok(tail) = consume(b"R", input) {
            let (ty, tail) = TypeHandle::parse(ctx, subs, tail)?;
            let ty = Type::LvalueRef(ty);
            return insert_and_return_handle(ty, subs, tail);
        }

        if let Ok(tail) = consume(b"O", input) {
            let (ty, tail) = TypeHandle::parse(ctx, subs, tail)?;
            let ty = Type::RvalueRef(ty);
            return insert_and_return_handle(ty, subs, tail);
        }

        if let Ok(tail) = consume(b"C", input) {
            let (ty, tail) = TypeHandle::parse(ctx, subs, tail)?;
            let ty = Type::Complex(ty);
            return insert_and_return_handle(ty, subs, tail);
        }

        if let Ok(tail) = consume(b"G", input) {
            let (ty, tail) = TypeHandle::parse(ctx, subs, tail)?;
            let ty = Type::Imaginary(ty);
            return insert_and_return_handle(ty, subs, tail);
        }

        let tail = consume(b"Dp", input)?;
        let (ty, tail) = TypeHandle::parse(ctx, subs, tail)?;
        let ty = Type::PackExpansion(ty);
        insert_and_return_handle(ty, subs, tail)
    }
}

impl GetTemplateArgs for TypeHandle {
    fn get_template_args<'a>(&'a self, subs: &'a SubstitutionTable) -> Option<&'a TemplateArgs> {
        subs.get_type(self).and_then(|ty| ty.get_template_args(subs))
    }
}

impl<'subs> Demangle<'subs> for Type {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        match *self {
            Type::Function(ref func_ty) => func_ty.demangle(ctx, scope),
            Type::ClassEnum(ref cls_enum_ty) => cls_enum_ty.demangle(ctx, scope),
            Type::Array(ref array_ty) => array_ty.demangle(ctx, scope),
            Type::Vector(ref vector_ty) => vector_ty.demangle(ctx, scope),
            Type::PointerToMember(ref ptm) => ptm.demangle(ctx, scope),
            Type::TemplateParam(ref param) => param.demangle(ctx, scope),
            Type::TemplateTemplate(ref tt_param, ref args) => {
                tt_param.demangle(ctx, scope);
                args.demangle(ctx, scope)
            }
            Type::Decltype(ref dt) => dt.demangle(ctx, scope),
            Type::Qualified(_, ref ty) => {
                ctx.push_inner(self);
                ty.demangle(ctx, scope);
                if ctx.pop_inner_if(self) {
                    self.demangle_as_inner(ctx, scope);
                }
            }
            Type::PointerTo(ref ty) | Type::LvalueRef(ref ty) | Type::RvalueRef(ref ty) => {
                ctx.push_inner(self);
                ty.demangle(ctx, scope);
                if ctx.pop_inner_if(self) {
                    self.demangle_as_inner(ctx, scope);
                }
            }
            Type::Complex(ref ty) => {
                ty.demangle(ctx, scope);
                ctx.push(" complex", Colors::annotation());
            }
            Type::Imaginary(ref ty) => {
                ty.demangle(ctx, scope);
                ctx.push(" imaginary", Colors::annotation());
            }
            Type::VendorExtension(ref name, ref template_args, ref ty) => {
                ty.demangle(ctx, scope);
                ctx.push(" ", Colors::spacing());
                name.demangle(ctx, scope);
                if let Some(ref args) = *template_args {
                    args.demangle(ctx, scope);
                }
            }
            Type::PackExpansion(ref ty) => {
                ty.demangle(ctx, scope);
                if !ctx.is_template_argument_pack {
                    ctx.push("...", Colors::item());
                }
            }
        }
    }
}

impl<'subs> DemangleAsInner<'subs> for Type {
    fn demangle_as_inner<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        match *self {
            Type::Qualified(ref quals, _) => quals.demangle_as_inner(ctx, scope),
            Type::PointerTo(_) => ctx.push("*", Colors::special()),
            Type::RvalueRef(_) => {
                while let Some(v) = ctx.inner.last().and_then(|ty| ty.downcast_to_type()) {
                    match v {
                        // Two r-value references combine into a single r-value reference
                        // Consume any adjacent r-value references on the inner stack.
                        Type::RvalueRef(_) => {
                            ctx.inner.pop().unwrap();
                        }
                        // An r-value and an l-value reference combine into an l-value reference.
                        // Skip printing this, and allow the LvalueRef implementation to
                        // continue combining references.
                        Type::LvalueRef(_) => return,
                        _ => break,
                    }
                }
                ctx.push("&&", Colors::special())
            }
            Type::LvalueRef(_) => {
                while let Some(v) = ctx.inner.last().and_then(|ty| ty.downcast_to_type()) {
                    match v {
                        // An l-value reference combines with an r-value reference to form a
                        // single l-value reference. Consume any adjacent r-value references
                        // on the inner stack.
                        Type::RvalueRef(_) => {
                            ctx.inner.pop().unwrap();
                        }
                        // Two l-value references combine to form a single l-value reference.
                        // Skip printing this, and allow the LvalueRef implementation for
                        // the next l-value reference to continue combining references.
                        Type::LvalueRef(_) => return,
                        _ => break,
                    }
                }
                ctx.push("&", Colors::special())
            }
            ref otherwise => {
                unreachable!(
                    "We shouldn't ever put any other types on the inner stack: {:?}",
                    otherwise
                );
            }
        }
    }

    fn downcast_to_type(&self) -> Option<&Type> {
        Some(self)
    }

    fn downcast_to_function_type(&self) -> Option<&FunctionType> {
        if let Type::Function(ref f) = *self {
            Some(f)
        } else {
            None
        }
    }

    fn downcast_to_array_type(&self) -> Option<&ArrayType> {
        if let Type::Array(ref arr) = *self {
            Some(arr)
        } else {
            None
        }
    }

    fn downcast_to_pointer_to_member(&self) -> Option<&PointerToMemberType> {
        if let Type::PointerToMember(ref ptm) = *self {
            Some(ptm)
        } else {
            None
        }
    }

    fn is_qualified(&self) -> bool {
        matches!(self, Type::Qualified(..))
    }
}

impl GetTemplateArgs for Type {
    fn get_template_args<'a>(&'a self, subs: &'a SubstitutionTable) -> Option<&'a TemplateArgs> {
        // TODO: This should probably recurse through all the nested type
        // handles too.

        match *self {
            Type::VendorExtension(_, Some(ref args), _) | Type::TemplateTemplate(_, ref args) => {
                Some(args)
            }
            Type::PointerTo(ref ty) | Type::LvalueRef(ref ty) | Type::RvalueRef(ref ty) => {
                ty.get_template_args(subs)
            }
            _ => None,
        }
    }
}

impl<'a> GetLeafName<'a> for Type {
    fn get_leaf_name(&'a self, subs: &'a SubstitutionTable) -> Option<LeafName<'a>> {
        match *self {
            Type::ClassEnum(ref cls_enum_ty) => cls_enum_ty.get_leaf_name(subs),
            _ => None,
        }
    }
}

/// The `<CV-qualifiers>` production.
///
/// ```text
/// <CV-qualifiers> ::= [r] [V] [K]   # restrict (C99), volatile, const
/// ```
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
pub(crate) struct CvQualifiers {
    /// Is this `restrict` qualified?
    pub restrict: bool,
    /// Is this `volatile` qualified?
    pub volatile: bool,
    /// Is this `const` qualified?
    pub konst: bool,
}

impl CvQualifiers {
    #[inline]
    fn is_empty(&self) -> bool {
        !self.restrict && !self.volatile && !self.konst
    }
}

impl Parse for CvQualifiers {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        _subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(CvQualifiers, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        let (restrict, tail) = if let Ok(tail) = consume(b"r", input) {
            (true, tail)
        } else {
            (false, input)
        };

        let (volatile, tail) = if let Ok(tail) = consume(b"V", tail) {
            (true, tail)
        } else {
            (false, tail)
        };

        let (konst, tail) = if let Ok(tail) = consume(b"K", tail) {
            (true, tail)
        } else {
            (false, tail)
        };

        let qualifiers = CvQualifiers {
            restrict,
            volatile,
            konst,
        };

        Ok((qualifiers, tail))
    }
}

impl<'subs> Demangle<'subs> for CvQualifiers {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        _: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        if self.konst {
            ctx.ensure_space();
            ctx.push("const", Colors::annotation());
        }

        if self.volatile {
            ctx.ensure_space();
            ctx.push("volatile", Colors::annotation());
        }

        if self.restrict {
            ctx.ensure_space();
            ctx.push("restrict", Colors::annotation());
        }
    }
}

impl<'subs> DemangleAsInner<'subs> for CvQualifiers {}

define_vocabulary! {
    /// A <ref-qualifier> production.
    ///
    /// ```text
    /// <ref-qualifier> ::= R   # & ref-qualifier
    ///                 ::= O   # && ref-qualifier
    /// ```
    #[derive(Clone, Debug, PartialEq, Eq)]
    pub(crate) enum RefQualifier {
        LValueRef(b"R", "&"),
        RValueRef(b"O", "&&")
    }
}

define_vocabulary! {
    /// A one of the standard variants of the <builtin-type> production.
    ///
    /// ```text
    /// <builtin-type> ::= v  # void
    ///                ::= w  # wchar_t
    ///                ::= b  # bool
    ///                ::= c  # char
    ///                ::= a  # signed char
    ///                ::= h  # unsigned char
    ///                ::= s  # short
    ///                ::= t  # unsigned short
    ///                ::= i  # int
    ///                ::= j  # unsigned int
    ///                ::= l  # long
    ///                ::= m  # unsigned long
    ///                ::= x  # long long, __int64
    ///                ::= y  # unsigned long long, __int64
    ///                ::= n  # __int128
    ///                ::= o  # unsigned __int128
    ///                ::= f  # float
    ///                ::= d  # double
    ///                ::= e  # long double, __float80
    ///                ::= g  # __float128
    ///                ::= z  # ellipsis
    ///                ::= Dd # IEEE 754r decimal floating point (64 bits)
    ///                ::= De # IEEE 754r decimal floating point (128 bits)
    ///                ::= Df # IEEE 754r decimal floating point (32 bits)
    ///                ::= Dh # IEEE 754r half-precision floating point (16 bits)
    ///                ::= Di # char32_t
    ///                ::= Ds # char16_t
    ///                ::= Du # char8_t
    ///                ::= Da # auto
    ///                ::= Dc # decltype(auto)
    ///                ::= Dn # std::nullptr_t (i.e., decltype(nullptr))
    /// ```
    #[derive(Clone, Debug, PartialEq, Eq)]
    pub(crate) enum StandardBuiltinType {
        Void             (b"v",  "void"),
        Wchar            (b"w",  "wchar_t"),
        Bool             (b"b",  "bool"),
        Char             (b"c",  "char"),
        SignedChar       (b"a",  "signed char"),
        UnsignedChar     (b"h",  "unsigned char"),
        Short            (b"s",  "short"),
        UnsignedShort    (b"t",  "unsigned short"),
        Int              (b"i",  "int"),
        UnsignedInt      (b"j",  "unsigned int"),
        Long             (b"l",  "long"),
        UnsignedLong     (b"m",  "unsigned long"),
        LongLong         (b"x",  "long long"),
        UnsignedLongLong (b"y",  "unsigned long long"),
        Int128           (b"n",  "__int128"),
        Uint128          (b"o",  "unsigned __int128"),
        Float            (b"f",  "float"),
        Double           (b"d",  "double"),
        LongDouble       (b"e",  "long double"),
        Float128         (b"g",  "__float128"),
        Ellipsis         (b"z",  "..."),
        DecimalFloat64   (b"Dd", "decimal64"),
        DecimalFloat128  (b"De", "decimal128"),
        DecimalFloat32   (b"Df", "decimal32"),
        DecimalFloat16   (b"Dh", "half"),
        Char32           (b"Di", "char32_t"),
        Char16           (b"Ds", "char16_t"),
        Char8            (b"Du", "char8_t"),
        Auto             (b"Da", "auto"),
        Decltype         (b"Dc", "decltype(auto)"),
        Nullptr          (b"Dn", "std::nullptr_t")
    }
}

/// The `<builtin-type>` production.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum BuiltinType {
    /// A standards compliant builtin type.
    Standard(StandardBuiltinType),

    /// A non-standard, vendor extension type.
    ///
    /// ```text
    /// <builtin-type> ::= u <source-name>   # vendor extended type
    /// ```
    Extension(SourceName),
}

impl Parse for BuiltinType {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(BuiltinType, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        if let Ok((ty, tail)) = StandardBuiltinType::parse(ctx, subs, input) {
            return Ok((BuiltinType::Standard(ty), tail));
        }

        let tail = consume(b"u", input)?;
        let (name, tail) = SourceName::parse(ctx, subs, tail)?;
        Ok((BuiltinType::Extension(name), tail))
    }
}

impl<'subs> Demangle<'subs> for BuiltinType {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        match *self {
            BuiltinType::Standard(ref ty) => ty.demangle(ctx, scope),
            BuiltinType::Extension(ref name) => name.demangle(ctx, scope),
        }
    }
}

impl<'a> GetLeafName<'a> for BuiltinType {
    fn get_leaf_name(&'a self, _: &'a SubstitutionTable) -> Option<LeafName<'a>> {
        None
    }
}

/// A built-in type with CV-qualifiers.
///
/// Like unqualified built-in types, CV-qualified built-in types do not go into
/// the substitutions table.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct QualifiedBuiltin(CvQualifiers, BuiltinType);

impl<'subs> Demangle<'subs> for QualifiedBuiltin {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        ctx.push_inner(&self.0);
        self.1.demangle(ctx, scope);
        if ctx.pop_inner_if(&self.0) {
            self.0.demangle_as_inner(ctx, scope);
        }
    }
}

impl<'a> GetLeafName<'a> for QualifiedBuiltin {
    fn get_leaf_name(&'a self, _: &'a SubstitutionTable) -> Option<LeafName<'a>> {
        None
    }
}

/// The `<exception-spec>` production.
///
/// <exception-spec> ::= Do                # non-throwing exception-specification (e.g., noexcept, throw())
///                  ::= DO <expression> E # computed (instantiation-dependent) noexcept
///                  ::= Dw <type>+ E      # dynamic exception specification with instantiation-dependent types
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum ExceptionSpec {
    /// noexcept
    NoExcept,
    /// noexcept(expression)
    Computed(Expression),
    // Dynamic exception specification is deprecated, lets see if we can get away with
    // not implementing it.
}

impl Parse for ExceptionSpec {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(ExceptionSpec, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        if let Ok(tail) = consume(b"Do", input) {
            return Ok((ExceptionSpec::NoExcept, tail));
        }

        let tail = consume(b"DO", input)?;
        let (expr, tail) = Expression::parse(ctx, subs, tail)?;
        let tail = consume(b"E", tail)?;
        Ok((ExceptionSpec::Computed(expr), tail))
    }
}

impl<'subs> Demangle<'subs> for ExceptionSpec {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        match *self {
            ExceptionSpec::NoExcept => ctx.push("noexcept", Colors::annotation()),
            ExceptionSpec::Computed(ref expr) => {
                ctx.push("noexcept", Colors::annotation());
                ctx.push("(", Colors::brackets());
                expr.demangle(ctx, scope);
                ctx.push(")", Colors::brackets());
            }
        }
    }
}

/// The `<function-type>` production.
///
/// ```text
/// <function-type> ::= [<CV-qualifiers>] [exception-spec] [Dx] F [Y] <bare-function-type> [<ref-qualifier>] E
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct FunctionType {
    pub cv_qualifiers: CvQualifiers,
    pub exception_spec: Option<ExceptionSpec>,
    pub transaction_safe: bool,
    pub extern_c: bool,
    pub bare: BareFunctionType,
    pub ref_qualifier: Option<RefQualifier>,
}

impl Parse for FunctionType {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(FunctionType, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        let (cv_qualifiers, tail) =
            if let Ok((cv_qualifiers, tail)) = CvQualifiers::parse(ctx, subs, input) {
                (cv_qualifiers, tail)
            } else {
                (Default::default(), input)
            };

        let (exception_spec, tail) =
            if let Ok((exception_spec, tail)) = ExceptionSpec::parse(ctx, subs, tail) {
                (Some(exception_spec), tail)
            } else {
                (None, tail)
            };

        let (transaction_safe, tail) = if let Ok(tail) = consume(b"Dx", tail) {
            (true, tail)
        } else {
            (false, tail)
        };

        let tail = consume(b"F", tail)?;

        let (extern_c, tail) = if let Ok(tail) = consume(b"Y", tail) {
            (true, tail)
        } else {
            (false, tail)
        };

        let (bare, tail) = BareFunctionType::parse(ctx, subs, tail)?;

        let (ref_qualifier, tail) =
            if let Ok((ref_qualifier, tail)) = RefQualifier::parse(ctx, subs, tail) {
                (Some(ref_qualifier), tail)
            } else {
                (None, tail)
            };

        let tail = consume(b"E", tail)?;

        let func_ty = FunctionType {
            cv_qualifiers,
            exception_spec,
            transaction_safe,
            extern_c,
            bare,
            ref_qualifier,
        };
        Ok((func_ty, tail))
    }
}

impl<'subs> Demangle<'subs> for FunctionType {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        ctx.push_inner(self);
        self.bare.demangle(ctx, scope);
        if ctx.pop_inner_if(self) {
            self.demangle_as_inner(ctx, scope);
        }
        if let Some(ref es) = self.exception_spec {
            // Print out a space before printing "noexcept"
            ctx.ensure_space();
            es.demangle(ctx, scope);
        }
    }
}

impl<'subs> DemangleAsInner<'subs> for FunctionType {
    fn demangle_as_inner<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        if !self.cv_qualifiers.is_empty() {
            self.cv_qualifiers.demangle(ctx, scope);
        }

        if let Some(ref rq) = self.ref_qualifier {
            // Print out a space before printing "&" or "&&"
            ctx.ensure_space();
            rq.demangle(ctx, scope);
        }
    }

    fn downcast_to_function_type(&self) -> Option<&FunctionType> {
        Some(self)
    }
}

impl FunctionType {
    #[inline]
    fn starts_with(input: &IndexStr) -> bool {
        input.peek() == Some(b'F')
            || (input.peek() == Some(b'D')
                && (matches!(
                    input.peek_second(),
                    Some(b'o') | Some(b'O') | Some(b'x') | Some(b'w')
                )))
    }
}

/// The `<bare-function-type>` production.
///
/// ```text
/// <bare-function-type> ::= <signature type>+
///      # types are possible return type, then parameter types
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct BareFunctionType(pub Vec<TypeHandle>);

impl BareFunctionType {
    fn ret(&self) -> &TypeHandle {
        &self.0[0]
    }

    fn args(&self) -> &FunctionArgListAndReturnType {
        FunctionArgListAndReturnType::new(&self.0)
    }
}

impl Parse for BareFunctionType {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(BareFunctionType, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        let (types, tail) = one_or_more::<TypeHandle>(ctx, subs, input)?;
        Ok((BareFunctionType(types), tail))
    }
}

impl<'subs> Demangle<'subs> for BareFunctionType {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        ctx.push_inner(self);

        self.ret().demangle(ctx, scope);

        if ctx.pop_inner_if(self) {
            ctx.ensure_space();
            self.demangle_as_inner(ctx, scope);
        }
    }
}

impl<'subs> DemangleAsInner<'subs> for BareFunctionType {
    fn demangle_as_inner<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        self.args().demangle_as_inner(ctx, scope);
    }
}

/// The `<decltype>` production.
///
/// ```text
/// <decltype> ::= Dt <expression> E
///            ::= DT <expression> E
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Decltype {
    /// A `decltype` of an id-expression or class member access (C++0x).
    IdExpression(Expression),

    /// A `decltype` of an expression (C++0x).
    Expression(Expression),
}

impl Parse for Decltype {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(Decltype, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        let tail = consume(b"D", input)?;

        if let Ok(tail) = consume(b"t", tail) {
            let (expr, tail) = Expression::parse(ctx, subs, tail)?;
            let tail = consume(b"E", tail)?;
            return Ok((Decltype::IdExpression(expr), tail));
        }

        let tail = consume(b"T", tail)?;
        let (expr, tail) = Expression::parse(ctx, subs, tail)?;
        let tail = consume(b"E", tail)?;
        Ok((Decltype::Expression(expr), tail))
    }
}

impl<'subs> Demangle<'subs> for Decltype {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        match *self {
            Decltype::Expression(ref expr) | Decltype::IdExpression(ref expr) => {
                ctx.push("decltype ", Colors::known());
                ctx.push("(", Colors::brackets());
                expr.demangle(ctx, scope);
                ctx.push(")", Colors::brackets());
            }
        }
    }
}

/// The `<class-enum-type>` production.
///
/// ```text
/// <class-enum-type> ::= <name>
///                   ::= Ts <name>
///                   ::= Tu <name>
///                   ::= Te <name>
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum ClassEnumType {
    /// A non-dependent type name, dependent type name, or dependent
    /// typename-specifier.
    Named(Name),

    /// A dependent elaborated type specifier using 'struct' or 'class'.
    ElaboratedStruct(Name),

    /// A dependent elaborated type specifier using 'union'.
    ElaboratedUnion(Name),

    /// A dependent elaborated type specifier using 'enum'.
    ElaboratedEnum(Name),
}

impl Parse for ClassEnumType {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(ClassEnumType, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        if let Ok((name, tail)) = Name::parse(ctx, subs, input) {
            return Ok((ClassEnumType::Named(name), tail));
        }

        let tail = consume(b"T", input)?;

        if let Ok(tail) = consume(b"s", tail) {
            let (name, tail) = Name::parse(ctx, subs, tail)?;
            return Ok((ClassEnumType::ElaboratedStruct(name), tail));
        }

        if let Ok(tail) = consume(b"u", tail) {
            let (name, tail) = Name::parse(ctx, subs, tail)?;
            return Ok((ClassEnumType::ElaboratedUnion(name), tail));
        }

        let tail = consume(b"e", tail)?;
        let (name, tail) = Name::parse(ctx, subs, tail)?;
        Ok((ClassEnumType::ElaboratedEnum(name), tail))
    }
}

impl<'subs> Demangle<'subs> for ClassEnumType {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        match *self {
            ClassEnumType::Named(ref name) => name.demangle(ctx, scope),
            ClassEnumType::ElaboratedStruct(ref name) => {
                ctx.push("class ", Colors::known());
                name.demangle(ctx, scope)
            }
            ClassEnumType::ElaboratedUnion(ref name) => {
                ctx.push("union ", Colors::known());
                name.demangle(ctx, scope)
            }
            ClassEnumType::ElaboratedEnum(ref name) => {
                ctx.push("enum ", Colors::known());
                name.demangle(ctx, scope)
            }
        }
    }
}

impl<'a> GetLeafName<'a> for ClassEnumType {
    fn get_leaf_name(&'a self, subs: &'a SubstitutionTable) -> Option<LeafName<'a>> {
        match *self {
            ClassEnumType::Named(ref name)
            | ClassEnumType::ElaboratedStruct(ref name)
            | ClassEnumType::ElaboratedUnion(ref name)
            | ClassEnumType::ElaboratedEnum(ref name) => name.get_leaf_name(subs),
        }
    }
}

/// The `<unnamed-type-name>` production.
///
/// ```text
/// <unnamed-type-name> ::= Ut [ <nonnegative number> ] _
///                     ::= <closure-type-name>
/// ```
///
/// TODO: parse the <closure-type-name> variant
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct UnnamedTypeName(pub Option<usize>);

impl Parse for UnnamedTypeName {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        _subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(UnnamedTypeName, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        let input = consume(b"Ut", input)?;
        let (number, input) = match parse_number(10, false, input) {
            Ok((number, input)) => (Some(number as _), input),
            Err(_) => (None, input),
        };
        let input = consume(b"_", input)?;
        Ok((UnnamedTypeName(number), input))
    }
}

impl UnnamedTypeName {
    #[inline]
    fn starts_with(byte: u8) -> bool {
        byte == b'U'
    }
}

impl<'subs> Demangle<'subs> for UnnamedTypeName {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        _: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        ctx.push_owned(
            format!("{{unnamed type#{}}}", self.0.map_or(1, |n| n + 1)),
            Colors::comment(),
        );
    }
}

impl<'subs> DemangleAsLeaf<'subs> for UnnamedTypeName {
    fn demangle_as_leaf<'me, 'ctx>(&'me self, ctx: &'ctx mut DemangleContext<'subs>) {
        let text = format!("{{unnamed type#{}}}", self.0.map_or(1, |n| n + 1));

        if let Some(source_name) = ctx.source_name {
            ctx.push_owned(text, Colors::comment());
            ctx.push_owned(source_name.to_string(), Colors::item());
        } else {
            ctx.push_owned(text, Colors::comment());
        }
    }
}

impl<'subs> ArgScope<'subs, 'subs> for UnnamedTypeName {
    fn leaf_name(&'subs self) -> Result<LeafName<'subs>> {
        Ok(LeafName::UnnamedType(self))
    }

    fn get_template_arg(
        &'subs self,
        _: usize,
    ) -> Result<(&'subs TemplateArg, &'subs TemplateArgs)> {
        Err(error::Error::BadTemplateArgReference)
    }

    fn get_function_arg(&'subs self, _: usize) -> Result<&'subs Type> {
        Err(error::Error::BadFunctionArgReference)
    }
}

/// The `<array-type>` production.
///
/// ```text
/// <array-type> ::= A <positive dimension number> _ <element type>
///              ::= A [<dimension expression>] _ <element type>
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum ArrayType {
    /// An array with a number-literal dimension.
    DimensionNumber(usize, TypeHandle),

    /// An array with an expression for its dimension.
    DimensionExpression(Expression, TypeHandle),

    /// An array with no dimension.
    NoDimension(TypeHandle),
}

impl Parse for ArrayType {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(ArrayType, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        let tail = consume(b"A", input)?;

        if let Ok((num, tail)) = parse_number(10, false, tail) {
            debug_assert!(num >= 0);
            let tail = consume(b"_", tail)?;
            let (ty, tail) = TypeHandle::parse(ctx, subs, tail)?;
            return Ok((ArrayType::DimensionNumber(num as _, ty), tail));
        }

        if let Ok((expr, tail)) = Expression::parse(ctx, subs, tail) {
            let tail = consume(b"_", tail)?;
            let (ty, tail) = TypeHandle::parse(ctx, subs, tail)?;
            return Ok((ArrayType::DimensionExpression(expr, ty), tail));
        }

        let tail = consume(b"_", tail)?;
        let (ty, tail) = TypeHandle::parse(ctx, subs, tail)?;
        Ok((ArrayType::NoDimension(ty), tail))
    }
}

impl<'subs> Demangle<'subs> for ArrayType {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        ctx.push_inner(self);

        match *self {
            ArrayType::DimensionNumber(_, ref ty)
            | ArrayType::DimensionExpression(_, ref ty)
            | ArrayType::NoDimension(ref ty) => {
                ty.demangle(ctx, scope);
            }
        }

        if ctx.pop_inner_if(self) {
            self.demangle_as_inner(ctx, scope);
        }
    }
}

impl<'subs> DemangleAsInner<'subs> for ArrayType {
    fn demangle_as_inner<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        // Whether we should add a final space before the dimensions.
        let mut needs_space = true;

        while let Some(inner) = ctx.pop_inner() {
            // We need to add parentheses around array inner types, unless they
            // are also (potentially qualified) arrays themselves, in which case
            // we format them as multi-dimensional arrays.
            let inner_is_array = match inner.downcast_to_type() {
                Some(Type::Qualified(_, ty)) => ctx.subs.get_type(ty).map_or(false, |ty| {
                    DemangleAsInner::downcast_to_array_type(ty).is_some()
                }),
                _ => {
                    if inner.downcast_to_array_type().is_some() {
                        needs_space = false;
                        true
                    } else {
                        false
                    }
                }
            };

            if inner_is_array {
                inner.demangle_as_inner(ctx, scope);
            } else {
                ctx.ensure_space();

                // CvQualifiers should have the parentheses printed after, not before
                if inner.is_qualified() {
                    inner.demangle_as_inner(ctx, scope);
                    ctx.ensure_space();
                    ctx.push("(", Colors::brackets());
                } else {
                    ctx.push("(", Colors::brackets());
                    inner.demangle_as_inner(ctx, scope);
                }

                ctx.demangle_inners(scope);
                ctx.push(")", Colors::brackets());
            }
        }

        if needs_space {
            ctx.ensure_space();
        }

        match *self {
            ArrayType::DimensionNumber(n, _) => {
                ctx.push("[", Colors::brackets());
                ctx.push_owned(n.to_string(), Colors::item());
                ctx.push("]", Colors::brackets());
            }
            ArrayType::DimensionExpression(ref expr, _) => {
                ctx.push("[", Colors::brackets());
                expr.demangle(ctx, scope);
                ctx.push("]", Colors::brackets());
            }
            ArrayType::NoDimension(_) => {
                ctx.push("[]", Colors::brackets());
            }
        }
    }

    fn downcast_to_array_type(&self) -> Option<&ArrayType> {
        Some(self)
    }
}

/// The `<vector-type>` production.
///
/// ```text
/// <vector-type> ::= Dv <number> _ <type>
///               ::= Dv <expression> _ <type>
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum VectorType {
    /// An vector with a number-literal dimension.
    DimensionNumber(usize, TypeHandle),

    /// An vector with an expression for its dimension.
    DimensionExpression(Expression, TypeHandle),
}

impl Parse for VectorType {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(VectorType, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        let tail = consume(b"Dv", input)?;

        if let Ok((num, tail)) = parse_number(10, false, tail) {
            debug_assert!(num >= 0);
            let tail = consume(b"_", tail)?;
            let (ty, tail) = TypeHandle::parse(ctx, subs, tail)?;
            return Ok((VectorType::DimensionNumber(num as _, ty), tail));
        }

        let (expr, tail) = Expression::parse(ctx, subs, tail)?;
        let tail = consume(b"_", tail)?;
        let (ty, tail) = TypeHandle::parse(ctx, subs, tail)?;
        Ok((VectorType::DimensionExpression(expr, ty), tail))
    }
}

impl<'subs> Demangle<'subs> for VectorType {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        ctx.push_inner(self);

        match *self {
            VectorType::DimensionNumber(_, ref ty) | VectorType::DimensionExpression(_, ref ty) => {
                ty.demangle(ctx, scope);
            }
        }

        if ctx.pop_inner_if(self) {
            self.demangle_as_inner(ctx, scope);
        }
    }
}

impl<'subs> DemangleAsInner<'subs> for VectorType {
    fn demangle_as_inner<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        match *self {
            VectorType::DimensionNumber(n, _) => {
                ctx.push("__vector", Colors::root());
                ctx.push("(", Colors::brackets());
                ctx.push_owned(n.to_string(), Colors::item());
                ctx.push(")", Colors::brackets());
            }
            VectorType::DimensionExpression(ref expr, _) => {
                ctx.push("__vector", Colors::root());
                ctx.push("(", Colors::brackets());
                expr.demangle(ctx, scope);
                ctx.push(")", Colors::brackets());
            }
        }
    }
}

/// The `<pointer-to-member-type>` production.
///
/// ```text
/// <pointer-to-member-type> ::= M <class type> <member type>
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct PointerToMemberType(pub TypeHandle, pub TypeHandle);

impl Parse for PointerToMemberType {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(PointerToMemberType, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        let tail = consume(b"M", input)?;
        let (ty1, tail) = TypeHandle::parse(ctx, subs, tail)?;
        let (ty2, tail) = TypeHandle::parse(ctx, subs, tail)?;
        Ok((PointerToMemberType(ty1, ty2), tail))
    }
}

impl<'subs> Demangle<'subs> for PointerToMemberType {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        ctx.push_inner(self);
        self.1.demangle(ctx, scope);
        if ctx.pop_inner_if(self) {
            self.demangle_as_inner(ctx, scope);
        }
    }
}

impl<'subs> DemangleAsInner<'subs> for PointerToMemberType {
    fn demangle_as_inner<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        if ctx.last_char_written != Some('(') {
            ctx.ensure_space();
        }

        self.0.demangle(ctx, scope);
        ctx.push("::*", Colors::delimiter());
    }

    fn downcast_to_pointer_to_member(&self) -> Option<&PointerToMemberType> {
        Some(self)
    }
}

/// The `<template-param>` production.
///
/// ```text
/// <template-param> ::= T_ # first template parameter
///                  ::= T <parameter-2 non-negative number> _
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct TemplateParam(pub usize);

impl Parse for TemplateParam {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        _subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(TemplateParam, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        let input = consume(b"T", input)?;
        let (number, input) = match parse_number(10, false, input) {
            Ok((number, input)) => ((number + 1) as _, input),
            Err(_) => (0, input),
        };
        let input = consume(b"_", input)?;
        Ok((TemplateParam(number), input))
    }
}

impl<'subs> Demangle<'subs> for TemplateParam {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        if ctx.is_lambda_arg {
            // To match libiberty, template references are converted to `auto`.
            ctx.push("auto", Colors::known());
            ctx.push(":", Colors::delimiter());
            ctx.push_owned((self.0 + 1).to_string(), Colors::item());
        } else {
            let arg = self.resolve(scope);
            arg.demangle(ctx, scope)
        }
    }
}

impl TemplateParam {
    fn resolve<'subs>(&'subs self, scope: Option<ArgScopeStack<'_, 'subs>>) -> &'subs TemplateArg {
        static FAILED: TemplateArg = TemplateArg::ArgPack(Vec::new());

        scope
            .get_template_arg(self.0)
            .map_err(|_| fmt::Error)
            .map(|v| v.0)
            .unwrap_or(&FAILED)
    }
}

impl<'a> Hash for &'a TemplateParam {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let self_ref: &TemplateParam = self;
        let self_ptr = self_ref as *const TemplateParam;
        self_ptr.hash(state);
    }
}

/// The `<template-template-param>` production.
///
/// ```text
/// <template-template-param> ::= <template-param>
///                           ::= <substitution>
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct TemplateTemplateParam(pub TemplateParam);

define_handle! {
    /// A reference to a parsed `TemplateTemplateParam`.
    pub(crate) enum TemplateTemplateParamHandle
}

impl Parse for TemplateTemplateParamHandle {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(TemplateTemplateParamHandle, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        if let Ok((sub, tail)) = Substitution::parse(ctx, subs, input) {
            match sub {
                Substitution::WellKnown(component) => {
                    return Ok((TemplateTemplateParamHandle::WellKnown(component), tail));
                }
                Substitution::BackReference(idx) => {
                    // TODO: should this check if the thing at idx is a
                    // template-template-param? There could otherwise be ambiguity
                    // with <type>'s <substitution> form...
                    return Ok((TemplateTemplateParamHandle::BackReference(idx), tail));
                }
            }
        }

        let (param, tail) = TemplateParam::parse(ctx, subs, input)?;
        let ttp = TemplateTemplateParam(param);
        let ttp = Substitutable::TemplateTemplateParam(ttp);
        let idx = subs.insert(ttp);
        let handle = TemplateTemplateParamHandle::BackReference(idx);
        Ok((handle, tail))
    }
}

impl<'subs> Demangle<'subs> for TemplateTemplateParam {
    #[inline]
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        self.0.demangle(ctx, scope)
    }
}

/// The <function-param> production.
///
/// ```text
/// <function-param> ::= fp <top-level CV-qualifiers> _
///                          # L == 0, first parameter
///                  ::= fp <top-level CV-qualifiers> <parameter-2 non-negative number> _
///                          # L == 0, second and later parameters
///                  ::= fL <L-1 non-negative number> p <top-level CV-qualifiers> _
///                          # L > 0, first parameter
///                  ::= fL <L-1 non-negative number> p <top-level CV-qualifiers> <parameter-2 non-negative number> _
///                          # L > 0, second and later parameters
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct FunctionParam(pub usize, pub CvQualifiers, pub Option<usize>);

impl Parse for FunctionParam {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(FunctionParam, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        let tail = consume(b"f", input)?;
        if tail.is_empty() {
            return Err(error::Error::UnexpectedEnd);
        }

        let (scope, tail) = if let Ok(tail) = consume(b"L", tail) {
            parse_number(10, false, tail)?
        } else {
            (0, tail)
        };

        let tail = consume(b"p", tail)?;

        let (qualifiers, tail) = CvQualifiers::parse(ctx, subs, tail)?;

        let (param, tail) = if tail.peek() == Some(b'T') {
            (None, consume(b"T", tail)?)
        } else if let Ok((num, tail)) = parse_number(10, false, tail) {
            (Some(num as usize + 1), consume(b"_", tail)?)
        } else {
            (Some(0), consume(b"_", tail)?)
        };

        Ok((FunctionParam(scope as _, qualifiers, param), tail))
    }
}

impl<'subs> Demangle<'subs> for FunctionParam {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        _: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        match self.2 {
            None => ctx.push("this", Colors::annotation()),
            Some(i) => ctx.push_owned(format!("{{parm#{}}}", i + 1), Colors::comment()),
        }
    }
}

/// The `<template-args>` production.
///
/// ```text
/// <template-args> ::= I <template-arg>+ E
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct TemplateArgs(pub Vec<TemplateArg>);

impl Parse for TemplateArgs {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(TemplateArgs, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        let tail = consume(b"I", input)?;

        let (args, tail) = one_or_more::<TemplateArg>(ctx, subs, tail)?;
        let tail = consume(b"E", tail)?;
        Ok((TemplateArgs(args), tail))
    }
}

impl<'subs> Demangle<'subs> for TemplateArgs {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        mut scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        inner_barrier!(ctx);

        if ctx.last_char_written == Some('<') {
            ctx.push(" ", Colors::spacing());
        }

        ctx.push("<", Colors::annotation());
        let mut need_comma = false;
        for arg_index in 0..self.0.len() {
            if need_comma {
                ctx.push(", ", Colors::delimiter());
            }
            if let Some(ref mut scope) = scope {
                scope.in_arg = Some((arg_index, self));
            }
            self.0[arg_index].demangle(ctx, scope);
            need_comma = true;
        }

        ctx.push(">", Colors::annotation());
    }
}

impl<'subs> ArgScope<'subs, 'subs> for TemplateArgs {
    fn leaf_name(&'subs self) -> Result<LeafName<'subs>> {
        Err(error::Error::BadLeafNameReference)
    }

    fn get_template_arg(
        &'subs self,
        idx: usize,
    ) -> Result<(&'subs TemplateArg, &'subs TemplateArgs)> {
        self.0.get(idx).ok_or(error::Error::BadTemplateArgReference).map(|v| (v, self))
    }

    fn get_function_arg(&'subs self, _: usize) -> Result<&'subs Type> {
        Err(error::Error::BadFunctionArgReference)
    }
}

/// A <template-arg> production.
///
/// ```text
/// <template-arg> ::= <type>                # type or template
///                ::= X <expression> E      # expression
///                ::= <expr-primary>        # simple expressions
///                ::= J <template-arg>* E   # argument pack
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum TemplateArg {
    /// A type or template.
    Type(TypeHandle),

    /// An expression.
    Expression(Expression),

    /// A simple expression.
    SimpleExpression(ExprPrimary),

    /// An argument pack.
    ArgPack(Vec<TemplateArg>),
}

impl Parse for TemplateArg {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(TemplateArg, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        if let Ok(tail) = consume(b"X", input) {
            let (expr, tail) = Expression::parse(ctx, subs, tail)?;
            let tail = consume(b"E", tail)?;
            return Ok((TemplateArg::Expression(expr), tail));
        }

        if let Ok((expr, tail)) = ExprPrimary::parse(ctx, subs, input) {
            return Ok((TemplateArg::SimpleExpression(expr), tail));
        }

        if let Ok((ty, tail)) = TypeHandle::parse(ctx, subs, input) {
            return Ok((TemplateArg::Type(ty), tail));
        }

        let tail = if input.peek() == Some(b'J') {
            consume(b"J", input)?
        } else {
            consume(b"I", input)?
        };

        let (args, tail) = if tail.peek() == Some(b'E') {
            (vec![], tail)
        } else {
            zero_or_more::<TemplateArg>(ctx, subs, tail)?
        };
        let tail = consume(b"E", tail)?;
        Ok((TemplateArg::ArgPack(args), tail))
    }
}

impl<'subs> Demangle<'subs> for TemplateArg {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        match *self {
            TemplateArg::Type(ref ty) => ty.demangle(ctx, scope),
            TemplateArg::Expression(ref expr) => expr.demangle(ctx, scope),
            TemplateArg::SimpleExpression(ref expr) => expr.demangle(ctx, scope),
            TemplateArg::ArgPack(ref args) => {
                ctx.is_template_argument_pack = true;
                let mut need_comma = false;
                for arg in &args[..] {
                    if need_comma {
                        ctx.push(", ", Colors::delimiter());
                    }
                    arg.demangle(ctx, scope);
                    need_comma = true;
                }
            }
        }
    }
}

/// In libiberty, Member and DerefMember expressions have special handling.
/// They parse an `UnqualifiedName` (not an `UnscopedName` as the cxxabi docs
/// say) and optionally a `TemplateArgs` if it is present. We can't just parse
/// a `Name` or an `UnscopedTemplateName` here because that allows other inputs
/// that libiberty does not.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct MemberName(pub Name);

impl Parse for MemberName {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(MemberName, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        let (name, tail) = UnqualifiedName::parse(ctx, subs, input)?;
        let name = UnscopedName::Unqualified(name);
        if let Ok((template, tail)) = TemplateArgs::parse(ctx, subs, tail) {
            let name = UnscopedTemplateName(name);
            // In libiberty, these are unsubstitutable.
            let idx = subs.insert_non_substitution(Substitutable::UnscopedTemplateName(name));
            let handle = UnscopedTemplateNameHandle::NonSubstitution(NonSubstitution(idx));
            Ok((MemberName(Name::UnscopedTemplate(handle, template)), tail))
        } else {
            Ok((MemberName(Name::Unscoped(name)), tail))
        }
    }
}

impl<'subs> Demangle<'subs> for MemberName {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        let needs_parens = self.0.get_template_args(ctx.subs).is_some();
        if needs_parens {
            ctx.push("(", Colors::brackets());
        }

        self.0.demangle(ctx, scope);

        if needs_parens {
            ctx.push(")", Colors::brackets());
        }
    }
}

/// The `<expression>` production.
///
/// ```text
///  <expression> ::= <unary operator-name> <expression>
///               ::= <binary operator-name> <expression> <expression>
///               ::= <ternary operator-name> <expression> <expression> <expression>
///               ::= pp_ <expression>                             # prefix ++
///               ::= mm_ <expression>                             # prefix --
///               ::= cl <expression>+ E                           # expression (expr-list), call
///               ::= cv <type> <expression>                       # type (expression), conversion with one argument
///               ::= cv <type> _ <expression>* E                  # type (expr-list), conversion with other than one argument
///               ::= tl <type> <expression>* E                    # type {expr-list}, conversion with braced-init-list argument
///               ::= il <expression> E                            # {expr-list}, braced-init-list in any other context
///               ::= [gs] nw <expression>* _ <type> E             # new (expr-list) type
///               ::= [gs] nw <expression>* _ <type> <initializer> # new (expr-list) type (init)
///               ::= [gs] na <expression>* _ <type> E             # new[] (expr-list) type
///               ::= [gs] na <expression>* _ <type> <initializer> # new[] (expr-list) type (init)
///               ::= [gs] dl <expression>                         # delete expression
///               ::= [gs] da <expression>                         # delete[] expression
///               ::= dc <type> <expression>                       # dynamic_cast<type> (expression)
///               ::= sc <type> <expression>                       # static_cast<type> (expression)
///               ::= cc <type> <expression>                       # const_cast<type> (expression)
///               ::= rc <type> <expression>                       # reinterpret_cast<type> (expression)
///               ::= ti <type>                                    # typeid (type)
///               ::= te <expression>                              # typeid (expression)
///               ::= st <type>                                    # sizeof (type)
///               ::= sz <expression>                              # sizeof (expression)
///               ::= at <type>                                    # alignof (type)
///               ::= az <expression>                              # alignof (expression)
///               ::= nx <expression>                              # noexcept (expression)
///               ::= so <subobject-expr>
///               ::= <template-param>
///               ::= <function-param>
///               ::= dt <expression> <unresolved-name>            # expr.name
///               ::= pt <expression> <unresolved-name>            # expr->name
///               ::= ds <expression> <expression>                 # expr.*expr
///               ::= sZ <template-param>                          # sizeof...(T), size of a template parameter pack
///               ::= sZ <function-param>                          # sizeof...(parameter), size of a function parameter pack
///               ::= sP <template-arg>* E                         # sizeof...(T), size of a captured template parameter pack from an alias template
///               ::= sp <expression>                              # expression..., pack expansion
///               ::= tw <expression>                              # throw expression
///               ::= tr                                           # throw with no operand (rethrow)
///               ::= <unresolved-name>                            # f(p), N::f(p), ::f(p),
///                                                                # freestanding dependent name (e.g., T::x),
///                                                                # objectless nonstatic member reference
///               ::= <expr-primary>
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Expression {
    /// A unary operator expression.
    Unary(OperatorName, Box<Expression>),

    /// A binary operator expression.
    Binary(OperatorName, Box<Expression>, Box<Expression>),

    /// A ternary operator expression.
    Ternary(
        OperatorName,
        Box<Expression>,
        Box<Expression>,
        Box<Expression>,
    ),

    /// A prefix `++`.
    PrefixInc(Box<Expression>),

    /// A prefix `--`.
    PrefixDec(Box<Expression>),

    /// A call with functor and arguments.
    Call(Box<Expression>, Vec<Expression>),

    /// A type conversion with one argument.
    ConversionOne(TypeHandle, Box<Expression>),

    /// A type conversion with many arguments.
    ConversionMany(TypeHandle, Vec<Expression>),

    /// A type conversion with many arguments.
    ConversionBraced(TypeHandle, Vec<Expression>),

    /// A braced init list expression.
    BracedInitList(Box<Expression>),

    /// The `new` operator.
    New(Vec<Expression>, TypeHandle, Option<Initializer>),

    /// The global `::new` operator.
    GlobalNew(Vec<Expression>, TypeHandle, Option<Initializer>),

    /// The `new[]` operator.
    NewArray(Vec<Expression>, TypeHandle, Option<Initializer>),

    /// The global `::new[]` operator.
    GlobalNewArray(Vec<Expression>, TypeHandle, Option<Initializer>),

    /// The `delete` operator.
    Delete(Box<Expression>),

    /// The global `::delete` operator.
    GlobalDelete(Box<Expression>),

    /// The `delete[]` operator.
    DeleteArray(Box<Expression>),

    /// The global `::delete[]` operator.
    GlobalDeleteArray(Box<Expression>),

    /// `dynamic_cast<type> (expression)`
    DynamicCast(TypeHandle, Box<Expression>),

    /// `static_cast<type> (expression)`
    StaticCast(TypeHandle, Box<Expression>),

    /// `const_cast<type> (expression)`
    ConstCast(TypeHandle, Box<Expression>),

    /// `reinterpret_cast<type> (expression)`
    ReinterpretCast(TypeHandle, Box<Expression>),

    /// `typeid (type)`
    TypeidType(TypeHandle),

    /// `typeid (expression)`
    TypeidExpr(Box<Expression>),

    /// `sizeof (type)`
    SizeofType(TypeHandle),

    /// `sizeof (expression)`
    SizeofExpr(Box<Expression>),

    /// `alignof (type)`
    AlignofType(TypeHandle),

    /// `alignof (expression)`
    AlignofExpr(Box<Expression>),

    /// `noexcept (expression)`
    Noexcept(Box<Expression>),

    /// Subobject expression,
    Subobject(SubobjectExpr),

    /// A named template parameter.
    TemplateParam(TemplateParam),

    /// A function parameter.
    FunctionParam(FunctionParam),

    /// `expr.name`
    Member(Box<Expression>, MemberName),

    /// `expr->name`
    DerefMember(Box<Expression>, MemberName),

    /// `expr.*expr`
    PointerToMember(Box<Expression>, Box<Expression>),

    /// `sizeof...(T)`, size of a template parameter pack.
    SizeofTemplatePack(TemplateParam),

    /// `sizeof...(parameter)`, size of a function parameter pack.
    SizeofFunctionPack(FunctionParam),

    /// `sizeof...(T)`, size of a captured template parameter pack from an alias
    /// template.
    SizeofCapturedTemplatePack(Vec<TemplateArg>),

    /// `expression...`, pack expansion.
    PackExpansion(Box<Expression>),

    /// `throw expression`
    Throw(Box<Expression>),

    /// `throw` with no operand
    Rethrow,

    /// `f(p)`, `N::f(p)`, `::f(p)`, freestanding dependent name (e.g., `T::x`),
    /// objectless nonstatic member reference.
    UnresolvedName(UnresolvedName),

    /// An `<expr-primary>` production.
    Primary(ExprPrimary),
}

impl Parse for Expression {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(Expression, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        if let Ok(tail) = consume(b"pp_", input) {
            let (expr, tail) = Expression::parse(ctx, subs, tail)?;
            let expr = Expression::PrefixInc(Box::new(expr));
            return Ok((expr, tail));
        }

        if let Ok(tail) = consume(b"mm_", input) {
            let (expr, tail) = Expression::parse(ctx, subs, tail)?;
            let expr = Expression::PrefixDec(Box::new(expr));
            return Ok((expr, tail));
        }

        if let Some((head, tail)) = input.try_split_at(2) {
            match head.as_ref() {
                b"cl" => {
                    let (func, tail) = Expression::parse(ctx, subs, tail)?;
                    let (args, tail) = zero_or_more::<Expression>(ctx, subs, tail)?;
                    let tail = consume(b"E", tail)?;
                    let expr = Expression::Call(Box::new(func), args);
                    return Ok((expr, tail));
                }
                b"cv" => {
                    let (ty, tail) = TypeHandle::parse(ctx, subs, tail)?;
                    if let Ok(tail) = consume(b"_", tail) {
                        let (exprs, tail) = zero_or_more::<Expression>(ctx, subs, tail)?;
                        let tail = consume(b"E", tail)?;
                        let expr = Expression::ConversionMany(ty, exprs);
                        return Ok((expr, tail));
                    } else {
                        let (expr, tail) = Expression::parse(ctx, subs, tail)?;
                        let expr = Expression::ConversionOne(ty, Box::new(expr));
                        return Ok((expr, tail));
                    }
                }
                b"tl" => {
                    let (ty, tail) = TypeHandle::parse(ctx, subs, tail)?;
                    let (exprs, tail) = zero_or_more::<Expression>(ctx, subs, tail)?;
                    let expr = Expression::ConversionBraced(ty, exprs);
                    let tail = consume(b"E", tail)?;
                    return Ok((expr, tail));
                }
                b"il" => {
                    let (expr, tail) = Expression::parse(ctx, subs, tail)?;
                    let tail = consume(b"E", tail)?;
                    let expr = Expression::BracedInitList(Box::new(expr));
                    return Ok((expr, tail));
                }
                b"dc" => {
                    let (ty, tail) = TypeHandle::parse(ctx, subs, tail)?;
                    let (expr, tail) = Expression::parse(ctx, subs, tail)?;
                    let expr = Expression::DynamicCast(ty, Box::new(expr));
                    return Ok((expr, tail));
                }
                b"sc" => {
                    let (ty, tail) = TypeHandle::parse(ctx, subs, tail)?;
                    let (expr, tail) = Expression::parse(ctx, subs, tail)?;
                    let expr = Expression::StaticCast(ty, Box::new(expr));
                    return Ok((expr, tail));
                }
                b"cc" => {
                    let (ty, tail) = TypeHandle::parse(ctx, subs, tail)?;
                    let (expr, tail) = Expression::parse(ctx, subs, tail)?;
                    let expr = Expression::ConstCast(ty, Box::new(expr));
                    return Ok((expr, tail));
                }
                b"rc" => {
                    let (ty, tail) = TypeHandle::parse(ctx, subs, tail)?;
                    let (expr, tail) = Expression::parse(ctx, subs, tail)?;
                    let expr = Expression::ReinterpretCast(ty, Box::new(expr));
                    return Ok((expr, tail));
                }
                b"ti" => {
                    let (ty, tail) = TypeHandle::parse(ctx, subs, tail)?;
                    let expr = Expression::TypeidType(ty);
                    return Ok((expr, tail));
                }
                b"te" => {
                    let (expr, tail) = Expression::parse(ctx, subs, tail)?;
                    let expr = Expression::TypeidExpr(Box::new(expr));
                    return Ok((expr, tail));
                }
                b"st" => {
                    let (ty, tail) = TypeHandle::parse(ctx, subs, tail)?;
                    let expr = Expression::SizeofType(ty);
                    return Ok((expr, tail));
                }
                b"sz" => {
                    let (expr, tail) = Expression::parse(ctx, subs, tail)?;
                    let expr = Expression::SizeofExpr(Box::new(expr));
                    return Ok((expr, tail));
                }
                b"at" => {
                    let (ty, tail) = TypeHandle::parse(ctx, subs, tail)?;
                    let expr = Expression::AlignofType(ty);
                    return Ok((expr, tail));
                }
                b"az" => {
                    let (expr, tail) = Expression::parse(ctx, subs, tail)?;
                    let expr = Expression::AlignofExpr(Box::new(expr));
                    return Ok((expr, tail));
                }
                b"nx" => {
                    let (expr, tail) = Expression::parse(ctx, subs, tail)?;
                    let expr = Expression::Noexcept(Box::new(expr));
                    return Ok((expr, tail));
                }
                b"so" => {
                    let (expr, tail) = SubobjectExpr::parse(ctx, subs, tail)?;
                    let expr = Expression::Subobject(expr);
                    return Ok((expr, tail));
                }
                b"dt" => {
                    let (expr, tail) = Expression::parse(ctx, subs, tail)?;
                    let (name, tail) = MemberName::parse(ctx, subs, tail)?;
                    let expr = Expression::Member(Box::new(expr), name);
                    return Ok((expr, tail));
                }
                b"pt" => {
                    let (expr, tail) = Expression::parse(ctx, subs, tail)?;
                    let (name, tail) = MemberName::parse(ctx, subs, tail)?;
                    let expr = Expression::DerefMember(Box::new(expr), name);
                    return Ok((expr, tail));
                }
                b"ds" => {
                    let (first, tail) = Expression::parse(ctx, subs, tail)?;
                    let (second, tail) = Expression::parse(ctx, subs, tail)?;
                    let expr = Expression::PointerToMember(Box::new(first), Box::new(second));
                    return Ok((expr, tail));
                }
                b"sZ" => {
                    if let Ok((param, tail)) = TemplateParam::parse(ctx, subs, tail) {
                        let expr = Expression::SizeofTemplatePack(param);
                        return Ok((expr, tail));
                    }

                    let (param, tail) = FunctionParam::parse(ctx, subs, tail)?;
                    let expr = Expression::SizeofFunctionPack(param);
                    return Ok((expr, tail));
                }
                b"sP" => {
                    let (args, tail) = zero_or_more::<TemplateArg>(ctx, subs, tail)?;
                    let expr = Expression::SizeofCapturedTemplatePack(args);
                    let tail = consume(b"E", tail)?;
                    return Ok((expr, tail));
                }
                b"sp" => {
                    let (expr, tail) = Expression::parse(ctx, subs, tail)?;
                    let expr = Expression::PackExpansion(Box::new(expr));
                    return Ok((expr, tail));
                }
                b"tw" => {
                    let (expr, tail) = Expression::parse(ctx, subs, tail)?;
                    let expr = Expression::Throw(Box::new(expr));
                    return Ok((expr, tail));
                }
                b"tr" => {
                    let expr = Expression::Rethrow;
                    return Ok((expr, tail));
                }
                b"gs" => {
                    if let Ok((expr, tail)) = can_be_global(true, ctx, subs, tail) {
                        return Ok((expr, tail));
                    }
                }
                _ => {}
            }
        }

        if let Ok((expr, tail)) = can_be_global(false, ctx, subs, input) {
            return Ok((expr, tail));
        }

        if let Ok((param, tail)) = TemplateParam::parse(ctx, subs, input) {
            let expr = Expression::TemplateParam(param);
            return Ok((expr, tail));
        }

        if let Ok((param, tail)) = FunctionParam::parse(ctx, subs, input) {
            let expr = Expression::FunctionParam(param);
            return Ok((expr, tail));
        }

        if let Ok((name, tail)) = UnresolvedName::parse(ctx, subs, input) {
            let expr = Expression::UnresolvedName(name);
            return Ok((expr, tail));
        }

        if let Ok((prim, tail)) = ExprPrimary::parse(ctx, subs, input) {
            let expr = Expression::Primary(prim);
            return Ok((expr, tail));
        }

        // "A production for <expression> that directly specifies an operation
        // code (e.g., for the -> operator) takes precedence over one that is
        // expressed in terms of (unary/binary/ternary) <operator-name>." So try
        // and parse unary/binary/ternary expressions last.
        let (expr, tail) = OperatorName::parse_from_expr(ctx, subs, input)?;
        return Ok((expr, tail));

        // Parse the various expressions that can optionally have a leading "gs"
        // to indicate that they are in the global namespace. The input is after
        // we have already detected consumed the optional "gs" and if we did
        // find it, then `is_global` should be true.
        fn can_be_global<'a, 'b>(
            is_global: bool,
            ctx: &'a ParseContext,
            subs: &'a mut SubstitutionTable,
            input: IndexStr<'b>,
        ) -> Result<(Expression, IndexStr<'b>)> {
            match input.try_split_at(2) {
                None => Err(error::Error::UnexpectedEnd),
                Some((head, tail)) => match head.as_ref() {
                    b"nw" => {
                        let (exprs, tail) = zero_or_more::<Expression>(ctx, subs, tail)?;
                        let tail = consume(b"_", tail)?;
                        let (ty, tail) = TypeHandle::parse(ctx, subs, tail)?;
                        if let Ok(tail) = consume(b"E", tail) {
                            let expr = if is_global {
                                Expression::GlobalNew(exprs, ty, None)
                            } else {
                                Expression::New(exprs, ty, None)
                            };
                            Ok((expr, tail))
                        } else {
                            let (init, tail) = Initializer::parse(ctx, subs, tail)?;
                            let expr = if is_global {
                                Expression::GlobalNew(exprs, ty, Some(init))
                            } else {
                                Expression::New(exprs, ty, Some(init))
                            };
                            Ok((expr, tail))
                        }
                    }
                    b"na" => {
                        let (exprs, tail) = zero_or_more::<Expression>(ctx, subs, tail)?;
                        let tail = consume(b"_", tail)?;
                        let (ty, tail) = TypeHandle::parse(ctx, subs, tail)?;
                        if let Ok(tail) = consume(b"E", tail) {
                            let expr = if is_global {
                                Expression::GlobalNewArray(exprs, ty, None)
                            } else {
                                Expression::NewArray(exprs, ty, None)
                            };
                            Ok((expr, tail))
                        } else {
                            let (init, tail) = Initializer::parse(ctx, subs, tail)?;
                            let expr = if is_global {
                                Expression::GlobalNewArray(exprs, ty, Some(init))
                            } else {
                                Expression::NewArray(exprs, ty, Some(init))
                            };
                            Ok((expr, tail))
                        }
                    }
                    b"dl" => {
                        let (expr, tail) = Expression::parse(ctx, subs, tail)?;
                        let expr = if is_global {
                            Expression::GlobalDelete(Box::new(expr))
                        } else {
                            Expression::Delete(Box::new(expr))
                        };
                        Ok((expr, tail))
                    }
                    b"da" => {
                        let (expr, tail) = Expression::parse(ctx, subs, tail)?;
                        let expr = if is_global {
                            Expression::GlobalDeleteArray(Box::new(expr))
                        } else {
                            Expression::DeleteArray(Box::new(expr))
                        };
                        Ok((expr, tail))
                    }
                    _ => Err(error::Error::UnexpectedText),
                },
            }
        }
    }
}

impl<'subs> Demangle<'subs> for Expression {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        match *self {
            Expression::Unary(OperatorName::Simple(ref op), ref expr)
                if *op == SimpleOperatorName::PostInc || *op == SimpleOperatorName::PostDec =>
            {
                expr.demangle_as_subexpr(ctx, scope);
                op.demangle(ctx, scope)
            }
            Expression::Unary(ref op, ref expr) => {
                op.demangle(ctx, scope);
                expr.demangle_as_subexpr(ctx, scope)
            }
            // These need an extra set of parens so that it doesn't close any
            // template argument accidentally.
            Expression::Binary(
                OperatorName::Simple(SimpleOperatorName::Greater),
                ref lhs,
                ref rhs,
            ) => {
                ctx.push("((", Colors::brackets());
                lhs.demangle(ctx, scope);
                ctx.push(")>(", Colors::brackets());
                rhs.demangle(ctx, scope);
                ctx.push("))", Colors::brackets());
            }
            Expression::Binary(ref op, ref lhs, ref rhs) => {
                lhs.demangle_as_subexpr(ctx, scope);
                op.demangle(ctx, scope);
                rhs.demangle_as_subexpr(ctx, scope)
            }
            Expression::Ternary(
                OperatorName::Simple(SimpleOperatorName::Question),
                ref condition,
                ref consequent,
                ref alternative,
            ) => {
                condition.demangle_as_subexpr(ctx, scope);
                ctx.push("", Colors::spacing());
                consequent.demangle_as_subexpr(ctx, scope);
                ctx.push("", Colors::spacing());
                ctx.push(" : ", Colors::delimiter());
                alternative.demangle_as_subexpr(ctx, scope)
            }
            Expression::Ternary(ref op, ref e1, ref e2, ref e3) => {
                // Nonsensical ternary operator Just print it like a function call,
                // I suppose...
                //
                // TODO: should we detect and reject this during parsing
                op.demangle(ctx, scope);
                ctx.push("(", Colors::brackets());
                e1.demangle(ctx, scope);
                ctx.push(", ", Colors::delimiter());
                e2.demangle(ctx, scope);
                ctx.push(", ", Colors::delimiter());
                e3.demangle(ctx, scope);
                ctx.push(")", Colors::brackets());
            }
            Expression::PrefixInc(ref expr) => {
                ctx.push("++", Colors::item());
                expr.demangle(ctx, scope)
            }
            Expression::PrefixDec(ref expr) => {
                ctx.push("--", Colors::item());
                expr.demangle(ctx, scope)
            }
            Expression::Call(ref functor_expr, ref args) => {
                functor_expr.demangle_as_subexpr(ctx, scope);
                ctx.push("(", Colors::brackets());
                let mut need_comma = false;
                for arg in args {
                    if need_comma {
                        ctx.push(", ", Colors::delimiter());
                    }
                    arg.demangle(ctx, scope);
                    need_comma = true;
                }
                ctx.push(")", Colors::brackets());
            }
            Expression::ConversionOne(ref ty, ref expr) => {
                ctx.push("(", Colors::brackets());
                ty.demangle(ctx, scope);
                ctx.push(")(", Colors::brackets());
                expr.demangle(ctx, scope);
                ctx.push(")", Colors::brackets());
            }
            Expression::ConversionMany(ref ty, ref exprs) => {
                ty.demangle(ctx, scope);
                ctx.push("(", Colors::brackets());
                let mut need_comma = false;
                for expr in exprs {
                    if need_comma {
                        ctx.push(", ", Colors::delimiter());
                    }
                    expr.demangle(ctx, scope);
                    need_comma = true;
                }
                ctx.push(")", Colors::brackets());
            }
            Expression::ConversionBraced(ref ty, ref exprs) => {
                ty.demangle(ctx, scope);
                ctx.push("{{", Colors::brackets());
                let mut need_comma = false;
                for expr in exprs {
                    if need_comma {
                        ctx.push(", ", Colors::delimiter());
                    }
                    expr.demangle(ctx, scope);
                    need_comma = true;
                }
                ctx.push("}}", Colors::brackets());
            }
            Expression::BracedInitList(ref expr) => {
                ctx.push("{{", Colors::brackets());
                expr.demangle(ctx, scope);
                ctx.push("}}", Colors::brackets());
            }
            // TODO: factor out all this duplication in the `new` variants.
            Expression::New(ref exprs, ref ty, ref init) => {
                ctx.push("new ", Colors::known());
                ctx.push("(", Colors::brackets());
                let mut need_comma = false;
                for expr in exprs {
                    if need_comma {
                        ctx.push(", ", Colors::delimiter());
                    }
                    expr.demangle(ctx, scope);
                    need_comma = true;
                }
                ctx.push(") ", Colors::brackets());
                ty.demangle(ctx, scope);
                if let Some(ref init) = *init {
                    init.demangle(ctx, scope);
                }
            }
            Expression::GlobalNew(ref exprs, ref ty, ref init) => {
                ctx.push("::", Colors::delimiter());
                ctx.push("new ", Colors::known());
                ctx.push("(", Colors::brackets());
                let mut need_comma = false;
                for expr in exprs {
                    if need_comma {
                        ctx.push(", ", Colors::delimiter());
                    }
                    expr.demangle(ctx, scope);
                    need_comma = true;
                }
                ctx.push(")", Colors::brackets());
                ty.demangle(ctx, scope);
                if let Some(ref init) = *init {
                    init.demangle(ctx, scope);
                }
            }
            Expression::NewArray(ref exprs, ref ty, ref init) => {
                ctx.push("new", Colors::known());
                ctx.push("[]", Colors::brackets());
                ctx.push(" (", Colors::brackets());
                let mut need_comma = false;
                for expr in exprs {
                    if need_comma {
                        ctx.push(", ", Colors::delimiter());
                    }
                    expr.demangle(ctx, scope);
                    need_comma = true;
                }
                ctx.push(") ", Colors::brackets());
                ty.demangle(ctx, scope);
                if let Some(ref init) = *init {
                    init.demangle(ctx, scope);
                }
            }
            Expression::GlobalNewArray(ref exprs, ref ty, ref init) => {
                ctx.push("::", Colors::delimiter());
                ctx.push("new", Colors::known());
                ctx.push("[] (", Colors::brackets());
                let mut need_comma = false;
                for expr in exprs {
                    if need_comma {
                        ctx.push(", ", Colors::delimiter());
                    }
                    expr.demangle(ctx, scope);
                    need_comma = true;
                }
                ctx.push(") ", Colors::brackets());
                ty.demangle(ctx, scope);
                if let Some(ref init) = *init {
                    init.demangle(ctx, scope);
                }
            }
            Expression::Delete(ref expr) => {
                ctx.push("delete ", Colors::known());
                expr.demangle(ctx, scope)
            }
            Expression::GlobalDelete(ref expr) => {
                ctx.push("::", Colors::item());
                ctx.push("delete ", Colors::known());
                expr.demangle(ctx, scope)
            }
            Expression::DeleteArray(ref expr) => {
                ctx.push("delete ", Colors::known());
                ctx.push("[] ", Colors::brackets());
                expr.demangle(ctx, scope)
            }
            Expression::GlobalDeleteArray(ref expr) => {
                ctx.push("::", Colors::delimiter());
                ctx.push("delete ", Colors::known());
                ctx.push("[] ", Colors::brackets());
                expr.demangle(ctx, scope)
            }
            // TODO: factor out duplicated code from cast variants.
            Expression::DynamicCast(ref ty, ref expr) => {
                ctx.push("dynamic_cast", Colors::known());
                ctx.push("<", Colors::annotation());
                ty.demangle(ctx, scope);
                ctx.push(">", Colors::annotation());
                ctx.push("(", Colors::brackets());
                expr.demangle(ctx, scope);
                ctx.push(")", Colors::brackets());
            }
            Expression::StaticCast(ref ty, ref expr) => {
                ctx.push("static_cast", Colors::known());
                ctx.push("<", Colors::annotation());
                ty.demangle(ctx, scope);
                ctx.push(">", Colors::annotation());
                ctx.push("(", Colors::brackets());
                expr.demangle(ctx, scope);
                ctx.push(")", Colors::brackets());
            }
            Expression::ConstCast(ref ty, ref expr) => {
                ctx.push("const_cast", Colors::known());
                ctx.push("<", Colors::annotation());
                ty.demangle(ctx, scope);
                ctx.push(">", Colors::annotation());
                ctx.push("(", Colors::brackets());
                expr.demangle(ctx, scope);
                ctx.push(")", Colors::brackets());
            }
            Expression::ReinterpretCast(ref ty, ref expr) => {
                ctx.push("reinterpret_cast", Colors::known());
                ctx.push("<", Colors::annotation());
                ty.demangle(ctx, scope);
                ctx.push(">", Colors::annotation());
                ctx.push("(", Colors::brackets());
                expr.demangle(ctx, scope);
                ctx.push(")", Colors::brackets());
            }
            Expression::TypeidType(ref ty) => {
                ctx.push("typeid", Colors::known());
                ctx.push(" (", Colors::brackets());
                ty.demangle(ctx, scope);
                ctx.push(")", Colors::brackets());
            }
            Expression::TypeidExpr(ref expr) => {
                ctx.push("typeid", Colors::known());
                ctx.push(" (", Colors::brackets());
                expr.demangle(ctx, scope);
                ctx.push(")", Colors::brackets());
            }
            Expression::SizeofType(ref ty) => {
                ctx.push("sizeof", Colors::known());
                ctx.push(" (", Colors::brackets());
                ty.demangle(ctx, scope);
                ctx.push(")", Colors::brackets());
            }
            Expression::SizeofExpr(ref expr) => {
                ctx.push("sizeof", Colors::known());
                ctx.push(" (", Colors::brackets());
                expr.demangle(ctx, scope);
                ctx.push(")", Colors::brackets());
            }
            Expression::AlignofType(ref ty) => {
                ctx.push("alignof", Colors::known());
                ctx.push(" (", Colors::brackets());
                ty.demangle(ctx, scope);
                ctx.push(")", Colors::brackets());
            }
            Expression::AlignofExpr(ref expr) => {
                ctx.push("alignof", Colors::known());
                ctx.push(" (", Colors::brackets());
                expr.demangle(ctx, scope);
                ctx.push(")", Colors::brackets());
            }
            Expression::Noexcept(ref expr) => {
                ctx.push("noexcept", Colors::known());
                ctx.push(" (", Colors::brackets());
                expr.demangle(ctx, scope);
                ctx.push(")", Colors::brackets());
            }
            Expression::Subobject(ref expr) => expr.demangle(ctx, scope),
            Expression::TemplateParam(ref param) => param.demangle(ctx, scope),
            Expression::FunctionParam(ref param) => param.demangle(ctx, scope),
            Expression::Member(ref expr, ref name) => {
                expr.demangle_as_subexpr(ctx, scope);
                ctx.push(".", Colors::delimiter());
                name.demangle(ctx, scope)
            }
            Expression::DerefMember(ref expr, ref name) => {
                expr.demangle(ctx, scope);
                ctx.push("->", Colors::brackets());
                name.demangle(ctx, scope)
            }
            Expression::PointerToMember(ref e1, ref e2) => {
                e1.demangle(ctx, scope);
                ctx.push(".", Colors::delimiter());
                ctx.push("*", Colors::brackets());
                e2.demangle(ctx, scope)
            }
            Expression::SizeofTemplatePack(ref param) => {
                ctx.push("sizeof", Colors::known());
                ctx.push("...", Colors::item());
                ctx.push("(", Colors::brackets());
                param.demangle(ctx, scope);
                ctx.push(")", Colors::brackets());
            }
            Expression::SizeofFunctionPack(ref param) => {
                ctx.push("sizeof", Colors::known());
                ctx.push("...", Colors::item());
                ctx.push("(", Colors::brackets());
                param.demangle(ctx, scope);
                ctx.push(")", Colors::brackets());
            }
            Expression::SizeofCapturedTemplatePack(ref args) => {
                ctx.push("sizeof", Colors::known());
                ctx.push("...", Colors::item());
                ctx.push("(", Colors::brackets());
                let mut need_comma = false;
                for arg in args {
                    if need_comma {
                        ctx.push(", ", Colors::delimiter());
                    }
                    arg.demangle(ctx, scope);
                    need_comma = true;
                }
                ctx.push(")", Colors::brackets());
            }
            Expression::PackExpansion(ref pack) => {
                pack.demangle_as_subexpr(ctx, scope);
                ctx.push("...", Colors::item());
            }
            Expression::Throw(ref expr) => {
                ctx.push("throw ", Colors::known());
                expr.demangle(ctx, scope)
            }
            Expression::Rethrow => {
                ctx.push("throw", Colors::known());
            }
            Expression::UnresolvedName(ref name) => name.demangle(ctx, scope),
            Expression::Primary(ref expr) => expr.demangle(ctx, scope),
        }
    }
}

impl Expression {
    fn demangle_as_subexpr<'subs>(
        &'subs self,
        ctx: &mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'_, 'subs>>,
    ) {
        let needs_parens = !matches!(
            self,
            Expression::FunctionParam(_) | Expression::Primary(ExprPrimary::External(_))
        );

        if needs_parens {
            ctx.push("(", Colors::brackets());
        }

        self.demangle(ctx, scope);

        if needs_parens {
            ctx.push(")", Colors::brackets());
        }
    }
}

/// The `<unresolved-name>` production.
///
/// ```text
/// <unresolved-name> ::= [gs] <base-unresolved-name>
///                          #
///                   ::= sr <unresolved-type> <base-unresolved-name>
///                          #
///                   ::= srN <unresolved-type> <unresolved-qualifier-level>+ E <base-unresolved-name>
///                          #
///                   ::= [gs] sr <unresolved-qualifier-level>+ E <base-unresolved-name>
///                          # A::x, N::y, A<T>::z; "gs" means leading "::"
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum UnresolvedName {
    /// `x`
    Name(BaseUnresolvedName),

    /// `::x`
    Global(BaseUnresolvedName),

    /// `T::x`  or `decltype(p)::x` or `T::N::x` or `decltype(p)::N::x`
    Nested1(
        UnresolvedTypeHandle,
        Vec<UnresolvedQualifierLevel>,
        BaseUnresolvedName,
    ),

    /// `A::x` or `N::y` or `A<T>::z`
    Nested2(Vec<UnresolvedQualifierLevel>, BaseUnresolvedName),

    /// `::A::x` or `::N::y` or `::A<T>::z`
    GlobalNested2(Vec<UnresolvedQualifierLevel>, BaseUnresolvedName),
}

impl Parse for UnresolvedName {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(UnresolvedName, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        if let Ok(tail) = consume(b"gs", input) {
            if let Ok((name, tail)) = BaseUnresolvedName::parse(ctx, subs, tail) {
                return Ok((UnresolvedName::Global(name), tail));
            }

            let tail = consume(b"sr", tail)?;
            let (levels, tail) = one_or_more::<UnresolvedQualifierLevel>(ctx, subs, tail)?;
            let tail = consume(b"E", tail)?;
            let (name, tail) = BaseUnresolvedName::parse(ctx, subs, tail)?;
            return Ok((UnresolvedName::GlobalNested2(levels, name), tail));
        }

        if let Ok((name, tail)) = BaseUnresolvedName::parse(ctx, subs, input) {
            return Ok((UnresolvedName::Name(name), tail));
        }

        let tail = consume(b"sr", input)?;

        if tail.peek() == Some(b'N') {
            let tail = consume(b"N", tail).unwrap();
            let (ty, tail) = UnresolvedTypeHandle::parse(ctx, subs, tail)?;
            let (levels, tail) = one_or_more::<UnresolvedQualifierLevel>(ctx, subs, tail)?;
            let tail = consume(b"E", tail)?;
            let (name, tail) = BaseUnresolvedName::parse(ctx, subs, tail)?;
            return Ok((UnresolvedName::Nested1(ty, levels, name), tail));
        }

        if let Ok((ty, tail)) = UnresolvedTypeHandle::parse(ctx, subs, tail) {
            let (name, tail) = BaseUnresolvedName::parse(ctx, subs, tail)?;
            return Ok((UnresolvedName::Nested1(ty, vec![], name), tail));
        }

        let (levels, tail) = one_or_more::<UnresolvedQualifierLevel>(ctx, subs, tail)?;
        let tail = consume(b"E", tail)?;
        let (name, tail) = BaseUnresolvedName::parse(ctx, subs, tail)?;
        Ok((UnresolvedName::Nested2(levels, name), tail))
    }
}

impl<'subs> Demangle<'subs> for UnresolvedName {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        match *self {
            UnresolvedName::Name(ref name) => name.demangle(ctx, scope),
            UnresolvedName::Global(ref name) => {
                ctx.push("::", Colors::delimiter());
                name.demangle(ctx, scope)
            }
            UnresolvedName::Nested1(ref ty, ref levels, ref name) => {
                ty.demangle(ctx, scope);
                ctx.push("::", Colors::delimiter());
                for lvl in &levels[..] {
                    lvl.demangle(ctx, scope);
                    ctx.push("::", Colors::delimiter());
                }
                name.demangle(ctx, scope)
            }
            UnresolvedName::Nested2(ref levels, ref name) => {
                for lvl in &levels[..] {
                    lvl.demangle(ctx, scope);
                    ctx.push("::", Colors::delimiter());
                }
                name.demangle(ctx, scope)
            }
            // `::A::x` or `::N::y` or `::A<T>::z`
            UnresolvedName::GlobalNested2(ref levels, ref name) => {
                ctx.push("::", Colors::delimiter());
                for lvl in &levels[..] {
                    lvl.demangle(ctx, scope);
                    ctx.push("::", Colors::delimiter());
                }
                name.demangle(ctx, scope)
            }
        }
    }
}

/// The `<unresolved-type>` production.
///
/// ```text
/// <unresolved-type> ::= <template-param> [ <template-args> ]  # T:: or T<X,Y>::
///                   ::= <decltype>                            # decltype(p)::
///                   ::= <substitution>
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum UnresolvedType {
    /// An unresolved template type.
    Template(TemplateParam, Option<TemplateArgs>),

    /// An unresolved `decltype`.
    Decltype(Decltype),
}

define_handle! {
    /// A reference to a parsed `<unresolved-type>` production.
    pub(crate) enum UnresolvedTypeHandle
}

impl Parse for UnresolvedTypeHandle {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(UnresolvedTypeHandle, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        if let Ok((param, tail)) = TemplateParam::parse(ctx, subs, input) {
            let (args, tail) = if let Ok((args, tail)) = TemplateArgs::parse(ctx, subs, tail) {
                (Some(args), tail)
            } else {
                (None, tail)
            };
            let ty = UnresolvedType::Template(param, args);
            let ty = Substitutable::UnresolvedType(ty);
            let idx = subs.insert(ty);
            let handle = UnresolvedTypeHandle::BackReference(idx);
            return Ok((handle, tail));
        }

        if let Ok((decltype, tail)) = Decltype::parse(ctx, subs, input) {
            let ty = UnresolvedType::Decltype(decltype);
            let ty = Substitutable::UnresolvedType(ty);
            let idx = subs.insert(ty);
            let handle = UnresolvedTypeHandle::BackReference(idx);
            return Ok((handle, tail));
        }

        let (sub, tail) = Substitution::parse(ctx, subs, input)?;
        match sub {
            Substitution::WellKnown(component) => {
                Ok((UnresolvedTypeHandle::WellKnown(component), tail))
            }
            Substitution::BackReference(idx) => {
                // TODO: should this check that the back reference actually
                // points to an `<unresolved-type>`?
                Ok((UnresolvedTypeHandle::BackReference(idx), tail))
            }
        }
    }
}

impl<'subs> Demangle<'subs> for UnresolvedType {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        match *self {
            UnresolvedType::Decltype(ref dt) => dt.demangle(ctx, scope),
            UnresolvedType::Template(ref param, ref args) => {
                if let Some(ref args) = *args {
                    let scope = scope.push(args);
                    param.demangle(ctx, scope);
                    args.demangle(ctx, scope);
                } else {
                    param.demangle(ctx, scope);
                }
            }
        }
    }
}

/// The `<unresolved-qualifier-level>` production.
///
/// ```text
/// <unresolved-qualifier-level> ::= <simple-id>
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct UnresolvedQualifierLevel(pub SimpleId);

impl Parse for UnresolvedQualifierLevel {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(UnresolvedQualifierLevel, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        let (id, tail) = SimpleId::parse(ctx, subs, input)?;
        Ok((UnresolvedQualifierLevel(id), tail))
    }
}

impl<'subs> Demangle<'subs> for UnresolvedQualifierLevel {
    #[inline]
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        self.0.demangle(ctx, scope)
    }
}

/// The `<simple-id>` production.
///
/// ```text
/// <simple-id> ::= <source-name> [ <template-args> ]
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct SimpleId(pub SourceName, pub Option<TemplateArgs>);

impl Parse for SimpleId {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(SimpleId, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        let (name, tail) = SourceName::parse(ctx, subs, input)?;
        let (args, tail) = if let Ok((args, tail)) = TemplateArgs::parse(ctx, subs, tail) {
            (Some(args), tail)
        } else {
            (None, tail)
        };
        Ok((SimpleId(name, args), tail))
    }
}

impl<'subs> Demangle<'subs> for SimpleId {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        self.0.demangle(ctx, scope);
        if let Some(ref args) = self.1 {
            args.demangle(ctx, scope);
        }
    }
}

/// The `<base-unresolved-name>` production.
///
/// ```text
/// <base-unresolved-name> ::= <simple-id>                        # unresolved name
///                        ::= on <operator-name>                 # unresolved operator-function-id
///                        ::= on <operator-name> <template-args> # unresolved operator template-id
///                        ::= dn <destructor-name>               # destructor or pseudo-destructor;
///                                                               # e.g. ~X or ~X<N-1>
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum BaseUnresolvedName {
    /// An unresolved name.
    Name(SimpleId),

    /// An unresolved function or template function name.
    Operator(OperatorName, Option<TemplateArgs>),

    /// An unresolved destructor name.
    Destructor(DestructorName),
}

impl Parse for BaseUnresolvedName {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(BaseUnresolvedName, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        if let Ok((name, tail)) = SimpleId::parse(ctx, subs, input) {
            return Ok((BaseUnresolvedName::Name(name), tail));
        }

        if let Ok(tail) = consume(b"on", input) {
            let (opname, tail) = OperatorName::parse(ctx, subs, tail)?;
            let (args, tail) = if let Ok((args, tail)) = TemplateArgs::parse(ctx, subs, tail) {
                (Some(args), tail)
            } else {
                (None, tail)
            };
            return Ok((BaseUnresolvedName::Operator(opname, args), tail));
        }

        let tail = consume(b"dn", input)?;
        let (name, tail) = DestructorName::parse(ctx, subs, tail)?;
        Ok((BaseUnresolvedName::Destructor(name), tail))
    }
}

impl<'subs> Demangle<'subs> for BaseUnresolvedName {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        match *self {
            BaseUnresolvedName::Name(ref name) => name.demangle(ctx, scope),
            BaseUnresolvedName::Destructor(ref dtor) => dtor.demangle(ctx, scope),
            BaseUnresolvedName::Operator(ref op, ref args) => {
                op.demangle(ctx, scope);
                if let Some(ref args) = *args {
                    args.demangle(ctx, scope);
                }
            }
        }
    }
}

/// The `<destructor-name>` production.
///
/// ```text
/// <destructor-name> ::= <unresolved-type> # e.g., ~T or ~decltype(f())
///                   ::= <simple-id>       # e.g., ~A<2*N>
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum DestructorName {
    /// A destructor for an unresolved type.
    Unresolved(UnresolvedTypeHandle),

    /// A destructor for a resolved type name.
    Name(SimpleId),
}

impl Parse for DestructorName {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(DestructorName, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        if let Ok((ty, tail)) = UnresolvedTypeHandle::parse(ctx, subs, input) {
            return Ok((DestructorName::Unresolved(ty), tail));
        }

        let (name, tail) = SimpleId::parse(ctx, subs, input)?;
        Ok((DestructorName::Name(name), tail))
    }
}

impl<'subs> Demangle<'subs> for DestructorName {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        ctx.push("~", Colors::item());
        match *self {
            DestructorName::Unresolved(ref ty) => ty.demangle(ctx, scope),
            DestructorName::Name(ref name) => name.demangle(ctx, scope),
        }
    }
}

/// The `<expr-primary>` production.
///
/// ```text
/// <expr-primary> ::= L <type> <value number> E                        # integer literal
///                ::= L <type> <value float> E                         # floating literal
///                ::= L <string type> E                                # string literal
///                ::= L <nullptr type> E                               # nullptr literal (i.e., "LDnE")
///                ::= L <pointer type> 0 E                             # null pointer template argument
///                ::= L <type> <real-part float> _ <imag-part float> E # complex floating point literal (C 2000)
///                ::= L <mangled-name> E                               # external name
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum ExprPrimary {
    /// A type literal.
    Literal(TypeHandle, usize, usize),

    /// An external name.
    External(MangledName),
}

impl Parse for ExprPrimary {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(ExprPrimary, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        let tail = consume(b"L", input)?;

        if let Ok((ty, tail)) = TypeHandle::parse(ctx, subs, tail) {
            let start = tail.index();
            let num_bytes_in_literal = tail.as_ref().iter().take_while(|&&c| c != b'E').count();
            let tail = tail.range_from(num_bytes_in_literal..);
            let end = tail.index();
            let tail = consume(b"E", tail)?;
            let expr = ExprPrimary::Literal(ty, start, end);
            return Ok((expr, tail));
        }

        let (name, tail) = MangledName::parse(ctx, subs, tail)?;
        let tail = consume(b"E", tail)?;
        let expr = ExprPrimary::External(name);
        Ok((expr, tail))
    }
}

impl<'subs> Demangle<'subs> for ExprPrimary {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        fn write_literal(ctx: &mut DemangleContext, start: usize, end: usize) {
            debug_assert!(start <= end);
            let start = if start < end && ctx.input[start] == b'n' {
                ctx.push("-", Colors::item());
                start + 1
            } else {
                start
            };
            ctx.push("-", Colors::item());
            ctx.push(&ctx.stream.inner()[start..end], Colors::item());
        }

        match *self {
            ExprPrimary::External(ref name) => name.demangle(ctx, scope),
            ExprPrimary::Literal(
                TypeHandle::Builtin(BuiltinType::Standard(StandardBuiltinType::Bool)),
                start,
                end,
            ) => match &ctx.input[start..end] {
                b"0" => ctx.push("false", Colors::known()),
                b"1" => ctx.push("true", Colors::known()),
                _ => {
                    ctx.push("(", Colors::brackets());
                    ctx.push("bool", Colors::known());
                    ctx.push(")", Colors::brackets());
                    write_literal(ctx, start, end)
                }
            },
            ExprPrimary::Literal(
                TypeHandle::Builtin(BuiltinType::Standard(StandardBuiltinType::Nullptr)),
                _,
                _,
            ) => ctx.push("nullptr", Colors::known()),
            ExprPrimary::Literal(
                ref ty @ TypeHandle::Builtin(BuiltinType::Standard(StandardBuiltinType::Double)),
                start,
                end,
            )
            | ExprPrimary::Literal(
                ref ty @ TypeHandle::Builtin(BuiltinType::Standard(StandardBuiltinType::Float)),
                start,
                end,
            ) => {
                if ctx.show_expression_literal_types {
                    ctx.push("(", Colors::brackets());
                    ty.demangle(ctx, scope);
                    ctx.push(")", Colors::brackets());
                }
                let start = if start < end && ctx.input[start] == b'n' {
                    ctx.push("-", Colors::item());
                    ctx.push("-[", Colors::brackets());
                    start + 1
                } else {
                    ctx.push("[", Colors::brackets());
                    start
                };

                ctx.push(&ctx.stream.inner()[start..end], Colors::item());
                ctx.push("]", Colors::brackets());
            }
            ExprPrimary::Literal(
                TypeHandle::Builtin(BuiltinType::Standard(StandardBuiltinType::Int)),
                start,
                end,
            ) => write_literal(ctx, start, end),
            ExprPrimary::Literal(ref ty, start, end) => {
                if ctx.show_expression_literal_types {
                    ctx.push("(", Colors::brackets());
                    ty.demangle(ctx, scope);
                    ctx.push(")", Colors::brackets());
                }
                write_literal(ctx, start, end)
            }
        }
    }
}

/// The `<initializer>` production.
///
/// ```text
/// <initializer> ::= pi <expression>* E # parenthesized initialization
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct Initializer(pub Vec<Expression>);

impl Parse for Initializer {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(Initializer, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        let tail = consume(b"pi", input)?;
        let (exprs, tail) = zero_or_more::<Expression>(ctx, subs, tail)?;
        let tail = consume(b"E", tail)?;
        Ok((Initializer(exprs), tail))
    }
}

impl<'subs> Demangle<'subs> for Initializer {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        ctx.push("(", Colors::brackets());
        let mut need_comma = false;
        for expr in &self.0 {
            if need_comma {
                ctx.push(", ", Colors::delimiter());
            }
            expr.demangle(ctx, scope);
            need_comma = true;
        }
        ctx.push(")", Colors::brackets());
    }
}

/// The `<local-name>` production.
///
/// ```text
/// <local-name> := Z <function encoding> E <entity name> [<discriminator>]
///              := Z <function encoding> E s [<discriminator>]
///              := Z <function encoding> Ed [ <parameter number> ] _ <entity name>
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum LocalName {
    /// The mangling of the enclosing function, the mangling of the entity
    /// relative to the function, and an optional discriminator.
    Relative(Box<Encoding>, Option<Box<Name>>, Option<Discriminator>),

    /// A default argument in a class definition.
    Default(Box<Encoding>, Option<usize>, Box<Name>),
}

impl Parse for LocalName {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(LocalName, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        let tail = consume(b"Z", input)?;
        let (encoding, tail) = Encoding::parse(ctx, subs, tail)?;
        let tail = consume(b"E", tail)?;

        if let Ok(tail) = consume(b"s", tail) {
            let (disc, tail) = if let Ok((disc, tail)) = Discriminator::parse(ctx, subs, tail) {
                (Some(disc), tail)
            } else {
                (None, tail)
            };
            return Ok((LocalName::Relative(Box::new(encoding), None, disc), tail));
        }

        if let Ok(tail) = consume(b"d", tail) {
            let (param, tail) = if let Ok((num, tail)) = Number::parse(ctx, subs, tail) {
                (Some(num as _), tail)
            } else {
                (None, tail)
            };
            let tail = consume(b"_", tail)?;
            let (name, tail) = Name::parse(ctx, subs, tail)?;
            return Ok((
                LocalName::Default(Box::new(encoding), param, Box::new(name)),
                tail,
            ));
        }

        let (name, tail) = Name::parse(ctx, subs, tail)?;
        let (disc, tail) = if let Ok((disc, tail)) = Discriminator::parse(ctx, subs, tail) {
            (Some(disc), tail)
        } else {
            (None, tail)
        };

        Ok((
            LocalName::Relative(Box::new(encoding), Some(Box::new(name)), disc),
            tail,
        ))
    }
}

impl<'subs> Demangle<'subs> for LocalName {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        match *self {
            LocalName::Relative(ref encoding, Some(ref name), _) => {
                encoding.demangle(ctx, scope);
                ctx.push("::", Colors::delimiter());
                name.demangle(ctx, scope)
            }
            LocalName::Relative(ref encoding, None, _) => {
                // No name means that this is the symbol for a string literal.
                encoding.demangle(ctx, scope);
                ctx.push("::", Colors::delimiter());
                ctx.push("string literal", Colors::known());
            }
            LocalName::Default(ref encoding, _, _) => encoding.demangle(ctx, scope),
        }
    }
}

impl GetTemplateArgs for LocalName {
    fn get_template_args<'a>(&'a self, subs: &'a SubstitutionTable) -> Option<&'a TemplateArgs> {
        match *self {
            LocalName::Relative(_, None, _) => None,
            LocalName::Relative(_, Some(ref name), _) | LocalName::Default(_, _, ref name) => {
                name.get_template_args(subs)
            }
        }
    }
}

impl<'a> GetLeafName<'a> for LocalName {
    fn get_leaf_name(&'a self, subs: &'a SubstitutionTable) -> Option<LeafName<'a>> {
        match *self {
            LocalName::Relative(_, None, _) => None,
            LocalName::Relative(_, Some(ref name), _) | LocalName::Default(_, _, ref name) => {
                name.get_leaf_name(subs)
            }
        }
    }
}

/// The `<discriminator>` production.
///
/// ```text
/// <discriminator> := _ <non-negative number>      # when number < 10
///                 := __ <non-negative number> _   # when number >= 10
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct Discriminator(pub usize);

impl Parse for Discriminator {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        _subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(Discriminator, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        let tail = consume(b"_", input)?;

        if let Ok(tail) = consume(b"_", tail) {
            let (num, tail) = parse_number(10, false, tail)?;
            debug_assert!(num >= 0);
            if num < 10 {
                return Err(error::Error::UnexpectedText);
            }
            let tail = consume(b"_", tail)?;
            return Ok((Discriminator(num as _), tail));
        }

        match tail.try_split_at(1) {
            None => Err(error::Error::UnexpectedEnd),
            Some((head, tail)) => match head.as_ref()[0] {
                b'0' => Ok((Discriminator(0), tail)),
                b'1' => Ok((Discriminator(1), tail)),
                b'2' => Ok((Discriminator(2), tail)),
                b'3' => Ok((Discriminator(3), tail)),
                b'4' => Ok((Discriminator(4), tail)),
                b'5' => Ok((Discriminator(5), tail)),
                b'6' => Ok((Discriminator(6), tail)),
                b'7' => Ok((Discriminator(7), tail)),
                b'8' => Ok((Discriminator(8), tail)),
                b'9' => Ok((Discriminator(9), tail)),
                _ => Err(error::Error::UnexpectedText),
            },
        }
    }
}

/// The `<closure-type-name>` production.
///
/// ```text
/// <closure-type-name> ::= Ul <lambda-sig> E [ <nonnegative number> ] _
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct ClosureTypeName(pub LambdaSig, pub Option<usize>);

impl Parse for ClosureTypeName {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(ClosureTypeName, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        let tail = consume(b"Ul", input)?;
        let (sig, tail) = LambdaSig::parse(ctx, subs, tail)?;
        let tail = consume(b"E", tail)?;
        let (num, tail) = if let Ok((num, tail)) = parse_number(10, false, tail) {
            (Some(num as _), tail)
        } else {
            (None, tail)
        };
        let tail = consume(b"_", tail)?;
        Ok((ClosureTypeName(sig, num), tail))
    }
}

impl<'subs> Demangle<'subs> for ClosureTypeName {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        ctx.push("{{", Colors::brackets());
        ctx.push("lambda", Colors::known());
        ctx.push("(", Colors::brackets());
        self.0.demangle(ctx, scope);

        ctx.push(")#", Colors::brackets());
        ctx.push_owned(self.1.map_or(1, |n| n + 2).to_string(), Colors::item());
        ctx.push("}}", Colors::brackets());
    }
}

impl<'subs> ArgScope<'subs, 'subs> for ClosureTypeName {
    fn leaf_name(&'subs self) -> Result<LeafName<'subs>> {
        Ok(LeafName::Closure(self))
    }

    fn get_template_arg(
        &'subs self,
        _: usize,
    ) -> Result<(&'subs TemplateArg, &'subs TemplateArgs)> {
        Err(error::Error::BadTemplateArgReference)
    }

    fn get_function_arg(&'subs self, _: usize) -> Result<&'subs Type> {
        Err(error::Error::BadFunctionArgReference)
    }
}

impl<'a> GetLeafName<'a> for ClosureTypeName {
    #[inline]
    fn get_leaf_name(&'a self, _: &'a SubstitutionTable) -> Option<LeafName<'a>> {
        Some(LeafName::Closure(self))
    }
}

impl ClosureTypeName {
    #[inline]
    fn starts_with(byte: u8, input: &IndexStr) -> bool {
        byte == b'U' && input.peek_second().map(|b| b == b'l').unwrap_or(false)
    }
}

/// The `<lambda-sig>` production.
///
/// ```text
/// <lambda-sig> ::= <parameter type>+  # Parameter types or "v" if the lambda has no parameters
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct LambdaSig(pub Vec<TypeHandle>);

impl LambdaSig {
    fn demangle_args<'subs>(
        &'subs self,
        ctx: &mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'_, 'subs>>,
    ) {
        let mut need_comma = false;
        for ty in &self.0 {
            if need_comma {
                ctx.push(", ", Colors::delimiter());
            }
            ty.demangle(ctx, scope);
            need_comma = true;
        }
    }
}

impl Parse for LambdaSig {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(LambdaSig, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        let (types, tail) = if let Ok(tail) = consume(b"v", input) {
            (vec![], tail)
        } else {
            one_or_more::<TypeHandle>(ctx, subs, input)?
        };
        Ok((LambdaSig(types), tail))
    }
}

impl<'subs> Demangle<'subs> for LambdaSig {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        ctx.is_lambda_arg = true;
        self.demangle_args(ctx, scope);
        ctx.is_lambda_arg = false;
    }
}

/// The `<data-member-prefix>` production.
///
/// ```text
/// <data-member-prefix> := <member source-name> M
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct DataMemberPrefix(pub SourceName);

impl Parse for DataMemberPrefix {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(DataMemberPrefix, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        let (name, tail) = SourceName::parse(ctx, subs, input)?;
        let tail = consume(b"M", tail)?;
        Ok((DataMemberPrefix(name), tail))
    }
}

impl<'a> GetLeafName<'a> for DataMemberPrefix {
    #[inline]
    fn get_leaf_name(&'a self, _: &'a SubstitutionTable) -> Option<LeafName<'a>> {
        Some(LeafName::SourceName(&self.0))
    }
}

impl DataMemberPrefix {
    fn starts_with(byte: u8) -> bool {
        SourceName::starts_with(byte)
    }
}

impl<'subs> Demangle<'subs> for DataMemberPrefix {
    #[inline]
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        self.0.demangle(ctx, scope)
    }
}

/// The `<substitution>` form: a back-reference to some component we've already
/// parsed.
///
/// ```text
/// <substitution> ::= S <seq-id> _
///                ::= S_
///                ::= St # ::std::
///                ::= Sa # ::std::allocator
///                ::= Sb # ::std::basic_string
///                ::= Ss # ::std::basic_string < char,
///                                               ::std::char_traits<char>,
///                                               ::std::allocator<char> >
///                ::= Si # ::std::basic_istream<char,  std::char_traits<char> >
///                ::= So # ::std::basic_ostream<char,  std::char_traits<char> >
///                ::= Sd # ::std::basic_iostream<char, std::char_traits<char> >
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Substitution {
    /// A reference to an entity that already occurred, ie the `S_` and `S
    /// <seq-id> _` forms.
    BackReference(usize),

    /// A well-known substitution component. These are the components that do
    /// not appear in the substitution table, but have abbreviations specified
    /// directly in the grammar.
    WellKnown(WellKnownComponent),
}

impl Parse for Substitution {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(Substitution, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        if let Ok((well_known, tail)) = WellKnownComponent::parse(ctx, subs, input) {
            return Ok((Substitution::WellKnown(well_known), tail));
        }

        let tail = consume(b"S", input)?;
        let (idx, tail) = if let Ok((idx, tail)) = SeqId::parse(ctx, subs, tail) {
            (idx.0 + 1, tail)
        } else {
            (0, tail)
        };

        if !subs.contains(idx) {
            return Err(error::Error::BadBackReference);
        }

        let tail = consume(b"_", tail)?;
        Ok((Substitution::BackReference(idx), tail))
    }
}

define_vocabulary! {
/// The `<substitution>` variants that are encoded directly in the grammar,
/// rather than as back references to other components in the substitution
/// table.
    #[derive(Clone, Debug, PartialEq, Eq)]
    pub(crate) enum WellKnownComponent {
        Std          (b"St", "std"),
        StdAllocator (b"Sa", "std::allocator"),
        StdString1   (b"Sb", "std::basic_string"),
        StdString2   (b"Ss", "std::string"),
        StdIstream   (b"Si", "std::basic_istream<char, std::char_traits<char> >"),
        StdOstream   (b"So", "std::ostream"),
        StdIostream  (b"Sd", "std::basic_iostream<char, std::char_traits<char> >")
    }
}

impl<'a> GetLeafName<'a> for WellKnownComponent {
    fn get_leaf_name(&'a self, _: &'a SubstitutionTable) -> Option<LeafName<'a>> {
        match *self {
            WellKnownComponent::Std => None,
            _ => Some(LeafName::WellKnownComponent(self)),
        }
    }
}

impl<'a> ArgScope<'a, 'a> for WellKnownComponent {
    fn leaf_name(&'a self) -> Result<LeafName<'a>> {
        Ok(LeafName::WellKnownComponent(self))
    }

    fn get_template_arg(&'a self, _: usize) -> Result<(&'a TemplateArg, &'a TemplateArgs)> {
        Err(error::Error::BadTemplateArgReference)
    }

    fn get_function_arg(&'a self, _: usize) -> Result<&'a Type> {
        Err(error::Error::BadFunctionArgReference)
    }
}

impl<'subs> DemangleAsLeaf<'subs> for WellKnownComponent {
    fn demangle_as_leaf<'me, 'ctx>(&'me self, ctx: &'ctx mut DemangleContext<'subs>) {
        match *self {
            WellKnownComponent::Std => {
                panic!("should never treat `WellKnownComponent::Std` as a leaf name")
            }
            WellKnownComponent::StdAllocator => ctx.push("allocator", Colors::known()),
            WellKnownComponent::StdString1 => ctx.push("basic_string", Colors::known()),
            WellKnownComponent::StdString2 => ctx.push("string", Colors::known()),
            WellKnownComponent::StdIstream => ctx.push("basic_istream", Colors::known()),
            WellKnownComponent::StdOstream => ctx.push("ostream", Colors::known()),
            WellKnownComponent::StdIostream => ctx.push("basic_iostream", Colors::known()),
        }
    }
}

/// The `<special-name>` production.
///
/// The `<special-name>` production is spread in pieces through out the ABI
/// spec, and then there are a bunch of `g++` extensions that have become de
/// facto.
///
/// ### 5.1.4.1 Virtual Tables and RTTI
///
/// ```text
/// <special-name> ::= TV <type>    # virtual table
///                ::= TT <type>    # VTT structure (construction vtable index)
///                ::= TI <type>    # typeinfo structure
///                ::= TS <type>    # typeinfo name (null-terminated byte string)
/// ```
///
/// ### 5.1.4.2 Virtual Override Thunks
///
/// ```text
/// <special-name> ::= T <call-offset> <base encoding>
///     # base is the nominal target function of thunk
///
/// <special-name> ::= Tc <call-offset> <call-offset> <base encoding>
///     # base is the nominal target function of thunk
///     # first call-offset is 'this' adjustment
///     # second call-offset is result adjustment
/// ```
///
/// ### 5.1.4.4 Guard Variables
///
/// ```text
/// <special-name> ::= GV <object name> # Guard variable for one-time initialization
///     # No <type>
/// ```
///
/// ### 5.1.4.5 Lifetime-Extended Temporaries
///
/// ```text
/// <special-name> ::= GR <object name> _             # First temporary
/// <special-name> ::= GR <object name> <seq-id> _    # Subsequent temporaries
/// ```
///
/// ### De Facto Standard Extensions
///
/// ```text
/// <special-name> ::= TC <type> <number> _ <type>    # construction vtable
///                ::= TF <type>                      # typinfo function
///                ::= TH <name>                      # TLS initialization function
///                ::= TW <name>                      # TLS wrapper function
///                ::= Gr <resource name>             # Java Resource
///                ::= GTt <encoding>                 # Transaction-Safe function
///                ::= GTn <encoding>                 # Non-Transaction-Safe function
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum SpecialName {
    /// A virtual table.
    VirtualTable(TypeHandle),

    /// A VTT structure (construction vtable index).
    Vtt(TypeHandle),

    /// A typeinfo structure.
    Typeinfo(TypeHandle),

    /// A typeinfo name (null-terminated byte string).
    TypeinfoName(TypeHandle),

    /// A virtual override thunk.
    VirtualOverrideThunk(CallOffset, Box<Encoding>),

    /// A virtual override thunk with a covariant return type.
    VirtualOverrideThunkCovariant(CallOffset, CallOffset, Box<Encoding>),

    /// An initialization guard for some static storage.
    Guard(Name),

    /// A temporary used in the initialization of a static storage and promoted
    /// to a static lifetime.
    GuardTemporary(Name, usize),

    /// A construction vtable structure.
    ConstructionVtable(TypeHandle, usize, TypeHandle),

    /// A typeinfo function.
    TypeinfoFunction(TypeHandle),

    /// A TLS initialization function.
    TlsInit(Name),

    /// A TLS wrapper function.
    TlsWrapper(Name),

    /// A Java Resource.
    JavaResource(Vec<ResourceName>),

    /// A function declared transaction-safe
    TransactionClone(Box<Encoding>),

    /// A function declared non-transaction-safe
    NonTransactionClone(Box<Encoding>),
}

impl Parse for SpecialName {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(SpecialName, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        let (head, tail) = match input.try_split_at(2) {
            None => return Err(error::Error::UnexpectedEnd),
            Some((head, tail)) => (head, tail),
        };

        match head.as_ref() {
            b"TV" => {
                let (ty, tail) = TypeHandle::parse(ctx, subs, tail)?;
                Ok((SpecialName::VirtualTable(ty), tail))
            }
            b"TT" => {
                let (ty, tail) = TypeHandle::parse(ctx, subs, tail)?;
                Ok((SpecialName::Vtt(ty), tail))
            }
            b"TI" => {
                let (ty, tail) = TypeHandle::parse(ctx, subs, tail)?;
                Ok((SpecialName::Typeinfo(ty), tail))
            }
            b"TS" => {
                let (ty, tail) = TypeHandle::parse(ctx, subs, tail)?;
                Ok((SpecialName::TypeinfoName(ty), tail))
            }
            b"Tc" => {
                let (first, tail) = CallOffset::parse(ctx, subs, tail)?;
                let (second, tail) = CallOffset::parse(ctx, subs, tail)?;
                let (base, tail) = Encoding::parse(ctx, subs, tail)?;
                Ok((
                    SpecialName::VirtualOverrideThunkCovariant(first, second, Box::new(base)),
                    tail,
                ))
            }
            b"Th" | b"Tv" => {
                // The "h"/"v" is part of the `<call-offset>`, so back up to the
                // `input`.
                let tail = consume(b"T", input).unwrap();
                let (offset, tail) = CallOffset::parse(ctx, subs, tail)?;
                let (base, tail) = Encoding::parse(ctx, subs, tail)?;
                Ok((
                    SpecialName::VirtualOverrideThunk(offset, Box::new(base)),
                    tail,
                ))
            }
            b"TC" => {
                let (ty1, tail) = TypeHandle::parse(ctx, subs, tail)?;
                let (n, tail) = parse_number(10, false, tail)?;
                let tail = consume(b"_", tail)?;
                let (ty2, tail) = TypeHandle::parse(ctx, subs, tail)?;
                Ok((SpecialName::ConstructionVtable(ty1, n as usize, ty2), tail))
            }
            b"TF" => {
                let (ty, tail) = TypeHandle::parse(ctx, subs, tail)?;
                Ok((SpecialName::TypeinfoFunction(ty), tail))
            }
            b"TH" => {
                let (name, tail) = Name::parse(ctx, subs, tail)?;
                Ok((SpecialName::TlsInit(name), tail))
            }
            b"TW" => {
                let (name, tail) = Name::parse(ctx, subs, tail)?;
                Ok((SpecialName::TlsWrapper(name), tail))
            }
            b"GV" => {
                let (name, tail) = Name::parse(ctx, subs, tail)?;
                Ok((SpecialName::Guard(name), tail))
            }
            b"GR" => {
                let (name, tail) = Name::parse(ctx, subs, tail)?;
                let (idx, tail) = if let Ok(tail) = consume(b"_", tail) {
                    (0, tail)
                } else {
                    let (idx, tail) = SeqId::parse(ctx, subs, tail)?;
                    let tail = consume(b"_", tail)?;
                    (idx.0 + 1, tail)
                };
                Ok((SpecialName::GuardTemporary(name, idx), tail))
            }
            b"Gr" => {
                let (resource_name_len, tail) = parse_number(10, false, tail)?;
                if resource_name_len == 0 {
                    return Err(error::Error::UnexpectedText);
                }

                let (head, tail) = match tail.try_split_at(resource_name_len as _) {
                    Some((head, tail)) => (head, tail),
                    None => return Err(error::Error::UnexpectedEnd),
                };

                let head = consume(b"_", head)?;

                let (resource_names, empty) = zero_or_more::<ResourceName>(ctx, subs, head)?;
                if !empty.is_empty() {
                    return Err(error::Error::UnexpectedText);
                }

                Ok((SpecialName::JavaResource(resource_names), tail))
            }
            b"GT" => {
                match tail.next_or(error::Error::UnexpectedEnd)? {
                    (b'n', tail) => {
                        let (base, tail) = Encoding::parse(ctx, subs, tail)?;
                        Ok((SpecialName::NonTransactionClone(Box::new(base)), tail))
                    }
                    // Different letters could stand for different types of
                    // transactional cloning, but for now, treat them all the same
                    (b't', tail) | (_, tail) => {
                        let (base, tail) = Encoding::parse(ctx, subs, tail)?;
                        Ok((SpecialName::TransactionClone(Box::new(base)), tail))
                    }
                }
            }
            _ => Err(error::Error::UnexpectedText),
        }
    }
}

impl<'subs> Demangle<'subs> for SpecialName {
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        match *self {
            SpecialName::VirtualTable(ref ty) => {
                ctx.push("{{", Colors::brackets());
                ctx.push("vtable", Colors::known());
                ctx.push("(", Colors::brackets());

                ty.demangle(ctx, scope);

                ctx.push(")}}", Colors::brackets());
            }
            SpecialName::Vtt(ref ty) => {
                ctx.push("{{", Colors::brackets());
                ctx.push("vtt", Colors::known());
                ctx.push("(", Colors::brackets());

                ty.demangle(ctx, scope);
                ctx.push(")}}", Colors::brackets());
            }
            SpecialName::Typeinfo(ref ty) => {
                ctx.push("typeinfo", Colors::known());
                ctx.push(" for ", Colors::brackets());
                ty.demangle(ctx, scope)
            }
            SpecialName::TypeinfoName(ref ty) => {
                ctx.push("typeinfo name", Colors::known());
                ctx.push(" for ", Colors::brackets());
                ty.demangle(ctx, scope)
            }
            SpecialName::VirtualOverrideThunk(ref offset, ref encoding) => {
                ctx.push("{{", Colors::brackets());
                ctx.push("virtual override thunk", Colors::known());
                ctx.push("(", Colors::brackets());

                offset.demangle(ctx, scope);
                ctx.push(", ", Colors::delimiter());
                encoding.demangle(ctx, scope);
                ctx.push(")}}", Colors::brackets());
            }
            SpecialName::VirtualOverrideThunkCovariant(
                ref this_offset,
                ref result_offset,
                ref encoding,
            ) => {
                ctx.push("{{", Colors::brackets());
                ctx.push("virtual override thunk", Colors::known());
                ctx.push("(", Colors::brackets());

                this_offset.demangle(ctx, scope);
                ctx.push(", ", Colors::delimiter());
                result_offset.demangle(ctx, scope);
                ctx.push(", ", Colors::delimiter());
                encoding.demangle(ctx, scope);
                ctx.push(")}}", Colors::brackets());
            }
            SpecialName::Guard(ref name) => {
                ctx.push("guard variable", Colors::known());
                ctx.push(" for ", Colors::brackets());
                name.demangle(ctx, scope)
            }
            SpecialName::GuardTemporary(ref name, n) => {
                ctx.push("reference temporary", Colors::known());
                ctx.push(" #", Colors::brackets());
                ctx.push_owned(n.to_string(), Colors::item());
                ctx.push(" for ", Colors::brackets());
                name.demangle(ctx, scope)
            }
            SpecialName::ConstructionVtable(ref ty1, _, ref ty2) => {
                ctx.push("construction vtable", Colors::known());
                ctx.push(" for ", Colors::brackets());

                ty1.demangle(ctx, scope);
                ctx.push("-in-", Colors::brackets());
                ty2.demangle(ctx, scope)
            }
            SpecialName::TypeinfoFunction(ref ty) => {
                ctx.push("typeinfo fn", Colors::known());
                ctx.push(" for ", Colors::brackets());
                ty.demangle(ctx, scope)
            }
            SpecialName::TlsInit(ref name) => {
                ctx.push("TLS init function", Colors::known());
                ctx.push(" for ", Colors::brackets());
                name.demangle(ctx, scope)
            }
            SpecialName::TlsWrapper(ref name) => {
                ctx.push("TLS wrapper function", Colors::known());
                ctx.push(" for ", Colors::brackets());
                name.demangle(ctx, scope)
            }
            SpecialName::TransactionClone(ref encoding) => {
                ctx.push("transaction clone", Colors::known());
                ctx.push(" for ", Colors::brackets());
                encoding.demangle(ctx, scope)
            }
            SpecialName::NonTransactionClone(ref encoding) => {
                ctx.push("non-transaction clone", Colors::known());
                ctx.push(" for ", Colors::brackets());
                encoding.demangle(ctx, scope)
            }
            SpecialName::JavaResource(ref names) => {
                ctx.push("java resource ", Colors::known());
                for name in names {
                    name.demangle(ctx, scope);
                }
            }
        }
    }
}

/// The `<resource name>` pseudo-terminal.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct ResourceName {
    pub start: usize,
    pub end: usize,
}

impl Parse for ResourceName {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        _subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(ResourceName, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        if input.is_empty() {
            return Err(error::Error::UnexpectedEnd);
        }

        let mut end = input
            .as_ref()
            .iter()
            .map(|&c| c as char)
            .take_while(|&c| c != '$' || c.is_digit(36))
            .count();

        if end == 0 {
            return Err(error::Error::UnexpectedText);
        }

        if input.range_from(end..).peek() == Some(b'$') {
            match input.range_from(end..).peek_second() {
                Some(b'S') | Some(b'_') | Some(b'$') => end += 2,
                _ => return Err(error::Error::UnexpectedText),
            }
        }

        let tail = input.range_from(end..);

        let resource_name = ResourceName {
            start: input.index(),
            end: tail.index(),
        };

        Ok((resource_name, tail))
    }
}

impl<'subs> Demangle<'subs> for ResourceName {
    #[inline]
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        _: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        let mut i = self.start;
        while i < self.end {
            let ch = ctx.input[i];
            if ch == b'$' {
                // Skip past the '$'
                i += 1;
                match ctx.input[i] {
                    b'S' => ctx.push("/", Colors::comment()),
                    b'_' => ctx.push(".", Colors::comment()),
                    b'$' => ctx.push("$", Colors::comment()),
                    _ => {
                        // Fall through
                    }
                }
            } else {
                ctx.push_owned(format!("{}", ch as char), Colors::comment())
            }
            i += 1;
        }
    }
}

/// The subobject expression production.
///
/// <expression> ::= so <referent type> <expr> [<offset number>] <union-selector>* [p] E
/// <union-selector> ::= _ [<number>]
///
/// Not yet in the spec: <https://github.com/itanium-cxx-abi/cxx-abi/issues/47>
/// But it has been shipping in clang for some time.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct SubobjectExpr {
    pub ty: TypeHandle,
    pub expr: Box<Expression>,
    pub offset: isize,
}

impl Parse for SubobjectExpr {
    fn parse<'a, 'b>(
        ctx: &'a ParseContext,
        subs: &'a mut SubstitutionTable,
        input: IndexStr<'b>,
    ) -> Result<(SubobjectExpr, IndexStr<'b>)> {
        try_begin_parse!(ctx);

        let (ty, tail) = TypeHandle::parse(ctx, subs, input)?;
        let (expr, tail) = Expression::parse(ctx, subs, tail)?;
        let (offset, tail) = parse_number(10, true, tail).unwrap_or((0, tail));

        // XXXkhuey handle union-selector and [p]
        let tail = consume(b"E", tail)?;
        Ok((
            SubobjectExpr {
                ty,
                expr: Box::new(expr),
                offset,
            },
            tail,
        ))
    }
}

impl<'subs> Demangle<'subs> for SubobjectExpr {
    #[inline]
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut DemangleContext<'subs>,
        scope: Option<ArgScopeStack<'prev, 'subs>>,
    ) {
        self.expr.demangle(ctx, scope);
        ctx.push(".", Colors::comment());
        ctx.push("<", Colors::brackets());
        self.ty.demangle(ctx, scope);
        ctx.push(" at offset ", Colors::brackets());
        ctx.push_owned(self.offset.to_string(), Colors::item());
        ctx.push(">", Colors::annotation());
    }
}

/// Expect and consume the given byte str, and return the advanced `IndexStr` if
/// we saw the expectation. Otherwise return an error of kind
/// `error::Error::UnexpectedText` if the input doesn't match, or
/// `error::Error::UnexpectedEnd` if it isn't long enough.
#[inline]
fn consume<'a>(expected: &[u8], input: IndexStr<'a>) -> Result<IndexStr<'a>> {
    match input.try_split_at(expected.len()) {
        Some((head, tail)) if head == expected => Ok(tail),
        Some(_) => Err(error::Error::UnexpectedText),
        None => Err(error::Error::UnexpectedEnd),
    }
}

fn one_or_more<'a, 'b, P: Parse>(
    ctx: &'a ParseContext,
    subs: &'a mut SubstitutionTable,
    input: IndexStr<'b>,
) -> Result<(Vec<P>, IndexStr<'b>)> {
    let (first, mut tail) = P::parse(ctx, subs, input)?;
    let mut results = vec![first];
    loop {
        if let Ok((parsed, tail_tail)) = P::parse(ctx, subs, tail) {
            results.push(parsed);
            tail = tail_tail;
        } else {
            return Ok((results, tail));
        }
    }
}

fn zero_or_more<'a, 'b, P: Parse>(
    ctx: &'a ParseContext,
    subs: &'a mut SubstitutionTable,
    input: IndexStr<'b>,
) -> Result<(Vec<P>, IndexStr<'b>)> {
    let mut tail = input;
    let mut results = vec![];
    loop {
        if let Ok((parsed, tail_tail)) = P::parse(ctx, subs, tail) {
            results.push(parsed);
            tail = tail_tail;
        } else {
            return Ok((results, tail));
        }
    }
}

/// Parse a number with the given `base`. Do not allow negative numbers
/// (prefixed with an 'n' instead of a '-') if `allow_signed` is false.
#[allow(unsafe_code)]
fn parse_number(base: u32, allow_signed: bool, mut input: IndexStr) -> Result<(isize, IndexStr)> {
    if input.is_empty() {
        return Err(error::Error::UnexpectedEnd);
    }

    let num_is_negative = if allow_signed && input.as_ref()[0] == b'n' {
        input = input.range_from(1..);

        if input.is_empty() {
            return Err(error::Error::UnexpectedEnd);
        }

        true
    } else {
        false
    };

    let num_numeric = input
        .as_ref()
        .iter()
        .map(|&c| c as char)
        .take_while(|c| c.is_digit(base) && (c.is_numeric() || c.is_uppercase()))
        .count();
    if num_numeric == 0 {
        return Err(error::Error::UnexpectedText);
    }

    let (head, tail) = input.split_at(num_numeric);
    let head = head.as_ref();

    if num_numeric > 1 && head[0] == b'0' {
        // "<number>s appearing in mangled names never have leading zeroes,
        // except for the value zero, represented as '0'."
        return Err(error::Error::UnexpectedText);
    }

    let head = unsafe {
        // Safe because we know we only have valid numeric chars in this
        // slice, which are valid UTF-8.
        str::from_utf8_unchecked(head)
    };

    let mut number = isize::from_str_radix(head, base).map_err(|_| error::Error::Overflow)?;
    if num_is_negative {
        number = -number;
    }

    Ok((number, tail))
}
