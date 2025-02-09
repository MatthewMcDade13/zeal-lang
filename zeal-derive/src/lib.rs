// TODO: Need to fix this: https://github.com/copilot/c/4327b484-d08c-4061-87aa-b734d0f12d05

// use proc_macro2::Span;
// use quote::{ToTokens, quote};
// use syn::{
//     Attribute, Data, DeriveInput, Fields, FieldsNamed, FieldsUnnamed, GenericArgument,
//     GenericParam, Ident, Meta, Path, PathArguments, Type, TypePath, Variant, parse_macro_input,
//     spanned::Spanned,
// };
//
// #[proc_macro_derive(FFISafe)]
// pub fn derive_ffi_safe(input: TokenStream) -> TokenStream {
//     let input = parse_macro_input!(input as DeriveInput);
//
//     match validate_type_definition(&input) {
//         Ok(_) => generate_implementation(&input),
//         Err(err) => err.to_compile_error().into(),
//     }
// }
//
// fn generate_implementation(input: &DeriveInput) -> TokenStream {
//     let name = &input.ident;
//     let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
//
//     // Generate additional trait bounds for generic parameters
//     let mut generics = input.generics.clone();
//     for param in &mut generics.params {
//         if let GenericParam::Type(type_param) = param {
//             type_param
//                 .bounds
//                 .push(syn::parse_quote!(::ffi_safe::FFISafeMarker));
//         }
//     }
//     let (impl_generics, _, where_clause) = generics.split_for_impl();
//
//     let validation = quote! {
//         const _: () = {
//             // Compile-time assertion for size being non-zero
//             assert!(::core::mem::size_of::<#name #ty_generics>() > 0);
//
//             // Verify alignment is a power of 2
//             const _: () = assert!(::core::mem::align_of::<#name #ty_generics>().is_power_of_two());
//
//             // Ensure the type is Copy if it's marked as transparent
//             #[cfg(any(target_arch = "x86", target_arch = "x86_64", target_arch = "aarch64"))]
//             const _: () = assert!(::core::mem::size_of::<#name #ty_generics>() <= 32);
//         };
//     };
//
//     let expanded = quote! {
//         #validation
//
//         unsafe impl #impl_generics ::ffi_safe::FFISafeMarker for #name #ty_generics #where_clause {}
//
//         // Implement additional safety traits
//         unsafe impl #impl_generics ::core::marker::Copy for #name #ty_generics #where_clause {}
//         unsafe impl #impl_generics ::core::marker::Send for #name #ty_generics #where_clause {}
//         unsafe impl #impl_generics ::core::marker::Sync for #name #ty_generics #where_clause {}
//     };
//
//     expanded.into()
// }
//
// fn validate_type_definition(input: &DeriveInput) -> anyhow::Result<()> {
//     // Validate repr attributes
//     validate_repr_attributes(input)?;
//
//     // Validate type parameters
//     validate_generics(&input.generics)?;
//
//     // Validate the type's structure
//     match &input.data {
//         Data::Struct(data) => validate_struct(input, data)?,
//         Data::Enum(data) => validate_enum(input, data)?,
//         Data::Union(data) => validate_union(input, data)?,
//     }
//
//     Ok(())
// }
//
// fn validate_repr_attributes(input: &DeriveInput) -> anyhow::Result<()> {
//     let mut has_repr_c = false;
//     let mut has_repr_transparent = false;
//     let mut has_repr_packed = false;
//
//     for attr in &input.attrs {
//         if !attr.path().is_ident("repr") {
//             continue;
//         }
//
//         if let Ok(Meta::List(list)) = attr.parse_args() {
//             let tokens = list.tokens.to_string();
//             has_repr_c |= tokens.contains("C");
//             has_repr_transparent |= tokens.contains("transparent");
//             has_repr_packed |= tokens.contains("packed");
//         }
//     }
//
//     if has_repr_packed {
//         return Err(syn::Error::new(
//             input.ident.span(),
//             "FFISafe types cannot be #[repr(packed)] as it may cause undefined behavior",
//         ));
//     }
//
//     if !has_repr_c && !has_repr_transparent {
//         return Err(syn::Error::new(
//             input.ident.span(),
//             "FFISafe types must have either #[repr(C)] or #[repr(transparent)]",
//         ));
//     }
//
//     if has_repr_transparent {
//         validate_transparent_type(input)?;
//     }
//
//     Ok(())
// }
//
// fn validate_transparent_type(input: &DeriveInput) -> anyhow::Result<()> {
//     if let Data::Struct(data) = &input.data {
//         match &data.fields {
//             Fields::Named(FieldsNamed { named, .. })
//             | Fields::Unnamed(FieldsUnnamed { unnamed: named, .. }) => {
//                 if named.len() != 1 {
//                     return Err(syn::Error::new(
//                         input.ident.span(),
//                         "repr(transparent) types must have exactly one non-zero-sized field",
//                     ));
//                 }
//             }
//             Fields::Unit => {
//                 return Err(syn::Error::new(
//                     input.ident.span(),
//                     "repr(transparent) cannot be used with unit structs",
//                 ));
//             }
//         }
//     } else {
//         return Err(syn::Error::new(
//             input.ident.span(),
//             "repr(transparent) can only be used with structs",
//         ));
//     }
//     Ok(())
// }
//
// fn validate_struct(input: &DeriveInput, data: &syn::DataStruct) -> anyhow::Result<()> {
//     validate_fields(&data.fields)?;
//     validate_field_offsets(&input.ident, &data.fields)?;
//     Ok(())
// }
//
// fn validate_enum(input: &DeriveInput, data: &syn::DataEnum) -> anyhow::Result<()> {
//     // Ensure enum has repr(C)
//     if !has_repr_c(&input.attrs) {
//         return Err(syn::Error::new(
//             input.ident.span(),
//             "FFISafe enums must have #[repr(C)]",
//         ));
//     }
//
//     // Validate discriminant size
//     let variant_count = data.variants.len();
//     let required_bits = 32 - (variant_count - 1).leading_zeros();
//     if required_bits > 32 {
//         return Err(syn::Error::new(
//             input.ident.span(),
//             "FFISafe enums must have fewer than 2^32 variants",
//         ));
//     }
//
//     // Validate all variants
//     for variant in &data.variants {
//         validate_variant(variant)?;
//     }
//
//     Ok(())
// }
//
// fn validate_named_fields(fields: &syn::FieldsNamed) -> anyhow::Result<()> {
//     for field in &fields.named {
//         validate_field_type(&field.ty)?;
//     }
//     Ok(())
// }
//
// fn validate_unnamed_fields(fields: &syn::FieldsUnnamed) -> anyhow::Result<()> {
//     for field in &fields.unnamed {
//         validate_field_type(&field.ty)?;
//     }
//     Ok(())
// }
//
// // And update the validate_fields function to use them:
// fn validate_fields(fields: &Fields) -> anyhow::Result<()> {
//     match fields {
//         Fields::Named(fields) => validate_named_fields(fields),
//         Fields::Unnamed(fields) => validate_unnamed_fields(fields),
//         Fields::Unit => Ok(()),
//     }
// }
//
// // Update the validate_union function:
// fn validate_union(input: &DeriveInput, data: &syn::DataUnion) -> anyhow::Result<()> {
//     validate_named_fields(&data.fields)?;
//
//     // Check that all fields have the same size
//     let fields = &data.fields;
//     if fields.named.is_empty() {
//         return Ok(());
//     }
//
//     let first_field = &fields.named.first().unwrap();
//     let first_field_type = &first_field.ty;
//     let first_field_size = quote! {
//         ::core::mem::size_of::<#first_field_type>()
//     };
//
//     for field in fields.named.iter().skip(1) {
//         let field_type = &field.ty;
//         let field_size = quote! {
//             ::core::mem::size_of::<#field_type>()
//         };
//         if field_size.to_string() != first_field_size.to_string() {
//             return Err(syn::Error::new(
//                 field.span(),
//                 "All fields in an FFISafe union must have the same size",
//             ));
//         }
//     }
//
//     Ok(())
// }
//
// // Update validate_variant function:
// fn validate_variant(variant: &Variant) -> anyhow::Result<()> {
//     match &variant.fields {
//         Fields::Named(fields) => validate_named_fields(fields)?,
//         Fields::Unnamed(fields) => validate_unnamed_fields(fields)?,
//         Fields::Unit => Ok(())?,
//     }
//
//     // Ensure variant discriminant is FFI-safe
//     if let Some((_, expr)) = &variant.discriminant {
//         // TODO: Validate that the discriminant expression is a constant integer
//     }
//
//     Ok(())
// }
//
// // Update validate_field_offsets:
// fn validate_field_offsets(name: &Ident, fields: &Fields) -> anyhow::Result<()> {
//     match fields {
//         Fields::Named(named_fields) => {
//             let field_tokens = named_fields.named.iter().map(|f| {
//                 let name = &f.ident;
//                 let ty = &f.ty;
//                 quote! { #name: #ty }
//             });
//
//             let assertions = quote! {
//                 const _: () = {
//                     #[repr(C)]
//                     struct AlignmentCheck {
//                         #(#field_tokens,)*
//                     }
//
//                     assert!(::core::mem::align_of::<AlignmentCheck>() == ::core::mem::align_of::<#name>());
//                 };
//             };
//         }
//         Fields::Unnamed(unnamed_fields) => {
//             let field_tokens = unnamed_fields.unnamed.iter().map(|f| {
//                 let ty = &f.ty;
//                 quote! { #ty }
//             });
//
//             let assertions = quote! {
//                 const _: () = {
//                     #[repr(C)]
//                     struct AlignmentCheck(#(#field_tokens,)*);
//
//                     assert!(::core::mem::align_of::<AlignmentCheck>() == ::core::mem::align_of::<#name>());
//                 };
//             };
//         }
//         Fields::Unit => {
//             // No fields to check alignment for
//         }
//     }
//
//     Ok(())
// }
//
// fn validate_fields(fields: &Fields) -> anyhow::Result<()> {
//     match fields {
//         Fields::Named(fields) => {
//             for field in &fields.named {
//                 validate_field_type(&field.ty)?;
//             }
//         }
//         Fields::Unnamed(fields) => {
//             for field in &fields.unnamed {
//                 validate_field_type(&field.ty)?;
//             }
//         }
//         Fields::Unit => {}
//     }
//     Ok(())
// }
//
// fn validate_field_type(ty: &Type) -> anyhow::Result<()> {
//     match ty {
//         Type::Path(TypePath { path, .. }) => {
//             let last_segment = path
//                 .segments
//                 .last()
//                 .ok_or_else(|| syn::Error::new(path.span(), "Empty type path"))?;
//
//             if !is_primitive_ffi_safe(&last_segment.ident.to_string()) {
//                 // Check if it's a custom type that implements FFISafeMarker
//                 let type_name = path
//                     .get_ident()
//                     .ok_or_else(|| syn::Error::new(path.span(), "Invalid type name"))?;
//
//                 validate_custom_type(type_name)?;
//             }
//
//             // Validate generic arguments
//             match &last_segment.arguments {
//                 PathArguments::None => {}
//                 PathArguments::AngleBracketed(args) => {
//                     for arg in &args.args {
//                         if let GenericArgument::Type(ty) = arg {
//                             validate_field_type(ty)?;
//                         }
//                     }
//                 }
//                 PathArguments::Parenthesized(_) => {
//                     return Err(syn::Error::new(
//                         last_segment.span(),
//                         "Function pointer types must be defined using unsafe extern fn",
//                     ));
//                 }
//             }
//         }
//         Type::Array(array) => {
//             validate_field_type(&array.elem)?;
//         }
//         Type::Ptr(ptr) => {
//             validate_field_type(&ptr.elem)?;
//         }
//         Type::Reference(_) => {
//             return Err(syn::Error::new(
//                 ty.span(),
//                 "References are not FFI-safe. Use raw pointers instead.",
//             ));
//         }
//         _ => {
//             return Err(syn::Error::new(
//                 ty.span(),
//                 "This type is not guaranteed to be FFI-safe",
//             ));
//         }
//     }
//     Ok(())
// }
//
// fn validate_variant(variant: &Variant) -> anyhow::Result<()> {
//     validate_fields(&variant.fields)?;
//
//     // Ensure variant discriminant is FFI-safe
//     if let Some((_, expr)) = &variant.discriminant {
//         // TODO: Validate that the discriminant expression is a constant integer
//     }
//
//     Ok(())
// }
//
// fn validate_field_offsets(name: &Ident, fields: &Fields) -> anyhow::Result<()> {
//     // Generate compile-time assertions for field alignment
//     let assertions = quote! {
//         const _: () = {
//             #[repr(C)]
//             struct AlignmentCheck {
//                 #fields
//             }
//
//             assert!(::core::mem::align_of::<AlignmentCheck>() == ::core::mem::align_of::<#name>());
//         };
//     };
//
//     Ok(())
// }
//
// fn validate_custom_type(type_name: &Ident) -> anyhow::Result<()> {
//     // This would be checked at compile time when the type is used
//     Ok(())
// }
//
// fn is_primitive_ffi_safe(type_name: &str) -> bool {
//     matches!(
//         type_name,
//         // Integers
//         "i8" | "i16" | "i32" | "i64" | "isize" |
//         "u8" | "u16" | "u32" | "u64" | "usize" |
//         // Floating point
//         "f32" | "f64" |
//         // C-compatible types
//         "c_char" | "c_schar" | "c_uchar" |
//         "c_short" | "c_ushort" |
//         "c_int" | "c_uint" |
//         "c_long" | "c_ulong" |
//         "c_longlong" | "c_ulonglong" |
//         "c_float" | "c_double" |
//         // Other
//         "bool" // assuming bool is defined as u8 on the target platform
//     )
// }
//
// fn validate_generics(generics: &syn::Generics) -> anyhow::Result<()> {
//     // Validate each generic parameter
//     for param in &generics.params {
//         match param {
//             syn::GenericParam::Type(type_param) => {
//                 // Validate type parameter bounds
//                 if type_param.bounds.is_empty() {
//                     anyhow::bail!(
//                         "Generic type parameter '{}' must have FFISafeMarker bound",
//                         type_param.ident
//                     )
//                 }
//
//                 // Check that the type parameter isn't used in const generics position
//                 for bound in &type_param.bounds {
//                     if let syn::TypeParamBound::Trait(trait_bound) = bound {
//                         if trait_bound
//                             .path
//                             .segments
//                             .last()
//                             .map(|seg| seg.ident == "Sized")
//                             .unwrap_or(false)
//                         {
//                             return Ok(());
//                         }
//                     }
//                 }
//
//                 // Add implicit Sized bound if not present
//                 // This is important for FFI safety
//             }
//             syn::GenericParam::Lifetime(lifetime_param) => {
//                 anyhow::bail!(
//                     "FFISafe types cannot have lifetime parameters {:?}",
//                     lifetime_param.lifetime.span()
//                 )
//             }
//             syn::GenericParam::Const(const_param) => {
//                 // Const generics are allowed only for array lengths
//                 if !is_valid_const_param_type(&const_param.ty) {
//                     anyhow::bail!(
//                         "Const parameters must be integral types for FFISafe types :: {:?}",
//                         const_param.ty.span()
//                     )
//                 }
//             }
//         }
//     }
//
//     // Validate where clause if present
//     if let Some(where_clause) = &generics.where_clause {
//         for predicate in &where_clause.predicates {
//             match predicate {
//                 syn::WherePredicate::Type(type_predicate) => {
//                     // Ensure the where clause doesn't introduce unsafe bounds
//                     validate_where_predicate(type_predicate)?;
//                 }
//                 _ => {
//                     anyhow::bail!(
//                         "Unsupported where clause predicate for FFISafe types :: {:?}",
//                         predicate.span(),
//                     )
//                 }
//             }
//         }
//     }
//
//     Ok(())
// }
//
// fn is_valid_const_param_type(ty: &syn::Type) -> bool {
//     if let syn::Type::Path(type_path) = ty {
//         let last_segment = type_path
//             .path
//             .segments
//             .last()
//             .map(|seg| seg.ident.to_string());
//
//         matches!(
//             last_segment.as_deref(),
//             Some("usize")
//                 | Some("u8")
//                 | Some("u16")
//                 | Some("u32")
//                 | Some("u64")
//                 | Some("isize")
//                 | Some("i8")
//                 | Some("i16")
//                 | Some("i32")
//                 | Some("i64")
//         )
//     } else {
//         false
//     }
// }
//
// fn validate_where_predicate(predicate: &syn::PredicateType) -> anyhow::Result<()> {
//     for bound in &predicate.bounds {
//         match bound {
//             syn::TypeParamBound::Trait(trait_bound) => {
//                 let last_segment = trait_bound
//                     .path
//                     .segments
//                     .last()
//                     .ok_or_else(|| syn::Error::new(trait_bound.span(), "Empty trait bound"))?;
//
//                 // Allow only specific traits that we know are FFI-safe
//                 let trait_name = last_segment.ident.to_string();
//                 if !is_safe_trait_bound(&trait_name) {
//                     anyhow::bail!(
//                         "Trait bound '{trait_name}' may not be FFI-safe:: {:?}",
//                         last_segment.ident.span(),
//                     );
//                 }
//             }
//             syn::TypeParamBound::Lifetime(_) => {
//                 anyhow::bail!(
//                     "Lifetime bounds are not allowed in FFISafe types: {:?}",
//                     bound.span(),
//                 )
//             }
//             _ => anyhow::bail!("UNKNOWN ERROR"),
//         }
//     }
//     Ok(())
// }
//
// fn is_safe_trait_bound(trait_name: &str) -> bool {
//     matches!(
//         trait_name,
//         "FFISafeMarker" | "Copy" | "Send" | "Sync" | "Sized"
//     )
// }
//
// fn has_repr_c(attrs: &[Attribute]) -> bool {
//     attrs.iter().any(|attr| {
//         if !attr.path().is_ident("repr") {
//             return false;
//         }
//
//         if let Ok(Meta::List(list)) = attr.parse_args() {
//             list.tokens.to_string().contains("C")
//         } else {
//             false
//         }
//     })
// }
//
// fn has_repr_transparent(attrs: &[Attribute]) -> bool {
//     attrs.iter().any(|attr| {
//         if !attr.path().is_ident("repr") {
//             return false;
//         }
//
//         if let Ok(Meta::List(list)) = attr.parse_args() {
//             list.tokens.to_string().contains("transparent")
//         } else {
//             false
//         }
//     })
// }
//
// fn has_repr_packed(attrs: &[Attribute]) -> bool {
//     attrs.iter().any(|attr| {
//         if !attr.path().is_ident("repr") {
//             return false;
//         }
//
//         if let Ok(Meta::List(list)) = attr.parse_args() {
//             list.tokens.to_string().contains("packed")
//         } else {
//             false
//         }
//     })
// }
//
// // Helper function to check if a type might be zero-sized
// fn is_potentially_zero_sized(ty: &Type) -> bool {
//     match ty {
//         Type::Tuple(tuple) if tuple.elems.is_empty() => true,
//         Type::Path(type_path) => {
//             let last_segment = type_path
//                 .path
//                 .segments
//                 .last()
//                 .map(|seg| seg.ident.to_string());
//             matches!(last_segment.as_deref(), Some("PhantomData"))
//         }
//         _ => false,
//     }
// }
//
// // Helper function to check if a field is padding
// fn is_padding_field(field: &syn::Field) -> bool {
//     field
//         .ident
//         .as_ref()
//         .map(|ident| ident.to_string().starts_with("_padding"))
//         .unwrap_or(false)
// }
