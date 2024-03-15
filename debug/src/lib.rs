use proc_macro::TokenStream;
use quote::{quote, quote_spanned};
use syn::{
    parse_macro_input, parse_quote, DeriveInput, ExprLit, Field, GenericParam, Generics, Lit,
    LitStr,
};

fn add_trait_bounds(mut generics: Generics) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(parse_quote!(std::fmt::Debug));
        }
    }
    generics
}

fn get_debug_field_attr(field: &Field) -> syn::Result<Option<&LitStr>> {
    let name_value = if let Some(attr) = field
        .attrs
        .iter()
        .find(|attr| attr.path().is_ident("debug"))
    {
        Some(attr.meta.require_name_value()?)
    } else {
        None
    };

    if let Some(name_value) = name_value {
        match &name_value.value {
            syn::Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) => Ok(Some(s)),
            _ => {
                return Err(syn::Error::new_spanned(
                    &name_value.value,
                    "expected format string",
                ))
            }
        }
    } else {
        Ok(None)
    }
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let derive_input = parse_macro_input!(input as DeriveInput);

    let struct_name = &derive_input.ident;

    let struct_name_str = format!("{struct_name}");

    let struct_fields = match &derive_input.data {
        syn::Data::Struct(s) => &s.fields,
        _ => {
            return syn::Error::new_spanned(struct_name, "expected struct")
                .into_compile_error()
                .into();
        }
    };

    let named_fields = match struct_fields {
        syn::Fields::Named(fields) => fields,
        _ => {
            return syn::Error::new_spanned(struct_fields, "expected struct with named fields")
                .into_compile_error()
                .into();
        }
    };

    let field_setters = named_fields.named.iter().map(|field| {
        let ident = field.ident.as_ref().expect("field is named");

        let debug_fmt = match get_debug_field_attr(field) {
            Ok(debug_fmt) => debug_fmt,
            Err(e) => return e.into_compile_error().into(),
        };

        let field_name = format!("{}", ident);

        match debug_fmt {
            Some(fmt) => quote_spanned!(ident.span()=>.field(#field_name, &DebugLiteral(format!(#fmt, &self.#ident)))),
            None => quote_spanned!(ident.span()=>.field(#field_name, &self.#ident)),
        }
    });

    let generics = add_trait_bounds(derive_input.generics);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    quote! {
        impl #impl_generics std::fmt::Debug for #struct_name #ty_generics #where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                struct DebugLiteral(String);

                impl std::fmt::Debug for DebugLiteral {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                        write!(f, "{}", self.0)
                    }
                }

                f.debug_struct(#struct_name_str)
                    #(#field_setters)*
                    .finish()
            }
        }
    }
    .into()
}
