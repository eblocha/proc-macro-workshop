mod special_type;

use proc_macro::TokenStream;
use quote::{quote, quote_spanned};
use special_type::{option_inner_type, vec_inner_type};
use syn::{
    parse_macro_input, punctuated::Punctuated, spanned::Spanned, DeriveInput, Expr, ExprLit, Field,
    Ident, Lit, Meta, Token,
};

fn get_each_name(field: &Field) -> Result<Option<Ident>, syn::Error> {
    const ERROR_MESSAGE: &str = r#"expected `builder(each = "...")`"#;

    let builder_attr = field
        .attrs
        .iter()
        .find(|attr| attr.path().is_ident("builder"));

    if let Some(builder_attr) = builder_attr {
        let nested =
            builder_attr.parse_args_with(Punctuated::<Meta, Token![,]>::parse_terminated)?;

        if let Some(meta) = nested.iter().next() {
            let name_literal = match meta {
                Meta::NameValue(name_value) if name_value.path.is_ident("each") => {
                    match &name_value.value {
                        Expr::Lit(ExprLit {
                            lit: Lit::Str(s), ..
                        }) => s,
                        _ => {
                            return Err(syn::Error::new_spanned(meta, ERROR_MESSAGE));
                        }
                    }
                }
                _ => {
                    return Err(syn::Error::new_spanned(meta, ERROR_MESSAGE));
                }
            };

            return Ok(Some(name_literal.parse()?));
        }
        return Err(syn::Error::new_spanned(nested, ERROR_MESSAGE));
    }

    Ok(None)
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let derive_input = parse_macro_input!(input as DeriveInput);

    let struct_name = &derive_input.ident;

    let builder_name = Ident::new(&format!("{}Builder", struct_name), derive_input.span());

    let struct_fields = match &derive_input.data {
        syn::Data::Struct(s) => &s.fields,
        _ => {
            return quote_spanned! {
                struct_name.span() => compile_error!("expected struct")
            }
            .into()
        }
    };

    let named_fields = match struct_fields {
        syn::Fields::Named(fields) => fields,
        _ => {
            return quote_spanned! {
                struct_fields.span() => compile_error!("expected struct with named fields.")
            }
            .into()
        }
    };

    let mut builder_methods = quote!();
    let mut builder_fields = quote!();
    let mut builder_initial_fields = quote!();
    let mut build_steps = quote!();

    for field in named_fields.named.iter() {
        // unwrap is infallible because it is checked to be a named field above.
        let name = field.ident.as_ref().unwrap();
        let ty = &field.ty;

        let each_name = match get_each_name(field) {
            Ok(each_name) => each_name,
            Err(err) => {
                return err.into_compile_error().into();
            }
        };

        if let Some(each_name) = each_name {
            // check if type is a vec, return compile error if not
            if let Some(vec_inner_type) = vec_inner_type(ty) {
                builder_initial_fields.extend(quote! {
                    #name: std::vec::Vec::new(),
                });

                builder_fields.extend(quote! {
                    #name: #ty,
                });

                builder_methods.extend(quote! {
                    fn #each_name(&mut self, #each_name: #vec_inner_type) -> &mut Self {
                        self.#name.push(#each_name);
                        self
                    }
                });

                if &each_name != name {
                    // if the `each` has a different name than the field, add a method to set the entire value.
                    builder_methods.extend(quote! {
                        fn #name(&mut self, #name: #ty) -> &mut Self {
                            self.#name = #name;
                            self
                        }
                    })
                }

                build_steps.extend(quote! {
                    #name: self.#name.clone(),
                });
            } else {
                return quote_spanned! {
                    ty.span() => compile_error!("expected Vec for field type with `each` attribute")
                }
                .into();
            }
        } else if let Some(inner_type) = option_inner_type(ty) {
            builder_initial_fields.extend(quote! {
                #name: None,
            });

            builder_fields.extend(quote! {
                #name: core::option::Option<#inner_type>,
            });

            builder_methods.extend(quote! {
                fn #name(&mut self, #name: #inner_type) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            });

            build_steps.extend(quote! {
                #name: self.#name.clone(),
            });
        } else {
            builder_initial_fields.extend(quote! {
                #name: None,
            });

            builder_fields.extend(quote! {
                #name: core::option::Option<#ty>,
            });

            builder_methods.extend(quote! {
                fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            });

            let name_str = format!("{name} was not specified");

            build_steps.extend(quote! {
                #name: self.#name.clone().ok_or_else(|| #name_str.to_owned())?,
            });
        }
    }

    let result = quote! {
        struct #builder_name {
            #builder_fields
        }

        impl #struct_name {
            fn builder() -> #builder_name {
                #builder_name {
                    #builder_initial_fields
                }
            }
        }

        impl #builder_name {
            #builder_methods

            fn build(&mut self) -> core::result::Result<#struct_name, std::boxed::Box<dyn std::error::Error>> {
                Ok(#struct_name {
                    #build_steps
                })
            }
        }
    };

    result.into()
}
