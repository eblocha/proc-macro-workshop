use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, spanned::Spanned, AngleBracketedGenericArguments, DeriveInput, GenericArgument, Ident, PathArguments, PathSegment, Type};

fn option_inner_type_from_type_args(args: &AngleBracketedGenericArguments) -> Option<&Type> {
    if args.args.len() != 1 {
        return None;
    }

    args.args.last().and_then(|arg| {
        match arg {
            GenericArgument::Type(ty) => Some(ty),
            _ => None
        }
    })
}

fn option_inner_type_from_ident(segment: &PathSegment) -> Option<&Type> {
    if segment.ident == "Option" {
        match &segment.arguments {
            PathArguments::AngleBracketed(args) => option_inner_type_from_type_args(args),
            _ => None
        }
    } else {
        None
    }
}

fn option_inner_type(ty: &Type) -> Option<&Type> {
    match ty {
        Type::Path(type_path) => type_path
            .path
            .segments
            .last()
            .and_then(option_inner_type_from_ident),
        _ => None,
    }
}

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let derive_input = parse_macro_input!(input as DeriveInput);

    let struct_name = &derive_input.ident;

    let builder_name = Ident::new(&format!("{}Builder", struct_name), derive_input.span());

    let struct_fields = match &derive_input.data {
        syn::Data::Struct(s) => &s.fields,
        _ => panic!("Builder can only be applied to structs."),
    };

    let named_fields = match struct_fields {
        syn::Fields::Named(fields) => fields,
        _ => panic!("Builder can only be applied to structs with named fields."),
    };

    let mut builder_methods = quote!();
    let mut builder_fields = quote!();
    let mut builder_initial_fields = quote!();
    let mut build_steps = quote!();

    for field in named_fields.named.iter() {
        // unwrap is infallible because it is checked to be a named field above.
        let name = field.ident.as_ref().unwrap();
        let ty = &field.ty;

        builder_initial_fields.extend(quote! {
            #name: None,
        });

        if let Some(inner_type) = option_inner_type(ty) {
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
