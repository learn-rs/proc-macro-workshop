extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{self, spanned::Spanned};
use std::process::id;

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let st = syn::parse_macro_input!(input as syn::DeriveInput);
    match do_expand(&st) {
        Ok(token_stream) => token_stream.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn do_expand(st: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    eprintln!("{:#?}", st.data);
    let struct_name_literal = st.ident.to_string();
    let builder_name_literal = format!("{}Builder", struct_name_literal);
    let builder_name_ident = syn::Ident::new(&builder_name_literal, st.span());

    let struct_ident = &st.ident;

    let fields = get_fields_from_derive_input(st)?;
    let builder_struct_fields_def = generate_builder_struct_fields_def(fields)?;
    let builder_struct_factory_init_clauses = generate_builder_factory_init_clauses(fields)?;
    let setters = generate_fields_setter(fields)?;
    let build_method = generate_build_method(fields, &st.ident)?;
    let ret = quote! {
      pub struct #builder_name_ident {
            #builder_struct_fields_def
      }

      impl #struct_ident {
            pub fn builder() -> #builder_name_ident {
                #builder_name_ident {
                    #(#builder_struct_factory_init_clauses),*
                }
            }
        }

      impl #builder_name_ident {
        #setters
      }

      impl #builder_name_ident {
        #build_method
      }
    };
    return Ok(ret);
}

type StructFields = syn::punctuated::Punctuated<syn::Field, syn::Token!(,)>;

fn get_fields_from_derive_input(d: &syn::DeriveInput) -> syn::Result<&StructFields> {
    if let syn::Data::Struct(syn::DataStruct {
                                 fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
                                 ..
                             }) = d.data
    {
        return Ok(named);
    }
    Err(syn::Error::new_spanned(
        d,
        "Must define one a Struct, not Enum".to_string(),
    ))
}

fn generate_builder_struct_fields_def(
    fields: &StructFields,
) -> syn::Result<proc_macro2::TokenStream> {
    let idents: Vec<_> = fields.iter().map(|f| &f.ident).collect();
    let types: Vec<_> = fields.iter().map(|f| {
        if let Some(inner_ty) = get_optional_inner_type(&f.ty) {
            quote!(std::option::Option<#inner_ty>)
        } else {
            let ty = &f.ty;
            quote!(std::option::Option<#ty>)
        }
    }).collect();
    let token_stream = quote! {
        #(#idents: std::option::Option<#types>),*
    };
    Ok(token_stream)
}

fn generate_builder_factory_init_clauses(
    fields: &StructFields,
) -> syn::Result<Vec<proc_macro2::TokenStream>> {
    let init_clauses: Vec<_> = fields
        .iter()
        .map(|f| {
            let ident = &f.ident;
            quote! {
                #ident: std::option::Option::None
            }
        })
        .collect();
    Ok(init_clauses)
}

// fn generate_fields_setter(fields: &StructFields) -> syn::Result<Vec<proc_macro2::TokenStream>> {
//     let setters = fields.iter().map(|f| {
//         let ident = &f.ident;
//         let ty = &f.ty;
//         quote! {
//             fn #ident(&mut self, #ident: #ty) -> &mut Self {
//                 self.#ident = std::option::Option::Some(#ident);
//                 self
//             }
//         }
//     }).collect();
//     Ok(setters)
// }

fn generate_fields_setter(fields: &StructFields) -> syn::Result<proc_macro2::TokenStream> {
    let idents: Vec<_> = fields.iter().map(|f| &f.ident).collect();
    let types: Vec<_> = fields.iter().map(|f| &f.ty).collect();

    let mut stream = proc_macro2::TokenStream::new();
    for (ident, ty) in idents.iter().zip(types.iter()) {
        let token_stream;
        if let Some(inner_ty) = get_optional_inner_type(ty) {
            token_stream = quote! {
                fn #ident(&mut self, #ident: #inner_ty) -> &mut Self {
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
            }
        } else {
            token_stream = quote! {
            fn #ident(&mut self, #ident: #ty) -> &mut Self {
                self.#ident = std::option::Option::Some(#ident);
                self
            }
        };
        }
        stream.extend(token_stream);
    }
    Ok(stream)
}

fn generate_build_method(fields: &StructFields, struct_ident: &syn::Ident) -> syn::Result<proc_macro2::TokenStream> {
    let idents: Vec<_> = fields.iter().map(|f| &f.ident).collect();
    let types: Vec<_> = fields.iter().map(|f| &f.ty).collect();

    let (mut field_streams, mut field_checkers) = (Vec::new(), Vec::new());

    for idx in 0..idents.len() {
        let ident = idents[idx];
        if get_optional_inner_type(&types[idx]).is_none() {
            let checker = quote! {
                if self.#ident.is_none() {
                    let err = format!("{} field missing", stringify!(#ident));
                    return std::result::Result::Err(err.into())
                }
            };
            field_checkers.push(checker);

            let stream = quote! {
                #ident: self.#ident.clone().unwrap(),
            };
            field_streams.push(stream);
        } else {
            let stream = quote! {
                #ident: self.#ident.clone(),
            };
            field_streams.push(stream);
        }
    }
    let token_stream = quote! {
        pub fn build(&mut self) -> std::result::Result<#struct_ident, std::boxed::Box<dyn std::error::Error>> {
            #(#field_checkers)*

            let ident = #struct_ident {
                #(#field_streams)*
            };
            std::result::Result::Ok(ident)
        }
    };
    Ok(token_stream)
}

fn get_optional_inner_type(t: &syn::Type) -> Option<&syn::Type> {
    if let syn::Type::Path(syn::TypePath { path: syn::Path { segments, .. }, .. }) = t {
        if let Some(seg) = segments.last() {
            if seg.ident.to_string() == "Option" {
                if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments { args, .. }) = &seg.arguments {
                    if let Some(syn::GenericArgument::Type(ty)) = args.first() {
                        return Some(ty);
                    }
                }
            }
        }
    }
    None
}