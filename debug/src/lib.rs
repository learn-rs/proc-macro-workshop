extern crate proc_macro;

use proc_macro::TokenStream;

#[proc_macro_derive(CustomDebug)]
pub fn derive(input: TokenStream) -> TokenStream {
    let _input = syn::parse_macro_input!(input as syn::DeriveInput);
    TokenStream::new()
}
