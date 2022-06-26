#![crate_type = "proc-macro"]
#![recursion_limit = "192"]

extern crate proc_macro;
extern crate proc_macro2;
#[macro_use]
extern crate quote;
extern crate syn;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use syn::Token;

macro_rules! my_quote {
    ($($t:tt)*) => (quote_spanned!(proc_macro2::Span::call_site() => $($t)*))
}

#[proc_macro_derive(constructor, attributes(custom_resolve, resolve, resolve_collection, resolve_by_component, ioc_context))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast: syn::DeriveInput = syn::parse(input).expect("Couldn't parse item");
    let result = match ast.data {
        syn::Data::Enum(ref e) => panic!("doesn't work with enum yet"),
        syn::Data::Struct(ref s) => new_for_struct(&ast, &s.fields),
        syn::Data::Union(_) => panic!("doesn't work with unions yet"),
    };
    result.into()
}

fn new_for_struct(
    ast: &syn::DeriveInput,
    fields: &syn::Fields,
) -> proc_macro2::TokenStream {
    match *fields {
        syn::Fields::Named(ref fields) => new_impl(&ast, Some(&fields.named)),
        syn::Fields::Unit => panic!("doesn't work with unit yet"),
        syn::Fields::Unnamed(_) => panic!("doesn't work with unnamed yet"),
    }
}

fn new_impl(
    ast: &syn::DeriveInput,
    fields: Option<&syn::punctuated::Punctuated<syn::Field, Token![,]>>,
) -> proc_macro2::TokenStream {
    let name = &ast.ident;
    let empty = Default::default();
    let fields: Vec<_> = fields
        .unwrap_or(&empty)
        .iter()
        .enumerate()
        .map(|(i, f)| FieldExt::new(f, i))
        .collect();

    let assigns = fields.iter().filter(|a| !a.is_ioc_context()).map(|f| f.as_assign());
    let assigns = my_quote![#(#assigns);*]; // ;

    let inits = fields.iter().map(|f| f.as_init());
    let inits = my_quote![#(#inits),*];

    let ioc_context_init = fields.iter().filter(|a| a.is_ioc_context()).map(|f| f.as_assign());
    let ioc_context_init = my_quote![#(#ioc_context_init)*];

    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();

    #[cfg(not(feature = "async-mode"))]
    my_quote! {
        impl #impl_generics anthill_di::Constructor for #name #ty_generics #where_clause {
            fn ctor(ctx: anthill_di::DependencyContext) -> anthill_di::types::BuildDependencyResult<Self> {
                let ctx = ctx;
                    
                #assigns;
                #ioc_context_init;

                Ok(#name {#inits} )
            }
        }
    }

    #[cfg(feature = "async-mode")]
    my_quote! {
        impl #impl_generics anthill_di::Constructor for #name #ty_generics #where_clause {
            fn ctor<'async_trait>(ctx: anthill_di::DependencyContext) -> std::pin::Pin<Box<dyn std::future::Future<Output = anthill_di::types::BuildDependencyResult<Self>> + core::marker::Send + core::marker::Sync + 'async_trait>> where Self: 'async_trait {
                Box::pin(async move {
                    let ctx = ctx;
                    
                    #assigns;
                    #ioc_context_init;

                    Ok(#name {#inits} )
                })
            }
        }
    }
}

struct FieldExt<'a> {
    ty: &'a syn::Type,
    attr: Option<FieldAttr>,
    ident: syn::Ident,
}

impl<'a> std::fmt::Debug for FieldExt<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FieldExt").field("attr", &self.attr).field("ident", &self.ident).finish()
    }
}

impl<'a> FieldExt<'a> {
    pub fn new(field: &'a syn::Field, idx: usize) -> FieldExt<'a> {
        FieldExt {
            ty: &field.ty,
            attr: FieldAttr::parse(&field.attrs),
            ident: field.ident.clone().unwrap(),
        }
    }

    pub fn is_phantom_data(&self) -> bool {
        match *self.ty {
            syn::Type::Path(syn::TypePath {
                qself: None,
                ref path,
            }) => path
                .segments
                .last()
                .map(|x| x.ident == "PhantomData")
                .unwrap_or(false),
            _ => false,
        }
    }

    pub fn is_ioc_context(&self) -> bool {
        if let Some (attr) = &self.attr {
            if let FieldAttr::IocContext = attr {
                return true;
            }
        }

        false
    }

    pub fn as_assign(&self) -> proc_macro2::TokenStream {
        let f_name = &self.ident;

        let init = if self.is_phantom_data() {
            my_quote!(::std::marker::PhantomData)
        } else {
            match self.attr {
                #[cfg(not(feature = "async-mode"))]
                None => my_quote!(ctx.resolve()?),
                #[cfg(feature = "async-mode")]
                None => my_quote!(ctx.resolve().await?),
                Some(ref attr) => attr.as_tokens(),
            }
        };

        my_quote!(let #f_name = #init)
    }

    pub fn as_init(&self) -> proc_macro2::TokenStream {
        let f_name = &self.ident;
        my_quote!(#f_name)
    }
}

#[derive(Debug)]
enum FieldAttr {
    IocContext,
    Resolve,
    ResolveCollection,
    ResolveByComponent(proc_macro2::Ident),
    Value(proc_macro2::TokenStream),
}

impl FieldAttr {
    pub fn as_tokens(&self) -> proc_macro2::TokenStream {
        match *self {
            FieldAttr::IocContext => my_quote!(ctx),
            #[cfg(not(feature = "async-mode"))]
            FieldAttr::Resolve => my_quote!(ctx.resolve()?),
            #[cfg(feature = "async-mode")]
            FieldAttr::Resolve => my_quote!(ctx.resolve().await?),
            #[cfg(not(feature = "async-mode"))]
            FieldAttr::ResolveCollection => my_quote!(ctx.resolve_collection()?),
            #[cfg(feature = "async-mode")]
            FieldAttr::ResolveCollection => my_quote!(ctx.resolve_collection().await?),
            #[cfg(not(feature = "async-mode"))]
            FieldAttr::ResolveByComponent(ref s) => my_quote!(ctx.resolve_by_type_id(std::any::TypeId::of::<#s>())?),
            #[cfg(feature = "async-mode")]
            FieldAttr::ResolveByComponent(ref s) => my_quote!(ctx.resolve_by_type_id(std::any::TypeId::of::<#s>()).await?),
            FieldAttr::Value(ref s) => my_quote!(#s),
        }
    }

    pub fn parse(attrs: &[syn::Attribute]) -> Option<FieldAttr> {
        use syn::{AttrStyle, Meta, NestedMeta};

        //let mut result = None;
        for attr in attrs.iter() {
            match attr.style {
                AttrStyle::Outer => {}
                _ => continue,
            }
            let last_attr_path = attr
                .path
                .segments
                .iter()
                .last()
                .expect("Expected at least one segment where #[segment[::segment*](..)]");

            if (*last_attr_path).ident != "ioc_context" &&
                (*last_attr_path).ident != "resolve" &&
                (*last_attr_path).ident != "resolve_collection" &&
                (*last_attr_path).ident != "resolve_by_component" &&
                (*last_attr_path).ident != "custom_resolve" {
                continue;
            }
            let meta = match attr.parse_meta() {
                Ok(meta) => meta,
                Err(_) => continue,
            };

            if meta.path().is_ident("ioc_context") {
                match (meta) {
                    Meta::Path(_) => {
                        return Some(FieldAttr::IocContext)    
                    }
                    _ => panic!("Invalid #[ioc_context] attribute: #[ioc_context{}]", path_to_string(&meta.path())),
                }
            }
 
            if meta.path().is_ident("resolve") {
                //if ()
                match (meta) {
                    Meta::Path(_) => {
                        return Some(FieldAttr::Resolve)    
                    }
                    _ => panic!("Invalid #[resolve] attribute: #[resolve{}]", path_to_string(&meta.path())),
                }
            }

            if meta.path().is_ident("resolve_collection") {
                match (meta) {
                    Meta::Path(_) => {
                        return Some(FieldAttr::ResolveCollection)    
                    }
                    _ => panic!("Invalid #[resolve_collection] attribute: #[{}]", path_to_string(&meta.path())),
                }
            }

            if meta.path().is_ident("resolve_by_component") {
                match (meta) {
                    Meta::List(list) => {//
                        match list.nested.iter().nth(0).expect("resolve_by_component attribute required 1 element") {
                            NestedMeta::Meta(Meta::Path(ref path)) => {
                                let ident = path.segments.iter().nth(0).unwrap().clone().ident;
                                //let r = path.get_ident();
                                return Some(FieldAttr::ResolveByComponent(ident));
                            },
                            _ => panic!("Invalid #[resolve_by_component] attribute")
                        }
                        
                    }
                    _ => panic!("Invalid #[resolve_by_component] attribute: #[resolve_by_component({})]", path_to_string(&meta.path())),
                }
            }

            if meta.path().is_ident("custom_resolve") {
                match (meta) {
                    Meta::List(list) => {
                        match list.nested.iter().nth(0).expect("custom_resolve attribute required 1 element") {
                            NestedMeta::Meta(Meta::NameValue(ref kv)) => {
                                if let syn::Lit::Str(ref s) = kv.lit {
                                    if kv.path.is_ident("value") {
                                        let tokens = lit_str_to_token_stream(s).ok().expect(&format!(
                                            "Invalid expression in #[custom_resolve]: `{}`",
                                            s.value()
                                        ));
                                        return Some(FieldAttr::Value(tokens));
                                    } else {
                                        panic!("Invalid #[custom_resolve] attribute: #[custom_resolve({} = ..)]", path_to_string(&kv.path));
                                    }
                                } else {
                                    panic!("Non-string literal value in #[custom_resolve] attribute");
                                }
                            },
                            _ => panic!("Non-string literal value in #[custom_resolve] attribute"),
                        }
                    }
                    _ => panic!("Invalid #[custom_resolve] attribute: #[custom_resolve({})]", path_to_string(&meta.path())),
                }
            }
        }
        return None;
    }
}

fn path_to_string(path: &syn::Path) -> String {
    path.segments.iter().map(|s| s.ident.to_string()).collect::<Vec<String>>().join("::")
}

fn lit_str_to_token_stream(s: &syn::LitStr) -> Result<TokenStream2, proc_macro2::LexError> {
    let code = s.value();
    let ts: TokenStream2 = code.parse()?;
    Ok(set_ts_span_recursive(ts, &s.span()))
}

fn set_ts_span_recursive(ts: TokenStream2, span: &proc_macro2::Span) -> TokenStream2 {
    ts.into_iter().map(|mut tt| {
        tt.set_span(span.clone());
        if let proc_macro2::TokenTree::Group(group) = &mut tt {
            let stream = set_ts_span_recursive(group.stream(), span);
            *group = proc_macro2::Group::new(group.delimiter(), stream);
        }
        tt
    }).collect()
}