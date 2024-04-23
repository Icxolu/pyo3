use crate::attributes::{get_pyo3_options, CrateAttribute};
use crate::utils::Ctx;
use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote};
use syn::{
    parse::{Parse, ParseStream},
    parse_quote,
    spanned::Spanned,
    Attribute, DataEnum, DeriveInput, Fields, Ident, Result, Token,
};

/// Attributes for deriving CloneRef scoped on containers.
enum ContainerPyO3Attribute {
    /// Change the path for the pyo3 crate
    Crate(CrateAttribute),
}

impl Parse for ContainerPyO3Attribute {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Token![crate]) {
            input.parse().map(ContainerPyO3Attribute::Crate)
        } else {
            Err(lookahead.error())
        }
    }
}

#[derive(Default)]
struct ContainerOptions {
    /// Change the path for the pyo3 crate
    krate: Option<CrateAttribute>,
}

impl ContainerOptions {
    fn from_attrs(attrs: &[Attribute]) -> Result<Self> {
        let mut options = ContainerOptions::default();

        for attr in attrs {
            if let Some(pyo3_attrs) = get_pyo3_options(attr)? {
                for pyo3_attr in pyo3_attrs {
                    match pyo3_attr {
                        ContainerPyO3Attribute::Crate(path) => {
                            ensure_spanned!(
                                options.krate.is_none(),
                                path.span() => "`crate` may only be provided once"
                            );
                            options.krate = Some(path);
                        }
                    }
                }
            }
        }
        Ok(options)
    }
}

struct StructField<'a> {
    ident: Option<&'a syn::Ident>,
}

/// Data container
///
/// Either describes a struct or an enum variant.
struct Container<'a> {
    path: syn::Path,
    fields: Vec<StructField<'a>>,
}

impl<'a> Container<'a> {
    /// Construct a container based on fields, identifier and attributes.
    fn new(fields: &'a Fields, path: syn::Path, _options: ContainerOptions) -> Self {
        let fields = match fields {
            Fields::Unnamed(unnamed) => unnamed
                .unnamed
                .iter()
                .map(|_field| StructField { ident: None })
                .collect::<Vec<_>>(),
            Fields::Named(named) => named
                .named
                .iter()
                .map(|field| {
                    let ident = field
                        .ident
                        .as_ref()
                        .expect("Named fields should have identifiers");

                    StructField { ident: Some(ident) }
                })
                .collect::<Vec<_>>(),

            Fields::Unit => Vec::new(),
        };

        Container { path, fields }
    }

    /// Build derivation body for a struct
    fn build(&self, _ctx: &Ctx) -> TokenStream {
        let f_idents = (0..self.fields.len())
            .map(|i| format_ident!("arg{i}"))
            .collect::<Vec<_>>();

        let idents = self
            .fields
            .iter()
            .enumerate()
            .map(|(i, field)| {
                if let Some(ident) = field.ident {
                    quote!(#ident)
                } else {
                    let i = Literal::usize_unsuffixed(i);
                    quote!(#i)
                }
            })
            .collect::<Vec<_>>();

        let path = &self.path;
        quote! { #path { #(#idents: #f_idents),* } => #path { #(#idents: #f_idents.clone_ref(py)),* }, }
    }
}

/// Describes derivation input of an enum.
struct Enum<'a> {
    variants: Vec<Container<'a>>,
}

impl<'a> Enum<'a> {
    /// Construct a new enum representation.
    ///
    /// `data_enum` is the `syn` representation of the input enum, `ident` is the
    /// `Identifier` of the enum.
    fn new(data_enum: &'a DataEnum, ident: &'a Ident) -> Result<Self> {
        let variants = data_enum
            .variants
            .iter()
            .map(|variant| {
                let attrs = ContainerOptions::from_attrs(&variant.attrs)?;
                let var_ident = &variant.ident;
                Ok(Container::new(
                    &variant.fields,
                    parse_quote!(#ident::#var_ident),
                    attrs,
                ))
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(Enum { variants })
    }

    /// Build derivation body for enums.
    fn build(&self, ctx: &Ctx) -> TokenStream {
        let clone_ref = self
            .variants
            .iter()
            .map(|variant| variant.build(ctx))
            .collect::<TokenStream>();

        // Handle empty enum
        let star = if clone_ref.is_empty() {
            quote!(*)
        } else {
            quote!()
        };

        quote!(match #star self {
            #clone_ref
        })
    }
}

/// Derive CloneRef for enums and structs.
pub fn build_derive_cloneref(tokens: &DeriveInput) -> Result<TokenStream> {
    let options = ContainerOptions::from_attrs(&tokens.attrs)?;
    let ctx = &Ctx::new(&options.krate);
    let Ctx { pyo3_path } = &ctx;

    let mut trait_generics = tokens.generics.clone();
    let generics = &tokens.generics;
    trait_generics.params.push(parse_quote!('py));

    let mut where_clause: syn::WhereClause = parse_quote!(where);
    for param in trait_generics.type_params() {
        let gen_ident = &param.ident;
        where_clause
            .predicates
            .push(parse_quote!(#gen_ident: #pyo3_path::conversion::CloneRef<'py>))
    }

    let derives = match &tokens.data {
        syn::Data::Enum(en) => {
            let en = Enum::new(en, &tokens.ident)?;
            en.build(ctx)
        }
        syn::Data::Struct(st) => {
            let ident = &tokens.ident;
            let st = Container::new(&st.fields, parse_quote!(#ident), options);
            let tokens = st.build(ctx);
            quote! {
                match self {
                    #tokens
                }
            }
        }
        syn::Data::Union(_) => bail_spanned!(
            tokens.span() => "#[derive(CloneRef)] is not supported for unions"
        ),
    };

    let ident = &tokens.ident;
    Ok(quote!(
        #[automatically_derived]
        impl #trait_generics #pyo3_path::conversion::CloneRef<'py> for #ident #generics #where_clause {
            fn clone_ref(&self, py: #pyo3_path::Python<'py>) -> Self {
                #derives
            }
        }
    ))
}
