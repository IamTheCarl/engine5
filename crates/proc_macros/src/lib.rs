use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{parse_macro_input, Attribute, DeriveInput, Expr, Fields, Ident};

#[proc_macro_attribute]
pub fn entity_serialization(attribute_input: TokenStream, input: TokenStream) -> TokenStream {
    let mut type_id = None;
    let mut tag = None;
    let mut bootstrap_info = None;
    let mut load_tree = false;

    let arg_parser = syn::meta::parser(|meta| {
        if meta.path.is_ident("type_id") {
            type_id = Some(meta.value()?.parse::<syn::LitInt>()?);
            Ok(())
        } else if meta.path.is_ident("marker") {
            tag = Some(meta.value()?.parse::<syn::Ident>()?);
            Ok(())
        } else if meta.path.is_ident("bootstrap") {
            bootstrap_info = Some(meta.value()?.parse::<Expr>()?);
            Ok(())
        } else if meta.path.is_ident("load_tree") {
            load_tree = true;
            Ok(())
        } else {
            Err(meta.error("Unsupported property."))
        }
    });

    parse_macro_input!(attribute_input with arg_parser);

    let type_id = type_id.expect("No type ID given.");
    let tag = tag.expect("No tag component given.");

    let bootstrap_info = if let Some(bootstrap_info) = bootstrap_info {
        quote! { const BOOTSTRAP: crate::world::spatial_entities::storage::BootstrapEntityInfo = crate::world::spatial_entities::storage::#bootstrap_info; }
    } else {
        quote! {}
    };

    let input = parse_macro_input!(input as DeriveInput);

    let fields = match input.data {
        syn::Data::Struct(data) => match data.fields {
            Fields::Named(fields) => fields.named,
            _ => panic!("Struct must have named fields."),
        },
        _ => {
            panic!("Entity serialization can only be done with a strut.");
        }
    };

    let deserialize_struct_name = &input.ident;

    let field_iter = fields.iter().map(|field| {
        let ident = &field.ident;
        let ty = &field.ty;

        quote! {
            #ident: #ty
        }
    });
    let deserialize_struct = quote! {
        #[derive(serde::Deserialize)]
        struct #deserialize_struct_name {
            #(#field_iter),*
        }
    };

    let serialize_struct_name = Ident::new(
        &format!("{}Serialize", input.ident.to_string()),
        Span::call_site(),
    );

    // We need to convert these into references, unless we're supposed to own them.
    let field_iter = fields.iter().map(|field| {
        let ident = &field.ident;
        let ty = &field.ty;

        quote! {
            #ident: &'a #ty
        }
    });

    #[rustfmt::skip]
    let serialize_struct = quote! {
        #[derive(serde::Serialize)]
        struct #serialize_struct_name <'a> {
            #(#field_iter),*
        }
    };

    let parameter_names = fields.iter().map(|field| &field.ident);
    let parameter_access = fields.iter().map(|field| {
        let config = parse_field_attributes(&field.attrs);

        let parameter_name = &field.ident;

        if let Some(get) = config.get {
            quote! { #parameter_name: &#parameter_name.#get }
        } else {
            quote! { #parameter_name }
        }
    });
    let query_types = fields.iter().map(|field| {
        let config = parse_field_attributes(&field.attrs);
        if let Some(query_type) = config.query_type {
            quote! { &#query_type }
        } else {
            let ty = &field.ty;
            quote! { &#ty }
        }
    });
    let query_types_2 = query_types.clone();

    let load_func = if load_tree {
        quote! {
             let (storable, parameters, tree) =
                 data_loader.load_with_tree::<#deserialize_struct_name>()?;
             Self::spawn_internal(
                 parent,
                 commands,
                 storable,
                 TerrainStorage::Local { tree },
                 parameters,
             );
        }
    } else {
        quote! {
            let (storable, parameters) = data_loader.load::<#deserialize_struct_name>()?;
            Self::spawn_internal(parent, commands, storable, parameters);
        }
    };

    #[rustfmt::skip]
    let spatial_entity_impl = quote! {
        impl crate::world::spatial_entities::storage::SpatialEntity<(bevy::prelude::Entity, &crate::world::spatial_entities::storage::Storable, #(#query_types),*)> for #tag {
            const TYPE_ID: crate::world::spatial_entities::storage::EntityTypeId = #type_id;
	    #bootstrap_info
	    
            fn load(data_loader: crate::world::spatial_entities::storage::DataLoader, parent: bevy::prelude::Entity, commands: &mut bevy::prelude::Commands) -> Result<()> {
		#load_func
		Ok(())
            }

            fn save((entity, storable, #(#parameter_names),*): (bevy::prelude::Entity, &crate::world::spatial_entities::storage::Storable, #(#query_types_2),*), data_saver: crate::world::spatial_entities::storage::DataSaver) {
		#serialize_struct

		data_saver.save(
                    entity,
                    storable,
                    &#serialize_struct_name { #(#parameter_access),* },
		);
            }
	}
    };

    #[rustfmt::skip]
    let result = TokenStream::from(quote! {
	#deserialize_struct
	#spatial_entity_impl
    });

    result
}

#[derive(Default)]
struct FieldConfig {
    query_type: Option<Expr>,
    get: Option<Expr>,
}

fn parse_field_attributes(attributes: &Vec<Attribute>) -> FieldConfig {
    let mut field_config = FieldConfig::default();

    for attribute in attributes {
        match &attribute.meta {
            syn::Meta::Path(_) => panic!("Paths are not a valid input for this macro."),
            syn::Meta::List(_) => panic!("Lists are not a valid input for this macro."),
            syn::Meta::NameValue(name_value) => {
                if name_value.path.is_ident("query") {
                    field_config.query_type = Some(name_value.value.clone());
                } else if name_value.path.is_ident("get") {
                    field_config.get = Some(name_value.value.clone());
                } else {
                    panic!("Unknown field attribute.");
                }
            }
        }
    }

    field_config
}
