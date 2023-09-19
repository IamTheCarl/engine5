use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{format_ident, quote};
use syn::{
    parse_macro_input, punctuated::Punctuated, Attribute, DeriveInput, Expr, Field, Fields, Ident,
    Lifetime, Token, Type, TypeNever, TypeReference,
};

/// Makes an entity archtype serializable.
/// # Arguments:
/// * `type_id` - a unique integer used to represent this entity in save files.
/// * `marker` - an ECS component used to tag entities as being of this archtype.
/// * `bootstrap_info` - the bootstrap type of this entity. Uses default value for BootstrapEntityInfo if unspecified.
/// * `load_tree` - if present, entity will be provided its own tree namespace within the world database.
#[proc_macro_attribute]
pub fn entity_serialization(attribute_input: TokenStream, input: TokenStream) -> TokenStream {
    let mut type_id = None;
    let mut tag = None;
    let mut bootstrap_info = None;

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

    #[rustfmt::skip]
    let spatial_entity_impl = quote! {
        impl crate::world::spatial_entities::storage::SpatialEntity<(bevy::prelude::Entity, &crate::world::spatial_entities::storage::Storable, #(#query_types),*), #deserialize_struct_name> for #tag {
            const TYPE_ID: crate::world::spatial_entities::storage::EntityTypeId = #type_id;
	    #bootstrap_info
	    
            fn load(data_loader: impl crate::world::spatial_entities::storage::DataLoader, entity_commands: &mut bevy::ecs::system::EntityCommands) -> Result<()> {
		if Self::REQUEST_TREE {
		    let (parameters, tree) =  
			data_loader.load_with_tree::<#deserialize_struct_name>()?;
		    
		    if let Some(tree) = tree {
			Self::construct_entity_with_tree(parameters, entity_commands, tree);
		    } else {
			Self::construct_entity(parameters, entity_commands);
		    }
		} else {
		    let parameters =
			data_loader.load::<#deserialize_struct_name>()?;
		    
		    Self::construct_entity(parameters, entity_commands);
		}

		Ok(())
            }

            fn save((entity, storable, #(#parameter_names),*): (bevy::prelude::Entity, &crate::world::spatial_entities::storage::Storable, #(#query_types_2),*), data_saver: impl crate::world::spatial_entities::storage::DataSaver) {
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

#[proc_macro_attribute]
pub fn ref_serializable(_attribute_input: TokenStream, input: TokenStream) -> TokenStream {
    let original_input = parse_macro_input!(input as DeriveInput);

    let box_struct = process_struct(&original_input, make_into_box, |_| {});

    let mut ref_struct = process_struct(&original_input, |_| {}, make_into_reference);
    ref_struct.ident = format_ident!("{}Ref", original_input.ident);
    ref_struct
        .generics
        .params
        .push(syn::GenericParam::Lifetime(syn::LifetimeParam {
            attrs: vec![],
            lifetime: Lifetime {
                apostrophe: Span::call_site(),
                ident: format_ident!("make_ref_lifetime"),
            },
            colon_token: None,
            bounds: Punctuated::default(),
        }));

    TokenStream::from(quote! {
    #[derive(serde::Serialize, serde::Deserialize)]
    #[allow(dead_code)]
    #box_struct
    #[derive(serde::Serialize)]
    #[allow(dead_code)]
    #ref_struct
    })
}

/// Sometimes when you need to serialize something, it references a very large block of data. Boxing that up and copying it over into the struct is very inefficient.
/// This will derive Serialize and Deserialize for the struct of interest, but will also produce a "referenced" version so that you can serialize it with fewer copy
/// operations.
fn process_struct(
    input: &DeriveInput,
    owned_box: impl Fn(&mut Type) + Copy,
    borrowed_ref: impl Fn(&mut Type) + Copy,
) -> DeriveInput {
    match &input.data {
        syn::Data::Struct(data_struct) => {
            let mut new_struct = data_struct.clone();

            for field in new_struct.fields.iter_mut() {
                modify_field(field, owned_box, borrowed_ref);
            }

            let mut output = input.clone();
            output.data = syn::Data::Struct(new_struct);
            output
        }
        syn::Data::Enum(data_enum) => {
            let mut new_enum = data_enum.clone();

            for variant in new_enum.variants.iter_mut() {
                for field in variant.fields.iter_mut() {
                    modify_field(field, owned_box, borrowed_ref);
                }
            }

            let mut output = input.clone();
            output.data = syn::Data::Enum(new_enum);
            output
        }
        syn::Data::Union(_) => todo!(),
    }
}

fn modify_field(
    field: &mut Field,
    owned_box: impl Fn(&mut Type),
    borrowed_ref: impl Fn(&mut Type),
) {
    let attrs = &mut field.attrs;

    attrs.retain(|attribute| match &attribute.meta {
        syn::Meta::Path(path) => {
            if path.is_ident("owned_box") {
                owned_box(&mut field.ty);
                false
            } else if path.is_ident("borrowed_ref") {
                borrowed_ref(&mut field.ty);
                false
            } else {
                true
            }
        }
        _ => true,
    });
}

fn make_into_reference(ty: &mut Type) {
    // We need a filler to hold the space. We'll ust set it to "never".
    let mut original_ty = Type::Never(TypeNever {
        bang_token: Token![!](Span::call_site()),
    });
    std::mem::swap(&mut original_ty, ty);

    // Now we do the actual conversion.
    *ty = Type::Reference(TypeReference {
        and_token: Token![&](Span::call_site()),
        lifetime: Some(Lifetime {
            apostrophe: Span::call_site(),
            ident: format_ident!("make_ref_lifetime"),
        }),
        mutability: None,
        elem: Box::new(original_ty),
    });
}

fn make_into_box(ty: &mut Type) {
    // We need a filler to hold the space. We'll ust set it to "never".
    let mut original_ty = Type::Never(TypeNever {
        bang_token: Token![!](Span::call_site()),
    });
    std::mem::swap(&mut original_ty, ty);

    *ty = Type::Verbatim(quote! { Box<#original_ty> })
}
