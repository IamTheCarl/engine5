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
    let input_ident = input.ident.clone();

    // Clone input before we move input.data
    let input_data = input.data.clone();
    let input_fields = match input_data {
        syn::Data::Struct(ref data) => match &data.fields {
            Fields::Named(fields) => fields.named.clone(),
            _ => panic!("Struct must have named fields."),
        },
        _ => panic!("Entity serialization can only be done with a strut."),
    };

    // First, parse field configs from original fields (before stripping)
   let (fields, field_configs): (Punctuated<Field, Token![,]>, Vec<FieldConfig>) = {
        let mut cloned = input_fields.clone();
        let configs: Vec<FieldConfig> = input_fields.iter().map(|f| parse_field_attributes(&f.attrs)).collect();
        for field in cloned.iter_mut() {
            field.attrs.retain(|attr| {
                !attr.path().is_ident("query") && !attr.path().is_ident("get")
            });
        }
        (cloned, configs)
    };

    let deserialize_struct_name = input_ident.clone();

    let serialize_struct_name = Ident::new(
        &format!("{}Serialize", input_ident.to_string()),
        Span::call_site(),
    );

    // We need to convert these into references, unless we're supposed to own them.
    let _field_iter = fields.iter().map(|field| {
        let ident = &field.ident;
        let ty = &field.ty;

        quote! {
            #ident: &'a #ty
        }
    });

   // Collect all iterator-based fields into owned Vecs to avoid borrow conflicts
    let serialize_field_tokens: Vec<_> = fields.iter().map(|field| {
        let ident = &field.ident;
        let ty = &field.ty;
        quote! { #ident: &'a #ty }
    }).collect();

    #[rustfmt::skip]
    let serialize_struct = quote! {
        #[derive(serde::Serialize)]
        struct #serialize_struct_name <'a> {
            #(#serialize_field_tokens),*
        }
    };

    let parameter_names: Vec<_> = fields.iter().map(|field| &field.ident).collect();

    // Allow custom attributes #[query] and #[get] that are only used by this proc macro
    let allow_unused_attrs: proc_macro2::TokenStream = quote! {
        #[allow(unused_attributes)]
    };

    // Fields for update - the types that update_data accepts
    let update_param_types: Vec<_> = fields.iter().map(|field| {
        let ty = &field.ty;
        quote! { #ty }
    }).collect();

    let _update_param_names = fields.iter().map(|field| &field.ident).collect::<Vec<_>>();

    let _update_query_types: Vec<_> = fields.iter().map(|field| {
        let config = parse_field_attributes(&field.attrs);
        if let Some(query_type) = config.query_type {
            quote! { &#query_type }
        } else {
            let ty = &field.ty;
            quote! { &#ty }
        }
    }).collect();

    let _update_param_refs: Vec<_> = fields.iter().map(|field| {
        let param_name = &field.ident;
        quote! { &#param_name }
    }).collect();

    let update_actions: Vec<_> = fields.iter().map(|field| {
        let config = parse_field_attributes(&field.attrs);
        let parameter_name = &field.ident;
        if let Some(get) = config.get {
            quote! { #parameter_name.#get = parameters.#parameter_name; }
        } else {
            quote! { *#parameter_name = parameters.#parameter_name; }
        }
    }).collect();

    let query_param_names: Vec<_> = fields.iter()
        .filter_map(|field| field.ident.as_ref())
        .collect();

    // Query destructuring includes entity, storable, then component fields
    let mut query_destructure_names: Vec<proc_macro2::TokenStream> =
        vec![quote! { entity }, quote! { storable }];
    for name in &query_param_names {
        query_destructure_names.push(quote! { #name });
    }

    let query_param_types_with_ref: Vec<_> = fields.iter()
        .zip(field_configs.iter())
        .map(|(field, config)| {
            if let Some(query_type) = &config.query_type {
                quote! { &#query_type }
            } else {
                let ty = &field.ty;
                quote! { &#ty }
            }
        })
        .collect();

    let _query_param_refs: Vec<_> = fields.iter().map(|field| {
        let param_name = &field.ident;
        quote! { &#param_name }
    }).collect();

    let serialize_param_access: Vec<_> = fields.iter()
        .zip(field_configs.iter())
        .map(|(field, config)| {
            let parameter_name = &field.ident;
            if let Some(get) = &config.get {
                quote! { &#parameter_name.#get }
            } else {
                quote! { #parameter_name }
            }
        })
        .collect();

    // Owned Vec for serialize_call_args to avoid borrow issues
    let _serialize_call_args: Vec<_> = fields.iter().map(|field| {
        let param_name = &field.ident;
        quote! { #param_name }
    }).collect();

    let serialize_fields_fn = quote! {
        impl #tag {
            pub fn serialize_fields_for_save(
                #(#query_param_names: #query_param_types_with_ref),*
            ) -> Vec<u8> {
                let serialized_data = #serialize_struct_name {
                    #(#query_param_names: #serialize_param_access),*
                };
                let mut bytes = Vec::<u8>::new();
                ciborium::into_writer(&serialized_data, &mut bytes).unwrap();
                bytes
            }
        }
    };

    let update_fields_fn = quote! {
        pub fn update_fields(
            data_loader: impl crate::world::spatial_entities::storage::DataLoader,
            #(#parameter_names: &mut #update_param_types),*
        ) -> Result<()> {
            let parameters =
                data_loader.load::<#deserialize_struct_name>()?;
            #(#update_actions)*
            Ok(())
        }
    };

    // Generate query tuple types for the specialized save system (always references)
    let save_query_field_types: Vec<syn::Type> = fields
        .iter()
        .zip(field_configs.iter())
        .map(|(field, config)| {
            if let Some(query_type) = &config.query_type {
                syn::parse_quote!(&#query_type)
            } else {
                let ty: &syn::Type = &field.ty;
                let ty_str = quote!(&#ty).to_string();
                syn::parse_str(&ty_str).expect("Failed to parse field type")
            }
        })
        .collect();

    // Build query types: Entity, &Storable, then each component type
    let mut query_types = Vec::new();
    query_types.extend([
        syn::parse_quote!(bevy::prelude::Entity),
        syn::parse_quote!(&crate::world::spatial_entities::storage::Storable),
    ]);
    for qt in save_query_field_types {
        query_types.push(qt);
    }

    // Build query parameter names
    let mut query_param_names_list = Vec::new();
    query_param_names_list.extend([
        quote! { entity },
        quote! { storable },
    ]);
    query_param_names_list.extend(query_param_names.iter().map(|n| quote! { #n }));

    // Build serialize_fields call arguments
    let serialize_call_args = fields.iter().map(|field| {
        let param_name = &field.ident;
        quote! { #param_name }
    });

    let specialized_save_system = quote! {
        pub fn save_system_for_entity_type(
            mut commands: bevy::prelude::Commands,
            storage: bevy::prelude::Res<crate::world::spatial_entities::storage::EntityStorage>,
            query: bevy::prelude::Query<
                (#(#query_types),*),
                (bevy::ecs::query::With<#tag>, bevy::ecs::query::With<crate::world::spatial_entities::storage::ToSaveSpatial>)
            >,
        ) {
            for (#(#query_destructure_names),*) in query.iter() {
                let serialized_payload = #tag::serialize_fields_for_save(#(#serialize_call_args),*);
                let mut bytes: Vec<u8> = Vec::new();
                bytes.extend(<#tag as crate::world::spatial_entities::storage::SpatialEntity>::TYPE_ID.to_le_bytes().iter());
                bytes.extend(&serialized_payload);

                let tracer_id = storable.id();
                if let Err(error) = storage
                    .spatial_entities_storage
                    .insert(tracer_id.to_be_bytes(), bytes)
                {
                    log::error!(
                        "Failed to write entity of type {} to database: {:?}",
                        <#tag as crate::world::spatial_entities::storage::SpatialEntity>::TYPE_ID,
                        error
                    );
                }

                commands.entity(entity).remove::<crate::world::spatial_entities::storage::ToSaveSpatial>();
            }
        }

        pub fn register_save_system(app: &mut bevy::app::App) {
            app.add_systems(
                bevy::app::Update,
                save_system_for_entity_type,
            );
        }
    };

    #[rustfmt::skip]
    let spatial_entity_impl = quote! {
        impl crate::world::spatial_entities::storage::SpatialEntity for #tag {
            const TYPE_ID: crate::world::spatial_entities::storage::EntityTypeId = #type_id;
            type Parameters = #deserialize_struct_name;
            #bootstrap_info
            
            fn load(data_loader: impl crate::world::spatial_entities::storage::DataLoader, entity_commands: &mut bevy::ecs::system::EntityCommands) -> Result<()> {
                if <Self as crate::world::spatial_entities::storage::EntityConstruction<Self::Parameters>>::REQUEST_TREE {
                    let (parameters, tree) =  
                        data_loader.load_with_tree::<#deserialize_struct_name>()?;
                    
                    if let Some(tree) = tree {
                        <Self as crate::world::spatial_entities::storage::EntityConstruction<Self::Parameters>>::construct_entity_with_tree(parameters, entity_commands, tree);
                    } else {
                        <Self as crate::world::spatial_entities::storage::EntityConstruction<Self::Parameters>>::construct_entity(parameters, entity_commands);
                    }
                } else {
                    let parameters =
                        data_loader.load::<#deserialize_struct_name>()?;
                    
                    <Self as crate::world::spatial_entities::storage::EntityConstruction<Self::Parameters>>::construct_entity(parameters, entity_commands);
                }

                Ok(())
            }
            
            fn update(data_loader: impl crate::world::spatial_entities::storage::DataLoader, commands: &mut bevy::ecs::system::EntityCommands) -> Result<()> {
                let _ = (data_loader, commands);
                Ok(())
            }
            
            fn save(
                entity: bevy::prelude::Entity,
                storable: &crate::world::spatial_entities::storage::Storable,
                data_saver: impl crate::world::spatial_entities::storage::DataSaver,
                serialize_data: impl FnOnce() -> Vec<u8>,
            ) {
                let bytes = serialize_data();
                data_saver.save(entity, storable, &bytes);
            }

            fn save_data(
                entity: bevy::prelude::Entity,
                storable: &crate::world::spatial_entities::storage::Storable,
                serialize_data: impl FnOnce() -> Vec<u8>,
            ) -> Vec<u8> {
                serialize_data()
            }
        }
    };

    // Generate struct definition from field names and types - pass through the struct
    let struct_fields: Vec<_> = fields.iter().map(|field| {
        let ident = &field.ident;
        let ty = &field.ty;
        quote! { #ident: #ty }
    }).collect();
    let passthrough_struct: proc_macro2::TokenStream = quote! {
        #[allow(unused_attributes)]
        #[derive(serde::Deserialize)]
        pub struct #input_ident {
            #(#struct_fields),*
        }
    };

    #[rustfmt::skip]
    let result = TokenStream::from(quote! {
        #allow_unused_attrs
        #passthrough_struct
        #spatial_entity_impl
        #serialize_fields_fn
        #update_fields_fn
        #serialize_struct
        #specialized_save_system
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
    // We need a filler to hold the space. We'll just set it to "never".
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
    // We need a filler to hold the space. We'll just set it to "never".
    let mut original_ty = Type::Never(TypeNever {
        bang_token: Token![!](Span::call_site()),
    });
    std::mem::swap(&mut original_ty, ty);

    *ty = Type::Verbatim(quote! { Box<#original_ty> })
}
