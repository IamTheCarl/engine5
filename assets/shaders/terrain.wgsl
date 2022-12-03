
// @group(1) @binding(0)
// var color_texture_array: texture_2d_array<f32>;
// @group(1) @binding(1)
// var color_texture_array_sampler: sampler;
// @group(1) @binding(2)
// var normal_map_texture_array: texture_2d_array<f32>;
// @group(1) @binding(3)
// var normal_map_array_sampler: sampler;

#import bevy_pbr::pbr_bindings
#import bevy_pbr::mesh_view_bindings
#import bevy_pbr::mesh_bindings
#import bevy_pbr::mesh_functions
#import bevy_pbr::utils
#import bevy_pbr::clustered_forward
#import bevy_pbr::lighting
#import bevy_pbr::shadows
#import bevy_pbr::pbr_functions

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    #import bevy_pbr::mesh_vertex_output
};

fn extract_unsigned(word: u32, width: u32, offset: u32) -> u32 {
    var element = (word >> offset) & (~0u >> (32u - width));
    return element;
}

@vertex
fn vertex(@location(0) vertex: u32) -> VertexOutput {
    var out: VertexOutput;

    var model = mesh.model;

    var NORMAL_LOOKUP_TABLE = array(
        vec3( 0.0,  1.0,  0.0), // Up
        vec3( 0.0, -1.0,  0.0), // Down
        vec3( 0.0,  0.0,  1.0), // North
        vec3( 0.0,  0.0, -1.0), // South
        vec3( 1.0,  0.0,  0.0), // East
        vec3(-1.0,  0.0,  0.0), // West
    );

    var TANGENT_LOOKUP_TABLE = array(
        vec4( 1.0,  0.0,  0.0, -1.0), // Up
        vec4( 1.0,  0.0,  0.0,  1.0), // Down
        vec4( 1.0,  0.0,  0.0,  1.0), // North
        vec4( 1.0,  0.0,  0.0, -1.0), // South
        vec4( 0.0,  1.0,  0.0, -1.0), // East
        vec4( 0.0, -1.0,  0.0,  1.0), // West
    );

    var x = extract_unsigned(vertex, 5u, 0u);
    var y = extract_unsigned(vertex, 5u, 5u);
    var z = extract_unsigned(vertex, 5u, 10u);

    var direction_index = extract_unsigned(vertex, 3u, 15u);

    var u = extract_unsigned(vertex, 1u, 18u);
    var v = extract_unsigned(vertex, 1u, 19u);
    var texture_layer = extract_unsigned(vertex, 9u, 20u);

    out.world_position = mesh_position_local_to_world(model, vec4<f32>(f32(x), f32(y), f32(z), 1.0));
    out.clip_position = mesh_position_world_to_clip(out.world_position);
    out.world_normal = mesh_normal_local_to_world(NORMAL_LOOKUP_TABLE[direction_index]);
    out.world_tangent = mesh_tangent_local_to_world(model, TANGENT_LOOKUP_TABLE[direction_index]);
    out.uv = vec2(f32(u), f32(v)); // 968
    out.texture_layer = i32(texture_layer);

    return out;
}

// fn array_texture_apply_normal_mapping(
//     standard_material_flags: u32,
//     world_normal: vec3<f32>,
// #ifdef VERTEX_TANGENTS
// #ifdef NONSTANDARDMATERIAL_NORMAL_MAP
//     world_tangent: vec4<f32>,
// #endif
// #endif
// #ifdef VERTEX_UVS
//     uv: vec2<f32>,
// #endif
//     texture_layer: u32,
// ) -> vec3<f32> {
//     // NOTE: The mikktspace method of normal mapping explicitly requires that the world normal NOT
//     // be re-normalized in the fragment shader. This is primarily to match the way mikktspace
//     // bakes vertex tangents and normal maps so that this is the exact inverse. Blender, Unity,
//     // Unreal Engine, Godot, and more all use the mikktspace method. Do not change this code
//     // unless you really know what you are doing.
//     // http://www.mikktspace.com/
//     var N: vec3<f32> = world_normal;

// #ifdef VERTEX_TANGENTS
// #ifdef NONSTANDARDMATERIAL_NORMAL_MAP
//     // NOTE: The mikktspace method of normal mapping explicitly requires that these NOT be
//     // normalized nor any Gram-Schmidt applied to ensure the vertex normal is orthogonal to the
//     // vertex tangent! Do not change this code unless you really know what you are doing.
//     // http://www.mikktspace.com/
//     var T: vec3<f32> = world_tangent.xyz;
//     var B: vec3<f32> = world_tangent.w * cross(N, T);
// #endif
// #endif

// #ifdef VERTEX_TANGENTS
// #ifdef VERTEX_UVS
// #ifdef NONSTANDARDMATERIAL_NORMAL_MAP
//     // Nt is the tangent-space normal.
//     var Nt = textureSample(normal_map_texture_array, normal_map_array_sampler, uv, i32(texture_layer)).rgb;
//     if ((standard_material_flags & STANDARD_MATERIAL_FLAGS_TWO_COMPONENT_NORMAL_MAP) != 0u) {
//         // Only use the xy components and derive z for 2-component normal maps.
//         Nt = vec3<f32>(Nt.rg * 2.0 - 1.0, 0.0);
//         Nt.z = sqrt(1.0 - Nt.x * Nt.x - Nt.y * Nt.y);
//     } else {
//         Nt = Nt * 2.0 - 1.0;
//     }
//     // Normal maps authored for DirectX require flipping the y component
//     if ((standard_material_flags & STANDARD_MATERIAL_FLAGS_FLIP_NORMAL_MAP_Y) != 0u) {
//         Nt.y = -Nt.y;
//     }
//     // NOTE: The mikktspace method of normal mapping applies maps the tangent-space normal from
//     // the normal map texture in this way to be an EXACT inverse of how the normal map baker
//     // calculates the normal maps so there is no error introduced. Do not change this code
//     // unless you really know what you are doing.
//     // http://www.mikktspace.com/
//     N = Nt.x * T + Nt.y * B + Nt.z * N;
// #endif
// #endif
// #endif

//     return normalize(N);
// }

struct FragmentInput {
    @builtin(front_facing) is_front: bool,
    @builtin(position) frag_coord: vec4<f32>,
    // @location(6) texture_layer: u32,
    #import bevy_pbr::mesh_vertex_output
};

@fragment
fn fragment(in: FragmentInput) -> @location(0) vec4<f32> {
    
    var pbr_input: PbrInput = pbr_input_new();

    // pbr_input.material.base_color = vec4<f32>(1.0, 0.0, 0.0, 1.0);
    pbr_input.material.base_color = textureSample(base_color_texture, base_color_sampler, in.uv, in.texture_layer);
    // pbr_input.material.base_color = textureSample(normal_map_texture_array, normal_texture_sampler, in.uv, i32(in.layer));

    pbr_input.frag_coord = in.frag_coord;
    pbr_input.world_position = in.world_position;
    pbr_input.world_normal = prepare_world_normal(
        in.world_normal,
        (pbr_input.material.flags & STANDARD_MATERIAL_FLAGS_DOUBLE_SIDED_BIT) != 0u,
        in.is_front,
    );

    pbr_input.is_orthographic = view.projection[3].w == 1.0;

    pbr_input.N = apply_normal_mapping(
        pbr_input.material.flags,
        pbr_input.world_normal,
        in.world_tangent,
        in.uv,
        in.texture_layer,
    );
    pbr_input.V = calculate_view(in.world_position, pbr_input.is_orthographic);

    // /* set other PbrInput fields here */
    return tone_mapping(pbr(pbr_input));
}
