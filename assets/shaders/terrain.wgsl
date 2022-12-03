
#import bevy_pbr::mesh_view_bindings
#import bevy_pbr::mesh_bindings
#import bevy_pbr::mesh_functions
#import bevy_pbr::utils
#import bevy_pbr::clustered_forward
#import bevy_pbr::lighting
#import bevy_pbr::shadows
#import bevy_pbr::pbr_types
#import bevy_pbr::pbr_functions

// TODO define VERTEX_UVS

@group(1) @binding(0)
var color_texture: texture_2d_array<f32>;
@group(1) @binding(1)
var color_texture_sampler: sampler;
@group(1) @binding(2)
var normal_texture: texture_2d_array<f32>;
@group(1) @binding(3)
var normal_texture_sampler: sampler;


struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    // Is also defined within mesh_vertex_output but I can't get the VERTEX_UVS environment variable defined so I had to pull this jank.
    @location(2) uv: vec2<f32>,
    @location(6) layer: u32,
    #import bevy_pbr::mesh_vertex_output
};

fn extract_unsigned(word: u32, width: u32, offset: u32) -> u32 {
    var element = (word >> offset) & (~0u >> (32u - width));
    return element;
}

fn extract_signed(word: u32, width: u32, offset: u32) -> i32 {
    var unsigned_element = (word >> offset) & (~0u >> (32u - width));
    var mask = 1u << (width - 1u);
    var element = (unsigned_element ^ mask) - mask;
    return bitcast<i32>(element);
}

@vertex
fn vertex(@location(0) vertex: u32) -> VertexOutput {
    var out: VertexOutput;

    var model = mesh.model;

    var x = extract_unsigned(vertex, 5u, 0u);
    var y = extract_unsigned(vertex, 5u, 5u);
    var z = extract_unsigned(vertex, 5u, 10u);

    var normal_x = extract_signed(vertex, 2u, 15u);
    var normal_y = extract_signed(vertex, 2u, 17u);
    var normal_z = extract_signed(vertex, 2u, 19u);

    var u = extract_unsigned(vertex, 1u, 21u);
    var v = extract_unsigned(vertex, 1u, 22u);
    var w = extract_unsigned(vertex, 9u, 23u);

    out.world_position = mesh_position_local_to_world(model, vec4<f32>(f32(x), f32(y), f32(z), 1.0));
    // out.world_position[3] = w;
    out.clip_position = mesh_position_world_to_clip(out.world_position);
    out.world_normal =  mesh_normal_local_to_world(vec3(f32(normal_x), f32(normal_y), f32(normal_z)));
    out.uv = vec2(f32(u), f32(v));
    out.layer = w;

    return out;
}

struct FragmentInput {
    @builtin(front_facing) is_front: bool,
    @builtin(position) frag_coord: vec4<f32>,
    // Is also defined within mesh_vertex_output but I can't get the VERTEX_UVS environment variable defined so I had to pull this jank.
    @location(2) uv: vec2<f32>,
    @location(6) layer: u32,
    #import bevy_pbr::mesh_vertex_output
};

@fragment
fn fragment(in: FragmentInput) -> @location(0) vec4<f32> {
    var pbr_input: PbrInput = pbr_input_new();

    // pbr_input.material.base_color = vec4<f32>(1.0, 0.0, 0.0, 1.0);
    pbr_input.material.base_color = textureSample(color_texture, color_texture_sampler, in.uv, i32(in.layer));
    // pbr_input.material.base_color = textureSample(normal_texture, normal_texture_sampler, in.uv, i32(in.layer));

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
    );
    pbr_input.V = calculate_view(in.world_position, pbr_input.is_orthographic);

    // /* set other PbrInput fields here */
    return tone_mapping(pbr(pbr_input));
}