
#import bevy_pbr::mesh_view_bindings
#import bevy_pbr::mesh_bindings
#import bevy_pbr::mesh_functions

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
        vec4( 0.0,  1.0,  0.0, 0.0), // Up
        vec4( 0.0, -1.0,  0.0, 0.0), // Down
        vec4( 0.0,  0.0,  1.0, 0.0), // North
        vec4( 0.0,  0.0, -1.0, 0.0), // South
        vec4( 1.0,  0.0,  0.0, 0.0), // East
        vec4(-1.0,  0.0,  0.0, 0.0), // West
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
