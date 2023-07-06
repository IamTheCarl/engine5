use bevy::{math::Vec3Swizzles, prelude::*};
use bevy_prototype_debug_lines::DebugLines;

use crate::world::{
    spatial_entities::SpatialHash,
    terrain::{Chunk, ChunkPosition, LocalBlockCoordinate, TerrainSpace},
};

use super::{Cylinder, DebugRenderSettings, Position};

pub(super) fn check_for_intersections(
    mut cylinders: Query<(
        &mut Position,
        &Cylinder,
        With<SpatialHash>,
        Without<TerrainSpace>,
    )>,
    terrain_space: Query<(&TerrainSpace, &Position)>,
    terrain: Query<(&ChunkPosition, &Chunk)>,
    debug_render_settings: Res<DebugRenderSettings>,
    mut lines: ResMut<DebugLines>,
) {
    for (space, space_position) in terrain_space.iter() {
        let terrain_quat = space_position.quat();

        for (mut cylinder_position, cylinder, _with_spatial_hash, _without_terrain_space) in
            cylinders.iter_mut()
        {
            let scan_x_radius = cylinder.radius.ceil() as i8 + 1;
            let z_range_squared = ((scan_x_radius) as f32).powi(2);

            for x in -scan_x_radius..=scan_x_radius {
                let scan_z_radius = (z_range_squared - (x as f32).powi(2)).sqrt().floor() as i8;

                for z in -scan_z_radius..=scan_z_radius {
                    // Transfer the cylinder into our local space.
                    let localized_cylinder_position =
                        terrain_quat * (cylinder_position.translation - space_position.translation);

                    let block_index =
                        localized_cylinder_position.floor() + Vec3::new(x as f32, 0.0, z as f32);

                    let closest_point =
                        localized_cylinder_position.clamp(block_index, block_index + Vec3::ONE);

                    let terrain_color = Color::Hsla {
                        hue: space_position.rotation.to_degrees(),
                        saturation: 1.0,
                        lightness: 0.5,
                        alpha: 1.0,
                    };

                    let collision_normal = localized_cylinder_position - closest_point;

                    if debug_render_settings.cylinder_terrain_checks {
                        let point = (space_position.inverse_quat() * closest_point)
                            + space_position.translation;
                        let collision_normal = space_position.inverse_quat() * collision_normal;

                        lines.line_colored(point, point + collision_normal, 0.0, terrain_color);
                    }

                    // Okay actually make it 2D now.
                    let collision_vector = collision_normal.xz();
                    let collision_depth = collision_vector.length();

                    let rounded_cylinder_height = cylinder.height.ceil() as i32;

                    let contained = block_index == localized_cylinder_position.floor();

                    // It's possible for this block column to have collisions.
                    if collision_depth <= cylinder.radius || contained
                    // Or this block containers us.
                    {
                        // We can calculate what would happen on the XZ plane for all blocks in the column.
                        let collision_xz_normal =
                            collision_vector.normalize() * cylinder.radius - collision_vector;

                        // Used for debug later.
                        let block_side_direction = collision_xz_normal.normalize();
                        let block_side_direction = if block_side_direction.x.abs()
                            > block_side_direction.y.abs()
                        {
                            LocalBlockCoordinate::new(block_side_direction.x.signum() as i32, 0, 0)
                        } else {
                            LocalBlockCoordinate::new(0, 0, block_side_direction.y.signum() as i32)
                        };

                        let mut y_collision_depth = 0.0f32;

                        for y in 0..=rounded_cylinder_height {
                            let block_index_float = block_index + Vec3::new(0.0, y as f32, 0.0);

                            // We don't need to worry about an integer overflow here because the broadphase won't let us compare
                            // to terrain that far away from a cylinder.
                            let block_index = block_index_float.as_ivec3();

                            // Is there a block to collide with here?
                            if space
                                .get_block(
                                    &terrain,
                                    |terrain, entity| {
                                        terrain.get(entity).ok().map(|(_index, chunk)| chunk)
                                    },
                                    block_index,
                                )
                                .is_some()
                                && ((block_index_float.y + 1.0) > localized_cylinder_position.y
                                    && block_index_float.y
                                        < (localized_cylinder_position.y + cylinder.height))
                            {
                                // There is a block to collide with. The question is, how do we respond?

                                // We already know how to respond on the xz plane but we need to figure out how to respond on the Y axis.
                                let local_y_collision_depth = if localized_cylinder_position.y
                                    > block_index.y as f32
                                {
                                    1.0 - (localized_cylinder_position.y - block_index.y as f32)
                                } else {
                                    -(cylinder.height
                                        - (block_index.y as f32 - localized_cylinder_position.y))
                                };

                                if debug_render_settings.cylinder_terrain_checks {
                                    let point = (space_position.inverse_quat() * closest_point)
                                        + space_position.translation;

                                    let color = if local_y_collision_depth.abs()
                                        < collision_xz_normal.length()
                                    {
                                        Color::PURPLE
                                    } else {
                                        Color::CYAN
                                    };

                                    lines.line_colored(
                                        point,
                                        point
                                            + Vec3::new(
                                                collision_xz_normal.x,
                                                local_y_collision_depth,
                                                collision_xz_normal.y,
                                            ),
                                        0.0,
                                        color,
                                    );

                                    let point = (space_position.inverse_quat()
                                        * (block_index.as_vec3() + Vec3::new(0.5, 0.0, 0.5)))
                                        + space_position.translation;

                                    lines.line_colored(point, point + Vec3::Y, 0.0, color);

                                    lines.line_colored(
                                        point,
                                        point
                                            + space_position.inverse_quat()
                                                * Vec3::new(
                                                    block_side_direction.x as f32,
                                                    block_side_direction.y as f32,
                                                    block_side_direction.z as f32,
                                                ),
                                        0.0,
                                        Color::PINK,
                                    );

                                    let point = (space_position.inverse_quat()
                                        * (block_index.as_vec3() + Vec3::new(0.5, 0.5, 0.5)))
                                        + space_position.translation;

                                    lines.line_colored(
                                        point,
                                        point
                                            + space_position.inverse_quat()
                                                * Vec3::new(
                                                    0.0,
                                                    local_y_collision_depth.signum(),
                                                    0.0,
                                                ),
                                        0.0,
                                        Color::CRIMSON,
                                    );
                                }

                                if local_y_collision_depth.abs() < collision_xz_normal.length()
                                    || local_y_collision_depth.abs() < 0.1
                                    || collision_xz_normal.is_nan()
                                {
                                    if local_y_collision_depth.abs() > y_collision_depth.abs() {
                                        y_collision_depth = local_y_collision_depth;
                                    }
                                } else if collision_xz_normal.x.abs() > collision_xz_normal.y.abs()
                                {
                                    y_collision_depth = 0.0;
                                    cylinder_position.translation += space_position.inverse_quat()
                                        * Vec3::new(collision_xz_normal.x, 0.0, 0.0);
                                    break; // The cylinder just got pushed out of the column. There shouldn't be anymore collisions.
                                } else {
                                    y_collision_depth = 0.0;
                                    cylinder_position.translation += space_position.inverse_quat()
                                        * Vec3::new(0.0, 0.0, collision_xz_normal.y);
                                    break; // The cylinder just got pushed out of the column. There shouldn't be anymore collisions.
                                }
                            }
                        }
                        cylinder_position.translation += Vec3::new(0.0, y_collision_depth, 0.0);

                        debug_assert!(!cylinder_position.translation.is_nan());
                    }
                }
            }
        }
    }
}
