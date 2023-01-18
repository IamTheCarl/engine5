use bevy::{math::Vec3Swizzles, prelude::*};
use bevy_prototype_debug_lines::DebugLines;

use crate::terrain::{Chunk, ChunkPosition, LocalBlockCoordinate, TerrainSpace};

use super::{Cylinder, DebugRenderSettings, Position, SpatialHash};

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
                    let collision_normal = collision_normal.xz();
                    let collision_depth = collision_normal.length();

                    let rounded_cylinder_height = cylinder.height.ceil() as i32;

                    let contained = block_index == localized_cylinder_position.floor();

                    // It's possible for this block column to have collisions.
                    if collision_depth <= *cylinder.radius || contained
                    // Or this block containers us.
                    {
                        for layer in 0..=rounded_cylinder_height {
                            let block_index = block_index + Vec3::new(0.0, layer as f32, 0.0);

                            // We don't need to worry about an integer overflow here because the broadphase won't let us compare
                            // to terrain that far away from a cylinder.
                            let block_index = block_index.as_ivec3();

                            if space
                                .get_block(
                                    &terrain,
                                    |terrain, entity| {
                                        terrain.get(entity).ok().map(|(_index, chunk)| chunk)
                                    },
                                    block_index,
                                )
                                .is_some()
                            {
                                let normal = collision_normal.normalize() * *cylinder.radius
                                    - collision_normal;

                                let block_side_direction = normal.normalize();
                                let block_side_direction = if block_side_direction.x.abs()
                                    > block_side_direction.y.abs()
                                {
                                    LocalBlockCoordinate::new(
                                        block_side_direction.x.signum() as i32,
                                        0,
                                        0,
                                    )
                                } else {
                                    LocalBlockCoordinate::new(
                                        0,
                                        0,
                                        block_side_direction.y.signum() as i32,
                                    )
                                };

                                let y_collision_depth = if localized_cylinder_position.y
                                    > block_index.y as f32
                                {
                                    1.0 - (localized_cylinder_position.y - block_index.y as f32)
                                } else {
                                    -(*cylinder.height
                                        - (block_index.y as f32 - localized_cylinder_position.y))
                                };

                                // Add in Y component.
                                let normal = {
                                    if debug_render_settings.cylinder_terrain_checks {
                                        let point = (space_position.inverse_quat() * closest_point)
                                            + space_position.translation;

                                        let color = if y_collision_depth.abs() < normal.length() {
                                            Color::PURPLE
                                        } else {
                                            Color::CYAN
                                        };

                                        lines.line_colored(
                                            point,
                                            point
                                                + Vec3::new(normal.x, y_collision_depth, normal.y),
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
                                                        y_collision_depth.signum(),
                                                        0.0,
                                                    ),
                                            0.0,
                                            Color::CRIMSON,
                                        );
                                    }

                                    if (y_collision_depth.abs() < normal.length()
                                        || y_collision_depth.abs() < 0.1)
                                        || normal.is_nan()
                                    {
                                        Vec3::new(0.0, y_collision_depth, 0.0)
                                    } else if normal.x.abs() > normal.y.abs() {
                                        Vec3::new(normal.x, 0.0, 0.0)
                                    } else {
                                        Vec3::new(0.0, 0.0, normal.y)
                                    }
                                };

                                let true_normal = space_position.inverse_quat() * normal; // Rotate back into global space.
                                cylinder_position.translation += true_normal;

                                debug_assert!(!cylinder_position.translation.is_nan());
                            }
                        }
                    }
                }
            }
        }
    }
}
