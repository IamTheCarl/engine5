use std::{collections::HashMap, num::NonZeroUsize};

use bevy::prelude::*;
use bevy_prototype_debug_lines::DebugLines;

use super::{calculate_global_transform, DebugRenderSettings, Position, RayCast};
use crate::terrain::{Chunk, ChunkPosition, GlobalBlockCoordinate, TerrainSpace};

#[derive(Component, Default)]
pub struct RayTerrainIntersectionList {
    pub contact_limit: Option<NonZeroUsize>,
    pub contacts: HashMap<Entity, Vec<RayTerrainIntersection>>,
}

#[derive(Debug)]
pub enum RayTerrainIntersectionType {
    Entry,
    Tunneled,
    Exit,
}

/// An intersection with terrain.
/// All values are local to the terrain space.
#[derive(Debug)]
pub struct RayTerrainIntersection {
    pub distance: f32,
    pub intersection_type: RayTerrainIntersectionType,
    pub block_coordinate: GlobalBlockCoordinate,
    pub position: Vec3,
    pub normal: IVec3,
}

pub fn check_for_intersections(
    mut rays: Query<(Entity, &RayCast, &mut RayTerrainIntersectionList)>,
    terrain_spaces: Query<(Entity, &TerrainSpace, &Position)>,
    terrain: Query<(&ChunkPosition, &Chunk)>,
    transforms: Query<(Option<&Parent>, &Transform)>,
    debug_render_settings: Res<DebugRenderSettings>,
    mut lines: ResMut<DebugLines>,
) {
    rays.for_each_mut(|(_entity, _ray, mut list)| list.contacts.clear());

    // Since a terrain space is a *much* bigger piece of memory, lets iterate through those less often than we iterate through rays.
    for (terrain_space_entity, terrain_space, terrain_position) in terrain_spaces.iter() {
        let terrain_quat = terrain_position.quat();

        for (ray_entity, ray, mut intersection_list) in rays.iter_mut() {
            let ray_transform = calculate_global_transform(ray_entity, |entity| {
                let (parent, transform) = transforms.get(entity).expect("A parent didn't exist.");
                (parent.map(|parent| parent.get()), transform)
            });

            // Move ray into terrain space.
            let ray_position_in_terrain_space =
                terrain_quat * (ray_transform.translation - terrain_position.translation);
            let ray_direction_in_terrain_space =
                terrain_quat * (-ray_transform.rotation * ray.direction);

            fn calculate_step_axis(ray_direction: Vec3, axis: f32) -> f32 {
                let value = ((ray_direction.x / axis).powi(2)
                    + (ray_direction.y / axis).powi(2)
                    + (ray_direction.z / axis).powi(2))
                .sqrt();

                // For when 0/0 happens.
                if !value.is_nan() {
                    value
                } else {
                    f32::INFINITY
                }
            }

            // Prepare for block iteration.
            let step_directions = ray_direction_in_terrain_space.signum();
            let step_lengths = Vec3::new(
                calculate_step_axis(
                    ray_direction_in_terrain_space,
                    ray_direction_in_terrain_space.x,
                ),
                calculate_step_axis(
                    ray_direction_in_terrain_space,
                    ray_direction_in_terrain_space.y,
                ),
                calculate_step_axis(
                    ray_direction_in_terrain_space,
                    ray_direction_in_terrain_space.z,
                ),
            );

            let mut block_position = ray_position_in_terrain_space.floor().as_ivec3();

            let mut ray_axis_lengths = {
                let x = if step_directions.x < 0.0 {
                    ray_position_in_terrain_space.x - block_position.x as f32
                } else {
                    (block_position.x as f32 + 1.0) - ray_position_in_terrain_space.x
                };

                let y = if step_directions.y < 0.0 {
                    ray_position_in_terrain_space.y - block_position.y as f32
                } else {
                    (block_position.y as f32 + 1.0) - ray_position_in_terrain_space.y
                };

                let z = if step_directions.z < 0.0 {
                    ray_position_in_terrain_space.z - block_position.z as f32
                } else {
                    (block_position.z as f32 + 1.0) - ray_position_in_terrain_space.z
                };

                Vec3::new(x * step_lengths.x, y * step_lengths.y, z * step_lengths.z)
            };

            let step_directions = step_directions.as_ivec3();

            let mut distance_traveled = 0.0;

            let mut intersections = Vec::new();
            let mut was_in_terrain = false;

            enum StepDirection {
                X,
                Y,
                Z,
            }

            // Actually step through the terrain blocks.
            while distance_traveled < ray.length
                && intersection_list
                    .contact_limit
                    .map_or(true, |contact_limit| {
                        intersections.len() < contact_limit.get()
                    })
            {
                // Take the shortest step possible to just get into the next block.
                // I just think this style of if statements looks better in this context.
                #[allow(clippy::collapsible_else_if)]
                let last_step_direction = if ray_axis_lengths.x < ray_axis_lengths.y {
                    if ray_axis_lengths.x < ray_axis_lengths.z {
                        // X
                        block_position.x += step_directions.x;
                        distance_traveled = ray_axis_lengths.x;
                        ray_axis_lengths.x += step_lengths.x;
                        StepDirection::X
                    } else {
                        // Z
                        block_position.z += step_directions.z;
                        distance_traveled = ray_axis_lengths.z;
                        ray_axis_lengths.z += step_lengths.z;
                        StepDirection::Z
                    }
                } else {
                    if ray_axis_lengths.y < ray_axis_lengths.z {
                        // Y
                        block_position.y += step_directions.y;
                        distance_traveled = ray_axis_lengths.y;
                        ray_axis_lengths.y += step_lengths.y;
                        StepDirection::Y
                    } else {
                        // Z
                        block_position.z += step_directions.z;
                        distance_traveled = ray_axis_lengths.z;
                        ray_axis_lengths.z += step_lengths.z;
                        StepDirection::Z
                    }
                };

                if terrain_space
                    .get_block(
                        &terrain,
                        |terrain, entity| terrain.get(entity).ok().map(|(_index, chunk)| chunk),
                        block_position,
                    )
                    .is_some()
                {
                    // We hit a block.
                    let intersection_type = if was_in_terrain {
                        RayTerrainIntersectionType::Tunneled
                    } else {
                        RayTerrainIntersectionType::Entry
                    };

                    let position = ray_position_in_terrain_space
                        + ray_direction_in_terrain_space * distance_traveled;

                    let normal = match last_step_direction {
                        StepDirection::X => IVec3::new(-step_directions.x, 0, 0),
                        StepDirection::Y => IVec3::new(0, -step_directions.y, 0),
                        StepDirection::Z => IVec3::new(0, 0, -step_directions.z),
                    };

                    intersections.push(RayTerrainIntersection {
                        distance: distance_traveled,
                        intersection_type,
                        block_coordinate: block_position,
                        position,
                        normal,
                    });

                    if debug_render_settings.terrain_ray_casts {
                        draw_debug_cube(
                            &mut lines,
                            Color::DARK_GREEN,
                            terrain_position.translation
                                + terrain_quat.inverse() * block_position.as_vec3(),
                        );
                    }

                    was_in_terrain = true;
                } else {
                    // We did not hit a block.
                    if was_in_terrain {
                        // In fact, we just exited a block.
                        let intersection_type = RayTerrainIntersectionType::Exit;

                        let position = ray_position_in_terrain_space
                            + ray_direction_in_terrain_space * distance_traveled;

                        let normal = match last_step_direction {
                            StepDirection::X => IVec3::new(step_directions.x, 0, 0),
                            StepDirection::Y => IVec3::new(0, step_directions.y, 0),
                            StepDirection::Z => IVec3::new(0, 0, step_directions.z),
                        };

                        intersections.push(RayTerrainIntersection {
                            distance: distance_traveled,
                            intersection_type,
                            block_coordinate: block_position,
                            position,
                            normal,
                        });

                        was_in_terrain = false;

                        if debug_render_settings.terrain_ray_casts {
                            draw_debug_cube(
                                &mut lines,
                                Color::CRIMSON,
                                terrain_position.translation
                                    + terrain_quat.inverse() * block_position.as_vec3(),
                            );
                        }
                    } else if debug_render_settings.terrain_ray_casts {
                        draw_debug_cube(
                            &mut lines,
                            Color::BLUE,
                            terrain_position.translation
                                + terrain_quat.inverse() * block_position.as_vec3(),
                        );
                    }
                }
            }

            intersection_list
                .contacts
                .insert(terrain_space_entity, intersections);
        }
    }
}

pub fn debug_render(
    rays: Query<&mut RayTerrainIntersectionList>,
    terrain_spaces: Query<&Position>,
    mut lines: ResMut<DebugLines>,
) {
    for intersection_list in rays.iter() {
        for (terrain_space_entity, contacts) in intersection_list.contacts.iter() {
            // Should never fail.
            if let Ok(terrain_position) = terrain_spaces.get(*terrain_space_entity) {
                let inverse_terrain_quat = terrain_position.inverse_quat();

                let mut contact_iterator = contacts.iter().peekable();

                while let Some(contact) = contact_iterator.next() {
                    let point =
                        (inverse_terrain_quat * contact.position) + terrain_position.translation;

                    let color = match contact.intersection_type {
                        RayTerrainIntersectionType::Entry => {
                            lines.line_colored(
                                point,
                                point + inverse_terrain_quat * contact.normal.as_vec3() * 0.25,
                                0.0,
                                Color::GREEN,
                            );
                            Color::GREEN
                        }
                        RayTerrainIntersectionType::Tunneled => Color::ORANGE,
                        RayTerrainIntersectionType::Exit => Color::RED,
                    };

                    let next_point = contact_iterator
                        .peek()
                        .map(|contact| {
                            (inverse_terrain_quat * contact.position) + terrain_position.translation
                        })
                        .unwrap_or(point + Vec3::Y);

                    lines.line_colored(point, next_point, 0.0, color);
                }
            }
        }
    }
}

fn draw_debug_cube(lines: &mut ResMut<DebugLines>, color: Color, position: Vec3) {
    lines.line_colored(
        position + Vec3::new(0.0, 0.0, 0.0),
        position + Vec3::new(1.0, 0.0, 0.0),
        0.0,
        color,
    );
    lines.line_colored(
        position + Vec3::new(0.0, 0.0, 0.0),
        position + Vec3::new(0.0, 1.0, 0.0),
        0.0,
        color,
    );
    lines.line_colored(
        position + Vec3::new(0.0, 0.0, 0.0),
        position + Vec3::new(0.0, 0.0, 1.0),
        0.0,
        color,
    );

    lines.line_colored(
        position + Vec3::new(1.0, 1.0, 1.0),
        position + Vec3::new(0.0, 1.0, 1.0),
        0.0,
        color,
    );
    lines.line_colored(
        position + Vec3::new(1.0, 1.0, 1.0),
        position + Vec3::new(1.0, 0.0, 1.0),
        0.0,
        color,
    );
    lines.line_colored(
        position + Vec3::new(1.0, 1.0, 1.0),
        position + Vec3::new(1.0, 1.0, 0.0),
        0.0,
        color,
    );
}
