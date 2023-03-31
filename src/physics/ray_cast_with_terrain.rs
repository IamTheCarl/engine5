use std::{collections::HashMap, num::NonZeroUsize};

use bevy::prelude::*;
use bevy_prototype_debug_lines::DebugLines;
use ordered_float::NotNan;

use super::{ComponentIterator, Position, RayCast};
use crate::terrain::{Chunk, ChunkPosition, GlobalBlockCoordinate, TerrainSpace};

#[derive(Component, Default)]
pub struct RayTerrainIntersectionList {
    contact_limit: Option<NonZeroUsize>,
    contacts: HashMap<Entity, Vec<RayTerrainIntersection>>,
}

pub enum RayTerrainIntersectionType {
    Entry,
    Tunneled,
    Exit,
}

pub struct RayTerrainIntersection {
    intersection_type: RayTerrainIntersectionType,
    block_coordinate: GlobalBlockCoordinate,
    position: Vec3,
    normal: Vec3,
}

pub fn clear_intersection_lists(mut lists: Query<&mut RayTerrainIntersectionList>) {
    lists.for_each_mut(|mut list| list.contacts.clear());
}

pub fn check_for_intersections(
    mut rays: Query<(Entity, &RayCast, &mut RayTerrainIntersectionList)>,
    terrain_spaces: Query<(Entity, &TerrainSpace, &Position)>,
    terrain: Query<(&ChunkPosition, &Chunk)>,
    transforms: Query<(Option<&Parent>, &Transform)>,
    mut lines: ResMut<DebugLines>,
) {
    // Since a terrain space is a *much* bigger piece of memory, lets iterate through those less often than we iterate through rays.
    for (terrain_entity, terrain_space, terrain_position) in terrain_spaces.iter() {
        let inverse_terrain_quat = terrain_position.inverse_quat();
        let terrain_quat = terrain_position.quat();

        for (ray_entity, ray, mut intersection_list) in rays.iter_mut() {
            let mut next_entity = ray_entity;
            let ray_transform = {
                let mut calculated_transform = Transform::default();

                loop {
                    if let Ok((parent, transform)) = transforms.get(next_entity) {
                        calculated_transform = transform.mul_transform(calculated_transform);

                        if let Some(parent) = parent {
                            next_entity = parent.get();
                        } else {
                            // Looks like that's the last one.
                            break;
                        }
                    } else {
                        // A parent didn't exist.
                        unreachable!()
                    }
                }

                calculated_transform
            };

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

            let mut ray_travel =
                ray_position_in_terrain_space.fract() * step_lengths * step_directions;

            let mut block_position = ray_position_in_terrain_space.floor().as_ivec3();
            let step_directions = step_directions.as_ivec3();

            let mut distance_traveled = 0.0;

            // TODO you can also bail out once you have "enough" collisions.
            while distance_traveled < ray.length {
                if ray_travel.x < ray_travel.y {
                    if ray_travel.x < ray_travel.z {
                        // X
                        block_position.x += step_directions.x;
                        distance_traveled = ray_travel.x;
                        ray_travel.x += step_lengths.x;
                    } else {
                        // Z
                        block_position.z += step_directions.z;
                        distance_traveled = ray_travel.z;
                        ray_travel.z += step_lengths.z;
                    }
                } else if ray_travel.y < ray_travel.z {
                    // Y
                    block_position.y += step_directions.y;
                    distance_traveled = ray_travel.y;
                    ray_travel.y += step_lengths.y;
                } else {
                    // Z
                    block_position.z += step_directions.z;
                    distance_traveled = ray_travel.z;
                    ray_travel.z += step_lengths.z;
                }

                if terrain_space
                    .get_block(
                        &terrain,
                        |terrain, entity| terrain.get(entity).ok().map(|(_index, chunk)| chunk),
                        block_position,
                    )
                    .is_some()
                {
                    // We hit a block.
                    // TODO should that stay terrain local?
                    let impact_point = ray_transform.translation
                        + (inverse_terrain_quat * ray_direction_in_terrain_space)
                            * distance_traveled;

                    lines.line_colored(
                        impact_point,
                        impact_point + Vec3::Y * 0.5,
                        0.0,
                        Color::rgb(
                            block_position.x as f32 / 16.0,
                            block_position.y as f32 / 16.0,
                            block_position.z as f32 / 16.0,
                        ),
                    );

                    break;
                }
            }
        }
    }
}

pub fn debug_render(
    rays: Query<(&GlobalTransform, &RayCast, &mut RayTerrainIntersectionList)>,
    mut lines: ResMut<DebugLines>,
) {
    for (transform, ray, intersection_list) in rays.iter() {
        let transform = transform.compute_transform();
        // lines.line_colored(
        //     transform.translation,
        //     transform.transform_point(ray.direction * ray.length),
        //     0.0,
        //     Color::GREEN,
        // );

        for (entity, contacts) in intersection_list.contacts.iter() {
            for contact in contacts.iter() {
                let color = match contact.intersection_type {
                    RayTerrainIntersectionType::Entry => Color::GREEN,
                    RayTerrainIntersectionType::Tunneled => Color::ORANGE,
                    RayTerrainIntersectionType::Exit => Color::RED,
                };

                lines.line_colored(
                    transform.transform_point(contact.position),
                    transform.transform_point(contact.position) + Vec3::Y,
                    0.0,
                    color,
                );
            }
        }
    }
}
