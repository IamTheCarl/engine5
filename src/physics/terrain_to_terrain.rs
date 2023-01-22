use std::{collections::HashSet, time::Instant};

use bevy::prelude::*;
use bevy_prototype_debug_lines::DebugLines;

use crate::terrain::{Chunk, ChunkPosition, LocalBlockCoordinate, TerrainSpace};

use super::{DebugRenderSettings, Position, SpatialHash, SpatialObjectTracker};

struct ComponentIterator {
    index: usize,
    components: [f32; 3],
}

impl ComponentIterator {
    fn new(vector: Vec3) -> Self {
        Self {
            index: 0,
            components: [vector.x, vector.y, vector.z],
        }
    }

    fn into_vec3(mut iterator: impl Iterator<Item = f32>) -> Vec3 {
        Vec3::new(
            iterator.next().expect("No X component provided."),
            iterator.next().expect("No Y component provided."),
            iterator.next().expect("No Z component provided."),
        )
    }
}

impl Iterator for ComponentIterator {
    type Item = f32;

    fn next(&mut self) -> Option<Self::Item> {
        let component = self.components.get(self.index);
        self.index += 1;

        component.copied()
    }
}

pub(super) fn check_for_intersections(
    mut terrain_space: Query<(&TerrainSpace, &mut Position)>,
    terrain: Query<(&ChunkPosition, &Chunk)>,
    debug_render_settings: Res<DebugRenderSettings>,
    mut lines: ResMut<DebugLines>,
) {
    let start_time = Instant::now();
    let mut comparison_count = 0;

    let mut iter = terrain_space.iter_combinations_mut();

    while let Some([a, b]) = iter.fetch_next() {
        // Make sure space_a is the smaller one.
        let (a, b) = if a.0.num_non_empty_chunks() < b.0.num_non_empty_chunks() {
            (a, b)
        } else {
            (b, a)
        };

        let (space_a, position_a) = a;
        let (space_b, position_b) = b;

        dbg!(
            space_a.num_non_empty_chunks(),
            space_b.num_non_empty_chunks()
        );

        let a_quat = position_a.quat();

        for chunk_entity in space_a.iter_loaded_chunks() {
            if let Ok((chunk_a_position, chunk_a)) = terrain.get(*chunk_entity) {
                let chunk_a_position = chunk_a_position.as_block_coordinate();
                // let chunk_a_position_in_b_space = chunk_a_position
                for (local_block_position, block) in chunk_a.iter() {
                    let block_global_position = (chunk_a_position + local_block_position).as_vec3();

                    let block_a_in_global_space =
                        (a_quat * block_global_position) + position_a.translation;

                    comparison_count += 1;

                    // if debug_render_settings.terrain_terrain_checks {
                    //     lines.line_colored(
                    //         block_a_in_global_space,
                    //         block_a_in_global_space + Vec3::Y,
                    //         0.0,
                    //         Color::GREEN,
                    //     );
                    // }

                    // let block_a_in_b_space =
                    //     position_b.inverse_quat() * (block_a_in_global_space - position_b.translation);
                }
            }
        }
    }

    // fn collision_check(
    //     lines: &mut ResMut<DebugLines>,
    //     debug_color: Option<Color>,
    //     chunk_a: &Chunk,
    //     position_a: &mut Position,
    //     chunk_b: &Chunk,
    //     position_b: &mut Position,
    // ) {
    //     let inverse_quat_a = position_a.inverse_quat();
    //     let inverse_quat_b = position_b.inverse_quat();
    //     let quat_b = position_b.quat();
    //     let quat_a = position_a.quat();

    //     // Let's figure out the a smaller slice of possibly intersecting blocks.
    //     let b_top_left_in_a_space = quat_a * (position_b.translation - position_a.translation);
    //     let b_top_right_in_a_space = quat_a
    //         * ((position_b.translation
    //             + Vec3::new(
    //                 Chunk::CHUNK_DIAMETER as f32,
    //                 0.0,
    //                 Chunk::CHUNK_DIAMETER as f32,
    //             ))
    //             - position_a.translation);

    //     // Check for collisions from chunk a to chunk b.
    //     for (coordinate_a, block_a) in
    //         chunk_a.fuzzy_iter_range(b_top_left_in_a_space, b_top_right_in_a_space)
    //     {
    //         if block_a.is_some() {
    //             let coordinate_a_global_space = inverse_quat_a
    //                 * (coordinate_a.as_vec3() + Vec3::splat(0.5))
    //                 + position_a.translation;

    //             let coordinate_a_in_b_space =
    //                 quat_b * (coordinate_a_global_space - position_b.translation);

    //             let corner_coordinate = coordinate_a_in_b_space.floor().as_ivec3();
    //             let block_b = chunk_b.get_block_local(corner_coordinate);

    //             if block_b.is_some() {
    //                 // We have a collision. Let's figure out the normal of the collision.
    //                 let mut direction = ComponentIterator::into_vec3(
    //                     ComponentIterator::new(coordinate_a_in_b_space.fract()).map(|axis| {
    //                         if axis > 0.5 {
    //                             1.0 - axis
    //                         } else {
    //                             -axis
    //                         }
    //                     }),
    //                 );

    //                 // Check if a block exists in the direction of the axis.
    //                 // If there is a block there, we can't push in that direction.
    //                 let x = direction.x.signum() as i32;
    //                 let is_block = chunk_b
    //                     .get_block_local(corner_coordinate + LocalBlockCoordinate::new(x, 0, 0))
    //                     .is_some();
    //                 if is_block {
    //                     direction.x = 0.0;
    //                 }

    //                 let y = direction.y.signum() as i32;
    //                 let is_block = chunk_b
    //                     .get_block_local(corner_coordinate + LocalBlockCoordinate::new(0, y, 0))
    //                     .is_some();
    //                 if is_block {
    //                     direction.y = 0.0;
    //                 }

    //                 let z = direction.z.signum() as i32;
    //                 let is_block = chunk_b
    //                     .get_block_local(corner_coordinate + LocalBlockCoordinate::new(0, 0, z))
    //                     .is_some();
    //                 if is_block {
    //                     direction.z = 0.0;
    //                 }

    //                 // position_a.translation += inverse_quat_b * direction * 0.5;
    //                 // position_b.translation -= inverse_quat_b * direction * 0.5;

    //                 if let Some(debug_color) = debug_color {
    //                     let point =
    //                         inverse_quat_b * coordinate_a_in_b_space + position_b.translation;
    //                     lines.line_gradient(
    //                         point,
    //                         point + inverse_quat_b * direction,
    //                         0.0,
    //                         Color::Hsla {
    //                             hue: position_a.rotation + position_b.rotation,
    //                             saturation: 0.5,
    //                             lightness: 0.5,
    //                             alpha: 1.0,
    //                         },
    //                         debug_color,
    //                     );
    //                 }
    //             }
    //         }
    //     }
    // }

    // // We don't want to compare a set of entities more than once, so make sure we only get a set of unique comparisons.
    // let mut to_compare = HashSet::new();

    // for (entity_a, spatial_hash_a, _position_a, _chunk_a) in terrain.iter() {
    //     spatial_object_tracker.get_ballpark(spatial_hash_a, |entity_b| {
    //         if entity_a != *entity_b {
    //             let mut comparison_set = [entity_a, *entity_b];
    //             comparison_set.sort_unstable();

    //             to_compare.insert(comparison_set);
    //         }
    //     })
    // }

    // for comparison_set in to_compare.drain() {
    //     // Everything from the previous loop should still exist.
    //     // If this fails, it's because one of the entities wasn't terrain.
    //     if let Ok(mut entities) = terrain.get_many_mut(comparison_set) {
    //         let (entity_a, entity_b) = entities.split_at_mut(1);

    //         let (_entity_a, _spatial_hash_a, position_a, chunk_a) = &mut entity_a[0];
    //         let (_entity_b, _spatial_hash_b, position_b, chunk_b) = &mut entity_b[0];

    //         collision_check(
    //             &mut lines,
    //             debug_render_settings
    //                 .terrain_terrain_checks
    //                 .then_some(Color::GREEN),
    //             chunk_a,
    //             position_a,
    //             chunk_b,
    //             position_b,
    //         );

    //         collision_check(
    //             &mut lines,
    //             debug_render_settings
    //                 .terrain_terrain_checks
    //                 .then_some(Color::CYAN),
    //             chunk_b,
    //             position_b,
    //             chunk_a,
    //             position_a,
    //         );
    //     }
    // }

    dbg!(start_time.elapsed(), comparison_count);
}
