use bevy::prelude::*;
use bevy_prototype_debug_lines::DebugLines;

use crate::world::terrain::{Chunk, ChunkPosition, TerrainSpace};

use super::{ComponentIterator, DebugRenderSettings, Position};

pub(super) fn check_for_intersections(
    mut terrain_space: Query<(&TerrainSpace, &mut Position)>,
    terrain: Query<(&ChunkPosition, &Chunk)>,
    debug_render_settings: Res<DebugRenderSettings>,
    mut lines: ResMut<DebugLines>,
) {
    let mut iter = terrain_space.iter_combinations_mut();

    while let Some([a, b]) = iter.fetch_next() {
        // Make sure space_a is the smaller one.
        let (a, b) = if a.0.num_non_empty_chunks() < b.0.num_non_empty_chunks() {
            (a, b)
        } else {
            (b, a)
        };

        let (space_a, mut position_a) = a;
        let (space_b, mut position_b) = b;

        let a_inverse_quat = position_a.inverse_quat();

        let b_quat = position_b.quat();
        let b_inverse_quat = position_b.inverse_quat();

        for chunk_entity in space_a.iter_loaded_chunks() {
            if let Ok((chunk_a_position, chunk_a)) = terrain.get(*chunk_entity) {
                let chunk_a_position = chunk_a_position.as_block_coordinate();

                for (local_column_position, column) in chunk_a.iter_columns() {
                    for (layer, block_a) in column.iter().enumerate() {
                        let block_global_position = (chunk_a_position
                            + IVec3::new(local_column_position.x, 0, local_column_position.y))
                        .as_vec3();

                        let block_a_in_global_space =
                            (a_inverse_quat * block_global_position) + position_a.translation;

                        let block_b_fractional_position = b_quat
                            * (block_a_in_global_space - position_b.translation)
                            + Vec3::new(0.5, 0.0, 0.5); // We target the center of each block.

                        let block_a_in_b_space = block_b_fractional_position.floor();

                        if block_a.is_some() {
                            let block_a_in_b_space =
                                block_a_in_b_space + Vec3::new(0.0, layer as f32, 0.0);

                            let block_b_position = block_b_fractional_position.floor()
                                + Vec3::new(0.0, layer as f32, 0.0);

                            let block_b = space_b.get_block(
                                &terrain,
                                |query, entity| query.get(entity).ok().map(|e| e.1),
                                block_b_position.as_ivec3(),
                            );

                            if block_b.is_some() {
                                // Debug rendering.
                                if debug_render_settings.terrain_terrain_checks {
                                    let point = (b_inverse_quat * block_a_in_b_space)
                                        + position_b.translation;
                                    lines.line_colored(point, point + Vec3::Y, 0.0, Color::GREEN);

                                    let point = (b_inverse_quat * block_b_position)
                                        + position_b.translation;
                                    lines.line_colored(point, point + Vec3::Y, 0.0, Color::RED);
                                }

                                // We have a collision. Let's figure out the normal of the collision.
                                let direction = ComponentIterator::into_vec3(
                                    ComponentIterator::new_f32(block_b_fractional_position.fract())
                                        .map(|axis| if axis > 0.5 { 1.0 - axis } else { -axis }),
                                );

                                position_a.translation += b_inverse_quat * direction * 0.5;
                                position_b.translation -= b_inverse_quat * direction * 0.5;
                            }
                        }
                    }
                }
            }
        }
    }
}
