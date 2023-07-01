use std::collections::HashSet;

use bevy::{math::Vec3Swizzles, prelude::*};
use bevy_prototype_debug_lines::DebugLines;

use crate::world::spatial_entities::{SpatialHash, SpatialObjectTracker};

use super::{Cylinder, DebugRenderSettings, Position};

pub(super) fn check_for_intersections(
    mut cylinders: Query<(Entity, &SpatialHash, &mut Position, &Cylinder)>,
    spatial_object_tracker: Res<SpatialObjectTracker>,
    debug_render_settings: Res<DebugRenderSettings>,
    mut lines: ResMut<DebugLines>,
) {
    // We don't want to compare a set of entities more than once, so make sure we only get a set of unique comparisons.
    let mut to_compare = HashSet::new();

    for (entity_a, spatial_hash_a, _position_a, _cylinder_a) in cylinders.iter() {
        spatial_object_tracker.get_ballpark(spatial_hash_a, |entity_b| {
            if entity_a != *entity_b {
                let mut comparison_set = [entity_a, *entity_b];
                comparison_set.sort_unstable();

                to_compare.insert(comparison_set);
            }
        })
    }

    for comparison_set in to_compare.drain() {
        // Everything from the previous loop should still exist.
        // If this fails, it's because one of the entities wasn't a cylinder.
        if let Ok(mut entities) = cylinders.get_many_mut(comparison_set) {
            let (entity_a, entity_b) = entities.split_at_mut(1);

            let (_entity_a, _spatial_hash_a, position_a, cylinder_a) = &mut entity_a[0];
            let (_entity_b, _spatial_hash_b, position_b, cylinder_b) = &mut entity_b[0];

            let a_bottom = position_a.translation.y;
            let a_top = a_bottom + *cylinder_a.height;

            let b_bottom = position_b.translation.y;
            let b_top = b_bottom + *cylinder_b.height;

            let intersecting_y = (a_bottom >= b_bottom && a_bottom <= b_top)
                || (b_bottom >= a_bottom && b_bottom <= a_top);

            // Okay, our y axis are overlapping. Let's see if we're close enough to contact.
            let difference = position_a.translation.xz() - position_b.translation.xz();
            let normal = difference.normalize();
            let distance = difference.length() - *cylinder_a.radius - *cylinder_b.radius;

            let intersecting_xz = distance <= 0.0;
            let intersecting = intersecting_xz && intersecting_y;
            let distance = -distance;

            if intersecting {
                if debug_render_settings.cylinder_cylinder_checks {
                    lines.line_colored(
                        position_a.translation,
                        position_a.translation + Vec3::new(normal.x, 0.0, normal.y) * distance,
                        0.0,
                        Color::YELLOW,
                    );

                    lines.line_colored(
                        position_b.translation,
                        position_b.translation - Vec3::new(normal.x, 0.0, normal.y) * distance,
                        0.0,
                        Color::CYAN,
                    );
                }

                let y_collision_depth = if position_a.translation.y > position_b.translation.y {
                    *cylinder_b.height - (position_a.translation.y - position_b.translation.y)
                } else {
                    -(*cylinder_a.height - (position_b.translation.y - position_a.translation.y))
                };

                if y_collision_depth.abs() > distance {
                    position_a.translation += Vec3::new(normal.x, 0.0, normal.y) * distance;
                    position_b.translation -= Vec3::new(normal.x, 0.0, normal.y) * distance;
                } else {
                    position_a.translation.y += y_collision_depth;
                    position_b.translation.y -= y_collision_depth;
                }
            }
        }
    }
}
