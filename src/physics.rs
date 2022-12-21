use crate::terrain::{to_local_block_coordinate, BlockLocalCoordinate, Chunk};
use bevy::{math::Vec3Swizzles, prelude::*};
use bevy_prototype_debug_lines::DebugLines;
use ordered_float::NotNan;
use std::collections::{HashMap, HashSet};
use wgpu::PrimitiveTopology;

#[derive(Resource)]
struct MeshCollection {
    cylinders: HashMap<Cylinder, Handle<Mesh>>,
}

impl MeshCollection {
    fn new() -> Self {
        Self {
            cylinders: HashMap::new(),
        }
    }

    fn get_cylinder(
        &mut self,
        meshes: &mut ResMut<Assets<Mesh>>,
        cylinder: Cylinder,
    ) -> Handle<Mesh> {
        self.cylinders
            .entry(cylinder)
            .or_insert_with(|| {
                let mut mesh = Mesh::new(PrimitiveTopology::LineList);
                let mut vertices = Vec::new();

                const INDEXES: i32 = 16;
                const FULL_ROTATION: f32 = std::f64::consts::PI as f32 * 2.0;
                const ROTATION_SLICE: f32 = FULL_ROTATION / INDEXES as f32;

                let diameter = *cylinder.radius;
                let height = *cylinder.height;

                let angle = -ROTATION_SLICE;
                let mut previous_position = (angle.cos() * diameter, angle.sin() * diameter);

                for index in 0..INDEXES {
                    let index = index as f32;
                    let angle = index * ROTATION_SLICE;

                    let position = (angle.cos() * diameter, angle.sin() * diameter);

                    vertices.push([position.0, 0.0, position.1]);
                    vertices.push([position.0, height, position.1]);

                    vertices.push([position.0, 0.0, position.1]);
                    vertices.push([previous_position.0, 0.0, previous_position.1]);

                    vertices.push([position.0, height, position.1]);
                    vertices.push([previous_position.0, height, previous_position.1]);

                    previous_position = position;
                }

                mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, vertices);
                meshes.add(mesh)
            })
            .clone()
    }
}

#[derive(Resource)]
struct DebugShaderMaterial(Handle<StandardMaterial>);

#[derive(Component, Default, Debug)]
pub struct Velocity {
    pub translation: Vec3,
    pub rotational: f32,
}

#[derive(Component, Debug)]
pub struct Position {
    pub translation: Vec3,
    pub rotation: f32,
}

impl Position {
    #[inline]
    pub fn local_z(&self) -> Vec3 {
        Vec3::new(f32::sin(self.rotation), 0.0, f32::cos(self.rotation))
    }

    #[inline]
    pub fn quat(&self) -> Quat {
        Quat::from_rotation_y(-self.rotation)
    }

    #[inline]
    pub fn inverse_quat(&self) -> Quat {
        Quat::from_rotation_y(self.rotation)
    }
}

#[derive(Component, Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub struct Cylinder {
    pub height: NotNan<f32>,
    pub radius: NotNan<f32>,
}

#[derive(Resource)]
pub struct DebugRenderSettings {
    cylinder_terrain_checks: bool,
    hashing_center_point: bool,
}

#[derive(Resource)]
struct SpatialObjectTracker(HashMap<SpatialHash, HashSet<Entity>>);

impl SpatialObjectTracker {
    fn add_entity(&mut self, spatial_hash: SpatialHash, entity: Entity) {
        self.0
            .entry(spatial_hash)
            .or_insert_with(HashSet::new)
            .insert(entity);
    }

    fn remove_entity(&mut self, spatial_hash: SpatialHash, entity: Entity) {
        if let Some(cell) = self.0.get_mut(&spatial_hash) {
            cell.remove(&entity);

            // If the cell is empty, remove it. Save some memory.
            if cell.is_empty() {
                self.0.remove(&spatial_hash);
            }
        }
    }

    fn get_cell_entities(&self, spatial_hash: &SpatialHash) -> Option<&HashSet<Entity>> {
        self.0.get(spatial_hash)
    }

    #[rustfmt::skip]
    fn calculate_ballpark(spatial_hash: &SpatialHash) -> [SpatialHash; 27] {
        let x = spatial_hash.x;
        let y = spatial_hash.y;
        let z = spatial_hash.z;

        [
            SpatialHash { x,        y,        z        },
            SpatialHash { x,        y,        z: z + 1 },
            SpatialHash { x,        y,        z: z - 1 },
            SpatialHash { x,        y: y + 1, z        },
            SpatialHash { x,        y: y + 1, z: z + 1 },
            SpatialHash { x,        y: y + 1, z: z - 1 },
            SpatialHash { x,        y: y - 1, z        },
            SpatialHash { x,        y: y - 1, z: z + 1 },
            SpatialHash { x,        y: y - 1, z: z - 1 },
            SpatialHash { x: x + 1, y,        z        },
            SpatialHash { x: x + 1, y,        z: z + 1 },
            SpatialHash { x: x + 1, y,        z: z - 1 },
            SpatialHash { x: x + 1, y: y + 1, z        },
            SpatialHash { x: x + 1, y: y + 1, z: z + 1 },
            SpatialHash { x: x + 1, y: y + 1, z: z - 1 },
            SpatialHash { x: x + 1, y: y - 1, z        },
            SpatialHash { x: x + 1, y: y - 1, z: z + 1 },
            SpatialHash { x: x + 1, y: y - 1, z: z - 1 },
            SpatialHash { x: x - 1, y,        z        },
            SpatialHash { x: x - 1, y,        z: z + 1 },
            SpatialHash { x: x - 1, y,        z: z - 1 },
            SpatialHash { x: x - 1, y: y + 1, z        },
            SpatialHash { x: x - 1, y: y + 1, z: z + 1 },
            SpatialHash { x: x - 1, y: y + 1, z: z - 1 },
            SpatialHash { x: x - 1, y: y - 1, z        },
            SpatialHash { x: x - 1, y: y - 1, z: z + 1 },
            SpatialHash { x: x - 1, y: y - 1, z: z - 1 },
        ]
    }

    fn get_ballpark(&self, spatial_hash: &SpatialHash, mut processor: impl FnMut(&Entity)) {
        for spatial_hash in Self::calculate_ballpark(spatial_hash) {
            if let Some(cell) = self.get_cell_entities(&spatial_hash) {
                for entity in cell.iter() {
                    processor(entity);
                }
            }
        }
    }
}

#[derive(Component, Debug, PartialEq, Eq, Hash, Default, Clone, Copy)]
struct SpatialHash {
    x: i16,
    y: i16,
    z: i16,
}

#[derive(Component, Debug)]
pub struct SpatialHashOffset {
    pub translation: Vec3,
}

fn insert_spatial_hash(
    mut commands: Commands,
    entities: Query<(Entity, With<Position>, Without<SpatialHash>)>,
) {
    for (entity, _, _) in entities.iter() {
        commands.entity(entity).insert(SpatialHash::default());
    }
}

fn compute_cylinder_to_cylinder_intersections(
    entities: Query<(Entity, &SpatialHash, &Position, &Cylinder)>,
    spatial_object_tracker: Res<SpatialObjectTracker>,
) {
    // We don't want to compare a set of entities more than once, so make sure we only get a set of unique comparisons.
    let mut to_compare = HashSet::new();

    for (entity_a, spatial_hash_a, _position_a, _cylinder_a) in entities.iter() {
        spatial_object_tracker.get_ballpark(spatial_hash_a, |entity_b| {
            if entity_a != *entity_b {
                let mut comparison_set = [entity_a, *entity_b];
                comparison_set.sort_unstable();

                to_compare.insert(comparison_set);
            }
        })
    }

    for comparison_set in to_compare.drain() {
        let entity_a = comparison_set[0];
        let entity_b = comparison_set[1];

        // We should have proven all of these cylinders in the previous loop.
        let (_entity_a, _spatial_hash_a, position_a, cylinder_a) = entities
            .get(entity_a)
            .expect("Cylinder is no longer a cylinder.");

        // If this fails, it probably wasn't a cylinder.
        // That or it's a piece of garbage that got left behind.
        if let Ok((_entity_b, _spatial_hash_b, position_b, cylinder_b)) = entities.get(entity_b) {
            let a_bottom = position_a.translation.y;
            let a_top = a_bottom + *cylinder_a.height;

            let b_bottom = position_b.translation.y;
            let b_top = b_bottom + *cylinder_b.height;

            let intersecting_y = (a_bottom >= b_bottom && a_bottom <= b_top)
                || (b_bottom >= a_bottom && b_bottom <= a_top);

            // Okay, our y axis are overlapping. Let's see if we're close enough to contact.
            let difference = position_a.translation.xy() - position_b.translation.xy();
            let distance = difference.length() - *cylinder_a.radius - *cylinder_b.radius;

            let intersecting_xz = distance <= 0.0;
            let intersecting = intersecting_xz && intersecting_y;

            if intersecting {
                dbg!(entity_a, entity_b);
            }
        }
    }
}

fn compute_cylinder_to_terrain_intersections(
    mut cylinders: Query<(With<SpatialHash>, &mut Position, &Cylinder)>,
    terrain: Query<(&SpatialHash, &Position, &Chunk, Without<Cylinder>)>,
    spatial_object_tracker: Res<SpatialObjectTracker>,
    debug_render_settings: Res<DebugRenderSettings>,
    mut lines: ResMut<DebugLines>,
) {
    for (terrain_spatial_hash, terrain_position, terrain, _) in terrain.iter() {
        let terrain_quat = terrain_position.quat();

        let mut block_scan_set = HashSet::new();

        spatial_object_tracker.get_ballpark(terrain_spatial_hash, |maybe_cylinder_entity| {
            // Need to make sure it's actually a cylinder.
            if let Ok((_cylinder_spatial_hash, mut cylinder_position, cylinder)) =
                cylinders.get_mut(*maybe_cylinder_entity)
            {
                // Translate cylinder space into chunk local space.
                let localized_cylinder_position =
                    cylinder_position.translation - terrain_position.translation;

                // Rotate cylinder space into chunk local space.
                let localized_cylinder_position = terrain_quat * localized_cylinder_position;
                let block_localized_cylinder_position = localized_cylinder_position.floor();

                let scan_x_radius = cylinder.radius.ceil() as i8 + 1;
                let z_range_squared = ((scan_x_radius) as f32).powi(2);

                // TODO cache this as an asset? Can we make it not require the hashset instead?
                for x in -scan_x_radius..=scan_x_radius {
                    let scan_z_radius = (z_range_squared - (x as f32).powi(2)).sqrt().floor() as i8;

                    for z in -scan_z_radius..=scan_z_radius {
                        block_scan_set.insert((
                            NotNan::new(x as f32).unwrap(),
                            NotNan::new(z as f32).unwrap(),
                        ));
                    }
                }

                let mut max_normal = Vec3::ZERO;
                let mut min_normal = Vec3::ZERO;

                let rounded_height = cylinder.height.ceil() as i8;

                for (x, z) in block_scan_set.iter() {
                    let block_index_unrounded =
                        Vec3::new(**x, 0.0, **z) + block_localized_cylinder_position;

                    let closest_point = localized_cylinder_position
                        .clamp(block_index_unrounded, block_index_unrounded + Vec3::ONE);

                    let terrain_color = Color::Hsla {
                        hue: terrain_position.rotation.to_degrees(),
                        saturation: 1.0,
                        lightness: 0.5,
                        alpha: 1.0,
                    };

                    let collision_normal = localized_cylinder_position - closest_point;

                    if debug_render_settings.cylinder_terrain_checks {
                        let point = (terrain_position.inverse_quat() * closest_point)
                            + terrain_position.translation;
                        let collision_normal = terrain_position.inverse_quat() * collision_normal;

                        lines.line_colored(point, point + collision_normal, 0.0, terrain_color);
                    }

                    // Okay actually make it 2D now.
                    let collision_normal = collision_normal.xz();
                    let collision_depth = collision_normal.length();

                    // It's possible for this block column to have collisions.
                    if collision_depth <= *cylinder.radius {
                        for layer in 0..=rounded_height {
                            let block_index_unrounded =
                                block_index_unrounded + Vec3::new(0.0, layer as f32, 0.0);

                            // We don't need to worry about an integer overflow here because the broadphase won't let us compare
                            // to terrain that far away from a cylinder.
                            let block_index = to_local_block_coordinate(&block_index_unrounded);

                            if terrain.get_block_local(block_index).is_some()
                                && collision_depth > 0.0
                            {
                                let normal = collision_normal.normalize() * *cylinder.radius
                                    - collision_normal;

                                let block_side_direction = normal.normalize();
                                let block_side_direction = if block_side_direction.x.abs()
                                    > block_side_direction.y.abs()
                                {
                                    BlockLocalCoordinate::new(
                                        block_side_direction.x.signum() as i8,
                                        0,
                                        0,
                                    )
                                } else {
                                    BlockLocalCoordinate::new(
                                        0,
                                        0,
                                        block_side_direction.y.signum() as i8,
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

                                let is_block_to_side = terrain
                                    .get_block_local(block_index + block_side_direction)
                                    .is_some();

                                let is_block_to_top_or_bottom = terrain
                                    .get_block_local(
                                        block_index
                                            + BlockLocalCoordinate::new(
                                                0,
                                                y_collision_depth.signum() as i8,
                                                0,
                                            ),
                                    )
                                    .is_some();

                                // Add in Y component.
                                let normal = {
                                    if debug_render_settings.cylinder_terrain_checks {
                                        let point = (terrain_position.inverse_quat()
                                            * closest_point)
                                            + terrain_position.translation;

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

                                        let point = (terrain_position.inverse_quat()
                                            * (block_index_unrounded.floor()
                                                + Vec3::new(0.5, 0.0, 0.5)))
                                            + terrain_position.translation;

                                        lines.line_colored(point, point + Vec3::Y, 0.0, color);

                                        lines.line_colored(
                                            point,
                                            point
                                                + terrain_position.inverse_quat()
                                                    * Vec3::new(
                                                        block_side_direction.x as f32,
                                                        block_side_direction.y as f32,
                                                        block_side_direction.z as f32,
                                                    ),
                                            0.0,
                                            Color::PINK,
                                        );

                                        let point = (terrain_position.inverse_quat()
                                            * (block_index_unrounded.floor()
                                                + Vec3::new(0.5, 0.5, 0.5)))
                                            + terrain_position.translation;

                                        lines.line_colored(
                                            point,
                                            point
                                                + terrain_position.inverse_quat()
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
                                        || (is_block_to_side && y_collision_depth.abs() < 0.1))
                                        && !is_block_to_top_or_bottom
                                    {
                                        Vec3::new(0.0, y_collision_depth, 0.0)
                                    } else if normal.x.abs() > normal.y.abs() {
                                        Vec3::new(normal.x, 0.0, 0.0)
                                    } else {
                                        Vec3::new(0.0, 0.0, normal.y)
                                    }
                                };

                                max_normal = max_normal.max(normal);
                                min_normal = min_normal.min(normal);

                                debug_assert!(!cylinder_position.translation.is_nan());
                            }
                        }
                    }
                }

                // Apply our collisions.
                let true_normal = {
                    let max_normal_abs = max_normal.abs();
                    let min_normal_abs = min_normal.abs();

                    let x = if min_normal_abs.x > max_normal_abs.x {
                        min_normal.x
                    } else {
                        max_normal.x
                    };

                    let y = if min_normal_abs.y > max_normal_abs.y {
                        min_normal.y
                    } else {
                        max_normal.y
                    };

                    let z = if min_normal_abs.z > max_normal_abs.z {
                        min_normal.z
                    } else {
                        max_normal.z
                    };

                    Vec3::new(x, y, z)
                };

                let true_normal = terrain_position.inverse_quat() * true_normal; // Rotate back into global space.

                cylinder_position.translation += true_normal;

                // We're going to reuse these buffers.
                block_scan_set.clear();
                // collision_normals.clear();
            }
        });
    }
}

fn handle_removed_spatial_hash_entities(
    removals: RemovedComponents<SpatialHash>,
    spatial_objects: Query<&SpatialHash>,
    mut spatial_object_tracker: ResMut<SpatialObjectTracker>,
) {
    for entity in removals.iter() {
        let spatial_hash = spatial_objects
            .get(entity)
            .expect("Entity was already removed.");
        spatial_object_tracker.remove_entity(*spatial_hash, entity);
    }
}

fn update_spatial_hash_entities(
    mut spatial_objects: Query<(Entity, &Position, &mut SpatialHash), Without<SpatialHashOffset>>,
    mut spatial_object_tracker: ResMut<SpatialObjectTracker>,
    debug_render_settings: Res<DebugRenderSettings>,
    mut lines: ResMut<DebugLines>,
) {
    // TODO only update the hash for entities that have a velocity?

    for (entity, position, mut spatial_hash) in spatial_objects.iter_mut() {
        let old_hash = *spatial_hash;

        let translation = (position.translation / Chunk::CHUNK_DIAMETER as f32).floor();

        spatial_hash.x = translation.x as i16;
        spatial_hash.y = translation.y as i16;
        spatial_hash.z = translation.z as i16;

        if debug_render_settings.hashing_center_point {
            lines.line_colored(
                position.translation,
                position.translation + Vec3::Y,
                0.0,
                Color::GREEN,
            );

            lines.line_colored(
                position.translation,
                translation * Chunk::CHUNK_DIAMETER as f32,
                0.0,
                Color::BLUE,
            );
        }

        // An update is actually needed.
        if old_hash != *spatial_hash {
            spatial_object_tracker.remove_entity(old_hash, entity);
            spatial_object_tracker.add_entity(*spatial_hash, entity);
        }
    }
}

fn update_spatial_hash_entities_with_offset(
    mut spatial_objects: Query<(Entity, &Position, &SpatialHashOffset, &mut SpatialHash)>,
    mut spatial_object_tracker: ResMut<SpatialObjectTracker>,
    debug_render_settings: Res<DebugRenderSettings>,
    mut lines: ResMut<DebugLines>,
) {
    // TODO only update the hash for entities that have a velocity?

    for (entity, position, spatial_offset, mut spatial_hash) in spatial_objects.iter_mut() {
        let old_hash = *spatial_hash;

        let offset_translation =
            position.translation + position.inverse_quat() * spatial_offset.translation;

        let translation = (offset_translation / Chunk::CHUNK_DIAMETER as f32).floor();

        spatial_hash.x = translation.x as i16;
        spatial_hash.y = translation.y as i16;
        spatial_hash.z = translation.z as i16;

        if debug_render_settings.hashing_center_point {
            lines.line_colored(
                offset_translation,
                offset_translation + Vec3::Y,
                0.0,
                Color::GREEN,
            );

            lines.line_colored(
                offset_translation,
                translation * Chunk::CHUNK_DIAMETER as f32,
                0.0,
                Color::BLUE,
            );
        }

        // An update is actually needed.
        if old_hash != *spatial_hash {
            spatial_object_tracker.remove_entity(old_hash, entity);
            spatial_object_tracker.add_entity(*spatial_hash, entity);
        }
    }
}

fn add_debug_mesh_cylinders(
    mut commands: Commands,
    settings: Res<DebugRenderSettings>,
    mut meshes: ResMut<Assets<Mesh>>,
    debug_shader_material: Res<DebugShaderMaterial>,
    mut mesh_collections: ResMut<MeshCollection>,
    cylinders: Query<(Entity, &Cylinder, Without<Handle<Mesh>>)>,
) {
    if settings.cylinder_terrain_checks {
        let material = &debug_shader_material.0;

        for (entity, cylinder, _) in cylinders.iter() {
            let mesh = mesh_collections.get_cylinder(&mut meshes, *cylinder);
            commands.entity(entity).insert((
                mesh,
                material.clone(),
                GlobalTransform::default(),
                Visibility::default(),
                ComputedVisibility::default(),
            ));
        }
    }
}

fn remove_debug_mesh_cylinders(
    mut commands: Commands,
    settings: Res<DebugRenderSettings>,
    cylinders: Query<(Entity, &Cylinder, &Handle<Mesh>)>,
) {
    if !settings.cylinder_terrain_checks {
        for (entity, _, _) in cylinders.iter() {
            commands
                .entity(entity)
                .remove::<Handle<Mesh>>()
                .remove::<Handle<StandardMaterial>>()
                .remove::<GlobalTransform>()
                .remove::<Visibility>()
                .remove::<ComputedVisibility>();
        }
    }
}

fn update_movement(time: Res<Time>, mut entities: Query<(&mut Position, &Velocity)>) {
    // TODO Remove velocity from objects that are no longer moving?

    for (mut position, velocity) in entities.iter_mut() {
        position.translation += velocity.translation * time.delta_seconds();
        position.rotation += velocity.rotational * time.delta_seconds();
    }
}

/// Updates transforms to match the physics information. Useful for rendering.
fn update_transforms(mut entities: Query<(&Position, &mut Transform)>) {
    for (position, mut transform) in entities.iter_mut() {
        transform.translation = position.translation;
        transform.rotation = Quat::from_axis_angle(Vec3::Y, position.rotation);
    }
}

pub fn setup(app: &mut App) {
    app.add_startup_system(
        |mut commands: Commands, mut materials: ResMut<Assets<StandardMaterial>>| {
            commands.spawn((
                Cylinder {
                    height: NotNan::new(1.0).unwrap(),
                    radius: NotNan::new(0.5).unwrap(),
                },
                Velocity::default(),
                Transform::default(),
                Position {
                    translation: Vec3::new(-5.0, 5.0, -5.0),
                    rotation: 0.0,
                },
                SpatialHash::default(),
            ));

            // TODO make this accessible from a menu or terminal.
            commands.insert_resource(DebugRenderSettings {
                cylinder_terrain_checks: true,
                hashing_center_point: true,
            });
            commands.insert_resource(MeshCollection::new());

            let debug_shader_material = materials.add(Color::RED.into());
            commands.insert_resource(DebugShaderMaterial(debug_shader_material));

            commands.insert_resource(SpatialObjectTracker(HashMap::new()));
        },
    );

    app.add_system(update_movement);

    app.add_system(add_debug_mesh_cylinders);
    app.add_system(remove_debug_mesh_cylinders);

    let collision_checks = "collision_checks";
    let spatial_hashing = "spatial_hashing";

    app.add_system_set(
        SystemSet::new()
            .label(collision_checks)
            .with_system(compute_cylinder_to_cylinder_intersections)
            .with_system(compute_cylinder_to_terrain_intersections)
            .after(update_movement),
    );

    app.add_system(update_transforms.after(collision_checks));

    app.add_system_set(
        SystemSet::new()
            .label(spatial_hashing)
            .with_system(insert_spatial_hash)
            // .with_system(add_spatial_hash_entities_to_tracker)
            .with_system(update_spatial_hash_entities)
            .with_system(update_spatial_hash_entities_with_offset)
            .with_system(
                handle_removed_spatial_hash_entities
                    .after(update_spatial_hash_entities)
                    .after(update_spatial_hash_entities_with_offset),
            )
            .before(collision_checks)
            .after(update_movement),
    );
}
