use crate::terrain::{to_local_block_coordinate, Chunk};
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
    pub axial: Vec3,
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
    enabled: bool,
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

    fn calculate_ballpark(position: Vec3) -> [SpatialHash; 27] {
        let cell_position = position / Chunk::CHUNK_DIAMETER as f32;

        let x = cell_position.x as i16;
        let y = cell_position.y as i16;
        let z = cell_position.z as i16;

        [
            SpatialHash { x, y, z },
            SpatialHash { x, y, z: z + 1 },
            SpatialHash { x, y, z: z - 1 },
            SpatialHash { x, y: y + 1, z },
            SpatialHash {
                x,
                y: y + 1,
                z: z + 1,
            },
            SpatialHash {
                x,
                y: y + 1,
                z: z - 1,
            },
            SpatialHash { x, y: y - 1, z },
            SpatialHash {
                x,
                y: y - 1,
                z: z + 1,
            },
            SpatialHash {
                x,
                y: y - 1,
                z: z - 1,
            },
            SpatialHash { x: x + 1, y, z },
            SpatialHash {
                x: x + 1,
                y,
                z: z + 1,
            },
            SpatialHash {
                x: x + 1,
                y,
                z: z - 1,
            },
            SpatialHash {
                x: x + 1,
                y: y + 1,
                z,
            },
            SpatialHash {
                x: x + 1,
                y: y + 1,
                z: z + 1,
            },
            SpatialHash {
                x: x + 1,
                y: y + 1,
                z: z - 1,
            },
            SpatialHash {
                x: x + 1,
                y: y - 1,
                z,
            },
            SpatialHash {
                x: x + 1,
                y: y - 1,
                z: z + 1,
            },
            SpatialHash {
                x: x + 1,
                y: y - 1,
                z: z - 1,
            },
            SpatialHash { x: x - 1, y, z },
            SpatialHash {
                x: x - 1,
                y,
                z: z + 1,
            },
            SpatialHash {
                x: x - 1,
                y,
                z: z - 1,
            },
            SpatialHash {
                x: x - 1,
                y: y + 1,
                z,
            },
            SpatialHash {
                x: x - 1,
                y: y + 1,
                z: z + 1,
            },
            SpatialHash {
                x: x - 1,
                y: y + 1,
                z: z - 1,
            },
            SpatialHash {
                x: x - 1,
                y: y - 1,
                z,
            },
            SpatialHash {
                x: x - 1,
                y: y - 1,
                z: z + 1,
            },
            SpatialHash {
                x: x - 1,
                y: y - 1,
                z: z - 1,
            },
        ]
    }

    fn get_ballpark(&self, position: Vec3, mut processor: impl FnMut(&Entity)) {
        for spatial_hash in Self::calculate_ballpark(position) {
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

impl SpatialHash {
    fn update_from_position(&mut self, position: &Position) {
        let translation = position.translation / Chunk::CHUNK_DIAMETER as f32;

        self.x = translation.x as i16;
        self.y = translation.y as i16;
        self.z = translation.z as i16;
    }
}

fn insert_spatial_hash(
    mut commands: Commands,
    entities: Query<(Entity, With<Position>, Without<SpatialHash>)>,
) {
    for (entity, _, _) in entities.iter() {
        commands.entity(entity).insert(SpatialHash::default());
    }
    // SpatialHash::default(),
}

fn compute_cylinder_to_cylinder_intersections(
    entities: Query<(Entity, With<SpatialHash>, &Position, &Cylinder)>,
    spatial_object_tracker: Res<SpatialObjectTracker>,
) {
    // TODO use events to pass all intersections to the next system.

    // We don't want to compare a set of entities more than once, so make sure we only get a set of unique comparisons.
    let mut to_compare = HashSet::new();

    for (entity_a, _spatial_hash_a, position_a, _cylinder_a) in entities.iter() {
        spatial_object_tracker.get_ballpark(position_a.translation, |entity_b| {
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
    cylinders: Query<(Entity, With<SpatialHash>, &Position, &Cylinder)>,
    terrain: Query<(Entity, With<SpatialHash>, &Position, &Chunk)>,
    spatial_object_tracker: Res<SpatialObjectTracker>,
    mut lines: ResMut<DebugLines>,
) {
    for (terrain_entity, _terrain_spatial_hash, terrain_position, terrain) in terrain.iter() {
        let terrain_quat = terrain_position.quat();

        let mut block_scan_set = HashSet::new();

        spatial_object_tracker.get_ballpark(
            terrain_position.translation,
            |maybe_cylinder_entity| {
                // Need to make sure it's actually a cylinder.
                if let Ok((cylinder_entity, _cylinder_spatial_hash, cylinder_position, cylinder)) =
                    cylinders.get(*maybe_cylinder_entity)
                {
                    // Translate cylinder space into chunk local space.
                    let localized_cylinder_position =
                        cylinder_position.translation - terrain_position.translation;

                    // Rotate cylinder space into chunk local space.
                    let localized_cylinder_position = terrain_quat * localized_cylinder_position;
                    let block_localized_cylinder_position = localized_cylinder_position.floor();

                    let scan_x_radius = cylinder.radius.ceil() as i8;

                    dbg!();

                    // TODO cache this as an asset?
                    for x in -scan_x_radius..=scan_x_radius {
                        let scan_z_radius = (1.0 - (x as f32).powi(2)).sqrt().ceil() as i8;

                        for z in -scan_z_radius..=scan_z_radius {
                            block_scan_set.insert((
                                NotNan::new(x as f32).unwrap(),
                                NotNan::new(z as f32).unwrap(),
                            ));
                        }
                    }

                    let rounded_height = cylinder.height.ceil() as i8;

                    for layer in 0..=rounded_height {
                        let y = layer as f32;
                        for (x, z) in block_scan_set.iter() {
                            let block_index =
                                Vec3::new(**x, y, **z) + block_localized_cylinder_position;

                            let closest_point = localized_cylinder_position
                                .clamp(block_index, block_index + Vec3::ONE);

                            {
                                let point = (terrain_position.inverse_quat() * closest_point)
                                    + terrain_position.translation;
                                lines.line_colored(
                                    point,
                                    point + Vec3::Y,
                                    0.0,
                                    Color::rgb_linear(
                                        terrain_position.translation.x,
                                        terrain_position.translation.y,
                                        terrain_position.translation.z,
                                    ),
                                );
                            }

                            // We're in the box! Are we contacting?
                            if (closest_point - localized_cylinder_position).length()
                                <= *cylinder.radius
                            {
                                // We don't need to worry about an integer overflow here because the broadphase won't let us compare
                                // to terrain that far away from a cylinder.
                                let block_index = to_local_block_coordinate(&block_index);

                                if terrain.get_block_local(block_index).is_some() {
                                    dbg!(
                                        block_index,
                                        block_localized_cylinder_position,
                                        terrain_entity,
                                        cylinder_entity
                                    );
                                }
                            }
                        }
                    }

                    // We're going to reuse that buffer.
                    block_scan_set.clear();
                }
            },
        );
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

fn add_spatial_hash_entities_to_tracker(
    spatial_objects: Query<(Entity, &SpatialHash), Added<SpatialHash>>,
    mut spatial_object_tracker: ResMut<SpatialObjectTracker>,
) {
    for (entity, spatial_hash) in spatial_objects.iter() {
        spatial_object_tracker.add_entity(*spatial_hash, entity);
    }
}

fn update_spatial_hash_entities(
    mut spatial_objects: Query<(Entity, &Position, &mut SpatialHash)>,
    mut spatial_object_tracker: ResMut<SpatialObjectTracker>,
) {
    // TODO only update the hash for entities that have a velocity.

    for (entity, position, mut spatial_hash) in spatial_objects.iter_mut() {
        let old_hash = *spatial_hash;

        spatial_hash.update_from_position(position);

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
    if settings.enabled {
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
    if !settings.enabled {
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

fn update_movement(mut entities: Query<(&mut Position, &Velocity)>) {
    // FIXME we need the timestep here.
    // TODO Remove velocity from objects that are no longer moving.

    for (mut position, velocity) in entities.iter_mut() {
        position.translation += velocity.axial;
        position.rotation += velocity.rotational;
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
            // commands.spawn((
            //     Cylinder {
            //         height: NotNan::new(1.0).unwrap(),
            //         radius: NotNan::new(0.5).unwrap(),
            //     },
            //     Velocity::default(),
            //     Transform::default(),
            //     Position {
            //         translation: Vec3::new(-5.0, 5.0, -5.0),
            //         rotation: 0.0,
            //     },
            //     SpatialHash::default(),
            // ));

            // TODO make this accessible from a menu or terminal.
            commands.insert_resource(DebugRenderSettings { enabled: true });
            commands.insert_resource(MeshCollection::new());

            let debug_shader_material = materials.add(Color::RED.into());
            commands.insert_resource(DebugShaderMaterial(debug_shader_material));

            commands.insert_resource(SpatialObjectTracker(HashMap::new()));
        },
    );

    app.add_system(update_transforms);

    app.add_system(update_movement);
    app.add_system(add_debug_mesh_cylinders);
    app.add_system(remove_debug_mesh_cylinders);
    app.add_system(compute_cylinder_to_cylinder_intersections);
    app.add_system(compute_cylinder_to_terrain_intersections);

    app.add_system(insert_spatial_hash);
    app.add_system(add_spatial_hash_entities_to_tracker);
    app.add_system(update_spatial_hash_entities);
    app.add_system(handle_removed_spatial_hash_entities); // MUST come after `update_spatial_hash_entities`.
}
