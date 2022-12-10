use crate::terrain::Chunk;
use bevy::{math::Vec3Swizzles, prelude::*};
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

    fn calculate_axis_ballpark(axis: f32) -> (i16, i16) {
        let axis_a = axis as i16;

        let axis_b = if axis.fract() > 0.5 {
            axis_a + 1
        } else {
            axis_a - 1
        };

        (axis_a, axis_b)
    }

    fn calculate_ballpark(position: Vec3) -> [SpatialHash; 8] {
        let cell_position = position / Chunk::CHUNK_DIAMETER as f32;

        let (x, x_alt) = Self::calculate_axis_ballpark(cell_position.x);
        let (y, y_alt) = Self::calculate_axis_ballpark(cell_position.y);
        let (z, z_alt) = Self::calculate_axis_ballpark(cell_position.z);

        // FIXME we're just grabbing the 2x2 space but this may actually be too small if our largest object will actually be a chunk.
        // Maybe we should take object size into account?
        [
            SpatialHash { x, y, z },
            SpatialHash { x, y, z: z_alt },
            SpatialHash { x, y: y_alt, z },
            SpatialHash {
                x,
                y: y_alt,
                z: z_alt,
            },
            SpatialHash { x: x_alt, y, z },
            SpatialHash {
                x: x_alt,
                y,
                z: z_alt,
            },
            SpatialHash {
                x: x_alt,
                y: y_alt,
                z,
            },
            SpatialHash {
                x: x_alt,
                y: y_alt,
                z: z_alt,
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

#[test]
fn spatial_ballpark_calculation() {
    assert_eq!(SpatialObjectTracker::calculate_axis_ballpark(0.0), (0, -1));
    assert_eq!(SpatialObjectTracker::calculate_axis_ballpark(0.5), (0, -1)); // Is arbitrary. (0, 1) would be fine too.
    assert_eq!(SpatialObjectTracker::calculate_axis_ballpark(0.6), (0, 1));
}

#[derive(Component, Debug, PartialEq, Eq, Hash, Default, Clone, Copy)]
pub struct SpatialHash {
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

fn compute_cylinder_to_cylinder_intersections(
    entities: Query<(Entity, &SpatialHash, &Position, &Cylinder)>,
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

            dbg!(intersecting);
        }
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

fn add_spatial_hash_entities(
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

fn update_transforms(mut entities: Query<(&Position, &mut Transform)>) {
    for (position, mut transform) in entities.iter_mut() {
        transform.translation = position.translation;
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
                    translation: Vec3::new(0.0, 5.0, 0.0),
                    rotation: 0.0,
                },
                SpatialHash::default(),
            ));

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

    app.add_system(add_spatial_hash_entities);
    app.add_system(update_spatial_hash_entities);
    app.add_system(handle_removed_spatial_hash_entities); // MUST come after `update_spatial_hash_entities`.
}
