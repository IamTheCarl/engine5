use crate::terrain::{Chunk, LoadTerrain};
use bevy::prelude::*;
use bevy_prototype_debug_lines::DebugLines;
use ordered_float::NotNan;
use std::{
    collections::{HashMap, HashSet},
    num::NonZeroUsize,
};
use wgpu::PrimitiveTopology;

mod cylinder_to_cylinder;
mod cylinder_to_terrain;
mod ray_cast_with_terrain;
pub use ray_cast_with_terrain::{
    RayTerrainIntersection, RayTerrainIntersectionList, RayTerrainIntersectionType,
};
mod terrain_to_terrain;

#[derive(Component)]
pub struct RayCast {
    pub direction: Vec3,
    pub length: f32,
}

impl From<Vec3> for RayCast {
    fn from(vector: Vec3) -> Self {
        Self {
            direction: vector.normalize(),
            length: vector.length(),
        }
    }
}

// TODO move this cylinder-ray stuff into its own module.
#[derive(Component)]
pub struct RayCylinderIntersectionList {
    pub contact_limit: Option<NonZeroUsize>,
    pub contacts: HashMap<Entity, RayCylinderIntersection>,
}
pub enum RayCylinderIntersection {
    Entry { position: Vec3, normal: Vec3 },
    Exit { position: Vec3, normal: Vec3 },
}

// TODO Should this go in a general math library?
pub struct ComponentIterator<Component: Copy> {
    index: usize,
    components: [Component; 3],
}

impl ComponentIterator<f32> {
    pub fn new_f32(vector: Vec3) -> Self {
        Self {
            index: 0,
            components: [vector.x, vector.y, vector.z],
        }
    }

    pub fn into_vec3(mut iterator: impl Iterator<Item = f32>) -> Vec3 {
        Vec3::new(
            iterator.next().expect("No X component provided."),
            iterator.next().expect("No Y component provided."),
            iterator.next().expect("No Z component provided."),
        )
    }
}

impl ComponentIterator<i32> {
    pub fn new_i32(vector: IVec3) -> Self {
        Self {
            index: 0,
            components: [vector.x, vector.y, vector.z],
        }
    }

    pub fn into_ivec3(mut iterator: impl Iterator<Item = i32>) -> IVec3 {
        IVec3::new(
            iterator.next().expect("No X component provided."),
            iterator.next().expect("No Y component provided."),
            iterator.next().expect("No Z component provided."),
        )
    }
}

impl<Component: Copy> Iterator for ComponentIterator<Component> {
    type Item = Component;

    fn next(&mut self) -> Option<Self::Item> {
        let component = self.components.get(self.index);
        self.index += 1;

        component.copied()
    }
}

// TODO this is probably a pretty clunky way to render the cylinders. I just made it because I didn't have debug lines at the time.
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
    cylinders: bool,
    cylinder_terrain_checks: bool,
    hashing_center_point: bool,
    cylinder_cylinder_checks: bool,
    terrain_terrain_checks: bool,
}

// TODO experiment with storing entities by archtype.
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
    if settings.cylinders {
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
    if !settings.cylinders {
        for (entity, _, _) in cylinders.iter() {
            commands.entity(entity).despawn();
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

#[derive(SystemLabel)]
pub struct CollisionCheck;

#[derive(SystemLabel)]
pub struct UpdateSpatialHashes;

#[derive(StageLabel)]
pub struct PhysicsPlugin;

impl Plugin for PhysicsPlugin {
    fn build(&self, app: &mut App) {
        app.add_startup_system(
            |mut commands: Commands, mut materials: ResMut<Assets<StandardMaterial>>| {
                // TODO make this accessible from a menu or terminal.
                commands.insert_resource(DebugRenderSettings {
                    cylinders: true,
                    cylinder_terrain_checks: false,
                    hashing_center_point: false,
                    cylinder_cylinder_checks: false,
                    terrain_terrain_checks: false,
                });
                commands.insert_resource(MeshCollection::new());

                let debug_shader_material = materials.add(Color::RED.into());
                commands.insert_resource(DebugShaderMaterial(debug_shader_material));

                commands.insert_resource(SpatialObjectTracker(HashMap::new()));
            },
        );

        app.add_stage_after(LoadTerrain, PhysicsPlugin, SystemStage::parallel());

        app.add_system_to_stage(PhysicsPlugin, add_debug_mesh_cylinders);
        app.add_system_to_stage(PhysicsPlugin, remove_debug_mesh_cylinders);

        app.add_system_set_to_stage(
            PhysicsPlugin,
            SystemSet::new()
                .with_system(update_movement)
                .with_system(ray_cast_with_terrain::clear_intersection_lists)
                .before(CollisionCheck),
        );

        app.add_system_set_to_stage(
            PhysicsPlugin,
            SystemSet::new()
                .with_system(cylinder_to_cylinder::check_for_intersections)
                .with_system(cylinder_to_terrain::check_for_intersections)
                .with_system(terrain_to_terrain::check_for_intersections)
                .with_system(ray_cast_with_terrain::check_for_intersections)
                .after(update_movement)
                .label(CollisionCheck),
        );

        app.add_system_set_to_stage(
            PhysicsPlugin,
            SystemSet::new()
                .with_system(insert_spatial_hash)
                // .with_system(add_spatial_hash_entities_to_tracker)
                .with_system(update_spatial_hash_entities)
                .with_system(update_spatial_hash_entities_with_offset)
                .with_system(
                    handle_removed_spatial_hash_entities
                        .after(update_spatial_hash_entities)
                        .after(update_spatial_hash_entities_with_offset),
                )
                .with_system(update_transforms)
                .with_system(ray_cast_with_terrain::debug_render)
                .after(update_movement)
                .after(CollisionCheck),
        );
    }
}
