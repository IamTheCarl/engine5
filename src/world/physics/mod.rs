use crate::{config::graphics::DebugRenderSettings, world::terrain::LoadTerrain, AppState};
use bevy::{prelude::*, render::render_resource::PrimitiveTopology};
use ordered_float::NotNan;
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, num::NonZeroUsize};

mod cylinder_to_cylinder;
mod cylinder_to_terrain;
mod ray_cast_with_terrain;
pub use ray_cast_with_terrain::{
    RayTerrainIntersection, RayTerrainIntersectionList, RayTerrainIntersectionType,
};

use super::WorldState;
mod terrain_to_terrain;

#[derive(Component, Reflect, Default)]
#[reflect(Component)]
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
    cylinders: HashMap<(NotNan<f32>, NotNan<f32>), Handle<Mesh>>,
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
        cylinder: &Cylinder,
    ) -> Handle<Mesh> {
        self.cylinders
            .entry((
                NotNan::new(cylinder.height).expect("Cylinder height was NaN"),
                NotNan::new(cylinder.radius).expect("Cylinder radius was NaN"),
            ))
            .or_insert_with(|| {
                let mut mesh = Mesh::new(PrimitiveTopology::LineList);
                let mut vertices = Vec::new();

                const INDEXES: i32 = 16;
                const FULL_ROTATION: f32 = std::f64::consts::PI as f32 * 2.0;
                const ROTATION_SLICE: f32 = FULL_ROTATION / INDEXES as f32;

                let diameter = cylinder.radius;
                let height = cylinder.height;

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

#[derive(Component, Reflect, Default, Serialize, Deserialize, Debug, Clone)]
#[reflect(Component)]
pub struct Velocity {
    pub translation: Vec3,
    pub rotational: f32,
}

#[derive(Component, Serialize, Deserialize, Debug, Clone)]
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

#[derive(Component, Reflect, Debug, Default)]
#[reflect(Component)]
pub struct Cylinder {
    pub height: f32,
    pub radius: f32,
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
            let mesh = mesh_collections.get_cylinder(&mut meshes, cylinder);
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

/// Calculates the global transform for an entity by climbing up the hierarchy of transforms.
/// entity - The entity we wish to calculate the global transform of.
/// transform_getter - a closure to fetch an entity's transform and its parent.
pub fn calculate_global_transform<'a>(
    entity: Entity,
    transform_getter: impl Fn(Entity) -> (Option<Entity>, &'a Transform),
) -> Transform {
    let mut next_entity = entity;
    let mut calculated_transform = Transform::default();

    loop {
        let (parent, transform) = transform_getter(next_entity);
        calculated_transform = transform.mul_transform(calculated_transform);

        if let Some(parent) = parent {
            next_entity = parent;
        } else {
            // Looks like that's the last one.
            break;
        }
    }

    calculated_transform
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, SystemSet)]
pub struct CollisionCheck;

#[derive(Debug, PartialEq, Eq, Hash, Clone, SystemSet)]
pub struct PostCollisionCheck;

#[derive(Debug, PartialEq, Eq, Hash, Clone, SystemSet)]
pub struct UpdateSpatialHashes;

#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
pub struct PhysicsPlugin;

impl Plugin for PhysicsPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(
            Startup,
            |mut commands: Commands, mut materials: ResMut<Assets<StandardMaterial>>| {
                let debug_shader_material = materials.add(Color::RED.into());
                commands.insert_resource(DebugShaderMaterial(debug_shader_material));

                commands.insert_resource(MeshCollection::new());
            },
        );

        app.configure_set(
            Update,
            PhysicsPlugin
                .after(LoadTerrain)
                .run_if(in_state(AppState::InGame))
                .run_if(in_state(WorldState::Running)),
        );

        app.add_systems(Update, add_debug_mesh_cylinders.in_set(PhysicsPlugin));
        app.add_systems(Update, remove_debug_mesh_cylinders.in_set(PhysicsPlugin));

        // TODO we should consider tieing this to a fixed time step.
        app.add_systems(Update, update_movement.in_set(PhysicsPlugin));

        // TODO we should take advantage of the "Modified" tag so we only do collision checks on things that actually move.
        app.configure_set(
            Update,
            CollisionCheck.after(update_movement).in_set(PhysicsPlugin),
        );
        app.add_systems(
            Update,
            (
                cylinder_to_cylinder::check_for_intersections.in_set(CollisionCheck),
                cylinder_to_terrain::check_for_intersections.in_set(CollisionCheck),
                terrain_to_terrain::check_for_intersections.in_set(CollisionCheck),
            ),
        );

        app.configure_set(
            Update,
            PostCollisionCheck
                .after(CollisionCheck)
                .in_set(PhysicsPlugin),
        );
        app.add_systems(
            Update,
            (
                ray_cast_with_terrain::check_for_intersections.in_set(PostCollisionCheck),
                ray_cast_with_terrain::debug_render
                    .after(ray_cast_with_terrain::check_for_intersections)
                    .in_set(PostCollisionCheck),
                update_transforms.in_set(PostCollisionCheck),
            ),
        );
    }
}
