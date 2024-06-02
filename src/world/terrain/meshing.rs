use super::{terrain_space, Block, BlockDirection, BlockNeighborSet, BlockRegistry, Chunk};
use avian3d::prelude::*;
use bevy::{
    prelude::*,
    render::{
        mesh::{Indices, VertexAttributeValues},
        render_asset::RenderAssetUsages,
        render_resource::PrimitiveTopology,
    },
};

trait MeshBuilder {
    fn next_index(&self) -> usize;

    fn add_face(
        &mut self,
        verticies: impl IntoIterator<Item = Vec3>,
        normals: impl IntoIterator<Item = Vec3>,
        uvs: impl IntoIterator<Item = Vec2>,
        indices: impl IntoIterator<Item = u32>,
    );
}

trait AddToMesh {
    fn add_to_mesh(
        &self,
        block_registry: &BlockRegistry,
        neighbors: BlockNeighborSet,
        offset: Vec3,
        mesh_builder: &mut impl MeshBuilder,
    );
}

impl AddToMesh for Block {
    fn add_to_mesh(
        &self,
        block_registry: &BlockRegistry,
        neighbors: BlockNeighborSet,
        offset: Vec3,
        mesh_builder: &mut impl MeshBuilder,
    ) {
        let block_data = block_registry
            .get(self)
            .expect("Did not find self in registry.");

        for direction in BlockDirection::ALL {
            let neighbor = neighbors.get(direction);

            // We have to draw this face.
            if neighbor.is_none() {
                insert_face(direction, offset, mesh_builder);
            }
        }
    }
}

const FACE_VERTICIES: [[Vec3; 4]; 6] = [
    // Top
    [
        Vec3::new(1.0, 1.0, 0.0),
        Vec3::new(0.0, 1.0, 0.0),
        Vec3::new(0.0, 1.0, 1.0),
        Vec3::new(1.0, 1.0, 1.0),
    ],
    // Bottom
    [
        Vec3::new(1.0, 0.0, 1.0),
        Vec3::new(0.0, 0.0, 1.0),
        Vec3::new(0.0, 0.0, 0.0),
        Vec3::new(1.0, 0.0, 0.0),
    ],
    // East
    [
        Vec3::new(1.0, 0.0, 0.0),
        Vec3::new(1.0, 1.0, 0.0),
        Vec3::new(1.0, 1.0, 1.0),
        Vec3::new(1.0, 0.0, 1.0),
    ],
    // West
    [
        Vec3::new(0.0, 0.0, 1.0),
        Vec3::new(0.0, 1.0, 1.0),
        Vec3::new(0.0, 1.0, 0.0),
        Vec3::new(0.0, 0.0, 0.0),
    ],
    // North
    [
        Vec3::new(0.0, 0.0, 1.0),
        Vec3::new(1.0, 0.0, 1.0),
        Vec3::new(1.0, 1.0, 1.0),
        Vec3::new(0.0, 1.0, 1.0),
    ],
    // South
    [
        Vec3::new(0.0, 1.0, 0.0),
        Vec3::new(1.0, 1.0, 0.0),
        Vec3::new(1.0, 0.0, 0.0),
        Vec3::new(0.0, 0.0, 0.0),
    ],
];

const FACE_NORMALS: [[Vec3; 4]; 6] = [
    // Top
    [
        Vec3::new(0.0, 1.0, 0.0),
        Vec3::new(0.0, 1.0, 0.0),
        Vec3::new(0.0, 1.0, 0.0),
        Vec3::new(0.0, 1.0, 0.0),
    ],
    // Bottom
    [
        Vec3::new(0.0, -1.0, 0.0),
        Vec3::new(0.0, -1.0, 0.0),
        Vec3::new(0.0, -1.0, 0.0),
        Vec3::new(0.0, -1.0, 0.0),
    ],
    // East
    [
        Vec3::new(1.0, 0.0, 0.0),
        Vec3::new(1.0, 0.0, 0.0),
        Vec3::new(1.0, 0.0, 0.0),
        Vec3::new(1.0, 0.0, 0.0),
    ],
    // West
    [
        Vec3::new(-1.0, 0.0, 0.0),
        Vec3::new(-1.0, 0.0, 0.0),
        Vec3::new(-1.0, 0.0, 0.0),
        Vec3::new(-1.0, 0.0, 0.0),
    ],
    // North
    [
        Vec3::new(0.0, 0.0, 1.0),
        Vec3::new(0.0, 0.0, 1.0),
        Vec3::new(0.0, 0.0, 1.0),
        Vec3::new(0.0, 0.0, 1.0),
    ],
    // South
    [
        Vec3::new(0.0, 0.0, -1.0),
        Vec3::new(0.0, 0.0, -1.0),
        Vec3::new(0.0, 0.0, -1.0),
        Vec3::new(0.0, 0.0, -1.0),
    ],
];

const FACE_UV_COORDINATES: [[Vec2; 4]; 6] = [
    // Top
    [
        Vec2::new(0.0, 0.0),
        Vec2::new(1.0, 0.0),
        Vec2::new(1.0, 1.0),
        Vec2::new(0.0, 1.0),
    ],
    // Bottom
    [
        Vec2::new(0.0, 0.0),
        Vec2::new(1.0, 0.0),
        Vec2::new(1.0, 1.0),
        Vec2::new(0.0, 1.0),
    ],
    // East
    [
        Vec2::new(0.0, 0.0),
        Vec2::new(1.0, 0.0),
        Vec2::new(1.0, 1.0),
        Vec2::new(0.0, 1.0),
    ],
    // West
    [
        Vec2::new(0.0, 0.0),
        Vec2::new(1.0, 0.0),
        Vec2::new(1.0, 1.0),
        Vec2::new(0.0, 1.0),
    ],
    // North
    [
        Vec2::new(0.0, 0.0),
        Vec2::new(1.0, 0.0),
        Vec2::new(1.0, 1.0),
        Vec2::new(0.0, 1.0),
    ],
    // South
    [
        Vec2::new(0.0, 0.0),
        Vec2::new(1.0, 0.0),
        Vec2::new(1.0, 1.0),
        Vec2::new(0.0, 1.0),
    ],
];

fn insert_face(direction: BlockDirection, offset: Vec3, mesh_builder: &mut impl MeshBuilder) {
    // if matches!(direction, BlockDirection::Up) {
    let index = match direction {
        BlockDirection::Up => 0,
        BlockDirection::Down => 1,
        BlockDirection::East => 2,
        BlockDirection::West => 3,
        BlockDirection::North => 4,
        BlockDirection::South => 5,
    };

    let starting_index = mesh_builder.next_index() as u32;

    let vertex_iter = FACE_VERTICIES[index].iter().map(|v| *v + offset);
    let normal_iter = FACE_NORMALS[index];
    let uv_iter = FACE_UV_COORDINATES[index];
    let index_iter = [
        starting_index,
        starting_index + 1,
        starting_index + 2,
        starting_index + 2,
        starting_index + 3,
        starting_index,
    ];

    mesh_builder.add_face(vertex_iter, normal_iter, uv_iter, index_iter);
}

#[derive(Component)]
#[component(storage = "SparseSet")]
pub struct UpdateMesh;

/// A unique type to store a handle to the terrain material as a resource.
#[derive(Resource)]
pub struct TerrainMaterialHandle(Handle<StandardMaterial>);

#[allow(clippy::complexity)]
fn generate_chunk_visual_mesh(
    mut commands: Commands,
    block_registry: Res<BlockRegistry>,
    terrain_material: Res<TerrainMaterialHandle>,
    chunks: Query<(Entity, &Chunk), (With<UpdateMesh>, Changed<Chunk>)>,
    mut meshes: ResMut<Assets<Mesh>>,
) {
    for (entity, chunk) in chunks.iter() {
        let chunk_mesh = build_visual(chunk, &block_registry);

        commands
            .entity(entity)
            .insert((meshes.add(chunk_mesh), terrain_material.0.clone()));
    }
}

fn remove_chunk_visual_mesh(mut commands: Commands, mut removed: RemovedComponents<Chunk>) {
    for entity in removed.read() {
        // Sometimes the whole entity was removed.
        if let Some(mut commands) = commands.get_entity(entity) {
            commands.remove::<Handle<Mesh>>();
        }
    }
}

#[allow(clippy::complexity)]
fn generate_chunk_physical_mesh(
    mut commands: Commands,
    block_registry: Res<BlockRegistry>,
    chunks: Query<(Entity, &Chunk), (With<UpdateMesh>, Changed<Chunk>)>,
) {
    for (entity, chunk) in chunks.iter() {
        let chunk_collider = build_physical(chunk, &block_registry);

        commands.entity(entity).insert(chunk_collider);
    }
}

fn remove_chunk_physical_mesh(mut commands: Commands, mut removed: RemovedComponents<Chunk>) {
    for entity in removed.read() {
        // Sometimes the whole entity was removed.
        if let Some(mut commands) = commands.get_entity(entity) {
            commands.remove::<Collider>();
        }
    }
}

fn build_mesh(chunk: &Chunk, block_registry: &BlockRegistry, mesh_builder: &mut impl MeshBuilder) {
    for (position, block) in chunk.iter() {
        if let Some(block) = block {
            let neighbors = BlockNeighborSet {
                up: chunk.get_neighbor(position, BlockDirection::Up),
                down: chunk.get_neighbor(position, BlockDirection::Down),
                north: chunk.get_neighbor(position, BlockDirection::North),
                south: chunk.get_neighbor(position, BlockDirection::South),
                east: chunk.get_neighbor(position, BlockDirection::East),
                west: chunk.get_neighbor(position, BlockDirection::West),
            };

            block.add_to_mesh(block_registry, neighbors, position.as_vec3(), mesh_builder);
        }
    }
}

fn build_visual(chunk: &Chunk, block_registry: &BlockRegistry) -> Mesh {
    #[derive(Default)]
    struct VisualMeshBuilder {
        vertex_buffer: Vec<Vec3>,
        normal_buffer: Vec<Vec3>,
        uv_buffer: Vec<Vec2>,
        index_buffer: Vec<u32>,
    }

    impl MeshBuilder for VisualMeshBuilder {
        fn next_index(&self) -> usize {
            self.vertex_buffer.len()
        }

        fn add_face(
            &mut self,
            verticies: impl IntoIterator<Item = Vec3>,
            normals: impl IntoIterator<Item = Vec3>,
            uvs: impl IntoIterator<Item = Vec2>,
            indices: impl IntoIterator<Item = u32>,
        ) {
            self.vertex_buffer.extend(verticies);
            self.normal_buffer.extend(normals);
            self.uv_buffer.extend(uvs);
            self.index_buffer.extend(indices);
        }
    }

    let mut mesh_builder = VisualMeshBuilder::default();

    build_mesh(chunk, block_registry, &mut mesh_builder);

    let mut mesh = Mesh::new(PrimitiveTopology::TriangleList, RenderAssetUsages::all());
    mesh.insert_attribute(
        Mesh::ATTRIBUTE_POSITION,
        VertexAttributeValues::from(mesh_builder.vertex_buffer),
    );
    mesh.insert_attribute(
        Mesh::ATTRIBUTE_NORMAL,
        VertexAttributeValues::from(mesh_builder.normal_buffer),
    );
    mesh.insert_attribute(
        Mesh::ATTRIBUTE_UV_0,
        VertexAttributeValues::from(mesh_builder.uv_buffer),
    );

    mesh.insert_indices(Indices::U32(mesh_builder.index_buffer));

    mesh
}

fn build_physical(chunk: &Chunk, block_registry: &BlockRegistry) -> Collider {
    #[derive(Default)]
    struct ColliderMeshBuilder {
        vertex_buffer: Vec<Vec3>,
        index_buffer: Vec<[u32; 3]>,
    }

    impl MeshBuilder for ColliderMeshBuilder {
        fn next_index(&self) -> usize {
            self.vertex_buffer.len()
        }

        fn add_face(
            &mut self,
            verticies: impl IntoIterator<Item = Vec3>,
            _normals: impl IntoIterator<Item = Vec3>,
            _uvs: impl IntoIterator<Item = Vec2>,
            indices: impl IntoIterator<Item = u32>,
        ) {
            self.vertex_buffer.extend(verticies);

            let mut indices = indices.into_iter();
            let indices: [u32; 3] = std::array::from_fn(|_| indices.next().unwrap());
            self.index_buffer.push(indices);
        }
    }

    let mut mesh_builder = ColliderMeshBuilder::default();

    build_mesh(chunk, block_registry, &mut mesh_builder);

    Collider::trimesh(mesh_builder.vertex_buffer, mesh_builder.index_buffer)
}

pub fn register(app: &mut App) {
    app.add_systems(
        Startup,
        |mut commands: Commands,
         mut materials: ResMut<Assets<StandardMaterial>>,
         asset_server: Res<AssetServer>| {
            let texture = asset_server.load("terrain/default-color.png");

            let terrain_material = materials.add(StandardMaterial::from(texture));
            let terrain_material = TerrainMaterialHandle(terrain_material);
            commands.insert_resource(terrain_material);
        },
    );

    app.add_systems(
        Update,
        (
            generate_chunk_visual_mesh.after(terrain_space::ModifyTerrain),
            remove_chunk_visual_mesh.after(terrain_space::ModifyTerrain),
            generate_chunk_physical_mesh.after(terrain_space::ModifyTerrain),
            remove_chunk_physical_mesh.after(terrain_space::ModifyTerrain),
        ),
    );
}
