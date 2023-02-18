#[repr(C)]
#[derive(Debug, Copy, Clone, bytemuck::Zeroable, bytemuck::Pod)]
pub struct Vertex {
    pos: glam::Vec3,
    tex: glam::Vec2,
}

pub fn create_vertices() -> (Vec<Vertex>, Vec<u16>) {
    let vertex_data = vec![
        // top-left
        Vertex {
            pos: glam::vec3(-1.0, -1.0, 1.0),
            tex: glam::vec2(0.0, 0.0),
        },
        // top-right
        Vertex {
            pos: glam::vec3(1.0, -1.0, 1.0),
            tex: glam::vec2(1.0, 0.0),
        },
        // bottom-left
        Vertex {
            pos: glam::vec3(1.0, 1.0, 1.0),
            tex: glam::vec2(1.0, 1.0),
        },
        // bottom-right
        Vertex {
            pos: glam::vec3(-1.0, 1.0, 1.0),
            tex: glam::vec2(0.0, 1.0),
        },
    ];

    let index_data = vec![0, 1, 2, 2, 3, 0];

    (vertex_data, index_data)
}
