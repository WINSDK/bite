// Vertex shader
struct Globals {
    window_size: vec2<f32>,
}

@group(0) @binding(0) var<uniform> globals: Globals;

struct VertexInput {
    @location(0) position: vec3<f32>,
};

struct InstanceInput {
    @location(1) offset: vec2<f32>,
    @location(2) size: vec2<f32>,
    @location(3) color: vec3<f32>,
}

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) color: vec3<f32>,
};

@vertex
fn vs_main(
    model: VertexInput,
    instance: InstanceInput,
) -> VertexOutput {
    var out: VertexOutput;
    out.color = instance.color;

    var offset = instance.offset / globals.window_size
    * vec2<f32>(2.0, -2.0) - vec2<f32>(1.0, -1.0);

    var pos: vec2<f32> = model.position.xy * instance.size / globals.window_size
    * vec2<f32>(2.0, 2.0);
    pos += offset;
    /* pos += instance.offset / globals.window_size * vec2<f32>(1.0, -1.0); */
    /* pos -= vec2<f32>(0.5, -0.5); */
    out.clip_position = vec4<f32>(pos, 0.0, 1.0);

    /* out.clip_position = vec4<f32>(model.position * */
    /*                     vec3<f32>(instance.size / globals.window_size, 0.0) + */
    /*                     vec3<f32>(instance.offset / globals.window_size * */
    /*                     vec2<f32>(1.0, -1.0) */
    /*                     - vec2<f32>(0.5, -0.5), 0.0), 1.0); */
    return out;
}

// Fragment shader

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    return vec4<f32>(in.color, 1.0);
}
