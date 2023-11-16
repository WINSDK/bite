#version 450 core

layout(location=0) in vec3 in_pos;
layout(location=1) in vec2 in_tex_coards;

layout(location=0) out vec2 out_tex_coards;

void main() {
    out_tex_coards = in_tex_coards;
    gl_Position = vec4(in_pos, 1.0);
}
