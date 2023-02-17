#version 450 core

layout(location=0) in vec3 in_pos;
layout(location=1) in vec2 in_tex;

layout(location=0) out vec2 out_tex;

void main() {
  out_tex = in_tex;
}
