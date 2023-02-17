#version 450 core

layout(location=0) in vec2 in_tex;

layout(location=0) out vec4 f_color;

layout(set=0, binding=0) uniform texture2D v_texture;
layout(set=0, binding=1) uniform sampler v_sampler;

void main() {
  f_color = texture(sampler2D(v_texture, v_sampler), in_tex);
}
