#version 330

uniform vec4 color;

uniform sampler2D tex;

in vec2 vert;
in vec2 tex_coord;

out vec4 out_color;

const float smoothing = 1.0/16.0;

void main () {
    out_color.rgb = pow(color.rgb, vec3(2.2));
    out_color.a = max(texture(tex, tex_coord).r - 0.21, 0.0); //smoothstep(0.5 - smoothing, 0.5 + smoothing, texture(tex, tex_coord).r);
}
