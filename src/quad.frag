#version 330

uniform vec4 color;

in vec2 vert;

out vec4 out_color;

void main () {
    out_color = color;
}
