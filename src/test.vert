#version 330

const vec2 verts[6] = vec2[6] (
    vec2(0.0f, 1.0f),
    vec2(0.0f, 0.0f),
    vec2(1.0f, 0.0f),
    vec2(0.0f, 1.0f),
    vec2(1.0f, 0.0f),
    vec2(1.0f, 1.0f)
);

const vec3 colors[6] = vec3[6] (
    vec3(1.0, 0.0, 0.0),
    vec3(0.0, 1.0, 0.0),
    vec3(0.0, 0.0, 1.0),
    vec3(1.0, 0.0, 0.0),
    vec3(0.0, 0.0, 1.0),
    vec3(0.0, 1.0, 0.0)
);

uniform vec2 offset;
uniform vec2 scale;

out vec2 vert;
out vec3 in_color;

void main() {
    vert = verts[gl_VertexID];
    in_color = colors[gl_VertexID];
    gl_Position = vec4(((vert * scale * 2.0 + vec2(offset.x, -offset.y) * 2.0) + vec2(-1.0, 1.0 - scale.y * 2.0)), 0.0, 1.0);
}
