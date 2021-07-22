#version 450

layout(binding = 0) uniform Foo {
    float depth;
};

void main() {
    float x = -1.0 + float((gl_VertexID & 1) << 2);
    float y = -1.0 + float((gl_VertexID & 2) << 1);
    gl_Position = vec4(x, y, depth, 1);
}
