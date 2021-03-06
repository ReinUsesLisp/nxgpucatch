#version 450

layout(location = 0) in vec4 in_passtru;

layout(location = 0) out vec4 out_passtru;

void main() {
    float x = -1.0 + float((gl_VertexID & 1) << 2);
    float y = -1.0 + float((gl_VertexID & 2) << 1);
    gl_Position = vec4(x, y, 0, 1);

    out_passtru = in_passtru;
}
