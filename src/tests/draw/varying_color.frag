#version 330
#extension GL_ARB_separate_shader_objects : require

layout(location = 0) flat in vec4 color;

void main() {
    gl_FragColor = color;
}
