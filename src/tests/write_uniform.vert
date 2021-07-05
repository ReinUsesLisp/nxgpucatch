#version 450

layout(binding = 0) uniform Input {
    uint value;
};
layout(binding = 0) buffer Result {
    uint result;
};

void main() {
    result = value;
}
