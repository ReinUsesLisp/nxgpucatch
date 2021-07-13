#version 330

void main() {
    if (gl_FragCoord.y > 32) {
        gl_FragColor = vec4(1, 0, 0, 1);
    } else {
        gl_FragColor = vec4(0, 1, 0, 1);
    }
}
