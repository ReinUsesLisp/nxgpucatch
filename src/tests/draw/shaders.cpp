#include <fstream>
#include <string>
#include <vector>

#include "shaders.h"

Shader LoadShader(const char* name) {
    const std::string path = std::string("romfs:/shaders/") + name + ".dksh";
    std::ifstream file(path, std::ios::binary);
    if (!file.is_open()) {
        throw std::runtime_error("Shader in " + path + " not found");
    }
    file.seekg(0, std::ios::end);
    std::vector<unsigned char> code(file.tellg());
    file.seekg(0, std::ios::beg);
    file.read(reinterpret_cast<char*>(code.data()), code.size());
    return Shader(std::span<unsigned char>(code));
}
