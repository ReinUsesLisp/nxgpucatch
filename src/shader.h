#pragma once

#include <span>

#include <incbin.h>

#include <deko3d.hpp>

#define INCLUDE_SHADER(name, path)                                                                 \
    INCBIN(name, path ".dksh");                                                                    \
    static const auto name = std::span(g##name##Data, g##name##Size)

class Shader {
public:
    explicit Shader(std::span<const unsigned char> dksh);
    explicit Shader(const char* code);

    void Bind(dk::CmdBuf& cmdbuf, uint32_t stage_flags) const {
        cmdbuf.bindShaders(stage_flags, {get()});
    }

    [[nodiscard]] const DkShader* get() const noexcept {
        return &shader;
    }

private:
    void Init(std::span<const unsigned char> dksh);

    dk::UniqueMemBlock heap;
    dk::Shader shader;
};
