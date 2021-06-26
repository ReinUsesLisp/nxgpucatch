#pragma once

#include <span>

#include <deko3d.hpp>

class Shader {
public:
    explicit Shader(const char* code);
    explicit Shader(std::span<const unsigned char> dksh);

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
