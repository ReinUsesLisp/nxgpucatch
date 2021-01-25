#pragma once

#include <deko3d.hpp>

class Shader {
public:
    explicit Shader(const char* code);

    void Bind(dk::CmdBuf& cmdbuf, uint32_t stage_flags) const {
        cmdbuf.bindShaders(stage_flags, {get()});
    }

    [[nodiscard]] const DkShader* get() const noexcept {
        return &shader;
    }

private:
    dk::UniqueMemBlock heap;
    dk::Shader shader;
};
