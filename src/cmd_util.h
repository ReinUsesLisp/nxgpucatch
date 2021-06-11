#pragma once

#include <stdexcept>

#include <deko3d.hpp>

#include "device.h"

void RecordRunWait(auto&& func) {
    static constexpr size_t MAX_SIZE{64 * 1024};
    if (queue.isInErrorState()) {
        throw std::runtime_error{"Queue in error state"};
    }
    dk::UniqueCmdBuf cmdbuf{dk::CmdBufMaker{device}.create()};
    dk::UniqueMemBlock heap{dk::MemBlockMaker{device, MAX_SIZE}.create()};
    cmdbuf.addMemory(heap, 0, MAX_SIZE);
    func(cmdbuf);
    queue.submitCommands(cmdbuf.finishList());
    queue.flush();
    queue.waitIdle();
    if (queue.isInErrorState()) {
        throw std::runtime_error{"Queue in error state"};
    }
}

inline void SetRenderTarget(const auto& render_target) {
    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        render_target.Bind(cmdbuf);
        cmdbuf.setViewports(0, {{0.0f, 0.0f, static_cast<float>(render_target.Width()),
                                 static_cast<float>(render_target.Height()), 0.0f, 1.0f}});
        cmdbuf.setScissors(0, {{0, 0, render_target.Width(), render_target.Height()}});
    });
}

inline void ResetState() {
    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        dk::RasterizerState rasterizer_state;
        dk::ColorState color_state;
        dk::ColorWriteState color_write_state;
        cmdbuf.bindRasterizerState(rasterizer_state);
        cmdbuf.bindColorState(color_state);
        cmdbuf.bindColorWriteState(color_write_state);
    });
}

inline void BindShaders(const auto&... shaders) {
    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        for (const auto shader : {&shaders...}) {
            shader->Bind(cmdbuf, DkStageFlag_GraphicsMask);
        }
    });
}

inline void Draw(DkPrimitive primitive, uint32_t num_vertices, uint32_t num_instances = 1,
                 uint32_t first_vertex = 0, uint32_t first_instance = 0) {
    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        cmdbuf.draw(primitive, num_vertices, num_instances, first_vertex, first_instance);
    });
}
