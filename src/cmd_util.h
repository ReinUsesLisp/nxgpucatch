#pragma once

#include <stdexcept>

#include <deko3d.hpp>

#include "device.h"

template <typename Func>
void RecordRunWait(Func&& func) {
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
