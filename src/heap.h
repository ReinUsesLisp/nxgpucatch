#pragma once

#include <cstdint>
#include <cstring>

#include <deko3d.hpp>

#include "alignment.h"
#include "device.h"

class Heap {
public:
    explicit Heap(size_t size, uint32_t flags)
        : heap{dk::MemBlockMaker{device, AlignUp<uint32_t>(size, DK_MEMBLOCK_ALIGNMENT)}
                   .setFlags(flags | DkMemBlockFlags_ZeroFillInit)
                   .create()} {}

    [[nodiscard]] DkMemBlock MemBlock() const noexcept {
        return heap;
    }

    [[nodiscard]] DkGpuAddr GpuAddr() const noexcept {
        return heap.getGpuAddr();
    }

    [[nodiscard]] void* CpuAddr() const noexcept {
        return heap.getCpuAddr();
    }

    [[nodiscard]] uint32_t Size() const noexcept {
        return heap.getSize();
    }

    void BindUniform(dk::CmdBuf& cmdbuf, DkStage stage, uint32_t id = 0) {
        cmdbuf.bindUniformBuffer(stage, id, GpuAddr(), Size());
    }

    void BindStorage(dk::CmdBuf& cmdbuf, DkStage stage, uint32_t id = 0) {
        cmdbuf.bindStorageBuffer(stage, id, GpuAddr(), Size());
    }

private:
    mutable dk::UniqueMemBlock heap;
};

template <typename T>
class TypedHeap : public Heap {
public:
    explicit TypedHeap()
        : Heap{sizeof(T), DkMemBlockFlags_CpuUncached | DkMemBlockFlags_GpuCached} {}

    TypedHeap(const T& data) : TypedHeap{} {
        std::memcpy(CpuAddr(), &data, sizeof(data));
    }

    T& operator*() noexcept {
        return *static_cast<T*>(CpuAddr());
    }

    T* operator->() noexcept {
        return static_cast<T*>(CpuAddr());
    }

    template <typename Index>
    auto& operator[](const Index& index) {
        return (*static_cast<T*>(CpuAddr()))[index];
    }
};
