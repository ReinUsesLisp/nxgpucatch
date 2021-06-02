#pragma once

#include <deko3d.hpp>

#include "heap.h"
#include "resource.h"

class DescriptorSet {
public:
    void Bind(dk::CmdBuf& cmdbuf) {
        cmdbuf.bindImageDescriptorSet(image_heap.GpuAddr(), 512);
        cmdbuf.bindSamplerDescriptorSet(sampler_heap.GpuAddr(), 512);
    }

    void PushImageView(dk::CmdBuf& cmdbuf, dk::ImageView image_view, size_t index) {
        dk::ImageDescriptor image_descriptor;
        image_descriptor.initialize(image_view);
        
        const size_t size = sizeof(image_descriptor);
        const size_t offset = size * index;
        cmdbuf.pushData(image_heap.GpuAddr() + offset, &image_descriptor, size);
    }

    void PushTexture(dk::CmdBuf& cmdbuf, const Texture& texture, size_t index) {
        dk::ImageDescriptor image_descriptor;
        image_descriptor.initialize(texture.ImageView());

        dk::SamplerDescriptor sampler_descriptor;
        sampler_descriptor.initialize(texture.Sampler());

        const size_t size = sizeof(image_descriptor);
        const size_t offset = size * index;
        cmdbuf.pushData(image_heap.GpuAddr() + offset, &image_descriptor, size);
        cmdbuf.pushData(sampler_heap.GpuAddr() + offset, &sampler_descriptor, size);
    }

private:
    Heap image_heap{64 * 1024, DkMemBlockFlags_CpuUncached | DkMemBlockFlags_GpuCached};
    Heap sampler_heap{64 * 1024, DkMemBlockFlags_CpuUncached | DkMemBlockFlags_GpuCached};
};
