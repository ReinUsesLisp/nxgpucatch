#include <cstddef>
#include <cstdint>
#include <cstring>
#include <vector>

#include <deko3d.hpp>
#include <nxas.h>

#include "alignment.h"
#include "device.h"
#include "shader.h"

Shader::Shader(const char* code) {
    const std::vector<uint64_t> dksh{nxas::assemble(code)};
    const size_t size{dksh.size() * sizeof(uint64_t)};

    heap = dk::MemBlockMaker{device, AlignUp<uint32_t>(size, DK_MEMBLOCK_ALIGNMENT)}
               .setFlags(DkMemBlockFlags_CpuUncached | DkMemBlockFlags_GpuCached |
                         DkMemBlockFlags_Code | DkMemBlockFlags_ZeroFillInit)
               .create();
    std::memcpy(heap.getCpuAddr(), dksh.data(), size);

    dk::ShaderMaker{heap, 0}.initialize(shader);
}
