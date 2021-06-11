#include <cstddef>
#include <cstdint>
#include <cstring>
#include <vector>

#include <deko3d.hpp>
#include <nxas.h>

#include "alignment.h"
#include "device.h"
#include "shader.h"

Shader::Shader(std::span<const unsigned char> dksh) {
    Init(dksh);
}

Shader::Shader(const char* code) {
    const std::vector<uint64_t> dksh{nxas::assemble(code)};
    const size_t size_bytes{dksh.size() * sizeof(uint64_t)};
    const std::span dksh_span(reinterpret_cast<const unsigned char*>(dksh.data()), size_bytes);
    Init(dksh_span);
}

void Shader::Init(std::span<const unsigned char> dksh) {
    heap = dk::MemBlockMaker{device,
                             AlignUp<uint32_t>(dksh.size_bytes() + 0x4000, DK_MEMBLOCK_ALIGNMENT)}
               .setFlags(DkMemBlockFlags_CpuUncached | DkMemBlockFlags_GpuCached |
                         DkMemBlockFlags_Code | DkMemBlockFlags_ZeroFillInit)
               .create();
    std::memcpy(heap.getCpuAddr(), dksh.data(), dksh.size_bytes());

    dk::ShaderMaker{heap, 0}.initialize(shader);
}
