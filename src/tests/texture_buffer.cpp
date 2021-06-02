#include <catch2/catch_test_macros.hpp>

#include "cmd_util.h"
#include "descriptor_set.h"
#include "heap.h"
#include "resource.h"
#include "shader.h"

static Shader MakeShader(std::string code) {
    code = ".dksh compute\n"
           "main:\n" +
           code + "EXIT;\n";
    return Shader(code.c_str());
}

TEST_CASE("Texture buffer simple", "[texture_buffer]") {
    Texture texture(512, 1, 1, 1, DkImageType_Buffer);
    Shader stg_shader{MakeShader(R"(
        MOV R4, c[0x0][0x140];
        MOV R5, c[0x0][0x144];
        MOV32I R0, 0x00ffff00;
        STG.E [R4], R0;
    )")};
    Shader tld_shader{MakeShader(R"(
        MOV R4, c[0x0][0x140];
        MOV R5, c[0x0][0x144];
        MOV R0, 0;
        TLD.LZ PT, R0, R0, RZ, 0x28, 1D, 0xf;
        STG.E.128 [R4], R0;
    )")};
    DescriptorSet descriptor_set;
    TypedHeap<std::array<float, 4>> result_heap;

    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        cmdbuf.bindStorageBuffer(DkStage_Compute, 0, texture.GetImage().Heap().getGpuAddr(), 64);
        stg_shader.Bind(cmdbuf, DkStageFlag_Compute);
        cmdbuf.dispatchCompute(1, 1, 1);
        cmdbuf.barrier(DkBarrier_Full, DkInvalidateFlags_L2Cache);

        descriptor_set.Bind(cmdbuf);
        descriptor_set.PushTexture(cmdbuf, texture, 0);

        tld_shader.Bind(cmdbuf, DkStageFlag_Compute);
        result_heap.BindStorage(cmdbuf, DkStage_Compute, 0);
        cmdbuf.bindTextures(DkStage_Compute, 0, {dkMakeTextureHandle(0, 0)});
        cmdbuf.dispatchCompute(1, 1, 1);
    });
    REQUIRE(*result_heap == (std::array<float, 4>{0.0f, 1.0f, 1.0f, 0.0f}));
}

TEST_CASE("Texture buffer with texture offset", "[texture_buffer]") {
    dk::ImageLayout image_layout;
    dk::ImageLayoutMaker{device}
        .setDimensions(512, 1, 1)
        .setType(DkImageType_Buffer)
        .setFormat(DkImageFormat_RGBA8_Unorm)
        .initialize(image_layout);

    dk::UniqueMemBlock heap = dk::MemBlockMaker{device, DK_MEMBLOCK_ALIGNMENT * 4}
                                  .setFlags(DkMemBlockFlags_CpuUncached | DkMemBlockFlags_GpuCached)
                                  .create();
    dk::Image image;
    image.initialize(image_layout, heap, 0x80);

    dk::ImageView image_view{image};

    Shader stg_shader{MakeShader(R"(
        MOV R4, c[0x0][0x140];
        MOV R5, c[0x0][0x144];
        IADD R4, R4, 0x80;
        MOV32I R0, 0x00ffffff;
        STG.E [R4], R0;
    )")};
    Shader tld_shader{MakeShader(R"(
        MOV R4, c[0x0][0x140];
        MOV R5, c[0x0][0x144];
        MOV R0, 0;
        TLD.LZ PT, R0, R0, RZ, 0x28, 1D, 0xf;
        STG.E.128 [R4], R0;
    )")};
    DescriptorSet descriptor_set;
    TypedHeap<std::array<float, 4>> result_heap;

    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        cmdbuf.bindStorageBuffer(DkStage_Compute, 0, heap.getGpuAddr(), 64);
        stg_shader.Bind(cmdbuf, DkStageFlag_Compute);
        cmdbuf.dispatchCompute(1, 1, 1);
        cmdbuf.barrier(DkBarrier_Full, DkInvalidateFlags_L2Cache);

        descriptor_set.Bind(cmdbuf);
        descriptor_set.PushImageView(cmdbuf, image_view, 0);

        tld_shader.Bind(cmdbuf, DkStageFlag_Compute);
        result_heap.BindStorage(cmdbuf, DkStage_Compute, 0);
        cmdbuf.bindTextures(DkStage_Compute, 0, {dkMakeTextureHandle(0, 0)});
        cmdbuf.dispatchCompute(1, 1, 1);
    });
    REQUIRE(*result_heap == (std::array<float, 4>{1.0f, 1.0f, 1.0f, 0.0f}));
}

TEST_CASE("Texture buffer uniform update", "[texture_buffer]") {
    Texture texture(512, 1, 1, 1, DkImageType_Buffer);
    Shader stg_shader{MakeShader(R"(
        MOV R4, c[0x0][0x140];
        MOV R5, c[0x0][0x144];
        MOV32I R0, 0x00ffff00;
        STG.E [R4], R0;
    )")};
    Shader tld_shader{MakeShader(R"(
        MOV R4, c[0x0][0x140];
        MOV R5, c[0x0][0x144];
        MOV R0, 0;
        TLD.LZ PT, R0, R0, RZ, 0x28, 1D, 0xf;
        STG.E.128 [R4], R0;
    )")};
    DescriptorSet descriptor_set;
    TypedHeap<std::array<float, 4>> result_heap;

    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        DkGpuAddr gpu_addr = texture.GetImage().Heap().getGpuAddr();
        cmdbuf.bindStorageBuffer(DkStage_Compute, 0, gpu_addr, 64);
        stg_shader.Bind(cmdbuf, DkStageFlag_Compute);
        cmdbuf.dispatchCompute(1, 1, 1);
        cmdbuf.barrier(DkBarrier_Full, DkInvalidateFlags_L2Cache);

        uint32_t value = 0xffffff00;
        cmdbuf.pushConstants(gpu_addr, DK_UNIFORM_BUF_ALIGNMENT, 0, 4, &value);

        descriptor_set.Bind(cmdbuf);
        descriptor_set.PushTexture(cmdbuf, texture, 0);

        tld_shader.Bind(cmdbuf, DkStageFlag_Compute);
        result_heap.BindStorage(cmdbuf, DkStage_Compute, 0);
        cmdbuf.bindTextures(DkStage_Compute, 0, {dkMakeTextureHandle(0, 0)});
        cmdbuf.dispatchCompute(1, 1, 1);
    });
    REQUIRE(*result_heap == (std::array<float, 4>{0.0f, 1.0f, 1.0f, 1.0f}));
}

TEST_CASE("Texture buffer robustness", "[texture_buffer]") {
    Texture texture(3, 1, 1, 1, DkImageType_Buffer);
    Shader tld_shader{MakeShader(R"(
        MOV R4, c[0x0][0x140];
        MOV R5, c[0x0][0x144];
        MOV R0, c[2][0];
        TLD.LZ PT, R0, R0, RZ, 0x28, 1D, 0xf;
        STG.E.128 [R4], R0;
    )")};
    DescriptorSet descriptor_set;
    TypedHeap<std::array<float, 4>> result_heap;
    TypedHeap<uint32_t> offset_cbuf;

    auto tex_data = static_cast<std::array<uint8_t, 4>*>(texture.CpuAddr());
    tex_data[0] = {255, 0, 0, 0};
    tex_data[1] = {0, 255, 0, 0};
    tex_data[2] = {0, 0, 255, 0};
    tex_data[3] = {0, 0, 0, 255};

    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        cmdbuf.bindUniformBuffer(DkStage_Compute, 0, offset_cbuf.GpuAddr(), 256);

        descriptor_set.Bind(cmdbuf);
        descriptor_set.PushTexture(cmdbuf, texture, 0);

        tld_shader.Bind(cmdbuf, DkStageFlag_Compute);
        result_heap.BindStorage(cmdbuf, DkStage_Compute, 0);
        cmdbuf.bindTextures(DkStage_Compute, 0, {dkMakeTextureHandle(0, 0)});
    });

    *offset_cbuf = 0;
    RecordRunWait([&](dk::CmdBuf cmdbuf) { cmdbuf.dispatchCompute(1, 1, 1); });
    REQUIRE(*result_heap == (std::array<float, 4>{1.0f, 0.0f, 0.0f, 0.0f}));

    *offset_cbuf = 1;
    RecordRunWait([&](dk::CmdBuf cmdbuf) { cmdbuf.dispatchCompute(1, 1, 1); });
    REQUIRE(*result_heap == (std::array<float, 4>{0.0f, 1.0f, 0.0f, 0.0f}));

    *offset_cbuf = 2;
    RecordRunWait([&](dk::CmdBuf cmdbuf) { cmdbuf.dispatchCompute(1, 1, 1); });
    REQUIRE(*result_heap == (std::array<float, 4>{0.0f, 0.0f, 1.0f, 0.0f}));

    *offset_cbuf = 3;
    RecordRunWait([&](dk::CmdBuf cmdbuf) { cmdbuf.dispatchCompute(1, 1, 1); });
    REQUIRE(*result_heap == (std::array<float, 4>{0.0f, 0.0f, 0.0f, 0.0f}));
}
