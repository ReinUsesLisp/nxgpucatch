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

TEST_CASE("Render linear R32 sample R8 SUST", "[texture_buffer]") {
    dk::ImageLayout r32_image_layout, r8_image_layout;
    dk::ImageLayoutMaker{device}
        .setDimensions(240, 540, 1)
        .setPitchStride(4096)
        .setType(DkImageType_2D)
        .setFormat(DkImageFormat_R32_Uint)
        .setFlags(DkImageFlags_PitchLinear)
        .initialize(r32_image_layout);
    dk::ImageLayoutMaker{device}
        .setDimensions(960, 540, 1)
        .setPitchStride(4096)
        .setType(DkImageType_2D)
        .setFormat(DkImageFormat_R8_Uint)
        .setFlags(DkImageFlags_PitchLinear)
        .initialize(r8_image_layout);

    uint32_t size = 4096 * 540;
    dk::UniqueMemBlock heap =
        dk::MemBlockMaker{device, size}
            .setFlags(DkMemBlockFlags_CpuUncached | DkMemBlockFlags_GpuCached |
                      DkMemBlockFlags_ZeroFillInit | DkMemBlockFlags_Image)
            .create();
    dk::Image r32_image, r8_image;
    r32_image.initialize(r32_image_layout, heap, 0);
    r8_image.initialize(r8_image_layout, heap, 0);

    dk::ImageView r32_image_view(r32_image);
    dk::ImageView r8_image_view(r8_image);

    Shader sust_shader{MakeShader(R"(
        MOV32I R0, 0xdeadbeef;
        MOV R1, RZ;
        MOV R2, RZ;
        MOV R3, RZ;
        MOV R6, c[2][0];
        MOV R7, c[2][4];
        SUST.P.2D.RGBA.IGN [R6], R0, 0x48;
    )")};
    Shader suld_shader{MakeShader(R"(
        MOV R4, c[0x0][0x140];
        MOV R5, c[0x0][0x144];
        MOV R6, c[2][0];
        MOV R7, c[2][4];
        SHL R6, R6, 2;
        SULD.P.2D.R.IGN R0, [R6], 0x48;
        IADD R6, R6, 1;
        SULD.P.2D.R.IGN R1, [R6], 0x48;
        IADD R6, R6, 1;
        SULD.P.2D.R.IGN R2, [R6], 0x48;
        IADD R6, R6, 1;
        SULD.P.2D.R.IGN R3, [R6], 0x48;
        STG.E.128 [R4], R0;
    )")};
    DescriptorSet descriptor_set;
    TypedHeap<std::array<uint32_t, 4>> result_heap;
    TypedHeap<std::pair<uint32_t, uint32_t>> base_coord_heap;
    std::pair<uint32_t, uint32_t> base_coord{};

    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        descriptor_set.Bind(cmdbuf);
        result_heap.BindStorage(cmdbuf, DkStage_Compute, 0);
        cmdbuf.bindImages(DkStage_Compute, 0, {dkMakeImageHandle(0)});
        cmdbuf.bindUniformBuffer(DkStage_Compute, 0, base_coord_heap.GpuAddr(), 256);
    });
    for (uint32_t coord = 0; coord < 240; coord += 8) {
        RecordRunWait([&](dk::CmdBuf cmdbuf) {
            base_coord.first = coord;
            base_coord.second = coord + 2;
            cmdbuf.pushConstants(base_coord_heap.GpuAddr(), 256, 0, sizeof(base_coord),
                                 &base_coord);

            descriptor_set.PushImageView(cmdbuf, r32_image_view, 0);
            sust_shader.Bind(cmdbuf, DkStageFlag_Compute);
            cmdbuf.dispatchCompute(1, 1, 1);

            cmdbuf.barrier(DkBarrier_Full, DkInvalidateFlags_Image);

            descriptor_set.PushImageView(cmdbuf, r8_image_view, 0);
            suld_shader.Bind(cmdbuf, DkStageFlag_Compute);
            cmdbuf.dispatchCompute(1, 1, 1);
        });
        REQUIRE(*result_heap == (std::array<uint32_t, 4>{0xef, 0xbe, 0xad, 0xde}));
        *result_heap = {};
    }
}

TEST_CASE("Render linear R32 sample R8", "[texture_buffer]") {
    dk::ImageLayout r32_image_layout, r8_image_layout;
    dk::ImageLayoutMaker{device}
        .setDimensions(240, 540, 1)
        .setPitchStride(4096)
        .setType(DkImageType_2D)
        .setFormat(DkImageFormat_R32_Uint)
        .setFlags(DkImageFlags_PitchLinear | DkImageFlags_UsageRender)
        .initialize(r32_image_layout);
    dk::ImageLayoutMaker{device}
        .setDimensions(960, 540, 1)
        .setPitchStride(4096)
        .setType(DkImageType_2D)
        .setFormat(DkImageFormat_R8_Uint)
        .setFlags(DkImageFlags_PitchLinear | DkImageFlags_UsageRender)
        .initialize(r8_image_layout);

    uint32_t size = 4096 * 540;
    dk::UniqueMemBlock heap =
        dk::MemBlockMaker{device, size}
            .setFlags(DkMemBlockFlags_CpuUncached | DkMemBlockFlags_GpuCached |
                      DkMemBlockFlags_ZeroFillInit | DkMemBlockFlags_Image)
            .create();
    dk::Image r32_image, r8_image;
    r32_image.initialize(r32_image_layout, heap, 0);
    r8_image.initialize(r8_image_layout, heap, 0);

    dk::ImageView r32_image_view(r32_image);
    dk::ImageView r8_image_view(r8_image);

    DkImageView* c_r32_image_view = &r32_image_view;

    Shader suld_shader{MakeShader(R"(
        MOV R4, c[0x0][0x140];
        MOV R5, c[0x0][0x144];
        MOV R6, c[2][0];
        MOV R7, c[2][4];
        SHL R6, R6, 2;
        SULD.P.2D.R.IGN R0, [R6], 0x48;
        IADD R6, R6, 1;
        SULD.P.2D.R.IGN R1, [R6], 0x48;
        IADD R6, R6, 1;
        SULD.P.2D.R.IGN R2, [R6], 0x48;
        IADD R6, R6, 1;
        SULD.P.2D.R.IGN R3, [R6], 0x48;
        STG.E.128 [R4], R0;
    )")};
    DescriptorSet descriptor_set;
    TypedHeap<std::array<uint32_t, 4>> result_heap;
    TypedHeap<std::pair<uint32_t, uint32_t>> base_coord_heap;
    std::pair<uint32_t, uint32_t> base_coord{};

    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        descriptor_set.Bind(cmdbuf);
        result_heap.BindStorage(cmdbuf, DkStage_Compute, 0);
        cmdbuf.bindImages(DkStage_Compute, 0, {dkMakeImageHandle(0)});
        cmdbuf.bindUniformBuffer(DkStage_Compute, 0, base_coord_heap.GpuAddr(), 256);
        descriptor_set.PushImageView(cmdbuf, r8_image_view, 0);
        suld_shader.Bind(cmdbuf, DkStageFlag_Compute);

        cmdbuf.bindRenderTargets(c_r32_image_view);
        cmdbuf.setScissors(0, {DkScissor{0, 0, 4096, 4096}});
        cmdbuf.clearColor(0, DkColorMask_RGBA, 0xdeadbeefu, 0u, 0u, 0u);
    });
    for (uint32_t coord = 0; coord < 240; coord += 8) {
        RecordRunWait([&](dk::CmdBuf cmdbuf) {
            base_coord.first = coord;
            base_coord.second = coord + 2;
            cmdbuf.pushConstants(base_coord_heap.GpuAddr(), 256, 0, sizeof(base_coord),
                                 &base_coord);
            cmdbuf.dispatchCompute(1, 1, 1);
        });
        REQUIRE(*result_heap == (std::array<uint32_t, 4>{0xef, 0xbe, 0xad, 0xde}));
        *result_heap = {};
    }
}
