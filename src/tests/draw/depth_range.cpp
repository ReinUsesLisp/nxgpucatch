#include <catch2/catch_test_macros.hpp>

#include <deko3d.hpp>

#include "cmd_util.h"
#include "descriptor_set.h"
#include "heap.h"
#include "resource.h"
#include "shader.h"

static Shader ComputeShader() {
    return Shader(R"(.dksh compute
main:
        MOV R0, RZ;
        MOV R1, RZ;
        TEX.LZ PT, R0, R0, RZ, 0x28, 2D, 1;
        MOV R2, c[0x0][0x140];
        MOV R3, c[0x0][0x144];
        STG.E [R2], R0;
        EXIT;
    )");
}

TEST_CASE("Depth range", "[draw]") {
    DescriptorSet descriptor_set;
    Texture depth_image(64, 64, 1, 1, DkImageType_2D, DkImageFormat_ZF32_X24S8);

    auto vert_shader = LoadShader("full_screen_triangle_depth_vert");
    auto frag_shader = LoadShader("white_frag");
    auto comp_shader = ComputeShader();
    TypedHeap<float> uniform;
    TypedHeap<float> result;

    ResetState();

    dk::UniqueCmdBuf cmdbuf{dk::CmdBufMaker{device}.create()};
    dk::UniqueMemBlock heap{dk::MemBlockMaker{device, 64 * 1024}.create()};
    cmdbuf.addMemory(heap, 0, 64 * 1024);

    cmdbuf.setViewports(0, {{0.0f, 0.0f, 64.0f, 64.0f, 0.0f, 1.0f}});
    auto zero_to_one_depth_range_cmdlist = cmdbuf.finishList();

    cmdbuf.setViewports(0, {{0.0f, 0.0f, 64.0f, 64.0f, -1.0f, 1.0f}});
    auto minus_one_to_one_depth_range_cmdlist = cmdbuf.finishList();

    vert_shader.Bind(cmdbuf, DkStageFlag_GraphicsMask);
    frag_shader.Bind(cmdbuf, DkStageFlag_GraphicsMask);

    dk::DepthStencilState depth_state;
    depth_state.setDepthWriteEnable(true);
    depth_state.setDepthTestEnable(true);
    depth_state.setDepthCompareOp(DkCompareOp_Always);

    cmdbuf.bindDepthStencilState(depth_state);
    cmdbuf.bindRenderTargets({}, &depth_image.ImageView());
    cmdbuf.bindUniformBuffer(DkStage_Vertex, 0, uniform.GpuAddr(), 256);
    cmdbuf.setScissors(0, {{0, 0, 64, 64}});
    cmdbuf.draw(DkPrimitive_Triangles, 3, 1, 0, 0);
    cmdbuf.barrier(DkBarrier_Full, DkInvalidateFlags_L2Cache);

    comp_shader.Bind(cmdbuf, DkStageFlag_Compute);

    descriptor_set.Bind(cmdbuf);
    descriptor_set.PushTexture(cmdbuf, depth_image, 0);
    cmdbuf.bindTextures(DkStage_Compute, 0, {dkMakeTextureHandle(0, 0)});

    cmdbuf.bindStorageBuffer(DkStage_Compute, 0, result.GpuAddr(), result.Size());
    cmdbuf.dispatchCompute(1, 1, 1);

    auto cmdlist = cmdbuf.finishList();

    SECTION("Simple") {
        static constexpr std::array ranges{
            std::pair{1.0f, 1.0f},      std::pair{0.5f, 0.75f}, std::pair{0.25f, 0.625f},
            std::pair{0.125f, 0.5625f}, std::pair{-1.0f, 0.0f},
        };
        queue.submitCommands(zero_to_one_depth_range_cmdlist);
        for (auto [input, output] : ranges) {
            *uniform = input;
            queue.submitCommands(cmdlist);
            queue.flush();
            queue.waitIdle();
            REQUIRE(*result == output);
        }
    }
    SECTION("Negative one to one") {
        static constexpr std::array ranges{
            std::pair{1.0f, 1.0f},      std::pair{0.5f, 0.5f}, std::pair{0.25f, 0.25f},
            std::pair{0.125f, 0.125f}, std::pair{-1.0f, 0.0f},
        };
        queue.submitCommands(minus_one_to_one_depth_range_cmdlist);
        for (auto [input, output] : ranges) {
            *uniform = input;
            queue.submitCommands(cmdlist);
            queue.flush();
            queue.waitIdle();
            REQUIRE(*result == output);
        }
    }
}
