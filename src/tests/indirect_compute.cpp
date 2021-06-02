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

TEST_CASE("Indirect compute", "[indirect_compute]") {
    Texture texture(512, 1, 1, 1, DkImageType_Buffer);
    Shader stg_shader{MakeShader(R"(
        MOV R4, c[0x0][0x140];
        MOV R5, c[0x0][0x144];
        MOV32I R0, 4;
        MOV32I R1, 8;
        MOV32I R2, 16;
        MOV32I R3, 0;
        STG.E.128 [R4], R0;
    )")};
    Shader red_shader{MakeShader(R"(
        MOV R4, c[0x0][0x140];
        MOV R5, c[0x0][0x144];
        MOV32I R0, 0xffffffff;
        RED.E.INC [R4], R0;
    )")};
    TypedHeap<DkDispatchIndirectData> dispatch_heap;
    TypedHeap<uint32_t> result_heap;
    dispatch_heap->numGroupsX = 4;
    dispatch_heap->numGroupsY = 4;
    dispatch_heap->numGroupsZ = 4;

    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        stg_shader.Bind(cmdbuf, DkStageFlag_Compute);
        dispatch_heap.BindStorage(cmdbuf, DkStage_Compute, 0);
        cmdbuf.dispatchCompute(1, 1, 1);
        cmdbuf.barrier(DkBarrier_Full, DkInvalidateFlags_L2Cache);

        red_shader.Bind(cmdbuf, DkStageFlag_Compute);
        result_heap.BindStorage(cmdbuf, DkStage_Compute, 0);
        cmdbuf.dispatchComputeIndirect(dispatch_heap.GpuAddr());
    });
    REQUIRE(*result_heap == 4 * 8 * 16);
}

TEST_CASE("Indirect compute with DMA copy", "[indirect_compute]") {
    Texture texture(512, 1, 1, 1, DkImageType_Buffer);
    Shader stg_shader{MakeShader(R"(
        MOV R4, c[0x0][0x140];
        MOV R5, c[0x0][0x144];
        MOV32I R0, 4;
        MOV32I R1, 8;
        MOV32I R2, 16;
        MOV32I R3, 0;
        STG.E.128 [R4], R0;
    )")};
    Shader red_shader{MakeShader(R"(
        MOV R4, c[0x0][0x140];
        MOV R5, c[0x0][0x144];
        MOV32I R0, 0xffffffff;
        RED.E.INC [R4], R0;
    )")};
    TypedHeap<DkDispatchIndirectData> dispatch_heap;
    TypedHeap<uint32_t> result_heap;
    dispatch_heap->numGroupsX = 4;
    dispatch_heap->numGroupsY = 4;
    dispatch_heap->numGroupsZ = 4;

    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        stg_shader.Bind(cmdbuf, DkStageFlag_Compute);
        dispatch_heap.BindStorage(cmdbuf, DkStage_Compute, 0);
        cmdbuf.dispatchCompute(1, 1, 1);
        cmdbuf.barrier(DkBarrier_Full, DkInvalidateFlags_L2Cache);

        const DkGpuAddr dispatch_addr = dispatch_heap.GpuAddr() + sizeof(DkDispatchIndirectData);
        cmdbuf.copyBuffer(dispatch_heap.GpuAddr(), dispatch_addr, sizeof(DkDispatchIndirectData));

        red_shader.Bind(cmdbuf, DkStageFlag_Compute);
        result_heap.BindStorage(cmdbuf, DkStage_Compute, 0);
        cmdbuf.dispatchComputeIndirect(dispatch_addr);
    });
    REQUIRE(*result_heap == 4 * 8 * 16);
}

TEST_CASE("Indirect compute with partial DMA copy", "[indirect_compute]") {
    Texture texture(512, 1, 1, 1, DkImageType_Buffer);
    Shader stg_shader{MakeShader(R"(
        MOV R4, c[0x0][0x140];
        MOV R5, c[0x0][0x144];
        MOV32I R0, 5;
        MOV32I R1, 8;
        MOV32I R2, 16;
        MOV32I R3, 0;
        STG.E.128 [R4], R0;
    )")};
    Shader red_shader{MakeShader(R"(
        MOV R4, c[0x0][0x140];
        MOV R5, c[0x0][0x144];
        MOV32I R0, 0xffffffff;
        RED.E.INC [R4], R0;
    )")};
    TypedHeap<DkDispatchIndirectData[2]> dispatch_heap;
    TypedHeap<uint32_t> result_heap;
    dispatch_heap[0].numGroupsX = 4;
    dispatch_heap[0].numGroupsY = 4;
    dispatch_heap[0].numGroupsZ = 4;
    dispatch_heap[1].numGroupsX = 4;
    dispatch_heap[1].numGroupsY = 4;
    dispatch_heap[1].numGroupsZ = 4;

    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        stg_shader.Bind(cmdbuf, DkStageFlag_Compute);
        dispatch_heap.BindStorage(cmdbuf, DkStage_Compute, 0);
        cmdbuf.dispatchCompute(1, 1, 1);
        cmdbuf.barrier(DkBarrier_Full, DkInvalidateFlags_L2Cache);

        const DkGpuAddr dispatch_addr = dispatch_heap.GpuAddr() + sizeof(DkDispatchIndirectData);
        cmdbuf.copyBuffer(dispatch_heap.GpuAddr(), dispatch_addr,
                          sizeof(DkDispatchIndirectData) - sizeof(uint32_t));

        red_shader.Bind(cmdbuf, DkStageFlag_Compute);
        result_heap.BindStorage(cmdbuf, DkStage_Compute, 0);
        cmdbuf.dispatchComputeIndirect(dispatch_addr);
    });
    REQUIRE(*result_heap == 5 * 8 * 4);
}
