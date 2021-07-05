#include <catch2/catch_test_macros.hpp>

#include <deko3d.hpp>

#include "cmd_util.h"
#include "heap.h"
#include "shader.h"

TEST_CASE("Buffer push constants simple", "[buffer][push_constants]") {
    constexpr uint32_t expected = 0xdeadbeef;
    TypedHeap<uint32_t> uniform_buffer = 0;
    TypedHeap<uint32_t> result = 0;

    auto vert_shader = LoadShader("write_uniform_vert");

    ResetState();
    BindShaders(vert_shader);
    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        cmdbuf.pushConstants(uniform_buffer.GpuAddr(), 256, 0, sizeof(expected), &expected);
        cmdbuf.bindUniformBuffer(DkStage_Vertex, 0, uniform_buffer.GpuAddr(), 256);
        cmdbuf.bindStorageBuffer(DkStage_Vertex, 0, result.GpuAddr(), result.Size());
    });
    Draw(DkPrimitive_Points, 1);

    REQUIRE(*result == expected);
}

TEST_CASE("Buffer push constants sequential", "[buffer][push_constants]") {
    struct Result {
        alignas(64) uint32_t a;
        alignas(64) uint32_t b;
    };
    constexpr uint32_t expected1 = 0xdeadbeef;
    constexpr uint32_t expected2 = 0xcafecafe;
    TypedHeap<uint32_t> uniform_buffer = 0;
    TypedHeap<Result> result;

    auto vert_shader = LoadShader("write_uniform_vert");

    ResetState();
    BindShaders(vert_shader);
    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        cmdbuf.bindUniformBuffer(DkStage_Vertex, 0, uniform_buffer.GpuAddr(), 256);

        cmdbuf.pushConstants(uniform_buffer.GpuAddr(), 256, 0, sizeof(expected1), &expected1);
        cmdbuf.bindStorageBuffer(DkStage_Vertex, 0, result.GpuAddr() + 0, 4);
        cmdbuf.draw(DkPrimitive_Points, 1, 1, 0, 0);

        cmdbuf.pushConstants(uniform_buffer.GpuAddr(), 256, 0, sizeof(expected2), &expected2);
        cmdbuf.bindStorageBuffer(DkStage_Vertex, 0, result.GpuAddr() + 64, 4);
        cmdbuf.draw(DkPrimitive_Points, 1, 1, 0, 0);
    });

    REQUIRE(result->a == expected1);
    REQUIRE(result->b == expected2);
}

TEST_CASE("Buffer copy simple", "[buffer][dma]") {
    constexpr uint32_t expected = 0xcafebeef;
    TypedHeap<uint32_t> copy_source = expected;
    TypedHeap<uint32_t> uniform_buffer = 0;
    TypedHeap<uint32_t> result = 0;

    auto vert_shader = LoadShader("write_uniform_vert");

    ResetState();
    BindShaders(vert_shader);
    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        cmdbuf.copyBuffer(copy_source.GpuAddr(), uniform_buffer.GpuAddr(), 4);
        cmdbuf.bindUniformBuffer(DkStage_Vertex, 0, uniform_buffer.GpuAddr(), 256);
        cmdbuf.bindStorageBuffer(DkStage_Vertex, 0, result.GpuAddr(), result.Size());
    });
    Draw(DkPrimitive_Points, 1);

    REQUIRE(*result == expected);
}

TEST_CASE("Buffer copy sequential", "[buffer][dma]") {
    struct Result {
        alignas(64) uint32_t a;
        alignas(64) uint32_t b;
    };
    constexpr uint32_t expected1 = 0xdeadbeef;
    constexpr uint32_t expected2 = 0xcafecafe;
    TypedHeap<uint32_t> copy_source_a = expected1;
    TypedHeap<uint32_t> copy_source_b = expected2;
    TypedHeap<uint32_t> uniform_buffer = 0;
    TypedHeap<Result> result;

    auto vert_shader = LoadShader("write_uniform_vert");

    ResetState();
    BindShaders(vert_shader);
    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        cmdbuf.bindUniformBuffer(DkStage_Vertex, 0, uniform_buffer.GpuAddr(), 256);

        cmdbuf.copyBuffer(copy_source_a.GpuAddr(), uniform_buffer.GpuAddr(), 4);
        cmdbuf.bindStorageBuffer(DkStage_Vertex, 0, result.GpuAddr() + 0, 4);
        cmdbuf.draw(DkPrimitive_Points, 1, 1, 0, 0);

        cmdbuf.copyBuffer(copy_source_b.GpuAddr(), uniform_buffer.GpuAddr(), 4);
        cmdbuf.bindStorageBuffer(DkStage_Vertex, 0, result.GpuAddr() + 64, 4);
        cmdbuf.draw(DkPrimitive_Points, 1, 1, 0, 0);
    });

    REQUIRE(result->a == expected1);
    REQUIRE(result->b == expected2);
}

TEST_CASE("Buffer copy result", "[buffer][dma]") {
    constexpr uint32_t expected = 0xcafebeef;
    TypedHeap<uint32_t> copy_source = expected;
    TypedHeap<uint32_t> uniform_buffer = 0;
    TypedHeap<uint32_t> storage_buffer = 0;
    TypedHeap<uint32_t> result = 0;

    auto vert_shader = LoadShader("write_uniform_vert");

    ResetState();
    BindShaders(vert_shader);
    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        cmdbuf.copyBuffer(copy_source.GpuAddr(), uniform_buffer.GpuAddr(), 4);
        cmdbuf.bindUniformBuffer(DkStage_Vertex, 0, uniform_buffer.GpuAddr(), 256);
        cmdbuf.bindStorageBuffer(DkStage_Vertex, 0, storage_buffer.GpuAddr(),
                                 storage_buffer.Size());
        cmdbuf.draw(DkPrimitive_Points, 1, 1, 0, 0);
        cmdbuf.barrier(DkBarrier_Full, DkInvalidateFlags_L2Cache);
        cmdbuf.copyBuffer(storage_buffer.GpuAddr(), result.GpuAddr(), 4);
    });

    REQUIRE(*storage_buffer == expected);
}

TEST_CASE("Buffer copy after push simple", "[buffer][dma]") {
    constexpr uint32_t expected = 0xcafebeef;
    constexpr uint32_t fake = 0xcccccccc;
    TypedHeap<uint32_t> copy_source = expected;
    TypedHeap<uint32_t> uniform_buffer = 0;
    TypedHeap<uint32_t> result = 0;

    auto vert_shader = LoadShader("write_uniform_vert");

    ResetState();
    BindShaders(vert_shader);
    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        cmdbuf.pushConstants(uniform_buffer.GpuAddr(), 256, 0, sizeof(fake), &fake);
        cmdbuf.copyBuffer(copy_source.GpuAddr(), uniform_buffer.GpuAddr(), 4);
        cmdbuf.bindUniformBuffer(DkStage_Vertex, 0, uniform_buffer.GpuAddr(), 256);
        cmdbuf.bindStorageBuffer(DkStage_Vertex, 0, result.GpuAddr(), result.Size());
        cmdbuf.draw(DkPrimitive_Points, 1, 1, 0, 0);
    });

    REQUIRE(*result == expected);
}

TEST_CASE("Buffer push after copy simple", "[buffer][dma]") {
    constexpr uint32_t expected = 0xbadbeef;
    constexpr uint32_t fake = 0xcccccccc;
    TypedHeap<uint32_t> copy_source = fake;
    TypedHeap<uint32_t> uniform_buffer = 0;
    TypedHeap<uint32_t> result = 0;

    auto vert_shader = LoadShader("write_uniform_vert");

    ResetState();
    BindShaders(vert_shader);
    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        cmdbuf.copyBuffer(copy_source.GpuAddr(), uniform_buffer.GpuAddr(), 4);
        cmdbuf.pushConstants(uniform_buffer.GpuAddr(), 256, 0, sizeof(expected), &expected);
        cmdbuf.bindUniformBuffer(DkStage_Vertex, 0, uniform_buffer.GpuAddr(), 256);
        cmdbuf.bindStorageBuffer(DkStage_Vertex, 0, result.GpuAddr(), result.Size());
        cmdbuf.draw(DkPrimitive_Points, 1, 1, 0, 0);
    });

    REQUIRE(*result == expected);
}

TEST_CASE("Buffer CPU write same page", "[buffer]") {
    struct Result {
        // Separate them by one cache line
        alignas(64) uint32_t ssbo;
        alignas(64) uint32_t cpu;
    };
    TypedHeap<uint32_t> uniform_buffer = 0;
    TypedHeap<Result> result;

    result->cpu = 0xcafecafe;

    auto vert_shader = LoadShader("write_uniform_vert");

    ResetState();
    BindShaders(vert_shader);

    static constexpr size_t MAX_SIZE{64 * 1024};
    dk::UniqueCmdBuf cmdbuf{dk::CmdBufMaker{device}.create()};
    dk::UniqueMemBlock heap{dk::MemBlockMaker{device, MAX_SIZE}.create()};
    cmdbuf.addMemory(heap, 0, MAX_SIZE);

    cmdbuf.bindUniformBuffer(DkStage_Vertex, 0, uniform_buffer.GpuAddr(), 256);
    cmdbuf.bindStorageBuffer(DkStage_Vertex, 0, result.GpuAddr(), result.Size());
    cmdbuf.draw(DkPrimitive_Points, 1, 1, 0, 0);

    DkCmdList list = cmdbuf.finishList();

    for (int i = 0; i < 64; ++i) {
        *uniform_buffer = i * 0xbabadada;

        queue.submitCommands(list);
        queue.flush();

        result->cpu += 0xdeaddead;

        queue.waitIdle();

        REQUIRE(result->ssbo == i * 0xbabadada);
        REQUIRE(result->cpu == 0xcafecafe + 0xdeaddead * (i + 1));
    }
}

TEST_CASE("Buffer CPU write same cache line uncached", "[buffer]") {
    struct Result {
        uint32_t ssbo;
        uint32_t cpu;
    };
    TypedHeap<uint32_t> uniform_buffer = 0;
    Heap result_heap(sizeof(Result), DkMemBlockFlags_CpuUncached | DkMemBlockFlags_GpuCached);
    auto result = reinterpret_cast<Result*>(result_heap.CpuAddr());

    result->cpu = 0xcafecafe;

    auto vert_shader = LoadShader("write_uniform_vert");

    ResetState();
    BindShaders(vert_shader);

    static constexpr size_t MAX_SIZE{64 * 1024};
    dk::UniqueCmdBuf cmdbuf{dk::CmdBufMaker{device}.create()};
    dk::UniqueMemBlock heap{dk::MemBlockMaker{device, MAX_SIZE}.create()};
    cmdbuf.addMemory(heap, 0, MAX_SIZE);

    cmdbuf.bindUniformBuffer(DkStage_Vertex, 0, uniform_buffer.GpuAddr(), 256);
    cmdbuf.bindStorageBuffer(DkStage_Vertex, 0, result_heap.GpuAddr(), result_heap.Size());
    cmdbuf.draw(DkPrimitive_Points, 1, 1, 0, 0);

    DkCmdList list = cmdbuf.finishList();

    for (int i = 0; i < 64; ++i) {
        *uniform_buffer = i * 0xbabadada;

        queue.submitCommands(list);
        queue.flush();

        result->cpu += 0xdeaddead;

        queue.waitIdle();

        REQUIRE(result->ssbo == i * 0xbabadada);
        REQUIRE(result->cpu == 0xcafecafe + 0xdeaddead * (i + 1));
    }
}
