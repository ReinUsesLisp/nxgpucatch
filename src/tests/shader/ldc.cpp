#include <memory>

#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

template <typename Result, typename InputData, size_t N>
static Result Run(std::string code, InputData data[N]) {
    code = ".dksh compute\n"
           "main:\n"
           "MOV R4, c[0x0][0x140];"
           "MOV R5, c[0x0][0x144];\n" +
           code +
           "STG.E [R4], R0;"
           "EXIT;\n";

    std::array<TypedHeap<InputData>, N> input_heaps;
    TypedHeap<Result> output_heap;
    for (size_t i = 0; i < N; ++i) {
        *input_heaps[i] = data[i];
    }

    Shader shader(code.c_str());
    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        for (int i = 0; i < static_cast<int>(N); ++i) {
            input_heaps[i].BindUniform(cmdbuf, DkStage_Compute, i);
        }
        output_heap.BindStorage(cmdbuf, DkStage_Compute);
        shader.Bind(cmdbuf, DkStageFlag_Compute);
        cmdbuf.dispatchCompute(1, 1, 1);
    });
    return *output_heap;
}

template <typename Result, typename InputData>
static Result Run(std::string code, InputData data) {
    return Run<Result, InputData, 1>(std::move(code), &data);
}

TEST_CASE("LDC Simple", "[shader]") {
    REQUIRE(Run<uint32_t>("LDC.32 R0, c[2][0];", 0xdeadbeef) == 0xdeadbeef);

    REQUIRE(Run<uint32_t>("MOV R0, 0; LDC.32 R0, c[2][R0];", 0xdeadbeef) == 0xdeadbeef);

    REQUIRE(Run<uint32_t>("MOV R0, 0; LDC.U8 R0, c[2][R0];", 0xdeadbeef) == 0xef);
    REQUIRE(Run<uint32_t>("MOV R0, 1; LDC.U8 R0, c[2][R0];", 0xdeadbeef) == 0xbe);
    REQUIRE(Run<uint32_t>("MOV R0, 2; LDC.U8 R0, c[2][R0];", 0xdeadbeef) == 0xad);
    REQUIRE(Run<uint32_t>("MOV R0, 3; LDC.U8 R0, c[2][R0];", 0xdeadbeef) == 0xde);

    REQUIRE(Run<uint32_t>("MOV R0, 0; LDC.U8 R0, c[2][R0+1];", 0xdeadbeef) == 0xbe);
    REQUIRE(Run<uint32_t>("MOV R0, 0; LDC.U8 R0, c[2][R0+2];", 0xdeadbeef) == 0xad);
    REQUIRE(Run<uint32_t>("MOV R0, 0; LDC.U8 R0, c[2][R0+3];", 0xdeadbeef) == 0xde);

    REQUIRE(Run<uint32_t>("MOV R0, 4; LDC.U8 R0, c[2][R0-2];", 0xdeadbeef) == 0xad);
    REQUIRE(Run<uint32_t>("MOV R0, 4; LDC.U8 R0, c[2][R0-3];", 0xdeadbeef) == 0xbe);
    REQUIRE(Run<uint32_t>("MOV R0, 4; LDC.U8 R0, c[2][R0-4];", 0xdeadbeef) == 0xef);

    REQUIRE(Run<uint32_t>("MOV R0, 0; LDC.S8 R0, c[2][R0];", 0xdeadbeef) == 0xffffffef);
    REQUIRE(Run<uint32_t>("MOV R0, 1; LDC.S8 R0, c[2][R0];", 0xdeadbeef) == 0xffffffbe);
    REQUIRE(Run<uint32_t>("MOV R0, 2; LDC.S8 R0, c[2][R0];", 0xdeadbeef) == 0xffffffad);
    REQUIRE(Run<uint32_t>("MOV R0, 3; LDC.S8 R0, c[2][R0];", 0xdeadbeef) == 0xffffffde);

    REQUIRE(Run<uint32_t>("MOV R0, 0; LDC.U16 R0, c[2][R0];", 0xdeadbeef) == 0xbeef);
    REQUIRE(Run<uint32_t>("MOV R0, 2; LDC.U16 R0, c[2][R0];", 0xdeadbeef) == 0xdead);

    REQUIRE(Run<uint32_t>("MOV R0, 0; LDC.S16 R0, c[2][R0];", 0xdeadbeef) == 0xffffbeef);
    REQUIRE(Run<uint32_t>("MOV R0, 2; LDC.S16 R0, c[2][R0];", 0xdeadbeef) == 0xffffdead);

    REQUIRE(Run<uint32_t>("LDC.64 R0, c[2][0];", 0xdeadbeefcafecafe) == 0xcafecafe);
    REQUIRE(Run<uint32_t>("LDC.64 R0, c[2][0]; MOV R0, R1;", 0xdeadbeefcafecafe) == 0xdeadbeef);
}

TEST_CASE("LDC Negative offsets", "[shader]") {
    std::array<uint32_t, 16384> mem;
    mem.fill(0xcccccccc);
    mem[0] = 8;
    mem[1] = 16;
    mem[2] = 32;
    mem[mem.size() - 1] = 0xffffffff;
    mem[mem.size() - 2] = 0xeeeeeeee;

    REQUIRE(Run<uint32_t>("MOV R0, c[2][0];", mem) == 8);
    REQUIRE(Run<uint32_t>("MOV R0, c[2][-4];", mem) == 0xffffffff);
    REQUIRE(Run<uint32_t>("MOV R0, c[2][-8];", mem) == 0xeeeeeeee);
    REQUIRE(Run<uint32_t>("MOV R0, c[2][-180];", mem) == 0xcccccccc);
    REQUIRE(Run<uint32_t>("MOV R0, c[2][-0x8000];", mem) == 0xcccccccc);

    REQUIRE(Run<uint32_t>("MOV R0, -4; LDC.32 R0, c[2][R0];", mem) == 0);
}

TEST_CASE("LDC Foldable robustness", "[shader]") {
    std::array<uint32_t, 16384> mem;
    mem.fill(0xcccccccc);

    REQUIRE(Run<uint32_t>("MOV R0, 8; LDC.32 R0, c[2][R0];", mem) == 0xcccccccc);
    REQUIRE(Run<uint32_t>("MOV R0, 0xfffc; LDC.32 R0, c[2][R0];", mem) == 0xcccccccc);
    REQUIRE(Run<uint32_t>("MOV R0, 0x1ffff; LDC.32 R0, c[2][R0]; IADD R0, R0, 1;", mem) == 1);
    REQUIRE(Run<uint32_t>("MOV R0, 0x10000; LDC.32 R0, c[2][R0]; IADD R0, R0, 1;", mem) == 1);
    REQUIRE(Run<uint32_t>("MOV32I R0, 0xaaaaa0; LDC.32 R0, c[2][R0];", 0xdeadbeef) == 0);
}

TEST_CASE("LDC Foldable unaligned", "[shader]") {
    std::array<uint32_t, 16384> mem;
    mem.fill(0xcccccccc);

    REQUIRE(Run<uint32_t>("MOV R0, 7; LDC.32 R0, c[2][R0]; IADD R0, R0, 1;", mem) == 0xcccccccd);
    REQUIRE(Run<uint32_t>("MOV R0, 6; LDC.32 R0, c[2][R0]; IADD R0, R0, 1;", mem) == 0xcccccccd);
    REQUIRE(Run<uint32_t>("MOV R0, 6; LDC.U16 R0, c[2][R0]; IADD R0, R0, 1;", mem) == 0xcccd);
    REQUIRE(Run<uint32_t>("MOV R0, 0xffff; LDC.32 R0, c[2][R0];", mem) == 0xcccccccc);

    mem.back() = 0xdddddddd;
    REQUIRE(Run<uint32_t>("MOV R0, 0xffff; LDC.32 R0, c[2][R0];", mem) == 0xdddddddd);
}

TEST_CASE("LDC IL", "[shader]") {
    using u32 = uint32_t;
    u32 data[4]{0xaa, 0xbb, 0xcc, 0xdd};
    REQUIRE(Run<u32, u32, 8>("MOV32I R0, 0x00000000; LDC.32.IL R0, c[2][R0];", data) == 0xaa);
    REQUIRE(Run<u32, u32, 8>("MOV32I R0, 0x00010000; LDC.32.IL R0, c[2][R0];", data) == 0xbb);
    REQUIRE(Run<u32, u32, 8>("MOV32I R0, 0x00020000; LDC.32.IL R0, c[2][R0];", data) == 0xcc);
    REQUIRE(Run<u32, u32, 8>("MOV32I R0, 0x00030000; LDC.32.IL R0, c[2][R0];", data) == 0xdd);
}

// TODO: Test IS
// TODO: Test ISL
