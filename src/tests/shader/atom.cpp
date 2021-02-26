#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

using F16x2 = std::array<__fp16, 2>;

template <typename T = uint32_t>
static T Run(T base, T op_value, std::string code) {
    code = ".dksh compute\n"
           "main:\n"
           "MOV R0, c[0x0][0x140];\n"
           "MOV R1, c[0x0][0x144];\n"
           "MOV R2, c[2][0];\n"
           "MOV R3, c[2][4];\n"
           "STG.E.64 [R0], R2;\n"
           "MOV R2, c[2][8];\n"
           "MOV R3, c[2][12];\n" +
           code;

    struct {
        alignas(8) T base;
        alignas(8) T op_value;
    } const data{base, op_value};

    T return_value;
    if constexpr (sizeof(T) > sizeof(uint32_t)) {
        return_value = EvalUtil::Run<T>(code + "STG.E.64 [R0], R2; EXIT;", data);
    } else {
        return_value = EvalUtil::Run<T>(code + "STG.E [R0], R2; EXIT;", data);
    }
    REQUIRE(return_value == base);

    return EvalUtil::Run<T>(code + "EXIT;", data);
}

TEST_CASE("ATOM Simple", "[shader]") {
    REQUIRE(Run(47, 3, "ATOM.E.ADD.U32 R2, [R0], R2;") == 50);
    REQUIRE(Run(7, 3, "ATOM.E.MIN.U32 R2, [R0], R2;") == 3);
    REQUIRE(Run(7, -3, "ATOM.E.MIN.U32 R2, [R0], R2;") == 7);
    REQUIRE(Run(7, 3, "ATOM.E.MAX.U32 R2, [R0], R2;") == 7);
    REQUIRE(Run(7, -3, "ATOM.E.MAX.U32 R2, [R0], R2;") == -3);
    REQUIRE(Run(7, 255, "ATOM.E.INC.U32 R2, [R0], R2;") == 8);
    REQUIRE(Run(7, 7, "ATOM.E.INC.U32 R2, [R0], R2;") == 0);
    REQUIRE(Run(7, 8, "ATOM.E.INC.U32 R2, [R0], R2;") == 8);
    REQUIRE(Run(8, 8, "ATOM.E.INC.U32 R2, [R0], R2;") == 0);
    REQUIRE(Run(7, 255, "ATOM.E.DEC.U32 R2, [R0], R2;") == 6);
    REQUIRE(Run(17, 10, "ATOM.E.DEC.U32 R2, [R0], R2;") == 10);
    REQUIRE(Run(0, 10, "ATOM.E.DEC.U32 R2, [R0], R2;") == 10);
    REQUIRE(Run(0, 255, "ATOM.E.DEC.U32 R2, [R0], R2;") == 255);
    REQUIRE(Run(0xfe, 0xef, "ATOM.E.AND.U32 R2, [R0], R2;") == 0xee);
    REQUIRE(Run(0xfe, 0xef, "ATOM.E.OR.U32 R2, [R0], R2;") == 0xff);
    REQUIRE(Run(0xfe, 0xef, "ATOM.E.XOR.U32 R2, [R0], R2;") == 0x11);

    // INC, and DEC do nothing
    REQUIRE(Run(47, 3, "ATOM.E.ADD.S32 R2, [R0], R2;") == 50);
    REQUIRE(Run(7, 3, "ATOM.E.MIN.S32 R2, [R0], R2;") == 3);
    REQUIRE(Run(7, -3, "ATOM.E.MIN.S32 R2, [R0], R2;") == -3);
    REQUIRE(Run(7, 3, "ATOM.E.MAX.S32 R2, [R0], R2;") == 7);
    REQUIRE(Run(7, -3, "ATOM.E.MAX.S32 R2, [R0], R2;") == 7);
    REQUIRE(Run(7, 255, "ATOM.E.INC.S32 R2, [R0], R2;") == 7);
    REQUIRE(Run(7, 7, "ATOM.E.INC.S32 R2, [R0], R2;") == 7);
    REQUIRE(Run(7, 8, "ATOM.E.INC.S32 R2, [R0], R2;") == 7);
    REQUIRE(Run(8, 8, "ATOM.E.INC.S32 R2, [R0], R2;") == 8);
    REQUIRE(Run(7, 255, "ATOM.E.DEC.S32 R2, [R0], R2;") == 7);
    REQUIRE(Run(17, 10, "ATOM.E.DEC.S32 R2, [R0], R2;") == 17);
    REQUIRE(Run(0, 10, "ATOM.E.DEC.S32 R2, [R0], R2;") == 0);
    REQUIRE(Run(0, 255, "ATOM.E.DEC.S32 R2, [R0], R2;") == 0);
    REQUIRE(Run(0xfe, 0xef, "ATOM.E.AND.S32 R2, [R0], R2;") == 0xee);
    REQUIRE(Run(0xfe, 0xef, "ATOM.E.OR.S32 R2, [R0], R2;") == 0xff);
    REQUIRE(Run(0xfe, 0xef, "ATOM.E.XOR.S32 R2, [R0], R2;") == 0x11);

    // INC, and DEC do nothing
    REQUIRE(Run<int64_t>(47, 3, "ATOM.E.ADD.U64 R2, [R0], R2;") == 50);
    REQUIRE(Run<int64_t>(7, 3, "ATOM.E.MIN.U64 R2, [R0], R2;") == 3);
    REQUIRE(Run<int64_t>(7, -3, "ATOM.E.MIN.U64 R2, [R0], R2;") == 7);
    REQUIRE(Run<int64_t>(7, 3, "ATOM.E.MAX.U64 R2, [R0], R2;") == 7);
    REQUIRE(Run<int64_t>(7, -3, "ATOM.E.MAX.U64 R2, [R0], R2;") == -3);
    REQUIRE(Run<int64_t>(7, 255, "ATOM.E.INC.U64 R2, [R0], R2;") == 7);
    REQUIRE(Run<int64_t>(7, 7, "ATOM.E.INC.U64 R2, [R0], R2;") == 7);
    REQUIRE(Run<int64_t>(7, 8, "ATOM.E.INC.U64 R2, [R0], R2;") == 7);
    REQUIRE(Run<int64_t>(8, 8, "ATOM.E.INC.U64 R2, [R0], R2;") == 8);
    REQUIRE(Run<int64_t>(7, 255, "ATOM.E.DEC.U64 R2, [R0], R2;") == 7);
    REQUIRE(Run<int64_t>(17, 10, "ATOM.E.DEC.U64 R2, [R0], R2;") == 17);
    REQUIRE(Run<int64_t>(0, 10, "ATOM.E.DEC.U64 R2, [R0], R2;") == 0);
    REQUIRE(Run<int64_t>(0, 255, "ATOM.E.DEC.U64 R2, [R0], R2;") == 0);
    REQUIRE(Run<int64_t>(0xfe, 0xef, "ATOM.E.AND.U64 R2, [R0], R2;") == 0xee);
    REQUIRE(Run<int64_t>(0xfe, 0xef, "ATOM.E.OR.U64 R2, [R0], R2;") == 0xff);
    REQUIRE(Run<int64_t>(0xfe, 0xef, "ATOM.E.XOR.U64 R2, [R0], R2;") == 0x11);

    // ADD, INC, and DEC, do nothing
    REQUIRE(Run<int64_t>(47, 3, "ATOM.E.ADD.S64 R2, [R0], R2;") == 47);
    REQUIRE(Run<int64_t>(7, 3, "ATOM.E.MIN.S64 R2, [R0], R2;") == 3);
    REQUIRE(Run<int64_t>(7, -3, "ATOM.E.MIN.S64 R2, [R0], R2;") == -3);
    REQUIRE(Run<int64_t>(7, 3, "ATOM.E.MAX.S64 R2, [R0], R2;") == 7);
    REQUIRE(Run<int64_t>(7, -3, "ATOM.E.MAX.S64 R2, [R0], R2;") == 7);
    REQUIRE(Run<int64_t>(7, 255, "ATOM.E.INC.S64 R2, [R0], R2;") == 7);
    REQUIRE(Run<int64_t>(7, 7, "ATOM.E.INC.S64 R2, [R0], R2;") == 7);
    REQUIRE(Run<int64_t>(7, 8, "ATOM.E.INC.S64 R2, [R0], R2;") == 7);
    REQUIRE(Run<int64_t>(8, 8, "ATOM.E.INC.S64 R2, [R0], R2;") == 8);
    REQUIRE(Run<int64_t>(7, 255, "ATOM.E.DEC.S64 R2, [R0], R2;") == 7);
    REQUIRE(Run<int64_t>(17, 10, "ATOM.E.DEC.S64 R2, [R0], R2;") == 17);
    REQUIRE(Run<int64_t>(0, 10, "ATOM.E.DEC.S64 R2, [R0], R2;") == 0);
    REQUIRE(Run<int64_t>(0, 255, "ATOM.E.DEC.S64 R2, [R0], R2;") == 0);
    REQUIRE(Run<int64_t>(0xfe, 0xef, "ATOM.E.AND.S64 R2, [R0], R2;") == 0xfe);
    REQUIRE(Run<int64_t>(0xfe, 0xef, "ATOM.E.OR.S64 R2, [R0], R2;") == 0xfe);
    REQUIRE(Run<int64_t>(0xfe, 0xef, "ATOM.E.XOR.S64 R2, [R0], R2;") == 0xfe);

    // Only ADD does something
    REQUIRE(Run<float>(3.0f, 1.0f, "ATOM.E.ADD.F32.FTZ.RN R2, [R0], R2;") == 4.0f);

    // Only ADD, MIN and MAX do something
    REQUIRE(Run<F16x2>(F16x2{1.0, 3.0}, F16x2{4.0, 7.0}, "ATOM.E.ADD.F16x2.RN R2, [R0], R2;") ==
            F16x2{5.0, 10.0});
    REQUIRE(Run<F16x2>(F16x2{1.0, 3.0}, F16x2{4.0, -2.0}, "ATOM.E.MIN.F16x2.RN R2, [R0], R2;") ==
            F16x2{1.0, -2.0});
    REQUIRE(Run<F16x2>(F16x2{1.0, 3.0}, F16x2{4.0, -2.0}, "ATOM.E.MAX.F16x2.RN R2, [R0], R2;") ==
            F16x2{4.0, 3.0});
}

TEST_CASE("ATOM Denorm", "[shader][fpcontrol]") {
    REQUIRE(Run<float>(1e-40, 1.0f, "ATOM.E.ADD.F32.FTZ.RN R2, [R0], R2;") == 1.0f);
}
