#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

template <typename T = uint32_t>
static T Run(T base, T op_value, std::string code) {
    code = ".dksh compute\n"
           ".shared_memory 16\n"
           "main:\n"
           "MOV R0, c[0x0][0x140];\n"
           "MOV R1, c[0x0][0x144];\n"
           "MOV R2, c[2][0];\n"
           "MOV R3, c[2][4];\n"
           "STS.64 [0], R2;\n"
           "MOV R2, c[2][8];\n"
           "MOV R3, c[2][12];\n" +
           code + ";\n";

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

    return EvalUtil::Run<T>(code + "LDS.64 R2, [0]; STG.E.64 [R0], R2; EXIT;", data);
}

TEST_CASE("ATOMS Simple", "[shader]") {
    REQUIRE(Run(47, 3, "ATOMS.ADD.U32 R2, [0], R2") == 50);
    REQUIRE(Run(7, 3, "ATOMS.MIN.U32 R2, [0], R2") == 3);
    REQUIRE(Run(7, -3, "ATOMS.MIN.U32 R2, [0], R2") == 7);
    REQUIRE(Run(7, 3, "ATOMS.MAX.U32 R2, [0], R2") == 7);
    REQUIRE(Run(7, -3, "ATOMS.MAX.U32 R2, [0], R2") == -3);
    REQUIRE(Run(7, 255, "ATOMS.INC.U32 R2, [0], R2") == 8);
    REQUIRE(Run(7, 7, "ATOMS.INC.U32 R2, [0], R2") == 0);
    REQUIRE(Run(7, 8, "ATOMS.INC.U32 R2, [0], R2") == 8);
    REQUIRE(Run(8, 8, "ATOMS.INC.U32 R2, [0], R2") == 0);
    REQUIRE(Run(7, 255, "ATOMS.DEC.U32 R2, [0], R2") == 6);
    REQUIRE(Run(17, 10, "ATOMS.DEC.U32 R2, [0], R2") == 10);
    REQUIRE(Run(0, 10, "ATOMS.DEC.U32 R2, [0], R2") == 10);
    REQUIRE(Run(0, 255, "ATOMS.DEC.U32 R2, [0], R2") == 255);
    REQUIRE(Run(0xfe, 0xef, "ATOMS.AND.U32 R2, [0], R2") == 0xee);
    REQUIRE(Run(0xfe, 0xef, "ATOMS.OR.U32 R2, [0], R2") == 0xff);
    REQUIRE(Run(0xfe, 0xef, "ATOMS.XOR.U32 R2, [0], R2") == 0x11);
    REQUIRE(Run(0xfe, 0xef, "ATOMS.EXCH.U32 R2, [0], R2") == 0xef);

    // INC and DEC abort execution
    REQUIRE(Run(47, 3, "ATOMS.ADD.S32 R2, [0], R2") == 50);
    REQUIRE(Run(7, 3, "ATOMS.MIN.S32 R2, [0], R2") == 3);
    REQUIRE(Run(7, -3, "ATOMS.MIN.S32 R2, [0], R2") == -3);
    REQUIRE(Run(7, 3, "ATOMS.MAX.S32 R2, [0], R2") == 7);
    REQUIRE(Run(7, -3, "ATOMS.MAX.S32 R2, [0], R2") == 7);
    REQUIRE(Run(0xfe, 0xef, "ATOMS.AND.S32 R2, [0], R2") == 0xee);
    REQUIRE(Run(0xfe, 0xef, "ATOMS.OR.S32 R2, [0], R2") == 0xff);
    REQUIRE(Run(0xfe, 0xef, "ATOMS.XOR.S32 R2, [0], R2") == 0x11);
    REQUIRE(Run(0xfe, 0xef, "ATOMS.EXCH.S32 R2, [0], R2") == 0xef);

    // Everything but EXCH aborts execution
    REQUIRE(Run<uint64_t>(0xaaaa'bbbb'cccc'dddd, 0xbbbb'cccc'dddd'eeee,
                          "ATOMS.EXCH.U64 R2, [0], R2") == 0xbbbb'cccc'dddd'eeee);
}
