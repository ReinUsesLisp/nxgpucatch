#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

static uint32_t Run(uint32_t value, std::string code) {
    return EvalUtil::Run(".dksh compute\n"
                         "main:\n"
                         "MOV R0, c[0x0][0x140];\n"
                         "MOV R1, c[0x0][0x144];\n"
                         "MOV R2, 0x0ff0\n;"
                         "MOV R3, 0x00ff\n;" +
                             code +
                             "STG.E [R0], R2;\n"
                             "EXIT;\n",
                         value);
}

TEST_CASE("FLO Simple", "[shader]") {
    REQUIRE(Run(0x8000'0000, "FLO.U32 R2, c[2][0];") == 31);
    REQUIRE(Run(0x0000'0000, "FLO.U32 R2, c[2][0];") == 0xffff'ffff);
    REQUIRE(Run(0x0000'0002, "FLO.U32 R2, c[2][0];") == 1);
    REQUIRE(Run(0x0000'0001, "FLO.U32 R2, c[2][0];") == 0);

    REQUIRE(Run(0x0000'0004, "FLO.U32 R2, 4;") == 2);
    REQUIRE(Run(0x0000'0004, "MOV R2, 4; FLO.U32 R2, R2;") == 2);
}

TEST_CASE("FLO Negative", "[shader]") {
    REQUIRE(Run(0x0000'0000, "FLO.S32 R2, c[2][0];") == 0xffff'ffff);
    REQUIRE(Run(0xffff'ffff, "FLO.S32 R2, c[2][0];") == 0xffff'ffff);
    REQUIRE(Run(0x8000'0000, "FLO.S32 R2, c[2][0];") == 30);
    REQUIRE(Run(0xf000'0000, "FLO.S32 R2, c[2][0];") == 27);
    REQUIRE(Run(0xff00'0000, "FLO.S32 R2, c[2][0];") == 23);
    REQUIRE(Run(0x0300'0000, "FLO.S32 R2, c[2][0];") == 25);
    REQUIRE(Run(0x0000'0001, "FLO.S32 R2, c[2][0];") == 0);
    REQUIRE(Run(0x0000'0002, "FLO.S32 R2, c[2][0];") == 1);
}

TEST_CASE("FLO SH", "[shader]") {
    REQUIRE(Run(0x8000'0000, "FLO.U32.SH R2, c[2][0];") == 0);
    REQUIRE(Run(0x0f00'0000, "FLO.U32.SH R2, c[2][0];") == 4);
    REQUIRE(Run(0x00f0'0000, "FLO.U32.SH R2, c[2][0];") == 8);
    REQUIRE(Run(0x00f0'0000, "FLO.S32.SH R2, c[2][0];") == 8);
    REQUIRE(Run(0xf0f0'0000, "FLO.S32.SH R2, c[2][0];") == 4);
    REQUIRE(Run(0xf000'0000, "FLO.S32.SH R2, c[2][0];") == 4);
    REQUIRE(Run(0x9000'0000, "FLO.S32.SH R2, c[2][0];") == 1);
}

TEST_CASE("FLO Invert", "[shader]") {
    REQUIRE(Run(0x8000'0000, "FLO.U32 R2, ~c[2][0];") == 30);
    REQUIRE(Run(0xffff'0000, "FLO.U32 R2, ~c[2][0];") == 15);
    REQUIRE(Run(0x8000'0000, "FLO.S32 R2, ~c[2][0];") == 30);
    REQUIRE(Run(0x8000'0000, "FLO.S32 R2, ~c[2][0];") == 30);
    REQUIRE(Run(0x0000'0000, "FLO.S32 R2, ~c[2][0];") == 0xffff'ffff);
    REQUIRE(Run(0x7800'0000, "FLO.S32 R2, ~c[2][0];") == 30);
    REQUIRE(Run(0xffff'0000, "FLO.S32 R2, ~c[2][0];") == 15);
    REQUIRE(Run(0x0000'f000, "FLO.S32 R2, ~c[2][0];") == 15);
    REQUIRE(Run(0x0000'0f00, "FLO.S32 R2, ~c[2][0];") == 11);
}
