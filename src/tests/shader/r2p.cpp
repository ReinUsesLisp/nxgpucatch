#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

static uint32_t Run(uint32_t value, uint32_t mask, std::string code) {
    return EvalUtil::Run(".dksh compute\n"
                         "main:\n"
                         "MOV R0, c[0x0][0x140];\n"
                         "MOV R1, c[0x0][0x144];\n"
                         "ISETP.T.AND P0, P1, RZ, RZ, !PT;\n"
                         "ISETP.T.AND P2, P3, RZ, RZ, !PT;\n"
                         "ISETP.T.AND P4, P5, RZ, RZ, !PT;\n"
                         "ISETP.T.AND P6, PT, RZ, RZ, !PT;\n"
                         "IADD RZ.CC, RZ, 1;\n"
                         "MOV R2, c[2][0];\n"
                         "MOV R3, c[2][4];\n" +
                             code +
                             "P2R.B0 R2, PR, RZ, 0xff;\n"
                             "P2R.B1 R2, CC, R2, 0xff;\n"
                             "STG.E [R0], R2;\n"
                             "EXIT;\n",
                         std::array{value, mask});
}

TEST_CASE("R2P Simple", "[shader]") {
    REQUIRE(Run(0xf, 0xff, "R2P CC, R2, R3;") == 0xf00);
    REQUIRE(Run(0xff, 0xff, "R2P CC, R2, R3;") == 0xf00);
    REQUIRE(Run(0x1, 0xff, "R2P CC, R2, R3;") == 0x100);
    REQUIRE(Run(0x2, 0xff, "R2P CC, R2, R3;") == 0x200);
    REQUIRE(Run(0x4, 0xff, "R2P CC, R2, R3;") == 0x400);
    REQUIRE(Run(0x8, 0xff, "R2P CC, R2, R3;") == 0x800);
    REQUIRE(Run(0x800, 0xff, "R2P CC, R2.B1, R3;") == 0x800);
    REQUIRE(Run(0x80000, 0xff, "R2P CC, R2.B2, R3;") == 0x800);
    REQUIRE(Run(0x8000000, 0xff, "R2P CC, R2.B3, R3;") == 0x800);
    REQUIRE(Run(0xf, 0x4, "R2P CC, R2, R3;") == 0x400);
    REQUIRE(Run(0xf, 0xc, "R2P CC, R2, R3;") == 0xc00);
    REQUIRE(Run(0xd, 0x1, "R2P CC, R2, R3;") == 0x100);

    REQUIRE(Run(0xf, 0xff, "R2P PR, R2, R3;") == 0xf);
    REQUIRE(Run(0xff, 0xff, "R2P PR, R2, R3;") == 0x7f);
    REQUIRE(Run(0x3f, 0x3f, "R2P PR, R2, R3;") == 0x3f);
    REQUIRE(Run(0xff, 0x3f, "R2P PR, R2, R3;") == 0x3f);
    REQUIRE(Run(1, 0x3f, "R2P PR, R2, R3;") == 0x1);

    REQUIRE(Run(0xff, 0x3f, "R2P PR, R2, 0x3f;") == 0x3f);
    REQUIRE(Run(0xff, 0x3f, "R2P PR, R2, 0x3f;") == 0x3f);
    REQUIRE(Run(0xff, 0x3f, "R2P PR, R2, c[2][4];") == 0x3f);
}
