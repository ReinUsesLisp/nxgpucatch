#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

static uint32_t Run(uint32_t base, uint32_t offset, uint32_t count, std::string code) {
    return EvalUtil::Run(".dksh compute\n"
                         "main:\n"
                         "MOV R0, c[0x0][0x140];\n"
                         "MOV R1, c[0x0][0x144];\n"
                         "MOV R2, c[2][0]\n;"
                         "MOV R3, c[2][4]\n;" +
                             code +
                             "STG.E [R0], R2;\n"
                             "EXIT;\n",
                         std::array{base, offset | (count << 8)});
}

TEST_CASE("BFE Simple", "[shader]") {
    REQUIRE(Run(0xdead, 8, 8, "BFE.U32 R2, R2, R3;") == 0xde);
    REQUIRE(Run(0xdead, 8, 8, "BFE.U32 R2, R2, c[2][4];") == 0xde);
    REQUIRE(Run(0xdead, 8, 8, "BFE.U32 R2, R2, 0x0808;") == 0xde);

    REQUIRE(Run(0xdead, 4, 4, "BFE.U32 R2, R2, R3;") == 0xa);
    REQUIRE(Run(0xdead, 8, 8, "BFE.S32 R2, R2, R3;") == 0xffffffde);

    REQUIRE(Run(0x7ead, 8, 8, "BFE.S32 R2, R2, R3;") == 0x7e);
    REQUIRE(Run(0x8ead, 8, 8, "BFE.S32 R2, R2, R3;") == 0xffffff8e);
}

TEST_CASE("BFE BREV", "[shader]") {
    REQUIRE(Run(0xdead, 8, 16, "BFE.U32.BREV R2, R2, R3;") == 0x7b00);
    REQUIRE(Run(0xdead, 0, 32, "BFE.U32.BREV R2, R2, R3;") == 0xb57b0000);
}

TEST_CASE("BFE Undefined", "[shader][undefined]") {
    REQUIRE(Run(0xdead, 8, 0, "BFE.U32 R2, R2, R3;") == 0);
    REQUIRE(Run(0xdead0000, 24, 16, "BFE.S32 R2, R2, R3;") == 0xffffffde);
    REQUIRE(Run(0xdead0000, 32, 24, "BFE.S32 R2, R2, R3;") == 0xffffffff);
    REQUIRE(Run(0x7ead0000, 32, 24, "BFE.S32 R2, R2, R3;") == 0);
    REQUIRE(Run(0xdead0000, 32, 0, "BFE.S32 R2, R2, R3;") == 0);
    REQUIRE(Run(1, 32, 1, "BFE.S32.BREV R2, R2, R3;") == 0xffffffff);

    REQUIRE(Run(0x80000000, 0, 0x100, "BFE.S32 R2, R2, R3;") == 0);
    REQUIRE(Run(0x80000000, 0, 0xff, "BFE.S32 R2, R2, R3;") == 0x80000000);
}

TEST_CASE("BFE CC", "[shader]") {
    REQUIRE(Run(0xdead, 8, 16, "BFE.U32 RZ.CC, R2, R3; P2R R2, CC, RZ, 0xff;") == 0); // No CC's written
    REQUIRE(Run(0xdead, 16, 16, "BFE.U32 RZ.CC, R2, R3; P2R R2, CC, RZ, 0xff;") == 1); // Zero
    REQUIRE(Run(0xdead, 8, 0, "BFE.U32 RZ.CC, R2, R3; P2R R2, CC, RZ, 0xff;") == 1);   // Zero
    REQUIRE(Run(0xdead0000, 24, 16, "BFE.S32 RZ.CC, R2, R3; P2R R2, CC, RZ, 0xff;") == 2); // Sign
    REQUIRE(Run(1, 32, 1, "BFE.S32.BREV RZ.CC, R2, R3; P2R R2, CC, RZ, 0xff;") == 2);      // Sign
}
