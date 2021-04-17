#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

static uint32_t Run(uint32_t base, uint32_t insert, uint32_t offset, uint32_t count,
                    std::string code) {
    return EvalUtil::Run(".dksh compute\n"
                         "main:\n"
                         "MOV R0, c[0x0][0x140];\n"
                         "MOV R1, c[0x0][0x144];\n"
                         "MOV R4, c[2][0]\n;"
                         "MOV R2, c[2][4]\n;"
                         "MOV R3, c[2][8]\n;" +
                             code +
                             "STG.E [R0], R2;\n"
                             "EXIT;\n",
                         std::array{base, insert, offset | (count << 8), 0U});
}

TEST_CASE("BFI Simple", "[shader]") {
    REQUIRE(Run(0xcccccccc, 0, 8, 8, "BFI R2, R2, R3, R4;") == 0xcccc00cc);
    REQUIRE(Run(0xcccccccc, 0, 4, 8, "BFI R2, R2, R3, R4;") == 0xccccc00c);
    REQUIRE(Run(0xcccccccc, 0xffdd, 12, 8, "BFI R2, R2, R3, R4;") == 0xcccddccc);
    REQUIRE(Run(0xcccccc32, 0xffdd, 12, 8, "BFI R2, R2, R3, R4;") == 0xcccddc32);

    REQUIRE(Run(0xcccccc32, 0xffdd, 12, 8, "BFI R2, R2, R3, c[2][0];") == 0xcccddc32);
    REQUIRE(Run(0xcccccc32, 0xffdd, 12, 8, "BFI R2, R2, c[2][8], R4;") == 0xcccddc32);
    REQUIRE(Run(0xcccccc32, 0xffdd, 12, 8, "BFI R2, R2, 0x080c, R4;") == 0xcccddc32);
}

TEST_CASE("BFI Undefined", "[shader][undefined]") {
    REQUIRE(Run(0xcccccccc, 0xcafe, 8, 0, "BFI R2, R2, R3, R4;") == 0xcccccccc);
    REQUIRE(Run(0xcccccccc, 0xcafe, 8, 64, "BFI R2, R2, R3, R4;") == 0x00cafecc);
    REQUIRE(Run(0xcccccccc, 0xcafe, 32, 32, "BFI R2, R2, R3, R4;") == 0xcccccccc);
}

TEST_CASE("BFI CC", "[shader]") {
    REQUIRE(Run(0x00cccccc, 0xcafe, 8, 0, "BFI RZ.CC, R2, R3, R4; P2R R2, CC, RZ, 0xff;") == 0); // No CC's written
    REQUIRE(Run(0xcccccccc, 0xcafe, 8, 64, "BFI RZ.CC, R2, R3, R4; P2R R2, CC, RZ, 0xff;") == 0); // No CC's written
    REQUIRE(Run(0xcccccccc, 0, 0, 32, "BFI RZ.CC, R2, R3, R4; P2R R2, CC, RZ, 0xff;") == 1); // Zero
    REQUIRE(Run(0xcafe, 0, 0, 16, "BFI RZ.CC, R2, R3, R4; P2R R2, CC, RZ, 0xff;") == 1);     // Zero
    REQUIRE(Run(0x00cccccc, 0xcc, 28, 4, "BFI RZ.CC, R2, R3, R4; P2R R2, CC, RZ, 0xff;") == 2);  // Sign
    REQUIRE(Run(0xcccccccc, 0xcafe, 8, 0, "BFI RZ.CC, R2, R3, R4; P2R R2, CC, RZ, 0xff;") == 2); // Sign
}
