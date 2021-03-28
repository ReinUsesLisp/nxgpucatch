#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

static uint32_t Run(uint32_t value, std::string code) {
    return EvalUtil::Run(".dksh compute\n"
                         "main:\n"
                         "MOV R0, c[0x0][0x140];\n"
                         "MOV R1, c[0x0][0x144];\n"
                         "MOV R2, 2;"
                         "MOV R3, 3;"
                         "MOV R4, 4;" +
                             code +
                             "STG.E [R0], R2;\n"
                             "EXIT;\n",
                         value);
}

TEST_CASE("ISCADD Simple", "[shader]") {
    REQUIRE(Run(16, "ISCADD R2, RZ, c[2][0], 2;") == 16);
    REQUIRE(Run(7, "MOV R2, 7; ISCADD R2, R2, c[2][0], 2;") == 35);
    REQUIRE(Run(7, "MOV R2, 7; ISCADD R2, R3, 7, 3;") == 31);
    REQUIRE(Run(7, "MOV R2, 7; ISCADD R2, R4, R2, 4;") == 71);

    REQUIRE(Run(1, "MOV32I R2, 0xffffffff;"
                   "ISCADD R2.CC, R2, 0, 1;") == 0xffff'fffe);
}

TEST_CASE("ISCADD Negate", "[shader]") {
    REQUIRE(Run(16, "ISCADD R2, -R3, c[2][0], 2;") == 4);
    REQUIRE(Run(16, "ISCADD R2, R3, -c[2][0], 2;") == 0xffff'fffc);
}

TEST_CASE("ISCADD CC", "[shader]") {
    REQUIRE(Run(1, "ISCADD RZ.CC, RZ, RZ, 0;"
                   "P2R R2, CC, RZ, 0xff;") == 1);
    REQUIRE(Run(1, "ISCADD RZ.CC, RZ, RZ, 16;"
                   "P2R R2, CC, RZ, 0xff;") == 1);
    REQUIRE(Run(1, "ISCADD RZ.CC, RZ, -1, 0;"
                   "P2R R2, CC, RZ, 0xff;") == 2);
    REQUIRE(Run(1, "ISCADD R2, RZ, -1, 0;"
                   "ISCADD RZ.CC, R2, 2, 0;"
                   "P2R R2, CC, RZ, 0xff;") == 4);
    REQUIRE(Run(1, "MOV32I R2, 0x7fffffff;"
                   "ISCADD RZ.CC, R2, 2, 0;"
                   "P2R R2, CC, RZ, 0xff;") == 10);
    // No carry from shift
    REQUIRE(Run(1, "MOV32I R2, 0xffffffff;"
                   "ISCADD RZ.CC, R2, 0, 1;"
                   "P2R R2, CC, RZ, 0xff;") == 2);
    // Carry and zero when adding after shift
    REQUIRE(Run(1, "MOV32I R2, 0xffffffff;"
                   "ISCADD RZ.CC, R2, 2, 1;"
                   "P2R R2, CC, RZ, 0xff;") == 5);
    // Sign from the shift
    REQUIRE(Run(1, "MOV32I R2, 1;"
                   "ISCADD RZ.CC, R2, 0, 31;"
                   "P2R R2, CC, RZ, 0xff;") == 2);

}

TEST_CASE("ISCADD PO", "[shader]") {
    REQUIRE(Run(1, "MOV32I R2, 0xffffffff;"
                   "ISCADD.PO R2.CC, R2, 0, 1;") == 0xffff'ffff);
}

// TODO: Test CC + P0
