#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

static uint32_t Run(uint32_t value, std::string code) {
    return EvalUtil::Run(".dksh compute\n"
                         "main:\n"
                         "MOV R0, c[0x0][0x140];\n"
                         "MOV R1, c[0x0][0x144];\n" +
                             code +
                             "STG.E [R0], R2;\n"
                             "EXIT;\n",
                         value);
}

TEST_CASE("IADD Simple", "[shader]") {
    REQUIRE(Run(16, "IADD R2, RZ, c[2][0];") == 16);
    REQUIRE(Run(7, "MOV R2, 7; IADD R2, R2, c[2][0];") == 14);
    REQUIRE(Run(7, "MOV R2, 7; IADD R2, R2, 7;") == 14);
    REQUIRE(Run(7, "MOV R2, 7; IADD R2, R2, R2;") == 14);
}

TEST_CASE("IADD Negate", "[shader]") {
    REQUIRE(Run(16, "IADD R2, RZ, -c[2][0];") == uint32_t(-16));
    REQUIRE(Run(16, "MOV R2, -5; IADD R2, -R2, RZ;") == 5);
    REQUIRE(Run(16, "MOV R2, 5; IADD R2, -R2, RZ;") == uint32_t(-5));
}

TEST_CASE("IADD Negate undefined", "[shader][undefined]") {
    REQUIRE(Run(0x80000000, "IADD R2, RZ, -c[2][0];") == 0x80000000);
}

TEST_CASE("IADD CC", "[shader]") {
    REQUIRE(Run(1, "IADD RZ.CC, RZ, RZ; P2R R2, CC, RZ, 0xff;") == 1);                 // Zero
    REQUIRE(Run(1, "IADD RZ.CC, RZ, -1; P2R R2, CC, RZ, 0xff;") == 2);                 // Sign
    REQUIRE(Run(1, "IADD R2, RZ, -1; IADD RZ.CC, R2, 2; P2R R2, CC, RZ, 0xff;") == 4); // Carry
    REQUIRE(Run(1, "MOV32I R2, 0x7fffffff;"
                   "IADD RZ.CC, R2, 2; P2R R2, CC, RZ, 0xff;") == 10); // Overflow
    REQUIRE(Run(1, "MOV32I R2, 1;"
                   "IADD RZ.CC, R2, -1; P2R R2, CC, RZ, 0xff;") == 5); // Zero+Carry
}

TEST_CASE("IADD X", "[shader]") {
    REQUIRE(Run(1, "MOV32I R2, 0xffffffff;"
                   "IADD RZ.CC, R2, 1;"
                   "IADD.X R2, RZ, RZ;") == 1);
    REQUIRE(Run(1, "MOV32I R2, 0xffffffff;"
                   "IADD RZ.CC, R2, 1;"
                   "IADD.X R2, RZ, 4;") == 5);
}

TEST_CASE("IADD PO", "[shader]") {
    REQUIRE(Run(1, "IADD.PO R2, RZ, 5;") == 6);
    // PO + X aborts execution
}

TEST_CASE("IADD32I", "[shader]") {
    // IADD32I has different encoding, test each bit individually
    REQUIRE(Run(1, "IADD32I RZ.CC, RZ, 0; P2R R2, CC, RZ, 0xff;") == 1);
    REQUIRE(Run(1, "MOV R2, -1; IADD32I RZ.CC, R2, 3; IADD32I.X R2, RZ, 0;") == 1);
    REQUIRE(Run(1, "IADD32I.PO R2, RZ, 0;") == 1);
    REQUIRE(Run(1, "MOV R2, 1; IADD32I R2, -R2, 0;") == 0xffffffff);
}

// TODO: Test for CC + X
// TODO: Test for CC + PO
// TODO: Test for SAT
