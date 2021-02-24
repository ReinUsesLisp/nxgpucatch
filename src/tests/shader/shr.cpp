#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

static uint32_t Run(uint32_t value, std::string code) {
    return EvalUtil::Run(".dksh compute\n"
                         "main:\n"
                         "MOV R0, c[0x0][0x140];\n"
                         "MOV R1, c[0x0][0x144];\n"
                         "MOV R3, 64;\n"
                         "MOV R4, -64;\n" +
                             code +
                             "STG.E [R0], R2;\n"
                             "EXIT;\n",
                         value);
}

static int32_t SRun(uint32_t value, std::string code) {
    return static_cast<int32_t>(Run(value, std::move(code)));
}

TEST_CASE("SHR Simple", "[shader]") {
    REQUIRE(Run(3, "MOV R2, 3; SHR.U32 R2, R3, R2;") == 8);
    REQUIRE(Run(3, "SHR.U32 R2, R3, c[2][0];") == 8);
    REQUIRE(Run(3, "SHR.U32 R2, R3, 3;") == 8);
}

TEST_CASE("SHR Negatives", "[shader]") {
    REQUIRE(SRun(3, "MOV R2, 3; SHR.S32 R2, R4, R2;") == -8);
    REQUIRE(SRun(3, "SHR.S32 R2, R4, c[2][0];") == -8);
    REQUIRE(SRun(3, "SHR.S32 R2, R4, 3;") == -8);
}

TEST_CASE("SHR BREV", "[shader]") {
    REQUIRE(Run(3, "SHR.U32.BREV R2, R3, c[2][0];") == 0x40'0000);
    REQUIRE(Run(3, "SHR.S32.BREV R2, R3, c[2][0];") == 0x40'0000);

    REQUIRE(Run(3, "SHR.U32.BREV R2, R4, c[2][0];") == 0x7f'ffff);
    REQUIRE(Run(3, "SHR.S32.BREV R2, R4, c[2][0];") == 0x7f'ffff);
}

TEST_CASE("SHR Undefined", "[shader][undefined]") {
    REQUIRE(Run(32, "SHR.U32 R2, R3, c[2][0];") == 0);
    REQUIRE(Run(32, "SHR.U32.W R2, R3, c[2][0];") == 64);
    REQUIRE(Run(33, "SHR.U32 R2, R3, c[2][0];") == 0);
    REQUIRE(Run(33, "SHR.U32.W R2, R3, c[2][0];") == 32);

    REQUIRE(SRun(32, "SHR.S32 R2, R3, c[2][0];") == 0);
    REQUIRE(SRun(32, "SHR.S32 R2, R4, c[2][0];") == -1);
    REQUIRE(SRun(32, "SHR.S32.W R2, R3, c[2][0];") == 64);
    REQUIRE(SRun(32, "SHR.S32.W R2, R4, c[2][0];") == -64);
    REQUIRE(SRun(33, "SHR.S32 R2, R3, c[2][0];") == 0);
    REQUIRE(SRun(33, "SHR.S32 R2, R4, c[2][0];") == -1);
    REQUIRE(SRun(33, "SHR.S32.W R2, R3, c[2][0];") == 32);
    REQUIRE(SRun(33, "SHR.S32.W R2, R4, c[2][0];") == -32);

    REQUIRE(SRun(35, "SHR.S32.BREV R2, R4, c[2][0];") == 0);
    REQUIRE(SRun(35, "SHR.S32.W.BREV R2, R4, c[2][0];") == 0x7f'ffff);
}
