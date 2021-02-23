#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

static uint32_t Run(std::string code, uint32_t value = 0) {
    return EvalUtil::Run(".dksh compute\n"
                         "main:\n"
                         "MOV R0, c[0x0][0x140];\n"
                         "MOV R1, c[0x0][0x144];\n"
                         "MOV R2, 1;\n"
                         "MOV R3, 0x10000;\n"
                         "MOV R4, 2;\n"
                         "MOV R5, 0x20000;\n"
                         "MOV32I R6, 0xfffffffe;\n" +
                             code +
                             "STG.E [R0], R2;\n"
                             "EXIT;\n",
                         value);
}

static uint32_t FuzzRun(std::string code, uint32_t value = 0) {
    return EvalUtil::Run(".dksh compute\n"
                         "main:\n"
                         "MOV R0, c[0x0][0x140];\n"
                         "MOV R1, c[0x0][0x144];\n"
                         "MOV32I R2, 0x12345678;\n"
                         "MOV32I R3, 0x8a24c923;\n"
                         "MOV32I R4, 0x9123064a;\n" +
                             code +
                             "STG.E [R0], R2;\n"
                             "EXIT;\n",
                         value);
}

static int32_t SRun(std::string code, uint32_t value = 0) {
    return int32_t(Run(std::move(code), value));
}

TEST_CASE("XMAD Simple", "[shader]") {
    REQUIRE(Run("XMAD R2, R2, R4, R2;") == 3);
    REQUIRE(Run("XMAD R2, R3.H1, R5.H1, R2;") == 3);
}

TEST_CASE("XMAD Signs", "[shader]") {
    REQUIRE(SRun("XMAD.S16.S16 R2, R6.H1, R4, R2;") == -1);
    REQUIRE(SRun("XMAD.U16.U16 R2, R6.H1, R4, R2;") == 0x1ffff);
    REQUIRE(SRun("XMAD.S16.U16 R2, R6.H1, R4, R2;") == -1);
    REQUIRE(SRun("XMAD.U16.S16 R2, R6.H1, R4, R2;") == 0x1ffff);

    REQUIRE(SRun("XMAD.S16.S16 R2, R6, c[2][0].H1, R2;", 0xfffeffff) == 5);
    REQUIRE(SRun("XMAD.U16.U16 R2, R6, c[2][0].H1, R2;", 0xfffeffff) == -262139);

    REQUIRE(SRun("XMAD.S16.S16 R2, R6.H1, R6.H1, c[2][0];", 14) == 15);
    REQUIRE(SRun("XMAD.U16.U16 R2, R6.H1, R6.H1, c[2][0];", 14) == -131057);
}

TEST_CASE("XMAD Mode", "[shader]") {
    REQUIRE(FuzzRun("XMAD R2, R2, R3, R4;") == 0xd51310b2);
    REQUIRE(FuzzRun("XMAD.CLO R2, R2, R3, R4;") == 0x43f010b2);
    REQUIRE(FuzzRun("XMAD.CHI R2, R2, R3, R4;") == 0x43f09b8b);
    REQUIRE(FuzzRun("XMAD.CBCC R2, R2, R3, R4;") == 0x9e3610b2);

    REQUIRE(FuzzRun("XMAD R2, R2, R3, c[2][0];", 0x58238134) == 0x9c138b9c);
    REQUIRE(FuzzRun("XMAD.CLO R2, R2, R3, c[2][0];", 0x58238134) == 0x43f08b9c);
    REQUIRE(FuzzRun("XMAD.CHI R2, R2, R3, c[2][0];", 0x58238134) == 0x43f0628b);

    REQUIRE(FuzzRun("XMAD R2, R2, c[2][0], R4;", 0x58238134) == 0xbcc70eaa);
    REQUIRE(FuzzRun("XMAD.CLO R2, R2, c[2][0], R4;", 0x58238134) == 0x2ba40eaa);
    REQUIRE(FuzzRun("XMAD.CHI R2, R2, c[2][0], R4;", 0x58238134) == 0x2ba49983);

    REQUIRE(FuzzRun("XMAD R2, R2, 34, R4;") == 0x912e823a);
    REQUIRE(FuzzRun("XMAD.CLO R2, R2, 34, R4;") == 0xb823a);
    REQUIRE(FuzzRun("XMAD.CHI R2, R2, 34, R4;") == 0xc0d13);
    REQUIRE(FuzzRun("XMAD.CBCC R2, R2, 34, R4;") == 0x9150823a);
}

TEST_CASE("XMAD PSL", "[shader]") {
    REQUIRE(FuzzRun("XMAD.PSL R2, R2, R3, R4;") == 0x9b8b064a);
    REQUIRE(FuzzRun("XMAD.PSL.CLO R2, R2, R3, R4;") == 0xa68064a);
    REQUIRE(FuzzRun("XMAD.PSL.CHI R2, R2, R3, R4;") == 0xa689123);
    REQUIRE(FuzzRun("XMAD.PSL.CBCC R2, R2, R3, R4;") == 0x64ae064a);

    REQUIRE(FuzzRun("XMAD.PSL R2, R2, c[2][0], R4;", 0x58238134) == 0x9983064a);
    REQUIRE(FuzzRun("XMAD.PSL.CLO R2, R2, c[2][0], R4;", 0x58238134) == 0x860064a);
    REQUIRE(FuzzRun("XMAD.PSL.CHI R2, R2, c[2][0], R4;", 0x58238134) == 0x8609123);
}

TEST_CASE("XMAD MRG", "[shader]") {
    REQUIRE(FuzzRun("XMAD.MRG R2, R2, R3, R4;") == 0xc92310b2);
    REQUIRE(FuzzRun("XMAD.CLO.MRG R2, R2, R3, R4;") == 0xc92310b2);
    REQUIRE(FuzzRun("XMAD.CHI.MRG R2, R2, R3, R4;") == 0xc9239b8b);
    REQUIRE(FuzzRun("XMAD.CBCC.MRG R2, R2, R3, R4;") == 0xc92310b2);

    REQUIRE(FuzzRun("XMAD.MRG R2, R2, c[2][0], R4;", 0x58238134) == 0x81340eaa);
    REQUIRE(FuzzRun("XMAD.CLO.MRG R2, R2, c[2][0], R4;", 0x58238134) == 0x81340eaa);
    REQUIRE(FuzzRun("XMAD.CHI.MRG R2, R2, c[2][0], R4;", 0x58238134) == 0x81349983);
}
