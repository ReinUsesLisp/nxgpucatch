#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

static uint32_t Run(uint32_t value, std::string code) {
    return EvalUtil::Run(".dksh compute\n"
                         "main:\n"
                         "MOV R0, c[0x0][0x140];\n"
                         "MOV R1, c[0x0][0x144];\n"
                         "ISETP.NE.AND P0, PT, RZ, RZ, PT;\n" // Reset PR
                         "ISETP.NE.AND P1, PT, RZ, RZ, PT;\n"
                         "ISETP.NE.AND P2, PT, RZ, RZ, PT;\n"
                         "ISETP.NE.AND P3, PT, RZ, RZ, PT;\n"
                         "ISETP.NE.AND P4, PT, RZ, RZ, PT;\n"
                         "ISETP.NE.AND P5, PT, RZ, RZ, PT;\n"
                         "ISETP.NE.AND P6, PT, RZ, RZ, PT;\n" +
                             code +
                             "P2R R2, PR, RZ, 0xff;\n"
                             "STG.E [R0], R2;\n"
                             "EXIT;\n",
                         value);
}

TEST_CASE("ISETP Simple", "[shader]") {
    REQUIRE(Run(17, R"(
        MOV R2, 17;
        MOV R3, 34;
        ISETP.F.AND  P0, PT, RZ, c[2][0], PT;
        ISETP.LT.AND P1, PT, RZ, c[2][0], PT;
        ISETP.EQ.AND P2, PT, R2, c[2][0], PT;
        ISETP.LE.AND P3, PT, R2, c[2][0], PT;
        ISETP.GT.AND P4, PT, R3, c[2][0], PT;
        ISETP.NE.AND P5, PT, R3, c[2][0], PT;
        ISETP.GE.AND P6, PT, R2, c[2][0], PT;
    )") == 0x7e);
    REQUIRE(Run(0, "ISETP.T.AND P0, PT, RZ, c[2][0], PT;\n") == 1);
    REQUIRE(Run(0, R"(
        MOV R2, 7;
        MOV R3, 5;
        ISETP.GT.AND P0, PT, R2, R3, PT;
    )") == 1);
    REQUIRE(Run(0, R"(
        MOV R2, 7;
        ISETP.GT.AND P0, PT, R2, 5, PT;
    )") == 1);
}

TEST_CASE("ISETP Combining", "[shader]") {
    REQUIRE(Run(17, "ISETP.GT.AND P0, P1, RZ, c[2][0], PT;") == 2);
    REQUIRE(Run(17, "ISETP.GT.OR P0, P1, RZ, c[2][0], P1;") == 2);
    REQUIRE(Run(17, "ISETP.GT.OR P0, P1, RZ, c[2][0], PT;") == 3);
    REQUIRE(Run(17, "ISETP.LT.AND P0, P1, RZ, c[2][0], PT;") == 1);
    REQUIRE(Run(17, R"(
        ISETP.T.AND P0, PT, RZ, c[2][0], PT;
        ISETP.GT.AND P1, P2, RZ, c[2][0], !PT;
        ISETP.GT.AND P3, P4, RZ, c[2][0], P0;
    )") == 0b10001);
}

TEST_CASE("ISETP Negatives", "[shader]") {
    REQUIRE(Run(uint32_t(-17), R"(
        MOV R2, -17;
        MOV R3, -34;
        ISETP.F.AND  P0, PT, RZ, c[2][0], PT;
        ISETP.LT.AND P1, PT, RZ, c[2][0], PT;
        ISETP.EQ.AND P2, PT, R2, c[2][0], PT;
        ISETP.LE.AND P3, PT, R2, c[2][0], PT;
        ISETP.GT.AND P4, PT, R3, c[2][0], PT;
        ISETP.NE.AND P5, PT, R3, c[2][0], PT;
        ISETP.GE.AND P6, PT, R2, c[2][0], PT;
    )") == 0b1101100);
    REQUIRE(Run(uint32_t(-17), R"(
        MOV R2, -17;
        MOV R3, -34;
        ISETP.F.U32.AND  P0, PT, RZ, c[2][0], PT;
        ISETP.LT.U32.AND P1, PT, RZ, c[2][0], PT;
        ISETP.EQ.U32.AND P2, PT, R2, c[2][0], PT;
        ISETP.LE.U32.AND P3, PT, R2, c[2][0], PT;
        ISETP.GT.U32.AND P4, PT, R3, c[2][0], PT;
        ISETP.NE.U32.AND P5, PT, R3, c[2][0], PT;
        ISETP.GE.U32.AND P6, PT, R2, c[2][0], PT;
    )") == 0b1101110);
}
