#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

static int32_t Run(int32_t value, std::string code) {
    return EvalUtil::Run(".dksh compute\n"
                         "main:\n"
                         "MOV R0, c[0x0][0x140];\n"
                         "MOV R1, c[0x0][0x144];\n"
                         "ISETP.T.AND P0, P1, RZ, RZ, PT;\n"
                         "MOV R2, 50;\n"
                         "MOV R3, -50;\n"
                         "MOV R4, 75;\n"
                         "MOV R5, 25;\n" +
                             code +
                             "STG.E [R0], R2;\n"
                             "EXIT;\n",
                         value);
}

TEST_CASE("ISET Simple", "[shader]") {
    REQUIRE(Run(0, "ISET.F.AND R2, R4, R5, PT;") == 0);
    REQUIRE(Run(0, "ISET.T.AND R2, R4, R5, PT;") == -1);

    REQUIRE(Run(0, "ISET.LT.AND R2, R4, R5, PT;") == 0);
    REQUIRE(Run(0, "ISET.EQ.AND R2, R4, R5, PT;") == 0);
    REQUIRE(Run(0, "ISET.LE.AND R2, R4, R5, PT;") == 0);
    REQUIRE(Run(0, "ISET.GT.AND R2, R4, R5, PT;") == -1);
    REQUIRE(Run(0, "ISET.NE.AND R2, R4, R5, PT;") == -1);
    REQUIRE(Run(0, "ISET.GE.AND R2, R4, R5, PT;") == -1);

    REQUIRE(Run(0, "ISET.LT.AND R2, R4, R4, PT;") == 0);
    REQUIRE(Run(0, "ISET.EQ.AND R2, R4, R4, PT;") == -1);
    REQUIRE(Run(0, "ISET.LE.AND R2, R4, R4, PT;") == -1);
    REQUIRE(Run(0, "ISET.GT.AND R2, R4, R4, PT;") == 0);
    REQUIRE(Run(0, "ISET.NE.AND R2, R4, R4, PT;") == 0);
    REQUIRE(Run(0, "ISET.GE.AND R2, R4, R4, PT;") == -1);

    REQUIRE(Run(0, "ISET.LT.AND R2, R5, R4, PT;") == -1);
    REQUIRE(Run(0, "ISET.EQ.AND R2, R5, R4, PT;") == 0);
    REQUIRE(Run(0, "ISET.LE.AND R2, R5, R4, PT;") == -1);
    REQUIRE(Run(0, "ISET.GT.AND R2, R5, R4, PT;") == 0);
    REQUIRE(Run(0, "ISET.NE.AND R2, R5, R4, PT;") == -1);
    REQUIRE(Run(0, "ISET.GE.AND R2, R5, R4, PT;") == 0);

    REQUIRE(Run(0, "ISET.EQ.AND R2, R4, 75, PT;") == -1);
    REQUIRE(Run(75, "ISET.EQ.AND R2, R4, c[2][0], PT;") == -1);
}

TEST_CASE("ISET Negatives", "[shader]") {
    REQUIRE(Run(0, "ISET.LT.U32.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, "ISET.EQ.U32.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, "ISET.LE.U32.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, "ISET.GT.U32.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, "ISET.NE.U32.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, "ISET.GE.U32.AND R2, R4, R3, PT;") == 0);

    REQUIRE(Run(0, "ISET.LT.S32.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, "ISET.EQ.S32.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, "ISET.LE.S32.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, "ISET.GT.S32.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, "ISET.NE.S32.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, "ISET.GE.S32.AND R2, R4, R3, PT;") == -1);
}

TEST_CASE("ISET Combining", "[shader]") {
    REQUIRE(Run(0, "ISET.EQ.XOR R2, R4, R4, PT;") == 0);
    REQUIRE(Run(0, "ISET.NE.OR R2, R4, R4, PT;") == -1);
    REQUIRE(Run(0, "ISET.LT.AND R2, R4, R4, !PT;") == 0);
}

TEST_CASE("ISET BF", "[shader]") {
    REQUIRE(Run(0, "ISET.BF.EQ.AND R2, R4, R4, PT;") == 0x3f80'0000);
}
