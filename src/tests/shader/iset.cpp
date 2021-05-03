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

TEST_CASE("ISET X", "[shader]") {
    const std::string flags_unset = "IADD R2, RZ, -1; IADD RZ.CC, R2, 0; ";
    const std::string zero_set = "IADD R2, RZ, 0; IADD RZ.CC, R2, 0; ";
    const std::string carry_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 2; ";
    const std::string carry_zero_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 1; ";

    const std::string zero_extend = "MOV R3, 74; ";
    const std::string negative_extend = "MOV R3, 75; ";
    
    // Simple
    // TODO: These fail in hades due to the flags not being set by a prior instruction
    REQUIRE(Run(0, flags_unset + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, flags_unset + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, flags_unset + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, flags_unset + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, flags_unset + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, flags_unset + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == 0);
    
    REQUIRE(Run(0, flags_unset + "ISET.LT.S32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, flags_unset + "ISET.EQ.S32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, flags_unset + "ISET.LE.S32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, flags_unset + "ISET.GT.S32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, flags_unset + "ISET.NE.S32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, flags_unset + "ISET.GE.S32.X.AND R2, R4, R3, PT;") == -1);
    
    // No flags set
    REQUIRE(Run(0, zero_extend + flags_unset + "ISET.LT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, zero_extend + flags_unset + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, zero_extend + flags_unset + "ISET.LE.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, zero_extend + flags_unset + "ISET.GT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, zero_extend + flags_unset + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, zero_extend + flags_unset + "ISET.GE.X.AND R2, R4, R3, PT;") == -1);

    REQUIRE(Run(0, negative_extend + flags_unset + "ISET.LT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, negative_extend + flags_unset + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, negative_extend + flags_unset + "ISET.LE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, negative_extend + flags_unset + "ISET.GT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, negative_extend + flags_unset + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, negative_extend + flags_unset + "ISET.GE.X.AND R2, R4, R3, PT;") == 0);

    // Zero flag set
    REQUIRE(Run(0, zero_extend + zero_set + "ISET.LT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, zero_extend + zero_set + "ISET.EQ.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, zero_extend + zero_set + "ISET.LE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, zero_extend + zero_set + "ISET.GT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, zero_extend + zero_set + "ISET.NE.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, zero_extend + zero_set + "ISET.GE.X.AND R2, R4, R3, PT;") == -1);

    REQUIRE(Run(0, negative_extend + zero_set + "ISET.LT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, negative_extend + zero_set + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, negative_extend + zero_set + "ISET.LE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, negative_extend + zero_set + "ISET.GT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, negative_extend + zero_set + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, negative_extend + zero_set + "ISET.GE.X.AND R2, R4, R3, PT;") == 0);

    // Carry flag set
    REQUIRE(Run(0, zero_extend + carry_set + "ISET.LT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, zero_extend + carry_set + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, zero_extend + carry_set + "ISET.LE.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, zero_extend + carry_set + "ISET.GT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, zero_extend + carry_set + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, zero_extend + carry_set + "ISET.GE.X.AND R2, R4, R3, PT;") == -1);

    REQUIRE(Run(0, negative_extend + carry_set + "ISET.LT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, negative_extend + carry_set + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, negative_extend + carry_set + "ISET.LE.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, negative_extend + carry_set + "ISET.GT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, negative_extend + carry_set + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, negative_extend + carry_set + "ISET.GE.X.AND R2, R4, R3, PT;") == -1);

    // Carry+Zero flag set
    REQUIRE(Run(0, zero_extend + carry_zero_set + "ISET.LT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, zero_extend + carry_zero_set + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, zero_extend + carry_zero_set + "ISET.LE.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, zero_extend + carry_zero_set + "ISET.GT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, zero_extend + carry_zero_set + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, zero_extend + carry_zero_set + "ISET.GE.X.AND R2, R4, R3, PT;") == -1);

    REQUIRE(Run(0, negative_extend + carry_zero_set + "ISET.LT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, negative_extend + carry_zero_set + "ISET.EQ.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, negative_extend + carry_zero_set + "ISET.LE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, negative_extend + carry_zero_set + "ISET.GT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, negative_extend + carry_zero_set + "ISET.NE.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, negative_extend + carry_zero_set + "ISET.GE.X.AND R2, R4, R3, PT;") == -1);
}
