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

TEST_CASE("ISET X Simple", "[shader]") {
    const std::string flags_unset = "IADD R2, RZ, -1; IADD RZ.CC, R2, 0; ";
    const std::string zero_set = "IADD R2, RZ, 0; IADD RZ.CC, R2, 0; ";
    const std::string carry_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 2; ";
    const std::string carry_zero_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 1; ";

    const std::string less_by_one = "MOV R3, 74; ";
    const std::string equal = "MOV R3, 75; ";
    const std::string greater_by_one = "MOV R3, 76; ";
    
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
}

TEST_CASE("ISET X Less By Two", "[shader]") {
    const std::string flags_unset = "IADD R2, RZ, -1; IADD RZ.CC, R2, 0; ";
    const std::string zero_set = "IADD R2, RZ, 0; IADD RZ.CC, R2, 0; ";
    const std::string carry_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 2; ";
    const std::string carry_zero_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 1; ";

    const std::string less_by_two = "MOV R3, 73; ";

    // No flags set
    REQUIRE(Run(0, less_by_two + flags_unset + "ISET.LT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_two + flags_unset + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_two + flags_unset + "ISET.LE.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_two + flags_unset + "ISET.GT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, less_by_two + flags_unset + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, less_by_two + flags_unset + "ISET.GE.X.AND R2, R4, R3, PT;") == -1);

    // Zero flag set
    REQUIRE(Run(0, less_by_two + zero_set + "ISET.LT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_two + zero_set + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_two + zero_set + "ISET.LE.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_two + zero_set + "ISET.GT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, less_by_two + zero_set + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, less_by_two + zero_set + "ISET.GE.X.AND R2, R4, R3, PT;") == -1);

    // Carry flag set
    REQUIRE(Run(0, less_by_two + carry_set + "ISET.LT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_two + carry_set + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_two + carry_set + "ISET.LE.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_two + carry_set + "ISET.GT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, less_by_two + carry_set + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, less_by_two + carry_set + "ISET.GE.X.AND R2, R4, R3, PT;") == -1);

    // Carry+Zero flag set
    REQUIRE(Run(0, less_by_two + carry_zero_set + "ISET.LT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_two + carry_zero_set + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_two + carry_zero_set + "ISET.LE.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_two + carry_zero_set + "ISET.GT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, less_by_two + carry_zero_set + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, less_by_two + carry_zero_set + "ISET.GE.X.AND R2, R4, R3, PT;") == -1);
}

TEST_CASE("ISET X U32 Less By Two", "[shader]") {
    const std::string flags_unset = "IADD R2, RZ, -1; IADD RZ.CC, R2, 0; ";
    const std::string zero_set = "IADD R2, RZ, 0; IADD RZ.CC, R2, 0; ";
    const std::string carry_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 2; ";
    const std::string carry_zero_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 1; ";

    const std::string less_by_two = "MOV R3, 73; ";

    // No flags set
    REQUIRE(Run(0, less_by_two + flags_unset + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_two + flags_unset + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_two + flags_unset + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_two + flags_unset + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, less_by_two + flags_unset + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, less_by_two + flags_unset + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == -1);

    // Zero flag set
    REQUIRE(Run(0, less_by_two + zero_set + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_two + zero_set + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_two + zero_set + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_two + zero_set + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, less_by_two + zero_set + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, less_by_two + zero_set + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == -1);

    // Carry flag set
    REQUIRE(Run(0, less_by_two + carry_set + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_two + carry_set + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_two + carry_set + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_two + carry_set + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, less_by_two + carry_set + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, less_by_two + carry_set + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == -1);

    // Carry+Zero flag set
    REQUIRE(Run(0, less_by_two + carry_zero_set + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_two + carry_zero_set + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_two + carry_zero_set + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_two + carry_zero_set + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, less_by_two + carry_zero_set + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, less_by_two + carry_zero_set + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == -1);
}

TEST_CASE("ISET X Less By One", "[shader]") {
    const std::string flags_unset = "IADD R2, RZ, -1; IADD RZ.CC, R2, 0; ";
    const std::string zero_set = "IADD R2, RZ, 0; IADD RZ.CC, R2, 0; ";
    const std::string carry_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 2; ";
    const std::string carry_zero_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 1; ";

    const std::string less_by_one = "MOV R3, 74; ";

    // No flags set
    REQUIRE(Run(0, less_by_one + flags_unset + "ISET.LT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_one + flags_unset + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_one + flags_unset + "ISET.LE.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_one + flags_unset + "ISET.GT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, less_by_one + flags_unset + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, less_by_one + flags_unset + "ISET.GE.X.AND R2, R4, R3, PT;") == -1);

    // Zero flag set
    REQUIRE(Run(0, less_by_one + zero_set + "ISET.LT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_one + zero_set + "ISET.EQ.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, less_by_one + zero_set + "ISET.LE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, less_by_one + zero_set + "ISET.GT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_one + zero_set + "ISET.NE.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_one + zero_set + "ISET.GE.X.AND R2, R4, R3, PT;") == -1);

    // Carry flag set
    REQUIRE(Run(0, less_by_one + carry_set + "ISET.LT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_one + carry_set + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_one + carry_set + "ISET.LE.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_one + carry_set + "ISET.GT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, less_by_one + carry_set + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, less_by_one + carry_set + "ISET.GE.X.AND R2, R4, R3, PT;") == -1);

    // Carry+Zero flag set
    REQUIRE(Run(0, less_by_one + carry_zero_set + "ISET.LT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_one + carry_zero_set + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_one + carry_zero_set + "ISET.LE.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_one + carry_zero_set + "ISET.GT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, less_by_one + carry_zero_set + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, less_by_one + carry_zero_set + "ISET.GE.X.AND R2, R4, R3, PT;") == -1);
}

TEST_CASE("ISET X U32 Less By One", "[shader]") {
    const std::string flags_unset = "IADD R2, RZ, -1; IADD RZ.CC, R2, 0; ";
    const std::string zero_set = "IADD R2, RZ, 0; IADD RZ.CC, R2, 0; ";
    const std::string carry_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 2; ";
    const std::string carry_zero_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 1; ";

    const std::string less_by_one = "MOV R3, 74; ";

    // No flags set
    REQUIRE(Run(0, less_by_one + flags_unset + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_one + flags_unset + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_one + flags_unset + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_one + flags_unset + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, less_by_one + flags_unset + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, less_by_one + flags_unset + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == -1);

    // Zero flag set
    REQUIRE(Run(0, less_by_one + zero_set + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_one + zero_set + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, less_by_one + zero_set + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, less_by_one + zero_set + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_one + zero_set + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_one + zero_set + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == -1);

    // Carry flag set
    REQUIRE(Run(0, less_by_one + carry_set + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_one + carry_set + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_one + carry_set + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_one + carry_set + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, less_by_one + carry_set + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, less_by_one + carry_set + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == -1);

    // Carry+Zero flag set
    REQUIRE(Run(0, less_by_one + carry_zero_set + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_one + carry_zero_set + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_one + carry_zero_set + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, less_by_one + carry_zero_set + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, less_by_one + carry_zero_set + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, less_by_one + carry_zero_set + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == -1);
}

TEST_CASE("ISET X Equal", "[shader]") {
    const std::string flags_unset = "IADD R2, RZ, -1; IADD RZ.CC, R2, 0; ";
    const std::string zero_set = "IADD R2, RZ, 0; IADD RZ.CC, R2, 0; ";
    const std::string carry_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 2; ";
    const std::string carry_zero_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 1; ";

    const std::string equal = "MOV R3, 75; ";

    // No flags set
    REQUIRE(Run(0, equal + flags_unset + "ISET.LT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + flags_unset + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + flags_unset + "ISET.LE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + flags_unset + "ISET.GT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + flags_unset + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + flags_unset + "ISET.GE.X.AND R2, R4, R3, PT;") == 0);

    // Zero flag set
    REQUIRE(Run(0, equal + zero_set + "ISET.LT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + zero_set + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + zero_set + "ISET.LE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + zero_set + "ISET.GT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + zero_set + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + zero_set + "ISET.GE.X.AND R2, R4, R3, PT;") == 0);

    // Carry flag set
    REQUIRE(Run(0, equal + carry_set + "ISET.LT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_set + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_set + "ISET.LE.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_set + "ISET.GT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_set + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_set + "ISET.GE.X.AND R2, R4, R3, PT;") == -1);

    // Carry+Zero flag set
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.LT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.EQ.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.LE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.GT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.NE.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.GE.X.AND R2, R4, R3, PT;") == -1);
}

TEST_CASE("ISET X U32 Equal", "[shader]") {
    const std::string flags_unset = "IADD R2, RZ, -1; IADD RZ.CC, R2, 0; ";
    const std::string zero_set = "IADD R2, RZ, 0; IADD RZ.CC, R2, 0; ";
    const std::string carry_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 2; ";
    const std::string carry_zero_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 1; ";

    const std::string equal = "MOV R3, 75; ";

    // No flags set
    REQUIRE(Run(0, equal + flags_unset + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + flags_unset + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + flags_unset + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + flags_unset + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + flags_unset + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + flags_unset + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == 0);

    // Zero flag set
    REQUIRE(Run(0, equal + zero_set + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + zero_set + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + zero_set + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + zero_set + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + zero_set + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + zero_set + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == 0);

    // Carry flag set
    REQUIRE(Run(0, equal + carry_set + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_set + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_set + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_set + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_set + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_set + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == -1);

    // Carry+Zero flag set
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == -1);
}

TEST_CASE("ISET X Greater By One", "[shader]") {
    const std::string flags_unset = "IADD R2, RZ, -1; IADD RZ.CC, R2, 0; ";
    const std::string zero_set = "IADD R2, RZ, 0; IADD RZ.CC, R2, 0; ";
    const std::string carry_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 2; ";
    const std::string carry_zero_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 1; ";

    const std::string greater_by_one = "MOV R3, 76; ";

    // No flags set
    REQUIRE(Run(0, greater_by_one + flags_unset + "ISET.LT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_one + flags_unset + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, greater_by_one + flags_unset + "ISET.LE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_one + flags_unset + "ISET.GT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, greater_by_one + flags_unset + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_one + flags_unset + "ISET.GE.X.AND R2, R4, R3, PT;") == 0);

    // Zero flag set
    REQUIRE(Run(0, greater_by_one + zero_set + "ISET.LT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_one + zero_set + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, greater_by_one + zero_set + "ISET.LE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_one + zero_set + "ISET.GT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, greater_by_one + zero_set + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_one + zero_set + "ISET.GE.X.AND R2, R4, R3, PT;") == 0);

    // Carry flag set
    REQUIRE(Run(0, greater_by_one + carry_set + "ISET.LT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_one + carry_set + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, greater_by_one + carry_set + "ISET.LE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_one + carry_set + "ISET.GT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, greater_by_one + carry_set + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_one + carry_set + "ISET.GE.X.AND R2, R4, R3, PT;") == 0);

    // Carry+Zero flag set
    REQUIRE(Run(0, greater_by_one + carry_zero_set + "ISET.LT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_one + carry_zero_set + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, greater_by_one + carry_zero_set + "ISET.LE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_one + carry_zero_set + "ISET.GT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, greater_by_one + carry_zero_set + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_one + carry_zero_set + "ISET.GE.X.AND R2, R4, R3, PT;") == 0);
}

TEST_CASE("ISET X U32 Greater By One", "[shader]") {
    const std::string flags_unset = "IADD R2, RZ, -1; IADD RZ.CC, R2, 0; ";
    const std::string zero_set = "IADD R2, RZ, 0; IADD RZ.CC, R2, 0; ";
    const std::string carry_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 2; ";
    const std::string carry_zero_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 1; ";

    const std::string greater_by_one = "MOV R3, 76; ";

    // No flags set
    REQUIRE(Run(0, greater_by_one + flags_unset + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_one + flags_unset + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, greater_by_one + flags_unset + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_one + flags_unset + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, greater_by_one + flags_unset + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_one + flags_unset + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == 0);

    // Zero flag set
    REQUIRE(Run(0, greater_by_one + zero_set + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_one + zero_set + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, greater_by_one + zero_set + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_one + zero_set + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, greater_by_one + zero_set + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_one + zero_set + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == 0);

    // Carry flag set
    REQUIRE(Run(0, greater_by_one + carry_set + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_one + carry_set + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, greater_by_one + carry_set + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_one + carry_set + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, greater_by_one + carry_set + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_one + carry_set + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == 0);

    // Carry+Zero flag set
    REQUIRE(Run(0, greater_by_one + carry_zero_set + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_one + carry_zero_set + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, greater_by_one + carry_zero_set + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_one + carry_zero_set + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, greater_by_one + carry_zero_set + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_one + carry_zero_set + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == 0);
}

TEST_CASE("ISET X Greater By Two", "[shader]") {
    const std::string flags_unset = "IADD R2, RZ, -1; IADD RZ.CC, R2, 0; ";
    const std::string zero_set = "IADD R2, RZ, 0; IADD RZ.CC, R2, 0; ";
    const std::string carry_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 2; ";
    const std::string carry_zero_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 1; ";

    const std::string greater_by_two = "MOV R3, 77; ";

    // No flags set
    REQUIRE(Run(0, greater_by_two + flags_unset + "ISET.LT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_two + flags_unset + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, greater_by_two + flags_unset + "ISET.LE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_two + flags_unset + "ISET.GT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, greater_by_two + flags_unset + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_two + flags_unset + "ISET.GE.X.AND R2, R4, R3, PT;") == 0);

    // Zero flag set
    REQUIRE(Run(0, greater_by_two + zero_set + "ISET.LT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_two + zero_set + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, greater_by_two + zero_set + "ISET.LE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_two + zero_set + "ISET.GT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, greater_by_two + zero_set + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_two + zero_set + "ISET.GE.X.AND R2, R4, R3, PT;") == 0);

    // Carry flag set
    REQUIRE(Run(0, greater_by_two + carry_set + "ISET.LT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_two + carry_set + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, greater_by_two + carry_set + "ISET.LE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_two + carry_set + "ISET.GT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, greater_by_two + carry_set + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_two + carry_set + "ISET.GE.X.AND R2, R4, R3, PT;") == 0);

    // Carry+Zero flag set
    REQUIRE(Run(0, greater_by_two + carry_zero_set + "ISET.LT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_two + carry_zero_set + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, greater_by_two + carry_zero_set + "ISET.LE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_two + carry_zero_set + "ISET.GT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, greater_by_two + carry_zero_set + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_two + carry_zero_set + "ISET.GE.X.AND R2, R4, R3, PT;") == 0);
}

TEST_CASE("ISET X U32 Greater By Two", "[shader]") {
    const std::string flags_unset = "IADD R2, RZ, -1; IADD RZ.CC, R2, 0; ";
    const std::string zero_set = "IADD R2, RZ, 0; IADD RZ.CC, R2, 0; ";
    const std::string carry_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 2; ";
    const std::string carry_zero_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 1; ";

    const std::string greater_by_two = "MOV R3, 77; ";

    // No flags set
    REQUIRE(Run(0, greater_by_two + flags_unset + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_two + flags_unset + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, greater_by_two + flags_unset + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_two + flags_unset + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, greater_by_two + flags_unset + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_two + flags_unset + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == 0);

    // Zero flag set
    REQUIRE(Run(0, greater_by_two + zero_set + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_two + zero_set + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, greater_by_two + zero_set + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_two + zero_set + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, greater_by_two + zero_set + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_two + zero_set + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == 0);

    // Carry flag set
    REQUIRE(Run(0, greater_by_two + carry_set + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_two + carry_set + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, greater_by_two + carry_set + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_two + carry_set + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, greater_by_two + carry_set + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_two + carry_set + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == 0);

    // Carry+Zero flag set
    REQUIRE(Run(0, greater_by_two + carry_zero_set + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_two + carry_zero_set + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, greater_by_two + carry_zero_set + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_two + carry_zero_set + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, greater_by_two + carry_zero_set + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, greater_by_two + carry_zero_set + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == 0);
}

TEST_CASE("ISET X Negative A", "[shader]") {
    const std::string flags_unset = "IADD R2, RZ, -1; IADD RZ.CC, R2, 0; ";
    const std::string zero_set = "IADD R2, RZ, 0; IADD RZ.CC, R2, 0; ";
    const std::string carry_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 2; ";
    const std::string carry_zero_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 1; ";

    const std::string equal = "MOV R3, 75; MOV R4, -30;";

    // No flags set
    REQUIRE(Run(0, equal + flags_unset + "ISET.LT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + flags_unset + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + flags_unset + "ISET.LE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + flags_unset + "ISET.GT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + flags_unset + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + flags_unset + "ISET.GE.X.AND R2, R4, R3, PT;") == 0);

    // Zero flag set
    REQUIRE(Run(0, equal + zero_set + "ISET.LT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + zero_set + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + zero_set + "ISET.LE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + zero_set + "ISET.GT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + zero_set + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + zero_set + "ISET.GE.X.AND R2, R4, R3, PT;") == 0);

    // Carry flag set
    REQUIRE(Run(0, equal + carry_set + "ISET.LT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_set + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_set + "ISET.LE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_set + "ISET.GT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_set + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_set + "ISET.GE.X.AND R2, R4, R3, PT;") == 0);

    // Carry+Zero flag set
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.LT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.LE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.GT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.GE.X.AND R2, R4, R3, PT;") == 0);
}

TEST_CASE("ISET X U32 Negative A", "[shader]") {
    const std::string flags_unset = "IADD R2, RZ, -1; IADD RZ.CC, R2, 0; ";
    const std::string zero_set = "IADD R2, RZ, 0; IADD RZ.CC, R2, 0; ";
    const std::string carry_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 2; ";
    const std::string carry_zero_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 1; ";

    const std::string equal = "MOV R3, 75; MOV R4, -30;";

    // No flags set
    REQUIRE(Run(0, equal + flags_unset + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + flags_unset + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + flags_unset + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + flags_unset + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + flags_unset + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + flags_unset + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == -1);

    // Zero flag set
    REQUIRE(Run(0, equal + zero_set + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + zero_set + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + zero_set + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + zero_set + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + zero_set + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + zero_set + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == -1);

    // Carry flag set
    REQUIRE(Run(0, equal + carry_set + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_set + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_set + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_set + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_set + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_set + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == -1);

    // Carry+Zero flag set
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == -1);
}

TEST_CASE("ISET X Negative B", "[shader]") {
    const std::string flags_unset = "IADD R2, RZ, -1; IADD RZ.CC, R2, 0; ";
    const std::string zero_set = "IADD R2, RZ, 0; IADD RZ.CC, R2, 0; ";
    const std::string carry_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 2; ";
    const std::string carry_zero_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 1; ";

    const std::string negative = "MOV R3, -30; ";

    // No flags set
    REQUIRE(Run(0, negative + flags_unset + "ISET.LT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, negative + flags_unset + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, negative + flags_unset + "ISET.LE.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, negative + flags_unset + "ISET.GT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, negative + flags_unset + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, negative + flags_unset + "ISET.GE.X.AND R2, R4, R3, PT;") == -1);

    // Zero flag set
    REQUIRE(Run(0, negative + zero_set + "ISET.LT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, negative + zero_set + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, negative + zero_set + "ISET.LE.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, negative + zero_set + "ISET.GT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, negative + zero_set + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, negative + zero_set + "ISET.GE.X.AND R2, R4, R3, PT;") == -1);

    // Carry flag set
    REQUIRE(Run(0, negative + carry_set + "ISET.LT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, negative + carry_set + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, negative + carry_set + "ISET.LE.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, negative + carry_set + "ISET.GT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, negative + carry_set + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, negative + carry_set + "ISET.GE.X.AND R2, R4, R3, PT;") == -1);

    // Carry+Zero flag set
    REQUIRE(Run(0, negative + carry_zero_set + "ISET.LT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, negative + carry_zero_set + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, negative + carry_zero_set + "ISET.LE.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, negative + carry_zero_set + "ISET.GT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, negative + carry_zero_set + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, negative + carry_zero_set + "ISET.GE.X.AND R2, R4, R3, PT;") == -1);
}

TEST_CASE("ISET X U32 Negative B", "[shader]") {
    const std::string flags_unset = "IADD R2, RZ, -1; IADD RZ.CC, R2, 0; ";
    const std::string zero_set = "IADD R2, RZ, 0; IADD RZ.CC, R2, 0; ";
    const std::string carry_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 2; ";
    const std::string carry_zero_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 1; ";

    const std::string negative = "MOV R3, -30; ";

    // No flags set
    REQUIRE(Run(0, negative + flags_unset + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, negative + flags_unset + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, negative + flags_unset + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, negative + flags_unset + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, negative + flags_unset + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, negative + flags_unset + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == 0);

    // Zero flag set
    REQUIRE(Run(0, negative + zero_set + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, negative + zero_set + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, negative + zero_set + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, negative + zero_set + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, negative + zero_set + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, negative + zero_set + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == 0);

    // Carry flag set
    REQUIRE(Run(0, negative + carry_set + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, negative + carry_set + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, negative + carry_set + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, negative + carry_set + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, negative + carry_set + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, negative + carry_set + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == 0);

    // Carry+Zero flag set
    REQUIRE(Run(0, negative + carry_zero_set + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, negative + carry_zero_set + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, negative + carry_zero_set + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, negative + carry_zero_set + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, negative + carry_zero_set + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, negative + carry_zero_set + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == 0);
}

TEST_CASE("ISET X Negative AB", "[shader]") {
    const std::string flags_unset = "IADD R2, RZ, -1; IADD RZ.CC, R2, 0; ";
    const std::string zero_set = "IADD R2, RZ, 0; IADD RZ.CC, R2, 0; ";
    const std::string carry_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 2; ";
    const std::string carry_zero_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 1; ";

    const std::string equal = "MOV R4, -75; MOV R3, -30;";

    // No flags set
    REQUIRE(Run(0, equal + flags_unset + "ISET.LT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + flags_unset + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + flags_unset + "ISET.LE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + flags_unset + "ISET.GT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + flags_unset + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + flags_unset + "ISET.GE.X.AND R2, R4, R3, PT;") == 0);

    // Zero flag set
    REQUIRE(Run(0, equal + zero_set + "ISET.LT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + zero_set + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + zero_set + "ISET.LE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + zero_set + "ISET.GT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + zero_set + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + zero_set + "ISET.GE.X.AND R2, R4, R3, PT;") == 0);

    // Carry flag set
    REQUIRE(Run(0, equal + carry_set + "ISET.LT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_set + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_set + "ISET.LE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_set + "ISET.GT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_set + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_set + "ISET.GE.X.AND R2, R4, R3, PT;") == 0);

    // Carry+Zero flag set
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.LT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.LE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.GT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.GE.X.AND R2, R4, R3, PT;") == 0);
}

TEST_CASE("ISET X U32 Negative AB", "[shader]") {
    const std::string flags_unset = "IADD R2, RZ, -1; IADD RZ.CC, R2, 0; ";
    const std::string zero_set = "IADD R2, RZ, 0; IADD RZ.CC, R2, 0; ";
    const std::string carry_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 2; ";
    const std::string carry_zero_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 1; ";

    const std::string equal = "MOV R4, -75; MOV R3, -30;";

    // No flags set
    REQUIRE(Run(0, equal + flags_unset + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + flags_unset + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + flags_unset + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + flags_unset + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + flags_unset + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + flags_unset + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == 0);

    // Zero flag set
    REQUIRE(Run(0, equal + zero_set + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + zero_set + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + zero_set + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + zero_set + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + zero_set + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + zero_set + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == 0);

    // Carry flag set
    REQUIRE(Run(0, equal + carry_set + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_set + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_set + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_set + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_set + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_set + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == 0);

    // Carry+Zero flag set
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == 0);
}

TEST_CASE("ISET X Negative BA", "[shader]") {
    const std::string flags_unset = "IADD R2, RZ, -1; IADD RZ.CC, R2, 0; ";
    const std::string zero_set = "IADD R2, RZ, 0; IADD RZ.CC, R2, 0; ";
    const std::string carry_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 2; ";
    const std::string carry_zero_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 1; ";

    const std::string equal = "MOV R3, -75; MOV R4, -30;";

    // No flags set
    REQUIRE(Run(0, equal + flags_unset + "ISET.LT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + flags_unset + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + flags_unset + "ISET.LE.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + flags_unset + "ISET.GT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + flags_unset + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + flags_unset + "ISET.GE.X.AND R2, R4, R3, PT;") == -1);

    // Zero flag set
    REQUIRE(Run(0, equal + zero_set + "ISET.LT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + zero_set + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + zero_set + "ISET.LE.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + zero_set + "ISET.GT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + zero_set + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + zero_set + "ISET.GE.X.AND R2, R4, R3, PT;") == -1);

    // Carry flag set
    REQUIRE(Run(0, equal + carry_set + "ISET.LT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_set + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_set + "ISET.LE.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_set + "ISET.GT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_set + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_set + "ISET.GE.X.AND R2, R4, R3, PT;") == -1);

    // Carry+Zero flag set
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.LT.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.EQ.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.LE.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.GT.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.NE.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.GE.X.AND R2, R4, R3, PT;") == -1);
}

TEST_CASE("ISET X U32 Negative BA", "[shader]") {
    const std::string flags_unset = "IADD R2, RZ, -1; IADD RZ.CC, R2, 0; ";
    const std::string zero_set = "IADD R2, RZ, 0; IADD RZ.CC, R2, 0; ";
    const std::string carry_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 2; ";
    const std::string carry_zero_set = "IADD R2, RZ, -1; IADD RZ.CC, R2, 1; ";

    const std::string equal = "MOV R3, -75; MOV R4, -30;";

    // No flags set
    REQUIRE(Run(0, equal + flags_unset + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + flags_unset + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + flags_unset + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + flags_unset + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + flags_unset + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + flags_unset + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == -1);

    // Zero flag set
    REQUIRE(Run(0, equal + zero_set + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + zero_set + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + zero_set + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + zero_set + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + zero_set + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + zero_set + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == -1);

    // Carry flag set
    REQUIRE(Run(0, equal + carry_set + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_set + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_set + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_set + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_set + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_set + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == -1);

    // Carry+Zero flag set
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.LT.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.EQ.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.LE.U32.X.AND R2, R4, R3, PT;") == 0);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.GT.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.NE.U32.X.AND R2, R4, R3, PT;") == -1);
    REQUIRE(Run(0, equal + carry_zero_set + "ISET.GE.U32.X.AND R2, R4, R3, PT;") == -1);
}