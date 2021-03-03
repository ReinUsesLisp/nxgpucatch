#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

using f16x2 = std::array<__fp16, 2>;

static uint32_t Run(auto lhs, auto rhs, std::string code) {
    return EvalUtil::Run(".dksh compute\n"
                         "main:\n"
                         "MOV R0, c[0x0][0x140];\n"
                         "MOV R1, c[0x0][0x144];\n"
                         "MOV R2, c[2][0];"
                         "MOV R3, c[2][4];" +
                             code +
                             "STG.E [R0], R2;\n"
                             "EXIT;\n",
                         std::array{BitCast<uint32_t>(lhs), BitCast<uint32_t>(rhs)});
}

TEST_CASE("HSET2 Simple", "[shader]") {
    REQUIRE(Run(f16x2{1, 2}, f16x2{3, 2}, "HSET2.F.FTZ.AND R2, R2, R3, PT;") == 0x0000'0000);
    REQUIRE(Run(f16x2{1, 2}, f16x2{3, 2}, "HSET2.T.FTZ.AND R2, R2, R3, PT;") == 0xffff'ffff);

    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.LT.FTZ.AND R2, R2, R3, PT;") == 0x0000'ffff);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.EQ.FTZ.AND R2, R2, R3, PT;") == 0xffff'0000);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.LE.FTZ.AND R2, R2, R3, PT;") == 0xffff'ffff);
    REQUIRE(Run(f16x2{1, 3}, f16x2{2, 2}, "HSET2.GT.FTZ.AND R2, R2, R3, PT;") == 0xffff'0000);
    REQUIRE(Run(f16x2{2, 3}, f16x2{2, 2}, "HSET2.GE.FTZ.AND R2, R2, R3, PT;") == 0xffff'ffff);
    REQUIRE(Run(f16x2{2, 3}, f16x2{2, 2}, "HSET2.NE.FTZ.AND R2, R2, R3, PT;") == 0xffff'0000);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.LT.FTZ.AND R2, R2, -R3, PT;") == 0x0000'0000);
    REQUIRE(Run(f16x2{1, 2}, f16x2{-2, -2}, "HSET2.LT.FTZ.AND R2, R2, |R3|, PT;") == 0x0000'ffff);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.LT.FTZ.AND R2, -R2, R3, PT;") == 0xffff'ffff);
    REQUIRE(Run(f16x2{-1, -2}, f16x2{2, 2}, "HSET2.LT.FTZ.AND R2, |R2|, R3, PT;") == 0x0000'ffff);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.LT.FTZ.AND R2, R2.H1_H1, R3, PT;") == 0x0000'0000);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.LT.FTZ.AND R2, R2.H0_H0, R3, PT;") == 0xffff'ffff);
    REQUIRE(Run(3.0f, f16x2{2, 2}, "HSET2.LT.FTZ.AND R2, R2.F32, R3, PT;") == 0x0000'0000);
    REQUIRE(Run(f16x2{1, 2}, f16x2{1, 2}, "HSET2.LE.FTZ.AND R2, R2, R3.H0_H0, PT;") == 0x0000'ffff);
    REQUIRE(Run(f16x2{1, 2}, f16x2{1, 2}, "HSET2.LT.FTZ.AND R2, R2, R3.H1_H1, PT;") == 0x0000'ffff);
    REQUIRE(Run(f16x2{1, 2}, 2.0f, "HSET2.LT.FTZ.AND R2, R2, R3.F32, PT;") == 0x0000'ffff);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.LT.FTZ.OR R2, R2, R3, PT;") == 0xffff'ffff);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.LT.FTZ.XOR R2, R2, R3, PT;") == 0xffff'0000);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.BF.LT.FTZ.AND R2, R2, R3, PT;") == 0x0000'3c00);

    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.LTU.FTZ.AND R2, R2, R3, PT;") == 0x0000'ffff);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.EQU.FTZ.AND R2, R2, R3, PT;") == 0xffff'0000);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.LEU.FTZ.AND R2, R2, R3, PT;") == 0xffff'ffff);
    REQUIRE(Run(f16x2{1, 3}, f16x2{2, 2}, "HSET2.GTU.FTZ.AND R2, R2, R3, PT;") == 0xffff'0000);
    REQUIRE(Run(f16x2{2, 3}, f16x2{2, 2}, "HSET2.GEU.FTZ.AND R2, R2, R3, PT;") == 0xffff'ffff);
    REQUIRE(Run(f16x2{2, 3}, f16x2{2, 2}, "HSET2.NEU.FTZ.AND R2, R2, R3, PT;") == 0xffff'0000);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.LTU.FTZ.AND R2, R2, -R3, PT;") == 0x0000'0000);
    REQUIRE(Run(f16x2{1, 2}, f16x2{-2, -2}, "HSET2.LTU.FTZ.AND R2, R2, |R3|, PT;") == 0x0000'ffff);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.LTU.FTZ.AND R2, -R2, R3, PT;") == 0xffff'ffff);
    REQUIRE(Run(f16x2{-1, -2}, f16x2{2, 2}, "HSET2.LTU.FTZ.AND R2, |R2|, R3, PT;") == 0x0000'ffff);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.LTU.FTZ.AND R2, R2.H1_H1, R3, PT;") == 0x0000'0000);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.LTU.FTZ.AND R2, R2.H0_H0, R3, PT;") == 0xffff'ffff);
    REQUIRE(Run(3.0f, f16x2{2, 2}, "HSET2.LTU.FTZ.AND R2, R2.F32, R3, PT;") == 0x0000'0000);
    REQUIRE(Run(f16x2{1, 2}, f16x2{1, 2}, "HSET2.LEU.FTZ.AND R2, R2, R3.H0_H0, PT;") == 0x0000'ffff);
    REQUIRE(Run(f16x2{1, 2}, f16x2{1, 2}, "HSET2.LTU.FTZ.AND R2, R2, R3.H1_H1, PT;") == 0x0000'ffff);
    REQUIRE(Run(f16x2{1, 2}, 2.0f, "HSET2.LTU.FTZ.AND R2, R2, R3.F32, PT;") == 0x0000'ffff);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.LTU.FTZ.OR R2, R2, R3, PT;") == 0xffff'ffff);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.LTU.FTZ.XOR R2, R2, R3, PT;") == 0xffff'0000);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.BF.LTU.FTZ.AND R2, R2, R3, PT;") == 0x0000'3c00);

    REQUIRE(Run(f16x2{1, 2}, f16x2{3, 2}, "HSET2.F.FTZ.AND R2, R2, 2, 3, PT;") == 0x0000'0000);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.LT.FTZ.AND R2, R2, 2, 2, PT;") == 0x0000'ffff);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.EQ.FTZ.AND R2, R2, 2, 2, PT;") == 0xffff'0000);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.LE.FTZ.AND R2, R2, 2, 2, PT;") == 0xffff'ffff);
    REQUIRE(Run(f16x2{1, 3}, f16x2{2, 2}, "HSET2.GT.FTZ.AND R2, R2, 2, 2, PT;") == 0xffff'0000);
    REQUIRE(Run(f16x2{2, 3}, f16x2{2, 2}, "HSET2.GE.FTZ.AND R2, R2, 2, 2, PT;") == 0xffff'ffff);
    REQUIRE(Run(f16x2{2, 3}, f16x2{2, 2}, "HSET2.NE.FTZ.AND R2, R2, 2, 2, PT;") == 0xffff'0000);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.LT.FTZ.AND R2, R2, -2, -2, PT;") == 0x0000'0000);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.LT.FTZ.AND R2, -R2, 2, 2, PT;") == 0xffff'ffff);
    REQUIRE(Run(f16x2{-1, -2}, f16x2{2, 2}, "HSET2.LT.FTZ.AND R2, |R2|, 2, 2, PT;") == 0x0000'ffff);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.LT.FTZ.AND R2, R2.H1_H1, 2, 2, PT;") == 0x0000'0000);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.LT.FTZ.AND R2, R2.H0_H0, 2, 2, PT;") == 0xffff'ffff);
    REQUIRE(Run(3.0f, f16x2{2, 2}, "HSET2.LT.FTZ.AND R2, R2.F32, 2, 2, PT;") == 0x0000'0000);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.LT.FTZ.OR R2, R2, 2, 2, PT;") == 0xffff'ffff);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.LT.FTZ.XOR R2, R2, 2, 2, PT;") == 0xffff'0000);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.BF.LT.FTZ.AND R2, R2, 2, 2, PT;") == 0x0000'3c00);

    // Some of these tests read an f16x2 as f32
    REQUIRE(Run(f16x2{1, 2}, f16x2{3, 2}, "HSET2.F.FTZ.AND R2, R2, c[2][4], PT;") == 0x0000'0000);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.LT.FTZ.AND R2, R2, c[2][4], PT;") == 0xffff'ffff);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.EQ.FTZ.AND R2, R2, c[2][4], PT;") == 0x0000'0000);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.LE.FTZ.AND R2, R2, c[2][4], PT;") == 0xffff'ffff);
    REQUIRE(Run(f16x2{1, 3}, f16x2{2, 2}, "HSET2.GT.FTZ.AND R2, R2, c[2][4], PT;") == 0xffff'0000);
    REQUIRE(Run(f16x2{2, 3}, f16x2{2, 2}, "HSET2.GE.FTZ.AND R2, R2, c[2][4], PT;") == 0xffff'0000);
    REQUIRE(Run(f16x2{2, 3}, f16x2{2, 2}, "HSET2.NE.FTZ.AND R2, R2, c[2][4], PT;") == 0xffff'ffff);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.LT.FTZ.AND R2, R2, -c[2][4], PT;") == 0x0000'0000);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.LT.FTZ.AND R2, -R2, c[2][4], PT;") == 0xffff'ffff);
    REQUIRE(Run(f16x2{-1, -2}, f16x2{2, 2}, "HSET2.LT.FTZ.AND R2, |R2|, c[2][4], PT;") == 0xffff'ffff);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.LT.FTZ.AND R2, R2.H1_H1, c[2][4], PT;") == 0xffff'ffff);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.LT.FTZ.AND R2, R2.H0_H0, c[2][4], PT;") == 0xffff'ffff);
    REQUIRE(Run(3.0f, f16x2{2, 2}, "HSET2.LT.FTZ.AND R2, R2.F32, c[2][4], PT;") == 0x0000'0000);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.LT.FTZ.OR R2, R2, c[2][4], PT;") == 0xffff'ffff);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.LT.FTZ.XOR R2, R2, c[2][4], PT;") == 0x0000'0000);
    REQUIRE(Run(f16x2{1, 2}, f16x2{2, 2}, "HSET2.BF.LT.FTZ.AND R2, R2, c[2][4], PT;") == 0x3c00'3c00);
}

TEST_CASE("HSET2 NAN", "[shader]") {
    const __fp16 nan{BitCast<__fp16, uint16_t>(0x7fff)};

    REQUIRE(Run(f16x2{1, nan}, f16x2{nan, nan}, "HSET2.LT.FTZ.AND R2, R2, R3, PT;") == 0x0000'0000);
    REQUIRE(Run(f16x2{1, nan}, f16x2{nan, nan}, "HSET2.EQ.FTZ.AND R2, R2, R3, PT;") == 0x0000'0000);
    REQUIRE(Run(f16x2{1, nan}, f16x2{nan, nan}, "HSET2.LE.FTZ.AND R2, R2, R3, PT;") == 0x0000'0000);
    REQUIRE(Run(f16x2{1, nan}, f16x2{nan, nan}, "HSET2.GT.FTZ.AND R2, R2, R3, PT;") == 0x0000'0000);
    REQUIRE(Run(f16x2{1, nan}, f16x2{nan, nan}, "HSET2.NE.FTZ.AND R2, R2, R3, PT;") == 0x0000'0000);
    REQUIRE(Run(f16x2{1, nan}, f16x2{nan, nan}, "HSET2.GE.FTZ.AND R2, R2, R3, PT;") == 0x0000'0000);
    REQUIRE(Run(f16x2{1, nan}, f16x2{nan, nan}, "HSET2.NUM.FTZ.AND R2, R2, R3, PT;") == 0x0000'0000);

    REQUIRE(Run(f16x2{1, nan}, f16x2{nan, nan}, "HSET2.NAN.FTZ.AND R2, R2, R3, PT;") == 0xffff'ffff);
    REQUIRE(Run(f16x2{1, nan}, f16x2{nan, nan}, "HSET2.LTU.FTZ.AND R2, R2, R3, PT;") == 0xffff'ffff);
    REQUIRE(Run(f16x2{1, nan}, f16x2{nan, nan}, "HSET2.EQU.FTZ.AND R2, R2, R3, PT;") == 0xffff'ffff);
    REQUIRE(Run(f16x2{1, nan}, f16x2{nan, nan}, "HSET2.LEU.FTZ.AND R2, R2, R3, PT;") == 0xffff'ffff);
    REQUIRE(Run(f16x2{1, nan}, f16x2{nan, nan}, "HSET2.GTU.FTZ.AND R2, R2, R3, PT;") == 0xffff'ffff);
    REQUIRE(Run(f16x2{1, nan}, f16x2{nan, nan}, "HSET2.NEU.FTZ.AND R2, R2, R3, PT;") == 0xffff'ffff);
    REQUIRE(Run(f16x2{1, nan}, f16x2{nan, nan}, "HSET2.GEU.FTZ.AND R2, R2, R3, PT;") == 0xffff'ffff);
}
