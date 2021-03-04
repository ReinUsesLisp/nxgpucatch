#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

using f16x2 = std::array<__fp16, 2>;

static uint32_t Run(auto lhs, auto rhs, std::string code) {
    return EvalUtil::Run(".dksh compute\n"
                         "main:\n"
                         "MOV R0, c[0x0][0x140];\n"
                         "MOV R1, c[0x0][0x144];\n"
                         "ISETP.F.AND P0, P1, RZ, RZ, !PT;\n"
                         "MOV R2, c[2][0];\n"
                         "MOV R3, c[2][4];\n" +
                             code +
                             "MOV R2, RZ;\n"
                             "@P0 IADD32I R2, R2, 0x0000ffff;\n"
                             "@P1 IADD32I R2, R2, -0x00010000;\n"
                             "STG.E [R0], R2;\n"
                             "EXIT;\n",
                         std::array{BitCast<uint32_t>(lhs), BitCast<uint32_t>(rhs)});
}

TEST_CASE("HSETP2 Simple", "[shader]") {
    REQUIRE(Run(f16x2{3, 2}, f16x2{3, 3}, "HSETP2.LT.FTZ.AND P0, P1, R2, R3, PT;") == 0xffff'0000);
    REQUIRE(Run(f16x2{3, 2}, f16x2{3, 3}, "HSETP2.GE.FTZ.AND P0, P1, R2, R3, PT;") == 0x0000'ffff);
    REQUIRE(Run(f16x2{3, 2}, f16x2{3, 3}, "HSETP2.GE.FTZ.XOR P0, P1, R2, R3, PT;") == 0xffff'0000);
    REQUIRE(Run(f16x2{3, 2}, f16x2{3, 3}, "HSETP2.GE.FTZ.OR P0, P1, R2, R3, PT;") == 0xffff'ffff);
    REQUIRE(Run(f16x2{2, 3}, f16x2{3, 4}, "HSETP2.LT.H_AND.FTZ.AND P0, P1, R2, R3, PT;") ==
            0x0000'ffff);
    REQUIRE(Run(f16x2{6, 3}, f16x2{3, 4}, "HSETP2.LT.H_AND.FTZ.AND P0, P1, R2, R3, PT;") ==
            0xffff'0000);
    REQUIRE(Run(f16x2{4, 5}, f16x2{3, 4}, "HSETP2.LT.H_AND.FTZ.AND P0, P1, R2, R3, PT;") ==
            0xffff'0000);

    REQUIRE(Run(f16x2{3, 2}, f16x2{3, 3}, "HSETP2.LT.FTZ.AND P0, P1, -R2, R3, PT;") == 0xffff'ffff);
    REQUIRE(Run(f16x2{-3, -2}, f16x2{3, 3}, "HSETP2.LT.FTZ.AND P0, P1, -|R2|, R3, PT;") == 0xffff'ffff);
    REQUIRE(Run(f16x2{3, 2}, f16x2{-3, -3}, "HSETP2.LT.FTZ.AND P0, P1, R2, -R3, PT;") == 0xffff'0000);
    REQUIRE(Run(f16x2{3, 2}, f16x2{-3, -3}, "HSETP2.LT.FTZ.AND P0, P1, R2, -|R3|, PT;") == 0x0000'0000);

    REQUIRE(Run(f16x2{3, 2}, f16x2{3, 4}, "HSETP2.LT.FTZ.AND P0, P1, R2.H1_H1, R3, PT;") == 0xffff'ffff);
    REQUIRE(Run(f16x2{3, 2}, f16x2{3, 4}, "HSETP2.LT.FTZ.AND P0, P1, R2.H0_H0, R3, PT;") == 0xffff'0000);
    REQUIRE(Run(f16x2{3, 2}, f16x2{7, 3}, "HSETP2.LT.FTZ.AND P0, P1, R2, R3.H1_H1, PT;") == 0xffff'0000);

    REQUIRE(Run(f16x2{3, 3}, 0, "HSETP2.GT.FTZ.AND P0, P1, R2, 2, 3, PT;") == 0xffff'0000);
    
    REQUIRE(Run(f16x2{3, 2}, 3.0f, "HSETP2.LT.FTZ.AND P0, P1, R2, c[2][4], PT;") == 0xffff'0000);
}
