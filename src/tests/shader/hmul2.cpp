#include <catch2/catch_test_macros.hpp>

#include "fp16_run.h"

using namespace Fp16Run;

TEST_CASE("HMUL2 Simple", "[shader]") {
    REQUIRE(RunF16x2({1, 3}, {5, 7}, "HMUL2.FTZ R2, R3.H1_H0, R4.H1_H0;") == f16x2{5, 21});
    REQUIRE(RunF16x2({1, 3}, {5, 7}, "HMUL2.FTZ R2, R3.H0_H0, R4.H1_H0;") == f16x2{5, 7});
    REQUIRE(RunF16x2({1, 3}, {5, 7}, "HMUL2.FTZ R2, R3.H1_H1, R4.H1_H0;") == f16x2{15, 21});
    REQUIRE(RunF16x2(4, {5, 7}, "HMUL2.FTZ R2, R3.F32, R4.H1_H0;") == f16x2{20, 28});

    REQUIRE(RunF16x2({1, 3}, {5, 7}, "HMUL2.FTZ R2, R3.H1_H0, R4.H0_H0;") == f16x2{5, 15});
    REQUIRE(RunF16x2({1, 3}, {5, 7}, "HMUL2.FTZ R2, R3.H1_H0, R4.H1_H1;") == f16x2{7, 21});
    REQUIRE(RunF16x2({1, 3}, 8, "HMUL2.FTZ R2, R3.H1_H0, R4.F32;") == f16x2{8, 24});

    REQUIRE(RunF16x2({1, 3}, {5, 7}, "HMUL2.FTZ R2, R3.H1_H0, -R4.H1_H0;") == f16x2{-5, -21});
    REQUIRE(RunF16x2({1, 3}, {5, 7}, "HMUL2.FTZ R2, R3.H1_H0, -R4.H0_H0;") == f16x2{-5, -15});
    REQUIRE(RunF16x2({1, 3}, {5, 7}, "HMUL2.FTZ R2, R3.H1_H0, -R4.H1_H1;") == f16x2{-7, -21});
    REQUIRE(RunF16x2({1, 3}, 8, "HMUL2.FTZ R2, R3.H1_H0, -R4.F32;") == f16x2{-8, -24});

    REQUIRE(RunF16x2({-1, -3}, {5, 7}, "HMUL2.FTZ R2, |R3|.H1_H0, R4.H1_H0;") == f16x2{5, 21});
    REQUIRE(RunF16x2({-1, -3}, {5, 7}, "HMUL2.FTZ R2, |R3|.H0_H0, R4.H1_H0;") == f16x2{5, 7});
    REQUIRE(RunF16x2({-1, -3}, {5, 7}, "HMUL2.FTZ R2, |R3|.H1_H1, R4.H1_H0;") == f16x2{15, 21});
    REQUIRE(RunF16x2(-4, {5, 7}, "HMUL2.FTZ R2, |R3|.F32, R4.H1_H0;") == f16x2{20, 28});

    REQUIRE(RunF16x2({1, 3}, {-5, -7}, "HMUL2.FTZ R2, R3.H1_H0, |R4|.H1_H0;") == f16x2{5, 21});
    REQUIRE(RunF16x2({1, 3}, {-5, -7}, "HMUL2.FTZ R2, R3.H1_H0, |R4|.H0_H0;") == f16x2{5, 15});
    REQUIRE(RunF16x2({1, 3}, {-5, -7}, "HMUL2.FTZ R2, R3.H1_H0, |R4|.H1_H1;") == f16x2{7, 21});
    REQUIRE(RunF16x2({1, 3}, -8, "HMUL2.FTZ R2, R3.H1_H0, |R4|.F32;") == f16x2{8, 24});

    REQUIRE(RunF16x2({1, 3}, 5, "HMUL2.FTZ R2, R3.H1_H0, -c[2][8];") == f16x2{-5, -15});

    REQUIRE(RunF16x2({1, 3}, 0, "HMUL2.FTZ R2, R3.H1_H0, -4, 2;") == f16x2{2, -12});
    REQUIRE(RunF16x2({1, 3}, 0, "HMUL2.FTZ R2, -R3.H1_H0, -4, 2;") == f16x2{-2, 12});

    REQUIRE(RunF32({1, 3}, 0, "HMUL2.F32.FTZ R2, R3.H1_H0, -4, 2;") == 2);

    REQUIRE(RunF16x2({1, 3}, 0, "HMUL2.MRG_H0.FTZ R2, R3.H1_H0, -4, 2;", {12, 12}) == f16x2{2, 12});
    REQUIRE(RunF16x2({1, 3}, 0, "HMUL2.MRG_H1.FTZ R2, R3.H1_H0, -4, 2;", {12, 12}) ==
            f16x2{12, -12});

    REQUIRE(RunF16x2({1, 3}, 0, "HMUL2_32I R2, R3.H1_H0, -4, 2;") == f16x2{2, -12});
    REQUIRE(RunF16x2({1, 3}, 0, "HMUL2_32I R2, R3.H0_H0, -4, 2;") == f16x2{2, -4});
    REQUIRE(RunF16x2({3, 1}, 0, "HMUL2_32I R2, R3.H1_H1, -4, 2;") == f16x2{2, -4});
    REQUIRE(RunF16x2(1, 0, "HMUL2_32I R2, R3.F32, -4, 2;") == f16x2{2, -4});
    REQUIRE(RunF16x2(1, 0, "HMUL2_32I R2, R3.F32, 4, -2;") == f16x2{-2, 4});
}

TEST_CASE("HMUL2 Saturate", "[shader]") {
    REQUIRE(RunF16x2({1, 3}, 0, "HMUL2.MRG_H0.FTZ.SAT R2, R3.H1_H0, -4, 2;", {12, 12}) ==
            f16x2{1, 12});
    REQUIRE(RunF16x2({1, 3}, {5, 7}, "HMUL2.FTZ.SAT R2, R3.H1_H0, -R4.H1_H0;") == f16x2{0, 0});
    REQUIRE(RunF32({1, 3}, 0, "HMUL2.F32.FTZ.SAT R2, R3.H1_H0, -4, 2;") == 1);
    REQUIRE(RunF32({1, 3}, 0, "HMUL2.F32.FTZ.SAT R2, R3.H1_H0, -4, -2;") == 0);
}

TEST_CASE("HMUL2 NAN", "[shader]") {
    const f16 nan = BitCast<f16>(uint16_t(0x7fff));
    const f16 inf = BitCast<f16>(uint16_t(0x7c00));

    REQUIRE(RunF16x2({inf, inf}, {inf, inf}, "HMUL2.FTZ R2, R3, R4;") == f16x2{inf, inf});
    REQUIRE(BitCast<uint16_t>(RunF16x2({inf, inf}, {inf, -inf}, "HMUL2.FTZ R2, R3, R4;")[1]) ==
            0xfc00);

    REQUIRE(BitCast<uint16_t>(RunF16x2({nan, 0}, {nan, 0}, "HMUL2.FTZ R2, R3, R4;")[0]) == 0x7fff);
    REQUIRE(BitCast<uint16_t>(RunF16x2({-nan, 0}, {-nan, 0}, "HMUL2.FTZ R2, R3, R4;")[0]) ==
            0x7fff);
    REQUIRE(BitCast<uint16_t>(RunF16x2({nan, 0}, {-nan, 0}, "HMUL2.FTZ R2, R3, R4;")[0]) == 0x7fff);

    REQUIRE(BitCast<uint16_t>(RunF16x2({nan, 0}, {nan, 0}, "HMUL2.FTZ.SAT R2, R3, R4;")[0]) == 0);
    REQUIRE(RunF16x2({inf, 0}, {inf, 0}, "HMUL2.FTZ.SAT R2, R3, R4;")[0] == 1);
}

// TODO: Test denorms
// TODO: Test FMZ
