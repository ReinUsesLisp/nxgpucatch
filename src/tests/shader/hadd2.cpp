#include <catch2/catch_test_macros.hpp>

#include "fp16_run.h"

using namespace Fp16Run;

TEST_CASE("HADD2 Simple", "[shader]") {
    REQUIRE(RunF16x2({1, 3}, {5, 7}, "HADD2.FTZ R2, R3.H1_H0, R4.H1_H0;") == f16x2{6, 10});
    REQUIRE(RunF16x2({1, 3}, {5, 7}, "HADD2.FTZ R2, R3.H0_H0, R4.H1_H0;") == f16x2{6, 8});
    REQUIRE(RunF16x2({1, 3}, {5, 7}, "HADD2.FTZ R2, R3.H1_H1, R4.H1_H0;") == f16x2{8, 10});
    REQUIRE(RunF16x2(4, {5, 7}, "HADD2.FTZ R2, R3.F32, R4.H1_H0;") == f16x2{9, 11});

    REQUIRE(RunF16x2({1, 3}, {5, 7}, "HADD2.FTZ R2, R3.H1_H0, R4.H0_H0;") == f16x2{6, 8});
    REQUIRE(RunF16x2({1, 3}, {5, 7}, "HADD2.FTZ R2, R3.H1_H0, R4.H1_H1;") == f16x2{8, 10});
    REQUIRE(RunF16x2({1, 3}, 8, "HADD2.FTZ R2, R3.H1_H0, R4.F32;") == f16x2{9, 11});

    REQUIRE(RunF16x2({1, 3}, {5, 7}, "HADD2.FTZ R2, -R3.H1_H0, R4.H1_H0;") == f16x2{4, 4});
    REQUIRE(RunF16x2({1, 3}, {5, 7}, "HADD2.FTZ R2, -R3.H0_H0, R4.H1_H0;") == f16x2{4, 6});
    REQUIRE(RunF16x2({1, 3}, {5, 7}, "HADD2.FTZ R2, -R3.H1_H1, R4.H1_H0;") == f16x2{2, 4});
    REQUIRE(RunF16x2(4, {5, 7}, "HADD2.FTZ R2, -R3.F32, R4.H1_H0;") == f16x2{1, 3});

    REQUIRE(RunF16x2({1, 3}, {5, 7}, "HADD2.FTZ R2, R3.H1_H0, -R4.H1_H0;") == f16x2{-4, -4});
    REQUIRE(RunF16x2({1, 3}, {5, 7}, "HADD2.FTZ R2, R3.H1_H0, -R4.H0_H0;") == f16x2{-4, -2});
    REQUIRE(RunF16x2({1, 3}, {5, 7}, "HADD2.FTZ R2, R3.H1_H0, -R4.H1_H1;") == f16x2{-6, -4});
    REQUIRE(RunF16x2({1, 3}, 8, "HADD2.FTZ R2, R3.H1_H0, -R4.F32;") == f16x2{-7, -5});

    REQUIRE(RunF16x2({-1, -3}, {5, 7}, "HADD2.FTZ R2, |R3|.H1_H0, R4.H1_H0;") == f16x2{6, 10});
    REQUIRE(RunF16x2({-1, -3}, {5, 7}, "HADD2.FTZ R2, |R3|.H0_H0, R4.H1_H0;") == f16x2{6, 8});
    REQUIRE(RunF16x2({-1, -3}, {5, 7}, "HADD2.FTZ R2, |R3|.H1_H1, R4.H1_H0;") == f16x2{8, 10});
    REQUIRE(RunF16x2(-4, {5, 7}, "HADD2.FTZ R2, |R3|.F32, R4.H1_H0;") == f16x2{9, 11});

    REQUIRE(RunF16x2({1, 3}, {-5, -7}, "HADD2.FTZ R2, R3.H1_H0, |R4|.H1_H0;") == f16x2{6, 10});
    REQUIRE(RunF16x2({1, 3}, {-5, -7}, "HADD2.FTZ R2, R3.H1_H0, |R4|.H0_H0;") == f16x2{6, 8});
    REQUIRE(RunF16x2({1, 3}, {-5, -7}, "HADD2.FTZ R2, R3.H1_H0, |R4|.H1_H1;") == f16x2{8, 10});
    REQUIRE(RunF16x2({1, 3}, -8, "HADD2.FTZ R2, R3.H1_H0, |R4|.F32;") == f16x2{9, 11});

    REQUIRE(RunF16x2({1, 3}, 5, "HADD2.FTZ R2, R3.H1_H0, -c[2][8];") == f16x2{-4, -2});

    REQUIRE(RunF16x2({1, 3}, 0, "HADD2.FTZ R2, R3.H1_H0, -4, 2;") == f16x2{3, -1});
    REQUIRE(RunF16x2({1, 3}, 0, "HADD2.FTZ R2, -R3.H1_H0, -4, 2;") == f16x2{1, -7});

    REQUIRE(RunF32({1, 3}, 0, "HADD2.F32.FTZ R2, R3.H1_H0, -4, 2;") == 3);

    REQUIRE(RunF16x2({1, 3}, 0, "HADD2.MRG_H0.FTZ R2, R3.H1_H0, -4, 2;", {12, 12}) == f16x2{3, 12});
    REQUIRE(RunF16x2({1, 3}, 0, "HADD2.MRG_H1.FTZ R2, R3.H1_H0, -4, 2;", {12, 12}) ==
            f16x2{12, -1});

    REQUIRE(RunF16x2({1, 3}, 0, "HADD2_32I R2, R3.H1_H0, -4, 2;") == f16x2{3, -1});
    REQUIRE(RunF16x2({1, 3}, 0, "HADD2_32I R2, R3.H0_H0, -4, 2;") == f16x2{3, -3});
    REQUIRE(RunF16x2({3, 1}, 0, "HADD2_32I R2, R3.H1_H1, -4, 2;") == f16x2{3, -3});
    REQUIRE(RunF16x2(1, 0, "HADD2_32I R2, R3.F32, -4, 2;") == f16x2{3, -3});
    REQUIRE(RunF16x2(1, 0, "HADD2_32I R2, -R3.F32, -4, 2;") == f16x2{1, -5});
}

TEST_CASE("HADD2 Saturate", "[shader]") {
    REQUIRE(RunF16x2({1, 3}, 0, "HADD2.MRG_H0.FTZ.SAT R2, R3.H1_H0, -4, 2;", {12, 12}) ==
            f16x2{1, 12});
    REQUIRE(RunF16x2({1, 3}, {5, 7}, "HADD2.FTZ.SAT R2, R3.H1_H0, -R4.H1_H0;") == f16x2{0, 0});
    REQUIRE(RunF32({1, 3}, 0, "HADD2.F32.FTZ.SAT R2, R3.H1_H0, -4, 2;") == 1);
    REQUIRE(RunF32({1, 3}, 0, "HADD2.F32.FTZ.SAT R2, R3.H1_H0, -4, -2;") == 0);
}

TEST_CASE("HADD2 NAN", "[shader]") {
    const f16 nan = BitCast<f16>(uint16_t(0x7fff));
    const f16 inf = BitCast<f16>(uint16_t(0x7c00));

    REQUIRE(RunF16x2({inf, inf}, {inf, inf}, "HADD2.FTZ R2, R3, R4;") == f16x2{inf, inf});
    REQUIRE(BitCast<uint16_t>(RunF16x2({inf, inf}, {inf, -inf}, "HADD2.FTZ R2, R3, R4;")[1]) ==
            0x7fff);

    REQUIRE(BitCast<uint16_t>(RunF16x2({nan, 0}, {nan, 0}, "HADD2.FTZ R2, R3, R4;")[0]) == 0x7fff);
    REQUIRE(BitCast<uint16_t>(RunF16x2({-nan, 0}, {-nan, 0}, "HADD2.FTZ R2, R3, R4;")[0]) ==
            0x7fff);
    REQUIRE(BitCast<uint16_t>(RunF16x2({nan, 0}, {-nan, 0}, "HADD2.FTZ R2, R3, R4;")[0]) == 0x7fff);

    REQUIRE(BitCast<uint16_t>(RunF16x2({nan, 0}, {nan, 0}, "HADD2.FTZ.SAT R2, R3, R4;")[0]) == 0);
    REQUIRE(RunF16x2({inf, 0}, {inf, 0}, "HADD2.FTZ.SAT R2, R3, R4;")[0] == 1);
}

// TODO: Test denorms
