#include <catch2/catch_test_macros.hpp>

#include "fp16_run.h"

using namespace Fp16Run;

TEST_CASE("HFMA2 Simple", "[shader]") {
    REQUIRE(RunF16x2({1, 2}, {2, 1}, {1, 2}, "HFMA2.FTZ R2, R3, R4, R5;") == f16x2{3, 4});

    REQUIRE(RunF16x2({1, 2}, {2, 1}, {1, 1}, "HFMA2.FTZ R2, R3, R4, R5;") == f16x2{3, 3});
    REQUIRE(RunF16x2({1, 2}, {2, 1}, {1, 1}, "HFMA2.FTZ R2, R3, -R4, R5;") == f16x2{-1, -1});
    REQUIRE(RunF16x2({1, 2}, {2, 1}, {1, 1}, "HFMA2.FTZ R2, R3, R4, -R5;") == f16x2{1, 1});

    REQUIRE(RunF16x2({1, 2}, {2, 1}, {1, 1}, "HFMA2.FTZ R2, R3.H0_H0, R4, R5;") == f16x2{3, 2});
    REQUIRE(RunF16x2({1, 2}, {2, 1}, {1, 1}, "HFMA2.FTZ R2, R3.H1_H1, R4, R5;") == f16x2{5, 3});
    REQUIRE(RunF16x2(1, {2, 1}, {1, 1}, "HFMA2.FTZ R2, R3.F32, R4, R5;") == f16x2{3, 2});

    REQUIRE(RunF16x2({1, 2}, {2, 1}, {1, 1}, "HFMA2.FTZ R2, R3, R4.H0_H0, R5;") == f16x2{3, 5});
    REQUIRE(RunF16x2({1, 2}, {2, 1}, {1, 1}, "HFMA2.FTZ R2, R3, R4.H1_H1, R5;") == f16x2{2, 3});
    REQUIRE(RunF16x2({1, 2}, 1, {1, 1}, "HFMA2.FTZ R2, R3, R4.F32, R5;") == f16x2{2, 3});

    REQUIRE(RunF16x2({1, 2}, {2, 1}, {1, 2}, "HFMA2.FTZ R2, R3, R4, R5.H0_H0;") == f16x2{3, 3});
    REQUIRE(RunF16x2({1, 2}, {2, 1}, {2, 1}, "HFMA2.FTZ R2, R3, R4, R5.H1_H1;") == f16x2{3, 3});
    REQUIRE(RunF16x2({1, 2}, {2, 1}, 1, "HFMA2.FTZ R2, R3, R4, R5.F32;") == f16x2{3, 3});

    REQUIRE(RunF16x2({1, 2}, {2, 1}, {1, 1}, "HFMA2.MRG_H0.FTZ R2, R3, R4, R5;", {0, 0}) ==
            f16x2{3, 0});
    REQUIRE(RunF16x2({1, 2}, {2, 1}, {1, 1}, "HFMA2.MRG_H1.FTZ R2, R3, R4, R5;", {0, 0}) ==
            f16x2{0, 3});
    REQUIRE(RunF32({1, 2}, {2, 1}, {1, 1}, "HFMA2.F32.FTZ R2, R3, R4, R5;", {0, 0}) == 3);
    REQUIRE(RunF32({1, 2}, {2, 1}, {1, 1}, "HFMA2.F32.FTZ.SAT R2, R3, R4, R5;", {0, 0}) == 1);

    REQUIRE(RunF16x2({1, 1}, 1, {1, 1}, "HFMA2.FTZ R2, R3, c[2][8], R5;") == f16x2{2, 2});
    REQUIRE(RunF16x2({1, 1}, 1, {1, 1}, "HFMA2.FTZ R2, R3, -c[2][8], R5;") == f16x2{0, 0});
    REQUIRE(RunF16x2({1, 2}, 1, {1, 1}, "HFMA2.FTZ R2, R3, c[2][8], -R5;") == f16x2{0, 1});

    REQUIRE(RunF16x2({1, 2}, 1, {1, 1}, "HFMA2.FTZ R2, R3.H0_H0, c[2][8], R5;") == f16x2{2, 2});
    REQUIRE(RunF16x2({1, 2}, 1, {1, 1}, "HFMA2.FTZ R2, R3.H1_H1, c[2][8], R5;") == f16x2{3, 3});
    REQUIRE(RunF16x2(1, 1, {1, 1}, "HFMA2.FTZ R2, R3.F32, c[2][8], R5;") == f16x2{2, 2});

    REQUIRE(RunF16x2({1, 2}, 1, {1, 1}, "HFMA2.FTZ R2, R3, c[2][8], R5;") == f16x2{2, 3});

    REQUIRE(RunF16x2({1, 2}, 1, {1, 2}, "HFMA2.FTZ R2, R3, c[2][8], R5.H0_H0;") == f16x2{2, 3});
    REQUIRE(RunF16x2({1, 2}, 1, {2, 1}, "HFMA2.FTZ R2, R3, c[2][8], R5.H1_H1;") == f16x2{2, 3});
    REQUIRE(RunF16x2({1, 2}, 1, 1, "HFMA2.FTZ R2, R3, c[2][8], R5.F32;") == f16x2{2, 3});

    REQUIRE(RunF16x2({1, 2}, 1, {1, 1}, "HFMA2.MRG_H0.FTZ R2, R3, c[2][8], R5;", {0, 0}) ==
            f16x2{2, 0});
    REQUIRE(RunF16x2({1, 2}, 1, {1, 1}, "HFMA2.MRG_H1.FTZ R2, R3, c[2][8], R5;", {0, 0}) ==
            f16x2{0, 3});
    REQUIRE(RunF32({1, 2}, 1, {1, 1}, "HFMA2.F32.FTZ R2, R3, c[2][8], R5;", {0, 0}) == 2);
    REQUIRE(RunF32({1, 2}, 1, {1, 1}, "HFMA2.F32.FTZ.SAT R2, R3, c[2][8], R5;", {0, 0}) == 1);

    REQUIRE(RunF16x2({1, 2}, {2, 1}, 1, "HFMA2.FTZ R2, R3, R4, c[2][12];") == f16x2{3, 3});
    REQUIRE(RunF16x2({1, 2}, {2, 1}, 1, "HFMA2.FTZ R2, R3, -R4, c[2][12];") == f16x2{-1, -1});
    REQUIRE(RunF16x2({1, 2}, {2, 1}, 1, "HFMA2.FTZ R2, R3, R4, -c[2][12];") == f16x2{1, 1});

    REQUIRE(RunF16x2({1, 2}, {2, 1}, 1, "HFMA2.FTZ R2, R3.H0_H0, R4, c[2][12];") == f16x2{3, 2});
    REQUIRE(RunF16x2({1, 2}, {2, 1}, 1, "HFMA2.FTZ R2, R3.H1_H1, R4, c[2][12];") == f16x2{5, 3});
    REQUIRE(RunF16x2(1, {2, 1}, 1, "HFMA2.FTZ R2, R3.F32, R4, c[2][12];") == f16x2{3, 2});

    REQUIRE(RunF16x2({1, 2}, {2, 1}, 1, "HFMA2.FTZ R2, R3, R4.H0_H0, c[2][12];") == f16x2{3, 5});
    REQUIRE(RunF16x2({1, 2}, {2, 1}, 1, "HFMA2.FTZ R2, R3, R4.H1_H1, c[2][12];") == f16x2{2, 3});
    REQUIRE(RunF16x2({1, 2}, 1, 1, "HFMA2.FTZ R2, R3, R4.F32, c[2][12];") == f16x2{2, 3});

    REQUIRE(RunF16x2({1, 2}, {2, 1}, 1, "HFMA2.MRG_H0.FTZ R2, R3, R4, c[2][12];", {0, 0}) ==
            f16x2{3, 0});
    REQUIRE(RunF16x2({1, 2}, {2, 1}, 1, "HFMA2.MRG_H1.FTZ R2, R3, R4, c[2][12];", {0, 0}) ==
            f16x2{0, 3});
    REQUIRE(RunF32({1, 2}, {2, 1}, 1, "HFMA2.F32.FTZ R2, R3, R4, c[2][12];", {0, 0}) == 3);
    REQUIRE(RunF32({1, 2}, {2, 1}, 1, "HFMA2.F32.FTZ.SAT R2, R3, R4, c[2][12];", {0, 0}) == 1);

    REQUIRE(RunF16x2({1, 2}, {2, 1}, {1, 1}, "HFMA2.FTZ R2, R3, 1, 2, R5;") == f16x2{3, 3});
    REQUIRE(RunF16x2({1, 2}, {2, 1}, {1, 1}, "HFMA2.FTZ R2, R3, -1, -2, R5;") == f16x2{-1, -1});
    REQUIRE(RunF16x2({1, 2}, {2, 1}, {1, 1}, "HFMA2.FTZ R2, R3, 1, 2, -R5;") == f16x2{1, 1});

    REQUIRE(RunF16x2({1, 2}, {2, 1}, {1, 1}, "HFMA2.FTZ R2, R3.H0_H0, 1, 2, R5;") == f16x2{3, 2});
    REQUIRE(RunF16x2({1, 2}, {2, 1}, {1, 1}, "HFMA2.FTZ R2, R3.H1_H1, 1, 2, R5;") == f16x2{5, 3});
    REQUIRE(RunF16x2(1, {2, 1}, {1, 1}, "HFMA2.FTZ R2, R3.F32, 1, 2, R5;") == f16x2{3, 2});

    REQUIRE(RunF16x2({1, 2}, {2, 1}, {1, 2}, "HFMA2.FTZ R2, R3, 1, 2, R5.H0_H0;") == f16x2{3, 3});
    REQUIRE(RunF16x2({1, 2}, {2, 1}, {2, 1}, "HFMA2.FTZ R2, R3, 1, 2, R5.H1_H1;") == f16x2{3, 3});
    REQUIRE(RunF16x2({1, 2}, {2, 1}, 1, "HFMA2.FTZ R2, R3, 1, 2, R5.F32;") == f16x2{3, 3});

    REQUIRE(RunF16x2({1, 2}, {2, 1}, {1, 1}, "HFMA2.MRG_H0.FTZ R2, R3, 1, 2, R5;", {0, 0}) ==
            f16x2{3, 0});
    REQUIRE(RunF16x2({1, 2}, {2, 1}, {1, 1}, "HFMA2.MRG_H1.FTZ R2, R3, 1, 2, R5;", {0, 0}) ==
            f16x2{0, 3});
    REQUIRE(RunF32({1, 2}, {2, 1}, {1, 1}, "HFMA2.F32.FTZ R2, R3, 1, 2, R5;", {0, 0}) == 3);
    REQUIRE(RunF32({1, 2}, {2, 1}, {1, 1}, "HFMA2.F32.FTZ.SAT R2, R3, 1, 2, R5;", {0, 0}) == 1);

    REQUIRE(RunF16x2({1, 2}, 0, 0, "HFMA2_32I.FTZ R2, R3, 1, 2, R2;", {1, 2}) == f16x2{3, 4});
    REQUIRE(RunF16x2({1, 2}, 0, 0, "HFMA2_32I.FTZ R2, R3, 1, 2, -R2;", {1, 2}) == f16x2{1, 0});
    REQUIRE(RunF16x2({1, 2}, 0, 0, "HFMA2_32I.FTZ R2, R3.H0_H0, 1, 2, R2;", {1, 2}) == f16x2{3, 3});
    REQUIRE(RunF16x2({1, 2}, 0, 0, "HFMA2_32I.FTZ R2, R3.H1_H1, 1, 2, R2;", {1, 2}) == f16x2{5, 4});
    REQUIRE(RunF16x2(3, 0, 0, "HFMA2_32I.FTZ R2, R3.F32, 1, 2, R2;", {1, 2}) == f16x2{7, 5});
}

TEST_CASE("HFMA2 Promotions", "[shader]") {
    REQUIRE(RunF16x2(1, {2, 1}, 1, "HFMA2.FTZ R2, R3.F32, R4, R5.F32;") == f16x2{3, 2});
    REQUIRE(RunF16x2(1, 1, {1, 2}, "HFMA2.FTZ R2, R3.F32, R4.F32, R5;") == f16x2{2, 3});
    REQUIRE(RunF16x2(1, 1, 1, "HFMA2.FTZ R2, R3.F32, R4.F32, R5.F32;") == f16x2{2, 2});
    REQUIRE(RunF32(1, 1, 2, "HFMA2.F32.FTZ R2, R3.F32, R4.F32, R5.F32;") == 3);
    REQUIRE(RunF32(1, 1, 2, "HFMA2.F32.FTZ.SAT R2, R3.F32, R4.F32, R5.F32;") == 1);
    REQUIRE(RunF32(1, 1, 2, "HFMA2.F32.FTZ.SAT R2, R3.F32, -R4.F32, -R5.F32;") == 0);
}
