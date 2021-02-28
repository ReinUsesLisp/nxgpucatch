#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

using namespace EvalUtil;

static double RoundEven(double value) {
    return std::round(value * 0.5) * 2.0;
}

static double Saturate(double value) {
    return std::clamp(value, 0.0, 1.0);
}

template <typename Result>
static Result Run(auto value, std::string code) {
    return EvalUtil::Run<Result>(".dksh compute\n"
                                 "main:\n"
                                 "MOV R0, c[0x0][0x140];"
                                 "MOV R1, c[0x0][0x144];" +
                                     code +
                                     "STG.E [R0], R2;"
                                     "EXIT;\n",
                                 value);
}

TEST_CASE("F2F Simple", "[shader]") {
    for (const double value : {8.0, -2.0, 0.0}) {
        REQUIRE(EvalUnary<f16, f16>("F2F.FTZ.F16.F16", value) == value);
        REQUIRE(EvalUnary<f16, f32>("F2F.FTZ.F16.F32", value) == value);
        // F16 <- F64 kills execution

        REQUIRE(EvalUnary<f32, f16>("F2F.FTZ.F32.F16", value) == value);
        REQUIRE(EvalUnary<f32, f32>("F2F.FTZ.F32.F32", value) == value);
        REQUIRE(EvalUnary<f32, f64>("F2F.FTZ.F32.F64", value) == value);

        // F64 <- F16 kills execution
        REQUIRE(EvalUnary<f64, f32>("F2F.FTZ.F64.F32", value) == value);
        REQUIRE(EvalUnary<f64, f64>("F2F.FTZ.F64.F64", value) == value);
    }
}

TEST_CASE("F2F Saturate", "[shader]") {
    for (const double value : {8.0, -2.0, 0.0}) {
        // Saturating from F64 or to F64 doesn't work
        REQUIRE(EvalUnary<f16, f16>("F2F.FTZ.F16.F16.SAT", value) == Saturate(value));
        REQUIRE(EvalUnary<f16, f32>("F2F.FTZ.F16.F32.SAT", value) == Saturate(value));
        REQUIRE(EvalUnary<f32, f16>("F2F.FTZ.F32.F16.SAT", value) == Saturate(value));
        REQUIRE(EvalUnary<f32, f32>("F2F.FTZ.F32.F32.SAT", value) == Saturate(value));
    }
}

TEST_CASE("F2F NAN", "[shader]") {
    constexpr u16 fp16_nnan = 0xffff;
    constexpr u16 fp16_pnan = 0x7fff;
    constexpr u16 fp16_inf = 0x7c00;
    constexpr u16 fp16_ninf = 0xfc00;

    REQUIRE(EvalUnary<u16, u16>("F2F.FTZ.F16.F16", fp16_nnan) == fp16_pnan);
    REQUIRE(EvalUnary<u16, u16>("F2F.FTZ.F16.F16", fp16_pnan) == fp16_pnan);

    REQUIRE(EvalUnary<u16, f32>("F2F.FTZ.F16.F32", NAN) == fp16_pnan);

    REQUIRE(std::isnan(EvalUnary<f32, u16>("F2F.FTZ.F32.F16", fp16_nnan)));
    REQUIRE(std::isnan(EvalUnary<f32, u16>("F2F.FTZ.F32.F16", fp16_pnan)));
    REQUIRE(std::isnan(EvalUnary<f32, f32>("F2F.FTZ.F32.F32", NAN)));
    REQUIRE(std::isnan(EvalUnary<f32, f64>("F2F.FTZ.F32.F64", NAN)));

    REQUIRE(std::isnan(EvalUnary<f64, f32>("F2F.FTZ.F64.F32", NAN)));
    REQUIRE(std::isnan(EvalUnary<f64, f64>("F2F.FTZ.F64.F64", NAN)));

    REQUIRE(EvalUnary<u16, u16>("F2F.FTZ.F16.F16", fp16_inf) == fp16_inf);
    REQUIRE(EvalUnary<u16, u16>("F2F.FTZ.F16.F16", fp16_ninf) == fp16_ninf);

    REQUIRE(EvalUnary<f32, u16>("F2F.FTZ.F32.F16", fp16_inf) == INFINITY);
    REQUIRE(EvalUnary<f32, u16>("F2F.FTZ.F32.F16", fp16_ninf) == -INFINITY);

    REQUIRE(EvalUnary<f32, f32>("F2F.FTZ.F32.F32", INFINITY) == INFINITY);
    REQUIRE(EvalUnary<f32, f64>("F2F.FTZ.F32.F64", INFINITY) == INFINITY);
    REQUIRE(EvalUnary<f32, f32>("F2F.FTZ.F32.F32", -INFINITY) == -INFINITY);
    REQUIRE(EvalUnary<f32, f64>("F2F.FTZ.F32.F64", -INFINITY) == -INFINITY);

    REQUIRE(EvalUnary<f64, f32>("F2F.FTZ.F64.F32", INFINITY) == INFINITY);
    REQUIRE(EvalUnary<f64, f64>("F2F.FTZ.F64.F64", INFINITY) == INFINITY);
    REQUIRE(EvalUnary<f64, f32>("F2F.FTZ.F64.F32", -INFINITY) == -INFINITY);
    REQUIRE(EvalUnary<f64, f64>("F2F.FTZ.F64.F64", -INFINITY) == -INFINITY);
}

TEST_CASE("F2F Rounding", "[shader]") {
    for (const double value : {2.5, 3.5, -1.5, -3.5}) {
        REQUIRE(EvalUnary<u16, f16>("F2F.FTZ.F16.F16.ROUND", value) ==
                BitCast<u16>(f16(RoundEven(value))));
        REQUIRE(EvalUnary<u16, f16>("F2F.FTZ.F16.F16.FLOOR", value) ==
                BitCast<u16>(f16(std::floor(value))));
        REQUIRE(EvalUnary<u16, f16>("F2F.FTZ.F16.F16.CEIL", value) ==
                BitCast<u16>(f16(std::ceil(value))));
        REQUIRE(EvalUnary<u16, f16>("F2F.FTZ.F16.F16.TRUNC", value) ==
                BitCast<u16>(f16(std::trunc(value))));

        REQUIRE(EvalUnary<f32, f32>("F2F.FTZ.F32.F32.ROUND", value) == RoundEven(value));
        REQUIRE(EvalUnary<f32, f32>("F2F.FTZ.F32.F32.FLOOR", value) == std::floor(value));
        REQUIRE(EvalUnary<f32, f32>("F2F.FTZ.F32.F32.CEIL", value) == std::ceil(value));
        REQUIRE(EvalUnary<f32, f32>("F2F.FTZ.F32.F32.TRUNC", value) == std::trunc(value));

        REQUIRE(EvalUnary<f64, f64>("F2F.FTZ.F64.F64.ROUND", value) == RoundEven(value));
        REQUIRE(EvalUnary<f64, f64>("F2F.FTZ.F64.F64.FLOOR", value) == std::floor(value));
        REQUIRE(EvalUnary<f64, f64>("F2F.FTZ.F64.F64.CEIL", value) == std::ceil(value));
        REQUIRE(EvalUnary<f64, f64>("F2F.FTZ.F64.F64.TRUNC", value) == std::trunc(value));
    }
    // Make sure we are reading the bits properly
    REQUIRE(EvalUnary<f16, f32>("F2F.FTZ.F16.F32.RZ", 1.5) == 1.5);
    REQUIRE(EvalUnary<f16, f32>("F2F.FTZ.F16.F32.RP", 1.5) == 1.5);
    REQUIRE(EvalUnary<f16, f32>("F2F.FTZ.F16.F32.RM", 1.5) == 1.5);

    REQUIRE(EvalUnary<f32, f64>("F2F.FTZ.F32.F64.RZ", 1.5) == 1.5);
    REQUIRE(EvalUnary<f32, f64>("F2F.FTZ.F32.F64.RP", 1.5) == 1.5);
    REQUIRE(EvalUnary<f32, f64>("F2F.FTZ.F32.F64.RM", 1.5) == 1.5);
}

TEST_CASE("F2F Denorm", "[shader][fpcontrol]") {
    REQUIRE(EvalUnary<f32, f32>("F2F.FTZ.F32.F32", 1e-40) == 0.0f);
    REQUIRE(EvalUnary<f32, f32>("F2F.F32.F32", 1e-40) > 0.0f);

    // FTZ is ignored for F64 inputs and outputs
    REQUIRE(EvalUnary<f64, f64>("F2F.FTZ.F64.F64", 5e-324) > 0.0f);
    REQUIRE(EvalUnary<f64, f32>("F2F.FTZ.F64.F32", 1e-40) > 0.0f);
}

TEST_CASE("F2F Extract", "[shader]") {
    REQUIRE(Run<f16>(std::array<f16, 2>{3, 7}, "F2F.FTZ.F16.F16 R2, c[2][0].H0;") == 3.0);
    REQUIRE(Run<f16>(std::array<f16, 2>{3, 7}, "F2F.FTZ.F16.F16 R2, c[2][0].H1;") == 7.0);
}

TEST_CASE("F2F Modifiers", "[shader]") {
    REQUIRE(Run<f16>(std::array<f16, 2>{3, 7}, "F2F.FTZ.F16.F16 R2, -c[2][0].H0;") == -3.0);
    REQUIRE(Run<f16>(std::array<f16, 2>{3, 7}, "F2F.FTZ.F16.F16 R2, -c[2][0].H1;") == -7.0);

    REQUIRE(Run<f16>(3.0f, "F2F.FTZ.F16.F32 R2, -c[2][0];") == -3.0);
    REQUIRE(Run<f16>(4.0f, "F2F.FTZ.F16.F32 R2, -c[2][0];") == -4.0);

    REQUIRE(Run<f32>(3.0f, "F2F.FTZ.F32.F32 R2, -c[2][0];") == -3.0);
    REQUIRE(Run<f32>(4.0f, "F2F.FTZ.F32.F32 R2, -c[2][0];") == -4.0);

    REQUIRE(Run<f32>(3.0, "F2F.FTZ.F32.F64 R2, -c[2][0];") == -3.0);
    REQUIRE(Run<f32>(4.0, "F2F.FTZ.F32.F64 R2, -c[2][0];") == -4.0);

    REQUIRE(Run<f16>(-3.0f, "F2F.FTZ.F16.F32 R2, |c[2][0]|;") == 3.0);
    REQUIRE(Run<f16>(-4.0f, "F2F.FTZ.F16.F32 R2, |c[2][0]|;") == 4.0);

    REQUIRE(Run<f32>(-3.0f, "F2F.FTZ.F32.F32 R2, |c[2][0]|;") == 3.0);
    REQUIRE(Run<f32>(-4.0f, "F2F.FTZ.F32.F32 R2, |c[2][0]|;") == 4.0);
}
