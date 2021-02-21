#include <catch2/catch_test_macros.hpp>

#include "../eval_util.h"

using namespace EvalUtil;

TEST_CASE("FADD Simple", "[float_arithmetic]") {
    REQUIRE(EvalBinaryImm<f32>("FADD.FTZ", 5.0f, 3.0f) == 8.0f);
    REQUIRE(EvalBinaryImm<f32>("FADD.FTZ", 1.0f, 3.0f) == 4.0f);
    REQUIRE(EvalBinaryImm<f32>("FADD.FTZ", -1.0f, 4.0f) == 3.0f);

    REQUIRE(EvalBinaryImm<f32>("FADD.FTZ", -1.0f, 4.0f, true, false, false, false) == 5.0f);
    REQUIRE(EvalBinaryImm<f32>("FADD.FTZ", -1.0f, 4.0f, true, true, false, false) == 3.0f);
    REQUIRE(EvalBinaryImm<f32>("FADD.FTZ", -1.0f, 4.0f, false, true, false, false) == 5.0f);

    REQUIRE(EvalBinaryImm<f32>("FADD.FTZ", 1.0f, -2.0f, false, false, true, false) == 3.0f);
    REQUIRE(EvalBinaryImm<f32>("FADD.FTZ", 1.0f, -2.0f, false, false, false, true) == 3.0f);
    REQUIRE(EvalBinaryImm<f32>("FADD.FTZ", 1.0f, -2.0f, false, false, true, true) == -1.0f);
}

TEST_CASE("FADD Saturate", "[float_arithmetic]") {
    REQUIRE(EvalBinaryImm<f32>("FADD.FTZ.SAT", 4.0f, 2.0f) == 1.0f);
    REQUIRE(EvalBinaryImm<f32>("FADD.FTZ.SAT", -4.0f, 2.0f) == 0.0f);
    REQUIRE(EvalBinaryImm<f32>("FADD.FTZ.SAT", 0.5f, 0.25f) == 0.75f);
}

TEST_CASE("FADD NAN", "[float_arithmetic]") {
    static constexpr f32 qnan{std::numeric_limits<f32>::quiet_NaN()};
    REQUIRE(std::isnan(EvalBinaryImm<f32>("FADD.FTZ", qnan, 0.0f)));
    REQUIRE(std::isnan(EvalBinaryImm<f32>("FADD.FTZ", 0.0f, qnan)));
    REQUIRE(std::isnan(EvalBinaryImm<f32>("FADD.FTZ", qnan, qnan)));
    REQUIRE(std::isnan(EvalBinaryImm<f32>("FADD.FTZ", qnan, qnan, true, false, false, false)));
    REQUIRE(EvalBinaryImm<f32>("FADD.FTZ.SAT", 0.0f, qnan) == 0.0f);
}

TEST_CASE("FADD Infinity", "[float_arithmetic]") {
    static constexpr f32 inf{std::numeric_limits<f32>::infinity()};
    REQUIRE(EvalBinaryImm<f32>("FADD.FTZ", inf, 0.0f) == inf);
    REQUIRE(EvalBinaryImm<f32>("FADD.FTZ", 0.0f, inf) == inf);
    REQUIRE(std::isnan(EvalBinaryImm<f32>("FADD.FTZ", inf, inf, true, false, false, false)));
    REQUIRE(std::isnan(EvalBinaryImm<f32>("FADD.FTZ", -inf, inf)));
}

/*
TEST_CASE("FADD Rounding", "[float_arithmetic]") {
    REQUIRE(EvalBinary<f32>("FADD.FTZ.RN", 0.001f, 0.002f) <= 0.003f);
    REQUIRE(EvalBinary<f32>("FADD.FTZ.RZ", 0.001f, 0.002f) <= 0.003f);
    REQUIRE(EvalBinary<f32>("FADD.FTZ.RM", 0.001f, 0.002f) <= 0.003f);
    REQUIRE(EvalBinary<f32>("FADD.FTZ.RP", 0.001f, 0.002f) > 0.003f);

    REQUIRE(EvalBinary<f32>("FADD.FTZ.RM", -0.001f, -0.002f) < -0.003f);
    REQUIRE(EvalBinary<f32>("FADD.FTZ.RN", -0.001f, -0.002f) >= -0.003f);
    REQUIRE(EvalBinary<f32>("FADD.FTZ.RZ", -0.001f, -0.002f) >= -0.003f);
    REQUIRE(EvalBinary<f32>("FADD.FTZ.RP", -0.001f, -0.002f) >= -0.003f);
}
*/

TEST_CASE("FADD Denorm", "[float_arithmetic]") {
    static constexpr f32 denorm{1e-40};
    static constexpr f32 denorm_two{denorm + denorm};
    REQUIRE(EvalBinary<f32>("FADD", denorm, denorm) == denorm_two);
    REQUIRE(EvalBinary<f32>("FADD.FTZ", denorm, denorm) == 0);
}
