#include <catch2/catch_test_macros.hpp>

#include "bit_cast.h"
#include "eval_util.h"

using namespace EvalUtil;

static float Run(float value, std::string code) {
    return BitCast<float>(EvalUtil::Run(".dksh compute\n"
                                        "main:\n"
                                        "MOV R0, c[0x0][0x140];\n"
                                        "MOV R1, c[0x0][0x144];\n"
                                        "MOV R2, c[2][0];" +
                                            code +
                                            "STG.E [R0], R2;\n"
                                            "EXIT;\n",
                                        value));
}

TEST_CASE("FADD Simple", "[shader]") {
    REQUIRE(EvalBinaryImm<f32>("FADD.FTZ", 5.0f, 3.0f) == 8.0f);
    REQUIRE(EvalBinaryImm<f32>("FADD.FTZ", 1.0f, 3.0f) == 4.0f);
    REQUIRE(EvalBinaryImm<f32>("FADD.FTZ", -1.0f, 4.0f) == 3.0f);

    REQUIRE(EvalBinaryImm<f32>("FADD.FTZ", -1.0f, 4.0f, true, false, false, false) == 5.0f);
    REQUIRE(EvalBinaryImm<f32>("FADD.FTZ", -1.0f, 4.0f, true, true, false, false) == 3.0f);
    REQUIRE(EvalBinaryImm<f32>("FADD.FTZ", -1.0f, 4.0f, false, true, false, false) == 5.0f);

    REQUIRE(EvalBinaryImm<f32>("FADD.FTZ", 1.0f, -2.0f, false, false, true, false) == 3.0f);
    REQUIRE(EvalBinaryImm<f32>("FADD.FTZ", 1.0f, -2.0f, false, false, false, true) == 3.0f);
    REQUIRE(EvalBinaryImm<f32>("FADD.FTZ", 1.0f, -2.0f, false, false, true, true) == -1.0f);

    REQUIRE(Run(5.0f, "FADD32I.FTZ R2, R2, 3.0;") == 8.0f);
    REQUIRE(Run(5.0f, "FADD32I.FTZ R2, -R2, 3.0;") == -2.0f);
}

TEST_CASE("FADD Saturate", "[shader]") {
    REQUIRE(EvalBinaryImm<f32>("FADD.FTZ.SAT", 4.0f, 2.0f) == 1.0f);
    REQUIRE(EvalBinaryImm<f32>("FADD.FTZ.SAT", -4.0f, 2.0f) == 0.0f);
    REQUIRE(EvalBinaryImm<f32>("FADD.FTZ.SAT", 0.5f, 0.25f) == 0.75f);
}

TEST_CASE("FADD NAN", "[shader]") {
    static constexpr f32 qnan{std::numeric_limits<f32>::quiet_NaN()};
    REQUIRE(std::isnan(EvalBinaryImm<f32>("FADD.FTZ", qnan, 0.0f)));
    REQUIRE(std::isnan(EvalBinaryImm<f32>("FADD.FTZ", 0.0f, qnan)));
    REQUIRE(std::isnan(EvalBinaryImm<f32>("FADD.FTZ", qnan, qnan)));
    REQUIRE(std::isnan(EvalBinaryImm<f32>("FADD.FTZ", qnan, qnan, true, false, false, false)));
    REQUIRE(EvalBinaryImm<f32>("FADD.FTZ.SAT", 0.0f, qnan) == 0.0f);
}

TEST_CASE("FADD INF", "[shader]") {
    static constexpr f32 inf{std::numeric_limits<f32>::infinity()};
    REQUIRE(EvalBinaryImm<f32>("FADD.FTZ", inf, 0.0f) == inf);
    REQUIRE(EvalBinaryImm<f32>("FADD.FTZ", 0.0f, inf) == inf);
    REQUIRE(std::isnan(EvalBinaryImm<f32>("FADD.FTZ", inf, inf, true, false, false, false)));
    REQUIRE(std::isnan(EvalBinaryImm<f32>("FADD.FTZ", -inf, inf)));
}

TEST_CASE("FADD Rounding", "[shader][fpcontrol]") {
    REQUIRE(EvalBinary<f32>("FADD.FTZ.RN", 0.001f, 0.002f) <= 0.003f);
    REQUIRE(EvalBinary<f32>("FADD.FTZ.RZ", 0.001f, 0.002f) <= 0.003f);
    REQUIRE(EvalBinary<f32>("FADD.FTZ.RM", 0.001f, 0.002f) <= 0.003f);
    REQUIRE(EvalBinary<f32>("FADD.FTZ.RP", 0.001f, 0.002f) > 0.003f);

    REQUIRE(EvalBinary<f32>("FADD.FTZ.RM", -0.001f, -0.002f) < -0.003f);
    REQUIRE(EvalBinary<f32>("FADD.FTZ.RN", -0.001f, -0.002f) >= -0.003f);
    REQUIRE(EvalBinary<f32>("FADD.FTZ.RZ", -0.001f, -0.002f) >= -0.003f);
    REQUIRE(EvalBinary<f32>("FADD.FTZ.RP", -0.001f, -0.002f) >= -0.003f);
}

TEST_CASE("FADD Denorm", "[shader][fpcontrol]") {
    static constexpr f32 denorm{1e-40};
    static constexpr f32 denorm_two{denorm + denorm};
    REQUIRE(EvalBinary<f32>("FADD", denorm, denorm) == denorm_two);
    REQUIRE(EvalBinary<f32>("FADD.FTZ", denorm, denorm) == 0);

    REQUIRE(Run(denorm, "FADD32I R2, R2, 0.0;") > 0.0f);
    REQUIRE(Run(denorm, "FADD32I.FTZ R2, R2, 0.0;") == 0.0f);
}
