#include <catch2/catch_test_macros.hpp>

#include "bit_cast.h"
#include "eval_util.h"

using namespace EvalUtil;

static float Run(float value, std::string code) {
    return BitCast<float>(EvalUtil::Run(".dksh compute\n"
                                        "main:\n"
                                        "MOV R0, c[0x0][0x140];\n"
                                        "MOV R1, c[0x0][0x144];\n"
                                        "MOV32I R2, 0x3fc00000;\n" +
                                            code +
                                            "STG.E [R0], R2;\n"
                                            "EXIT;\n",
                                        value));
}

TEST_CASE("FMUL Simple", "[shader]") {
    REQUIRE(EvalBinaryImm<f32>("FMUL.FTZ", 1.0f, 3.0f) == 3.0f);
    REQUIRE(EvalBinaryImm<f32>("FMUL.FTZ", 2.0f, 3.0f) == 6.0f);
    REQUIRE(EvalBinaryImm<f32>("FMUL.FTZ", -2.0f, 4.0f) == -8.0f);
    REQUIRE(EvalBinaryImm<f32>("FMUL.FTZ", 2.0f, 3.0f, false, false, true, false) == -6.0f);

    REQUIRE(Run(1, "FMUL32I.FTZ R2, R2, 1.0;") == 1.5f);
    REQUIRE(Run(1, "FMUL32I.FTZ R2, R2, -2.0;") == -3.0f);
    REQUIRE(Run(1, "FMUL32I.FTZ.SAT R2, R2, -2.0;") == 0.0f);
}

TEST_CASE("FMUL Scale", "[shader]") {
    REQUIRE(EvalBinaryImm<f32>("FMUL.FTZ.D2", 2.0f, 4.0f) == 4.0f);
    REQUIRE(EvalBinaryImm<f32>("FMUL.FTZ.D4", 2.0f, 4.0f) == 2.0f);
    REQUIRE(EvalBinaryImm<f32>("FMUL.FTZ.D8", 2.0f, 4.0f) == 1.0f);
    REQUIRE(EvalBinaryImm<f32>("FMUL.FTZ.M8", 2.0f, 4.0f) == 64.0f);
    REQUIRE(EvalBinaryImm<f32>("FMUL.FTZ.M4", 2.0f, 4.0f) == 32.0f);
    REQUIRE(EvalBinaryImm<f32>("FMUL.FTZ.M2", 2.0f, 4.0f) == 16.0f);
}

TEST_CASE("FMUL SAT", "[shader]") {
    REQUIRE(EvalBinaryImm<f32>("FMUL.FTZ.SAT", 2.0f, 2.0f) == 1.0f);
    REQUIRE(EvalBinaryImm<f32>("FMUL.FTZ.D2.SAT", 2.0f, 2.0f) == 1.0f);
    REQUIRE(EvalBinaryImm<f32>("FMUL.FTZ.D4.SAT", 2.0f, 2.0f) == 1.0f);
    REQUIRE(EvalBinaryImm<f32>("FMUL.FTZ.D8.SAT", 2.0f, 2.0f) == 0.5f);
    REQUIRE(EvalBinaryImm<f32>("FMUL.FTZ.M8.SAT", 2.0f, 2.0f) == 1.0f);
    REQUIRE(EvalBinaryImm<f32>("FMUL.FTZ.M4.SAT", 2.0f, 2.0f) == 1.0f);
    REQUIRE(EvalBinaryImm<f32>("FMUL.FTZ.M2.SAT", 2.0f, 2.0f) == 1.0f);
}

TEST_CASE("FMUL NAN", "[shader]") {
    REQUIRE(EvalBinaryImm<f32>("FMUL.FTZ", 2.0f, INFINITY) == INFINITY);
    REQUIRE(std::isnan(EvalBinaryImm<f32>("FMUL.FTZ", NAN, 2.0f)));
    REQUIRE(std::isnan(EvalBinaryImm<f32>("FMUL.FTZ", 2.0f, NAN)));
    REQUIRE(std::isnan(EvalBinaryImm<f32>("FMUL.FTZ", NAN, NAN)));
    REQUIRE(std::isnan(EvalBinaryImm<f32>("FMUL.FTZ", NAN, 0.0f)));
    REQUIRE(std::isnan(EvalBinaryImm<f32>("FMUL.FTZ", NAN, -0.0f)));
    REQUIRE(std::isnan(EvalBinaryImm<f32>("FMUL.FTZ", NAN, INFINITY)));
    REQUIRE(std::isnan(EvalBinaryImm<f32>("FMUL.FTZ", 0.0f, INFINITY)));
    REQUIRE(EvalBinaryImm<f32>("FMUL.FTZ.SAT", 2.0f, NAN) == 0.0f);
    REQUIRE(EvalBinaryImm<f32>("FMUL.FTZ.SAT", -2.0f, NAN) == 0.0f);
    REQUIRE(EvalBinaryImm<f32>("FMUL.FTZ.SAT", 2.0f, INFINITY) == 1.0f);
    REQUIRE(EvalBinaryImm<f32>("FMUL.FTZ.SAT", 2.0f, -INFINITY) == 0.0f);
}

TEST_CASE("FMUL FMZ", "[shader]") {
    REQUIRE(EvalBinaryImm<f32>("FMUL.FMZ", 2.0f, INFINITY) == INFINITY);
    REQUIRE(std::isnan(EvalBinaryImm<f32>("FMUL.FMZ", NAN, 2.0f)));
    REQUIRE(std::isnan(EvalBinaryImm<f32>("FMUL.FMZ", 2.0f, NAN)));
    REQUIRE(std::isnan(EvalBinaryImm<f32>("FMUL.FMZ", NAN, NAN)));
    REQUIRE(std::isnan(EvalBinaryImm<f32>("FMUL.FMZ", NAN, INFINITY)));

    REQUIRE(EvalBinaryImm<f32>("FMUL.FMZ", NAN, 0.0f) == 0.0f);
    REQUIRE(EvalBinaryImm<f32>("FMUL.FMZ", 0.0f, INFINITY) == 0.0f);
    REQUIRE(!std::signbit(EvalBinaryImm<f32>("FMUL.FMZ", 0.0f, INFINITY)));
    REQUIRE(!std::signbit(EvalBinaryImm<f32>("FMUL.FMZ", -0.0f, INFINITY)));

    REQUIRE(EvalBinaryImm<f32>("FMUL.FMZ.SAT", 2.0f, NAN) == 0.0f);
    REQUIRE(EvalBinaryImm<f32>("FMUL.FMZ.SAT", -2.0f, NAN) == 0.0f);
}

TEST_CASE("FMUL Denorm", "[shader][fpcontrol]") {
    static constexpr float denorm{1e-40};
    REQUIRE(Run(denorm, "FMUL.FTZ R2, R2, c[2][0];") == 0.0f);
    REQUIRE(Run(denorm, "FMUL R2, R2, c[2][0];") > 0.0f);
}
