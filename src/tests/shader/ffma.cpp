#include <catch2/catch_test_macros.hpp>

#include "bit_cast.h"
#include "eval_util.h"

static float Run(std::string code, float value) {
    return BitCast<float>(EvalUtil::Run(".dksh compute\n"
                                        "main:\n"
                                        "MOV R0, c[0x0][0x140];\n"
                                        "MOV R1, c[0x0][0x144];\n"
                                        "MOV32I R2, 0x3f800000;\n"
                                        "MOV32I R3, 0x40000000;\n" +
                                            code +
                                            "STG.E [R0], R2;\n"
                                            "EXIT;\n",
                                        value));
}

static float Run(std::string code) {
    return Run(std::move(code), 0.0f);
}

TEST_CASE("FFMA Simple", "[shader]") {
    //                    D = A * B + C;
    REQUIRE(Run("FFMA.FTZ R2, R3, R3, R2;") == 5.0f);
    REQUIRE(Run("FFMA.FTZ R2, R3, -R3, R2;") == -3.0f);
    REQUIRE(Run("FFMA.FTZ R2, R3, -R3, -R2;") == -5.0f);
    REQUIRE(Run("FFMA.FTZ R2, R3, R3, -R2;") == 3.0f);

    REQUIRE(Run("FFMA.FTZ R2, R3, R3, c[2][0];", 1.0f) == 5.0f);
    REQUIRE(Run("FFMA.FTZ R2, R3, -R3, c[2][0];", 1.0f) == -3.0f);
    REQUIRE(Run("FFMA.FTZ R2, R3, -R3, -c[2][0];", 1.0f) == -5.0f);
    REQUIRE(Run("FFMA.FTZ R2, R3, R3, -c[2][0];", 1.0f) == 3.0f);

    REQUIRE(Run("FFMA.FTZ R2, R3, c[2][0], R2;", 2.0f) == 5.0f);
    REQUIRE(Run("FFMA.FTZ R2, R3, -c[2][0], R2;", 2.0f) == -3.0f);
    REQUIRE(Run("FFMA.FTZ R2, R3, -c[2][0], -R2;", 2.0f) == -5.0f);
    REQUIRE(Run("FFMA.FTZ R2, R3, c[2][0], -R2;", 2.0f) == 3.0f);

    REQUIRE(Run("FFMA.FTZ R2, R3, 2, R2;") == 5.0f);
    REQUIRE(Run("FFMA.FTZ R2, R3, 2 .NEG, R2;") == -3.0f);
    REQUIRE(Run("FFMA.FTZ R2, R3, 2 .NEG, -R2;") == -5.0f);
    REQUIRE(Run("FFMA.FTZ R2, R3, 2, -R2;") == 3.0f);
    REQUIRE(Run("FFMA.FTZ R2, R3, -2 .NEG, -R2;") == 3.0f);

    REQUIRE(Run("FFMA32I.FTZ R2, R3, 2, R2;") == 5.0f);
    REQUIRE(Run("FFMA32I.FTZ R2, -R3, 2, R2;") == -3.0f);
    REQUIRE(Run("FFMA32I.FTZ R2, -R3, 2, -R2;") == -5.0f);
}

TEST_CASE("FFMA SAT", "[shader]") {
    REQUIRE(Run("FFMA.FTZ.SAT R2, R3, R3, R2;") == 1.0f);
    REQUIRE(Run("FFMA.FTZ.SAT R2, R3, 2, R2;") == 1.0f);
    REQUIRE(Run("FFMA.FTZ.SAT R2, R3, c[2][0], R2;", 2) == 1.0f);
    REQUIRE(Run("FFMA.FTZ.SAT R2, R3, R3, c[2][0];", 1) == 1.0f);
}

TEST_CASE("FFMA NAN", "[shader]") {
    static constexpr float nan{NAN};
    static constexpr float inf{INFINITY};

    REQUIRE(Run("FFMA.FTZ.SAT R2, R3, c[2][0], R2;", nan) == 0.0f);
    REQUIRE(std::isnan(Run("FFMA.FTZ R2, R3, c[2][0], R2;", nan)));
    REQUIRE(std::isnan(Run("FFMA.FTZ R2, R3, R3, c[2][0];", nan)));
    REQUIRE(Run("MOV R2, c[2][0]; FFMA.FTZ R2, R3, c[2][0], R2;", inf) == inf);

    REQUIRE(Run("FFMA.FTZ.SAT R2, R3, c[2][0], R2;", inf) == 1.0f);
    REQUIRE(Run("FFMA.FTZ.SAT R2, R3, c[2][0], R2;", -inf) == 0.0f);
    REQUIRE(Run("FFMA.FTZ R2, R3, c[2][0], R2;", inf) == inf);
    REQUIRE(Run("FFMA.FTZ R2, R3, c[2][0], R2;", -inf) == -inf);
}

TEST_CASE("FFMA Denorm", "[shader][fpcontrol]") {
    static constexpr float denorm{1e-40};
    REQUIRE(Run("FFMA.FTZ R2, R2, c[2][0], RZ;", denorm) == 0.0f);
    REQUIRE(Run("FFMA R2, R2, c[2][0], RZ;", denorm) > 0.0f);
}

TEST_CASE("FFMA FMZ", "[shader]") {
    static constexpr float nan{NAN};
    static constexpr float inf{INFINITY};

    REQUIRE(Run("FFMA.FMZ R2, RZ, c[2][0], R2;", nan) == 1.0f);
    REQUIRE(Run("FFMA.FMZ R2, RZ, c[2][0], R2;", inf) == 1.0f);
}
