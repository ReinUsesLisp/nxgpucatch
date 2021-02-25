#include <catch2/catch_test_macros.hpp>

#include "bit_cast.h"
#include "eval_util.h"

static float Run(float a, float b, std::string code) {
    return BitCast<float>(EvalUtil::Run(".dksh compute\n"
                                        "main:\n"
                                        "MOV R0, c[0x0][0x140];\n"
                                        "MOV R1, c[0x0][0x144];\n"
                                        "MOV R2, c[2][0]\n;"
                                        "MOV R3, c[2][4]\n;" +
                                            code +
                                            "STG.E [R0], R2;\n"
                                            "EXIT;\n",
                                        std::array{a, b}));
}

TEST_CASE("FMNMX Simple", "[shader]") {
    REQUIRE(Run(1.0f, 3.0f, "FMNMX.FTZ R2, R2, R3, PT;") == 1.0f);
    REQUIRE(Run(3.0f, 1.0f, "FMNMX.FTZ R2, R2, R3, PT;") == 1.0f);

    REQUIRE(Run(1.0f, 3.0f, "FMNMX.FTZ R2, R2, R3, !PT;") == 3.0f);
    REQUIRE(Run(3.0f, 1.0f, "FMNMX.FTZ R2, R2, R3, !PT;") == 3.0f);

    REQUIRE(Run(1.0f, 3.0f, "FMNMX.FTZ R2, R2, 3, !PT;") == 3.0f);
    REQUIRE(Run(3.0f, 1.0f, "FMNMX.FTZ R2, R2, 3, !PT;") == 3.0f);

    REQUIRE(Run(1.0f, 3.0f, "FMNMX.FTZ R2, R2, c[2][4], !PT;") == 3.0f);
    REQUIRE(Run(3.0f, 1.0f, "FMNMX.FTZ R2, R2, c[2][4], !PT;") == 3.0f);

    REQUIRE(Run(1.0f, 3.0f, "FMNMX.FTZ R2, -R2, R3, PT;") == -1.0f);
    REQUIRE(Run(3.0f, 1.0f, "FMNMX.FTZ R2, R2, -R3, !PT;") == 3.0f);

    REQUIRE(Run(-1.0f, -3.0f, "FMNMX.FTZ R2, |R2|, R3, PT;") == -3.0f);
    REQUIRE(Run(-3.0f, -1.0f, "FMNMX.FTZ R2, R2, |R3|, !PT;") == 1.0f);
}

TEST_CASE("FMNMX NAN", "[shader]") {
    static constexpr float nan{NAN};
    static constexpr float inf{INFINITY};

    REQUIRE(Run(0.0f, nan, "FMNMX.FTZ R2, R2, R3, PT;") == 0.0f);
    REQUIRE(Run(0.0f, nan, "FMNMX.FTZ R2, R2, R3, !PT;") == 0.0f);

    REQUIRE(Run(nan, 0.0f, "FMNMX.FTZ R2, R2, R3, PT;") == 0.0f);
    REQUIRE(Run(nan, 0.0f, "FMNMX.FTZ R2, R2, R3, !PT;") == 0.0f);

    REQUIRE(std::isnan(Run(nan, nan, "FMNMX.FTZ R2, R2, R3, PT;")));
    REQUIRE(std::isnan(Run(nan, nan, "FMNMX.FTZ R2, R2, R3, !PT;")));

    REQUIRE(Run(inf, 0.0f, "FMNMX.FTZ R2, R2, R3, PT;") == 0.0f);
    REQUIRE(Run(-inf, 0.0f, "FMNMX.FTZ R2, R2, R3, PT;") == -inf);

    REQUIRE(Run(inf, 0.0f, "FMNMX.FTZ R2, R2, R3, !PT;") == inf);
    REQUIRE(Run(-inf, 0.0f, "FMNMX.FTZ R2, R2, R3, !PT;") == 0.0f);

    REQUIRE(Run(inf, inf, "FMNMX.FTZ R2, R2, R3, PT;") == inf);
    REQUIRE(Run(inf, -inf, "FMNMX.FTZ R2, R2, R3, PT;") == -inf);

    REQUIRE(Run(-inf, inf, "FMNMX.FTZ R2, R2, R3, !PT;") == inf);
    REQUIRE(Run(-inf, -inf, "FMNMX.FTZ R2, R2, R3, !PT;") == -inf);

    REQUIRE(Run(inf, nan, "FMNMX.FTZ R2, R2, R3, PT;") == inf);
    REQUIRE(Run(-inf, nan, "FMNMX.FTZ R2, R2, R3, PT;") == -inf);
}

TEST_CASE("FMNMX Denorm", "[shader][fpcontrol]") {
    static constexpr float denorm{1e-40};

    REQUIRE(Run(0.0f, -denorm, "FMNMX.FTZ R2, R2, R3, PT;") == 0.0f);
    REQUIRE(Run(0.0f, denorm, "FMNMX.FTZ R2, R2, R3, !PT;") == 0.0f);
    REQUIRE(Run(denorm, 0.0f, "FMNMX.FTZ R2, R2, R3, !PT;") == 0.0f);

    REQUIRE(Run(0.0f, -denorm, "FMNMX R2, R2, R3, PT;") == -denorm);
    REQUIRE(Run(0.0f, denorm, "FMNMX R2, R2, R3, !PT;") == denorm);
    REQUIRE(Run(denorm, 0.0f, "FMNMX R2, R2, R3, !PT;") == denorm);
}
