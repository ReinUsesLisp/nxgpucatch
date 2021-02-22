#include <numbers>

#include <catch2/catch_test_macros.hpp>

#include "bit_cast.h"
#include "compare.h"
#include "eval_util.h"

using namespace std::numbers;

static float Run(float value, std::string code) {
    return BitCast<float>(EvalUtil::Run(".dksh compute\n"
                                        "main:\n"
                                        "MOV R0, c[0x0][0x140];\n"
                                        "MOV R1, c[0x0][0x144];\n"
                                        "MOV R4, c[2][0];\n"
                                        "RRO.SINCOS R2, c[2][0];\n"
                                        "RRO.EX2 R3, c[2][0];\n" +
                                            code +
                                            "STG.E [R0], R2;\n"
                                            "EXIT;\n",
                                        value));
}

TEST_CASE("MUFU Simple", "[shader]") {
    REQUIRE(ThresholdCompare(Run(pi_v<float> * 0.0f, "MUFU.COS R2, R2;"), 1.0f, 0.0001f));
    REQUIRE(Run(pi_v<float> * 0.5f, "MUFU.COS R2, R2;") == 0.0f);
    REQUIRE(ThresholdCompare(Run(pi_v<float> * 1.0f, "MUFU.COS R2, R2;"), -1.0f, 0.0001f));

    REQUIRE(Run(pi_v<float> * 0.0f, "MUFU.SIN R2, R2;") == 0.0f);
    REQUIRE(ThresholdCompare(Run(pi_v<float> * 0.5f, "MUFU.SIN R2, R2;"), 1.0f, 0.0001f));
    REQUIRE(ThresholdCompare(Run(pi_v<float> * 1.0f, "MUFU.SIN R2, R2;"), 0.0f, 0.0001f));

    REQUIRE(Run(0.0f, "MUFU.EX2 R2, R3;") == 1.0f);
    REQUIRE(Run(1.0f, "MUFU.EX2 R2, R3;") == 2.0f);
    REQUIRE(Run(2.0f, "MUFU.EX2 R2, R3;") == 4.0f);
    REQUIRE(Run(3.0f, "MUFU.EX2 R2, R3;") == 8.0f);

    REQUIRE(Run(1.0f, "MUFU.LG2 R2, R4;") == 0.0f);
    REQUIRE(Run(2.0f, "MUFU.LG2 R2, R4;") == 1.0f);
    REQUIRE(Run(4.0f, "MUFU.LG2 R2, R4;") == 2.0f);
    REQUIRE(Run(8.0f, "MUFU.LG2 R2, R4;") == 3.0f);

    REQUIRE(Run(1.0f, "MUFU.RCP R2, R4;") == 1.00f);
    REQUIRE(Run(2.0f, "MUFU.RCP R2, R4;") == 0.50f);
    REQUIRE(Run(4.0f, "MUFU.RCP R2, R4;") == 0.25f);

    REQUIRE(Run(1.0f, "MUFU.RSQ R2, R4;") == 1.0f);
    REQUIRE(ThresholdCompare(Run(2.0f, "MUFU.RSQ R2, R4;"), 1.0f / std::sqrt(2.0f), 0.25f));
    REQUIRE(ThresholdCompare(Run(0.5f, "MUFU.RSQ R2, R4;"), 1.0f / std::sqrt(0.5f), 0.25f));

    REQUIRE(Run(1.0f, "MUFU.SQRT R2, R4;") == 1.0f);
    REQUIRE(Run(2.0f, "MUFU.SQRT R2, R4;") == std::sqrt(2.0f));
    REQUIRE(Run(4.0f, "MUFU.SQRT R2, R4;") == std::sqrt(4.0f));
}

TEST_CASE("MUFU NAN", "[shader]") {
    static constexpr float denorm{1e-40};
    static constexpr float inf{INFINITY};
    static constexpr float nan{NAN};
    static constexpr float zero{0.0f};

    REQUIRE(ThresholdCompare(Run(zero, "MUFU.COS R2, R2;"), 1.0f, 0.0001f));
    REQUIRE(ThresholdCompare(Run(-zero, "MUFU.COS R2, R2;"), 1.0f, 0.0001f));
    REQUIRE(ThresholdCompare(Run(denorm, "MUFU.COS R2, R2;"), 1.0f, 0.0001f));
    REQUIRE(ThresholdCompare(Run(-denorm, "MUFU.COS R2, R2;"), 1.0f, 0.0001f));
    REQUIRE(std::isnan(Run(inf, "MUFU.COS R2, R2;")));
    REQUIRE(std::isnan(Run(-inf, "MUFU.COS R2, R2;")));
    REQUIRE(std::isnan(Run(nan, "MUFU.COS R2, R2;")));

    REQUIRE(Run(zero, "MUFU.SIN R2, R2;") == 0.0f);
    REQUIRE(Run(-zero, "MUFU.SIN R2, R2;") == 0.0f);
    REQUIRE(Run(denorm, "MUFU.SIN R2, R2;") == 0.0f);
    REQUIRE(Run(-denorm, "MUFU.SIN R2, R2;") == 0.0f);
    REQUIRE(std::isnan(Run(inf, "MUFU.SIN R2, R2;")));
    REQUIRE(std::isnan(Run(-inf, "MUFU.SIN R2, R2;")));
    REQUIRE(std::isnan(Run(nan, "MUFU.SIN R2, R2;")));

    REQUIRE(Run(zero, "MUFU.EX2 R2, R3;") == 1.0f);
    REQUIRE(Run(-zero, "MUFU.EX2 R2, R3;") == 1.0f);
    REQUIRE(Run(denorm, "MUFU.EX2 R2, R3;") == 1.0f);
    REQUIRE(Run(-denorm, "MUFU.EX2 R2, R3;") == 1.0f);
    REQUIRE(Run(inf, "MUFU.EX2 R2, R3;") == inf);
    REQUIRE(Run(-inf, "MUFU.EX2 R2, R3;") == 0.0f);
    REQUIRE(std::isnan(Run(nan, "MUFU.EX2 R2, R3;")));

    REQUIRE(Run(zero, "MUFU.LG2 R2, R4;") == -inf);
    REQUIRE(Run(-zero, "MUFU.LG2 R2, R4;") == -inf);
    REQUIRE(Run(denorm, "MUFU.LG2 R2, R4;") == -inf);
    REQUIRE(Run(-denorm, "MUFU.LG2 R2, R4;") == -inf);
    REQUIRE(Run(inf, "MUFU.LG2 R2, R4;") == inf);
    REQUIRE(std::isnan(Run(-inf, "MUFU.LG2 R2, R4;")));
    REQUIRE(std::isnan(Run(nan, "MUFU.LG2 R2, R4;")));

    REQUIRE(Run(zero, "MUFU.RCP R2, R4;") == inf);
    REQUIRE(Run(-zero, "MUFU.RCP R2, R4;") == -inf);
    REQUIRE(Run(denorm, "MUFU.RCP R2, R4;") == inf);
    REQUIRE(Run(-denorm, "MUFU.RCP R2, R4;") == -inf);
    REQUIRE(!std::signbit(Run(inf, "MUFU.RCP R2, R4;")));
    REQUIRE(std::signbit(Run(-inf, "MUFU.RCP R2, R4;")));
    REQUIRE(std::isnan(Run(nan, "MUFU.RCP R2, R4;")));

    REQUIRE(Run(zero, "MUFU.RSQ R2, R4;") == inf);
    REQUIRE(Run(-zero, "MUFU.RSQ R2, R4;") == -inf);
    REQUIRE(Run(denorm, "MUFU.RSQ R2, R4;") == inf);
    REQUIRE(Run(-denorm, "MUFU.RSQ R2, R4;") == -inf);
    REQUIRE(Run(inf, "MUFU.RSQ R2, R4;") == 0.0f);
    REQUIRE(std::isnan(Run(-inf, "MUFU.RSQ R2, R4;")));
    REQUIRE(std::isnan(Run(nan, "MUFU.RSQ R2, R4;")));

    REQUIRE(!std::signbit(Run(zero, "MUFU.SQRT R2, R4;")));
    REQUIRE(std::signbit(Run(-zero, "MUFU.SQRT R2, R4;")));
    REQUIRE(!std::signbit(Run(denorm, "MUFU.SQRT R2, R4;")));
    REQUIRE(std::signbit(Run(-denorm, "MUFU.SQRT R2, R4;")));
    REQUIRE(Run(inf, "MUFU.SQRT R2, R4;") == inf);
    REQUIRE(std::isnan(Run(-inf, "MUFU.SQRT R2, R4;")));
    REQUIRE(std::isnan(Run(-1.0f, "MUFU.SQRT R2, R4;")));
    REQUIRE(std::isnan(Run(nan, "MUFU.SQRT R2, R4;")));
}

TEST_CASE("MUFU SAT", "[shader]") {
    REQUIRE(Run(2.0f, "MUFU.SQRT.SAT R2, R4;") == 1.0f);
    REQUIRE(Run(1.0f, "MUFU.SQRT.SAT R2, R4;") == 1.0f);
}
