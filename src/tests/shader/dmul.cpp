#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

static double Run(double lhs, double rhs, std::string code) {
    return EvalUtil::Run<double>(".dksh compute\n"
                                 "main:\n"
                                 "MOV R0, c[0x0][0x140];\n"
                                 "MOV R1, c[0x0][0x144];\n"
                                 "MOV R2, c[2][0];"
                                 "MOV R3, c[2][4];"
                                 "MOV R4, c[2][8];"
                                 "MOV R5, c[2][12];" +
                                     code +
                                     "STG.E.64 [R0], R2;\n"
                                     "EXIT;\n",
                                 std::array{lhs, rhs});
}

TEST_CASE("DMUL Simple", "[shader]") {
    REQUIRE(Run(4, 7, "DMUL R2, R2, R4;") == 4 * 7);
    REQUIRE(Run(4, 7, "DMUL R2, R2, -R4;") == 4 * -7);

    REQUIRE(Run(4, 7, "DMUL R2, R2, 7;") == 4 * 7);
    REQUIRE(Run(4, 7, "DMUL R2, R2, 7 .NEG;") == 4 * -7);

    REQUIRE(Run(4, 7, "DMUL R2, R2, c[2][8];") == 4 * 7);
    REQUIRE(Run(4, 7, "DMUL R2, R2, -c[2][8];") == 4 * -7);
}

TEST_CASE("DMUL NAN", "[shader]") {
    constexpr double nan = NAN;
    constexpr double inf = INFINITY;

    REQUIRE(std::isnan(Run(4, nan, "DMUL R2, R2, R4;")));
    REQUIRE(std::isnan(Run(nan, nan, "DMUL R2, R2, R4;")));
    REQUIRE(std::isnan(Run(inf, nan, "DMUL R2, R2, R4;")));
    REQUIRE(std::isnan(Run(nan, inf, "DMUL R2, R2, R4;")));
    REQUIRE(Run(inf, -inf, "DMUL R2, R2, R4;") == -inf);
    REQUIRE(Run(-inf, inf, "DMUL R2, R2, R4;") == -inf);
}

TEST_CASE("DMUL Unaligned", "[shader]") {
    constexpr double small = 7.000001;

    // When using an unaligned offset, the lower bits get discarded
    REQUIRE(Run(4, small, "DMUL R2, R2, c[2][8];") == 4 * small);
    REQUIRE(Run(4, small, "DMUL R2, R2, c[2][12];") == 4 * 7);
}
