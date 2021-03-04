#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

static double Run(double a, double b, double c, std::string code) {
    return EvalUtil::Run<double>(".dksh compute\n"
                                 "main:\n"
                                 "MOV R0, c[2][0];"
                                 "MOV R1, c[2][4];"
                                 "MOV R2, c[2][8];"
                                 "MOV R3, c[2][12];"
                                 "MOV R4, c[2][16];"
                                 "MOV R5, c[2][20];" +
                                     code +
                                     "MOV R2, c[0x0][0x140];"
                                     "MOV R3, c[0x0][0x144];"
                                     "STG.E.64 [R2], R0;\n"
                                     "EXIT;\n",
                                 std::array{a, b, c});
}

TEST_CASE("DFMA Simple", "[shader]") {
    REQUIRE(Run(1, 2, 4, "DFMA R0, R0, R2, R4;") == 1 * 2 + 4);
    REQUIRE(Run(1, 2, 4, "DFMA R0, R0, -R2, R4;") == 1 * -2 + 4);
    REQUIRE(Run(1, 2, 4, "DFMA R0, R0, R2, -R4;") == 1 * 2 + -4);

    REQUIRE(Run(1, 2, 4, "DFMA R0, R0, c[2][8], R4;") == 1 * 2 + 4);
    REQUIRE(Run(1, 2, 4, "DFMA R0, R0, -c[2][8], R4;") == 1 * -2 + 4);
    REQUIRE(Run(1, 2, 4, "DFMA R0, R0, c[2][8], -R4;") == 1 * 2 + -4);

    REQUIRE(Run(1, 2, 4, "DFMA R0, R0, R2, c[2][16];") == 1 * 2 + 4);
    REQUIRE(Run(1, 2, 4, "DFMA R0, R0, -R2, c[2][16];") == 1 * -2 + 4);
    REQUIRE(Run(1, 2, 4, "DFMA R0, R0, R2, -c[2][16];") == 1 * 2 + -4);

    REQUIRE(Run(1, 2, 4, "DFMA R0, R0, 2, R4;") == 1 * 2 + 4);
    REQUIRE(Run(1, 2, 4, "DFMA R0, R0, -2, R4;") == 1 * -2 + 4);
    REQUIRE(Run(1, 2, 4, "DFMA R0, R0, 2 .NEG, R4;") == 1 * -2 + 4);
    REQUIRE(Run(1, 2, 4, "DFMA R0, R0, 2, -R4;") == 1 * 2 + -4);
}

TEST_CASE("DFMA Unaligned", "[shader]") {
    constexpr double small = 7.00001;

    const double cr = Run(1, small, 1, "DFMA R0, R0, c[2][12], R4;");
    REQUIRE((cr > 8 && cr < 8.00001));
    REQUIRE(Run(1, small, 1, "DFMA R0, R0, c[2][8], R4;") == fma(1, small, 1));

    const double rc = Run(1, 2, small, "DFMA R0, R0, R2, c[2][20];");
    REQUIRE((rc > 9 && rc < 9.00001));
    REQUIRE(Run(1, 2, small, "DFMA R0, R0, R2, c[2][16];") == fma(1, 2, small));
}
