#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

static double Run(double a, double b, std::string code) {
    return EvalUtil::Run<double>(".dksh compute\n"
                                 "main:\n"
                                 "MOV R0, c[2][0];"
                                 "MOV R1, c[2][4];"
                                 "MOV R2, c[2][8];"
                                 "MOV R3, c[2][12];" +
                                     code +
                                     "MOV R2, c[0x0][0x140];"
                                     "MOV R3, c[0x0][0x144];"
                                     "STG.E.64 [R2], R0;\n"
                                     "EXIT;\n",
                                 std::array{a, b});
}

TEST_CASE("DMNMX Simple", "[shader]") {
    REQUIRE(Run(3, 5, "DMNMX R0, R0, R2, PT;") == 3);
    REQUIRE(Run(3, 5, "DMNMX R0, R0, R2, !PT;") == 5);
    REQUIRE(Run(3, 5, "DMNMX R0, -R0, R2, PT;") == -3);
    REQUIRE(Run(3, 5, "DMNMX R0, -R0, -R2, PT;") == -5);
    REQUIRE(Run(-3, 5, "DMNMX R0, |R0|, R2, PT;") == 3);
    REQUIRE(Run(3, -5, "DMNMX R0, R0, |R2|, !PT;") == 5);

    REQUIRE(Run(3, 5, "DMNMX R0, R0, c[2][8], PT;") == 3);
    REQUIRE(Run(3, 5, "DMNMX R0, R0, c[2][8], !PT;") == 5);
    REQUIRE(Run(3, 5, "DMNMX R0, -R0, c[2][8], PT;") == -3);
    REQUIRE(Run(3, 5, "DMNMX R0, -R0, -c[2][8], PT;") == -5);
    REQUIRE(Run(-3, 5, "DMNMX R0, |R0|, c[2][8], PT;") == 3);
    REQUIRE(Run(3, -5, "DMNMX R0, R0, |c[2][8]|, !PT;") == 5);

    REQUIRE(Run(3, 5, "DMNMX R0, R0, 5, PT;") == 3);
    REQUIRE(Run(3, 5, "DMNMX R0, R0, 5, !PT;") == 5);
    REQUIRE(Run(3, 5, "DMNMX R0, R0, -5 .ABS, !PT;") == 5);
    REQUIRE(Run(3, 5, "DMNMX R0, -R0, 5, PT;") == -3);
    REQUIRE(Run(3, 5, "DMNMX R0, -R0, -5, PT;") == -5);
    REQUIRE(Run(3, 5, "DMNMX R0, -R0, 5 .NEG, PT;") == -5);
    REQUIRE(Run(-3, 5, "DMNMX R0, |R0|, 5, PT;") == 3);
    REQUIRE(Run(3, -5, "DMNMX R0, R0, -5 .ABS, !PT;") == 5);

    REQUIRE(Run(3, 5, "ISETP.F.AND P0, PT, RZ, RZ, PT; DMNMX R0, R0, R2, !P0;") == 3);
}