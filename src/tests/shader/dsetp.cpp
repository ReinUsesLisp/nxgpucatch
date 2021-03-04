#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

static int32_t Run(double a, double b, std::string code) {
    return EvalUtil::Run<int32_t>(".dksh compute\n"
                                  "main:\n"
                                  "MOV R0, c[2][0];"
                                  "MOV R1, c[2][4];"
                                  "MOV R2, c[2][8];"
                                  "MOV R3, c[2][12];" +
                                      code +
                                      "MOV R0, RZ;"
                                      "@P0 IADD R0, R0, 0x0f;"
                                      "@P1 IADD R0, R0, 0xf0;"
                                      "MOV R2, c[0x0][0x140];"
                                      "MOV R3, c[0x0][0x144];"
                                      "STG.E [R2], R0;\n"
                                      "EXIT;\n",
                                  std::array{a, b});
}

TEST_CASE("DSETP Simple", "[shader]") {
    REQUIRE(Run(1, 2, "DSETP.F.AND P0, P1, R0, R2, PT;") == 0xf0);
    REQUIRE(Run(1, 2, "DSETP.T.AND P0, P1, R0, R2, PT;") == 0x0f);

    REQUIRE(Run(2, 2, "DSETP.LT.AND P0, P1, R0, R2, PT;") == 0xf0);
    REQUIRE(Run(1, 2, "DSETP.LT.AND P0, P1, R0, R2, PT;") == 0x0f);
    REQUIRE(Run(1, 2, "DSETP.EQ.AND P0, P1, R0, R2, PT;") == 0xf0);
    REQUIRE(Run(2, 2, "DSETP.EQ.AND P0, P1, R0, R2, PT;") == 0x0f);
    REQUIRE(Run(2, 2, "DSETP.LE.AND P0, P1, R0, R2, PT;") == 0x0f);
    REQUIRE(Run(2, 3, "DSETP.LE.AND P0, P1, R0, R2, PT;") == 0x0f);
    REQUIRE(Run(3, 2, "DSETP.LE.AND P0, P1, R0, R2, PT;") == 0xf0);
    REQUIRE(Run(2, 2, "DSETP.GT.AND P0, P1, R0, R2, PT;") == 0xf0);
    REQUIRE(Run(3, 2, "DSETP.GT.AND P0, P1, R0, R2, PT;") == 0x0f);
    REQUIRE(Run(2, 2, "DSETP.NE.AND P0, P1, R0, R2, PT;") == 0xf0);
    REQUIRE(Run(2, 3, "DSETP.NE.AND P0, P1, R0, R2, PT;") == 0x0f);
    REQUIRE(Run(2, 2, "DSETP.GE.AND P0, P1, R0, R2, PT;") == 0x0f);
    REQUIRE(Run(3, 2, "DSETP.GE.AND P0, P1, R0, R2, PT;") == 0x0f);
    REQUIRE(Run(2, 3, "DSETP.GE.AND P0, P1, R0, R2, PT;") == 0xf0);

    REQUIRE(Run(2, 2, "DSETP.LTU.AND P0, P1, R0, R2, PT;") == 0xf0);
    REQUIRE(Run(1, 2, "DSETP.LTU.AND P0, P1, R0, R2, PT;") == 0x0f);
    REQUIRE(Run(1, 2, "DSETP.EQU.AND P0, P1, R0, R2, PT;") == 0xf0);
    REQUIRE(Run(2, 2, "DSETP.EQU.AND P0, P1, R0, R2, PT;") == 0x0f);
    REQUIRE(Run(2, 2, "DSETP.LEU.AND P0, P1, R0, R2, PT;") == 0x0f);
    REQUIRE(Run(2, 3, "DSETP.LEU.AND P0, P1, R0, R2, PT;") == 0x0f);
    REQUIRE(Run(3, 2, "DSETP.LEU.AND P0, P1, R0, R2, PT;") == 0xf0);
    REQUIRE(Run(2, 2, "DSETP.GTU.AND P0, P1, R0, R2, PT;") == 0xf0);
    REQUIRE(Run(3, 2, "DSETP.GTU.AND P0, P1, R0, R2, PT;") == 0x0f);
    REQUIRE(Run(2, 2, "DSETP.NEU.AND P0, P1, R0, R2, PT;") == 0xf0);
    REQUIRE(Run(2, 3, "DSETP.NEU.AND P0, P1, R0, R2, PT;") == 0x0f);
    REQUIRE(Run(2, 2, "DSETP.GEU.AND P0, P1, R0, R2, PT;") == 0x0f);
    REQUIRE(Run(3, 2, "DSETP.GEU.AND P0, P1, R0, R2, PT;") == 0x0f);
    REQUIRE(Run(2, 3, "DSETP.GEU.AND P0, P1, R0, R2, PT;") == 0xf0);

    REQUIRE(Run(2, 2, "DSETP.LT.AND P0, P1, R0, c[2][8], PT;") == 0xf0);
    REQUIRE(Run(1, 2, "DSETP.LT.AND P0, P1, R0, c[2][8], PT;") == 0x0f);
    REQUIRE(Run(1, 2, "DSETP.EQ.AND P0, P1, R0, c[2][8], PT;") == 0xf0);
    REQUIRE(Run(2, 2, "DSETP.EQ.AND P0, P1, R0, c[2][8], PT;") == 0x0f);
    REQUIRE(Run(2, 2, "DSETP.LE.AND P0, P1, R0, c[2][8], PT;") == 0x0f);
    REQUIRE(Run(2, 3, "DSETP.LE.AND P0, P1, R0, c[2][8], PT;") == 0x0f);
    REQUIRE(Run(3, 2, "DSETP.LE.AND P0, P1, R0, c[2][8], PT;") == 0xf0);
    REQUIRE(Run(2, 2, "DSETP.GT.AND P0, P1, R0, c[2][8], PT;") == 0xf0);
    REQUIRE(Run(3, 2, "DSETP.GT.AND P0, P1, R0, c[2][8], PT;") == 0x0f);
    REQUIRE(Run(2, 2, "DSETP.NE.AND P0, P1, R0, c[2][8], PT;") == 0xf0);
    REQUIRE(Run(2, 3, "DSETP.NE.AND P0, P1, R0, c[2][8], PT;") == 0x0f);
    REQUIRE(Run(2, 2, "DSETP.GE.AND P0, P1, R0, c[2][8], PT;") == 0x0f);
    REQUIRE(Run(3, 2, "DSETP.GE.AND P0, P1, R0, c[2][8], PT;") == 0x0f);
    REQUIRE(Run(2, 3, "DSETP.GE.AND P0, P1, R0, c[2][8], PT;") == 0xf0);

    REQUIRE(Run(2, 2, "DSETP.LT.AND P0, P1, R0, 2, PT;") == 0xf0);
    REQUIRE(Run(1, 2, "DSETP.LT.AND P0, P1, R0, 2, PT;") == 0x0f);
    REQUIRE(Run(1, 2, "DSETP.EQ.AND P0, P1, R0, 2, PT;") == 0xf0);
    REQUIRE(Run(2, 2, "DSETP.EQ.AND P0, P1, R0, 2, PT;") == 0x0f);
    REQUIRE(Run(2, 2, "DSETP.LE.AND P0, P1, R0, 2, PT;") == 0x0f);
    REQUIRE(Run(2, 3, "DSETP.LE.AND P0, P1, R0, 3, PT;") == 0x0f);
    REQUIRE(Run(3, 2, "DSETP.LE.AND P0, P1, R0, 2, PT;") == 0xf0);
    REQUIRE(Run(2, 2, "DSETP.GT.AND P0, P1, R0, 2, PT;") == 0xf0);
    REQUIRE(Run(3, 2, "DSETP.GT.AND P0, P1, R0, 2, PT;") == 0x0f);
    REQUIRE(Run(2, 2, "DSETP.NE.AND P0, P1, R0, 2, PT;") == 0xf0);
    REQUIRE(Run(2, 3, "DSETP.NE.AND P0, P1, R0, 3, PT;") == 0x0f);
    REQUIRE(Run(2, 2, "DSETP.GE.AND P0, P1, R0, 2, PT;") == 0x0f);
    REQUIRE(Run(3, 2, "DSETP.GE.AND P0, P1, R0, 2, PT;") == 0x0f);
    REQUIRE(Run(2, 3, "DSETP.GE.AND P0, P1, R0, 3, PT;") == 0xf0);

    REQUIRE(Run(2, 2, "DSETP.LT.AND P0, P1, -R0, R2, PT;") == 0x0f);
    REQUIRE(Run(-3, 2, "DSETP.GT.AND P0, P1, |R0|, R2, PT;") == 0x0f);
    REQUIRE(Run(2, 2, "DSETP.GT.AND P0, P1, R0, -R2, PT;") == 0x0f);
    REQUIRE(Run(2, -3, "DSETP.LT.AND P0, P1, R0, |R2|, PT;") == 0x0f);

    REQUIRE(Run(2, 2, "DSETP.LT.OR P0, P1, R0, R2, PT;") == 0xff);
    REQUIRE(Run(2, 2, "DSETP.LT.XOR P0, P1, R0, R2, PT;") == 0x0f);
}

TEST_CASE("DSETP NAN", "[shader]") {
    constexpr double nan = NAN;

    REQUIRE(Run(2, nan, "DSETP.LT.AND P0, P1, R0, R2, PT;") == 0xf0);
    REQUIRE(Run(2, nan, "DSETP.EQ.AND P0, P1, R0, R2, PT;") == 0xf0);
    REQUIRE(Run(2, nan, "DSETP.LE.AND P0, P1, R0, R2, PT;") == 0xf0);
    REQUIRE(Run(2, nan, "DSETP.GT.AND P0, P1, R0, R2, PT;") == 0xf0);
    REQUIRE(Run(2, nan, "DSETP.NE.AND P0, P1, R0, R2, PT;") == 0xf0);
    REQUIRE(Run(2, nan, "DSETP.GE.AND P0, P1, R0, R2, PT;") == 0xf0);

    REQUIRE(Run(2, nan, "DSETP.LTU.AND P0, P1, R0, R2, PT;") == 0x0f);
    REQUIRE(Run(2, nan, "DSETP.EQU.AND P0, P1, R0, R2, PT;") == 0x0f);
    REQUIRE(Run(2, nan, "DSETP.LEU.AND P0, P1, R0, R2, PT;") == 0x0f);
    REQUIRE(Run(2, nan, "DSETP.GTU.AND P0, P1, R0, R2, PT;") == 0x0f);
    REQUIRE(Run(2, nan, "DSETP.NEU.AND P0, P1, R0, R2, PT;") == 0x0f);
    REQUIRE(Run(2, nan, "DSETP.GEU.AND P0, P1, R0, R2, PT;") == 0x0f);

    REQUIRE(Run(2, 2, "DSETP.NUM.AND P0, P1, R0, R2, PT;") == 0x0f);
    REQUIRE(Run(2, nan, "DSETP.NUM.AND P0, P1, R0, R2, PT;") == 0xf0);
    REQUIRE(Run(nan, 2, "DSETP.NUM.AND P0, P1, R0, R2, PT;") == 0xf0);
    REQUIRE(Run(nan, nan, "DSETP.NUM.AND P0, P1, R0, R2, PT;") == 0xf0);

    REQUIRE(Run(2, 2, "DSETP.NAN.AND P0, P1, R0, R2, PT;") == 0xf0);
    REQUIRE(Run(2, nan, "DSETP.NAN.AND P0, P1, R0, R2, PT;") == 0x0f);
    REQUIRE(Run(nan, 2, "DSETP.NAN.AND P0, P1, R0, R2, PT;") == 0x0f);
    REQUIRE(Run(nan, nan, "DSETP.NAN.AND P0, P1, R0, R2, PT;") == 0x0f);
}
