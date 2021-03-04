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
                                      "MOV R2, c[0x0][0x140];"
                                      "MOV R3, c[0x0][0x144];"
                                      "STG.E [R2], R0;\n"
                                      "EXIT;\n",
                                  std::array{a, b});
}

TEST_CASE("DSET Simple", "[shader]") {
    REQUIRE(Run(1, 2, "DSET.F.AND R0, R0, R2, PT;") == 0);
    REQUIRE(Run(1, 2, "DSET.T.AND R0, R0, R2, PT;") == -1);

    REQUIRE(Run(2, 2, "DSET.LT.AND R0, R0, R2, PT;") == 0);
    REQUIRE(Run(1, 2, "DSET.LT.AND R0, R0, R2, PT;") == -1);
    REQUIRE(Run(1, 2, "DSET.EQ.AND R0, R0, R2, PT;") == 0);
    REQUIRE(Run(2, 2, "DSET.EQ.AND R0, R0, R2, PT;") == -1);
    REQUIRE(Run(2, 2, "DSET.LE.AND R0, R0, R2, PT;") == -1);
    REQUIRE(Run(2, 3, "DSET.LE.AND R0, R0, R2, PT;") == -1);
    REQUIRE(Run(3, 2, "DSET.LE.AND R0, R0, R2, PT;") == 0);
    REQUIRE(Run(2, 2, "DSET.GT.AND R0, R0, R2, PT;") == 0);
    REQUIRE(Run(3, 2, "DSET.GT.AND R0, R0, R2, PT;") == -1);
    REQUIRE(Run(2, 2, "DSET.NE.AND R0, R0, R2, PT;") == 0);
    REQUIRE(Run(2, 3, "DSET.NE.AND R0, R0, R2, PT;") == -1);
    REQUIRE(Run(2, 2, "DSET.GE.AND R0, R0, R2, PT;") == -1);
    REQUIRE(Run(3, 2, "DSET.GE.AND R0, R0, R2, PT;") == -1);
    REQUIRE(Run(2, 3, "DSET.GE.AND R0, R0, R2, PT;") == 0);

    REQUIRE(Run(2, 2, "DSET.LTU.AND R0, R0, R2, PT;") == 0);
    REQUIRE(Run(1, 2, "DSET.LTU.AND R0, R0, R2, PT;") == -1);
    REQUIRE(Run(1, 2, "DSET.EQU.AND R0, R0, R2, PT;") == 0);
    REQUIRE(Run(2, 2, "DSET.EQU.AND R0, R0, R2, PT;") == -1);
    REQUIRE(Run(2, 2, "DSET.LEU.AND R0, R0, R2, PT;") == -1);
    REQUIRE(Run(2, 3, "DSET.LEU.AND R0, R0, R2, PT;") == -1);
    REQUIRE(Run(3, 2, "DSET.LEU.AND R0, R0, R2, PT;") == 0);
    REQUIRE(Run(2, 2, "DSET.GTU.AND R0, R0, R2, PT;") == 0);
    REQUIRE(Run(3, 2, "DSET.GTU.AND R0, R0, R2, PT;") == -1);
    REQUIRE(Run(2, 2, "DSET.NEU.AND R0, R0, R2, PT;") == 0);
    REQUIRE(Run(2, 3, "DSET.NEU.AND R0, R0, R2, PT;") == -1);
    REQUIRE(Run(2, 2, "DSET.GEU.AND R0, R0, R2, PT;") == -1);
    REQUIRE(Run(3, 2, "DSET.GEU.AND R0, R0, R2, PT;") == -1);
    REQUIRE(Run(2, 3, "DSET.GEU.AND R0, R0, R2, PT;") == 0);

    REQUIRE(Run(2, 2, "DSET.LT.AND R0, R0, c[2][8], PT;") == 0);
    REQUIRE(Run(1, 2, "DSET.LT.AND R0, R0, c[2][8], PT;") == -1);
    REQUIRE(Run(1, 2, "DSET.EQ.AND R0, R0, c[2][8], PT;") == 0);
    REQUIRE(Run(2, 2, "DSET.EQ.AND R0, R0, c[2][8], PT;") == -1);
    REQUIRE(Run(2, 2, "DSET.LE.AND R0, R0, c[2][8], PT;") == -1);
    REQUIRE(Run(2, 3, "DSET.LE.AND R0, R0, c[2][8], PT;") == -1);
    REQUIRE(Run(3, 2, "DSET.LE.AND R0, R0, c[2][8], PT;") == 0);
    REQUIRE(Run(2, 2, "DSET.GT.AND R0, R0, c[2][8], PT;") == 0);
    REQUIRE(Run(3, 2, "DSET.GT.AND R0, R0, c[2][8], PT;") == -1);
    REQUIRE(Run(2, 2, "DSET.NE.AND R0, R0, c[2][8], PT;") == 0);
    REQUIRE(Run(2, 3, "DSET.NE.AND R0, R0, c[2][8], PT;") == -1);
    REQUIRE(Run(2, 2, "DSET.GE.AND R0, R0, c[2][8], PT;") == -1);
    REQUIRE(Run(3, 2, "DSET.GE.AND R0, R0, c[2][8], PT;") == -1);
    REQUIRE(Run(2, 3, "DSET.GE.AND R0, R0, c[2][8], PT;") == 0);

    REQUIRE(Run(2, 2, "DSET.LT.AND R0, R0, 2, PT;") == 0);
    REQUIRE(Run(1, 2, "DSET.LT.AND R0, R0, 2, PT;") == -1);
    REQUIRE(Run(1, 2, "DSET.EQ.AND R0, R0, 2, PT;") == 0);
    REQUIRE(Run(2, 2, "DSET.EQ.AND R0, R0, 2, PT;") == -1);
    REQUIRE(Run(2, 2, "DSET.LE.AND R0, R0, 2, PT;") == -1);
    REQUIRE(Run(2, 3, "DSET.LE.AND R0, R0, 3, PT;") == -1);
    REQUIRE(Run(3, 2, "DSET.LE.AND R0, R0, 2, PT;") == 0);
    REQUIRE(Run(2, 2, "DSET.GT.AND R0, R0, 2, PT;") == 0);
    REQUIRE(Run(3, 2, "DSET.GT.AND R0, R0, 2, PT;") == -1);
    REQUIRE(Run(2, 2, "DSET.NE.AND R0, R0, 2, PT;") == 0);
    REQUIRE(Run(2, 3, "DSET.NE.AND R0, R0, 3, PT;") == -1);
    REQUIRE(Run(2, 2, "DSET.GE.AND R0, R0, 2, PT;") == -1);
    REQUIRE(Run(3, 2, "DSET.GE.AND R0, R0, 2, PT;") == -1);
    REQUIRE(Run(2, 3, "DSET.GE.AND R0, R0, 3, PT;") == 0);

    REQUIRE(Run(2, 2, "DSET.LT.AND R0, -R0, R2, PT;") == -1);
    REQUIRE(Run(-3, 2, "DSET.GT.AND R0, |R0|, R2, PT;") == -1);
    REQUIRE(Run(2, 2, "DSET.GT.AND R0, R0, -R2, PT;") == -1);
    REQUIRE(Run(2, -3, "DSET.LT.AND R0, R0, |R2|, PT;") == -1);

    REQUIRE(Run(2, 2, "DSET.LT.OR R0, R0, R2, PT;") == -1);
    REQUIRE(Run(2, 2, "DSET.LT.XOR R0, R0, R2, PT;") == -1);

    REQUIRE(Run(2, 2, "DSET.BF.LT.OR R0, R0, R2, PT;") == 0x3f80'0000);
}

TEST_CASE("DSET NAN", "[shader]") {
    constexpr double nan = NAN;

    REQUIRE(Run(2, nan, "DSET.LT.AND R0, R0, R2, PT;") == 0);
    REQUIRE(Run(2, nan, "DSET.EQ.AND R0, R0, R2, PT;") == 0);
    REQUIRE(Run(2, nan, "DSET.LE.AND R0, R0, R2, PT;") == 0);
    REQUIRE(Run(2, nan, "DSET.GT.AND R0, R0, R2, PT;") == 0);
    REQUIRE(Run(2, nan, "DSET.NE.AND R0, R0, R2, PT;") == 0);
    REQUIRE(Run(2, nan, "DSET.GE.AND R0, R0, R2, PT;") == 0);

    REQUIRE(Run(2, nan, "DSET.LTU.AND R0, R0, R2, PT;") == -1);
    REQUIRE(Run(2, nan, "DSET.EQU.AND R0, R0, R2, PT;") == -1);
    REQUIRE(Run(2, nan, "DSET.LEU.AND R0, R0, R2, PT;") == -1);
    REQUIRE(Run(2, nan, "DSET.GTU.AND R0, R0, R2, PT;") == -1);
    REQUIRE(Run(2, nan, "DSET.NEU.AND R0, R0, R2, PT;") == -1);
    REQUIRE(Run(2, nan, "DSET.GEU.AND R0, R0, R2, PT;") == -1);

    REQUIRE(Run(2, 2, "DSET.NUM.AND R0, R0, R2, PT;") == -1);
    REQUIRE(Run(2, nan, "DSET.NUM.AND R0, R0, R2, PT;") == 0);
    REQUIRE(Run(nan, 2, "DSET.NUM.AND R0, R0, R2, PT;") == 0);
    REQUIRE(Run(nan, nan, "DSET.NUM.AND R0, R0, R2, PT;") == 0);

    REQUIRE(Run(2, 2, "DSET.NAN.AND R0, R0, R2, PT;") == 0);
    REQUIRE(Run(2, nan, "DSET.NAN.AND R0, R0, R2, PT;") == -1);
    REQUIRE(Run(nan, 2, "DSET.NAN.AND R0, R0, R2, PT;") == -1);
    REQUIRE(Run(nan, nan, "DSET.NAN.AND R0, R0, R2, PT;") == -1);
}
