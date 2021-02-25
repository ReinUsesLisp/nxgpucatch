#include <catch2/catch_test_macros.hpp>

#include "bit_cast.h"
#include "eval_util.h"

static int32_t Run(float a, float b, std::string code) {
    return int32_t(EvalUtil::Run(".dksh compute\n"
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

TEST_CASE("FSET Simple", "[shader]") {
    REQUIRE(Run(2.0f, 3.0f, "FSET.F.FTZ.AND R2, R2, R3, PT;") == 0);
    REQUIRE(Run(2.0f, 3.0f, "FSET.LT.FTZ.AND R2, R2, R3, PT;") == -1);
    REQUIRE(Run(3.0f, 3.0f, "FSET.LE.FTZ.AND R2, R2, R3, PT;") == -1);
    REQUIRE(Run(3.0f, 3.0f, "FSET.EQ.FTZ.AND R2, R2, R3, PT;") == -1);
    REQUIRE(Run(2.0f, 3.0f, "FSET.NE.FTZ.AND R2, R2, R3, PT;") == -1);
    REQUIRE(Run(3.0f, 2.0f, "FSET.GT.FTZ.AND R2, R2, R3, PT;") == -1);
    REQUIRE(Run(3.0f, 3.0f, "FSET.GE.FTZ.AND R2, R2, R3, PT;") == -1);
    REQUIRE(Run(3.0f, 2.0f, "FSET.T.FTZ.AND R2, R2, R3, PT;") == -1);

    REQUIRE(Run(3.0f, 3.0f, "FSET.EQ.FTZ.AND R2, R2, 3, PT;") == -1);
    REQUIRE(Run(3.0f, 3.0f, "FSET.EQ.FTZ.AND R2, R2, c[2][4], PT;") == -1);

    REQUIRE(Run(3.0f, 3.0f, "FSET.BF.EQ.FTZ.AND R2, R2, c[2][4], PT;") == 0x3f800000);
}

TEST_CASE("FSET Combining", "[shader]") {
    REQUIRE(Run(2.0f, 3.0f, "FSET.F.FTZ.OR R2, R2, R3, PT;") == -1);
    REQUIRE(Run(2.0f, 3.0f, "FSET.F.FTZ.XOR R2, R2, R3, PT;") == -1);
}

TEST_CASE("FSET NAN", "[shader]") {
    static constexpr float nan{NAN};
    static constexpr float inf{INFINITY};

    REQUIRE(Run(2.0f, nan, "FSET.LT.FTZ.AND R2, R2, R3, PT;") == 0);
    REQUIRE(Run(3.0f, nan, "FSET.LE.FTZ.AND R2, R2, R3, PT;") == 0);
    REQUIRE(Run(3.0f, nan, "FSET.EQ.FTZ.AND R2, R2, R3, PT;") == 0);
    REQUIRE(Run(nan, nan, "FSET.EQ.FTZ.AND R2, R2, R3, PT;") == 0);
    REQUIRE(Run(2.0f, nan, "FSET.NE.FTZ.AND R2, R2, R3, PT;") == 0);
    REQUIRE(Run(3.0f, nan, "FSET.GT.FTZ.AND R2, R2, R3, PT;") == 0);
    REQUIRE(Run(3.0f, nan, "FSET.GE.FTZ.AND R2, R2, R3, PT;") == 0);
    REQUIRE(Run(3.0f, nan, "FSET.NUM.FTZ.AND R2, R2, R3, PT;") == 0);
    REQUIRE(Run(nan, nan, "FSET.NUM.FTZ.AND R2, R2, R3, PT;") == 0);
    REQUIRE(Run(3.0f, 3.0f, "FSET.NUM.FTZ.AND R2, R2, R3, PT;") == -1);
    REQUIRE(Run(3.0f, nan, "FSET.NAN.FTZ.AND R2, R2, R3, PT;") == -1);
    REQUIRE(Run(nan, nan, "FSET.NAN.FTZ.AND R2, R2, R3, PT;") == -1);

    REQUIRE(Run(2.0f, nan, "FSET.LTU.FTZ.AND R2, R2, R3, PT;") == -1);
    REQUIRE(Run(3.0f, nan, "FSET.LEU.FTZ.AND R2, R2, R3, PT;") == -1);
    REQUIRE(Run(3.0f, nan, "FSET.EQU.FTZ.AND R2, R2, R3, PT;") == -1);
    REQUIRE(Run(nan, nan, "FSET.EQU.FTZ.AND R2, R2, R3, PT;") == -1);
    REQUIRE(Run(2.0f, nan, "FSET.NEU.FTZ.AND R2, R2, R3, PT;") == -1);
    REQUIRE(Run(3.0f, nan, "FSET.GTU.FTZ.AND R2, R2, R3, PT;") == -1);
    REQUIRE(Run(3.0f, nan, "FSET.GEU.FTZ.AND R2, R2, R3, PT;") == -1);

    REQUIRE(Run(2.0f, 3.0f, "FSET.LTU.FTZ.AND R2, R2, R3, PT;") == -1);
    REQUIRE(Run(3.0f, 3.0f, "FSET.LEU.FTZ.AND R2, R2, R3, PT;") == -1);
    REQUIRE(Run(3.0f, 3.0f, "FSET.EQU.FTZ.AND R2, R2, R3, PT;") == -1);
    REQUIRE(Run(2.0f, 3.0f, "FSET.NEU.FTZ.AND R2, R2, R3, PT;") == -1);
    REQUIRE(Run(3.0f, 2.0f, "FSET.GTU.FTZ.AND R2, R2, R3, PT;") == -1);
    REQUIRE(Run(3.0f, 3.0f, "FSET.GEU.FTZ.AND R2, R2, R3, PT;") == -1);

    REQUIRE(Run(inf, nan, "FSET.LT.FTZ.AND R2, R2, R3, PT;") == 0);
    REQUIRE(Run(0.0f, inf, "FSET.LT.FTZ.AND R2, R2, R3, PT;") == -1);
    REQUIRE(Run(0.0f, inf, "FSET.LTU.FTZ.AND R2, R2, R3, PT;") == -1);
    REQUIRE(Run(inf, inf, "FSET.LE.FTZ.AND R2, R2, R3, PT;") == -1);
    REQUIRE(Run(inf, inf, "FSET.EQ.FTZ.AND R2, R2, R3, PT;") == -1);
    REQUIRE(Run(inf, inf, "FSET.LT.FTZ.AND R2, R2, R3, PT;") == 0);
    REQUIRE(Run(-inf, 0.0f, "FSET.LT.FTZ.AND R2, R2, R3, PT;") == -1);
    REQUIRE(Run(-inf, inf, "FSET.LT.FTZ.AND R2, R2, R3, PT;") == -1);
}

TEST_CASE("FSET Denorm", "[shader][fpcontrol]") {
    static constexpr float denorm{1e-40};
    REQUIRE(Run(denorm, 0.0f, "FSET.GT.FTZ.AND R2, R2, R3, PT;") == 0);
    REQUIRE(Run(denorm, 0.0f, "FSET.GT.AND R2, R2, R3, PT;") == -1);
}
