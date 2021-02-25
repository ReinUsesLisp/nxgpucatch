#include <catch2/catch_test_macros.hpp>

#include "bit_cast.h"
#include "eval_util.h"

static int32_t Run(float value, std::string code) {
    return int32_t(EvalUtil::Run(".dksh compute\n"
                                 "main:\n"
                                 "MOV R0, c[0x0][0x140];\n"
                                 "MOV R1, c[0x0][0x144];\n"
                                 "MOV R2, c[2][0]\n;"
                                 "MOV R3, -1;\n" +
                                     code +
                                     "STG.E [R0], R2;\n"
                                     "EXIT;\n",
                                 value));
}

TEST_CASE("FCMP Simple", "[shader]") {
    REQUIRE(Run(1.0f, "FCMP.F.FTZ R2, R3, RZ, R2;") == 0);
    REQUIRE(Run(1.0f, "FCMP.T.FTZ R2, R3, RZ, R2;") == -1);

    REQUIRE(Run(-1.0f, "FCMP.LT.FTZ R2, R3, RZ, c[2][0];") == -1);
    REQUIRE(Run(1.0f, "FCMP.LT.FTZ R2, R3, c[2][0], R2;") == 0x3f800000);
    REQUIRE(Run(1.0f, "FCMP.LT.FTZ R2, R3, 1, R2;") == 0x3f800000);

    REQUIRE(Run(-1.0f, "FCMP.LT.FTZ R2, R3, RZ, R2;") == -1);
    REQUIRE(Run(0.0f, "FCMP.EQ.FTZ R2, R3, RZ, R2;") == -1);
    REQUIRE(Run(1.0f, "FCMP.NE.FTZ R2, R3, RZ, R2;") == -1);
    REQUIRE(Run(-1.0f, "FCMP.LE.FTZ R2, R3, RZ, R2;") == -1);
    REQUIRE(Run(1.0f, "FCMP.GT.FTZ R2, R3, RZ, R2;") == -1);
    REQUIRE(Run(0.0f, "FCMP.GE.FTZ R2, R3, RZ, R2;") == -1);
    REQUIRE(Run(0.0f, "FCMP.GE.FTZ R2, R3, RZ, R2;") == -1);
}

TEST_CASE("FCMP NAN", "[shader]") {
    static constexpr float nan{NAN};

    REQUIRE(Run(1.0f, "FCMP.NUM.FTZ R2, R3, RZ, R2;") == -1);
    REQUIRE(Run(nan, "FCMP.NUM.FTZ R2, R3, RZ, R2;") == 0);

    REQUIRE(Run(nan, "FCMP.F.FTZ R2, R3, RZ, R2;") == 0);
    REQUIRE(Run(nan, "FCMP.T.FTZ R2, R3, RZ, R2;") == -1);

    REQUIRE(Run(nan, "FCMP.LT.FTZ R2, R3, RZ, R2;") == 0);
    REQUIRE(Run(nan, "FCMP.LE.FTZ R2, R3, RZ, R2;") == 0);
    REQUIRE(Run(nan, "FCMP.GT.FTZ R2, R3, RZ, R2;") == 0);
    REQUIRE(Run(nan, "FCMP.GE.FTZ R2, R3, RZ, R2;") == 0);
    REQUIRE(Run(nan, "FCMP.EQ.FTZ R2, R3, RZ, R2;") == 0);
    REQUIRE(Run(nan, "FCMP.NE.FTZ R2, R3, RZ, R2;") == 0);
    REQUIRE(Run(nan, "FCMP.NAN.FTZ R2, R3, RZ, R2;") == -1);
    REQUIRE(Run(nan, "FCMP.LTU.FTZ R2, R3, RZ, R2;") == -1);
    REQUIRE(Run(nan, "FCMP.EQU.FTZ R2, R3, RZ, R2;") == -1);
    REQUIRE(Run(nan, "FCMP.LEU.FTZ R2, R3, RZ, R2;") == -1);
    REQUIRE(Run(nan, "FCMP.GTU.FTZ R2, R3, RZ, R2;") == -1);
    REQUIRE(Run(nan, "FCMP.NEU.FTZ R2, R3, RZ, R2;") == -1);
    REQUIRE(Run(nan, "FCMP.GEU.FTZ R2, R3, RZ, R2;") == -1);
}

TEST_CASE("FCMP Denorm", "[shader][fpcontrol]") {
    static constexpr float denorm{1e-40};
    REQUIRE(Run(denorm, "FCMP.GT.FTZ R2, R3, RZ, R2;") == 0);
    REQUIRE(Run(denorm, "FCMP.GT R2, R3, RZ, R2;") == -1);
}
