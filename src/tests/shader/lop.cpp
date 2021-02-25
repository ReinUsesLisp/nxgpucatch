#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

static uint32_t Run(uint32_t value, std::string code) {
    return EvalUtil::Run(".dksh compute\n"
                         "main:\n"
                         "MOV R0, c[0x0][0x140];\n"
                         "MOV R1, c[0x0][0x144];\n"
                         "MOV R2, 0x0ff0\n;"
                         "MOV R3, 0x00ff\n;" +
                             code +
                             "STG.E [R0], R2;\n"
                             "EXIT;\n",
                         value);
}

TEST_CASE("LOP Simple", "[shader]") {
    REQUIRE(Run(0, "LOP.AND PT, R2, R2, R3;") == 0x00f0);
    REQUIRE(Run(0, "LOP.OR PT, R2, R2, R3;") == 0x0fff);
    REQUIRE(Run(0, "LOP.XOR PT, R2, R2, R3;") == 0x0f0f);
    REQUIRE(Run(0, "LOP.PASS_B PT, R2, R2, R3;") == 0x00ff);

    REQUIRE(Run(0, "LOP.PASS_B PT, R2, R2, 0xdead;") == 0xdead);
    REQUIRE(Run(0x40, "LOP.PASS_B PT, R2, R2, c[2][0];") == 0x40);
}

TEST_CASE("LOP Invert", "[shader]") {
    REQUIRE(Run(0, "LOP.AND PT, R2, R2, ~R3;") == 0x0f00);
    REQUIRE(Run(0, "LOP.AND PT, R2, ~R2, ~R3;") == 0xffff'f000);
    REQUIRE(Run(0, "LOP.AND PT, R2, ~R2, R3;") == 0x000f);
}

TEST_CASE("LOP Predicate", "[shader]") {
    REQUIRE(Run(0, "LOP.AND P0, RZ, RZ, RZ; @!P0 MOV R2, 1;") == 1);
    REQUIRE(Run(0, "LOP.AND.T P0, RZ, RZ, RZ; @P0 MOV R2, 2;") == 2);
    REQUIRE(Run(0, "LOP.AND.Z P0, RZ, RZ, RZ; @P0 MOV R2, 3;") == 3);
    REQUIRE(Run(0, "LOP.AND.NZ P0, RZ, R2, R2; @P0 MOV R2, 4;") == 4);
}
