#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

static uint32_t Run(int32_t value, std::string code) {
    return EvalUtil::Run(".dksh compute\n"
                         "main:\n"
                         "MOV R0, c[0x0][0x140];\n"
                         "MOV R1, c[0x0][0x144];\n"
                         "MOV R2, c[2][8];\n"
                         "MOV R3, c[2][4];\n"
                         "MOV R4, c[2][0];\n" +
                             code +
                             "STG.E [R0], R2;\n"
                             "EXIT;\n",
                         std::array{value, 0, 1, 0});
}

TEST_CASE("ICMP Simple", "[shader]") {
    REQUIRE(Run(0, "ICMP.F R2, R2, R3, R4;") == 0);
    REQUIRE(Run(-2, "ICMP.LT R2, R2, R3, R4;") == 1);
    REQUIRE(Run(0, "ICMP.EQ R2, R2, R3, R4;") == 1);
    REQUIRE(Run(0, "ICMP.LE R2, R2, R3, R4;") == 1);
    REQUIRE(Run(-1, "ICMP.LE R2, R2, R3, R4;") == 1);
    REQUIRE(Run(1, "ICMP.GT R2, R2, R3, R4;") == 1);
    REQUIRE(Run(0, "ICMP.GT R2, R2, R3, R4;") == 0);
    REQUIRE(Run(1, "ICMP.GE R2, R2, R3, R4;") == 1);
    REQUIRE(Run(0, "ICMP.GE R2, R2, R3, R4;") == 1);
    REQUIRE(Run(-1, "ICMP.GE R2, R2, R3, R4;") == 0);
    REQUIRE(Run(-1, "ICMP.NE R2, R2, R3, R4;") == 1);
    REQUIRE(Run(0, "ICMP.NE R2, R2, R3, R4;") == 0);
    REQUIRE(Run(0, "ICMP.T R2, R2, R3, R4;") == 1);
    REQUIRE(Run(1, "ICMP.T R2, R2, R3, R4;") == 1);
    REQUIRE(Run(-1, "ICMP.T R2, R2, R3, R4;") == 1);

    REQUIRE(Run(-1, "ICMP.GT.U32 R2, R2, R3, R4;") == 1);

    REQUIRE(Run(-1, "ICMP.GT.U32 R2, R2, R3, c[2][0];") == 1);
    REQUIRE(Run(-1, "ICMP.GT.U32 R2, R2, c[2][8], R4;") == 1);
    REQUIRE(Run(-1, "ICMP.GT.U32 R2, R2, 0, R4;") == 1);
}
