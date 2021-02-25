#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

static int32_t Run(uint32_t a, uint32_t b, uint32_t c, std::string code) {
    return int32_t(EvalUtil::Run(".dksh compute\n"
                                 "main:\n"
                                 "MOV R0, c[0x0][0x140];\n"
                                 "MOV R1, c[0x0][0x144];\n"
                                 "MOV R2, c[2][0]\n;"
                                 "MOV R3, c[2][4]\n;"
                                 "MOV R4, c[2][8]\n;" +
                                     code +
                                     "STG.E [R0], R2;\n"
                                     "EXIT;\n",
                                 std::array{a, b, c}));
}

TEST_CASE("IADD3 Simple", "[shader]") {
    REQUIRE(Run(4, 5, 6, "IADD3 R2, R2, R3, R4;") == 15);
    REQUIRE(Run(4, 5, 6, "IADD3 R2, -R2, R3, R4;") == 7);
    REQUIRE(Run(4, 5, 6, "IADD3 R2, R2, -R3, R4;") == 5);
    REQUIRE(Run(4, 5, 6, "IADD3 R2, R2, R3, -R4;") == 3);
    REQUIRE(Run(4, 5, 6, "IADD3 R2, R2, -R3, -R4;") == -7);
    REQUIRE(Run(4, 5, 6, "IADD3 R2, -R2, R3, -R4;") == -5);

    REQUIRE(Run(0x0040009, 0x00020003, 0x00010008, "IADD3 R2, R2, R3, R4;") == 0x70014);
    REQUIRE(Run(0x0040009, 0x00020003, 0x00010008, "IADD3 R2, R2.H0, R3, R4;") == 0x30014);
    REQUIRE(Run(0x0040009, 0x00020003, 0x00010008, "IADD3 R2, R2, R3.H0, R4;") == 0x50014);
    REQUIRE(Run(0x0040009, 0x00020003, 0x00010008, "IADD3 R2, R2, R3, R4.H0;") == 0x60014);
    REQUIRE(Run(0x0040009, 0x00020003, 0x00010008, "IADD3 R2, R2.H1, R3, R4;") == 0x3000f);
    REQUIRE(Run(0x0040009, 0x00020003, 0x00010008, "IADD3 R2, R2, R3.H1, R4;") == 0x50013);
    REQUIRE(Run(0x0040009, 0x00020003, 0x00010008, "IADD3 R2, R2, R3, R4.H1;") == 0x6000d);
    REQUIRE(Run(0x0040009, 0x00020003, 0x00010008, "IADD3 R2, R2.H1, R3.H0, R4;") == 0x1000f);
    REQUIRE(Run(0x0040009, 0x00020003, 0x00010008, "IADD3 R2, R2, R3, -R4.H1;") == 0x6000b);

    REQUIRE(Run(0x40000, 0x50000, 6, "IADD3.LS R2, R2, R3, R4;") == 15);
    REQUIRE(Run(4, 5, 6, "IADD3.RS R2, R2, R3, R4;") == 0x90006);

    REQUIRE(Run(4, 5, 6, "IADD3 R2, R2, c[2][4], R4;") == 15);
    REQUIRE(Run(4, 5, 6, "IADD3 R2, R2, 5, R4;") == 15);
}
