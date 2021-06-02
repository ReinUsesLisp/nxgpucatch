#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

static uint32_t Run(uint32_t base, int32_t offset, int32_t offset_hi, std::string code) {
    return EvalUtil::Run(".dksh compute\n"
                         "main:\n"
                         "MOV R0, c[0x0][0x140];\n"
                         "MOV R1, c[0x0][0x144];\n"
                         "MOV R2, c[2][0];\n"
                         "MOV R3, c[2][4];\n"
                         "MOV R4, c[2][8];\n" +
                             code +
                             "STG.E [R0], R2;\n"
                             "EXIT;\n",
                         std::array{uint32_t(offset), base, uint32_t(offset_hi)});
}

static uint32_t Run(uint32_t base, int32_t offset, std::string code) {
    return Run(base, offset, 0, std::move(code));
}

TEST_CASE("LEA Simple", "[shader]") {
    REQUIRE(Run(32, 1, "LEA.LO R2, R2, R3, 2;") == 36);
    REQUIRE(Run(32, -1, "LEA.LO R2, R2, R3, 2;") == 28);
    REQUIRE(Run(32, 1, "LEA.LO R2, -R2, R3, 2;") == 28);
    REQUIRE(Run(32, 1, "LEA.LO R2, -R2, R3, 3;") == 24);

    REQUIRE(Run(32, 1, "LEA.LO R2, R2, c[2][4], 2;") == 36);
    REQUIRE(Run(32, 1, "LEA.LO R2, R2, 1, 2;") == 5);
    REQUIRE(Run(32, 1, "LEA.LO R2, R2, -1, 2;") == 3);

    REQUIRE(Run(32, 1, 0, "LEA.HI R2, R2, R3, R4, 2;") == 32);
    REQUIRE(Run(32, 0xffff'ffff, 0, "LEA.HI R2, R2, R3, R4, 1;") == 33);
    REQUIRE(Run(32, 0xffff'ffff, 2, "LEA.HI R2, R2, R3, R4, 1;") == 37);
    REQUIRE(Run(32, 0xffff'ffff, 2, "LEA.HI R2, R2, c[2][4], R4, 1;") == 37);
    REQUIRE(Run(32, 0x0000'0000, 2, "LEA.HI R2, R2, c[2][4], R4, 1;") == 36);
    REQUIRE(Run(32, 0x0000'0000, 2, "LEA.HI R2, R2, c[2][4], R4, 0;") == 34);
    REQUIRE(Run(32, 0x0000'0000, -1, "LEA.HI R2, R2, c[2][4], R4, 0;") == 31);

    REQUIRE(Run(0, 0xdead, "LEA.HI R2, R2, R3, RZ, 0x1e;") == 0x37ab);
    REQUIRE(Run(0, 0xcafe, "LEA.HI R2, R2, R3, RZ, 0x1e;") == 0x32bf);
    REQUIRE(Run(0xaaa3, 0xcafe, "LEA.HI R2, R2, R3, RZ, 0x1e;") == 0xdd62);

    REQUIRE(Run(0xffffffff, 1, "LEA.LO R2, R2, R3, 0;") == 0);
    REQUIRE(Run(0xffffffff, 2, "LEA.LO R2, R2, R3, 0;") == 1);

    REQUIRE(Run(0xffff'ffff, 0xffff'ffff, 2, "LEA.HI R2, R2, R3, R4, 1;") == 4);
}

TEST_CASE("LEA Keoi Tecmo", "[shader]") {
    struct Entry {
        uint32_t input_a;
        uint32_t input_b;
        uint32_t output;
    };
    static constexpr Entry entries[]{
        {0xacdab923, 0xdea1394a, 0xe4830775},
        {0xff21a924, 0x012a9cde, 0xff6c505b},
        {0x220314a8, 0xdeadbeef, 0x59ae8463},
        {0xbea81324, 0xcafecafe, 0xf167c5e3},
    };
    for (const Entry& entry : entries) {
        REQUIRE(Run(entry.input_a, entry.input_b, "LEA.HI R2, R2, R3, RZ, 0x1e;") == entry.output);
    }
}
