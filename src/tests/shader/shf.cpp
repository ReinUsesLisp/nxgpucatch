#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

static uint32_t Run(uint64_t base, uint32_t shift, std::string code) {
    return EvalUtil::Run(".dksh compute\n"
                         "main:\n"
                         "MOV R0, c[0x0][0x140];\n"
                         "MOV R1, c[0x0][0x144];\n"
                         "MOV R2, c[2][0];\n"
                         "MOV R4, c[2][4];\n"
                         "MOV R3, c[2][8];\n" +
                             code +
                             "STG.E [R0], R2;\n"
                             "EXIT;\n",
                         std::array{uint32_t(base), uint32_t(base >> 32), shift});
}

TEST_CASE("SHF Simple", "[shader]") {
    REQUIRE(Run(0x0cc0'0dd0'0ee0'0ff0, 4, "SHF.R R2, R2, R3, R4;") == 0x00ee'00ff);
    REQUIRE(Run(0x0cc0'0dd0'0ee0'0ff0, 24, "SHF.R R2, R2, R3, R4;") == 0xc00d'd00e);
    REQUIRE(Run(0x0cc0'0dd0'0ee0'0ff0, 40, "SHF.R.U64 R2, R2, R3, R4;") == 0x000c'c00d);
    REQUIRE(Run(0x0cc0'0dd0'0ee0'0ff0, 48, "SHF.R.U64 R2, R2, R3, R4;") == 0x0cc0);
    REQUIRE(Run(0xcc00'0dd0'0ee0'0ff0, 48, "SHF.R.S64 R2, R2, R3, R4;") == 0xffff'cc00);
    REQUIRE(Run(0xcc00'0dd0'0ee0'0ff0, 48, "SHF.R.U64 R2, R2, R3, R4;") == 0x0000'cc00);

    REQUIRE(Run(0x0cc0'0dd0'0ee0'0ff0, 4, "SHF.L R2, R2, R3, R4;") == 0xcc00'dd00);
    REQUIRE(Run(0x0cc0'0dd0'0ee0'0ff0, 24, "SHF.L R2, R2, R3, R4;") == 0xd00e'e00f);
    REQUIRE(Run(0x0cc0'0dd0'0ee0'0ff0, 36, "SHF.L.U64 R2, R2, R3, R4;") == 0xee00'ff00);

    REQUIRE(Run(0x0cc0'0dd0'0ee0'0ff0, 4, "SHF.R R2, R2, 4, R4;") == 0x00ee'00ff);
}

TEST_CASE("SHF Undefined", "[shader][undefined]") {
    REQUIRE(Run(0x0cc0'0dd0'0ee0'0ff0, 40, "SHF.R R2, R2, R3, R4;") == 0x0cc0'0dd0);
    REQUIRE(Run(0x0cc0'0dd0'0ee0'0ff0, 40, "SHF.L R2, R2, R3, R4;") == 0x0ee0'0ff0);

    REQUIRE(Run(0x0cc0'0dd0'0ee0'0ff0, 40, "SHF.R.U64 R2, R2, R3, R4;") == 0xcc00d);
    REQUIRE(Run(0x0cc0'0dd0'0ee0'0ff0, 40, "SHF.L.U64 R2, R2, R3, R4;") == 0xe00ff000);

    REQUIRE(Run(0x0cc0'0dd0'0ee0'0ff0, 70, "SHF.R.U64 R2, R2, R3, R4;") == 0);
    REQUIRE(Run(0xcc00'0dd0'0ee0'0ff0, 70, "SHF.R.S64 R2, R2, R3, R4;") == 0xffff'ffff);
    REQUIRE(Run(0x0cc0'0dd0'0ee0'0ff0, 70, "SHF.L.U64 R2, R2, R3, R4;") == 0);

    REQUIRE(Run(0x0cc0'0dd0'0ee0'0ff0, 40, "SHF.R.W R2, R2, R3, R4;") == 0xd00ee00f);
    REQUIRE(Run(0x0cc0'0dd0'0ee0'0ff0, 40, "SHF.L.W R2, R2, R3, R4;") == 0xc00dd00e);

    REQUIRE(Run(0x0cc0'0dd0'0ee0'0ff0, 70, "SHF.R.W.U64 R2, R2, R3, R4;") == 0x403b803f);
    REQUIRE(Run(0xcc00'0dd0'0ee0'0ff0, 70, "SHF.R.W.S64 R2, R2, R3, R4;") == 0x403b803f);
    REQUIRE(Run(0x0cc0'0dd0'0ee0'0ff0, 70, "SHF.L.W.U64 R2, R2, R3, R4;") == 0x30037403);

    REQUIRE(Run(0x0cc0'0dd0'0ee0'0ff0, 110, "SHF.R.W.U64 R2, R2, R3, R4;") == 0x3300);
    REQUIRE(Run(0xcc00'0dd0'0ee0'0ff0, 110, "SHF.R.W.S64 R2, R2, R3, R4;") == 0xffff'3000);
    REQUIRE(Run(0x0cc0'0dd0'0ee0'0ff0, 110, "SHF.L.W.U64 R2, R2, R3, R4;") == 0x3fc0000);
}
