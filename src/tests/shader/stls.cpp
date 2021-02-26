#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

// https://stackoverflow.com/questions/3418231/replace-part-of-a-string-with-another-string
static std::string ReplaceAll(std::string str, std::string_view from, std::string_view to) {
    size_t start_pos = 0;
    while ((start_pos = str.find(from, start_pos)) != std::string::npos) {
        str.replace(start_pos, from.length(), to);
        start_pos += to.length();
    }
    return str;
}

static uint32_t Run(std::string code, uint32_t value = 0) {
    code = ".dksh compute\n"
           ".shared_memory 16\n"
           ".local_memory 256\n"
           "main:\n"
           "MOV R4, c[0x0][0x140];\n"
           "MOV R5, c[0x0][0x144];\n" +
           code +
           "STG.E [R4], R0;\n"
           "EXIT;\n";
    const uint32_t local = EvalUtil::Run(code, value);

    code = ReplaceAll(code, "STL", "STS");
    code = ReplaceAll(code, "LDL", "LDS");
    const uint32_t shared = EvalUtil::Run(code, value);

    REQUIRE(local == shared);
    return local;
}

TEST_CASE("STL LDL STS LDS Simple", "[shader]") {
    REQUIRE(Run(R"(
        MOV32I R1, 0xcafecafe;
        STL [RZ], R1;
        LDL R0, [RZ];
    )") == 0xcafecafe);

    REQUIRE(Run(R"(
        MOV32I R0, 0x11111111;
        MOV32I R1, 0x22222222;
        STL.64 [RZ], R0;
        MOV R0, RZ;
        MOV R1, RZ;
        LDL.64 R0, [RZ];
        IADD R0, R0, R1;
    )") == 0x33333333);

    REQUIRE(Run(R"(
        MOV32I R0, 0x11111111;
        MOV32I R1, 0x22222222;
        MOV32I R2, 0x44444444;
        MOV32I R3, 0x88888888;
        STL.128 [RZ], R0;
        MOV R0, RZ;
        MOV R1, RZ;
        MOV R2, RZ;
        MOV R3, RZ;
        LDL.128 R0, [RZ];
        IADD R0, R0, R1;
        IADD R0, R0, R2;
        IADD R0, R0, R3;
    )") == 0xffffffff);

    REQUIRE(Run(R"(
        MOV32I R1, 0xc000000e;
        STL.U16 [RZ], R1;
        LDL.U16 R0, [RZ];
    )") == 0xe);

    REQUIRE(Run(R"(
        MOV32I R1, 0xc000000e;
        STL.S16 [RZ], R1;
        LDL.S16 R0, [RZ];
    )") == 0xe);

    REQUIRE(Run(R"(
        MOV32I R1, 0xc000800e;
        STL.U16 [RZ], R1;
        LDL.U16 R0, [RZ];
    )") == 0x800e);

    REQUIRE(Run(R"(
        MOV32I R1, 0xc000800e;
        STL.S16 [RZ], R1;
        LDL.S16 R0, [RZ];
    )") == 0xffff'800e);

    REQUIRE(Run(R"(
        MOV32I R1, 0xc000002e;
        STL.U8 [RZ], R1;
        LDL.U8 R0, [RZ];
    )") == 0x2e);

    REQUIRE(Run(R"(
        MOV32I R1, 0xc000001e;
        STL.S8 [RZ], R1;
        LDL.S8 R0, [RZ];
    )") == 0x1e);

    REQUIRE(Run(R"(
        MOV32I R1, 0xc000008e;
        STL.U8 [RZ], R1;
        LDL.U8 R0, [RZ];
    )") == 0x8e);

    REQUIRE(Run(R"(
        MOV32I R1, 0xc000808e;
        STL.S8 [RZ], R1;
        LDL.S8 R0, [RZ];
    )") == 0xffff'ff8e);

    REQUIRE(Run(R"(
        MOV32I R1, 0xcafecafe;
        MOV R0, c[2][0];
        STL.U16 [R0], R1;
        LDL.U16 R0, [R0];
    )") == 0xcafe);

    REQUIRE(Run(R"(
        MOV32I R1, 0xcafecafe;
        MOV R0, c[2][0];
        STL.U16 [R0], R1;
        LDL.U16 R0, [R0];
    )", 6) == 0xcafe);

    REQUIRE(Run(R"(
        MOV32I R1, 0xcafecafe;
        MOV R0, c[2][0];
        STL [R0], R1;
        LDL R0, [R0];
    )", 8) == 0xcafecafe);

    REQUIRE(Run(R"(
        MOV32I R1, 0xcafecafe;
        MOV R0, c[2][0];
        STL.S8 [R0], R1;
        LDL.S8 R0, [R0];
    )", 3) == 0xfffffffe);

    REQUIRE(Run(R"(
        MOV32I R1, 0xcafecafe;
        MOV R0, c[2][0];
        STL.S8 [R0+2], R1;
        LDL.S8 R0, [R0+2];
    )", 3) == 0xfffffffe);

    REQUIRE(Run(R"(
        MOV32I R1, 0xcafecafe;
        MOV R0, c[2][0];
        STL.S8 [R0-1], R1;
        LDL.S8 R0, [R0-1];
    )", 3) == 0xfffffffe);
}
