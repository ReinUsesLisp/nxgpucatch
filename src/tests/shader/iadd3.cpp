#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

static int32_t Run(uint32_t a, uint32_t b, uint32_t c, std::string code) {
    return int32_t(EvalUtil::Run(".dksh compute\n"
                                 "main:\n"
                                 "MOV R0, c[0x0][0x140];"
                                 "MOV R1, c[0x0][0x144];"
                                 "ISETP.F.AND P0, PT, RZ, RZ, PT;"
                                 "MOV R2, c[2][0];"
                                 "MOV R3, c[2][4];"
                                 "MOV R4, c[2][8];" +
                                     code +
                                     "STG.E [R0], R2;"
                                     "EXIT;",
                                 std::array{a, b, c}));
}

static auto FuzzRun(std::string code) {
    return std::array<uint32_t, 4>{
        static_cast<uint32_t>(Run(0xaaaabbbb, 0xdeadbeef, 0xcafecafe, code)),
        static_cast<uint32_t>(Run(0x00408023, 0xab303abd, 0x7ace930a, code)),
        static_cast<uint32_t>(Run(0x80008000, 0x34a050be, 0xaeae2040, code)),
        static_cast<uint32_t>(Run(0x5921acde, 0x52abd03e, 0xe3e3e3e3, code)),
    };
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

    REQUIRE(Run(0x40000, 0x50000, 6, "IADD3.RS R2, R2, R3, R4;") == 15);
    REQUIRE(Run(4, 5, 6, "IADD3.LS R2, R2, R3, R4;") == 0x90006);

    REQUIRE(Run(4, 5, 6, "IADD3 R2, R2, c[2][4], R4;") == 15);
    REQUIRE(Run(4, 5, 6, "IADD3 R2, R2, 5, R4;") == 15);
}

TEST_CASE("IADD3 CC", "[shader]") {
    REQUIRE(Run(0, 0, 0, "IADD3 RZ.CC, R2, R3, R4; P2R R2, CC, RZ, 0xff;") == 1); // Zero
    REQUIRE(Run(-2, 1, 1, "IADD3 RZ.CC, R2, R3, R4; P2R R2, CC, RZ, 0xff;") == 5); // Zero+Carry
    REQUIRE(Run(1, -1, 0, "IADD3 RZ.CC, R2, R3, R4; P2R R2, CC, RZ, 0xff;") == 9); // Zero+Overflow
    REQUIRE(Run(-1, 0, 0, "IADD3 RZ.CC, R2, R3, R4; P2R R2, CC, RZ, 0xff;") == 2); // Sign
    REQUIRE(Run(0, -1, 0, "IADD3 RZ.CC, R2, R3, R4; P2R R2, CC, RZ, 0xff;") == 2); // Sign
    REQUIRE(Run(0, 0, -1, "IADD3 RZ.CC, R2, R3, R4; P2R R2, CC, RZ, 0xff;") == 2); // Sign
    REQUIRE(Run(-1, 2, 0, "IADD3 RZ.CC, R2, R3, R4; P2R R2, CC, RZ, 0xff;") == 8); // Overflow
    REQUIRE(Run(1, -1, 1, "IADD3 RZ.CC, R2, R3, R4; P2R R2, CC, RZ, 0xff;") == 8); // Overflow
    REQUIRE(Run(0, 2, -1, "IADD3 RZ.CC, R2, R3, R4; P2R R2, CC, RZ, 0xff;") == 4); // Carry

    // Shifts
    REQUIRE(Run(2, 2, 0, "IADD3.RS RZ.CC, R2, R3, R4; P2R R2, CC, RZ, 0xff;") == 1); // Zero
    REQUIRE(Run(0x8000, 2, 1, "IADD3.LS RZ.CC, R2, R3, R4; P2R R2, CC, RZ, 0xff;") == 2); // Sign
}

TEST_CASE("IADD3 33-bit", "[shader]") {
    REQUIRE(Run(0xffff'ffff, 1, 0, "IADD3.RS R2, R2, R3, R4;") == 0x10000);
    REQUIRE(Run(0xffff'ffff, 0, 0, "IADD3.RS R2, R2, R3, R4;") == 0x0ffff);
    REQUIRE(Run(0xffff'ffff, 0xffff'ffff, 0, "IADD3.RS R2, R2, R3, R4;") == 0x1ffff);
}

TEST_CASE("IADD3 198X", "[shader]") {
    REQUIRE(FuzzRun("IADD3.LS R2, R2, R3, R4;") == std::array<uint32_t, 4>{
        0x45a8cafe, 0x35ae930a, 0x7f6c2040, 0x60ffe3e3,
    });
    REQUIRE(FuzzRun("IADD3 R2, -R2, c[2][0], R4;") == std::array<uint32_t, 4>{
        0xcafecafe, 0x7ace930a, 0xaeae2040, 0xe3e3e3e3,
    });
    REQUIRE(FuzzRun("IADD3 R2, -R2, c[2][4], R4;") == std::array<uint32_t, 4>{
        0xff01ce32, 0x25be4da4, 0x634df0fe, 0xdd6e0743,
    });
    REQUIRE(FuzzRun("IADD3 R2, -R2, c[2][8], R4;") == std::array<uint32_t, 4>{
        0xeb52da41, 0xf55ca5f1, 0xdd5bc080, 0x6ea61ae8,
    });
    REQUIRE(FuzzRun("IADD3 R2, -R2, 0xaee, R3;") == std::array<uint32_t, 4>{
        0x34030e22, 0xaaefc588, 0xb49fdbac, 0xf98a2e4e,
    });
}

TEST_CASE("IADD3 Age of Calamity", "[shader]") {
    REQUIRE(FuzzRun("IADD3 R2, R3, 0x40, R4;") == std::array<uint32_t, 4>{
        0xa9ac8a2d, 0x25fece07, 0xe34e713e, 0x368fb461,
    });
    REQUIRE(FuzzRun("IADD3.LS R2, R4, R3, R2;") == std::array<uint32_t, 4>{
        0x3497bbbb, 0xce078023, 0xf0fe8000, 0x0d42acde,
    });
    REQUIRE(FuzzRun("IADD3.RS R2, R4, R3, R2;") == std::array<uint32_t, 4>{
        0xaaac6567, 0x41a621, 0x8001634e, 0x5922e36d,
    });
    REQUIRE(FuzzRun("IADD3 R2, R3, c[2][0], R4;") == std::array<uint32_t, 4>{
        0x545745a8, 0x263f4dea, 0x634ef0fe, 0x8fb160ff,
    });
    REQUIRE(FuzzRun("@!P0 IADD3 R2, R4, 0xfff82, -R3;") == std::array<uint32_t, 4>{
        0xec510b91, 0xcf9e57cf, 0x7a0dcf04, 0x91381327,
    });
    REQUIRE(FuzzRun("@P0 IADD3 R2, R4, 0xfff82, -R3;") == std::array<uint32_t, 4>{
        0xaaaabbbb, 0x408023, 0x80008000, 0x5921acde,
    });
}
