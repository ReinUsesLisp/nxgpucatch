#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

static uint32_t FuzzRun(std::string code, uint32_t value = 0) {
    return EvalUtil::Run(".dksh compute\n"
                         "main:\n"
                         "MOV32I R2, 0x12345688;\n"
                         "MOV32I R3, 0x8a24c993;\n"
                         "MOV32I R4, 0x9123064a;\n" +
                             code +
                             "MOV R4, c[0x0][0x140];\n"
                             "MOV R5, c[0x0][0x144];\n"
                             "STG.E [R4], R0;\n"
                             "EXIT;\n",
                         value);
}

TEST_CASE("VMNMX Simple", "[shader]") {
    REQUIRE(FuzzRun("VMNMX.UD.U32.U32.MN.MAX R0, R2, R3, R4;") == 0x9123064a);
    REQUIRE(FuzzRun("VMNMX.UD.U32.U32.MN.MIN R0, R2, R3, R4;") == 0x12345688);
    REQUIRE(FuzzRun("VMNMX.UD.U32.U32.MX.MIN R0, R2, R3, R4;") == 0x8a24c993);

    REQUIRE(FuzzRun("VMNMX.UD.S32.U32.MN.MAX R0, R2, R3, R4;") == 0x9123064a);
    REQUIRE(FuzzRun("VMNMX.UD.S32.S32.MN.MIN R0, R2, R3, R4;") == 0x8a24c993);
    REQUIRE(FuzzRun("VMNMX.UD.U32.S32.MX.MIN R0, R2, R3, R4;") == 0x12345688);

    REQUIRE(FuzzRun("VMNMX.SD.U32.U32.MN.MAX R0, R2, R3, R4;") == 0x12345688);
    REQUIRE(FuzzRun("VMNMX.SD.U32.U32.MN.MIN R0, R2, R3, R4;") == 0x9123064a);
    REQUIRE(FuzzRun("VMNMX.SD.U32.U32.MX.MIN R0, R2, R3, R4;") == 0x8a24c993);

    REQUIRE(FuzzRun("VMNMX.SD.S32.U32.MN.MAX R0, R2, R3, R4;") == 0x12345688);
    REQUIRE(FuzzRun("VMNMX.SD.S32.S32.MN.MIN R0, R2, R3, R4;") == 0x8a24c993);
    REQUIRE(FuzzRun("VMNMX.SD.U32.S32.MX.MIN R0, R2, R3, R4;") == 0x9123064a);

    REQUIRE(FuzzRun("VMNMX.UD.U32.U16.MN.MIN R0, R2, 0xdead, R4;") == 0xdead);
    REQUIRE(FuzzRun("VMNMX.SD.U32.S16.MN.MIN R0, R2, 0xdead, RZ;") == 0xffffdead);
}

TEST_CASE("VMNMX Selector Src B", "[shader]") {
    REQUIRE(FuzzRun("VMNMX.UD.U32.U16.MN.MAX R0, R2, R3.H0, R4;") == 0x9123064a);
    REQUIRE(FuzzRun("VMNMX.UD.U32.U16.MN.MAX R0, R2, R3.H1, R4;") == 0x9123064a);
    REQUIRE(FuzzRun("VMNMX.UD.U32.U8.MN.MAX R0, R2, R3.B0, R4;") == 0x9123064a);
    REQUIRE(FuzzRun("VMNMX.UD.U32.U8.MN.MAX R0, R2, R3.B1, R4;") == 0x9123064a);
    REQUIRE(FuzzRun("VMNMX.UD.U32.U8.MN.MAX R0, R2, R3.B2, R4;") == 0x9123064a);
    REQUIRE(FuzzRun("VMNMX.UD.U32.U8.MN.MAX R0, R2, R3.B3, R4;") == 0x9123064a);

    REQUIRE(FuzzRun("VMNMX.UD.U32.U16.MN.MIN R0, R2, R3.H0, R4;") == 0xc993);
    REQUIRE(FuzzRun("VMNMX.UD.U32.U16.MN.MIN R0, R2, R3.H1, R4;") == 0x8a24);
    REQUIRE(FuzzRun("VMNMX.UD.U32.U8.MN.MIN R0, R2, R3.B0, R4;") == 0x93);
    REQUIRE(FuzzRun("VMNMX.UD.U32.U8.MN.MIN R0, R2, R3.B1, R4;") == 0xc9);
    REQUIRE(FuzzRun("VMNMX.UD.U32.U8.MN.MIN R0, R2, R3.B2, R4;") == 0x24);
    REQUIRE(FuzzRun("VMNMX.UD.U32.U8.MN.MIN R0, R2, R3.B3, R4;") == 0x8a);
}

TEST_CASE("VMNMX Selector Src A", "[shader]") {
    REQUIRE(FuzzRun("VMNMX.UD.U16.U32.MN.MAX R0, R2.H0, R3, R4;") == 0x9123064a);
    REQUIRE(FuzzRun("VMNMX.UD.U16.U32.MN.MAX R0, R2.H1, R3, R4;") == 0x9123064a);
    REQUIRE(FuzzRun("VMNMX.UD.U8.U32.MN.MAX R0, R2.B0, R3, R4;") == 0x9123064a);
    REQUIRE(FuzzRun("VMNMX.UD.U8.U32.MN.MAX R0, R2.B1, R3, R4;") == 0x9123064a);
    REQUIRE(FuzzRun("VMNMX.UD.U8.U32.MN.MAX R0, R2.B2, R3, R4;") == 0x9123064a);
    REQUIRE(FuzzRun("VMNMX.UD.U8.U32.MN.MAX R0, R2.B3, R3, R4;") == 0x9123064a);
    
    REQUIRE(FuzzRun("VMNMX.UD.U16.U32.MN.MIN R0, R2.H0, R3, R4;") == 0x5688);
    REQUIRE(FuzzRun("VMNMX.UD.U16.U32.MN.MIN R0, R2.H1, R3, R4;") == 0x1234);
    REQUIRE(FuzzRun("VMNMX.UD.U8.U32.MN.MIN R0, R2.B0, R3, R4;") == 0x88);
    REQUIRE(FuzzRun("VMNMX.UD.U8.U32.MN.MIN R0, R2.B1, R3, R4;") == 0x56);
    REQUIRE(FuzzRun("VMNMX.UD.U8.U32.MN.MIN R0, R2.B2, R3, R4;") == 0x34);
    REQUIRE(FuzzRun("VMNMX.UD.U8.U32.MN.MIN R0, R2.B3, R3, R4;") == 0x12);
}
