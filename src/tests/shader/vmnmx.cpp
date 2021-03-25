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
