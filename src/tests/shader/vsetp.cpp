#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

static int32_t FuzzRun(std::string code, uint32_t value = 0) {
    return EvalUtil::Run(".dksh compute\n"
                         "main:\n"
                         "MOV32I R2, 0x12345688;\n"
                         "MOV32I R3, 0x8a24c993;\n"
                         "MOV32I R4, 0x9123064a;\n" +
                             code +
                             "MOV R4, c[0x0][0x140];\n"
                             "MOV R5, c[0x0][0x144];\n"
                             "SEL R0, RZ, -1, !P3;"
                             "STG.E [R4], R0;\n"
                             "EXIT;\n",
                         value);
}

TEST_CASE("VSETP Simple", "[shader]") {
    REQUIRE(FuzzRun("VSETP.T.U8.U32.AND P3, PT, R2.B2, RZ, PT;") == -1);
    REQUIRE(FuzzRun("VSETP.F.U8.U32.AND P3, PT, R2.B2, RZ, PT;") == 0);
    REQUIRE(FuzzRun("VSETP.LE.U8.U32.AND P3, PT, R2.B2, RZ, PT;") == 0);
    REQUIRE(FuzzRun("VSETP.GT.U8.U32.AND P3, PT, R2.B2, RZ, PT;") == -1);
    REQUIRE(FuzzRun("VSETP.NE.U8.U32.AND P3, PT, R2.B2, RZ, PT;") == -1);
    REQUIRE(FuzzRun("MOV32I R3, 0x34; VSETP.EQ.U8.U32.AND P3, PT, R2.B2, R3, PT;") == -1);
    REQUIRE(FuzzRun("MOV32I R3, 0x56; VSETP.EQ.U8.U32.AND P3, PT, R2.B1, R3, PT;") == -1);
    REQUIRE(FuzzRun("MOV32I R3, 0x88; VSETP.EQ.U8.U32.AND P3, PT, R2.B0, R3, PT;") == -1);
    REQUIRE(FuzzRun("MOV32I R3, 0x12; VSETP.EQ.U8.U32.AND P3, PT, R2.B3, R3, PT;") == -1);
}
