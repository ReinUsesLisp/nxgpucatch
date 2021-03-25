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

TEST_CASE("VMAD Simple", "[shader]") {
    REQUIRE(FuzzRun("VMAD.U8.U8 R0, R2, R3, R4;") == 0x91235462);
    REQUIRE(FuzzRun("VMAD.S8.U8 R0, R2, R3, R4;") == 0x9122c162);
    REQUIRE(FuzzRun("VMAD.S8.S8 R0, R2, R3, R4;") == 0x91233962);
    REQUIRE(FuzzRun("VMAD.U8.S8 R0, R2, R3, R4;") == 0x9122cc62);

    REQUIRE(FuzzRun("VMAD.U8.U8 R0, R2.B1, R3,    R4;") == 0x912337ac);
    REQUIRE(FuzzRun("VMAD.S8.U8 R0, R2.B2, R3,    R4;") == 0x91232426);
    REQUIRE(FuzzRun("VMAD.S8.S8 R0, R2,    R3.B3, R4;") == 0x91233d9a);
    REQUIRE(FuzzRun("VMAD.U8.S8 R0, R2.B3, R3.B2, R4;") == 0x912308d2);

    REQUIRE(FuzzRun("VMAD.U16.U16 R0, R2, R3, R4;") == 0xd5457e62);
    REQUIRE(FuzzRun("VMAD.S16.U16 R0, R2, R3, R4;") == 0xd5457e62);
    REQUIRE(FuzzRun("VMAD.S16.S16 R0, R2, R3, R4;") == 0x7ebd7e62);
    REQUIRE(FuzzRun("VMAD.U16.S16 R0, R2, R3, R4;") == 0x7ebd7e62);

    REQUIRE(FuzzRun("VMAD.U16.U16 R0, R2.H1, R3,    R4;") == 0x9f784e26);
    REQUIRE(FuzzRun("VMAD.S16.U16 R0, R2,    R3.H1, R4;") == 0xbfd4816a);
    REQUIRE(FuzzRun("VMAD.S16.S16 R0, R2.H1, R3.H1, R4;") == 0x88c19d9a);
    REQUIRE(FuzzRun("VMAD.U16.S16 R0, R2,    R3.H1, R4;") == 0x694c816a);

    REQUIRE(FuzzRun("VMAD.U8.U16 R0, R2, 0xaaaa, RZ;") == 0x005aaa50);
    REQUIRE(FuzzRun("VMAD.U8.U16 R0, R2, 0xbbbb, R4;") == 0x9186c1a2);

    REQUIRE(FuzzRun("VMAD.U32.U32 R0, R2, R3, R4;") == 0x98417e62);
    REQUIRE(FuzzRun("VMAD.S32.S32 R0, R2, R3, R4;") == 0x98417e62);

    REQUIRE(FuzzRun("VMAD.U32.U16 R0, R2, 0xaded, R4;") == 0xca110a32);
    REQUIRE(FuzzRun("VMAD.U32.U16 R0, R2, 0xbded, R4;") == 0x0f798a32);
    REQUIRE(FuzzRun("VMAD.U32.S16 R0, R2, 0xbded, R4;") == 0xb8f18a32);
}
