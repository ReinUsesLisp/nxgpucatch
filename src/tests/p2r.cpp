#include <catch2/catch_test_macros.hpp>

#include "../eval_util.h"

static uint32_t Run(std::string code, uint32_t value = 0) {
    return EvalUtil::Run(".dksh compute\n"
                         "main:\n"
                         "MOV R0, c[0x0][0x140];\n"
                         "MOV R1, c[0x0][0x144];\n"
                         "IADD RZ.CC, RZ, 1;\n"               // Reset CC
                         "ISETP.NE.AND P0, PT, RZ, RZ, PT;\n" // Reset PR
                         "ISETP.NE.AND P1, PT, RZ, RZ, PT;\n"
                         "ISETP.NE.AND P2, PT, RZ, RZ, PT;\n"
                         "ISETP.NE.AND P3, PT, RZ, RZ, PT;\n"
                         "ISETP.NE.AND P4, PT, RZ, RZ, PT;\n"
                         "ISETP.NE.AND P5, PT, RZ, RZ, PT;\n"
                         "ISETP.NE.AND P6, PT, RZ, RZ, PT;\n" +
                             code +
                             "STG.E [R0], R2;\n"
                             "EXIT;\n",
                         value);
}

TEST_CASE("P2R PR Simple", "[shader]") {
    REQUIRE(Run(R"(
        ISETP.EQ.AND P0, PT, RZ, RZ, PT;
        P2R R2, PR, RZ, 0xff;
    )") == 1);
}

TEST_CASE("P2R CC Simple", "[shader]") {
    // Zero flag
    REQUIRE(Run(R"(
        IADD RZ.CC, RZ, RZ;
        P2R R2, CC, RZ, 0xff;
    )") == 1);
    // Sign flag
    REQUIRE(Run(R"(
        IADD RZ.CC, RZ, -1;
        P2R R2, CC, RZ, 0xff;
    )") == 2);
    // Carry flag
    REQUIRE(Run(R"(
        MOV32I R2, 0xffffffff;
        IADD RZ.CC, R2, 2;
        P2R R2, CC, RZ, 0xff;
    )") == 4);
    // Overflow flag (with sign)
    REQUIRE(Run(R"(
        MOV32I R2, 0x7fffffff;
        IADD RZ.CC, R2, 1;
        P2R R2, CC, RZ, 0xff;
    )") == 10);
}

TEST_CASE("P2R Mask", "[shader]") {
    REQUIRE(Run(R"(
        ISETP.EQ.AND P0, PT, RZ, RZ, PT;
        ISETP.EQ.AND P1, PT, RZ, RZ, PT;
        P2R R2, PR, RZ, 2;
    )") == 2);
}

TEST_CASE("P2R Insert", "[shader]") {
    REQUIRE(Run(R"(
        ISETP.EQ.AND P1, PT, RZ, RZ, PT;
        MOV32I R2, 0xccccccf0;
        P2R R2, PR, R2, 0xff;
    )") == 0xcc'cc'cc'02);

    REQUIRE(Run(R"(
        ISETP.EQ.AND P1, PT, RZ, RZ, PT;
        MOV32I R2, 0xccccccf0;
        P2R R2, PR, R2, 0x03;
    )") == 0xcc'cc'cc'f2);

    REQUIRE(Run(R"(
        ISETP.EQ.AND P0, PT, RZ, RZ, PT;
        ISETP.EQ.AND P1, PT, RZ, RZ, PT;
        P2R.B1 R2, PR, RZ, 2;
    )") == 0x02'00);

    REQUIRE(Run(R"(
        ISETP.EQ.AND P0, PT, RZ, RZ, PT;
        ISETP.EQ.AND P1, PT, RZ, RZ, PT;
        P2R.B2 R2, PR, RZ, 2;
    )") == 0x02'00'00);

    REQUIRE(Run(R"(
        ISETP.EQ.AND P0, PT, RZ, RZ, PT;
        ISETP.EQ.AND P1, PT, RZ, RZ, PT;
        P2R.B3 R2, PR, RZ, 2;
    )") == 0x02'00'00'00);

    REQUIRE(Run(R"(
        ISETP.EQ.AND P2, PT, RZ, RZ, PT;
        MOV32I R2, 0xcccccccc;
        P2R.B2 R2, PR, R2, 0x0f;
    )") == 0xcc'c4'cc'cc);
}
