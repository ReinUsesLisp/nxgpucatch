#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

TEST_CASE("CAL Simple", "[shader]") {
    REQUIRE(EvalUtil::Run(R"(
.dksh compute
main:
        MOV R0, c[0x0][0x140];
        MOV R1, c[0x0][0x144];
        MOV R2, 4;
        CAL foo;
        STG.E [R0], R2;
        EXIT;
foo:
        MOV R2, 7;
        RET;
    )") == 7);

    REQUIRE(EvalUtil::Run(R"(
.dksh compute
main:
        MOV R0, c[0x0][0x140];
        MOV R1, c[0x0][0x144];
        MOV R2, 4;
        CAL foo;
        EXIT;
foo:
        MOV R2, 78;
        STG.E [R0], R2;
        RET;
    )") == 78);

    REQUIRE(EvalUtil::Run(R"(
.dksh compute
main:
        MOV R0, c[0x0][0x140];
        MOV R1, c[0x0][0x144];
        MOV R2, 4;
        CAL foo;
        EXIT;
foo:
        MOV R2, 78;
        STG.E [R0], R2;
        RET;
    )") == 78);
}

TEST_CASE("CAL Exit in subroutine", "[shader]") {
    REQUIRE(EvalUtil::Run(R"(
.dksh compute
main:
        MOV R0, c[0x0][0x140];
        MOV R1, c[0x0][0x144];
        MOV R2, 4;
        CAL foo;
        MOV R2, 3;
        STG.E [R0], R2;
        EXIT;
foo:
        MOV R2, 79;
        STG.E [R0], R2;
        EXIT;
    )") == 79);
}

TEST_CASE("CAL with complex flow", "[shader]") {
    SECTION("Inside conditional") {
        REQUIRE(EvalUtil::Run(R"(
.dksh compute
main:
            MOV R0, c[0x0][0x140];
            MOV R1, c[0x0][0x144];
            MOV R2, 4;
            ISETP.T.AND P0, P1, RZ, RZ, PT;
            MOV R2, 3;
            SSY merge;
        @P1 SYNC;
            CAL foo;
            SYNC;
merge:
            STG.E [R0], R2;
            EXIT;
foo:
            MOV R2, 13;
            RET;
        )") == 13);
    }
    SECTION("Inside loop") {
        REQUIRE(EvalUtil::Run(R"(
.dksh compute
main:
            MOV R0, c[0x0][0x140];
            MOV R1, c[0x0][0x144];
            MOV R3, 4;
            MOV R2, RZ;
loop:
            CAL foo;
        @P0 BRA loop;
            STG.E [R0], R2;
            EXIT;
foo:
            IADD R2, R2, 13;
            IADD R3, R3, -1;
            ISETP.NE.AND P0, PT, R3, RZ, PT;
            RET;
        )") == 52);
    }
    SECTION("Loop inside call") {
        REQUIRE(EvalUtil::Run(R"(
.dksh compute
main:
            MOV R0, c[0x0][0x140];
            MOV R1, c[0x0][0x144];
            CAL foo;
            STG.E [R0], R2;
            EXIT;
foo:
            MOV R3, 4;
            MOV R2, RZ;
loop:
            IADD R2, R2, 13;
            IADD R3, R3, -1;
            ISETP.NE.AND P0, PT, R3, RZ, PT;
        @P0 BRA loop;
            RET;
        )") == 52);
    }
    SECTION("Loop inside call to function label") {
        REQUIRE(EvalUtil::Run(R"(
.dksh compute
main:
            MOV R0, c[0x0][0x140];
            MOV R1, c[0x0][0x144];
            MOV R3, 4;
            MOV R2, RZ;
            CAL foo;
            STG.E [R0], R2;
            EXIT;
foo:
            IADD R2, R2, 13;
            IADD R3, R3, -1;
            ISETP.NE.AND P0, PT, R3, RZ, PT;
        @P0 BRA foo;
            RET;
        )") == 52);
    }
}

TEST_CASE("CAL multiple functions", "[shader]") {
    REQUIRE(EvalUtil::Run(R"(
.dksh compute
main:
        MOV R0, c[0x0][0x140];
        MOV R1, c[0x0][0x144];
        MOV R2, 4;
        MOV R3, 6;
        CAL add_duplicate;
        STG.E [R0], R2;
        EXIT;
add_duplicate:
        CAL add;
        CAL duplicate;
        RET;
add:
        IADD R2, R2, R3;
        RET;
duplicate:
        IADD R2, R2, R2;
        RET;
    )") == 20);
}

TEST_CASE("CAL multiple calls", "[shader]") {
    REQUIRE(EvalUtil::Run(R"(
.dksh compute
main:
        MOV R0, c[0x0][0x140];
        MOV R1, c[0x0][0x144];
        MOV R2, 4;
        CAL duplicate;
        CAL duplicate;
        CAL duplicate;
        CAL duplicate;
        STG.E [R0], R2;
        EXIT;
duplicate:
        IADD R2, R2, R2;
        RET;
    )") == 64);
}

TEST_CASE("CAL conditional return", "[shader]") {
    REQUIRE(EvalUtil::Run(R"(
.dksh compute
main:
        MOV R0, c[0x0][0x140];
        MOV R1, c[0x0][0x144];
        CAL foo;
        STG.E [R0], R2;
        EXIT;
foo:
        MOV R3, 4;
        MOV R2, RZ;
loop:
        IADD R2, R2, 13;
        IADD R3, R3, -1;
        ISETP.NE.AND P0, PT, R3, RZ, PT;
   @!P0 RET;
        BRA loop;
    )") == 52);
}

TEST_CASE("CAL early return", "[shader]") {
    SECTION("Hit early") {
        REQUIRE(EvalUtil::Run(R"(
.dksh compute
main:
            MOV R0, c[0x0][0x140];
            MOV R1, c[0x0][0x144];
            CAL foo;
            STG.E [R0], R2;
            EXIT;
foo:
            MOV R3, 14;
            MOV R2, RZ;
loop:
            ISETP.EQ.AND P0, PT, R3, 12, PT;
        @P0 RET;
            IADD R2, R2, 13;
            IADD R3, R3, -1;
            ISETP.NE.AND P0, PT, R3, RZ, PT;
        @P0 BRA loop;
            RET;
        )") == 26);
    }
    SECTION("Not hit early") {
        REQUIRE(EvalUtil::Run(R"(
.dksh compute
main:
            MOV R0, c[0x0][0x140];
            MOV R1, c[0x0][0x144];
            CAL foo;
            STG.E [R0], R2;
            EXIT;
foo:
            MOV R3, 14;
            MOV R2, RZ;
loop:
            ISETP.EQ.AND P0, PT, R3, 17, PT;
        @P0 RET;
            IADD R2, R2, 13;
            IADD R3, R3, -1;
            ISETP.NE.AND P0, PT, R3, RZ, PT;
        @P0 BRA loop;
            RET;
        )") == 182);
    }
}
