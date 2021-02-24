#include <catch2/catch_test_macros.hpp>

#include "bit_cast.h"
#include "eval_util.h"

static uint32_t Run(std::string code) {
    return EvalUtil::Run(".dksh compute\n"
                         "main:\n"
                         "MOV R0, c[0x0][0x140];\n"
                         "MOV R1, c[0x0][0x144];\n"
                         "MOV R2, 0;\n"
                         "MOV R3, 0;\n"
                         "MOV R4, 0;\n" +
                             code +
                             "\nfinish:\n"
                             "STG.E [R0], R2;\n"
                             "EXIT;\n",
                         0);
}

TEST_CASE("Flow conditional execution", "[shader]") {
    REQUIRE(Run(R"(
        ISETP.T.AND P0, PT, RZ, RZ, PT;
    @P0 MOV R2, 3;
    )") == 3);
    REQUIRE(Run(R"(
        ISETP.T.AND P0, PT, RZ, RZ, PT;
   @!P0 MOV R2, 3;
    )") == 0);
    REQUIRE(Run(R"(
        ISETP.T.AND P0, PT, RZ, RZ, PT;
   @!P0 MOV R2, 3;
    @P0 MOV R2, 5;
    )") == 5);
    REQUIRE(Run(R"(
        ISETP.T.AND P0, PT, RZ, RZ, PT;
   @!P0 MOV R2, 3;
    @P0 MOV R2, 5;
    @P0 MOV R2, 6;
    @P0 MOV R2, 7;
    @P0 MOV R2, 8;
    @P0 MOV R2, 9;
   @!P0 MOV R2, 2;
    )") == 9);
    REQUIRE(Run(R"(
        ISETP.F.AND P0, PT, RZ, RZ, PT;
   @!P0 MOV R2, 4;
   @!P0 MOV R2, 3;
    @P0 MOV R2, 5;
   @!P0 MOV R3, 5;
    )") == 3);
}

TEST_CASE("Flow BRA", "[shader]") {
    REQUIRE(Run(R"(
        MOV R2, 2;
        BRA finish;
        MOV R2, 3;
    )") == 2);
    REQUIRE(Run(R"(
        ISETP.F.AND P0, PT, RZ, RZ, PT;
        MOV R2, 2;
   @!P0 BRA finish;
        MOV R2, 3;
    )") == 2);
    REQUIRE(Run(R"(
        ISETP.F.AND P0, PT, RZ, RZ, PT;
        MOV R2, 2;
    @P0 BRA finish;
        MOV R2, 3;
    )") == 3);
    REQUIRE(Run(R"(
        IADD RZ.CC, RZ, RZ;
        MOV R2, 2;
        BRA CC.EQ, finish;
        MOV R2, 3;
    )") == 2);
    REQUIRE(Run(R"(
        IADD RZ.CC, RZ, RZ;
        MOV R2, 2;
        BRA CC.NE, finish;
        MOV R2, 3;
    )") == 3);
}

TEST_CASE("Flow SYNC", "[shader]") {
    REQUIRE(Run(R"(
        ISETP.T.AND P0, PT, RZ, RZ, PT;
        SSY finish;
        MOV R2, 4;
        @P0 SYNC;
        MOV R2, 7;
        SYNC;
    )") == 4);
    REQUIRE(Run(R"(
        ISETP.T.AND P0, PT, RZ, RZ, PT;
        SSY finish;
        MOV R2, 4;
        @!P0 SYNC;
        MOV R2, 7;
        SYNC;
    )") == 7);
    REQUIRE(Run(R"(
        IADD RZ.CC, RZ, RZ;
        SSY finish;
        MOV R2, 4;
        SYNC CC.EQ;
        MOV R2, 7;
        SYNC;
    )") == 4);
    REQUIRE(Run(R"(
        IADD RZ.CC, RZ, RZ;
        SSY finish;
        MOV R2, 4;
        SYNC CC.NE;
        MOV R2, 7;
        SYNC;
    )") == 7);
}

TEST_CASE("Flow BRK", "[shader]") {
    REQUIRE(Run(R"(
        ISETP.T.AND P0, PT, RZ, RZ, PT;
        PBK finish;
        MOV R2, 4;
        @P0 BRK;
        MOV R2, 7;
        BRK;
    )") == 4);
    REQUIRE(Run(R"(
        ISETP.T.AND P0, PT, RZ, RZ, PT;
        PBK finish;
        MOV R2, 4;
        @!P0 BRK;
        MOV R2, 7;
        BRK;
    )") == 7);
    REQUIRE(Run(R"(
        IADD RZ.CC, RZ, RZ;
        PBK finish;
        MOV R2, 4;
        BRK CC.EQ;
        MOV R2, 7;
        BRK;
    )") == 4);
    REQUIRE(Run(R"(
        IADD RZ.CC, RZ, RZ;
        PBK finish;
        MOV R2, 4;
        BRK CC.NE;
        MOV R2, 7;
        BRK;
    )") == 7);
}

TEST_CASE("Flow CONT", "[shader]") {
    REQUIRE(Run(R"(
        ISETP.T.AND P0, PT, RZ, RZ, PT;
        PCNT finish;
        MOV R2, 4;
        @P0 CONT;
        MOV R2, 7;
        CONT;
    )") == 4);
    REQUIRE(Run(R"(
        ISETP.T.AND P0, PT, RZ, RZ, PT;
        PCNT finish;
        MOV R2, 4;
        @!P0 CONT;
        MOV R2, 7;
        CONT;
    )") == 7);
    REQUIRE(Run(R"(
        IADD RZ.CC, RZ, RZ;
        PCNT finish;
        MOV R2, 4;
        CONT CC.EQ;
        MOV R2, 7;
        CONT;
    )") == 4);
    REQUIRE(Run(R"(
        IADD RZ.CC, RZ, RZ;
        PCNT finish;
        MOV R2, 4;
        CONT CC.NE;
        MOV R2, 7;
        CONT;
    )") == 7);
}

TEST_CASE("Flow nested break", "[shader]") {
    REQUIRE(Run(R"(
        ISETP.T.AND P0, PT, RZ, RZ, PT;
        PBK finish;
        MOV R2, 1;
        SSY merge;
    @P0 BRA else;
if:
        MOV R2, 3;
        SYNC;
else:
        MOV R2, 4;
        BRK;
merge:
        MOV R2, 2;
        BRK;
    )") == 4);
    REQUIRE(Run(R"(
        ISETP.T.AND P0, P1, RZ, RZ, PT;
        PBK finish;
        MOV R2, 1;
        SSY merge;
    @P0 BRA else;
if:
        MOV R2, 3;
        SYNC;
else:
        MOV R2, 4;
    @P1 BRK;
        SYNC;
merge:
        MOV R2, 2;
        BRK;
    )") == 2);
    REQUIRE(Run(R"(
        ISETP.T.AND P0, P1, RZ, RZ, PT;
        PBK finish;
        MOV R2, 1;
        SSY merge;
    @P0 BRA else;
if:
        MOV R2, 3;
        SYNC;
else:
        MOV R2, 4;
   @!P1 BRK;
        SYNC;
merge:
        MOV R2, 2;
        BRK;
    )") == 4);
}

TEST_CASE("Flow simple loop", "[shader]") {
    REQUIRE(Run(R"(
loop:
        IADD R2, R2, 5;
        IADD R3, R3, 1;
        ISETP.LT.AND P0, PT, R3, 10, PT;
    @P0 BRA loop;
    )") == 50);
    REQUIRE(Run(R"(
loop:
        PCNT loop;
        IADD R2, R2, 5;
        IADD R3, R3, 1;
        ISETP.LT.AND P0, PT, R3, 10, PT;
    @P0 CONT;
    )") == 50);
}

TEST_CASE("Flow loop with break", "[shader]") {
    REQUIRE(Run(R"(
        PBK finish;
loop:
        IADD R3, R3, 1;
        ISETP.GT.AND P0, PT, R3, 10, PT;
    @P0 BRK;
        IADD R2, R2, 5;
        BRA loop;
    )") == 50);
}

TEST_CASE("Flow early exit") {
    REQUIRE(Run(R"(
        MOV R2, 4;
        STG.E [R0], R2 @RB 0 @WAIT 15 @Y;
        IADD RZ.CC, RZ, RZ @DEP 0 @WAIT 5;
        MOV R2, 7;
        EXIT CC.NE;
    )") == 7);
    REQUIRE(Run(R"(
        MOV R2, 4;
        STG.E [R0], R2 @RB 0 @WAIT 15 @Y;
        IADD RZ.CC, RZ, RZ @DEP 0 @WAIT 5;
        MOV R2, 7;
        EXIT CC.EQ;
    )") == 4);
    REQUIRE(Run(R"(
        MOV R2, 4;
        STG.E [R0], R2 @RB 0 @WAIT 15 @Y;
        ISETP.T.AND P0, PT, RZ, RZ, PT @DEP 0 @WAIT 5;
        MOV R2, 7;
    @P0 EXIT;
    )") == 4);
    REQUIRE(Run(R"(
        MOV R2, 4;
        STG.E [R0], R2 @RB 0 @WAIT 15 @Y;
        ISETP.T.AND P0, PT, RZ, RZ, PT @DEP 0 @WAIT 5;
        MOV R2, 7;
   @!P0 EXIT;
    )") == 7);
    REQUIRE(Run(R"(
        MOV R2, 4;
        STG.E [R0], R2 @RB 0 @WAIT 15 @Y;
        ISETP.T.AND P0, PT, RZ, RZ, PT @DEP 0 @WAIT 5;
        IADD RZ.CC, RZ, RZ;
        MOV R2, 7;
    @P0 EXIT CC.NE;
    )") == 7);
    REQUIRE(Run(R"(
        MOV R2, 4;
        STG.E [R0], R2 @RB 0 @WAIT 15 @Y;
        ISETP.T.AND P0, PT, RZ, RZ, PT @DEP 0 @WAIT 5;
        IADD RZ.CC, RZ, RZ;
        MOV R2, 7;
    @P0 EXIT CC.EQ;
    )") == 4);
}

TEST_CASE("Flow loop with break inside conditional", "[shader]") {
    REQUIRE(Run(R"(
        PBK finish;
loop:
        IADD R3, R3, 1;
        SSY merge;
        ISETP.LT.AND P0, PT, R3, 20, PT;
    @P0 SYNC;
        IADD R2, R2, 10;
        BRK;
merge:
        IADD R2, R2, 5;
        BRA loop;
    )") == 105);
    // Adds a loop to the previous assert
    REQUIRE(Run(R"(
upper_loop:
        PBK almost_finish;
loop:
        IADD R3, R3, 1;
        SSY merge;
        ISETP.LT.AND P0, PT, R3, 20, PT;
    @P0 SYNC;
        IADD R2, R2, 10;
        BRK;
merge:
        IADD R2, R2, 5;
        BRA loop;
almost_finish:
        IADD R4, R4, 1;
        ISETP.LT.AND P0, PT, R4, 3, PT;
    @P0 BRA upper_loop;
    )") == 125);
    // This is the same as the test above, but calls PBK in the loop for extra evilness
    REQUIRE(Run(R"(
        PBK almost_finish;
loop:
        IADD R3, R3, 1;
        SSY merge;
        ISETP.LT.AND P0, PT, R3, 20, PT;
    @P0 SYNC;
        IADD R2, R2, 10;
        BRK;
merge:
        IADD R2, R2, 5;
        BRA loop;
almost_finish:
        IADD R4, R4, 1;
        ISETP.LT.AND P0, PT, R4, 3, PT;
        PBK almost_finish;
    @P0 BRA loop;
    )") == 125);
}

TEST_CASE("Flow loop conditional loop", "[shader]") {
    REQUIRE(Run(R"(
        ISETP.T.AND P0, P1, RZ, RZ, PT;
        SSY finish;
   @!P0 SYNC;
loop:
        IADD R2, R2, 5;
        IADD R3, R3, 1;
        ISETP.LT.AND P1, PT, R3, 10, PT;
    @P1 BRA loop;
        SYNC;
    )") == 50);
}

TEST_CASE("Flow loop branch inside loop", "[shader]") {
    REQUIRE(Run(R"(
        ISETP.T.AND P0, P1, RZ, RZ, PT;
        SSY finish;
   @!P1 BRA inside;
   @!P0 SYNC;
loop:
        IADD R2, R2, 5;
inside:
        IADD R3, R3, 1;
        ISETP.LT.AND P1, PT, R3, 10, PT;
    @P1 BRA loop;
        SYNC;
    )") == 45);
}
