#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

static uint32_t Run(std::string code) {
    return EvalUtil::Run(".dksh compute\n"
                         ".workgroup_size 32 1 1\n"
                         "main:\n"
                         "MOV R0, c[0x0][0x140];\n"
                         "MOV R1, c[0x0][0x144];\n"
                         "S2R R2, SR_TID.X;\n" +
                         code +
                         "\nmerge:\n"
                         "S2R R3, SR_TID.X;\n"
                         "ISETP.EQ.AND P0, PT, R3, RZ, PT;"
                         "@P0 STG.E [R0], R2;\n"
                         "EXIT;\n");
}

TEST_CASE("VOTE Ballot", "[shader]") {
    REQUIRE(Run("VOTE.ALL R2, PT, PT;") == 0xffffffff);
    REQUIRE(Run("VOTE.EQ R2, PT, PT;") == 0xffffffff);
    REQUIRE(Run("VOTE.ANY R2, PT, PT;") == 0xffffffff);
    REQUIRE(Run("VOTE.ALL R2, PT, !PT;") == 0);
    REQUIRE(Run("VOTE.EQ R2, PT, !PT;") == 0);
    REQUIRE(Run("VOTE.ANY R2, PT, !PT;") == 0);

    REQUIRE(Run(R"(
        SSY merge;
        ISETP.LT.AND P0, PT, R2, 16, PT;
        MOV R2, RZ;
        @!P0 SYNC;
        VOTE.ALL R2, PT, PT;
        SYNC;
    )") == 0x0000ffff);

    REQUIRE(Run(R"(
        ISETP.GE.AND P0, PT, R2, 16, PT;
        @!P0 EXIT;

        S2R R3, SR_TID.X;
        VOTE.ALL R2, PT, PT;
        STG.E [R0], R2;
        EXIT;
    )") == 0xffff0000);

    REQUIRE(Run(R"(
        ISETP.GE.AND P0, PT, R2, 16, PT;
        VOTE.ALL R2, PT, P0;
    )") == 0xffff0000);

    REQUIRE(Run(R"(
        ISETP.GE.AND P0, PT, R2, 24, PT;
        VOTE.ALL R2, PT, P0;
    )") == 0xff000000);

    REQUIRE(Run(R"(
        ISETP.LT.AND P0, PT, R2, 24, PT;
        VOTE.ALL R2, PT, P0;
    )") == 0x00ffffff);
}

TEST_CASE("VOTE Predicate", "[shader]") {
    REQUIRE(Run(R"(
        VOTE.ALL RZ, P0, PT;
        SEL R2, RZ, 1, !P0;  
    )") == 1);

    REQUIRE(Run(R"(
        ISETP.GE.AND P0, PT, R2, 24, PT;
        @P0 EXIT;

        VOTE.ALL RZ, P0, PT;
        SEL R2, RZ, 1, !P0;  
    )") == 1);

    REQUIRE(Run(R"(
        ISETP.GE.AND P0, PT, R2, 24, PT;
        @P0 EXIT;

        VOTE.ANY RZ, P0, PT;
        SEL R2, RZ, 1, !P0;  
    )") == 1);

    REQUIRE(Run(R"(
        ISETP.GE.AND P0, PT, R2, 24, PT;
        @P0 EXIT;

        VOTE.EQ RZ, P0, PT;
        SEL R2, RZ, 1, !P0;  
    )") == 1);

    REQUIRE(Run(R"(
        ISETP.GE.AND P0, PT, R2, 24, PT;
        @P0 EXIT;

        ISETP.LE.AND P0, PT, R2, 8, PT;
        VOTE.EQ RZ, P0, P0;
        SEL R2, RZ, 1, !P0;  
    )") == 0);

    REQUIRE(Run(R"(
        ISETP.GE.AND P0, PT, R2, 24, PT;
        @P0 EXIT;

        ISETP.LE.AND P0, PT, R2, 8, PT;
        VOTE.ANY RZ, P0, P0;
        SEL R2, RZ, 1, !P0;  
    )") == 1);

    REQUIRE(Run(R"(
        ISETP.GE.AND P0, PT, R2, 24, PT;
        @P0 EXIT;

        ISETP.LE.AND P0, PT, R2, 24, PT;
        VOTE.ALL RZ, P0, P0;
        SEL R2, RZ, 1, !P0;  
    )") == 1);

    REQUIRE(Run(R"(
        ISETP.GE.AND P0, PT, R2, 24, PT;
        @P0 EXIT;

        VOTE.EQ RZ, P0, !P0;
        SEL R2, RZ, 1, !P0;  
    )") == 1);
}

TEST_CASE("VOTE Ballot catching 64 thread GPUs", "[shader]") {
    REQUIRE(EvalUtil::Run(R"(.dksh compute
.workgroup_size 64 1 1
main:
MOV R0, c[0][0x140];
MOV R1, c[0][0x144];
S2R R3, SR_TID.X;
ISETP.LT.AND P0, PT, R3, 32, PT;
@P0 EXIT;
ISETP.GE.AND P0, PT, R3, 48, PT;
@P0 EXIT;
VOTE.ALL R2, PT, PT;
ISETP.EQ.AND P0, PT, R3, 32, PT;
@P0 STG.E [R0], R2;
EXIT;
    )") == 0xffff);
}
