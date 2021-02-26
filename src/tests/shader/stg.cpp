#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

TEST_CASE("STG Simple", "[shader]") {
    REQUIRE(EvalUtil::Run(R"(.dksh compute
main:
        MOV R0, c[0x0][0x140];
        MOV R1, c[0x0][0x144];
        MOV32I R2, 0x80000001;
        STG.E.U8 [R0], R2;
    )") == 1);

    REQUIRE(EvalUtil::Run(R"(.dksh compute
main:
        MOV R0, c[0x0][0x140];
        MOV R1, c[0x0][0x144];
        MOV32I R2, 0x80000001;
        STG.E.S8 [R0], R2;
    )") == 1);

    REQUIRE(EvalUtil::Run(R"(.dksh compute
main:
        MOV R0, c[0x0][0x140];
        MOV R1, c[0x0][0x144];
        MOV32I R2, 0x80000001;
        STG.E.U16 [R0], R2;
    )") == 1);

    REQUIRE(EvalUtil::Run(R"(.dksh compute
main:
        MOV R0, c[0x0][0x140];
        MOV R1, c[0x0][0x144];
        MOV32I R2, 0x80000001;
        STG.E.S16 [R0], R2;
    )") == 1);

    REQUIRE(EvalUtil::Run(R"(.dksh compute
main:
        MOV R0, c[0x0][0x140];
        MOV R1, c[0x0][0x144];
        MOV32I R2, 0x80000001;
        STG.E.32 [R0], R2;
    )") == 0x80000001);

    REQUIRE(EvalUtil::Run<uint64_t>(R"(.dksh compute
main:
        MOV R0, c[0x0][0x140];
        MOV R1, c[0x0][0x144];
        MOV32I R2, 0x80000001;
        MOV32I R3, 0xffffffff;
        STG.E.64 [R0], R2;
    )") == 0xffffffff80000001);

    REQUIRE(EvalUtil::Run<std::array<uint64_t, 2>>(R"(.dksh compute
main:
        MOV R4, c[0x0][0x140];
        MOV R5, c[0x0][0x144];
        MOV32I R0, 0x80000001;
        MOV32I R1, 0xffffffff;
        MOV32I R2, 0xcccccccc;
        MOV32I R3, 0xaaaaaaaa;
        STG.E.128 [R4], R0;
    )") == std::array<uint64_t, 2>{0xffffffff80000001, 0xaaaaaaaacccccccc});
}

TEST_CASE("STG Offset", "[shader]") {
    REQUIRE(EvalUtil::Run(R"(.dksh compute
main:
        MOV R0, c[0x0][0x140];
        MOV R1, c[0x0][0x144];
        IADD R0.CC, R0, 4;
        IADD.X R1, R1, RZ;
        MOV32I R2, 0xdead;
        STG.E [R0-4], R2;
    )") == 0xdead);
}

TEST_CASE("STG Concurrency", "[shader]") {
    REQUIRE(EvalUtil::Run(R"(.dksh compute
.workgroup_size 32 1 1
main:
        MOV R0, c[0x0][0x140];
        MOV R1, c[0x0][0x144];
        S2R R2, SR_TID.X;
        LOP.AND PT, R2, R2, 3;
        IADD R0.CC, R0, R2;
        IADD.X R1, R1, RZ;
        IADD R2, R2, 1;
        STG.E.U8 [R0], R2;
    )") == 0x04030201);

    REQUIRE(EvalUtil::Run(R"(.dksh compute
.workgroup_size 32 1 1
main:
        MOV R0, c[0x0][0x140];
        MOV R1, c[0x0][0x144];
        S2R R2, SR_TID.X;
        LOP.AND PT, R2, R2, 1;
        ISCADD R0.CC, R2, R0, 1;
        IADD.X R1, R1, RZ;
        IADD R2, R2, 1;
        STG.E.U16 [R0], R2;
    )") == 0x00020001);
}
