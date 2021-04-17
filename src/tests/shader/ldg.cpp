#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

TEST_CASE("LDG Simple", "[shader]") {
    REQUIRE(EvalUtil::Run(R"(.dksh compute
main:
        MOV R0, c[0x0][0x140];
        MOV R1, c[0x0][0x144];
        MOV32I R2, 0x800000f8;
        STG.E [R0], R2;
        LDG.E.S8 R2, [R0];
        STG.E [R0], R2;
        EXIT;
    )") == 0xfffffff8);

    REQUIRE(EvalUtil::Run(R"(.dksh compute
main:
        MOV R0, c[0x0][0x140];
        MOV R1, c[0x0][0x144];
        MOV32I R2, 0x800000f8;
        STG.E [R0], R2;
        LDG.E.U8 R2, [R0];
        STG.E [R0], R2;
        EXIT;
    )") == 0x000000f8);

    REQUIRE(EvalUtil::Run(R"(.dksh compute
main:
        MOV R0, c[0x0][0x140];
        MOV R1, c[0x0][0x144];
        MOV32I R2, 0x800080f8;
        STG.E [R0], R2;
        LDG.E.S16 R2, [R0];
        STG.E [R0], R2;
        EXIT;
    )") == 0xffff80f8);

    REQUIRE(EvalUtil::Run(R"(.dksh compute
main:
        MOV R0, c[0x0][0x140];
        MOV R1, c[0x0][0x144];
        MOV32I R2, 0x800080f8;
        STG.E [R0], R2;
        LDG.E.U16 R2, [R0];
        STG.E [R0], R2;
        EXIT;
    )") == 0x000080f8);

    REQUIRE(EvalUtil::Run(R"(.dksh compute
main:
        MOV R0, c[0x0][0x140];
        MOV R1, c[0x0][0x144];
        MOV32I R2, 0x800080f8;
        STG.E [R0], R2;
        LDG.E R2, [R0];
        STG.E [R0], R2;
        EXIT;
    )") == 0x800080f8);

    REQUIRE(EvalUtil::Run<uint64_t>(R"(.dksh compute
main:
        MOV R4, c[0x0][0x140];
        MOV R5, c[0x0][0x144];
        MOV32I R0, 0x800080f8;
        MOV32I R1, 0x800080f2;
        STG.E.64 [R4], R0;
        LDG.E.64 R0, [R4];
        STG.E.64 [R4], R0;
        EXIT;
    )") == 0x800080f2'800080f8);

    REQUIRE(EvalUtil::Run<std::array<uint64_t, 2>>(R"(.dksh compute
main:
        MOV R4, c[0x0][0x140];
        MOV R5, c[0x0][0x144];
        MOV32I R0, 0x800080f8;
        MOV32I R1, 0x800080f2;
        MOV32I R2, 0x700080f2;
        MOV32I R3, 0x600080f2;
        STG.E.128 [R4], R0;
        LDG.E.128 R0, [R4];
        STG.E.128 [R4], R0;
        EXIT;
    )") == std::array<uint64_t, 2>{0x8000'80f2'8000'80f8, 0x6000'80f2'7000'80f2});

    REQUIRE(EvalUtil::Run<std::array<uint64_t, 2>>(R"(.dksh compute
main:
        MOV R4, c[0x0][0x140];
        MOV R5, c[0x0][0x144];
        MOV32I R0, 0x800080f8;
        MOV32I R1, 0x800080f2;
        MOV32I R2, 0x700080f2;
        MOV32I R3, 0x600080f2;
        STG.E.128 [R4], R0;
        LDG.E.U.128 R0, [R4];
        STG.E.128 [R4], R0;
        EXIT;
    )") == std::array<uint64_t, 2>{0x8000'80f2'8000'80f8, 0x6000'80f2'7000'80f2});
}
