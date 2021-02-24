#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

static uint32_t Run(uint32_t value, std::string code) {
    return EvalUtil::Run(".dksh compute\n"
                         "main:\n"
                         "MOV R0, c[0x0][0x140];\n"
                         "MOV R1, c[0x0][0x144];\n"
                         "ISETP.T.AND P0, P1, RZ, RZ, PT;\n"
                         "MOV R2, 50;\n"
                         "MOV R3, -50;\n"
                         "MOV R4, 75;\n"
                         "MOV R5, 25;\n" +
                             code +
                             "STG.E [R0], R2;\n"
                             "EXIT;\n",
                         value);
}

TEST_CASE("IMNMX Simple", "[shader]") {
    REQUIRE(Run(75, "IMNMX R2, R2, R5, PT;") == 25);

    REQUIRE(Run(75, "IMNMX R2, R2, R4, PT;") == 50);
    REQUIRE(Run(75, "IMNMX R2, R2, c[2][0], PT;") == 50);
    REQUIRE(Run(75, "IMNMX R2, R2, 75, PT;") == 50);

    REQUIRE(Run(75, "IMNMX R2, R2, R4, !PT;") == 75);
    REQUIRE(Run(75, "IMNMX R2, R2, c[2][0], !PT;") == 75);
    REQUIRE(Run(75, "IMNMX R2, R2, 75, !PT;") == 75);

    REQUIRE(Run(75, "IMNMX R2, R2, R4, P1;") == 75);
    REQUIRE(Run(75, "IMNMX R2, R2, c[2][0], P1;") == 75);
    REQUIRE(Run(75, "IMNMX R2, R2, 75, P1;") == 75);

    REQUIRE(Run(75, "IMNMX R2, R2, R4, P0;") == 50);
    REQUIRE(Run(75, "IMNMX R2, R2, c[2][0], P0;") == 50);
    REQUIRE(Run(75, "IMNMX R2, R2, 75, P0;") == 50);
}

TEST_CASE("IMNMX Signed", "[shader]") {
    REQUIRE(Run(75, "IMNMX.U32 R2, R2, R3, PT;") == 50);
    REQUIRE(Run(75, "IMNMX.S32 R2, R2, R3, PT;") == uint32_t(-50));

    REQUIRE(Run(75, "IMNMX.U32 R2, R2, R3, !PT;") == uint32_t(-50));
    REQUIRE(Run(75, "IMNMX.S32 R2, R2, R3, !PT;") == 50);
}
