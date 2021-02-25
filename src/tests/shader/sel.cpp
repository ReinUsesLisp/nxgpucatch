#include <numbers>

#include <catch2/catch_test_macros.hpp>

#include "bit_cast.h"
#include "compare.h"
#include "eval_util.h"

using namespace std::numbers;

static uint32_t Run(uint32_t value, std::string code) {
    return EvalUtil::Run(".dksh compute\n"
                         "main:\n"
                         "MOV R0, c[0x0][0x140];\n"
                         "MOV R1, c[0x0][0x144];\n"
                         "MOV R3, " +
                             std::to_string(value) +
                             ";\n"
                             "ISETP.T.AND P0, P1, RZ, RZ, PT;" +
                             code +
                             "STG.E [R0], R2;\n"
                             "EXIT;\n",
                         value);
}

TEST_CASE("SEL Reg", "[shader]") {
    REQUIRE(Run(5, "SEL R2, RZ, R3, P1;") == 5);
    REQUIRE(Run(5, "SEL R2, RZ, R3, !P1;") == 0);
    REQUIRE(Run(5, "SEL R2, RZ, R3, P0;") == 0);
    REQUIRE(Run(5, "SEL R2, RZ, R3, !P0;") == 5);
}

TEST_CASE("SEL Cbuf", "[shader]") {
    REQUIRE(Run(5, "SEL R2, RZ, c[2][0], P1;") == 5);
    REQUIRE(Run(5, "SEL R2, RZ, c[2][0], !P1;") == 0);
    REQUIRE(Run(5, "SEL R2, RZ, c[2][0], P0;") == 0);
    REQUIRE(Run(5, "SEL R2, RZ, c[2][0], !P0;") == 5);
}

TEST_CASE("SEL Imm", "[shader]") {
    REQUIRE(Run(5, "SEL R2, RZ, 54, P1;") == 54);
    REQUIRE(Run(5, "SEL R2, RZ, 54, !P1;") == 0);
    REQUIRE(Run(5, "SEL R2, RZ, 54, P0;") == 0);
    REQUIRE(Run(5, "SEL R2, RZ, 54, !P0;") == 54);
}
