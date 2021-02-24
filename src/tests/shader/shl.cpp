#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

static uint32_t Run(uint32_t value, std::string code) {
    return EvalUtil::Run(".dksh compute\n"
                         "main:\n"
                         "MOV R0, c[0x0][0x140];\n"
                         "MOV R1, c[0x0][0x144];\n"
                         "MOV R3, 3;\n" +
                             code +
                             "STG.E [R0], R2;\n"
                             "EXIT;\n",
                         value);
}

TEST_CASE("SHL Simple", "[shader]") {
    REQUIRE(Run(1, "MOV R2, 1; SHL R2, R2, R3;") == 8);
    REQUIRE(Run(5, "SHL R2, R3, c[2][0];") == 96);
    REQUIRE(Run(5, "SHL R2, R3, 5;") == 96);
}

TEST_CASE("SHL Undefined", "[shader][undefined]") {
    REQUIRE(Run(32, "SHL R2, R3, c[2][0];") == 0);
    REQUIRE(Run(32, "SHL.W R2, R3, c[2][0];") == 3);
    REQUIRE(Run(33, "SHL R2, R3, c[2][0];") == 0);
    REQUIRE(Run(33, "SHL.W R2, R3, c[2][0];") == 6);
}
