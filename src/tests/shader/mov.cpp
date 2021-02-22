#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

static uint32_t Run(uint32_t value, std::string code) {
    return EvalUtil::Run(".dksh compute\n"
                         "main:\n"
                         "MOV R0, c[0x0][0x140];\n"
                         "MOV R1, c[0x0][0x144];\n" +
                             code +
                             "STG.E [R0], R2;\n"
                             "EXIT;\n",
                         value);
}

TEST_CASE("MOV Simple", "[shader]") {
    REQUIRE(Run(4, "MOV R2, c[2][0];") == 4);
    REQUIRE(Run(4, "MOV R2, 34;") == 34);
    REQUIRE(Run(4, "MOV R2, RZ;") == 0);
    REQUIRE(Run(4, "MOV R3, c[2][0]; MOV R2, R3;") == 4);
    REQUIRE(Run(4, "MOV R2, -34;") == uint32_t(-34));
    REQUIRE(Run(4, "MOV32I R2, 87;") == 87);
}
