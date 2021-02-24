#include <bit>

#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

using namespace EvalUtil;

static uint32_t Run(std::string code, uint32_t value = 0) {
    return EvalUtil::Run(".dksh compute\n"
                         "main:\n"
                         "MOV R0, c[0x0][0x140];\n"
                         "MOV R1, c[0x0][0x144];\n" +
                             code +
                             "STG.E [R0], R2;\n"
                             "EXIT;\n",
                         value);
}

TEST_CASE("POPC Simple", "[shader]") {
    for (const u32 value : {25, 64, 123, 195, -45}) {
        REQUIRE(EvalUnary<u32, u32>("POPC", value) == u32(std::popcount(value)));
    }
}

TEST_CASE("POPC Negation", "[shader]") {
    for (const u32 value : {25, 64, 123, 195, -45}) {
        const u32 res = u32(std::popcount(~value));
        REQUIRE(Run("POPC R2, ~c[2][0];", value) == res);
        REQUIRE(Run("MOV32I R2, " + std::to_string(value) + "; POPC R2, ~R2;") == res);
        REQUIRE(Run("POPC R2, " + std::to_string(s32(value)) + " .INV;") == res);
    }
}
