#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

static int32_t Run(std::string code) {
    return int32_t(EvalUtil::Run(".dksh compute\n"
                                 "main:\n"
                                 "MOV R0, c[0x0][0x140];\n"
                                 "MOV R1, c[0x0][0x144];\n"
                                 "ISETP.T.AND P0, P1, RZ, RZ, PT;\n" +
                                     code +
                                     "MOV R2, 0;\n"
                                     "@P0 IADD R2, R2, 1;\n"
                                     "@P1 IADD R2, R2, 2;\n"
                                     "STG.E [R0], R2;\n"
                                     "EXIT;\n",
                                 0));
}

TEST_CASE("PSETP Simple", "[shader]") {
    REQUIRE(Run("PSETP.AND.AND P0, P1, PT, PT, PT;") == 1);
    REQUIRE(Run("PSETP.AND.AND P0, P1, PT, PT, !PT;") == 0);
    REQUIRE(Run("PSETP.AND.AND P0, P1, PT, !PT, PT;") == 0);
    REQUIRE(Run("PSETP.AND.AND P0, P1, !PT, PT, PT;") == 2);
    REQUIRE(Run("PSETP.OR.AND P0, P1, !PT, PT, PT;") == 3);
    REQUIRE(Run("PSETP.OR.AND P0, P1, PT, !PT, PT;") == 1);
    REQUIRE(Run("PSETP.XOR.AND P0, P1, PT, !PT, PT;") == 1);
    REQUIRE(Run("PSETP.XOR.AND P0, P1, !PT, !PT, PT;") == 2);
    REQUIRE(Run("PSETP.XOR.AND P0, P1, PT, PT, PT;") == 2);
    REQUIRE(Run("PSETP.XOR.OR P0, P1, PT, PT, PT;") == 3);
    REQUIRE(Run("PSETP.XOR.XOR P0, P1, PT, PT, PT;") == 1);
    REQUIRE(Run("PSETP.XOR.XOR P0, P1, PT, PT, !PT;") == 2);
    REQUIRE(Run("PSETP.XOR.XOR P0, P1, !PT, PT, PT;") == 2);
}
