#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

static int32_t Run(std::string code) {
    return int32_t(EvalUtil::Run(".dksh compute\n"
                                 "main:\n"
                                 "MOV R0, c[0x0][0x140];\n"
                                 "MOV R1, c[0x0][0x144];\n"
                                 "ISETP.T.AND P0, P1, RZ, RZ, PT;\n" +
                                     code +
                                     "STG.E [R0], R2;\n"
                                     "EXIT;\n",
                                 0));
}

TEST_CASE("PSET Simple", "[shader]") {
    REQUIRE(Run("PSET.AND.AND R2, PT, PT, PT;") == -1);
    REQUIRE(Run("PSET.AND.AND R2, PT, PT, !PT;") == 0);
    REQUIRE(Run("PSET.AND.AND R2, PT, !PT, PT;") == 0);
    REQUIRE(Run("PSET.AND.AND R2, !PT, PT, PT;") == 0);
    REQUIRE(Run("PSET.OR.AND R2, !PT, PT, PT;") == -1);
    REQUIRE(Run("PSET.OR.AND R2, PT, !PT, PT;") == -1);
    REQUIRE(Run("PSET.XOR.AND R2, PT, !PT, PT;") == -1);
    REQUIRE(Run("PSET.XOR.AND R2, !PT, !PT, PT;") == 0);
    REQUIRE(Run("PSET.XOR.AND R2, PT, PT, PT;") == 0);
    REQUIRE(Run("PSET.XOR.OR R2, PT, PT, PT;") == -1);
    REQUIRE(Run("PSET.XOR.XOR R2, PT, PT, PT;") == -1);
    REQUIRE(Run("PSET.XOR.XOR R2, PT, PT, !PT;") == 0);
    REQUIRE(Run("PSET.XOR.XOR R2, !PT, PT, PT;") == 0);
    REQUIRE(Run("PSET.BF.XOR.OR R2, !PT, PT, PT;") == 0x3f80'0000);
}
