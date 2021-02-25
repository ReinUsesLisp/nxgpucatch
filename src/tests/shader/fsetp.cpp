#include <catch2/catch_test_macros.hpp>

#include "bit_cast.h"
#include "eval_util.h"

// Most comparison operations are already tested in FSET

static int32_t Run(float a, float b, std::string code) {
    return int32_t(EvalUtil::Run(".dksh compute\n"
                                 "main:\n"
                                 "MOV R0, c[0x0][0x140];\n"
                                 "MOV R1, c[0x0][0x144];\n"
                                 "MOV R2, c[2][0]\n;"
                                 "MOV R3, c[2][4]\n;" +
                                     code +
                                     "SEL R2, RZ, -1, !P0;\n"
                                     "STG.E [R0], R2;\n"
                                     "EXIT;\n",
                                 std::array{a, b}));
}

TEST_CASE("FSETP Simple", "[shader]") {
    REQUIRE(Run(2.0f, 3.0f, "FSETP.LT.FTZ.AND P0, PT, R2, R3, PT;") == -1);
    REQUIRE(Run(3.0f, 2.0f, "FSETP.LT.FTZ.AND P0, PT, R2, R3, PT;") == 0);
    REQUIRE(Run(3.0f, 2.0f, "FSETP.LT.FTZ.XOR P0, PT, R2, R3, PT;") == -1);
    REQUIRE(Run(3.0f, 2.0f, "FSETP.LT.FTZ.OR P0, PT, R2, R3, PT;") == -1);

    REQUIRE(Run(2.0f, 3.0f, "FSETP.LT.FTZ.OR PT, P0, R2, R3, PT;") == -1);
    REQUIRE(Run(2.0f, 3.0f, "FSETP.LT.FTZ.AND PT, P0, R2, R3, PT;") == 0);
    REQUIRE(Run(3.0f, 2.0f, "FSETP.LT.FTZ.XOR PT, P0, R2, R3, !PT;") == -1);
}

TEST_CASE("FSETP Denorm", "[shader][fpcontrol]") {
    static constexpr float denorm{1e-40};
    REQUIRE(Run(denorm, 0.0f, "FSETP.GT.FTZ.AND P0, PT, R2, R3, PT;") == 0);
    REQUIRE(Run(denorm, 0.0f, "FSETP.GT.AND P0, PT, R2, R3, PT;") == -1);
}
