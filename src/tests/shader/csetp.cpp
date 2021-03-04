#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

static int32_t Run(std::string code) {
    return EvalUtil::Run<int32_t>(".dksh compute\n"
                                  "main:\n"
                                  "MOV R0, c[2][0];" +
                                  code +
                                  "MOV R2, c[0x0][0x140];"
                                  "MOV R3, c[0x0][0x144];"
                                  "MOV R0, RZ;"
                                  "@P0 MOV R0, -1;"
                                  "STG.E [R2], R0;"
                                  "EXIT;\n");
}

static int32_t Run(uint32_t cc, std::string cond) {
    return EvalUtil::Run<int32_t>(".dksh compute\n"
                                  "main:\n"
                                  "MOV R0, c[2][0];"
                                  "R2P CC, R0, 0xff;"
                                  "CSETP." +
                                      cond +
                                      ".AND P0, PT, CC, PT;"
                                      "MOV R0, RZ;"
                                      "@P0 MOV R0, -1;"
                                      "MOV R2, c[0x0][0x140];"
                                      "MOV R3, c[0x0][0x144];"
                                      "STG.E [R2], R0;"
                                      "EXIT;\n",
                                  cc);
}

TEST_CASE("CSETP Simple", "[shader]") {
    static constexpr std::pair<const char*, uint32_t> conds[]{
        {"F", 0x0000},   {"LT", 0xBB44},  {"EQ", 0x2222},  {"LE", 0x3366},  {"GT", 0x4411},
        {"NE", 0x5555},  {"GE", 0xcc33},  {"NUM", 0x7777}, {"NAN", 0x8888}, {"LTU", 0x33cc},
        {"EQU", 0xaaaa}, {"LEU", 0xbbee}, {"GTU", 0xcc99}, {"NEU", 0xdddd}, {"GEU", 0x44bb},
        {"T", 0xffff},   {"OFF", 0x00ff}, {"LO", 0x0f0f},  {"SFF", 0x3333}, {"LS", 0xafaf},
        {"HI", 0x5050},  {"SFT", 0xcccc}, {"HS", 0xf0f0},  {"OFT", 0xff00}, {"RLE", 0xeeee},
        {"RGT", 0x1111},
    };
    for (auto [cond, mask] : conds) {
        uint32_t result{};
        for (uint32_t cc = 0; cc <= 0xf; ++cc) {
            if (Run(cc, cond) == -1) {
                result |= 1 << cc;
            }
        }
        // Add some bits to make errors show up in hex
        result |= 0x10000;
        mask |= 0x10000;
        REQUIRE(result == mask);
    }
}

TEST_CASE("CSETP Variant", "[shader]") {
    REQUIRE(Run("CSETP.F.AND PT, P0, CC, PT;") == -1);
    REQUIRE(Run("CSETP.F.XOR PT, P0, CC, PT;") == 0);
    REQUIRE(Run("CSETP.T.OR PT, P0, CC, PT;") == -1);
    REQUIRE(Run("CSETP.T.OR PT, P0, CC, !PT;") == 0);
}
