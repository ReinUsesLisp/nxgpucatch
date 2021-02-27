#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

template <int workgroup_size>
static auto Run(std::string code) {
    return EvalUtil::Run<std::array<int32_t, workgroup_size>>(".dksh compute\n"
                                                              ".workgroup_size " +
                                                              std::to_string(workgroup_size) +
                                                              " 1 1\n"
                                                              "main:\n"
                                                              "S2R R2, SR_TID.X;" +
                                                              code +
                                                              "\nmerge:\n"
                                                              "S2R R3, SR_TID.X;\n"
                                                              "ISCADD R0.CC, R3, c[0x0][0x140], 2;"
                                                              "IADD.X R1, RZ, c[0x0][0x144];"
                                                              "@!P1 MOV R2, 0xdead;"
                                                              "STG.E [R0], R2;"
                                                              "EXIT;");
}

TEST_CASE("SHFL IDX", "[shader]") {
    REQUIRE(Run<32>("SHFL.IDX P1, R2, R2, 7, 0x001f;") ==
            std::array{7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
                       7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7});
    REQUIRE(Run<32>("SHFL.IDX P1, R2, R2, 3, 0x101f;") ==
            std::array{3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,
                       19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19});
    REQUIRE(Run<32>("SHFL.IDX P1, R2, R2, 3, 0x0810;") ==
            std::array{3, 3, 3, 3, 3, 3, 3, 3, 11, 11, 11, 11, 11, 11, 11, 11,
                       3, 3, 3, 3, 3, 3, 3, 3, 11, 11, 11, 11, 11, 11, 11, 11});
}

TEST_CASE("SHFL UP", "[shader]") {
    REQUIRE(Run<32>("SHFL.UP P1, R2, R2, 2, 0;") ==
            std::array{0xdead, 0xdead, 0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12, 13,
                       14,     15,     16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29});
    REQUIRE(Run<32>("SHFL.UP P1, R2, R2, 2, 0x0003;") ==
            std::array{0xdead, 0xdead, 0xdead, 0xdead, 0xdead, 3,  4,  5,  6,  7,  8,
                       9,      10,     11,     12,     13,     14, 15, 16, 17, 18, 19,
                       20,     21,     22,     23,     24,     25, 26, 27, 28, 29});
    REQUIRE(Run<32>("SHFL.UP P1, R2, R2, 2, 0x1203;") ==
            std::array{0xdead, 0xdead, 0xdead, 0xdead, 2,  3,      4,      5,      6,      7,  8,
                       9,      10,     11,     12,     13, 0xdead, 0xdead, 0xdead, 0xdead, 18, 19,
                       20,     21,     22,     23,     24, 25,     26,     27,     28,     29});
}

TEST_CASE("SHFL DOWN", "[shader]") {
    REQUIRE(Run<32>("SHFL.DOWN P1, R2, R2, 2, 0x001f;") ==
            std::array{2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12, 13, 14, 15, 16,     17,
                       18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 0xdead, 0xdead});
    REQUIRE(Run<32>("SHFL.DOWN P1, R2, R2, 2, 0x031f;") ==
            std::array{2,  3,  4,  5,  6,  7,  8,      9,      10,     11,    12,
                       13, 14, 15, 16, 17, 18, 19,     20,     21,     22,    23,
                       24, 25, 26, 27, 28, 29, 0xdead, 0xdead, 0xdead, 0xdead});
    REQUIRE(Run<32>("SHFL.DOWN P1, R2, R2, 2, 0x0712;") ==
            std::array{2,      3,      4,      5,      6,      7,      8,      9,
                       10,     11,     12,     13,     14,     15,     16,     17,
                       0xdead, 0xdead, 0xdead, 0xdead, 0xdead, 0xdead, 0xdead, 0xdead,
                       0xdead, 0xdead, 0xdead, 0xdead, 0xdead, 0xdead, 0xdead, 0xdead});
}

TEST_CASE("SHFL BFLY", "[shader]") {
    REQUIRE(Run<32>("SHFL.BFLY P1, R2, R2, 1, 0x1c03;") ==
            std::array{1,  0,  3,  2,  5,  4,  7,  6,  9,  8,  11, 10, 13, 12, 15, 14,
                       17, 16, 19, 18, 21, 20, 23, 22, 25, 24, 27, 26, 29, 28, 31, 30});
    REQUIRE(Run<32>("SHFL.BFLY P1, R2, R2, 2, 0x1c03;") ==
            std::array{2,  3,  0,  1,  6,  7,  4,  5,  10, 11, 8,  9,  14, 15, 12, 13,
                       18, 19, 16, 17, 22, 23, 20, 21, 26, 27, 24, 25, 30, 31, 28, 29});
    REQUIRE(Run<32>("SHFL.BFLY P1, R2, R2, 5, 0x1f03;") ==
            std::array{0xdead, 0xdead, 0xdead, 0xdead, 1,  0,  3,  2,
                       0xdead, 0xdead, 0xdead, 0xdead, 9,  8,  11, 10,
                       0xdead, 0xdead, 0xdead, 0xdead, 17, 16, 19, 18,
                       0xdead, 0xdead, 0xdead, 0xdead, 25, 24, 27, 26});
    REQUIRE(Run<32>("SHFL.BFLY P1, R2, R2, 2, 0x1c05;") ==
            std::array<int, 32>{0xdead, 0xdead, 0,  1,  0xdead, 0xdead, 4,  5,
                                0xdead, 0xdead, 8,  9,  0xdead, 0xdead, 12, 13,
                                0xdead, 0xdead, 16, 17, 0xdead, 0xdead, 20, 21,
                                0xdead, 0xdead, 24, 25, 0xdead, 0xdead, 28, 29});
}

TEST_CASE("SHFL Register", "[shader]") {
    REQUIRE(Run<32>("MOV R3, 7; SHFL.IDX P1, R2, R2, R3, 0x001f;") ==
            std::array{7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
                       7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7});
    REQUIRE(Run<32>("MOV R3, 0x001f; SHFL.IDX P1, R2, R2, 7, R3;") ==
            std::array{7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
                       7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7});
}
