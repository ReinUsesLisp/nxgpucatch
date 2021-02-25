#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

namespace {
struct Tuple {
    uint32_t a, b, c;
};
} // Anonymous namespace

uint32_t Run(Tuple value, std::string code) {
    return EvalUtil::Run(".dksh compute\n"
                         "main:\n"
                         "MOV R0, c[0x0][0x140];\n"
                         "MOV R1, c[0x0][0x144];\n"
                         "MOV R2, c[2][0];"
                         "MOV R3, c[2][4];"
                         "MOV R4, c[2][8];" +
                             code +
                             "STG.E [R0], R2;\n"
                             "EXIT;\n",
                         value);
}

// https://forums.developer.nvidia.com/t/reverse-lut-for-lop3-lut/110651
// Emulate GPU's LOP3.LUT (three-input logic op with 8-bit truth table)
static uint32_t lop3_fast(uint32_t a, uint32_t b, uint32_t c, unsigned ttbl) {
    uint32_t r = 0;
    if (ttbl & 0x01) {
        r |= ~a & ~b & ~c;
    }
    if (ttbl & 0x02) {
        r |= ~a & ~b & c;
    }
    if (ttbl & 0x04) {
        r |= ~a & b & ~c;
    }
    if (ttbl & 0x08) {
        r |= ~a & b & c;
    }
    if (ttbl & 0x10) {
        r |= a & ~b & ~c;
    }
    if (ttbl & 0x20) {
        r |= a & ~b & c;
    }
    if (ttbl & 0x40) {
        r |= a & b & ~c;
    }
    if (ttbl & 0x80) {
        r |= a & b & c;
    }
    return r;
}

TEST_CASE("LOP3 Simple", "[shader]") {
    static constexpr std::array TUPLES{
        Tuple{0x81293021u, 0x12839145u, 0xde92c1a8u},
        Tuple{0xa821838du, 0xccccccccu, 0x00000000u},
    };
    for (unsigned table = 0; table <= 0xff; ++table) {
        for (const Tuple tuple : TUPLES) {
            REQUIRE(Run(tuple, "LOP3.LUT R2, R2, R3, R4, " + std::to_string(table) + ';') ==
                    lop3_fast(tuple.a, tuple.b, tuple.c, table));
        }
    }
    const Tuple cbuf{0x0fff, 0xff00, 0x0ff};
    REQUIRE(Run(cbuf, "LOP3.LUT R2, R2, c[2][4], R4, 0x80;") ==
            lop3_fast(cbuf.a, cbuf.b, cbuf.c, 0x80));
    REQUIRE(Run(cbuf, "LOP3.LUT R2, R2, 0xff00, R4, 0x80;") ==
            lop3_fast(cbuf.a, cbuf.b, cbuf.c, 0x80));
}

TEST_CASE("LOP3 Predicate", "[shader]") {
    const Tuple value{1, 3, 5};
    REQUIRE(Run(value, "LOP3.LUT.T P0, R2, R2, R3, R4, 0xff; @P0 MOV R2, 13;") == 13);
    REQUIRE(Run(value, "LOP3.LUT.Z P0, R2, R2, R3, R4, 0; @P0 MOV R2, 14;") == 14);
    REQUIRE(Run(value, "LOP3.LUT.NZ P0, R2, R2, R3, R4, 0xff; @P0 MOV R2, 15;") == 15);
}
