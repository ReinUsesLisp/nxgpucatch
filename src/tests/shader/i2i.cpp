#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

using namespace EvalUtil;

static constexpr int32_t VALUES[]{
    -5,     16,    -140,   170,   -280,       260,         -5001,      5013,
    -32820, 33767, -67535, 69535, 2000000000, -2000000000, 2147483647, -2147483648,
};

static uint32_t Run(uint32_t value, std::string code) {
    return EvalUtil::Run(".dksh compute\n"
                         "main:\n"
                         "MOV R0, c[0x0][0x140];\n"
                         "MOV R1, c[0x0][0x144];\n"
                         "MOV R2, c[2][0];\n" +
                             code +
                             "STG.E [R0], R2;\n"
                             "EXIT;\n",
                         value);
}

template <typename Dest, typename Src>
uint32_t Cast(int32_t v) {
    Src src{};
    std::memcpy(&src, &v, sizeof(src));
    Dest dest = static_cast<Dest>(src);
    uint32_t ret{};
    std::memcpy(&ret, &dest, sizeof(dest));
    return ret;
}

TEST_CASE("I2I Simple", "[shader]") {
    REQUIRE(Run(0xff, "I2I.S32.S8 R2, R2;") == 0xffffffff);
    REQUIRE(Run(0xff7f, "I2I.S32.S8 R2, R2.B0;") == 0x7f);
    REQUIRE(Run(0xff7f, "I2I.S32.S8 R2, R2.B1;") == 0xffffffff);
    REQUIRE(Run(0xff7f, "I2I.S32.S8 R2, R2.B2;") == 0);
    REQUIRE(Run(0xffccff7f, "I2I.S32.S8 R2, R2.B3;") == 0xffffffff);
    REQUIRE(Run(0xffccff7f, "I2I.S32.U8 R2, R2.B3;") == 0x000000ff);
    // The assembler doesn't support H1, so we hack it with .B2 (same behavior)
    REQUIRE(Run(0xffccff7f, "I2I.S32.S16 R2, R2.B2;") == 0xffffffcc);
    REQUIRE(Run(0xffccff7f, "I2I.S32.U16 R2, R2.B2;") == 0x0000ffcc);
    REQUIRE(Run(0x7fccff7f, "I2I.S32.S16 R2, R2.B2;") == 0x00007fcc);

    REQUIRE(Run(0xff, "I2I.S32.S8 R2, |R2|;") == 1);
    REQUIRE(Run(0xff, "I2I.S32.S8 R2, -R2;") == 1);
    REQUIRE(Run(0xff, "I2I.S32.S8 R2, -|R2|;") == 0xffffffff);

    for (const int32_t value : VALUES) {
        // Destination sign only matters when saturating, so we skip testing that
        REQUIRE(EvalUnary<u32>("I2I.S8.S8", value) == Cast<s8, s8>(value));
        REQUIRE(EvalUnary<u32>("I2I.S8.U8", value) == Cast<s8, u8>(value));
        REQUIRE(EvalUnary<u32>("I2I.S8.S16", value) == Cast<s8, s16>(value));
        REQUIRE(EvalUnary<u32>("I2I.S8.U16", value) == Cast<s8, u16>(value));
        REQUIRE(EvalUnary<u32>("I2I.S8.S32", value) == Cast<s8, s32>(value));
        REQUIRE(EvalUnary<u32>("I2I.S8.U32", value) == Cast<s8, u32>(value));

        REQUIRE(EvalUnary<u32>("I2I.S16.S8", value) == Cast<s16, s8>(value));
        REQUIRE(EvalUnary<u32>("I2I.S16.U8", value) == Cast<s16, u8>(value));
        REQUIRE(EvalUnary<u32>("I2I.S16.S16", value) == Cast<s16, s16>(value));
        REQUIRE(EvalUnary<u32>("I2I.S16.U16", value) == Cast<s16, u16>(value));
        REQUIRE(EvalUnary<u32>("I2I.S16.S32", value) == Cast<s16, s32>(value));
        REQUIRE(EvalUnary<u32>("I2I.S16.U32", value) == Cast<s16, u32>(value));

        REQUIRE(EvalUnary<u32>("I2I.S32.S8", value) == Cast<s32, s8>(value));
        REQUIRE(EvalUnary<u32>("I2I.S32.U8", value) == Cast<s32, u8>(value));
        REQUIRE(EvalUnary<u32>("I2I.S32.S16", value) == Cast<s32, s16>(value));
        REQUIRE(EvalUnary<u32>("I2I.S32.U16", value) == Cast<s32, u16>(value));
        REQUIRE(EvalUnary<u32>("I2I.S32.S32", value) == Cast<s32, s32>(value));
        REQUIRE(EvalUnary<u32>("I2I.S32.U32", value) == Cast<s32, u32>(value));
    }
}

static u32 SignedSaturate(s32 value, u32 destination_width) {
    const s32 sat = 1 << (destination_width - 1);
    const s32 dest = std::clamp(value, -sat, sat - 1);
    u32 ret{};
    std::memcpy(&ret, &dest, sizeof(dest));
    return ret;
}

static u32 SignedSaturate(u32 value, u32 destination_width) {
    const u32 sat = 1 << (destination_width - 1);
    const u32 dest = std::clamp<u32>(value, 0, sat - 1);
    return dest;
}

static u32 UnsignedSaturate(s32 value, u32 destination_width) {
    const u32 sat = (1 << destination_width) - 1;
    const u32 unsigned_value = value < 0 ? 0u : static_cast<u32>(value);
    return std::clamp(unsigned_value, 0u, sat);
}

static u32 UnsignedSaturate(u32 value, u32 destination_width) {
    const u32 sat = (1 << destination_width) - 1;
    return std::clamp(value, 0u, sat);
}

TEST_CASE("I2I Saturate Signed", "[shader]") {
    for (const int32_t value : VALUES) {
        const s8 s8_value{static_cast<s8>(value)};
        const s16 s16_value{static_cast<s16>(value)};

        REQUIRE(Run(value, "I2I.S8.S8.SAT R2, R2;") == SignedSaturate(s8_value, 8));
        REQUIRE(Run(value, "I2I.S16.S8.SAT R2, R2;") == SignedSaturate(s8_value, 16));
        REQUIRE(Run(value, "I2I.S32.S8.SAT R2, R2;") == SignedSaturate(s8_value, 32));

        REQUIRE(Run(value, "I2I.S8.S16.SAT R2, R2;") == SignedSaturate(s16_value, 8));
        REQUIRE(Run(value, "I2I.S16.S16.SAT R2, R2;") == SignedSaturate(s16_value, 16));
        REQUIRE(Run(value, "I2I.S32.S16.SAT R2, R2;") == SignedSaturate(s16_value, 32));

        REQUIRE(Run(value, "I2I.S8.S32.SAT R2, R2;") == SignedSaturate(value, 8));
        REQUIRE(Run(value, "I2I.S16.S32.SAT R2, R2;") == SignedSaturate(value, 16));
        REQUIRE(Run(value, "I2I.S32.S32.SAT R2, R2;") == SignedSaturate(value, 32));
    }
}

TEST_CASE("I2I Saturate Unsigned", "[shader]") {
    for (const int32_t value : VALUES) {
        const u8 u8_value{static_cast<u8>(value)};
        const u16 u16_value{static_cast<u16>(value)};
        const u32 u32_value{static_cast<u32>(value)};

        REQUIRE(Run(value, "I2I.U8.U8.SAT R2, R2;") == UnsignedSaturate(u8_value, 8));
        REQUIRE(Run(value, "I2I.U16.U8.SAT R2, R2;") == UnsignedSaturate(u8_value, 16));
        REQUIRE(Run(value, "I2I.U32.U8.SAT R2, R2;") == UnsignedSaturate(u8_value, 32));

        REQUIRE(Run(value, "I2I.U8.U16.SAT R2, R2;") == UnsignedSaturate(u16_value, 8));
        REQUIRE(Run(value, "I2I.U16.U16.SAT R2, R2;") == UnsignedSaturate(u16_value, 16));
        REQUIRE(Run(value, "I2I.U32.U16.SAT R2, R2;") == UnsignedSaturate(u16_value, 32));

        REQUIRE(Run(value, "I2I.U8.U32.SAT R2, R2;") == UnsignedSaturate(u32_value, 8));
        REQUIRE(Run(value, "I2I.U16.U32.SAT R2, R2;") == UnsignedSaturate(u32_value, 16));
        REQUIRE(Run(value, "I2I.U32.U32.SAT R2, R2;") == UnsignedSaturate(u32_value, 32));
    }
}

TEST_CASE("I2I Saturate Mixed", "[shader]") {
    for (const int32_t s32_value : VALUES) {
        const s8 s8_value{static_cast<s8>(s32_value)};
        const s16 s16_value{static_cast<s16>(s32_value)};
        const u8 u8_value{static_cast<u8>(s32_value)};
        const u16 u16_value{static_cast<u16>(s32_value)};
        const u32 u32_value{static_cast<u32>(s32_value)};

        REQUIRE(Run(u32_value, "I2I.S8.U8.SAT R2, R2;") == SignedSaturate(u8_value, 8));
        REQUIRE(Run(u32_value, "I2I.U16.S8.SAT R2, R2;") == UnsignedSaturate(s8_value, 16));
        REQUIRE(Run(u32_value, "I2I.S32.U8.SAT R2, R2;") == SignedSaturate(u8_value, 32));

        REQUIRE(Run(u32_value, "I2I.U8.S16.SAT R2, R2;") == UnsignedSaturate(s16_value, 8));
        REQUIRE(Run(u32_value, "I2I.S16.U16.SAT R2, R2;") == SignedSaturate(u16_value, 16));
        REQUIRE(Run(u32_value, "I2I.U32.S16.SAT R2, R2;") == UnsignedSaturate(s16_value, 32));

        REQUIRE(Run(u32_value, "I2I.S8.U32.SAT R2, R2;") == SignedSaturate(u32_value, 8));
        REQUIRE(Run(u32_value, "I2I.U16.S32.SAT R2, R2;") == UnsignedSaturate(s32_value, 16));
        REQUIRE(Run(u32_value, "I2I.S32.U32.SAT R2, R2;") == SignedSaturate(u32_value, 32));
    }
}

TEST_CASE("I2I CC", "[shader]") {
    // 1 - Zero
    // 2 - Sign
    // 4 - Carry
    // 8 - Overflow
    // I2I ops can only set the Zero and Sign bits.
    REQUIRE(Run(0xffff0000, "I2I.S8.S32 RZ.CC, R2; P2R R2, CC, RZ, 0xff;") == 1);
    REQUIRE(Run(0xffff0000, "I2I.S16.S32 RZ.CC, R2; P2R R2, CC, RZ, 0xff;") == 1);
    REQUIRE(Run(12 - 12, "I2I.S32.S32 RZ.CC, R2; P2R R2, CC, RZ, 0xff;") == 1);
    REQUIRE(Run(0xffff0000, "I2I.U8.S32 RZ.CC, R2; P2R R2, CC, RZ, 0xff;") == 1);
    REQUIRE(Run(0xffff0000, "I2I.U16.S32 RZ.CC, R2; P2R R2, CC, RZ, 0xff;") == 1);
    REQUIRE(Run(12 - 12, "I2I.U32.S32 RZ.CC, R2; P2R R2, CC, RZ, 0xff;") == 1);
    REQUIRE(Run(0xfffffff7, "I2I.U8.S32.SAT RZ.CC, R2; P2R R2, CC, RZ, 0xff;") == 1);
    REQUIRE(Run(0xfffffff7, "I2I.U16.S32.SAT RZ.CC, R2; P2R R2, CC, RZ, 0xff;") == 1);
    REQUIRE(Run(0xfffffff7, "I2I.U32.S32.SAT RZ.CC, R2; P2R R2, CC, RZ, 0xff;") == 1);

    // Sign bit only set with 32-bit wide results
    REQUIRE(Run(0xffff0000, "I2I.S32.S32 RZ.CC, R2; P2R R2, CC, RZ, 0xff;") == 2);
    REQUIRE(Run(0xffffffff, "I2I.U32.S32 RZ.CC, R2; P2R R2, CC, RZ, 0xff;") == 2);
    REQUIRE(Run(0xffffffff, "I2I.U32.U32 RZ.CC, R2; P2R R2, CC, RZ, 0xff;") == 2);
}
