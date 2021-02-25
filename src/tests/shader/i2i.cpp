#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

using namespace EvalUtil;

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
[[maybe_unused]] uint32_t Cast(int32_t v) {
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
    
    static constexpr int32_t VALUES[]{
        -5,     16,    -140,   170,   -280,       260,         -5001,      5013,
        -32820, 33767, -67535, 69535, 2000000000, -2000000000, 2147483647, -2147483648,
    };
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
