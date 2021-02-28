#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

using namespace EvalUtil;

template <typename Result>
static Result Run(auto value, std::string code) {
    return EvalUtil::Run<Result>(".dksh compute\n"
                                 "main:\n"
                                 "MOV R0, c[0x0][0x140];"
                                 "MOV R1, c[0x0][0x144];" +
                                     code +
                                     "STG.E [R0], R2;"
                                     "EXIT;\n",
                                 value);
}

template <typename Dest, typename Src>
Dest CVT(u64 value) {
    Src src{};
    std::memcpy(&src, &value, sizeof(src));
    return static_cast<Dest>(src);
}

TEST_CASE("I2F Simple", "[shader]") {
    for (const u64 value : {0x17u, 0x80u, 0xdeu, 0xffu, 0x170u, 0x8000u, 0xdee0u}) {
        REQUIRE(EvalUnary<f16, s8>("I2F.F16.S8", value) == CVT<f16, s8>(value));
        REQUIRE(EvalUnary<f16, u8>("I2F.F16.U8", value) == CVT<f16, u8>(value));
        REQUIRE(EvalUnary<f16, s16>("I2F.F16.S16", value) == CVT<f16, s16>(value));
        REQUIRE(EvalUnary<f16, u16>("I2F.F16.U16", value) == CVT<f16, u16>(value));

        REQUIRE(EvalUnary<f32, s8>("I2F.F32.S8", value) == CVT<f32, s8>(value));
        REQUIRE(EvalUnary<f32, u8>("I2F.F32.U8", value) == CVT<f32, u8>(value));
        REQUIRE(EvalUnary<f32, s16>("I2F.F32.S16", value) == CVT<f32, s16>(value));
        REQUIRE(EvalUnary<f32, u16>("I2F.F32.U16", value) == CVT<f32, u16>(value));
        REQUIRE(EvalUnary<f32, s32>("I2F.F32.S32", value) == value);
        REQUIRE(EvalUnary<f32, u32>("I2F.F32.U32", value) == value);
        REQUIRE(EvalUnary<f32, s64>("I2F.F32.S64", value) == value);
        REQUIRE(EvalUnary<f32, u64>("I2F.F32.U64", value) == value);

        REQUIRE(EvalUnary<f64, s32>("I2F.F64.S32", value) == value);
        REQUIRE(EvalUnary<f64, u32>("I2F.F64.U32", value) == value);
        REQUIRE(EvalUnary<f64, s64>("I2F.F64.S64", value) == value);
        REQUIRE(EvalUnary<f64, u64>("I2F.F64.U64", value) == value);
    }
    REQUIRE(Run<f32>(u16{17}, "I2F.F32.S16 R2, -c[2][0];") == -17);
    REQUIRE(Run<f32>(u16{17}, "I2F.F32.U16 R2, -c[2][0];") == -17);
    REQUIRE(Run<f32>(s16{-17}, "I2F.F32.S16 R2, |c[2][0]|;") == 17);
    REQUIRE(Run<f32>(s32{-17}, "I2F.F32.S32 R2, |c[2][0]|;") == 17);

    REQUIRE(Run<f32>(u32{0xffff'0000}, "I2F.F32.S16 R2, c[2][0].H1;") == -1);
    REQUIRE(Run<f32>(u32{0x0000'ff00}, "I2F.F32.S8 R2, c[2][0].B1;") == -1);
    REQUIRE(Run<f32>(u32{0x00ff'0000}, "I2F.F32.S8 R2, c[2][0].B2;") == -1);
    REQUIRE(Run<f32>(u32{0xff00'0000}, "I2F.F32.S8 R2, c[2][0].B3;") == -1);
    REQUIRE(Run<f32>(u32{0x0000'ff00}, "I2F.F32.U8 R2, c[2][0].B1;") == 255);
    REQUIRE(Run<f32>(u32{0x00ff'0000}, "I2F.F32.U8 R2, c[2][0].B2;") == 255);
    REQUIRE(Run<f32>(u32{0xff00'0000}, "I2F.F32.U8 R2, c[2][0].B3;") == 255);
}

TEST_CASE("I2F Undefined", "[shader][undefined]") {
    REQUIRE(Run<f32>(u16{0x8000}, "I2F.F32.S16 R2, -c[2][0];") == -0x8000);
    REQUIRE(Run<f32>(u32{0x8000'0000}, "I2F.F32.S32 R2, -c[2][0];") == -2147483648.0f);
    REQUIRE(Run<f32>(u32{0x8000'0000}, "I2F.F32.S32 R2, c[2][0];") == -2147483648.0f);
}

TEST_CASE("I2F Rounding", "[shader][cvtrounding]") {
    constexpr u64 precise_int_a = 0xffff'ffff'ffff'ff00;
    REQUIRE(Run<f32>(precise_int_a, "I2F.F32.U64.RN R2, c[2][0];") == 18446744073709551616.0f);
    REQUIRE(Run<f32>(precise_int_a, "I2F.F32.U64.RM R2, c[2][0];") == 18446742974197923840.0f);
    REQUIRE(Run<f32>(precise_int_a, "I2F.F32.U64.RP R2, c[2][0];") == 18446744073709551616.0f);
    REQUIRE(Run<f32>(precise_int_a, "I2F.F32.U64.RZ R2, c[2][0];") == 18446742974197923840.0f);
}
