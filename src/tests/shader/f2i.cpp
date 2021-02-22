#include <catch2/catch_test_macros.hpp>

#include "eval_util.h"

using namespace EvalUtil;

TEST_CASE("F2I Simple", "[shader]") {
    REQUIRE(EvalUnary<u32, f16>("F2I.FTZ.U16.F16", 5.0) == 5);
    REQUIRE(EvalUnary<s32, f16>("F2I.FTZ.S16.F16", 7.0) == 7);
    REQUIRE(EvalUnary<u32, f16>("F2I.FTZ.U32.F16", 5.0) == 5);
    REQUIRE(EvalUnary<s32, f16>("F2I.FTZ.S32.F16", 2.0) == 2);
    // F16->U64 aborts execution
    // F16->S64 aborts execution

    REQUIRE(EvalUnary<u32, f32>("F2I.FTZ.U16.F32", 5.0) == 5);
    REQUIRE(EvalUnary<s32, f32>("F2I.FTZ.S16.F32", 7.0) == 7);
    REQUIRE(EvalUnary<u32, f32>("F2I.FTZ.U32.F32", 5.0) == 5);
    REQUIRE(EvalUnary<s32, f32>("F2I.FTZ.S32.F32", 2.0) == 2);
    REQUIRE(EvalUnary<u64, f32>("F2I.FTZ.U64.F32", 6.0) == 6);
    REQUIRE(EvalUnary<s64, f32>("F2I.FTZ.S64.F32", 2.0) == 2);

    // F64->U16 aborts execution
    // F64->S16 aborts execution
    REQUIRE(EvalUnary<u32, f64>("F2I.FTZ.U32.F64", 5.0) == 5);
    REQUIRE(EvalUnary<s32, f64>("F2I.FTZ.S32.F64", 2.0) == 2);
    REQUIRE(EvalUnary<u64, f64>("F2I.FTZ.U64.F64", 6.0) == 6);
    REQUIRE(EvalUnary<s64, f64>("F2I.FTZ.S64.F64", 2.0) == 2);
}

TEST_CASE("F2I Modes", "[shader]") {
    REQUIRE(EvalUnary<s32, f16>("F2I.FTZ.S32.F16", 1.5) == 2);
    REQUIRE(EvalUnary<s32, f32>("F2I.FTZ.S32.F32", 1.5) == 2);
    REQUIRE(EvalUnary<s32, f64>("F2I.FTZ.S32.F64", 1.5) == 2);

    REQUIRE(EvalUnary<s32, f16>("F2I.FTZ.S32.F16", 2.5) == 2);
    REQUIRE(EvalUnary<s32, f32>("F2I.FTZ.S32.F32", 2.5) == 2);
    REQUIRE(EvalUnary<s32, f64>("F2I.FTZ.S32.F64", 2.5) == 2);

    REQUIRE(EvalUnary<s32, f16>("F2I.FTZ.S32.F16", -1.5) == -2);
    REQUIRE(EvalUnary<s32, f32>("F2I.FTZ.S32.F32", -1.5) == -2);
    REQUIRE(EvalUnary<s32, f64>("F2I.FTZ.S32.F64", -1.5) == -2);

    REQUIRE(EvalUnary<s32, f16>("F2I.FTZ.S32.F16.FLOOR", 1.8) == 1);
    REQUIRE(EvalUnary<s32, f32>("F2I.FTZ.S32.F32.FLOOR", 1.8) == 1);
    REQUIRE(EvalUnary<s32, f64>("F2I.FTZ.S32.F64.FLOOR", 1.8) == 1);

    REQUIRE(EvalUnary<s32, f16>("F2I.FTZ.S32.F16.FLOOR", -1.8) == -2);
    REQUIRE(EvalUnary<s32, f32>("F2I.FTZ.S32.F32.FLOOR", -1.8) == -2);
    REQUIRE(EvalUnary<s32, f64>("F2I.FTZ.S32.F64.FLOOR", -1.8) == -2);

    REQUIRE(EvalUnary<s32, f16>("F2I.FTZ.S32.F16.CEIL", 1.8) == 2);
    REQUIRE(EvalUnary<s32, f32>("F2I.FTZ.S32.F32.CEIL", 1.8) == 2);
    REQUIRE(EvalUnary<s32, f64>("F2I.FTZ.S32.F64.CEIL", 1.8) == 2);

    REQUIRE(EvalUnary<s32, f16>("F2I.FTZ.S32.F16.CEIL", -1.8) == -1);
    REQUIRE(EvalUnary<s32, f32>("F2I.FTZ.S32.F32.CEIL", -1.8) == -1);
    REQUIRE(EvalUnary<s32, f64>("F2I.FTZ.S32.F64.CEIL", -1.8) == -1);

    REQUIRE(EvalUnary<s32, f16>("F2I.FTZ.S32.F16.TRUNC", 1.8) == 1);
    REQUIRE(EvalUnary<s32, f32>("F2I.FTZ.S32.F32.TRUNC", 1.8) == 1);
    REQUIRE(EvalUnary<s32, f64>("F2I.FTZ.S32.F64.TRUNC", 1.8) == 1);

    REQUIRE(EvalUnary<s32, f16>("F2I.FTZ.S32.F16.TRUNC", -1.8) == -1);
    REQUIRE(EvalUnary<s32, f32>("F2I.FTZ.S32.F32.TRUNC", -1.8) == -1);
    REQUIRE(EvalUnary<s32, f64>("F2I.FTZ.S32.F64.TRUNC", -1.8) == -1);
}

TEST_CASE("F2I Clamp", "[shader]") {
    REQUIRE(EvalUnary<u32, f32>("F2I.FTZ.U16.F32", f32{0x1'0000} + 1) == 0xffff);
    REQUIRE(EvalUnary<u32, f32>("F2I.FTZ.U16.F32", -f32{0x1'0000}) == 0);

    REQUIRE(EvalUnary<u32, f32>("F2I.FTZ.S16.F32", f32{0x1'0000}) == 0x0000'7fff);
    REQUIRE(EvalUnary<u32, f32>("F2I.FTZ.S16.F32", -f32{0x1'0000}) == 0xffff'8000);

    REQUIRE(EvalUnary<u32, f32>("F2I.FTZ.U32.F32", f32{0x1'0000'0000}) == 0xffff'ffff);
    REQUIRE(EvalUnary<u32, f32>("F2I.FTZ.U32.F32", -f32{0x1'0000'0000}) == 0);

    REQUIRE(EvalUnary<u32, f32>("F2I.FTZ.S32.F32", f32{0x1'0000'0000}) == 0x7fff'ffff);
    REQUIRE(EvalUnary<u32, f32>("F2I.FTZ.S32.F32", -f32{0x1'0000'0000}) == 0x8000'0000);

    REQUIRE(EvalUnary<u64, f32>("F2I.FTZ.U64.F32", 1.8446744e+19) == 0xffff'ffff'ffff'ffffull);
    REQUIRE(EvalUnary<u64, f32>("F2I.FTZ.U64.F32", -1.8446744e+19) == 0);

    REQUIRE(EvalUnary<u64, f32>("F2I.FTZ.S64.F32", 1.8446744e+19) == 0x7fff'ffff'ffff'ffffull);
    REQUIRE(EvalUnary<u64, f32>("F2I.FTZ.S64.F32", -1.8446744e+19) == 0x8000'0000'0000'0000ull);
}

TEST_CASE("F2I NAN", "[shader]") {
    static constexpr f32 f32_qnan{std::numeric_limits<f32>::quiet_NaN()};
    static constexpr f64 f64_qnan{std::numeric_limits<f64>::quiet_NaN()};

    // If src_format == U64 || src_format == U64, NAN gets converted into a special value
    // otherwise it returns zero
    REQUIRE(EvalUnary<u64, f32>("F2I.FTZ.U64.F32", f32_qnan) == 0x8000'0000'0000'0000);
    REQUIRE(EvalUnary<u64, f32>("F2I.FTZ.S64.F32", f32_qnan) == 0x8000'0000'0000'0000);
    REQUIRE(EvalUnary<u32, f32>("F2I.FTZ.U32.F32", f32_qnan) == 0);
    REQUIRE(EvalUnary<u32, f32>("F2I.FTZ.S32.F32", f32_qnan) == 0);
    REQUIRE(EvalUnary<u32, f32>("F2I.FTZ.U16.F32", f32_qnan) == 0);
    REQUIRE(EvalUnary<u32, f32>("F2I.FTZ.S16.F32", f32_qnan) == 0);

    // If dest_format is F64, it also returns the special value
    // but only when the result is U32 and S32
    REQUIRE(EvalUnary<u32, f64>("F2I.FTZ.U32.F64", f64_qnan) == 0x8000'0000);
    REQUIRE(EvalUnary<u32, f64>("F2I.FTZ.S32.F64", f64_qnan) == 0x8000'0000);
    REQUIRE(EvalUnary<u32, f64>("F2I.FTZ.U64.F64", f64_qnan) == 0);
    REQUIRE(EvalUnary<u32, f64>("F2I.FTZ.S64.F64", f64_qnan) == 0);
    REQUIRE(EvalUnary<u32, f64>("F2I.FTZ.U16.F64", f64_qnan) == 0);
    REQUIRE(EvalUnary<u32, f64>("F2I.FTZ.S16.F64", f64_qnan) == 0);
}

TEST_CASE("F2I Denorm", "[shader][fpcontrol]") {
    static constexpr f32 denorm{1e-40};
    static_assert(denorm > 0);
    REQUIRE(EvalUnary<s32, f32>("F2I.FTZ.S32.F32.CEIL", denorm) == 0);
    REQUIRE(EvalUnary<s32, f32>("F2I.S32.F32.CEIL", denorm) == 1);

    REQUIRE(EvalUnary<s32, f32>("F2I.FTZ.S32.F32.FLOOR", -denorm) == 0);
    REQUIRE(EvalUnary<s32, f32>("F2I.S32.F32.FLOOR", -denorm) == -1);
}
