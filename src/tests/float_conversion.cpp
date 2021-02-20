#include <array>

#include <catch2/catch_test_macros.hpp>

#include "../bit_cast.h"
#include "../cmd_util.h"
#include "../compare.h"
#include "../device.h"
#include "../heap.h"
#include "../resource.h"
#include "../shader.h"

using f16 = __fp16;
using f32 = float;
using f64 = double;

using u32 = uint32_t;
using s32 = int32_t;
using u64 = uint64_t;
using s64 = int64_t;

namespace {
enum class InputMode {
    Reg,
    Cbuf,
};
} // Anonymous namespace

template <typename Result, typename Input>
static std::string Text(const char* operation, InputMode mode) {
    std::string text = ".dksh compute\n"
                       "main:\n"
                       "MOV R0, c[0x0][0x140];\n"
                       "MOV R1, c[0x0][0x144];\n"
                       "MOV R2, c[0x2][0x0];\n"
                       "MOV R3, c[0x2][0x4];\n";
    text += operation;
    text += " R2, ";
    switch (mode) {
    case InputMode::Reg:
        text += "R2";
        break;
    case InputMode::Cbuf:
        text += "c[2][0]";
        break;
    }
    text += ";\n";
    if constexpr (sizeof(Result) == 8) {
        text += "STG.E.64 [R0], R2;\n";
    } else {
        text += "STG.E [R0], R2;\n";
    }
    text += "EXIT;\n";
    return text;
}

template <typename Result, typename Input>
static Result Eval(const char* operation, Input value) {
    TypedHeap<Input> input_heap;
    *input_heap = value;

    // Register mode setup
    TypedHeap<Result> reg_heap;
    Shader reg_shader(Text<Result, Input>(operation, InputMode::Reg).c_str());
    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        input_heap.BindUniform(cmdbuf, DkStage_Compute, 0);
        reg_heap.BindStorage(cmdbuf, DkStage_Compute);
        reg_shader.Bind(cmdbuf, DkStageFlag_Compute);
        cmdbuf.dispatchCompute(1, 1, 1);
    });

    // Constant buffer setup
    TypedHeap<Result> cbuf_heap;
    Shader cbuf_shader(Text<Result, Input>(operation, InputMode::Cbuf).c_str());
    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        cbuf_heap.BindStorage(cmdbuf, DkStage_Compute);
        cbuf_shader.Bind(cmdbuf, DkStageFlag_Compute);
        cmdbuf.dispatchCompute(1, 1, 1);
    });

    REQUIRE(*reg_heap == *cbuf_heap);
    return *reg_heap;
}

TEST_CASE("F2I base", "[float_conversion]") {
    REQUIRE(Eval<u32, f16>("F2I.FTZ.U16.F16", 5.0) == 5);
    REQUIRE(Eval<s32, f16>("F2I.FTZ.S16.F16", 7.0) == 7);
    REQUIRE(Eval<u32, f16>("F2I.FTZ.U32.F16", 5.0) == 5);
    REQUIRE(Eval<s32, f16>("F2I.FTZ.S32.F16", 2.0) == 2);
    // F16->U64 aborts execution
    // F16->S64 aborts execution

    REQUIRE(Eval<u32, f32>("F2I.FTZ.U16.F32", 5.0) == 5);
    REQUIRE(Eval<s32, f32>("F2I.FTZ.S16.F32", 7.0) == 7);
    REQUIRE(Eval<u32, f32>("F2I.FTZ.U32.F32", 5.0) == 5);
    REQUIRE(Eval<s32, f32>("F2I.FTZ.S32.F32", 2.0) == 2);
    REQUIRE(Eval<u64, f32>("F2I.FTZ.U64.F32", 6.0) == 6);
    REQUIRE(Eval<s64, f32>("F2I.FTZ.S64.F32", 2.0) == 2);

    // F64->U16 aborts execution
    // F64->S16 aborts execution
    REQUIRE(Eval<u32, f64>("F2I.FTZ.U32.F64", 5.0) == 5);
    REQUIRE(Eval<s32, f64>("F2I.FTZ.S32.F64", 2.0) == 2);
    REQUIRE(Eval<u64, f64>("F2I.FTZ.U64.F64", 6.0) == 6);
    REQUIRE(Eval<s64, f64>("F2I.FTZ.S64.F64", 2.0) == 2);
}

TEST_CASE("F2I rounding", "[float_conversion]") {
    REQUIRE(Eval<s32, f16>("F2I.FTZ.S32.F16", 1.5) == 2);
    REQUIRE(Eval<s32, f32>("F2I.FTZ.S32.F32", 1.5) == 2);
    REQUIRE(Eval<s32, f64>("F2I.FTZ.S32.F64", 1.5) == 2);

    REQUIRE(Eval<s32, f16>("F2I.FTZ.S32.F16", 2.5) == 2);
    REQUIRE(Eval<s32, f32>("F2I.FTZ.S32.F32", 2.5) == 2);
    REQUIRE(Eval<s32, f64>("F2I.FTZ.S32.F64", 2.5) == 2);

    REQUIRE(Eval<s32, f16>("F2I.FTZ.S32.F16", -1.5) == -2);
    REQUIRE(Eval<s32, f32>("F2I.FTZ.S32.F32", -1.5) == -2);
    REQUIRE(Eval<s32, f64>("F2I.FTZ.S32.F64", -1.5) == -2);

    REQUIRE(Eval<s32, f16>("F2I.FTZ.S32.F16.FLOOR", 1.8) == 1);
    REQUIRE(Eval<s32, f32>("F2I.FTZ.S32.F32.FLOOR", 1.8) == 1);
    REQUIRE(Eval<s32, f64>("F2I.FTZ.S32.F64.FLOOR", 1.8) == 1);

    REQUIRE(Eval<s32, f16>("F2I.FTZ.S32.F16.FLOOR", -1.8) == -2);
    REQUIRE(Eval<s32, f32>("F2I.FTZ.S32.F32.FLOOR", -1.8) == -2);
    REQUIRE(Eval<s32, f64>("F2I.FTZ.S32.F64.FLOOR", -1.8) == -2);

    REQUIRE(Eval<s32, f16>("F2I.FTZ.S32.F16.CEIL", 1.8) == 2);
    REQUIRE(Eval<s32, f32>("F2I.FTZ.S32.F32.CEIL", 1.8) == 2);
    REQUIRE(Eval<s32, f64>("F2I.FTZ.S32.F64.CEIL", 1.8) == 2);

    REQUIRE(Eval<s32, f16>("F2I.FTZ.S32.F16.CEIL", -1.8) == -1);
    REQUIRE(Eval<s32, f32>("F2I.FTZ.S32.F32.CEIL", -1.8) == -1);
    REQUIRE(Eval<s32, f64>("F2I.FTZ.S32.F64.CEIL", -1.8) == -1);

    REQUIRE(Eval<s32, f16>("F2I.FTZ.S32.F16.TRUNC", 1.8) == 1);
    REQUIRE(Eval<s32, f32>("F2I.FTZ.S32.F32.TRUNC", 1.8) == 1);
    REQUIRE(Eval<s32, f64>("F2I.FTZ.S32.F64.TRUNC", 1.8) == 1);

    REQUIRE(Eval<s32, f16>("F2I.FTZ.S32.F16.TRUNC", -1.8) == -1);
    REQUIRE(Eval<s32, f32>("F2I.FTZ.S32.F32.TRUNC", -1.8) == -1);
    REQUIRE(Eval<s32, f64>("F2I.FTZ.S32.F64.TRUNC", -1.8) == -1);
}

TEST_CASE("F2I clamp") {
    REQUIRE(Eval<u32, f32>("F2I.FTZ.U16.F32", f32{0x1'0000} + 1) == 0xffff);
    REQUIRE(Eval<u32, f32>("F2I.FTZ.U16.F32", -f32{0x1'0000}) == 0);

    REQUIRE(Eval<u32, f32>("F2I.FTZ.S16.F32", f32{0x1'0000}) == 0x0000'7fff);
    REQUIRE(Eval<u32, f32>("F2I.FTZ.S16.F32", -f32{0x1'0000}) == 0xffff'8000);

    REQUIRE(Eval<u32, f32>("F2I.FTZ.U32.F32", f32{0x1'0000'0000}) == 0xffff'ffff);
    REQUIRE(Eval<u32, f32>("F2I.FTZ.U32.F32", -f32{0x1'0000'0000}) == 0);

    REQUIRE(Eval<u32, f32>("F2I.FTZ.S32.F32", f32{0x1'0000'0000}) == 0x7fff'ffff);
    REQUIRE(Eval<u32, f32>("F2I.FTZ.S32.F32", -f32{0x1'0000'0000}) == 0x8000'0000);

    REQUIRE(Eval<u64, f32>("F2I.FTZ.U64.F32", 1.8446744e+19) == 0xffff'ffff'ffff'ffffull);
    REQUIRE(Eval<u64, f32>("F2I.FTZ.U64.F32", -1.8446744e+19) == 0);

    REQUIRE(Eval<u64, f32>("F2I.FTZ.S64.F32", 1.8446744e+19) == 0x7fff'ffff'ffff'ffffull);
    REQUIRE(Eval<u64, f32>("F2I.FTZ.S64.F32", -1.8446744e+19) == 0x8000'0000'0000'0000ull);
}

TEST_CASE("F2I FTZ") {
    static constexpr f32 denorm{1e-40};
    static_assert(denorm > 0);
    REQUIRE(Eval<s32, f32>("F2I.FTZ.S32.F32.CEIL", denorm) == 0);
    REQUIRE(Eval<s32, f32>("F2I.S32.F32.CEIL", denorm) == 1);

    REQUIRE(Eval<s32, f32>("F2I.FTZ.S32.F32.FLOOR", -denorm) == 0);
    REQUIRE(Eval<s32, f32>("F2I.S32.F32.FLOOR", -denorm) == -1);
}

TEST_CASE("F2I NAN") {
    static constexpr f32 f32_qnan{std::numeric_limits<f32>::quiet_NaN()};
    static constexpr f64 f64_qnan{std::numeric_limits<f64>::quiet_NaN()};

    // If src_format == U64 || src_format == U64, NAN gets converted into a special value
    // otherwise it returns zero
    REQUIRE(Eval<u64, f32>("F2I.FTZ.U64.F32", f32_qnan) == 0x8000'0000'0000'0000);
    REQUIRE(Eval<u64, f32>("F2I.FTZ.S64.F32", f32_qnan) == 0x8000'0000'0000'0000);
    REQUIRE(Eval<u32, f32>("F2I.FTZ.U32.F32", f32_qnan) == 0);
    REQUIRE(Eval<u32, f32>("F2I.FTZ.S32.F32", f32_qnan) == 0);
    REQUIRE(Eval<u32, f32>("F2I.FTZ.U16.F32", f32_qnan) == 0);
    REQUIRE(Eval<u32, f32>("F2I.FTZ.S16.F32", f32_qnan) == 0);

    // If dest_format is F64, it also returns the special value
    // but only when the result is U32 and S32
    REQUIRE(Eval<u32, f64>("F2I.FTZ.U32.F64", f64_qnan) == 0x8000'0000);
    REQUIRE(Eval<u32, f64>("F2I.FTZ.S32.F64", f64_qnan) == 0x8000'0000);
    REQUIRE(Eval<u32, f64>("F2I.FTZ.U64.F64", f64_qnan) == 0);
    REQUIRE(Eval<u32, f64>("F2I.FTZ.S64.F64", f64_qnan) == 0);
    REQUIRE(Eval<u32, f64>("F2I.FTZ.U16.F64", f64_qnan) == 0);
    REQUIRE(Eval<u32, f64>("F2I.FTZ.S16.F64", f64_qnan) == 0);
}
