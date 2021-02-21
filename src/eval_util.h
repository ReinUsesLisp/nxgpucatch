#pragma once

#include <cmath>
#include <cstdint>
#include <string>

#include "cmd_util.h"
#include "heap.h"
#include "shader.h"

namespace EvalUtil {

using f16 = __fp16;
using f32 = float;
using f64 = double;

using u32 = uint32_t;
using s32 = int32_t;
using u64 = uint64_t;
using s64 = int64_t;

enum class InputMode {
    Reg,
    Cbuf,
    Imm,
};

template <typename Type>
std::string ToString(Type value) {
    if constexpr (std::is_floating_point_v<Type> || std::is_same_v<Type, __fp16>) {
        if (std::isinf(value)) {
            return value < 0 ? "-INF" : "INF";
        }
        if (std::isnan(value)) {
            return value < 0 ? "-QNAN" : "QNAN";
        }
    }
    return std::to_string(value);
}

template <typename Result, typename Input, InputMode mode>
std::string TextUnary(const char* operation, Input input = Input{}) {
    std::string text = ".dksh compute\n"
                       "main:\n"
                       "MOV R0, c[0x0][0x140];\n"
                       "MOV R1, c[0x0][0x144];\n"
                       "MOV R2, c[0x2][0x0];\n"
                       "MOV R3, c[0x2][0x4];\n";
    text += operation;
    text += " R2, ";
    if constexpr (mode == InputMode::Reg) {
        text += "R2";
    }
    if constexpr (mode == InputMode::Cbuf) {
        text += "c[2][0]";
    }
    if constexpr (mode == InputMode::Imm) {
        text += ToString(input);
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

inline std::string AbsNeg(std::string_view expr, bool abs, bool neg, bool imm) {
    std::string ret;
    if (neg && !imm) {
        ret += '-';
    }
    if (abs && !imm) {
        ret += '|';
    }
    ret += expr;
    if (abs && !imm) {
        ret += '|';
    }
    if (imm && (abs || neg)) {
        ret += ' ';
        if (neg) {
            ret += ".NEG";
        }
        if (abs) {
            ret += ".ABS";
        }
    }
    return ret;
}

template <typename Result, typename Input, InputMode mode>
std::string TextBinary(const char* operation, Input input = Input{}, bool neg_a = false,
                       bool abs_a = false, bool neg_b = false, bool abs_b = false) {
    std::string text = ".dksh compute\n"
                       "main:\n"
                       "MOV R0, c[0x0][0x140];\n"
                       "MOV R1, c[0x0][0x144];\n"
                       "MOV R2, c[0x2][0x0];\n"
                       "MOV R3, c[0x2][0x4];\n"
                       "MOV R4, c[0x2][0x8];\n"
                       "MOV R5, c[0x2][0xc];\n";
    text += operation;
    text += " R2, ";
    text += AbsNeg("R2", abs_a, neg_a, false);
    text += ", ";
    if constexpr (mode == InputMode::Reg) {
        text += AbsNeg("R4", abs_b, neg_b, false);
    }
    if constexpr (mode == InputMode::Cbuf) {
        text += AbsNeg("c[0x2][0x8]", abs_b, neg_b, false);
    }
    if constexpr (mode == InputMode::Imm) {
        text += AbsNeg(ToString(input), abs_b, neg_b, true);
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

template <typename Result, typename InputData>
[[nodiscard]] Result Run(std::string code, InputData data) {
    TypedHeap<InputData> input_heap;
    TypedHeap<Result> output_heap;
    *input_heap = data;

    Shader shader(code.c_str());
    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        input_heap.BindUniform(cmdbuf, DkStage_Compute, 0);
        output_heap.BindStorage(cmdbuf, DkStage_Compute);
        shader.Bind(cmdbuf, DkStageFlag_Compute);
        cmdbuf.dispatchCompute(1, 1, 1);
    });
    return *output_heap;
}

template <typename Type>
void RequireEqual(Type a, Type b) {
    if (std::is_floating_point_v<Type> || std::is_same_v<Type, f16>) {
        if (std::isnan(a)) {
            REQUIRE(std::isnan(a) == std::isnan(b));
        } else {
            REQUIRE(a == b);
        }
    } else {
        REQUIRE(a == b);
    }
}

template <typename Result, typename Input>
Result EvalUnary(const char* operation, Input value) {
    Result reg_res = Run<Result>(TextUnary<Result, Input, InputMode::Reg>(operation), value);
    Result cbuf_res = Run<Result>(TextUnary<Result, Input, InputMode::Cbuf>(operation), value);

    RequireEqual(reg_res, cbuf_res);
    return reg_res;
}

template <typename Result, typename Input>
Result EvalBinary(const char* operation, Input op_a, Input op_b, bool neg_a = false,
                  bool abs_a = false, bool neg_b = false, bool abs_b = false) {
    struct Pack {
        alignas(8) Input op_a;
        alignas(8) Input op_b;
    } value{op_a, op_b};

    Result reg_res = Run<Result, Pack>(
        TextBinary<Result, Input, InputMode::Reg>(operation, {}, neg_a, abs_a, neg_b, abs_b),
        value);
    Result cbuf_res = Run<Result, Pack>(
        TextBinary<Result, Input, InputMode::Cbuf>(operation, {}, neg_a, abs_a, neg_b, abs_b),
        value);

    RequireEqual(reg_res, cbuf_res);
    return reg_res;
}

template <typename Result, typename Input>
Result EvalUnaryImm(const char* operation, Input value) {
    Result reg_res = EvalUnary<Result>(operation, value);
    Result cbuf_res = Run<Result>(TextUnary<Result, Input, InputMode::Cbuf>(operation), value);
    Result imm_res = Run<Result>(TextUnary<Result, Input, InputMode::Imm>(operation), value);

    RequireEqual(reg_res, cbuf_res);
    RequireEqual(reg_res, imm_res);
    return reg_res;
}

template <typename Result, typename Input>
Result EvalBinaryImm(const char* operation, Input op_a, Input op_b, bool neg_a = false,
                     bool abs_a = false, bool neg_b = false, bool abs_b = false) {
    struct Pack {
        alignas(8) Input op_a;
        alignas(8) Input op_b;
    } const value{op_a, op_b};

    Result reg_res = Run<Result>(
        TextBinary<Result, Input, InputMode::Reg>(operation, {}, neg_a, abs_a, neg_b, abs_b),
        value);
    Result cbuf_res = Run<Result>(
        TextBinary<Result, Input, InputMode::Cbuf>(operation, {}, neg_a, abs_a, neg_b, abs_b),
        value);
    Result imm_res = Run<Result>(
        TextBinary<Result, Input, InputMode::Imm>(operation, op_b, neg_a, abs_a, neg_b, abs_b),
        value);

    RequireEqual(reg_res, cbuf_res);
    RequireEqual(reg_res, imm_res);
    return reg_res;
}

} // namespace EvalUtil
