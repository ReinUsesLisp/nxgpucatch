#pragma once

#include "eval_util.h"

namespace Fp16Run {

using u32 = uint32_t;
using f16 = __fp16;
using f16x2 = std::array<f16, 2>;

inline u32 Run(u32 lhs, u32 rhs, std::string code, f16x2 base = {}) {
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
                         std::array{BitCast<u32>(base), lhs, rhs});
}

inline u32 Run(u32 a, u32 b, u32 c, std::string code, f16x2 base = {}) {
    return EvalUtil::Run(".dksh compute\n"
                         "main:\n"
                         "MOV R0, c[0x0][0x140];\n"
                         "MOV R1, c[0x0][0x144];\n"
                         "MOV R2, c[2][0];"
                         "MOV R3, c[2][4];"
                         "MOV R4, c[2][8];"
                         "MOV R5, c[2][12];" +
                             code +
                             "STG.E [R0], R2;\n"
                             "EXIT;\n",
                         std::array{BitCast<u32>(base), a, b, c});
}

inline f16x2 RunF16x2(float lhs, float rhs, std::string code, f16x2 base = {}) {
    return BitCast<f16x2>(Run(BitCast<u32>(lhs), BitCast<u32>(rhs), std::move(code), base));
}

inline f16x2 RunF16x2(f16x2 lhs, float rhs, std::string code, f16x2 base = {}) {
    return BitCast<f16x2>(Run(BitCast<u32>(lhs), BitCast<u32>(rhs), std::move(code), base));
}

inline f16x2 RunF16x2(float lhs, f16x2 rhs, std::string code, f16x2 base = {}) {
    return BitCast<f16x2>(Run(BitCast<u32>(lhs), BitCast<u32>(rhs), std::move(code), base));
}

inline f16x2 RunF16x2(f16x2 lhs, f16x2 rhs, std::string code, f16x2 base = {}) {
    return BitCast<f16x2>(Run(BitCast<u32>(lhs), BitCast<u32>(rhs), std::move(code), base));
}

inline float RunF32(float lhs, float rhs, std::string code, f16x2 base = {}) {
    return BitCast<float>(Run(BitCast<u32>(lhs), BitCast<u32>(rhs), std::move(code), base));
}

inline float RunF32(f16x2 lhs, float rhs, std::string code, f16x2 base = {}) {
    return BitCast<float>(Run(BitCast<u32>(lhs), BitCast<u32>(rhs), std::move(code), base));
}

inline float RunF32(float lhs, f16x2 rhs, std::string code, f16x2 base = {}) {
    return BitCast<float>(Run(BitCast<u32>(lhs), BitCast<u32>(rhs), std::move(code), base));
}

inline float RunF32(f16x2 lhs, f16x2 rhs, std::string code, f16x2 base = {}) {
    return BitCast<float>(Run(BitCast<u32>(lhs), BitCast<u32>(rhs), std::move(code), base));
}

inline f16x2 RunF16x2(float a, float b, float c, std::string code, f16x2 base = {}) {
    return BitCast<f16x2>(
        Run(BitCast<u32>(a), BitCast<u32>(b), BitCast<u32>(c), std::move(code), base));
}

inline f16x2 RunF16x2(f16x2 a, float b, float c, std::string code, f16x2 base = {}) {
    return BitCast<f16x2>(
        Run(BitCast<u32>(a), BitCast<u32>(b), BitCast<u32>(c), std::move(code), base));
}

inline f16x2 RunF16x2(float a, f16x2 b, float c, std::string code, f16x2 base = {}) {
    return BitCast<f16x2>(
        Run(BitCast<u32>(a), BitCast<u32>(b), BitCast<u32>(c), std::move(code), base));
}

inline f16x2 RunF16x2(f16x2 a, f16x2 b, float c, std::string code, f16x2 base = {}) {
    return BitCast<f16x2>(
        Run(BitCast<u32>(a), BitCast<u32>(b), BitCast<u32>(c), std::move(code), base));
}

inline f16x2 RunF16x2(float a, float b, f16x2 c, std::string code, f16x2 base = {}) {
    return BitCast<f16x2>(
        Run(BitCast<u32>(a), BitCast<u32>(b), BitCast<u32>(c), std::move(code), base));
}

inline f16x2 RunF16x2(f16x2 a, float b, f16x2 c, std::string code, f16x2 base = {}) {
    return BitCast<f16x2>(
        Run(BitCast<u32>(a), BitCast<u32>(b), BitCast<u32>(c), std::move(code), base));
}

inline f16x2 RunF16x2(float a, f16x2 b, f16x2 c, std::string code, f16x2 base = {}) {
    return BitCast<f16x2>(
        Run(BitCast<u32>(a), BitCast<u32>(b), BitCast<u32>(c), std::move(code), base));
}

inline f16x2 RunF16x2(f16x2 a, f16x2 b, f16x2 c, std::string code, f16x2 base = {}) {
    return BitCast<f16x2>(
        Run(BitCast<u32>(a), BitCast<u32>(b), BitCast<u32>(c), std::move(code), base));
}

inline float RunF32(float a, float b, float c, std::string code, f16x2 base = {}) {
    return BitCast<float>(
        Run(BitCast<u32>(a), BitCast<u32>(b), BitCast<u32>(c), std::move(code), base));
}

inline float RunF32(f16x2 a, float b, float c, std::string code, f16x2 base = {}) {
    return BitCast<float>(
        Run(BitCast<u32>(a), BitCast<u32>(b), BitCast<u32>(c), std::move(code), base));
}

inline float RunF32(float a, f16x2 b, float c, std::string code, f16x2 base = {}) {
    return BitCast<float>(
        Run(BitCast<u32>(a), BitCast<u32>(b), BitCast<u32>(c), std::move(code), base));
}

inline float RunF32(f16x2 a, f16x2 b, float c, std::string code, f16x2 base = {}) {
    return BitCast<float>(
        Run(BitCast<u32>(a), BitCast<u32>(b), BitCast<u32>(c), std::move(code), base));
}

inline float RunF32(float a, float b, f16x2 c, std::string code, f16x2 base = {}) {
    return BitCast<float>(
        Run(BitCast<u32>(a), BitCast<u32>(b), BitCast<u32>(c), std::move(code), base));
}

inline float RunF32(f16x2 a, float b, f16x2 c, std::string code, f16x2 base = {}) {
    return BitCast<float>(
        Run(BitCast<u32>(a), BitCast<u32>(b), BitCast<u32>(c), std::move(code), base));
}

inline float RunF32(float a, f16x2 b, f16x2 c, std::string code, f16x2 base = {}) {
    return BitCast<float>(
        Run(BitCast<u32>(a), BitCast<u32>(b), BitCast<u32>(c), std::move(code), base));
}

inline float RunF32(f16x2 a, f16x2 b, f16x2 c, std::string code, f16x2 base = {}) {
    return BitCast<float>(
        Run(BitCast<u32>(a), BitCast<u32>(b), BitCast<u32>(c), std::move(code), base));
}

} // namespace Fp16Run
