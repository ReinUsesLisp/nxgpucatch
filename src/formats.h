#pragma once

#include <array>
#include <cstdint>

using R8U = std::array<uint8_t, 1>;
using R8I = std::array<int8_t, 1>;
using R16F = std::array<__fp16, 1>;
using R16U = std::array<uint16_t, 1>;
using R16I = std::array<int16_t, 1>;
using R32F = std::array<float, 1>;
using R32U = std::array<uint32_t, 1>;
using R32I = std::array<int32_t, 1>;
using RG8U = std::array<uint8_t, 2>;
using RG8I = std::array<int8_t, 2>;
using RG16F = std::array<__fp16, 2>;
using RG16U = std::array<uint16_t, 2>;
using RG16I = std::array<int16_t, 2>;
using RG32F = std::array<float, 2>;
using RG32U = std::array<uint32_t, 2>;
using RG32I = std::array<int32_t, 2>;
using RGB32F = std::array<float, 3>;
using RGB32U = std::array<uint32_t, 3>;
using RGB32I = std::array<int32_t, 3>;
using RGBA8U = std::array<uint8_t, 4>;
using RGBA8I = std::array<int8_t, 4>;
using RGBA16F = std::array<__fp16, 4>;
using RGBA16U = std::array<uint16_t, 4>;
using RGBA16I = std::array<int16_t, 4>;
using RGBA32F = std::array<float, 4>;
using RGBA32U = std::array<uint32_t, 4>;
using RGBA32I = std::array<int32_t, 4>;

struct RGB10A2 {
    uint32_t r : 10;
    uint32_t g : 10;
    uint32_t b : 10;
    uint32_t a : 2;
};

struct RG11B10 {
    uint32_t r : 11;
    uint32_t g : 11;
    uint32_t b : 10;
};

struct BGR565 {
    uint16_t b : 5;
    uint16_t g : 6;
    uint16_t r : 5;
};

struct BGR5A1 {
    uint16_t b : 5;
    uint16_t g : 5;
    uint16_t r : 5;
    uint16_t a : 1;
};
