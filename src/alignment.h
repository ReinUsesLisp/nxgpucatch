#pragma once

#include <cstddef>
#include <type_traits>

template <typename T>
requires std::is_unsigned_v<T>[[nodiscard]] constexpr T AlignUp(T value, size_t size) {
    auto mod{static_cast<T>(value % size)};
    value -= mod;
    return static_cast<T>(mod == T{0} ? value : value + size);
}

template <typename T>
requires std::is_unsigned_v<T>[[nodiscard]] constexpr T AlignUpLog2(T value, size_t align_log2) {
    return static_cast<T>((value + ((1ULL << align_log2) - 1)) >> align_log2 << align_log2);
}

template <typename T>
requires std::is_unsigned_v<T>[[nodiscard]] constexpr T AlignDown(T value, size_t size) {
    return static_cast<T>(value - value % size);
}

template <typename T>
requires std::is_unsigned_v<T>[[nodiscard]] constexpr bool Is4KBAligned(T value) {
    return (value & 0xFFF) == 0;
}

template <typename T>
requires std::is_unsigned_v<T>[[nodiscard]] constexpr bool IsWordAligned(T value) {
    return (value & 0b11) == 0;
}

template <typename T>
requires std::is_integral_v<T>[[nodiscard]] constexpr bool IsAligned(T value, size_t alignment) {
    using U = typename std::make_unsigned_t<T>;
    const U mask = static_cast<U>(alignment - 1);
    return (value & mask) == 0;
}
