#pragma once

#include <algorithm>
#include <cstring>
#include <type_traits>

template <typename T>
requires(std::is_scalar_v<T> || std::is_same_v<T, __fp16>) bool ThresholdCompare(T lhs, T rhs,
                                                                                 T max_diff = {}) {
    T diff{};
    if constexpr (std::is_integral_v<T>) {
        if (__builtin_sub_overflow(lhs, rhs, &diff) && __builtin_sub_overflow(rhs, lhs, &diff)) {
            return false;
        }
        if (diff > max_diff) {
            printf("%d vs %d\n", lhs, rhs);
        }
    } else {
        if (max_diff == T{}) {
            return std::memcmp(&lhs, &rhs, sizeof(T)) == 0;
        }
        if (lhs > rhs) {
            diff = lhs - rhs;
        } else {
            diff = rhs - lhs;
        }
    }
    return diff <= max_diff;
}

template <typename T>
requires(!std::is_scalar_v<T>) bool ThresholdCompare(const T& lhs, const T& rhs,
                                                     typename T::value_type max_diff = {}) {
    using ElementType = typename T::value_type;
    return std::ranges::equal(lhs, rhs, [max_diff](ElementType a, ElementType b) {
        return ThresholdCompare(a, b, max_diff);
    });
}