#pragma once

#include <cstring>
#include <type_traits>

template <typename Dest, typename Source>
    requires std::is_trivially_copyable_v<Dest>&& std::is_trivially_copyable_v<Source> &&
    (sizeof(Dest) == sizeof(Source)) Dest BitCast(const Source& src) {
    Dest dest;
    std::memcpy(&dest, &src, sizeof(dest));
    return dest;
}
