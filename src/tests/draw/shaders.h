#pragma once

#include <incbin.h>

#include "shader.h"

#ifndef DEFINE_SHADER
#define DEFINE_SHADER(name, path)                                                                  \
    INCBIN_EXTERN(name);                                                                           \
    namespace Shaders {                                                                            \
    static const std::span name(::g##name##Data, ::g##name##Size);                                 \
    }
#endif

DEFINE_SHADER(FullScreenTriangleDksh, "tests/draw/full_screen_triangle.vert")
DEFINE_SHADER(FullScreenTriangleVanillaDksh, "tests/draw/full_screen_triangle_vanilla.vert")
DEFINE_SHADER(VaryingColorDksh, "tests/draw/varying_color.frag")
DEFINE_SHADER(WhiteDksh, "tests/draw/white.frag")