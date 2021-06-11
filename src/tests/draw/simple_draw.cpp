#include <catch2/catch_test_macros.hpp>

#include <deko3d.hpp>

#include <incbin.h>

#include "cmd_util.h"
#include "resource.h"
#include "shader.h"

INCLUDE_SHADER(FullScreenTriangleDksh, "tests/draw/full_screen_triangle.vert");
INCLUDE_SHADER(WhiteDksh, "tests/draw/white.frag");

TEST_CASE("Simple", "[draw]") {
    RenderTarget2D render_target{DkImageFormat_RGBA8_Unorm, 64, 64};
    Shader vert_shader(FullScreenTriangleDksh);
    Shader frag_shader(WhiteDksh);

    ResetState();
    SetRenderTarget(render_target);
    BindShaders(vert_shader, frag_shader);
    Draw(DkPrimitive_Triangles, 3);
    
    REQUIRE(render_target.Read<RGBA8U>(30, 30) == (RGBA8U{0xff, 0xff, 0xff, 0xff}));
}
