#include <catch2/catch_test_macros.hpp>

#include <deko3d.hpp>

#include "cmd_util.h"
#include "resource.h"
#include "shader.h"

TEST_CASE("Simple FragCoord", "[draw]") {
    RenderTarget2D render_target{DkImageFormat_RGBA8_Unorm, 64, 64};
    auto vert_shader = LoadShader("full_screen_triangle_vert");
    auto frag_shader = LoadShader("flat_frag_coord_32_frag");

    ResetState();
    SetRenderTarget(render_target);
    BindShaders(vert_shader, frag_shader);
    Draw(DkPrimitive_Triangles, 3);

    REQUIRE(render_target.Read<RGBA8U>(30, 31) == (RGBA8U{0x00, 0xff, 0x00, 0xff}));
    REQUIRE(render_target.Read<RGBA8U>(30, 33) == (RGBA8U{0xff, 0x00, 0x00, 0xff}));
}
