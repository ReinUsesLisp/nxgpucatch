#include <catch2/catch_test_macros.hpp>

#include <deko3d.hpp>

#include <incbin.h>

#include "cmd_util.h"
#include "resource.h"
#include "shaders.h"

TEST_CASE("Simple", "[draw]") {
    RenderTarget2D render_target{DkImageFormat_RGBA8_Unorm, 64, 64};
    Shader vert_shader(Shaders::FullScreenTriangleDksh);
    Shader frag_shader(Shaders::WhiteDksh);

    ResetState();
    SetRenderTarget(render_target);
    BindShaders(vert_shader, frag_shader);
    Draw(DkPrimitive_Triangles, 3);

    REQUIRE(render_target.Read<RGBA8U>(30, 30) == (RGBA8U{0xff, 0xff, 0xff, 0xff}));
}
