#include <catch2/catch_test_macros.hpp>

#include <deko3d.hpp>

#include "cmd_util.h"
#include "resource.h"
#include "shaders.h"

TEST_CASE("Varying unwritten", "[draw][varying_undefined]") {
    RenderTarget2D render_target{DkImageFormat_RGBA8_Unorm, 64, 64};
    Shader vert_shader(Shaders::FullScreenTriangleVanillaDksh);
    Shader frag_shader(Shaders::VaryingColorDksh);

    ResetState();
    SetRenderTarget(render_target);
    BindShaders(vert_shader, frag_shader);
    Draw(DkPrimitive_Triangles, 3);

    REQUIRE(render_target.Read<RGBA8U>(30, 30) == (RGBA8U{0x00, 0x00, 0x00, 0xff}));
}

TEST_CASE("Varying disabled attribute", "[draw][varying_undefined]") {
    RenderTarget2D render_target{DkImageFormat_RGBA8_Unorm, 64, 64};
    Shader vert_shader(Shaders::FullScreenTriangleDksh);
    Shader frag_shader(Shaders::VaryingColorDksh);

    ResetState();
    SetRenderTarget(render_target);
    BindShaders(vert_shader, frag_shader);
    Draw(DkPrimitive_Triangles, 3);

    REQUIRE(render_target.Read<RGBA8U>(30, 30) == (RGBA8U{0x00, 0x00, 0x00, 0x00}));
}