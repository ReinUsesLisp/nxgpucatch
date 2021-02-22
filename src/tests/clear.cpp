#include <limits>

#include <catch2/catch_test_macros.hpp>

#include <deko3d.hpp>

#include "cmd_util.h"
#include "compare.h"
#include "device.h"
#include "resource.h"

TEST_CASE("Basic", "[clear]") {
    RenderTarget2D render_target{DkImageFormat_RGBA8_Unorm, 64, 64};
    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        render_target.Bind(cmdbuf);
        cmdbuf.clearColor(0, DkColorMask_RGBA, 1.0f, 0.5f, 0.25f, 1.0f);
    });
    REQUIRE(ThresholdCompare(render_target.Read<RGBA8U>(0, 0), {0xff, 0x7f, 0x40, 0xff}, 1));
}

TEST_CASE("Out of range", "[clear]") {
    RenderTarget2D render_target{DkImageFormat_RGBA8_Unorm, 64, 64};
    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        render_target.Bind(cmdbuf);
        cmdbuf.clearColor(0, DkColorMask_RGBA, -1.0f, 0.5f, 0.25f, 2.0f);
    });
    REQUIRE(ThresholdCompare(render_target.Read<RGBA8U>(0, 0), {0x00, 0x7f, 0x40, 0xff}, 1));
}

TEST_CASE("Out of range clamping", "[clear]") {
    RenderTarget2D render_target{DkImageFormat_RGBA8_Sint, 64, 64};
    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        render_target.Bind(cmdbuf);
        cmdbuf.clearColor<int>(0, DkColorMask_RGBA, -500, 500, 64, -15);
    });
    REQUIRE(ThresholdCompare(render_target.Read<RGBA8I>(0, 0), RGBA8I{-128, 127, 64, -15}));
}

TEST_CASE("Masked", "[clear]") {
    RenderTarget2D render_target{DkImageFormat_RGBA8_Unorm, 64, 64};
    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        render_target.Bind(cmdbuf);
        cmdbuf.clearColor(0, DkColorMask_R, 1.0f, 1.0f, 1.0f, 1.0f);
    });
    REQUIRE(ThresholdCompare(render_target.Read<RGBA8U>(0, 0), {0xff, 0x00, 0x00, 0x00}, 1));
}

TEST_CASE("Scissor", "[clear]") {
    RenderTarget2D render_target{DkImageFormat_RGBA8_Unorm, 512, 512};
    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        render_target.Bind(cmdbuf);
        cmdbuf.setScissors(0, {DkScissor{0, 0, 256, 256}});
        cmdbuf.clearColor(0, DkColorMask_RGBA, 1.0f, 1.0f, 1.0f, 1.0f);
        cmdbuf.setScissors(0, {DkScissor{256, 0, 256, 256}});
        cmdbuf.clearColor(0, DkColorMask_RGBA, 0.0f, 1.0f, 1.0f, 1.0f);
        cmdbuf.setScissors(0, {DkScissor{256, 256, 256, 256}});
        cmdbuf.clearColor(0, DkColorMask_RGBA, 0.0f, 0.0f, 1.0f, 1.0f);
        cmdbuf.setScissors(0, {DkScissor{0, 256, 256, 256}});
        cmdbuf.clearColor(0, DkColorMask_RGBA, 1.0f, 0.0f, 0.0f, 1.0f);
    });
    REQUIRE(ThresholdCompare(render_target.Read<RGBA8U>(128, 128), {0xff, 0xff, 0xff, 0xff}, 1));
    REQUIRE(ThresholdCompare(render_target.Read<RGBA8U>(384, 128), {0x00, 0xff, 0xff, 0xff}, 1));
    REQUIRE(ThresholdCompare(render_target.Read<RGBA8U>(384, 384), {0x00, 0x00, 0xff, 0xff}, 1));
    REQUIRE(ThresholdCompare(render_target.Read<RGBA8U>(128, 384), {0xff, 0x00, 0x00, 0xff}, 1));
}

TEST_CASE("Scissor masked", "[clear]") {
    RenderTarget2D render_target{DkImageFormat_RGBA8_Unorm, 32, 32};
    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        render_target.Bind(cmdbuf);
        cmdbuf.setScissors(0, {DkScissor{0, 0, 32, 32}});
        cmdbuf.clearColor(0, DkColorMask_R, 1.0f, 1.0f, 1.0f, 1.0f);
        cmdbuf.setScissors(0, {DkScissor{0, 0, 16, 16}});
        cmdbuf.clearColor(0, DkColorMask_G, 1.0f, 1.0f, 1.0f, 1.0f);
        cmdbuf.setScissors(0, {DkScissor{16, 0, 16, 16}});
        cmdbuf.clearColor(0, DkColorMask_B, 1.0f, 1.0f, 1.0f, 1.0f);
        cmdbuf.setScissors(0, {DkScissor{0, 16, 16, 16}});
        cmdbuf.clearColor(0, DkColorMask_A, 1.0f, 1.0f, 1.0f, 1.0f);
    });
    REQUIRE(ThresholdCompare(render_target.Read<RGBA8U>(8, 8), {0xff, 0xff, 0x00, 0x00}, 0));
    REQUIRE(ThresholdCompare(render_target.Read<RGBA8U>(24, 8), {0xff, 0x00, 0xff, 0x00}, 0));
    REQUIRE(ThresholdCompare(render_target.Read<RGBA8U>(24, 24), {0xff, 0x00, 0x00, 0x00}, 0));
    REQUIRE(ThresholdCompare(render_target.Read<RGBA8U>(8, 24), {0xff, 0x00, 0x00, 0xff}, 0));
}

TEST_CASE("Negative values", "[clear]") {
    static constexpr RGBA32F ref{-32.0f, -64.0f, -128.0f, -256.0f};
    RenderTarget2D render_target{DkImageFormat_RGBA32_Float, 32, 32};
    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        render_target.Bind(cmdbuf);
        cmdbuf.setScissors(0, {DkScissor{0, 0, 32, 32}});
        cmdbuf.clearColor(0, DkColorMask_RGBA, ref[0], ref[1], ref[2], ref[3]);
    });
    REQUIRE(ThresholdCompare(render_target.Read<RGBA32F>(8, 8), ref, 0.01f));
}

TEST_CASE("Quiet NaN", "[clear]") {
    static constexpr RGBA32F ref{1.0f, std::numeric_limits<float>::quiet_NaN(), 0.0f, 0.0f};
    RenderTarget2D render_target{DkImageFormat_RGBA32_Float, 32, 32};
    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        render_target.Bind(cmdbuf);
        cmdbuf.setScissors(0, {DkScissor{0, 0, 32, 32}});
        cmdbuf.clearColor(0, DkColorMask_RGBA, ref[0], ref[1], ref[2], ref[3]);
    });
    REQUIRE(ThresholdCompare(render_target.Read<RGBA32F>(8, 8), ref));
}

TEST_CASE("Empty", "[clear]") {
    static constexpr RGBA32F ref{1.0f, 5.0f, 10.0f, -50.0f};
    RenderTarget2D render_target{DkImageFormat_RGBA32_Float, 32, 32};
    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        render_target.Bind(cmdbuf);
        cmdbuf.setScissors(0, {DkScissor{0, 0, 32, 32}});
        cmdbuf.clearColor(0, DkColorMask_RGBA, ref[0], ref[1], ref[2], ref[3]);
        cmdbuf.setScissors(0, {DkScissor{0, 0, 0, 0}});
        cmdbuf.clearColor(0, DkColorMask_RGBA, 1.0f, 1.0f, 1.0f, 1.0f);
    });
    REQUIRE(ThresholdCompare(render_target.Read<RGBA32F>(0, 0), ref));
}
