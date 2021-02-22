#include <catch2/catch_test_macros.hpp>

#include <deko3d.hpp>

#include "cmd_util.h"
#include "compare.h"
#include "device.h"
#include "resource.h"

template <typename ClearInput, typename RefType>
static bool Test(DkImageFormat format, const ClearInput& clear, const RefType& ref,
                 typename RefType::value_type max_diff = {}) {
    RenderTarget2D render_target{format, 128, 128};
    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        render_target.Bind(cmdbuf);
        cmdbuf.setScissors(0, {DkScissor{0, 0, 128, 128}});
        cmdbuf.clearColor(0, DkColorMask_RGBA, clear[0], clear[1], clear[2], clear[3]);
    });
    return ThresholdCompare(render_target.Read<RefType>(64, 64), ref, max_diff);
}

TEST_CASE("R8_Unorm", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_R8_Unorm, RGBA32F{0.5f}, R8U{127}, 1));
}

TEST_CASE("R8_Snorm", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_R8_Snorm, RGBA32F{-1.0f}, R8I{-127}, 1));
}

TEST_CASE("R8_Uint", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_R8_Uint, RGBA32U{0xccccccde}, R8U{255}));
    REQUIRE(Test(DkImageFormat_R8_Uint, RGBA32U{0xde}, R8U{0xde}));
}

TEST_CASE("R8_Sint", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_R8_Sint, RGBA32I{-500}, R8I{-128}));
    REQUIRE(Test(DkImageFormat_R8_Sint, RGBA32I{500}, R8I{127}));
    REQUIRE(Test(DkImageFormat_R8_Sint, RGBA32I{46}, R8I{46}));
}

TEST_CASE("R16_Float", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_R16_Float, RGBA32F{1.0f}, R16F{1.0f}));
    REQUIRE(Test(DkImageFormat_R16_Float, RGBA32F{0.5f}, R16F{0.5f}, 0.5f));
    REQUIRE(Test(DkImageFormat_R16_Float, RGBA32F{-8.0f}, R16F{-8.0f}, 0.5f));
}

TEST_CASE("R16_Unorm", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_R16_Unorm, RGBA32F{1.0f}, R16U{0xffff}));
    REQUIRE(Test(DkImageFormat_R16_Unorm, RGBA32F{0.5f}, R16U{0x7fff}));
    REQUIRE(Test(DkImageFormat_R16_Unorm, RGBA32F{-8.0f}, R16U{0}));
}

TEST_CASE("R16_Snorm", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_R16_Snorm, RGBA32F{1.0f}, R16I{0x7fff}));
    REQUIRE(Test(DkImageFormat_R16_Snorm, RGBA32F{0.5f}, R16I{0x3fff}));
    REQUIRE(Test(DkImageFormat_R16_Snorm, RGBA32F{-8.0f}, R16I{-0x7fff}));
}

TEST_CASE("R16_Uint", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_R16_Uint, RGBA32U{2020}, R16U{2020}));
    REQUIRE(Test(DkImageFormat_R16_Uint, RGBA32U{0x40000}, R16U{0xffff}));
}

TEST_CASE("R16_Sint", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_R16_Sint, RGBA32I{-2020}, R16I{-2020}));
    REQUIRE(Test(DkImageFormat_R16_Sint, RGBA32I{2020}, R16I{2020}));
    REQUIRE(Test(DkImageFormat_R16_Sint, RGBA32I{-0x10000}, R16I{-0x8000}));
    REQUIRE(Test(DkImageFormat_R16_Sint, RGBA32I{0x10000}, R16I{0x7fff}));
}

TEST_CASE("R32_Float", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_R32_Float, RGBA32F{1.0f}, R32F{1.0f}));
    REQUIRE(Test(DkImageFormat_R32_Float, RGBA32F{-2.5f}, R32F{-2.5f}, 0.5f));
}

TEST_CASE("R32_Uint", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_R32_Uint, RGBA32U{0xdeadbeef}, R32U{0xdeadbeef}));
}

TEST_CASE("R32_Sint", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_R32_Sint, RGBA32I{-0xcafe}, R32I{-0xcafe}));
}

TEST_CASE("RG8_Unorm", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_RG8_Unorm, RGBA32F{1.5f, 0.5f}, RG8U{255, 127}, 1));
}

TEST_CASE("RG8_Snorm", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_RG8_Snorm, RGBA32F{-1.5f, 1.5f}, RG8I{-127, 127}));
    REQUIRE(Test(DkImageFormat_RG8_Snorm, RGBA32F{0.0f, 0.5f}, RG8I{0, 127}, 1));
}

TEST_CASE("RG8_Uint", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_RG8_Uint, RGBA32U{270, 7}, RG8U{255, 7}));
}

TEST_CASE("RG8_Sint", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_RG8_Sint, RGBA32I{270, -270}, RG8I{127, -128}));
    REQUIRE(Test(DkImageFormat_RG8_Sint, RGBA32I{8, -8}, RG8I{8, -8}));
}

TEST_CASE("RG16_Float", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_RG16_Float, RGBA32F{1.0f, -1.5f}, RG16F{1.0f, -1.5f}, 0.5f));
}

TEST_CASE("RG16_Unorm", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_RG16_Unorm, RGBA32F{1.5f, -1.5f}, RG16U{0xffff, 0}));
    REQUIRE(Test(DkImageFormat_RG16_Unorm, RGBA32F{0.5f, 0.0f}, RG16U{0x7fff, 0}));
}

TEST_CASE("RG16_Snorm", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_RG16_Snorm, RGBA32F{1.5f, -1.5f}, RG16I{0x7fff, -0x7fff}));
    REQUIRE(Test(DkImageFormat_RG16_Snorm, RGBA32F{0.5f, 0.0f}, RG16I{0x3fff, 0}, 1));
}

TEST_CASE("RG16_Uint", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_RG16_Uint, RGBA32U{0x10000, 0xdead}, RG16U{0xffff, 0xdead}));
}

TEST_CASE("RG16_Sint", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_RG16_Sint, RGBA32I{-0x10000, 0x10000}, RG16I{-0x7fff, 0x7fff}));
    REQUIRE(Test(DkImageFormat_RG16_Sint, RGBA32I{-5050, 2020}, RG16I{-5050, 2020}));
}

TEST_CASE("RG32_Float", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_RG32_Float, RGBA32F{4.0f, -8.0f}, RG32F{4.0f, -8.0f}, 0.5f));
}

TEST_CASE("RG32_Uint", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_RG32_Uint, RGBA32U{439, 0x80001234}, RG32U{439, 0x80001234}));
}

TEST_CASE("RG32_Sint", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_RG32_Sint, RGBA32I{-439, 0x30001234}, RG32I{-439, 0x30001234}));
}

TEST_CASE("RGBA8_Unorm", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_RGBA8_Unorm, RGBA32F{-1.0f, 0.0f, 1.0f, 0.5f},
                 RGBA8U{0, 0, 255, 127}, 1));
}

TEST_CASE("RGBA8_Snorm", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_RGBA8_Snorm, RGBA32F{-1.0f, 0.0f, 1.0f, 0.0f},
                 RGBA8I{-127, 0, 127, 0}, 0));
    REQUIRE(
        Test(DkImageFormat_RGBA8_Snorm, RGBA32F{-0.5f, 0, 0, .5f}, RGBA8I{-64, 0, 0, 64}, 1.0f));
}

TEST_CASE("RGBA8_Uint", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_RGBA8_Uint, RGBA32U{280, 250, 210, 30}, RGBA8U{255, 250, 210, 30}));
}

TEST_CASE("RGBA8_Sint", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_RGBA8_Sint, RGBA32I{-280, 140, 5, -1}, RGBA8I{-127, 127, 5, -1}));
}

TEST_CASE("RGBA16_Float", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_RGBA16_Float, RGBA32F{4.0f, -8.0f, 1.0f, 16.0f},
                 RGBA16F{4.0f, -8.0f, 1.0f, 16.0f}, 0.5f));
}

TEST_CASE("RGBA16_Unorm", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_RGBA16_Unorm, RGBA32F{1.5f, 1.0f, 0.5f, 0.0f},
                 RGBA16U{0xffff, 0xffff, 0x7fff, 0}, 1));
}

TEST_CASE("RGBA16_Snorm", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_RGBA16_Snorm, RGBA32F{1.5f, 1.0f, -1.0f, 0.5f},
                 RGBA16I{0x7fff, 0x7fff, -0x7fff, 0x3fff}, 1));
}

TEST_CASE("RGBA16_Uint", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_RGBA16_Uint, RGBA32U{0x10000, 0x4000, 43, 891},
                 RGBA16U{0xffff, 0x4000, 43, 891}));
}

TEST_CASE("RGBA16_Sint", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_RGBA16_Sint, RGBA32I{0x8000, -0x8000, 43, -891},
                 RGBA16I{0x7fff, -0x7fff, 43, -891}));
}

TEST_CASE("RGBA32_Float", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_RGBA32_Float, RGBA32F{1.0f, 8.0f, -4.0f, -2.0f},
                 RGBA32F{1.0f, 8.0f, -4.0f, -2.0f}, 0.5f));
}

TEST_CASE("RGBA32_Uint", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_RGBA32_Uint, RGBA32U{1, 5, 0xdeadbeef, 54},
                 RGBA32U{1, 5, 0xdeadbeef, 54}));
}

TEST_CASE("RGBA32_Sint", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_RGBA32_Sint, RGBA32I{1, 5, -0x3eadbeef, 54},
                 RGBA32I{1, 5, -0x3eadbeef, 54}));
}

TEST_CASE("RGBX8_Unorm_sRGB", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_RGBX8_Unorm_sRGB, RGBA32F{0.25f, 0.5f, 1.0f, 0.25f},
                 RGBA8U{137, 188, 255, 0}, 1));
}

TEST_CASE("RGBA8_Unorm_sRGB", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_RGBA8_Unorm_sRGB, RGBA32F{0.25f, 0.5f, 1.0f, 0.25f},
                 RGBA8U{137, 188, 255, 64}, 1));
}

TEST_CASE("RGB10A2_Unorm", "[render_formats]") {
    RenderTarget2D render_target{DkImageFormat_RGB10A2_Unorm, 128, 128};
    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        render_target.Bind(cmdbuf);
        cmdbuf.setScissors(0, {DkScissor{0, 0, 128, 128}});
        cmdbuf.clearColor(0, DkColorMask_RGBA, 1.0f, 1.0f, 1.0f, 0.0f);
    });
    const auto sample1{render_target.Read<RGB10A2>(64, 64)};
    REQUIRE(sample1.r == 1023);
    REQUIRE(sample1.g == 1023);
    REQUIRE(sample1.b == 1023);
    REQUIRE(sample1.a == 0);

    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        cmdbuf.clearColor(0, DkColorMask_RGBA, 0.25f, 0.5f, 0.75f, 0.4f);
    });
    const auto sample2{render_target.Read<RGB10A2>(64, 64)};
    REQUIRE(ThresholdCompare<uint32_t>(sample2.r, 256, 1));
    REQUIRE(ThresholdCompare<uint32_t>(sample2.g, 511, 1));
    REQUIRE(ThresholdCompare<uint32_t>(sample2.b, 767, 1));
    REQUIRE(ThresholdCompare<uint32_t>(sample2.a, 1, 0));

    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        cmdbuf.clearColor(0, DkColorMask_RGBA, 2.0f, -1.0f, 0.0f, 0.6f);
    });
    const auto sample3{render_target.Read<RGB10A2>(64, 64)};
    REQUIRE(ThresholdCompare<uint32_t>(sample3.r, 1023, 1));
    REQUIRE(ThresholdCompare<uint32_t>(sample3.g, 0, 1));
    REQUIRE(ThresholdCompare<uint32_t>(sample3.b, 0, 1));
    REQUIRE(ThresholdCompare<uint32_t>(sample3.a, 2, 0));
}

TEST_CASE("RG11B10_Float", "[render_formats]") {
    // RG11B10_Float is harder to test due to its low floating point precision
    // The asserts will compare against known good values to avoid conversions
    RenderTarget2D render_target{DkImageFormat_RG11B10_Float, 128, 128};
    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        render_target.Bind(cmdbuf);
        cmdbuf.setScissors(0, {DkScissor{0, 0, 128, 128}});
        cmdbuf.clearColor(0, DkColorMask_RGBA, 1.0f, 0.0f, 1.0f, 1.0f);
    });
    const auto sample1{render_target.Read<RG11B10>(64, 64)};
    REQUIRE(sample1.r == 0x3c0);
    REQUIRE(sample1.g == 0);
    REQUIRE(sample1.b == 0x1e0);

    RecordRunWait(
        [&](dk::CmdBuf cmdbuf) { cmdbuf.clearColor(0, DkColorMask_RGBA, 0.0f, 1.0f, 0.0f, 1.0f); });
    const auto sample2{render_target.Read<RG11B10>(64, 64)};
    REQUIRE(sample2.r == 0);
    REQUIRE(sample2.g == 0x3c0);
    REQUIRE(sample2.b == 0);
}

TEST_CASE("RGBX8_Unorm", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_RGBX8_Unorm, RGBA32F{0.25f, 0.5f, 1.0f, 0.25f},
                 RGBA8U{64, 127, 255, 0}, 1));
}

TEST_CASE("RGBX8_Snorm", "[render_formats]") {
    // RGBX8_Snorm writes to alpha?!
    REQUIRE(Test(DkImageFormat_RGBX8_Snorm, RGBA32F{-0.5f, 1.0f, -2.0f, 0.5f},
                 RGBA8I{-31, 127, -127, 63}, 1));
}

TEST_CASE("RGBX8_Uint", "[render_formats]") {
    // RGBX8_Uint writes to alpha too...
    REQUIRE(Test(DkImageFormat_RGBX8_Uint, RGBA32U{6, 280, 14, 36}, RGBA8U{6, 255, 14, 36}, 0));
}

TEST_CASE("RGBX8_Sint", "[render_formats]") {
    // RGBX8_Sint writes to alpha
    REQUIRE(
        Test(DkImageFormat_RGBX8_Sint, RGBA32I{-6, 280, -240, 36}, RGBA8I{-6, 127, -128, 36}, 0));
}

TEST_CASE("RGBX16_Float", "[render_formats]") {
    // Unlike previous formats, RGBX16_Float does .not. write to alpha
    REQUIRE(Test(DkImageFormat_RGBX16_Float, RGBA32F{1.0f, -8.0f, 4.0f, 2.0f},
                 RGBA16F{1.0f, -8.0f, 4.0f, 0.0f}, 0.5f));
}

TEST_CASE("RGBX16_Unorm", "[render_formats]") {
    // But RGBX16_Unorm writes to alpha
    REQUIRE(Test(DkImageFormat_RGBX16_Unorm, RGBA32F{1.0f, -8.0f, 0.5f, 0.25f},
                 RGBA16U{0xffff, 0, 0x7fff, 0x4000}, 1));
}

TEST_CASE("RGBX16_Snorm", "[render_formats]") {
    // RGBX16_Snorm also writes to alpha
    REQUIRE(Test(DkImageFormat_RGBX16_Snorm, RGBA32F{1.0f, -8.0f, 0.5f, 0.25f},
                 RGBA16I{0x7fff, -0x7fff, 0x4000, 0x2000}, 1));
}

TEST_CASE("RGBX16_Uint", "[render_formats]") {
    // RGBX16_Uint writes to alpha
    REQUIRE(Test(DkImageFormat_RGBX16_Uint, RGBA32U{50, 0x10000, 13, 7}, RGBA16U{50, 0xffff, 13, 7},
                 1));
}

TEST_CASE("RGBX16_Sint", "[render_formats]") {
    // RGBX16_Sint writes to alpha
    REQUIRE(Test(DkImageFormat_RGBX16_Sint, RGBA32I{-0x10000, 0x10000, 19, -7},
                 RGBA16I{-0x7fff, 0x7fff, 19, -7}, 0));
}

TEST_CASE("RGBX32_Float", "[render_formats]") {
    // RGBX32_Float does .not. write to alpha
    REQUIRE(Test(DkImageFormat_RGBX32_Float, RGBA32F{1.0f, 2.0f, -4.0f, 8.0f},
                 RGBA32F{1.0f, 2.0f, -4.0f, 0.0f}, 0.5f));
}

TEST_CASE("RGBX32_Uint", "[render_formats]") {
    // RGBX32_Uint does .not. write to alpha
    REQUIRE(Test(DkImageFormat_RGBX32_Uint, RGBA32U{64, 0xdeadbeef, 934, 252},
                 RGBA32U{64, 0xdeadbeef, 934, 0}));
}

TEST_CASE("RGBX32_Sint", "[render_formats]") {
    // RGBX32_Sint does .not. write to alpha
    REQUIRE(Test(DkImageFormat_RGBX32_Sint, RGBA32I{-64, -0x3eadbeef, 934, -252},
                 RGBA32I{-64, -0x3eadbeef, 934, 0}));
}

TEST_CASE("BGR565_Unorm", "[render_formats]") {
    RenderTarget2D render_target{DkImageFormat_BGR565_Unorm, 128, 128};
    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        render_target.Bind(cmdbuf);
        cmdbuf.setScissors(0, {DkScissor{0, 0, 128, 128}});
        cmdbuf.clearColor(0, DkColorMask_RGBA, 0.25f, 0.5f, 0.75f, 0.0f);
    });
    const auto sample1{render_target.Read<BGR565>(64, 64)};
    REQUIRE(ThresholdCompare<uint16_t>(sample1.r, 8, 1));
    REQUIRE(ThresholdCompare<uint16_t>(sample1.g, 31, 1));
    REQUIRE(ThresholdCompare<uint16_t>(sample1.b, 23, 1));

    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        cmdbuf.clearColor(0, DkColorMask_RGBA, -1.0f, 2.0f, 0.0f, 0.0f);
    });
    const auto sample2{render_target.Read<BGR565>(64, 64)};
    REQUIRE(ThresholdCompare<uint16_t>(sample2.r, 0, 1));
    REQUIRE(ThresholdCompare<uint16_t>(sample2.g, 63, 1));
    REQUIRE(ThresholdCompare<uint16_t>(sample2.b, 0, 1));
}

TEST_CASE("BGR5_Unorm", "[render_formats]") {
    RenderTarget2D render_target{DkImageFormat_BGR5_Unorm, 128, 128};
    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        render_target.Bind(cmdbuf);
        cmdbuf.setScissors(0, {DkScissor{0, 0, 128, 128}});
        cmdbuf.clearColor(0, DkColorMask_RGBA, 0.25f, 0.5f, 0.75f, 1.0f);
    });
    const auto sample1{render_target.Read<BGR5A1>(64, 64)};
    REQUIRE(ThresholdCompare<uint16_t>(sample1.r, 8, 1));
    REQUIRE(ThresholdCompare<uint16_t>(sample1.g, 15, 1));
    REQUIRE(ThresholdCompare<uint16_t>(sample1.b, 23, 1));
    REQUIRE(sample1.a == 0);

    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        cmdbuf.clearColor(0, DkColorMask_RGBA, -1.0f, 2.0f, 0.0f, 1.0f);
    });
    const auto sample2{render_target.Read<BGR5A1>(64, 64)};
    REQUIRE(ThresholdCompare<uint16_t>(sample2.r, 0, 1));
    REQUIRE(ThresholdCompare<uint16_t>(sample2.g, 31, 1));
    REQUIRE(ThresholdCompare<uint16_t>(sample2.b, 0, 1));
    REQUIRE(sample2.a == 0);
}

TEST_CASE("BGR5A1_Unorm", "[render_formats]") {
    RenderTarget2D render_target{DkImageFormat_BGR5A1_Unorm, 128, 128};
    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        render_target.Bind(cmdbuf);
        cmdbuf.setScissors(0, {DkScissor{0, 0, 128, 128}});
        cmdbuf.clearColor(0, DkColorMask_RGBA, 0.25f, 0.5f, 0.75f, 1.0f);
    });
    const auto sample1{render_target.Read<BGR5A1>(64, 64)};
    REQUIRE(ThresholdCompare<uint16_t>(sample1.r, 8, 1));
    REQUIRE(ThresholdCompare<uint16_t>(sample1.g, 15, 1));
    REQUIRE(ThresholdCompare<uint16_t>(sample1.b, 23, 1));
    REQUIRE(sample1.a == 1);

    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        cmdbuf.clearColor(0, DkColorMask_RGBA, -1.0f, 2.0f, 0.0f, 0.0f);
    });
    const auto sample2{render_target.Read<BGR5A1>(64, 64)};
    REQUIRE(ThresholdCompare<uint16_t>(sample2.r, 0, 1));
    REQUIRE(ThresholdCompare<uint16_t>(sample2.g, 31, 1));
    REQUIRE(ThresholdCompare<uint16_t>(sample2.b, 0, 1));
    REQUIRE(sample2.a == 0);
}

TEST_CASE("BGRX8_Unorm", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_BGRX8_Unorm, RGBA32F{2.0f, 0.5f, 0.25f, 1.0f},
                 RGBA8U{64, 127, 255, 0}, 1));
}

TEST_CASE("BGRA8_Unorm", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_BGRA8_Unorm, RGBA32F{2.0f, 0.5f, 0.25f, 1.0f},
                 RGBA8U{64, 127, 255, 255}, 1));
}

TEST_CASE("BGRX8_Unorm_sRGB", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_BGRX8_Unorm_sRGB, RGBA32F{2.0f, 0.5f, 0.25f, 1.0f},
                 RGBA8U{137, 188, 255, 0}, 1));
}

TEST_CASE("BGRA8_Unorm_sRGB", "[render_formats]") {
    REQUIRE(Test(DkImageFormat_BGRA8_Unorm_sRGB, RGBA32F{2.0f, 0.5f, 0.25f, 1.0f},
                 RGBA8U{137, 188, 255, 255}, 1));
}
