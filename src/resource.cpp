#include <algorithm>
#include <cstring>
#include <memory>
#include <numeric>

#include <deko3d.hpp>
#include <switch.h>

#include "cmd_util.h"
#include "resource.h"
#include "wait_input.h"

static int BytesPerPixel(DkImageFormat format) {
    switch (format) {
    case DkImageFormat_R8_Unorm:
    case DkImageFormat_R8_Snorm:
    case DkImageFormat_R8_Uint:
    case DkImageFormat_R8_Sint:
        return 1;
    case DkImageFormat_R16_Float:
    case DkImageFormat_R16_Unorm:
    case DkImageFormat_R16_Snorm:
    case DkImageFormat_R16_Uint:
    case DkImageFormat_R16_Sint:
    case DkImageFormat_RG8_Unorm:
    case DkImageFormat_RG8_Snorm:
    case DkImageFormat_RG8_Uint:
    case DkImageFormat_RG8_Sint:
    case DkImageFormat_BGR565_Unorm:
    case DkImageFormat_BGR5_Unorm:
    case DkImageFormat_BGR5A1_Unorm:
        return 2;
    case DkImageFormat_R32_Float:
    case DkImageFormat_R32_Uint:
    case DkImageFormat_R32_Sint:
    case DkImageFormat_RG16_Float:
    case DkImageFormat_RG16_Unorm:
    case DkImageFormat_RG16_Snorm:
    case DkImageFormat_RG16_Uint:
    case DkImageFormat_RG16_Sint:
    case DkImageFormat_RGBA8_Unorm:
    case DkImageFormat_RGBA8_Snorm:
    case DkImageFormat_RGBA8_Uint:
    case DkImageFormat_RGBA8_Sint:
    case DkImageFormat_RGBX8_Unorm_sRGB:
    case DkImageFormat_RGBA8_Unorm_sRGB:
    case DkImageFormat_RGB10A2_Unorm:
    case DkImageFormat_RG11B10_Float:
    case DkImageFormat_RGBX8_Unorm:
    case DkImageFormat_RGBX8_Snorm:
    case DkImageFormat_RGBX8_Uint:
    case DkImageFormat_RGBX8_Sint:
    case DkImageFormat_BGRX8_Unorm:
    case DkImageFormat_BGRA8_Unorm:
    case DkImageFormat_BGRX8_Unorm_sRGB:
    case DkImageFormat_BGRA8_Unorm_sRGB:
        return 4;
    case DkImageFormat_RG32_Float:
    case DkImageFormat_RG32_Uint:
    case DkImageFormat_RG32_Sint:
    case DkImageFormat_RGBA16_Float:
    case DkImageFormat_RGBA16_Unorm:
    case DkImageFormat_RGBA16_Snorm:
    case DkImageFormat_RGBA16_Uint:
    case DkImageFormat_RGBA16_Sint:
    case DkImageFormat_RGBX16_Float:
    case DkImageFormat_RGBX16_Unorm:
    case DkImageFormat_RGBX16_Snorm:
    case DkImageFormat_RGBX16_Uint:
    case DkImageFormat_RGBX16_Sint:
        return 8;
    case DkImageFormat_RGBA32_Float:
    case DkImageFormat_RGBA32_Uint:
    case DkImageFormat_RGBA32_Sint:
    case DkImageFormat_RGBX32_Float:
    case DkImageFormat_RGBX32_Uint:
    case DkImageFormat_RGBX32_Sint:
        return 16;
    default:
        printf("Unknown format=%d\n", format);
        return 4;
    }
}

static bool IsDepthStencil(DkImageFormat format) {
    switch (format) {
    case DkImageFormat_S8:
    case DkImageFormat_Z16:
    case DkImageFormat_Z24X8:
    case DkImageFormat_ZF32:
    case DkImageFormat_Z24S8:
    case DkImageFormat_ZF32_X24S8:
        return true;
    default:
        return false;
    }
}

uint64_t GpuResource::Hash() {
    auto buffer{std::make_unique<uint8_t[]>(m_heap.getSize())};
    std::memcpy(buffer.get(), m_heap.getCpuAddr(), m_heap.getSize());
    std::array<uint64_t, SHA256_HASH_SIZE / sizeof(uint64_t)> sha256;
    sha256CalculateHash(sha256.data(), buffer.get(), m_heap.getSize());
    return std::reduce(sha256.begin(), sha256.end(), uint64_t{0},
                       [](uint64_t a, uint64_t b) { return a ^ b; });
}

Image::Image(const dk::ImageLayoutMaker& layout_maker) {
    m_width = layout_maker.dimensions[0];
    m_height = layout_maker.dimensions[1];
    m_depth = layout_maker.dimensions[2];
    m_is_depth_stencil = IsDepthStencil(layout_maker.format);

    dk::ImageLayout image_layout;
    layout_maker.initialize(image_layout);

    m_heap = dk::MemBlockMaker{layout_maker.device, static_cast<uint32_t>(image_layout.getSize())}
                 .setFlags(DkMemBlockFlags_CpuUncached | DkMemBlockFlags_GpuCached |
                           DkMemBlockFlags_ZeroFillInit | DkMemBlockFlags_Image)
                 .create();
    m_image.initialize(image_layout, m_heap, 0);
    m_image_view = dk::ImageView{m_image};
}

void Image::Bind(dk::CmdBuf cmdbuf) {
    const DkImageView& view{*m_image_view};
    if (m_is_depth_stencil) {
        cmdbuf.bindRenderTargets({}, &view);
    } else {
        cmdbuf.bindRenderTargets({&view}, nullptr);
    }
}

void Image::Show() {
    constexpr uint32_t SCREEN_WIDTH{1280};
    constexpr uint32_t SCREEN_HEIGHT{720};
    dk::ImageLayoutMaker maker{device};
    maker
        .setFlags(DkImageFlags_BlockLinear | DkImageFlags_HwCompression |
                  DkImageFlags_Usage2DEngine | DkImageFlags_UsagePresent | DkImageFlags_UsageRender)
        .setFormat(DkImageFormat_RGBA8_Unorm)
        .setDimensions(SCREEN_WIDTH, SCREEN_HEIGHT);
    const std::array<Image, 2> present_images{Image{maker}, Image{maker}};
    const DkImage swapchain_images_data[]{present_images[0].m_image, present_images[1].m_image};
    const std::array<const DkImage*, 2> swapchain_images{&swapchain_images_data[0],
                                                         &swapchain_images_data[1]};

    consoleExit(nullptr);

    const DkImageRect src_rect{
        .x{0},
        .y{0},
        .z{0},
        .width{m_width},
        .height{m_height},
        .depth{1},
    };
    const DkImageRect dst_rect{
        .x{0},
        .y{0},
        .z{0},
        .width{SCREEN_HEIGHT},
        .height{SCREEN_HEIGHT},
        .depth{1},
    };
    auto swapchain{dk::SwapchainMaker{device, nwindowGetDefault(), swapchain_images}.create()};
    do {
        int image_slot;
        DkFence fence;
        swapchain.acquireImage(image_slot, fence);
        RecordRunWait([&](dk::CmdBuf cmdbuf) {
            cmdbuf.blitImage(*m_image_view, src_rect, *present_images[image_slot].m_image_view,
                             dst_rect, DkBlitFlag_FilterNearest);
        });
        queue.waitFence(fence);
        queue.presentImage(swapchain, image_slot);
    } while (!HasUserInput());

    queue.waitIdle();
    swapchain.destroy();

    consoleInit(nullptr);
}

template <typename T>
T Image::Read(int32_t x, int32_t y) {
    const size_t offset{(x + y * m_width) * sizeof(T)};
    T data;
    std::memcpy(&data, reinterpret_cast<const char*>(m_heap.getCpuAddr()) + offset, sizeof(T));
    return data;
}
template R8U Image::Read<R8U>(int32_t, int32_t);
template R8I Image::Read<R8I>(int32_t, int32_t);
template R16F Image::Read<R16F>(int32_t, int32_t);
template R16U Image::Read<R16U>(int32_t, int32_t);
template R16I Image::Read<R16I>(int32_t, int32_t);
template R32F Image::Read<R32F>(int32_t, int32_t);
template R32U Image::Read<R32U>(int32_t, int32_t);
template R32I Image::Read<R32I>(int32_t, int32_t);
template RG8U Image::Read<RG8U>(int32_t, int32_t);
template RG8I Image::Read<RG8I>(int32_t, int32_t);
template RG16F Image::Read<RG16F>(int32_t, int32_t);
template RG16U Image::Read<RG16U>(int32_t, int32_t);
template RG16I Image::Read<RG16I>(int32_t, int32_t);
template RG32F Image::Read<RG32F>(int32_t, int32_t);
template RG32U Image::Read<RG32U>(int32_t, int32_t);
template RG32I Image::Read<RG32I>(int32_t, int32_t);
template RGB32F Image::Read<RGB32F>(int32_t, int32_t);
template RGB32U Image::Read<RGB32U>(int32_t, int32_t);
template RGB32I Image::Read<RGB32I>(int32_t, int32_t);
template RGBA8U Image::Read<RGBA8U>(int32_t, int32_t);
template RGBA8I Image::Read<RGBA8I>(int32_t, int32_t);
template RGBA16F Image::Read<RGBA16F>(int32_t, int32_t);
template RGBA16U Image::Read<RGBA16U>(int32_t, int32_t);
template RGBA16I Image::Read<RGBA16I>(int32_t, int32_t);
template RGBA32F Image::Read<RGBA32F>(int32_t, int32_t);
template RGBA32U Image::Read<RGBA32U>(int32_t, int32_t);
template RGBA32I Image::Read<RGBA32I>(int32_t, int32_t);
template RGB10A2 Image::Read<RGB10A2>(int32_t, int32_t);
template RG11B10 Image::Read<RG11B10>(int32_t, int32_t);
template BGR565 Image::Read<BGR565>(int32_t, int32_t);
template BGR5A1 Image::Read<BGR5A1>(int32_t, int32_t);

RenderTarget2D::RenderTarget2D(DkImageFormat format, uint32_t width, uint32_t height)
    : Image(dk::ImageLayoutMaker{device}
                .setFlags(DkImageFlags_PitchLinear | DkImageFlags_UsageRender |
                          DkImageFlags_Usage2DEngine)
                .setPitchStride(width * BytesPerPixel(format))
                .setFormat(format)
                .setDimensions(width, height)) {}
