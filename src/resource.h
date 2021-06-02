#pragma once

#include <optional>

#include <deko3d.hpp>

#include "formats.h"

class GpuResource {
public:
    [[nodiscard]] uint64_t Hash();

    [[nodiscard]] dk::MemBlock Heap() const noexcept {
        return m_heap;
    }

protected:
    dk::UniqueMemBlock m_heap;
};

class Image : public GpuResource {
public:
    explicit Image(const dk::ImageLayoutMaker& layout_maker);
    void Bind(dk::CmdBuf cmdbuf);
    void Show();

    template <typename T>
    T Read(int32_t x, int32_t y);

    [[nodiscard]] const dk::Image& ImageHandle() const noexcept {
        return m_image;
    }

protected:
    dk::Image m_image;
    std::optional<dk::ImageView> m_image_view;
    uint32_t m_width;
    uint32_t m_height;
    uint32_t m_depth;
    bool m_is_depth_stencil;
};

class RenderTarget2D : public Image {
public:
    explicit RenderTarget2D(DkImageFormat format, uint32_t width, uint32_t height);
};

class Texture {
public:
    explicit Texture(uint32_t width, uint32_t height, uint32_t depth, uint32_t mips,
                     DkImageType type, bool is_depth = false, DkMsMode ms = DkMsMode_1x)
        : Texture(width, height, depth, mips, type,
                  is_depth ? DkImageFormat_ZF32 : DkImageFormat_RGBA8_Unorm, ms) {
        if (is_depth) {
            m_sampler.setDepthCompare(true);
        }
    }

    explicit Texture(uint32_t width, uint32_t height, uint32_t depth, uint32_t mips,
                     DkImageType type, DkImageFormat format, DkMsMode ms = DkMsMode_1x)
        : m_image(dk::ImageLayoutMaker{device}
                      .setDimensions(width, height, depth)
                      .setType(type)
                      .setFormat(format)
                      .setMipLevels(mips)
                      .setMsMode(ms)
                      .setFlags(type == DkImageType_Buffer
                                    ? 0
                                    : (DkImageFlags_BlockLinear |
                                       (type == DkImageType_2D ? DkImageFlags_Usage2DEngine : 0)))),
          m_image_view{m_image.ImageHandle()}, m_width{width}, m_height{height}, m_depth{depth},
          m_type{type} {
        m_sampler.setFilter(DkFilter_Nearest, DkFilter_Nearest, DkMipFilter_Nearest);
    }

    const dk::ImageView& ImageView() const noexcept {
        return m_image_view;
    }

    const dk::Sampler& Sampler() const noexcept {
        return m_sampler;
    }

    template <typename ColorType>
    requires(sizeof(ColorType) == sizeof(uint32_t)) //
        void FillColor(int mip, const ColorType& color) {
        const bool reduce_height = m_type != DkImageType_1D && m_type != DkImageType_1DArray &&
                                   m_type != DkImageType_Buffer;
        const bool reduce_depth = m_type == DkImageType_3D;

        uint8_t* data = static_cast<uint8_t*>(CpuAddr());
        uint32_t width = m_width;
        uint32_t height = m_height;
        uint32_t depth = m_depth;
        while (mip > 0) {
            data += width * height * depth * 4;

            width = std::max(width >> 1, 1U);
            if (reduce_height) {
                height = std::max(height >> 1, 1U);
            }
            if (reduce_depth) {
                depth = std::max(depth >> 1, 1U);
            }
            --mip;
        }
        for (size_t i = 0; i < width * height * depth; ++i) {
            std::memcpy(data, &color, sizeof(color));
            data += sizeof(color);
        }
    }

    void FillColor(int mip, uint8_t red, uint8_t green, uint8_t blue, uint8_t alpha) {
        FillColor(mip, std::array<uint8_t, 4>{red, green, blue, alpha});
    }

    [[nodiscard]] void* CpuAddr() const noexcept {
        return m_image.Heap().getCpuAddr();
    }

    [[nodiscard]] size_t SizeBytes() const noexcept {
        return m_image.Heap().getSize();
    }

    [[nodiscard]] Image& GetImage() {
        return m_image;
    }

    void Show() {
        m_image.Show();
    }

private:
    Image m_image;
    dk::ImageView m_image_view;
    dk::Sampler m_sampler;
    uint32_t m_width;
    uint32_t m_height;
    uint32_t m_depth;
    DkImageType m_type;
};
