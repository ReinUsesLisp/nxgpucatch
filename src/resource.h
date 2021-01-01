#pragma once

#include <optional>

#include <deko3d.hpp>

#include "formats.h"

class GpuResource {
public:
    [[nodiscard]] uint64_t Hash();

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
