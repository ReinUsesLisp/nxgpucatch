#include <catch2/catch_test_macros.hpp>

#include "cmd_util.h"
#include "descriptor_set.h"
#include "heap.h"
#include "resource.h"
#include "shader.h"

namespace {
using Color = std::array<float, 4>;
using ColorF16 = std::array<__fp16, 4>;

Shader MakeShader(std::string code) {
    code = R"(.dksh compute
main:
        MOV R0, RZ;
        MOV R1, RZ;
        MOV R2, RZ;
        MOV R3, RZ;
        MOV32I R4, 0x3f000000;
        MOV32I R5, 0x3f000000;
)" + code + R"(
        MOV R4, c[0x0][0x140];
        MOV R5, c[0x0][0x144];
        STG.E.128 [R4], R0;
        EXIT;
    )";
    return Shader(code.c_str());
}

class Runner {
public:
    explicit Runner(std::initializer_list<std::reference_wrapper<Texture>> textures) {
        RecordRunWait([&](dk::CmdBuf cmdbuf) {
            result_heap.BindStorage(cmdbuf, DkStage_Compute, 0);
            descriptor_set.Bind(cmdbuf);

            int index = 0;
            for (auto& texture : textures) {
                descriptor_set.PushTexture(cmdbuf, texture, index);
                cmdbuf.bindTextures(DkStage_Compute, index, {dkMakeTextureHandle(index, index)});
                ++index;
            }
        });
    }

    Color Run(const Shader& shader) {
        RecordRunWait([&](dk::CmdBuf cmdbuf) {
            shader.Bind(cmdbuf, DkStageFlag_Compute);
            cmdbuf.dispatchCompute(1, 1, 1);
        });
        return *result_heap;
    };

    Color RunF16(const Shader& shader) {
        Color value = Run(shader);
        ColorF16 ret;
        std::memcpy(&ret, &value, sizeof(ret));
        return {ret[0], ret[1], ret[2], ret[3]};
    }

private:
    DescriptorSet descriptor_set;
    TypedHeap<Color> result_heap;
};
} // Anonymous namespace

TEST_CASE("TEXS swizzle", "[shader]") {
    Texture texture(512, 512, 1, 4, DkImageType_2D);
    texture.FillColor(0, 255, 255, 0, 255);
    texture.FillColor(1, 0, 255, 0, 0);
    texture.FillColor(2, 0, 0, 255, 0);
    texture.FillColor(3, 255, 255, 255, 255);

    Runner util{texture};

    REQUIRE(util.Run(MakeShader(R"(
        TEXS.LZ RZ, R0, R4, R5, 0x28, 2D, G;
    )")) == Color{1, 0, 0, 0});

    REQUIRE(util.Run(MakeShader(R"(
        TEXS.LZ RZ, R0, R4, R5, 0x28, 2D, B;
    )")) == Color{0, 0, 0, 0});

    REQUIRE(util.Run(MakeShader(R"(
        TEXS.LZ RZ, R0, R4, R5, 0x28, 2D, A;
    )")) == Color{1, 0, 0, 0});

    REQUIRE(util.Run(MakeShader(R"(
        TEXS.LZ RZ, R0, R4, R5, 0x28, 2D, RG;
    )")) == Color{1, 1, 0, 0});

    REQUIRE(util.Run(MakeShader(R"(
        TEXS.LZ RZ, R0, R4, R5, 0x28, 2D, RA;
    )")) == Color{1, 1, 0, 0});

    REQUIRE(util.Run(MakeShader(R"(
        TEXS.LZ RZ, R0, R4, R5, 0x28, 2D, GA;
    )")) == Color{1, 1, 0, 0});

    REQUIRE(util.Run(MakeShader(R"(
        TEXS.LZ RZ, R0, R4, R5, 0x28, 2D, BA;
    )")) == Color{0, 1, 0, 0});

    REQUIRE(util.Run(MakeShader(R"(
        TEXS.LZ R2, R0, R4, R5, 0x28, 2D, RGB;
    )")) == Color{1, 1, 0, 0});

    REQUIRE(util.Run(MakeShader(R"(
        TEXS.LZ R2, R0, R4, R5, 0x28, 2D, RGA;
    )")) == Color{1, 1, 1, 0});

    REQUIRE(util.Run(MakeShader(R"(
        TEXS.LZ R2, R0, R4, R5, 0x28, 2D, RBA;
    )")) == Color{1, 0, 1, 0});

    REQUIRE(util.Run(MakeShader(R"(
        TEXS.LZ R2, R0, R4, R5, 0x28, 2D, GBA;
    )")) == Color{1, 0, 1, 0});

    REQUIRE(util.Run(MakeShader(R"(
        TEXS.LZ R2, R0, R4, R5, 0x28, 2D, RGBA;
    )")) == Color{1, 1, 0, 1});
}

TEST_CASE("TEXS fp16 swizzle", "[shader]") {
    Texture texture(512, 512, 1, 4, DkImageType_2D);
    texture.FillColor(0, 255, 255, 0, 255);
    texture.FillColor(1, 0, 255, 0, 0);
    texture.FillColor(2, 0, 0, 255, 0);
    texture.FillColor(3, 255, 255, 255, 255);

    Runner util{texture};

    REQUIRE(util.RunF16(MakeShader(R"(
        TEXS.F16.LZ RZ, R0, R4, R5, 0x28, 2D, G;
    )")) == Color{1, 0, 0, 0});

    REQUIRE(util.RunF16(MakeShader(R"(
        TEXS.F16.LZ RZ, R0, R4, R5, 0x28, 2D, B;
    )")) == Color{0, 0, 0, 0});

    REQUIRE(util.RunF16(MakeShader(R"(
        TEXS.F16.LZ RZ, R0, R4, R5, 0x28, 2D, A;
    )")) == Color{1, 0, 0, 0});

    REQUIRE(util.RunF16(MakeShader(R"(
        TEXS.F16.LZ RZ, R0, R4, R5, 0x28, 2D, RG;
    )")) == Color{1, 1, 0, 0});

    REQUIRE(util.RunF16(MakeShader(R"(
        TEXS.F16.LZ RZ, R0, R4, R5, 0x28, 2D, RA;
    )")) == Color{1, 1, 0, 0});

    REQUIRE(util.RunF16(MakeShader(R"(
        TEXS.F16.LZ RZ, R0, R4, R5, 0x28, 2D, GA;
    )")) == Color{1, 1, 0, 0});

    REQUIRE(util.RunF16(MakeShader(R"(
        TEXS.F16.LZ RZ, R0, R4, R5, 0x28, 2D, BA;
    )")) == Color{0, 1, 0, 0});

    REQUIRE(util.RunF16(MakeShader(R"(
        TEXS.F16.LZ R1, R0, R4, R5, 0x28, 2D, RGB;
    )")) == Color{1, 1, 0, 0});

    REQUIRE(util.RunF16(MakeShader(R"(
        TEXS.F16.LZ R1, R0, R4, R5, 0x28, 2D, RGA;
    )")) == Color{1, 1, 1, 0});

    REQUIRE(util.RunF16(MakeShader(R"(
        TEXS.F16.LZ R1, R0, R4, R5, 0x28, 2D, RBA;
    )")) == Color{1, 0, 1, 0});

    REQUIRE(util.RunF16(MakeShader(R"(
        TEXS.F16.LZ R1, R0, R4, R5, 0x28, 2D, GBA;
    )")) == Color{1, 0, 1, 0});

    REQUIRE(util.RunF16(MakeShader(R"(
        TEXS.F16.LZ R1, R0, R4, R5, 0x28, 2D, RGBA;
    )")) == Color{1, 1, 0, 1});
}

TEST_CASE("TEXS 2D", "[shader]") {
    Texture texture(512, 512, 1, 4, DkImageType_2D);
    texture.FillColor(0, 255, 255, 0, 255);
    texture.FillColor(1, 0, 255, 0, 0);
    texture.FillColor(2, 0, 0, 255, 0);
    texture.FillColor(3, 255, 255, 255, 255);

    Runner util{texture};

    REQUIRE(util.Run(MakeShader(R"(
        TEXS.LZ R2, R0, R4, RZ, 0x28, 1D, RGBA;
    )")) == Color{0, 0, 0, 0});

    REQUIRE(util.Run(MakeShader(R"(
        TEXS R2, R0, R4, R5, 0x28, 2D, RGBA;
    )")) == Color{1, 1, 0, 1});

    REQUIRE(util.Run(MakeShader(R"(
        TEXS.LZ R2, R0, R4, R5, 0x28, 2D, RGBA;
    )")) == Color{1, 1, 0, 1});

    REQUIRE(util.Run(MakeShader(R"(
        MOV32I R6, 0x00000000;
        TEXS.LL R2, R0, R4, R6, 0x28, 2D, RGBA;
    )")) == Color{1, 1, 0, 1});

    REQUIRE(util.Run(MakeShader(R"(
        MOV32I R6, 0x3f800000;
        TEXS.LL R2, R0, R4, R6, 0x28, 2D, RGBA;
    )")) == Color{0, 1, 0, 0});

    REQUIRE(util.Run(MakeShader(R"(
        MOV32I R6, 0x40000000;
        TEXS.LL R2, R0, R4, R6, 0x28, 2D, RGBA;
    )")) == Color{0, 0, 1, 0});

    REQUIRE(util.Run(MakeShader(R"(
        MOV32I R6, 0x40400000;
        TEXS.LL R2, R0, R4, R6, 0x28, 2D, RGBA;
    )")) == Color{1, 1, 1, 1});
}

TEST_CASE("TEXS 2D DC", "[shader]") {
    Texture texture(512, 512, 1, 4, DkImageType_2D, true);
    texture.FillColor(0, 0.7f);
    texture.FillColor(1, 0.3f);

    Runner util{texture};

    REQUIRE(util.Run(MakeShader(R"(
        MOV32I R6, 0x3F4CCCCD;
        TEXS.DC R2, R0, R4, R6, 0x28, 2D, RGBA;
    )")) == Color{0, 0, 0, 1});

    REQUIRE(util.Run(MakeShader(R"(
        MOV32I R6, 0x3F19999A;
        TEXS.DC R2, R0, R4, R6, 0x28, 2D, RGBA;
    )")) == Color{1, 1, 1, 1});

    REQUIRE(util.Run(MakeShader(R"(
        MOV32I R6, 0x3F19999A;
        TEXS.LZ.DC R2, R0, R4, R6, 0x28, 2D, RGBA;
    )")) == Color{1, 1, 1, 1});

    REQUIRE(util.Run(MakeShader(R"(
        MOV32I R6, 0x3F800000;
        MOV32I R7, 0x3F19999A;
        TEXS.LZ.DC R2, R0, R4, R6, 0x28, 2D, RGBA;
    )")) == Color{0, 0, 0, 1});

    REQUIRE(util.Run(MakeShader(R"(
        MOV32I R6, 0x3F800000;
        MOV32I R7, 0;
        TEXS.LZ.DC R2, R0, R4, R6, 0x28, 2D, RGBA;
    )")) == Color{0, 0, 0, 1});

    REQUIRE(util.Run(MakeShader(R"(
        MOV32I R6, 0x3F800000;
        MOV32I R7, 0x3F19999A;
        TEXS.LZ.DC R2, R0, R4, R6, 0x28, 2D, GBA;
    )")) == Color{0, 0, 1, 0});
}

TEST_CASE("TEXS ARRAY_2D", "[shader]") {
    Texture texture(512, 512, 4, 4, DkImageType_2DArray);
    texture.FillColor(0, 255, 0, 0, 0);
    texture.FillColor(1, 0, 255, 0, 0);
    texture.FillColor(2, 0, 0, 255, 0);
    texture.FillColor(3, 0, 0, 0, 255);

    // Set the first pixel as black
    static_cast<uint8_t*>(texture.CpuAddr())[0] = 0;

    Runner util{texture};

    REQUIRE(util.Run(MakeShader(R"(
        MOV32I R4, 0;
        MOV32I R5, 0;
        MOV32I R6, 0;
        TEXS R2, R0, R4, R6, 0x28, ARRAY_2D, RGBA;
    )")) == Color{0, 0, 0, 0});

    REQUIRE(util.Run(MakeShader(R"(
        MOV32I R4, 1;
        MOV32I R5, 0;
        MOV32I R6, 0;
        TEXS R2, R0, R4, R6, 0x28, ARRAY_2D, RGBA;
    )")) == Color{1, 0, 0, 0});

    REQUIRE(util.Run(MakeShader(R"(
        MOV32I R4, 0x3F800000;
        MOV32I R5, 0;
        MOV32I R6, 0;
        TEXS R2, R0, R4, R6, 0x28, ARRAY_2D, RGBA;
    )")) == Color{0, 0, 0, 0});

    REQUIRE(util.Run(MakeShader(R"(
        MOV32I R4, 0;
        MOV32I R5, 0x3F000000;
        MOV32I R6, 0;
        TEXS R2, R0, R4, R6, 0x28, ARRAY_2D, RGBA;
    )")) == Color{1, 0, 0, 0});

    REQUIRE(util.Run(MakeShader(R"(
        MOV32I R4, 0;
        MOV32I R5, 0;
        MOV32I R6, 0x3F000000;
        TEXS R2, R0, R4, R6, 0x28, ARRAY_2D, RGBA;
    )")) == Color{1, 0, 0, 0});

    REQUIRE(util.Run(MakeShader(R"(
        MOV32I R4, 0;
        MOV32I R5, 0;
        MOV32I R6, 0x3F000000;
        TEXS.LZ R2, R0, R4, R6, 0x28, ARRAY_2D, RGBA;
    )")) == Color{1, 0, 0, 0});
}

TEST_CASE("TEXS ARRAY_2D DC", "[shader]") {
    Texture texture(512, 512, 4, 1, DkImageType_2DArray, true);
    texture.FillColor(0, 0.7f);

    Runner util{texture};

    REQUIRE(util.Run(MakeShader(R"(
        MOV R6, R4;
        MOV R4, RZ;
        MOV32I R6, 0x3F4CCCCD;
        TEXS.DC R2, R0, R4, R6, 0x28, 2D, RGBA;
    )")) == Color{0, 0, 0, 1});

    REQUIRE(util.Run(MakeShader(R"(
        MOV R6, R4;
        MOV R4, RZ;
        MOV32I R7, 0x3F19999A;
        TEXS.DC R2, R0, R4, R6, 0x28, 2D, RGBA;
    )")) == Color{1, 1, 1, 1});
}

TEST_CASE("TEXS 3D", "[shader]") {
    Texture texture(32, 32, 32, 1, DkImageType_3D);
    texture.FillColor(0, 255, 255, 0, 255);

    Runner util{texture};

    REQUIRE(util.Run(MakeShader(R"(
        MOV R6, R4;
        TEXS R2, R0, R4, R6, 0x28, 3D, RGBA;
    )")) == Color{1, 1, 0, 1});

    REQUIRE(util.Run(MakeShader(R"(
        MOV R6, R4;
        TEXS.LZ R2, R0, R4, R6, 0x28, 3D, RGBA;
    )")) == Color{1, 1, 0, 1});
}

TEST_CASE("TEXS CUBE", "[shader]") {
    Texture texture(512, 512, 6, 2, DkImageType_Cubemap);
    texture.FillColor(0, 255, 255, 0, 255);
    texture.FillColor(1, 0, 0, 255, 255);

    Runner util{texture};

    REQUIRE(util.Run(MakeShader(R"(
        MOV R6, R4;
        TEXS R2, R0, R4, R6, 0x28, CUBE, RGBA;
    )")) == Color{1, 1, 0, 1});

    REQUIRE(util.Run(MakeShader(R"(
        MOV R6, R4;
        MOV32I R7, 0;
        TEXS.LL R2, R0, R4, R6, 0x28, CUBE, RGBA;
    )")) == Color{1, 1, 0, 1});

    REQUIRE(util.Run(MakeShader(R"(
        MOV R6, R4;
        MOV32I R7, 0x3f800000;
        TEXS.LL R2, R0, R4, R6, 0x28, CUBE, RGBA;
    )")) == Color{0, 0, 1, 1});
}
