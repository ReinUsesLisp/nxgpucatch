#include <random>
#include <catch2/catch_test_macros.hpp>

#include "cmd_util.h"
#include "descriptor_set.h"
#include "heap.h"
#include "resource.h"
#include "shader.h"

namespace {
using Color = std::array<float, 4>;

Shader MakeShader(std::string code) {
    code = ".dksh compute\n"
           "main:\n" +
           code + R"(
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
            bindless_handle.BindUniform(cmdbuf, DkStage_Compute, 0);
        });
    }

    Color Run(const Shader& shader) {
        RecordRunWait([&](dk::CmdBuf cmdbuf) {
            shader.Bind(cmdbuf, DkStageFlag_Compute);
            cmdbuf.dispatchCompute(1, 1, 1);
        });
        return *result_heap;
    };

private:
    DescriptorSet descriptor_set;
    TypedHeap<Color> result_heap;
    TypedHeap<uint32_t> bindless_handle;
};

void Randomize(Texture& texture, unsigned seed) {
    std::mt19937 rng{seed};
    const size_t size = texture.SizeBytes();
    auto data = static_cast<uint8_t*>(texture.CpuAddr());
    for (size_t index = 0; index < size;) {
        uint32_t v = rng();
        data[index++] = (v & 1) ? 255 : 0;
        data[index++] = (v & 2) ? 255 : 0;
        data[index++] = (v & 4) ? 255 : 0;
        data[index++] = (v & 8) ? 255 : 0;
    }
}
} // Anonymous namespace

#define X(code) util.Run(MakeShader(code))

TEST_CASE("TLDS 1D", "[shader]") {
    Texture texture(256, 256, 1, 4, DkImageType_1D);
    Randomize(texture, 534);
    Runner util{texture};

    REQUIRE(X(R"(
        MOV R0, 3;
        MOV R1, 2;
        TLDS.LZ R2, R0, R0, RZ, 0x28, 1D, RGBA;
    )") == Color{0, 1, 1, 1});

    REQUIRE(X(R"(
        MOV R0, 3;
        MOV R1, 2;
        TLDS.LL R2, R0, R0, R1, 0x28, 1D, RGBA;
    )") == Color{1, 1, 1, 1});
}

TEST_CASE("TLDS 2D", "[shader]") {
    Texture texture(1024, 1024, 4, 5, DkImageType_2DArray);
    Randomize(texture, 450);
    Runner util{texture};

    REQUIRE(X(R"(
        MOV R0, 3;
        MOV R1, 2;
        TLDS.LZ R2, R0, R0, R1, 0x28, 2D, RGBA;
    )") == Color{0, 1, 0, 0});

    REQUIRE(X(R"(
        MOV32I R0, 3;
        MOV32I R1, 2;
        MOV32I R2, 0xffff;
        TLDS.LZ.AOFFI R2, R0, R0, R2, 0x28, 2D, RGBA;
    )") == Color{1, 1, 1, 0});

    REQUIRE(X(R"(
        MOV32I R0, 3;
        MOV32I R1, 2;
        MOV32I R2, 4;
        TLDS.LL R2, R0, R0, R2, 0x28, 2D, RGBA;
    )") == Color{1, 1, 1, 0});

    REQUIRE(X(R"(
        MOV32I R0, 3;
        MOV32I R1, 2;
        MOV32I R2, 4;
        MOV32I R3, 0xff;
        TLDS.LL.AOFFI R2, R0, R0, R2, 0x28, 2D, RGBA;
    )") == Color{0, 0, 1, 0});

    REQUIRE(X(R"(
        MOV32I R0, 3;
        MOV32I R1, 2;
        MOV32I R2, 1;
        TLDS.LZ R2, R0, R2, R0, 0x28, ARRAY_2D, RGBA;
    )") == Color{1, 0, 0, 1});
}

TEST_CASE("TLDS 2D MS", "[shader]") {
    Texture texture(256, 256, 1, 4, DkImageType_2DMS, false, DkMsMode_8x);
    Randomize(texture, 534);
    Runner util{texture};

    REQUIRE(X(R"(
        MOV R0, 3;
        MOV R1, 2;
        MOV R2, 0;
        TLDS.LZ.MS R2, R0, R0, R2, 0x28, 2D, RGBA;
    )") == Color{1, 0, 0, 0});

    REQUIRE(X(R"(
        MOV R0, 3;
        MOV R1, 2;
        MOV R2, 1;
        TLDS.LZ.MS R2, R0, R0, R2, 0x28, 2D, RGBA;
    )") == Color{1, 0, 1, 1});

    REQUIRE(X(R"(
        MOV R0, 3;
        MOV R1, 2;
        MOV R2, 2;
        TLDS.LZ.MS R2, R0, R0, R2, 0x28, 2D, RGBA;
    )") == Color{0, 1, 0, 1});

    REQUIRE(X(R"(
        MOV R0, 3;
        MOV R1, 2;
        MOV R2, 3;
        TLDS.LZ.MS R2, R0, R0, R2, 0x28, 2D, RGBA;
    )") == Color{0, 1, 0, 0});

    REQUIRE(X(R"(
        MOV R0, 3;
        MOV R1, 2;
        MOV R2, 4;
        TLDS.LZ.MS R2, R0, R0, R2, 0x28, 2D, RGBA;
    )") == Color{1, 1, 1, 0});

    REQUIRE(X(R"(
        MOV R0, 3;
        MOV R1, 2;
        MOV R2, 5;
        TLDS.LZ.MS R2, R0, R0, R2, 0x28, 2D, RGBA;
    )") == Color{0, 1, 1, 1});

    REQUIRE(X(R"(
        MOV R0, 3;
        MOV R1, 2;
        MOV R2, 6;
        TLDS.LZ.MS R2, R0, R0, R2, 0x28, 2D, RGBA;
    )") == Color{0, 0, 0, 0});

    REQUIRE(X(R"(
        MOV R0, 3;
        MOV R1, 2;
        MOV R2, 7;
        TLDS.LZ.MS R2, R0, R0, R2, 0x28, 2D, RGBA;
    )") == Color{0, 1, 1, 0});
}

TEST_CASE("TLDS 3D", "[shader]") {
    Texture texture(32, 32, 32, 2, DkImageType_3D);
    Randomize(texture, 49);
    Runner util{texture};

    REQUIRE(X(R"(
        MOV R0, 3;
        MOV R1, 2;
        MOV R2, 7;
        TLDS.LZ R2, R0, R0, R2, 0x28, 3D, RGBA;
    )") == Color{1, 1, 0, 1});

    REQUIRE(X(R"(
        MOV R0, 19;
        MOV R1, 5;
        MOV R2, 3;
        TLDS.LZ R2, R0, R0, R2, 0x28, 3D, RGBA;
    )") == Color{1, 0, 1, 1});
}
