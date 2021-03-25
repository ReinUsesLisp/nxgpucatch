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
    for (size_t index = 0; index < size; ++index) {
        data[index] = rng() % 2 != 0 ? 255 : 0;
    }
}
} // Anonymous namespace

#define X(code) util.Run(MakeShader(code))

TEST_CASE("TLD 1D", "[shader]") {
    Texture texture(512, 1, 1, 4, DkImageType_1D);
    Randomize(texture, 129034);
    Runner util{texture};

    REQUIRE(X("MOV R0, 3; TLD.LZ PT, R0, R0, RZ, 0x28, 1D, 0xf;") == Color{0, 0, 0, 0});
    REQUIRE(X("MOV R0, 17; TLD.LZ PT, R0, R0, RZ, 0x28, 1D, 0xf;") == Color{1, 0, 0, 1});
    REQUIRE(X("MOV R0, -1; TLD.LZ PT, R0, R0, RZ, 0x28, 1D, 0xf;") == Color{0, 0, 0, 0});
}

TEST_CASE("TLD ARRAY_1D", "[shader]") {
    Texture texture(512, 17, 1, 4, DkImageType_1DArray);
    Randomize(texture, 129034);
    Runner util{texture};

    REQUIRE(X("MOV R0, 3; MOV R1, 5; TLD.LZ PT, R0, R0, RZ, 0x28, ARRAY_1D, 0xf;") == Color{1, 0, 0, 1});
    REQUIRE(X("MOV R0, 3; MOV R1, 6; TLD.LZ PT, R0, R0, RZ, 0x28, ARRAY_1D, 0xf;") == Color{0, 1, 1, 1});
    REQUIRE(X("MOV R0, 3; MOV R1, 7; TLD.LZ PT, R0, R0, RZ, 0x28, ARRAY_1D, 0xf;") == Color{0, 0, 0, 1});
}

TEST_CASE("TLD 2D", "[shader]") {
    Texture texture(512, 256, 1, 6, DkImageType_2D);
    Randomize(texture, 129034);
    Runner util{texture};

    // clang-format off
    REQUIRE(X("MOV R0, 3; MOV R1, 5; MOV R2, 4; TLD.LZ PT, R0, R0, RZ, 0x28, 2D, 0xf;") == Color{1, 0, 1, 1});
    REQUIRE(X("MOV R0, 6; MOV R1, 7; MOV R2, 4; TLD.LZ PT, R0, R0, RZ, 0x28, 2D, 0xf;") == Color{1, 0, 0, 1});
    REQUIRE(X("MOV R0, 4; MOV R1, 8; MOV R2, 4; TLD.LZ PT, R0, R0, RZ, 0x28, 2D, 0xf;") == Color{1, 1, 0, 1});
    REQUIRE(X("MOV R0, 2; MOV R1, 1; MOV R2, 4; TLD.LZ PT, R0, R0, RZ, 0x28, 2D, 0xf;") == Color{0, 1, 1, 0});

    REQUIRE(X("MOV R0, 2; MOV R1, 1; MOV R2, 3; TLD.LL PT, R0, R0, R2, 0x28, 2D, 0xf;") == Color{1, 0, 1, 0});
    REQUIRE(X("MOV R0, 2; MOV R1, 1; MOV R2, 4; TLD.LL PT, R0, R0, R2, 0x28, 2D, 0xf;") == Color{1, 1, 1, 1});
    REQUIRE(X("MOV R0, 2; MOV R1, 1; MOV R2, 5; TLD.LL PT, R0, R0, R2, 0x28, 2D, 0xf;") == Color{1, 1, 1, 1});
    // clang-format on
}

TEST_CASE("TLD ARRAY_2D", "[shader]") {
    Texture texture(512, 256, 8, 6, DkImageType_2DArray);
    Randomize(texture, 4142);
    Runner util{texture};

    // clang-format off
    REQUIRE(X("MOV R0, 3; MOV R1, 5; MOV R2, 4; TLD.LZ PT, R0, R0, RZ, 0x28, ARRAY_2D, 0xf;") == Color{0, 0, 0, 0});
    REQUIRE(X("MOV R0, 6; MOV R1, 7; MOV R2, 4; TLD.LZ PT, R0, R0, RZ, 0x28, ARRAY_2D, 0xf;") == Color{0, 0, 1, 0});
    REQUIRE(X("MOV R0, 4; MOV R1, 8; MOV R2, 4; TLD.LZ PT, R0, R0, RZ, 0x28, ARRAY_2D, 0xf;") == Color{1, 1, 0, 1});
    REQUIRE(X("MOV R0, 2; MOV R1, 1; MOV R2, 5; TLD.LZ PT, R0, R0, RZ, 0x28, ARRAY_2D, 0xf;") == Color{0, 0, 1, 0});
    // clang-format on
}

TEST_CASE("TLD 3D", "[shader]") {
    Texture texture(32, 32, 32, 6, DkImageType_3D);
    Randomize(texture, 4142);
    Runner util{texture};

    // clang-format off
    REQUIRE(X("MOV R0, 3; MOV R1, 5; MOV R2, 4; TLD.LZ PT, R0, R0, RZ, 0x28, 3D, 0xf;") == Color{0, 1, 0, 1});
    REQUIRE(X("MOV R0, 6; MOV R1, 7; MOV R2, 4; TLD.LZ PT, R0, R0, RZ, 0x28, 3D, 0xf;") == Color{1, 0, 0, 0});
    REQUIRE(X("MOV R0, 4; MOV R1, 8; MOV R2, 4; TLD.LZ PT, R0, R0, RZ, 0x28, 3D, 0xf;") == Color{0, 0, 1, 0});
    REQUIRE(X("MOV R0, 2; MOV R1, 1; MOV R2, 5; TLD.LZ PT, R0, R0, RZ, 0x28, 3D, 0xf;") == Color{0, 1, 1, 0});
    // clang-format on
}

TEST_CASE("TLD Mask", "[shader]") {
    Texture texture(512, 256, 1, 6, DkImageType_2D);
    Randomize(texture, 123);
    Runner util{texture};

    REQUIRE(X("MOV R0, 3; MOV R1, 0; TLD.LZ PT, R0, R0, RZ, 0x28, 2D, 0x1;")[0] == 0);
    REQUIRE(X("MOV R0, 3; MOV R1, 0; TLD.LZ PT, R0, R0, RZ, 0x28, 2D, 0x2;")[0] == 1);
    REQUIRE(X("MOV R0, 3; MOV R1, 0; TLD.LZ PT, R0, R0, RZ, 0x28, 2D, 0x4;")[0] == 0);
    REQUIRE(X("MOV R0, 3; MOV R1, 0; TLD.LZ PT, R0, R0, RZ, 0x28, 2D, 0x8;")[0] == 1);

    REQUIRE(X("MOV R0, 3; MOV R1, 0; TLD.LZ PT, R0, R0, RZ, 0x28, 2D, 0x5;")[1] == 0);
}

TEST_CASE("TLD AOFFI", "[shader]") {
    Texture texture(512, 512, 1, 6, DkImageType_2D);
    Randomize(texture, 1);
    Runner util{texture};

    // clang-format off
    REQUIRE(X("MOV R0, 32; MOV R1, 32; MOV R4, 0x00; TLD.LZ.AOFFI PT, R0, R0, R4, 0x28, 2D, 0xf;") == Color{0, 1, 0, 0});
    REQUIRE(X("MOV R0, 32; MOV R1, 32; MOV R4, 0xff; TLD.LZ.AOFFI PT, R0, R0, R4, 0x28, 2D, 0xf;") == Color{1, 0, 0, 0});
    REQUIRE(X("MOV R0, 32; MOV R1, 32; MOV R4, 0x03; TLD.LZ.AOFFI PT, R0, R0, R4, 0x28, 2D, 0xf;") == Color{1, 0, 0, 1});

    REQUIRE(X("MOV R0, 32; MOV R1, 32; MOV R4, 0; MOV R5, 0x03; TLD.LL.AOFFI PT, R0, R0, R4, 0x28, 2D, 0xf;") == Color{1, 0, 0, 1});
    REQUIRE(X("MOV R0, 32; MOV R1, 32; MOV R4, 1; MOV R5, 0x03; TLD.LL.AOFFI PT, R0, R0, R4, 0x28, 2D, 0xf;") == Color{1, 1, 1, 0});
    REQUIRE(X("MOV R0, 32; MOV R1, 32; MOV R4, 2; MOV R5, 0x03; TLD.LL.AOFFI PT, R0, R0, R4, 0x28, 2D, 0xf;") == Color{1, 0, 1, 0});
    // clang-format on
}

TEST_CASE("TLD Bindless", "[shader]") {
    Texture texture(512, 17, 1, 4, DkImageType_1DArray);
    Randomize(texture, 129034);
    Runner util{texture};

    // clang-format off
    REQUIRE(X("MOV R4, c[2][0]; MOV R0, 3; MOV R1, 5; TLD.B.LZ PT, R0, R0, R4, 0, ARRAY_1D, 0xf;") == Color{1, 0, 0, 1});
    REQUIRE(X("MOV R4, c[2][0]; MOV R0, 3; MOV R1, 6; TLD.B.LZ PT, R0, R0, R4, 0, ARRAY_1D, 0xf;") == Color{0, 1, 1, 1});
    REQUIRE(X("MOV R4, c[2][0]; MOV R0, 3; MOV R1, 7; TLD.B.LZ PT, R0, R0, R4, 0, ARRAY_1D, 0xf;") == Color{0, 0, 0, 1});
    // clang-format on
}

TEST_CASE("TLD Sparse", "[shader]") {
    Texture texture(512, 17, 1, 4, DkImageType_1DArray);
    Randomize(texture, 42);
    Runner util{texture};

    REQUIRE(X(R"(
        MOV R4, c[2][0];
        MOV R0, 3;
        MOV R1, 5;
        ISETP.T.AND P3, PT, RZ, RZ, PT;
        TLD.B.LZ P3, R0, R0, R4, 0, ARRAY_1D, 0xf;
    @P3 FADD.FTZ R0, RZ, 2;
   @!P3 FADD.FTZ R0, RZ, 4;
    )")[0] == 4);
}

TEST_CASE("TLD Multisample", "[shader]") {
    Texture texture(64, 64, 1, 1, DkImageType_2DMS, false, DkMsMode_8x);
    Randomize(texture, 46);
    Runner util{texture};

    REQUIRE(X(R"(
        MOV R4, c[2][0];
        MOV R5, 0;
        MOV R0, 3;
        MOV R1, 5;
        TLD.B.LZ.MS PT, R0, R0, R4, 0, 2D, 0xf;
    )") == Color{1, 1, 0, 0});

    REQUIRE(X(R"(
        MOV R4, c[2][0];
        MOV R5, 4;
        MOV R0, 3;
        MOV R1, 5;
        TLD.B.LZ.MS PT, R0, R0, R4, 0, 2D, 0xf;
    )") == Color{0, 1, 1, 0});
}

TEST_CASE("TLD Multisample Array", "[shader]") {
    Texture texture(64, 64, 4, 1, DkImageType_2DMSArray, false, DkMsMode_8x);
    Randomize(texture, 46);
    Runner util{texture};

    REQUIRE(X(R"(
        MOV R4, c[2][0];
        MOV R5, 0;
        MOV R0, 3;
        MOV R1, 5;
        MOV R2, 5;
        TLD.B.LZ.MS PT, R0, R0, R4, 0, ARRAY_2D, 0xf;
    )") == Color{1, 1, 1, 1});

    REQUIRE(X(R"(
        MOV R4, c[2][0];
        MOV R5, 2;
        MOV R0, 3;
        MOV R1, 5;
        MOV R2, 5;
        TLD.B.LZ.MS PT, R0, R0, R4, 0, ARRAY_2D, 0xf;
    )") == Color{0, 1, 1, 1});
}
