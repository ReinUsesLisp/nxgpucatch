
#include <random>
#include <catch2/catch_test_macros.hpp>

#include "bit_cast.h"
#include "cmd_util.h"
#include "descriptor_set.h"
#include "heap.h"
#include "resource.h"
#include "shader.h"

namespace {
using Color = std::array<float, 4>;

Shader MakeShader(std::string code) {
    code = ".dksh compute\n"
           "main:\n"
           "MOV32I R0, 0x3f000000;"
           "MOV32I R1, 0x3f000000;"
           "MOV32I R2, 0x3f000000;"
           "MOV32I R3, 0x3f000000;"
           "MOV32I R4, 0x3f400000;"
           "MOV32I R5, 0x3f000000;"
           "MOV32I R6, 0x3f000000;"
           "MOV32I R7, 0x3f000000;\n" +
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
        ++index;
    }
}

void RandomizeShadow(Texture& texture, unsigned seed) {
    std::mt19937 rng{seed};
    const size_t size = texture.SizeBytes();
    auto data = static_cast<float*>(texture.CpuAddr());
    for (size_t index = 0; index < size / 4; ++index) {
        data[index] = (rng() % 4096) / 4096.0f - 2048.0f;
        ++index;
    }
}
} // Anonymous namespace

TEST_CASE("TLD4 2D", "[shader]") {
    Texture texture(512, 512, 2, 1, DkImageType_2DArray);
    Randomize(texture, 4);

    Runner util{texture};

    SECTION("Basic") {
        REQUIRE(util.Run(MakeShader("TLD4.R PT, R0, R0, RZ, 0x28, 2D, 0xf;")) == Color{0, 0, 1, 0});
        REQUIRE(util.Run(MakeShader("TLD4.G PT, R0, R0, RZ, 0x28, 2D, 0xf;")) == Color{0, 0, 0, 0});
        REQUIRE(util.Run(MakeShader("TLD4.B PT, R0, R0, RZ, 0x28, 2D, 0xf;")) == Color{1, 1, 1, 0});
        REQUIRE(util.Run(MakeShader("TLD4.A PT, R0, R0, RZ, 0x28, 2D, 0xf;")) == Color{0, 0, 0, 0});

        REQUIRE(util.Run(MakeShader(R"(
            MOV R0, 1;
            TLD4.R PT, R0, R0, RZ, 0x28, ARRAY_2D, 0xf;
        )")) == Color{1, 0, 1, 1});
    }
    SECTION("Mask") {
        REQUIRE(util.Run(MakeShader("TLD4.B PT, R0, R0, RZ, 0x28, 2D, 0x1;")) ==
                Color{1, 0.5, 0.5, 0.5});
        REQUIRE(util.Run(MakeShader("TLD4.B PT, R0, R0, RZ, 0x28, 2D, 0x2;")) ==
                Color{1, 0.5, 0.5, 0.5});
        REQUIRE(util.Run(MakeShader("TLD4.B PT, R0, R0, RZ, 0x28, 2D, 0x4;")) ==
                Color{1, 0.5, 0.5, 0.5});
        REQUIRE(util.Run(MakeShader("TLD4.B PT, R0, R0, RZ, 0x28, 2D, 0x8;")) ==
                Color{0, 0.5, 0.5, 0.5});
        REQUIRE(util.Run(MakeShader("TLD4.B PT, R0, R0, RZ, 0x28, 2D, 0x3;")) ==
                Color{1, 1, 0.5, 0.5});
        REQUIRE(util.Run(MakeShader("TLD4.B PT, R0, R0, RZ, 0x28, 2D, 0x9;")) ==
                Color{1, 0, 0.5, 0.5});
    }
    SECTION("AOFFI") {
        REQUIRE(util.Run(MakeShader(R"(
            MOV R0, 1;
            MOV32I R4, 0xffffffff;
            TLD4.R.AOFFI PT, R0, R0, R4, 0x28, ARRAY_2D, 0xf;
        )")) == Color{1, 1, 0, 1});

        REQUIRE(util.Run(MakeShader(R"(
            MOV R0, 1;
            MOV32I R4, 0xcccccccc;
            TLD4.R.AOFFI PT, R0, R0, R4, 0x28, ARRAY_2D, 0xf;
        )")) == Color{1, 0, 0, 1});

        REQUIRE(util.Run(MakeShader(R"(
            MOV32I R4, 0xffffffff;
            TLD4.R.AOFFI PT, R0, R0, R4, 0x28, 2D, 0xf;
        )")) == Color{0, 0, 0, 0});

        REQUIRE(util.Run(MakeShader(R"(
            MOV32I R4, 0xcccccccc;
            TLD4.R.AOFFI PT, R0, R0, R4, 0x28, 2D, 0xf;
        )")) == Color{1, 0, 1, 1});

        REQUIRE(util.Run(MakeShader(R"(
            MOV32I R4, 0x7cda73ac;
            TLD4.R.AOFFI PT, R0, R0, R4, 0x28, 2D, 0xf;
        )")) == Color{0, 0, 0, 0});

        REQUIRE(util.Run(MakeShader(R"(
            MOV R4, c[2][0];
            MOV32I R5, 0xcccccccc;
            TLD4.R.B.AOFFI PT, R0, R0, R4, 0, 2D, 0xf;
        )")) == Color{1, 0, 1, 1});
    }
    SECTION("PTP") {
        REQUIRE(util.Run(MakeShader(R"(
            MOV R0, 1;
            MOV32I R4, 0xffffffff;
            MOV32I R5, 0xcccccccc;
            TLD4.R.PTP PT, R0, R0, R4, 0x28, ARRAY_2D, 0xf;
        )")) == Color{1, 1, 1, 1});

        REQUIRE(util.Run(MakeShader(R"(
            MOV R0, 1;
            MOV32I R4, 0xcccccccc;
            MOV32I R5, 0xffffffff;
            TLD4.R.PTP PT, R0, R0, R4, 0x28, ARRAY_2D, 0xf;
        )")) == Color{1, 1, 1, 1});

        REQUIRE(util.Run(MakeShader(R"(
            MOV32I R4, 0xffffffff;
            MOV32I R5, 0xcccccccc;
            TLD4.R.PTP PT, R0, R0, R4, 0x28, 2D, 0xf;
        )")) == Color{0, 0, 1, 1});

        REQUIRE(util.Run(MakeShader(R"(
            MOV32I R4, 0xcccccccc;
            MOV32I R5, 0xffffffff;
            TLD4.R.PTP PT, R0, R0, R4, 0x28, 2D, 0xf;
        )")) == Color{1, 1, 0, 0});

        REQUIRE(util.Run(MakeShader(R"(
            MOV32I R4, 0x7cda73ac;
            MOV32I R5, 0xadf8e2a9;
            TLD4.R.PTP PT, R0, R0, R4, 0x28, 2D, 0xf;
        )")) == Color{0, 1, 1, 0});

        REQUIRE(util.Run(MakeShader(R"(
            MOV R4, c[2][0];
            MOV32I R5, 0x7cda73ac;
            MOV32I R6, 0xadf8e2a9;
            TLD4.R.B.PTP PT, R0, R0, R4, 0, 2D, 0xf;
        )")) == Color{0, 1, 1, 0});
    }
    SECTION("Sparse") {
        REQUIRE(util.Run(MakeShader(R"(
            MOV R0, 1;
            ISETP.F.AND P0, PT, RZ, RZ, PT;
            TLD4.R P0, R0, R0, RZ, 0x28, ARRAY_2D, 0xf;
            @P0 FADD.FTZ R0, RZ, -2;
            @!P0 FADD.FTZ R0, RZ, 2;
        )")) == Color{2, 0, 1, 1});

        REQUIRE(util.Run(MakeShader(R"(
            MOV R0, 1;
            ISETP.T.AND P0, PT, RZ, RZ, PT;
            TLD4.R P0, R0, R0, RZ, 0x28, ARRAY_2D, 0xf;
            @P0 FADD.FTZ R0, RZ, -2;
            @!P0 FADD.FTZ R0, RZ, 2;
        )")) == Color{2, 0, 1, 1});
    }
}

TEST_CASE("TLD4 Cube", "[shader]") {
    Texture texture(512, 512, 12, 1, DkImageType_CubemapArray);
    Randomize(texture, 7);

    Runner util{texture};

    REQUIRE(util.Run(MakeShader("TLD4.R PT, R0, R0, RZ, 0x28, CUBE, 0xf;")) == Color{1, 1, 1, 1});
    REQUIRE(util.Run(MakeShader("TLD4.G PT, R0, R0, RZ, 0x28, CUBE, 0xf;")) == Color{0, 0, 0, 0});
    REQUIRE(util.Run(MakeShader("TLD4.B PT, R0, R0, RZ, 0x28, CUBE, 0xf;")) == Color{0, 1, 1, 0});
    REQUIRE(util.Run(MakeShader("TLD4.A PT, R0, R0, RZ, 0x28, CUBE, 0xf;")) == Color{0, 0, 0, 0});

    REQUIRE(util.Run(MakeShader(R"(
        MOV R0, 1;
        TLD4.R PT, R0, R0, RZ, 0x28, ARRAY_CUBE, 0xf;
    )")) == Color{1, 0, 0, 1});

    REQUIRE(util.Run(MakeShader(R"(
        MOV R0, 1;
        MOV R4, c[2][0];
        TLD4.R.B PT, R0, R0, R4, 0, ARRAY_CUBE, 0xf;
    )")) == Color{1, 0, 0, 1});
}

TEST_CASE("TLD4 Cube Shadow", "[shader]") {
    Texture texture(512, 512, 12, 1, DkImageType_CubemapArray, true);
    RandomizeShadow(texture, 14);

    Runner util{texture};

    REQUIRE(util.Run(MakeShader(R"(
        FADD R4, RZ, -1;
        TLD4.R.DC PT, R0, R0, R4, 0x28, CUBE, 0xf;
    )")) == Color{1, 0, 0, 1});

    REQUIRE(util.Run(MakeShader(R"(
        MOV R0, 1;
        FADD R4, RZ, -1;
        TLD4.R.DC PT, R0, R0, R4, 0x28, ARRAY_CUBE, 0xf;
    )")) == Color{1, 0, 0, 1});

    REQUIRE(util.Run(MakeShader(R"(
        MOV R0, 1;
        MOV R4, c[2][0];
        FADD R5, RZ, -1;
        TLD4.R.B.DC PT, R0, R0, R4, 0, ARRAY_CUBE, 0xf;
    )")) == Color{1, 0, 0, 1});
}

TEST_CASE("TLD4 2D Shadow", "[shader]") {
    Texture texture(512, 512, 2, 1, DkImageType_2DArray, true);
    RandomizeShadow(texture, 13);

    Runner util{texture};
    auto X = [&](std::string code) { return util.Run(MakeShader(code)); };

    SECTION("Basic") {
        REQUIRE(X("TLD4.R.DC PT, R0, R0, R4, 0x28, 2D, 0xf;") == Color{0, 0, 0, 0});
        REQUIRE(X("TLD4.G.DC PT, R0, R0, R4, 0x28, 2D, 0xf;") == Color{0, 0, 0, 0});
        REQUIRE(X("TLD4.B.DC PT, R0, R0, R4, 0x28, 2D, 0xf;") == Color{0, 0, 0, 0});
        REQUIRE(X("TLD4.A.DC PT, R0, R0, R4, 0x28, 2D, 0xf;") == Color{0, 0, 0, 0});

        REQUIRE(X(R"(
            MOV R0, 1;
            TLD4.R.DC PT, R0, R0, R4, 0x28, ARRAY_2D, 0xf;
        )") == Color{0, 0, 0, 0});

        REQUIRE(X(R"(
            MOV R0, 1;
            FADD.FTZ R4, RZ, -1;
            TLD4.R.DC PT, R0, R0, R4, 0x28, ARRAY_2D, 0xf;
        )") == Color{1, 0, 0, 1});

        REQUIRE(X(R"(
            MOV R0, 1;
            MOV R4, c[2][0];
            FADD.FTZ R5, RZ, -1;
            TLD4.R.B.DC PT, R0, R0, R4, 0, ARRAY_2D, 0xf;
        )") == Color{1, 0, 0, 1});
    }
    SECTION("Mask") {
        REQUIRE(X(R"(
            MOV R0, 1;
            FADD.FTZ R4, RZ, -1;
            TLD4.R.DC PT, R0, R0, R4, 0x28, ARRAY_2D, 0x1;
        )") == Color{1, 0.5, 0.5, 0.5});

        REQUIRE(X(R"(
            MOV R0, 1;
            FADD.FTZ R4, RZ, -1;
            TLD4.R.DC PT, R0, R0, R4, 0x28, ARRAY_2D, 0x9;
        )") == Color{1, 1, 0.5, 0.5});
    }
    SECTION("AOFFI") {
        REQUIRE(X(R"(
            MOV R0, 1;
            MOV R4, c[2][0];
            MOV32I R5, 0xcccccccc;
            FADD.FTZ R6, RZ, -1;
            TLD4.R.B.AOFFI.DC PT, R0, R0, R4, 0, ARRAY_2D, 0xf;
        )") == Color{1, 0, 0, 1});
    }
    SECTION("PTP") {
        REQUIRE(X(R"(
            MOV R0, 1;
            MOV R4, c[2][0];
            MOV32I R5, 0xcccccccc;
            MOV32I R6, 0xdddddddd;
            FADD.FTZ R7, RZ, -1;
            TLD4.R.B.PTP.DC PT, R0, R0, R4, 0, ARRAY_2D, 0xf;
        )") == Color{1, 1, 0, 0});
    }
}
