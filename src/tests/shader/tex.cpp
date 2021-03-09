#include <catch2/catch_test_macros.hpp>

#include "cmd_util.h"
#include "heap.h"
#include "resource.h"
#include "shader.h"
#include "descriptor_set.h"

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
} // Anonymous namespace

TEST_CASE("TEX 1D Array", "[shader]") {
    Texture texture(512, 512, 1, 1, DkImageType_1DArray);
    texture.FillColor(0, 255, 255, 0, 255);

    Runner util{texture};

    REQUIRE(util.Run(MakeShader(R"(
        MOV32I R0, 0x3f000000;
        MOV32I R1, 0;
        TEX.LZ PT, R0, R0, RZ, 0x28, 1D, 0xf;
    )")) == Color{1, 1, 0, 1});

    REQUIRE(util.Run(MakeShader(R"(
        MOV32I R0, 0x3f000000;
        MOV32I R1, 0;
        TEX.LZ PT, R0, R0, RZ, 0x28, ARRAY_1D, 0xf;
    )")) == Color{1, 1, 0, 1});

    REQUIRE(util.Run(MakeShader(R"(
        MOV32I R0, 0x3f000000;
        MOV32I R1, 0;
        TEX.LZ PT, R0, R0, RZ, 0x28, 2D, 0xf;
    )")) == Color{0, 0, 0, 0});
}

TEST_CASE("TEX 2D", "[shader]") {
    Texture texture(512, 512, 1, 4, DkImageType_2D);
    texture.FillColor(0, 255, 0, 0, 0);
    texture.FillColor(1, 0, 255, 0, 0);
    texture.FillColor(2, 0, 0, 255, 0);
    texture.FillColor(3, 255, 255, 255, 255);

    // Set the first pixel as black
    static_cast<uint8_t*>(texture.CpuAddr())[0] = 0;

    Runner util{texture};

    SECTION("Lod zero") {
        REQUIRE(util.Run(MakeShader(R"(
            MOV32I R0, 0x3f000000;
            MOV32I R1, 0x3f000000;
            TEX.LZ PT, R0, R0, RZ, 0x28, 2D, 0xf;
        )")) == Color{1, 0, 0, 0});
    }
    SECTION("Lod explicit") {
        REQUIRE(util.Run(MakeShader(R"(
            MOV32I R0, 0x3f000000;
            MOV32I R1, 0x3f000000;
            MOV32I R2, 0x40000000;
            TEX.LL PT, R0, R0, R2, 0x28, 2D, 0xf;
        )")) == Color{0, 0, 1, 0});
    }
    SECTION("Lod bias") {
        // Bias doesn't seem to be applied on compute
        REQUIRE(util.Run(MakeShader(R"(
            MOV32I R0, 0x3f000000;
            MOV32I R1, 0x3f000000;
            MOV32I R2, 0x40000000;
            TEX.LB PT, R0, R0, R2, 0x28, 2D, 0xf;
        )")) == Color{1, 0, 0, 0});
    }
    SECTION("Lod bias LBA") {
        // Bias doesn't seem to be applied on compute
        REQUIRE(util.Run(MakeShader(R"(
            MOV32I R0, 0x3f000000;
            MOV32I R1, 0x3f000000;
            MOV32I R2, 0x40000000;
            TEX.LBA PT, R0, R0, R2, 0x28, 2D, 0xf;
        )")) == Color{1, 0, 0, 0});
    }
    SECTION("Lod explicit LLA") {
        REQUIRE(util.Run(MakeShader(R"(
            MOV32I R0, 0x3f000000;
            MOV32I R1, 0x3f000000;
            MOV32I R2, 0x3f800000;
            TEX.LLA PT, R0, R0, R2, 0x28, 2D, 0xf;
        )")) == Color{0, 1, 0, 0});
    }
    SECTION("Lod explicit + AOFFI") {
        REQUIRE(util.Run(MakeShader(R"(
            MOV32I R0, 0x00000000;
            MOV32I R1, 0x00000000;
            MOV32I R2, 0x00000000;
            MOV32I R3, 0x00000001;
            TEX.AOFFI.LL PT, R0, R0, R2, 0x28, 2D, 0xf;
        )")) == Color{1, 0, 0, 0});
    }
    SECTION("Lod explicit + AOFFI + compatible type") {
        REQUIRE(util.Run(MakeShader(R"(
            MOV32I R0, 0x00000000;
            MOV32I R1, 0x00000000;
            MOV32I R2, 0x00000000;
            MOV32I R4, 0x00000000;
            MOV32I R5, 0x00000001;
            TEX.AOFFI.LL PT, R0, R0, R4, 0x28, ARRAY_2D, 0xf;
        )")) == Color{1, 0, 0, 0});
    }
    SECTION("Lod explicit + AOFFI + wrong type") {
        REQUIRE(util.Run(MakeShader(R"(
            MOV32I R0, 0x00000000;
            MOV32I R1, 0x00000000;
            MOV32I R2, 0x00000000;
            MOV32I R4, 0x00000000;
            MOV32I R5, 0x00000001;
            TEX.AOFFI.LL PT, R0, R0, R4, 0x28, 3D, 0xf;
            FADD.FTZ R3, R3, 1;
        )")) == Color{0, 0, 0, 1});
    }
    SECTION("Bindless") {
        REQUIRE(util.Run(MakeShader(R"(
            MOV32I R0, 0x3f000000;
            MOV32I R1, 0x3f000000;
            MOV R2, c[2][0];
            TEX.B.LZ PT, R0, R0, R2, 0x0, 2D, 0xf;
        )")) == Color{1, 0, 0, 0});
    }
    SECTION("Bindless + AOFFI") {
        REQUIRE(util.Run(MakeShader(R"(
            MOV32I R0, 0x00000000;
            MOV32I R1, 0x00000000;
            MOV R4, c[2][0];
            MOV32I R5, 0x00000000;
            MOV32I R6, 0x00000001;
            TEX.B.AOFFI.LL PT, R0, R0, R4, 0x0, 2D, 0xf;
        )")) == Color{1, 0, 0, 0});
    }
    SECTION("Bindless + mask") {
        REQUIRE(util.Run(MakeShader(R"(
            MOV32I R0, 0x00000000;
            MOV32I R1, 0x00000000;
            MOV R2, RZ;
            MOV R3, RZ;
            MOV R4, c[2][0];
            MOV32I R5, 0x40400000;
            TEX.B.LL PT, R0, R0, R4, 0x0, 2D, 0xf;
        )")) == Color{1, 1, 1, 1});

        REQUIRE(util.Run(MakeShader(R"(
            MOV32I R0, 0x00000000;
            MOV32I R1, 0x00000000;
            MOV R2, RZ;
            MOV R3, RZ;
            MOV R4, c[2][0];
            MOV32I R5, 0x40400000;
            TEX.B.LL PT, R0, R0, R4, 0x0, 2D, 0x7;
        )")) == Color{1, 1, 1, 0});

        REQUIRE(util.Run(MakeShader(R"(
            MOV32I R0, 0x00000000;
            MOV32I R1, 0x00000000;
            MOV R2, RZ;
            MOV R3, RZ;
            MOV R4, c[2][0];
            MOV32I R5, 0x40400000;
            TEX.B.LL PT, R0, R0, R4, 0x0, 2D, 0x1;
        )")) == Color{1, 0, 0, 0});

        REQUIRE(util.Run(MakeShader(R"(
            MOV32I R0, 0x00000000;
            MOV32I R1, 0x00000000;
            MOV32I R2, 0x40400000;
            MOV R3, RZ;
            MOV R4, c[2][0];
            MOV32I R5, 0x40400000;
            TEX.B.LL PT, R0, R0, R4, 0x0, 2D, 0x3;
        )")) == Color{1, 1, 3, 0});
    }
    SECTION("Bindless + sparse predicate") {
        REQUIRE(util.Run(MakeShader(R"(
            MOV32I R0, 0x00000000;
            MOV32I R1, 0x00000000;
            MOV R4, c[2][0];
            MOV32I R5, 0x00000000;
            MOV32I R6, 0x00000001;
            ISETP.T.AND P0, PT, RZ, RZ, PT;
            TEX.B.AOFFI.LL P0, R0, R0, R4, 0x0, 2D, 0xf;
            @P0 MOV32I R3, 0x40400000;
            @!P0 MOV32I R3, 0xc0400000;
        )")) == Color{1, 0, 0, -3});
    }
}

TEST_CASE("TEX 2D Array", "[shader]") {
    Texture texture(512, 512, 1, 1, DkImageType_2DArray);
    texture.FillColor(0, 255, 255, 0, 255);

    Runner util{texture};

    REQUIRE(util.Run(MakeShader(R"(
        MOV32I R0, 0x3f000000;
        MOV32I R1, 0x3f000000;
        TEX.LZ PT, R0, R0, RZ, 0x28, 2D, 0xf;
    )")) == Color{1, 1, 0, 1});

    REQUIRE(util.Run(MakeShader(R"(
        MOV32I R0, 0x00000000;
        MOV32I R1, 0x3f000000;
        MOV32I R2, 0x3f000000;
        TEX.LZ PT, R0, R0, RZ, 0x28, ARRAY_2D, 0xf;
    )")) == Color{1, 1, 0, 1});

    REQUIRE(util.Run(MakeShader(R"(
        MOV32I R0, 0x00000000;
        MOV32I R1, 0x3f000000;
        MOV32I R2, 0x3f000000;
        TEX.LZ PT, R0, R0, RZ, 0x28, CUBE, 0xf;
    )")) == Color{0, 0, 0, 0});
}

TEST_CASE("TEX 3D", "[shader]") {
    Texture texture(512, 512, 512, 1, DkImageType_3D);
    texture.FillColor(0, 255, 0, 255, 255);

    Runner util{texture};

    REQUIRE(util.Run(MakeShader(R"(
        MOV32I R0, 0x3f000000;
        MOV32I R1, 0x3f000000;
        MOV32I R2, 0x3f000000;
        TEX.LZ PT, R0, R0, RZ, 0x28, 3D, 0xf;
    )")) == Color{1, 0, 1, 1});

    REQUIRE(util.Run(MakeShader(R"(
        MOV32I R0, 0x3f000000;
        MOV32I R1, 0x3f000000;
        MOV32I R2, 0x3f000000;
        TEX.LZ PT, R0, R0, RZ, 0x28, 2D, 0xf;
    )")) == Color{0, 0, 0, 0});
}

TEST_CASE("TEX Cube", "[shader]") {
    Texture texture(512, 512, 6, 1, DkImageType_Cubemap);
    texture.FillColor(0, 255, 0, 255, 255);

    Runner util{texture};

    REQUIRE(util.Run(MakeShader(R"(
        MOV32I R0, 0x3f000000;
        MOV32I R1, 0x3f000000;
        MOV32I R2, 0x3f000000;
        TEX.LZ PT, R0, R0, RZ, 0x28, CUBE, 0xf;
    )")) == Color{1, 0, 1, 1});

    REQUIRE(util.Run(MakeShader(R"(
        MOV32I R0, 0x3f000000;
        MOV32I R1, 0x3f000000;
        MOV32I R2, 0x3f000000;
        MOV32I R3, 0;
        TEX.LZ PT, R0, R0, RZ, 0x28, ARRAY_CUBE, 0xf;
    )")) == Color{1, 0, 1, 1});
}

TEST_CASE("TEX 2D DC", "[shader]") {
    Texture texture(512, 512, 1, 2, DkImageType_2D, true);
    texture.FillColor(0, 0.7f);

    Runner util{texture};

    REQUIRE(util.Run(MakeShader(R"(
        MOV32I R0, 0x3f000000;
        MOV32I R1, 0;
        MOV32I R2, 0x3f800000;
        MOV32I R3, 0;
        MOV32I R4, 0x3f000000;
        MOV32I R5, 0x3f000000;
        TEX.LZ.DC PT, R0, R4, R0, 0x28, 2D, 0xf;
    )")) == Color{1, 1, 1, 1});

    REQUIRE(util.Run(MakeShader(R"(
        MOV32I R0, 0x3f800000;
        MOV32I R1, 0;
        MOV32I R2, 0x3f800000;
        MOV32I R3, 0;
        MOV32I R4, 0x3f000000;
        MOV32I R5, 0x3f000000;
        TEX.LZ.DC PT, R0, R4, R0, 0x28, 2D, 0xf;
    )")) == Color{0, 0, 0, 1});
}

TEST_CASE("TEX Cube Array DC", "[shader]") {
    Texture texture(512, 512, 6, 1, DkImageType_CubemapArray, true);
    texture.FillColor(0, 0.7f);

    Runner util{texture};

    REQUIRE(util.Run(MakeShader(R"(
        MOV32I R0, 0;
        MOV32I R1, 0x3f000000;
        MOV32I R2, 0x3f000000;
        MOV32I R3, 0x3f000000;
        MOV32I R4, 0x3f000000;
        TEX.LZ.DC PT, R0, R0, R4, 0x28, ARRAY_CUBE, 0xf;
    )")) == Color{1, 1, 1, 1});

    REQUIRE(util.Run(MakeShader(R"(
        MOV32I R0, 0;
        MOV32I R1, 0x3f000000;
        MOV32I R2, 0x3f000000;
        MOV32I R3, 0x3f000000;
        MOV32I R4, 0x3f800000;
        TEX.LZ.DC PT, R0, R0, R4, 0x28, ARRAY_CUBE, 0xf;
    )")) == Color{0, 0, 0, 1});
}
