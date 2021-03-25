#include <atomic>

#include <catch2/catch_test_macros.hpp>

#include "cmd_util.h"
#include "descriptor_set.h"
#include "heap.h"
#include "resource.h"
#include "shader.h"

namespace {
using Vector = std::array<uint32_t, 4>;

std::atomic_uint id;

Shader MakeShader(std::string code) {
    code = ".dksh compute\n"
           "main:\n"
           "MOV32I R0, 0;"
           "MOV32I R1, 1;"
           "MOV32I R2, 2;"
           "MOV32I R3, 3;"
           "MOV32I R4, 4;"
           "MOV32I R5, 5;"
           "MOV32I R6, 6;"
           "MOV32I R7, 7;" +
           code + "MOV32I RZ, " + std::to_string(++id) + R"(;
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

    Vector Run(const Shader& shader) {
        RecordRunWait([&](dk::CmdBuf cmdbuf) {
            shader.Bind(cmdbuf, DkStageFlag_Compute);
            cmdbuf.dispatchCompute(1, 1, 1);
        });
        return *result_heap;
    };

private:
    DescriptorSet descriptor_set;
    TypedHeap<Vector> result_heap;
    TypedHeap<uint32_t> bindless_handle;
};
} // Anonymous namespace

#define X(code) util.Run(MakeShader(code))

TEST_CASE("TXQ 1D", "[shader]") {
    Texture texture(512, 1, 1, 6, DkImageType_1D);
    Runner util{texture};
    REQUIRE(X("TXQ R0, R0, TEX_HEADER_DIMENSION, 0x28, 0xf;") == Vector{512, 0, 0, 6});
    REQUIRE(X("TXQ R0, R1, TEX_HEADER_DIMENSION, 0x28, 0xf;") == Vector{256, 0, 0, 6});
    REQUIRE(X("TXQ R0, R2, TEX_HEADER_DIMENSION, 0x28, 0xf;") == Vector{128, 0, 0, 6});
    REQUIRE(X("TXQ R0, R3, TEX_HEADER_DIMENSION, 0x28, 0xf;") == Vector{64, 0, 0, 6});
    REQUIRE(X("TXQ R0, R4, TEX_HEADER_DIMENSION, 0x28, 0xf;") == Vector{32, 0, 0, 6});
    REQUIRE(X("TXQ R0, R5, TEX_HEADER_DIMENSION, 0x28, 0xf;") == Vector{16, 0, 0, 6});
    REQUIRE(X("TXQ R0, R6, TEX_HEADER_DIMENSION, 0x28, 0xf;") == Vector{0, 0, 0, 6});
}

TEST_CASE("TXQ ARRAY_1D", "[shader]") {
    Texture texture(512, 4, 1, 6, DkImageType_1DArray);
    Runner util{texture};
    REQUIRE(X("TXQ R0, R0, TEX_HEADER_DIMENSION, 0x28, 0xf;") == Vector{512, 4, 0, 6});
    REQUIRE(X("TXQ R0, R1, TEX_HEADER_DIMENSION, 0x28, 0xf;") == Vector{256, 4, 0, 6});
    REQUIRE(X("TXQ R0, R2, TEX_HEADER_DIMENSION, 0x28, 0xf;") == Vector{128, 4, 0, 6});
    REQUIRE(X("TXQ R0, R3, TEX_HEADER_DIMENSION, 0x28, 0xf;") == Vector{64, 4, 0, 6});
    REQUIRE(X("TXQ R0, R4, TEX_HEADER_DIMENSION, 0x28, 0xf;") == Vector{32, 4, 0, 6});
    REQUIRE(X("TXQ R0, R5, TEX_HEADER_DIMENSION, 0x28, 0xf;") == Vector{16, 4, 0, 6});
    REQUIRE(X("TXQ R0, R6, TEX_HEADER_DIMENSION, 0x28, 0xf;") == Vector{0, 0, 0, 6});
}

TEST_CASE("TXQ 2D", "[shader]") {
    Texture texture(512, 256, 1, 4, DkImageType_2D);
    Runner util{texture};
    REQUIRE(X("TXQ R0, R0, TEX_HEADER_DIMENSION, 0x28, 0xf;") == Vector{512, 256, 0, 4});
    REQUIRE(X("TXQ R0, R1, TEX_HEADER_DIMENSION, 0x28, 0xf;") == Vector{256, 128, 0, 4});
    REQUIRE(X("TXQ R0, R2, TEX_HEADER_DIMENSION, 0x28, 0xf;") == Vector{128, 64, 0, 4});
    REQUIRE(X("TXQ R0, R3, TEX_HEADER_DIMENSION, 0x28, 0xf;") == Vector{64, 32, 0, 4});
    REQUIRE(X("TXQ R0, R4, TEX_HEADER_DIMENSION, 0x28, 0xf;") == Vector{0, 0, 0, 4});
}

TEST_CASE("TXQ ARRAY_2D", "[shader]") {
    Texture texture(1024, 512, 8, 3, DkImageType_2DArray);
    Runner util{texture};
    REQUIRE(X("TXQ R0, R0, TEX_HEADER_DIMENSION, 0x28, 0xf;") == Vector{1024, 512, 8, 3});
    REQUIRE(X("TXQ R0, R1, TEX_HEADER_DIMENSION, 0x28, 0xf;") == Vector{512, 256, 8, 3});
    REQUIRE(X("TXQ R0, R2, TEX_HEADER_DIMENSION, 0x28, 0xf;") == Vector{256, 128, 8, 3});
    REQUIRE(X("TXQ R0, R3, TEX_HEADER_DIMENSION, 0x28, 0xf;") == Vector{0, 0, 0, 3});
}

TEST_CASE("TXQ 3D", "[shader]") {
    Texture texture(32, 16, 16, 3, DkImageType_3D);
    Runner util{texture};
    REQUIRE(X("TXQ R0, R0, TEX_HEADER_DIMENSION, 0x28, 0xf;") == Vector{32, 16, 16, 3});
    REQUIRE(X("TXQ R0, R1, TEX_HEADER_DIMENSION, 0x28, 0xf;") == Vector{16, 8, 8, 3});
    REQUIRE(X("TXQ R0, R2, TEX_HEADER_DIMENSION, 0x28, 0xf;") == Vector{8, 4, 4, 3});
    REQUIRE(X("TXQ R0, R3, TEX_HEADER_DIMENSION, 0x28, 0xf;") == Vector{0, 0, 0, 3});

    REQUIRE(X("MOV R0, c[2][0];"
              "TXQ.B R0, R0, TEX_HEADER_DIMENSION, 0, 0xf;") == Vector{16, 8, 8, 3});

    REQUIRE(X("TXQ R0, R1, TEX_HEADER_DIMENSION, 0x28, 0x1;") == Vector{16, 1, 2, 3});
    REQUIRE(X("TXQ R0, R1, TEX_HEADER_DIMENSION, 0x28, 0x2;") == Vector{8, 1, 2, 3});
    REQUIRE(X("TXQ R0, R1, TEX_HEADER_DIMENSION, 0x28, 0x4;") == Vector{8, 1, 2, 3});
    REQUIRE(X("TXQ R0, R1, TEX_HEADER_DIMENSION, 0x28, 0x8;") == Vector{3, 1, 2, 3});

    REQUIRE(X("MOV R0, c[2][0];"
              "TXQ.B R0, R0, TEX_HEADER_DIMENSION, 0, 0x3;") == Vector{16, 8, 2, 3});
    REQUIRE(X("MOV R0, c[2][0];"
              "TXQ.B R0, R0, TEX_HEADER_DIMENSION, 0, 0xc;") == Vector{8, 3, 2, 3});
}

TEST_CASE("TXQ CUBE", "[shader]") {
    Texture texture(32, 32, 1, 3, DkImageType_Cubemap);
    Runner util{texture};
    REQUIRE(X("TXQ R0, R0, TEX_HEADER_DIMENSION, 0x28, 0xf;") == Vector{32, 32, 0, 3});
    REQUIRE(X("TXQ R0, R1, TEX_HEADER_DIMENSION, 0x28, 0xf;") == Vector{16, 16, 0, 3});
    REQUIRE(X("TXQ R0, R2, TEX_HEADER_DIMENSION, 0x28, 0xf;") == Vector{8, 8, 0, 3});
    REQUIRE(X("TXQ R0, R3, TEX_HEADER_DIMENSION, 0x28, 0xf;") == Vector{0, 0, 0, 3});
}

TEST_CASE("TXQ ARRAY_CUBE", "[shader]") {
    Texture texture(32, 32, 6, 3, DkImageType_CubemapArray);
    Runner util{texture};
    REQUIRE(X("TXQ R0, R0, TEX_HEADER_DIMENSION, 0x28, 0xf;") == Vector{32, 32, 6, 3});
    REQUIRE(X("TXQ R0, R1, TEX_HEADER_DIMENSION, 0x28, 0xf;") == Vector{16, 16, 6, 3});
    REQUIRE(X("TXQ R0, R2, TEX_HEADER_DIMENSION, 0x28, 0xf;") == Vector{8, 8, 6, 3});
    REQUIRE(X("TXQ R0, R3, TEX_HEADER_DIMENSION, 0x28, 0xf;") == Vector{0, 0, 0, 3});
}
