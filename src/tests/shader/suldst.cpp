#include <random>
#include <catch2/catch_test_macros.hpp>

#include "bit_cast.h"
#include "cmd_util.h"
#include "descriptor_set.h"
#include "heap.h"
#include "resource.h"
#include "shader.h"

namespace {
using Color = std::array<uint32_t, 4>;

Shader MakeShader(std::string code) {
    code = ".dksh compute\n"
           "main:\n"
           "MOV32I R0, 0x00000000;"
           "MOV32I R1, 0x00000000;"
           "MOV32I R2, 0x00000000;"
           "MOV32I R3, 0x00000000;"
           "MOV32I R4, 0x00000001;"
           "MOV32I R5, 0x00000003;"
           "MOV32I R6, 0x00000000;"
           "MOV32I R7, 0x00000000;\n" +
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
                cmdbuf.bindImages(DkStage_Compute, index, {dkMakeImageHandle(index)});
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

TEST_CASE("SULD", "[shader]") {
    Texture texture(128, 128, 1, 1, DkImageType_2D);
    Randomize(texture, 73);

    Runner util{texture};

    SECTION("P") {
        REQUIRE(X("SULD.P.2D.R.IGN R0, [R4], 0x48;") == Color{0x3f800000, 0, 0, 0});
        REQUIRE(X("SULD.P.2D.RG.IGN R0, [R4], 0x48;") == Color{0x3f800000, 0, 0, 0});
        REQUIRE(X("SULD.P.2D.RGBA.IGN R0, [R4], 0x48;") == Color{0x3f800000, 0, 0, 0});
        REQUIRE(X("MOV R0, 3; MOV R1, 8; SULD.P.2D.RGBA.IGN R0, [R4], 0x48;") ==
                Color{0x3f800000, 0, 0, 0});
        REQUIRE(X("MOV R6, c[2][0]; SULD.P.2D.R.IGN R0, [R4], R6;") == Color{0x3f800000, 0, 0, 0});
    }
    SECTION("D") {
        REQUIRE(X("SULD.D.2D.32.IGN R0, [R4], 0x48;") == Color{0xff, 0, 0, 0});
        REQUIRE(X("MOV R6, c[2][0]; SULD.D.2D.32.IGN R0, [R4], R6;") == Color{0xff, 0, 0, 0});
    }
}
