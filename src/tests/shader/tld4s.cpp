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
           "MOV32I R1, 0x3f800000;"
           "MOV32I R2, 0x3f400000;"
           "MOV32I R3, 0x3f000000;"
           "MOV32I R4, 0x3f400000;"
           "MOV32I R5, 0x3f000000;"
           "MOV32I R6, 0xbf000000;"
           "MOV32I R7, 0xbf000000;\n" +
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

[[maybe_unused]] void RandomizeShadow(Texture& texture, unsigned seed) {
    std::mt19937 rng{seed};
    const size_t size = texture.SizeBytes();
    auto data = static_cast<float*>(texture.CpuAddr());
    for (size_t index = 0; index < size / 4; ++index) {
        data[index] = (rng() % 4096) / 4096.0f - 2048.0f;
        ++index;
    }
}
} // Anonymous namespace

TEST_CASE("TLD4S", "[shader]") {
    Texture texture(512, 512, 1, 1, DkImageType_2D);
    Randomize(texture, 1532);

    Runner util{texture};
    auto X = [&](std::string code) { return util.Run(MakeShader(std::move(code))); };
    auto Y = [&](std::string code) {
        auto raw = BitCast<std::array<uint32_t, 4>>(X(std::move(code)));
        auto casted = BitCast<std::array<__fp16, 4>>(std::array{raw[0], raw[2]});
        return Color{casted[0], casted[1], casted[2], casted[3]};
    };

    SECTION("Basic") {
        REQUIRE(X("TLD4S.R R0, R2, R0, R2, 0x28;") == Color{0, 1, 0, 1});
        REQUIRE(X("TLD4S.R R0, R2, R2, R0, 0x28;") == Color{0, 0, 0, 1});

        REQUIRE(X("TLD4S.G R0, R2, R0, R2, 0x28;") == Color{0, 0, 0, 0});
        REQUIRE(X("TLD4S.G R0, R2, R2, R0, 0x28;") == Color{0, 0, 0, 0});

        REQUIRE(X("TLD4S.B R0, R2, R0, R2, 0x28;") == Color{0, 0, 1, 1});
        REQUIRE(X("TLD4S.B R0, R2, R2, R0, 0x28;") == Color{0, 0, 1, 1});

        REQUIRE(X("TLD4S.A R0, R2, R0, R2, 0x28;") == Color{0, 0, 0, 0});
        REQUIRE(X("TLD4S.A R0, R2, R2, R0, 0x28;") == Color{0, 0, 0, 0});

        REQUIRE(X("TLD4S.R R2, R0, R2, R0, 0x28;") == Color{0, 1, 0, 0});
    }
    SECTION("AOFFI") {
        REQUIRE(X("MOV32I R2, 0xffffffff;"
                  "TLD4S.R.AOFFI R0, R2, R0, R2, 0x28;") == Color{0, 0, 1, 0});
        REQUIRE(X("MOV32I R2, 0x00000000;"
                  "TLD4S.R.AOFFI R0, R2, R0, R2, 0x28;") == Color{0, 0, 0, 1});
        REQUIRE(X("MOV32I R2, 0xcccccccc;"
                  "TLD4S.R.AOFFI R0, R2, R0, R2, 0x28;") == Color{0, 1, 0, 1});
        REQUIRE(X("MOV32I R2, 0xcccccccc;"
                  "TLD4S.B.AOFFI R0, R2, R0, R2, 0x28;") == Color{0, 1, 0, 0});
    }
    SECTION("F16") {
        REQUIRE(Y("TLD4S.F16.R R0, R2, R0, R2, 0x28;") == Color{0, 1, 0, 1});
        REQUIRE(Y("TLD4S.F16.R R0, R2, R2, R0, 0x28;") == Color{0, 0, 0, 1});

        REQUIRE(Y("TLD4S.F16.G R0, R2, R0, R2, 0x28;") == Color{0, 0, 0, 0});
        REQUIRE(Y("TLD4S.F16.G R0, R2, R2, R0, 0x28;") == Color{0, 0, 0, 0});

        REQUIRE(Y("TLD4S.F16.B R0, R2, R0, R2, 0x28;") == Color{0, 0, 1, 1});
        REQUIRE(Y("TLD4S.F16.B R0, R2, R2, R0, 0x28;") == Color{0, 0, 1, 1});

        REQUIRE(Y("TLD4S.F16.A R0, R2, R0, R2, 0x28;") == Color{0, 0, 0, 0});
        REQUIRE(Y("TLD4S.F16.A R0, R2, R2, R0, 0x28;") == Color{0, 0, 0, 0});

        REQUIRE(Y("TLD4S.F16.R R2, R0, R2, R0, 0x28;") == Color{0, 1, 0, 0});
    }
}

TEST_CASE("TLD4S DC", "[shader]") {
    Texture texture(512, 512, 1, 1, DkImageType_2D, true);
    RandomizeShadow(texture, 1345);

    Runner util{texture};
    auto X = [&](std::string code) { return util.Run(MakeShader(std::move(code))); };
    auto Y = [&](std::string code) {
        auto raw = BitCast<std::array<uint32_t, 4>>(X(std::move(code)));
        auto casted = BitCast<std::array<__fp16, 4>>(std::array{raw[0], raw[2]});
        return Color{casted[0], casted[1], casted[2], casted[3]};
    };

    SECTION("Basic") {
        REQUIRE(X("TLD4S.R.DC R0, R2, R0, R7, 0x28;") == Color{0, 1, 1, 0});
        REQUIRE(X("TLD4S.R.DC R0, R2, R2, R7, 0x28;") == Color{0, 1, 1, 0});
        REQUIRE(X("TLD4S.R.DC R0, R2, R2, R2, 0x28;") == Color{0, 0, 0, 0});

        REQUIRE(X("TLD4S.G.DC R0, R2, R0, R7, 0x28;") == Color{0, 1, 1, 0});
        REQUIRE(X("TLD4S.B.DC R0, R2, R0, R7, 0x28;") == Color{0, 1, 1, 0});
        REQUIRE(X("TLD4S.A.DC R0, R2, R0, R7, 0x28;") == Color{0, 1, 1, 0});
    }
    SECTION("AOFFI") {
        REQUIRE(X("MOV32I R6, 0xffffffff;"
                  "TLD4S.R.AOFFI.DC R0, R2, R0, R6, 0x28;") == Color{1, 0, 0, 1});
        REQUIRE(X("MOV32I R6, 0x00000000;"
                  "TLD4S.R.AOFFI.DC R0, R2, R0, R6, 0x28;") == Color{0, 1, 1, 0});
        REQUIRE(X("MOV32I R6, 0xcccccccc;"
                  "TLD4S.R.AOFFI.DC R0, R2, R0, R6, 0x28;") == Color{0, 1, 1, 0});
        REQUIRE(X("MOV32I R6, 0x33333333;"
                  "TLD4S.R.AOFFI.DC R0, R2, R0, R6, 0x28;") == Color{1, 0, 0, 1});
    }
    SECTION("F16") {
        REQUIRE(Y("TLD4S.F16.R.DC R0, R2, R0, R7, 0x28;") == Color{0, 1, 1, 0});
        REQUIRE(Y("TLD4S.F16.R.DC R0, R2, R2, R7, 0x28;") == Color{0, 1, 1, 0});
        REQUIRE(Y("TLD4S.F16.R.DC R0, R2, R2, R2, 0x28;") == Color{0, 0, 0, 0});

        REQUIRE(Y("TLD4S.F16.G.DC R0, R2, R0, R7, 0x28;") == Color{0, 1, 1, 0});
        REQUIRE(Y("TLD4S.F16.B.DC R0, R2, R0, R7, 0x28;") == Color{0, 1, 1, 0});
        REQUIRE(Y("TLD4S.F16.A.DC R0, R2, R0, R7, 0x28;") == Color{0, 1, 1, 0});
    }
}
