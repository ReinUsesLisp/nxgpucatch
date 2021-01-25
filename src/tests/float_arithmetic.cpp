#include <catch2/catch_test_macros.hpp>

#include "../cmd_util.h"
#include "../compare.h"
#include "../device.h"
#include "../heap.h"
#include "../resource.h"
#include "../shader.h"

TEST_CASE("FADD Simple", "[float_arithmetic]") {
    Shader shader(".dksh compute\n"
                  "main:\n"
                  "MOV R0, c[0x0][0x140];\n"
                  "MOV R1, c[0x0][0x144];\n"
                  "MOV32I R3, 0x40a00000;\n"
                  "MOV32I R4, 0x40400000;\n"
                  "FADD.FTZ R2, R3, R4;\n"
                  "STG.E [R0], R2;\n"
                  "EXIT;\n");
    TypedHeap<float> heap;
    RecordRunWait([&](dk::CmdBuf cmdbuf) {
        shader.Bind(cmdbuf, DkStageFlag_Compute);
        heap.BindStorage(cmdbuf, DkStage_Compute);
        cmdbuf.dispatchCompute(1, 1, 1);
    });
    REQUIRE(*heap == 8.0f);
}
