cmake_minimum_required(VERSION 3.16)

set(CMAKE_POLICY_DEFAULT_CMP0077 NEW)

project(nxgpucatch C CXX ASM)
set(CMAKE_C_STANDARD 20)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -frtti -fexceptions -DSA_ONSTACK=0")

add_subdirectory(externals)

add_executable(nxgpucatch
    src/alignment.h
    src/bit_cast.h
    src/cmd_util.h
    src/compare.h
    src/descriptor_set.h
    src/device.cpp
    src/device.h
    src/eval_util.h
    src/formats.h
    src/fp16_run.h
    src/heap.h
    src/main.cpp
    src/resource.cpp
    src/resource.h
    src/shader.cpp
    src/shader.h
    src/tests/clear.cpp
    src/tests/draw/simple_draw.cpp
    src/tests/draw/varying_undefined.cpp
    src/tests/indirect_compute.cpp
    src/tests/render_formats.cpp
    src/tests/render_linear_aliased.cpp
    src/tests/shader/atom.cpp
    src/tests/shader/atoms.cpp
    src/tests/shader/bfe.cpp
    src/tests/shader/bfi.cpp
    src/tests/shader/cal.cpp
    src/tests/shader/cset.cpp
    src/tests/shader/csetp.cpp
    src/tests/shader/dadd.cpp
    src/tests/shader/dfma.cpp
    src/tests/shader/dmnmx.cpp
    src/tests/shader/dmul.cpp
    src/tests/shader/dset.cpp
    src/tests/shader/dsetp.cpp
    src/tests/shader/f2f.cpp
    src/tests/shader/f2i.cpp
    src/tests/shader/fadd.cpp
    src/tests/shader/fcmp.cpp
    src/tests/shader/ffma.cpp
    src/tests/shader/flo.cpp
    src/tests/shader/flow.cpp
    src/tests/shader/fmnmx.cpp
    src/tests/shader/fmul.cpp
    src/tests/shader/fset.cpp
    src/tests/shader/fsetp.cpp
    src/tests/shader/hadd2.cpp
    src/tests/shader/hfma2.cpp
    src/tests/shader/hmul2.cpp
    src/tests/shader/hset2.cpp
    src/tests/shader/hsetp2.cpp
    src/tests/shader/i2f.cpp
    src/tests/shader/i2i.cpp
    src/tests/shader/iadd.cpp
    src/tests/shader/iadd3.cpp
    src/tests/shader/icmp.cpp
    src/tests/shader/imnmx.cpp
    src/tests/shader/iscadd.cpp
    src/tests/shader/iset.cpp
    src/tests/shader/isetp.cpp
    src/tests/shader/ldc.cpp
    src/tests/shader/ldg.cpp
    src/tests/shader/lea.cpp
    src/tests/shader/lop.cpp
    src/tests/shader/lop3.cpp
    src/tests/shader/mov.cpp
    src/tests/shader/mufu.cpp
    src/tests/shader/p2r.cpp
    src/tests/shader/popc.cpp
    src/tests/shader/pset.cpp
    src/tests/shader/psetp.cpp
    src/tests/shader/r2p.cpp
    src/tests/shader/red.cpp
    src/tests/shader/sel.cpp
    src/tests/shader/shf.cpp
    src/tests/shader/shfl.cpp
    src/tests/shader/shl.cpp
    src/tests/shader/shr.cpp
    src/tests/shader/stg.cpp
    src/tests/shader/stls.cpp
    src/tests/shader/suldst.cpp
    src/tests/shader/tex.cpp
    src/tests/shader/texs.cpp
    src/tests/shader/tld.cpp
    src/tests/shader/tld4.cpp
    src/tests/shader/tld4s.cpp
    src/tests/shader/tlds.cpp
    src/tests/shader/txq.cpp
    src/tests/shader/vmad.cpp
    src/tests/shader/vmnmx.cpp
    src/tests/shader/vote.cpp
    src/tests/shader/vsetp.cpp
    src/tests/shader/xmad.cpp
    src/tests/texture_buffer.cpp
    src/wait_input.cpp
    src/wait_input.h
)

nx_add_shader_program(full_screen_triangle_vanilla_vert src/tests/draw/full_screen_triangle_vanilla.vert vert)
nx_add_shader_program(full_screen_triangle_vert src/tests/draw/full_screen_triangle.vert vert)
nx_add_shader_program(varying_color_frag src/tests/draw/varying_color.frag frag)
nx_add_shader_program(white_frag src/tests/draw/white.frag frag)

dkp_add_asset_target(nxgpucatch_romfs romfs)

dkp_install_assets(nxgpucatch_romfs
    DESTINATION shaders
    TARGETS
        full_screen_triangle_vanilla_vert
        full_screen_triangle_vert
        varying_color_frag
        white_frag
)

target_link_libraries(nxgpucatch Catch2::Catch2 nxas_lib deko3d)
target_include_directories(nxgpucatch PRIVATE src externals/incbin)

target_compile_options(nxgpucatch PRIVATE
    -std=c++20
    -Wall
    -Wextra
    -Werror
    -Werror=shadow
    -pedantic
)
target_precompile_headers(nxgpucatch PRIVATE
    <catch2/catch_test_macros.hpp>
    <deko3d.hpp>
    <array>
    <algorithm>
    <type_traits>
    <cstdint>
    <cstddef>
    <cstring>
    <optional>
    <vector>
    <stdexcept>
)

nx_generate_nacp(nxgpucatch.nacp
    NAME "nxgpucatch"
    AUTHOR "yuzu Team"
)

nx_create_nro(nxgpucatch
    NACP nxgpucatch.nacp
    ROMFS nxgpucatch_romfs
)

find_program(nxlink_exe nxlink PATHS ${DEVKITPRO}/tools/bin REQUIRED)
add_custom_target(nxlink
    COMMAND ${nxlink_exe} $<TARGET_FILE:nxgpucatch>.nro
    DEPENDS nxgpucatch
)
