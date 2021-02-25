cmake_minimum_required(VERSION 3.16)

project(nxgpucatch)
set(CMAKE_C_STANDARD 20)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -frtti -fexceptions -DSA_ONSTACK=0")

add_subdirectory(externals)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake)
find_package(deko3d REQUIRED)

add_executable(nxgpucatch
    src/alignment.h
    src/bit_cast.h
    src/cmd_util.h
    src/compare.h
    src/device.cpp
    src/device.h
    src/eval_util.h
    src/formats.h
    src/heap.h
    src/main.cpp
    src/resource.cpp
    src/resource.h
    src/shader.cpp
    src/shader.h
    src/tests/clear.cpp
    src/tests/render_formats.cpp
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
    src/tests/shader/iadd.cpp
    src/tests/shader/imnmx.cpp
    src/tests/shader/iset.cpp
    src/tests/shader/isetp.cpp
    src/tests/shader/lop.cpp
    src/tests/shader/lop3.cpp
    src/tests/shader/mufu.cpp
    src/tests/shader/mov.cpp
    src/tests/shader/p2r.cpp
    src/tests/shader/popc.cpp
    src/tests/shader/sel.cpp
    src/tests/shader/shl.cpp
    src/tests/shader/shr.cpp
    src/tests/shader/xmad.cpp
    src/wait_input.cpp
    src/wait_input.h
)

target_link_libraries(nxgpucatch switch::deko3d switch::libnx Catch2::Catch2 nxas_lib)
target_include_directories(nxgpucatch PRIVATE src)
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

set_target_properties(nxgpucatch PROPERTIES
    APP_TITLE "nxgpucatch"
    APP_AUTHOR "yuzu Team"
    APP_VERSION "0.1.0"
)

# Do not generate a .nacp file
set(NO_ICON TRUE)
set(NO_NACP TRUE)
add_nro_target(nxgpucatch)

find_program(nxlink_exe nxlink PATHS ${DEVKITPRO}/tools/bin REQUIRED)
add_custom_target(nxlink
    COMMAND ${nxlink_exe} $<TARGET_FILE:nxgpucatch>.nro
    DEPENDS nxgpucatch
)
