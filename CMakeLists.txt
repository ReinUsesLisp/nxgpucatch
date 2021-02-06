cmake_minimum_required(VERSION 3.16)

project(nxgpucatch C CXX)
set(CMAKE_CXX_STANDARD 20)

if (NOT NSWITCH)
    message(FATAL_ERROR
        "Not compiling for Switch\n"
        "Install devkitpro-pkgbuild-helpers delete your current build directory, and set the cmake toolchain\n"
        "For example \"cmake .. -DCMAKE_TOOLCHAIN_FILE=/opt/devkitpro/switch.cmake\"")
endif()

add_subdirectory(externals)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake)
find_package(deko3d REQUIRED)

add_executable(nxgpucatch
    src/tests/clear.cpp
    src/tests/float_arithmetic.cpp
    src/tests/float_conversion.cpp
    src/tests/render_formats.cpp
    src/alignment.h
    src/bit_cast.h
    src/cmd_util.h
    src/compare.h
    src/device.cpp
    src/device.h
    src/formats.h
    src/heap.h
    src/main.cpp
    src/resource.cpp
    src/resource.h
    src/shader.cpp
    src/shader.h
    src/wait_input.cpp
    src/wait_input.h
)

target_link_libraries(nxgpucatch switch::deko3d Catch2::Catch2 nxas_lib)
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

find_program(nxlink_exe nxlink PATHS ${DEVKITPRO}/tools/bin REQUIRED)
add_custom_target(nxlink
    COMMAND ${nxlink_exe} $<TARGET_FILE:nxgpucatch>.nro
    DEPENDS nxgpucatch
)
