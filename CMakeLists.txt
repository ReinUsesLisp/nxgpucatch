cmake_minimum_required(VERSION 3.16)

project(nxgpucatch)
set(CMAKE_C_STANDARD 20)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -frtti -fexceptions -DSA_ONSTACK=0")

add_subdirectory(externals)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake)
find_package(deko3d REQUIRED)

add_executable(nxgpucatch
    src/tests/clear.cpp
    src/tests/render_formats.cpp
    src/bit_cast.h
    src/cmd_util.h
    src/compare.h
    src/device.cpp
    src/device.h
    src/formats.h
    src/main.cpp
    src/resource.cpp
    src/resource.h
    src/wait_input.cpp
    src/wait_input.h
)

target_link_libraries(nxgpucatch switch::deko3d switch::libnx Catch2::Catch2)
target_compile_options(nxgpucatch PRIVATE
    -std=c++20
    -Wall
    -Wextra
    -Werror
    -Werror=shadow
    -pedantic
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
