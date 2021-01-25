# Libnx
list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/externals/switch-cmake/cmake)
include(SwitchTools)
find_package(Libnx REQUIRED)

# Catch2
set(CATCH_BUILD_TESTING OFF)
set(CATCH_BUILD_EXAMPLES OFF)
set(CATCH_INSTALL_DOCS OFF)
set(CATCH_INSTALL_HELPERS OFF)
add_subdirectory(Catch2)

# nxas
add_subdirectory(nxas)
