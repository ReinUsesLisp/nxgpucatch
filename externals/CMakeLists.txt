# Catch2
set(CATCH_BUILD_TESTING OFF)
set(CATCH_BUILD_EXAMPLES OFF)
set(CATCH_INSTALL_HELPERS OFF)
add_subdirectory(Catch2)
target_compile_options(Catch2 PRIVATE -DSA_ONSTACK=0)

# nxas
add_subdirectory(nxas)
