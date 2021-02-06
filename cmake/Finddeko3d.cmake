include(FindPackageHandleStandardArgs)

find_path(DEKO3D_INCLUDE_DIR deko3d.h PATHS ${NX_ROOT} PATH_SUFFIXES include)
find_library(DEKO3D_LIBRARY NAMES libdeko3dd.a PATHS ${NX_ROOT} PATH_SUFFIXES lib)

set(DEKO3D_INCLUDE_DIRS ${DEKO3D_INCLUDE_DIR})
set(DEKO3D_LIBRARIES ${DEKO3D_LIBRARY})

find_package_handle_standard_args(deko3d DEFAULT_MSG DEKO3D_INCLUDE_DIR DEKO3D_LIBRARY)

mark_as_advanced(DEKO3D_INCLUDE_DIR DEKO3D_LIBRARY)

if(DEKO3D_FOUND)
    set(DEKO3D ${DEKO3D_INCLUDE_DIR}/..)

    add_library(switch::deko3d STATIC IMPORTED GLOBAL)
    set_target_properties(switch::deko3d PROPERTIES
            IMPORTED_LOCATION "${DEKO3D_LIBRARY}"
            INTERFACE_INCLUDE_DIRECTORIES "${DEKO3D_INCLUDE_DIR}")
endif()
