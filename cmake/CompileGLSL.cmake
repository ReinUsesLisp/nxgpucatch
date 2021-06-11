function(add_shader_target target)
    set(shaders ${ARGV})
    list(REMOVE_AT shaders 0)

    foreach(source IN ITEMS ${shaders})
        if (NOT EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/${source}")
            message(FATAL_ERROR "Shader does not exist: \"${source}\"")
        endif()
        get_filename_component(extension "${source}" LAST_EXT)
        set(stage "")
        if ("${extension}" STREQUAL .vert)
            set(stage "vert")
        elseif ("${extension}" STREQUAL .frag)
            set(stage "frag")
        else()
            message(FATAL_ERROR "Unknown shader extension name ${extension}")
        endif()
        add_custom_command(
            OUTPUT
                "${source}.dksh"
            COMMAND
                uam -o "${CMAKE_CURRENT_SOURCE_DIR}/${source}.dksh" "--stage=${stage}" "${CMAKE_CURRENT_SOURCE_DIR}/${source}"
            MAIN_DEPENDENCY
                "${CMAKE_CURRENT_SOURCE_DIR}/${source}"
        )
    endforeach()

    add_custom_target("${target}" SOURCES "${shaders}")
endfunction()
