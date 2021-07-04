#define CATCH_CONFIG_RUNNER

#include <catch2/catch_all.hpp>

#include <iostream>
#include <switch.h>

#include "device.h"
#include "wait_input.h"

int main(int argc, char** argv) {
    std::string dummy_argv0{"nxgpucatch.nro"};
    char* dummy_argv0_ptr{dummy_argv0.data()};
    if (argc == 0) {
        argc = 1;
        argv = &dummy_argv0_ptr;
    }

    const Result rc = romfsInit();
    if (R_FAILED(rc)) {
        printf("romfsInit: %08X\n", rc);
        return EXIT_FAILURE;
    }

    InitializeDevice();

    std::streambuf* old_count_stream = std::cout.rdbuf();
    std::ostringstream str_cout;
    std::cout.rdbuf(str_cout.rdbuf());

    const int result{Catch::Session().run(argc, argv)};

    std::string output = str_cout.str();
    fsOutputAccessLogToSdCard(output.c_str(), output.size());

    std::cout.rdbuf(old_count_stream);
    std::cout << output;

    romfsExit();

    WaitForUserInput();
    return result;
}

extern "C" void userAppInit() {
    consoleInit(nullptr);
}

extern "C" void userAppExit() {
    consoleExit(nullptr);
}

// Stub these functions
#pragma GCC diagnostic ignored "-Wshadow"
extern "C" int sigaltstack(const stack_t*, stack_t*) {
    return 0;
}
extern "C" int sigaction(int, const struct sigaction*, struct sigaction*) {
    return 0;
}
