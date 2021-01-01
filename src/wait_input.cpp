#include <switch.h>

#include "wait_input.h"

static PadState& Pad() {
    static PadState pad = [] {
        padConfigureInput(1, HidNpadStyleSet_NpadStandard);
        PadState result;
        padInitializeDefault(&result);
        return result;
    }();
    return pad;
}

void WaitForUserInput() {
    PadState& pad = Pad();
    while (appletMainLoop()) {
        consoleUpdate(nullptr);
        padUpdate(&pad);
        if ((padGetButtonsDown(&pad) & HidNpadButton_Plus) != 0) {
            break;
        }
    }
}

bool HasUserInput() {
    PadState& pad = Pad();
    padUpdate(&pad);
    return (padGetButtonsDown(&pad) & HidNpadButton_Plus) != 0;
}
