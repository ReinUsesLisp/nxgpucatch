#pragma once

#include <cstdint>

// xorshift64star, analysis is done in http://vigna.di.unimi.it/ftp/papers/xorshift.pdf
class XorRandom {
    uint64_t state;

public:
    XorRandom(uint64_t state_ = 1) : state{state_ == 0 ? 1 : state_} {}

    uint32_t get() {
        uint64_t x = state; /* The state must be seeded with a nonzero value. */
        x ^= x >> 12;
        x ^= x << 25;
        x ^= x >> 27;
        state = x;
        return (x * 0x2545F4914F6CDD1D) >> 32;
    }
};
