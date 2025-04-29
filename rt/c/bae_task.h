#pragma once

#include "bae_future.h"

struct bae_task {
    struct bae_future future;
    struct bae_task* next;
};