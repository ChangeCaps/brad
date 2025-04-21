#pragma once

#include <stdio.h>

#ifdef DEBUG
#define DEBUG_MESSAGE_SINGLE(fmt) fprintf(stderr, fmt)
#define DEBUG_MESSAGE(fmt, ...) fprintf(stderr, fmt, __VA_ARGS__)
#else
#define DEBUG_MESSAGE_SINGLE(fmt)
#define DEBUG_MESSAGE(fmt, ...)
#endif
