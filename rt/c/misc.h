#pragma once

#include <limits.h>

#define __minmax_impl(op, a, b, c)                                                                 \
    ({                                                                                             \
        __auto_type __CONCAT(_a, c) = (a);                                                         \
        __auto_type __CONCAT(_b, c) = (b);                                                         \
        __CONCAT(_a, c)                                                                            \
        op __CONCAT(_b, c) ? __CONCAT(_a, c) : __CONCAT(_b, c);                                    \
    })

#define min(a, b) __minmax_impl(<, a, b, __COUNTER__)
#define max(a, b) __minmax_impl(>, a, b, __COUNTER__)

#define log2(n) (((sizeof(unsigned int) * CHAR_BIT) - 1) - (__builtin_clz((n))))
#define pow2(n) (1 << (log2(n)))

#define likely(x)   __builtin_expect(!!(x), 1)
#define unlikely(x) __builtin_expect(!!(x), 0)