#pragma once

#include "types.h"

static void brad_str_marker(
    brad_ptr str
) {
    (void)str;
}

brad_str brad_str_concat(brad_str a, brad_str b);
brad_str brad_int_to_str(brad_int i);
brad_str brad_float_to_str(brad_float f);
brad_str brad_str_from_c_str(const char* c_str);