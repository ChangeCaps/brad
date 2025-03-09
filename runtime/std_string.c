#include <string.h>

#include "gc.h"
#include "std_string.h"

static void brad_str_marker(
    brad_ptr str
) {}

brad_str brad_str_concat(
    brad_str a,
    brad_str b
) {
    brad_size new_size = sizeof(brad_str) + a->length + b->length;
    brad_str result = (brad_str)brad_alloc(new_size, brad_str_marker);

    result->length = a->length + b->length;

    memcpy(result->data, a->data, a->length);
    memcpy(result->data + a->length, b->data, b->length);

    return result;
}
