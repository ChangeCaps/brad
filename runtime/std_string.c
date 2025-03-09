#include "std_string.h"

#include <stdlib.h>
#include <string.h>

brad_str brad_str_concat(
    brad_str a,
    brad_str b
) {
    brad_str result = malloc(sizeof(brad_str) + a->length + b->length);

    result->length = a->length + b->length;

    memcpy(result->data, a->data, a->length);
    memcpy(result->data + a->length, b->data, b->length);

    return result;
}
