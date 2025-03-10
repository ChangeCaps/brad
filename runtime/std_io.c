#include <stdio.h>

#include "gc.h"
#include "std_io.h"

void brad_print(
    brad_str string
) {
    fwrite(string->data, 1, string->length, stdout);
    brad_release((brad_ptr)string);
}
