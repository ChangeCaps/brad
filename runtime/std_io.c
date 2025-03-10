#include <stdio.h>

#include "std_io.h"

void brad_print(
    brad_str string
) {
    fwrite(string->data, 1, string->length, stdout);
}
