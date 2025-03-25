#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gc.h"
#include "std_io.h"
#include "std_string.h"

void brad_print(
    brad_str string
) {
    fwrite(string->data, 1, string->length, stdout);
}

brad_file brad_file_open(
    brad_str path,
    brad_str mode
) {
    char* mode_c_str = malloc(mode->length + 1);
    memcpy(mode_c_str, mode->data, mode->length);
    mode_c_str[mode->length] = '\0';

    char* path_c_str = malloc(path->length + 1);
    memcpy(path_c_str, path->data, path->length);
    path_c_str[path->length] = '\0';

    FILE* file = fopen(path_c_str, mode_c_str);

    free(mode_c_str);
    free(path_c_str);

    return file;
}

void brad_file_write(
    brad_file file,
    brad_str string
) {
    if (file == NULL) {
        return;
    }

    fwrite(string->data, 1, string->length, file);
}

brad_str brad_file_read(
    brad_file file
) {
    if (file == NULL) {
        return brad_str_from_c_str("File is NULL");
    }

    // Get size of file
    if (fseek(file, 0, SEEK_END) != 0) {
        goto error;
    }

    int64_t file_size = ftell(file);
    rewind(file);

    if (file_size < 0) {
        goto error;
    }

    uint64_t buffer_size = sizeof(brad_str) + file_size + 1;
    brad_str buffer = (brad_str
    )brad_alloc(buffer_size, brad_str_marker, "brad_file_read buffer");

    size_t read_size = fread(buffer->data, 1, file_size, file);

    if (read_size != (size_t)file_size) {
        goto cleanup;
    }

    buffer->length = read_size;

    return buffer;

cleanup:
    brad_release((brad_ptr)buffer);
error:
    return brad_str_from_c_str("Error reading file");
}
void brad_file_close(
    brad_file file
) {
    if (file == NULL) {
        return;
    }

    fclose((FILE*)file);
}