#pragma once

#include "types.h"

typedef FILE* brad_file;

void brad_print(brad_str string);

brad_file brad_file_open(brad_str path, brad_str mode);
void brad_file_write(brad_file file, brad_str string);
brad_str brad_file_read(brad_file file);
void brad_file_close(brad_file file);