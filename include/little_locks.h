#ifndef LITTLE_LOCKS
#define LITTLE_LOCKS 1

#include <unistd.h>

int get_shared(size_t n, char* buffer);
int get_reserved(const char* name);
int get_exclusive();
int free_exclusive(size_t n, char* buffer);
int free_shared(const char* name);

#endif

