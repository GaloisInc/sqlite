#ifndef LITTLE_LOCKS
#define LITTLE_LOCKS 1

#include <unistd.h>

int check_res(const char* path);
int get_shared(const char* path);
int get_reserved(const char* path);
int get_exclusive(const char *path, int shared);
int free_exclusive(const char *path);
int free_shared(const char* path);

#endif

