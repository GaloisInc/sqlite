#ifndef LITTLE_LOCKS
#define LITTLE_LOCKS 1

#include <unistd.h>

int check_res();
int get_shared(const char* path, size_t n, char* buffer);
int get_reserved(const char* path, const char* name);
int get_exclusive(const char *path);
int free_exclusive(const char *path, size_t n, char* buffer);
int free_shared(const char* path, const char* name);

#endif

