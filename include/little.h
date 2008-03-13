#ifndef LITTLE_H
#define LITTLE_H 1

#define LITTLE_SLEEP_TIME   500
#define LITTLE_RETRIES      10
#define LITTLE_MAX_PATH     512
#define LITTLE_VERSION      1
#define LITTLE_SECTOR_SIZE  1024

int register_little_vfs(int makeDflt);
#endif

