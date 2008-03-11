#include "little_locks.h"
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>

// in microseconds
#define SLEEP_TIME   500
#define RETRIES      300000000

const char const *read_lock     = "read.lock";
const char const *reserved_lock = "reserved.lock";
const char const *shared_dir    = "shared";

static
int get_lock(const char* name) {
  int fd;
  int retries;
  for (retries = RETRIES; retries > 0; --retries) {
    fd = open(name,O_CREAT|O_EXCL,0666);
    if (fd != -1) {
      close(fd);
      errno = 0;
      return 0;
    }
    if (errno == EEXIST) {
      usleep(SLEEP_TIME);
    } else {
      return -1;
    }
  }
  errno = EBUSY;
  return -1;
}


static
int shared_name(size_t n, char* buffer) {
  pid_t pid;
  unsigned attempt = 0;
  unsigned bytes;
  int fd;
  pid = getpid();
  do {
    bytes = snprintf(buffer, n, "%s/%lu.%u", shared_dir, pid, attempt);
    if (bytes >= n) buffer[n-1] = 0;
    fd = open(buffer,O_CREAT|O_EXCL,0666);
    if (fd == -1) {
      if (errno == EEXIST) {
        attempt += 1;
        continue;
      } else
        return -1;
    } else {
      close(fd);
      errno = 0;
      return 0;
    }
  } while (1);
}


int get_shared(size_t n, char* buffer) {
  int res = -1;
  if (get_lock(read_lock) != 0) return res;
  res = shared_name(n,buffer);
  res += unlink(read_lock);
  return res;
}

int get_reserved(const char* name) {
  if (get_lock(reserved_lock) != 0) return -1;
  if (unlink(name) == 0) return 0;
  unlink(reserved_lock);
  return -1;
}

static
int is_empty_dir(const char* name) {
  DIR* dir;
  struct dirent *cur;
  dir = opendir(name);
  if (dir == NULL) return -1;
  while (cur = readdir(dir)) {
    if (cur->d_type == DT_REG) {
      closedir(dir);
      return 0;
    }
  }
  closedir(dir);
  return 1;
}

int get_exclusive() {
  int retries;

  if (get_lock(read_lock) != 0) return -1;

  for (retries = RETRIES; retries > 0; --retries) {
    switch (is_empty_dir(shared_dir)) {
      case 1: return 0;
      case 0: usleep(SLEEP_TIME); continue;
      default: unlink(read_lock); return -1;
    }
  }
  unlink(read_lock);
  errno = EBUSY;
  return -1;
}

// assume that we have the lock
// XXX: may overwrite errno
int free_exclusive(size_t n, char* buffer) {
  int err = 0;
  err += shared_name(n,buffer);
  err += unlink(read_lock);
  err += unlink(reserved_lock);
  return err;
}

int free_shared(const char* name) {
  return unlink(name);
}


