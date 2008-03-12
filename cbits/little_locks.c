#include "little_locks.h"
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>

// in microseconds
#define SLEEP_TIME   500
#define RETRIES      10

const char const *read_lock     = "read.lock";
const char const *reserved_lock = "reserved.lock";
const char const *shared_dir    = "shared";

static
int open_rel(const char *path, const char* file, int flags) {
  int fd, dfd, err;
  dfd = open(path,O_RDONLY);
  if (dfd == -1) return -errno;
  fd = openat(dfd,file,flags,0666);
  err = errno;
  close(dfd);
  errno = err;
  return fd;
}

static
int unlink_rel(const char *path, const char *file) {
  int dfd;
  dfd = open(path,O_RDONLY);
  if (dfd == -1) return -errno;
  if (unlinkat(dfd,file,0) == -1) return -errno;
  return 0;
}


int check_res(const char* path) {
  int fd;
  fd = open_rel(path,reserved_lock,O_RDONLY);
  if (fd != -1) {
    close(fd);
    return 1;
  }
  if (errno == ENOENT) return 0;
  return -errno;
}

static
int get_lock(const char* path, const char* name, int tries) {
  int fd;
  for (; tries > 0; --tries) {
    fd = open_rel(path,name,O_WRONLY|O_CREAT|O_EXCL);
    if (fd != -1) {
      close(fd);
      return 0;
    }
    if (errno == EEXIST) {
      usleep(SLEEP_TIME);
    } else {
      return -errno;
    }
  }
  return -EBUSY;
}


static
int shared_name(const char* path, size_t n, char* buffer) {
  pid_t pid;
  unsigned attempt = 0;
  unsigned bytes;
  int fd;
  pid = getpid();
  do {
    bytes = snprintf(buffer, n, "%s_%lu.%u", shared_dir, pid, attempt);
    if (bytes >= n) buffer[n-1] = 0;
    fd = open_rel(path,buffer,O_CREAT|O_EXCL);
    if (fd == -1) {
      if (errno == EEXIST) {
        attempt += 1;
        continue;
      } else
        return -errno;
    } else {
      close(fd);
      return 0;
    }
  } while (1);
}


int get_shared(const char* path, size_t n, char* buffer) {
  int res;
  res = get_lock(path, read_lock,RETRIES);
  if (res != 0) return res;
  errno = 0;
  shared_name(path,n,buffer);
  unlink_rel(path,read_lock);
  return -errno;
}

int get_reserved(const char *path, const char* name) {
  int res;
  res = get_lock(path,reserved_lock,1);
  if (res != 0) return res;
  errno = 0;
  unlink_rel(path,name);
  unlink_rel(path,reserved_lock);
  return -errno;
}

static
int no_shared_locks(const char *path) {
  DIR* dir;
  struct dirent *cur;
  dir = opendir(path);
  if (dir == NULL) return -errno;
  while (cur = readdir(dir)) {
    if (cur->d_type == DT_REG && (strncmp(shared_dir,cur->d_name,3) == 0)) {
      closedir(dir);
      return 0;
    }
  }
  closedir(dir);
  return 1;
}

int get_exclusive(const char* path) {
  int retries, res;

  res = get_lock(path,read_lock,RETRIES);
  if (res != 0) return res;

  for (retries = RETRIES; retries > 0; --retries) {
    switch (no_shared_locks(path)) {
      case 1: return 0;
      case 0: usleep(SLEEP_TIME); continue;
      default: res = -errno;
               return (unlink_rel(path,read_lock) == -1) ? -errno : res;
    }
  }
  if (unlink_rel(path,read_lock) == -1) return -errno;
  return -EBUSY;
}

// assume that we have the lock
// XXX: may overwrite errno
int free_exclusive(const char* path, size_t n, char* buffer) {
  int err = 0;
  err += shared_name(path, n,buffer);
  err += unlink_rel(path,read_lock);
  err += unlink_rel(path,reserved_lock);
  return err;
}

int free_shared(const char *path, const char* name) {
  return unlink_rel(path,name) == -1 ? -errno : 0;
}


