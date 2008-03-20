#include "little.h"
#define _ATFILE_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>

const char *version_file = "version";

int get_version(const char *path, version_t *version, int  *nextfreeblock) {
  int dfd, fd, err;

  dfd = open(path, O_RDONLY);
  fd = openat(dfd, version_file, O_RDONLY);
  err = errno;
  close(dfd);
  if (fd == -1) {
    if (err == ENOENT) {
      *version = 0;
      *nextfreeblock = 0;
    }
    errno=err;
    return -err;
  }
  if (read(fd,version, sizeof(version_t)) == sizeof(version_t)) {
    *version = DECODE_VERSION(*version);

    if (read(fd,nextfreeblock, sizeof(int)) == sizeof(int)) {
      *nextfreeblock = DECODE_INT(*nextfreeblock);

      close(fd);
      return 0;
    }
  }
  close(fd);
  return -1;
}

int read_block(const char* path, int block, void* buffer, version_t ver) {
  int dfd, fd, res;
  char name[LITTLE_MAX_PATH];
  version_t cur_ver;
  trace("R %d/%llu, %s\n", block, ver, path);

  dfd = open(path, O_RDONLY);
  if (dfd == -1) return -errno;
  snprintf(name,sizeof(name),"%d", block);
  fd = openat(dfd,name,O_RDONLY);
  close(dfd);
  if (fd == -1) {
    if (errno == ENOENT) {
      return 0;
    } else {
      return -errno;
    }
  }
  res = read(fd, &cur_ver, sizeof(version_t));
  if (res < sizeof(version_t) || DECODE_VERSION(cur_ver) > ver) {
    close(fd);
    trace("RW: version mismatch %llu\n", DECODE_VERSION(cur_ver));
    return -EIO;
  }
  res = read(fd, buffer, LITTLE_SECTOR_SIZE);
  close(fd);
  if (res == -1) return -errno;
  return res;
}

