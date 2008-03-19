#include "little.h"
#define _ATFILE_SOURCE
#include <errno.h>
#include <fcntl.h>
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

