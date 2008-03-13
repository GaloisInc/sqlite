#include "little.h"
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>


const char const *read_lock     = "read.lock";
const char const *reserved_lock = "reserved.lock";
const char const *shared_dir    = "shared";


// Returns -1 if we had to cut off something.
int in_dir(const char *path, const char* file, size_t n, char *buf) {
  int bytes;
  bytes = snprintf(buf,n,"%s/%s",path,file);
  if (bytes >= n) { buf[n-1] = 0; return -1; }
  return 0;
}


// Try to create a file representing a lock.
// Returns 0 on success, -EAGAIN if we exhaused the retries,
// or some other negative error.
static int set_lock(const char *path, const char* file, int tries) {
  char buffer[LITTLE_MAX_PATH];
  int fd;
  if (in_dir(path,file,LITTLE_MAX_PATH,buffer) != 0) return -EINVAL;

  for (; tries > 0; --tries) {
    fd = open(buffer,O_WRONLY|O_CREAT|O_EXCL,0666);
    if (fd != -1) {
      close(fd);
      return 0;
    }
    if (errno == EEXIST) {
      usleep(LITTLE_SLEEP_TIME);
    } else {
      return -errno;
    }
  }
  return -EAGAIN;
}


// Remove a file representing a lock.
static int remove_lock(const char *path, const char *file) {
  char buffer[LITTLE_MAX_PATH];
  if (in_dir(path,file,LITTLE_MAX_PATH,buffer) != 0) return -EINVAL;
  if (unlink(buffer) == -1) {
    if (errno == ENOENT) return 0; else return -errno;
  }
  return 0;
}


// Check if the directory contains any files.
// We only consider regular files.
static int is_empty_dir(const char *path) {
  DIR* dir;
  struct dirent *cur;
  dir = opendir(path);
  if (dir == NULL) return -errno;
  while (cur = readdir(dir)) {
    if (cur->d_type == DT_REG) {
      closedir(dir);
      return 0;
    }
  }
  closedir(dir);
  return 1;
}


// Check if a file exists.
static int exists(const char *path, const char *file) {
  int dfd, fd;
  dfd = open(path,O_RDONLY);
  if (dfd == -1) {
    return -errno;
  }
  fd = openat(dfd, file, O_RDONLY);
  if (fd > -1) {
    close(fd);
    return 1;
  }
  if (errno == ENOENT) return 0;
  return -errno;
}

int check_res(const char *path) {
  return exists(path, reserved_lock);
}


// Convert a shared lock id into a string.
// XXX: there may be problems with this
static void shared_name(int x, int n, char* buffer) {
  int bytes;
  bytes = snprintf(buffer,n,"%s/%lu.%u",shared_dir,getpid(), x);
  if (bytes >= n) buffer[n-1] = 0;
}


// Returns a new shared lock id, or negative error.
static int new_shared_name(const char* path) {
  unsigned attempt;
  char buffer[LITTLE_MAX_PATH];
  int res;
  for (attempt = 0, res = -EAGAIN; res == -EAGAIN; ++attempt) {
    shared_name(attempt, LITTLE_MAX_PATH, buffer);
    res = set_lock(path,buffer,1);
  }
  return res;
}


// -----------------------------------------------------------------------------

// Returns an id for the shared lock, or negative error.
int get_shared(const char* path) {
  int res;
  res = set_lock(path,read_lock,LITTLE_RETRIES);
  if (res != 0) return res;
  errno = 0;
  res = new_shared_name(path);
  remove_lock(path,read_lock);   // XXX: things will fail if this fails
  return res;
}


// Get the reserved lock.
// Returns -EBUSY if the lock laredy existed.
int get_reserved(const char* path, int shared) {
  int res;
  char buffer[LITTLE_MAX_PATH];
  res = set_lock(path,reserved_lock,1);
  if (res != 0) return res;
  shared_name(shared, LITTLE_MAX_PATH, buffer);
  remove_lock(path, buffer);    // XXX: hopefully this worked.
  return 0;
}

// Get the exclusive lock, waiting for all readers to finish.
int get_exclusive(const char* path) {
  int retries, res;
  char buffer[LITTLE_MAX_PATH];

  if (in_dir(path,shared_dir,LITTLE_MAX_PATH,buffer) != 0) return -EINVAL;

  res = set_lock(path, read_lock, LITTLE_RETRIES);
  if (res != 0) return res;

  for (retries = LITTLE_RETRIES; retries > 0; --retries) {
    res = is_empty_dir(buffer);
    switch (res) {
      case 1: return 0;
      case 0: usleep(LITTLE_SLEEP_TIME); continue;
      default: remove_lock(path,read_lock);
               return res;
    }
  }
  remove_lock(path,read_lock);      // XXX: Hope this worked.
  return -EAGAIN;
}


int free_exclusive(const char* path) {
  int res;
  res = new_shared_name(path);
  remove_lock(path,read_lock);      // XXX: Hope that these worked
  remove_lock(path,reserved_lock);  // ...
  return res;
}


int free_shared(const char *path, int shared) {
  char buffer[LITTLE_MAX_PATH];
  shared_name(shared,LITTLE_MAX_PATH,buffer);
  return remove_lock(path, buffer);
}


