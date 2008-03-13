#include "sqlite3.h"
#include "little.h"
#include "little_locks.h"

#include <stdio.h>
#include <dirent.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <string.h>

// XXX
#define LITTLE_DEVICE_CHARACTERISTICS 0

sqlite3_vfs little_vfs;
sqlite3_io_methods little_methods;

typedef struct {
  struct sqlite3_file base_file;
  const char* name;
  int shared_lock_number;
} little_file;

// XXX: check flags
static int little_open(sqlite3_vfs *self, const char* zName,
                sqlite3_file *f, int nOut, int *zOut) {

  little_file *file = (little_file*)f;

  printf("open %s\n", zName);
  (file->base_file).pMethods  = &little_methods;
  file->name      = zName;      // is it OK to hold on the ptr here?
  if (mkdir(zName,0666) == -1 && errno != EEXIST) return SQLITE_CANTOPEN;
  return SQLITE_OK;
}

static int little_delete (sqlite3_vfs* self, const char *zName, int syncDir) {
  printf("delete %s\n", zName);
  char buffer[LITTLE_MAX_PATH];
  if (snprintf(buffer,sizeof(buffer),"%s/shared",zName) >= LITTLE_MAX_PATH)
    return SQLITE_ERROR;
  if (rmFullDir(buffer) == -1) return SQLITE_ERROR;
  if (rmFullDir(zName) == -1) return SQLITE_ERROR;
  return SQLITE_OK;
}

static int little_access(sqlite3_vfs* self, const char *zName, int flags) {
  printf("access %s\n", zName);
  return SQLITE_OK;
}


static int little_close(sqlite3_file *file) {
  little_file *self = (little_file*)file;
  printf("close %s\n", self->name);
  return SQLITE_OK;
}


int read_block(const char* path, int block, void* buffer) {
  int dfd, fd, res;
  char name[LITTLE_MAX_PATH];
  dfd = open(path, O_RDONLY);
  if (dfd == -1) return -errno;
  snprintf(name,sizeof(name),"%d", block);
  fd = openat(dfd,name,O_RDONLY);
  close(dfd);
  if (fd == -1) return -errno;
  res = read(fd, buffer, LITTLE_SECTOR_SIZE);
  close(fd);
  if (res == -1) return -errno;
  return res;
}

int write_block(const char* path, int block, const char* buffer) {
  int dfd, fd, res;
  char name[LITTLE_MAX_PATH];
  dfd = open(path,O_RDONLY);
  if (dfd == -1) return -errno;
  snprintf(name,sizeof(name),"%d", block);
  fd = openat(dfd,path,O_WRONLY|O_CREAT,0666);
  close(dfd);
  if (fd == -1) return -errno;
  res = write(fd, buffer, LITTLE_SECTOR_SIZE);
  close(fd);
  if (res == -1) return -errno;
  return res;
}


static
int little_read(sqlite3_file *file, void *buf, int iAmt, sqlite3_int64 iOfst) {
  int filenumber;
  int got = 0;
  int littleAmt;

  little_file *self = (little_file*)file;
  printf("read %s, off: %ld, amt: %d\n", self->name, iOfst, iAmt);

  for (filenumber = iOfst / LITTLE_SECTOR_SIZE,
       iOfst -= filenumber * LITTLE_SECTOR_SIZE
      ; iAmt > 0
      ; ++filenumber) {

    littleAmt = min(LITTLE_SECTOR_SIZE, iAmt - iOfst);
    if (iOfst == 0 && littleAmt == LITTLE_SECTOR_SIZE) {
      got = read_block(self->name,filenumber,buf);
      if (got < 0) return SQLITE_IOERR_READ;
      if (got < LITTLE_SECTOR_SIZE) return SQLITE_IOERR_SHORT_READ;
    } else {
      char buffer[LITTLE_SECTOR_SIZE];
      got = read_block(self->name,filenumber,buffer);
      if (got < 0) return SQLITE_IOERR_READ;
      if (got < LITTLE_SECTOR_SIZE) return SQLITE_IOERR_SHORT_READ;
      memcpy(buf,buffer + iOfst,littleAmt);
    }

    iAmt -= littleAmt;
    buf  += littleAmt;
    iOfst = 0;
  }
  return SQLITE_OK;
}

static
int little_write(sqlite3_file *file,
                  const void *buf, int iAmt, sqlite3_int64 iOfst) {
  int filenumber, littleAmt, got;

  little_file *self = (little_file*)file;
  printf("write %s, off: %ld, amt: %d\n", self->name, iOfst, iAmt);

  for (filenumber = iOfst / LITTLE_SECTOR_SIZE,
       iOfst -= filenumber * LITTLE_SECTOR_SIZE
      ; iAmt > 0
      ; ++filenumber) {

    littleAmt = min(LITTLE_SECTOR_SIZE, iAmt - iOfst);
    if (iOfst == 0 && littleAmt == LITTLE_SECTOR_SIZE) {
      got = write_block(self->name,filenumber,buf);
      if (got < LITTLE_SECTOR_SIZE) return SQLITE_IOERR_WRITE;
    } else {
      char buffer[LITTLE_SECTOR_SIZE];
      got = read_block(self->name,filenumber,buffer);
      if (got < LITTLE_SECTOR_SIZE) return SQLITE_IOERR_WRITE;
      memcpy(buffer + iOfst,buf,littleAmt);
      got = write_block(self->name,filenumber,buffer);
      if (got < LITTLE_SECTOR_SIZE) return SQLITE_IOERR_WRITE;
    }

    iAmt -= littleAmt;
    buf  += littleAmt;
    iOfst = 0;
  }

  return SQLITE_OK;
}

static
int little_truncate(sqlite3_file *file, sqlite3_int64 size) {
  little_file *self = (little_file*)file;
  printf("truncte %s, size %ld\n", self->name, size);
  return SQLITE_OK;
}

static
int little_sync(sqlite3_file *file, int flags) {
  little_file *self = (little_file*)file;
  printf("sync %s, flags %d\n", self->name, flags);
  return SQLITE_OK;
}

static
int little_file_size(sqlite3_file *file, sqlite3_int64 *pSize) {
  DIR *dir;
  struct dirent *cur;
  sqlite3_int64 count = 0;
  little_file *self = (little_file*)file;
  printf("file_size %s\n", self->name);

  dir = opendir(self->name);
  if (dir == NULL) return SQLITE_ERROR;

  while ( (cur = readdir(dir)) != NULL ) {
    if (cur->d_type == DT_REG) {
      count += LITTLE_SECTOR_SIZE;
    }
  }
  closedir(dir);
  *pSize = count;

  return SQLITE_OK;
}

static
int little_lock(sqlite3_file *file, int lock) {
  int res;
  little_file *self = (little_file*)file;

  printf("lock %s %d\n", self->name, lock);
  switch (lock) {
    case SQLITE_LOCK_SHARED:
      res = get_shared(self->name);
      break;

    case SQLITE_LOCK_RESERVED:
      res = get_reserved(self->name, self->shared_lock_number);
      break;

    case SQLITE_LOCK_EXCLUSIVE:
      res = get_exclusive(self->name);
      break;

    default: return SQLITE_ERROR;
  }
  if (res == -EAGAIN) return SQLITE_BUSY;
  if (res < 0) return SQLITE_ERROR;
  self->shared_lock_number = res;
  return SQLITE_OK;
}

static
int little_unlock(sqlite3_file *file, int lock) {
  int res;
  little_file *self = (little_file*)file;

  printf("unlock %s %d\n", self->name, lock);
  switch (lock) {
    case SQLITE_LOCK_NONE:    res = free_shared(self->name); break;
    case SQLITE_LOCK_SHARED:  res = free_exclusive(self->name); break;
    default: return SQLITE_ERROR;
  }
  if (res < 0) return SQLITE_ERROR;
  self->shared_lock_number = res;
  return SQLITE_OK;
}

static
int little_check_reserved_lock(sqlite3_file *file) {
  little_file *self = (little_file*)file;
  printf("check_reserved %s\n", self->name);
  return SQLITE_OK;
}

static
int little_file_control(sqlite3_file *file, int op, void *pArg) {
  little_file *self = (little_file*)file;
  printf("file_control %s %d\n", self->name, op);
  return SQLITE_OK;
}

static
int little_sector_size(sqlite3_file *file) {
  little_file *self = (little_file*)file;
  printf("sector_size %s\n", self->name);
  return LITTLE_SECTOR_SIZE;
}

static
int little_device_characteristics(sqlite3_file *file) {
  little_file *self = (little_file*)file;
  printf("device_characterisitcs %s\n", self->name);
  return LITTLE_DEVICE_CHARACTERISTICS;
}



sqlite3_vfs* init_little_vfs(sqlite3_vfs *orig) {
  little_vfs.iVersion       = 1;
  little_vfs.szOsFile       = sizeof(little_file);
  little_vfs.mxPathname     = LITTLE_MAX_PATH;
  // little_vfs.pNext is not initialized by us
  little_vfs.zName          = "filebased";
  little_vfs.pAppData       = 0;
  little_vfs.xOpen          = little_open;
  little_vfs.xDelete        = little_delete;
  little_vfs.xAccess        = little_access;
  little_vfs.xGetTempname   = orig->xGetTempname;
  little_vfs.xFullPathname  = orig->xFullPathname;
  little_vfs.xDlOpen        = orig->xDlOpen;
  little_vfs.xDlError       = orig->xDlError;
  little_vfs.xDlSym         = orig->xDlSym;
  little_vfs.xDlClose       = orig->xDlClose;
  little_vfs.xRandomness    = orig->xRandomness;
  little_vfs.xSleep         = orig->xSleep;
  little_vfs.xCurrentTime   = orig->xCurrentTime;

  little_methods.iVersion   = LITTLE_VERSION;
  little_methods.xClose     = little_close;
  little_methods.xRead      = little_read;
  little_methods.xWrite     = little_write;
  little_methods.xTruncate  = little_truncate;
  little_methods.xSync      = little_sync;
  little_methods.xFileSize  = little_file_size;
  little_methods.xLock      = little_lock;
  little_methods.xUnlock    = little_unlock;
  little_methods.xCheckReservedLock     = little_check_reserved_lock;
  little_methods.xFileControl           = little_file_control;
  little_methods.xSectorSize            = little_sector_size;
  little_methods.xDeviceCharacteristics = little_device_characteristics;

  return &little_vfs;
}


int register_little_vfs(int makeDflt) {
  struct sqlite3_vfs *un;
  un = sqlite3_vfs_find("unix");
  if (un == NULL) return -1;
  sqlite3_vfs_register(init_little_vfs(un), makeDflt);
  return 0;
}

int rmFullDir(const char *name) {
  struct dirent *cur;
  int fd;
  DIR *dir;

  dir = opendir(name);
  if (dir == NULL) -1;

  fd = open(name, O_RDONLY);
  if (fd == -1) return -1;

  while (cur = readdir(dir)) {
    if (cur->d_type == DT_REG) {
      if (unlinkat(fd,cur->d_name,0) == -1) return -1;
    }
  }

  close(fd);
  closedir(dir);
  return rmdir(name);
}
