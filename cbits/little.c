#include "sqlite3.h"
#include <stdio.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <string.h>

#define LITTLE_MAX_PATH 512
#define LITTLE_VERSION 1
#define LITTLE_SECTOR_SIZE 1024

// XXX
#define LITTLE_DEVICE_CHARACTERISTICS 0

sqlite3_vfs little_vfs;
sqlite3_io_methods little_methods;

typedef struct {
  struct sqlite3_file base_file;
  const char* name;
  int dir_fd;
  int shared_lock_number;
} little_file;

// XXX: check flags
static
int little_open(sqlite3_vfs *self, const char* zName,
                sqlite3_file *f, int nOut, int *zOut) {

  little_file *file = (little_file*)f;

  printf("open %s\n", zName);
  (file->base_file).pMethods  = &little_methods;
  file->name      = zName;      // is it OK to hold on the ptr here?
  if (mkdir(zName,0666) == -1 && errno != EEXIST) return SQLITE_CANTOPEN;
  return SQLITE_OK;
}

static
int little_delete (sqlite3_vfs* self, const char *zName, int syncDir) {
  printf("delete %s\n", zName);
  // delete dir, etc
  return SQLITE_OK;
}

static
int little_access(sqlite3_vfs* self, const char *zName, int flags) {
  printf("access %s\n", zName);
  return SQLITE_OK;
}


static
int little_close(sqlite3_file *file) {
  little_file *self = (little_file*)file;
  printf("close %s\n", self->name);
  return SQLITE_OK;
}

static
int open_block(int dir_fd, char *name) {
  int fd;
  struct stat mystat;
  void *buf;

  fd = openat(dir_fd, name, O_RDWR | O_CREAT, 0666);
  if (fd == -1) return -1;

  if (fstat(fd,&mystat) == -1) {
    close(fd);
    return -1;
  }

  if (mystat.st_size == 0) {
    ftruncate(fd,LITTLE_SECTOR_SIZE);
  } else if (mystat.st_size != LITTLE_SECTOR_SIZE) {
    close(fd);
    return -1;
  }

  return fd;
}

static
int little_read(sqlite3_file *file, void *buf, int iAmt, sqlite3_int64 iOfst) {
  int filenumber;
  int fd;
  int got = 0;
  int littleAmt;
  void *file_contents;
  char namebuffer[10];

  little_file *self = (little_file*)file;
  printf("read %s, off: %ld, amt: %d\n", self->name, iOfst, iAmt);


  for (filenumber = iOfst / LITTLE_SECTOR_SIZE,
       iOfst -= filenumber * LITTLE_SECTOR_SIZE
      ; iAmt > 0
      ; ++filenumber) {

    littleAmt = min(LITTLE_SECTOR_SIZE, iAmt - iOfst);

    snprintf("%d", sizeof(namebuffer), namebuffer, filenumber);

    fd = open_block(self->dir_fd, namebuffer);
    if (fd == -1) {
      return SQLITE_IOERR_READ;
    }

    file_contents = mmap(NULL, LITTLE_SECTOR_SIZE, PROT_READ, MAP_SHARED | MAP_POPULATE, fd, 0);

    if (file_contents == MAP_FAILED) {
      close(fd);
      return SQLITE_IOERR_READ;
    }

    memcpy(buf,file_contents + iOfst,littleAmt);

    unmmap(file_contents, LITTLE_SECTOR_SIZE);
    close(fd);

    iAmt -= littleAmt;
    buf  += littleAmt;
    iOfst = 0;
  }
  return SQLITE_OK;
}

static
int little_write(sqlite3_file *file,
                  const void *buf, int iAmt, sqlite3_int64 iOfst) {
  int filenumber;
  int littleAmt;
  int fd;
  char namebuffer[10];
  void*file_contents;

  little_file *self = (little_file*)file;
  printf("write %s, off: %ld, amt: %d\n", self->name, iOfst, iAmt);

  for (filenumber = iOfst / LITTLE_SECTOR_SIZE,
       iOfst -= filenumber * LITTLE_SECTOR_SIZE
      ; iAmt > 0
      ; ++filenumber) {

    littleAmt = min(LITTLE_SECTOR_SIZE, iAmt - iOfst);

    snprintf("%d", sizeof(namebuffer), namebuffer, filenumber);

    fd = open_block(self->dir_fd, namebuffer);
    if (fd == -1) {
      return SQLITE_IOERR_WRITE;
    }

    file_contents = mmap(NULL, LITTLE_SECTOR_SIZE, PROT_WRITE, MAP_SHARED, fd, 0);

    if (file_contents == MAP_FAILED) {
      close(fd);
      return SQLITE_IOERR_WRITE;
    }

    memcpy(file_contents + iOfst, buf, littleAmt);

    unmmap(file_contents, LITTLE_SECTOR_SIZE);
    close(fd);

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
  little_file *self = (little_file*)file;
  printf("file_size %s\n", self->name);
  return SQLITE_OK;
}

static
int little_lock(sqlite3_file *file, int lock) {
  little_file *self = (little_file*)file;
  printf("lock %s %d\n", self->name, lock);
  return SQLITE_OK;
}

static
int little_unlock(sqlite3_file *file, int lock) {
  little_file *self = (little_file*)file;
  printf("unlock %s %d\n", self->name, lock);
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

