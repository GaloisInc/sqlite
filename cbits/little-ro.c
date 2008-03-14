
#define _ATFILE_SOURCE

#include "sqlite3.h"
#include "little.h"

#include <unistd.h>
#include <stdio.h>
#include <dirent.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <string.h>

#define min(x,y) ( (x)<(y)?(x):(y) )


sqlite3_vfs little_ro_vfs;
sqlite3_io_methods little_ro_methods;

typedef struct {
  struct sqlite3_file base_file;
  const char* name;
  version_t version;
} little_ro_file;


// XXX: check flags
static int little_ro_open(sqlite3_vfs *self, const char* zName,
                sqlite3_file *f, int nOut, int *zOut) {

  little_ro_file *file = (little_ro_file*)f;

  if (get_version(zName, &(file->version)) != 0) {
    return SQLITE_CANTOPEN;
  }
  file->base_file.pMethods = &little_ro_methods;
  file->name = zName;
  return SQLITE_OK;
}

static int little_ro_delete (sqlite3_vfs* self, const char *zName, int syncDir){
  return SQLITE_ERROR;
}


static int little_ro_close(sqlite3_file *file) {
  // little_ro_file *self = (little_ro_file*)file;
  return SQLITE_OK;
}


static int read_block(const char* path, int block, version_t ver, void* buffer) {
  int dfd, fd, res, err;
  char name[LITTLE_MAX_PATH];
  version_t cur_ver;
  dfd = open(path, O_RDONLY);
  if (dfd == -1) return -errno;
  snprintf(name,sizeof(name),"%d", block);
  fd = openat(dfd,name,O_RDONLY);
  err = errno;
  close(dfd);
  if (fd == -1) {
    if (errno == ENOENT) {
      return 0;   // XXX: hopefully it did not disappear on us...
    } else {
      return -err;
    }
  }
  res = read(fd, &cur_ver, sizeof(version_t));
  if (res < sizeof(version_t) || DECODE_VERSION(cur_ver) > ver) {
    close(fd);
    return -EIO;
  }
  res = read(fd, buffer, LITTLE_SECTOR_SIZE);
  err = errno;
  close(fd);
  if (res == -1) return -err;
  return res;
}


static int little_ro_read(sqlite3_file *file,
                            void *buf, int iAmt, sqlite3_int64 iOfst) {
  int filenumber;
  int got = 0;
  int littleAmt;

  little_ro_file *self = (little_ro_file*)file;

  for (filenumber = iOfst / LITTLE_SECTOR_SIZE,
       iOfst -= filenumber * LITTLE_SECTOR_SIZE
      ; iAmt > 0
      ; ++filenumber) {

    littleAmt = min(LITTLE_SECTOR_SIZE - iOfst , iAmt);
    if (iOfst == 0 && littleAmt == LITTLE_SECTOR_SIZE) {
      got = read_block(self->name,filenumber,self->version,buf);
      if (got < 0) return SQLITE_IOERR_READ;
      if (got < LITTLE_SECTOR_SIZE) return SQLITE_IOERR_SHORT_READ;
    } else {
      char buffer[LITTLE_SECTOR_SIZE];
      got = read_block(self->name,filenumber,self->version,buffer);
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

static int little_ro_write(sqlite3_file *file,
                  const void *buf, int iAmt, sqlite3_int64 iOfst) {
  return SQLITE_IOERR_WRITE;
}

static int little_ro_truncate(sqlite3_file *file, sqlite3_int64 size) {
  return SQLITE_ERROR;
}

static int little_ro_sync(sqlite3_file *file, int flags) {
  return SQLITE_ERROR;
}

// XXX
static
int little_ro_file_size(sqlite3_file *file, sqlite3_int64 *pSize) {
  DIR *dir;
  struct dirent *cur;
  sqlite3_int64 count = 0;
  little_ro_file *self = (little_ro_file*)file;

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

static int little_ro_lock(sqlite3_file *file, int lock) {
  return SQLITE_OK;
}

static int little_ro_unlock(sqlite3_file *file, int lock) {
  return SQLITE_OK;
}

// XXX
static int little_ro_check_reserved_lock(sqlite3_file *file) {
  // little_ro_file *self = (little_ro_file*)file;
  return 0; // check_res(self->name);
}

static int little_ro_file_control(sqlite3_file *file, int op, void *pArg) {
  return SQLITE_OK;
}

static int little_ro_sector_size(sqlite3_file *file) {
  return LITTLE_SECTOR_SIZE;
}

static int little_ro_device_characteristics(sqlite3_file *file) {
  return LITTLE_DEVICE_CHARACTERISTICS;
}



sqlite3_vfs* init_little_ro_vfs(sqlite3_vfs *orig) {
  little_ro_vfs.iVersion       = 1;
  little_ro_vfs.szOsFile       = sizeof(little_ro_file);
  little_ro_vfs.mxPathname     = LITTLE_MAX_PATH;
  // little_ro_vfs.pNext is not initialized by us
  little_ro_vfs.zName          = "filebased-ro";
  little_ro_vfs.pAppData       = 0;
  little_ro_vfs.xOpen          = little_ro_open;
  little_ro_vfs.xDelete        = little_ro_delete;
  little_ro_vfs.xAccess        = orig->xAccess;
  little_ro_vfs.xGetTempname   = orig->xGetTempname;
  little_ro_vfs.xFullPathname  = orig->xFullPathname;
  little_ro_vfs.xDlOpen        = orig->xDlOpen;
  little_ro_vfs.xDlError       = orig->xDlError;
  little_ro_vfs.xDlSym         = orig->xDlSym;
  little_ro_vfs.xDlClose       = orig->xDlClose;
  little_ro_vfs.xRandomness    = orig->xRandomness;
  little_ro_vfs.xSleep         = orig->xSleep;
  little_ro_vfs.xCurrentTime   = orig->xCurrentTime;

  little_ro_methods.iVersion   = 1;
  little_ro_methods.xClose     = little_ro_close;
  little_ro_methods.xRead      = little_ro_read;
  little_ro_methods.xWrite     = little_ro_write;
  little_ro_methods.xTruncate  = little_ro_truncate;
  little_ro_methods.xSync      = little_ro_sync;
  little_ro_methods.xFileSize  = little_ro_file_size;
  little_ro_methods.xLock      = little_ro_lock;
  little_ro_methods.xUnlock    = little_ro_unlock;
  little_ro_methods.xCheckReservedLock     = little_ro_check_reserved_lock;
  little_ro_methods.xFileControl           = little_ro_file_control;
  little_ro_methods.xSectorSize            = little_ro_sector_size;
  little_ro_methods.xDeviceCharacteristics = little_ro_device_characteristics;

  return &little_ro_vfs;
}


int register_little_ro_vfs(int makeDflt) {
  struct sqlite3_vfs *un;
  un = sqlite3_vfs_find("unix");
  if (un == NULL) return -1;
  sqlite3_vfs_register(init_little_ro_vfs(un), makeDflt);
  return 0;
}


