
#define _ATFILE_SOURCE

#include "sqlite3.h"
#include "little.h"
#include "little_locks.h"

#include <stdio.h>
#include <stdlib.h>
#include <dirent.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <string.h>

#define min(x,y) ( (x)<(y)?(x):(y) )
#define max(x,y) ( (x)>(y)?(x):(y) )

sqlite3_vfs little_vfs;
sqlite3_io_methods little_methods;

typedef struct {
  struct sqlite3_file base_file;
  const char* name;
  int shared_lock_number;
  version_t version;
  int nextfreeblock;
  int dirty;
  int lastblock;
  char lastbuffer[LITTLE_SECTOR_SIZE];
} little_file;

static int write_block(const char* path, int block, const char* buffer, version_t version);


int rmFullDir(const char *name) {
  struct dirent *cur;
  int fd;
  DIR *dir;

  dir = opendir(name);
  if (dir == NULL) return -1;

  fd = open(name, O_RDONLY);
  if (fd == -1) return -1;

  while ((cur = readdir(dir)) != NULL) {
    if (cur->d_type == DT_REG) {
      if (unlinkat(fd,cur->d_name,0) == -1) return -1;
    }
  }

  close(fd);
  closedir(dir);
  return rmdir(name);
}

void set_version(little_file *self) {
  int dfd, fd;
  version_t version = ENCODE_VERSION(self->version);
  int nextfreeblock = ENCODE_INT(self->nextfreeblock);

  dfd = open(self->name, O_RDONLY);
  fd  = openat(dfd, version_file, O_WRONLY | O_CREAT, 0666);
  close(dfd);
  write(fd, &version, sizeof(version_t));
  write(fd, &nextfreeblock, sizeof(int));
  close(fd);
}

static int recover(little_file *file) {
  struct dirent *cur;
  DIR *dir;

  trace("recover!..");

  file->nextfreeblock = 0;

  dir = opendir(file->name);
  if (dir == NULL) {
    trace("no dir\n");
    return -1;
  }

  while ((cur = readdir(dir)) != NULL) {
    if (cur->d_type == DT_REG) {
      int n = atoi(cur->d_name) + 1;
      file->nextfreeblock = max(file->nextfreeblock, n);
    }
  }

  closedir(dir);
  trace("size: %d\n",file->nextfreeblock);
  return 0;
}

// XXX: check flags
static int little_open(sqlite3_vfs *self, const char* zName,
                sqlite3_file *f, int nOut, int *zOut) {

  int dfd;

  trace("OPEN %s\n", zName);

  little_file *file = (little_file*)f;

  (file->base_file).pMethods  = &little_methods;
  file->name      = zName;      // is it OK to hold on the ptr here?
  if (mkdir(zName,0777) == -1 && errno != EEXIST) return SQLITE_CANTOPEN;

  dfd = open(zName,O_RDONLY);
  if (mkdirat(dfd,"shared",0777) == -1 && errno != EEXIST) {
    close(dfd);
    return SQLITE_CANTOPEN;
  }
  close(dfd);

  file->lastblock = -1;
  file->dirty = 0;

  if (0 != get_version(file->name, &(file->version), &(file->nextfreeblock))) {
    if (errno == ENOENT) {
      if (0 != recover(file))
        return SQLITE_CANTOPEN;
    }
  }
  return SQLITE_OK;
}

static int little_delete (sqlite3_vfs* self, const char *zName, int syncDir) {
  char buffer[LITTLE_MAX_PATH];
  trace("DEL %s\n", zName);
  if (snprintf(buffer,sizeof(buffer),"%s/shared",zName) >= LITTLE_MAX_PATH)
    return SQLITE_ERROR;
  if (rmFullDir(buffer) == -1) return SQLITE_ERROR;
  if (rmFullDir(zName) == -1) return SQLITE_ERROR;
  return SQLITE_OK;
}


static int little_close(sqlite3_file *file) {
  little_file *self = (little_file*)file;
  trace("CLOSE %s\n", self->name);
  return SQLITE_OK;
}

static int flush(little_file *self) {
  if (self->dirty) {
    write_block(self->name, self->lastblock, self->lastbuffer, self->version);
    self->dirty = 0;
  }
  return 0;
}

static int cached_read(little_file *self, int block) {
  int got;
  if (self->lastblock == block) {
    return LITTLE_SECTOR_SIZE;
  }
  flush(self);
  if (block < self->nextfreeblock)
    got = read_block(self->name, block, self->lastbuffer, self->version);
  else
    got = 0;
  if (got == LITTLE_SECTOR_SIZE) {
    self->lastblock = block;
  } else {
    self->lastblock = -1;
  }
  return got;
}

static int write_block(const char* path, int block, const char* buffer, version_t version) {
  int dfd, fd, res, err;
  char name[LITTLE_MAX_PATH];
  trace("W %d %s\n", block, path);
  dfd = open(path,O_RDONLY);
  if (dfd == -1) { perror(NULL); return -errno;}
  snprintf(name,sizeof(name),"%d", block);
  fd = openat(dfd,name,O_WRONLY|O_CREAT,0666);
  close(dfd);
  if (fd == -1) { perror(NULL); return -errno;}
  version = ENCODE_VERSION(version);
  res = write(fd, &version, sizeof(version_t));
  if (res == -1) {
    err=errno;
    close(fd);
    return -err;
  }
  res = write(fd, buffer, LITTLE_SECTOR_SIZE);
  close(fd);
  if (res == -1) { perror(NULL); return -errno;}
  return res;
}


static
int little_read(sqlite3_file *file, void *buf, int iAmt, sqlite3_int64 iOfst) {
  int filenumber;
  int got = 0;
  int littleAmt;

  little_file *self = (little_file*)file;

  for (filenumber = iOfst / LITTLE_SECTOR_SIZE,
       iOfst -= filenumber * LITTLE_SECTOR_SIZE
      ; iAmt > 0
      ; ++filenumber) {

    littleAmt = min(LITTLE_SECTOR_SIZE - iOfst , iAmt);

    got = cached_read(self,filenumber);
    if (got < LITTLE_SECTOR_SIZE) return SQLITE_IOERR_SHORT_READ;
    memcpy(buf,self->lastbuffer + iOfst,littleAmt);

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

  for (filenumber = iOfst / LITTLE_SECTOR_SIZE,
       iOfst -= filenumber * LITTLE_SECTOR_SIZE
      ; iAmt > 0
      ; ++filenumber) {

    littleAmt = min(LITTLE_SECTOR_SIZE - iOfst, iAmt);
    if (iOfst == 0 && littleAmt == LITTLE_SECTOR_SIZE) {
      got = write_block(self->name,filenumber,buf, self->version);
      if (got < LITTLE_SECTOR_SIZE) return SQLITE_IOERR_WRITE;
    } else {
      got = cached_read(self,filenumber);
      if (got < 0) return SQLITE_IOERR_WRITE;
      self->dirty = 1;
      self->lastblock = filenumber;
      memcpy(self->lastbuffer + iOfst,buf,littleAmt);
    }

    if (filenumber >= self->nextfreeblock) self->nextfreeblock = filenumber+1;
    iAmt -= littleAmt;
    buf  += littleAmt;
    iOfst = 0;
  }

  return SQLITE_OK;
}

static
int little_truncate(sqlite3_file *file, sqlite3_int64 size) {
  // little_file *self = (little_file*)file;
  return SQLITE_OK;
}

static
int little_sync(sqlite3_file *file, int flags) {
  little_file *self = (little_file*)file;
  trace("SYNC(%d) %s\n", flags, self->name);
  flush(self);
  return SQLITE_OK;
}

static
int little_file_size(sqlite3_file *file, sqlite3_int64 *pSize) {
  sqlite3_int64 count = 0;
  little_file *self = (little_file*)file;
  count = LITTLE_SECTOR_SIZE * self->nextfreeblock;
  *pSize = count;

  trace("FILESIZE %s = %llu\n", self->name, count);
  return SQLITE_OK;
}

static const char *locktypeName(int locktype){
  switch( locktype ){
  case 0: return "NONE";
  case 1: return "SHARED";
  case 2: return "RESERVED";
  case 3: return "PENDING";
  case 4: return "EXCLUSIVE";
  }
  return "ERROR";
}

static
int little_lock(sqlite3_file *file, int lock) {
  int res;
  little_file *self = (little_file*)file;

  trace("LOCK UP %s...\n", locktypeName(lock));
  fflush(stdout);
  switch (lock) {
    case SQLITE_LOCK_SHARED:
      res = get_shared(self->name);
      if (res != 0) trace ("Optimistic locking! forge ahead\n");
      else self->shared_lock_number = res;

      // don't worry if we don't get the lock, we have versioning
      res = get_version(self->name, &(self->version), &(self->nextfreeblock));
      if (errno == ENOENT) res = 0;
      break;

    case SQLITE_LOCK_RESERVED:
      res = get_reserved(self->name);
      break;

    case SQLITE_LOCK_EXCLUSIVE:
      res = get_exclusive(self->name, self->shared_lock_number);
      self->shared_lock_number = -1;
      ++(self->version);
      break;

    default: return SQLITE_ERROR;
  }
  if (res == -EAGAIN) return SQLITE_BUSY;
  if (res < 0) return SQLITE_ERROR;
  trace("LOCK UP %s OK\n", locktypeName(lock));
  return SQLITE_OK;
}

static
int little_unlock(sqlite3_file *file, int lock) {
  int res;
  little_file *self = (little_file*)file;
  trace("LOCK DOWN %s...\n", locktypeName(lock));

  switch (lock) {
    case SQLITE_LOCK_NONE:
       free_shared(self->name, self->shared_lock_number);
       flush(self);
       self->lastblock = -1;
       self->shared_lock_number = -1;
       break;
    case SQLITE_LOCK_SHARED:
       set_version(self);
       res = free_exclusive(self->name);
       if (res < 0) return SQLITE_ERROR;
       self->shared_lock_number = res;
       break;
    default: return SQLITE_ERROR;
  }
  trace("LOCK DOWN %s OK\n", locktypeName(lock));
  return SQLITE_OK;
}

static
int little_check_reserved_lock(sqlite3_file *file) {
  little_file *self = (little_file*)file;
  int res = check_res(self->name);
  trace("RESERVED: %d\n", res);
  return res;
}

static
int little_file_control(sqlite3_file *file, int op, void *pArg) {
  return SQLITE_OK;
}

static
int little_sector_size(sqlite3_file *file) {
  return LITTLE_SECTOR_SIZE;
}

static
int little_device_characteristics(sqlite3_file *file) {
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
  little_vfs.xAccess        = orig->xAccess;
  little_vfs.xGetTempname   = orig->xGetTempname;
  little_vfs.xFullPathname  = orig->xFullPathname;
  little_vfs.xDlOpen        = orig->xDlOpen;
  little_vfs.xDlError       = orig->xDlError;
  little_vfs.xDlSym         = orig->xDlSym;
  little_vfs.xDlClose       = orig->xDlClose;
  little_vfs.xRandomness    = orig->xRandomness;
  little_vfs.xSleep         = orig->xSleep;
  little_vfs.xCurrentTime   = orig->xCurrentTime;

  little_methods.iVersion   = 1;
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

int sqlite3_extension_init() {
  return register_little_vfs(1);
}

int register_little_vfs(int makeDflt) {
  struct sqlite3_vfs *un;
  un = sqlite3_vfs_find("unix");
  if (un == NULL) return -1;
  return sqlite3_vfs_register(init_little_vfs(un), makeDflt);
}
