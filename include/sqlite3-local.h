#ifndef __SQLITE3_LOCAL_H
#define __SQLITE3_LOCAL_H
#include "sqlite3.h"

extern void sqlite3_set_temp_directory(char* x);
extern char *sqlite3_get_temp_directory();

extern sqlite3_destructor_type get_SQLITE_STATIC();
extern sqlite3_destructor_type get_SQLITE_TRANSIENT();

typedef struct my_sqlite3_file my_sqlite3_file;
struct my_sqlite3_file {
  sqlite3_file base_file;
};
#endif
