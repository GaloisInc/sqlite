#ifndef __SQLITE3_LOCAL_H
#define __SQLITE3_LOCAL_H
#include "sqlite3.h"

extern void sqlite3_set_temp_directory(char* x);
extern char *sqlite3_get_temp_directory();

extern sqlite3_destructor_type get_SQLITE_STATIC();
extern sqlite3_destructor_type get_SQLITE_TRANSIENT();

#endif
