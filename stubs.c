#include "sqlite3.h"
#include "sqlite3-local.h"
#include <stdlib.h>
#include <string.h>

void
sqlite3_set_temp_directory(char* x)
{
    sqlite3_temp_directory = (char*)sqlite3_malloc(strlen(x)+1);
    if (sqlite3_temp_directory) {
	strcpy(sqlite3_temp_directory,x);
    }
}

char *
sqlite3_get_temp_directory(void)
{
    return sqlite3_temp_directory;
}

sqlite3_destructor_type
get_SQLITE_STATIC() 
{ return SQLITE_STATIC; }

sqlite3_destructor_type
get_SQLITE_TRANSIENT()
{ return SQLITE_TRANSIENT; }
