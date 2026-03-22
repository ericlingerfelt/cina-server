#include <stdio.h>
#include <sys/types.h>
#include <dirent.h>
#include <string.h>

#define SUCCESS       0
#define OPEN_ERROR   -1
#define READ_ERROR   -2
#define CLOSE_ERROR  -3
#define UNKNOWN       1
#define FILE          2
#define DIR           3

/* Converts a FORTRAN string to a C string.  
   The string is s, alloc_len is the amount of memory allocated for s */
#define fs2cs(s, alloc_len, len) {           \
  len = alloc_len; while (s[--len] == ' ');  \
  s[++len] = '\0'; }

/* Convert NULL terminated C string to space padded C string */
#define cs2fs(s, alloc_len, len) while (len < alloc_len) s[len++] = ' ';

/* Global variable for storing pointer to DIR
   The declaration DIR *d won't work so use an int pointer */
int *p;

int opendir_(char *path, const int path_len)
{
  int len;

  fs2cs(path, path_len, len);
  p = (int *) opendir(path);
  cs2fs(path, path_len, len);

  if (p == NULL) return OPEN_ERROR;
  return SUCCESS;
}

int read_dir_entry_(char *name, const int name_len)
{
  struct dirent *de;
  int len;

  if (p == NULL) return OPEN_ERROR;
  de = readdir(p);
  if (de == NULL) return READ_ERROR;

  strncpy(name, de->d_name, name_len);
  len = strlen(name);
  cs2fs(name, name_len, len);

  switch (de->d_type) {
  case DT_DIR: return DIR;
  case DT_REG: return FILE;
  default: return UNKNOWN;
  }
}

int closedir_()
{
  if (p == NULL) return OPEN_ERROR;
  if (closedir(p)) return CLOSE_ERROR;
  else return SUCCESS;
}


