/*
 * this file is designed to only read the "step_numNN" files that are part of
 * the newer simulations. They store the last step for a Zone NN. The reason
 * this was even made was to use with a UI to create input files for the
 * bottle_neck finder "off-line".
 * The files were create by fortran and it always writes a "record". The record
 * consists of 4 bytes, an integer indicating how many bytes in a record, the
 * "record data" number of bytes and a last 4 bytes the same as the first 4
 * bytes. You have to know what datatype(s) the data contains to correctly
 * decode it. In this case the file contains 3 integers, the first and the last
 * are 4 and the middle one the number of steps for that zone.
 */
#include<stdio.h>
#include<unistd.h>

int main(int argc, char **argv)
{
  FILE *fp;
  char buf[12];
  int i;

  for(i=1;i<argc;i++) {
    fp=fopen(argv[i],"r");
    if(fp != NULL ) {
      fread(buf,1,12,fp);
      fclose(fp);
      //printf("%d\n",*(int *)(buf));
      printf("%d\n",*(int *)(buf+4));
      //printf("%d\n",*(int *)(buf+8));
    }
    else {
      perror("Reading 'last step' file");
    }
  }
  return 0;
}

       


