#include<stdio.h>
#include<unistd.h>
#include<sys/stat.h>
#include "create_input.h"


int main(int argc, char **argv)
{
  FILE *fp1;
  struct stat buf;
  int rtn,mass_min,mass_max;
  int z_min,z_max,n_min,n_max;
  int iso;
  IsoData idata;


  if(argc < 2 || argc > 3) {
    fprintf(stderr,"Usage: read_masses filename [i]\n");
    return 1;
  }

  rtn=stat(argv[1],&buf);
  if(rtn != 0) {
    fprintf(stderr,"ERROR: Isomap size: %s: %s\n",argv[1],strerror(errno));
    return 1;
  }

  fp1=fopen(argv[1],"r");
  if(fp1==NULL) {
    fprintf(stderr,"ERROR= Opening isomap file:%s\n",strerror(errno));
    return 1;
  }
  if(argc==3 && (strcmp(argv[2],"i") == 0)) {
    iso=1;
  }
  else {
    iso=0;
  }

  mass_min=999;
  z_min=999;
  n_min=999;
  mass_max=0;
  z_max=0;
  n_max=0;
  // this read is weird because of the padding the C compiler
  // puts in.  I can't just move stuff around because of the file format
  // read 17 and 4 bytes in separate operations and 'stick' it in the 
  // the data structure at the appropriate places. 
  //while((rtn=fread((void *)&idata,1,17,fp1)) && 
   // (rtn=fread((void *)&(idata.s2),1,4,fp1))){
  while((rtn=fread((void *)&idata,1,21,fp1))) {
    if(!iso) {
      if(mass_min > idata.a) mass_min=idata.a;
      if(mass_max < idata.a) mass_max=idata.a;
    }
    else {
      if(z_min > idata.z) z_min=idata.z;
      if(z_max < idata.z) z_max=idata.z;
      if(n_min > (idata.a-idata.z)) n_min=(idata.a-idata.z);
      if(n_max < (idata.a-idata.z)) n_max=(idata.a-idata.z);
    }
  }

  fclose(fp1);

  if(iso) {
    printf("%d,%d %d,%d",z_min,n_min,z_max,n_max);
  }
  else {
    printf("%d,%d",mass_min,mass_max);
  }
  return 0;
}
