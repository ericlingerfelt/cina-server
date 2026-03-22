/*! \file
 *
 * \verbatim
 *
 * $Author: bucknerk $
 * $Date: 2008/07/18 17:35:57 $
 * $Id: data_file_functions.c,v 1.4 2008/07/18 17:35:57 bucknerk Exp $
 *
 * $Log: data_file_functions.c,v $
 * Revision 1.4  2008/07/18 17:35:57  bucknerk
 * Changed these hopefully for last time to ensure that the file suffix is
 * correct regardless of the zone. Remember that the suffiz is the index in a
 * list of the zone numbers of a simulation. The case where only one zone is run
 * is the problem because that one is always suffix "1". Also if there are more
 * than 9 (usually 1 or 23) then 1 is "01", 2 is "02" and so on.
 *
 * Revision 1.3  2008/07/10 19:17:28  bucknerk
 * updated
 *
 * Revision 1.2  2008/07/07 19:14:52  bucknerk
 * stuff
 *
 * Revision 1.1  2008/03/20 18:52:03  bucknerk
 * new files
 *
 *
 * \endverbatim
 *
 * This file contains all of the functions needed to open and process the
 * fortran created data files from <b>xnet</b>. These are written in C for
 * right because it handles binary data more easily.

 * Note: all of the 'unformatted' fortran files from xnet consist of the
 * following.  Every fortran 'unformatted' write is a record.  Every record
 * has a 4 byte 'header' and 4 byte 'footer'. They are the same, I suppose
 * to be used as delimiters, and they contain the size in bytes of the
 * delimited record.  This means that every read from such file needs to
 * account for that additional 8 bytes and skip the first 4 before
 * processing the actual data.
 *
 * So if you know in advance what size the data chunks are you are in good
 * shape. Otherwise you need to read 4 bytes, determine the record length, then
 * read the rest of the record.  The problem with that it there is no way to
 * know from the file (no metadata) what data types are stored in the record.
 *
 * There are several 'helper' functions in this to keep the data straight and to
 * help keep the code simpler.
 *
 * THIS version of the file is based on the waiting_point file of the same name.
 * There are a couple of changes and some deletions (things not needed/used).
 */

#include"create_input.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

/*
 * This is the cross_reference between z zone number and a file suffix
 * in the simulation directory.  Ugly but then I did not write that code.
 */

static char Zone_xref[3];

/*!
 * Trims strings by removing whitespace (isspace()) from either end.
 * The return value is the pointer to the beginning of the non-whitespace
 * characters and a null '\\0' is placed at the end of the non-whitespace.
 * \param[in] s a pointer to a null terminated string (otherwise a BAD problem
 * could occur)
 * \return A pointer to the beginning of the non-whitespace portion of the
 * string or a pointer to the '\\0' terminator if no non-whitespace chars
 */
char *trim(char *s) 
{
  char *p,*r;
  p=s;
  while(*p != '\0' && isspace(*p)) p++;
  if(p=='\0') return p;
  r=p;
  while(*p != '\0') p++; // at the end of the string
  p--;
  while(isspace(*p)) p--;
  p++;
  *p='\0';
  return r;
}
/*!
 * This is for traversing a set of nodes that is basically an array of
 * single-linked lists. Keeps the caller from having to pass multiple arguments
 *  
 */
static Vnode *current_pointer = NULL;
/*!
 * function initializes the current_pointer for the traversal function
 * \param[in] xref is a pointer to a cross_reference type, an array of pointers
 * into single-linked lists
 * \return Nothing
 */
void get_next_init(IsoXref *xref) 
{
  int i;
  current_pointer=NULL;
  if(xref==NULL) return;
  for(i=0;i<=xref->max_z;i++) {
    if(xref->iso[i] != NULL) {
      current_pointer=xref->iso[i];
      break;
    }
  }
  return;
}

/*!
 * function traverses the cross-reference which is really an array of linked
 * lists.
 * \param[in] xref a pointer to a cross_reference type
 * \return A pointer to the node that coresponds to the next isotope in the
 * lists OR NULL if the input is NULL or if at the end of the lists.
 */
Vnode *get_next_iso(IsoXref *xref) 
{
  int i;
  // uninitialized or badly initialized
  if(current_pointer==NULL) return NULL;
  if(current_pointer->next != NULL) {
    current_pointer=current_pointer->next;
    return current_pointer;
  }
  for(i=current_pointer->data.z+1;i<=xref->max_z;i++) {
      current_pointer=xref->iso[i];
      if(current_pointer!=NULL) {
	return current_pointer;
      }
  }
  return NULL;
}

/*!
 * Given a cross-reference and a z,a pair, will return the string name
 * of the isotope if it exists.
 * \param[in] xref a pointer to a cross_refernce type
 * \param[in] z the number of protons in the desired isotope
 * \param[in] a the weight (protons + neutrons) of the desired isotope
 * \return A pointer the the name string of the isotope or NULL if the
 * isotope is not in the list.
char *get_iso_name(IsoXref *xref,int z,int a) 
{
  Vnode *ptr;

  if(xref==NULL) return NULL;
  if(z < xref->min_z || a < xref->min_a) return NULL;
  if(z > xref->max_z || a > xref->max_a) return NULL;
  for(ptr=xref->iso[z];ptr!=NULL;ptr=ptr->next) {
    if(ptr->data.a==a) return ptr->data.name;
  }
  return NULL;
}
 */

/*!
 * Given a cross-reference and a z,a pair, will return the node
 * of the isotope if it exists
 * \param[in] xref a pointer to a cross_refernce type
 * \param[in] z the number of protons in the desired isotope
 * \param[in] a the weight (protons + neutrons) of the desired isotope
 * \return A pointer the the node of the desired isotope or NULL if the
 * isotope is not in the list.
Vnode *get_iso_node(IsoXref *xref,int z,int a) 
{
  Vnode *ptr;

  if(xref==NULL) return NULL;
  if(z < xref->min_z || a < xref->min_a) return NULL;
  if(z > xref->max_z || a > xref->max_a) return NULL;
  for(ptr=xref->iso[z];ptr!=NULL;ptr=ptr->next) {
    if(ptr->data.a==a) return ptr;
  }
  return NULL;
}
 */
/*!
 * Just sets a variable so we don't have keep doing this for every function
 * \param[in] zone the desired zone number
 * \return An integer 1 if no error.
 * If there are any errors a -1 is returned;
 */
int set_zone_value(char *zone)
{
  if(zone == NULL) return -1;
  strcpy(Zone_xref,zone);
  return 1;
}



/*!
 * Gets the timemap from the simulation timemapN file where N is the Zone 
 * number. The mapping is step_number to time
 * \remark zone is not a static variable in this file
 * \param[in] basedir the saved simulation directory
 * \param[out] outcount the number of steps.  This is used in the main program
 * as a paranoia check to see that all the zone files have same number of steps
 * \return A pointer to a Compress Time Data structure which is effective a step
 * indexed array of doubles.
 */

CompTimeData *get_time_data(char *basedir,int *outcount)
{
  FILE *fp1;
  int i,count,rtn,size;
  char filename[256];
  TimeData tdata[50];
  Step total;
  CompTimeData *data;

  sprintf(filename,"%s/step_num%s",basedir,Zone_xref);

  fp1=fopen(filename,"r");
  if(fp1) {
    rtn=fread(&total,sizeof(Step),1,fp1);
    if(!rtn) {
      fprintf(stderr,"ERROR=Could not read last step number\n");
      fclose(fp1);
      return NULL;
    }
  }
  else {
    fprintf(stderr,"ERROR=Could not open last step number file:%s\n",strerror(errno));
    return NULL;
  }

  // the number of steps
  size=total.s + 1;

  data=(CompTimeData *)calloc(sizeof(CompTimeData),size);
  if(data==NULL) {
    fprintf(stderr,"ERROR=Allocating time?? data storage:%s\n",strerror(errno));
    return NULL;
  }
  sprintf(filename,"%s/timemap%s",basedir,Zone_xref);

  fp1=fopen(filename,"r");
  if(fp1==NULL) {
    fprintf(stderr,"ERROR=Opening timemap?? file:%s\n",strerror(errno));
    free(data);
    return NULL;
  }
  count=0;
  while( (count < size ) && (rtn=fread(tdata,sizeof(TimeData),50,fp1))  ) {
    for(i=0;count < size && i<rtn;i++,count++) {
      data[count].step=
	tdata[i].step;
      data[count].time=
	tdata[i].time;
      /*
      printf("%d: %d t9t=%0.6E  rhot=%0.6E\n",count,tdata[i].step,
	  tdata[i].temp,tdata[i].rho);
      */
    }
  }
  if(ferror(fp1)) {
      fprintf(stderr,"ERROR=Reading time data:%s\n",strerror(errno));
      free(data);
      data=NULL;
      }
  fclose(fp1);

  *outcount=count;
  return data;
}

/*!
 * These macros are for more easily freeing complex data structures
 */
#define free_flux(a,b) for(b=a; b!=NULL;b++) { free(b->data);free(b);}

/*!
 *
 * To read flux data from the simulation files.
 * Function reads in a fluxN and a fluxN_cnt file and gets information for the
 * reactions of interest for each time step. 
 * 
 * The format of the flux files is as follows:
 * flux??_cnt - the metadata file, is an integer(4 byte) step number
 * then an integer(4 byte) number-of-non-zero fluxes, and last an
 * integer(4 byte) file position (this is from the beginning of the file).
 * It corresponds to the start of the record in the flux?? file from
 * the beginning of the file (SEEK_SET).
 * flux?? - the data file consists of an integer(4 byte) step number
 * followed by a set of nnz records each of which is integer(4 byte) (reaction)
 * index then a double(8 byte).
 * The index should correspond to the reaction map index
 *
 * \remark zone is not a static variable in this file
 * \param[in] n_reactions the number of reactions in the simulation
 * \param[in] basedir location of the saved simulation
 * \param[out] max_flux the maximum of ALL the fluxes for normalizing the
 * values sent to the waiting_point computation.
 * \return An array of Flux structures of size number of timesteps, 
 * each of which have n_reactions of data (which may be 0.0), easier that
 * hassling with nnz stuff
 *
 */
Flux *get_flux_data(int n_reactions, char *basedir, double *max_flux)
{
  FILE *fp1,*fp2;
  int i,j,rtn,tmp;
  int count;
  int nread,toread;
  double max;
  Step total,mystep;
  Data *data; // for reading actual abundance from FORTRAN file
  MetaData *mydata; // reading metadata from FORTRAN file
  Flux *all_flux,*bptr;
  char file1[256];
  char file2[256];

  // This file contains the number of the last step written,
  // the first step is numbered 0. There is one for each zone in the 
  // simulation but this function just reads one at a time. 
  sprintf(file2,"%s/step_num%s",basedir,Zone_xref);

  fp1=fopen(file2,"r");
  if(fp1) {
    rtn=fread(&total,sizeof(Step),1,fp1);
    if(!rtn) {
      fprintf(stderr,"ERROR=Could not read last step number\n");
      fclose(fp1);
      return NULL;
    }
  }
  else {
    fprintf(stderr,"ERROR=Could not open last step number file:%s\n",
	strerror(errno));
    return NULL;
  }
  //printf("step_num%d: %s\n",Zone_xref,total.s);

  sprintf(file1,"%s/flux%s",basedir,Zone_xref);     // the data
  sprintf(file2,"%s/flux%s_cnt",basedir,Zone_xref); // the metadata
  
  fp1=fopen(file1,"r");
  if(fp1 == NULL) {
    fprintf(stderr,"ERROR=Opening flux data file:%s\n",strerror(errno));
    fprintf(stderr,"%s\n",file1);
    return NULL;
  }
  fp2=fopen(file2,"r");
  if(fp2 == NULL) {
    fprintf(stderr,"ERROR=Opening flux metadata file:%s\n",strerror(errno));
    fprintf(stderr,"%s\n",file2);
    return NULL;
  }
  // one for each time step, s+1 because they start at 0
  all_flux=(Flux *)calloc(sizeof(Flux),total.s+1);
  if(all_flux==NULL) {
    fprintf(stderr,"ERROR=Allocating flux data storage:%s\n",strerror(errno));
    fclose(fp1);
    fclose(fp2);
    return NULL;
  }


  mydata=(MetaData *)calloc(sizeof(MetaData),50); // metadata
  data=(Data *)calloc(sizeof(Data),500); //data
  if(mydata==NULL || data == NULL) {
    fprintf(stderr,"ERROR=Allocating temporary flux storage:%s\n",strerror(errno));
    if(data!=NULL) free(data);
    if(mydata!=NULL) free(mydata);
    free(all_flux);
    fclose(fp1);
    fclose(fp2);
    return NULL;
  }
    

  // 50 was convenient size, count is the number of steps
  // dcount is the number of non-zero elements for a partcular step.

  count=0;
  rtn=fread(mydata,sizeof(MetaData),50,fp2);
  //printf("starting step is %d\n",mydata[0].step);
  max=0.0;
  while(rtn) {
    for(i=0;i<rtn;i++) {
      all_flux[count].step=mydata[i].step;
//      all_flux[count].nnz=mydata[i].nnz;
      fseek(fp1,mydata[i].fpos,SEEK_SET);
      tmp=fread(&mystep,sizeof(Step),1,fp1);
      if(!tmp  || mystep.s != mydata[i].step) {
	fprintf(stderr,"ERROR=%s,%d: Error reading flux data file",
	    __FILE__,__LINE__);
	free_flux(all_flux,bptr);
	free(mydata);
	free(data);
	fclose(fp1);
	fclose(fp2);
	return NULL;
      }
      all_flux[count].data=(double *)calloc(sizeof(double),n_reactions);
      if(all_flux[count].data==NULL) {
	printf("ERROR=Allocating flux data storeage:%s\n",strerror(errno));
	free_flux(all_flux,bptr);
	free(mydata);
	free(data);
	fclose(fp1);
	fclose(fp2);
	return NULL;
      }
      //now read from the data file, all the non-zero entries
      nread=0,toread=0;
      while(nread < mydata[i].nnz) {
	if(nread+500 < mydata[i].nnz) toread=500;
	else toread=mydata[i].nnz-nread;
	tmp=fread(data,sizeof(Data),toread,fp1);
	if(tmp) { // actually read something
	  for(j=0;j<tmp;j++) {
	    // if it is in the range
	    //if(data[j].index >= isostart && data[j].index <=isoend)
	    if(max < data[j].data) max=data[j].data;
            all_flux[count].data[data[j].index]=data[j].data;
	  }
	}
	else {
	  if(feof(fp1)) {
	    fprintf(stderr,"ERROR=Unexpected end of flux data file\n");
	  }
	  else {
	    fprintf(stderr,"ERROR=Error reading flux data file\n");
	  }
	  free_flux(all_flux,bptr);
	  free(mydata);
	  free(data);
	  fclose(fp1);
	  fclose(fp2);
	  return NULL;
	}
	nread+=toread;
      }
    count++;
    }
  rtn=fread(mydata,sizeof(MetaData),50,fp2);
  }
  free(mydata);
  free(data);
  fclose(fp1);
  fclose(fp2);
  //printf("ending step is %d\n",all_flux[count-1].step);
  *max_flux=max;
  return all_flux;
}// end of just handling the flux.

/* 
 * Reads in the isotope data.
 * The format of this file is fairly simple but makes a problem with C and
 * structures. Fortunately fread buffers the read so that there is not
 * actually a system call each time.  The record consists of 13 bytes -
 * the index (int 4 bytes) then Z (short 2 bytes) then A (short 2 bytes)
 * then 5 characters that are the name of the isotope. The fortran 4
 * trailing bytes is the issue because of padding. If you call it an int
 * then there are 3 bytes of internal padding between the characters and
 * the integer to align the int on the boundary.  If you call the last 9
 * bytes a float then there are 3 OR 7 bytes of padding depending on whether the
 * compiler wants it to start on a 4 byte or an 8 byte boundary.  Either
 * way, things get messed up when fread transfers n bytes to some memory
 * location.
 * Should only have to ever call this once, because these don't change on a
 * per zone basis.
 *
 * \param[in] basedir the directory of the saved simulation, and we will use
 * the \tdl{data} subdirectory of that
 * \param[out] outcount the number of isotopes returned
 * \return A pointer to a cross_reference structure, this contains values for
 * min and max Z and min and max A.
 *
 */
#define ISOBYTES 21  //number of bytes in a record in the isomap file

IsoXref *get_isotope_data(char *basedir, int *outcount)
{
  FILE *fp1;
  struct stat buf;
  int i,rtn,count,Z_end;
  //int first,last;
  int j;
  char path[256];
  IsoData idata;
  CompIsoData *data;
  Vnode *tmpiso,*vptr;
  IsoXref *xref;


  path[0]='\0';
  strcat(path,basedir);
  strcat(path,"/isomap");

  rtn=stat(path,&buf);
  if(rtn != 0) {
    fprintf(stderr,"ERROR=Isomap size: %s: %s\n",path,strerror(errno));
    return NULL;
  }
  data=(CompIsoData *)calloc(sizeof(CompIsoData),buf.st_size/ISOBYTES);
  if(data==NULL) {
    fprintf(stderr,"ERROR=Allocating isotope data storage:%s\n",
	strerror(errno));
    fprintf(stderr,"%d: %ld\n",__LINE__,buf.st_size/ISOBYTES);
    return NULL;
  }


  fp1=fopen(path,"r");
  if(fp1==NULL) {
    fprintf(stderr,"ERROR=Opening isomap file:%s\n",strerror(errno));
    free(data);
    return NULL;
  }

  count=0;
  Z_end=0;
  // this read is weird because of the padding the C compiler
  // puts in.  I can't just move stuff around because of the file format
  // read 17 and 4 bytes in separate operations and 'stick' it in the 
  // the data structure at the appropriate places. 
  while((rtn=fread((void *)&idata,1,17,fp1)) && 
      (rtn=fread((void *)&(idata.s2),1,4,fp1))){
      //if(strcmp(trim(idata.name),"al-6")==0) continue;
      //if(strcmp(trim(idata.name),"al*6")==0) continue;
      // should give me al26
      data[count].index=idata.index;
      data[count].z=idata.z;
      if(Z_end<idata.z) Z_end=idata.z;
      data[count].a=idata.a;
      // the data[n].name is 6 characters, idata.name is 5
      // if the charater in the input is non-displayable it won't be copied
      // otherwise it will be.  This should result in trimming the input string 
      // and ensuring that the result is '\0' terminated.
      for(i=0,j=0;i<5;i++) {
	if(isgraph(idata.name[i])) {
          data[count].name[j]=idata.name[i];
	  j++;
	}
      }
      count++;
  }
  fclose(fp1);

  xref=(IsoXref *)calloc(sizeof(IsoXref),1);
  if(xref==NULL) {
    fprintf(stderr,"ERROR=Allocating isotope xref data storage:%s\n",strerror(errno));
    fprintf(stderr,"%d: %d\n",__LINE__,count);
    free(data);
    return NULL;
  }
  // this should allocate at least the correct number
  xref->iso=(Vnode **)calloc(sizeof(Vnode *),Z_end+1);
  if(xref->iso==NULL) {
    fprintf(stderr,"ERROR=Allocating isotope xref vector storage:%s\n",strerror(errno));
    fprintf(stderr,"%d: %d\n",__LINE__,count);
    free(data);
    free(xref);
    return NULL;
  }
  xref->min_z=1000;
  xref->min_n=1000;
  xref->min_a=1000;
  xref->max_z=Z_end;
  xref->max_n=0;
  xref->max_a=0;

  for(i=0,j=0;i<count;i++) {
    if(xref->min_z > data[i].z) xref->min_z=data[i].z;
    if(xref->min_a > data[i].a) xref->min_a=data[i].a;
    if(xref->min_n > data[i].a - data[i].z) xref->min_n=data[i].a - data[i].z;
    if(xref->max_a < data[i].a) xref->max_a=data[i].a;
    if(xref->max_n < data[i].a - data[i].z) xref->max_n=data[i].a - data[i].z;
    if(xref->iso[data[i].z] == NULL) {
      xref->iso[data[i].z]=(Vnode *)calloc(sizeof(Vnode),1);
      memcpy(&xref->iso[data[i].z]->data,&data[i],sizeof(CompIsoData));
    }
    else {
      tmpiso=(Vnode *)calloc(sizeof(Vnode),1);
      memcpy(&tmpiso->data,&data[i],sizeof(CompIsoData));
      // put them in in order
      vptr=xref->iso[data[i].z];
      while(vptr->next != NULL) vptr=vptr->next;
      vptr->next=tmpiso;
    }
  }
  free(data);
  /*
  for(i=0;i<count;i++) {
    printf("%d: %hd,%hd %s\n",i,rdata[i].z, rdata[i].a,rdata[i].name);
  }
  */

  *outcount=count;
  return xref;
}
/**************************************************************************/

/*!
 *
 * This one, thermo??, is the thermo mapping for each zone and consists of
 * an integer step number, a double temperature and a double that is the
 * density (g/cc). 
 *
 * Apparently something is not kosher with xnet/save_em_sim or somewhere.
 * The test I am using from a recent simulation has incorrect information
 * in it. I has all the time steps but then it has additional timesteps.  I
 * am guessing that if you do not restart the entire simulation, it reuses
 * any existing files in the users temp area and writes over the top of them
 * instead of unlinking them first.  BADDDDDDDDD!!!!!!! That is why I have
 * to read the step_num file instead of just figuring out how many by the
 * file size.  
 * \remark zone is not a static variable in this file
 * \param[in] basedir the location of the saved simulation
 * \param[out] outcount the size of the array that is returned.
 * \return An array of CTD (compressed temp data) structures of size 
 * <i>outcount</i>
 */

CompThermData *get_thermo_data(char *basedir,int *outcount)
{
  FILE *fp1;
  int i,count,rtn,size;
  char filename[256];
  ThermData tdata[50];
  Step total;
  CompThermData *data;

  sprintf(filename,"%s/step_num%s",basedir,Zone_xref);

  fp1=fopen(filename,"r");
  if(fp1) {
    rtn=fread(&total,sizeof(Step),1,fp1);
    if(!rtn) {
      fprintf(stderr,"ERROR=Could not read last step number\n");
      fclose(fp1);
      return NULL;
    }
  }
  else {
    fprintf(stderr,"ERROR=Could not open last step number file:%s\n",strerror(errno));
    return NULL;
  }

  size=total.s + 1;

  data=(CompThermData *)calloc(sizeof(CompThermData),size);
  if(data==NULL) {
    fprintf(stderr,"ERROR=Allocating thermo?? data storage:%s\n",strerror(errno));
    return NULL;
  }
  sprintf(filename,"%s/thermo%s",basedir,Zone_xref);

  fp1=fopen(filename,"r");
  if(fp1==NULL) {
    fprintf(stderr,"ERROR=Opening thermo?? file:%s\n",strerror(errno));
    free(data);
    return NULL;
  }
  count=0;
  while( (count < size ) && (rtn=fread(tdata,sizeof(ThermData),50,fp1))  ) {
    for(i=0;count < size && i<rtn;i++,count++) {
      data[count].step=
	tdata[i].step;
      data[count].temp=
	tdata[i].temp;
      data[count].rho=
	tdata[i].rho;
      //printf("%d: %d t9t=%0.6E  rhot=%0.6E\n",count,tdata[i].step,
      //  tdata[i].temp,tdata[i].rho);
    }
  }
  if(ferror(fp1)) {
      fprintf(stderr,"ERROR=Reading thermo data:%s\n",strerror(errno));
      free(data);
      data=NULL;
      }
  fclose(fp1);

  *outcount=count;
  //printf("THERM: start = %d: end = %d\n",data[0].step,data[count-1].step);
  return data;
}

/*!
 * only used by the get_reaction_data() function, used to read in an unknown
 * number of elements and store them in a nice list. Then place them in the xref
 * and free the structures.
 */
struct react {
  int count;
  Reaction *reactions[10];
  struct react *next;
};
/*!
 * Used to free the unused Reaction structures when the get_reaction_data()
 * function returns
 * \param[in] r is a pointer to a Reaction-type struct
 * \return None
 ***********************************************************************/
void free_reaction(Reaction *r)
{
  int i;
  if(r==NULL) return;
  if(r->type !=NULL) free(r->type);
  if(r->str !=NULL) free(r->str);
  if(r->iso !=NULL) free(r->iso);
  if(r->decay !=NULL) free(r->decay);
  for(i=0;i<r->n;i++) 
    if(r->rates[i] !=NULL) free(r->rates[i]);
  return;
}
/*!
 * This function reads the reaction rates that are assumed to be in the base
 * directory /data/netsu file.  It uses another program, netsu.php, that
 * preprocesses the netsu file because php does string handling much better than
 * C. It ensures that the input is nicely formatted and as you can see I handle
 * that with a switch statement.  Once the reaction is received, It is placed in
 * the xref according to its type and structure.  The type 1,2,4,5 are the
 * creation and destruction reactions that only focus on the isotope of interest
 * and p,alpha,gamma,or bet+(ec,bet,btyk) decay
 *
 * \param[in] basedir the location of the simulation data
 * \param[in,out] xref this holds the isotopes, 
 * \output xref is modified so that we fill it with the reactions of
 * interest that correspond to an isotope
 * \return non-zero if there was any error, otherwise 0.  
 * The return values have no mapping to any particular error.  
 *
 ***********************************************************************/
int get_reaction_data(char *basedir,IsoXref *xref)
{
  struct react *Head=NULL,*curr_node;
  int field,count,total_reactions;
  int i,k,done,rcount;
  Reaction *current;
  char *rates[10];
  char buf[256];
  char command[256];
  char *crtn;
  Vnode *vptr;
  FILE *fp;

  sprintf(command,"./netsu.php %s",basedir);
  fp=popen(command,"r");
  if(fp==NULL) {
    fprintf(stderr,"ERROR=using netsu.php:%s\n",strerror(errno));
    return 1;
  }
  Head=(struct react *)calloc(sizeof(struct react),1);
  if(Head == NULL) {
    fprintf(stderr,"ERROR=allocting reaction head:%s\n",strerror(errno));
    return 1;
  }
  for(i=0;i<10;i++)rates[i]=NULL;
  curr_node=Head;
  crtn=fgets(buf,256,fp);
  total_reactions=0;
  rcount=0;
  while(!feof(fp)) {
    //printf("%s",buf);
    //end=strlen(buf);
    //if(buf[end-1] == '\n') buf[end-1]='\0';
    if(strlen(trim(buf)) ==0) {
      current=(Reaction *)calloc(sizeof(Reaction),1);
      if(current==NULL) {
	// need to free memory
	printf("ERROR=Allocating a new reaction:%s\n",strerror(errno));
	return 1;
      }
      current->count=rcount;
      rcount++;
      field=0;
    }
    else {
      switch(field) {
	case 0:
	  current->type=(char *)strdup(trim(buf));
	  if(errno) {
	    fprintf(stderr,"ERROR=Errno=%d: %d: line %d\n",
		errno,total_reactions,__LINE__);
	    return 1;
	  }
	  field++;
	  break;
	case 1:
	  current->str=(char *)strdup(trim(buf));
	  if(errno) {
	    fprintf(stderr,"ERROR=Errno=%d: %d: line %d\n",
		errno,total_reactions,__LINE__);
	    return 1;
	  }
	  field++;
	  break;
	case 2:
	  current->iso=(char *)strdup(trim(buf));
	  if(errno) {
	    fprintf(stderr,"ERROR=Errno=%d: %d: line %d\n",
		errno,total_reactions,__LINE__);
	    return 1;
	  }
	  field++;
	  break;
	case 3:
	  current->Q=strtod(trim(buf),NULL);
	  field++;
	  break;
	case 4:
	  current->decay=(char *)strdup(trim(buf));
	  if(errno) {
	    fprintf(stderr,"ERROR=Errno=%d: %d: line %d\n",
		errno,total_reactions,__LINE__);
	    return 1;
	  }
	  field++;
	  break;
	case 5:
	  count=0;
	  while( strlen(trim(buf)) > 0) {
	    rates[count++]=(char *)strdup(trim(buf));
	    if(errno) {
	      fprintf(stderr,"ERROR=Errno=%d: %d: line %d\n",
		  errno,total_reactions,__LINE__);
	      return 1;
	    }
	    crtn=fgets(buf,256,fp);
            if(feof(fp)) break;
	    //printf("%s",buf);
	  }
	  current->n=count;
	  current->rates=(char **)calloc(sizeof(char *),count);
	  if(current->rates==NULL) {
	    fprintf(stderr,"ERROR=Allocating rate array:%s\n",strerror(errno));
	    return 1;
	  }
	  for(i=0;i<count;i++) {
	    current->rates[i]=rates[i];
	    rates[i]=NULL;
	  }
	  if(curr_node->count==10) {
	    curr_node->next=(struct react *)calloc(sizeof(struct react),1);
	    if(curr_node->next==NULL) {
	      fprintf(stderr,"ERROR=Allocating another react struct:%s\n",
		  strerror(errno));
	      return 1;
	    }
	    curr_node=curr_node->next;
	  }
	  curr_node->reactions[curr_node->count]=current;
	  curr_node->count++;
	  total_reactions++;
	  // this causes the file read to be skipped and the
	  // next reaction started with this buffer.
	  continue;
	  break;
      }// end of switch
    }// end of else
  crtn=fgets(buf,256,fp);
  }// end of while
  fclose(fp);
  for(curr_node=Head;curr_node!=NULL;curr_node=curr_node->next) {
    for(k=0;k<curr_node->count;k++) {
      current=curr_node->reactions[k];
      done=0;
      for(i=0;i<=xref->max_z;i++) {
	vptr=xref->iso[i];
	while(vptr!=NULL) {
	  if((strcmp(vptr->data.name,current->iso))==0) {
	    switch(atoi(current->type)) {
	      case 1:
		vptr->data.reactions[0]=current;
		curr_node->reactions[k]=NULL;
		break;
	      case 2:
		if(strcmp(current->type,"2p") ==0) {
		  vptr->data.reactions[5]=current;
		}
		else {
		  vptr->data.reactions[6]=current;
		}
		curr_node->reactions[k]=NULL;
		break;
	      case 4:
		if(strcmp(current->type,"4p") ==0) {
		  vptr->data.reactions[1]=current;
		}
		else {
		  vptr->data.reactions[3]=current;
		}
		curr_node->reactions[k]=NULL;
		break;
	      case 5:
		if(strcmp(current->type,"5p") ==0) {
		  vptr->data.reactions[2]=current;
		}
		else {
		  vptr->data.reactions[4]=current;
		}
		curr_node->reactions[k]=NULL;
		break;
	    } // end of switch
	  done=1;
	  break; // should break out of the while
	  }
	vptr=vptr->next;
	} // end of while
      if(done) break; // break out of the for Z
      } // end of for Z
    } // end of current node
  } // done
  while(Head != NULL) {
    curr_node=Head->next;
    for(i=0;i<Head->count;i++) {
      if(Head->reactions[i] != NULL) {
	free_reaction(Head->reactions[i]);
	free(Head->reactions[i]);
      }
    }
    free(Head);
    Head=curr_node;
  }
  return 0;
}
/*!
 * This processes the reaction map file (only one, similar to the isotope map). 
 * The information is needed because it is the correspondence between the flux
 * values, the isotopes and the reactions.  The map entries are standard 8-byte
 * padded FORTRAN records that consist of an index value, unique to a reaction,
 * an array of 7 integers that represent the isotopes in the reaction, the first
 * three are the left-hand side and the other four are the right-hand side. If
 * the value is -1 then there is no isotope in that position.  There is also a
 * set of 4 bytes (characters) that is the reference string (NOT '\\0'
 * terminated) and another integer which currently has value 1 if the reaction
 * is an ec, 4 if any beta, and 0 otherwise.  The 1,4 reactions are considered
 * "weak" reactions and even though this flag is not actively used right now it
 * may be in the future.
 *
 * So this function reads the end of the file to determine the maximum index
 * number, allocates an array of structures to store the reaction (the refernece
 * is now 5 bytes so the string will be null terminated), reads the reactions
 * and stores the data.  Pretty simple.
 *
 * \param[in] basedir the location of the saved simulation
 * \param[out] outcount the count of the number of reactions.
 * \return A pointer to a reaction_map OR NULL on error.
 *
 ***************************************************************************/

ReacMap *get_reaction_map(char *basedir,int *outcount)
{
  int count,i,j,rtn;
  FILE *fp;
  char filename[256];
  Reaction_Map_Data data;
  ReacMap *map;

  sprintf(filename,"%s/reacmap",basedir);
  fp=fopen(filename,"r");
  if(!fp) {
    fprintf(stderr,"ERROR=Opening reaction map file:%s\n",strerror(errno));
    return NULL;
  }
  rtn=fseek(fp,-(sizeof(Reaction_Map_Data)),SEEK_END);
  if(rtn==-1) {
    fprintf(stderr,"ERROR=Seeking last record of reaction map file:%s\n",
	strerror(errno));
    fclose(fp);
    return NULL;
  }
  rtn=fread(&data,1,sizeof(Reaction_Map_Data),fp);
  if(rtn != sizeof(Reaction_Map_Data)) {
    if(feof(fp)) {
      fprintf(stderr,"ERROR=Got end of file instead of reactionn record\n");
      fclose(fp);
      return NULL;
    }
    else {
      fprintf(stderr,"ERROR=Reading last reaction map record:%s\n",
	  strerror(errno));
      fclose(fp);
      return NULL;
    }
  }

  map=(ReacMap *)calloc(sizeof(ReacMap),data.index+1);
  if(map == NULL) {
    fprintf(stderr,"ERROR=Allocating reaction map:%s\n",strerror(errno));
    fclose(fp);
    return NULL;
  }

  errno=0;
  rewind(fp);
  if(errno) {
    fprintf(stderr,"ERROR=Rewinding reaction file:%s\n",strerror(errno));
    fclose(fp);
    return NULL;
  }

  rtn=fread(&data,1,sizeof(Reaction_Map_Data),fp);
  if(rtn != sizeof(Reaction_Map_Data)) {
    if(feof(fp)) {
      fprintf(stderr,"ERROR=Got end of file instead of reaction record\n");
      fclose(fp);
      return NULL;
    }
    else {
      fprintf(stderr,"ERROR=Reading first reaction map record:%s\n",strerror(errno));
      fclose(fp);
      return NULL;
    }
  }

  count=0;
  while(!feof(fp) ) {
    for(i=0;i<7;i++) {
      map[data.index].isotopes[i]=data.isotopes[i];
    }
    // copies the reference, and trims off whitespace which is 
    // by definition NOT a 'visable' graphics character (maybe)
    for(i=0,j=0;i<4;i++) {
      if(isgraph(data.ref[i])) {
	  map[data.index].ref[j]=data.ref[i];
	  j++;
	}
      }
    map[data.index].flag=data.flag;
    count++;
    rtn=fread(&data,1,sizeof(Reaction_Map_Data),fp);
    if(rtn != sizeof(Reaction_Map_Data)) {
      if(ferror(fp)) {
	printf("ERROR=Reading a reaction map record:%s\n",strerror(errno));
	fclose(fp);
	free(map);
	return NULL;
      }
    }
  }// end of while fread

  fclose(fp);
  *outcount=count;
  return map;
}
