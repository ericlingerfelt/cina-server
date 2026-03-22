#include"create_input.h"
#include"rb.h"

/*! \file
 *
 * \verbatim
 *
 * $Author: bucknerk $
 * $Date: 2008/07/18 17:35:57 $
 * $Id: create_input_main.c,v 1.5 2008/07/18 17:35:57 bucknerk Exp $
 *
 * $Log: create_input_main.c,v $
 * Revision 1.5  2008/07/18 17:35:57  bucknerk
 * Changed these hopefully for last time to ensure that the file suffix is
 * correct regardless of the zone. Remember that the suffiz is the index in a
 * list of the zone numbers of a simulation. The case where only one zone is run
 * is the problem because that one is always suffix "1". Also if there are more
 * than 9 (usually 1 or 23) then 1 is "01", 2 is "02" and so on.
 *
 * Revision 1.4  2008/07/10 19:57:02  bucknerk
 * fixed comments for last change
 *
 * Revision 1.3  2008/07/10 19:48:29  bucknerk
 * changed the format of the file header
 *
 * Revision 1.2  2008/07/10 19:17:28  bucknerk
 * updated
 *
 * Revision 1.1  2008/07/07 19:02:58  bucknerk
 * new names for the bottle_neck input creation program source
 *
 * Revision 1.1  2008/06/13 15:30:14  bucknerk
 * renamed from new_input.c
 *
 * Revision 1.1  2008/03/25 18:10:39  bucknerk
 * new
 *
 * Revision 1.1  2008/03/20 18:52:03  bucknerk
 * new files
 *
 *
 *
 * \endverbatim
 * ********************************
 * Bottle necks are a little different from waiting points in that they don't
 * really need as much data to identify.  We will take a simulation, (and
 * eventually a min/max A I think) and then gather for each A,Z the (p,g), (a,p)
 * and (a,g) fluxes for that isotope.  The output will consist of;
 * <ul>
 * <li> A header line containing
 * <ul>
 * <li> number of nuclei (integer)
 * <li> max Z (integer)
 * <li> max A (integer)
 * <li> number of time steps (integer)
 * <li> largest mass number (integer)
 * <li> newline
 * </ul>
 * <li> then for each time step
 * <ul>
 * <li> the step number (integer)
 * <li> the time (double)
 * <li> newline
 * </ul>
 * <li> then for each A for that time step
 * <ul>
 * <li> A (integer)
 * <li> a count of the isotopes for that A (integer)
 * <li> newline
 * <li> then for each isotope
 * <ul> 
 * <li> the Z (integer)
 * <li> (p,g) flux (double)
 * <li> (a,p) flux (double)
 * <li> (a,g) flux (double)
 * <li> newline
 * </ul>
 * </ul>
 * </ul>
 *
 *
 * Now this file is similar to create_bn_input.c but acquires a different set of
 * reactions. These are what I am laughingly calling the 'reverse' set. Instead
 * of starting from a Z,N and getting the (a,p),(a,g),(p,g) reactions like 
 * Z,N -&gt; Z+1,N we get the set like Z-1,N -&gt; Z,N .
 *************************************************************************/

/************************************************************************/
/*!
 * A function to pass the rb_tree code to compare the arrays of integers.
 * Rb_trees require a function with 2 arguments and an integer return like
 * strcmp().  Memcmp requires 3 arguments thus this function.
 ************************************************************************/
int memcmp_map(char *a,char *b)
{
  return memcmp(a,b,7*sizeof(int));
}

/************************************************************************/
/*! 
 * The main function: Will keep the same arguments as the waiting_point version
 * used.
 *
 * \param[in] argc argument count
 * \param[in] argv argument array
 *
 * \input argv[1] simulation base directory
 * \input argv[2] beginning isotope Z value
 * \input argv[3] beginning isotope N value
 * \input argv[4] ending isotope Z value
 * \input argv[5] ending isotope N value
 * \input argv[5] beginning time step
 * \input argv[6] ending time step
 * \input argv[7] zone
 *
 * \output A file containing the processed data for the waiting_point code.
 ************************************************************************/

int main(int argc, char **argv)
{
  int i,j,k,l;
  int nucount,count;
  int rtn;
  char *zone;
  int reacbuf[7]; // for the isotope indices for a reaction
  int Step_start,Step_end; // start and end time steps from args
  int isocount,timecount; // rtns from functions
  int reaction_count;
  int A_start,A_end; // from args 
  // the indices of the isotopes p, 4He, start Z,A and end Z,A
  int p_index,a_index,start_index,end_index; 
  int max_count; // used to see what the max Z really is, actually ignored.
  int *A_counts; // that's 100 isotopes
  double *flux; // that's 100 isotopes at 3 fluxes per.
  double norm_time; // the current normalized time
  double flux_norm_factor; // the largest possible flux value this simulation
  char directory[512];
  char *tmp_ptr;
  CompIsoData ***iso_data=NULL,*current=NULL;
  CompIsoData *from=NULL,*to=NULL;
  CompTimeData *time_data=NULL,base_time;
  Flux *flux_data=NULL;
  ReacMap *reaction_map=NULL;
  Vnode *vptr=NULL;
  IsoXref *xref=NULL;
  Rb_node tree,node;
  
  if(argc != 7) {
   // fprintf(stderr, "ERROR=Usage: create_input basedir Zstart Nstart Zend Nend Stepstart Stepend zone\n");
    fprintf(stderr, "ERROR=Usage: create_input basedir Astart Aend Stepstart Stepend zone\n");
    for(i=0;i<argc;i++) {
      fprintf(stderr,"%d ::= %s\n",i,argv[i]);
    }
    return 1;
  }
  Step_start=atoi(argv[4]);
  Step_end=atoi(argv[5]);
  zone=argv[6];
  A_start=atoi(argv[2]);
  A_end=atoi(argv[3]);

  // set the path to the sub-directory
  sprintf(directory,"%s/data",argv[1]);

  // Initialize the zone-to-suffix cross-reference for this zone

  rtn=set_zone_value(zone);
  if(rtn<0) {
    fprintf(stderr,"ERROR=Could not get zone suffix information:%s\n",zone);
    return 1;
  }

  // Next get the reactions,abundances,times, and temps
 

  xref= get_isotope_data(argv[1], &isocount);
  if(xref==NULL) {
    fprintf(stderr,"ERROR=Could not get isotope data\n");
    return 1;
  }
  // have to print out the total number of isotopes in simulation
  // this is the number from the starting pair to the ending pair
  // Actually Michael seems to think it is only really Z that the N
  // is pretty much ignored.  And that's fine.

  // print out the total number of isotopes(-2 because we leave out p and n)
  // first count them, then see if n and p are there because we don't 
  // count those two
  iso_data=(CompIsoData ***)calloc(sizeof(CompIsoData **),xref->max_z+1);
  if(iso_data==NULL) {
    fprintf(stderr,"ERROR=Allocating iso_data:%s\n",strerror(errno));
    return(1);
  }
  for(i=0;i<=xref->max_z;i++) {
    iso_data[i]=(CompIsoData **)calloc(sizeof(CompIsoData *),xref->max_a+1);
    if(iso_data[i]==NULL) {
      fprintf(stderr,"ERROR=Allocating iso_data *array:%s\n",strerror(errno));
      return(1);
    }
  }
  if(A_start<xref->min_a) A_start=xref->min_a;
  if(A_end>xref->max_a) A_end=xref->max_a;
  

  get_next_init(xref);
  vptr=get_next_iso(xref);
  a_index=p_index=-1;
  start_index=end_index=-1;
  nucount=0;
  while(vptr!=NULL) {
    if(vptr->data.z==1 && vptr->data.a==1) p_index=vptr->data.index;
    if(vptr->data.z==2 && vptr->data.a==4) a_index=vptr->data.index;
 //   if (vptr->data.z >= Z_start && vptr->data.z <= Z_end ) {
      // this gets the first index and the last
      if(vptr->data.a >=A_start && vptr->data.a <=A_end) {
	if(start_index < 0) { // get the first
	  start_index=vptr->data.index;
	}
	// always assign and this is the last in the range
	end_index=vptr->data.index;
	if(vptr->data.a > 1) {
	  if(vptr->data.z==13 && vptr->data.a==26) {
	    // not 0 if NOT equal to Al*6
	    if(strcmp(vptr->data.name,"Al*6")) {
	      nucount++;
	      //fprintf(stderr,"%s,%d: %d %d\n",vptr->data.name,count,
	      //	  vptr->data.z,vptr->data.a);
	    }
	  }
	  else {
	    nucount++;
	    //fprintf(stderr,"%s,%d: %d %d\n",vptr->data.name,count,
	        //vptr->data.z,vptr->data.a);
	  }
	}
      }
      iso_data[vptr->data.z][vptr->data.a]=&vptr->data;
  //  }
    vptr=get_next_iso(xref);
  }
  
  

  
  rtn=get_reaction_data(directory,xref);
  if(rtn > 0) {
    fprintf(stderr,"ERROR:Could not get reaction data\n");
    return 1;
  }

  time_data=get_time_data(argv[1],&timecount);
  if(time_data==NULL) {
    fprintf(stderr,"ERROR:Could not get time data\n");
    return(1);
  }
  reaction_map=get_reaction_map(argv[1],&reaction_count);
  if(reaction_map==NULL) {
    fprintf(stderr,"ERROR:Could not get reaction map data\n");
    return(1);
  }

  tree=make_rb();
  if(tree==NULL) {
    fprintf(stderr,"ERROR:Allocating new rb tree:%s\n",strerror(errno));
    return(1);
  }

  // Using Jim Planks rb_tree code to create a rapidly searchable structure that
  // won't take up too much space.
  for(i=0;i<reaction_count;i++) {
    tmp_ptr=(char *)calloc(sizeof(int),7);
    if(tmp_ptr == NULL) {
      fprintf(stderr,"ERROR:Allocating rbtree key:%s\n",strerror(errno));
      return(1);
    }
    memcpy(tmp_ptr,reaction_map[i].isotopes,7*sizeof(int));
    rb_insertg(tree,tmp_ptr,(char *)i,memcmp_map);
  }
  /*
   * This is potentially a fairly large structure.  There will be a set of
   * entries for each timestep.  That set will be an array of doubles 
   * the size of the number of reactions. For instance 2000 steps, 300 reactions
   * means 4,800,000 bytes.  Not that there is a REAL better way.  If storage is
   * problem then I will drop to an nnz value and only just the correct number
   * of reactions. Problem is that I then also have to store an int with each
   * double. So what I should really do then is modify the reaction to store an
   * array of doubles of the size of the number of timesteps. But then I am back
   * to the about the same storage requirement.  Ah well, we'll see.
   */
  flux_data=get_flux_data(reaction_count,argv[1],&flux_norm_factor);
  if(flux_data==NULL) {
    fprintf(stderr,"ERROR: Could not get flux data\n");
    return(1);
  }

  // print out total number of timesteps selected
  //fprintf(stdout,"REAL_COUNT: %d\n",timecount);
  if(Step_start< 0) Step_start=0;
  if(Step_end > timecount || Step_end < Step_start) Step_end=timecount;
  printf("%4d\n",Step_start);// starting step from the gui
  printf("%4d\n",Step_end);// stopping step from the gui
  printf("%4d\n",Step_end-Step_start+1); // number of steps
  printf("%4d\n",A_start); // start A from gui, 
  printf("%4d\n",A_end); // stop A from gui,
  printf("%4d\n",xref->max_a); // max A from simulation
  flux=(double *)calloc(sizeof(double),(Step_end-Step_start+1)*3);
  if(flux==NULL) {
    perror("Allocating flux storage");
    exit(1);
  }

  // so we can "normalize" the times for each step
  base_time=time_data[0];

  for(k=Step_start;k<=Step_end;k++) { // really the number of time steps
    norm_time=time_data[k].time-base_time.time; 
    //printf("%4d %19.12E\n",k,norm_time);
    printf("%19.12E\n",norm_time);
  }
    // the idea is loop over all the isotopes. 
    // the important thing here is A, because the computation
    // will be across all isotopes with the same weight.
  A_counts=(int *)calloc(sizeof(int),xref->max_a+1);
  if(A_counts == NULL) {
    exit(1);
  }
  // need to know how many isotopes for a particular weight
  // the 3 continue statements skip n, p, and any 'missing' ones
  for(j=A_start;j<=A_end;j++) {
    for(l=1;l<=xref->max_z;l++) {
      current=iso_data[l][j];
      if(current==NULL) continue;
      if(current->z == 1  && current->a==1) {
	//fprintf(stdout,"Skipped %d,%d\n",current->z,current->a);
	continue;
      }
      if(current->z == 0  && current->a==1) {
	//fprintf(stdout,"Skipped %d,%d\n",current->z,current->a);
	continue;
      }
      A_counts[j]++;
    }
  }

  for(j=A_start;j<=A_end;j++) {
    printf("0\n%3d %3d\n",j,A_counts[j]);
    for(l=1;l<=xref->max_z;l++) {
	//skip n and p
      current=iso_data[l][j];
      if(current==NULL) continue;
      if(current->z == 1  && current->a==1) {
	//fprintf(stdout,"Skipped %d,%d\n",current->z,current->a);
	continue;
      }
      if(current->z == 0  && current->a==1) {
	//fprintf(stdout,"Skipped %d,%d\n",current->z,current->a);
	continue;
      }
      for(count=0,k=Step_start;k<=Step_end;k++,count++) { 
	// really the number of time steps
	// now the (p,g) reaction which is Z-1,N -> Z,N
	if((l-1) >= xref->min_z && l-1 <= xref->max_z && 
	    (j-1) >= xref->min_a && (j-1) <= xref->max_a) {
	  from=iso_data[l-1][j-1];// z-1,n(a-1)
	  to=iso_data[l][j];// z,n
	  if(from != NULL && to!=NULL) {
	    reacbuf[0]=p_index;
	    reacbuf[1]=from->index;
	    reacbuf[2]=-1;
	    reacbuf[3]=to->index;
	    reacbuf[4]=-1;
	    reacbuf[5]=reacbuf[6]=-1;
	    node=rb_find_gkey_n(tree,(char *)reacbuf,memcmp_map,&rtn);
	    if(rtn) {
	      flux[count*3]=
		flux_data[k].data[(int)node->v.val]/flux_norm_factor;
	      if(fabs(flux[count*3]) < 1.0E-30) flux[count*3]=0.0;
	      //ri[count*3]=(int)node->v.val;
	    }
	    else {
	      reacbuf[0]=to->index;
	      reacbuf[1]=-1;
	      reacbuf[2]=-1;
	      reacbuf[3]=p_index;
	      reacbuf[4]=from->index;
	      reacbuf[5]=reacbuf[6]=-1;
	      node=rb_find_gkey_n(tree,(char *)reacbuf,memcmp_map,&rtn);
	      if(rtn) {
		flux[count*3]=
		  -(flux_data[k].data[(int)node->v.val]/flux_norm_factor);
		if(fabs(flux[count*3]) < 1.0E-30) flux[count*3]=0.0;
	      }
	      // use the following else for this if(rtn)
	      else {
		flux[count*3]=0.0;
	      }
	    }
	  }
	  else {
	    flux[count*3]=0.0;
	  }
	}
	else {
	  flux[count*3]=0.0;
	}
	// now the (a,p) reaction which is Z-1,N-2 -> Z,N
	if((l-1) >= xref->min_z && (l-1) <= xref->max_z && 
	    (j-3) >= xref->min_a && (j-3) <= xref->max_a) {
	  from=iso_data[l-1][j-3];// z-1,n-2(a-3)
	  to=iso_data[l][j];// z,n
	  if(from != NULL && to!=NULL) { 
	    reacbuf[0]=a_index;
	    reacbuf[1]=from->index;
	    reacbuf[2]=-1;
	    reacbuf[3]=p_index;
	    reacbuf[4]=to->index;
	    reacbuf[5]=reacbuf[6]=-1;
	    node=rb_find_gkey_n(tree,(char *)reacbuf,memcmp_map,&rtn);
	    if(rtn) {
	      flux[count*3+1]=
		flux_data[k].data[(int)node->v.val]/flux_norm_factor;
	      if(fabs(flux[count*3+1]) < 1.0E-30) flux[count*3+1]=0.0;
	    }
	    else {
	      reacbuf[0]=p_index;
	      reacbuf[1]=to->index;
	      reacbuf[2]=-1;
	      reacbuf[3]=a_index;
	      reacbuf[4]=from->index;
	      reacbuf[5]=reacbuf[6]=-1;
	      node=rb_find_gkey_n(tree,(char *)reacbuf,memcmp_map,&rtn);
	      if(rtn) {
		flux[count*3+1]=
		  -(flux_data[k].data[(int)node->v.val]/flux_norm_factor);
		if(fabs(flux[count*3+1]) < 1.0E-30) flux[count*3+1]=0.0;
	      }
	      // use the following else for this if(rtn)
	      else {
		flux[count*3+1]=0.0;
	      }
	    }
	  }
	  else {
	    flux[count*3+1]=0.0;
	  }
	}
	else {
	  flux[count*3+1]=0.0;
	}
	// now the (a,g) reaction which is Z-2,N-2 -> Z,N
	if((l-2) >=xref->min_z && (l-2) <= xref->max_z && 
	    (j-4) >=xref->min_a && (j-4) <= xref->max_a) {
	  from=iso_data[l-2][j-4];// z-,n-2(a-4)
	  to=iso_data[l][j];// z,n
	  if(from != NULL && to!=NULL) { 
	    reacbuf[0]=a_index;
	    reacbuf[1]=from->index;
	    reacbuf[2]=-1;
	    reacbuf[3]=to->index;
	    reacbuf[4]=-1;
	    reacbuf[5]=reacbuf[6]=-1;
	    node=rb_find_gkey_n(tree,(char *)reacbuf,memcmp_map,&rtn);
	    if(rtn) {
	      flux[count*3+2]=flux_data[k].data[(int)node->v.val]/flux_norm_factor;
	      if(fabs(flux[count*3+2]) < 1.0E-30) flux[count*3+2]=0.0;
	      //ri[count*3+2]=(int)node->v.val;
	    }
	    else {
	      reacbuf[0]=to->index;
	      reacbuf[1]=-1;
	      reacbuf[2]=-1;
	      reacbuf[3]=a_index;
	      reacbuf[4]=from->index;
	      reacbuf[5]=reacbuf[6]=-1;
	      node=rb_find_gkey_n(tree,(char *)reacbuf,memcmp_map,&rtn);
	      if(rtn) {
		flux[count*3+2]=
		  -(flux_data[k].data[(int)node->v.val]/flux_norm_factor);
		if(fabs(flux[count*3+2]) < 1.0E-30) flux[count*3+2]=0.0;
	      }
	      // use the following else for this if(rtn)
	      else {
		flux[count*3+2]=0.0;
		//ri[count*3+2]=-1;
	      }
	    }
	  }
	  else {
	    flux[count*3+2]=0.0;
	  }
	}
	else {
	  flux[count*3+2]=0.0;
	}
      }
      if(max_count < count) max_count=count;
      for(i=0;i<count;i++) {
	printf("1 %3d %13.6E %13.6E %13.6E\n",l,flux[i*3],
	    flux[i*3+1],flux[i*3+2]);
      }
    } // end of for all the isotopes
   // fprintf(stdout,"Count is %d\n",count);
  } // end of for all the timesteps
  printf("2 %3d %13.6E %13.6E %13.6E\n",max_count,0.0,0.0,0.0);

  return 0;
}
