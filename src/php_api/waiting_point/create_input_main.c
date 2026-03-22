#include"create_input.h"
#include"rb.h"
/*****************************************************************************/
/*! \mainpage notitle 
*
* <img src="../dbinfo/stars.png">
*
* <br> <h1 align="center">The CINA Waiting Point Code Documentation</h1>
*
* \section intro Introduction
*
* This is the automatically (using doxygen) generated documentation of the
* code that computes the <em>Waiting Point</em> isotopes for the CINA 
* component of <b><i>nucastrodata.org</i></b>.  There is a single
* <b>.php</b> file, <tt>cina(d)_main.php</tt> which is the focus of the
* interface actions and it calls a function to handle the <em>Waiting Point</em>
* computation which is this code.
*
*
* \section probs Potential Problems
*
* Current issues are:
* <ul><li> If the simulation is too old, how old that is we really don't know,
* there is not enough data saved for waiting points to be computer with this
* code which relies on the input data for the \tbl{xnet} code to get the
* isotopes and reaction rates.
* </ul>
*
* \section ge General Error Handling
*
* This code is hopefully robust enough that there are no 'bugs' but there may be
* other errors, as in from the input data. The concept is simply return an error
* message and exit. All messages are written to standard output so that
* hopefully they will get back to the user.
* 
* 
******************************************************************************/

/*! \file
 *
 * \verbatim
 *
 * $Author: bucknerk $
 * $Date: 2008/07/17 15:37:28 $
 * $Id: create_input_main.c,v 1.6 2008/07/17 15:37:28 bucknerk Exp $
 *
 * $Log: create_input_main.c,v $
 * Revision 1.6  2008/07/17 15:37:28  bucknerk
 * Hopefuly fixed so that the zone suffix for the files like 'step_num' and 'abund'
 * will be correct. Seems that if there are more than 9 zones in the simulation
 * then there are two digits to all numbers, i.e. 01, 02 etc. If there are less
 * than ten then there is only a single digit.  Sucks.
 *
 * Revision 1.5  2008/03/26 20:08:15  bucknerk
 * I think I have the waiting point finder giving good responses now and
 * that it runs correctly with zones like 01, 05 and so on.
 *
 * Revision 1.4  2008/01/08 13:57:05  bucknerk
 * added a function that sets the zone in a static variable and now Zone is not
 * passed to any of the other functions.  required because if the simulation is not
 * run on zone 1 the first set of files (like abundN_cnt) are still numbered 1.
 * This causes the suffix of the files that corresponds to the zone number to be
 * found and stored.
 *
 * Revision 1.3  2007/12/05 19:27:12  bucknerk
 * fixed error reporting, fortran file handleing
 *
 * Revision 1.2  2007/12/03 17:48:50  bucknerk
 * merged with branch cina-rel-1-0
 *
 * Revision 1.1.2.2  2007/12/03 13:30:18  bucknerk
 * finally fixed the segfault.  typically was freeing already freed memory. Cut and
 * paste strikes again. Also updated some documentation.
 *
 * Revision 1.1.2.1  2007/11/28 18:35:25  bucknerk
 * Renamed craz_list list_crazy.dat, updated the makefile for the create_input
 * files and added all those create_input files.
 *
 *
 *
 * \endverbatim
 * ********************************
 * Start over. Again. First read in the set of isotopes.  This may be a range
 * smaller than the size of the input isotope list. So pass the start Z,N to
 * the isotope function.
 * 
 * Then read in the reactions. Need the set of only those in the isotope
 * list.  Order these somehow. Be nice if they had the same indexing. Make
 * it very simple and ensure there is one reaction set (maybe NULL) for each
 * isotope.
 *
 * read in the abundance data for the isotopes, again only those needed and
 * cross-index them. In the input abundace set, make sure that for each time
 * step there is an entry per isotope (may be 0.0); That way no messing
 * around.
 *
 * get the temperature data.
 * 
 * NOW (11/27/2007) were will use all that data that has been gathered but need
 * more information from the simulation files.  Now we need the reaction map and
 * the flux values.  The reaction map is a cross-reference point.  It has the
 * reaction ref string (bet+,bet-,woof,etc) an arbitrary index and a set of 7
 * indices which correspond to the isotopes in the particular reaction.  the
 * first 3 are the left-hand side and the last 4 are the right-hand side.  It
 * also has a weak-reaction flag value that is not really used but it is 
 * currently 1 for ec reactions, 4 for bet+/- reactions and 0 for all the
 * "strong" reactions.  Then the flux file contains all the non-zero fluxes for
 * a time step, these fluxes are associated with a reaction index value.  So
 * going from isotope to flux is a case of 1)find the reaction and 2)use that to
 * find the flux.
 *
 *
 * Now, for every time step, print step, time,temp,density,abundnace of p,
 * abundance of 4He, and the 'normalization factor'.  This is the largest
 * flux value for ANY reaction in this simulation.  Use it to 'normalize' the
 * values for each isotope.
 *
 * Next for each time step, iterate over the used isotopes and for each print a
 * line of z,n,abundance,bet+ creation RATE( rc1), rc2 flux, rc3 flux, 
 * rc4 flux, rc5 flux, rc6 flux, rc7 flux, bet+ destruction rate (rd1).  Each of
 * the fluxes is for the corresponding reaction. Apparently on positive flux
 * values are stored for the simulation so if (Z+1,N+2)<p,a>(Z,N) is not listed
 * check (Z,N)<a,p>(Z+1,N+2).  If that IS then use the negative of its flux.
 * Also remember that these flux values need to be 'normalized' by dividing them
 * by our normalization factor.
 * and print out the information, the only real processing is checking if
 * reaction/rate exists for an isotope.  This is faster than computing for 0
 * values.
 * 
 *
 */

/*!
 ***********************************************************************/
const double One_third=1.0/3.0; // for rate computation
const double Five_third=1.0/3.0; // for rate computation

int compute_rate(int,char **,double,double *);

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
 * The main function:
 *
 * \param[in] argc argument count
 * \param[in] argv argument array
 *
 * \input argv[1] simulation base directory
 * \input argv[2] begining isotope Z value
 * \input argv[3] begining isotope N value
 * \input argv[4] ending isotope Z value
 * \input argv[5] ending isotope N value
 * \input argv[5] beginngin time step
 * \input argv[6] ending time step
 * \input argv[7] zone
 *
 * \output A file containing the processed data for the waiting_point code.
 ************************************************************************/

int main(int argc, char **argv)
{
  int q1,q2,q3,q4; // q values
  int i,j,k,l,m;
  int count;
  int rtn;
  char *zone;
  int reacbuf[7]; // for the isotope indices for a reaction
  int Step_start,Step_end; // start and end time steps from args
  int isocount,abundcount,thermocount,timecount; // rtns from functions
  int reaction_count;
  int Z_start,Z_end,N_start,N_end,A_start,A_end; // from args (A is computed)
  int p_index,a_index,start_index,end_index; // the indices of the
  // isotopes p, 4He, start Z,A and end Z,A
  double rate,Qval,ltemp;
  double norm_time; // the current normalized time
  double flux_norm_factor; // the largest possible flux value this simulation
  char directory[1024];
  char *tmp_ptr;
  CompIsoData ***iso_data=NULL,*current=NULL,*local=NULL;
  CompIsoData *from=NULL,*to=NULL;
  CompTimeData *time_data=NULL,base_time;
  CompThermData *thermo_data=NULL;
  Abundance *abundance_data=NULL;
  Flux *flux_data=NULL;
  ReacMap *reaction_map=NULL;
  Vnode *vptr=NULL;
  IsoXref *xref=NULL;
  Rb_node tree,node;
  struct stat sbuf;

  
  if(argc != 9) {
    fprintf(stderr, "ERROR=Usage: create_input basedir Zstart Nstart Zend Nend Stepstart Stepend zone\n");
    for(i=0;i<argc;i++) {
      fprintf(stderr," %d ::= %s\n",i,argv[i]);
    }
    return 1;
  }
  stderr=fopen("/tmp/create_input.log","w");
  /*
  for(i=0;i<argc;i++) {
    fprintf(stderr," %d ::= %s\n",i,argv[i]);
  }
  */
  //fflush(stderr);

  rtn=stat(argv[1],&sbuf);
  if(rtn < 0) {
    perror("Stat of input directory");
    fprintf(stderr,"%s: Stat of input directory\n",strerror(errno));
    return 1;
  }

  q1=q2=q3=q4=0;
  Step_start=atoi(argv[6]);
  Step_end=atoi(argv[7]);
  zone=argv[8];
  // Step 1 get isotopes for the specified range.
  Z_start=atoi(argv[2]);
  Z_end=atoi(argv[4]);
  N_start=atoi(argv[3]);
  N_end=atoi(argv[5]);
  A_start=N_start+Z_start;
  A_end=N_end+Z_end;

  sprintf(directory,"%s/data",argv[1]);

  xref= get_isotope_data(argv[1], &isocount);
  if(xref==NULL) {
    fprintf(stderr,"ERROR=Could not get isotope data\n");
    //fflush(stderr);
    return 1;
  }
  //fprintf(stderr,"get_isotope_data was okay \n");
  //fflush(stderr);
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
    //fflush(stderr);
    return(1);
  }
  for(i=0;i<=xref->max_z;i++) {
    iso_data[i]=(CompIsoData **)calloc(sizeof(CompIsoData *),xref->max_a+1);
    if(iso_data[i]==NULL) {
      fprintf(stderr,"ERROR=Allocating iso_data *array:%s\n",strerror(errno));
      //fflush(stderr);
      return(1);
    }
  }
  get_next_init(xref);
  vptr=get_next_iso(xref);
  a_index=p_index=-1;
  start_index=end_index=-1;
  count=0;
  //fprintf(stderr,"starting xref stuff \n");
  //fflush(stderr);
  while(vptr!=NULL) {
    if(vptr->data.z==1 && vptr->data.a==1) p_index=vptr->data.index;
    if(vptr->data.z==2 && vptr->data.a==4) a_index=vptr->data.index;
    if (vptr->data.z >= Z_start && vptr->data.z <= Z_end ) {
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
	      count++;
	      //fprintf(stderr,"%s,%d: %d %d\n",vptr->data.name,count,
	      //	  vptr->data.z,vptr->data.a);
	    }
	  }
	  else {
	    count++;
	    //fprintf(stderr,"%s,%d: %d %d\n",vptr->data.name,count,
	        //vptr->data.z,vptr->data.a);
	  }
	}
      }
    }
    iso_data[vptr->data.z][vptr->data.a]=&vptr->data;
    vptr=get_next_iso(xref);
  }
  if(Z_start<xref->min_z) Z_start=xref->min_z;
  if(Z_end>xref->max_z) Z_end=xref->max_z;
  if(A_start<xref->min_a) A_start=xref->min_a;
  if(A_end>xref->max_a) A_end=xref->max_a;
  //fprintf(stderr,"finish xref stuff \n");
  //fflush(stderr);
  
  
  /*
  if(A_start>1) { // already excludes n and p
    printf("%d\n",count);
  }
  else {
    if(Z_start==0 && N_start < 2) { // includes n and p
      printf("%d\n",count-2);
    }
    else { // only other way is Z=1,N=0 which is p but not n
     printf("%d\n",count-1);
    }
  }
  */
  printf("%d\n",count);

  // Initialize the zone-to-suffix cross-reference for this zone

  rtn=set_zone_value(zone);
  if(rtn<0) {
    fprintf(stderr,"ERROR=Could not get zone suffix information\n");
    //fflush(stderr);
    return 1;
  }

  // Next get the reactions,abundances,times, and temps
  
  //fprintf(stderr,"getting reaction data from '%s'\n",directory);
  //fflush(stderr);
  rtn=get_reaction_data(directory,xref);
  if(rtn > 0) {
    fprintf(stderr,"ERROR=Could not get reaction data\n");
    fflush(stderr);
    return 1;
  }
  //fprintf(stderr,"get_reaction_data was okay \n");
  //fflush(stderr);
  abundance_data=get_abundance_data(isocount,argv[1],&abundcount);
  if(abundance_data==NULL) {
    fprintf(stderr,"ERROR=Could not get abundance data\n");
  //fflush(stderr);
    return(1);
  }
  //fprintf(stderr,"get_abundance_data was okay \n");
  //fflush(stderr);
  thermo_data=get_thermo_data(argv[1],&thermocount);
  if(thermo_data==NULL) {
    fprintf(stderr,"ERROR=Could not get thermo data\n");
  //fflush(stderr);
    return(1);
  }
  //fprintf(stderr,"get_thermo_data was okay \n");
  //fflush(stderr);
  time_data=get_time_data(argv[1],&timecount);
  if(time_data==NULL) {
    fprintf(stderr,"ERROR=Could not get time data\n");
  //fflush(stderr);
    return(1);
  }
  //fprintf(stderr,"get_time_data was okay \n");
  //fflush(stderr);
  reaction_map=get_reaction_map(argv[1],&reaction_count);
  if(reaction_map==NULL) {
    fprintf(stderr,"ERROR=Could not get reaction map data\n");
  //fflush(stderr);
    return(1);
  }
  //fprintf(stderr,"get_reaction_map was oaky \n");
  //fflush(stderr);

  tree=make_rb();
  if(tree==NULL) {
    fprintf(stderr,"ERROR=Allocating new rb tree:%s\n",strerror(errno));
    //fflush(stderr);
    return(1);
  }

  // Using Jim Planks rb_tree code to create a rapidly searchable structure that
  // won't take up too much space.
  for(i=0;i<reaction_count;i++) {
    tmp_ptr=(char *)calloc(sizeof(int),7);
    if(tmp_ptr == NULL) {
      fprintf(stderr,"ERROR=Allocating rbtree key:%s\n",strerror(errno));
  //fflush(stderr);
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
    fprintf(stderr,"Could not get flux data\n");
  //fflush(stderr);
    return(1);
  }
  //fprintf(stderr,"get_flux_data was okay \n");
  //fflush(stderr);

  // print out total number of timesteps selected
  //fprintf(stdout,"REAL_COUNT: %d\n",timecount);
  printf("%d\n",Step_end-Step_start+1);

  // Now I should have them all.  I need to verify that the thermo and
  // abundance data and time are the same size by checking the count.
  // This is a paranoia check
  if(abundcount != thermocount || timecount != abundcount) {
    fprintf(stderr,"Different numbers of abundances and thermo");
    fprintf(stderr," points for zone %s\n",zone);
    return(1);
  }

  // so we can "normalize" the times for each step
  base_time=time_data[0];

  for(k=0;k<abundcount;k++) { // really the number of time steps
    if(abundance_data[k].step < Step_start ||
	abundance_data[k].step > Step_end) continue;
    norm_time=time_data[k].time-base_time.time; 
    ltemp=thermo_data[k].temp; // need it to compute with later
    // this needs to be step_num, normalized_time
    // temp, density,p_abundance,4He_abundance,flux_norm_factor\n
    printf("%4d %19.12E %13.6E %13.6E",abundance_data[k].step,
	norm_time,ltemp, thermo_data[k].rho);
    // print the abundance of p and 4He but the might not be there (??)
    // anyway that's the reason for the strangeness
    printf(" %13.6E %13.6E %13.6E\n",
	p_index >=0 ? abundance_data[k].data[p_index]:0.0,
    	a_index >=0 ? abundance_data[k].data[a_index]:0.0,
	flux_norm_factor);
    // the idea is loop over all the isotopes.  We only focus on the
    // ones in the range which is what this loop should do.  For now
    // we will only worry about the Z value.
    for(l=Z_start;l<=Z_end;l++) {
      for(j=A_start;j<=A_end;j++) {
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
      // count++;
      printf("%3d %3d %13.6E ",current->z,current->a-current->z,
	  abundance_data[k].data[current->index]);
      Qval=0.0;
      for(m=0;m<8;m++) { // loop over the reactions there are now 14
	// but here we only do the 7 creations and the first desctruction
	rate=0.0;
	switch(m) {
	  case 0: // rc0 - this stays the same
	    if((l+1) > xref->max_z)  break;
	    local=iso_data[l+1][j];// z+1,n-1
	    if(local == NULL || local->reactions[0]==NULL ||
		local->reactions[0]->decay == NULL) break;
	    // This is a beta decay need to make sure
	    rtn=compute_rate(local->reactions[0]->n,local->reactions[0]->rates,
		ltemp,&rate);
	    if(rtn) rate=0.0;
	    if(fabs(rate) < 1.0E-30) rate=0.0;
	    break;
	  case 1: // rc1  (z-1,n)<p,g>(z,n)
	    if((l-1) < 0 || (j-1) < 0)  break;
	    from=iso_data[l-1][j-1];// z-1,n(a-1)
	    to=iso_data[l][j];// z,n(a)
	    if(from == NULL || to==NULL) break;
	    reacbuf[0]=p_index;
	    reacbuf[1]=from->index;
	    reacbuf[2]=-1;
	    reacbuf[3]=to->index;
	    reacbuf[4]=-1;
	    reacbuf[5]=reacbuf[6]=-1;
	    node=rb_find_gkey_n(tree,(char *)reacbuf,memcmp_map,&rtn);
	    if(rtn) {
	      rate=flux_data[k].data[(int)node->v.val]/flux_norm_factor;
	      if(rate < 1.0E-30) rate=0.0;
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
		rate = flux_data[k].data[(int)node->v.val]/flux_norm_factor;
		if(fabs(rate) < 1.0E-30) {
		  rate=0.0;
		}
		else {
		  rate=-rate;
		}
	      }
	      else {
		rate=0.0;
	      }
	    }
	    break;
	  case 2: // rc2 (z-1,n-2)<a,p>(z,n)
	    if((l-1) < 0 || (j-3) < 0)  break;
	    from=iso_data[l-1][j-3];// z-1,n-2(a-3)
	    to=iso_data[l][j];// z,n(a)
	    if(from == NULL || to==NULL) break;
	    reacbuf[0]=a_index;
	    reacbuf[1]=from->index;
	    reacbuf[2]=-1;
	    reacbuf[3]=p_index;
	    reacbuf[4]=to->index;
	    reacbuf[5]=reacbuf[6]=-1;
	    node=rb_find_gkey_n(tree,(char *)reacbuf,memcmp_map,&rtn);
	    if(rtn) {
	      rate=flux_data[k].data[(int)node->v.val]/flux_norm_factor;
	      if(fabs(rate) < 1.0E-30) rate=0.0;
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
		rate = flux_data[k].data[(int)node->v.val]/flux_norm_factor;
		if(fabs(rate) < 1.0E-30) {
		  rate=0.0;
		}
		else {
		  rate=-rate;
		}
	      }
	      else {
		rate=0.0;
	      }
	    }
	    break;
	  case 3: // rc3 (z-2,n-2)<a,g>(z,n)
	    if((l-2) < 0 || (j-4) < 0)  break;
	    from=iso_data[l-2][j-4];// z-2,n-2(a-4)
	    to=iso_data[l][j];// z,n(a)
	    if(from == NULL || to==NULL) break;
	    reacbuf[0]=a_index;
	    reacbuf[1]=from->index;
	    reacbuf[2]=-1;
	    reacbuf[3]=to->index;
	    reacbuf[4]=-1;
	    reacbuf[5]=reacbuf[6]=-1;
	    node=rb_find_gkey_n(tree,(char *)reacbuf,memcmp_map,&rtn);
	    if(rtn) {
	      rate=flux_data[k].data[(int)node->v.val]/flux_norm_factor;
	      if(fabs(rate) < 1.0E-30) rate=0.0;
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
		rate = flux_data[k].data[(int)node->v.val]/flux_norm_factor;
		if(fabs(rate) < 1.0E-30) {
		  rate=0.0;
		}
		else {
		  rate=-rate;
		}
	      }
	      else {
		rate=0.0;
	      }
	    }
	    break;
	  case 4: // rc4 (z+1,n+2)<p,a>(z,n)
	    if((l+1) > xref->max_z || (j+3) > xref->max_a)  break;
	    from=iso_data[l+1][j+3];// z+1,n+2(a+3)
	    to=iso_data[l][j];// z,n(a)
	    if(from == NULL || to==NULL) break;
	    reacbuf[0]=p_index;
	    reacbuf[1]=from->index;
	    reacbuf[2]=-1;
	    reacbuf[3]=a_index;
	    reacbuf[4]=to->index;
	    reacbuf[5]=reacbuf[6]=-1;
	    node=rb_find_gkey_n(tree,(char *)reacbuf,memcmp_map,&rtn);
	    if(rtn) {
	      rate=flux_data[k].data[(int)node->v.val]/flux_norm_factor;
	      if(fabs(rate) < 1.0E-30) rate=0.0;
	    }
	    else {
	      reacbuf[0]=a_index;
	      reacbuf[1]=to->index;
	      reacbuf[2]=-1;
	      reacbuf[3]=p_index;
	      reacbuf[4]=from->index;
	      reacbuf[5]=reacbuf[6]=-1;
	      node=rb_find_gkey_n(tree,(char *)reacbuf,memcmp_map,&rtn);
	      if(rtn) {
		rate = flux_data[k].data[(int)node->v.val]/flux_norm_factor;
		if(fabs(rate) < 1.0E-30) {
		  rate=0.0;
		}
		else {
		  rate=-rate;
		}
	      }
	      else {
		rate=0.0;
	      }
	    }
	    break;
	  case 5: // rc5 (z+1,n)<g,p>(z,n)
	    if((l+1) > xref->max_z || (j+1) > xref->max_a)  break;
	    from=iso_data[l+1][j+1];// z+1,n(a+1)
	    to=iso_data[l][j];// z,n(a)
	    if(from == NULL || to==NULL) break;
	    reacbuf[0]=from->index;
	    reacbuf[1]=-1;
	    reacbuf[2]=-1;
	    reacbuf[3]=p_index;
	    reacbuf[4]=to->index;
	    reacbuf[5]=reacbuf[6]=-1;
	    node=rb_find_gkey_n(tree,(char *)reacbuf,memcmp_map,&rtn);
	    if(rtn) {
	      rate=flux_data[k].data[(int)node->v.val]/flux_norm_factor;
	      if(fabs(rate) < 1.0E-30) rate=0.0;
	    }
	    else {
	      reacbuf[0]=p_index;
	      reacbuf[1]=to->index;
	      reacbuf[2]=-1;
	      reacbuf[3]=from->index;
	      reacbuf[4]=-1;
	      reacbuf[5]=reacbuf[6]=-1;
	      node=rb_find_gkey_n(tree,(char *)reacbuf,memcmp_map,&rtn);
	      if(rtn) {
		rate = flux_data[k].data[(int)node->v.val]/flux_norm_factor;
		if(fabs(rate) < 1.0E-30) {
		  rate=0.0;
		}
		else {
		  rate=-rate;
		}
	      }
	      else {
		rate=0.0;
	      }
	    }
	    break;
	  case 6: // rc6 (z+2,n+2)(g,a)(z,n)
	    if((l+2) > xref->max_z || (j+4) > xref->max_a)  break;
	    from=iso_data[l+2][j+4];// z+2,n+2(a+4)
	    to=iso_data[l][j];// z,n(a)
	    if(from == NULL || to==NULL) break;
	    reacbuf[0]=from->index;
	    reacbuf[1]=-1;
	    reacbuf[2]=-1;
	    reacbuf[3]=a_index;
	    reacbuf[4]=to->index;
	    reacbuf[5]=reacbuf[6]=-1;
	    node=rb_find_gkey_n(tree,(char *)reacbuf,memcmp_map,&rtn);
	    if(rtn) {
	      rate=flux_data[k].data[(int)node->v.val]/flux_norm_factor;
	      if(fabs(rate) < 1.0E-30) rate=0.0;
	    }
	    else {
	      reacbuf[0]=a_index;
	      reacbuf[1]=to->index;
	      reacbuf[2]=-1;
	      reacbuf[3]=from->index;
	      reacbuf[4]=-1;
	      reacbuf[5]=reacbuf[6]=-1;
	      node=rb_find_gkey_n(tree,(char *)reacbuf,memcmp_map,&rtn);
	      if(rtn) {
		rate = flux_data[k].data[(int)node->v.val]/flux_norm_factor;
		if(fabs(rate) < 1.0E-30) {
		  rate=0.0;
		}
		else {
		  rate=-rate;
		}
	      }
	      else {
		rate=0.0;
	      }
	    }
	    break;
	  case 7: // rd0
	    if(current->reactions[0]==NULL) break;
	    rtn=compute_rate(current->reactions[0]->n,
		current->reactions[0]->rates,ltemp,&rate);
	    if(rtn) rate=0.0;
	    if(fabs(rate) < 1.0E-30) rate=0.0;
	    break;
	}// end of switch
	printf("%13.6E ",rate);
      }
      printf("\n");

      }
    } // end of for all the isotopes
   // fprintf(stdout,"Count is %d\n",count);
  } // end of for all the abundances
  //fprintf(stdout,"%d := Q1 %d : Q2 %d ; Q3 %d : Q4 %d \n",ncount,q1,q2,q3,q4);



  return 0;
}
/*!
 * Does the hard part of the computation
 */

int compute_rate(int num,char **rates,double temp,double *ret_val)
{
  int i,c;
  char rstr[256];
  char *ptr,*ptr_a[10];
  double a1,a2,a3,a4,a5,a6,a7;

  *ret_val=0.0;
  for(i=0;i<num;i++) {
    // parse the rate
    strcpy(rstr,rates[i]);
    ptr=rstr;
    c=0;
    memset(ptr_a,0,sizeof(char *)*10);
    ptr_a[c++]=ptr;
    while(*ptr != '\0') {
      if(*ptr==',') {
	*ptr='\0';
	ptr_a[c++]=++ptr;
      }
      else {
	ptr++;
      }
    }
    if(c != 7) {
      fprintf(stderr,"%s: Not enough rate values",
	 rates[i]);
      return(1);
    }
    // compute the rate as
    // exp(a1 + a2T^-1 + a3T^-1/3 + a4T^1/3 +
    // a5T + a6T^5/3 +a7ln(T))

    errno=0;
    a1=strtod(ptr_a[0],NULL);
    a2=strtod(ptr_a[1],NULL);
    a3=strtod(ptr_a[2],NULL);
    a4=strtod(ptr_a[3],NULL);
    a5=strtod(ptr_a[4],NULL);
    a6=strtod(ptr_a[5],NULL);
    a7=strtod(ptr_a[6],NULL);
    if(errno > 0) {
      fprintf(stderr,"ERROR=Converting an a-value:%s\n",strerror(errno));
      return(1);
    }
    *ret_val += exp((a1 + (a2*(1.0/temp)) + (a3*pow(temp,-One_third))
	  + (a4*pow(temp,One_third)) + (a5*temp) +
	  (a6*pow(temp,Five_third)) + (a7*log(temp))));
  }
  return 0;
} 
