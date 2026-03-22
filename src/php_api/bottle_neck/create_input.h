/*****************************************************************************/
/*! \file
 *
 * \verbatim
 *
 * $Author: bucknerk $
 * $Date: 2008/07/18 17:35:57 $
 * $Id: create_input.h,v 1.3 2008/07/18 17:35:57 bucknerk Exp $
 *
 * $Log: create_input.h,v $
 * Revision 1.3  2008/07/18 17:35:57  bucknerk
 * Changed these hopefully for last time to ensure that the file suffix is
 * correct regardless of the zone. Remember that the suffiz is the index in a
 * list of the zone numbers of a simulation. The case where only one zone is run
 * is the problem because that one is always suffix "1". Also if there are more
 * than 9 (usually 1 or 23) then 1 is "01", 2 is "02" and so on.
 *
 * Revision 1.2  2008/07/10 19:17:28  bucknerk
 * updated
 *
 * Revision 1.1  2008/07/07 19:02:58  bucknerk
 * new names for the bottle_neck input creation program source
 *
 * Revision 1.1  2008/03/20 18:52:02  bucknerk
 * new files
 *
 *
 * \endverbatim
 *
 ***************************************************************************
 * The header file for the bottle-neck input file creation program. This has a
 * definition for each of the structures used to read/handle the data from the
 * fortran files.  It also includes the headers needed by most of the
 * functions. The exception there is rb.h which is only used by main().
******************************************************************************/
#include<stdlib.h>
#include<stdio.h>
#include<string.h>
#include<strings.h>
#include<errno.h>
#include<ctype.h>
#include<math.h>
#include<mysql/mysql.h>

//Need to change this eventually to use a db_info struct like the rest of the
//code.
//#define DBNAME "cinad"
//#define PW "Ihwfabnd"
//#define TABLE "reaction_test"
/*
 * In all the data structures the s1/s2 are the 
 * overhead that fortran adds to a record in an 
 * unformatted data file
 */

/** for reading the timemap */
typedef struct rawtime {
  int s1;
  int step;
  double time;
  int s2;
}TimeData;

/** compressed version for storing the timemap */
typedef struct time {
  int step;
  double time;
}CompTimeData;

/** for reading abundance and flux metadata */
typedef struct metadata {
  int s1;
  int step; // simulation step number
  int nnz; // number of non-zero entries
  size_t fpos; // the file position at which the entry starts
  int s2;
}MetaData;

/** for reading abundance(isotope) and flux(reaction) data */
typedef struct data {
  int s1;
  int index; // the index of the isotope/reaction
  double data; // the actual data
  int s2;
}Data;

/*!
 * needed for reading the reaction map file.  This is a cross reference
 * between the reactions, the isotope mapping and the fluxes.  The flux
 * index is for a reaction, then from the reaction you can see which 
 * isotopes are impacted (heaviest?)
 */ 

typedef struct reaction_map_data {
  int s1;
  int index;
  int isotopes[7];
  char ref[4];
  int flag;
  int s2;
}Reaction_Map_Data;

/** for actually storing the reaction map */
typedef struct reacmap {
  int isotopes[7];
  char ref[5]; // and 1 for the null char
  int flag;
}ReacMap;

/* compressed version for storing the abundance data */
/*
typedef struct ab_data {
  int step; // the time step
  int count; // the size of data
  double  *data; // the abundances
}Abundance;
*/

/*!
 * Flux will now have an
 * entry in the data array for every REACTION in the map, 
 * regardless of whether it is
 * zero or not. 
 */
typedef struct fl_data {
  int step; // the time step
  int count; // the size of data
  double  *data; // the fluxes
}Flux;

/*! 
 * for reading a record that just has the step number 
 * This occurs for each simulation zone, as there is a file
 * recording the last step used per zone.
 *
**/
typedef struct step {
  int s1;
  int s; // step number
  int s2;
}Step;

/*!
 * thermo file data, temp is in T9 Kelvin
 * and rho is density in (g/cc)
 */

typedef struct thermodata {
  int s1;
  int step; // simulation step number
  double temp,rho;
  int s2;
}ThermData;

/** compressed thermo file data */
typedef struct ctd{
  int step;
  double temp,rho;
}CompThermData;

/*!
 * The reaction struct stores the rates, which are from 1 to n strings 
 * of comma separated floating point values.
 */

typedef struct reaction{
  int n;
  char *type;
  double Q; 
  char *str; // the reaction string
  char *iso; // the primary isotope
  char *decay; // the "decay" string
  char **rates; // an array of size n of string pointers for the rates
  int count;
}Reaction;

/* 
 * For reading/isotope data from a database. This is not going to be used, at
 * least for now because all the information meeded is stored with the
 * simulations.
typedef struct db_z_data {
  int z,n;
  Reaction reactions[7];// a point array for 7 reaction structs
  // that is one for each of the 7 reactions needed for the waiting
  // point file
}DBaseZData;
*/

/** isotope map file data */
typedef struct isodata {
  int s1;
  int index; // of the isotope 
  short z,a; // of the isotope
  char name[5]; // as in Cr54 or 54Cr
  int s2;
}IsoData;

/*!
 * isotope map file compressed data, plus
 * Adding an array of reactions so that they are stored with the
 * isotopes as they are processed
 */
typedef struct comp_idata {
  int index;
  int check;
  short z,a;
  char name[6]; // one for a NULL
  Reaction *reactions[7];
}CompIsoData;

/*!
 * Nodes used by the xref data structure to store the 
 * isotopes. This allows the storage to be restricted to just the isotopes used
 * without having to create an array for each Z that is as large as N.
 * This then is a single-linked list.
 */
typedef struct vnode {
  CompIsoData data;
  struct vnode *next;
}Vnode;

/*!
 * A cross reference data structure that allows fast lookup
 * of an isotope using z and a
 */
typedef struct isoxref {
  int min_z,max_z,min_a,max_a,min_n,max_n;
  Vnode **iso;
}IsoXref;

/*
 * Function prototypes
 */
// this one sets the zone-to-suffix cross-reference value
extern int set_zone_value(char *);
// this one gets the largest timestep number for the zone 
extern int last_step(char *);
// these 3 are for processing the the xref
extern char *get_iso_name(IsoXref *,int,int);
extern Vnode *get_next_iso(IsoXref *);
extern void get_next_init(IsoXref *);
// these are for retrieving the data from the simulation files.
extern CompThermData *get_thermo_data(char *,int *);
extern CompTimeData *get_time_data(char *,int *);
extern IsoXref *get_isotope_data(char *,int *);
//extern Abundance *get_abundance_data(int,char *,int *);
extern Flux *get_flux_data(int,char *,double *);
extern ReacMap *get_reaction_map(char *,int *);
extern int get_reaction_data(char *,IsoXref *);
//extern int get_database_wait(char *,IsoXref *);


