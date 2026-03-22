/****************************************************************************/
/* \file
 *
 * \verbatim
 *
 * $Author: bucknerk $
 * $Date: 2008/07/18 17:35:57 $
 * $Id: read_time_data.c,v 1.3 2008/07/18 17:35:57 bucknerk Exp $
 *
 * $Log: read_time_data.c,v $
 * Revision 1.3  2008/07/18 17:35:57  bucknerk
 * Changed these hopefully for last time to ensure that the file suffix is
 * correct regardless of the zone. Remember that the suffiz is the index in a
 * list of the zone numbers of a simulation. The case where only one zone is run
 * is the problem because that one is always suffix "1". Also if there are more
 * than 9 (usually 1 or 23) then 1 is "01", 2 is "02" and so on.
 *
 * Revision 1.2  2008/07/07 19:15:49  bucknerk
 * stuff
 *
 * Revision 1.1  2008/03/20 18:52:03  bucknerk
 * new files
 *
 * Revision 1.1  2008/01/07 21:20:59  bucknerk
 * new file
 *
 *
 * \endverbatim
 * ********************************
 * 
 * This is being used to convert (a pair of) times from the Evaluation
 * Visualizer into time steps that can be used with the waiting point or bottle
 * neck code. The
 * problem is that The Visualizer uses 'normalized' time which means that the
 * time for the highest temperature in the simulation is 0 and all the displayed
 * times have that value subtracted from them. 
 *
 * This gets the thermo data for the simulation, gets the max temp and its
 * corresponding time for normalization.  Then it takes the input values, adds
 * the normalization factor back to them and searches for the results in the
 * in the time map.  
 *
 * \output A pair of integer time steps, the smaller is the largest time step
 * with a time value less than or equal to the first input time value.  The 
 * larger output is the smallest time step whose time is equal to or greater 
 * than the second input value.
 *
 ****************************************************************************/
#include "create_input.h"
#include<values.h>

int main(int argc, char **argv)
{
  CompTimeData *time;
  CompThermData *temp;
  double start,end,norm,norm_time;
  int i,val,tval,steps[2],norm_step,rtn;

  if(argc != 5) {
    fprintf(stderr,"USAGE: read_time_data directory zone start end\n");
    return 1;
  }
  rtn=set_zone_value(argv[2]);
  if(rtn<0) {
    fprintf(stderr,"ERROR=Could not set zone file suffix\n");
    return 1;
  }

  steps[0]=steps[1]=-1;
  time=get_time_data(argv[1],&val);
  temp=get_thermo_data(argv[1],&tval);
  if(time && temp) {
    norm=MINFLOAT;
    for(i=0;i<tval;i++) {
      if(temp[i].temp > norm) {
	norm=temp[i].temp;
	norm_step=temp[i].step;
      }
	
    }

    norm_time=time[norm_step].time;


    start=strtod(argv[3],NULL);
    end=strtod(argv[4],NULL);
    i=0;
    for(i=0;i<val;i++) {
      if(steps[0]<0) {
	if((time[i].time-norm_time) == start) steps[0]=time[i].step;
	else if((time[i].time-norm_time) > start) {
	  // should be largest less than
	  if(i > 0) steps[0]=time[i-1].step;
	  else steps[0]=time[0].step;
	  }
	}
      if(steps[1]<0) {
	if((time[i].time-norm_time) == end) steps[1]=time[i].step;
	else if((time[i].time-norm_time) > end) {
	  // should be smallest greater than
	  steps[1]=time[i].step;
	  }
	}
      }
    }
  else {
    fprintf(stderr,"could not get time or temperature values\n");
  }

  printf("%d,%d\n",steps[0],steps[1]);
  return 0;
}
