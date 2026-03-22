/*
 *Moved from the original directory and added the CVS info
 *
 *   $Author: bucknerk $
 *   $Id: use_syslog.c,v 1.1.1.1 2008/04/22 13:36:34 bucknerk Exp $
 *   $Log: use_syslog.c,v $
 *   Revision 1.1.1.1  2008/04/22 13:36:34  bucknerk
 *   all in one place
 *
 *   Revision 1.1  2007/07/17 19:10:07  bucknerk
 *   Moved here
 *
 *   
 */
/****p* program/use_syslog
 *  VERSION
 *    use_syslog 0.1
 *  PURPOSE
 *    Provide a command-line interface to syslog
 *  USAGE
 *    use_syslog prgm_name log_level message
 *
 *  COMMAND-LINE OPTIONS
 *    prgm_name: Name of program logging the message, same as IDENT
 *    log_level: EMERG    system is unusable
 *               ALERT    action must be taken immediately
 *               CRIT     critical conditions
 *               ERR      error conditions
 *               WARNING  warning conditions
 *               NOTICE   normal, but significant, condition
 *               INFO     informational message
 *               DEBUG    debug-level message
 *    message:   Message to log
 *  RETURN VALUE
 *    Zero is returned if successful, nonzero otherwise
 *    See programmer documentation or source code for return value meanings
 *  EXAMPLE
 *    use_syslog cgi_cina ERR 'Invalid or Expired ID'
 *  BUGS
 *    No known bugs as of release
 *  AUTHORS
 *    Jason Scott (jpscott@mail.phy.ornl.gov)
 *  MODIFICATION HISTORY
 *    04/27/2004 jpscott  use_syslog 0.1 started
 *  COPYRIGHT
 *    Copyright 2004 Astrophysics Data Team, Physics Division, 
 *    Oak Ridge National Laboratory.  All rights reserved.
 *    This program is for internal, private use only.
 *
 * USES
 *    stdio.h, string.h, syslog.h
 * SEE ALSO
 *    syslog(3) man page
 */
#include <stdio.h>
#include <string.h>
#include <syslog.h>

/* DEBUG should be defined if debug messages should be printed */
#define DEBUG 0

/* openlog constants */
#define OPT LOG_PID
#define FACILITY LOG_LOCAL3

/* Return values */
#define OK 0
#define BAD_ARGC 1
#define BAD_LOGLEVEL 2
/******/

/****if use_syslog.c/main
 * PURPOSE
 *    main function for use_syslog
 * STATUS
 *    Complete
 * USAGE
 *    Invoke from command line, see "program/use_syslog"
 * AUTHORS
 *    Jason Scott (jpscott@mail.phy.ornl.gov)
 * MODIFICATION HISTORY
 *    04/27/2004 jpscott  main started
 *    05/03/2004 jpscott  fixed wrong priority bug
 * SOURCE
 */
int main(int argc, char *argv[]) 
{
  int log_level = LOG_DEBUG;

  if (argc != 4) {
    fprintf(stderr,"use_syslog should have 3 arguments instead of %d\n",argc-1);
    return BAD_ARGC;
  }

  /* Make sure log_level is valid */
  if (strcmp(argv[2],"EMERG") == 0)
    log_level = LOG_EMERG;
  else if (strcmp(argv[2],"ALERT") == 0)
    log_level = LOG_ALERT;
  else if (strcmp(argv[2],"CRIT") == 0)
    log_level = LOG_CRIT;
  else if (strcmp(argv[2],"ERR") == 0)
    log_level = LOG_ERR;
  else if (strcmp(argv[2],"WARNING") == 0)
    log_level = LOG_WARNING;
  else if (strcmp(argv[2],"NOTICE") == 0)
    log_level = LOG_NOTICE;
  else if (strcmp(argv[2],"INFO") == 0)
    log_level = LOG_INFO;
  else if (strcmp(argv[2],"DEBUG") == 0)
    log_level = LOG_DEBUG;
  else {
    fprintf(stderr,"Unknown log_level %s\n",argv[2]);
    return BAD_LOGLEVEL;
  }

  /* Print summary for debugging */
#ifdef DEBUG
  printf("Program name (ident) = %s\n",argv[1]);
  printf("Log level = %s (%d)\n",argv[2],log_level);
  printf("Message = '%s'\n",argv[3]);
  printf("Priority = (level %d OR facility %d) = %d\n",log_level,FACILITY,log_level | FACILITY);
#endif

  /* Make sure you read the syslog(3) manpage */
  openlog(argv[1],OPT,FACILITY);

  syslog(log_level | FACILITY,"%s",argv[3]);

  closelog;
  return OK;
}
/******/
