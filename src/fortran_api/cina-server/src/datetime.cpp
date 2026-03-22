#include "include/datetime.h"

Date CINAgetDate()
{
  Date   d;
  time_t t;
  tm     *tm;

  if (time(&t) == -1) throw new Err(MSG_MISSING,"time() returned -1",0,-1,"",
				    "$Id: datetime.cpp,v 1.1.1.1 2008/04/22 13:36:36 bucknerk Exp $");
  tm = localtime(&t);
  d.year = tm->tm_year + 1900;
  d.month = tm->tm_mon + 1;
  d.day = tm->tm_mday;
  return d;
}

Date &CINAgetDate(Date &d, const int Days2add)
{
  time_t t;
  tm     *tm;

  if (time(&t) == -1) throw new Err(MSG_MISSING,"time() returned -1",0,-1,"",
				    "$Id: datetime.cpp,v 1.1.1.1 2008/04/22 13:36:36 bucknerk Exp $");
  t += Days2add * (24 * 60 * 60);

  tm = localtime(&t);
  d.year = tm->tm_year + 1900;
  d.month = tm->tm_mon + 1;
  d.day = tm->tm_mday;
  return d;
}

Time CINAgetTime()
{
  Time    t;
  time_t  tt;
  tm     *tm;

  if (time(&tt) == -1) throw new Err(MSG_MISSING,"time() returned -1",0,-1,"",
				     "$Id: datetime.cpp,v 1.1.1.1 2008/04/22 13:36:36 bucknerk Exp $");
  tm = localtime(&tt);
  t.hour = tm->tm_hour;
  t.minute = tm->tm_min;
  t.second = tm->tm_sec;
  return t;
}

Time &CINAgetTime(Time &t)
{
  time_t  tt;
  tm     *tm;

  if (time(&tt) == -1) throw new Err(MSG_MISSING,"time() returned -1",0,-1,"",
				    "$Id: datetime.cpp,v 1.1.1.1 2008/04/22 13:36:36 bucknerk Exp $");
  tm = localtime(&tt);
  t.hour = tm->tm_hour;
  t.minute = tm->tm_min;
  t.second = tm->tm_sec;
  return t;
}

DateTime &CINAgetDateTime(DateTime &d, const int Days2add)
{
  time_t t;
  tm     *tm;

  if (time(&t) == -1) throw new Err(MSG_MISSING,"time() returned -1",0,-1,"",
				    "$Id: datetime.cpp,v 1.1.1.1 2008/04/22 13:36:36 bucknerk Exp $");
  t += Days2add * (24 * 60 * 60);

  tm = localtime(&t);
  d.year = tm->tm_year + 1900;
  d.month = tm->tm_mon + 1;
  d.day = tm->tm_mday;
  d.hour = tm->tm_hour;
  d.minute = tm->tm_min;
  d.second = tm->tm_sec;
  return d;
}

DateTime CINAgetDateTime()
{
  DateTime d;
  time_t t;
  tm     *tm;

  if (time(&t) == -1) throw new Err(MSG_MISSING,"time() returned -1",0,-1,"",
				    "$Id: datetime.cpp,v 1.1.1.1 2008/04/22 13:36:36 bucknerk Exp $");
  tm = localtime(&t);
  d.year = tm->tm_year + 1900;
  d.month = tm->tm_mon + 1;
  d.day = tm->tm_mday;
  d.hour = tm->tm_hour;
  d.minute = tm->tm_min;
  d.second = tm->tm_sec;
  return d;
}

extern DateTime CINAmakeDbTime(const string &date, const string &time)
{
  char ans[20];
  char *start = ans;
  int i;

  // Copy YYYY
  for (i = 0; i < 4; i++) ans[i] = date[i + 7];
  ans[4] = '-';
  start += 5;

  // Get month
  // Get Mmm for converting into MM
  start[0] = date[0];
  start[1] = date[1];
  start[2] = date[2];
  start[3] = '\0';

  if (!strcmp(start,"Jan")) {ans[5] = '0'; ans[6] = '1';}
  else if (!strcmp(start,"Feb")) {ans[5] = '0'; ans[6] = '2';}
  else if (!strcmp(start,"Mar")) {ans[5] = '0'; ans[6] = '3';}
  else if (!strcmp(start,"Apr")) {ans[5] = '0'; ans[6] = '4';}
  else if (!strcmp(start,"May")) {ans[5] = '0'; ans[6] = '5';}
  else if (!strcmp(start,"Jun")) {ans[5] = '0'; ans[6] = '6';}
  else if (!strcmp(start,"Jul")) {ans[5] = '0'; ans[6] = '7';}
  else if (!strcmp(start,"Aug")) {ans[5] = '0'; ans[6] = '8';}
  else if (!strcmp(start,"Sep")) {ans[5] = '0'; ans[6] = '9';}
  else if (!strcmp(start,"Oct")) {ans[5] = '1'; ans[6] = '0';}
  else if (!strcmp(start,"Nov")) {ans[5] = '1'; ans[6] = '1';}
  else if (!strcmp(start,"Dec")) {ans[5] = '1'; ans[6] = '2';}
  else {ans[5] = '0'; ans[6] = '0';}
  ans[7] = '-';

  // Get DD
  ans[8] = date[4];
  ans[9] = date[5];
  ans[10] = ' ';

  // Append time to date
  for (i = 0; i < 8; i++) ans[11 + i] = time[i];
  ans[19] = '\0';
  return ans;
}
