#ifndef __CINA_DATETIME_H__
#define __CINA_DATETIME_H__

#include <mysql++.h>

using namespace std;
using namespace mysqlpp;

#include "include/ErrObj.h"

extern Date CINAgetDate();
extern Date &CINAgetDate(Date &d, const int Days2add=0);

extern Time CINAgetTime();
extern Time &CINAgetTime(Time &time);

extern DateTime CINAgetDateTime();
extern DateTime &CINAgetDateTime(DateTime &d, const int Days2add=0);
// Date is in Mmm DD YYYY format and converted to YYYY-MM-DD, time is appended
extern DateTime CINAmakeDbTime(const string &date, const string &time);

#endif // ifndef __CINA_DATETIME_H__
