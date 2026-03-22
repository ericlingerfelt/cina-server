
#ifndef __CINA_LOGS_H__
#define __CINA_LOGS_H__

#include <sys/time.h>
#include <string>
#include <list>
#include <mysql++.h>
//#include <libxml++/libxml++.h>

using namespace std;
using namespace mysqlpp;

#include "include/ErrObj.h"
#include "include/datetime.h"
#include "include/sessions.h"

class SErrObjList : public ErrObjList {
 public:
  void save(Query &q, const CINAsession &s);
};

// Global stack for storing errors
extern SErrObjList SErrObjStack;

class AccessLog {
 public:
  AccessLog(): access(0), astack(0) {}
  ~AccessLog();

  // init sets access and allocates empty stack
  void init(const CINAsession &s);
  void add(const string &request);
  void save(Query &q, CINAsessionControl &s);
  void print(ostream &stream = cout);
  //Glib::ustring body;
  string body;
 protected:
  void *access;
  void *astack;
};

#endif // ifndef __CINA_LOGS_H__
