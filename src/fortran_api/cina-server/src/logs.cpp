#include "include/logs.h"
#include <custom.h>

sql_create_6(logs_error, 1, 6,
	     string, Session,
	     DateTime, Time,
	     unsigned short int, Rnd,
	     DateTime, TimeOfError,
	     unsigned short int, Depth,
	     string, Dmsg);

sql_create_13(logs_error_full, 1, 13,
	      string, Session,
	      DateTime, Time,
	      unsigned short int, Rnd,
	      unsigned short int, Depth,
	      string, file,
	      int, line,
	      int, type,
	      int, val,
	      string, cvsid,
	      string, msg,
	      string, dmsg,
	      string, str,
	      DateTime, datecompiled);

// Global stack for storing errors
SErrObjList SErrObjStack;

void SErrObjList::save(Query &q, const CINAsession &s)
{
  logs_error e;
  logs_error_full ef;
  DateTime now;
  int depth = 1;

  // This method must not throw any exceptions
  try {
    CINAgetDateTime(now);

    // session may not be loaded so catch errors
    try {
      
      e.Session = s.getID();
      e.Time = s.getLastRequest();
      e.Rnd = s.getRnd();
    } catch (...) {
      e.Time = now;
      // Set Rnd
      timeval tv;
      if (gettimeofday(&tv,0))
	throw new Err(MSG_UNEXPECTED,"gettimeofday returned nonzero",0,-1,"",
		      "$Id: logs.cpp,v 1.2 2008/04/25 13:14:35 bucknerk Exp $");
      e.Rnd = (tv.tv_usec) % 65536;  // 65536 used because that is all that will fit in the SMALLINT db type and an unsigned short int
    }

    e.TimeOfError = now;
    ef.Session = e.Session;
    ef.Time = e.Time;
    ef.Rnd = e.Rnd;

    list<ErrObj>::iterator p;
    string date,time;
    p = l.begin();
    if (!l.empty()) e.Dmsg = p->getDMsg();

    for (; p != l.end(); p++) {
      ef.Depth = depth++;
      ef.file = p->getFile();
      ef.line = p->getLine();
      ef.type = p->getType();
      ef.val = p->getVal();
      ef.cvsid = p->getCVSid();
      ef.msg = p->getMsg();
      ef.dmsg = p->getDMsg();
      ef.str = p->getStr();
      // Get datecompiled from two strings
      //ef.datecompiled = CINAmakeDbTime(p->getDate(), p->getTime());

      // All values in ef are filled so save to logs_error_full table
      q.insert(ef);
      q.execute();
    }
    e.Depth = --depth;

    // All values in e are filled so save to logs_error table
    q.insert(e);
    q.execute();

  } catch (...) {
    cerr << "Unable to save error stack to database.  stderr dump:" << endl;
    cerr << this->print();
  }
}

sql_create_6(logs_access, 1, 6,
	     string, Session,
	     DateTime, Time,
	     unsigned short int, Rnd,
	     string, Username,
	     string, IP,
	     unsigned short int, Depth);

sql_create_5(logs_access_stack, 1, 5,
	     string, Session,
	     DateTime, Time,
	     unsigned short int, Rnd,
	     unsigned short int, Depth,
	     string, Request);

sql_create_4(logs_access_body, 1, 4,
	     string, Session,
	     DateTime, Time,
	     unsigned short int, Rnd,
	     string, Body);

AccessLog::~AccessLog()
{
  list<logs_access_stack> *lasList = (list<logs_access_stack> *) astack;

  if (access) delete (logs_access *) access;
  if (lasList) delete lasList;
}

void AccessLog::init(const CINAsession &s)
{
  logs_access *la = (logs_access *) access;

  if (astack || access) {
    throw new Err(MSG_BUG,"called init more than once?",0,0,"",
		  "$Id: logs.cpp,v 1.2 2008/04/25 13:14:35 bucknerk Exp $");
  } else {
    try {
      // Allocate and set access member
      la = new logs_access;
      la->Session = s.getID();
      CINAgetDateTime(la->Time);
      la->Rnd = s.getRnd();
      la->Username = s.getUsername();
      
      char *IPp = getenv("REMOTE_ADDR");
      if (IPp == 0) la->IP = "missing";
      else la->IP = IPp;
      
      la->Depth = 0;
      access = la;

      // Allocate and set stack member
      astack = new list<logs_access_stack>;
      
    } catch (bad_alloc xa) {
      throw new Err(MSG_MEMORY,"Out of memory",0,0,"",
		    "$Id: logs.cpp,v 1.2 2008/04/25 13:14:35 bucknerk Exp $");
    }
  }
}

void AccessLog::print(ostream &stream)
{
  list<logs_access_stack> *lasList = (list<logs_access_stack> *) astack;
  logs_access *la = (logs_access *) access;

  stream << "body: '" << body << "'" << endl << endl;

  if (lasList && la) {
    stream << "logs_access:" << endl
	 << "\t" << la->Session << " " << la->Time
	 << " (" << la->Rnd << ")" << endl
	 << "\tUsername: '" << la->Username << "'" << endl
	 << "\tIP: '" << la->IP << "'" << endl
	 << "\tDepth: '" << la->Depth << "'" << endl << endl
	 << "logs_access_stack: " << lasList->size() << endl;

    list<logs_access_stack>::iterator i;
    for (i = lasList->begin(); i != lasList->end(); i++) {
      stream << "\t" << i->Depth << "\t" << i->Session << " " << i->Time
	   << " (" << i->Rnd << ") '" << i->Request << "'" << endl;
    }

    stream << endl;
  }
  else stream << "access or astack is empty." << endl;
}

void AccessLog::add(const string &request)
{
  list<logs_access_stack> *lasList = (list<logs_access_stack> *) astack;
  logs_access *la = (logs_access *) access;

  if (lasList && la) {
    logs_access_stack las;

    las.Session = la->Session;
    las.Time = la->Time;
    las.Rnd = la->Rnd;
    las.Depth = ++(la->Depth);
    las.Request = request;
    lasList->push_back(las);
  } else {
    throw new Err(MSG_BUG,"Called add on empty object",0,0,"",
		  "$Id: logs.cpp,v 1.2 2008/04/25 13:14:35 bucknerk Exp $");
  }
}

void AccessLog::save(Query &q, CINAsessionControl &s)
{
  list<logs_access_stack> *lasList = (list<logs_access_stack> *) astack;
  logs_access *la = (logs_access *) access;

  try {
    // If this->init() was not called, then make up values so that access can be logged
    // Store made up values in s so that ErrorList will have save Session,Time, and Rnd as AccessList
    if (astack == 0 || access == 0) {
      s.BlankSession();
      this->init(s);
      lasList = (list<logs_access_stack> *) astack;
      la = (logs_access *) access;
    }

    // save body
    logs_access_body ab;
    ab.Session = la->Session;
    ab.Time = la->Time;
    ab.Rnd = la->Rnd;
    ab.Body = body;
    //body.erase();  // reduce memory useage?
    body.clear();  // reduce memory useage?
    q.insert(ab);
    q.execute();
    //ab.Body.erase();
    ab.Body.clear();
    
    // save access
    q.insert(*la);
    q.execute();
    
    // save astack
    list<logs_access_stack>::iterator i;
    for (i = lasList->begin(); i != lasList->end(); i++) {
      q.insert(*i);
      q.execute();
    }
  } catch (ErrObj *e) {
    SErrObjStack.add(*e);
    throw new Err("","Exception thrown from save",0,0,"",
		  "$Id: logs.cpp,v 1.2 2008/04/25 13:14:35 bucknerk Exp $");
  } catch (BadQuery er) {
    throw new Err(MSG_BUG,"BadQuery",0,0,er.what(),
		  "$Id: logs.cpp,v 1.2 2008/04/25 13:14:35 bucknerk Exp $");
  } catch (BadConversion er) {
    throw new Err(MSG_BUG,"BadConversion",0,0,er.data + " to a " +
		  er.type_name,
		  "$Id: logs.cpp,v 1.2 2008/04/25 13:14:35 bucknerk Exp $");
  }
}
