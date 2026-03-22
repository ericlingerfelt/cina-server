#ifndef __ERROBJ_H__
#define __ERROBJ_H__

#include <string>
#include <cstdio>
#include <list>
#include <exception>
#include <iostream>

using namespace std;

// Use Err instead of ErrObj for creating ErrObj objects
#define Err(Msg, DMsg, Type, Val, Str, CVSid) \
  ErrObj(__FILE__,__LINE__,__DATE__,__TIME__,Msg,DMsg,Type,Val,Str,CVSid)

#define MSG_AUTHENCATION "Your username or password is incorrect.  Please try again and contact coordinator@nucastrodata.org for more help."
#define MSG_UNEXPECTED "An unexpected error occurred on the server and your request could not be completed.  Details about the problem have been sent to the coordinator."
#define MSG_MISSING "The server is missing a feature required to complete your request."
#define MSG_MEMORY "The server ran out of memory and could not complete your request."
#define MSG_BUG "A software bug was found on the server and the coordinator has been notified with a bug report."

extern void ErrTerminate();
extern void ErrUnexpected();
extern void useErrObjHandlers();

class ErrObj {
 public:
  // Use Err instead of ErrObj for creating ErrObj objects
  inline ErrObj() : line(-1), type(-1), val(-1) {}

  inline ErrObj(const string File, const int Line,
		const string Date, const string Time,
		const string Msg, const string DMsg,
		const int Type, const int Val, const string Str,
		const char *CVSid) :
    file(File), line(Line), date(Date), time(Time),
    msg(Msg), dmsg(DMsg), type(Type), val(Val), str(Str), cvsid(CVSid) {}

  string Export(const string &prefix = "", const string &postfix = "\n") const;
  inline const string &getCVSid() const { return cvsid; }
  inline const string &getFile() const { return file; }
  inline const int getLine() const { return line; }
  inline const string &getDate() const { return date; }
  inline const string &getTime() const { return time; }
  inline const string &getMsg() const { return msg; }
  inline const string &getDMsg() const { return dmsg; }
  inline const int getType() const { return type; }
  inline const int getVal() const { return val; }
  inline const string &getStr() const { return str; }
 protected:
  const string file;
  const int line;
  const string date;  // Date file was compiled
  const string time;  // Time file was compiled
  const string msg;
  const string dmsg;
  const int type;
  const int val;
  const string str;
  const string cvsid;
};

class ErrObjList {
 public:
  //ErrObjList() {printf("Constructing ErrObjList\n");};

  //~ErrObjList() {printf("Destroying ErrObjList\n");};

  inline ErrObjList &add(const ErrObj &e) { l.push_back(e); return *this; }
  inline ErrObjList &clear() { l.clear(); return *this; }
  string print(const string &prefix = "  ", const string &postfix = "\n");
  inline const char *getUserMsg() { return l.front().getMsg().c_str(); }
  
 protected:
  list<ErrObj> l;
};

// Global error stack
extern ErrObjList ErrObjStack;

#endif // defined __ERROBJ_H__
