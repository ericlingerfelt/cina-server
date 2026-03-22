#include "include/ErrObj.h"

// Global error stack
ErrObjList ErrObjStack;

string ErrObj::Export(const string &prefix, const string &postfix) const {
  string s(prefix);
  // large enough for a 64 bit int log10(2^64) + 2 for sign and NULL
  char n[22];
  n[0] = '\0';

  sprintf(n,"%d",line);
  s += file + ':' + n + " [";
  sprintf(n,"%d",type);
  s += n;
  s += "][";
  sprintf(n,"%d",val);
  s += n;
  s += "] " + cvsid + "\n" +
    prefix + "   " + msg + " (" + dmsg + ") " + str + postfix;
  return s;
}

string ErrObjList::print(const string &prefix, const string &postfix) {
  list<ErrObj>::iterator p;
  string s;

  for (p = l.begin(); p != l.end(); p++)
    s += prefix + p->Export("","") + postfix;
  return s;
}

void ErrTerminate() {
  cerr << "\nCall to terminate(), missing exception handler, " <<
    "ErrObjStack dump:\n" << ErrObjStack.print() << "dump complete\n";
  //abort();
}

void ErrUnexpected() {
  cerr << "\nCall to unexpected(), missing exception handler, " <<
    "ErrObjStack dump:\n" << ErrObjStack.print() << "dump complete\n";
  //abort();
}

void useErrObjHandlers() {
  set_terminate(ErrTerminate);
  set_unexpected(ErrUnexpected);
}
