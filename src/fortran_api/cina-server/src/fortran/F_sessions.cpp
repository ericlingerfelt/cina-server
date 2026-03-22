#include "include/CINA.h"
#include "utils.h"
#include <iostream>
#include <cstdlib>
#include <ctime>

/* Input: user and pw
   Output: id and err_msg
   err_msg is empty if successful
*/
extern "C" void new_session_(const char *user, const char *pw,
			     const char *request, const char *body,
			     char *id, CINAsessionControl **sc, char *err_msg,
			     int n1, int n2, int n3, int n4, int n5, int n6)
{
  string s;
  try {
    AccessLog alog;
    string User,Pw,Request,Body;
  
    //if (*sc != 0) throw new Err(MSG_BUG,"sc not zero, loading sessions twice?",0,(int)*sc,"",
	//		   "$Id: F_sessions.cpp,v 1.1.1.1 2008/04/22 13:36:36 bucknerk Exp $");
    
    *sc = new CINAsessionControl;
    //cout << "new_session::sc=" << *sc << endl;
    (*sc)->BlankSession();

    // get system time for seeding random number generator
    srandom(time(0));
    
    Connection con(CINA_DB_NAME,CINA_DB_HOST,CINA_DB_USER,CINA_DB_PW);
    Query q=con.query();
    
    fromFortran(User,user,n1);
    fromFortran(Pw,pw,n2);
    fromFortran(Request,request,n3);
    fromFortran(Body,body,n4);
    alog.body = Body;
    
    try {
      (*sc)->NewSession(q,User,Pw);
    
      alog.init(**sc);
      alog.add(Request);
    
      strncpy(id,(*sc)->getID().c_str(),n5);
      toFortran(id,n5);
      err_msg[0] = '\0';
    }
    catch (ErrObj *e) {
      alog.save(q,**sc);
      throw;
    }
    alog.save(q,**sc);
  }
  catch (ErrObj *e) {
    SErrObjStack.add(*e);
    strncpy(err_msg,SErrObjStack.print().c_str(),n6);
  }
  catch (BadQuery er) { // handle any connection or
    // query errors that may come up
    //strncpy(err_msg,er.error.c_str(),n6);
  }
  catch (BadConversion er) { // handle bad conversions
    s = "Error: Tried to convert \"" + er.data + "\" to a \"" +
      er.type_name + "\".\n";
    strncpy(err_msg,s.c_str(),n6);
  }
  catch (...) {
    strncpy(err_msg,"Unknown exception caught",n6);
  }
  toFortran(err_msg,n6);
  cout.flush();
  cerr.flush();
}

/* Input: user and pw
   Output: id and err_msg
   err_msg is empty if successful
*/
extern "C" void close_session_(CINAsessionControl **sc, char *err_msg,
			       int n1)
{
  string s;
  try {
    Connection con(CINA_DB_NAME,CINA_DB_HOST,CINA_DB_USER,CINA_DB_PW);
    Query q=con.query();

    //if (*sc == 0) throw Err(MSG_BUG,"sc zero, closing empty session",0,0,"",
	//		   "$Id: F_sessions.cpp,v 1.1.1.1 2008/04/22 13:36:36 bucknerk Exp $");

    (*sc)->CloseSession(q);
    delete *sc;
    *sc = 0;
    err_msg[0] = '\0';
  }
catch (ErrObj *e) {
    SErrObjStack.add(*e);
    strncpy(err_msg,SErrObjStack.print().c_str(),n1);
  }
  catch (BadQuery er) { // handle any connection or
                          // query errors that may come up
    //strncpy(err_msg,er.error.c_str(),n1);
  }
  catch (BadConversion er) { // handle bad conversions
    s = "Error: Tried to convert \"" + er.data + "\" to a \"" +
         er.type_name + "\".\n";
    strncpy(err_msg,s.c_str(),n1);
  }
  catch (...) {
    strncpy(err_msg,"Unknown exception caught",n1);
  }
  toFortran(err_msg,n1);
  cout.flush();
  cerr.flush();
}

/* Input: user and pw
   Output: id and err_msg
   err_msg is empty if successful
*/
extern "C" void valid_session_(const char *user, const char *pw,
			       const char *request, const char *body,
			       const char *id, CINAsessionControl **sc, char *err_msg,
			       int n1, int n2, int n3, int n4, int n5, int n6)
{
  string s;
  try {
    AccessLog alog;
    string User,Pw,Id,Request,Body;

    //if (*sc != 0) throw Err(MSG_BUG,"sc not zero, loading sessions twice?",0,(int)*sc,"",
	//		   "$Id: F_sessions.cpp,v 1.1.1.1 2008/04/22 13:36:36 bucknerk Exp $");
    
    *sc = new CINAsessionControl;
    //cout << "new_session::*sc=" << *sc << endl;
    (*sc)->BlankSession();

    Connection con(CINA_DB_NAME,CINA_DB_HOST,CINA_DB_USER,CINA_DB_PW);
    Query q=con.query();

    fromFortran(User,user,n1);
    fromFortran(Pw,pw,n2);
    fromFortran(Request,request,n3);
    fromFortran(Body,body,n4);
    fromFortran(Id,id,n5);
    alog.body = Body;
     
    try {
      (*sc)->LoadSession(q,Id,User,Pw);
      (*sc)->UpdateLastRequestDate(q);
      
      alog.init(**sc);
      alog.add(Request);
      err_msg[0] = '\0';
    }
    catch (ErrObj *e) {
      alog.save(q,**sc);
      throw;
    }
    alog.save(q,**sc);
   }
  catch (ErrObj *e) {
    SErrObjStack.add(*e);
    strncpy(err_msg,SErrObjStack.print().c_str(),n6);
  }
  catch (BadQuery er) { // handle any connection or
                          // query errors that may come up
    //strncpy(err_msg,er.error.c_str(),n6);
  }
  catch (BadConversion er) { // handle bad conversions
    s = "Error: Tried to convert \"" + er.data + "\" to a \"" +
         er.type_name + "\".\n";
    strncpy(err_msg,s.c_str(),n6);
  }
  catch (...) {
    cout << "e4" << endl;
    strncpy(err_msg,"Unknown exception caught",n6);
  }
  toFortran(err_msg,n6);
  cout.flush();
  cerr.flush();
}
