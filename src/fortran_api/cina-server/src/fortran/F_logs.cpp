#include "include/CINA.h"
#include "utils.h"
#include <iostream>

extern "C" void log_error_(CINAsessionControl **sc,
			   const char *client_msg,const char *debug_msg,
			   const int *type, const int *val, 
			   const char *str, const char *cvsid,
			   int n1, int n2, int n3, int n4)
{
  try {
    string Client_msg,Debug_msg,Str,CVSid;
    
    fromFortran(Client_msg,client_msg,n1);
    fromFortran(Debug_msg,debug_msg,n2);
    fromFortran(Str,str,n3);
    fromFortran(CVSid,cvsid,n4);

    ErrObj *eo = new Err(Client_msg,Debug_msg,*type,*val,Str,CVSid.c_str());
    SErrObjStack.add(*eo);
    
    try {
      Connection con(CINA_DB_NAME,CINA_DB_HOST,CINA_DB_USER,CINA_DB_PW);
      Query q=con.query();
      
      //cout << "log_error::*sc=" << *sc << endl;
      if (*sc == 0) {
	*sc = new CINAsessionControl;
	(*sc)->BlankSession();
	cerr << "sc was zero when logging error" << endl;
      }

      SErrObjStack.save(q,**sc);
    }
    catch (ErrObj *e) {
      SErrObjStack.add(*e);
    }
    catch (BadQuery er) { // handle any connection or
                          // query errors that may come up
      throw new Err(MSG_BUG,er.what(),0,0,"BadQuery",
		    "$Id: F_logs.cpp,v 1.1.1.1 2008/04/22 13:36:36 bucknerk Exp $");
    }
    catch (BadConversion er) { // handle bad conversions
      string s = "Error: Tried to convert \"" + er.data + "\" to a \"" +
	er.type_name + "\".\n";
      throw new Err(MSG_BUG,er.what(),0,0,"BadConversion",
		    "$Id: F_logs.cpp,v 1.1.1.1 2008/04/22 13:36:36 bucknerk Exp $");
    }
    catch (...) {
      throw new Err(MSG_BUG,"Unknown exception caught",0,0,"",
		    "$Id: F_logs.cpp,v 1.1.1.1 2008/04/22 13:36:36 bucknerk Exp $");
    }
  }
  catch (ErrObj *e) {
    SErrObjStack.add(*e);
    cerr << SErrObjStack.print();
    cerr.flush();
  }
  catch (...) {
    SErrObjStack.add(Err(MSG_BUG,"Unknown exception caught",0,0,"",
		 "$Id: F_logs.cpp,v 1.1.1.1 2008/04/22 13:36:36 bucknerk Exp $"));
    cerr << SErrObjStack.print();
    cerr.flush();
  }
}

