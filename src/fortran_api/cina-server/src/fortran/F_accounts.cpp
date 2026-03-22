#include "include/CINA.h"
#include "utils.h"
#include <iostream>

/* Input: user and pw
   Output: alias, email, and err_msg
   err_msg is empty if successful
*/
extern "C" void get_account_(const CINAsessionControl **sc,
			     char *alias, char *email, char *err_msg,
			     int n1, int n2, int n3)
{
  string s;

  try {
    Connection con(CINA_DB_NAME,CINA_DB_HOST,CINA_DB_USER,CINA_DB_PW);
    Query q=con.query();

    if (*sc == 0) throw Err(MSG_BUG,"sc zero, getting account of empty session",0,0,"",
			   "$Id: F_accounts.cpp,v 1.1.1.1 2008/04/22 13:36:36 bucknerk Exp $");

    CINAaccount a(q,(*sc)->getUsername().c_str());

    strncpy(alias,a.getAlias().c_str(),n1);
    strncpy(email,a.getEmail().c_str(),n2);

    toFortran(alias,n1);
    toFortran(email,n2);
    err_msg[0] = '\0';  // Signal no error
  }
  catch (ErrObj *e) {
    SErrObjStack.add(*e);
    strncpy(err_msg,SErrObjStack.print().c_str(),n3);
  }
  catch (BadQuery er) { // handle any connection or
                          // query errors that may come up
    //strncpy(err_msg,er.error.c_str(),n3);
  }
  catch (BadConversion er) { // handle bad conversions
    s = "Error: Tried to convert \"" + er.data + "\" to a \"" +
         er.type_name + "\".\n";
    strncpy(err_msg,s.c_str(),n3);
  }
  catch (...) {
    strncpy(err_msg,"Unknown exception caught",n3);
  }
  toFortran(err_msg,n3);
  cout.flush();
}
