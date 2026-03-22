#include "include/sessions.h"
#include <custom.h>

sql_create_9(sessions, 1, 9,
	     string, Session,
	     string, Username,
	     string, Password,
	     unsigned int, Permissions,
	     unsigned short int, Status,
	     DateTime, LastRequest,
	     unsigned short int, NucDataBgPID,
	     unsigned short int, RateGenBgPID,
	     unsigned short int, ElemSynBgPID)

CINAsession::CINAsession(char *header, unsigned short int request_count,
			 bool compress_output) :
  Rnd(0), CompressOutput(compress_output), RequestCount(request_count), data(0)
{
  if (header == 0) Header[0] = '\0';
  else {
    strncpy(Header, header, sizeof(Header));
    Header[sizeof(Header) - 1] = '\0';
  }
}

CINAsession::~CINAsession()
{
  if (data) delete (sessions *) data;
}

const string &CINAsession::getID() const
{
  sessions *s = (sessions *) data;
  
  if (s) return s->Session;
  else {
    throw new Err(MSG_BUG,"getting ID of empty session",0,0,"",
		  "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");
  }
}

const string &CINAsession::getUsername() const
{
  sessions *s = (sessions *) data;
  
  if (s) return s->Username;
  else {
    throw new Err(MSG_BUG,"getting Username of empty session",0,0,"",
		  "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");
  }
}

const DateTime &CINAsession::getLastRequest() const
{
  sessions *s = (sessions *) data;
  
  if (s) return s->LastRequest;
  else {
    throw new Err(MSG_BUG,"getting LastRequest of empty session",0,0,"",
		  "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");
  }
}

int CINAsession::getNucDataBgPID() const
{
  sessions *s = (sessions *) data;
  
  if (s) return s->NucDataBgPID;
  else {
    throw new Err(MSG_BUG,"getting NucDataBgPID of empty session",0,0,"",
		  "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");
  }
}

int CINAsession::getRateGenBgPID() const
{
  sessions *s = (sessions *) data;
  
  if (s) return s->RateGenBgPID;
  else {
    throw new Err(MSG_BUG,"getting RateGenBgPID of empty session",0,0,"",
		  "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");
  }
}

int CINAsession::getElemSynBgPID() const
{
  sessions *s = (sessions *) data;
  
  if (s) return s->ElemSynBgPID;
  else {
    throw new Err(MSG_BUG,"getting ElemSynBgPID of empty session",0,0,"",
		  "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");
  }
}

CINAsessionControl::CINAsessionControl(char *header, unsigned short int request_count,
				       bool compress_output)
{
  if (header == 0) Header[0] = '\0';
  else {
    strncpy(Header, header, sizeof(Header));
    Header[sizeof(Header) - 1] = '\0';
  }
  CompressOutput = compress_output;
  RequestCount = request_count;
}

void CINAsessionControl::LoadSession(Query &q, const string &session,
				     const string &username, const string &pw)
{
  sessions *s = (sessions *) data;
  
  try {
    q << "select * from sessions where Session='" << escape << session << "' AND Username='"
      << escape << username << "' AND Password='" << escape << pw << "'";
    Result res = q.store();
    if (res.empty()) throw new Err(MSG_AUTHENCATION,"invalid session ID, username, or password",0,0,session,
				   "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");
    if (s == 0) s = new sessions;
    *s = res.at(0);
    //*s = res[0];
    data = s;
  } catch (BadQuery er) {
    throw new Err(MSG_BUG,"BadQuery",0,0,er.what(),
		  "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");
  } catch (BadConversion er) {
    throw new Err(MSG_BUG,"BadConversion",0,0,er.data +" to a "+ er.type_name,
		  "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");
  } catch (bad_alloc xa) {
    throw new Err(MSG_MEMORY,"Out of memory",0,0,"",
		  "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");
  }
}

void CINAsessionControl::UpdateLastRequestDate(Query &q)
{
  sessions *s = (sessions *) data;
  
  if (s) {
    try {
      // Put current time and date into LastRequest
      CINAgetDateTime(s->LastRequest);

      // Set Rnd
      timeval tv;
      if (gettimeofday(&tv,0))
	throw new Err(MSG_UNEXPECTED,"gettimeofday returned nonzero",0,-1,"",
		      "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");
      Rnd = (tv.tv_usec) % 65536;  // 65536 used because that is all that will fit in the SMALLINT db type and an unsigned short int
      
      q << "update sessions set LastRequest='" << s->LastRequest
	<< "' where Session='" << escape << s->Session << "'";
      q.execute();
    } catch (BadQuery er) {
      throw new Err(MSG_BUG,"BadQuery",0,0,er.what(),
		    "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");
    } catch (BadConversion er) {
      throw new Err(MSG_BUG,"BadConversion",0,0,er.data + " to a " +
		    er.type_name,
		    "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");
    }
  } else {
    throw new Err(MSG_BUG,"Updating NucDataBgPID for empty session",0,0,"",
		  "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");
  }
}

void CINAsessionControl::UpdateNucDataBgPID(Query &q, unsigned short int pid)
{
  sessions *s = (sessions *) data;
  
  if (s) {
    try {
      q << "update sessions set NucDataBgPID=" << pid << " where Session='"
	<< escape << s->Session << "'";
      q.execute();
      s->NucDataBgPID = pid;
    } catch (BadQuery er) {
      throw new Err(MSG_BUG,"BadQuery",0,0,er.what(),
		    "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");
    } catch (BadConversion er) {
      throw new Err(MSG_BUG,"BadConversion",0,0,er.data + " to a " +
		    er.type_name,
		    "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");
    }
  } else {
    throw new Err(MSG_BUG,"Updating NucDataBgPID for empty session",0,0,"",
		  "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");
  }
}

void CINAsessionControl::UpdateRateGenBgPID(Query &q, unsigned short int pid)
{
  sessions *s = (sessions *) data;
  
  if (s) {
    try {
      q << "update sessions set RateGenBgPID=" << pid << " where Session='"
	<< escape << s->Session << "'";
      q.execute();
      s->RateGenBgPID = pid;
    } catch (BadQuery er) {
      throw new Err(MSG_BUG,"BadQuery",0,0,er.what(),
		    "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");
    } catch (BadConversion er) {
      throw new Err(MSG_BUG,"BadConversion",0,0,er.data + " to a " +
		    er.type_name,
		    "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");
    }
  } else {
    throw new Err(MSG_BUG,"Updating RateGenBgPID for empty session",0,0,"",
		  "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");
  }
}

void CINAsessionControl::UpdateElemSynBgPID(Query &q, unsigned short int pid)
{
  sessions *s = (sessions *) data;
  
  if (s) {
    try {
      q << "update sessions set ElemSynBgPID=" << pid << " where Session='"
	<< escape << s->Session << "'";
      q.execute();
      s->ElemSynBgPID = pid;
    } catch (BadQuery er) {
      throw new Err(MSG_BUG,"BadQuery",0,0,er.what(),
		    "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");
    } catch (BadConversion er) {
      throw new Err(MSG_BUG,"BadConversion",0,0,er.data + " to a " +
		    er.type_name,
		    "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");
    }
  } else {
    throw new Err(MSG_BUG,"Updating ElemSynBgPID for empty session",0,0,"",
		  "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");
  }
}

void CINAsessionControl::NewSession(Query &q, const string &username, const string &pw)
{
  sessions *s = (sessions *) data;
  int i;
  
  try {
    CINAaccountGetAll acnt;
    // Load user's data
    //throw new Err(MSG_AUTHENCATION,"&" + username + "&",0,0,"^" + pw + "^","NO ID");
    //return;
    acnt.dbGet(q, username.c_str());

    if (!acnt.isPwGood(pw)) throw new Err(MSG_AUTHENCATION,"invalid password",0,0,"", "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");

//     if (!acnt.isPwExpired(pw)) throw new Err("Your password has expired.  Please change your password and contact coordinator@nucastrodata.org for more help.",
//                                              "password expired",0,0,pw,
// 					        "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");

    if (s == 0) {
      s = new sessions;
      data = s;
    }

    // Put current time and date into LastRequest
    CINAgetDateTime(s->LastRequest);
    // Set Rnd
    timeval tv;
    if (gettimeofday(&tv,0))
      throw new Err(MSG_UNEXPECTED,"gettimeofday returned nonzero",0,-1,"",
		    "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");
    Rnd = (tv.tv_usec) % 65536;  // 65536 used because that is all that will fit in the SMALLINT db type and an unsigned short int
    
    s->Username = username;
    s->Password = acnt.getPassword();
    s->Permissions = acnt.getPermissions();
    s->Status = acnt.getStatus();
    s->NucDataBgPID = 0;
    s->RateGenBgPID = 0;
    s->ElemSynBgPID = 0;
    s->Session.erase();
    
    // Password is good so now check if MaxLogins will allow another session
    q << "lock tables sessions write";
    q.execute();
    q << "select count(*) from sessions where Username='"
      << escape << username << "'";
    Result res = q.store();
    Row row = res.at(0);
    //Row row = res[0];
    if ((unsigned short int) row.at(0) >= acnt.getMaxLogins()) {
    //if ((unsigned short int) row[0] >= acnt.getMaxLogins()) {
      q << "unlock tables";
      q.execute();
      throw new Err("You have too many open login sessions to open a new one.  If you would like your limit increased, please contact coordinator@nucastrodata.org","reached MaxLogins",0,acnt.getMaxLogins(),username,
		    "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");
    }

    // Ok all checks passed, user may now have a new session
    // Save with a unique SessionID
    i = 1;
    while (s->Session.empty()) {
      generateSessionID(s->Session);
      q << "select count(*) from sessions where Session='"
	<< escape << s->Session << "'";
      res = q.store();
      row = res.at(0);
      //row = res[0];
      if ((int) row.at(0) > 0) s->Session.erase();
      //if ((int) row[0] > 0) s->Session.erase();
      if (i++ > MAX_TRIES_FOR_NEW_SESSION) {
	q << "unlock tables";
	q.execute();
	throw new Err("Unable to open a new session.  The coordinator has been notified and should have the problem fixed shortly.",
		      "reached MAX_TRIES_FOR_NEW_SESSION",0,i,username,
		      "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");
      }    
    }

    // Finally insert new session into databse
    q.insert(*s);
    q.execute();
    // Unlock sessions table
    q << "unlock tables";
    q.execute();
  } catch (BadQuery er) {
    throw new Err(MSG_BUG,"BadQuery",0,0,er.what(),
		  "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");
  } catch (BadConversion er) {
    throw new Err(MSG_BUG,"BadConversion",0,0,er.data +" to a "+ er.type_name,
		  "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");
  } catch (bad_alloc xa) {
    throw new Err(MSG_MEMORY,"Out of memory",0,0,"",
		  "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");
  }
}

void CINAsessionControl::CloseSession(Query &q)
{
  sessions snew, *s = (sessions *) data;

  if (s) {
    try {
      // Get latest user info from database
      q << "lock tables sessions write";
      q.execute();

      q << "select " << snew.field_list() << " from sessions where Session='"
	<< escape << s->Session << "' AND Username='"
	<< escape << s->Username << "' AND Password='"
	<< escape << s->Password << "'";

      Result res = q.store();
      if (res.empty()) {
	q << "unlock tables";
	q.execute();
	throw new Err(MSG_AUTHENCATION,"invalid session ID, username, or password",0,0,s->Session,
		      "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");
      }
      snew = res.at(0);
      //snew = res[0];

      // Close programs running in the background

      if (snew.NucDataBgPID > 0)
	if (kill(snew.NucDataBgPID, SIGTERM) == 0) snew.NucDataBgPID = 0;

      if (snew.RateGenBgPID > 0)
	if (kill(snew.RateGenBgPID, SIGTERM) == 0) snew.RateGenBgPID = 0;

      if (snew.ElemSynBgPID > 0)
	if (kill(snew.ElemSynBgPID, SIGTERM) == 0) snew.ElemSynBgPID = 0;
      
      if (snew.NucDataBgPID || snew.RateGenBgPID || snew.ElemSynBgPID) {
	char err_str[40];
	sprintf(err_str,"%d,%d,%d",snew.NucDataBgPID,snew.RateGenBgPID,snew.ElemSynBgPID);
	throw new Err(MSG_UNEXPECTED,"Background process didn't shutdown",0,0,err_str,
		      "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");
      }

      // Delete session from database table
      q << "delete from sessions where Session='"
	<< escape << s->Session << "' AND Username='"
	<< escape << s->Username << "' AND Password='"
	<< escape << s->Password << "' limit 1";
      q.execute();
      q << "unlock tables";
      q.execute();

    } catch (BadQuery er) {
      throw new Err(MSG_BUG,"BadQuery",0,0,er.what(),
		    "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");
    } catch (BadConversion er) {
      throw new Err(MSG_BUG,"BadConversion",0,0,er.data + " to a " +
		    er.type_name,
		    "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");
    } catch (ErrObj *e) {
      throw e;
    }
  } else {
    throw new Err(MSG_BUG,"Closing empty session",0,0,"",
		  "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");
  }
}

void CINAsessionControl::BlankSession()
{
  sessions *s = (sessions *) data;

  try {
    if (s == 0) {
      s = new sessions;
      s->Session = "?";
      s->Username = "?";
      s->Password = "?";
      s->Permissions = 0;
      s->Status = 0;
      CINAgetDateTime(s->LastRequest);
      s->NucDataBgPID = 0;
      s->RateGenBgPID = 0;
      s->ElemSynBgPID = 0;
      data = s;

      // Set Rnd
      timeval tv;
      if (gettimeofday(&tv,0))
	throw new Err(MSG_UNEXPECTED,"gettimeofday returned nonzero",0,-1,"",
		      "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");
      Rnd = (tv.tv_usec) % 65536;  // 65536 used because that is all that will fit in the SMALLINT db type and an unsigned short int
      Header[0] = '?';
      Header[1] = '\0';
    }
  } catch (bad_alloc xa) {
    throw new Err(MSG_MEMORY,"Out of memory",0,0,"",
		  "$Id: sessions.cpp,v 1.3 2008/04/25 17:55:18 bucknerk Exp $");
  }
}

// Generate random alphanumeric string where each character can have
// 62 possible values (26 lowercase, 26 uppercase, 10 numbers)
void CINAsessionControl::generateSessionID(string &s)
{
  int i,v;
  s.erase();
  for (i = 0; i < SESSION_ID_LEN; i++) {
    v = (int) (61.0 * random() / RAND_MAX);
    if      (v >= 0  && v <= 9 ) s += v + 48;  // number from 0 to 9
    else if (v >= 10 && v <= 35) s += v + 55;  // letter from A to Z
    else s += v + 61;                          // letter from a to z
  }
}

