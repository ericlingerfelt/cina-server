#include "include/accounts.h"
#include <custom.h>

sql_create_10(accounts, 1, 10,
	      string, Username,
	      string, Alias,
	      string, Password,
	      string, Email,
	      Date,   CreateDate,
	      Date,   PwChangeDate,
	      unsigned short int, MaxLogins,
	      unsigned long  int, Permissions,
	      unsigned short int, Status,
	      unsigned short int, Timeout)

CINAaccount::CINAaccount(const string &username, const string &alias,
			 const string &password, const string &email,
			 const Date &createdate, const Date &pwchangedate,
			 const unsigned short int maxlogins,
			 const unsigned long int permissions,
			 const unsigned short int status,
			 const unsigned short int timeout)
{
  accounts *d;

  try {
    d = new accounts;
    d->Username = username;
    d->Alias = alias;
    d->Password = password;
    d->Email = email;
    d->MaxLogins = maxlogins;
    d->Permissions = permissions;
    d->Status = status;
    d->Timeout = timeout;
    if (createdate.compare("1970-01-01") == 0) CINAgetDate(d->CreateDate);
    else d->CreateDate = createdate;
    
    if (pwchangedate.compare("1970-01-01") == 0)
      CINAgetDate(d->PwChangeDate,CINA_DAYS_PW_GOOD);
    else d->PwChangeDate = pwchangedate;
    data = d;
  } catch (bad_alloc xa) {
    throw new Err(MSG_MEMORY,"Out of memory",0,0,"",
		  "$Id: accounts.cpp,v 1.3 2008/04/25 17:55:17 bucknerk Exp $");
  }
}

CINAaccount::CINAaccount(Query &q, const char *username)
{
  accounts *d;

  try {
    if (username == 0) throw new Err(MSG_BUG,"username=0",0,0,"",
				     "$Id: accounts.cpp,v 1.3 2008/04/25 17:55:17 bucknerk Exp $");
    q << "select * from accounts where Username='" << escape << username << "'";

    Result res = q.store();
    if (res.empty()) throw new Err(MSG_BUG,"invalid username",0,0,username,
				   "$Id: accounts.cpp,v 1.3 2008/04/25 17:55:17 bucknerk Exp $");
    d = new accounts;
    *d = res.at(0);
    //*d = res[0];
    data = d;
  } catch (BadQuery er) {
    throw new Err(MSG_BUG,"BadQuery",0,0,er.what(),
		  "$Id: accounts.cpp,v 1.3 2008/04/25 17:55:17 bucknerk Exp $");
  } catch (BadConversion er) {
    //throw new Err(MSG_BUG,"BadConversion",0,0,er.what() +" to a "+ er.type_name,
		  //"$Id: accounts.cpp,v 1.3 2008/04/25 17:55:17 bucknerk Exp $");
  } catch (bad_alloc xa) {
    throw new Err(MSG_MEMORY,"Out of memory",0,0,"",
		  "$Id: accounts.cpp,v 1.3 2008/04/25 17:55:17 bucknerk Exp $");
  }
}

CINAaccount::~CINAaccount() 
{
  if (data) delete (accounts *) data;
}

void CINAaccount::print(ostream &stream)
{
  accounts *d = (accounts *) data;

  if (d) {
    stream << "CINAaccount for " << d->Username
	   << ":\n  Alias = " << d->Alias
	   << "\n  Password = " << d->Password
	   << "\n  Email = " << d->Email
	   << "\n  CreateDate = " << d->CreateDate
	   << "\n  PwChangeDate = " << d->PwChangeDate
	   << "\n  MaxLogins = " << d->MaxLogins
	   << "\n  Permissions = " << d->Permissions
	   << "\n  Status = " << d->Status
	   << "\n  Timeout = " << d->Timeout << endl;
  } else {
    stream << "CINAaccount empty" << endl;
  }
}

CINAaccount &CINAaccount::dbGet(Query &q, const char *username)
{
  accounts *d = (accounts *) data;

  try {
    if (username == 0) throw new Err(MSG_BUG,"username=0",0,0,"",
				     "$Id: accounts.cpp,v 1.3 2008/04/25 17:55:17 bucknerk Exp $");
    q << "select * from accounts where Username='" << escape << username << "'";

    Result res = q.store();
    if (res.empty()) throw new Err(MSG_BUG,"invalid username",0,0,username,
				   "$Id: accounts.cpp,v 1.3 2008/04/25 17:55:17 bucknerk Exp $");
    if (d == 0) d = new accounts;
    *d = res.at(0);
    //*d = res[0];
    data = d;
  } catch (BadQuery er) {
    throw new Err(MSG_BUG,"BadQuery",0,0,er.what(),
		  "$Id: accounts.cpp,v 1.3 2008/04/25 17:55:17 bucknerk Exp $");
  } catch (BadConversion er) {
    throw new Err(MSG_BUG,"BadConversion",0,0,er.what(),
		  "$Id: accounts.cpp,v 1.3 2008/04/25 17:55:17 bucknerk Exp $");
  } catch (bad_alloc xa) {
    throw new Err(MSG_MEMORY,"Out of memory",0,0,"",
		  "$Id: accounts.cpp,v 1.3 2008/04/25 17:55:17 bucknerk Exp $");
  }
  return *this;
}

CINAaccount &CINAaccount::dbReplace(Query &q)
{
  accounts *d = (accounts *) data;

  if (d) {
    try {
      q.replace(*d);
      q.execute();
    } catch (BadQuery er) {
      throw new Err(MSG_BUG,"BadQuery",0,0,er.what(),
		    "$Id: accounts.cpp,v 1.3 2008/04/25 17:55:17 bucknerk Exp $");
    } catch (BadConversion er) {
      throw new Err(MSG_BUG,"BadConversion",0,0,er.data + " to a " +
		    er.type_name,
		    "$Id: accounts.cpp,v 1.3 2008/04/25 17:55:17 bucknerk Exp $");
    }
  } else {
    throw new Err(MSG_BUG,"Saving empty account",0,0,"",
		  "$Id: accounts.cpp,v 1.3 2008/04/25 17:55:17 bucknerk Exp $");
  }
  return *this;
}

CINAaccount &CINAaccount::dbInsert(Query &q)
{
  accounts *d = (accounts *) data;

  if (d) {
    try {
      q.insert(*d);
      q.execute();
    } catch (BadQuery er) {
      throw new Err(MSG_BUG,"BadQuery",0,0,er.what(),
		    "$Id: accounts.cpp,v 1.3 2008/04/25 17:55:17 bucknerk Exp $");
    } catch (BadConversion er) {
      throw new Err(MSG_BUG,"BadConversion",0,0,er.data + " to a " +
		    er.type_name,
		    "$Id: accounts.cpp,v 1.3 2008/04/25 17:55:17 bucknerk Exp $");
    }
  } else {
    throw new Err(MSG_BUG,"Saving empty account",0,0,"",
		  "$Id: accounts.cpp,v 1.3 2008/04/25 17:55:17 bucknerk Exp $");
  }
  return *this;
}

const string &CINAaccount::getUsername() const
{
  accounts *d = (accounts *) data;

  if (d) return d->Username;
  else {
    throw new Err(MSG_BUG,"Called getUsername on empty object",0,0,"",
		  "$Id: accounts.cpp,v 1.3 2008/04/25 17:55:17 bucknerk Exp $");
  }
}

const string &CINAaccount::getAlias() const
{
  accounts *d = (accounts *) data;

  if (d) return d->Alias;
  else {
    throw new Err(MSG_BUG,"Called getAlias on empty object",0,0,"",
		  "$Id: accounts.cpp,v 1.3 2008/04/25 17:55:17 bucknerk Exp $");
  }
}

const string &CINAaccount::getEmail() const
{
  accounts *d = (accounts *) data;

  if (d) return d->Email;
  else {
    throw new Err(MSG_BUG,"Called getEmail on empty object",0,0,"",
		  "$Id: accounts.cpp,v 1.3 2008/04/25 17:55:17 bucknerk Exp $");
  }
}

const Date &CINAaccount::getCreateDate() const
{
  accounts *d = (accounts *) data;

  if (d) return d->CreateDate;
  else {
    throw new Err(MSG_BUG,"Called getCreateDate on empty object",0,0,"",
		  "$Id: accounts.cpp,v 1.3 2008/04/25 17:55:17 bucknerk Exp $");
  }
}

const unsigned short int CINAaccount::getMaxLogins() const
{
  accounts *d = (accounts *) data;

  if (d) return d->MaxLogins;
  else {
    throw new Err(MSG_BUG,"Called getMaxLogins on empty object",0,0,"",
		  "$Id: accounts.cpp,v 1.3 2008/04/25 17:55:17 bucknerk Exp $");
  }
}
  
const unsigned short int CINAaccount::getTimeout() const
{
  accounts *d = (accounts *) data;

  if (d) return d->Timeout;
  else {
    throw new Err(MSG_BUG,"Called getTimeout on empty object",0,0,"",
		  "$Id: accounts.cpp,v 1.3 2008/04/25 17:55:17 bucknerk Exp $");
  }
}
  

bool CINAaccount::isPwExpired() const
{
  accounts *d = (accounts *) data;
  Date now;

  if (d) {
    if (d->PwChangeDate.compare(CINAgetDate(now)) > 0) return true;
    else return false;
  } else {
    throw new Err(MSG_BUG,"Called isPwExpired on empty object",0,0,"",
		  "$Id: accounts.cpp,v 1.3 2008/04/25 17:55:17 bucknerk Exp $");
  }
}

bool CINAaccount::isPwGood(const string &pw) const
{
  accounts *d = (accounts *) data;

  if (d) {
    if (pw == d->Password) return true;
    else return false;
  }
  else {
    throw new Err(MSG_BUG,"Called isPwGood on empty object",0,0,pw.c_str(),
		  "$Id: accounts.cpp,v 1.3 2008/04/25 17:55:17 bucknerk Exp $");
  }
}

const string &CINAaccountGetAll::getPassword() const
{
  accounts *d = (accounts *) data;

  if (d) return d->Password;
  else {
    throw new Err(MSG_BUG,"Called getPassword on empty object",0,0,"",
		  "$Id: accounts.cpp,v 1.3 2008/04/25 17:55:17 bucknerk Exp $");
  }
}

const Date &CINAaccountGetAll::getPwChangeDate() const
{
  accounts *d = (accounts *) data;

  if (d) return d->PwChangeDate;
  else {
    throw new Err(MSG_BUG,"Called getPwChangeDate on empty object",0,0,"",
		  "$Id: accounts.cpp,v 1.3 2008/04/25 17:55:17 bucknerk Exp $");
  }
}

const unsigned long int CINAaccountGetAll::getPermissions() const
{
  accounts *d = (accounts *) data;

  if (d) return d->Permissions;
  else {
    throw new Err(MSG_BUG,"Called getPermissions on empty object",0,0,"",
		  "$Id: accounts.cpp,v 1.3 2008/04/25 17:55:17 bucknerk Exp $");
  }
}

const unsigned short int CINAaccountGetAll::getStatus() const
{
  accounts *d = (accounts *) data;

  if (d) return d->Status;
  else {
    throw new Err(MSG_BUG,"Called getStatus on empty object",0,0,"",
		  "$Id: accounts.cpp,v 1.3 2008/04/25 17:55:17 bucknerk Exp $");
  }
}

