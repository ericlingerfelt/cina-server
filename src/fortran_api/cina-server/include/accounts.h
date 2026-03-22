
#ifndef __CINA_ACCOUNTS_H__
#define __CINA_ACCOUNTS_H__

#include <ctime>
#include <mysql++.h>

using namespace std;
using namespace mysqlpp;

#include "include/datetime.h"
#include "include/ErrObj.h"

const int CINA_DAYS_PW_GOOD = 365/4; // 0 means no expiration

class CINAaccount {
 public:
  // A constructor without arguments will have undeclared date values to
  // save time, specify at least one argument to have valid date values
  inline CINAaccount() : data(0) {}

  CINAaccount(const string &username, const string &alias="",
	      const string &password="", const string &email="",
	      const Date &createdate="1970-01-01",
	      const Date &pwchangedate="1970-01-01",
	      const unsigned short int maxlogins=5,
	      const unsigned long int permissions=0,
	      const unsigned short int status=0,
	      const unsigned short int timeout=180);
  
  CINAaccount(Query &q, const char *username);

  ~CINAaccount();
  void print(ostream &stream=cout);
  CINAaccount &dbGet(Query &q, const char *username);
  CINAaccount &dbReplace(Query &q);
  CINAaccount &dbInsert(Query &q);

  const string &getUsername() const;
  const string &getAlias() const;
  const string &getEmail() const;
  const Date &getCreateDate() const;
  const unsigned short int getMaxLogins() const;
  const unsigned short int getTimeout() const;

  bool isPwExpired() const;
  bool isPwGood(const string &pw) const;

 protected:
  void *data;
};

class CINAaccountGetAll : public CINAaccount {
 public:
  inline CINAaccountGetAll() {}
  inline ~CINAaccountGetAll() {}

  inline CINAaccountGetAll(const string &username, const string &alias="",
			   const string &password="", const string &email="",
			   const Date &createdate="1970-01-01",
			   const Date &pwchangedate="1970-01-01",
			   const unsigned short int maxlogins=5,
			   const unsigned long int permissions=0,
			   const unsigned short int status=0,
			   const unsigned short int timeout=180) :
    CINAaccount(username,alias,password,email,createdate,pwchangedate,
		maxlogins,permissions,status,timeout) {}

  const string &getPassword() const;
  const Date   &getPwChangeDate() const;
  const unsigned long  int getPermissions() const;
  const unsigned short int getStatus() const;
};

#endif // ifndef __CINA_ACCOUNTS_H__
