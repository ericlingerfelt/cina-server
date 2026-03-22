// Make sure you seed the random number generator with srandom() before calling NewSession()

#ifndef __CINA_SESSION_H__
#define __CINA_SESSION_H__

// needed for microseconds to guarantee a unique time for each CGI call
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>

#include <string>
#include <cstring>
#include <cstdlib>
#include <mysql++.h>

using namespace std;
using namespace mysqlpp;

#include "include/ErrObj.h"
#include "include/accounts.h"

const int SESSION_ID_LEN = 20;
const int MAX_TRIES_FOR_NEW_SESSION = 100;

// Class meant for read-only access to session information
// Values are set using CINAsessionControl class
// First call CINAsessionControl constructor with a
// header, request_count, and copress_output
// Then call either the NewSession method or
// LoadSession followed by UpdateLastRequestDate methods
class CINAsession {
 public:
  inline CINAsession() :
    Rnd(0), CompressOutput(false), RequestCount(0), data(0) { Header[0] = '\0'; }

  // header is copied to member variable
  CINAsession(char *header, unsigned short int request_count = 0,
		     bool compress_output = false);
  ~CINAsession();

  const string &getID() const;
  const string &getUsername() const;
  const DateTime &getLastRequest() const;

  // Returns -1 and throws exception if session is not loaded or valid
  int getNucDataBgPID() const;
  int getRateGenBgPID() const;
  int getElemSynBgPID() const;

  inline unsigned short int getRnd() const { return Rnd; }
  inline const char *getHeader() const { return Header; }
  inline bool isCompressOutput() const { return CompressOutput; }
  inline unsigned short int getRequestCount() const { return RequestCount; }

 protected:
  unsigned short int Rnd; // microsecond portion of time used in CGI Id
  char Header[11];
  bool CompressOutput;
  unsigned short int RequestCount;
  void *data;
};

class CINAsessionControl : public CINAsession {
 public:
  inline CINAsessionControl() {}
  // header is copied to member variable
  CINAsessionControl(char *header, unsigned short int request_count = 0,
		     bool compress_output = false);
  
  inline ~CINAsessionControl() {}

  // Rnd and LastRequestDate are updated NewSession
  // When loading existing session call LoadSession and UpdateLastRequestDate
  void LoadSession(Query &q, const string &session, const string &username, const string &pw);
  inline void UpdateHeader(const string &header) 
    { strncpy(Header,header.c_str(),sizeof(Header)); Header[sizeof(Header)-1] = '\0';}
  inline void UpdateCompressOutput(const bool compress) { CompressOutput = compress; }
  void UpdateLastRequestDate(Query &q);  // Update with current time and date
  inline void UpdateRequestCount(unsigned short int request_count)
    { RequestCount = request_count; }
  void UpdateNucDataBgPID(Query &q, unsigned short int pid);
  void UpdateRateGenBgPID(Query &q, unsigned short int pid);
  void UpdateElemSynBgPID(Query &q, unsigned short int pid);
  void NewSession(Query &q, const string &username, const string &pw);
  void CloseSession(Query &q);

  void BlankSession();

 protected:
  void generateSessionID(string &s);
};

#endif // ifndef __CINA_SESSION_H__
