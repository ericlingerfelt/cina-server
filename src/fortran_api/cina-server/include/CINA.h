
#ifndef __CINA_H__
#define __CINA_H__


// core CINA related header files
#include "ErrObj.h"
#include "datetime.h"
#include "accounts.h"
#include "sessions.h"
#include "logs.h"
//#include "xmlParser.h"

// other CINA related header files
//#include "comments.h"


// Set to 1 to use developmental database
// Set to 0 to use normal database

#if (USE_DEVELOPMENTAL == 0)
#define CINA_DB_HOST "localhost"
#define CINA_DB_NAME "cina"
#define CINA_DB_USER "cina"
#define CINA_DB_PW "IhWfAbN!2"

#else
#define CINA_DB_HOST "localhost"
#define CINA_DB_NAME "cinad"
#define CINA_DB_USER "cinad"
#define CINA_DB_PW "IhWfAbN!2d"

#endif // if (USE_DEVELOPMENTAL)

#endif // ifndef __CINA_H__
