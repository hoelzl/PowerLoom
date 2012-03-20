//  -*- Mode: C++ -*-

/*--------------------------- BEGIN LICENSE BLOCK ---------------------------+
|                                                                            |
| Version: MPL 1.1/GPL 2.0/LGPL 2.1                                          |
|                                                                            |
| The contents of this file are subject to the Mozilla Public License        |
| Version 1.1 (the "License"); you may not use this file except in           |
| compliance with the License. You may obtain a copy of the License at       |
| http://www.mozilla.org/MPL/                                                |
|                                                                            |
| Software distributed under the License is distributed on an "AS IS" basis, |
| WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   |
| for the specific language governing rights and limitations under the       |
| License.                                                                   |
|                                                                            |
| The Original Code is the STELLA Programming Language.                      |
|                                                                            |
| The Initial Developer of the Original Code is                              |
| UNIVERSITY OF SOUTHERN CALIFORNIA, INFORMATION SCIENCES INSTITUTE          |
| 4676 Admiralty Way, Marina Del Rey, California 90292, U.S.A.               |
|                                                                            |
| Portions created by the Initial Developer are Copyright (C) 1996-2010      |
| the Initial Developer. All Rights Reserved.                                |
|                                                                            |
| Contributor(s):                                                            |
|                                                                            |
| Alternatively, the contents of this file may be used under the terms of    |
| either the GNU General Public License Version 2 or later (the "GPL"), or   |
| the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),   |
| in which case the provisions of the GPL or the LGPL are applicable instead |
| of those above. If you wish to allow use of your version of this file only |
| under the terms of either the GPL or the LGPL, and not to allow others to  |
| use your version of this file under the terms of the MPL, indicate your    |
| decision by deleting the provisions above and replace them with the notice |
| and other provisions required by the GPL or the LGPL. If you do not delete |
| the provisions above, a recipient may use your version of this file under  |
| the terms of any one of the MPL, the GPL or the LGPL.                      |
|                                                                            |
+---------------------------- END LICENSE BLOCK ----------------------------*/

// Version: sdbc-support.cc,v 1.10 2010/10/11 22:35:14 hans Exp

// C Support file for sdbc.


#include "sdbc/sdbc-system.hh"

#define MAXCOLS		32
#define debug           0

namespace sdbc {
  using namespace stella;

  Surrogate* SGT_INTEGER;
  Surrogate* SGT_LONG_INTEGER;
  Surrogate* SGT_DOUBLE_FLOAT;
  Surrogate* SGT_STRING;
  Symbol* SYM_NULL;
  Surrogate* SGT_CALENDAR_DATE;
  SQLINTEGER* null_indicator_buffer;

  void initializeSymbols() {
    // Setup symbols for comparison
    SGT_INTEGER = ((Surrogate*)(internRigidSymbolWrtModule("INTEGER", NULL, 1)));
    SGT_LONG_INTEGER = ((Surrogate*)(internRigidSymbolWrtModule("LONG-INTEGER", NULL, 1)));
    SGT_DOUBLE_FLOAT = ((Surrogate*)(internRigidSymbolWrtModule("DOUBLE-FLOAT", NULL, 1)));
    SGT_STRING = ((Surrogate*)(internRigidSymbolWrtModule("STRING", NULL, 1)));
    SYM_NULL = ((Symbol*)(internRigidSymbolWrtModule("NULL", NULL, 0)));
    SGT_CALENDAR_DATE = ((Surrogate*)(internRigidSymbolWrtModule("CALENDAR-DATE", NULL, 1)));
    null_indicator_buffer = new (GC) SQLINTEGER;
    *null_indicator_buffer = SQL_NULL_DATA;
  }

/*
 *  Show all the error information that is available
 */
int
ODBC_Errors (SQLRETURN status, SQLHENV *henv, SQLHDBC *hdbc, SQLHSTMT *hstmt, char *where)
{
  unsigned char buf[250];
  unsigned char sqlstate[15];

  if (status == SQL_SUCCESS_WITH_INFO) {
    if (debug) {
      std::cerr << "status is: SQL_SUCCESS_WITH_INFO" << std::endl;
    }
  } else if (status == SQL_NEED_DATA) {
    if (debug) {
      std::cerr << "status is: SQL_NEED_DATA" << std::endl;
    }
    DatabaseException* exception = newDatabaseException("SQL_NEED_DATA");
    throw *exception;
  } else if (status == SQL_STILL_EXECUTING) {
    if (debug) {
      std::cerr << "status is: SQL_STILL_EXECUTING" << std::endl;
    }
    DatabaseException* exception = newDatabaseException("SQL_STILL_EXECUTING");
    throw *exception;
  } else if (status == SQL_ERROR) {
    if (debug) {
      std::cerr << "status is: SQL_ERROR" << std::endl;
    }
  } else if (status == SQL_NO_DATA) {
    if (debug) {
      std::cerr << "status is: SQL_NO_DATA" << std::endl;
    }
    DatabaseException* exception = newDatabaseException("SQL_NO_DATA");
    throw *exception;
  } else if (status == SQL_INVALID_HANDLE) {
    if (debug) {
      std::cerr << "status is: SQL_INVALID_HANDLE" << std::endl;
    }
    DatabaseException* exception = newDatabaseException("SQL_INVALID_HANDLE");
    throw *exception;
  } else {
    if (debug) {
      std::cerr << "unknown status:" << status << std::endl;
    }
    std::ostringstream s;
    s << "UNKNOWN STATUS:" << status;
    char* errString = ostringstream_to_c_string(&s);

    DatabaseException* exception = newDatabaseException(errString);
    throw *exception;
  }
  /*
   *  Get statement errors
   */
  if (!strcmp(where, "SQLExec")) {
    while (SQLError (*henv, *hdbc, *hstmt, sqlstate, NULL,
		     buf, sizeof(buf), NULL) == SQL_SUCCESS)
      {
	if (debug) {
	  std::cerr << buf << ", SQLSTATE=" << sqlstate << std::endl;
	}
	DatabaseException* exception = newDatabaseException((char*)buf);
	throw *exception;
      }
  }

  /*
   *  Get connection errors
   */
  if (!strcmp(where, "SQLConnect")) {
    while (SQLError (*henv, *hdbc, SQL_NULL_HSTMT, sqlstate, NULL,
		     buf, sizeof(buf), NULL) == SQL_SUCCESS)
      {
	if (debug) {
	  std::cerr << buf << ", SQLSTATE=" << sqlstate << std::endl;
	}
	DatabaseException* exception = newDatabaseException((char*)buf);
	throw *exception;
      }
  }

  /*
   *  Get environmental errors
   */
  if (!strcmp(where, "SQLEnv")) {
    while (SQLError (*henv, SQL_NULL_HDBC, SQL_NULL_HSTMT, sqlstate, NULL,
		     buf, sizeof(buf), NULL) == SQL_SUCCESS)
      {
	if (debug) {
	  std::cerr << buf << ", SQLSTATE=" << sqlstate << std::endl;
	}
	DatabaseException* exception = newDatabaseException((char*)buf);
	throw *exception;
      }
  }

  /*
   * If we've made it this far, something's wrong.  Just throw an unhandled exception...
   */
  UnhandledException* exception = newUnhandledException("Unknown database exception");
  throw *exception;

  /*
   *  We never make it here...
   */
  return -1;
}


/*
 *  Connect to the datasource
 *
 *  The connect string can have the following parts and they refer to
 *  the values in the odbc.ini file
 *
 *	DSN=<data source name>		[mandatory]
 *	HOST=<server host name>		[optional - value of Host]
 *	SVT=<database server type>	[optional - value of ServerType]
 *	DATABASE=<database path>	[optional - value of Database]
 *	OPTIONS=<db specific opts>	[optional - value of Options]
 *	UID=<user name>			[optional - value of LastUser]
 *	PWD=<password>			[optional]
 *	READONLY=<N|Y>			[optional - value of ReadOnly]
 *	FBS=<fetch buffer size>		[optional - value of FetchBufferSize]
 *
 *   Examples:
 *
 *	HOST=star;SVT=Informix 5;UID=demo;PWD=demo;DATABASE=stores5
 *
 *	DSN=stores5_informix;PWD=demo
 */
int ODBC_Connect(char *connectionString, SQLHENV *henv, SQLHDBC *hdbc, int *connected) {
  short buflen;
  char buf[257];
  SQLCHAR dataSource[120];
  SQLCHAR dsn[33];
  SQLCHAR desc[255];
  SQLCHAR driverInfo[255];
  SWORD len1, len2;
  SQLRETURN status = -1;

  if (debug) {
    std::cout << "ODBC support: allocating environment and connection..." << std::endl;
  }

#if (ODBCVER < 0x0300)
  status = SQLAllocEnv (henv);
  if (status != SQL_SUCCESS) {
    ODBC_Errors (status, henv, hdbc, SQL_NULL_HSTMT, "SQLConnect");
    return -2;
  }

  status = SQLAllocConnect (*henv, hdbc);
  if (status != SQL_SUCCESS) {
    ODBC_Errors (status, henv, hdbc, SQL_NULL_HSTMT, "SQLConnect");
    return -3;
  }
#else
  status = SQLAllocHandle (SQL_HANDLE_ENV, NULL, henv);
  if (status != SQL_SUCCESS) {
    ODBC_Errors (status, henv, hdbc, SQL_NULL_HSTMT, "SQLConnect");
    return -4;
  }

  SQLSetEnvAttr (*henv, SQL_ATTR_ODBC_VERSION, (SQLPOINTER) SQL_OV_ODBC3,
      SQL_IS_UINTEGER);

  status = SQLAllocHandle (SQL_HANDLE_DBC, *henv, hdbc);
  if (status != SQL_SUCCESS) {
    ODBC_Errors (status, henv, hdbc, SQL_NULL_HSTMT, "SQLConnect");
    return -5;
  }
#endif

  if (debug) {
    std::cout << "ODBC support: connecting to driver..." << std::endl;
  }

  status = SQLDriverConnect (*hdbc, 0, (UCHAR *) connectionString, SQL_NTS,
                             (UCHAR *) buf, sizeof (buf), &buflen, SQL_DRIVER_NOPROMPT);

  if (status != SQL_SUCCESS && status != SQL_SUCCESS_WITH_INFO) {
    ODBC_Errors (status, henv, hdbc, SQL_NULL_HSTMT, "SQLConnect");
    return -6;
  }

  // commented for now, since we can't cast the string to UDWORD in 64-bit environment
  // - figure out how to do this right:
  //SQLSetConnectOption (*hdbc, SQL_OPT_TRACEFILE, (UDWORD) "\\SQL.LOG");

  *connected = 1;

  if (debug) {
    status = SQLGetInfo (*hdbc, SQL_DRIVER_VER, driverInfo, sizeof (driverInfo), &len1);
    if (status == SQL_SUCCESS) {
      std::cout << "ODBC support: driver: " << driverInfo << std::endl;
    }
  }

  return 0;

}


  /**
   * Get the next row in a result set. 
   * Return NULL if there's no more data.
   */
  Cons* fetchRow(SQLHENV *henv, SQLHDBC *hdbc, SQLHSTMT *hstmt, short numCols, Cons* types) {
    short colNum;
    SDWORD colIndicator;
    char fetchBuffer[1000];
    Cons* currentRow = NIL;
    SQLRETURN status = SQLFetch (*hstmt);
    LiteralWrapper* valueWrapper;
    GeneralizedSymbol* colType = NULL;

    if (status == SQL_NO_DATA_FOUND) {
      if (debug) {
	std::cout << "ODBC support: freeing statement..." << std::endl;
      }
      SQLFreeStmt (hstmt, SQL_CLOSE);
      return NULL;
    }    

    if (status != SQL_SUCCESS) {
      ODBC_Errors (status, henv, hdbc, hstmt, "SQLExec");
    }

    for (colNum = 1; colNum <= numCols; colNum++) {
      // DANGEROUS: Assume that type list is valid.  May want to check for premature end-of-list...
      if (types != NULL) {
	colType = (Surrogate*)types->value;
	types = types->rest;
      } 
      
      if ((colType == NULL) || 
	  (colType == SYM_NULL) ||
	  (colType == SGT_STRING) ||
	  (colType == SGT_CALENDAR_DATE)) {
	/*
	 *  Fetch this column as a string
	 */
	status = SQLGetData (*hstmt, colNum, SQL_CHAR, fetchBuffer,
			     sizeof (fetchBuffer), &colIndicator);
	if (status != SQL_SUCCESS) {
	  ODBC_Errors (status, henv, hdbc, hstmt, "SQLExec");
	} else {
	  char* valueString = new char[strlen(fetchBuffer) + 1];
	  strcpy(valueString, fetchBuffer);
	  valueWrapper = wrapString(valueString);
	}
      } else if (colType == SGT_INTEGER) {
	/*
	 *  Fetch this column as integer (2 bytes)
	 */
	status = SQLGetData (*hstmt, colNum, SQL_SMALLINT, fetchBuffer,
			     sizeof (fetchBuffer), &colIndicator);
        if (status != SQL_SUCCESS) {
          ODBC_Errors (status, henv, hdbc, hstmt, "SQLExec");
	} else {
	  int intValue = ((int)*(fetchBuffer));
	  valueWrapper = wrapInteger(intValue);
	}	
      } else if (colType == SGT_DOUBLE_FLOAT) {
	/*
	 *  Fetch this column as double
	 */
	double doubleBuf = 0;
	SQLGetData (*hstmt, colNum, SQL_C_DOUBLE, &doubleBuf,
		    sizeof (doubleBuf), &colIndicator);
        if (status != SQL_SUCCESS) {
          ODBC_Errors (status, henv, hdbc, hstmt, "SQLExec");
	} else {
	  double doubleValue = doubleBuf;
	  valueWrapper = wrapFloat(doubleValue);
	}
      } else if (colType == SGT_LONG_INTEGER) {
	/*
	 *  Fetch this column as long (4 bytes)
	 */
	long longBuf = 0;
	status = SQLGetData (*hstmt, colNum, SQL_INTEGER, &longBuf,
			     sizeof (longBuf), &colIndicator);
        if (status != SQL_SUCCESS) {
          ODBC_Errors (status, henv, hdbc, hstmt, "SQLExec");
	} else {
	  long longValue = longBuf;
	  // PROBLEM: Stella doesn't have a LONG-WRAPPER, so we cram
	  // the long into an integer.
	  valueWrapper = wrapInteger((int)longValue);
	}	
      } else {
	// Shouldn't get here if we validate the list beforehand:
	DatabaseException* exception = newDatabaseException("Bad Typelist");
	throw *exception;
      }


      if (colIndicator == SQL_NULL_DATA) {
	/*
	 *  Represent NULL fields as NULL.  
	 */
	currentRow = cons(NULL, currentRow);
      } else {
	currentRow = cons(valueWrapper, currentRow);
      }
    }
    return currentRow->reverse();
  }


  Cons* collectAllRows(SQLHENV *henv, SQLHDBC *hdbc, SQLHSTMT *hstmt, Cons* types) {
    UDWORD totalSets;
    SQLRETURN status;
    short numCols;
    short colNum;
    char colName[50];
    short colType;
    UDWORD colPrecision;
    SDWORD colIndicator;
    short colScale;
    short colNullable;
    UDWORD totalRows;
    Cons* currentRow = NULL;
    Cons* tableResult = NIL;

    /*
     *  Loop through all the result sets
     */
    totalSets = 1;
    do {
      /*
       *  Get the number of result columns for this cursor.
       *  If it is 0, then the statement was probably a select
       */
      status = SQLNumResultCols (*hstmt, &numCols);
      if (status != SQL_SUCCESS) {
	ODBC_Errors (status, henv, hdbc, hstmt, "SQLExec");
	goto endCursor;
      }
      if (numCols == 0) {
	if (debug) {
	  std::cout << "ODBC support: statement executed." << std::endl;
	}
	goto endCursor;
      }

      if (numCols > MAXCOLS)
	numCols = MAXCOLS;
      /*
       *  Get the names for the columns
       *  NOTE: we don't do anything this info, but we might eventually,
       *  so keep code around...
       */
      for (colNum = 1; colNum <= numCols; colNum++) {
	/*
	 *  Get the name and other type information
	 */
	status = SQLDescribeCol (*hstmt, colNum, (UCHAR *) colName,
				 sizeof (colName), NULL, &colType, &colPrecision,
				 &colScale, &colNullable);
	if (status != SQL_SUCCESS) {
	  ODBC_Errors (status, henv, hdbc, hstmt, "SQLExec");
	  goto endCursor;
	}
      }

      /*
       *  Collect all the fields
       */
      totalRows = 0;
      while (1) {
	currentRow = fetchRow(henv, hdbc, hstmt, numCols, types);
	if (currentRow == NULL) {
	  break;
	}
	totalRows++;
	tableResult = cons(currentRow, tableResult);
      }

      totalSets++;
    }
    while (SQLMoreResults (*hstmt) == SQL_SUCCESS);

    if (debug) {
      std::cout << "ODBC support: freeing hstmt..." << std::endl;
    }
  endCursor:
    SQLFreeStmt (*hstmt, SQL_CLOSE);

    if (tableResult->emptyP()) {
      return NULL;
    } else {
      return tableResult->reverse();
    }
  }



/**
 * Create a statement object and execute the sql. 
 * Return (CONS OF (CONS OF (STRING-WRAPPER)))
 */
Object* executeSQL(NativeConnection* nativeConnection, char* sqlRequest, Cons* types) {
  char request[512];
  char fetchBuffer[1000];
  int i;
  SQLHSTMT hstmt;
  SQLHENV *henv = nativeConnection->henv;
  SQLHDBC *hdbc = nativeConnection->hdbc;
  SQLRETURN status;
  
  /*
   *  Prepare & Execute the statement
   */
  if (debug) {
    std::cout << "ODBC support: allocating statement..." << std::endl;
  }
#if (ODBCVER < 0x0300)
  status = SQLAllocStmt (*hdbc, &hstmt);
  if (status != SQL_SUCCESS) {
    ODBC_Errors (status, henv, hdbc, &hstmt, "SQLExec");
    return NULL;
  }
#else
  status = SQLAllocHandle (SQL_HANDLE_STMT, *hdbc, &hstmt);
  if (status != SQL_SUCCESS) {
    ODBC_Errors (status, henv, hdbc, &hstmt, "SQLExec");
    return NULL;
  }
#endif
  

  if (debug){
    std::cout << "ODBC support: preparing statement..." << std::endl;
  }
  status = SQLPrepare (hstmt, (UCHAR *) sqlRequest, SQL_NTS);
  if (status != SQL_SUCCESS) {
    ODBC_Errors (status, henv, hdbc, &hstmt, "SQLExec");
    return NULL;
  }
  if (debug) {
    std::cout << "ODBC support: executing statement..." << std::endl;
  }
  status = SQLExecute (hstmt);
  if (status != SQL_SUCCESS) {
    ODBC_Errors (status, henv, hdbc, &hstmt, "SQLExec");
    return NULL;
  }

  return collectAllRows(henv, hdbc, &hstmt, types);

//   /*
//    *  Loop through all the result sets
//    */
//   totalSets = 1;
//   do {
//     /*
//      *  Get the number of result columns for this cursor.
//      *  If it is 0, then the statement was probably a select
//      */
//     status = SQLNumResultCols (hstmt, &numCols);
//     if (status != SQL_SUCCESS) {
//       ODBC_Errors (status, henv, hdbc, &hstmt, "SQLExec");
//       goto endCursor;
//     }
//     if (numCols == 0) {
//       if (debug) {
// 	std::cout << "Statement executed." << std::endl;
//       }
//       goto endCursor;
//     }

//     if (numCols > MAXCOLS)
//       numCols = MAXCOLS;
    
//     /*
//      *  Get the names for the columns
//      *  NOTE: we don't do anything this info, but we might eventually,
//      *  so keep code around...
//      */
//     for (colNum = 1; colNum <= numCols; colNum++) {
//       /*
//        *  Get the name and other type information
//        */
//       status = SQLDescribeCol (hstmt, colNum, (UCHAR *) colName,
// 			       sizeof (colName), NULL, &colType, &colPrecision,
// 			       &colScale, &colNullable);
//       if (status != SQL_SUCCESS) {
// 	ODBC_Errors (status, henv, hdbc, &hstmt, "SQLExec");
// 	goto endCursor;
//       }

//     }

//     /*
//      *  Collect all the fields
//      */
//     totalRows = 0;
//     while (1) {
//       currentRow = fetchRow(henv, hdbc, &hstmt, numCols, types);
//       if (currentRow == NULL) {
// 	break;
//       }

//       /*
//       currentRow = NIL;
//       int status = SQLFetch (hstmt);

//       if (status == SQL_NO_DATA_FOUND)
// 	break;

//       if (status != SQL_SUCCESS) {
// 	ODBC_Errors (status, henv, hdbc, &hstmt, "SQLExec");
// 	break;
//       }
//       for (colNum = 1; colNum <= numCols; colNum++) {
// 	if (SQLGetData (hstmt, colNum, SQL_CHAR, fetchBuffer,
// 			sizeof (fetchBuffer), &colIndicator) != SQL_SUCCESS) {
// 	  ODBC_Errors (status, henv, hdbc, &hstmt, "SQLExec");
// 	  goto endCursor;
// 	}

// 	if (colIndicator == SQL_NULL_DATA) {
// 	  currentRow = cons(NULL, currentRow);
// 	} else {
// 	  char* valueString = new char[strlen(fetchBuffer) + 1];
// 	  strcpy(valueString, fetchBuffer);
// 	  StringWrapper* wrappedValue = wrapString(valueString);
// 	  currentRow = cons(wrappedValue, currentRow);
// 	}
//       }
//       */
//       totalRows++;
//       tableResult = cons(currentRow, tableResult);
//     }

//     totalSets++;
//   }
//   while (SQLMoreResults (hstmt) == SQL_SUCCESS);

//   if (debug) {
//     std::cout << "Freeing hstmt..." << std::endl;
//   }
//  endCursor:
//   SQLFreeStmt (hstmt, SQL_CLOSE);

//   if (tableResult->emptyP()) {
//     return NULL;
//   } else {
//     return tableResult->reverse();
//   }
}

  

  /**
   * Fetch a row given a result set.
   */
  Cons* fetchRow(NativeResultSet* resultSet) {
    if (resultSet->done) {
      return NULL;
    }
    // Todo: add types...
    Cons* result = fetchRow(resultSet->henv, resultSet->hdbc, resultSet->hstmt, resultSet->numCols, 
			    resultSet->types);
    if (result == NULL) {
      resultSet->done = 1;
    }
    return result;
  }

  /**
   * Get a native resultset
   */
  NativeResultSet* getNativeResultSet(NativeConnection* nativeConnection, char* sqlRequest, Cons* types) {
    NativeResultSet* result = new (GC) NativeResultSet();
    UDWORD colPrecision;
    SDWORD colIndicator;
    short colScale;
    short colNullable;
    SQLHSTMT *hstmt = new SQLHSTMT();
    SQLHENV *henv = nativeConnection->henv;
    SQLHDBC *hdbc = nativeConnection->hdbc;
    short numCols;
    short colNum;
    char colName[50];
    short colType;
    SQLRETURN status;
  
    /*
     *  Prepare & Execute the statement
     */
    if (debug) {
      std::cout << "ODBC support: allocating statement..." << std::endl;
    }
#if (ODBCVER < 0x0300)
    status = SQLAllocStmt (*hdbc, hstmt);
    if (status != SQL_SUCCESS) {
      ODBC_Errors (status, henv, hdbc, &hstmt, "SQLExec");
      return NULL;
    }
#else
    status = SQLAllocHandle (SQL_HANDLE_STMT, *hdbc, hstmt);
    if (status != SQL_SUCCESS) {
      ODBC_Errors (status, henv, hdbc, hstmt, "SQLExec");
      return NULL;
    }
#endif
  

    if (debug){
      std::cout << "ODBC support: preparing statement..." << std::endl;
    }
    status = SQLPrepare (*hstmt, (UCHAR *) sqlRequest, SQL_NTS);
    if (status != SQL_SUCCESS) {
      ODBC_Errors (status, henv, hdbc, hstmt, "SQLExec");
      return NULL;
    }
    if (debug) {
      std::cout << "ODBC support: executing statement..." << std::endl;
    }
    status = SQLExecute (*hstmt);
    if (status != SQL_SUCCESS) {
      ODBC_Errors (status, henv, hdbc, hstmt, "SQLExec");
      return NULL;
    }

    /*
     *  Get the number of result columns for this cursor.
     *  If it is 0, then the statement was probably a select
     */
    status = SQLNumResultCols (*hstmt, &numCols);
    if (status != SQL_SUCCESS) {
      ODBC_Errors (status, henv, hdbc, hstmt, "SQLExec");
    }
    if (numCols == 0) {
      if (debug) {
	std::cout << "ODBC support: statement executed." << std::endl;
      }
	return NULL;
      }

    if (numCols > MAXCOLS)
      numCols = MAXCOLS;
    
    /*
     *  Get the names for the columns
     *  NOTE: we don't do anything this info, but we might eventually,
     *  so keep code around...
     */
    for (colNum = 1; colNum <= numCols; colNum++) {
      /*
       *  Get the name and other type information
       */
      status = SQLDescribeCol (*hstmt, colNum, (UCHAR *) colName,
			       sizeof (colName), NULL, &colType, &colPrecision,
			       &colScale, &colNullable);
      if (status != SQL_SUCCESS) {
	ODBC_Errors (status, henv, hdbc, hstmt, "SQLExec");
      }
    }      
      
    result->henv = henv;
    result->hdbc = hdbc;
    result->hstmt = hstmt;
    result->numCols = numCols;
    result->types = types;
    result->done = 0;
    return result;
  }


/**
 * Get a native ODBC connection.
 * TODO: 
 * - error handling
 * - make sure that the STELLA object frees the connection when it is destroyed
 */
NativeConnection* getConnection(char* connectionString) {
  if (debug) {
    std::cout << "ODBC support: connection string = " << connectionString << std::endl;
  }

  SQLHDBC* hdbc = new SQLHDBC();
  SQLHENV* henv = new SQLHENV();
  int connected = -1;
  SQLRETURN status = ODBC_Connect(connectionString, henv, hdbc, & connected);
  if (debug) {
    std::cout << "ODBC support: return status = " << status << std::endl;
    std::cout << "ODBC support: connected = " << connected << std::endl;
  }
  NativeConnection* result = new (GC) NativeConnection;
  result->hdbc = hdbc;
  result->henv = henv;
  result->connected = connected;
  
  return result;
}

  /*
   *  Disconnect from the database
   */
  int ODBC_Disconnect (NativeConnection* connection) {
    if (connection->connected) {
      SQLDisconnect (*(connection->hdbc));
      connection->connected = 0;
    } else {
	DatabaseException* exception = newDatabaseException("Trying to disconnect a connection that is already disconnected");
	throw *exception;
    }

#if (ODBCVER < 0x0300)
    
    if (*(connection->hdbc))
      SQLFreeConnect (*(connection->hdbc));

    if (*(connection->henv))
      SQLFreeEnv (*(connection->henv));
#else
    if (*(connection->hdbc))
      SQLFreeHandle (SQL_HANDLE_DBC, *(connection->hdbc));

    if (*(connection->henv))
      SQLFreeHandle (SQL_HANDLE_ENV, *(connection->henv));
#endif

    return 0;
  }


  void nativeDisconnect(NativeConnection* nativeConnection) {
    if (debug) {
      std::cout << "ODBC support: disconnecting..." << std::endl;
    }
    ODBC_Disconnect(nativeConnection);
  }

  /*
   * Transaction suppoort
   */
  void setAutocommitFlag(NativeConnection* nativeConnection, boolean flag) {
    SQLRETURN status = SQLSetConnectAttr(*(nativeConnection->hdbc), SQL_ATTR_AUTOCOMMIT, 
				   flag ? (void*)SQL_AUTOCOMMIT_ON : (void*)SQL_AUTOCOMMIT_OFF, 0);
    if (status != SQL_SUCCESS) {
      ODBC_Errors (status, nativeConnection->henv, nativeConnection->hdbc, SQL_NULL_HSTMT, "SQLConnect");
    }
  }

  void doCommit(NativeConnection* nativeConnection) {
    SQLRETURN status = SQLEndTran(SQL_HANDLE_DBC, *(nativeConnection->hdbc), SQL_COMMIT);
    if (status != SQL_SUCCESS) {
      ODBC_Errors (status, nativeConnection->henv, nativeConnection->hdbc, SQL_NULL_HSTMT, "SQLConnect");
    }
  }

  void doRollback(NativeConnection* nativeConnection) {
    SQLRETURN status = SQLEndTran(SQL_HANDLE_DBC, *(nativeConnection->hdbc), SQL_ROLLBACK);
    if (status != SQL_SUCCESS) {
      ODBC_Errors (status, nativeConnection->henv, nativeConnection->hdbc, SQL_NULL_HSTMT, "SQLConnect");
    }
  }

  /**
   * Prepared statements
   */
  void doPrepareStatement(NativeConnection* nativeConnection, char* sql) {
    SQLRETURN status;

    // Create a new statement
    SQLHSTMT *hstmt = new SQLHSTMT();
    nativeConnection->hstmt = hstmt;
#if (ODBCVER < 0x0300)
    status = SQLAllocStmt (*(nativeConnection->hdbc), hstmt);
    if (status != SQL_SUCCESS) {
      ODBC_Errors (status, nativeConnection->henv, nativeConnection->hdbc, nativeConnection->hstmt, "SQLExec");
    }
#else
    status = SQLAllocHandle (SQL_HANDLE_STMT, *(nativeConnection->hdbc), hstmt);
    if (status != SQL_SUCCESS) {
      ODBC_Errors (status, nativeConnection->henv, nativeConnection->hdbc, nativeConnection->hstmt, "SQLExec");
    }
#endif

    status = SQLPrepare(*(nativeConnection->hstmt), (SQLCHAR*)sql, (SQLINTEGER)strlen(sql));
    if (status != SQL_SUCCESS) {
      ODBC_Errors (status, nativeConnection->henv, nativeConnection->hdbc, nativeConnection->hstmt, "SQLExec");
    }
  }

  Object* doExecutePreparedStatement(NativeConnection* nativeConnection) {
    if (debug)
      std::cout << "ODBC support: executing prepared statement..." << std::endl;
    SQLRETURN status = SQLExecute(*(nativeConnection->hstmt));
    if (status != SQL_SUCCESS) {
      ODBC_Errors (status, nativeConnection->henv, nativeConnection->hdbc, nativeConnection->hstmt, "SQLExec");
    }
    return collectAllRows(nativeConnection->henv, nativeConnection->hdbc, nativeConnection->hstmt, NULL);
  }

  void doBindParameter(NativeConnection* nativeConnection, int position, Surrogate* typeSpec, Object* wrappedValue) {
    SQLRETURN status = -1;

    if (typeSpec == SGT_INTEGER) {
      long* value = NULL;
      int valueLength = sizeof(*value);
      if (wrappedValue != NULL) {
	value = new (GC) long;
	*value = (long)unwrapInteger((stella::IntegerWrapper*)wrappedValue);
	if (debug) {
	  std::cout << "ODBC support: passed an integer(" << position << ", size=" << valueLength << "):" << value << std::endl;
	}
      } else {
	std::cout << "ODBC support: integer is null, null_indicator_buffer = " << null_indicator_buffer << std::endl;
      }
      status = SQLBindParameter(*(nativeConnection->hstmt), position, SQL_PARAM_INPUT, 
				SQL_C_SLONG, SQL_INTEGER, 0, 0, value, 0, 
				(wrappedValue == NULL) ? null_indicator_buffer : NULL);
    } else if (typeSpec == SGT_DOUBLE_FLOAT) {
      double* value = new (GC) double;
      int valueLength = sizeof(*value);
      if (wrappedValue != NULL) {
	*value = (double)unwrapFloat((stella::FloatWrapper*)wrappedValue);
	if (debug) {
	  std::cout << "ODBC support: passed a double(" << position << ", size=" << valueLength << "):" << value << std::endl;
	}
      }
      status = SQLBindParameter(*(nativeConnection->hstmt), position, SQL_PARAM_INPUT, 
				SQL_C_DOUBLE, SQL_FLOAT, 0, 0, value, 0, 
				(wrappedValue == NULL) ? null_indicator_buffer : NULL);
    } else if (typeSpec == SGT_STRING) {
      char* unwrappedString;
      char* value;
      if (wrappedValue != NULL) {
	unwrappedString = unwrapString((stella::StringWrapper*)wrappedValue); 
	value = new (GC) char[strlen(unwrappedString) + 1];
	strcpy(value, unwrappedString);
	value[strlen(unwrappedString)] = '\0';
	if (debug) {
	  std::cout << "ODBC support: passed a string(" << position << "): " << value << std::endl;
	}
      }
      status = SQLBindParameter(*(nativeConnection->hstmt), (SQLUSMALLINT)position, SQL_PARAM_INPUT, SQL_C_CHAR, SQL_VARCHAR, 
				(SQLUINTEGER)strlen(value), /* ColumnSize used? */
				(SQLSMALLINT)0, /* Decimal Digits ignored */
				(SQLPOINTER)value, (SQLINTEGER)(wrappedValue == NULL) ? 0 : strlen(value), 
				(SQLINTEGER*)(wrappedValue == NULL) ? null_indicator_buffer : NULL);
    }
    if (status != SQL_SUCCESS) {
      ODBC_Errors (status, nativeConnection->henv, nativeConnection->hdbc, nativeConnection->hstmt, "SQLExec");
    }
  }

  Wrapper* doGetServerInfo(NativeConnection* nativeConnection, char* property) {
    // Retrieve information about `property' from `nativeConnection' and return
    // the result as a wrapped string, integer, boolean or float as appropriate.
    SQLHDBC* hdbc = nativeConnection->hdbc;
    char stringResult[1024];
    SQLUSMALLINT infoType;
    SQLSMALLINT resultLength = 0;
    SQLRETURN status;
    int stringInfo = 0;
    // not yet used:
    //int intInfo = 0;
    //int booleanInfo = 0;

    // see http://msdn.microsoft.com/library/en-us/odbc/htm/odbcsqlgetinfo.asp?frame=true
    // for a comprehensive list of supported information types:
    if (stringEqualP(property, "DBMS-NAME")) {stringInfo = 1; infoType = SQL_DBMS_NAME;}
    if (stringEqualP(property, "DBMS-VERSION")) {stringInfo = 1; infoType = SQL_DBMS_VER;}

    if (stringInfo) {
      status = SQLGetInfo(*hdbc,
                          infoType,
                          (SQLPOINTER)stringResult,
                          sizeof(stringResult),
                          &resultLength);
      if (status != SQL_SUCCESS)
        ODBC_Errors (status, nativeConnection->henv, hdbc, nativeConnection->hstmt, "SQLGetInfo");
      else
        return wrapString(strncpy(makeString(resultLength, 0), stringResult, resultLength));
    }
    return NULL;
  }

} // end namespace sdbc
