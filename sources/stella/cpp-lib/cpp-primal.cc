// -*- Mode: C++ -*-

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
| Portions created by the Initial Developer are Copyright (C) 1996-2006      |
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

// Version: cpp-primal.cc,v 1.64 2006/05/10 02:33:14 hans Exp

// Native C++ support for STELLA


#include "stella/stella-system.hh"

namespace stella {

  //
///// 'null?' and 'defined?':
 //

// NOTE: 'definedP' and 'nullP' really aren't called anymore, since they
//       get translated directly into '==' tests by the C++ translator.

boolean definedP (void* p) {
  if (p)
    return TRUE;
  else
    return FALSE;
}

boolean definedP (int i) {
  if (i == NULL_INTEGER)
    return FALSE;
  else
    return TRUE;
}

boolean definedP (double f) {
  if (f == NULL_FLOAT)
    return FALSE;
  else
    return TRUE;
}

boolean nullP (void* p) {
  if (p)
    return FALSE;
  else
    return TRUE;
}

boolean nullP (int i) {
  if (i == NULL_INTEGER)
    return TRUE;
  else
    return FALSE;
}

boolean nullP (double f) {
  if (f == NULL_FLOAT)
    return TRUE;
  else
    return FALSE;
}

  //
///// Arithmetic functions
 //

int ceiling(int n) {
  return n;
}
int ceiling(double n) {
  return (int) ::ceil(n);
}

int floor(int n) {
  return n;
}
int floor(double n) {
  return (int) ::floor(n);
}

int round(int n) {
  return n;
}

int round(double n) {
  return (int) ::floor(n+0.5);
}

int truncate (int n) {
  return n;
}

int truncate(double n) {
  return (int) n;
}


// `double exp(double x)' already available in math.h.


int integerLength (int x) {
  // Implements a search for the leftmost bit of "X"
  //   using a binary search method.
  int ilength = 0;
  if (x < 0) {
    x = -x;
    ilength++;
  }
  if ((x ^ 0xFFFF0000L)  != 0) {
     x >>= 16;
     ilength += 16;
  }
  if ((x ^ 0xFF00)  != 0) {
     x >>= 8;
     ilength += 8;
  }
  if ((x ^ 0xF0)  != 0) {
     x >>= 4;
     ilength += 4;
  }
  if ((x ^ 0xC)  != 0) {
     x >>= 2;
     ilength += 2;
  }
  switch (x) {
    case 0: return ilength;
    case 1: return ilength + 1;
    case 2:
    case 3: return ilength + 2;
  }
}


  //
///// Character library functions
 //

  //
///// String library functions
 //

char* makeString(int size, char initialElement) {
  char* newstring = new (GC) char[size+1];
  for (int i = 0; i< size; i++) {
    newstring[i] = initialElement;
  }
  newstring[size] = '\0';
  return newstring;
}

char* stringConcatenate(char* string1, char* string2) {
  char* newstring = new (GC) char[strlen(string1) + strlen(string2) + 1];
  strcpy(newstring, string1);
  strcat(newstring, string2);
  return newstring;
}

char* stringUpcase(char* string) {
  // Return an upper-case copy of 'string'.
  char* newstring = new (GC) char[strlen(string) + 1];
  strcpy(newstring, string);
  int offset = ('a' - 'A');
  for (char* p = newstring; *p != '\0'; p++)
    if (islower(*p))
      *p = *p - offset;
  return newstring;
}

char* stringDowncase(char* string) {
  // Return a lower-case copy of 'string'.
  char* newstring = new (GC) char[strlen(string) + 1];
  strcpy(newstring, string);
  int offset = ('A' - 'a');
  for (char* p = newstring; *p != '\0'; p++)
    if (isupper(*p))
      *p = *p - offset;
  return newstring;
}

char* stringCapitalize(char* string) {
  // Return a capitalized version of 'string'.
  // Upcase first character and characters after non-alphanumeric characters.
  char* newstring = stringDowncase(string);
  int offset = ('a' - 'A');
  int capitalizeFlag = TRUE;
  for (char* p = newstring; *p != '\0'; p++) {
    if ((capitalizeFlag) && (isalpha(*p))) {
      if (islower(*p))
	*p = *p - offset;
      capitalizeFlag = FALSE;
    }
    if (!isalpha(*p)) {
      capitalizeFlag = TRUE;
    }
  }
  return newstring;
}

char* stringCopy(char* string) {
  char* newstring = new (GC) char[strlen(string) + 1];
  strcpy(newstring, string);
  return newstring;
}

char* stringRemove(char* string, char character) {
  // Return 'string' with all instances of 'character' removed.
  // If 'character' doesn't appear in 'string', then 'string' is returned.
  int i, end, matches = 0;
  end = strlen(string);
  for(i = 0; i < end; i++) {
      if (string[i] == character) {
	  matches++;
      }
  }
  if (matches == 0) {
      return string;
  } else {
      char* newstring = new (GC) char[end-matches];
      int j = 0;
      for(i = 0; i < end; i++) {
	  if (string[i] != character) {
	      newstring[j] = string[i];
	      j++;
	  }
      }
      return newstring;
  }
}

char* stringSubstitute(char* self, char newchar, char oldchar) {
  char* newstring = stringCopy(self);
  for (char* p = newstring; *p != '\0'; p++)
    if (*p == oldchar)
      *p = newchar;
  return newstring;
}

char* mutableStringSubstitute(char* self, char newchar, char oldchar) {
  for (char* p = self; *p != '\0'; p++)
    if (*p == oldchar)
      *p = newchar;
  return self;
}

boolean stringMemberP(char* self, char character) {
  for (char* p = self; *p != '\0'; p++)
    if (*p == character)
      return TRUE;
  return FALSE;
}

char* stringRest(char* self) {
  // Return pointer to second character.
  return self+1;
}

int stringPosition(char* string, char character, int start) {
  // Return the position of 'character' within 'string' (counting
  // from zero), or return NULL if 'character' does not occur within 'string'.

  int i = (start == NULL_INTEGER) ? 0 : start;
  int string_length = strlen(string);
  for (; i < string_length; i++) {
    if (string[i] == character) {
      return i;
    }
  }
  return NULL_INTEGER;
}

int stringLastPosition(char* string, char character, int end) {
  // Return the position of the last occurence of 'character' within
  // 'string' (counting from zero), or return NULL if 'character'
  //  does not occur within 'string'.  Counts backward from 'end'.

  int i = (end == NULL_INTEGER) ? strlen(string) : end;
  for (; i >= 0; i--) {
    if (string[i] == character) {
      return i;
    }
  }
  return NULL_INTEGER;
}

int stringSearch(char* string, char* substring, int start) {
  // Return start position of the left-most occurrence right of
  // 'start' of 'substring' in 'string'.  Return NULL if it is not 
  // a substring.
  int begin, end;

  begin = (start == NULL_INTEGER) ? 0 : start;
  int stringLength = strlen(string);
  if (stringLength < (strlen(substring) + begin))
    return NULL_INTEGER;

  char* newstring = stringCopy(string);
  for (end = (strlen(substring) + begin); end <= stringLength;
       begin++, end++) {
    char savedEndChar = string[end];
    // Temporarily truncate string to the same size of substring
    newstring[end] = '\0';
    if (!strcmp((newstring + begin), substring)) {
      return begin;
    }
    newstring[end] = savedEndChar;
  }
  return NULL_INTEGER;
}

char* stringSubsequence(char* string, int start, int end) {
  // Return a substring of 'string' beginning at position 'start'
  // and ending up to but not including position 'end', counting from zero.  
  // An 'end' value of NULL stands for the rest of the string.

  int i, j;
  if (end == NULL_INTEGER) 
    end = strlen(string);
  char* newstring = new (GC) char[end-start + 1];
  for(i = 0, j = start; j < end; i++, j++)
    newstring[i] = string[j];
  newstring[i] = '\0';
  return newstring;
}

char* mutableStringSubsequence(char* string, int start, int end) {
  // Return a substring of 'string' beginning at position 'start'
  // and ending up to but not including position 'end', counting from zero.  
  // An 'end' value of NULL stands for the rest of the string.
  
  // strings and mutable strings are the same in C++
  return stringSubsequence(string, start, end);
}

//  Converting to and from string representations:

char* ostringstream_to_c_string(std::ostringstream* stream) {
  // Copy the current content of `stream' into a newly allocated
  // C-string and return the result.
  // Unfortunately, this copies twice (or trice?): stream to result,
  // (result to result.c_str()?) and result.c_str() to returned string;
  // we could avoid that by copying from the std::string character by
  // character, the question is whether that's too slow or not.
  std::string result = stream->str();
  return strcpy(new (GC) char[result.length() + 1], result.c_str());
}

char* stringify(Object* expression) {
  std::ostringstream s;
  BIND_STELLA_SPECIAL(oPRINTREADABLYpo, boolean, TRUE);
  BIND_STELLA_SPECIAL(oPRINTPRETTYpo, boolean, FALSE);
  s << expression;
  return(ostringstream_to_c_string(&s));
}

char* integerToString(int i) {
  std::ostringstream s;
  s << i;
  return(ostringstream_to_c_string(&s));
}

char* integerToHexString(int i) {
  std::ostringstream s;
  s << std::hex << i;
  return(ostringstream_to_c_string(&s));
}

char* integerToStringInBase(int i, int base) {
  // BUG:  Only supports base 8, 10, 16:
  // SHOULD ADD IN THE GENERAL CODE FROM THE LISP VERSION
  // AND USE IT IF base IS NOT 8, 10 OR 16.
  std::ostringstream s;
  s << std::setbase(base) << i;
  return(ostringstream_to_c_string(&s));
}


char* floatToString(double f) {
  std::ostringstream s;
  s << f;
  return(ostringstream_to_c_string(&s));
}

char* formatFloat (double v, int n) {
  std::ostringstream s;
  s.setf(std::ios::fixed);
  s.precision(n);
  s << v;
  return(ostringstream_to_c_string(&s));
}

int stringToInteger(char* string) {
  // TO DO: Consider using std::atoi(string)  instead.
  std::istringstream s(string);
  int result;
  s >> result;
  return result;
}

double stringToFloat(char* string) {
  // TO DO: Consider using std::atof(string)  instead.
  std::istringstream s(string);
  double result;
  s >> result;
  return result;
}

unsigned int native_hash_string (const char* x) {
  // Compute a hash code for the string `x' and return the result.
  // Taken from Dragon book, p436.
  register unsigned int h = 0;
  register unsigned int g;

  while (*x != 0)
  {
    h = (h << 4) + *x++;
    if ((g = h & 0xf0000000) != 0)
      h = (h ^ (g >> 24)) ^ g;
  }
  return h;
}


  //
///// Vectors, extensible vectors, and vector sequences
 //

// Now handled by verbatim statements in main Stella sources.

  //
///// Stream operations
 //

char* native_read_line(std::istream* stream) {
  // Read one line from `stream' and return the result.
  // Return NULL upon EOF.

  // Probe for EOF before we read so we can correctly
  // return final lines that don't end in a newline:
  stream->peek();
  if (stream->eof())
    return NULL;

  std::string line;
  std::getline(*stream, line);
  char* result = new (GC) char[line.length() + 1];
  strcpy(result, line.c_str());
  return result;
}

char native_read_character(std::istream* stream, boolean& eofP) {
  // Read one character from `stream' and return the result.
  // Return NULL upon EOF and set `eofP' to TRUE.
  char c = stream->get();
  if (c == EOF) {
    eofP = TRUE;
    return '\0';
  }
  else
    return c;
}


  //
///// File operations
 //

boolean probeFileP(char* filename) {
  filename = translateLogicalPathname(filename);
  std::ifstream fStream;
  fStream.open(filename, std::ios::in);
  if (!fStream)
    return FALSE;
  else
    return TRUE;
}

} // end of namespace stella; close here to make MinGW happy
#include <sys/types.h>
#include <sys/stat.h>
namespace stella {

CalendarDate* fileWriteDate(char* filename) {
  // Return the time of the last modification of `filename' if available.
  // Return NULL otherwise.
  filename = translateLogicalPathname(filename);
  struct stat status;
  if (stat(filename, &status) == 0)
     return nativeDateTimeToCalendarDate(status.st_mtime);
  return (NULL);
}

int fileLength(char* filename) {
  // NOTE: Using 'int' to store sizes might not be sufficient!!
  if (!probeFileP(filename)) {
   //// WE SHOULD THROW AN EXCEPTION HERE!!
    std::cerr << "ERROR in 'file_length': File '"
         << filename << "' does not exist!" << std::endl;
    exit(1);
  }
  else {
    filename = translateLogicalPathname(filename);
    std::ifstream fStream(filename);
    fStream.seekg(0, std::ios::end);
    return (fStream.tellg());
  }
}

#include <cstdio>

void deleteFile(char* filename) {
  if (!probeFileP(filename)) {
    std::cerr << "ERROR in 'delete_file': File '"
              << filename << "' does not exist!" << std::endl;
  //// WE SHOULD THROW AN EXCEPTION HERE!!
    exit(1);
  }
  else {
    filename = translateLogicalPathname(filename);
    // this is only available in g++ 3 and later:
    //std::remove(filename);
    // this seems to work in in earlier and later versions
    // in combination with using <cstdio>:
    remove(filename);
  }
}

  //
///// Timing support
 //

clock_t getTicktock () {
    return (clock());
}

double ticktockDifference (clock_t t1, clock_t t2) {
    // The difference in two TICKTOCK time values in seconds.
    // The resolution is implementation dependent but will normally
    // be some fractional value of a second.
  if (t2 >= t1) {
    return (((double)(t2 - t1)) / CLOCKS_PER_SEC);
  }
  else if (sizeof(long) == sizeof(clock_t)) {  // Wrapped around with longs
    return (((double) ((LONG_MAX - t1) + (t2 - LONG_MIN) - 1)) / CLOCKS_PER_SEC) ;
  }
  else { // We don't know the real size of the clock data type
    std::cerr << "ERROR: Clock time wrapped around and we don't know the size of clock_t" << std::endl;
    return NULL_FLOAT;
  }
}

double ticktockResolution () {
    // The minimum theoretically detectable resolution of the
    // difference in two TICKTOCK time values in seconds.  This resolution is
    // implementation dependent.  It may also not be realizable in practice,
    //  since the timing grain size may be larger than this resolution.
    return (1.0 / CLOCKS_PER_SEC);
  }


  //
///// Startup
 //

void startupCppPrimal() {
  // For Darwin dynamic libraries, we need to call GC_init() before
  // calling any other GC functions (cf. docs/README.darwin).
  // This shouldn't hurt any non-Darwin systems.
#ifdef STELLA_USE_GC
  GC_init();
#endif
}

} // end of namespace stella
