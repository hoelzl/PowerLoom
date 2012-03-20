// -*- Mode: Java -*-

/*---------------------------- BEGIN LICENSE BLOCK ---------------------------+
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
 | The Original Code is the PowerLoom KR&R System.                            |
 |                                                                            |
 | The Initial Developer of the Original Code is                              |
 | UNIVERSITY OF SOUTHERN CALIFORNIA, INFORMATION SCIENCES INSTITUTE          |
 | 4676 Admiralty Way, Marina Del Rey, California 90292, U.S.A.               |
 |                                                                            |
 | Portions created by the Initial Developer are Copyright (C) 2002-2010      |
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
 +----------------------------- END LICENSE BLOCK ---------------------------*/


// Version: Scanner.java,v 1.8 2010/02/04 05:20:15 hans Exp

package edu.isi.powerloom.gui.parser;
import java.lang.System;
import java.util.*;
import java.io.*;
import static edu.isi.powerloom.gui.common.Utils.*;

/**
 * Scanner which is automatically generated from JLex and the Scanner spec
 * in the resources directory.
 */
public class Scanner {
    /**
     * @return a List of YyTokens
     */
    public static List scan(String inputString) {
	List result = new ArrayList();
	Yylex yy = new Yylex(new StringReader(inputString));
	Yytoken t;
	try {
	    while ((t = yy.yylex()) != null) {
		result.add(t);
	    }
	} catch (IOException e) {
	    e.printStackTrace();
	    return null;
	}
	Yytoken endToken = new Yytoken("$", "", 0, 0, 0);
	result.add(endToken);
	return result;
    }
    public static void testScan() {
	String input = "(rel domain range)";
        List tokens = scan(input);
        debugPrintln(3, "result of scan: " );
        Iterator iter = tokens.iterator();
        while (iter.hasNext()) {
	    Yytoken token = (Yytoken)iter.next();
	    debugPrintln(3, "  " + token);
	}
    }
    public static void main(String argv[]) throws java.io.IOException {
	testScan();
	/*
	Yylex yy = new Yylex(System.in);
	Yytoken t;
	while ((t = yy.yylex()) != null)
	    System.out.println(t);
	*/
    }
}
class PLUtility {
  public static void plAssert
    (
     boolean expr
     )
      { 
	if (false == expr) {
	  throw (new Error("Error: Assertion failed."));
	}
      }
  private static final String errorMsg[] = {
    "Error: Unmatched end-of-comment punctuation.",
    "Error: Unmatched start-of-comment punctuation.",
    "Error: Unclosed string.",
    "Error: Illegal character."
    };
  public static final int E_ENDCOMMENT = 0; 
  public static final int E_STARTCOMMENT = 1; 
  public static final int E_UNCLOSEDSTR = 2; 
  public static final int E_UNMATCHED = 3; 
  public static void error
    (
     int code
     )
      {
	System.out.println(errorMsg[code]);
      }
}


class Yylex {
	private final int YY_BUFFER_SIZE = 512;
	private final int YY_F = -1;
	private final int YY_NO_STATE = -1;
	private final int YY_NOT_ACCEPT = 0;
	private final int YY_START = 1;
	private final int YY_END = 2;
	private final int YY_NO_ANCHOR = 4;
	private final int YY_BOL = 128;
	private final int YY_EOF = 129;

  private int comment_count = 0;
	private java.io.BufferedReader yy_reader;
	private int yy_buffer_index;
	private int yy_buffer_read;
	private int yy_buffer_start;
	private int yy_buffer_end;
	private char yy_buffer[];
	private int yychar;
	private int yyline;
	private boolean yy_at_bol;
	private int yy_lexical_state;

	Yylex (java.io.Reader reader) {
		this ();
		if (null == reader) {
			throw (new Error("Error: Bad input stream initializer."));
		}
		yy_reader = new java.io.BufferedReader(reader);
	}

	Yylex (java.io.InputStream instream) {
		this ();
		if (null == instream) {
			throw (new Error("Error: Bad input stream initializer."));
		}
		yy_reader = new java.io.BufferedReader(new java.io.InputStreamReader(instream));
	}

	private Yylex () {
		yy_buffer = new char[YY_BUFFER_SIZE];
		yy_buffer_read = 0;
		yy_buffer_index = 0;
		yy_buffer_start = 0;
		yy_buffer_end = 0;
		yychar = 0;
		yyline = 0;
		yy_at_bol = true;
		yy_lexical_state = YYINITIAL;
	}

	private boolean yy_eof_done = false;
	private final int YYINITIAL = 0;
	private final int COMMENT = 1;
	private final int yy_state_dtrans[] = {
		0,
		35
	};
	private void yybegin (int state) {
		yy_lexical_state = state;
	}
	private int yy_advance ()
		throws java.io.IOException {
		int next_read;
		int i;
		int j;

		if (yy_buffer_index < yy_buffer_read) {
			return yy_buffer[yy_buffer_index++];
		}

		if (0 != yy_buffer_start) {
			i = yy_buffer_start;
			j = 0;
			while (i < yy_buffer_read) {
				yy_buffer[j] = yy_buffer[i];
				++i;
				++j;
			}
			yy_buffer_end = yy_buffer_end - yy_buffer_start;
			yy_buffer_start = 0;
			yy_buffer_read = j;
			yy_buffer_index = j;
			next_read = yy_reader.read(yy_buffer,
					yy_buffer_read,
					yy_buffer.length - yy_buffer_read);
			if (-1 == next_read) {
				return YY_EOF;
			}
			yy_buffer_read = yy_buffer_read + next_read;
		}

		while (yy_buffer_index >= yy_buffer_read) {
			if (yy_buffer_index >= yy_buffer.length) {
				yy_buffer = yy_double(yy_buffer);
			}
			next_read = yy_reader.read(yy_buffer,
					yy_buffer_read,
					yy_buffer.length - yy_buffer_read);
			if (-1 == next_read) {
				return YY_EOF;
			}
			yy_buffer_read = yy_buffer_read + next_read;
		}
		return yy_buffer[yy_buffer_index++];
	}
	private void yy_move_end () {
		if (yy_buffer_end > yy_buffer_start &&
		    '\n' == yy_buffer[yy_buffer_end-1])
			yy_buffer_end--;
		if (yy_buffer_end > yy_buffer_start &&
		    '\r' == yy_buffer[yy_buffer_end-1])
			yy_buffer_end--;
	}
	private boolean yy_last_was_cr=false;
	private void yy_mark_start () {
		int i;
		for (i = yy_buffer_start; i < yy_buffer_index; ++i) {
			if ('\n' == yy_buffer[i] && !yy_last_was_cr) {
				++yyline;
			}
			if ('\r' == yy_buffer[i]) {
				++yyline;
				yy_last_was_cr=true;
			} else yy_last_was_cr=false;
		}
		yychar = yychar
			+ yy_buffer_index - yy_buffer_start;
		yy_buffer_start = yy_buffer_index;
	}
	private void yy_mark_end () {
		yy_buffer_end = yy_buffer_index;
	}
	private void yy_to_mark () {
		yy_buffer_index = yy_buffer_end;
		yy_at_bol = (yy_buffer_end > yy_buffer_start) &&
		            ('\r' == yy_buffer[yy_buffer_end-1] ||
		             '\n' == yy_buffer[yy_buffer_end-1] ||
		             2028/*LS*/ == yy_buffer[yy_buffer_end-1] ||
		             2029/*PS*/ == yy_buffer[yy_buffer_end-1]);
	}
	private java.lang.String yytext () {
		return (new java.lang.String(yy_buffer,
			yy_buffer_start,
			yy_buffer_end - yy_buffer_start));
	}
	private int yylength () {
		return yy_buffer_end - yy_buffer_start;
	}
	private char[] yy_double (char buf[]) {
		int i;
		char newbuf[];
		newbuf = new char[2*buf.length];
		for (i = 0; i < buf.length; ++i) {
			newbuf[i] = buf[i];
		}
		return newbuf;
	}
	private final int YY_E_INTERNAL = 0;
	private final int YY_E_MATCH = 1;
	private java.lang.String yy_error_string[] = {
		"Error: Internal error.\n",
		"Error: Unmatched input.\n"
	};
	private void yy_error (int code,boolean fatal) {
		java.lang.System.out.print(yy_error_string[code]);
		java.lang.System.out.flush();
		if (fatal) {
			throw new Error("Fatal Error.\n");
		}
	}
	private int[][] unpackFromString(int size1, int size2, String st) {
		int colonIndex = -1;
		String lengthString;
		int sequenceLength = 0;
		int sequenceInteger = 0;

		int commaIndex;
		String workString;

		int res[][] = new int[size1][size2];
		for (int i= 0; i < size1; i++) {
			for (int j= 0; j < size2; j++) {
				if (sequenceLength != 0) {
					res[i][j] = sequenceInteger;
					sequenceLength--;
					continue;
				}
				commaIndex = st.indexOf(',');
				workString = (commaIndex==-1) ? st :
					st.substring(0, commaIndex);
				st = st.substring(commaIndex+1);
				colonIndex = workString.indexOf(':');
				if (colonIndex == -1) {
					res[i][j]=Integer.parseInt(workString);
					continue;
				}
				lengthString =
					workString.substring(colonIndex+1);
				sequenceLength=Integer.parseInt(lengthString);
				workString=workString.substring(0,colonIndex);
				sequenceInteger=Integer.parseInt(workString);
				res[i][j] = sequenceInteger;
				sequenceLength--;
			}
		}
		return res;
	}
	private int yy_acpt[] = {
		/* 0 */ YY_NOT_ACCEPT,
		/* 1 */ YY_NO_ANCHOR,
		/* 2 */ YY_NO_ANCHOR,
		/* 3 */ YY_NO_ANCHOR,
		/* 4 */ YY_NO_ANCHOR,
		/* 5 */ YY_NO_ANCHOR,
		/* 6 */ YY_NO_ANCHOR,
		/* 7 */ YY_NO_ANCHOR,
		/* 8 */ YY_NO_ANCHOR,
		/* 9 */ YY_NO_ANCHOR,
		/* 10 */ YY_NO_ANCHOR,
		/* 11 */ YY_NO_ANCHOR,
		/* 12 */ YY_NO_ANCHOR,
		/* 13 */ YY_NO_ANCHOR,
		/* 14 */ YY_NO_ANCHOR,
		/* 15 */ YY_NO_ANCHOR,
		/* 16 */ YY_NO_ANCHOR,
		/* 17 */ YY_NO_ANCHOR,
		/* 18 */ YY_NO_ANCHOR,
		/* 19 */ YY_NO_ANCHOR,
		/* 20 */ YY_NO_ANCHOR,
		/* 21 */ YY_NO_ANCHOR,
		/* 22 */ YY_NO_ANCHOR,
		/* 23 */ YY_NO_ANCHOR,
		/* 24 */ YY_NO_ANCHOR,
		/* 25 */ YY_NO_ANCHOR,
		/* 26 */ YY_NO_ANCHOR,
		/* 27 */ YY_NO_ANCHOR,
		/* 28 */ YY_NO_ANCHOR,
		/* 29 */ YY_NO_ANCHOR,
		/* 30 */ YY_NO_ANCHOR,
		/* 31 */ YY_NO_ANCHOR,
		/* 32 */ YY_NO_ANCHOR,
		/* 33 */ YY_NO_ANCHOR,
		/* 34 */ YY_NO_ANCHOR,
		/* 35 */ YY_NO_ANCHOR,
		/* 36 */ YY_NO_ANCHOR,
		/* 37 */ YY_NO_ANCHOR,
		/* 38 */ YY_NO_ANCHOR,
		/* 39 */ YY_NOT_ACCEPT,
		/* 40 */ YY_NO_ANCHOR,
		/* 41 */ YY_NO_ANCHOR,
		/* 42 */ YY_NO_ANCHOR,
		/* 43 */ YY_NO_ANCHOR,
		/* 44 */ YY_NO_ANCHOR,
		/* 45 */ YY_NOT_ACCEPT,
		/* 46 */ YY_NO_ANCHOR,
		/* 47 */ YY_NO_ANCHOR,
		/* 48 */ YY_NO_ANCHOR,
		/* 49 */ YY_NO_ANCHOR,
		/* 50 */ YY_NOT_ACCEPT,
		/* 51 */ YY_NO_ANCHOR,
		/* 52 */ YY_NO_ANCHOR,
		/* 53 */ YY_NO_ANCHOR,
		/* 54 */ YY_NOT_ACCEPT,
		/* 55 */ YY_NO_ANCHOR,
		/* 56 */ YY_NO_ANCHOR,
		/* 57 */ YY_NO_ANCHOR,
		/* 58 */ YY_NO_ANCHOR,
		/* 59 */ YY_NO_ANCHOR,
		/* 60 */ YY_NO_ANCHOR,
		/* 61 */ YY_NO_ANCHOR,
		/* 62 */ YY_NO_ANCHOR,
		/* 63 */ YY_NO_ANCHOR,
		/* 64 */ YY_NO_ANCHOR,
		/* 65 */ YY_NO_ANCHOR,
		/* 66 */ YY_NO_ANCHOR,
		/* 67 */ YY_NO_ANCHOR,
		/* 68 */ YY_NO_ANCHOR,
		/* 69 */ YY_NO_ANCHOR,
		/* 70 */ YY_NO_ANCHOR,
		/* 71 */ YY_NO_ANCHOR,
		/* 72 */ YY_NO_ANCHOR,
		/* 73 */ YY_NO_ANCHOR,
		/* 74 */ YY_NO_ANCHOR,
		/* 75 */ YY_NO_ANCHOR,
		/* 76 */ YY_NO_ANCHOR,
		/* 77 */ YY_NO_ANCHOR,
		/* 78 */ YY_NO_ANCHOR,
		/* 79 */ YY_NO_ANCHOR,
		/* 80 */ YY_NO_ANCHOR,
		/* 81 */ YY_NO_ANCHOR,
		/* 82 */ YY_NO_ANCHOR,
		/* 83 */ YY_NO_ANCHOR,
		/* 84 */ YY_NO_ANCHOR,
		/* 85 */ YY_NO_ANCHOR,
		/* 86 */ YY_NO_ANCHOR,
		/* 87 */ YY_NO_ANCHOR,
		/* 88 */ YY_NO_ANCHOR,
		/* 89 */ YY_NO_ANCHOR,
		/* 90 */ YY_NO_ANCHOR,
		/* 91 */ YY_NO_ANCHOR
	};
	private int yy_cmap[] = unpackFromString(1,130,
"36:8,9:2,10,36:2,12,36:18,9,34,13,34:4,36,1,2,11,34,36,33,36,5,33:10,36:2,7" +
",4,6,32,34,23,27,33,28,21,20,33,22,16,33,24,15,26,31,19,25,33,29,17,18,33:3" +
",30,33:2,3,14,36,34,33,36,23,27,33,28,21,20,33,22,16,33,24,15,26,31,19,25,3" +
"3,29,17,18,33:3,30,33:2,36,35,36,8,36,0:2")[0];

	private int yy_rmap[] = unpackFromString(1,92,
"0,1:4,2,3,4,5,6,7,8,9:2,10,1,11,1,9:4,1:2,9:3,1,12,9:6,13,1:3,14,9,1,15,6,1" +
"6,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,4" +
"1,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63")[0];

	private int yy_nxt[][] = unpackFromString(64,37,
"1,2,3,4,5,6,40,46,7,8:2,40,-1,9,41,89,40,83,67,51,90,91,40,68,84,40:6,69,10" +
",40:2,47,41,-1:41,40:2,11,40,-1:3,40,-1:3,40:17,-1,40:2,-1:6,12,40:3,-1:3,1" +
"3,-1:3,40:17,-1,40:2,-1:8,16,-1:39,8:2,-1:27,9:9,-1,9:2,17,42,9:22,-1:15,10" +
":17,-1,10,-1:7,40:2,19,40,-1:3,40,-1:3,40:17,-1,40:2,-1:6,40:4,-1:3,40,-1:3" +
",40:17,-1,40:2,-1:6,40:2,20,40,-1:3,40,-1:3,40:17,-1,40:2,-1:8,23,-1:34,40:" +
"4,-1:3,40,-1:3,40:8,76,40:8,-1,40:2,-1:2,1,44:4,52,44:4,36,56,44:25,-1:4,39" +
":4,-1,39:3,-1:3,39:17,-1,39:2,27,-1:2,9:8,48,45,9:2,43,42,9:22,-1,44:4,50,4" +
"4:4,-1,54,44:25,-1:9,45:2,-1:3,9,-1:26,14,40:2,55,15,-1:2,40,-1:3,40:17,-1," +
"40:2,-1:6,39:4,-1,39:3,-1:3,39:17,-1,39:2,-1:3,9:8,48,45,9:2,17,42,9:22,-1," +
"44:4,49,44:4,-1,54,44:25,-1,44:4,49,44:4,-1:2,44:25,-1:4,40:4,-1:3,40,-1:3," +
"40:14,18,40:2,-1,40:2,-1:3,44:4,49,44:4,-1,37,44:25,-1,44:4,50,44:4,-1,53,4" +
"4:25,-1,44:4,-1,44:4,-1,53,44:25,-1:4,21,40:3,22,-1:2,40,-1:3,40:17,-1,40:2" +
",-1:3,44:4,38,44:4,-1,53,44:25,-1:4,40:4,-1:3,40,-1:3,40:6,24,40:10,-1,40:2" +
",-1:6,40:4,-1:3,40,-1:3,40:13,25,40:3,-1,40:2,-1:6,40:4,-1:3,40,-1:3,40:3,2" +
"6,40:13,-1,40:2,-1:6,40:4,-1:3,40,-1:3,40:5,28,40:11,-1,40:2,-1:6,40:4,-1:3" +
",40,-1:3,40:8,29,40:8,-1,40:2,-1:6,40:4,-1:3,40,-1:3,40:5,30,40:11,-1,40:2," +
"-1:6,40:4,-1:3,40,-1:3,40:8,31,40:8,-1,40:2,-1:6,40:4,-1:3,40,-1:3,32,40:16" +
",-1,40:2,-1:6,40:4,-1:3,40,-1:3,40:2,33,40:14,-1,40:2,-1:6,40:4,-1:3,40,-1:" +
"3,34,40:16,-1,40:2,-1:6,40:4,-1:3,40,-1:3,40:7,57,40:9,-1,40:2,-1:6,40:4,-1" +
":3,40,-1:3,40:16,58,-1,40:2,-1:6,40:4,-1:3,40,-1:3,40:4,59,40:12,-1,40:2,-1" +
":6,40:4,-1:3,40,-1:3,40:4,60,40:12,-1,40:2,-1:6,40:4,-1:3,40,-1:3,40:10,61," +
"40:6,-1,40:2,-1:6,40:4,-1:3,40,-1:3,40:4,62,40:12,-1,40:2,-1:6,40:4,-1:3,40" +
",-1:3,40:13,63,40:3,-1,40:2,-1:6,40:4,-1:3,40,-1:3,64,40:16,-1,40:2,-1:6,40" +
":4,-1:3,40,-1:3,40:3,65,40:13,-1,40:2,-1:6,40:4,-1:3,40,-1:3,66,40:16,-1,40" +
":2,-1:6,40:4,-1:3,40,-1:3,40:3,70,40:13,-1,40:2,-1:6,40:4,-1:3,40,-1:3,40:1" +
"0,71,40:6,-1,40:2,-1:6,40:4,-1:3,40,-1:3,40:3,72,40:13,-1,40:2,-1:6,40:4,-1" +
":3,40,-1:3,40:12,73,40:4,-1,40:2,-1:6,40:4,-1:3,40,-1:3,40:8,74,40:8,-1,40:" +
"2,-1:6,40:4,-1:3,40,-1:3,40:2,75,40:14,-1,40:2,-1:6,40:4,-1:3,40,-1:3,40:6," +
"77,40:10,-1,40:2,-1:6,40:4,-1:3,40,-1:3,40:8,78,40:8,-1,40:2,-1:6,40:4,-1:3" +
",40,-1:3,40:2,79,40:14,-1,40:2,-1:6,40:4,-1:3,40,-1:3,40:11,80,40:5,-1,40:2" +
",-1:6,40:4,-1:3,40,-1:3,40:14,81,40:2,-1,40:2,-1:6,40:4,-1:3,40,-1:3,40,82," +
"40:15,-1,40:2,-1:6,40:4,-1:3,40,-1:3,40,85,40:6,86,40:8,-1,40:2,-1:6,40:4,-" +
"1:3,40,-1:3,40:4,87,40:12,-1,40:2,-1:6,40:4,-1:3,40,-1:3,40:15,88,40,-1,40:" +
"2,-1:2");

	public Yytoken yylex ()
		throws java.io.IOException {
		int yy_lookahead;
		int yy_anchor = YY_NO_ANCHOR;
		int yy_state = yy_state_dtrans[yy_lexical_state];
		int yy_next_state = YY_NO_STATE;
		int yy_last_accept_state = YY_NO_STATE;
		boolean yy_initial = true;
		int yy_this_accept;

		yy_mark_start();
		yy_this_accept = yy_acpt[yy_state];
		if (YY_NOT_ACCEPT != yy_this_accept) {
			yy_last_accept_state = yy_state;
			yy_mark_end();
		}
		while (true) {
			if (yy_initial && yy_at_bol) yy_lookahead = YY_BOL;
			else yy_lookahead = yy_advance();
			yy_next_state = YY_F;
			yy_next_state = yy_nxt[yy_rmap[yy_state]][yy_cmap[yy_lookahead]];
			if (YY_EOF == yy_lookahead && true == yy_initial) {
				return null;
			}
			if (YY_F != yy_next_state) {
				yy_state = yy_next_state;
				yy_initial = false;
				yy_this_accept = yy_acpt[yy_state];
				if (YY_NOT_ACCEPT != yy_this_accept) {
					yy_last_accept_state = yy_state;
					yy_mark_end();
				}
			}
			else {
				if (YY_NO_STATE == yy_last_accept_state) {
					throw (new Error("Lexical Error: Unmatched Input."));
				}
				else {
					yy_anchor = yy_acpt[yy_last_accept_state];
					if (0 != (YY_END & yy_anchor)) {
						yy_move_end();
					}
					yy_to_mark();
					switch (yy_last_accept_state) {
					case 1:
						
					case -2:
						break;
					case 2:
						{ return (new Yytoken("lparen",yytext(),yyline,yychar,yychar+1)); }
					case -3:
						break;
					case 3:
						{ return (new Yytoken("rparen",yytext(),yyline,yychar,yychar+1)); }
					case -4:
						break;
					case 4:
						{ return (new Yytoken("lbracket",yytext(),yyline,yychar,yychar+1)); }
					case -5:
						break;
					case 5:
						{ return (new Yytoken("equal",yytext(),yyline,yychar,yychar+1)); }
					case -6:
						break;
					case 6:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -7:
						break;
					case 7:
						{
        debugPrintln(3, "Illegal character: <" + yytext() + ">");
	PLUtility.error(PLUtility.E_UNMATCHED);
}
					case -8:
						break;
					case 8:
						{ }
					case -9:
						break;
					case 9:
						{
	String str =  yytext().substring(1,yytext().length());
	PLUtility.error(PLUtility.E_UNCLOSEDSTR);
	PLUtility.plAssert(str.length() == yytext().length() - 1);
	return (new Yytoken("unclosedstring",str,yyline,yychar,yychar + str.length()));
}
					case -10:
						break;
					case 10:
						{
	return (new Yytoken("indvar",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -11:
						break;
					case 11:
						{ return (new Yytoken("implies",yytext(),yyline,yychar,yychar+2)); }
					case -12:
						break;
					case 12:
						{ return (new Yytoken("notequal",yytext(),yyline,yychar,yychar+2)); }
					case -13:
						break;
					case 13:
						{ yybegin(COMMENT); comment_count = comment_count + 1; }
					case -14:
						break;
					case 14:
						{ return (new Yytoken("reverseimplies",yytext(),yyline,yychar,yychar+2)); }
					case -15:
						break;
					case 15:
						{ return (new Yytoken("squigglereverseimplies",yytext(),yyline,yychar,yychar+3)); }
					case -16:
						break;
					case 16:
						{ return (new Yytoken("squiggleimplies",yytext(),yyline,yychar,yychar+3)); }
					case -17:
						break;
					case 17:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -18:
						break;
					case 18:
						{
	return (new Yytoken("or",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -19:
						break;
					case 19:
						{ return (new Yytoken("doubleimplies",yytext(),yyline,yychar,yychar+3)); }
					case -20:
						break;
					case 20:
						{ return (new Yytoken("iff",yytext(),yyline,yychar,yychar+3)); }
					case -21:
						break;
					case 21:
						{ return (new Yytoken("doublereverseimplies",yytext(),yyline,yychar,yychar+3)); }
					case -22:
						break;
					case 22:
						{ return (new Yytoken("doublesquigglereverseimplies",yytext(),yyline,yychar,yychar+3)); }
					case -23:
						break;
					case 23:
						{ return (new Yytoken("doublesquiggleimplies",yytext(),yyline,yychar,yychar+3)); }
					case -24:
						break;
					case 24:
						{
	return (new Yytoken("the",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -25:
						break;
					case 25:
						{
	return (new Yytoken("and",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -26:
						break;
					case 26:
						{
	return (new Yytoken("not",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -27:
						break;
					case 27:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -28:
						break;
					case 28:
						{
	return (new Yytoken("setof",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -29:
						break;
					case 29:
						{
	return (new Yytoken("kappa",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -30:
						break;
					case 30:
						{
	return (new Yytoken("listof",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -31:
						break;
					case 31:
						{
	return (new Yytoken("lambda",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -32:
						break;
					case 32:
						{
	return (new Yytoken("forall",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -33:
						break;
					case 33:
						{
	return (new Yytoken("exists",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -34:
						break;
					case 34:
						{
	return (new Yytoken("setofall",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -35:
						break;
					case 35:
						{ }
					case -36:
						break;
					case 36:
						{ }
					case -37:
						break;
					case 37:
						{ comment_count = comment_count + 1; }
					case -38:
						break;
					case 38:
						{ 
	comment_count = comment_count - 1; 
	PLUtility.plAssert(comment_count >= 0);
	if (comment_count == 0) {
    		yybegin(YYINITIAL);
	}
}
					case -39:
						break;
					case 40:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -40:
						break;
					case 41:
						{
        debugPrintln(3, "Illegal character: <" + yytext() + ">");
	PLUtility.error(PLUtility.E_UNMATCHED);
}
					case -41:
						break;
					case 42:
						{
	String str =  yytext().substring(1,yytext().length());
	PLUtility.error(PLUtility.E_UNCLOSEDSTR);
	PLUtility.plAssert(str.length() == yytext().length() - 1);
	return (new Yytoken("unclosedstring",str,yyline,yychar,yychar + str.length()));
}
					case -42:
						break;
					case 43:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -43:
						break;
					case 44:
						{ }
					case -44:
						break;
					case 46:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -45:
						break;
					case 47:
						{
        debugPrintln(3, "Illegal character: <" + yytext() + ">");
	PLUtility.error(PLUtility.E_UNMATCHED);
}
					case -46:
						break;
					case 48:
						{
	String str =  yytext().substring(1,yytext().length());
	PLUtility.error(PLUtility.E_UNCLOSEDSTR);
	PLUtility.plAssert(str.length() == yytext().length() - 1);
	return (new Yytoken("unclosedstring",str,yyline,yychar,yychar + str.length()));
}
					case -47:
						break;
					case 49:
						{ }
					case -48:
						break;
					case 51:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -49:
						break;
					case 52:
						{
        debugPrintln(3, "Illegal character: <" + yytext() + ">");
	PLUtility.error(PLUtility.E_UNMATCHED);
}
					case -50:
						break;
					case 53:
						{ }
					case -51:
						break;
					case 55:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -52:
						break;
					case 56:
						{
        debugPrintln(3, "Illegal character: <" + yytext() + ">");
	PLUtility.error(PLUtility.E_UNMATCHED);
}
					case -53:
						break;
					case 57:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -54:
						break;
					case 58:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -55:
						break;
					case 59:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -56:
						break;
					case 60:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -57:
						break;
					case 61:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -58:
						break;
					case 62:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -59:
						break;
					case 63:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -60:
						break;
					case 64:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -61:
						break;
					case 65:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -62:
						break;
					case 66:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -63:
						break;
					case 67:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -64:
						break;
					case 68:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -65:
						break;
					case 69:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -66:
						break;
					case 70:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -67:
						break;
					case 71:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -68:
						break;
					case 72:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -69:
						break;
					case 73:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -70:
						break;
					case 74:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -71:
						break;
					case 75:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -72:
						break;
					case 76:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -73:
						break;
					case 77:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -74:
						break;
					case 78:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -75:
						break;
					case 79:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -76:
						break;
					case 80:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -77:
						break;
					case 81:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -78:
						break;
					case 82:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -79:
						break;
					case 83:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -80:
						break;
					case 84:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -81:
						break;
					case 85:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -82:
						break;
					case 86:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -83:
						break;
					case 87:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -84:
						break;
					case 88:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -85:
						break;
					case 89:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -86:
						break;
					case 90:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -87:
						break;
					case 91:
						{
	return (new Yytoken("constant",yytext(),yyline,yychar,yychar + yytext().length()));
}
					case -88:
						break;
					default:
						yy_error(YY_E_INTERNAL,false);
					case -1:
					}
					yy_initial = true;
					yy_state = yy_state_dtrans[yy_lexical_state];
					yy_next_state = YY_NO_STATE;
					yy_last_accept_state = YY_NO_STATE;
					yy_mark_start();
					yy_this_accept = yy_acpt[yy_state];
					if (YY_NOT_ACCEPT != yy_this_accept) {
						yy_last_accept_state = yy_state;
						yy_mark_end();
					}
				}
			}
		}
	}
}
