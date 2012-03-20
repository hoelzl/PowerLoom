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


// Version: StringUtils.java,v 1.4 2010/02/04 05:16:32 hans Exp

package edu.isi.powerloom.gui.common;

import java.util.*;
import java.util.regex.*;


/**
 * Utility class for doing String manipulation.  This class provides utilities
 * for finding matching parens, compacting whitespace, etc.
 *
 * @since Wed Oct 16 11:47:43 2002
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */
public class StringUtils {

    private static class Interval {
	int start, end;

	public Interval(int start, int end) {
	    this.start = start;
	    this.end = end;
	}

	public boolean contains(int index) {
	    return ((index >= start) && (index <= end));
	}

	public String toString() {
	    return "[" + start + ", " + end + "]";
	}
    }

    private static class Intervals {
	List intervals = new ArrayList();

	public void addInterval(Interval interval) {
	    intervals.add(interval);
	}

	public boolean isIndexInInterval(int index) {
	    Iterator iter = intervals.iterator();
	    while(iter.hasNext()) {
		Interval interval = (Interval)iter.next();
		if (interval.contains(index)) {
		    return true;
		}
	    }
	    return false;
	}
	
	public String toString() {
	    String result = "";
	    Iterator iter = intervals.iterator();
	    while (iter.hasNext()) {
		Interval interval = (Interval)iter.next();
		result += interval.toString() + "\n";
	    }
	    return result;
	}

    }

    /**
     *  Strip out all redundant whitespace, and convert newlines to spaces.
     */
    public static String compactWhitespace(String inString) {
	StringBuffer sbResult = new StringBuffer();
	boolean gobblingWhitespace = false;
	for (int i = 0; i < inString.length(); i++) {
	    char c = inString.charAt(i);
	    // append whitespace on transition from whitespace to nonwhitespace
	    if (Character.isWhitespace(c)) {
		gobblingWhitespace = true;
	    } else {  // not whitespace
		if (gobblingWhitespace) {
		    gobblingWhitespace = false;
		    sbResult.append(' ');
		}
		sbResult.append(c);
	    }
	}
	return sbResult.toString();
    }


    /**
     * Find the position of the left paren of the enclosing set of parenthesis.
     * @return index of left paren if found, -1 if not found
     */
    public static int findParentLeftParen(String text, int currentPos) {
	Intervals stringIntervals = collectStringIntervals(text);
	if (stringIntervals.isIndexInInterval(currentPos)) {
	    return -1;
	}

	for (int i = currentPos - 1; i >= 0; i--) {
	    if (!stringIntervals.isIndexInInterval(i)) {
		if (text.charAt(i) == '(') {
		    return i;
		}
	    }
	}
	return -1;
    }


    /**
     * Find the position of the left paren of the first set of enclosed parenthesis.
     * @return 1 + index of left paren if found, -1 if not found
     */
    public static int findChildLeftParen(String text, int currentPos) {
	Intervals stringIntervals = collectStringIntervals(text);
	if (stringIntervals.isIndexInInterval(currentPos)) {
	    return -1;
	}

	for (int i = currentPos; i < text.length(); i++) {
	    if (!stringIntervals.isIndexInInterval(i)) {
		if (text.charAt(i) == '(') {
		    return i + 1;
		}
	    }
	}
	return -1;
    }

    public static int findMatchingLeftParen(String text, int rightParenPos) {
        // Cheesy little function that finds the position of a matching left
        //    parenthesis for the right parenthesis which is assumed to be
        //    in `text' at position `rightParenPos'.
        // Returns -1 if no match could be found.
        // TO DO: Account for syntactic complications such as comments, escape
        //    characters, etc.

	Intervals stringIntervals = collectStringIntervals(text);
        int balance = 1;
        int cursor = rightParenPos;

	if (stringIntervals.isIndexInInterval(rightParenPos)) {
	    // Hey! you can't do matching in a string!
	    return -1;
	}

        while ((balance != 0) && (cursor > 0)) {
            cursor--;
	    // ignore stuff inside strings
	    if (!stringIntervals.isIndexInInterval(cursor)) {
		switch (text.charAt(cursor)) {
		case '(': balance--;
		    break;
		case ')': balance++;
		    break;
		}
	    }
        }
        if (balance != 0)
            return -1;
        else
            return cursor;
    }


    public static int findMatchingRightParen(String text, int leftParenPos) {
        // Cheesy little function that finds the position of a matching right
        //    parenthesis for the left parenthesis which is assumed to be
        //    in `text' at position `leftParenPos'.
        // Returns -1 if no match could be found.
        // TO DO: Account for syntactic complications such as comments, escape
        //    characters, etc.
	Intervals stringIntervals = collectStringIntervals(text);
        int balance = 1;
        int cursor = leftParenPos;

	if (stringIntervals.isIndexInInterval(leftParenPos)) {
	    // Hey! you can't do matching in a string!
	    return -1;
	}

        while ((balance != 0) && (cursor < (text.length()  - 1))) {
            cursor++;
	    // ignore stuff inside strings
	    if (!stringIntervals.isIndexInInterval(cursor)) {
		switch (text.charAt(cursor)) {
		case '(': balance++;
		    break;
		case ')': balance--;
		    break;
		}
	    }
        }
        if (balance != 0)
            return -1;
        else
            return cursor;
    }


    private static Intervals collectStringIntervals(String string) {
	Intervals result = new Intervals();
	Pattern pattern = Pattern.compile("\"[^\"]*\"");
	Matcher matcher = pattern.matcher(string);
	boolean found = false;
	do {
	    found = matcher.find();
	    if (found) {
		String group = matcher.group();
		Interval interval = new Interval(matcher.start(), matcher.end() - 1);
		result.addInterval(interval);
	    }
	} while (found);
	return result;
    }

    public static void main(String args[]) {
	System.out.println("hi");
	// experiment: use x instead of quotes:
	/*
	String testString = "ab\"cd\"ef\"gh\"ab";
	System.out.println("TestString is: " + testString);
	Intervals intervals = collectStringIntervals(testString);
	System.out.println("String Intervals:");
	System.out.println(intervals);
	System.out.println("indexIsInString:");
	for (int i = 0; i < testString.length(); i++) {
	    System.out.println(i + " : " + intervals.isIndexInInterval(i));
	}
	*/
	String whiteyString = "(ab   cde\ndef gh)  ";
	String compactedString = compactWhitespace(whiteyString);
	System.out.println("whiteyString    = |" + whiteyString + "|");
	System.out.println("compactedString = |" + compactedString + "|");
    }

}// StringUtils
