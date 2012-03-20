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


// Version: Yytoken.java,v 1.4 2010/02/04 05:20:22 hans Exp

package edu.isi.powerloom.gui.parser;

/**
 * Class representing a scanned token.
 */
public class Yytoken {
  Yytoken 
    (
     String type,
     String text,
     int line,
     int charBegin,
     int charEnd
     )
      {
        m_type = type;
	m_text = new String(text);
	m_line = line;
	m_charBegin = charBegin;
	m_charEnd = charEnd;
      }

  public String m_type;
  public String m_text;
  public int m_line;
  public int m_charBegin;
  public int m_charEnd;
  public String toString() {
      return "|TOK|"+m_type+": "+m_text+" (line "+m_line+", charBegin=" + m_charBegin + ", charEnd=" + m_charEnd +")";
  }

    public String getText() {
        return m_text;
    }

    public String getName() {
	int indexOfLastSlash = m_text.lastIndexOf('/');
	if (indexOfLastSlash < 0) {
	    return m_text;
	}
	return m_text.substring(indexOfLastSlash + 1);
    }

    public String getModule() {
	int indexOfLastSlash = m_text.lastIndexOf('/');
	if (indexOfLastSlash < 0) {
	    return null;
	}
	return m_text.substring(0, indexOfLastSlash);
    }

    public String getType() {
        return m_type;
    }

    public int getCharBegin() {
        return m_charBegin;
    }

    public int getCharEnd() {
        return m_charEnd;
    }

    public int getLine() {
        return m_line;
    }
}
