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


// Version: ParseTable.java,v 1.3 2010/02/04 05:20:08 hans Exp

package edu.isi.powerloom.gui.parser;

import java.util.*;

/**
 * Table indexed by NonTerminal and Input Symbol.
 *
 * @since Tue May 07 15:15:28 2002
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */
public class ParseTable {
    private HashMap nonTerminalTable = new HashMap(); 

    public ParseTable () {
    }

    /**
     *  @return a collection of productions
     */
    public Set getProduction(NonTerminal nonTerminal, Symbol inputSymbol) {
        HashMap symbolTable = (HashMap)nonTerminalTable.get(nonTerminal);
        if (symbolTable == null) {
            return null;
        }
        return (Set)symbolTable.get(inputSymbol);
    }

    public void putProduction(NonTerminal nonTerminal, Symbol inputSymbol, Production production) {
        HashMap symbolTable = (HashMap)nonTerminalTable.get(nonTerminal);
        if (symbolTable == null) {
            symbolTable = new HashMap();
            nonTerminalTable.put(nonTerminal, symbolTable);
        }
        Set productions = (Set)symbolTable.get(inputSymbol);
        if (productions == null) {
            productions = new HashSet();
            symbolTable.put(inputSymbol, productions);
        }
        productions.add(production);
    }

    public String toString() {
        String result = "";
        Set ntEntrySet = nonTerminalTable.entrySet();
        Iterator outerIter = ntEntrySet.iterator();
        while (outerIter.hasNext()) {
            Map.Entry entry = (Map.Entry)outerIter.next();
            result += entry.getKey() + ": ";
            HashMap symbolTable = (HashMap)entry.getValue();
            Set symEntrySet = symbolTable.entrySet();
            Iterator innerIter = symEntrySet.iterator();
            while (innerIter.hasNext()) {
                Map.Entry symEntry = (Map.Entry)innerIter.next();
                result += "[" + symEntry.getKey() + " : " + symEntry.getValue() + "] ";
            }
            result += "\n";
        }
        return result;
    }
}
