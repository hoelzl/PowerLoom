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


// Version: SymbolManager.java,v 1.3 2010/02/04 05:20:19 hans Exp

package edu.isi.powerloom.gui.parser;

import java.util.*;

/**
 * Class used for managing a symbol table.
 *
 * @since Mon May 06 15:27:17 2002
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */

public class SymbolManager {
    private HashMap nonTerminals = new HashMap();
    private HashMap terminals = new HashMap();
    private HashMap specialSymbols = new HashMap();
    private static SymbolManager theInstance = null;

    public static SymbolManager getInstance() {
        if (theInstance == null) {
            theInstance = new SymbolManager();
        }
        return theInstance;
    }

    private SymbolManager (){
        internSpecialSymbols();
    }

    private void internSpecialSymbols() {
        specialSymbols.put("EMPTY", Symbol.EMPTY);
        specialSymbols.put("DOT", Symbol.DOT);
        specialSymbols.put("$", Symbol.$);
    }

    public Symbol findSymbol(String symbolName) {
        if (nonTerminals.get(symbolName) != null) {
            return (Symbol)nonTerminals.get(symbolName);
        }
        if (terminals.get(symbolName) != null) {
            return (Symbol)terminals.get(symbolName);
        }
        if (specialSymbols.get(symbolName) != null) {
            return (Symbol)specialSymbols.get(symbolName);
        }
        return null;
        
    }

    public NonTerminal findOrCreateNonTerminal(String symbolName) {
        if (nonTerminals.get(symbolName) != null) {
            return (NonTerminal)nonTerminals.get(symbolName);
        }
        NonTerminal nonTerminal = new NonTerminal(symbolName);
        nonTerminals.put(symbolName, nonTerminal);
        return nonTerminal;
    }

    public Terminal findOrCreateTerminal(String symbolName) {
        if (terminals.get(symbolName) != null) {
            return (Terminal)terminals.get(symbolName);
        }
        Terminal terminal = new Terminal(symbolName);
        terminals.put(symbolName, terminal);
        return terminal;
    }
}
