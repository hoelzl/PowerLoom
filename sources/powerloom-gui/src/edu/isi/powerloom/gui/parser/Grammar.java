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


// Version: Grammar.java,v 1.4 2010/02/04 05:19:58 hans Exp

package edu.isi.powerloom.gui.parser;

import java.util.*;
import static edu.isi.powerloom.gui.common.Utils.*;

/**
 * Data Structure for representing a grammar.
 *
 * @since Mon May 06 13:22:16 2002
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */
public class Grammar {
    private Collection nonTerminals;
    private Collection terminals;
    private Collection productions;
    private NonTerminal startSymbol;
    private ParseTable parseTable;
    private HashMap firstCache = new HashMap();
    private HashMap followCache = new HashMap();

    public void setStartSymbol(NonTerminal startSymbol) {
        this.startSymbol = startSymbol;
    }

    public NonTerminal getStartSymbol() {
        return startSymbol;
    }

    public void setNonTerminals(Collection nonTerminals) {
        this.nonTerminals = nonTerminals;
    } 

    public void setTerminals(Collection terminals) {
        this.terminals = terminals;
    }

    public void setProductions(Collection productions) {
        this.productions = productions;
    }

    public Collection getNonTerminals() {
        return nonTerminals;
    }

    public Collection getTerminals() {
        return terminals;
    }

    public Collection getProductions() {
        return productions;
    }

    public Collection getProductionsForNonTerminal(NonTerminal nonTerminal) {
        ArrayList result = new ArrayList();
        Iterator iter = getProductions().iterator();
        while (iter.hasNext()) {
            Production prod = ((Production)iter.next());
            NonTerminal lhs = prod.getLHS();
            if (lhs == nonTerminal) {
                result.add(prod);
            }
        }
        return result;
    }

    /**
     * Compute first(X) for all nonterminals in grammar.
     */
    public void computeFirsts() {
        Collection nonTerms = getNonTerminals();
        Iterator ntIter = nonTerms.iterator();
        while (ntIter.hasNext()) {
            NonTerminal nt = (NonTerminal)ntIter.next();
            Set firstResult = first(nt);
            //debugPrintln(3, "first for " + nt + " : " + firstResult);
        }
    }

    /**
     *  From page 189 of Dragon Book.
     *  Input: a symbol
     *  Ouput: A set of terminals, which represent all possible first terminals
     *    in strings that are derivable from the symbol.
     */
    public Set first(Symbol symbol) {
        Set cachedFirst = (Set)firstCache.get(symbol);
        if (cachedFirst != null) {
            return cachedFirst;
        }
        if (symbol instanceof Terminal) {
            Set result = new HashSet();
            result.add(symbol);
            firstCache.put(symbol, result);
            return result;
        }
        // otherwise, we have a non terminal, so we must recursively compute
        // first based on the rhs of the nonterm
        if (symbol instanceof NonTerminal) {
            NonTerminal nonTerminal = (NonTerminal)symbol;
            Collection productions = getProductionsForNonTerminal(nonTerminal);
            Set firstResult = new HashSet();
            Iterator prodIter = productions.iterator();
            while (prodIter.hasNext()) {
                Production prod = (Production)prodIter.next();
                Set firstForProduction = getFirstForProduction(prod);
                firstResult.addAll(firstForProduction);
            }
            firstCache.put(symbol, firstResult);
            return firstResult;
        } else if (symbol == Symbol.EMPTY) {
            Set result = new HashSet();
            result.add(symbol);
            firstCache.put(symbol, result);
            return result;
        } else {
            // error
            debugPrintln(3, "grammar.first: Shouldn't be here!!");
            return null;
        } 
    }

    /**
     *  Input: a production
     *  Output:  All the first nonterminals for the production.
     */ 
    private Set getFirstForProduction(Production production) {
        List rhs = production.getRHS();
        return first(rhs);
    }

    /**
     *  From page 189 of Dragon Book.
     *  Input: a list of symbols, representing a string of terminals and nonterminals.
     *  Ouput: A set of terminals, which represent all possible first terminals
     *    in strings that are derivable from the string.
     */
    public Set first(List symbols) {
        Iterator iter = symbols.iterator();
        Set result = new HashSet();
        while (iter.hasNext()) {
            Symbol symbol = (Symbol)iter.next();
            Set firstForSymbol = first(symbol);
            result.addAll(firstForSymbol);
            // if the empty string isn't derivable from the current symbol,
            // no other symbols should be considered
            if (!firstForSymbol.contains(Symbol.EMPTY)) {
                return result;
            }
            // kludgy: remove the Empty symbol from the result, since we should
            // only add it at the end, when we know that all symbols derive Empty.
            result.remove(Symbol.EMPTY);
        }
        // if we've made it this far, empty is derivable from all symbols in the
        // rhs of the production.  So, empty is derivable from the LHS of the production
        result.add(Symbol.EMPTY);
        return result;
    }

    /**
     *  Compute follow (X) for all nonterminals in grammar.
     */ 
    public void computeFollows() {
        // Initialize the algorithm
        Set startSet = new HashSet();
        startSet.add(Symbol.$);
        followCache.put(getStartSymbol(), startSet);
        boolean changed = true;
        // Iterate until the follow sets have "settled", i.e., all possible
        // members have been found.
        while (changed) {
            changed = false;
            Iterator prodIter = getProductions().iterator();
            while (prodIter.hasNext()) {
                Production prod = (Production)prodIter.next();
                changed |= computeFollowForProduction(prod);
            }
        }
        Iterator nonTermIter = getNonTerminals().iterator();
        while (nonTermIter.hasNext()) {
            NonTerminal nonTerm = (NonTerminal)nonTermIter.next();
            //debugPrintln(3, "follow for " + nonTerm + " : " + follow(nonTerm));
        }
    }

    /**
     * @return boolean indicating whether new members were added to some Follow(x)
     */
    public boolean computeFollowForProduction(Production production) {
        // Iterate over each symbol in the rhs production, and compute follow for that
        // symbol based on the following string in the production
        boolean changed = false;
        List rhs = production.getRHS();
        Set result = new HashSet();
        for (int i = 0; i < rhs.size(); i++) {
            Symbol currSymbol = (Symbol)rhs.get(i);
            if (!(currSymbol instanceof NonTerminal)) {
                continue;  // we're not interested in terminals.
            }
            List rest = rhs.subList(i + 1, rhs.size());
            Set firstsOfRest = first(rest);
            // save result, except for EMPTY
            Set currFollow = follow((NonTerminal)currSymbol);
            int prevSize = currFollow.size();
            currFollow.addAll(firstsOfRest);
            currFollow.remove(Symbol.EMPTY);
            int newSize = currFollow.size();
            if (prevSize != newSize) {
                changed = true;
            }
            if ((firstsOfRest.contains(Symbol.EMPTY)) ||
                (i == (rhs.size() - 1))) {
                // step 3, p 189 of Dragon Book
                Set lhsFollow = follow(production.getLHS());
                // Add the lhs' follow to the current follow.  Note if there is any change
                // in the current follow.
                prevSize = currFollow.size();
                currFollow.addAll(lhsFollow);
                newSize = currFollow.size();
                if (prevSize != newSize) {
                    changed = true;
                }
            }             
        } 
        return changed;
    }

    /**
     *  From page 189 of Dragon Book.
     *  Input: a nonterminal
     *  Ouput: A set of terminals, representing all the terminals which
     *  can legally immediately follow this nonterminal.
     */
    public Set follow(NonTerminal nonTerminal) {
        // Assume that follow has already been computed by the computeFollow methods....
        Set cachedResult = (Set)followCache.get(nonTerminal);
        if (cachedResult == null) {
            cachedResult = new HashSet();
            followCache.put(nonTerminal, cachedResult);
        }
        return cachedResult;
    }

    
    /**
     *  Compute a parse table based on page 190 of the Dragon Book.
     */
    private void createParseTable() {
        parseTable = new ParseTable();
        computeFirsts();
        computeFollows();
        Iterator prodIter = getProductions().iterator();
        while (prodIter.hasNext()) {
            Production production = (Production)prodIter.next();
            populateParseTable(parseTable, production);
        }
    }

    // steps 2 and 3 of alg 4.4 in Dragon Book
    private void populateParseTable(ParseTable table, Production production) {
        NonTerminal lhs = production.getLHS();
        List rhs = production.getRHS();
        Set firsts = first(rhs);
        Set terminals = new HashSet(firsts);
        terminals.remove(Symbol.EMPTY);
        Iterator termIter = terminals.iterator();
        while (termIter.hasNext()) {
            Terminal terminal = (Terminal)termIter.next();
            table.putProduction(lhs, terminal, production);
        }
        if (firsts.contains(Symbol.EMPTY)) {
            Set follows = follow(lhs);
            termIter = follows.iterator();
            // Note: could contain $
            while (termIter.hasNext()) {
                Symbol terminal = (Symbol)termIter.next();
                table.putProduction(lhs, terminal, production);
            }
        }
    }

    public ParseTable getParseTable() {
        if (parseTable == null) {
            createParseTable();
        }
        return parseTable;
    }

    public String toString() {
        String result = "";
        Iterator iter;

        if (terminals != null) {
            result += "Terminals: ";
            iter = terminals.iterator();
            while (iter.hasNext()) {
                result += iter.next().toString() + " " ;
            } 
        } else {
            result += "Terminals: [none]";
        } 
        
        if (nonTerminals != null) {
            result += "\nNonTerminals: ";
            iter = nonTerminals.iterator();
            while (iter.hasNext()) {
                result += iter.next().toString() + " " ;
            } 
        } else {
            result += "NonTerminals: [none]";
        } 
        
        if (productions != null) {
            result += "\nProductions:\n";
            iter = productions.iterator();
            while (iter.hasNext()) {
                result += "  " + iter.next().toString() + "\n" ;
            } 
        } else {
            result += "Productions: [none]";
        } 
        
        result += "StartSymbol: " + startSymbol;
        return result;
    }
}
