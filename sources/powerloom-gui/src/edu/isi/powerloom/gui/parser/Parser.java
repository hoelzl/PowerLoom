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


// Version: Parser.java,v 1.5 2010/02/04 05:20:09 hans Exp

package edu.isi.powerloom.gui.parser;

import java.util.*;
import static edu.isi.powerloom.gui.common.Utils.*;

/**
 * Backtracking parser, which "forks" the stack when there are multiple productions
 * which can be applied to a given stack/input configuration.
 * This is adapted from alg 4.14. p. 187 in Compilers by Aho, Sethi, Ullman.
 *
 * @since Tue May 07 17:52:21 2002
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */
public class Parser {
    private static boolean debug = false;

    /**
     * @return a ParseResult structure which contains information about the success status,
     * fired productions (if success), and exceptions.
     */
    public static ParseResult parse(Grammar grammar, List input) throws Exception {
        // initialize stack and input pointer
        int ip = 0;  // first position in input
        Stack stack = new Stack();
        stack.push(Symbol.$);
        stack.push(grammar.getStartSymbol());
        ParseResult parseResult = new ParseResult();
        List firedProductions = new ArrayList();
        parse(grammar, stack, input, ip, firedProductions, parseResult);
        return parseResult;
    }

    /**
     * @return List of productions if successful, null if failure.
     */
    private static void parse(Grammar grammar, Stack stack, List input, int ip, 
                              List firedProductions, ParseResult parseResult) throws Exception {
        if (stack.peek() == Symbol.$) {  // we're all done...
            parseResult.isSuccess = true;
            parseResult.firedProductions = firedProductions;
            return;
        }
        ParseTable table = grammar.getParseTable();
        do {
            if (debug) {
                printConfiguration(stack, input, ip);                
            }
            Symbol stackSym = (Symbol)stack.peek();
            // Get current input symbol
            Yytoken token = (Yytoken)input.get(ip);
            Symbol inputSym = SymbolManager.getInstance().findSymbol(token.getType());
            if (inputSym == null) {
                throw new Exception("No matching type for token: " + token);
            }
            if ((stackSym instanceof Terminal) || (stackSym == Symbol.$)) {
                if (stackSym == inputSym) {
                    stack.pop();
                    ip++;
                } else {
                    ParseException pe = new StackInputMismatchException(stackSym, inputSym);
                    parseResult.parseExceptions.add(pe);
                    if (debug) {
                        debugPrintln(3, "Parse Exception: top of stack (" + stackSym + ") does not match current input (" + inputSym + ")");                        
                    }
                    return;
                } 
            } else {  // stackSym is a nonterminal
                Set productions = table.getProduction((NonTerminal)stackSym, inputSym);
                if (productions == null) {
                    ParseException pe = new NoProductionsException(stackSym, inputSym);
                    parseResult.parseExceptions.add(pe);
                    if (debug) {
                        debugPrintln(3, "Parse Exception: No Productions found for stackSym : " + stackSym + ", inputsym = " + inputSym);
                    }
                    return;
                }
                // for testing... reverse list (just so backtrack.in example is tested)
                List prodList = new ArrayList();
                Iterator revIter = productions.iterator();
                while (revIter.hasNext()) {
                    prodList.add(0, revIter.next());
                }
                // Search for a successful parse by copying the stack and trying each production
                //                Iterator prodIter = productions.iterator();
                Iterator prodIter = prodList.iterator();
                // end for testing
                boolean firstProduction = true;
                while (prodIter.hasNext()) {
                    // if a the parse fails, null will be returned, and we will try the next
                    // production
                    // TODO: Have subtype of Exception, ParseException, so real exceptions
                    // are properly recognized.
                    Production prod = (Production)prodIter.next();
                    // Copy stack, firedProductions
                    Stack newStack = (Stack)stack.clone();
                    List newFiredProductions = new ArrayList(firedProductions);
                    if (firstProduction) {
                        if (debug) {
                            debugPrintln(3, "  Trying production: " + prod);
                        }
                        firstProduction = false;
                    } else {
                        if (debug) {
                            debugPrintln(3, "  Backtracking with production: " + prod);
                        }
                    } 
                    newFiredProductions.add(prod);
                    newStack.pop();
                    List rhs = prod.getRHS();
                    // push rhs onto stack in reverse order  (if rhs is not empty)
                    if (rhs.get(0) != Symbol.EMPTY) {
                        for (int i = rhs.size() - 1; i >= 0; i--) {
                            Symbol sym = (Symbol)rhs.get(i);
                            newStack.push(sym);
                        } 
                    }
                    // try running parse engine with new configuration
                    parse(grammar, newStack, input, ip, newFiredProductions, parseResult);
                    // if the there was a successulf parse, return.
                    // otherwise, continue trying with the next production.
                    if (parseResult.isSuccess) {
                        // We've found a successful parse... wipe out previous errors
                        parseResult.parseExceptions.clear();
                        return;
                    }
                }
                // if we've made it here, there were no productions that lead to a succesful parse.
                // Return without success.
                if (debug) {
                    debugPrintln(3, "Parse Error: no applicable productions");
                }
                return;
            } 
        } while (stack.peek() != Symbol.$);
        if (debug) {
            debugPrintln(3, "Parse successful.");
        }
        parseResult.isSuccess = true;
        parseResult.firedProductions = firedProductions;
    }

    private static void printConfiguration(Stack stack, List input, int ip) {
        debugPrintln(3, "-----\nStack: " + stack + "\nInput: " + input.subList(ip, input.size()));
    }

    /**
     *  Build a parseTree based on the parse result, which contains a list of
     *  fired productions, and the input, which contains a list of tokens.
     */
    public static ParseNode buildParseTree(ParseResult parseResult, List input) {
        Stack ruleStack = new Stack();
        Stack inputStack = new Stack();
        // push rules and input onto stacks... these items will be popped off their stacks
        // by the tree-building algorithm.
        ListIterator iter = parseResult.firedProductions.listIterator(parseResult.firedProductions.size());
        while (iter.hasPrevious()) {
            ruleStack.push(iter.previous());
        }
        iter = input.listIterator(input.size());
        while (iter.hasPrevious()) {
            inputStack.push(iter.previous());
        }
        if (ruleStack.isEmpty()) {
            return null;
        }
        return createNode((Production)ruleStack.pop(), inputStack, ruleStack);
    }

    private static ParseNode createNode(Production production, Stack inputStack, Stack ruleStack) {
        NonTerminal symbol = production.getLHS();
        ParseNode newNode = new ParseNode(symbol);
        List children = production.getRHS();
        Iterator iter = children.iterator();
        while (iter.hasNext()) {
            ParseNode childNode = null;
            Object child = iter.next();
            if (child instanceof Terminal) {
                childNode = createTerminalNode((Terminal)child, (Yytoken)inputStack.pop());
            } else if (child instanceof NonTerminal) {
                childNode = createNode((Production)ruleStack.pop(), inputStack, ruleStack);
            } 
            if (childNode != null) {
                newNode.getChildren().add(childNode);
            }
        }
        return newNode;
    }

    private static ParseNode createTerminalNode(Terminal type, Yytoken token) {
        return new ParseNode(token, type);
    }

}
