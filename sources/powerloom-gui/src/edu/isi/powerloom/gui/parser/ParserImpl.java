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


// Version: ParserImpl.java,v 1.5 2010/02/04 05:20:10 hans Exp

package edu.isi.powerloom.gui.parser;

import java.util.*;

/**
 * Default implementation of ParserInterface.
 *
 * @author <a href=mailto:eric@ericmelz.com>Eric Melz</a>
 * @since Sat May 18 20:32:02 2002
 */

public class ParserImpl implements ParserInterface {
    private static boolean standalone = false;  // for testing
    private Grammar grammar;
    
    public CompletionResult getCompletionList(String inputString, int dotLocation) throws Exception {
	Completor completor = Completor.getInstance();
	return completor.getCompletionList(inputString, dotLocation);
    }

    public void setInstanceGenerator(Generator generator) {
	Completor.getInstance().setInstanceGenerator(generator);
    }

    public void setRelationGenerator(Generator generator) {
	Completor.getInstance().setRelationGenerator(generator);	
    }

    public void setConceptGenerator(Generator generator) {
	Completor.getInstance().setConceptGenerator(generator);
    }

    public void setFunctionGenerator(Generator generator) {
	Completor.getInstance().setFunctionGenerator(generator);
    }

    private Grammar getGrammar() {
	try {
	    if (grammar == null) {
		if (standalone) {
		    grammar = GrammarParser.getPowerloomGrammarFromFile();
		} else {
		    grammar = GrammarParser.getPowerloomGrammar();
		} 
	    }
	} catch (Exception e) {
	    edu.isi.powerloom.gui.components.PowerloomApp.getInstance().handleException(e);
	}
	return grammar;
    }

    public PLElement parse(String inputString) throws Exception{
	try {
	    List tokens = Scanner.scan(inputString);
	    ParseResult parseResult = Parser.parse(getGrammar(), tokens);
	    //System.out.println(parseResult);
	    PLElement parseTree = createParseTree(new ListHolder(tokens), 
						  new ListHolder(parseResult.firedProductions));
	    return parseTree;
	} catch (Exception e) {
	    throw new Exception("Parse Exception: illegal expression: " + inputString);
	}
    }

    // Used to pass handle to shared list 
    class ListHolder {
	public List list;

	public ListHolder(List list) {
	    this.list = list;
	}

	public ListHolder pop() {
	    list = list.subList(1, list.size());
	    return this;
	}

	public Object first() {
	    return list.get(0);
	}
    }

    private PLElement createParseTree(ListHolder tokens, ListHolder productions) throws Exception{
	PLElement root = new PLElement();
	root.setAttribute("Symbol", ((Production)productions.first()).getLHS());
	Production prod = (Production)productions.first();
	processProduction(root, prod, productions.pop(), tokens);
	return root;
    }

    private void processProduction(PLElement nodeToExpand, Production prod, ListHolder remainingProductions, ListHolder remainingTokens) throws Exception{
	List rhsTokens = prod.getRHS();
	Iterator rhsIterator = rhsTokens.iterator();
	while (rhsIterator.hasNext()) {
	    Symbol symbol = (Symbol)rhsIterator.next();
	    PLElement node = new PLElement();
	    node.setSymbol(symbol);
	    node.setParentElement(nodeToExpand);
	    if (symbol instanceof NonTerminal) {
		Production nextProduction = (Production)remainingProductions.first();
		processProduction(node, nextProduction, 
				  remainingProductions.pop(),
				  remainingTokens);
	    } else if (symbol instanceof Terminal) {
		node.setToken((Yytoken)remainingTokens.first());
		remainingTokens.pop();
	    } else if (symbol == Symbol.EMPTY) {
		// ignore empty
		       } else {
			   throw new Exception("Unknown symbol type: " + symbol);
	    }
	}
    }
}
