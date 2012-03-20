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


// Version: GrammarParser.java,v 1.4 2010/02/04 05:19:59 hans Exp

package edu.isi.powerloom.gui.parser;

import java.util.*;
import java.net.*;
import java.io.*;

import static edu.isi.powerloom.gui.common.Utils.*;

/**
 * Static class used for parsing a grammar file and creating a Grammar
 * structure.
 *
 * @since Mon May 06 13:20:49 2002
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 * @see edu.isi.powerloom.gui.parser.Grammar Grammar
 */
public class GrammarParser {
    public static Grammar parseGrammarFile(String fileName) throws Exception {
        debugPrintln(3, "Parsing file " + fileName + "...");
        return parseGrammarString(readFile(fileName));
    }

    public static Grammar parseGrammarFile(URL url) throws Exception {
        debugPrintln(3, "Parsing url " + url + "...");
        return parseGrammarString(readURL(url));
    }

    private static Grammar parseGrammarString(String grammarString) throws Exception {
        //        debugPrintln(3, "parsing string:\n" + grammarString);
        int nonTermPos = scanForNonTerminalPos(grammarString);
        Collection terminals = parseTerminals(grammarString.substring(0, nonTermPos));
        int productionsPos = scanForProductionsPos(grammarString);
        List nonTerminals = parseNonTerminals(grammarString.substring(nonTermPos, productionsPos));
        Collection productions = parseProductions(grammarString.substring(productionsPos));
        Grammar grammar = new Grammar();
        grammar.setTerminals(terminals);
        grammar.setNonTerminals(nonTerminals);
        grammar.setProductions(productions);
        grammar.setStartSymbol((NonTerminal)nonTerminals.get(0));
        return grammar;
    }

    static int scanForNonTerminalPos(String grammarString) throws Exception {
        return grammarString.indexOf("non term");
    }

    static int scanForSemicolon(String grammarString, int startFrom) throws Exception {
        return grammarString.indexOf(";", startFrom);
    }

    static int scanForProductionsPos(String grammarString) throws Exception {
        int firstEqPos = grammarString.indexOf("::=");
        if (firstEqPos < 0) {
            return firstEqPos;
        }
        // scan to beginning of line
        return grammarString.substring(0, firstEqPos).lastIndexOf("\n") + 1;
    }

    static Collection parseTerminals(String terminalsString) {
        Collection result = new ArrayList();
        //        debugPrintln(3, "parsing terminals with:[" + terminalsString + "]");
        StringTokenizer stok = new StringTokenizer(terminalsString, "\n");
        while (stok.hasMoreTokens()) {
            String line = stok.nextToken();
            if (line.startsWith("terminal")) {
                StringTokenizer termTok = new StringTokenizer(line.substring("terminal".length()), ",;");
                while (termTok.hasMoreTokens()) {
                    String termToken = termTok.nextToken().trim();
                    int spaceIndex = termToken.indexOf(' ');
                    String text = null;
                    int startIndex = 0;
                    if (spaceIndex != -1) {
                        text = termToken.substring(0, spaceIndex).trim();
                        startIndex = spaceIndex;
                    } 
                    termToken = termToken.substring(startIndex).trim();
                    startIndex = 0;
                    spaceIndex = termToken.indexOf(' ');
                    String color = null;
                    if (spaceIndex != -1) {
                        color = termToken.substring(0, spaceIndex).trim();
                        startIndex = spaceIndex;
                    } 
                    String token = termToken.substring(startIndex).trim();
                    //debugPrintln(3, "parsed term: [text:" + text + ", color:" + color + ", terminal: " + token + "]");
                    Terminal terminal = SymbolManager.getInstance().findOrCreateTerminal(token);
                    terminal.setColor(color);
                    terminal.setText(text);
                    result.add(terminal);
                }
            }
        }
        
        return result;
    }

    static List parseNonTerminals(String nonTerminalsString) {
        List result = new ArrayList();
        StringTokenizer stok = new StringTokenizer(nonTerminalsString, "\n");
        while (stok.hasMoreTokens()) {
            String line = stok.nextToken();
            if (line.startsWith("non terminal")) {
                StringTokenizer termTok = new StringTokenizer(line.substring("non terminal".length()), ",;");
                while (termTok.hasMoreTokens()) {
                    String termToken = termTok.nextToken().trim();
                    int spaceIndex = termToken.indexOf(' ');
                    String type = null;
                    int startIndex = 0;
                    if (spaceIndex != -1) {
                        type = termToken.substring(0, spaceIndex).trim();
                        startIndex = spaceIndex;
                    } 
                    String token = termToken.substring(startIndex).trim();
                    //                    debugPrintln(3, "parsed nonterm: [" + type + ", " + token + "]");
                    NonTerminal nonTerminal = SymbolManager.getInstance().findOrCreateNonTerminal(token);
                    result.add(nonTerminal);
                }
            }
        }
        
        return result;
    }

    static Collection parseProductions(String productionsString) throws Exception {
        Collection result = new ArrayList();
        //        debugPrintln(3, "parsing productions with:[" + productionsString + "]");
        int startPos = 0;
        int endPos = scanForSemicolon(productionsString, 0);
        while (startPos < productionsString.length() && (endPos > 0)) {
            //            debugPrintln(3, "startpos = " + startPos + ", endpos = " + endPos);
            String productionsGroup = productionsString.substring(startPos, endPos);
            Collection productions = parseProductionsGroup(productionsGroup);
            result.addAll(productions);
            startPos = endPos + 1;
            endPos = scanForSemicolon(productionsString, startPos);
        }
        
        return result;
    }

    // Parse group of form   a ::= b c | d e | .... ;
    static Collection parseProductionsGroup(String productionsGroupString) throws Exception {
        Collection result = new ArrayList();
        //        debugPrintln(3, "parsing group: " + productionsGroupString);
        int eqPos = productionsGroupString.indexOf("::=");
        String nonTermToken = productionsGroupString.substring(0, eqPos).trim();
        NonTerminal nonTerminal = SymbolManager.getInstance().findOrCreateNonTerminal(nonTermToken);
        StringTokenizer prodTokenizer = new StringTokenizer(productionsGroupString.substring(eqPos + "::=".length()), "|;");
        while (prodTokenizer.hasMoreTokens()) {
            String prodToken = prodTokenizer.nextToken();
            Production production = parseProduction(nonTerminal, prodToken);
            result.add(production);
        }
        return result;
    }

    private static Production parseProduction(NonTerminal nonTerminal, String productionString) throws Exception {
        List rhs = new ArrayList();
        StringTokenizer rhsTokenizer = new StringTokenizer(productionString, " ");
        while (rhsTokenizer.hasMoreTokens()) {
            String rhsToken = rhsTokenizer.nextToken().trim();
            if ((rhsToken.length() > 0) && !rhsToken.startsWith("%")) {  // ignore %prec op for now
                //                debugPrintln(3, "adding rhstoken: " + rhsToken + "|");
                Symbol rhsSymbol = SymbolManager.getInstance().findSymbol(rhsToken);
                if (rhsSymbol == null) {
                    throw new Exception("Error:: symbol " + rhsToken + " has not been declared.");
                }
                rhs.add(rhsSymbol);
            }
        }
        Production result = new Production(nonTerminal, rhs);
        //        debugPrintln(3, "created production : " + result);
        return result;
    }

    private static String readFile(String fileName) throws Exception {
        StringBuffer buffer = new StringBuffer();
        FileReader fr = new FileReader(fileName);
        BufferedReader br = new BufferedReader(fr);
        String line;
        while ((line = br.readLine()) != null) {
            buffer.append(line + "\n");
        }
        return buffer.toString();
    }

    private static String readURL(URL url) throws Exception {
	URLConnection connection = url.openConnection();
	connection.connect();
	InputStream is = connection.getInputStream();
	InputStreamReader isr = new InputStreamReader(is);
        BufferedReader br = new BufferedReader(isr);
        StringBuffer buffer = new StringBuffer();
        String line;
        while ((line = br.readLine()) != null) {
            buffer.append(line + "\n");
        }
	is.close();
        return buffer.toString();
    }

    public static Grammar getPowerloomGrammar() throws Exception {
	java.net.URL url = GrammarParser.class.getClassLoader().getResource("resources/conf/powerloom.grammar");
        return parseGrammarFile(url);
    }

    public static Grammar getPowerloomGrammarFromFile() throws Exception {
	String file = "c:/data/isi/powerloom-gui/Client/serverinterface/src/edu/isi/powerloom/gui/parser/powerloom.grammar";
        return parseGrammarFile(file);
    }


    /********************
     **  Tests
     **/

    private static void testSample() throws Exception {
        Grammar g = parseGrammarFile("c:/Data/TogetherJ Projects/parser/sample.in");
        debugPrintln(3, "Grammar:\n" + g);
    }

    private static void testDragon() throws Exception {
        Grammar g = parseGrammarFile("c:/Data/TogetherJ Projects/parser/dragon1.in");
        //debugPrintln(3, "Grammar:\n" + g);
        ParseTable pt = g.getParseTable();
        debugPrintln(3, "ParseTable:\n" + pt);
        List input = new ArrayList();
        input.add(SymbolManager.getInstance().findSymbol("ID"));
        input.add(SymbolManager.getInstance().findSymbol("PLUS"));
        input.add(SymbolManager.getInstance().findSymbol("ID"));
        input.add(SymbolManager.getInstance().findSymbol("TIMES"));
        input.add(SymbolManager.getInstance().findSymbol("ID"));
        input.add(SymbolManager.getInstance().findSymbol("$"));
        ParseResult result = Parser.parse(g, input);
        System.out.println(result);
    }

    private static void testBacktrack() throws Exception {
        Grammar g = parseGrammarFile("c:/Data/TogetherJ Projects/parser/backtrack.in");
        ParseTable pt = g.getParseTable();
        debugPrintln(3, "ParseTable:\n" + pt);
        List input = new ArrayList();
        input.add(SymbolManager.getInstance().findSymbol("c"));
        input.add(SymbolManager.getInstance().findSymbol("a"));
        input.add(SymbolManager.getInstance().findSymbol("d"));
        input.add(SymbolManager.getInstance().findSymbol("$"));
        ParseResult result = Parser.parse(g, input);
        System.out.println(result);
    }

    private static void testPowerloom(Grammar grammar, List input) throws Exception {
        ParseTable pt = grammar.getParseTable();
        debugPrintln(3, "ParseTable:\n" + pt);
        ParseResult parseResult = Parser.parse(grammar, input);
        System.out.println(parseResult);
        ParseNode parseTree = Parser.buildParseTree(parseResult, input);
        debugPrintln(3, "ParseTree:\n" + parseTree);
    }
    
    private static void testPowerloom1() throws Exception {
        Grammar grammar = parseGrammarFile("c:/Data/TogetherJ Projects/parser/powerloom.grammar");
        List input = Scanner.scan("(rel domain range)");
        testPowerloom(grammar, input);
    }

    private static void testPowerloom2() throws Exception {
        Grammar grammar = parseGrammarFile("c:/Data/TogetherJ Projects/parser/powerloom.grammar");
        List input = Scanner.scan("(");
        testPowerloom(grammar, input);
    }

    private static void testPowerloom3() throws Exception {
        Grammar grammar = parseGrammarFile("c:/Data/TogetherJ Projects/parser/powerloom.grammar");
        List input = Scanner.scan("(rel");
        testPowerloom(grammar, input);
    }

    public static void main (String[] args) throws Exception {
        //        testDragon();
        //testBacktrack();
        //testPowerloom1();
        //testPowerloom2();
        //testPowerloom3();

    }
}
