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


// Version: Completor.java,v 1.8 2010/02/04 05:19:53 hans Exp

package edu.isi.powerloom.gui.parser;

import edu.isi.powerloom.gui.xmlobject.*;
import static edu.isi.powerloom.gui.common.Utils.*;
import java.util.*;

/**
 * Singleton class used for performing symbol completions.
 *
 *
 * @since Fri May 17 09:42:43 2002
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */

public class Completor {
    private static boolean standalone = false;  // for testing
    private static Completor theInstance;
    private HashMap completionTable;
    private Grammar grammar;
    private boolean debug = false;
    private Generator instanceGenerator;
    private Generator relationGenerator;
    private Generator conceptGenerator;
    private Generator functionGenerator;

    private Completor () {
        try {
	    if (standalone) {
		grammar = GrammarParser.getPowerloomGrammarFromFile();
	    } else {
		grammar = GrammarParser.getPowerloomGrammar();
	    } 
        } catch (Exception e) {
            e.printStackTrace();
	    edu.isi.powerloom.gui.components.PowerloomApp.getInstance().handleException(e);
        }
        initializeCompletionTable();
	// for testing only.. generators should be overriden by the application.
	initializeGenerators();
    }

    public static Completor getInstance() {
        if (theInstance == null) {
            theInstance = new Completor();
        }
        return theInstance;
    }

    private void initializeGenerators() {
	instanceGenerator = new Generator() {
		public List generate(String module, String prefix) {
		    return getInstances(prefix);
		}
	    };
	relationGenerator = new Generator() {
		public List generate(String module, String prefix) {
		    return getRelations(prefix);
		}
	    };
	conceptGenerator = new Generator() {
		public List generate(String module, String prefix) {
		    return getConcepts(prefix);
		}
	    };
	functionGenerator = new Generator() {
		public List generate(String module, String prefix) {
		    return getFunctions(prefix);
		}
	    };
    }

    private String getModuleFromTokenList(List tokens) {
	Yytoken token = (Yytoken)tokens.get(tokens.size() - 2);
	return token.getModule();
    }

    private void initializeCompletionTable() {
        completionTable = new HashMap();
	NTCompletor sentenceCompletor = new NTCompletor(){
                                    public List complete(List tokens, String prefix) {
                                        return getSentences();
                                    }
	    };
        completionTable.put(SymbolManager.getInstance().findSymbol("SENTENCE"), sentenceCompletor);
	completionTable.put(SymbolManager.getInstance().findSymbol("SENTENCELIST"), sentenceCompletor);
        completionTable.put(SymbolManager.getInstance().findSymbol("RELCONST"),
                            new NTCompletor(){
                                    public List complete(List tokens, String prefix) {
					if (relationGenerator != null) {
					    //debugPrintln(3, "using: " + relationGenerator);
					    String module = getModuleFromTokenList(tokens);
					    return relationGenerator.generate(module, prefix);
					}
                                        return null;
                                    }
                                });
        completionTable.put(SymbolManager.getInstance().findSymbol("FUNCONST"),
                            new NTCompletor(){
                                    public List complete(List tokens, String prefix) {
					if (functionGenerator != null) {
					    String module = getModuleFromTokenList(tokens);
					    return functionGenerator.generate(module, prefix);
					}
                                        return null;
                                    }
                                });
        completionTable.put(SymbolManager.getInstance().findSymbol("CONCEPTCONST"),
                            new NTCompletor(){
                                    public List complete(List tokens, String prefix) {
					if (conceptGenerator != null) {
					    String module = getModuleFromTokenList(tokens);
					    return conceptGenerator.generate(module, prefix);
					}
					return null;
                                    }
                                });
        NTCompletor varCompletor = new NTCompletor(){
                                    public List complete(List tokens, String prefix) {
                                        return getVarDecls(tokens);
                                    }
            };
        NTCompletor varListCompletor = new NTCompletor(){
                                   public List complete(List tokens, String prefix) {
                                        return getVarListDecls(tokens);
                                    }
            };
        completionTable.put(SymbolManager.getInstance().findSymbol("VARDECL"), varCompletor);
        completionTable.put(SymbolManager.getInstance().findSymbol("VARDECLLIST"), varCompletor);
        completionTable.put(SymbolManager.getInstance().findSymbol("VARDECLLISTPLUS"), varCompletor);
        completionTable.put(SymbolManager.getInstance().findSymbol("VARLIST"), varListCompletor);
        NTCompletor termCompletor = new NTCompletor(){
                public List complete(List tokens, String prefix) {
                    return getTerms(tokens, prefix);
                }
            };
        completionTable.put(SymbolManager.getInstance().findSymbol("TERM"), termCompletor);
        completionTable.put(SymbolManager.getInstance().findSymbol("TERMLIST"), termCompletor);
        completionTable.put(SymbolManager.getInstance().findSymbol("TERMLISTPLUS"), termCompletor);
    }

    public void setInstanceGenerator(Generator generator) {
	instanceGenerator = generator;
    }

    public void setRelationGenerator(Generator generator) {
	relationGenerator = generator;
	initializeCompletionTable();
    }

    public void setConceptGenerator(Generator generator) {
	conceptGenerator = generator;
    }

    public void setFunctionGenerator(Generator generator) {
	functionGenerator = generator;
    }

    /**
     * Input: entire input string, location of current cursor dot.
     * Output: list of terminals and nonterminals.
     */
    private SymbolCompletionResult getCompletionSymbols(String inputString, int dotLocation) throws Exception {
        SymbolCompletionResult result = new SymbolCompletionResult();
        result.completionSymbols = new ArrayList();
        // Scan string up to current dot.
        String subString = inputString.substring(0, dotLocation);
        List tokens = Scanner.scan(subString);
	// Scan all tokens so that we can get the full token that the current point is on.
	List allTokens = Scanner.scan(inputString);
        result.tokens = new ArrayList(tokens);

        // if last token was not a delimeter (e.g., lparen, implies, etc),
        // and the rightmost char is not whitespace,
        // remove the last- scanned token, just before the $ token.
        // Here we assume that the user is inputting some type of identifier
        // (relation name, instance name, etc).
        if (tokens.size() < 2) {
            return result;  // We need at least one token before $. 
        }
        boolean isWhitespace = Character.isWhitespace(subString.charAt(subString.length()-1));
        Yytoken axedToken = ((Yytoken)(tokens.get(tokens.size()-2)));
        if (!isWhitespace &&
            ((axedToken.getType().equals("constant")) ||
             (axedToken.getType().equals("indvar")))) {
            // The partial input is the input typed for the current constant
            result.partialInput = axedToken.getText();
	    // Save size and position of axed token by looking up corresponding
	    //   token in allTokens
	    Yytoken fullToken = ((Yytoken)(allTokens.get(tokens.size()-2)));
	    result.line = fullToken.getLine();
	    result.charBegin = fullToken.getCharBegin();
	    result.charEnd = fullToken.getCharEnd();
	    result.text = fullToken.getText();
	    result.name = fullToken.getName();
	    result.module = fullToken.getModule();
            tokens.remove(axedToken);
        }

        // try parsing input.  The parse result will return a bunch of
        //   exceptions. 

        ParseResult parseResult = Parser.parse(grammar, tokens);
        if (debug) {
            debugPrintln(3, "parseResult = " + parseResult);
        }

        // Filter all exceptions except those which have input = $.
        // For expecting terminal-type errors, collect the terminal.
        // For no-production type errors, collect the nonterminal.
        Iterator iter = parseResult.parseExceptions.iterator();
        while (iter.hasNext()) {
            ParseException pe = (ParseException)iter.next();
            if (pe instanceof NoProductionsException) {
                NoProductionsException npe = (NoProductionsException)pe;
                if (npe.getInputSymbol() == Symbol.$) {
                    result.completionSymbols.add(npe.getStackSymbol());
                }
            } else if (pe instanceof StackInputMismatchException) {
                StackInputMismatchException sime = (StackInputMismatchException)pe;
                if (sime.getInputSymbol() == Symbol.$) {
                    result.completionSymbols.add(sime.getStackSymbol());
                }
            } 
        }
        return result;
    } 

    /**
     * Input: entire input string, location of current cursor dot.
     * Output: alphabetical list of possible completions, suitable for
     *   displaying in completion combobox.
     */
    public CompletionResult getCompletionList(String inputString, int dotLocation) throws Exception {
        SymbolCompletionResult scr = getCompletionSymbols(inputString, dotLocation);
        List completionSymbols = scr.completionSymbols;
        String partialInput = scr.name;  // characters between prev whitespace and dot
        List tokens = scr.tokens;
        List unFilteredCompletions = new ArrayList();
        SortedSet completions = new TreeSet();  // array of text

        Iterator iter = completionSymbols.iterator();
        while (iter.hasNext()) {
            Symbol symbol = (Symbol)iter.next();
            if (symbol instanceof Terminal) {
                String completionString = ((Terminal)symbol).getText();
                if (completionString == null) {
                    completionString = ((Terminal)symbol).getName();
                }
                // special case certain terminals
                if (completionString.equals("indvar")) {
                    completionString = "?var";
                }
                unFilteredCompletions.add(completionString);
            } else if (symbol instanceof NonTerminal) {
                NTCompletor completor = (NTCompletor)completionTable.get(symbol);
                if (completor == null) {
                    throw new Exception("No completor in completionTable for symbol: " + symbol);
                }
                unFilteredCompletions.addAll(completor.complete(tokens, partialInput));  
            }
            // todo:figure out how to do context-sensitive completion, e.g., legal
            // values for the range of a relation
            // this sorts the list...
	    Yytoken lastToken = (Yytoken)scr.tokens.get(scr.tokens.size() - 2);
	    List filteredCompletions = PLSurrogateContainer.surrogatifyList(filterCompletions(unFilteredCompletions, partialInput));
            completions.addAll(filteredCompletions);
        }
	CompletionResult result = new CompletionResult(completions, scr.text, scr.name, scr.module, scr.line, scr.charBegin, scr.charEnd);
        return result;
    }

    /**
     * @return characters between previous whitespace and dot
     */
    private String getPartialInput(String input, int dot) {
        return null;
    }

    /**
     * @return all unfilteredCompletions whose string starts with partialInput.
     */
    private List filterCompletions(List unfilteredCompletions, String partialInput) {
        if (partialInput == null) {
            return unfilteredCompletions;
        }
        partialInput = partialInput.toLowerCase();
        List result = new ArrayList();
        Iterator iter = unfilteredCompletions.iterator();
        while (iter.hasNext()) {
	    Object candidate = iter.next();
	    String candidateString = "**error**";
	    if (candidate instanceof PLSurrogate) {
		PLObject plObject = (PLObject)((PLSurrogate)candidate).getValue();
		candidateString = plObject.getID();
	    } else {
		candidateString = candidate.toString();
	    }
            if (candidateString.toLowerCase().startsWith(partialInput)) {
                result.add(candidate);
            }
        }
        return result;
    }

    /**
     * Helper functions for completors.  These contain application-specific functionality.
     */
    private List getSentences() {
        List result = new ArrayList();
        result.add("constant");
        result.add("(/= sentence1 sentence2)");
        result.add("(<<= sentence1 sentence2)");
        result.add("(<<~ sentence1 sentence2)");
        result.add("(<= sentence1 sentence2)");
        result.add("(<~ sentence1 sentence2)");
        result.add("(= sentence1 sentence2)");
        result.add("(=> sentence1 sentence2)");
        result.add("(=>> sentence1 sentence2)");
        result.add("(~> sentence1 sentence2)");
        result.add("(~>> sentence1 sentence2)");
        result.add("(and sentence1 sentence2)");
        result.add("(exists ((?var1 type1) (?var2 type2)) sentence)");
        result.add("(forall ((?var1 type1) (?var2 type2)) sentence)");
        result.add("(forall ((?var1 type1) (?var2 type2)) sentence1 sentence2)");
        result.add("(not sentence)");
        result.add("(or sentence1 sentence2)");
        result.add("(relation term1 term2)");
        return result;
    }

    private List getRelations(String prefix) {
        // todo: substitute this with getRelationsForModule
        List result = new ArrayList();
        result.add("rel1");
        result.add("rel2");
        result.add("newrel3");
        return result;
    }

    private List getFunctions(String prefix) {
        // todo: substitute this with getFunctionsForModule
        List result = new ArrayList();
        result.add("fun1");
        result.add("fun2");
        result.add("newfun3");
        result.add("seeitsanewfun4");
        return result;
    }

    private List getConcepts(String prefix) {
        // todo: substitute this with getConceptsForModule
        List result = new ArrayList();
        result.add("con1");
        result.add("con2");
        result.add("newcon3");
        return result;
    }

    private List getInstances(String prefix) {
        // todo: substitute this with getInstancesForModule
        List result = new ArrayList();
        result.add("inst1");
        result.add("inst2");
        result.add("newinst3");
        return result;
    }

    private List getVariables(List tokens) {
        // Scan through tokens to collect variables
        List result = new ArrayList();
        SortedSet variables = new TreeSet(); 
        Iterator iter = tokens.iterator();
        while (iter.hasNext()) {
            Yytoken token = (Yytoken)iter.next();
            if (isVariable(token)) {
                variables.add(token.getText());
            }
        }
        result.addAll(variables);
        return result;
    }

    private List getVarDecls(List tokens) {
        List result = getVariables(tokens);
        // templates
        result.add("?var");
        result.add("(?var type)");
        return result;
    }

    private List getVarListDecls(List tokens) {
        List result = getVarDecls(tokens);
        // additional templates
        result.add("(?var)");
        result.add("((?var type))");
        return result;
    }

    private List getTerms(List tokens, String prefix) {
        List result = new ArrayList();
        SortedSet sortedResult = new TreeSet();
	if (instanceGenerator != null) {
	    String module = getModuleFromTokenList(tokens);
	    sortedResult.addAll(instanceGenerator.generate(module, prefix));
	}
        sortedResult.addAll(getVariables(tokens));
        result.addAll(sortedResult);
        //templates: funterm, listterm, setterm, quantterm
        result.add("(function term term)");
        result.add("(kappa ((?var1 type1) (?var2 type2)) sentence)");
        result.add("(lambda ((?var1 type1) (?var2 type2)) sentence)");
        result.add("(listof term term)");
        result.add("(setof term term)");
        result.add("(setofall (?var type) sentence)");
        result.add("(the (?var type) sentence)");
        return result;
    }

    private boolean isVariable(Yytoken candidate) {
        return candidate.getType().equals("indvar");
    }

    private interface NTCompletor {
        /**
         * @return a list of Strings of potential completions.
         * Each object of type NTCompletor is indexed in the completionTable
         * by an associated nonterminal symbol.
         */
        public List complete(List tokens, String prefix);
    }

    /**
     * Class used to return multiple values from getCompletionSymbols
     */
    private class SymbolCompletionResult {
        List completionSymbols;
        String partialInput;
	int line;
	int charBegin;
	int charEnd;
	String text;
	String name;
	String module;
        List tokens;  // tokens scanned up to point
    }

    void runtest(String input, int dotLocation) {
        try {
            Set completions = getCompletionList(input, dotLocation).completions;
            Iterator iter = completions.iterator();
            debugPrintln(3, "----------");
            debugPrintln(3, "Input: |" + input + "|");
            debugPrintln(3, "Completions:");
            while (iter.hasNext()) {
                String completion = (String)iter.next();
                debugPrintln(3, "  " + completion);
            }
        } catch (Exception e) {
            e.printStackTrace();
	    edu.isi.powerloom.gui.components.PowerloomApp.getInstance().handleException(e);
        }
    }

    void test1() {
        String input = "(re";
        runtest(input, input.length());
    }

    void test2() {
        String input = "(n";
        runtest(input, input.length());
    }

    void test3() {
        String input = "(";
        runtest(input, input.length());
    }

    void test4() {
        String input = "(rel1 inst";
        runtest(input, input.length());
    }

    void test5a() {
        String input = "(rel1 ";
        runtest(input, input.length());
    }

    void test5b() {
        String input = "(rel1 ?x ";
        runtest(input, input.length());
    }

    void test6() {
        String input = "(forall (";
        runtest(input, input.length());
    }

    void test6b() {
        String input = "(forall (?";
        runtest(input, input.length());
    }

    void test7() {
        String input = "(forall (?x";
        runtest(input, input.length());
    }

    void test8() {
        String input = "(forall (?x ";
        runtest(input, input.length());
    }

    void test9a() {
        String input = "(forall (?x ?yam ?yar ?";
        runtest(input, input.length());
    }

    void test9b() {
        String input = "(forall (?x ?yam ?yar ?y";
        runtest(input, input.length());
    }

    void test10() {
        String input = "(rel1 (se";
        runtest(input, input.length());
    }

    void test11() {
        String input = "(rel1 (lambda ((?var1 ";
        runtest(input, input.length());
    }

    void test12() {
        String input = "(rel1 (lambda ((?var1 n";
        runtest(input, input.length());
    }

    void test13() {
        String input = "(rel1 (lambda ((?var1 type1) (?var2 type2)";
        runtest(input, input.length());
    }

    void test14() {
        String input = "(rel1 (lambda ((?var1 type1) (?var2 type2)) ";
        runtest(input, input.length());
    }

    void test15() {
        String input = "(rel1 (kappa ((?var1 type1) (?var2 type2)) ";
        runtest(input, input.length());
    }

    void test16() {
        String input = "(forall ";
        runtest(input, input.length());
    }

    void test17() {
        String input = "(forall (?g) (<= ";
        runtest(input, input.length());
    }

    void test18() {
        String input = "(and ";
        runtest(input, input.length());
    }

  
    public static void main (String[] args) {
        Completor completor = Completor.getInstance();
	/*
        completor.test1();
        completor.test2();
        completor.test3();
        completor.test4();
        completor.test5a();
        completor.test5b();
        completor.test6();
        completor.test6b();
        completor.test7();
        completor.test8();
        completor.test9a();
        completor.test9b();
        completor.test10();
        completor.test11();
        completor.test12();
        completor.test13();
        completor.test14();
        completor.test15();
	completor.test16();
	completor.test17();
	*/
	completor.test18();
    }
}
