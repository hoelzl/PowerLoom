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


// Version: ExpressionEditorPanel.java,v 1.15 2010/02/04 05:17:21 hans Exp

package edu.isi.powerloom.gui.components;

import edu.isi.powerloom.gui.xmlobject.*;
import edu.isi.powerloom.gui.serverinterface.*;
import edu.isi.powerloom.gui.parser.*;
import edu.isi.powerloom.gui.common.*;
import static edu.isi.powerloom.gui.common.Utils.*;

import java.awt.datatransfer.*;
import javax.swing.*;
import java.awt.event.*;
import javax.swing.text.*;
import javax.swing.event.*;
import java.util.*;
import java.awt.Color;

/**
 * Text Component which supports matching parenthesis, symbol completion, and emacs-style
 * keybindings.
 * 
 * @since 5/22/2002 12:42:34 PM
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */
public class ExpressionEditorPanel extends javax.swing.JTextPane implements PLClipboardOwner 
{
	private edu.isi.powerloom.gui.xmlobject.PLModule module;

    class MatchingParenthesisHighlighter
    extends Object
    implements ChangeListener {

    JTextComponent text;

    Highlighter highlighter;
    Highlighter.HighlightPainter painter;

    Object currentLeftHighlight;
    Object currentRightHighlight;

    public MatchingParenthesisHighlighter (JTextComponent tc) {
        text = tc;
        if (text != null) {
            highlighter = text.getHighlighter();
            painter = new DefaultHighlighter.DefaultHighlightPainter(Color.pink);
        }
    }
    
    public void stateChanged(ChangeEvent event) {
	// for now, while trying to use expressionEditor for a PLListRenderer,
	//  turn off highlighting.
	if (false) {
	    return;
	}

        // TO DO: Figure out text-area deletion problem across line breaks.
        // SOLVED: Turning line-wrapping off did the trick.
        if (text != null) {
            if (currentLeftHighlight != null) {
                highlighter.removeHighlight(currentLeftHighlight);
                highlighter.removeHighlight(currentRightHighlight);
                currentLeftHighlight = null;
                currentRightHighlight = null;
            }
            // Caret caret = (Caret) event.getSource(); // alternative
            int caretPos = text.getCaret().getDot();
            int matchPos;
            if (caretPos >= 0) {
                try {
		    // left-paren gets matched *at* the dot
		    if ((text.getText(caretPos, 1).charAt(0)) == '(') {
                        matchPos = StringUtils.findMatchingRightParen
                            (text.getText(), caretPos);
                        if (matchPos >= 0) {
                            currentLeftHighlight =
                                highlighter.addHighlight
                                (caretPos, caretPos + 1, painter);
                            currentRightHighlight =
                                highlighter.addHighlight
                                (matchPos, matchPos + 1, painter);
                        }
			// right-paren gets matched *before* the dot
		    } else if ((caretPos > 0) && ((text.getText(caretPos - 1, 1).charAt(0)) == ')')) {
                        matchPos = StringUtils.findMatchingLeftParen
                            (text.getText(), caretPos - 1);
                        //System.out.println
                        //    ("dot=" + caretPos + " match=" + matchPos);
                        if (matchPos >= 0) {
                            currentLeftHighlight =
                                highlighter.addHighlight
                                (matchPos, matchPos + 1, painter);
                            currentRightHighlight =
                                highlighter.addHighlight
                                (caretPos - 1, caretPos, painter);
                        }
		    } 
                }
                catch (BadLocationException e) {
		    PowerloomApp.getInstance().handleException(e);
		}
            }
        }
    }
}
	
/**
 * ExpressionEditorPanel constructor comment.
 */
public ExpressionEditorPanel() {
	super();
	initialize();
}
/**
 * ExpressionEditorPanel constructor comment.
 * @param doc javax.swing.text.StyledDocument
 */
public ExpressionEditorPanel(javax.swing.text.StyledDocument doc) {
	super(doc);
	initialize();
}

    private boolean testModuleForCompletion(PLModule module) {
	if (module == null) {
	    JOptionPane.showMessageDialog(PowerloomApp.getInstance(), "You must first select a module before using completion.", "Select Module", JOptionPane.WARNING_MESSAGE);
	    return false;
	}
	return true;
    }

/**
 * Insert the method's description here.
 * Creation date: (5/20/2002 2:54:54 PM)
 */
private List getConcepts(String moduleName, String prefix) {
     List result = new ArrayList();

     try {
	 ExpressionEditorPanel currentEditor = PowerloomApp.getInstance().getSelectedEditor();
	 PLModule module = currentEditor.getModule();
	 if (!testModuleForCompletion(module)) {
	     return result;
	 }
	 if (moduleName == null) {
	     moduleName = module.getID();
	 }
	 PLSurrogateContainer container = KnowledgeManager.getInstance().getConceptCompletions(moduleName, prefix);
	 List list = container.getSurrogates();
	 Iterator iter = list.iterator();
	 while (iter.hasNext()) {
	     PLSurrogate surrogate = (PLSurrogate)iter.next();
	     result.add(surrogate);
	 }
     } catch (Exception e) {
	 PowerloomApp.getInstance().handleException(e);
     }
     return result;
}
/**
 * Insert the method's description here.
 * Creation date: (5/20/2002 2:55:45 PM)
 * @return java.util.List
 */
private List getFunctions(String moduleName, String prefix) {
     List result = new ArrayList();
	try {
	    ExpressionEditorPanel currentEditor = PowerloomApp.getInstance().getSelectedEditor();
	    PLModule module = currentEditor.getModule();
	    if (!testModuleForCompletion(module)) {
		return result;
	    }
	    if (moduleName == null) {
		moduleName = module.getID();
	    }
	    PLSurrogateContainer container = KnowledgeManager.getInstance().getRelationCompletions(moduleName, prefix);
	    List list = container.getSurrogates();
	    Iterator iter = list.iterator();
	    while (iter.hasNext()) {
		PLSurrogate surrogate = (PLSurrogate)iter.next();
		PLRelation relation = (PLRelation)surrogate.getValue();
		if ((relation != null) && (relation.attrIsFunction.equals("TRUE"))) {
		    result.add(surrogate);			
		}
	    }
	} catch (Exception e) {
	    PowerloomApp.getInstance().handleException(e);
	}
	return result;

}
/**
 * Insert the method's description here.
 * Creation date: (5/20/2002 2:39:18 PM)
 * @return java.util.List
 */
private List getInstances(String moduleName, String prefix) {
     List result = new ArrayList();
	try {
	    ExpressionEditorPanel currentEditor = PowerloomApp.getInstance().getSelectedEditor();
	    PLModule module = currentEditor.getModule();
	    if (!testModuleForCompletion(module)) {
		return result;
	    }
	    if (moduleName == null) {
		moduleName = module.getID();
	    }

		PLSurrogateContainer container = KnowledgeManager.getInstance().
		    getConceptAndRelationAndInstanceCompletions(moduleName, prefix);
		List list = container.getSurrogates();
		Iterator iter = list.iterator();
		while (iter.hasNext()) {
			PLSurrogate surrogate = (PLSurrogate)iter.next();
			result.add(surrogate);
		}
	} catch (Exception e) {
	    PowerloomApp.getInstance().handleException(e);
	}
	return result;
}
/**
 * Insert the method's description here.
 * Creation date: (5/22/2002 12:43:27 PM)
 * @return edu.isi.powerloom.gui.xmlobject.PLModule
 */
public edu.isi.powerloom.gui.xmlobject.PLModule getModule() {
	return module;
}
/**
 * Insert the method's description here.
 * Creation date: (5/20/2002 2:40:24 PM)
 * @return java.util.List
 */
    private List getRelationsAndConcepts(String typedModuleName, String prefix) {
     List result = new ArrayList();
	try {
	    ExpressionEditorPanel currentEditor = PowerloomApp.getInstance().getSelectedEditor();
	    PLModule module = currentEditor.getModule();
	    if (!testModuleForCompletion(module)) {
		return result;
	    }
	    String moduleName = null;
	    if (typedModuleName == null) {
		moduleName = module.getID();
	    } else {
		moduleName = typedModuleName;
	    }
	    PLSurrogateContainer relations = KnowledgeManager.getInstance().getConceptAndRelationCompletions(moduleName, prefix);
	    List relationList = relations.getSurrogates();
	    Iterator iter = relationList.iterator();
	    while (iter.hasNext()) {
		PLSurrogate surrogate = (PLSurrogate)iter.next();
		result.add(surrogate);
	    }
	} catch (Exception e) {
	    PowerloomApp.getInstance().handleException(e);
	}
	return result;
    }
/**
 * Insert the method's description here.
 * Creation date: (5/22/2002 12:45:15 PM)
 */
public void initialize() {
	initializeCompletorGenerators();
	// Tricky: since the KeyMap, Parser, and Completor are all singleton objects, we need
        // to keep track of the curently-selected expression editor in order for
	// completions and key actions to be associated with the correct editor.
	       addFocusListener(new FocusListener() {
		       public void focusGained(FocusEvent e) {
			   //debugPrintln(3, "focusGained in editor: " + hashCode());
			   PowerloomApp.getInstance().setSelectedEditor(ExpressionEditorPanel.this);
		       }
		       public void focusLost(FocusEvent e) {
			   //debugPrintln(3, "focusLost in editor: " + hashCode());
			   PowerloomApp.getInstance().setSelectedEditor(null);
		       }
		   });
	getCaret().addChangeListener(new MatchingParenthesisHighlighter(this));
	debugPrintln(3, "setting keymap for expression editor...");
	EditorKeyMap.setupKeyMap(this);
}
/**
 * Insert the method's description here.
 * Creation date: (5/20/2002 2:37:52 PM)
 */
private void initializeCompletorGenerators() {
	Generator instanceGenerator = new Generator() {
		public java.util.List generate(String module, String prefix) {
		    return getInstances(module, prefix);
		}
	};
	Generator relationGenerator = new Generator() {
		public java.util.List generate(String module, String prefix) {
		    return getRelationsAndConcepts(module, prefix);
		}
	};
	Generator conceptGenerator = new Generator() {
		public java.util.List generate(String module, String prefix) {
		    return getConcepts(module, prefix);
		}
	};
	Generator functionGenerator = new Generator() {
		public java.util.List generate(String module, String prefix) {
		    return getFunctions(module, prefix);
		}
	};
	try {
		ParserInterfaceFactory.getParserInterface().setInstanceGenerator(instanceGenerator);
		ParserInterfaceFactory.getParserInterface().setRelationGenerator(relationGenerator);
		ParserInterfaceFactory.getParserInterface().setConceptGenerator(conceptGenerator);
		ParserInterfaceFactory.getParserInterface().setFunctionGenerator(functionGenerator);
	} catch (Exception e) {
	    PowerloomApp.getInstance().handleException(e);
	}
}
/**
 * Insert the method's description here.
 * Creation date: (5/22/2002 12:43:27 PM)
 * @param newModule edu.isi.powerloom.gui.xmlobject.PLModule
 */
public void setModule(edu.isi.powerloom.gui.xmlobject.PLModule newModule) {
	module = newModule;
}

    public PLElement parseExpression() throws Exception {
	try {
	    PLElement root = ParserInterfaceFactory.getParserInterface().parse(getText());
	    return root;
	} catch (Exception e) {
	    throw e;
	}
    }

    /**
     *  Implementation of PLClipboardOwner
     */
    protected PLClipboardOwnerSupport support;

    public ExpressionEditorPanel(PLFrame parent, String name, List types) {
	super();
	initialize();
	support = new PLClipboardOwnerSupport(name, types, parent, this);
    }
    
    public boolean supportsPLObjects() {
	return false;
    }

    public void doCut(Object object) {
	cut();
    }

    public void doDelete(PLModule module, Object object) {
	replaceSelection("");
    }

    public void doCopy(Object object) {
	copy();
    }

    public void lostOwnership(Clipboard c, Transferable t) {}

    public String getName() {
	return support.getName();
    }

    public List getTypes() {
	return support.getTypes();
    }

    public Object getSelectedObject() {
	return null;
    }

    public PLModule getSelectedModule() {
	return support.getParent().getModule();
    }

    public void doPasteFromCut(PLModule module, Object object) {
	System.err.println("Warning: doPasteFromCut is not implemented.  Use a subclass to implement this method.");
    }

    public void doPasteFromCopy(PLModule module, Object object) {
	paste();
    }

}
