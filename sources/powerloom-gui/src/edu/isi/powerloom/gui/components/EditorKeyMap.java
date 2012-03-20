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


// Version: EditorKeyMap.java,v 1.15 2010/02/04 05:17:18 hans Exp

package edu.isi.powerloom.gui.components;

import java.util.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;

import edu.isi.powerloom.gui.parser.*;
import edu.isi.powerloom.gui.xmlobject.*;
import edu.isi.powerloom.gui.common.*;
import static edu.isi.powerloom.gui.common.Utils.*;

/**
 * Installs emacs-style keybindings for expression editor source pane.
 * Also includes keybindings for completion, performing actions, etc.
 * <h3>Current Bindings:</h3>
 * 
 * <table border=2>
 * <th>Key</th><th>Action</th>
 *
 * <tr><td>Ctrl-SPC</td> <td>Start Selection</td></tr>
 * <tr><td>Ctrl-RETURN</td> <td>Execute Component Action</td></tr>
 * <tr><td>Ctrl-Right Arrow</td> <td>Complete Symbol</td></tr>
 * <tr><td>Ctrl-A</td> <td>Begin Line</td></tr>
 * <tr><td>Ctrl-B</td> <td>Backward Char</td></tr>
 * <tr><td>Ctrl-D</td> <td>Delete Next Char</td></tr>
 * <tr><td>Ctrl-E</td> <td>End Line</td></tr>
 * <tr><td>Ctrl-F</td> <td>Forward Char</td></tr>
 * <tr><td>Ctrl-G</td> <td>Cancel Selection</td></tr>
 * <tr><td>Ctrl-K</td> <td>Kill to End of Line</td></tr>
 * <tr><td>Ctrl-N</td> <td>Next Line</td></tr>
 * <tr><td>Ctrl-P</td> <td>Previous Line</td></tr>
 * <tr><td>Ctrl-W</td> <td>Cut</td></tr>
 * <tr><td>Ctrl-Y</td> <td>Paste</td></tr>
 * <tr><td>Meta-B</td> <td>Begin Word</td></tr>
 * <tr><td>Meta-F</td> <td>End Word</td></tr>
 * <tr><td>Meta-W</td> <td>Copy</td></tr>
 * <tr><td>Ctrl-Meta-SPC</td> <td>Select Forward Expression</td></tr>
 * <tr><td>Ctrl-Meta-B</td> <td>Backward Expression</td></tr>
 * <tr><td>Ctrl-Meta-D</td> <td>Down Expression</td></tr>
 * <tr><td>Ctrl-Meta-F</td> <td>Forward Expression</td></tr>
 * <tr><td>Ctrl-Meta-K</td> <td>Kill Forward Expression</td></tr>
 * <tr><td>Ctrl-Meta-U</td> <td>Up Expression</td></tr>
 * </table>
 *
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 * @since Thu Apr 04 12:25:22 2002
 */

public class EditorKeyMap {
    static final Hashtable actionTable = new Hashtable();
    static final Hashtable editState = new Hashtable();

    // Action constants
    static final String executeComponentAction = "executeComponent";
    static final String killEndLineAction = "killEndLine";
    static final String completeSymbolAction = "completeSymbol";
    static final String startSelectionAction = "startSelection";
    static final String selectableForwardAction = "selectableForward";
    static final String selectableBackwardAction = "selectableBackward";
    static final String selectableBeginLineAction = "selectableBeginLine";
    static final String selectableEndLineAction = "selectableEndLine";
    static final String selectableUpAction = "selectableUp";
    static final String selectableDownAction = "selectableDown";
    static final String selectableBeginWordAction = "selectableBeginWord";
    static final String selectableEndWordAction = "selectableEndWord";
    static final String selectableUpExpressionAction = "selectableUpExpressionAction";
    static final String selectableDownExpressionAction = "selectableDownExpressionAction";
    static final String selectableForwardExpressionAction = "selectableForwardExpressionAction";
    static final String selectableBackwardExpressionAction = "selectableBackwardExpressionAction";
    static final String selectForwardExpressionAction = "selectForwardExpressionAction";
    static final String killForwardExpressionAction = "killForwardExpressionAction";
    static final String cancelSelectionWithBeepAction = "cancelSelectionActionWithBeep";
    static final String cancelSelectionAction = "cancelSelectionAction";
    static final String cutAction = "cut";
    static final String copyAction = "copy";

    static class ExecuteComponentAction extends TextAction {
	public ExecuteComponentAction(String name) {
	    super(name);
	}

	public void actionPerformed(ActionEvent e) {
	    PowerloomApp.getInstance().doActionOnCurrentlySelectedComponent();
	}
    }


    static class KillEndLineAction extends TextAction {
	public KillEndLineAction(String name) {
	    super(name);
	}

	public void actionPerformed(ActionEvent ev) {
	    Action selectAction = (Action)actionTable.get(DefaultEditorKit.selectionEndLineAction);
	    Action cutAction = (Action)actionTable.get(DefaultEditorKit.cutAction);
	    selectAction.actionPerformed(ev);
	    cutAction.actionPerformed(ev);
	}
    }

    static class CompleteSymbolAction extends TextAction {
	public CompleteSymbolAction(String name) {
	    super(name);
	}

	String[] getCompletions(String partialString) {
	    try {
		//String[] completions = KnowledgeManager.getInstance().matchAllSurrogates(partialString);
		CompletionResult completionResult = ParserInterfaceFactory.getParserInterface().getCompletionList(partialString, partialString.length());
		Set completionsSet = completionResult.completions;
		String[] completions = (String[])completionsSet.toArray(new String[0]);
		debugPrintln(3, "completion text = " + completionResult.text + ", line = " + completionResult.line + ", begin = " + completionResult.charBegin + ", end = " + completionResult.charEnd);
		debugPrintln(3, "completsions for " + partialString + ": ");
		for (int i = 0; i < completions.length; i++) {
		    debugPrintln(3, "  |" + completions[i] + "|");
		}
		return completions;
	    } catch (Exception e) {
		PowerloomApp.getInstance().handleException(e);
	    } // end of try-catch
	    /*
		     
	    String[] items = new String[4];
	    for (int i = 0; i < 4; i++) {
		items[i] = partialString + i;
	    } 
	    return items;
	    */
	    return new String[0];
	}

	private int computeComboWidth(JComboBox combo) {
	    Font font = combo.getFont();
	    FontMetrics metrics = combo.getFontMetrics(font);
	    int charWidth = metrics.charWidth('m') + 1; // assume 1 pixel inter-char space
	    int maxWidth = 0;
	    for (int i = 0; i < combo.getItemCount(); i++) {
		PLObject object = (PLObject)combo.getItemAt(i);
		String item = object.getID();
		// assume fixed font like courier
		int itemWidth = charWidth * item.length();
		maxWidth = Math.max(maxWidth, itemWidth);
	    } 
	    return maxWidth + (3 * charWidth); // padding
	}

	public void actionPerformed(ActionEvent event) {
	    try {	    
		if (event.getSource() instanceof JTextPane) {
		    final JTextPane pane = (JTextPane)event.getSource();
		    int savedDot = pane.getCaret().getDot();
		    // get completions
		    final CompletionResult completionResult = ParserInterfaceFactory.getParserInterface().getCompletionList(pane.getText(), savedDot);
		    // create dummy concept for canceling...
		    String cancelString = "<Cancel Completion>";
		    PLSurrogate cancelSurrogate = PLSurrogateContainer.surrogatify(cancelString);
		    final PLObject CANCEL_STRING = (PLObject)cancelSurrogate.getValue();
		    Set completionsSet = completionResult.completions;
		    java.util.List completionsList = new ArrayList(completionsSet);
		    if (completionsList.size() > 1) {
			completionsList.add(0, cancelSurrogate);
		    }
		    PLListModel completionsModel = new PLListModel(PowerloomApp.getInstance().getMostRecentlyTouchedModule(), new PLSurrogateContainer(completionsList));

		    // obsolet: String[] completions = (String[])completionsList.toArray(new String[0]);

		    // if we're doing a completion on partial input, select the text
		    if (completionResult.text != null) {
			pane.setSelectionStart(completionResult.charBegin);
			pane.setSelectionEnd(completionResult.charEnd);
		    } else {
			pane.setSelectionStart(savedDot);
			pane.setSelectionEnd(savedDot);
		    } 

		    if (completionsList.size() == 0) {
			// deselect selection
			pane.setSelectionEnd(pane.getSelectionStart());
			// restore caret pos
			pane.getCaret().setDot(savedDot);
			// beep to indicate no selection
			Action beep = (Action)actionTable.get(DefaultEditorKit.beepAction);
			beep.actionPerformed(event);
			return;
		    }

		    if (completionsList.size() == 1) {
			pane.replaceSelection(((PLObject)((PLSurrogate)completionsList.get(0)).getValue()).getID());
			return;
		    }
		    final JComboBox combo = new JComboBox(completionsModel);
		    combo.setRenderer(new PLListRenderer());
		    combo.setSelectedItem(CANCEL_STRING);
		    Font font = new Font("Courier New", Font.PLAIN, 14);
		    combo.setFont(font);
		    int comboWidth = computeComboWidth(combo);
		    combo.setMaximumSize(new Dimension(comboWidth, (int)combo.getPreferredSize().getHeight()));
		    final int mark = pane.getSelectionStart();
		    combo.addActionListener(new ActionListener() {
			    public void actionPerformed(ActionEvent e) {
				PLObject selectedItem = (PLObject)combo.getSelectedItem();
				String selectedString = "**error**";
				if (selectedItem.equals(CANCEL_STRING)) {
				    selectedString = completionResult.text;
				} else {
				    selectedString = RenderUtils.getModuleSensitiveFQN(selectedItem);
				}
				pane.setSelectionStart(mark);
				pane.setSelectionEnd(mark+1);
				pane.replaceSelection(selectedString);
				// restore caret pos
				pane.getCaret().setDot(mark + selectedString.length());
			    }
			});

		    combo.addFocusListener(new FocusListener() {
			    public void focusGained(FocusEvent e) {
			    }

			    public void focusLost(FocusEvent e) {
				// necessary to reset the focus back to the pane
				pane.grabFocus();
			    }
			});
		    pane.insertComponent(combo);
		}
	    } catch (Exception e) {
		PowerloomApp.getInstance().handleException(e);
	    } // end of try-catch

	}
    }

    static class StartSelectionAction extends TextAction {
	public StartSelectionAction(String name) {
	    super(name);
	}

	public void actionPerformed(ActionEvent ev) {
	    JTextComponent pane = getTextComponent(ev);
	    pane.setSelectionStart(pane.getCaret().getDot());
	    editState.put("isSelectionOn", new Boolean(true));
	}
    }

    static class CancelSelectionAction extends TextAction {
	public CancelSelectionAction(String name) {
	    super(name);
	}

	public void actionPerformed(ActionEvent ev) {
	    JTextComponent pane = getTextComponent(ev);
	    int currentPos = pane.getCaretPosition();
	    pane.select(currentPos, currentPos);
	    editState.put("isSelectionOn", new Boolean(false));
	}
    }

    static class CancelSelectionWithBeepAction extends TextAction {
	public CancelSelectionWithBeepAction(String name) {
	    super(name);
	}

	public void actionPerformed(ActionEvent ev) {
	    Action action = (Action)actionTable.get(EditorKeyMap.cancelSelectionAction);
	    action.actionPerformed(ev);
	    // Since Ctrl-G is mapped to this action, beep too
	    Action beep = (Action)actionTable.get(DefaultEditorKit.beepAction);
	    beep.actionPerformed(ev);
	}
    }

    static class SelectableForwardAction extends TextAction {
	public SelectableForwardAction(String name) {
	    super(name);
	}

	public void actionPerformed(ActionEvent ev) {
	    if (((Boolean)editState.get("isSelectionOn")).booleanValue()) {
		Action selectAction = (Action)actionTable.get(DefaultEditorKit.selectionForwardAction);
		selectAction.actionPerformed(ev);
	    } else {
		Action selectAction = (Action)actionTable.get(DefaultEditorKit.forwardAction);
		selectAction.actionPerformed(ev);
	    } 
	}
    }

    static class SelectableBackwardAction extends TextAction {
	public SelectableBackwardAction(String name) {
	    super(name);
	}

	public void actionPerformed(ActionEvent ev) {
	    if (((Boolean)editState.get("isSelectionOn")).booleanValue()) {
		Action selectAction = (Action)actionTable.get(DefaultEditorKit.selectionBackwardAction);
		selectAction.actionPerformed(ev);
	    } else {
		Action selectAction = (Action)actionTable.get(DefaultEditorKit.backwardAction);
		selectAction.actionPerformed(ev);
	    } 
	}
    }

    static class SelectableBeginLineAction extends TextAction {
	public SelectableBeginLineAction(String name) {
	    super(name);
	}

	public void actionPerformed(ActionEvent ev) {
	    if (((Boolean)editState.get("isSelectionOn")).booleanValue()) {
		Action selectAction = (Action)actionTable.get(DefaultEditorKit.selectionBeginLineAction);
		selectAction.actionPerformed(ev);
	    } else {
		Action selectAction = (Action)actionTable.get(DefaultEditorKit.beginLineAction);
		selectAction.actionPerformed(ev);
	    } 
	}
    }

    static class SelectableEndLineAction extends TextAction {
	public SelectableEndLineAction(String name) {
	    super(name);
	}

	public void actionPerformed(ActionEvent ev) {
	    if (((Boolean)editState.get("isSelectionOn")).booleanValue()) {
		Action selectAction = (Action)actionTable.get(DefaultEditorKit.selectionEndLineAction);
		selectAction.actionPerformed(ev);
	    } else {
		Action selectAction = (Action)actionTable.get(DefaultEditorKit.endLineAction);
		selectAction.actionPerformed(ev);
	    } 
	}
    }

    static class SelectableUpAction extends TextAction {
	public SelectableUpAction(String name) {
	    super(name);
	}

	public void actionPerformed(ActionEvent ev) {
	    if (((Boolean)editState.get("isSelectionOn")).booleanValue()) {
		Action selectAction = (Action)actionTable.get(DefaultEditorKit.selectionUpAction);
		selectAction.actionPerformed(ev);
	    } else {
		Action selectAction = (Action)actionTable.get(DefaultEditorKit.upAction);
		selectAction.actionPerformed(ev);
	    } 
	}
    }


    static class SelectableDownAction extends TextAction {
	public SelectableDownAction(String name) {
	    super(name);
	}

	public void actionPerformed(ActionEvent ev) {
	    if (((Boolean)editState.get("isSelectionOn")).booleanValue()) {
		Action selectAction = (Action)actionTable.get(DefaultEditorKit.selectionDownAction);
		selectAction.actionPerformed(ev);
	    } else {
		Action selectAction = (Action)actionTable.get(DefaultEditorKit.downAction);
		selectAction.actionPerformed(ev);
	    } 
	}
    }


    static class SelectableBeginWordAction extends TextAction {
	public SelectableBeginWordAction(String name) {
	    super(name);
	}

	public void actionPerformed(ActionEvent ev) {
	    if (((Boolean)editState.get("isSelectionOn")).booleanValue()) {
		Action selectAction = (Action)actionTable.get(DefaultEditorKit.selectionBeginWordAction);
		selectAction.actionPerformed(ev);
	    } else {
		Action selectAction = (Action)actionTable.get(DefaultEditorKit.beginWordAction);
		selectAction.actionPerformed(ev);
	    } 
	}
    }

    static class SelectableEndWordAction extends TextAction {
	public SelectableEndWordAction(String name) {
	    super(name);
	}

	public void actionPerformed(ActionEvent ev) {
	    if (((Boolean)editState.get("isSelectionOn")).booleanValue()) {
		Action selectAction = (Action)actionTable.get(DefaultEditorKit.selectionEndWordAction);
		selectAction.actionPerformed(ev);
	    } else {
		Action selectAction = (Action)actionTable.get(DefaultEditorKit.endWordAction);
		selectAction.actionPerformed(ev);
	    } 
	}
    }

    static class SelectableUpExpressionAction extends TextAction {
	public SelectableUpExpressionAction(String name) {
	    super(name);
	}

	public void actionPerformed(ActionEvent ev) {
	    JTextPane pane = (JTextPane)ev.getSource();
	    int currentPos = pane.getCaret().getDot();
	    int newPos = StringUtils.findParentLeftParen(pane.getText(), currentPos);
	    if (newPos >= 0) {
		if (((Boolean)editState.get("isSelectionOn")).booleanValue()) {
		    pane.moveCaretPosition(newPos);
		} else {
		    pane.getCaret().setDot(newPos);
		}
	    }
	}
    }

    static class SelectableDownExpressionAction extends TextAction {
	public SelectableDownExpressionAction(String name) {
	    super(name);
	}

	public void actionPerformed(ActionEvent ev) {
	    JTextPane pane = (JTextPane)ev.getSource();
	    int currentPos = pane.getCaret().getDot();
	    int newPos = StringUtils.findChildLeftParen(pane.getText(), currentPos);
	    if (newPos >= 0) {
		if (((Boolean)editState.get("isSelectionOn")).booleanValue()) {
		    pane.moveCaretPosition(newPos);
		} else {
		    pane.getCaret().setDot(newPos);
		}
	    }
	}
    }
    

    static class SelectableForwardExpressionAction extends TextAction {
	public SelectableForwardExpressionAction(String name) {
	    super(name);
	}

	public void actionPerformed(ActionEvent ev) {
	    JTextComponent pane = getTextComponent(ev);
	    int currentPos = pane.getCaretPosition();
	    if (pane.getText().charAt(currentPos) == '(') {
		int newPos = StringUtils.findMatchingRightParen(pane.getText(), currentPos);
		if (newPos > 0) {
		    if (((Boolean)editState.get("isSelectionOn")).booleanValue()) {
			pane.moveCaretPosition(newPos + 1);
		    } else {
			pane.getCaret().setDot(newPos + 1);
		    }
		}
	    } else {
		Action selectAction = (Action)actionTable.get(EditorKeyMap.selectableEndWordAction);
		selectAction.actionPerformed(ev);
	    }
	}
    }

    static class SelectForwardExpressionAction extends TextAction {
	public SelectForwardExpressionAction(String name) {
	    super(name);
	}

	public void actionPerformed(ActionEvent ev) {
	    Action startSelectAction = (Action)actionTable.get(EditorKeyMap.startSelectionAction);
	    startSelectAction.actionPerformed(ev);	    
	    Action selectAction = (Action)actionTable.get(EditorKeyMap.selectableForwardExpressionAction);
	    selectAction.actionPerformed(ev);
	    JTextComponent pane = getTextComponent(ev);
	    int mark = pane.getCaret().getMark();
	    int dot = pane.getCaret().getDot();
	    // switch mark and dot
	    pane.setSelectionStart(dot);
	    pane.moveCaretPosition(mark);
	}
    }



    static class KillForwardExpressionAction extends TextAction {
	public KillForwardExpressionAction(String name) {
	    super(name);
	}

	public void actionPerformed(ActionEvent ev) {
	    Action selectAction = (Action)actionTable.get(EditorKeyMap.selectForwardExpressionAction);
	    selectAction.actionPerformed(ev);
	    Action cutAction = (Action)actionTable.get(EditorKeyMap.cutAction);
	    cutAction.actionPerformed(ev);
	}
    }

    static class SelectableBackwardExpressionAction extends TextAction {
	public SelectableBackwardExpressionAction(String name) {
	    super(name);
	}

	public void actionPerformed(ActionEvent ev) {
	    JTextComponent pane = getTextComponent(ev);
	    int currentPos = pane.getCaretPosition();
	    if ((currentPos > 1) && (pane.getText().charAt(currentPos - 1) == ')')) {
		int newPos = StringUtils.findMatchingLeftParen(pane.getText(), currentPos - 1);
		if (newPos > 0) {
		    if (((Boolean)editState.get("isSelectionOn")).booleanValue()) {
			pane.moveCaretPosition(newPos);
		    } else {
			pane.getCaret().setDot(newPos);
		    }
		}
	    } else {
		Action selectAction = (Action)actionTable.get(EditorKeyMap.selectableBeginWordAction);
		selectAction.actionPerformed(ev);
	    }
	}
    }

    /**
     * Cut text and disable selection mode
     */
    // TODO: MOUSE-CLICKS ALSO TURN OFF SELECTION MODE
    static class CutAction extends TextAction {
	public CutAction(String name) {
	    super(name);
	}

	public void actionPerformed(ActionEvent ev) {
	    Action cutAction = (Action)actionTable.get(DefaultEditorKit.cutAction);
	    cutAction.actionPerformed(ev);
	    editState.put("isSelectionOn", new Boolean(false));
	}
    }

    static class CopyAction extends TextAction {
	public CopyAction(String name) {
	    super(name);
	}

	public void actionPerformed(ActionEvent ev) {
	    Action copyAction = (Action)actionTable.get(DefaultEditorKit.copyAction);
	    copyAction.actionPerformed(ev);
	    Action cancelSelectionAction = (Action)actionTable.get(EditorKeyMap.cancelSelectionAction);
	    cancelSelectionAction.actionPerformed(ev);
	}
    }



    private static void initializeActionTable(JTextComponent textPane) {
	// First get actions from defaulteditorkit
	Action[] actions = textPane.getActions();
	for (int i=0; i<actions.length; i++) {
	    String name = (String)actions[i].getValue(Action.NAME);
	    actionTable.put(name, actions[i]);
	} 
	actionTable.put(EditorKeyMap.executeComponentAction, new ExecuteComponentAction(EditorKeyMap.executeComponentAction));
	actionTable.put(EditorKeyMap.killEndLineAction, new KillEndLineAction(EditorKeyMap.killEndLineAction));
	actionTable.put(EditorKeyMap.completeSymbolAction, new CompleteSymbolAction(EditorKeyMap.completeSymbolAction));
	actionTable.put(EditorKeyMap.startSelectionAction, new StartSelectionAction(EditorKeyMap.startSelectionAction));
	actionTable.put(EditorKeyMap.cancelSelectionAction, new CancelSelectionAction(EditorKeyMap.cancelSelectionAction));
	actionTable.put(EditorKeyMap.cancelSelectionWithBeepAction, new CancelSelectionWithBeepAction(EditorKeyMap.cancelSelectionWithBeepAction));
	actionTable.put(EditorKeyMap.selectableForwardAction, 
			new SelectableForwardAction(EditorKeyMap.selectableForwardAction));
	actionTable.put(EditorKeyMap.selectableBackwardAction, 
			new SelectableBackwardAction(EditorKeyMap.selectableBackwardAction));
	actionTable.put(EditorKeyMap.selectableBeginLineAction, 
			new SelectableBeginLineAction(EditorKeyMap.selectableBeginLineAction));
	actionTable.put(EditorKeyMap.selectableEndLineAction, 
			new SelectableEndLineAction(EditorKeyMap.selectableEndLineAction));
	actionTable.put(EditorKeyMap.selectableUpAction, 
			new SelectableUpAction(EditorKeyMap.selectableUpAction));
	actionTable.put(EditorKeyMap.selectableDownAction, 
			new SelectableDownAction(EditorKeyMap.selectableDownAction));
	actionTable.put(EditorKeyMap.selectableBeginWordAction, 
			new SelectableBeginWordAction(EditorKeyMap.selectableBeginWordAction));
	actionTable.put(EditorKeyMap.selectableEndWordAction, 
			new SelectableEndWordAction(EditorKeyMap.selectableEndWordAction));
	actionTable.put(EditorKeyMap.selectableUpExpressionAction, 
			new SelectableUpExpressionAction(EditorKeyMap.selectableUpExpressionAction));
	actionTable.put(EditorKeyMap.selectableDownExpressionAction, 
			new SelectableDownExpressionAction(EditorKeyMap.selectableDownExpressionAction));
	actionTable.put(EditorKeyMap.selectableForwardExpressionAction, 
			new SelectableForwardExpressionAction(EditorKeyMap.selectableForwardExpressionAction));
	actionTable.put(EditorKeyMap.selectableBackwardExpressionAction, 
			new SelectableBackwardExpressionAction(EditorKeyMap.selectableBackwardExpressionAction));
	actionTable.put(EditorKeyMap.selectForwardExpressionAction, 
			new SelectForwardExpressionAction(EditorKeyMap.selectForwardExpressionAction));
	actionTable.put(EditorKeyMap.killForwardExpressionAction, 
			new KillForwardExpressionAction(EditorKeyMap.killForwardExpressionAction));
	actionTable.put(EditorKeyMap.cutAction, new CutAction(EditorKeyMap.cutAction));
	actionTable.put(EditorKeyMap.copyAction, new CopyAction(EditorKeyMap.copyAction));

	editState.put("isSelectionOn", new Boolean(false));

    }
    public static void setupKeyMap(JTextComponent textPane) {
	initializeActionTable(textPane);
	Keymap parent = textPane.getKeymap();
	Keymap map = JTextComponent.addKeymap("EmacsMap", parent);
	
	KeyStroke keyStroke;
	// CTRL-SPC -> start-selection
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_SPACE,
					   InputEvent.CTRL_MASK, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(EditorKeyMap.startSelectionAction));

	// CTRL-RET -> executeComponentAction
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, InputEvent.CTRL_MASK, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(EditorKeyMap.executeComponentAction));

	// CTRL-A -> begin-line
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_A,
					   InputEvent.CTRL_MASK, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(EditorKeyMap.selectableBeginLineAction));

	// CTRL-B -> backward-char
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_B,
					   InputEvent.CTRL_MASK, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(EditorKeyMap.selectableBackwardAction));

	// CTRL-RIGHT-ARROW -> complete-symbol
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_RIGHT,
					   InputEvent.CTRL_MASK, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(EditorKeyMap.completeSymbolAction));

	// CTRL-D -> delete-next-char
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_D,
					   InputEvent.CTRL_MASK, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(DefaultEditorKit.deleteNextCharAction));

	// CTRL-E -> end-line
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_E,
					   InputEvent.CTRL_MASK, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(EditorKeyMap.selectableEndLineAction));

	// CTRL-F -> forward-char
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_F,
					   InputEvent.CTRL_MASK, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(EditorKeyMap.selectableForwardAction));

	// CTRL-G -> cancel selection and beep
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_G,
					   InputEvent.CTRL_MASK, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(EditorKeyMap.cancelSelectionWithBeepAction));

	// CTRL-K -> kill-to-end-of-line
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_K,
					   InputEvent.CTRL_MASK, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(EditorKeyMap.killEndLineAction));

	// CTRL-N -> next-line
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_N,
					   InputEvent.CTRL_MASK, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(EditorKeyMap.selectableDownAction));

	// CTRL-P -> prev-line
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_P,
					   InputEvent.CTRL_MASK, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(EditorKeyMap.selectableUpAction));

	// CTRL-W -> cut
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_W,
					   InputEvent.CTRL_MASK, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(EditorKeyMap.cutAction));

	// CTRL-Y -> paste
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_Y,
					   InputEvent.CTRL_MASK, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(DefaultEditorKit.pasteAction));

	// META-B -> begin-word
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_B,
					   InputEvent.META_MASK, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(EditorKeyMap.selectableBeginWordAction));
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_B,
					   InputEvent.ALT_MASK, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(EditorKeyMap.selectableBeginWordAction));

	// META-F -> end-word
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_F,
					   InputEvent.META_MASK, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(EditorKeyMap.selectableEndWordAction));
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_F,
					   InputEvent.ALT_MASK, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(EditorKeyMap.selectableEndWordAction));

	// META-W -> copy
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_W,
					   InputEvent.META_MASK, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(EditorKeyMap.copyAction));
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_W,
					   InputEvent.ALT_MASK, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(EditorKeyMap.copyAction));


	// CTRL-META-SPC -> Select-forward-expression
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_SPACE,
					   InputEvent.CTRL_MASK | InputEvent.META_MASK, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(EditorKeyMap.selectForwardExpressionAction));
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_SPACE,
					   InputEvent.CTRL_MASK | InputEvent.ALT_MASK, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(EditorKeyMap.selectForwardExpressionAction));

	// CTRL-META-B -> Backward-expression
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_B,
					   InputEvent.CTRL_MASK | InputEvent.META_MASK, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(EditorKeyMap.selectableBackwardExpressionAction));
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_B,
					   InputEvent.CTRL_MASK | InputEvent.ALT_MASK, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(EditorKeyMap.selectableBackwardExpressionAction));

	// CTRL-META-D -> Down-expression
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_D,
					   InputEvent.CTRL_MASK | InputEvent.META_MASK, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(EditorKeyMap.selectableDownExpressionAction));
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_D,
					   InputEvent.CTRL_MASK | InputEvent.ALT_MASK, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(EditorKeyMap.selectableDownExpressionAction));

	// CTRL-META-F -> Forward-expression
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_F,
					   InputEvent.CTRL_MASK | InputEvent.META_MASK, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(EditorKeyMap.selectableForwardExpressionAction));
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_F,
					   InputEvent.CTRL_MASK | InputEvent.ALT_MASK, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(EditorKeyMap.selectableForwardExpressionAction));

	// CTRL-META-K -> Kill-Forward-expression
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_K,
					   InputEvent.CTRL_MASK | InputEvent.META_MASK, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(EditorKeyMap.killForwardExpressionAction));
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_K,
					   InputEvent.CTRL_MASK | InputEvent.ALT_MASK, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(EditorKeyMap.killForwardExpressionAction));

	// CTRL-META-U -> Up-expression
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_U,
					   InputEvent.CTRL_MASK | InputEvent.META_MASK, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(EditorKeyMap.selectableUpExpressionAction));
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_U,
					   InputEvent.CTRL_MASK | InputEvent.ALT_MASK, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(EditorKeyMap.selectableUpExpressionAction));

	// Set the map for our text pane to our new map
	textPane.setKeymap(map);
    }
}
