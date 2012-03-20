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


// Version: EditInstanceCellKeyMap.java,v 1.5 2010/02/04 05:17:15 hans Exp

package edu.isi.powerloom.gui.components;

import java.util.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;
import edu.isi.powerloom.gui.parser.*;
import static edu.isi.powerloom.gui.common.Utils.*;

/**
 * Keymap used by cell editors.  For example, the Extension Editor uses this.
 *
 * @since Wed Sep 11 11:44:26 2002
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */

public class EditInstanceCellKeyMap {
    static final Hashtable actionTable = new Hashtable();
    static final String completeInstanceAction = "completeInstance";
    static final String enterValueAction = "enterValue";
    static final String tabToNextCellAction = "tabToNextCell";
    static final String tabToPreviousCellAction = "tabToPreviousCell";
    static final String deleteRowAction = "deleteRow";

    static class DeleteRowAction extends TextAction {
	public DeleteRowAction(String name) {
	    super(name);
	}

	public void actionPerformed(ActionEvent e) {
	    debugPrintln(3, "Deleting row...");
	    EditInstanceCellPanel currentPanel = PowerloomApp.getInstance().getSelectedInstanceCellEditor();
	    debugPrintln(3, "currentPanel = " + currentPanel);
	    if (currentPanel instanceof ExtensionCellEditor) {
		ExtensionCellEditor editor = (ExtensionCellEditor)currentPanel;
		editor.deleteRow();
	    }
	}
    }

    static class CompleteInstanceAction extends TextAction {
	public CompleteInstanceAction(String name) {
	    super(name);
	}

	public void actionPerformed(ActionEvent e) {
	    EditInstanceCellPanel currentPanel = PowerloomApp.getInstance().getSelectedInstanceCellEditor();
	    if (currentPanel instanceof ExtensionCellEditor) {
		ExtensionCellEditor editor = (ExtensionCellEditor)currentPanel;
		ExtensionTableModel tableModel = (ExtensionTableModel)editor.getTable().getModel();
		int row = editor.getRow();
		int column = editor.getColumn();
		String currentText = editor.getText();
		tableModel.setCompletionFragment(currentText);
		tableModel.setCurrentlyCompletingCell(row, column);
		editor.cancelCellEditing();
		editor.getTable().editCellAt(row, column);
	    }
	}
    }


    static class EnterValueAction extends TextAction {
	public EnterValueAction(String name) {
	    super(name);
	}

	public void actionPerformed(ActionEvent e) {
	    EditInstanceCellPanel currentPanel = PowerloomApp.getInstance().getSelectedInstanceCellEditor();
	    if (currentPanel instanceof ExtensionCellEditor) {
		ExtensionCellEditor editor = (ExtensionCellEditor)currentPanel;
		editor.stopCellEditing();
	    }
	}
    }

    static class TabToNextCellAction extends TextAction {
	private int direction = 0;

	public TabToNextCellAction(String name, int direction) {
	    super(name);
	    this.direction = direction;
	}

	public void actionPerformed(ActionEvent e) {
	    EditInstanceCellPanel currentPanel = PowerloomApp.getInstance().getSelectedInstanceCellEditor();
	    if (currentPanel instanceof ExtensionCellEditor) {
		ExtensionCellEditor editor = (ExtensionCellEditor)currentPanel;
		JTable table = editor.getTable();
		editor.stopCellEditing();
		table.clearSelection();
		table.requestFocus();
		
		int currentRow = editor.getRow();
		int currentColumn = editor.getColumn();
		int maxColumns = table.getColumnCount();
		int maxRows = table.getRowCount();
		int row = -1;
		int column = (currentColumn + direction) % maxColumns;
		if (column < 0) {
		    column = maxColumns - 1;
		}
		if (((column < currentColumn) && (direction == 1)) ||
		    ((column > currentColumn) && (direction == -1))) {
		    row = (currentRow + direction) % maxRows;
		    if (row < 0) {
			row = maxRows - 1;
		    }
		} else {
		    row = currentRow;
		}
		debugPrintln(3, "currentRow = " + currentRow + ", row = " + row);
		debugPrintln(3, "currentCol = " + currentColumn + ", col = " + column);
		table.getSelectionModel().setAnchorSelectionIndex(row);
		table.getColumnModel().getSelectionModel().setAnchorSelectionIndex(column);

		boolean isSelected = table.isCellSelected(row, column);
		boolean rowIsAnchor = (table.getSelectionModel().getAnchorSelectionIndex() == row);
		boolean colIsAnchor =
		    (table.getColumnModel().getSelectionModel().getAnchorSelectionIndex() == column);
		boolean hasFocus = (rowIsAnchor && colIsAnchor) && table.isFocusOwner();
		debugPrintln(3, "isSelected = " + isSelected + ", rowIsAnchor = " + rowIsAnchor + ", colIsAnchor = " + colIsAnchor + ", hasFocus = " + hasFocus);

	    }
	}
    }


    private static void initializeActionTable(JTextComponent textPane) {
	// First get actions from defaulteditorkit
	actionTable.put(EditInstanceCellKeyMap.completeInstanceAction, new CompleteInstanceAction(EditInstanceCellKeyMap.completeInstanceAction));
	actionTable.put(EditInstanceCellKeyMap.enterValueAction, new EnterValueAction(EditInstanceCellKeyMap.enterValueAction));
	actionTable.put(EditInstanceCellKeyMap.tabToNextCellAction, new TabToNextCellAction(EditInstanceCellKeyMap.tabToNextCellAction, 1));
	actionTable.put(EditInstanceCellKeyMap.tabToPreviousCellAction, new TabToNextCellAction(EditInstanceCellKeyMap.tabToPreviousCellAction, -1));
	actionTable.put(EditInstanceCellKeyMap.deleteRowAction, new DeleteRowAction(EditInstanceCellKeyMap.deleteRowAction));
    }


    public static void setupKeyMap(JTextComponent textPane) {
	initializeActionTable(textPane);
	// "inherit" from EditorKeyMap
	Keymap parent = textPane.getKeymap("EmacsMap");
	Keymap map = JTextComponent.addKeymap("InstanceCellMap", parent);

	KeyStroke keyStroke;
	// RET -> EnterValueAction
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(EditInstanceCellKeyMap.enterValueAction));

	// DEL -> DeleteRowAction
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(EditInstanceCellKeyMap.deleteRowAction));

	// TAB -> TabToNextCellAction
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_TAB, 0, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(EditInstanceCellKeyMap.tabToNextCellAction));

	// SHIFT-TAB -> TabToPreviousCellAction
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_TAB, InputEvent.SHIFT_MASK, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(EditInstanceCellKeyMap.tabToPreviousCellAction));

	// CTRL-RET -> EnterValueAction
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER,
					   InputEvent.CTRL_MASK, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(EditInstanceCellKeyMap.enterValueAction));

	// CTRL-C -> CompleteInstanceAction
	keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_C, InputEvent.CTRL_MASK, false);
	map.addActionForKeyStroke(keyStroke, (Action)actionTable.get(EditInstanceCellKeyMap.completeInstanceAction));


	// Set the map for our text pane to our new map
	textPane.setKeymap(map);

	debugPrintln(3, "setup editinstancekeymap.");
    }    
}
