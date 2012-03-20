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


// Version: ExtensionCellEditor.java,v 1.12 2010/02/04 05:17:22 hans Exp

package edu.isi.powerloom.gui.components;

import edu.isi.powerloom.gui.xmlobject.*;
import edu.isi.powerloom.gui.serverinterface.*;
import static edu.isi.powerloom.gui.common.Utils.*;

import java.awt.Component;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import javax.swing.table.*;
import javax.swing.event.*;

/**
 * Table Cell Editor used by the Extension Editor.
 *
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 * @see edu.isi.powerloom.gui.components.ExtensionFrame ExtensionFrame
 */
public class ExtensionCellEditor extends EditInstanceCellPanel implements TableCellEditor {
    protected Vector listeners;
    protected Object originalValue;
    protected Object value;
    protected ExtensionFrame parentFrame;
    protected JTable table;
    private boolean ignoreAllWarnings = false;
    private boolean ignoreAllDeleteWarnings = false;
    private int clickCountToStart = 2;
    private int row, column;

    public ExtensionCellEditor() {
	/*
	System.err.println("Creating extension default cell editor.  hashcode = " + hashCode());	
	try {
	    throw new Exception("Dummy Exception");
	} catch (Exception e) {
	    e.printStackTrace();
	}
	*/
    }

    public ExtensionCellEditor(ExtensionFrame frame, String name, List types) {
	super(frame, name, types);
	/*
	System.err.println("Creating extension cell editor with frame " + frame.hashCode() + ".  My hashcode = " + hashCode());
	try {
	    throw new Exception("Dummy Exception");
	} catch (Exception e) {
	    e.printStackTrace();
	}
	*/
	parentFrame = frame;
	listeners = new Vector();
    }

    public Object getOriginalValue() {
	return originalValue;
    }

    public void setOriginalValue(Object originalValue) {
	this.originalValue = originalValue;
    }

    public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected, int row, int column) {
	PLModule module = parentFrame.getModule();
	setModule(module);
	ExtensionTableModel model = (ExtensionTableModel)table.getModel();
	this.table = table;
	this.row = row;
	this.column = column;
	PowerloomApp.getInstance().setSelectedInstanceCellEditor(this);
	debugPrintln(3, "constructor. value = " + value);
	if (value == null) {
	    value = "";
	}
	originalValue = value;
	if (model.isCellBeingCompleted(row, column)) {
	    JComboBox comboBox = createCompletionComboBox(this, table);
	    model.setSavedRowHeight(table.getRowHeight(row));
	    table.setRowHeight(row, 23);
	    return comboBox;
	}
	if (value instanceof PLObject) {
	    setText(((PLObject)value).getID());
	} else {
	    setText(value.toString());
	}
	return this;
    }

    private PLConcept getTypeForRelation(int column) {
	PLConcept result = null;
	PLModule module = parentFrame.getModule();
	ExtensionTableModel model = (ExtensionTableModel)table.getModel();
	String relationName = model.getRelationName();
	try {
	    PLRelation relation = KnowledgeManager.getInstance().getRelationObject(module, relationName);
	    if (relation == null) {
		JOptionPane.showMessageDialog(PowerloomApp.getInstance(), "There is no such relation named " + relationName + " in module " + module.getID(), "No such Relation", JOptionPane.WARNING_MESSAGE);
		return null;
	    }
	    PLVariableList vars = KnowledgeManager.getInstance().getVariablesForRelation(module, relation);
	    PLSurrogate surrogate = ((PLVariable)((List)vars.elemPLVariable).get(column)).elemPLSurrogate;
	    if (surrogate != null) {
		PLObject object = (PLObject)surrogate.getValue();
		if ((object != null) &&
		    (object instanceof PLConcept)) {
		    result = (PLConcept) object;
		}
	    }
	} catch (Exception e) {
	    PowerloomApp.getInstance().handleException(e);
	}
	return result;
    }

    private JComboBox createCompletionComboBox(final ExtensionCellEditor editor, final JTable parentTable) {
	try {
	    final ExtensionTableModel model = (ExtensionTableModel)parentTable.getModel();
	    String[] strings = new String[]{"one", "two", "three", "four"};
	    String prefix = model.getCompletionFragment();
	    // do completion here... get all the instances for a the concept corresponding to the type in the current column, 
	    //   then filter on prefix
	    PLConcept concept = getTypeForRelation(column);
	    debugPrintln(3, "got type for relation = " + model.getRelationName() + ", col = " + column + " = " + concept.getID());
	    PLModule module = parentFrame.getModule();
	    PLSurrogateContainer instances = KnowledgeManager.getInstance().getInstancesForConcept(module, concept);
	    PLListModel comboModel = new PLListModel(module, instances.getFilteredSurrogateContainer(prefix));
	    final JComboBox result = new JComboBox(comboModel);
	    result.setRenderer(new PLListRenderer());
	    if (comboModel.getSize() > 0) {
		result.setSelectedItem(comboModel.getElementAt(0));
	    }
	    result.addActionListener(new ActionListener() {
		    public void actionPerformed(ActionEvent e) {
			Object selectedItem = result.getSelectedItem();
			debugPrintln(3, "You selected: " + selectedItem);
			int row = model.getCompletingRow();
			int column = model.getCompletingColumn();
			Object savedOriginalValue = editor.getOriginalValue();
			model.setCurrentlyCompletingCell(-1, -1);
			model.setCompletionItem(selectedItem);
			editor.cancelCellEditing();
			model.setValueAt(selectedItem, row, column);
			// convert from combobox to text...
			parentTable.editCellAt(row, column);
			// trick the textbox into thinking it had the pre-combobox original value
			editor.setOriginalValue(savedOriginalValue);
			// restore the height
			parentTable.setRowHeight(row, model.getSavedRowHeight());
			// "commit" the new text value
			editor.stopCellEditing();
		    }
		});
	    return result;
	} catch (Exception e) {
	    PowerloomApp.getInstance().handleException(e);
	}

	return null;
    }

    public int getRow() {
	return row;
    }

    public int getColumn() {
	return column;
    }

    public JTable getTable() {
	return table;
    }

    public void cancelCellEditing() {
	fireEditingCanceled();
    }

    public Object getCellEditorValue() {
	try {
	    String text = getText();
	    return PLObjectUnion.getObjectFromString(text);
	} catch (Exception e) {
	    PowerloomApp.getInstance().handleException(e);
	}
	return null;
    }

    public boolean isCellEditable(EventObject eo) {
	if (eo instanceof MouseEvent) { 
	    return ((MouseEvent)eo).getClickCount() >= clickCountToStart;
	}
	return true;
    }

    public boolean shouldSelectCell(EventObject eo) {
	return true;
    }

    public boolean stopCellEditing() {
	debugPrintln(3, "stopcellediting. original = " + originalValue + ", new = " + getText());
	if (originalValue.toString().equals(getText())) {
	    fireEditingCanceled();
	    return false;
	}
	if (ignoreAllWarnings || validateValue()) {
	    fireEditingStopped();
	    return true;
	} 
	// validatevalue = false
	int choice = showValidationOptionDialog();
	if ((choice == -1) || (choice == 1)) { // no or cancel
	    setText(originalValue.toString());
	    debugPrintln(3, "setting text to original value: " + originalValue);
	    fireEditingCanceled();
   	    return false;
        }
	if (choice == 0) { //yes
	    fireEditingStopped();
	    return true;
	}
	if (choice == 2) { // ignore all
	    fireEditingStopped();
	    ignoreAllWarnings = true;
	    return true;
	}
	fireEditingCanceled();
	return false;
    }

    private int showValidationOptionDialog() {
	try {
	    String[] options = {"Yes", "No", "Yes For All Such Warnings"};

	    return JOptionPane.
		showOptionDialog(PowerloomApp.getInstance(), 
				 getText() + 
				 " is not the name of an object in the current module.  Do you wish to create a new object?", 
				 "Invalid Entry", 
				 JOptionPane.YES_NO_CANCEL_OPTION, 
				 JOptionPane.WARNING_MESSAGE, 
				 null, 
				 options, 
				 options[1]);
	} catch (Exception e) {
	    PowerloomApp.getInstance().handleException(e);
	}
	return -1;
    }

    public void deleteRow() {
	fireEditingCanceled();
	JTable table = getTable();
	int row = getRow();
	if (!ignoreAllDeleteWarnings) {
	    int choice = showConfirmDeleteDialog();
	    if (choice == 2) { // Yes to all
		ignoreAllDeleteWarnings = true;
	    }
	    if (choice == 1) { // No
		return;
	    }
	}
	debugPrintln(3, "Deleting row " + row);
	ExtensionTableModel tableModel = (ExtensionTableModel)table.getModel();
	tableModel.deleteRow(row);
    }

    private int showConfirmDeleteDialog() {
	try {
	    String[] options = {"Yes", "No", "Yes For All Such Warnings"};
		
	    return JOptionPane.
		showOptionDialog(PowerloomApp.getInstance(), 
				 "Are you sure you wish to delete this row?", 
				 "Confirm Delete", 
				 JOptionPane.YES_NO_CANCEL_OPTION, 
				 JOptionPane.WARNING_MESSAGE, 
				 null, 
				 options, 
				 options[1]);
	} catch (Exception e) {
	    PowerloomApp.getInstance().handleException(e);
	}
	return -1;
    }


    private boolean validateValue() {
	try {
	    PLModule module = parentFrame.getModule();
	    if (module == null) {
		JOptionPane.showMessageDialog(PowerloomApp.getInstance(), "You cannot edit values while the module is not set", "Module not set", JOptionPane.WARNING_MESSAGE);
		return false;
	    }
	    try {
		int x = Integer.parseInt(getText());
		debugPrintln(3, "text is an integer.");
		return true;
	    } catch (Exception e) {
	    }
	    try {
		float x = Float.parseFloat(getText());
		debugPrintln(3, "text is a float.");
		return true;
	    } catch (Exception e) {
	    }
	    PLObject object = KnowledgeManager.getInstance().getPLObject(module, getText());
	    if (object != null) {
		debugPrintln(3, " validated cell value.  object = " + object + ", class = " + object.getClass());
		return true;
	    }
	} catch (Exception e) {
	    PowerloomApp.getInstance().handleException(e);
	}
	return false;
    }

    public void addCellEditorListener(CellEditorListener cell) {
	listeners.add(cell);
    }

    public void removeCellEditorListener(CellEditorListener cell) {
	listeners.remove(cell);
    }

    protected void fireEditingCanceled() {
	getTable().requestFocus();
	setText(originalValue.toString());
	((ExtensionTableModel)getTable().getModel()).setValueAt(originalValue, row, column);
	debugPrintln(3, "fireEditingCanceled: new value is: " + getText());
	ChangeEvent ce = new ChangeEvent(this);
	for (int i = listeners.size() -1 ; i >=0; i--) {
	    CellEditorListener listener = (CellEditorListener)listeners.elementAt(i);
	    listener.editingCanceled(ce);
	}
    }

    protected void fireEditingStopped() {
	getTable().requestFocus();
	debugPrintln(3, " fireeditingstopped: table.isFocus() = " + getTable().isFocusOwner());
	ChangeEvent ce = new ChangeEvent(this);
	for (int i = listeners.size() - 1; i >= 0; i--) {
	    CellEditorListener listener = (CellEditorListener)listeners.elementAt(i);
	    listener.editingStopped(ce);
	}
	ExtensionTableModel tableModel = (ExtensionTableModel)table.getModel();
	tableModel.maybeAddBlankTuple();
    }
}

