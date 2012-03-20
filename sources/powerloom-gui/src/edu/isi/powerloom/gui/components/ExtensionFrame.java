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


// Version: ExtensionFrame.java,v 1.18 2010/02/04 05:17:23 hans Exp

package edu.isi.powerloom.gui.components;

import edu.isi.powerloom.gui.serverinterface.*;
import edu.isi.powerloom.gui.xmlobject.*;
import edu.isi.powerloom.gui.parser.*;
import static edu.isi.powerloom.gui.common.Utils.*;

import java.awt.event.*;
import javax.swing.table.*;
import javax.swing.event.*;
import javax.swing.*;
import java.util.*;

/**
 * Frame for relation extension editor.
 *
 * @since 5/22/2002 1:32:35 PM
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */
public class ExtensionFrame extends PLFrame implements ActionComponent {
	private javax.swing.JPanel ivjButtonPanel = null;
	private javax.swing.JButton ivjCancelButton = null;
	private javax.swing.JButton ivjExecuteButton = null;
	private javax.swing.JButton ivjCommitButton = null;
	private javax.swing.JPanel ivjJInternalFrameContentPane = null;
	private javax.swing.JLabel ivjJLabel1 = null;
	private javax.swing.JPanel ivjQueryGroupPanel = null;
	private javax.swing.JLabel ivjQueryLabel = null;
	private ExpressionEditorPanel ivjQueryPanel = null;
	private TableAdderPanel ivjVariableAdderPanel = null;
	private javax.swing.JPanel ivjVariableGroupPanel = null;
	private javax.swing.JScrollPane ivjJScrollPane2 = null;
	private javax.swing.JPanel ivjResultGroupPanel = null;
	private javax.swing.JLabel ivjResultsLabel = null;
	private javax.swing.JTable ivjResultsTable = null;
	private javax.swing.JPanel ivjVariableQueryResultGroupPanel = null;
	private javax.swing.BoxLayout ivjVariableQueryResultGroupPanelBoxLayout = null;
	private javax.swing.JComboBox ivjModuleComboBox = null;
	private javax.swing.JPanel ivjModulePanel = null;
	private java.util.Collection editListeners = new java.util.ArrayList();
	private PowerloomApp app;
	IvjEventHandler ivjEventHandler = new IvjEventHandler();
	private javax.swing.JScrollPane ivjJScrollPane1 = null;
    private BrowserFrame4 queryBrowser;
    private PLRelation relation;
    private static boolean alwaysRetractFunctions = true;

class IvjEventHandler implements java.awt.event.ActionListener {
		public void actionPerformed(java.awt.event.ActionEvent e) {
			if (e.getSource() == ExtensionFrame.this.getVariableAdderPanel()) 
				connEtoC1(e);
			if (e.getSource() == ExtensionFrame.this.getCancelButton()) 
				connEtoC2(e);
			if (e.getSource() == ExtensionFrame.this.getExecuteButton()) 
				connEtoC3(e);
			if (e.getSource() == ExtensionFrame.this.getCommitButton()) 
				connEtoC4(e);
		};
	};
	private javax.swing.JLabel ivjModuleLabel = null;
/**
 * ExtensionFrame constructor comment.
 */
public ExtensionFrame() {
	super();
	initialize();
}
/**
 * ExtensionFrame constructor comment.
 * @param title java.lang.String
 */
public ExtensionFrame(String title) {
	super(title);
}
/**
 * ExtensionFrame constructor comment.
 * @param title java.lang.String
 * @param resizable boolean
 */
public ExtensionFrame(String title, boolean resizable) {
	super(title, resizable);
}
/**
 * ExtensionFrame constructor comment.
 * @param title java.lang.String
 * @param resizable boolean
 * @param closable boolean
 */
public ExtensionFrame(String title, boolean resizable, boolean closable) {
	super(title, resizable, closable);
}
/**
 * ExtensionFrame constructor comment.
 * @param title java.lang.String
 * @param resizable boolean
 * @param closable boolean
 * @param maximizable boolean
 */
public ExtensionFrame(String title, boolean resizable, boolean closable, boolean maximizable) {
	super(title, resizable, closable, maximizable);
}
/**
 * ExtensionFrame constructor comment.
 * @param title java.lang.String
 * @param resizable boolean
 * @param closable boolean
 * @param maximizable boolean
 * @param iconifiable boolean
 */
public ExtensionFrame(String title, boolean resizable, boolean closable, boolean maximizable, boolean iconifiable) {
	super(title, resizable, closable, maximizable, iconifiable);
}
/**
 * Comment
 */
public void cancelButton_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
     doDefaultCloseAction();
}
/**
 * connEtoC1:  (VariableAdderPanel.action.actionPerformed(java.awt.event.ActionEvent) --> ExtensionFrame.variableAdderPanel_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
 * @param arg1 java.awt.event.ActionEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC1(java.awt.event.ActionEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		this.variableAdderPanel_ActionPerformed(arg1);
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoC2:  (CancelButton.action.actionPerformed(java.awt.event.ActionEvent) --> ExtensionFrame.cancelButton_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
 * @param arg1 java.awt.event.ActionEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC2(java.awt.event.ActionEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		this.cancelButton_ActionPerformed(arg1);
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoC3:  (ExecuteButton.action.actionPerformed(java.awt.event.ActionEvent) --> ExtensionFrame.executeButton_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
 * @param arg1 java.awt.event.ActionEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC3(java.awt.event.ActionEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		this.executeButton_ActionPerformed(arg1);
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}


private void connEtoC4(java.awt.event.ActionEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		this.commitButton_ActionPerformed(arg1);
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * Comment
 */
public void executeButton_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
    try {
		PLModule module = (PLModule)getModuleComboBox().getSelectedItem();
		if (module == null) {
		    JOptionPane.showMessageDialog(app, "You must select a module before you execute a query.", "Select Module", JOptionPane.WARNING_MESSAGE);
		    return;
		}

		String relation = getQueryPanel().getText();
		String[] variables = getVariables(module, relation);
		if (variables == null) {
		    return;
		}
		PLQueryResult queryResult = KnowledgeManager.getInstance().getExtensionForRelation(module, relation);
		ExtensionTableModel tableModel = new ExtensionTableModel(queryResult, relation, variables);
		getResultsTable().setModel(tableModel);
		tableModel.fireTableChanged(null);
    } catch (Exception e) {
	handleException(e);
    } // end of try-catch
}

public void commitButton_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
    try {
	ExtensionTableModel tableModel = (ExtensionTableModel)getResultsTable().getModel();
	PLTuple[] oldTuples = tableModel.getOldTuples();
	debugPrintln(3, "old tuples:");
	for (int i = 0; i < oldTuples.length; i++) {
	    debugPrintln(3, "  " + oldTuples[i]);
	}
	PLTuple[] newTuples = tableModel.getNewTuples();
	debugPrintln(3, "new tuples:");
	for (int i = 0; i < newTuples.length; i++) {
	    debugPrintln(3, "  " + newTuples[i]);
	}
	if (alwaysRetractFunctions || !relation.attrIsFunction.equals("TRUE")) {
	    retractOldPropositions(oldTuples);
	}
	assertNewPropositions(newTuples);
	// update list to reflect current kb state
	executeButton_ActionPerformed(null);
	// create an edit event to refresh everything
	PLEditEvent event = new PLEditEvent(this, null, null);
	fireEditPerformed(event);
	tableModel.resetOldAndNewTuples();
    } catch (Exception e) {
	handleException(e);
    }
}

    private void retractOldPropositions(PLTuple[] oldTuples) {
	PLModule module = getModule();
	try {
	    for (int i = 0; i < oldTuples.length; i++) {
		String propString = createRetractProposition(oldTuples[i]);
		debugPrintln(3, "retracting: " + propString);
		KnowledgeManager.getInstance().evaluateLogicCommand(module, propString);
	    }
	} catch (Exception e) {
	    handleException(e);
	}
    }

    private void assertNewPropositions(PLTuple[] newTuples) {
	PLModule module = getModule();
	try {
	    for (int i = 0; i < newTuples.length; i++) {
		String propString = createAssertProposition(newTuples[i]);
		debugPrintln(3, "asserting: " + propString);
		KnowledgeManager.getInstance().evaluateLogicCommand(module, propString);
	    }
	} catch (Exception e) {
	    handleException(e);
	}
    }

    private String createRetractProposition(PLTuple tuple) {
	String result = "(retract (" + getQueryPanel().getText() + " ";
	Iterator iter = ((List)tuple.elemPLObjectUnion).iterator();
	while (iter.hasNext()) {
	    PLObjectUnion union = (PLObjectUnion)iter.next();
	    result += union.toReadableString() + " ";
	}
	result += "))";
	return result;
    }


    private String createAssertProposition(PLTuple tuple) {
	String result = "(assert (" + getQueryPanel().getText() + " ";
	Iterator iter = ((List)tuple.elemPLObjectUnion).iterator();
	while (iter.hasNext()) {
	    PLObjectUnion union = (PLObjectUnion)iter.next();
	    result += union.toReadableString() + " ";
	}
	result += "))";
	return result;
    }

/**
 * Insert the method's description here.
 * Creation date: (5/22/2002 3:48:08 PM)
 * @return redesign.gui.components.PowerloomApp
 */
public PowerloomApp getApp() {
	return app;
}
/**
 * Return the ButtonPanel property value.
 * @return javax.swing.JPanel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JPanel getButtonPanel() {
	if (ivjButtonPanel == null) {
		try {
			ivjButtonPanel = new javax.swing.JPanel();
			ivjButtonPanel.setName("ButtonPanel");
			ivjButtonPanel.setLayout(new java.awt.FlowLayout());
			getButtonPanel().add(getExecuteButton(), getExecuteButton().getName());
			getButtonPanel().add(getCommitButton(), getCommitButton().getName());
			getButtonPanel().add(getCancelButton(), getCancelButton().getName());
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjButtonPanel;
}
/**
 * Return the CancelButton property value.
 * @return javax.swing.JButton
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JButton getCancelButton() {
	if (ivjCancelButton == null) {
		try {
			ivjCancelButton = new javax.swing.JButton();
			ivjCancelButton.setName("CancelButton");
			ivjCancelButton.setText("Cancel");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjCancelButton;
}
/**
 * Return the ExecuteButton property value.
 * @return javax.swing.JButton
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JButton getExecuteButton() {
	if (ivjExecuteButton == null) {
		try {
			ivjExecuteButton = new javax.swing.JButton();
			ivjExecuteButton.setName("ExecuteButton");
			ivjExecuteButton.setText("Refresh");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjExecuteButton;
}


private javax.swing.JButton getCommitButton() {
	if (ivjCommitButton == null) {
		try {
			ivjCommitButton = new javax.swing.JButton();
			ivjCommitButton.setName("CommitButton");
			ivjCommitButton.setText("Commit");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjCommitButton;
}
/**
 * Return the JInternalFrameContentPane property value.
 * @return javax.swing.JPanel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JPanel getJInternalFrameContentPane() {
	if (ivjJInternalFrameContentPane == null) {
		try {
			ivjJInternalFrameContentPane = new javax.swing.JPanel();
			ivjJInternalFrameContentPane.setName("JInternalFrameContentPane");
			ivjJInternalFrameContentPane.setBorder(new javax.swing.border.EmptyBorder(10,10,10,10));
			ivjJInternalFrameContentPane.setLayout(new java.awt.BorderLayout());
			getJInternalFrameContentPane().add(getVariableQueryResultGroupPanel(), "Center");
			getJInternalFrameContentPane().add(getButtonPanel(), "South");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJInternalFrameContentPane;
}
/**
 * Return the JLabel1 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel1() {
	if (ivjJLabel1 == null) {
		try {
			ivjJLabel1 = new javax.swing.JLabel();
			ivjJLabel1.setName("JLabel1");
			ivjJLabel1.setText("Variables:");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel1;
}
/**
 * Return the JScrollPane1 property value.
 * @return javax.swing.JScrollPane
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JScrollPane getJScrollPane1() {
	if (ivjJScrollPane1 == null) {
		try {
			ivjJScrollPane1 = new javax.swing.JScrollPane();
			ivjJScrollPane1.setName("JScrollPane1");
			getJScrollPane1().setViewportView(getQueryPanel());
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJScrollPane1;
}
/**
 * Return the JScrollPane2 property value.
 * @return javax.swing.JScrollPane
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JScrollPane getJScrollPane2() {
	if (ivjJScrollPane2 == null) {
		try {
			ivjJScrollPane2 = new javax.swing.JScrollPane();
			ivjJScrollPane2.setName("JScrollPane2");
			ivjJScrollPane2.setVerticalScrollBarPolicy(javax.swing.JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
			ivjJScrollPane2.setHorizontalScrollBarPolicy(javax.swing.JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
			getJScrollPane2().setViewportView(getResultsTable());
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJScrollPane2;
}
/**
 * Return the ModuleComboBox property value.
 * @return javax.swing.JComboBox
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JComboBox getModuleComboBox() {
	if (ivjModuleComboBox == null) {
		try {
			ivjModuleComboBox = new javax.swing.JComboBox();
			ivjModuleComboBox.setName("ModuleComboBox");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjModuleComboBox;
}
/**
 * Return the ModuleLabel property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getModuleLabel() {
	if (ivjModuleLabel == null) {
		try {
			ivjModuleLabel = new javax.swing.JLabel();
			ivjModuleLabel.setName("ModuleLabel");
			ivjModuleLabel.setText("Module:");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjModuleLabel;
}
/**
 * Return the ModulePanel property value.
 * @return javax.swing.JPanel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JPanel getModulePanel() {
	if (ivjModulePanel == null) {
		try {
			ivjModulePanel = new javax.swing.JPanel();
			ivjModulePanel.setName("ModulePanel");
			ivjModulePanel.setLayout(new java.awt.FlowLayout());
			getModulePanel().add(getModuleLabel(), getModuleLabel().getName());
			getModulePanel().add(getModuleComboBox(), getModuleComboBox().getName());
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjModulePanel;
}
/**
 * Return the QueryGroupPanel property value.
 * @return javax.swing.JPanel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JPanel getQueryGroupPanel() {
	if (ivjQueryGroupPanel == null) {
		try {
			ivjQueryGroupPanel = new javax.swing.JPanel();
			ivjQueryGroupPanel.setName("QueryGroupPanel");
			ivjQueryGroupPanel.setPreferredSize(new java.awt.Dimension(37, 32));
			ivjQueryGroupPanel.setLayout(new java.awt.BorderLayout());
			ivjQueryGroupPanel.setMinimumSize(new java.awt.Dimension(37, 50));
			getQueryGroupPanel().add(getQueryLabel(), "North");
			getQueryGroupPanel().add(getJScrollPane1(), "Center");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjQueryGroupPanel;
}
/**
 * Return the QueryLabel property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getQueryLabel() {
	if (ivjQueryLabel == null) {
		try {
			ivjQueryLabel = new javax.swing.JLabel();
			ivjQueryLabel.setName("QueryLabel");
			ivjQueryLabel.setText("Relation:");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjQueryLabel;
}
/**
 * Return the QueryPanel property value.
 * @return redesign.gui.components.ExpressionEditorPanel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private ExpressionEditorPanel getQueryPanel() {
	if (ivjQueryPanel == null) {
		try {
		    String[] types = {"Text Holder"};
		    ivjQueryPanel = new edu.isi.powerloom.gui.components.ExpressionEditorPanel(this, "Extension Frame Input Field", Arrays.asList(types));
			ivjQueryPanel.setName("QueryPanel");
			ivjQueryPanel.setBounds(0, 0, 485, 10);
			/*
			ivjQueryPanel.setPreferredSize(new java.awt.Dimension(485, 10));
			ivjQueryPanel.setMaximumSize(new java.awt.Dimension(485, 10));
			*/
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjQueryPanel;
}
    public void setRelation(PLObject relation) {
	getQueryPanel().setText(relation.getID());
    }

/**
 * Return the ResultGroupPanel property value.
 * @return javax.swing.JPanel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JPanel getResultGroupPanel() {
	if (ivjResultGroupPanel == null) {
		try {
			ivjResultGroupPanel = new javax.swing.JPanel();
			ivjResultGroupPanel.setName("ResultGroupPanel");
			ivjResultGroupPanel.setLayout(new java.awt.BorderLayout());
			getResultGroupPanel().add(getResultsLabel(), "North");
			getResultGroupPanel().add(getJScrollPane2(), "Center");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjResultGroupPanel;
}
/**
 * Return the ResultsLabel property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getResultsLabel() {
	if (ivjResultsLabel == null) {
		try {
			ivjResultsLabel = new javax.swing.JLabel();
			ivjResultsLabel.setName("ResultsLabel");
			ivjResultsLabel.setText("Results:");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjResultsLabel;
}

    void navigateToSelection() {
	try {
	    int rowIndex = getResultsTable().getSelectedRow();
	    int colIndex = getResultsTable().getSelectedColumn();
	    if ((rowIndex > -1) && (colIndex > -1)) {
		TableModel tableModel = getResultsTable().getModel();
		if (tableModel.getValueAt(rowIndex, colIndex) instanceof PLObject) {
		    PLObject object = (PLObject)tableModel.getValueAt(rowIndex, colIndex);
		    debugPrintln(3, "You selected: " + object + ", row = "+ rowIndex + ", col = " + colIndex);
		    PLModule module = (PLModule)getModuleComboBox().getSelectedItem();
		    debugPrintln(3, "module = " + module);
		    getQueryBrowser().getPubBrowserPanel().getPowerloomTrees().makeObjectVisible(module, object, true);
		}
	    }
	}  catch (Exception e) {
	    handleException(e);
	}
    }


    BrowserFrame4 getQueryBrowser() {
	if (queryBrowser == null) {
	    getApp().browseMenuItem_ActionPerformed(null);
	    queryBrowser = getApp().getSelectedBrowserFrame();
	    /*
	    try {
		//Thread.sleep(1000);
		// make sure browser is displayed before other events happen, e.g., selection.
	    } catch (Exception e) {
		e.printStackTrace();
	    }
	    */
	}
	return queryBrowser;
    }

/**
 * Return the ResultsTable property value.
 * @return javax.swing.JTable
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTable getResultsTable() {
	if (ivjResultsTable == null) {
		try {
			ivjResultsTable = new javax.swing.JTable();
			ivjResultsTable.setName("ResultsTable");
			getJScrollPane2().setColumnHeaderView(ivjResultsTable.getTableHeader());
			getJScrollPane2().getViewport().setBackingStoreEnabled(true);
			ivjResultsTable.setBounds(0, 0, 200, 200);
			// user code begin {1}
			ivjResultsTable.setRowSelectionAllowed(true);
			ivjResultsTable.setColumnSelectionAllowed(true); 
			/*
			TableColumnModel columnModel = ivjResultsTable.getColumnModel();
			int columnCount = columnModel.getColumnCount();
			for (int i = 0; i < columnCount; i++) {
			    TableColumn tc = columnModel.getColumn(i);
			    tc.setCellEditor(new ExtensionCellEditor(this));
			}
			*/
			//ivjResultsTable.setDefaultEditor(Object.class, null);
			String[] types = {"Text Holder"};
			ivjResultsTable.setDefaultEditor(Object.class, new ExtensionCellEditor(this, "Extension Cell Editor", Arrays.asList(types)));
			//ivjResultsTable.setCellEditor(new ExtensionCellEditor(this));
			ivjResultsTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
			ListSelectionListener selectionListener = 
			    (new ListSelectionListener() { 
				    public void valueChanged(ListSelectionEvent lse) {
					debugPrintln(3, "Selection event: " + lse); 
					if (!lse.getValueIsAdjusting()) {
					    navigateToSelection();
					}
				    }
				});
			ivjResultsTable.getSelectionModel().addListSelectionListener(selectionListener);
			ivjResultsTable.getTableHeader().addMouseListener(new MouseListener() {
				public void mousePressed(MouseEvent e) { sortItems(e); }
				public void mouseClicked(MouseEvent e) {}
				public void mouseEntered(MouseEvent e) {}
				public void mouseExited(MouseEvent e) {}
				public void mouseReleased(MouseEvent e) {}
				private void sortItems(MouseEvent e) {
				    TableColumnModel colModel = (TableColumnModel)ivjResultsTable.getColumnModel();
				    int colIndex = colModel.getColumnIndexAtX(e.getX());
				    int modelIndex = ivjResultsTable.getColumnModel().getColumn(colIndex).getModelIndex();
				    debugPrintln(3, "You selected col #" + colIndex + ", modelindex = " + modelIndex);
				    ((ExtensionTableModel)ivjResultsTable.getModel()).sortColumn(modelIndex);
				    
				}
			    });
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjResultsTable;
}
/**
 * Return the VariableAdderPanel property value.
 * @return redesign.gui.components.TableAdderPanel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private TableAdderPanel getVariableAdderPanel() {
	if (ivjVariableAdderPanel == null) {
		try {
			ivjVariableAdderPanel = new edu.isi.powerloom.gui.components.TableAdderPanel();
			ivjVariableAdderPanel.setName("VariableAdderPanel");
			ivjVariableAdderPanel.setPreferredSize(new java.awt.Dimension(44, 50));
			ivjVariableAdderPanel.setText("VariableAdderPanel");
			ivjVariableAdderPanel.setMinimumSize(new java.awt.Dimension(44, 80));
			// user code begin {1}
			Object[] columnNames = {"Variable", "Type"};
			javax.swing.JTable table = new javax.swing.JTable();
			table.setModel(new javax.swing.table.DefaultTableModel(columnNames, 0));
			ivjVariableAdderPanel.setTable(table);
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjVariableAdderPanel;
}
/**
 * Return the VariableGroupPanel property value.
 * @return javax.swing.JPanel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JPanel getVariableGroupPanel() {
	if (ivjVariableGroupPanel == null) {
		try {
			ivjVariableGroupPanel = new javax.swing.JPanel();
			ivjVariableGroupPanel.setName("VariableGroupPanel");
			ivjVariableGroupPanel.setLayout(new java.awt.BorderLayout());
			getVariableGroupPanel().add(getJLabel1(), "North");
			getVariableGroupPanel().add(getVariableAdderPanel(), "Center");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjVariableGroupPanel;
}
/**
 * Return the VariableQueryGroupPanel property value.
 * @return javax.swing.JPanel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JPanel getVariableQueryResultGroupPanel() {
	if (ivjVariableQueryResultGroupPanel == null) {
		try {
			ivjVariableQueryResultGroupPanel = new javax.swing.JPanel();
			ivjVariableQueryResultGroupPanel.setName("VariableQueryResultGroupPanel");
			ivjVariableQueryResultGroupPanel.setLayout(getVariableQueryResultGroupPanelBoxLayout());
			//getVariableQueryResultGroupPanel().add(getVariableGroupPanel(), getVariableGroupPanel().getName());
			getVariableQueryResultGroupPanel().add(getQueryGroupPanel(), getQueryGroupPanel().getName());
			getVariableQueryResultGroupPanel().add(getModulePanel(), getModulePanel().getName());
			getVariableQueryResultGroupPanel().add(getResultGroupPanel(), getResultGroupPanel().getName());
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjVariableQueryResultGroupPanel;
}
/**
 * Return the VariableQueryResultGroupPanelBoxLayout property value.
 * @return javax.swing.BoxLayout
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.BoxLayout getVariableQueryResultGroupPanelBoxLayout() {
	javax.swing.BoxLayout ivjVariableQueryResultGroupPanelBoxLayout = null;
	try {
		/* Create part */
		ivjVariableQueryResultGroupPanelBoxLayout = new javax.swing.BoxLayout(getVariableQueryResultGroupPanel(), javax.swing.BoxLayout.Y_AXIS);
	} catch (java.lang.Throwable ivjExc) {
		handleException(ivjExc);
	};
	return ivjVariableQueryResultGroupPanelBoxLayout;
}
/**
 * Insert the method's description here.
 * Creation date: (5/23/2002 4:44:59 PM)
 * @return java.lang.String[]
 */
private String[] getVariables(PLModule module, String relationName) {
     String[] result = null;
     try {
	 relation = KnowledgeManager.getInstance().getRelationObject(module, relationName);
	 if (relation == null) {
	     JOptionPane.showMessageDialog(PowerloomApp.getInstance(), "There is no such relation named " + relationName + " in module " + module.getID(), "No such Relation", JOptionPane.WARNING_MESSAGE);
	     return null;
	 }
	 PLVariableList vars = KnowledgeManager.getInstance().getVariablesForRelation(module, relation);
	 result = new String[vars.elemPLVariable.size()];
	 for (int i = 0; i < result.length; i++) {
	     result[i] = ((PLVariable)((List)vars.elemPLVariable).get(i)).elemPLString.getValue();
	 }
     } catch (Exception e) {
	 handleException(e);
     }
     return result;
}

/**
 * Called whenever the part throws an exception.
 * @param exception java.lang.Throwable
 */
private void handleException(java.lang.Throwable exception) {
     PowerloomApp.getInstance().handleException(exception);
}
/**
 * Initializes connections
 * @exception java.lang.Exception The exception description.
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void initConnections() throws java.lang.Exception {
	// user code begin {1}
	// user code end
	getVariableAdderPanel().addActionListener(ivjEventHandler);
	getCancelButton().addActionListener(ivjEventHandler);
	getExecuteButton().addActionListener(ivjEventHandler);
	getCommitButton().addActionListener(ivjEventHandler);
}
/**
 * Initialize the class.
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void initialize() {
	try {
		// user code begin {1}
		debugPrintln(3, "setting focus listener: ");
		addInternalFrameListener(new InternalFrameAdapter() {
		       public void internalFrameActivated(InternalFrameEvent e) {
			   debugPrintln(3, "activated component: " + hashCode());
			   PowerloomApp.getInstance().setSelectedActionComponent(ExtensionFrame.this);
		       }
		       public void internalFrameOpened(InternalFrameEvent e) {
			   debugPrintln(3, "opeened component: " + hashCode());
			   PowerloomApp.getInstance().setSelectedActionComponent(ExtensionFrame.this);
		       }
		    });
		app = PowerloomApp.getInstance();
		edu.isi.powerloom.gui.xmlobject.PLSurrogateContainer modules = 
		    KnowledgeManager.getInstance().getModules().listifyTreeContainer();
		PLListModel model = new PLListModel(getModule(), modules);
		getModuleComboBox().setModel(model);
		getModuleComboBox().addItemListener(new ItemListener () {
			public void itemStateChanged(ItemEvent e) {
			    getQueryPanel().setModule((PLModule)getModuleComboBox().getSelectedItem());
			}
		    });

		PLModule selectedModule = getApp().getMostRecentlyTouchedModule();
		if (selectedModule != null) {
			getModuleComboBox().setSelectedItem(selectedModule);
		}
		PLModule module = (PLModule)getModuleComboBox().getSelectedItem();
		getQueryPanel().setModule(module);
		debugPrintln(3, "set module in query panel to: " + getQueryPanel().getModule());
		// user code end
		setName("ExtensionFrame");
		setTitle("Edit Extension");
		setIconifiable(true);
		setClosable(true);
		setMaximum(false);
		setSize(495, 396);
		setMaximizable(true);
		setResizable(true);
		setContentPane(getJInternalFrameContentPane());
		initConnections();
	} catch (java.lang.Throwable ivjExc) {
		handleException(ivjExc);
	}
	// user code begin {2}
	// user code end
}
/**
 * main entrypoint - starts the part when it is run as an application
 * @param args java.lang.String[]
 */
public static void main(java.lang.String[] args) {
	try {
		javax.swing.JFrame frame = new javax.swing.JFrame();
		ExtensionFrame aExtensionFrame;
		aExtensionFrame = new ExtensionFrame();
		frame.setContentPane(aExtensionFrame);
		frame.setSize(aExtensionFrame.getSize());
		frame.addWindowListener(new java.awt.event.WindowAdapter() {
			public void windowClosing(java.awt.event.WindowEvent e) {
				exitGui(0);
			};
		});
		frame.show();
		java.awt.Insets insets = frame.getInsets();
		frame.setSize(frame.getWidth() + insets.left + insets.right, frame.getHeight() + insets.top + insets.bottom);
		frame.setVisible(true);
	} catch (Throwable exception) {
		System.err.println("Exception occurred in main() of javax.swing.JInternalFrame");
		exception.printStackTrace(System.out);
	}
}


    public PLModule getModule() {
	return ((PLModule)getModuleComboBox().getSelectedItem());
    }

/**
 * Comment
 */
public void variableAdderPanel_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	if (actionEvent.getActionCommand().equals("add")) {
	    try {
		debugPrintln(3, "add button pressed.");
		VariableChooserDialog chooser = new VariableChooserDialog();
		chooser.setModal(true);
		chooser.setTitle("Choose variable");
		chooser.getChooserPanel().getEnterLabel().setText("Enter Concept Name:");
		PLModule module = (PLModule)getModuleComboBox().getSelectedItem();
		chooser.setModule(module);
		chooser.show();
		if ((chooser.getChooserPanel().getVariableResult() != null)) {
		    Object[] row = new Object[2];
		    row[0] = chooser.getChooserPanel().getVariableResult();
		    if (chooser.getChooserPanel().getConceptResult() != null) {
				PLSurrogate surrogate = KnowledgeManager.getInstance().
			    findOrCreateSurrogate(PLConcept.class, chooser.getChooserPanel().getConceptResult().getID());
				row[1] = surrogate.getID();
		    }
		    getVariableAdderPanel().addRow(row);
		    debugPrintln(3, "adding row: " + row[0] + ", " + row[1]); 
		}
	    } catch (Exception e) {
		handleException(e);
	    } // end of try-catch
	} else if (actionEvent.getActionCommand().equals("delete")) {
		debugPrintln(3, "delete button pressed.");
		getVariableAdderPanel().removeSelectedRow();
	} else {
		debugPrintln(3, "Uknown action command: " + actionEvent.getActionCommand());
	}
}
    // implemention of ActionComponent interface
	public void doAction() {
	executeButton_ActionPerformed(null);
    }

    public Icon getFrameIcon() {
	return PowerloomApp.getInstance().getImage("resources/images/edit-extension.gif");
    }

}
