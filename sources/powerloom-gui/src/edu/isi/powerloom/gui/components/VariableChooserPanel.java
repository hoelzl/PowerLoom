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


// Version: VariableChooserPanel.java,v 1.7 2010/02/04 05:19:47 hans Exp

package edu.isi.powerloom.gui.components;

import java.util.*;
import java.beans.*;
import javax.swing.*;
import javax.swing.event.*;
import edu.isi.powerloom.gui.xmlobject.*;
import java.awt.event.*;
import edu.isi.powerloom.gui.serverinterface.*;
import static edu.isi.powerloom.gui.common.Utils.*;

/**
 * Panel containing components for VariableChooserDialog
 *
 * @since 4/2/2002 6:17:10 PM
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 * @see edu.isi.powerloom.gui.components.VariableChooserDialog VariableChooserDialog
 */
public class VariableChooserPanel extends JPanel {
    private JLabel ivjEnterLabel = null;
    private JList ivjList = null;
    private JPanel ivjListBorderPanel = null;
    private JScrollPane ivjListScrollPane = null;
    private JPanel ivjNameGroupPanel = null;
    private JTextField ivjNameTextField = null;
    private JButton ivjOKButton = null;
    private ChooserDialog dialog;
    IvjEventHandler ivjEventHandler = new IvjEventHandler();
    private JPanel ivjButtonPanel = null;
    private JButton ivjCancelButton = null;
    private JComboBox ivjJComboBox1 = null;
    private JLabel ivjModuleLabel = null;
    private VerticalStrut ivjVerticalStrut1 = null;
    private JPanel ivjInputPanel = null;
    private BoxLayout ivjInputPanelBoxLayout = null;
    private JPanel ivjLabelPanel = null;
    private BoxLayout ivjLabelPanelBoxLayout = null;
    private VerticalStrut ivjVerticalStrut11 = null;
    private edu.isi.powerloom.gui.xmlobject.PLSurrogateContainer allItems;
    private edu.isi.powerloom.gui.xmlobject.PLConcept ConceptResult;
    private edu.isi.powerloom.gui.xmlobject.PLConcept conceptResult;
    private edu.isi.powerloom.gui.xmlobject.PLModule module;
    private JLabel ivjVariableLabel = null;
    private JTextField ivjVariableTextField = null;
    private VerticalStrut ivjVerticalStrut111 = null;
    private VerticalStrut ivjVerticalStrut12 = null;
    private java.lang.String variableResult;

    class IvjEventHandler implements java.awt.event.ActionListener {
	public void actionPerformed(java.awt.event.ActionEvent e) {
	    if (e.getSource() == VariableChooserPanel.this.getOKButton()) 
		connEtoC1(e);
	    if (e.getSource() == VariableChooserPanel.this.getCancelButton()) 
		connEtoC2(e);
	};
    };
    /**
     * ChooserPanel constructor comment.
     */
    public VariableChooserPanel() {
	super();
	initialize();
    }
    /**
     * ChooserPanel constructor comment.
     * @param layout java.awt.LayoutManager
     */
    public VariableChooserPanel(java.awt.LayoutManager layout) {
	super(layout);
    }
    /**
     * ChooserPanel constructor comment.
     * @param layout java.awt.LayoutManager
     * @param isDoubleBuffered boolean
     */
    public VariableChooserPanel(java.awt.LayoutManager layout, boolean isDoubleBuffered) {
	super(layout, isDoubleBuffered);
    }
    /**
     * ChooserPanel constructor comment.
     * @param isDoubleBuffered boolean
     */
    public VariableChooserPanel(boolean isDoubleBuffered) {
	super(isDoubleBuffered);
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/12/2002 12:20:26 PM)
     * @param actionEvent java.awt.event.ActionEvent
     */
    public void cancelButton_ActionPerformed(ActionEvent actionEvent) {
	/*
	  setConceptResult(null);
	  setVariableResult(null);
	  if (getDialog() != null) {
	  dialog.hide();
	  dialog.setVisible(false);
	  } else {
	  debugPrintln(3, "error::: dialog = " + dialog);
	  }
	*/
    }
    /**
     * Comment
     */
    public void cancelButton_ActionPerformed1(java.awt.event.ActionEvent actionEvent) {
	// experimental: try focusing component
	/*
	  debugPrintln(3, "execing cancelbutton");
	  getNameTextField().requestFocus();
	*/
	if (getDialog() != null) {
	    dialog.hide();
	    dialog.setVisible(false);
	} else {
	    debugPrintln(3, "error::: dialog = " + dialog);
	}
    }
    /**
     * connEtoC1:  (OKButton.action.actionPerformed(java.awt.event.ActionEvent) --> ChooserPanel.oKButton_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC1(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.oKButton_ActionPerformed(arg1);
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/12/2002 12:16:03 PM)
     * @param arg1 java.awt.event.ActionEvent
     */
    private void connEtoC2(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.cancelButton_ActionPerformed1(arg1);
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/11/2002 12:41:07 PM)
     * @return edu.isi.powerloom.gui.xmlobject.PLSurrogateContainer
     */
    private edu.isi.powerloom.gui.xmlobject.PLSurrogateContainer getAllItems() {
	return allItems;
    }
    /**
     * Return the SearchPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getButtonPanel() {
	if (ivjButtonPanel == null) {
	    try {
		ivjButtonPanel = new javax.swing.JPanel();
		ivjButtonPanel.setName("ButtonPanel");
		ivjButtonPanel.setPreferredSize(new java.awt.Dimension(10, 35));
		ivjButtonPanel.setLayout(new java.awt.FlowLayout());
		ivjButtonPanel.setMaximumSize(new java.awt.Dimension(32767, 32767));
		getButtonPanel().add(getOKButton(), getOKButton().getName());
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
     * Return the SearchButton property value.
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
     * Insert the method's description here.
     * Creation date: (4/11/2002 1:50:14 PM)
     * @return edu.isi.powerloom.gui.xmlobject.PLObject
     */
    public edu.isi.powerloom.gui.xmlobject.PLConcept getConceptResult() {
	return conceptResult;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/2/2002 9:19:35 PM)
     * @return javax.swing.JDialog
     */
    public ChooserDialog getDialog() {
	return dialog;
    }
    /**
     * Return the EnterLabel property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    public javax.swing.JLabel getEnterLabel() {
	if (ivjEnterLabel == null) {
	    try {
		ivjEnterLabel = new javax.swing.JLabel();
		ivjEnterLabel.setName("EnterLabel");
		ivjEnterLabel.setAlignmentY(java.awt.Component.TOP_ALIGNMENT);
		ivjEnterLabel.setText("Object Name:");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjEnterLabel;
    }
    /**
     * Return the InputPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getInputPanel() {
	if (ivjInputPanel == null) {
	    try {
		ivjInputPanel = new javax.swing.JPanel();
		ivjInputPanel.setName("InputPanel");
		ivjInputPanel.setLayout(getInputPanelBoxLayout());
		getInputPanel().add(getVariableTextField(), getVariableTextField().getName());
		getInputPanel().add(getVerticalStrut111(), getVerticalStrut111().getName());
		getInputPanel().add(getNameTextField(), getNameTextField().getName());
		getInputPanel().add(getVerticalStrut11(), getVerticalStrut11().getName());
		getInputPanel().add(getJComboBox1(), getJComboBox1().getName());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjInputPanel;
    }
    /**
     * Return the InputPanelBoxLayout property value.
     * @return javax.swing.BoxLayout
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.BoxLayout getInputPanelBoxLayout() {
	javax.swing.BoxLayout ivjInputPanelBoxLayout = null;
	try {
	    /* Create part */
	    ivjInputPanelBoxLayout = new javax.swing.BoxLayout(getInputPanel(), javax.swing.BoxLayout.Y_AXIS);
	} catch (java.lang.Throwable ivjExc) {
	    handleException(ivjExc);
	};
	return ivjInputPanelBoxLayout;
    }
    /**
     * Return the JComboBox1 property value.
     * @return javax.swing.JComboBox
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JComboBox getJComboBox1() {
	if (ivjJComboBox1 == null) {
	    try {
		ivjJComboBox1 = new javax.swing.JComboBox();
		ivjJComboBox1.setName("JComboBox1");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjJComboBox1;
    }
    /**
     * Return the LabelPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getLabelPanel() {
	if (ivjLabelPanel == null) {
	    try {
		ivjLabelPanel = new javax.swing.JPanel();
		ivjLabelPanel.setName("LabelPanel");
		ivjLabelPanel.setLayout(getLabelPanelBoxLayout());
		ivjLabelPanel.setMaximumSize(new java.awt.Dimension(76, 50));
		ivjLabelPanel.setMinimumSize(new java.awt.Dimension(76, 50));
		getLabelPanel().add(getVariableLabel(), getVariableLabel().getName());
		getLabelPanel().add(getVerticalStrut12(), getVerticalStrut12().getName());
		getLabelPanel().add(getEnterLabel(), getEnterLabel().getName());
		getLabelPanel().add(getVerticalStrut1(), getVerticalStrut1().getName());
		getLabelPanel().add(getModuleLabel(), getModuleLabel().getName());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjLabelPanel;
    }
    /**
     * Return the LabelPanelBoxLayout property value.
     * @return javax.swing.BoxLayout
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.BoxLayout getLabelPanelBoxLayout() {
	javax.swing.BoxLayout ivjLabelPanelBoxLayout = null;
	try {
	    /* Create part */
	    ivjLabelPanelBoxLayout = new javax.swing.BoxLayout(getLabelPanel(), javax.swing.BoxLayout.Y_AXIS);
	} catch (java.lang.Throwable ivjExc) {
	    handleException(ivjExc);
	};
	return ivjLabelPanelBoxLayout;
    }
    /**
     * Return the List property value.
     * @return javax.swing.JList
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JList getList() {
	if (ivjList == null) {
	    try {
		ivjList = new javax.swing.JList();
		ivjList.setName("List");
		ivjList.setBounds(0, 0, 160, 120);
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjList;
    }
    /**
     * Return the ListBorderPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getListBorderPanel() {
	if (ivjListBorderPanel == null) {
	    try {
		ivjListBorderPanel = new javax.swing.JPanel();
		ivjListBorderPanel.setName("ListBorderPanel");
		ivjListBorderPanel.setBorder(new javax.swing.border.EmptyBorder(10,10,10,10));
		ivjListBorderPanel.setLayout(new java.awt.BorderLayout());
		getListBorderPanel().add(getListScrollPane(), "Center");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjListBorderPanel;
    }
    /**
     * Return the ListScrollPane property value.
     * @return javax.swing.JScrollPane
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JScrollPane getListScrollPane() {
	if (ivjListScrollPane == null) {
	    try {
		ivjListScrollPane = new javax.swing.JScrollPane();
		ivjListScrollPane.setName("ListScrollPane");
		getListScrollPane().setViewportView(getList());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjListScrollPane;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/12/2002 12:13:41 PM)
     * @return edu.isi.powerloom.gui.xmlobject.PLModule
     */
    public edu.isi.powerloom.gui.xmlobject.PLModule getModule() {
	return module;
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
		ivjModuleLabel.setComponentOrientation(java.awt.ComponentOrientation.RIGHT_TO_LEFT);
		ivjModuleLabel.setHorizontalTextPosition(javax.swing.SwingConstants.RIGHT);
		ivjModuleLabel.setAlignmentX(java.awt.Component.LEFT_ALIGNMENT);
		ivjModuleLabel.setHorizontalAlignment(javax.swing.SwingConstants.RIGHT);
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
     * Return the NameGroupPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getNameGroupPanel() {
	if (ivjNameGroupPanel == null) {
	    try {
		ivjNameGroupPanel = new javax.swing.JPanel();
		ivjNameGroupPanel.setName("NameGroupPanel");
		ivjNameGroupPanel.setBorder(new javax.swing.border.EmptyBorder(10,10,0,10));
		ivjNameGroupPanel.setLayout(new java.awt.BorderLayout());
		ivjNameGroupPanel.setMaximumSize(new java.awt.Dimension(2147483647, 50));
		getNameGroupPanel().add(getLabelPanel(), "West");
		getNameGroupPanel().add(getInputPanel(), "Center");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjNameGroupPanel;
    }
    /**
     * Return the NameGroupPanelBoxLayout property value.
     * @return javax.swing.BoxLayout
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.BoxLayout getNameGroupPanelBoxLayout() {
	javax.swing.BoxLayout ivjNameGroupPanelBoxLayout = null;
	try {
	    /* Create part */
	    ivjNameGroupPanelBoxLayout = new javax.swing.BoxLayout(getNameGroupPanel(), javax.swing.BoxLayout.X_AXIS);
	} catch (java.lang.Throwable ivjExc) {
	    handleException(ivjExc);
	};
	return ivjNameGroupPanelBoxLayout;
    }
    /**
     * Return the NameTextField property value.
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    public javax.swing.JTextField getNameTextField() {
	if (ivjNameTextField == null) {
	    try {
		ivjNameTextField = new javax.swing.JTextField();
		ivjNameTextField.setName("NameTextField");
		ivjNameTextField.setMaximumSize(new java.awt.Dimension(2147483647, 20));
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjNameTextField;
    }
    /**
     * Return the OKButton property value.
     * @return javax.swing.JButton
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JButton getOKButton() {
	if (ivjOKButton == null) {
	    try {
		ivjOKButton = new javax.swing.JButton();
		ivjOKButton.setName("OKButton");
		ivjOKButton.setText("OK");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjOKButton;
    }
    /**
     * Return the VariableLabel property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getVariableLabel() {
	if (ivjVariableLabel == null) {
	    try {
		ivjVariableLabel = new javax.swing.JLabel();
		ivjVariableLabel.setName("VariableLabel");
		ivjVariableLabel.setText("Variable Name:");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjVariableLabel;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/14/2002 9:03:08 PM)
     * @return java.lang.String
     */
    public java.lang.String getVariableResult() {
	return variableResult;
    }
    /**
     * Return the VariableTextField property value.
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    public javax.swing.JTextField getVariableTextField() {
	if (ivjVariableTextField == null) {
	    try {
		ivjVariableTextField = new javax.swing.JTextField();
		ivjVariableTextField.setName("VariableTextField");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjVariableTextField;
    }
    /**
     * Return the VerticalStrut1 property value.
     * @return redesign.gui.components.VerticalStrut
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private VerticalStrut getVerticalStrut1() {
	if (ivjVerticalStrut1 == null) {
	    try {
		ivjVerticalStrut1 = new edu.isi.powerloom.gui.components.VerticalStrut();
		ivjVerticalStrut1.setName("VerticalStrut1");
		ivjVerticalStrut1.setStrutSize(13);
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjVerticalStrut1;
    }
    /**
     * Return the VerticalStrut11 property value.
     * @return redesign.gui.components.VerticalStrut
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private VerticalStrut getVerticalStrut11() {
	if (ivjVerticalStrut11 == null) {
	    try {
		ivjVerticalStrut11 = new edu.isi.powerloom.gui.components.VerticalStrut();
		ivjVerticalStrut11.setName("VerticalStrut11");
		ivjVerticalStrut11.setStrutSize(5);
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjVerticalStrut11;
    }
    /**
     * Return the VerticalStrut111 property value.
     * @return redesign.gui.components.VerticalStrut
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private VerticalStrut getVerticalStrut111() {
	if (ivjVerticalStrut111 == null) {
	    try {
		ivjVerticalStrut111 = new edu.isi.powerloom.gui.components.VerticalStrut();
		ivjVerticalStrut111.setName("VerticalStrut111");
		ivjVerticalStrut111.setStrutSize(5);
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjVerticalStrut111;
    }
    /**
     * Return the VerticalStrut12 property value.
     * @return redesign.gui.components.VerticalStrut
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private VerticalStrut getVerticalStrut12() {
	if (ivjVerticalStrut12 == null) {
	    try {
		ivjVerticalStrut12 = new edu.isi.powerloom.gui.components.VerticalStrut();
		ivjVerticalStrut12.setName("VerticalStrut12");
		ivjVerticalStrut12.setStrutSize(13);
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjVerticalStrut12;
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
	getOKButton().addActionListener(ivjEventHandler);
	getCancelButton().addActionListener(ivjEventHandler);
    }
    /**
     * Initialize the class.
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void initialize() {
	try {
	    // user code begin {1}
	    // user code end
	    setName("ChooserPanel");
	    setLayout(new java.awt.BorderLayout());
	    setSize(288, 419);
	    add(getNameGroupPanel(), "North");
	    add(getListBorderPanel(), "Center");
	    add(getButtonPanel(), "South");
	    initConnections();
	} catch (java.lang.Throwable ivjExc) {
	    handleException(ivjExc);
	}
	// user code begin {2}
	// setup listener for textfield typed text
	debugPrintln(3, "setting up prop listener...");
	getNameTextField().addPropertyChangeListener(new PropertyChangeListener() {
		public void propertyChange(PropertyChangeEvent e) {
		    debugPrintln(3, "detected prop change: " + e);
		    if (e.getPropertyName().equals("text")) {
			debugPrintln(3, "new text is: " + e.getNewValue());
		    }
		}
	    });
	getNameTextField().addCaretListener(new CaretListener() {
		public void caretUpdate(CaretEvent e) {
		    debugPrintln(3, "detected caret change: ");
		    debugPrintln(3, "text = " + getNameTextField().getText());
		    updateList(getNameTextField().getText());
		}});

	getNameTextField().addActionListener(new AbstractAction() {
		public void actionPerformed(ActionEvent e) {
		    debugPrintln(3, "You hit return!");
		    oKButton_ActionPerformed(e);
		}
	    });
	// for testing: initially populate w/ a bunch of dummy surrogates
	List surrogates = new ArrayList();
	//String[] names = {"outmoded", "outofdate", "outpost", "output", "outrage", "prostrate", "prosy", "protect", "protection", "protege"};
	String[] names = {};
	for (int i = 0; i < names.length; i++) {
	    PLConcept con = new PLConcept();
	    con.attrConceptName = names[i];
	    PLSurrogate surrogate = new PLSurrogate(names[i]);
	    surrogate.setValue(con);
	    surrogates.add(surrogate);
	}
	
	PLSurrogateContainer container = new PLSurrogateContainer(surrogates);
	setAllItems(null, container);

	// setup modules combobox
	try {
	    edu.isi.powerloom.gui.xmlobject.PLSurrogateContainer modules = 
		KnowledgeManager.getInstance().getModules().listifyTreeContainer();
	    PLListModel model = new PLListModel(modules);
	    // this should be called moduleComboBox
	    getJComboBox1().setModel(model);
	    getJComboBox1().addActionListener(new ActionListener() {
		    public void actionPerformed (ActionEvent e) {
			//debugPrintln(3, "combobox selection: " + e);
			PLModule module = (PLModule)getJComboBox1().getSelectedItem();
			debugPrintln(3, "selected module: " + module);
			if (module != null) {
			    // this retrieves new surrogates for the module, and updates the list.
			    getDialog().setModule(module);
			}
			    
		    }
		});
	} catch (Exception e) {
	    handleException(e);
	} 


	// user code end
    }
    /**
     * @return true if 'candidate' is matched by 'matchString'
     */
    // eventually this will handle regular expressions
    private boolean isMatch(String candidate, String matchString) {
	return (candidate.toUpperCase().startsWith(matchString.toUpperCase()));
    }
    /**
     * main entrypoint - starts the part when it is run as an application
     * @param args java.lang.String[]
     */
    public static void main(java.lang.String[] args) {
	try {
	    javax.swing.JFrame frame = new javax.swing.JFrame();
	    ChooserPanel aChooserPanel;
	    aChooserPanel = new ChooserPanel();
	    frame.setContentPane(aChooserPanel);
	    frame.setSize(aChooserPanel.getSize());
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
	    System.err.println("Exception occurred in main() of javax.swing.JPanel");
	    exception.printStackTrace(System.out);
	}
    }
    /**
     * Comment
     */
    public void oKButton_ActionEvents() {
	if (getDialog() != null) {
	    dialog.hide();
	    dialog.dispose();
	}
    }
    /**
     * Comment
     */
    public void oKButton_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	setConceptResult((PLConcept)getList().getSelectedValue());
	setVariableResult(getVariableTextField().getText());
	if (getDialog() != null) {
	    dialog.hide();
	    dialog.setVisible(false);
	} else {
	    debugPrintln(3, "error::: dialog = " + dialog);
	}
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/11/2002 12:41:07 PM)
     * @param newAllItems edu.isi.powerloom.gui.xmlobject.PLSurrogateContainer
     */
    public void setAllItems(PLModule module, edu.isi.powerloom.gui.xmlobject.PLSurrogateContainer newAllItems) {
	module = module;
	allItems = newAllItems;
	//create a list with all items
	updateList("");
	if (module != null) {
	    if (getJComboBox1().getSelectedItem() != module) {
		getJComboBox1().setSelectedItem(module);
	    }
	}
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/11/2002 1:50:14 PM)
     * @param newResult edu.isi.powerloom.gui.xmlobject.PLObject
     */
    private void setConceptResult(edu.isi.powerloom.gui.xmlobject.PLConcept newResult) {
	conceptResult = newResult;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/2/2002 9:19:35 PM)
     * @param newDialog javax.swing.JDialog
     */
    public void setDialog(ChooserDialog newDialog) {
	dialog = newDialog;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/11/2002 12:43:40 PM)
     * @param list javax.swing.JList
     */
    void setList(JList list) {
	ivjList = list;
	getListScrollPane().setViewportView(list);
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/12/2002 12:13:41 PM)
     * @param newModule edu.isi.powerloom.gui.xmlobject.PLModule
     */
    public void setModule(edu.isi.powerloom.gui.xmlobject.PLModule newModule) {
	module = newModule;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/14/2002 9:03:08 PM)
     * @param newVariableResult java.lang.String
     */
    public void setVariableResult(java.lang.String newVariableResult) {
	variableResult = newVariableResult;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/11/2002 12:44:09 PM)
     * @param matchString java.lang.String
     */

    private void updateList(String matchString) {
	List matchList = new ArrayList();
	Iterator iter = getAllItems().getSurrogates().iterator();
	while (iter.hasNext()) {
	    PLSurrogate candidate = (PLSurrogate)iter.next();
	    if (isMatch(candidate.getID(), matchString)) {
		matchList.add(candidate);
	    }
	}		
	PLSurrogateContainer filteredContainer = new PLSurrogateContainer(matchList);
	PLListModel model = new PLListModel(filteredContainer);
	JList list = new JList(model);
	setList(list);
	if ((model.getSize() > 0) && (getNameTextField().getText().trim().length() > 0)) {
	    list.setSelectedIndex(0);
	}
    }
}
