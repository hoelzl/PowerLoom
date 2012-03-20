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


// Version: ConceptPanel2.java,v 1.17 2010/02/04 05:16:54 hans Exp

package edu.isi.powerloom.gui.components;

import edu.isi.powerloom.gui.xmlobject.*;
import edu.isi.powerloom.gui.serverinterface.*;
import static edu.isi.powerloom.gui.common.Utils.*;

import javax.swing.*;
import java.util.*;
import java.awt.event.*;

/**
 * Panel for Concept Editor.
 *
 * @since 4/1/2002 5:24:08 PM
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */
public class ConceptPanel2 extends JPanel {
    private JLabel ivjDocumentation = null;
    private JTextArea ivjDocumentationTextArea = null;
    private JLabel ivjRelations = null;
    private JLabel ivjSupers = null;
    private JLabel ivjConceptName = null;
    private JLabel ivjPropositions = null;
    private JLabel ivjRules = null;
    private AdderPanel2 ivjPropositionsPanel = null;
    private AdderPanel2 ivjRulePanel = null;
    private AdderPanel2 ivjSupersPanel = null;
    private BoxLayout ivjConceptPanel2BoxLayout = null;
    private JPanel ivjDocumentationGroupPanel = null;
    private JScrollPane ivjDocumentationScrollPane = null;
    private HorizontalStrut ivjHorizontalStrut1 = null;
    private JPanel ivjNameDocumentationGroupPanel = null;
    private BoxLayout ivjNameDocumentationGroupPanelBoxLayout = null;
    private JPanel ivjNameDocumentationSupersGroupPanel = null;
    private JPanel ivjNameGroupPanel = null;
    private JTextField ivjNameTextField = null;
    private JPanel ivjPropositionGroupPanel = null;
    private JPanel ivjRelationsGroupPanel = null;
    private JPanel ivjRuleGroupPanel = null;
    private JPanel ivjSupersGroupPanel = null;
    private VerticalStrut ivjVerticalStrut1 = null;
    private VerticalStrut ivjVerticalStrut2 = null;
    private VerticalStrut ivjVerticalStrut21 = null;
    private VerticalStrut ivjVerticalStrut22 = null;
    IvjEventHandler ivjEventHandler = new IvjEventHandler();
    private JPanel ivjButtonGroupPanel = null;
    private JButton ivjCancelButton = null;
    private JButton ivjOKButton = null;
    private javax.swing.table.TableColumn ivjCardinalityColumn = null;
    private javax.swing.table.TableColumn ivjNameColumn = null;
    private JTable ivjRelationTable = null;
    private javax.swing.table.TableColumn ivjTypeColumn = null;
    public ConceptFrame2 parentFrame;
    private HorizontalStrut ivjHorizontalStrut2 = null;
    private JComboBox ivjModuleComboBox = null;
    private JLabel ivjModuleLabel = null;
    private JPanel ivjModulePanel = null;
    private VerticalStrut ivjVerticalStrut11 = null;

    class IvjEventHandler implements java.awt.event.ActionListener {
	public void actionPerformed(java.awt.event.ActionEvent e) {
	    if (e.getSource() == ConceptPanel2.this.getSupersPanel()) 
		connEtoC1(e);
	    if (e.getSource() == ConceptPanel2.this.getOKButton()) 
		connEtoC2(e);
	    if (e.getSource() == ConceptPanel2.this.getCancelButton()) 
		connEtoC3(e);
	    if (e.getSource() == ConceptPanel2.this.getRelationPanel()) 
		connEtoC100(e);
	    if (e.getSource() == ConceptPanel2.this.getPropositionsPanel()) 
		propositionsPanel_ActionPerformed(e);				
	    if (e.getSource() == ConceptPanel2.this.getRulePanel()) 
		rulePanel_ActionPerformed(e);				
	};
    };
    private AdderPanel2 ivjRelationPanel = null;
    /**
     * ConceptPanel2 constructor comment.
     */
    public ConceptPanel2() {
	super();
	initialize();
    }
    public ConceptPanel2(ConceptFrame2 frame) {
	super();
	parentFrame = frame;
	initialize();
    }
    /**
     * ConceptPanel2 constructor comment.
     * @param layout java.awt.LayoutManager
     */
    public ConceptPanel2(java.awt.LayoutManager layout) {
	super(layout);
    }
    /**
     * ConceptPanel2 constructor comment.
     * @param layout java.awt.LayoutManager
     * @param isDoubleBuffered boolean
     */
    public ConceptPanel2(java.awt.LayoutManager layout, boolean isDoubleBuffered) {
	super(layout, isDoubleBuffered);
    }
    /**
     * ConceptPanel2 constructor comment.
     * @param isDoubleBuffered boolean
     */
    public ConceptPanel2(boolean isDoubleBuffered) {
	super(isDoubleBuffered);
    }
    /**
     * Comment
     */
    public void cancelButton_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	getParentFrame().doDefaultCloseAction();
    }
    /**
     * connEtoC1:  (SupersPanel.action.actionPerformed(java.awt.event.ActionEvent) --> ConceptPanel2.supersPanel_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC1(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.supersPanel_ActionPerformed(arg1);
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
     * Creation date: (4/15/2002 10:27:56 PM)
     * @param arg1 java.awt.event.ActionEvent
     */
    private void connEtoC100(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.relationPanel_ActionPerformed(arg1);
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }
    /**
     * connEtoC2:  (OKButton.action.actionPerformed(java.awt.event.ActionEvent) --> ConceptPanel2.oKButton_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC2(java.awt.event.ActionEvent arg1) {
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
     * connEtoC3:  (CancelButton.action.actionPerformed(java.awt.event.ActionEvent) --> ConceptPanel2.cancelButton_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC3(java.awt.event.ActionEvent arg1) {
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
     * Return the ButtonGroupPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getButtonGroupPanel() {
	if (ivjButtonGroupPanel == null) {
	    try {
		ivjButtonGroupPanel = new javax.swing.JPanel();
		ivjButtonGroupPanel.setName("ButtonGroupPanel");
		ivjButtonGroupPanel.setLayout(new java.awt.FlowLayout());
		getButtonGroupPanel().add(getOKButton(), getOKButton().getName());
		getButtonGroupPanel().add(getCancelButton(), getCancelButton().getName());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjButtonGroupPanel;
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
     * Return the TableColumn3 property value.
     * @return javax.swing.table.TableColumn
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.table.TableColumn getCardinalityColumn() {
	if (ivjCardinalityColumn == null) {
	    try {
		ivjCardinalityColumn = new javax.swing.table.TableColumn();
		ivjCardinalityColumn.setHeaderValue("Cardinality");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjCardinalityColumn;
    }
    /**
     * Return the Name property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getConceptName() {
	if (ivjConceptName == null) {
	    try {
		ivjConceptName = new javax.swing.JLabel();
		ivjConceptName.setName("ConceptName");
		ivjConceptName.setText("Name");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjConceptName;
    }
    /**
     * Return the ConceptPanel2BoxLayout property value.
     * @return javax.swing.BoxLayout
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.BoxLayout getConceptPanel2BoxLayout() {
	javax.swing.BoxLayout ivjConceptPanel2BoxLayout = null;
	try {
	    /* Create part */
	    ivjConceptPanel2BoxLayout = new javax.swing.BoxLayout(this, javax.swing.BoxLayout.Y_AXIS);
	} catch (java.lang.Throwable ivjExc) {
	    handleException(ivjExc);
	};
	return ivjConceptPanel2BoxLayout;
    }
    /**
     * Return the Documentation property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getDocumentation() {
	if (ivjDocumentation == null) {
	    try {
		ivjDocumentation = new javax.swing.JLabel();
		ivjDocumentation.setName("Documentation");
		ivjDocumentation.setText("Documentation");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjDocumentation;
    }
    /**
     * Return the DocumentationGroupPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getDocumentationGroupPanel() {
	if (ivjDocumentationGroupPanel == null) {
	    try {
		ivjDocumentationGroupPanel = new javax.swing.JPanel();
		ivjDocumentationGroupPanel.setName("DocumentationGroupPanel");
		ivjDocumentationGroupPanel.setLayout(new java.awt.BorderLayout());
		getDocumentationGroupPanel().add(getDocumentationScrollPane(), "Center");
		getDocumentationGroupPanel().add(getDocumentation(), "North");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjDocumentationGroupPanel;
    }
    /**
     * Return the JScrollPane1 property value.
     * @return javax.swing.JScrollPane
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JScrollPane getDocumentationScrollPane() {
	if (ivjDocumentationScrollPane == null) {
	    try {
		ivjDocumentationScrollPane = new javax.swing.JScrollPane();
		ivjDocumentationScrollPane.setName("DocumentationScrollPane");
		getDocumentationScrollPane().setViewportView(getDocumentationTextArea());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjDocumentationScrollPane;
    }
    /**
     * Return the DocumentationTextArea property value.
     * @return javax.swing.JTextArea
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    public javax.swing.JTextArea getDocumentationTextArea() {
	if (ivjDocumentationTextArea == null) {
	    try {
		String[] types = {"Text Holder"};
		ivjDocumentationTextArea = new PLJTextArea(getParentFrame(), "Concept Documentation Text Area", Arrays.asList(types));
		debugPrintln(3, "Creating doc text holder, parentframe = " + getParentFrame());
		ivjDocumentationTextArea.setName("DocumentationTextArea");
		ivjDocumentationTextArea.setLineWrap(true);
		ivjDocumentationTextArea.setWrapStyleWord(true);
		ivjDocumentationTextArea.setBounds(0, 0, 160, 120);
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjDocumentationTextArea;
    }
    /**
     * Return the HorizontalStrut1 property value.
     * @return redesign.gui.components.HorizontalStrut
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private HorizontalStrut getHorizontalStrut1() {
	if (ivjHorizontalStrut1 == null) {
	    try {
		ivjHorizontalStrut1 = new edu.isi.powerloom.gui.components.HorizontalStrut();
		ivjHorizontalStrut1.setName("HorizontalStrut1");
		ivjHorizontalStrut1.setStrutSize(30);
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjHorizontalStrut1;
    }
    /**
     * Return the HorizontalStrut2 property value.
     * @return redesign.gui.components.HorizontalStrut
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private HorizontalStrut getHorizontalStrut2() {
	if (ivjHorizontalStrut2 == null) {
	    try {
		ivjHorizontalStrut2 = new edu.isi.powerloom.gui.components.HorizontalStrut();
		ivjHorizontalStrut2.setName("HorizontalStrut2");
		ivjHorizontalStrut2.setStrutSize(5);
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjHorizontalStrut2;
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
		ivjModuleComboBox.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
			    updateModuleInLists(getModule());
			}
		    });
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
		ivjModulePanel.setLayout(new javax.swing.BoxLayout(getModulePanel(), javax.swing.BoxLayout.X_AXIS));
		getModulePanel().add(getModuleLabel(), getModuleLabel().getName());
		getModulePanel().add(getHorizontalStrut2(), getHorizontalStrut2().getName());
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
     * Return the TableColumn2 property value.
     * @return javax.swing.table.TableColumn
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.table.TableColumn getNameColumn() {
	if (ivjNameColumn == null) {
	    try {
		ivjNameColumn = new javax.swing.table.TableColumn();
		ivjNameColumn.setHeaderValue("Name");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjNameColumn;
    }
    /**
     * Return the NameDocumentationGroupPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getNameDocumentationGroupPanel() {
	if (ivjNameDocumentationGroupPanel == null) {
	    try {
		ivjNameDocumentationGroupPanel = new javax.swing.JPanel();
		ivjNameDocumentationGroupPanel.setName("NameDocumentationGroupPanel");
		ivjNameDocumentationGroupPanel.setPreferredSize(new java.awt.Dimension(250, 120));
		ivjNameDocumentationGroupPanel.setLayout(getNameDocumentationGroupPanelBoxLayout());
		getNameDocumentationGroupPanel().add(getNameGroupPanel(), getNameGroupPanel().getName());
		getNameDocumentationGroupPanel().add(getVerticalStrut1(), getVerticalStrut1().getName());
		getNameDocumentationGroupPanel().add(getDocumentationGroupPanel(), getDocumentationGroupPanel().getName());
		getNameDocumentationGroupPanel().add(getVerticalStrut11(), getVerticalStrut11().getName());
		getNameDocumentationGroupPanel().add(getModulePanel(), getModulePanel().getName());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjNameDocumentationGroupPanel;
    }
    /**
     * Return the NameDocumentationGroupPanelBoxLayout property value.
     * @return javax.swing.BoxLayout
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.BoxLayout getNameDocumentationGroupPanelBoxLayout() {
	javax.swing.BoxLayout ivjNameDocumentationGroupPanelBoxLayout = null;
	try {
	    /* Create part */
	    ivjNameDocumentationGroupPanelBoxLayout = new javax.swing.BoxLayout(getNameDocumentationGroupPanel(), javax.swing.BoxLayout.Y_AXIS);
	} catch (java.lang.Throwable ivjExc) {
	    handleException(ivjExc);
	};
	return ivjNameDocumentationGroupPanelBoxLayout;
    }
    /**
     * Return the NameDocumentationSupersGroupPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getNameDocumentationSupersGroupPanel() {
	if (ivjNameDocumentationSupersGroupPanel == null) {
	    try {
		ivjNameDocumentationSupersGroupPanel = new javax.swing.JPanel();
		ivjNameDocumentationSupersGroupPanel.setName("NameDocumentationSupersGroupPanel");
		ivjNameDocumentationSupersGroupPanel.setLayout(new javax.swing.BoxLayout(getNameDocumentationSupersGroupPanel(), javax.swing.BoxLayout.X_AXIS));
		getNameDocumentationSupersGroupPanel().add(getNameDocumentationGroupPanel(), getNameDocumentationGroupPanel().getName());
		getNameDocumentationSupersGroupPanel().add(getHorizontalStrut1(), getHorizontalStrut1().getName());
		getNameDocumentationSupersGroupPanel().add(getSupersGroupPanel(), getSupersGroupPanel().getName());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjNameDocumentationSupersGroupPanel;
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
		ivjNameGroupPanel.setLayout(new java.awt.BorderLayout());
		ivjNameGroupPanel.setMaximumSize(new java.awt.Dimension(2147483647, 5));
		getNameGroupPanel().add(getNameTextField(), "Center");
		getNameGroupPanel().add(getConceptName(), "North");
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
     * Return the JTextField1 property value.
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    public javax.swing.JTextField getNameTextField() {
	if (ivjNameTextField == null) {
	    try {
		String[] types = {"Text Holder"};
		ivjNameTextField = new PLJTextField(getParentFrame(), "Concept Name Text Field", Arrays.asList(types));
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
     * Insert the method's description here.
     * Creation date: (4/10/2002 4:07:07 PM)
     * @return redesign.gui.components.ConceptFrame2
     */
    public ConceptFrame2 getParentFrame() {
	return parentFrame;
    }
    /**
     * Return the PropositionGroupPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getPropositionGroupPanel() {
	if (ivjPropositionGroupPanel == null) {
	    try {
		ivjPropositionGroupPanel = new javax.swing.JPanel();
		ivjPropositionGroupPanel.setName("PropositionGroupPanel");
		ivjPropositionGroupPanel.setPreferredSize(new java.awt.Dimension(259, 160));
		ivjPropositionGroupPanel.setLayout(new java.awt.BorderLayout());
		getPropositionGroupPanel().add(getPropositionsPanel(), "Center");
		getPropositionGroupPanel().add(getPropositions(), "North");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjPropositionGroupPanel;
    }
    /**
     * Return the Propositions property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getPropositions() {
	if (ivjPropositions == null) {
	    try {
		ivjPropositions = new javax.swing.JLabel();
		ivjPropositions.setName("Propositions");
		ivjPropositions.setText("Propositions");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjPropositions;
    }
    /**
     * Return the PropositionsPanel property value.
     * @return redesign.gui.components.AdderPanel2
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    AdderPanel2 getPropositionsPanel() {
	if (ivjPropositionsPanel == null) {
	    try {
		ivjPropositionsPanel = new edu.isi.powerloom.gui.components.AdderPanel2(getModule());
		ivjPropositionsPanel.setName("PropositionsPanel");
		ivjPropositionsPanel.setText("PropositionsPanel");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjPropositionsPanel;
    }
    /**
     * Return the RelationPanel property value.
     * @return redesign.gui.components.AdderPanel2
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    AdderPanel2 getRelationPanel() {
	if (ivjRelationPanel == null) {
	    try {
		ivjRelationPanel = new edu.isi.powerloom.gui.components.AdderPanel2(getModule());
		ivjRelationPanel.setName("RelationPanel");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjRelationPanel;
    }
    /**
     * Return the Relations property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getRelations() {
	if (ivjRelations == null) {
	    try {
		ivjRelations = new javax.swing.JLabel();
		ivjRelations.setName("Relations");
		ivjRelations.setText("Relations");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjRelations;
    }
    /**
     * Return the RelationsGroupPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getRelationsGroupPanel() {
	if (ivjRelationsGroupPanel == null) {
	    try {
		ivjRelationsGroupPanel = new javax.swing.JPanel();
		ivjRelationsGroupPanel.setName("RelationsGroupPanel");
		ivjRelationsGroupPanel.setPreferredSize(new java.awt.Dimension(259, 170));
		ivjRelationsGroupPanel.setLayout(new java.awt.BorderLayout());
		getRelationsGroupPanel().add(getRelations(), "North");
		getRelationsGroupPanel().add(getRelationPanel(), "Center");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjRelationsGroupPanel;
    }
    /**
     * Return the ScrollPaneTable property value.
     * @return javax.swing.JTable
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTable getRelationTable() {
	if (ivjRelationTable == null) {
	    try {
		ivjRelationTable = new javax.swing.JTable();
		ivjRelationTable.setName("RelationTable");
		ivjRelationTable.setPreferredSize(new java.awt.Dimension(30, 30));
		ivjRelationTable.setBounds(20, 600, 734, 120);
		ivjRelationTable.setAutoCreateColumnsFromModel(false);
		ivjRelationTable.addColumn(getNameColumn());
		ivjRelationTable.addColumn(getTypeColumn());
		ivjRelationTable.addColumn(getCardinalityColumn());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjRelationTable;
    }
    /**
     * Return the RuleGroupPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getRuleGroupPanel() {
	if (ivjRuleGroupPanel == null) {
	    try {
		ivjRuleGroupPanel = new javax.swing.JPanel();
		ivjRuleGroupPanel.setName("RuleGroupPanel");
		ivjRuleGroupPanel.setPreferredSize(new java.awt.Dimension(259, 160));
		ivjRuleGroupPanel.setLayout(new java.awt.BorderLayout());
		getRuleGroupPanel().add(getRules(), "North");
		getRuleGroupPanel().add(getRulePanel(), "Center");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjRuleGroupPanel;
    }
    /**
     * Return the RulePanel property value.
     * @return redesign.gui.components.AdderPanel2
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    AdderPanel2 getRulePanel() {
	if (ivjRulePanel == null) {
	    try {
		ivjRulePanel = new edu.isi.powerloom.gui.components.AdderPanel2(getModule());
		ivjRulePanel.setName("RulePanel");
		ivjRulePanel.setText("RulePanel");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjRulePanel;
    }
    /**
     * Return the Rules property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getRules() {
	if (ivjRules == null) {
	    try {
		ivjRules = new javax.swing.JLabel();
		ivjRules.setName("Rules");
		ivjRules.setText("Rules");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjRules;
    }
    /**
     * Return the Supers property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getSupers() {
	if (ivjSupers == null) {
	    try {
		ivjSupers = new javax.swing.JLabel();
		ivjSupers.setName("Supers");
		ivjSupers.setText("SuperConcepts");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjSupers;
    }
    /**
     * Return the SupersGroupPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getSupersGroupPanel() {
	if (ivjSupersGroupPanel == null) {
	    try {
		ivjSupersGroupPanel = new javax.swing.JPanel();
		ivjSupersGroupPanel.setName("SupersGroupPanel");
		ivjSupersGroupPanel.setPreferredSize(new java.awt.Dimension(100, 160));
		ivjSupersGroupPanel.setLayout(new java.awt.BorderLayout());
		getSupersGroupPanel().add(getSupers(), "North");
		getSupersGroupPanel().add(getSupersPanel(), "Center");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjSupersGroupPanel;
    }
    /**
     * Return the SupersPanel property value.
     * @return redesign.gui.components.AdderPanel2
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    public AdderPanel2 getSupersPanel() {
	if (ivjSupersPanel == null) {
	    try {
		ivjSupersPanel = new edu.isi.powerloom.gui.components.AdderPanel2(getModule());
		ivjSupersPanel.setName("SupersPanel");
		ivjSupersPanel.setText("SupersPanel");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjSupersPanel;
    }
    /**
     * Return the TableColumn1 property value.
     * @return javax.swing.table.TableColumn
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.table.TableColumn getTypeColumn() {
	if (ivjTypeColumn == null) {
	    try {
		ivjTypeColumn = new javax.swing.table.TableColumn();
		ivjTypeColumn.setHeaderValue("Type");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjTypeColumn;
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
		ivjVerticalStrut1.setStrutSize(15);
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
		ivjVerticalStrut11.setStrutSize(15);
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
     * Return the VerticalStrut2 property value.
     * @return redesign.gui.components.VerticalStrut
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private VerticalStrut getVerticalStrut2() {
	if (ivjVerticalStrut2 == null) {
	    try {
		ivjVerticalStrut2 = new edu.isi.powerloom.gui.components.VerticalStrut();
		ivjVerticalStrut2.setName("VerticalStrut2");
		ivjVerticalStrut2.setStrutSize(15);
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjVerticalStrut2;
    }
    /**
     * Return the VerticalStrut21 property value.
     * @return redesign.gui.components.VerticalStrut
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private VerticalStrut getVerticalStrut21() {
	if (ivjVerticalStrut21 == null) {
	    try {
		ivjVerticalStrut21 = new edu.isi.powerloom.gui.components.VerticalStrut();
		ivjVerticalStrut21.setName("VerticalStrut21");
		ivjVerticalStrut21.setStrutSize(15);
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjVerticalStrut21;
    }
    /**
     * Return the VerticalStrut22 property value.
     * @return redesign.gui.components.VerticalStrut
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private VerticalStrut getVerticalStrut22() {
	if (ivjVerticalStrut22 == null) {
	    try {
		ivjVerticalStrut22 = new edu.isi.powerloom.gui.components.VerticalStrut();
		ivjVerticalStrut22.setName("VerticalStrut22");
		ivjVerticalStrut22.setStrutSize(15);
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjVerticalStrut22;
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
	getSupersPanel().addActionListener(ivjEventHandler);
	getOKButton().addActionListener(ivjEventHandler);
	getCancelButton().addActionListener(ivjEventHandler);
	getRelationPanel().addActionListener(ivjEventHandler);
	getPropositionsPanel().addActionListener(ivjEventHandler);	
	getRulePanel().addActionListener(ivjEventHandler);	
    }
    /**
     * Initialize the class.
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void initialize() {
	try {
	    // user code begin {1}
	    // user code end
	    setName("ConceptPanel2");
	    setLayout(getConceptPanel2BoxLayout());
	    setSize(592, 560);
	    add(getNameDocumentationSupersGroupPanel(), getNameDocumentationSupersGroupPanel().getName());
	    add(getVerticalStrut2(), getVerticalStrut2().getName());
	    add(getRelationsGroupPanel(), getRelationsGroupPanel().getName());
	    add(getVerticalStrut21(), getVerticalStrut21().getName());
	    add(getPropositionGroupPanel(), getPropositionGroupPanel().getName());
	    add(getVerticalStrut22(), getVerticalStrut22().getName());
	    add(getRuleGroupPanel(), getRuleGroupPanel().getName());
	    add(getButtonGroupPanel(), getButtonGroupPanel().getName());
	    initConnections();
	    ActionListener nameFieldAction = new ActionListener() {
		    public void actionPerformed(ActionEvent e) {
			debugPrintln(3, "You hit return in the name field.");
			ConceptPanel2.this.oKButton_ActionPerformed(e);
		    }
		};
	    getNameTextField().addActionListener(nameFieldAction);
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
	    ConceptPanel2 aConceptPanel2;
	    aConceptPanel2 = new ConceptPanel2();
	    frame.setContentPane(aConceptPanel2);
	    frame.setSize(aConceptPanel2.getSize());
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
    public void oKButton_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	if (getModule() == null) {
	    JOptionPane.showMessageDialog(PowerloomApp.getInstance(), "You must first select a module before commiting a concept.", "Select Module", JOptionPane.WARNING_MESSAGE);
	    return;
	}
	
	getParentFrame().commitConcept();
	getParentFrame().doDefaultCloseAction();
	return;
    }

    public void updateModuleInLists(PLModule module) {
	getPropositionsPanel().setModule(module);
	getRulePanel().setModule(module);
	getSupersPanel().setModule(module);
	getRelationPanel().setModule(module);
    }

    /**
     * Insert the method's description here.
     * Creation date: (4/12/2002 12:03:04 PM)
     */
    public void postCreateInitialize() {
	// initialize momdules
	try {
	    edu.isi.powerloom.gui.xmlobject.PLSurrogateContainer modules = 
		KnowledgeManager.getInstance().getModules().listifyTreeContainer();
	    PLListModel model = new PLListModel(modules);
	    getModuleComboBox().setModel(model);
	    PLModule selectedModule = PowerloomApp.getInstance().getMostRecentlyTouchedModule();
	    if (selectedModule != null) {
		getModuleComboBox().setSelectedItem(selectedModule);
	    }
	} catch (Exception e) {
	    handleException(e);
	} 
    }
    PLModule getModule() {
	return (PLModule)getModuleComboBox().getSelectedItem();
    }


    /**
     * Insert the method's description here.
     * Creation date: (4/20/2002 12:53:04 PM)
     * @param actionEvent java.awt.event.ActionEvent
     */
    public void propositionsPanel_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	if (actionEvent.getActionCommand().equals("add")) {
	    try {
		PropositionFrame2 frame = new PropositionFrame2();
		frame.displayFrame();
	    } catch (Exception e) {
		handleException(e);
	    } // end of try-catch
	} else if (actionEvent.getActionCommand().equals("delete")) {
            // TODO? Save deleted propositions, don't retract them until the concept
            // is committed...
	    try {
		PLModule module = (PLModule)getModuleComboBox().getSelectedItem();
		PLProposition proposition = (PLProposition)(getPropositionsPanel().getJList().getSelectedValue());
		if (proposition != null) {
		    (new RetractPropositionAction(getParentFrame().getModule(), proposition)).actionPerformed(actionEvent);
		}
	    } catch (Exception e) {
		handleException(e);
	    }
	    
	    /* obsolete code 
	       try {
	       PLProposition prop = (PLProposition)getPropositionsPanel().getJList().getSelectedValue();
	       if (prop != null) {
	       String propositionText = prop.getID();
	       String command = "(RETRACT " + propositionText + ")";
	       PLModule module = (PLModule)getModuleComboBox().getSelectedItem();
	       if (module == null) {
	       System.out.println("***conceptPanel2.propositionsPanel_actionperformed: error: module is null");
	       return; // todo throw exception
	       }
	       KnowledgeManager.getInstance().evaluateLogicCommand(module, command);
	       KnowledgeManager.getInstance().invalidatePropositionCaches();
	       getPropositionsPanel().removePLObject(prop);
	       // todo: update all listeners who might be displaying the prop...
	       //getParentFrame().fireEditPerformed();
	       } 
	       } catch (Exception e) {
	       handleException(e);
	       }
	    */
	} else {
	    debugPrintln(3, "Uknown action command: " + actionEvent.getActionCommand());
	}
    }
    /**
     * Insert the method's description here.
     * Creation date: (5/20/2002 9:53:29 PM)
     * @return javax.swing.JComboBox
     */
    public JComboBox pubGetModuleComboBox() {
	return getModuleComboBox();
    }

    public void relationPanel_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	if (actionEvent.getActionCommand().equals("add")) {
	    try {
		RelationFrame relationFrame = new RelationFrame();
		String name = getNameTextField().getText();
		relationFrame.setupNewRelation("NEW-" + name + "-RELATION", "?" + name.toLowerCase(), name.toUpperCase());
		relationFrame.displayFrame();
	    } catch (Exception e) {
		handleException(e);
	    } // end of try-catch
	} else if (actionEvent.getActionCommand().equals("delete")) {
	    debugPrintln(3, "conceptpanel2 relation delete button pressed.");
	    PLObject object = (PLObject)(getRelationPanel().getJList().getSelectedValue());
	    if (object != null) {
		(new DeleteObjectAction(getParentFrame().getModule(), object)).actionPerformed(actionEvent);
	    }
	} else {
	    debugPrintln(3, "Uknown action command: " + actionEvent.getActionCommand());
	}
    }

    /**
     * Insert the method's description here.
     * Creation date: (4/20/2002 12:53:04 PM)
     * @param actionEvent java.awt.event.ActionEvent
     */
    public void rulePanel_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	if (actionEvent.getActionCommand().equals("add")) {
	    try {
		PropositionFrame2 frame = new PropositionFrame2();
		frame.displayFrame();
	    } catch (Exception e) {
		handleException(e);
	    } // end of try-catch
	} else if (actionEvent.getActionCommand().equals("delete")) {
	    try {
		PLModule module = (PLModule)getModuleComboBox().getSelectedItem();
		PLProposition proposition = (PLProposition)(getRulePanel().getJList().getSelectedValue());
		if (proposition != null) {
		    (new RetractPropositionAction(getParentFrame().getModule(), proposition)).actionPerformed(actionEvent);
		}
	    } catch (Exception e) {
		handleException(e);
	    }

	} else {
	    debugPrintln(3, "Uknown action command: " + actionEvent.getActionCommand());
	}
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/10/2002 4:07:07 PM)
     * @param newParentFrame redesign.gui.components.ConceptFrame2
     */
    public void setParentFrame(ConceptFrame2 newParentFrame) {
	parentFrame = newParentFrame;
    }
    /**
     * Comment
     */
    public void supersPanel_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	if (actionEvent.getActionCommand().equals("add")) {
	    try {
		debugPrintln(3, "add button pressed.");
		ConceptChooserDialog chooser = new ConceptChooserDialog();
		chooser.setModal(true);
		chooser.setTitle("Choose concept");
		chooser.getChooserPanel().getEnterLabel().setText("Enter Concept Name:");
		chooser.getChooserPanel().revalidate();
		PLModule module = (PLModule)getModuleComboBox().getSelectedItem();
		chooser.setModule(module);
		chooser.show();
		if (chooser.getChooserPanel().getResult() != null) {
		    getSupersPanel().addPLObject(chooser.getChooserPanel().getResult());
		}
	    } catch (Exception e) {
		handleException(e);
	    } // end of try-catch
	} else if (actionEvent.getActionCommand().equals("delete")) {
	    debugPrintln(3, "delete button pressed.");
	    PLObject selected = (PLObject)getSupersPanel().getJList().getSelectedValue();
	    if (selected != null) {
		getSupersPanel().removePLObject(selected);
	    }
	} else {
	    debugPrintln(3, "Uknown action command: " + actionEvent.getActionCommand());
	}
    }
}
