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


// Version: InstancePanel.java,v 1.19 2010/02/04 05:17:42 hans Exp

package edu.isi.powerloom.gui.components;

import javax.swing.*;
import java.util.*;
import java.awt.event.*;

import edu.isi.powerloom.gui.serverinterface.*;
import edu.isi.powerloom.gui.xmlobject.*;
import edu.isi.powerloom.gui.common.*;
import static edu.isi.powerloom.gui.common.Utils.*;

/**
 * Pane which contains the controls for the instance editor.
 *
 * @since 4/1/2002 5:24:08 PM
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 * @see edu.isi.powerloom.gui.components.InstanceFrame InstanceFrame
 */
public class InstancePanel extends javax.swing.JPanel {
    private javax.swing.JLabel ivjDocumentation = null;
    private javax.swing.JTextArea ivjDocumentationTextArea = null;
    private javax.swing.JLabel ivjConceptName = null;
    private javax.swing.JLabel ivjPropositions = null;
    private AdderPanel2 ivjPropositionsPanel = null;
    private javax.swing.BoxLayout ivjConceptPanel2BoxLayout = null;
    private javax.swing.JPanel ivjDocumentationGroupPanel = null;
    private javax.swing.JScrollPane ivjDocumentationScrollPane = null;
    private javax.swing.JPanel ivjNameDocumentationGroupPanel = null;
    private javax.swing.BoxLayout ivjNameDocumentationGroupPanelBoxLayout = null;
    private javax.swing.JPanel ivjNameDocumentationSupersGroupPanel = null;
    private javax.swing.JPanel ivjNameGroupPanel = null;
    private javax.swing.JTextField ivjNameTextField = null;
    private javax.swing.JPanel ivjPropositionGroupPanel = null;
    private VerticalStrut ivjVerticalStrut1 = null;
    private VerticalStrut ivjVerticalStrut21 = null;
    private VerticalStrut ivjVerticalStrut22 = null;
    private javax.swing.JPanel ivjButtonGroupPanel = null;
    private javax.swing.JButton ivjCancelButton = null;
    private javax.swing.JButton ivjOKButton = null;
    private VerticalStrut ivjVerticalStrut11 = null;
    private VerticalStrut ivjVerticalStrut12 = null;
    private InstanceFrame parentFrame;
    private HorizontalStrut ivjHorizontalStrut2 = null;
    private javax.swing.JComboBox ivjModuleComboBox = null;
    private javax.swing.JLabel ivjModuleLabel = null;
    private javax.swing.JPanel ivjModulePanel = null;
    IvjEventHandler ivjEventHandler = new IvjEventHandler();
    private HorizontalStrut ivjHorizontalStrut1 = null;
    private javax.swing.JPanel ivjSupersGroupPanel = null;
    private AdderPanel2 ivjSupersPanel = null;
    private javax.swing.JLabel ivjTypesLabel = null;
    private javax.swing.JPanel ivjPropositionGroupAndHTMLGroupPanel = null;
    private javax.swing.JPanel ivjHTMLGroupPanel = null;
    private HTMLBrowserPane ivjHTMLPanel = null;


    class IvjEventHandler implements java.awt.event.ActionListener {
	public void actionPerformed(java.awt.event.ActionEvent e) {
	    if (e.getSource() == InstancePanel.this.getOKButton()) 
		connEtoC1(e);
	    if (e.getSource() == InstancePanel.this.getCancelButton()) 
		connEtoC2(e);
	    if (e.getSource() == InstancePanel.this.getSupersPanel()) 
		connEtoC3(e);
	    if (e.getSource() == InstancePanel.this.getPropositionsPanel()) 
		connEtoC4(e);
	};
    };
    /**
     * ConceptPanel2 constructor comment.
     */
    public InstancePanel() {
	super();
	initialize();
    }

    public InstancePanel(InstanceFrame parent) {
	super();
	parentFrame = parent;
	initialize();
    }
    /**
     * ConceptPanel2 constructor comment.
     * @param layout java.awt.LayoutManager
     */
    public InstancePanel(java.awt.LayoutManager layout) {
	super(layout);
    }
    /**
     * ConceptPanel2 constructor comment.
     * @param layout java.awt.LayoutManager
     * @param isDoubleBuffered boolean
     */
    public InstancePanel(java.awt.LayoutManager layout, boolean isDoubleBuffered) {
	super(layout, isDoubleBuffered);
    }
    /**
     * ConceptPanel2 constructor comment.
     * @param isDoubleBuffered boolean
     */
    public InstancePanel(boolean isDoubleBuffered) {
	super(isDoubleBuffered);
    }
    /**
     * Comment
     */
    public void cancelButton_ActionPerformed1(java.awt.event.ActionEvent actionEvent) {
	getParentFrame().doDefaultCloseAction();
    }
    /**
     * connEtoC1:  (OKButton.action.actionPerformed(java.awt.event.ActionEvent) --> InstancePanel.oKButton_ActionPerformed1(Ljava.awt.event.ActionEvent;)V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC1(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.oKButton_ActionPerformed1(arg1);
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }
    /**
     * connEtoC2:  (CancelButton.action.actionPerformed(java.awt.event.ActionEvent) --> InstancePanel.cancelButton_ActionPerformed1(Ljava.awt.event.ActionEvent;)V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
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
     * connEtoC3:  (SupersPanel.action.actionPerformed(java.awt.event.ActionEvent) --> InstancePanel.supersPanel_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC3(java.awt.event.ActionEvent arg1) {
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
     * connEtoC4:  (PropositionsPanel.action.actionPerformed(java.awt.event.ActionEvent) --> InstancePanel.propositionsPanel_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC4(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.propositionsPanel_ActionPerformed(arg1);
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
		ivjDocumentationGroupPanel.setPreferredSize(new java.awt.Dimension(1000, 33));
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
    private javax.swing.JTextArea getDocumentationTextArea() {
	if (ivjDocumentationTextArea == null) {
	    try {
		String[] types = {"Text Holder"};
		ivjDocumentationTextArea = new PLJTextArea(getParentFrame(), "Instance Documentation Text Area", Arrays.asList(types));
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
		ivjHorizontalStrut1.setStrutSize(10);
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
    public void updateModuleInLists(PLModule module) {
	getPropositionsPanel().setModule(module);
	getSupersPanel().setModule(module);
    }

    PLModule getModule() {
	return (PLModule)getModuleComboBox().getSelectedItem();
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
		ivjModulePanel.setPreferredSize(new java.awt.Dimension(1000, 23));
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
     * Return the NameDocumentationGroupPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getNameDocumentationGroupPanel() {
	if (ivjNameDocumentationGroupPanel == null) {
	    try {
		ivjNameDocumentationGroupPanel = new javax.swing.JPanel();
		ivjNameDocumentationGroupPanel.setName("NameDocumentationGroupPanel");
		ivjNameDocumentationGroupPanel.setPreferredSize(new java.awt.Dimension(100, 1000));
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
		//ivjNameDocumentationSupersGroupPanel.setPreferredSize(new java.awt.Dimension(250, 120));
                // hc: adapted to have a smaller empty documentation area in instance panels.
                // NOTES: - `setMaximumSize' and `setPreferredSize' interact, if we set the maximum,
                //           we also have to set the preferred size to get the desired effect.
                //        - not sure this is the proper fix; it fixed one of the demos on a smaller
                //          screen, but it leaves the documentation box kind of small for larger
                //          docstrings such as in the Aircraft KB.
                //        - if this ever actually gets automatically regenerated, no big loss
		ivjNameDocumentationSupersGroupPanel.setPreferredSize(new java.awt.Dimension(250, 150));
                ivjNameDocumentationSupersGroupPanel.setMaximumSize(new java.awt.Dimension(2000, 150));
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
		ivjNameGroupPanel.setPreferredSize(new java.awt.Dimension(1000, 34));
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
    private javax.swing.JTextField getNameTextField() {
	if (ivjNameTextField == null) {
	    try {
		String[] types = {"Text Holder"};
		ivjNameTextField = new PLJTextField(getParentFrame(), "Instance Name Text Field", Arrays.asList(types));

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
     * Creation date: (4/13/2002 12:26:55 PM)
     * @return javax.swing.JInternalFrame
     */
    public InstanceFrame getParentFrame() {
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

    private javax.swing.JPanel getPropositionGroupAndHTMLGroupPanel() {
	if (ivjPropositionGroupAndHTMLGroupPanel == null) {
	    try {
		ivjPropositionGroupAndHTMLGroupPanel = new javax.swing.JPanel();
		ivjPropositionGroupAndHTMLGroupPanel.setName("PropositionGroupAndHTMLGroupPanel");
		ivjPropositionGroupAndHTMLGroupPanel.setPreferredSize(new java.awt.Dimension(259, 160));
		ivjPropositionGroupAndHTMLGroupPanel.setLayout(new java.awt.BorderLayout());
		getPropositionGroupAndHTMLGroupPanel().add(getPropositionGroupPanel(), "Center");
		getPropositionGroupAndHTMLGroupPanel().add(getHTMLGroupPanel(), "South");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjPropositionGroupAndHTMLGroupPanel;
    }


    public javax.swing.JPanel getHTMLGroupPanel() {
	if (ivjHTMLGroupPanel == null) {
	    try {
		ivjHTMLGroupPanel = new javax.swing.JPanel();
		ivjHTMLGroupPanel.setName("HTMLGroupPanel");
		//ivjHTMLGroupPanel.setPreferredSize(new java.awt.Dimension(259, 160));
		ivjHTMLGroupPanel.setLayout(new java.awt.BorderLayout());
		ivjHTMLGroupPanel.setBorder(new javax.swing.border.EmptyBorder(10, 00, 10, 0));
		//getHTMLGroupPanel().add(new JLabel("Image URL:");
		getHTMLGroupPanel().add(getHTMLPanel(), "Center");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjHTMLGroupPanel;
    }

    public HTMLBrowserPane getHTMLPanel() {
	debugPrintln(3, "in getHTMLPanel()");
	if (ivjHTMLPanel == null) {
	    debugPrintln(3, "ivjHTMLPanel is null");
	    try {
		String placeholderHTML = "<HTML><HEAD></HEAD><BODY><CENTER>No Image for this instance.</CENTER></BODY></HTML>";
		ivjHTMLPanel = new HTMLBrowserPane(placeholderHTML);
		ivjHTMLPanel.setName("HTMLPanel");
		getHTMLPanel().setPreferredSize(new java.awt.Dimension(50, 50));
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjHTMLPanel;
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
    private AdderPanel2 getPropositionsPanel() {
	if (ivjPropositionsPanel == null) {
	    try {
		ivjPropositionsPanel = new edu.isi.powerloom.gui.components.AdderPanel2(null);
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
     * Insert the method's description here.
     * Creation date: (4/17/2002 3:28:26 PM)
     * @return javax.swing.JTextArea
     */
    public javax.swing.JTextArea getPubDocumentationTextArea() {
	return getDocumentationTextArea();
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/17/2002 3:25:06 PM)
     * @return javax.swing.JComboBox
     */
    public javax.swing.JComboBox getPubModuleComboBox() {
	return getModuleComboBox();
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/17/2002 3:27:36 PM)
     * @return javax.swing.JTextField
     */
    public javax.swing.JTextField getPubNameTextField() {
	return getNameTextField();
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/17/2002 3:25:52 PM)
     * @return redesign.gui.components.AdderPanel2
     */
    public AdderPanel2 getPubPropositionsPanel() {
	return getPropositionsPanel();
    }
    /**
     * Insert the method's description here.
     * Creation date: (5/1/2002 11:00:54 AM)
     * @return redesign.gui.components.AdderPanel2
     */
    public AdderPanel2 getPubSupersPanel() {
	return getSupersPanel();
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
		ivjSupersGroupPanel.setPreferredSize(new java.awt.Dimension(100, 169));
		ivjSupersGroupPanel.setLayout(new java.awt.BorderLayout());
		getSupersGroupPanel().add(getSupersPanel(), "Center");
		getSupersGroupPanel().add(getTypesLabel(), "North");
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
    private AdderPanel2 getSupersPanel() {
	if (ivjSupersPanel == null) {
	    try {
		ivjSupersPanel = new edu.isi.powerloom.gui.components.AdderPanel2(null);
		ivjSupersPanel.setName("SupersPanel");
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
     * Return the TypesLabel property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getTypesLabel() {
	if (ivjTypesLabel == null) {
	    try {
		ivjTypesLabel = new javax.swing.JLabel();
		ivjTypesLabel.setName("TypesLabel");
		ivjTypesLabel.setText("Types");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjTypesLabel;
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
     * Return the VerticalStrut12 property value.
     * @return redesign.gui.components.VerticalStrut
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private VerticalStrut getVerticalStrut12() {
	if (ivjVerticalStrut12 == null) {
	    try {
		ivjVerticalStrut12 = new edu.isi.powerloom.gui.components.VerticalStrut();
		ivjVerticalStrut12.setName("VerticalStrut12");
		ivjVerticalStrut12.setStrutSize(5);
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
	getOKButton().addActionListener(ivjEventHandler);
	getCancelButton().addActionListener(ivjEventHandler);
	getSupersPanel().addActionListener(ivjEventHandler);
	getPropositionsPanel().addActionListener(ivjEventHandler);
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
	    setSize(484, 331);
	    add(getNameDocumentationSupersGroupPanel(), getNameDocumentationSupersGroupPanel().getName());
	    add(getVerticalStrut12(), getVerticalStrut12().getName());
	    add(getVerticalStrut21(), getVerticalStrut21().getName());
	    add(getPropositionGroupAndHTMLGroupPanel(), getPropositionGroupPanel().getName());
	    add(getVerticalStrut22(), getVerticalStrut22().getName());
	    add(getButtonGroupPanel(), getButtonGroupPanel().getName());
	    initConnections();
	    ActionListener nameFieldAction = new ActionListener() {
		    public void actionPerformed(ActionEvent e) {
			debugPrintln(3, "You hit return in the name field.");
			InstancePanel.this.oKButton_ActionPerformed1(e);
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
    public void oKButton_ActionPerformed1(java.awt.event.ActionEvent actionEvent) {
	if (getModule() == null) {
	    JOptionPane.showMessageDialog(PowerloomApp.getInstance(), "You must first select a module before commiting an instance.", "Select Module", JOptionPane.WARNING_MESSAGE);
	    return;
	}
	getParentFrame().commitInstance();
	getParentFrame().doDefaultCloseAction();
	return;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/12/2002 12:03:04 PM)
     */
    public void postCreateInitialize() {
	// initialize modules
	try {
	    edu.isi.powerloom.gui.xmlobject.PLSurrogateContainer modules = 
		KnowledgeManager.getInstance().getModules().listifyTreeContainer();
	    PLListModel model = new PLListModel(modules);
	    getModuleComboBox().setModel(model);
	    PLModule selectedModule = getParentFrame().getApp().getMostRecentlyTouchedModule();
	    if (selectedModule != null) {
		getModuleComboBox().setSelectedItem(selectedModule);
	    }
	} catch (Exception e) {
	    handleException(e);
	} 
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
		frame.setProposition(getModule(), "(<RELATION> " + getNameTextField().getText() + ")", 1, 11);
		frame.displayFrame();
	    } catch (Exception e) {
		handleException(e);
	    } // end of try-catch
	} else if (actionEvent.getActionCommand().equals("delete")) {
	    // TODO? Save deleted propositions, don't retract them until the concept
            // is committed...
            try {
                PLProposition prop = (PLProposition)getPropositionsPanel().getJList().getSelectedValue();
                if (prop != null) {
                    String propositionText = prop.getID();
                    String command = "(RETRACT " + propositionText + ")";
                    PLModule module = (PLModule)getModuleComboBox().getSelectedItem();
		    if (module == null) {
		  	debugPrintln(3, "***instancePanel.propositionsPanel_actionperformed: error: module is null");
			return; // todo throw exception
		    }
		    KnowledgeManager.getInstance().evaluateLogicCommand(module, command);
		    KnowledgeManager.getInstance().invalidatePropositionCaches();
                    getPropositionsPanel().removePLObject(prop);
		    getParentFrame().refreshPropositionsPanel();		    
                    // todo: update all listeners who might be displaying the prop...
		    //getParentFrame().fireEditPerformed();
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
     * Creation date: (4/13/2002 12:26:55 PM)
     * @param newParentFrame javax.swing.JInternalFrame
     */
    public void setParentFrame(InstanceFrame newParentFrame) {
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
