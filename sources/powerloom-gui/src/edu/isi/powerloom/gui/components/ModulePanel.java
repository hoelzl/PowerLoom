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


// Version: ModulePanel.java,v 1.13 2010/02/04 05:17:58 hans Exp

package edu.isi.powerloom.gui.components;

import java.awt.event.*;
import java.util.*;
import edu.isi.powerloom.gui.serverinterface.*;
import edu.isi.powerloom.gui.xmlobject.*;
import static edu.isi.powerloom.gui.common.Utils.*;

/**
 * Panel which contains the components for the module editor.
 *
 * @since 4/1/2002 5:24:08 PM
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 * @see edu.isi.powerloom.gui.components.ModuleFrame ModuleFrame
 */
public class ModulePanel extends javax.swing.JPanel {
    private javax.swing.JLabel ivjDocumentation = null;
    private javax.swing.JTextArea ivjDocumentationTextArea = null;
    private javax.swing.JPanel ivjDocumentationGroupPanel = null;
    private javax.swing.JScrollPane ivjDocumentationScrollPane = null;
    private HorizontalStrut ivjHorizontalStrut1 = null;
    private javax.swing.JPanel ivjNameDocumentationGroupPanel = null;
    private javax.swing.BoxLayout ivjNameDocumentationGroupPanelBoxLayout = null;
    private javax.swing.JPanel ivjNameGroupPanel = null;
    private javax.swing.JTextField ivjNameTextField = null;
    private VerticalStrut ivjVerticalStrut1 = null;
    private VerticalStrut ivjVerticalStrut22 = null;
    private javax.swing.JPanel ivjButtonGroupPanel = null;
    private javax.swing.JButton ivjCancelButton = null;
    private javax.swing.JButton ivjOKButton = null;
    private javax.swing.JPanel ivjCheckboxGroupPanel = null;
    private VerticalStrut ivjVerticalStrut12 = null;
    private ModuleFrame parentFrame;
    private javax.swing.JPanel ivjCppGroupPanel = null;
    private javax.swing.JLabel ivjCppNamespaceLabel = null;
    private javax.swing.JTextField ivjCppNamespaceTextField = null;
    private javax.swing.JPanel ivjJavaGroupPanel = null;
    private javax.swing.JLabel ivjJavaPackageLabel = null;
    private javax.swing.JTextField ivjJavaPackageTextField = null;
    private javax.swing.JPanel ivjLispGroupPanel = null;
    private javax.swing.JLabel ivjLispPackageLabel = null;
    private javax.swing.JTextField ivjLispPackageTextField = null;
    private javax.swing.JPanel ivjNameDocumentationNamespaceGroupPanel = null;
    private javax.swing.JPanel ivjNamespaceGroupPanel = null;
    private javax.swing.BoxLayout ivjNamespaceGroupPanelBoxLayout = null;
    private javax.swing.JCheckBox ivjAPICheckBox = null;
    private javax.swing.JCheckBox ivjCaseSensitiveCheckBox = null;
    private HorizontalStrut ivjHorizontalStrut11 = null;
    private javax.swing.JPanel ivjIncludesGroupPanel = null;
    private javax.swing.JLabel ivjIncludesLabel = null;
    private AdderPanel2 ivjIncludesPanel = null;
    private javax.swing.JPanel ivjIncludesUsesGroupPanel = null;
    private javax.swing.JPanel ivjJavaCatchallGroupPanel = null;
    private javax.swing.JLabel ivjJavaCatchallLabel = null;
    private javax.swing.JTextField ivjJavaCatchallTextField = null;
    private javax.swing.JPanel ivjMiscGroupPanel = null;
    private javax.swing.JPanel ivjShadowGroupPanel = null;
    private javax.swing.JLabel ivjShadowLabel = null;
    private AdderPanel2 ivjShadowPanel = null;
    private javax.swing.JPanel ivjUsesGroupPanel = null;
    private javax.swing.JLabel ivjUsesLabel = null;
    private AdderPanel2 ivjUsesPanel = null;
    private javax.swing.JPanel ivjJPanel1 = null;
    private javax.swing.JPanel ivjJPanel2 = null;
    private javax.swing.JLabel ivjModuleNameLabel = null;
    private javax.swing.BoxLayout ivjModulePanelBoxLayout = null;
    IvjEventHandler ivjEventHandler = new IvjEventHandler();

    class IvjEventHandler implements java.awt.event.ActionListener {
	public void actionPerformed(java.awt.event.ActionEvent e) {
	    if (e.getSource() == ModulePanel.this.getIncludesPanel()) 
		connEtoC1(e);
	    if (e.getSource() == ModulePanel.this.getUsesPanel()) 
		connEtoC2(e);
	    if (e.getSource() == ModulePanel.this.getShadowPanel()) 
		connEtoC3(e);
	    if (e.getSource() == ModulePanel.this.getOKButton()) 
		connEtoC4(e);
	    if (e.getSource() == ModulePanel.this.getCancelButton()) 
		connEtoC5(e);
	};
    };
    /**
     * ConceptPanel2 constructor comment.
     */
    public ModulePanel(ModuleFrame parent) {
	super();
	parentFrame = parent;
	initialize();
    }
    /**
     * ConceptPanel2 constructor comment.
     * @param layout java.awt.LayoutManager
     */
    public ModulePanel(java.awt.LayoutManager layout) {
	super(layout);
    }
    /**
     * ConceptPanel2 constructor comment.
     * @param layout java.awt.LayoutManager
     * @param isDoubleBuffered boolean
     */
    public ModulePanel(java.awt.LayoutManager layout, boolean isDoubleBuffered) {
	super(layout, isDoubleBuffered);
    }
    /**
     * ConceptPanel2 constructor comment.
     * @param isDoubleBuffered boolean
     */
    public ModulePanel(boolean isDoubleBuffered) {
	super(isDoubleBuffered);
    }
    /**
     * Comment
     */
    public void cancelButton_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	getParentFrame().doDefaultCloseAction();
    }
    /**
     * Comment
     */
    public void cancelButton_ActionPerformed1(java.awt.event.ActionEvent actionEvent) {
	getParentFrame().doDefaultCloseAction();
    }
    /**
     * connEtoC1:  (IncludesPanel.action.actionPerformed(java.awt.event.ActionEvent) --> ModulePanel.includesPanel_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC1(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.includesPanel_ActionPerformed(arg1);
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }
    /**
     * connEtoC2:  (UsesPanel.action.actionPerformed(java.awt.event.ActionEvent) --> ModulePanel.usesPanel_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC2(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.usesPanel_ActionPerformed(arg1);
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
     * Creation date: (4/15/2002 1:08:09 PM)
     * @param arg1 java.awt.event.ActionEvent
     */
    private void connEtoC3(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.shadowPanel_ActionPerformed(arg1);
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
     * Creation date: (4/15/2002 1:08:37 PM)
     * @param arg1 java.awt.event.ActionEvent
     */
    private void connEtoC4(java.awt.event.ActionEvent arg1) {
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
     * connEtoC5:  (CancelButton.action.actionPerformed(java.awt.event.ActionEvent) --> ModulePanel.cancelButton_ActionPerformed1(Ljava.awt.event.ActionEvent;)V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC5(java.awt.event.ActionEvent arg1) {
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
     * Return the FunctionCheckBox property value.
     * @return javax.swing.JCheckBox
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JCheckBox getAPICheckBox() {
	if (ivjAPICheckBox == null) {
	    try {
		ivjAPICheckBox = new javax.swing.JCheckBox();
		ivjAPICheckBox.setName("APICheckBox");
		ivjAPICheckBox.setText("API");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjAPICheckBox;
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
     * Return the ClosedCheckbox property value.
     * @return javax.swing.JCheckBox
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JCheckBox getCaseSensitiveCheckBox() {
	if (ivjCaseSensitiveCheckBox == null) {
	    try {
		ivjCaseSensitiveCheckBox = new javax.swing.JCheckBox();
		ivjCaseSensitiveCheckBox.setName("CaseSensitiveCheckBox");
		ivjCaseSensitiveCheckBox.setText("Case-Sensitive");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjCaseSensitiveCheckBox;
    }
    /**
     * Return the CheckboxGroupPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getCheckboxGroupPanel() {
	if (ivjCheckboxGroupPanel == null) {
	    try {
		ivjCheckboxGroupPanel = new javax.swing.JPanel();
		ivjCheckboxGroupPanel.setName("CheckboxGroupPanel");
		ivjCheckboxGroupPanel.setLayout(new java.awt.FlowLayout());
		getCheckboxGroupPanel().add(getAPICheckBox(), getAPICheckBox().getName());
		getCheckboxGroupPanel().add(getCaseSensitiveCheckBox(), getCaseSensitiveCheckBox().getName());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjCheckboxGroupPanel;
    }
    /**
     * Return the CppGroupPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getCppGroupPanel() {
	if (ivjCppGroupPanel == null) {
	    try {
		ivjCppGroupPanel = new javax.swing.JPanel();
		ivjCppGroupPanel.setName("CppGroupPanel");
		ivjCppGroupPanel.setPreferredSize(new java.awt.Dimension(93, 34));
		ivjCppGroupPanel.setLayout(new java.awt.BorderLayout());
		ivjCppGroupPanel.setMaximumSize(new java.awt.Dimension(2147483647, 2147483647));
		getCppGroupPanel().add(getCppNamespaceTextField(), "Center");
		getCppGroupPanel().add(getCppNamespaceLabel(), "North");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjCppGroupPanel;
    }
    /**
     * Return the CppNamespaceLabel property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getCppNamespaceLabel() {
	if (ivjCppNamespaceLabel == null) {
	    try {
		ivjCppNamespaceLabel = new javax.swing.JLabel();
		ivjCppNamespaceLabel.setName("CppNamespaceLabel");
		ivjCppNamespaceLabel.setText("C++ Namespace");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjCppNamespaceLabel;
    }
    /**
     * Return the CppNamespaceTextField property value.
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTextField getCppNamespaceTextField() {
	if (ivjCppNamespaceTextField == null) {
	    try {
		String[] types = {"Text Holder"};
		ivjCppNamespaceTextField = new PLJTextField(getParentFrame(), "C++ Namespace Text Field", Arrays.asList(types));
		ivjCppNamespaceTextField.setName("CppNamespaceTextField");
		ivjCppNamespaceTextField.setMaximumSize(new java.awt.Dimension(2147483647, 20));
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjCppNamespaceTextField;
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
    private javax.swing.JTextArea getDocumentationTextArea() {
	if (ivjDocumentationTextArea == null) {
	    try {
		String[] types = {"Text Holder"};
		ivjDocumentationTextArea = new PLJTextArea(getParentFrame(), "Module Documentation Text Area", Arrays.asList(types));
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
     * Return the HorizontalStrut11 property value.
     * @return redesign.gui.components.HorizontalStrut
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private HorizontalStrut getHorizontalStrut11() {
	if (ivjHorizontalStrut11 == null) {
	    try {
		ivjHorizontalStrut11 = new edu.isi.powerloom.gui.components.HorizontalStrut();
		ivjHorizontalStrut11.setName("HorizontalStrut11");
		ivjHorizontalStrut11.setStrutSize(30);
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjHorizontalStrut11;
    }
    /**
     * Return the IncludesGroupPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getIncludesGroupPanel() {
	if (ivjIncludesGroupPanel == null) {
	    try {
		ivjIncludesGroupPanel = new javax.swing.JPanel();
		ivjIncludesGroupPanel.setName("IncludesGroupPanel");
		ivjIncludesGroupPanel.setLayout(new java.awt.BorderLayout());
		getIncludesGroupPanel().add(getIncludesLabel(), "North");
		getIncludesGroupPanel().add(getIncludesPanel(), "Center");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjIncludesGroupPanel;
    }
    /**
     * Return the Relations property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getIncludesLabel() {
	if (ivjIncludesLabel == null) {
	    try {
		ivjIncludesLabel = new javax.swing.JLabel();
		ivjIncludesLabel.setName("IncludesLabel");
		ivjIncludesLabel.setText("Includes");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjIncludesLabel;
    }
    /**
     * Return the IncludesPanel property value.
     * @return redesign.gui.components.AdderPanel2
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private AdderPanel2 getIncludesPanel() {
	if (ivjIncludesPanel == null) {
	    try {
		ivjIncludesPanel = new edu.isi.powerloom.gui.components.AdderPanel2(null);
		ivjIncludesPanel.setName("IncludesPanel");
		ivjIncludesPanel.setText("PropositionsPanel");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjIncludesPanel;
    }
    /**
     * Return the RelationsGroupPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getIncludesUsesGroupPanel() {
	if (ivjIncludesUsesGroupPanel == null) {
	    try {
		ivjIncludesUsesGroupPanel = new javax.swing.JPanel();
		ivjIncludesUsesGroupPanel.setName("IncludesUsesGroupPanel");
		ivjIncludesUsesGroupPanel.setPreferredSize(new java.awt.Dimension(259, 130));
		ivjIncludesUsesGroupPanel.setLayout(new javax.swing.BoxLayout(getIncludesUsesGroupPanel(), javax.swing.BoxLayout.X_AXIS));
		ivjIncludesUsesGroupPanel.setMinimumSize(new java.awt.Dimension(0, 0));
		getIncludesUsesGroupPanel().add(getIncludesGroupPanel(), getIncludesGroupPanel().getName());
		getIncludesUsesGroupPanel().add(getHorizontalStrut11(), getHorizontalStrut11().getName());
		getIncludesUsesGroupPanel().add(getUsesGroupPanel(), getUsesGroupPanel().getName());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjIncludesUsesGroupPanel;
    }
    /**
     * Return the JavaCatchallGroupPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getJavaCatchallGroupPanel() {
	if (ivjJavaCatchallGroupPanel == null) {
	    try {
		ivjJavaCatchallGroupPanel = new javax.swing.JPanel();
		ivjJavaCatchallGroupPanel.setName("JavaCatchallGroupPanel");
		ivjJavaCatchallGroupPanel.setLayout(new java.awt.BorderLayout());
		ivjJavaCatchallGroupPanel.setMaximumSize(new java.awt.Dimension(2147483647, 5));
		getJavaCatchallGroupPanel().add(getJavaCatchallTextField(), "Center");
		getJavaCatchallGroupPanel().add(getJavaCatchallLabel(), "North");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjJavaCatchallGroupPanel;
    }
    /**
     * Return the JavaCatchallLabel property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getJavaCatchallLabel() {
	if (ivjJavaCatchallLabel == null) {
	    try {
		ivjJavaCatchallLabel = new javax.swing.JLabel();
		ivjJavaCatchallLabel.setName("JavaCatchallLabel");
		ivjJavaCatchallLabel.setText("Java Catch-All Class");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjJavaCatchallLabel;
    }
    /**
     * Return the JavaCatchallTextField property value.
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTextField getJavaCatchallTextField() {
	if (ivjJavaCatchallTextField == null) {
	    try {
		String[] types = {"Text Holder"};
		ivjJavaCatchallTextField = new PLJTextField(getParentFrame(), "Java Catchall Text Field", Arrays.asList(types));
		ivjJavaCatchallTextField.setName("JavaCatchallTextField");
		ivjJavaCatchallTextField.setMaximumSize(new java.awt.Dimension(2147483647, 20));
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjJavaCatchallTextField;
    }
    /**
     * Return the JavaGroupPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getJavaGroupPanel() {
	if (ivjJavaGroupPanel == null) {
	    try {
		ivjJavaGroupPanel = new javax.swing.JPanel();
		ivjJavaGroupPanel.setName("JavaGroupPanel");
		ivjJavaGroupPanel.setPreferredSize(new java.awt.Dimension(80, 34));
		ivjJavaGroupPanel.setLayout(new java.awt.BorderLayout());
		ivjJavaGroupPanel.setMaximumSize(new java.awt.Dimension(2147483647, 2147483647));
		getJavaGroupPanel().add(getJavaPackageTextField(), "Center");
		getJavaGroupPanel().add(getJavaPackageLabel(), "North");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjJavaGroupPanel;
    }
    /**
     * Return the JavaPackageLabel property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getJavaPackageLabel() {
	if (ivjJavaPackageLabel == null) {
	    try {
		ivjJavaPackageLabel = new javax.swing.JLabel();
		ivjJavaPackageLabel.setName("JavaPackageLabel");
		ivjJavaPackageLabel.setText("Java Package");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjJavaPackageLabel;
    }
    /**
     * Return the JavaPackageTextField property value.
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTextField getJavaPackageTextField() {
	if (ivjJavaPackageTextField == null) {
	    try {
		String[] types = {"Text Holder"};
		ivjJavaPackageTextField = new PLJTextField(getParentFrame(), "Java Package Text Field", Arrays.asList(types));
		ivjJavaPackageTextField.setName("JavaPackageTextField");
		ivjJavaPackageTextField.setPreferredSize(new java.awt.Dimension(4, 5));
		ivjJavaPackageTextField.setMaximumSize(new java.awt.Dimension(2147483647, 500));
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjJavaPackageTextField;
    }
    /**
     * Return the JPanel1 property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getJPanel1() {
	if (ivjJPanel1 == null) {
	    try {
		ivjJPanel1 = new javax.swing.JPanel();
		ivjJPanel1.setName("JPanel1");
		ivjJPanel1.setPreferredSize(new java.awt.Dimension(0, 30));
		ivjJPanel1.setLayout(null);
		ivjJPanel1.setMinimumSize(new java.awt.Dimension(0, 0));
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjJPanel1;
    }
    /**
     * Return the JPanel2 property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getJPanel2() {
	if (ivjJPanel2 == null) {
	    try {
		ivjJPanel2 = new javax.swing.JPanel();
		ivjJPanel2.setName("JPanel2");
		ivjJPanel2.setPreferredSize(new java.awt.Dimension(0, 30));
		ivjJPanel2.setLayout(null);
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjJPanel2;
    }
    /**
     * Return the LispGroupPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getLispGroupPanel() {
	if (ivjLispGroupPanel == null) {
	    try {
		ivjLispGroupPanel = new javax.swing.JPanel();
		ivjLispGroupPanel.setName("LispGroupPanel");
		ivjLispGroupPanel.setPreferredSize(new java.awt.Dimension(77, 34));
		ivjLispGroupPanel.setLayout(new java.awt.BorderLayout());
		ivjLispGroupPanel.setMaximumSize(new java.awt.Dimension(2147483647, 2147483647));
		getLispGroupPanel().add(getLispPackageTextField(), "Center");
		getLispGroupPanel().add(getLispPackageLabel(), "North");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjLispGroupPanel;
    }
    /**
     * Return the LispPackageLabel property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getLispPackageLabel() {
	if (ivjLispPackageLabel == null) {
	    try {
		ivjLispPackageLabel = new javax.swing.JLabel();
		ivjLispPackageLabel.setName("LispPackageLabel");
		ivjLispPackageLabel.setText("Lisp Package");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjLispPackageLabel;
    }
    /**
     * Return the LispPackageTextField property value.
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTextField getLispPackageTextField() {
	if (ivjLispPackageTextField == null) {
	    try {
		String[] types = {"Text Holder"};
		ivjLispPackageTextField = new PLJTextField(getParentFrame(), "Lisp Package Text Field", Arrays.asList(types));
		ivjLispPackageTextField.setName("LispPackageTextField");
		ivjLispPackageTextField.setMaximumSize(new java.awt.Dimension(2147483647, 20));
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjLispPackageTextField;
    }
    /**
     * Return the PropositionGroupPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getMiscGroupPanel() {
	if (ivjMiscGroupPanel == null) {
	    try {
		ivjMiscGroupPanel = new javax.swing.JPanel();
		ivjMiscGroupPanel.setName("MiscGroupPanel");
		ivjMiscGroupPanel.setPreferredSize(new java.awt.Dimension(259, 34));
		ivjMiscGroupPanel.setLayout(new java.awt.BorderLayout());
		getMiscGroupPanel().add(getJavaCatchallGroupPanel(), "South");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjMiscGroupPanel;
    }
    /**
     * Return the Name property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getModuleNameLabel() {
	if (ivjModuleNameLabel == null) {
	    try {
		ivjModuleNameLabel = new javax.swing.JLabel();
		ivjModuleNameLabel.setName("ModuleNameLabel");
		ivjModuleNameLabel.setText("Name");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjModuleNameLabel;
    }
    /**
     * Return the ModulePanelBoxLayout property value.
     * @return javax.swing.BoxLayout
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.BoxLayout getModulePanelBoxLayout() {
	javax.swing.BoxLayout ivjModulePanelBoxLayout = null;
	try {
	    /* Create part */
	    ivjModulePanelBoxLayout = new javax.swing.BoxLayout(this, javax.swing.BoxLayout.Y_AXIS);
	} catch (java.lang.Throwable ivjExc) {
	    handleException(ivjExc);
	};
	return ivjModulePanelBoxLayout;
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
		ivjNameDocumentationGroupPanel.setPreferredSize(new java.awt.Dimension(100, 300));
		ivjNameDocumentationGroupPanel.setLayout(getNameDocumentationGroupPanelBoxLayout());
		getNameDocumentationGroupPanel().add(getNameGroupPanel(), getNameGroupPanel().getName());
		getNameDocumentationGroupPanel().add(getVerticalStrut1(), getVerticalStrut1().getName());
		getNameDocumentationGroupPanel().add(getDocumentationGroupPanel(), getDocumentationGroupPanel().getName());
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
    private javax.swing.JPanel getNameDocumentationNamespaceGroupPanel() {
	if (ivjNameDocumentationNamespaceGroupPanel == null) {
	    try {
		ivjNameDocumentationNamespaceGroupPanel = new javax.swing.JPanel();
		ivjNameDocumentationNamespaceGroupPanel.setName("NameDocumentationNamespaceGroupPanel");
		ivjNameDocumentationNamespaceGroupPanel.setPreferredSize(new java.awt.Dimension(250, 120));
		ivjNameDocumentationNamespaceGroupPanel.setLayout(new javax.swing.BoxLayout(getNameDocumentationNamespaceGroupPanel(), javax.swing.BoxLayout.X_AXIS));
		getNameDocumentationNamespaceGroupPanel().add(getNameDocumentationGroupPanel(), getNameDocumentationGroupPanel().getName());
		getNameDocumentationNamespaceGroupPanel().add(getHorizontalStrut1(), getHorizontalStrut1().getName());
		getNameDocumentationNamespaceGroupPanel().add(getNamespaceGroupPanel(), getNamespaceGroupPanel().getName());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjNameDocumentationNamespaceGroupPanel;
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
		ivjNameGroupPanel.setPreferredSize(new java.awt.Dimension(33, 34));
		ivjNameGroupPanel.setLayout(new java.awt.BorderLayout());
		ivjNameGroupPanel.setMaximumSize(new java.awt.Dimension(2147483647, 5));
		getNameGroupPanel().add(getNameTextField(), "Center");
		getNameGroupPanel().add(getModuleNameLabel(), "North");
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
     * Return the SupersGroupPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getNamespaceGroupPanel() {
	if (ivjNamespaceGroupPanel == null) {
	    try {
		ivjNamespaceGroupPanel = new javax.swing.JPanel();
		ivjNamespaceGroupPanel.setName("NamespaceGroupPanel");
		ivjNamespaceGroupPanel.setPreferredSize(new java.awt.Dimension(100, 160));
		ivjNamespaceGroupPanel.setLayout(getNamespaceGroupPanelBoxLayout());
		ivjNamespaceGroupPanel.setMaximumSize(new java.awt.Dimension(2147483647, 2147483647));
		getNamespaceGroupPanel().add(getJavaGroupPanel(), getJavaGroupPanel().getName());
		getNamespaceGroupPanel().add(getJPanel1(), getJPanel1().getName());
		getNamespaceGroupPanel().add(getLispGroupPanel(), getLispGroupPanel().getName());
		getNamespaceGroupPanel().add(getJPanel2(), getJPanel2().getName());
		getNamespaceGroupPanel().add(getCppGroupPanel(), getCppGroupPanel().getName());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjNamespaceGroupPanel;
    }
    /**
     * Return the NamespaceGroupPanelBoxLayout property value.
     * @return javax.swing.BoxLayout
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.BoxLayout getNamespaceGroupPanelBoxLayout() {
	javax.swing.BoxLayout ivjNamespaceGroupPanelBoxLayout = null;
	try {
	    /* Create part */
	    ivjNamespaceGroupPanelBoxLayout = new javax.swing.BoxLayout(getNamespaceGroupPanel(), javax.swing.BoxLayout.Y_AXIS);
	} catch (java.lang.Throwable ivjExc) {
	    handleException(ivjExc);
	};
	return ivjNamespaceGroupPanelBoxLayout;
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
		ivjNameTextField = new PLJTextField(getParentFrame(), "Module Name Text Field", Arrays.asList(types));
		ivjNameTextField.setName("NameTextField");
		ivjNameTextField.setPreferredSize(new java.awt.Dimension(4, 20));
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
    public ModuleFrame getParentFrame() {
	return parentFrame;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/16/2002 4:30:42 PM)
     * @return javax.swing.JCheckBox
     */
    public javax.swing.JCheckBox getPubAPICheckBox() {
	return getAPICheckBox();
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/16/2002 4:33:12 PM)
     * @return javax.swing.JCheckBox
     */
    public javax.swing.JCheckBox getPubCaseSensitiveCheckBox() {
	return getCaseSensitiveCheckBox();
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/16/2002 4:35:46 PM)
     * @return javax.swing.JTextField
     */
    public javax.swing.JTextField getPubCppNamespaceTextField() {
	return getCppNamespaceTextField();
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/16/2002 4:36:52 PM)
     * @return javax.swing.JTextArea
     */
    public javax.swing.JTextArea getPubDocumentationTextArea() {
	return getDocumentationTextArea();
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/16/2002 4:37:25 PM)
     * @return redesign.gui.components.AdderPanel2
     */
    public AdderPanel2 getPubIncludesPanel() {
	return getIncludesPanel();
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/16/2002 4:38:05 PM)
     * @return javax.swing.JTextField
     */
    public javax.swing.JTextField getPubJavaCatchallTextField() {
	return getJavaCatchallTextField();
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/16/2002 4:47:00 PM)
     * @return javax.swing.JTextField
     */
    public javax.swing.JTextField getPubJavaPackageTextField() {
	return getJavaPackageTextField();
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/16/2002 4:39:08 PM)
     * @return javax.swing.JTextField
     */
    public javax.swing.JTextField getPubLispPackageTextField() {
	return getLispPackageTextField();
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/16/2002 4:39:57 PM)
     * @return javax.swing.JTextField
     */
    public javax.swing.JTextField getPubNameTextField() {
	return getNameTextField();
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/16/2002 4:31:25 PM)
     * @return redesign.gui.components.AdderPanel2
     */
    public AdderPanel2 getPubShadowPanel() {
	return getShadowPanel();
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/16/2002 4:32:15 PM)
     * @return redesign.gui.components.AdderPanel2
     */
    public AdderPanel2 getPubUsesPanel() {
	return getUsesPanel();
    }
    /**
     * Return the RuleGroupPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getShadowGroupPanel() {
	if (ivjShadowGroupPanel == null) {
	    try {
		ivjShadowGroupPanel = new javax.swing.JPanel();
		ivjShadowGroupPanel.setName("ShadowGroupPanel");
		ivjShadowGroupPanel.setPreferredSize(new java.awt.Dimension(259, 100));
		ivjShadowGroupPanel.setLayout(new java.awt.BorderLayout());
		getShadowGroupPanel().add(getShadowLabel(), "North");
		getShadowGroupPanel().add(getShadowPanel(), "Center");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjShadowGroupPanel;
    }
    /**
     * Return the Rules property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getShadowLabel() {
	if (ivjShadowLabel == null) {
	    try {
		ivjShadowLabel = new javax.swing.JLabel();
		ivjShadowLabel.setName("ShadowLabel");
		ivjShadowLabel.setText("Shadowing Symbols");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjShadowLabel;
    }
    /**
     * Return the RulePanel property value.
     * @return redesign.gui.components.AdderPanel2
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private AdderPanel2 getShadowPanel() {
	if (ivjShadowPanel == null) {
	    try {
		ivjShadowPanel = new edu.isi.powerloom.gui.components.AdderPanel2(null);
		ivjShadowPanel.setName("ShadowPanel");
		ivjShadowPanel.setText("RulePanel");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjShadowPanel;
    }
    /**
     * Return the UsesGroupPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getUsesGroupPanel() {
	if (ivjUsesGroupPanel == null) {
	    try {
		ivjUsesGroupPanel = new javax.swing.JPanel();
		ivjUsesGroupPanel.setName("UsesGroupPanel");
		ivjUsesGroupPanel.setLayout(new java.awt.BorderLayout());
		getUsesGroupPanel().add(getUsesLabel(), "North");
		getUsesGroupPanel().add(getUsesPanel(), "Center");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjUsesGroupPanel;
    }
    /**
     * Return the UsesLabel property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getUsesLabel() {
	if (ivjUsesLabel == null) {
	    try {
		ivjUsesLabel = new javax.swing.JLabel();
		ivjUsesLabel.setName("UsesLabel");
		ivjUsesLabel.setText("Uses");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjUsesLabel;
    }
    /**
     * Return the UsesPanel property value.
     * @return redesign.gui.components.AdderPanel2
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private AdderPanel2 getUsesPanel() {
	if (ivjUsesPanel == null) {
	    try {
		ivjUsesPanel = new edu.isi.powerloom.gui.components.AdderPanel2(null);
		ivjUsesPanel.setName("UsesPanel");
		ivjUsesPanel.setText("UsesPanel");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjUsesPanel;
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
     * Comment
     */
    public void includesPanel_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	if (actionEvent.getActionCommand().equals("add")) {
	    try {
		debugPrintln(3, "add button pressed.");
		ModuleChooserDialog chooser = new ModuleChooserDialog();
		chooser.setModal(true);
		chooser.setTitle("Choose Module");
		chooser.getPubChooserPanel().getEnterLabel().setText("Enter Module Name:");
		chooser.show();
		if (chooser.getPubChooserPanel().getResult() != null) {
		    getIncludesPanel().addPLObject(chooser.getPubChooserPanel().getResult());
		}
	    } catch (Exception e) {
		handleException(e);
	    } // end of try-catch
	} else if (actionEvent.getActionCommand().equals("delete")) {
	    debugPrintln(3, "delete button pressed.");
	    PLObject selected = (PLObject)getIncludesPanel().getJList().getSelectedValue();
	    if (selected != null) {
		getIncludesPanel().removePLObject(selected);
	    }
	} else {
	    debugPrintln(3, "Uknown action command: " + actionEvent.getActionCommand());
	}

    }
    /**
     * Initializes connections
     * @exception java.lang.Exception The exception description.
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void initConnections() throws java.lang.Exception {
	// user code begin {1}
	// user code end
	getIncludesPanel().addActionListener(ivjEventHandler);
	getUsesPanel().addActionListener(ivjEventHandler);
	getShadowPanel().addActionListener(ivjEventHandler);
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
	    setName("ModulePanel");
	    setLayout(getModulePanelBoxLayout());
	    setSize(592, 560);
	    add(getNameDocumentationNamespaceGroupPanel(), getNameDocumentationNamespaceGroupPanel().getName());
	    add(getVerticalStrut12(), getVerticalStrut12().getName());
	    add(getIncludesUsesGroupPanel(), getIncludesUsesGroupPanel().getName());
	    add(getCheckboxGroupPanel(), getCheckboxGroupPanel().getName());
	    add(getMiscGroupPanel(), getMiscGroupPanel().getName());
	    add(getVerticalStrut22(), getVerticalStrut22().getName());
	    add(getShadowGroupPanel(), getShadowGroupPanel().getName());
	    add(getButtonGroupPanel(), getButtonGroupPanel().getName());
	    initConnections();

	    ActionListener nameFieldAction = new ActionListener() {
		    public void actionPerformed(ActionEvent e) {
			debugPrintln(3, "You hit return in the name field.");
			ModulePanel.this.oKButton_ActionPerformed1(e);
		    }
		};
	    getPubNameTextField().addActionListener(nameFieldAction);

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
	getParentFrame().commitModule();
	getParentFrame().doDefaultCloseAction();
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/13/2002 12:26:55 PM)
     * @param newParentFrame javax.swing.JInternalFrame
     */
    public void setParentFrame(ModuleFrame newParentFrame) {
	parentFrame = newParentFrame;
    }
    /**
     * Comment
     */
    public void shadowPanel_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	if (actionEvent.getActionCommand().equals("add")) {
	    String name = javax.swing.JOptionPane. showInputDialog(this, "Enter Name:", "Enter Shadowing Symbol Name",
								   javax.swing.JOptionPane.QUESTION_MESSAGE);
	    if (name != null) {
		PLSymbol surrogateName = new PLSymbol();
		surrogateName.attrSymbolName = name;
		surrogateName.setUndefined(true);
		getShadowPanel().addPLObject(surrogateName);
	    }
	} else if (actionEvent.getActionCommand().equals("delete")) {
	    debugPrintln(3, "delete button pressed.");
	    PLObject selected = (PLObject)getShadowPanel().getJList().getSelectedValue();
	    if (selected != null) {
		getShadowPanel().removePLObject(selected);
	    }
	}
	
    }
    /**
     * Comment
     */
    public void usesPanel_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	if (actionEvent.getActionCommand().equals("add")) {
	    try {
		debugPrintln(3, "add button pressed.");
		ModuleChooserDialog chooser = new ModuleChooserDialog();
		chooser.setModal(true);
		chooser.setTitle("Choose Module");
		chooser.getPubChooserPanel().getEnterLabel().setText("Enter Module Name:");
		chooser.show();
		if (chooser.getPubChooserPanel().getResult() != null) {
		    getUsesPanel().addPLObject(chooser.getPubChooserPanel().getResult());
		}
	    } catch (Exception e) {
		handleException(e);
	    } // end of try-catch
	} else if (actionEvent.getActionCommand().equals("delete")) {
	    debugPrintln(3, "delete button pressed.");
	    PLObject selected = (PLObject)getUsesPanel().getJList().getSelectedValue();
	    if (selected != null) {
		getUsesPanel().removePLObject(selected);
	    }
	} else {
	    debugPrintln(3, "Uknown action command: " + actionEvent.getActionCommand());
	}

    }
}
