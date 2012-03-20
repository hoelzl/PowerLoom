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


// Version: SearchFrame.java,v 1.15 2010/02/04 05:19:35 hans Exp

package edu.isi.powerloom.gui.components;

import edu.isi.powerloom.gui.serverinterface.*;
import edu.isi.powerloom.gui.xmlobject.*;
import static edu.isi.powerloom.gui.common.Utils.*;

import java.util.*;
import javax.swing.event.*;
import javax.swing.*;
import javax.swing.table.*;
import java.awt.*;
import java.awt.event.*;

/**
 * Frame used for KB search.
 *
 * @since 6/6/2002 6:43:59 PM
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */
public class SearchFrame extends PLFrame {
    private javax.swing.JPanel ivjButtonPanel = null;
    private javax.swing.JButton ivjCancelButton = null;
    private javax.swing.JCheckBox ivjCaseSensitiveCheckBox = null;
    private javax.swing.JCheckBox ivjConceptsCheckBox = null;
    private javax.swing.JPanel ivjInputPanel = null;
    private javax.swing.JCheckBox ivjInstancesCheckBox = null;
    private javax.swing.JRadioButton ivjMatchBeginRadioButton = null;
    private javax.swing.JRadioButton ivjMatchEndRadioButton = null;
    private javax.swing.JRadioButton ivjExactMatchRadioButton = null;
    private javax.swing.JRadioButton ivjPartialMatchRadioButton = null;
    private javax.swing.JPanel ivjJInternalFrameContentPane = null;
    private javax.swing.BoxLayout ivjJInternalFrameContentPaneBoxLayout = null;
    private javax.swing.JPanel ivjMatchOptionsPanel = null;
    private javax.swing.JPanel ivjMiscOptionsPanel = null;
    private javax.swing.BoxLayout ivjMiscOptionsPanelBoxLayout = null;
    private javax.swing.JComboBox ivjModuleComboBox = null;
    private javax.swing.JLabel ivjModuleLabel = null;
    private javax.swing.JPanel ivjModulePanel = null;
    private javax.swing.JPanel ivjObjectTypePanel = null;
    private javax.swing.BoxLayout ivjObjectTypePanelBoxLayout = null;
    private javax.swing.JButton ivjOKButton = null;
    private javax.swing.JPanel ivjOptionsPanel = null;
    private javax.swing.JCheckBox ivjRelationsCheckBox = null;
    private javax.swing.JPanel ivjResultsPanel = null;
    private javax.swing.JScrollPane ivjResultsScrollPane = null;
    private javax.swing.JTable ivjResultsTable = null;
    private javax.swing.JLabel ivjSearchStringLabel = null;
    IvjEventHandler ivjEventHandler = new IvjEventHandler();
    private PowerloomApp app;
    private BrowserFrame4 searchBrowser;

    class IvjEventHandler implements java.awt.event.ActionListener {
	public void actionPerformed(java.awt.event.ActionEvent e) {
	    if (e.getSource() == SearchFrame.this.getOKButton()) 
		connEtoC1(e);
	    if (e.getSource() == SearchFrame.this.getCancelButton()) 
		connEtoC2(e);
	};
    };
    private javax.swing.JTextField ivjSearchTextField = null;
    /**
     * SearchFrame constructor comment.
     */
    public SearchFrame() {
	super();
	initialize();
    }

    public Icon getFrameIcon() {
	return PowerloomApp.getInstance().getImage("resources/images/find.gif");
    }


    public SearchFrame(BrowserFrame4 searchBrowser, boolean conceptsChecked, boolean relationsChecked, boolean instancesChecked) {
	super();
	initialize();
	this.searchBrowser = searchBrowser;
	getConceptsCheckBox().setSelected(conceptsChecked);
	getRelationsCheckBox().setSelected(relationsChecked);
	getInstancesCheckBox().setSelected(instancesChecked);
    }
                   
    /**
     * SearchFrame constructor comment.
     * @param title java.lang.String
     */
    public SearchFrame(String title) {
	super(title);
    }
    /**
     * SearchFrame constructor comment.
     * @param title java.lang.String
     * @param resizable boolean
     */
    public SearchFrame(String title, boolean resizable) {
	super(title, resizable);
    }
    /**
     * SearchFrame constructor comment.
     * @param title java.lang.String
     * @param resizable boolean
     * @param closable boolean
     */
    public SearchFrame(String title, boolean resizable, boolean closable) {
	super(title, resizable, closable);
    }
    /**
     * SearchFrame constructor comment.
     * @param title java.lang.String
     * @param resizable boolean
     * @param closable boolean
     * @param maximizable boolean
     */
    public SearchFrame(String title, boolean resizable, boolean closable, boolean maximizable) {
	super(title, resizable, closable, maximizable);
    }
    /**
     * SearchFrame constructor comment.
     * @param title java.lang.String
     * @param resizable boolean
     * @param closable boolean
     * @param maximizable boolean
     * @param iconifiable boolean
     */
    public SearchFrame(String title, boolean resizable, boolean closable, boolean maximizable, boolean iconifiable) {
	super(title, resizable, closable, maximizable, iconifiable);
    }
    /**
     * Comment
     */
    public void cancelButton_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	doDefaultCloseAction();
    }
    /**
     * connEtoC1:  (OKButton.action.actionPerformed(java.awt.event.ActionEvent) --> SearchFrame.oKButton_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
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
     * connEtoC2:  (CancelButton.action.actionPerformed(java.awt.event.ActionEvent) --> SearchFrame.cancelButton_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
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
     * Insert the method's description here.
     * Creation date: (6/7/2002 10:00:14 AM)
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
     * Return the CaseSensitiveCheckBox property value.
     * @return javax.swing.JCheckBox
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JCheckBox getCaseSensitiveCheckBox() {
	if (ivjCaseSensitiveCheckBox == null) {
	    try {
		ivjCaseSensitiveCheckBox = new javax.swing.JCheckBox();
		ivjCaseSensitiveCheckBox.setName("CaseSensitiveCheckBox");
		ivjCaseSensitiveCheckBox.setText("Case Sensitive");
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
     * Return the ConceptsCheckBox property value.
     * @return javax.swing.JCheckBox
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JCheckBox getConceptsCheckBox() {
	if (ivjConceptsCheckBox == null) {
	    try {
		ivjConceptsCheckBox = new javax.swing.JCheckBox();
		ivjConceptsCheckBox.setName("ConceptsCheckBox");
		ivjConceptsCheckBox.setText("Concepts");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjConceptsCheckBox;
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
		ivjInputPanel.setLayout(new java.awt.FlowLayout());
		getInputPanel().add(getSearchStringLabel(), getSearchStringLabel().getName());
		getInputPanel().add(getSearchTextField(), getSearchTextField().getName());
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
     * Return the InstancesCheckBox property value.
     * @return javax.swing.JCheckBox
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JCheckBox getInstancesCheckBox() {
	if (ivjInstancesCheckBox == null) {
	    try {
		ivjInstancesCheckBox = new javax.swing.JCheckBox();
		ivjInstancesCheckBox.setName("InstancesCheckBox");
		ivjInstancesCheckBox.setText("Instances");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjInstancesCheckBox;
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
		ivjJInternalFrameContentPane.setLayout(getJInternalFrameContentPaneBoxLayout());
		getJInternalFrameContentPane().add(getInputPanel(), getInputPanel().getName());
		getJInternalFrameContentPane().add(getModulePanel(), getModulePanel().getName());
		getJInternalFrameContentPane().add(getOptionsPanel(), getOptionsPanel().getName());
		getJInternalFrameContentPane().add(getResultsPanel(), getResultsPanel().getName());
		getJInternalFrameContentPane().add(getButtonPanel(), getButtonPanel().getName());
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
     * Return the JInternalFrameContentPaneBoxLayout property value.
     * @return javax.swing.BoxLayout
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.BoxLayout getJInternalFrameContentPaneBoxLayout() {
	javax.swing.BoxLayout ivjJInternalFrameContentPaneBoxLayout = null;
	try {
	    /* Create part */
	    ivjJInternalFrameContentPaneBoxLayout = new javax.swing.BoxLayout(getJInternalFrameContentPane(), javax.swing.BoxLayout.Y_AXIS);
	} catch (java.lang.Throwable ivjExc) {
	    handleException(ivjExc);
	};
	return ivjJInternalFrameContentPaneBoxLayout;
    }


    private javax.swing.JRadioButton getPartialMatchRadioButton() {
	if (ivjPartialMatchRadioButton == null) {
	    try {
		ivjPartialMatchRadioButton = new javax.swing.JRadioButton();
		ivjPartialMatchRadioButton.setName("PartialMatchRadioButton");
		ivjPartialMatchRadioButton.setText("Contains");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjPartialMatchRadioButton;
    }

    private javax.swing.JRadioButton getMatchBeginRadioButton() {
	if (ivjMatchBeginRadioButton == null) {
	    try {
		ivjMatchBeginRadioButton = new javax.swing.JRadioButton();
		ivjMatchBeginRadioButton.setName("MatchBeginRadioButton");
		ivjMatchBeginRadioButton.setText("Starts With");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjMatchBeginRadioButton;
    }

    private javax.swing.JRadioButton getMatchEndRadioButton() {
	if (ivjMatchEndRadioButton == null) {
	    try {
		ivjMatchEndRadioButton = new javax.swing.JRadioButton();
		ivjMatchEndRadioButton.setName("MatchEndRadioButton");
		ivjMatchEndRadioButton.setText("Ends With");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjMatchEndRadioButton;
    }

    private javax.swing.JRadioButton getExactMatchRadioButton() {
	if (ivjExactMatchRadioButton == null) {
	    try {
		ivjExactMatchRadioButton = new javax.swing.JRadioButton();
		ivjExactMatchRadioButton.setName("ExactMatchRadioButton");
		ivjExactMatchRadioButton.setText("Exactly Matches");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjExactMatchRadioButton;
    }


    private javax.swing.JPanel getMatchOptionsPanel() {
	if (ivjMatchOptionsPanel == null) {
	    try {
		ivjMatchOptionsPanel = new javax.swing.JPanel();
		ivjMatchOptionsPanel.setName("MatchOptionsPanel");
		ivjMatchOptionsPanel.setLayout(new javax.swing.BoxLayout(ivjMatchOptionsPanel, javax.swing.BoxLayout.Y_AXIS));
		getMatchOptionsPanel().add(getPartialMatchRadioButton());
		getMatchOptionsPanel().add(getMatchBeginRadioButton());
		getMatchOptionsPanel().add(getMatchEndRadioButton());
		getMatchOptionsPanel().add(getExactMatchRadioButton());
		ButtonGroup bg = new ButtonGroup();
		bg.add(getPartialMatchRadioButton());
		bg.add(getMatchBeginRadioButton());
		bg.add(getMatchEndRadioButton());
		bg.add(getExactMatchRadioButton());
		getPartialMatchRadioButton().setSelected(true);
	    } catch (java.lang.Throwable ivjExc) {
		handleException(ivjExc);
	    }
	}
	return ivjMatchOptionsPanel;
    }


    /**
     * Return the MiscOptionsPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getMiscOptionsPanel() {
	if (ivjMiscOptionsPanel == null) {
	    try {
		ivjMiscOptionsPanel = new javax.swing.JPanel();
		ivjMiscOptionsPanel.setName("MiscOptionsPanel");
		ivjMiscOptionsPanel.setLayout(getMiscOptionsPanelBoxLayout());
		getMiscOptionsPanel().add(getCaseSensitiveCheckBox(), getCaseSensitiveCheckBox().getName());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjMiscOptionsPanel;
    }
    /**
     * Return the MiscOptionsPanelBoxLayout property value.
     * @return javax.swing.BoxLayout
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.BoxLayout getMiscOptionsPanelBoxLayout() {
	javax.swing.BoxLayout ivjMiscOptionsPanelBoxLayout = null;
	try {
	    /* Create part */
	    ivjMiscOptionsPanelBoxLayout = new javax.swing.BoxLayout(getMiscOptionsPanel(), javax.swing.BoxLayout.Y_AXIS);
	} catch (java.lang.Throwable ivjExc) {
	    handleException(ivjExc);
	};
	return ivjMiscOptionsPanelBoxLayout;
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
     * Return the ObjectTypePanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getObjectTypePanel() {
	if (ivjObjectTypePanel == null) {
	    try {
		ivjObjectTypePanel = new javax.swing.JPanel();
		ivjObjectTypePanel.setName("ObjectTypePanel");
		ivjObjectTypePanel.setLayout(getObjectTypePanelBoxLayout());
		getObjectTypePanel().add(getConceptsCheckBox(), getConceptsCheckBox().getName());
		getObjectTypePanel().add(getRelationsCheckBox(), getRelationsCheckBox().getName());
		getObjectTypePanel().add(getInstancesCheckBox(), getInstancesCheckBox().getName());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjObjectTypePanel;
    }
    /**
     * Return the ObjectTypePanelBoxLayout property value.
     * @return javax.swing.BoxLayout
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.BoxLayout getObjectTypePanelBoxLayout() {
	javax.swing.BoxLayout ivjObjectTypePanelBoxLayout = null;
	try {
	    /* Create part */
	    ivjObjectTypePanelBoxLayout = new javax.swing.BoxLayout(getObjectTypePanel(), javax.swing.BoxLayout.Y_AXIS);
	} catch (java.lang.Throwable ivjExc) {
	    handleException(ivjExc);
	};
	return ivjObjectTypePanelBoxLayout;
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
     * Return the OptionsPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getOptionsPanel() {
	if (ivjOptionsPanel == null) {
	    try {
		ivjOptionsPanel = new javax.swing.JPanel();
		ivjOptionsPanel.setName("OptionsPanel");
		ivjOptionsPanel.setLayout(new javax.swing.BoxLayout(getOptionsPanel(), javax.swing.BoxLayout.X_AXIS));
		getOptionsPanel().add(getObjectTypePanel(), getObjectTypePanel().getName());
		getOptionsPanel().add(getMatchOptionsPanel());
		getOptionsPanel().add(getMiscOptionsPanel(), getMiscOptionsPanel().getName());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjOptionsPanel;
    }
    /**
     * Return the RelationsCheckBox property value.
     * @return javax.swing.JCheckBox
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JCheckBox getRelationsCheckBox() {
	if (ivjRelationsCheckBox == null) {
	    try {
		ivjRelationsCheckBox = new javax.swing.JCheckBox();
		ivjRelationsCheckBox.setName("RelationsCheckBox");
		ivjRelationsCheckBox.setText("Relations");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjRelationsCheckBox;
    }
    /**
     * Return the ResultsPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getResultsPanel() {
	if (ivjResultsPanel == null) {
	    try {
		ivjResultsPanel = new javax.swing.JPanel();
		ivjResultsPanel.setName("ResultsPanel");
		ivjResultsPanel.setLayout(new java.awt.BorderLayout());
		getResultsPanel().add(getResultsScrollPane(), "Center");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjResultsPanel;
    }
    /**
     * Return the ResultsScrollPane property value.
     * @return javax.swing.JScrollPane
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JScrollPane getResultsScrollPane() {
	if (ivjResultsScrollPane == null) {
	    try {
		ivjResultsScrollPane = new javax.swing.JScrollPane();
		ivjResultsScrollPane.setName("ResultsScrollPane");
		ivjResultsScrollPane.setVerticalScrollBarPolicy(javax.swing.JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
		ivjResultsScrollPane.setHorizontalScrollBarPolicy(javax.swing.JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
		getResultsScrollPane().setViewportView(getResultsTable());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjResultsScrollPane;
    }

    BrowserFrame4 getSearchBrowser() {
	if ((searchBrowser == null) || (!searchBrowser.isVisible())) {
	    getApp().browseMenuItem_ActionPerformed(null);
	    searchBrowser = getApp().getSelectedBrowserFrame();
	}
	return searchBrowser;
    }


    // Configure navigation window so that the selected object is displayed
    void navigateToSelection() {
	try {
	    TableModel tableModel = getResultsTable().getModel();
	    int selectionIndex = getResultsTable().getSelectionModel().getLeadSelectionIndex();
	    if (selectionIndex > -1) {
		PLObject object = (PLObject)tableModel.getValueAt(selectionIndex, 0);
		debugPrintln(3, "You selected: " + object);
		String moduleName = (String)tableModel.getValueAt(selectionIndex, 1);
		PLSurrogate moduleSurrogate = KnowledgeManager.getInstance().findOrCreateSurrogate(PLModule.class, moduleName);
		PLModule module = (PLModule)moduleSurrogate.getValue();
		//debugPrintln(3, "moduleName = " + ", moduleSurrogate = " + moduleSurrogate + ", module = " + module);
		getSearchBrowser().getPubBrowserPanel().getPowerloomTrees().makeObjectVisible(module, object, true);
	    }
	}  catch (Exception e) {
	    handleException(e);
	}
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
		getResultsScrollPane().setColumnHeaderView(ivjResultsTable.getTableHeader());
		getResultsScrollPane().getViewport().setBackingStoreEnabled(true);
		ivjResultsTable.setBounds(0, 0, 200, 200);
		// user code begin {1}
		ListSelectionListener selectionListener = 
		    (new ListSelectionListener() { 
			    public void valueChanged(ListSelectionEvent lse) {
				//debugPrintln(3, "Selection event: " + lse); 
				if (!lse.getValueIsAdjusting()) {
				    navigateToSelection();
				}
			    }
			});
		ivjResultsTable.getSelectionModel().addListSelectionListener(selectionListener);
		// setup table header sorting listener
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
			    ((SearchTableModel)ivjResultsTable.getModel()).sortColumn(modelIndex);
				    
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
     * Return the SearchStringLabel property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getSearchStringLabel() {
	if (ivjSearchStringLabel == null) {
	    try {
		ivjSearchStringLabel = new javax.swing.JLabel();
		ivjSearchStringLabel.setName("SearchStringLabel");
		ivjSearchStringLabel.setText("Search String:");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjSearchStringLabel;
    }
    /**
     * Return the JTextField1 property value.
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTextField getSearchTextField() {
	if (ivjSearchTextField == null) {
	    try {
		String[] types = {"Text Holder"};
		ivjSearchTextField = new PLJTextField(this, "Search Input Field", Arrays.asList(types));
		ivjSearchTextField.setName("SearchTextField");
		ivjSearchTextField.setColumns(20);
		// user code begin {1}
		ivjSearchTextField.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
			    oKButton_ActionPerformed(e);
			}
		    });
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjSearchTextField;
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
	    setName("SearchFrame");
	    setTitle("Search");
	    setClosable(true);
	    setIconifiable(true);
	    setSize(456, 426);
	    setMaximizable(true);
	    setResizable(true);
	    setContentPane(getJInternalFrameContentPane());
	    initConnections();
	    app = PowerloomApp.getInstance();
	    edu.isi.powerloom.gui.xmlobject.PLSurrogateContainer modules = 
		KnowledgeManager.getInstance().getModules().listifyTreeContainer();
	    PLSurrogate allModulesSurrogate = (PLSurrogate)KnowledgeManager.getInstance().findOrCreateSurrogate(PLModule.class, "ALL");
	    if (allModulesSurrogate.getValue() == null) {
		PLModule allModules = new PLModule();
		allModules.attrModuleName = "ALL";
		allModulesSurrogate.setValue(allModules);
	    }
	    modules.getSurrogates().add(allModulesSurrogate);
	    PLListModel model = new PLListModel(modules);
	    getModuleComboBox().setModel(model);
	    PLModule selectedModule = getApp().getMostRecentlyTouchedModule();
	    if (selectedModule != null) {
		getModuleComboBox().setSelectedItem(selectedModule);
	    } else {
		getModuleComboBox().setSelectedItem(allModulesSurrogate.getValue());
	    }

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
	    SearchFrame aSearchFrame;
	    aSearchFrame = new SearchFrame();
	    frame.setContentPane(aSearchFrame);
	    frame.setSize(aSearchFrame.getSize());
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
    /**
     * Comment
     */
    public void oKButton_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	try {
	    PLModule module = (PLModule)getModuleComboBox().getSelectedItem();
	    String moduleName = module.attrModuleName;
	    String searchString = getSearchTextField().getText();
	    boolean isConcept = getConceptsCheckBox().isSelected();
	    boolean isRelation = getRelationsCheckBox().isSelected();
	    boolean isInstance = getInstancesCheckBox().isSelected();
	    boolean isCaseSensitive = getCaseSensitiveCheckBox().isSelected();
	    if (getPartialMatchRadioButton().isSelected()) {
		searchString = "*" + searchString + "*";
	    } else if (getMatchBeginRadioButton().isSelected()) {
		searchString = searchString + "*";
	    } else if (getMatchEndRadioButton().isSelected()) {
		searchString = "*" + searchString;
	    }
	    PLSearchResult searchResult = KnowledgeManager.getInstance().
		executeSearch(moduleName, searchString, isConcept, isRelation, isInstance, isCaseSensitive);
	    System.out.println(searchResult);
	    SearchTableModel tableModel = new SearchTableModel(searchResult);
	    getResultsTable().setModel(tableModel);
	    tableModel.fireTableChanged(null);
	} catch (Exception e) {
	    handleException(e);
	} // end of try-catch
    }

    /**
     * Implementation of PLFrame
     */
    public PLModule getModule() {
	PLModule module = (PLModule)getModuleComboBox().getSelectedItem();
	return module;
    }
}
