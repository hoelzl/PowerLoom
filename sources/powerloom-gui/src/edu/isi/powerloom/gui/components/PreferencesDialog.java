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


// Version: PreferencesDialog.java,v 1.5 2010/02/04 05:19:00 hans Exp

package edu.isi.powerloom.gui.components;

import edu.isi.powerloom.gui.common.*;
import static edu.isi.powerloom.gui.common.Utils.*;

/**
 * Dialog for editing application preferences.
 *
 * @since 4/29/2002 6:52:27 AM
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */
public class PreferencesDialog extends javax.swing.JDialog {
    private javax.swing.JCheckBox ivjBrowseOnStartCheckBox = null;
    private javax.swing.JPanel ivjButtonPanel = null;
    private javax.swing.JButton ivjCancelButton = null;
    private javax.swing.JPanel ivjJDialogContentPane = null;
    private javax.swing.JButton ivjOKButton = null;
    private javax.swing.JPanel ivjPreferencePanel = null;
    private java.awt.GridLayout ivjPreferencePanelGridLayout = null;
    private javax.swing.JCheckBox ivjSavePreferencesCheckBox = null;
    private PreferencesResult result;

    class IvjEventHandler implements java.awt.event.ActionListener {
	public void actionPerformed(java.awt.event.ActionEvent e) {
	    if (e.getSource() == PreferencesDialog.this.getOKButton()) 
		connEtoC1(e);
	    if (e.getSource() == PreferencesDialog.this.getCancelButton()) 
		connEtoC2(e);
	};
    };
    IvjEventHandler ivjEventHandler = new IvjEventHandler();
    /**
     * PreferencesDialog constructor comment.
     */
    public PreferencesDialog() {
	super();
	initialize();
    }
    /**
     * PreferencesDialog constructor comment.
     * @param owner java.awt.Dialog
     */
    public PreferencesDialog(java.awt.Dialog owner) {
	super(owner);
    }
    /**
     * PreferencesDialog constructor comment.
     * @param owner java.awt.Dialog
     * @param title java.lang.String
     */
    public PreferencesDialog(java.awt.Dialog owner, String title) {
	super(owner, title);
    }
    /**
     * PreferencesDialog constructor comment.
     * @param owner java.awt.Dialog
     * @param title java.lang.String
     * @param modal boolean
     */
    public PreferencesDialog(java.awt.Dialog owner, String title, boolean modal) {
	super(owner, title, modal);
    }
    /**
     * PreferencesDialog constructor comment.
     * @param owner java.awt.Dialog
     * @param modal boolean
     */
    public PreferencesDialog(java.awt.Dialog owner, boolean modal) {
	super(owner, modal);
    }
    /**
     * PreferencesDialog constructor comment.
     * @param owner java.awt.Frame
     */
    public PreferencesDialog(java.awt.Frame owner) {
	super(owner);
    }
    /**
     * PreferencesDialog constructor comment.
     * @param owner java.awt.Frame
     * @param title java.lang.String
     */
    public PreferencesDialog(java.awt.Frame owner, String title) {
	super(owner, title);
    }
    /**
     * PreferencesDialog constructor comment.
     * @param owner java.awt.Frame
     * @param title java.lang.String
     * @param modal boolean
     */
    public PreferencesDialog(java.awt.Frame owner, String title, boolean modal) {
	super(owner, title, modal);
    }
    /**
     * PreferencesDialog constructor comment.
     * @param owner java.awt.Frame
     * @param modal boolean
     */
    public PreferencesDialog(java.awt.Frame owner, boolean modal) {
	super(owner, modal);
    }
    /**
     * Comment
     */
    public void cancelButton_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	setVisible(false);
    }
    /**
     * connEtoC1:  (OKButton.action.actionPerformed(java.awt.event.ActionEvent) --> PreferencesDialog.oKButton_ActionPerformed1(Ljava.awt.event.ActionEvent;)V)
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
     * connEtoC2:  (CancelButton.action.actionPerformed(java.awt.event.ActionEvent) --> PreferencesDialog.cancelButton_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
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
     * Return the BrowseOnStartCheckBox property value.
     * @return javax.swing.JCheckBox
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JCheckBox getBrowseOnStartCheckBox() {
	if (ivjBrowseOnStartCheckBox == null) {
	    try {
		ivjBrowseOnStartCheckBox = new javax.swing.JCheckBox();
		ivjBrowseOnStartCheckBox.setName("BrowseOnStartCheckBox");
		ivjBrowseOnStartCheckBox.setText("Open Browser on Startup");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjBrowseOnStartCheckBox;
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
     * Return the JDialogContentPane property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getJDialogContentPane() {
	if (ivjJDialogContentPane == null) {
	    try {
		ivjJDialogContentPane = new javax.swing.JPanel();
		ivjJDialogContentPane.setName("JDialogContentPane");
		ivjJDialogContentPane.setLayout(new java.awt.BorderLayout());
		getJDialogContentPane().add(getPreferencePanel(), "Center");
		getJDialogContentPane().add(getButtonPanel(), "South");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjJDialogContentPane;
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
     * Return the PreferencePanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getPreferencePanel() {
	if (ivjPreferencePanel == null) {
	    try {
		ivjPreferencePanel = new javax.swing.JPanel();
		ivjPreferencePanel.setName("PreferencePanel");
		ivjPreferencePanel.setBorder(new javax.swing.border.EmptyBorder(10,10,10,10));
		ivjPreferencePanel.setLayout(getPreferencePanelGridLayout());
		getPreferencePanel().add(getBrowseOnStartCheckBox(), getBrowseOnStartCheckBox().getName());
		getPreferencePanel().add(getSavePreferencesCheckBox(), getSavePreferencesCheckBox().getName());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjPreferencePanel;
    }
    /**
     * Return the PreferencePanelGridLayout property value.
     * @return java.awt.GridLayout
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private java.awt.GridLayout getPreferencePanelGridLayout() {
	java.awt.GridLayout ivjPreferencePanelGridLayout = null;
	try {
	    /* Create part */
	    ivjPreferencePanelGridLayout = new java.awt.GridLayout(2, 1);
	} catch (java.lang.Throwable ivjExc) {
	    handleException(ivjExc);
	};
	return ivjPreferencePanelGridLayout;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/29/2002 7:07:02 AM)
     * @return redesign.gui.components.PreferencesResult
     */
    public PreferencesResult getResult() {
	return result;
    }
    /**
     * Return the SavePreferencesCheckBox property value.
     * @return javax.swing.JCheckBox
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JCheckBox getSavePreferencesCheckBox() {
	if (ivjSavePreferencesCheckBox == null) {
	    try {
		ivjSavePreferencesCheckBox = new javax.swing.JCheckBox();
		ivjSavePreferencesCheckBox.setName("SavePreferencesCheckBox");
		ivjSavePreferencesCheckBox.setSelected(true);
		ivjSavePreferencesCheckBox.setText("Save Preferences");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjSavePreferencesCheckBox;
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
	    setName("PreferencesDialog");
	    setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
	    setSize(352, 119);
	    setTitle("Preferences");
	    setContentPane(getJDialogContentPane());
	    initConnections();
	} catch (java.lang.Throwable ivjExc) {
	    handleException(ivjExc);
	}
	// user code begin {2}
	/* Center frame on the screen */
	/* Calculate the screen size */
	java.awt.Dimension screenSize = java.awt.Toolkit.getDefaultToolkit().getScreenSize();		
	java.awt.Dimension frameSize = getSize();
	if (frameSize.height > screenSize.height)
	    frameSize.height = screenSize.height;
	if (frameSize.width > screenSize.width)
	    frameSize.width = screenSize.width;
	setLocation((screenSize.width - frameSize.width) / 2, (screenSize.height - frameSize.height) / 2);

	setupDialog();
	// user code end
    }
    /**
     * main entrypoint - starts the part when it is run as an application
     * @param args java.lang.String[]
     */
    public static void main(java.lang.String[] args) {
	try {
	    PreferencesDialog aPreferencesDialog;
	    aPreferencesDialog = new PreferencesDialog();
	    aPreferencesDialog.setModal(true);
	    aPreferencesDialog.addWindowListener(new java.awt.event.WindowAdapter() {
		    public void windowClosing(java.awt.event.WindowEvent e) {
			exitGui(0);
		    };
		});
	    aPreferencesDialog.show();
	    java.awt.Insets insets = aPreferencesDialog.getInsets();
	    aPreferencesDialog.setSize(aPreferencesDialog.getWidth() + insets.left + insets.right, aPreferencesDialog.getHeight() + insets.top + insets.bottom);
	    aPreferencesDialog.setVisible(true);
	} catch (Throwable exception) {
	    System.err.println("Exception occurred in main() of javax.swing.JDialog");
	    exception.printStackTrace(System.out);
	}
    }
    /**
     * Comment
     */
    public void oKButton_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	result = new PreferencesResult();
	result.setBrowseOnStartup(getBrowseOnStartCheckBox().isSelected());
	result.setSavePreferences(getSavePreferencesCheckBox().isSelected());
	setVisible(false);
    }
    /**
     * Comment
     */
    public void oKButton_ActionPerformed1(java.awt.event.ActionEvent actionEvent) {
	result = new PreferencesResult();
	result.setBrowseOnStartup(getBrowseOnStartCheckBox().isSelected());
	result.setSavePreferences(getSavePreferencesCheckBox().isSelected());
	setVisible(false);
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/29/2002 7:07:02 AM)
     * @param newResult redesign.gui.components.PreferencesResult
     */
    public void setResult(PreferencesResult newResult) {
	result = newResult;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/29/2002 7:53:17 AM)
     */
    public void setupDialog() {
	Preferences prefs = Preferences.getInstance();
	getBrowseOnStartCheckBox().setSelected(Boolean.valueOf(prefs.getProperty(prefs.BROWSE_ON_START)).booleanValue());
    }
}
