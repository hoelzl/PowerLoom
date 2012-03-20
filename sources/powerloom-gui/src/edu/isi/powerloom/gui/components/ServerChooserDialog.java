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


// Version: ServerChooserDialog.java,v 1.6 2010/02/04 05:19:37 hans Exp

package edu.isi.powerloom.gui.components;

import edu.isi.powerloom.gui.common.*;
import static edu.isi.powerloom.gui.common.Utils.*;

/**
 * Dialog for connecting to a server.
 *
 * @since 4/23/2002 11:26:17 AM
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */
public class ServerChooserDialog extends javax.swing.JDialog {
    private javax.swing.JPanel ivjButtonPanel = null;
    private javax.swing.JButton ivjCancelButton = null;
    private javax.swing.JLabel ivjHostLabel = null;
    private javax.swing.JPanel ivjJDialogContentPane = null;
    private javax.swing.JTextField ivjJTextField1 = null;
    private javax.swing.JTextField ivjJTextField2 = null;
    private javax.swing.JPanel ivjLabelPanel = null;
    private java.awt.GridLayout ivjLabelPanelGridLayout = null;
    private javax.swing.JButton ivjOKButton = null;
    private javax.swing.JLabel ivjPortLabel = null;
    private javax.swing.JPanel ivjTextFieldPanel = null;
    private java.awt.GridLayout ivjTextFieldPanelGridLayout = null;
    IvjEventHandler ivjEventHandler = new IvjEventHandler();
    private java.lang.String hostResult;
    private int portResult;
    private javax.swing.JPanel ivjButtonGroupPanel = null;
    private javax.swing.JPanel ivjCheckBoxPanel = null;
    private javax.swing.JCheckBox ivjSaveCheckBox = null;

    class IvjEventHandler implements java.awt.event.ActionListener {
	public void actionPerformed(java.awt.event.ActionEvent e) {
	    if (e.getSource() == ServerChooserDialog.this.getOKButton()) 
		connEtoC1(e);
	    if (e.getSource() == ServerChooserDialog.this.getCancelButton()) 
		connEtoC2(e);
	};
    };
    private boolean saveSettings;
    /**
     * ServerChooserDialog constructor comment.
     */
    public ServerChooserDialog() {
	super();
	initialize();
    }
    /**
     * ServerChooserDialog constructor comment.
     * @param owner java.awt.Dialog
     */
    public ServerChooserDialog(java.awt.Dialog owner) {
	super(owner);
    }
    /**
     * ServerChooserDialog constructor comment.
     * @param owner java.awt.Dialog
     * @param title java.lang.String
     */
    public ServerChooserDialog(java.awt.Dialog owner, String title) {
	super(owner, title);
    }
    /**
     * ServerChooserDialog constructor comment.
     * @param owner java.awt.Dialog
     * @param title java.lang.String
     * @param modal boolean
     */
    public ServerChooserDialog(java.awt.Dialog owner, String title, boolean modal) {
	super(owner, title, modal);
    }
    /**
     * ServerChooserDialog constructor comment.
     * @param owner java.awt.Dialog
     * @param modal boolean
     */
    public ServerChooserDialog(java.awt.Dialog owner, boolean modal) {
	super(owner, modal);
    }
    /**
     * ServerChooserDialog constructor comment.
     * @param owner java.awt.Frame
     */
    public ServerChooserDialog(java.awt.Frame owner) {
	super(owner);
    }
    /**
     * ServerChooserDialog constructor comment.
     * @param owner java.awt.Frame
     * @param title java.lang.String
     */
    public ServerChooserDialog(java.awt.Frame owner, String title) {
	super(owner, title);
    }
    /**
     * ServerChooserDialog constructor comment.
     * @param owner java.awt.Frame
     * @param title java.lang.String
     * @param modal boolean
     */
    public ServerChooserDialog(java.awt.Frame owner, String title, boolean modal) {
	super(owner, title, modal);
    }
    /**
     * ServerChooserDialog constructor comment.
     * @param owner java.awt.Frame
     * @param modal boolean
     */
    public ServerChooserDialog(java.awt.Frame owner, boolean modal) {
	super(owner, modal);
    }
    /**
     * Comment
     */
    public void cancelButton_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	setVisible(false);
    }
    /**
     * connEtoC1:  (OKButton.action.actionPerformed(java.awt.event.ActionEvent) --> ServerChooserDialog.oKButton_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
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
     * connEtoC2:  (CancelButton.action.actionPerformed(java.awt.event.ActionEvent) --> ServerChooserDialog.cancelButton_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
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
     * Return the ButtonGroupPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getButtonGroupPanel() {
	if (ivjButtonGroupPanel == null) {
	    try {
		ivjButtonGroupPanel = new javax.swing.JPanel();
		ivjButtonGroupPanel.setName("ButtonGroupPanel");
		ivjButtonGroupPanel.setLayout(new java.awt.BorderLayout());
		getButtonGroupPanel().add(getButtonPanel(), "South");
		getButtonGroupPanel().add(getCheckBoxPanel(), "North");
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
     * Return the CheckBoxPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getCheckBoxPanel() {
	if (ivjCheckBoxPanel == null) {
	    try {
		ivjCheckBoxPanel = new javax.swing.JPanel();
		ivjCheckBoxPanel.setName("CheckBoxPanel");
		ivjCheckBoxPanel.setLayout(new java.awt.FlowLayout());
		getCheckBoxPanel().add(getSaveCheckBox(), getSaveCheckBox().getName());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjCheckBoxPanel;
    }
    /**
     * Return the HostLabel property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getHostLabel() {
	if (ivjHostLabel == null) {
	    try {
		ivjHostLabel = new javax.swing.JLabel();
		ivjHostLabel.setName("HostLabel");
		ivjHostLabel.setText("Host:");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjHostLabel;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/23/2002 1:18:52 PM)
     * @return java.lang.String
     */
    public java.lang.String getHostResult() {
	return hostResult;
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
		ivjJDialogContentPane.setBorder(new javax.swing.border.EmptyBorder(10,10,10,10));
		ivjJDialogContentPane.setLayout(new java.awt.BorderLayout());
		getJDialogContentPane().add(getLabelPanel(), "West");
		getJDialogContentPane().add(getTextFieldPanel(), "Center");
		getJDialogContentPane().add(getButtonGroupPanel(), "South");
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
     * Return the JTextField1 property value.
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTextField getJTextField1() {
	if (ivjJTextField1 == null) {
	    try {
		String[] types = {"Text Holder"};
		//ivjJTextField1 = new PLJTextField(this, "Server Chooser Host Field", Arrays.asList(types));
		ivjJTextField1 = new javax.swing.JTextField();
		ivjJTextField1.setName("JTextField1");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjJTextField1;
    }
    /**
     * Return the JTextField2 property value.
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTextField getJTextField2() {
	if (ivjJTextField2 == null) {
	    try {
		ivjJTextField2 = new javax.swing.JTextField();
		ivjJTextField2.setName("JTextField2");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjJTextField2;
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
		ivjLabelPanel.setBorder(new javax.swing.border.EmptyBorder(0,10,0,10));
		ivjLabelPanel.setLayout(getLabelPanelGridLayout());
		getLabelPanel().add(getHostLabel(), getHostLabel().getName());
		getLabelPanel().add(getPortLabel(), getPortLabel().getName());
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
     * Return the LabelPanelGridLayout property value.
     * @return java.awt.GridLayout
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private java.awt.GridLayout getLabelPanelGridLayout() {
	java.awt.GridLayout ivjLabelPanelGridLayout = null;
	try {
	    /* Create part */
	    ivjLabelPanelGridLayout = new java.awt.GridLayout(2, 1);
	} catch (java.lang.Throwable ivjExc) {
	    handleException(ivjExc);
	};
	return ivjLabelPanelGridLayout;
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
     * Return the PortLabel property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getPortLabel() {
	if (ivjPortLabel == null) {
	    try {
		ivjPortLabel = new javax.swing.JLabel();
		ivjPortLabel.setName("PortLabel");
		ivjPortLabel.setText("Port:");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjPortLabel;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/23/2002 1:19:09 PM)
     * @return int
     */
    public int getPortResult() {
	return portResult;
    }
    /**
     * Return the SaveCheckBox property value.
     * @return javax.swing.JCheckBox
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JCheckBox getSaveCheckBox() {
	if (ivjSaveCheckBox == null) {
	    try {
		ivjSaveCheckBox = new javax.swing.JCheckBox();
		ivjSaveCheckBox.setName("SaveCheckBox");
		ivjSaveCheckBox.setSelected(true);
		ivjSaveCheckBox.setText("Save as Default");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjSaveCheckBox;
    }
    /**
     * Return the TextFieldPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getTextFieldPanel() {
	if (ivjTextFieldPanel == null) {
	    try {
		ivjTextFieldPanel = new javax.swing.JPanel();
		ivjTextFieldPanel.setName("TextFieldPanel");
		ivjTextFieldPanel.setLayout(getTextFieldPanelGridLayout());
		getTextFieldPanel().add(getJTextField1(), getJTextField1().getName());
		getTextFieldPanel().add(getJTextField2(), getJTextField2().getName());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjTextFieldPanel;
    }
    /**
     * Return the TextFieldPanelGridLayout property value.
     * @return java.awt.GridLayout
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private java.awt.GridLayout getTextFieldPanelGridLayout() {
	java.awt.GridLayout ivjTextFieldPanelGridLayout = null;
	try {
	    /* Create part */
	    ivjTextFieldPanelGridLayout = new java.awt.GridLayout(2, 1);
	} catch (java.lang.Throwable ivjExc) {
	    handleException(ivjExc);
	};
	return ivjTextFieldPanelGridLayout;
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
	    setName("ServerChooserDialog");
	    setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
	    setSize(291, 169);
	    setTitle("Choose KB Server");
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
     * Insert the method's description here.
     * Creation date: (4/23/2002 1:44:59 PM)
     * @return boolean
     */
    public boolean isSaveSettings() {
	return saveSettings;
    }
    /**
     * main entrypoint - starts the part when it is run as an application
     * @param args java.lang.String[]
     */
    public static void main(java.lang.String[] args) {
	try {
	    ServerChooserDialog aServerChooserDialog;
	    aServerChooserDialog = new ServerChooserDialog();
	    aServerChooserDialog.setModal(true);
	    aServerChooserDialog.addWindowListener(new java.awt.event.WindowAdapter() {
		    public void windowClosing(java.awt.event.WindowEvent e) {
			exitGui(0);
		    };
		});
	    aServerChooserDialog.show();
	    java.awt.Insets insets = aServerChooserDialog.getInsets();
	    aServerChooserDialog.setSize(aServerChooserDialog.getWidth() + insets.left + insets.right, aServerChooserDialog.getHeight() + insets.top + insets.bottom);
	    aServerChooserDialog.setVisible(true);
	} catch (Throwable exception) {
	    System.err.println("Exception occurred in main() of javax.swing.JDialog");
	    exception.printStackTrace(System.out);
	}
    }
    /**
     * Comment
     */
    public void oKButton_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	try {
	    hostResult = getJTextField1().getText();
	    portResult = -1;
	    portResult = Integer.parseInt(getJTextField2().getText());
	    saveSettings = getSaveCheckBox().isSelected();
	    setVisible(false);
	} catch (Exception e) {
	    handleException(e);
	};
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/23/2002 1:18:52 PM)
     * @param newHostResult java.lang.String
     */
    public void setHostResult(java.lang.String newHostResult) {
	hostResult = newHostResult;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/23/2002 1:19:09 PM)
     * @param newPortResult int
     */
    public void setPortResult(int newPortResult) {
	portResult = newPortResult;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/23/2002 1:44:59 PM)
     * @param newSaveSettings boolean
     */
    public void setSaveSettings(boolean newSaveSettings) {
	saveSettings = newSaveSettings;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/29/2002 8:32:28 AM)
     */
    public void setupDialog() {
	Preferences prefs = Preferences.getInstance();
	String host = prefs.getProperty(prefs.HOST);
	String port = prefs.getProperty(prefs.PORT);
	if (host != null)
	    getJTextField1().setText(host);	
	if (port != null)
	    getJTextField2().setText(port);
    }
}
