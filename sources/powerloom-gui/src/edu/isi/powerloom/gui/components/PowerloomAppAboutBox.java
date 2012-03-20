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


// Version: PowerloomAppAboutBox.java,v 1.8 2010/04/19 22:44:11 hans Exp

package edu.isi.powerloom.gui.components;

import java.awt.*;
import javax.swing.*;
import static edu.isi.powerloom.gui.common.Utils.*;

/**
 * About box for the Powerloom GUI application.
 *
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */
public class PowerloomAppAboutBox extends JDialog {

    // TO DO: SIMPLIFY THIS, since this is way too much code for the little it does.

    //private static Color BACKGROUND_COLOR = PowerloomApp.BACKGROUND_COLOR;
    //private static Color BUTTON_BACKGROUND_COLOR = BACKGROUND_COLOR;

    class IvjEventHandler implements java.awt.event.ActionListener {
	public void actionPerformed(java.awt.event.ActionEvent e) {
	    if (e.getSource() == PowerloomAppAboutBox.this.getOkButton()) 
		connEtoM1(e);
	};
    };
    private JLabel ivjAppName = null;
    private JPanel ivjButtonPane = null;
    private JLabel ivjCopyright = null;
    IvjEventHandler ivjEventHandler = new IvjEventHandler();
    private JLabel ivjIconLabel = null;
    private JPanel ivjIconPane = null;
    private JPanel ivjJDialogContentPane = null;
    private JButton ivjOkButton = null;
    private JLabel ivjSpacer = null;
    private JPanel ivjTextPane = null;
    private GridLayout ivjTextPaneGridLayout = null;
    private JLabel ivjUserName = null;
    private JLabel ivjVersion = null;
    /**
     * PowerloomAppAboutBox constructor comment.
     */
    public PowerloomAppAboutBox() {
	super();
	initialize();
    }
    /**
     * PowerloomAppAboutBox constructor comment.
     * @param owner java.awt.Dialog
     */
    public PowerloomAppAboutBox(Dialog owner) {
	super(owner, "About PowerLoom GUI");
    }
    /**
     * PowerloomAppAboutBox constructor comment.
     * @param owner java.awt.Dialog
     * @param title java.lang.String
     */
    public PowerloomAppAboutBox(Dialog owner, String title) {
	super(owner, title);
    }
    /**
     * PowerloomAppAboutBox constructor comment.
     * @param owner java.awt.Dialog
     * @param title java.lang.String
     * @param modal boolean
     */
    public PowerloomAppAboutBox(Dialog owner, String title, boolean modal) {
	super(owner, title, modal);
    }
    /**
     * PowerloomAppAboutBox constructor comment.
     * @param owner java.awt.Dialog
     * @param modal boolean
     */
    public PowerloomAppAboutBox(Dialog owner, boolean modal) {
	super(owner, modal);
    }
    /**
     * PowerloomAppAboutBox constructor comment.
     * @param owner java.awt.Frame
     */
    public PowerloomAppAboutBox(Frame owner) {
	super(owner);
    }
    /**
     * PowerloomAppAboutBox constructor comment.
     * @param owner java.awt.Frame
     * @param title java.lang.String
     */
    public PowerloomAppAboutBox(Frame owner, String title) {
	super(owner, title);
    }
    /**
     * PowerloomAppAboutBox constructor comment.
     * @param owner java.awt.Frame
     * @param title java.lang.String
     * @param modal boolean
     */
    public PowerloomAppAboutBox(Frame owner, String title, boolean modal) {
	super(owner, title, modal);
    }
    /**
     * PowerloomAppAboutBox constructor comment.
     * @param owner java.awt.Frame
     * @param modal boolean
     */
    public PowerloomAppAboutBox(Frame owner, boolean modal) {
	super(owner, modal);
    }
    /**
     * connEtoM1:  (OkButton.action.actionPerformed(java.awt.event.ActionEvent) --> PowerloomAppAboutBox.dispose()V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoM1(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.dispose();
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }
    /**
     * Return the AppName property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getAppName() {
	if (ivjAppName == null) {
	    try {
		ivjAppName = new javax.swing.JLabel();
		ivjAppName.setName("AppName");
		ivjAppName.setText("PowerLoom GUI");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjAppName;
    }
    /**
     * Return the ButtonPane property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getButtonPane() {
	if (ivjButtonPane == null) {
	    try {
		ivjButtonPane = new javax.swing.JPanel();
		ivjButtonPane.setName("ButtonPane");
		ivjButtonPane.setLayout(new java.awt.FlowLayout());
		getButtonPane().add(getOkButton(), getOkButton().getName());
		// user code begin {1}
		//getButtonPane().setBackground(BACKGROUND_COLOR);
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjButtonPane;
    }
    /**
     * Return the Copyright property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getCopyright() {
	if (ivjCopyright == null) {
	    try {
		ivjCopyright = new javax.swing.JLabel();
		ivjCopyright.setName("Copyright");
		ivjCopyright.setText("Copyright (C) " + PowerloomApp.POWERLOOM_GUI_COPYRIGHT_YEARS);
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjCopyright;
    }
    /**
     * Return the IconLabel property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getIconLabel() {
	if (ivjIconLabel == null) {
	    try {
		ivjIconLabel = new javax.swing.JLabel();
		ivjIconLabel.setName("IconLabel");
		ivjIconLabel.setIcon(PowerloomApp.getInstance().getImage(PowerloomApp.POWERLOOM_LOGO_PATH));
		ivjIconLabel.setText("");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjIconLabel;
    }
    /**
     * Return the IconPane property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getIconPane() {
	if (ivjIconPane == null) {
	    try {
		ivjIconPane = new javax.swing.JPanel();
		ivjIconPane.setName("IconPane");
		ivjIconPane.setLayout(new java.awt.BorderLayout());
		getIconPane().add(getIconLabel(), "Center");
		// user code begin {1}
		//getIconPane().setBackground(BACKGROUND_COLOR);
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjIconPane;
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
		getJDialogContentPane().add(getButtonPane(), "South");
		getJDialogContentPane().add(getTextPane(), "Center");
		getJDialogContentPane().add(getIconPane(), "West");
		// user code begin {1}
		getJDialogContentPane().setBorder(new javax.swing.border.EmptyBorder(10,10,10,10));
		//getJDialogContentPane().setBackground(BACKGROUND_COLOR);
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
     * Return the OkButton property value.
     * @return javax.swing.JButton
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JButton getOkButton() {
	if (ivjOkButton == null) {
	    try {
		ivjOkButton = new javax.swing.JButton();
		ivjOkButton.setName("OkButton");
		ivjOkButton.setText("OK");
		// user code begin {1}
		//ivjOkButton.setBackground(BUTTON_BACKGROUND_COLOR);
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjOkButton;
    }
    /**
     * Return the Spacer property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getSpacer() {
	if (ivjSpacer == null) {
	    try {
		ivjSpacer = new javax.swing.JLabel();
		ivjSpacer.setName("Spacer");
		ivjSpacer.setText("");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjSpacer;
    }
    /**
     * Return the TextPane property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getTextPane() {
	if (ivjTextPane == null) {
	    try {
		ivjTextPane = new javax.swing.JPanel();
		ivjTextPane.setName("TextPane");
		ivjTextPane.setLayout(getTextPaneGridLayout());
		getTextPane().add(getAppName(), getAppName().getName());
		getTextPane().add(getVersion(), getVersion().getName());
		getTextPane().add(getSpacer(), getSpacer().getName());
		getTextPane().add(getCopyright(), getCopyright().getName());
		getTextPane().add(getUserName(), getUserName().getName());
		// user code begin {1}
		getTextPane().setBorder(new javax.swing.border.EmptyBorder(10, 10, 10, 10));
		//getTextPane().setBackground(BACKGROUND_COLOR);
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjTextPane;
    }
    /**
     * Return the TextPaneGridLayout property value.
     * @return java.awt.GridLayout
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private java.awt.GridLayout getTextPaneGridLayout() {
	java.awt.GridLayout ivjTextPaneGridLayout = null;
	try {
	    /* Create part */
	    ivjTextPaneGridLayout = new java.awt.GridLayout(5, 1);
	} catch (java.lang.Throwable ivjExc) {
	    handleException(ivjExc);
	};
	return ivjTextPaneGridLayout;
    }
    /**
     * Return the UserName property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getUserName() {
	if (ivjUserName == null) {
	    try {
		ivjUserName = new javax.swing.JLabel();
		ivjUserName.setName("UserName");
		ivjUserName.setText(PowerloomApp.POWERLOOM_COPYRIGHT_HOLDER);
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjUserName;
    }
    /**
     * Return the Version property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getVersion() {
	if (ivjVersion == null) {
	    try {
		ivjVersion = new javax.swing.JLabel();
		ivjVersion.setName("Version");
		ivjVersion.setText(PowerloomApp.POWERLOOM_GUI_VERSION);
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjVersion;
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
	getOkButton().addActionListener(ivjEventHandler);
    }
    /**
     * Initialize the class.
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void initialize() {
	try {
	    // user code begin {1}
	    // user code end
	    setName("PowerloomAppAboutBox");
	    setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
	    setSize(405, 230); // ugly, should make it dynamically fit the content
	    setTitle("About PowerLoom GUI");
            PowerloomApp.setPowerLoomWindowIcon(this);
	    setContentPane(getJDialogContentPane());
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
	    PowerloomAppAboutBox aPowerloomAppAboutBox;
	    aPowerloomAppAboutBox = new PowerloomAppAboutBox();
	    aPowerloomAppAboutBox.setModal(true);
	    aPowerloomAppAboutBox.addWindowListener(new java.awt.event.WindowAdapter() {
		    public void windowClosing(java.awt.event.WindowEvent e) {
			exitGui(0);
		    };
		});
	    aPowerloomAppAboutBox.show();
	    java.awt.Insets insets = aPowerloomAppAboutBox.getInsets();
	    aPowerloomAppAboutBox.setSize(aPowerloomAppAboutBox.getWidth() + insets.left + insets.right, aPowerloomAppAboutBox.getHeight() + insets.top + insets.bottom);
	    aPowerloomAppAboutBox.setVisible(true);
	} catch (Throwable exception) {
	    System.err.println("Exception occurred in main() of javax.swing.JDialog");
	    exception.printStackTrace(System.out);
	}
    }
}
