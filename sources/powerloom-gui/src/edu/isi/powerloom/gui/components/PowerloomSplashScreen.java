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


// Version: PowerloomSplashScreen.java,v 1.7 2010/04/19 22:41:31 hans Exp

package edu.isi.powerloom.gui.components;

import java.awt.*;
import javax.swing.*;
import static edu.isi.powerloom.gui.common.Utils.*;

/**
 * Splash Screen for Powerloom GUI.
 *
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */
public class PowerloomSplashScreen extends JWindow {

    public static final String SPLASH_SCREEN_PATH =
        "resources/images/PowerLoomSplash-logo.gif";

    class IvjEventHandler implements java.awt.event.WindowListener {
	public void windowActivated(java.awt.event.WindowEvent e) {};
	public void windowClosed(java.awt.event.WindowEvent e) {};
	public void windowClosing(java.awt.event.WindowEvent e) {
	    if (e.getSource() == PowerloomSplashScreen.this) 
		connEtoC1(e);
	};
	public void windowDeactivated(java.awt.event.WindowEvent e) {};
	public void windowDeiconified(java.awt.event.WindowEvent e) {};
	public void windowIconified(java.awt.event.WindowEvent e) {};
	public void windowOpened(java.awt.event.WindowEvent e) {};
    };
    IvjEventHandler ivjEventHandler = new IvjEventHandler();
    private JLabel ivjJLabel1 = null;
    private JPanel ivjJWindowContentPane = null;
    /**
     * PowerloomSplashScreen constructor comment.
     */
    public PowerloomSplashScreen() {
	super();
	initialize();
    }
    /**
     * PowerloomSplashScreen constructor comment.
     * @param owner java.awt.Frame
     */
    public PowerloomSplashScreen(Frame owner) {
	super(owner);
    }
    /**
     * PowerloomSplashScreen constructor comment.
     * @param owner java.awt.Window
     */
    public PowerloomSplashScreen(Window owner) {
	super(owner);
    }
    /**
     * connEtoC1:  (PowerloomSplashScreen.window.windowClosing(java.awt.event.WindowEvent) --> PowerloomSplashScreen.dispose()V)
     * @param arg1 java.awt.event.WindowEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC1(java.awt.event.WindowEvent arg1) {
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
     * Return the JLabel1 property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getJLabel1() {
	if (ivjJLabel1 == null) {
	    try {
		ivjJLabel1 = new javax.swing.JLabel();
		ivjJLabel1.setName("JLabel1");
		ivjJLabel1.setIcon(new javax.swing.ImageIcon(getClass().getClassLoader().getResource(SPLASH_SCREEN_PATH)));
		// user code begin {1}
		ivjJLabel1.setText("Copyright (C) " + PowerloomApp.POWERLOOM_COPYRIGHT_HOLDER
                                   + " " + PowerloomApp.POWERLOOM_COPYRIGHT_YEARS);
                ivjJLabel1.setHorizontalTextPosition(SwingConstants.CENTER);
                ivjJLabel1.setVerticalTextPosition(SwingConstants.BOTTOM);
                ivjJLabel1.setIconTextGap(-20); // make it overlap with the background
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
     * Return the JWindowContentPane property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getJWindowContentPane() {
	if (ivjJWindowContentPane == null) {
	    try {
		ivjJWindowContentPane = new javax.swing.JPanel();
		ivjJWindowContentPane.setName("JWindowContentPane");
		ivjJWindowContentPane.setBorder(new javax.swing.border.EtchedBorder());
		ivjJWindowContentPane.setLayout(new java.awt.BorderLayout());
		getJWindowContentPane().add(getJLabel1(), "Center");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjJWindowContentPane;
    }
    /**
     * Called whenever the part throws an exception.
     * @param exception java.lang.Throwable
     */
    private void handleException(java.lang.Throwable exception) {

	/* Uncomment the following lines to print uncaught exceptions to stdout */
	// System.out.println("--------- UNCAUGHT EXCEPTION ---------");
	// exception.printStackTrace(System.out);
    }
    /**
     * Initializes connections
     * @exception java.lang.Exception The exception description.
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void initConnections() throws java.lang.Exception {
	// user code begin {1}
	// user code end
	this.addWindowListener(ivjEventHandler);
    }
    /**
     * Initialize the class.
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void initialize() {
	try {
	    // user code begin {1}
	    // user code end
	    setName("PowerloomSplashScreen");
	    setSize(300, 220);
	    setContentPane(getJWindowContentPane());
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
	    PowerloomSplashScreen aPowerloomSplashScreen;
	    aPowerloomSplashScreen = new PowerloomSplashScreen();
	    aPowerloomSplashScreen.addWindowListener(new java.awt.event.WindowAdapter() {
		    public void windowClosing(java.awt.event.WindowEvent e) {
			exitGui(0);
		    };
		});
	    aPowerloomSplashScreen.show();
	    java.awt.Insets insets = aPowerloomSplashScreen.getInsets();
	    aPowerloomSplashScreen.setSize(aPowerloomSplashScreen.getWidth() + insets.left + insets.right, aPowerloomSplashScreen.getHeight() + insets.top + insets.bottom);
	    aPowerloomSplashScreen.setVisible(true);
	} catch (Throwable exception) {
	    System.err.println("Exception occurred in main() of javax.swing.JWindow");
	    exception.printStackTrace(System.out);
	}
    }
}
