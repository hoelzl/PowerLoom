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


// Version: KBClearFrame.java,v 1.11 2010/02/04 05:17:44 hans Exp

package edu.isi.powerloom.gui.components;

import edu.isi.powerloom.gui.serverinterface.*;
import edu.isi.powerloom.gui.xmlobject.*;
import static edu.isi.powerloom.gui.common.Utils.*;
import java.util.*;
import javax.swing.*;

/**
 * Frame for the clear-module action.  This presents a list of modules.
 *
 * @since 4/24/2002 3:15:15 PM
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */
public class KBClearFrame extends PLFrame implements ActionComponent {

    class IvjEventHandler implements java.awt.event.ActionListener {
	public void actionPerformed(java.awt.event.ActionEvent e) {
	    if (e.getSource() == KBClearFrame.this.getCancelButton()) 
		connEtoC1(e);
	    if (e.getSource() == KBClearFrame.this.getOKButton()) 
		connEtoC2(e);
	};
    };
    private javax.swing.JPanel ivjButtonPanel = null;
    private javax.swing.JButton ivjCancelButton = null;
    IvjEventHandler ivjEventHandler = new IvjEventHandler();
    private javax.swing.JPanel ivjJInternalFrameContentPane = null;
    private javax.swing.JList ivjModuleList = null;
    private javax.swing.JScrollPane ivjModuleListScrollPane = null;
    private javax.swing.JButton ivjOKButton = null;
    private PowerloomApp app;
    /**
     * KBClearFrame constructor comment.
     */
    public KBClearFrame() {
	super();
	initialize();
    }
    /**
     * KBClearFrame constructor comment.
     * @param title java.lang.String
     */
    public KBClearFrame(String title) {
	super(title);
    }
    /**
     * KBClearFrame constructor comment.
     * @param title java.lang.String
     * @param resizable boolean
     */
    public KBClearFrame(String title, boolean resizable) {
	super(title, resizable);
    }
    /**
     * KBClearFrame constructor comment.
     * @param title java.lang.String
     * @param resizable boolean
     * @param closable boolean
     */
    public KBClearFrame(String title, boolean resizable, boolean closable) {
	super(title, resizable, closable);
    }
    /**
     * KBClearFrame constructor comment.
     * @param title java.lang.String
     * @param resizable boolean
     * @param closable boolean
     * @param maximizable boolean
     */
    public KBClearFrame(String title, boolean resizable, boolean closable, boolean maximizable) {
	super(title, resizable, closable, maximizable);
    }
    /**
     * KBClearFrame constructor comment.
     * @param title java.lang.String
     * @param resizable boolean
     * @param closable boolean
     * @param maximizable boolean
     * @param iconifiable boolean
     */
    public KBClearFrame(String title, boolean resizable, boolean closable, boolean maximizable, boolean iconifiable) {
	super(title, resizable, closable, maximizable, iconifiable);
    }
    /**
     * Comment
     */
    public void cancelButton_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	doDefaultCloseAction();
    }
    /**
     * connEtoC1:  (CancelButton.action.actionPerformed(java.awt.event.ActionEvent) --> KBClearFrame.cancelButton_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC1(java.awt.event.ActionEvent arg1) {
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
     * connEtoC2:  (OKButton.action.actionPerformed(java.awt.event.ActionEvent) --> KBClearFrame.oKButton_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
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
     * Insert the method's description here.
     * Creation date: (4/24/2002 3:25:47 PM)
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
		getJInternalFrameContentPane().add(getModuleListScrollPane(), "Center");
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
     * Return the ModuleList property value.
     * @return javax.swing.JList
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JList getModuleList() {
	if (ivjModuleList == null) {
	    try {
		ivjModuleList = new javax.swing.JList();
		ivjModuleList.setName("ModuleList");
		ivjModuleList.setBounds(0, 0, 160, 120);
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjModuleList;
    }
    /**
     * Return the ModuleListScrollPane property value.
     * @return javax.swing.JScrollPane
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JScrollPane getModuleListScrollPane() {
	if (ivjModuleListScrollPane == null) {
	    try {
		ivjModuleListScrollPane = new javax.swing.JScrollPane();
		ivjModuleListScrollPane.setName("ModuleListScrollPane");
		getModuleListScrollPane().setViewportView(getModuleList());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjModuleListScrollPane;
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
	getCancelButton().addActionListener(ivjEventHandler);
	getOKButton().addActionListener(ivjEventHandler);
    }
    /**
     * Initialize the class.
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void initialize() {
	try {
	    // user code begin {1}
	    // user code end
	    setName("KBClearFrame");
	    setTitle("Clear Module");
	    setClosable(true);
	    setIconifiable(true);
	    setSize(366, 311);
	    setMaximizable(true);
	    setResizable(true);
	    setContentPane(getJInternalFrameContentPane());
	    initConnections();
	} catch (java.lang.Throwable ivjExc) {
	    handleException(ivjExc);
	}
	// user code begin {2}
	try {
	    PLSurrogateContainer modules = KnowledgeManager.getInstance().getModules();
	    getModuleList().setModel(new PLListModel(modules.listifyTreeContainer()));
	    app = PowerloomApp.getInstance();

	} catch (Exception e) {
	    handleException(e);
	}
	// user code end
    }
    /**
     * main entrypoint - starts the part when it is run as an application
     * @param args java.lang.String[]
     */
    public static void main(java.lang.String[] args) {
	try {
	    javax.swing.JFrame frame = new javax.swing.JFrame();
	    KBClearFrame aKBClearFrame;
	    aKBClearFrame = new KBClearFrame();
	    frame.setContentPane(aKBClearFrame);
	    frame.setSize(aKBClearFrame.getSize());
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
	PLModule module = (PLModule)getModuleList().getSelectedValue();
	debugPrintln(3, "**Selected Module: " + module);
	if (module == null) {
	    return;
	}
		
	try {
	    edu.isi.powerloom.gui.serverinterface.KnowledgeManager.getInstance().clearKB(module);
	} catch (Exception e) {
	    handleException(e);
	}
	fireEditPerformed(new PLEditEvent(this, PLModule.class));
	setVisible(false);
    }


    public void setModule(PLModule module) {
	getModuleList().setSelectedValue(module, false);
    }

    public void doAction() {
	oKButton_ActionPerformed(null);
    }

    /**
     * Implementation of PLFrame
     */
    public PLModule getModule() {
	// todo? implement this?
	return null;
    }

}
