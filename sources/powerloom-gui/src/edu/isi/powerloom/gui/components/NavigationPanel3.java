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


// Version: NavigationPanel3.java,v 1.11 2010/02/04 05:18:02 hans Exp

package edu.isi.powerloom.gui.components;

import javax.swing.*;
import java.awt.event.*;
import static edu.isi.powerloom.gui.common.Utils.*;

/**
 * SubPanel of the BrowserPane which contains a list or a tree for displaying 
 * collections of objects.  For example the Concept Tree, Instance
 * List and Rule List are instances of this class.
 *
 * @since 3/29/2002 11:08:13 PM
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */
public class NavigationPanel3 extends javax.swing.JPanel {
    private javax.swing.JList ivjJList1 = null;
    private javax.swing.JButton ivjJToolBarButton1 = null;
    private javax.swing.JButton ivjJToolBarButton2 = null;
    private javax.swing.JLabel ivjNavigationLabel = null;
    private javax.swing.JScrollPane ivjNavigationScrollPane = null;
    private javax.swing.JPanel ivjNavigationToolLabelPanel = null;
    private javax.swing.JPanel ivjNavigationToolPanel = null;
    private java.awt.FlowLayout ivjNavigationToolPanelFlowLayout = null;
    private ModuleViewSelectorPanel ivjModuleViewSelectorPanel = null;
    private boolean addModuleViewSelectorPanel;
    private boolean useAddAction = false;
    private Action addAction = null;

    /**
     * NavigationPanel3 constructor comment.
     */
    public NavigationPanel3() {
	super();
	initialize();
    }

    public NavigationPanel3(Action addAction) {
	super();
	useAddAction = true;
	this.addAction = addAction;
	initialize();
    }

    public NavigationPanel3(boolean addModuleViewSelectorPanel) {
	super();
	this.addModuleViewSelectorPanel = addModuleViewSelectorPanel;
	initialize();
    }

    public NavigationPanel3(boolean addModuleViewSelectorPanel, Action addAction) {
	super();
	this.addModuleViewSelectorPanel = addModuleViewSelectorPanel;
	useAddAction = true;
	this.addAction = addAction;
	initialize();
    }

    

    /**
     * NavigationPanel3 constructor comment.
     * @param layout java.awt.LayoutManager
     */
    public NavigationPanel3(java.awt.LayoutManager layout) {
	super(layout);
    }
    /**
     * NavigationPanel3 constructor comment.
     * @param layout java.awt.LayoutManager
     * @param isDoubleBuffered boolean
     */
    public NavigationPanel3(java.awt.LayoutManager layout, boolean isDoubleBuffered) {
	super(layout, isDoubleBuffered);
    }
    /**
     * Return the JList1 property value.
     * @return javax.swing.JList
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JList getJList1() {
	if (ivjJList1 == null) {
	    try {
		ivjJList1 = new javax.swing.JList();
		ivjJList1.setName("JList1");
		ivjJList1.setBounds(0, 0, 160, 120);
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjJList1;
    }
    /**
     * Return the JToolBarButton1 property value.
     * @return javax.swing.JButton
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JButton getJToolBarButton1() {
	if (ivjJToolBarButton1 == null) {
	    try {
		ivjJToolBarButton1 = new javax.swing.JButton();
		ivjJToolBarButton1.setName("JToolBarButton1");
		ivjJToolBarButton1.setText("");
		ivjJToolBarButton1.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
		ivjJToolBarButton1.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
		ivjJToolBarButton1.setIcon(PowerloomApp.getImage("resources/images/add.gif"));
		ivjJToolBarButton1.setMargin(new java.awt.Insets(0, 0, 0, 0));
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjJToolBarButton1;
    }
    /**
     * Return the JToolBarButton2 property value.
     * @return javax.swing.JButton
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JButton getJToolBarButton2() {
	if (ivjJToolBarButton2 == null) {
	    try {
		ivjJToolBarButton2 = new javax.swing.JButton();
		ivjJToolBarButton2.setName("JToolBarButton2");
		ivjJToolBarButton2.setText("");
		ivjJToolBarButton2.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
		ivjJToolBarButton2.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
		ivjJToolBarButton2.setIcon(PowerloomApp.getImage("resources/images/find.gif"));
		ivjJToolBarButton2.setMargin(new java.awt.Insets(0, 0, 0, 0));
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjJToolBarButton2;
    }
    /**
     * Return the NavigationLabel property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getNavigationLabel() {
	if (ivjNavigationLabel == null) {
	    try {
		ivjNavigationLabel = new javax.swing.JLabel();
		ivjNavigationLabel.setName("NavigationLabel");
		ivjNavigationLabel.setAlignmentX(java.awt.Component.LEFT_ALIGNMENT);
		ivjNavigationLabel.setText("Navigation Label");
		ivjNavigationLabel.setComponentOrientation(java.awt.ComponentOrientation.LEFT_TO_RIGHT);
		ivjNavigationLabel.setHorizontalAlignment(javax.swing.SwingConstants.LEFT);
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjNavigationLabel;
    }
    /**
     * Return the NavigationScrollPane property value.
     * @return javax.swing.JScrollPane
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    javax.swing.JScrollPane getNavigationScrollPane() {
	if (ivjNavigationScrollPane == null) {
	    try {
		ivjNavigationScrollPane = new javax.swing.JScrollPane();
		ivjNavigationScrollPane.setName("NavigationScrollPane");
		getNavigationScrollPane().setViewportView(getJList1());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjNavigationScrollPane;
    }
    /**
     * Return the NavigationToolLabelPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getNavigationToolLabelPanel() {
	if (ivjNavigationToolLabelPanel == null) {
	    try {
		ivjNavigationToolLabelPanel = new javax.swing.JPanel();
		ivjNavigationToolLabelPanel.setName("NavigationToolLabelPanel");
		if (addModuleViewSelectorPanel) {
		    ivjNavigationToolLabelPanel.setPreferredSize(new java.awt.Dimension(154, 33));
		} else {
		    ivjNavigationToolLabelPanel.setPreferredSize(new java.awt.Dimension(154, 25));
		}
		ivjNavigationToolLabelPanel.setBorder(new javax.swing.border.EmptyBorder(0, 10, 0, 0));
		ivjNavigationToolLabelPanel.setLayout(new java.awt.BorderLayout());
		getNavigationToolLabelPanel().add(getNavigationLabel(), "Center");
		getNavigationToolLabelPanel().add(getNavigationToolPanel(), "East");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjNavigationToolLabelPanel;
    }

    public ModuleViewSelectorPanel getModuleViewSelectorPanel() {
	if (ivjModuleViewSelectorPanel == null) {
	    try {
		ivjModuleViewSelectorPanel = new ModuleViewSelectorPanel();
	    } catch (Exception e) {
		handleException(e);
	    }
	}
	return ivjModuleViewSelectorPanel;
    }

    /**
     * Return the NavigationToolPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getNavigationToolPanel() {
	if (ivjNavigationToolPanel == null) {
	    try {
		ivjNavigationToolPanel = new javax.swing.JPanel();
		ivjNavigationToolPanel.setName("NavigationToolPanel");
		ivjNavigationToolPanel.setLayout(getNavigationToolPanelFlowLayout());
		if (addModuleViewSelectorPanel) {
		    debugPrintln(3, "adding moduleview selector");
		    getNavigationToolPanel().add(getModuleViewSelectorPanel(), "ModuleViewSelectorPanel");
		}


		final Action theAddAction = addAction;
		if (useAddAction) {
		    getNavigationToolPanel().add(getJToolBarButton1(), getJToolBarButton1().getName());
		    if (addAction != null) {
			getJToolBarButton1().addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
				    theAddAction.actionPerformed(e);
				}
			    });
		    }
		} else {
		    getNavigationToolPanel().add(getJToolBarButton1(), getJToolBarButton1().getName());
		}

		getNavigationToolPanel().add(getJToolBarButton2(), getJToolBarButton2().getName());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjNavigationToolPanel;
    }
    /**
     * Return the NavigationToolPanelFlowLayout property value.
     * @return java.awt.FlowLayout
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private java.awt.FlowLayout getNavigationToolPanelFlowLayout() {
	java.awt.FlowLayout ivjNavigationToolPanelFlowLayout = null;
	try {
	    /* Create part */
	    ivjNavigationToolPanelFlowLayout = new java.awt.FlowLayout();
	    ivjNavigationToolPanelFlowLayout.setVgap(2);
	} catch (java.lang.Throwable ivjExc) {
	    handleException(ivjExc);
	};
	return ivjNavigationToolPanelFlowLayout;
    }
    public String getTitle() {
	return getNavigationLabel().getText();
    }
    /**
     * Called whenever the part throws an exception.
     * @param exception java.lang.Throwable
     */
    private void handleException(java.lang.Throwable exception) {
	PowerloomApp.getInstance().handleException(exception);
    }
    /**
     * Initialize the class.
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void initialize() {
	try {
	    // user code begin {1}
	    // user code end
	    setName("NavigationPanel3");
	    setLayout(new java.awt.BorderLayout());
	    setSize(354, 205);
	    add(getNavigationToolLabelPanel(), "North");
	    add(getNavigationScrollPane(), "Center");
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
	    NavigationPanel3 aNavigationPanel3;
	    aNavigationPanel3 = new NavigationPanel3();
	    frame.setContentPane(aNavigationPanel3);
	    frame.setSize(aNavigationPanel3.getSize());
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
     * Insert the method's description here.
     * Creation date: (3/30/2002 11:06:48 AM)
     */
    public void setTitle(String title) {
	getNavigationLabel().setText(title);
    }

    public javax.swing.JLabel getPubNavigationLabel() {
	return getNavigationLabel();
    }

    public javax.swing.JButton getAddButton() {
	return getJToolBarButton1();
    }

    public javax.swing.JButton getSearchButton() {
	return getJToolBarButton2();
    }
}
