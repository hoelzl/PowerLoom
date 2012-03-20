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


// Version: TableAdderPanel.java,v 1.6 2010/02/04 05:19:44 hans Exp

package edu.isi.powerloom.gui.components;

import javax.swing.table.*;
import static edu.isi.powerloom.gui.common.Utils.*;

/**
 * Panel that contains add and delete buttons for adding rows to a table.
 *
 * @since 4/2/2002 5:21:55 PM
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */
public class TableAdderPanel extends javax.swing.AbstractButton {
    private javax.swing.JButton ivjDefaultToolBarButton = null;
    private javax.swing.JScrollPane ivjJScrollPane1 = null;
    private javax.swing.JToolBar ivjJToolBar1 = null;
    private javax.swing.JButton ivjJToolBarButton1 = null;
    private javax.swing.JTable table;
    private int someAttr;
    /**
     * TableAdderPanel constructor comment.
     */
    public TableAdderPanel() {
	super();
	initialize();
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/14/2002 8:46:54 PM)
     * @param row java.lang.Object[]
     */
    public void addRow(Object[] row) {
	((DefaultTableModel)getTable().getModel()).addRow(row);
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/2/2002 8:26:24 PM)
     * @param e java.awt.event.ActionEvent
     */
    private void fireAction(java.awt.event.ActionEvent e) {
	fireActionPerformed(e);
    }
    /**
     * Return the DefaultToolBarButton property value.
     * @return javax.swing.JButton
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JButton getAddToolBarButton() {
	if (ivjDefaultToolBarButton == null) {
	    try {
		ivjDefaultToolBarButton = new javax.swing.JButton();
		ivjDefaultToolBarButton.setName("DefaultToolBarButton");
		ivjDefaultToolBarButton.setIcon(PowerloomApp.getInstance().getImage("resources/images/add.gif"));
		ivjDefaultToolBarButton.setMaximumSize(new java.awt.Dimension(20, 20));
		ivjDefaultToolBarButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
		ivjDefaultToolBarButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
		ivjDefaultToolBarButton.setPreferredSize(new java.awt.Dimension(20, 20));
		ivjDefaultToolBarButton.setMargin(new java.awt.Insets(0, 0, 0, 0));
		ivjDefaultToolBarButton.setMinimumSize(new java.awt.Dimension(20, 20));
		// user code begin {1}
		ivjDefaultToolBarButton.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent e) {
			    java.awt.event.ActionEvent e2 = new java.awt.event.ActionEvent(e.getSource(), -1, "add");
			    fireAction(e2);
			}
		    });				
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjDefaultToolBarButton;
    }
    /**
     * Return the JToolBarButton1 property value.
     * @return javax.swing.JButton
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JButton getDeleteToolBarButton() {
	if (ivjJToolBarButton1 == null) {
	    try {
		ivjJToolBarButton1 = new javax.swing.JButton();
		ivjJToolBarButton1.setName("JToolBarButton1");
		ivjJToolBarButton1.setIcon(PowerloomApp.getInstance().getImage("resources/images/delete.gif"));
		ivjJToolBarButton1.setMaximumSize(new java.awt.Dimension(20, 20));
		ivjJToolBarButton1.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
		ivjJToolBarButton1.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
		ivjJToolBarButton1.setPreferredSize(new java.awt.Dimension(20, 20));
		ivjJToolBarButton1.setMargin(new java.awt.Insets(0, 0, 0, 0));
		ivjJToolBarButton1.setMinimumSize(new java.awt.Dimension(20, 20));
		// user code begin {1}
		ivjJToolBarButton1.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent e) {
			    java.awt.event.ActionEvent e2 = new java.awt.event.ActionEvent(e.getSource(), -1, "delete");
			    fireAction(e2);
			}
		    });				
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
     * Return the JScrollPane1 property value.
     * @return javax.swing.JScrollPane
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JScrollPane getJScrollPane1() {
	if (ivjJScrollPane1 == null) {
	    try {
		ivjJScrollPane1 = new javax.swing.JScrollPane();
		ivjJScrollPane1.setName("JScrollPane1");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjJScrollPane1;
    }
    /**
     * Return the JToolBar1 property value.
     * @return javax.swing.JToolBar
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JToolBar getJToolBar1() {
	if (ivjJToolBar1 == null) {
	    try {
		ivjJToolBar1 = new javax.swing.JToolBar();
		ivjJToolBar1.setName("JToolBar1");
		ivjJToolBar1.setFloatable(false);
		ivjJToolBar1.add(getAddToolBarButton());
		getJToolBar1().add(getDeleteToolBarButton(), getDeleteToolBarButton().getName());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjJToolBar1;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/2/2002 5:33:24 PM)
     * @return javax.swing.JTable
     */
    public javax.swing.JTable getTable() {
	return table;
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
	    setName("TableAdderPanel");
	    setLayout(new java.awt.BorderLayout());
	    setSize(394, 181);
	    add(getJToolBar1(), "North");
	    add(getJScrollPane1(), "Center");
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
	    TableAdderPanel aTableAdderPanel;
	    aTableAdderPanel = new TableAdderPanel();
	    frame.setContentPane(aTableAdderPanel);
	    frame.setSize(aTableAdderPanel.getSize());
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
     * Creation date: (4/14/2002 8:47:21 PM)
     */
    public void removeSelectedRow() {
	int selectedIndex = getTable().getSelectedRow();
	if (selectedIndex >=0) {
	    ((DefaultTableModel)getTable().getModel()).removeRow(selectedIndex);
	}
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/2/2002 5:33:24 PM)
     * @param newTable javax.swing.JTable
     */
    public void setTable(javax.swing.JTable newTable) {
	table = newTable;
	getJScrollPane1().setViewportView(table);
    }
}
