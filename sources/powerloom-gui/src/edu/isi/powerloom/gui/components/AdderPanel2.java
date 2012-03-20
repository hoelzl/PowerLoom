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


// Version: AdderPanel2.java,v 1.8 2010/02/04 05:16:37 hans Exp

package edu.isi.powerloom.gui.components;

import javax.swing.*;
import java.util.*;
import edu.isi.powerloom.gui.xmlobject.*;

/**
 * Panel which contains a JList and buttons for adding and deleting list items.
 * @since 4/1/2002 7:25:06 PM
 * @author Eric Melz
 */
public class AdderPanel2 extends javax.swing.AbstractButton {
    private javax.swing.JButton ivjAddToolBarButton = null;
    private javax.swing.JList ivjJList = null;
    private javax.swing.JScrollPane ivjJScrollPane1 = null;
    private javax.swing.JToolBar ivjJToolBar1 = null;
    private javax.swing.JButton ivjDeleteToolBarButton = null;
    private PowerloomApp app;

    /**
     * AdderPanel2 constructor comment.
     */
    public AdderPanel2(PLModule module) {
	super();
	setModule(module);
	initialize(module);
    }

    public void setModule(PLModule module) {
	ListModel model = getJList().getModel();
	if (model instanceof PLListModel) {
	    ((PLListModel)model).setModule(module);
	}
    }

    /**
     * Insert the method's description here.
     * Creation date: (4/11/2002 4:38:58 PM)
     * @param object edu.isi.powerloom.gui.xmlobject.PLObject
     */
    public void addPLObject(PLObject object) {
	try {
	    PLListModel model = (PLListModel)(getJList().getModel());
	    model.addPLObject(object);
	} catch (Exception e) {
	    handleException(e);
	} 
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
     * Return the AddToolBarButton property value.
     * @return javax.swing.JButton
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    public javax.swing.JButton getAddToolBarButton() {
	if (ivjAddToolBarButton == null) {
	    try {
		ivjAddToolBarButton = new javax.swing.JButton();
		ivjAddToolBarButton.setName("AddToolBarButton");
		ivjAddToolBarButton.setIcon(PowerloomApp.getInstance().getImage("resources/images/add.gif"));
		ivjAddToolBarButton.setMaximumSize(new java.awt.Dimension(20, 20));
		ivjAddToolBarButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
		ivjAddToolBarButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
		ivjAddToolBarButton.setPreferredSize(new java.awt.Dimension(20, 20));
		ivjAddToolBarButton.setMargin(new java.awt.Insets(0, 0, 0, 0));
		ivjAddToolBarButton.setMinimumSize(new java.awt.Dimension(20, 20));
		// user code begin {1}
		ivjAddToolBarButton.addActionListener(new java.awt.event.ActionListener() {
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
	return ivjAddToolBarButton;
    }
    /**
     * Return the DeleteToolBarButton property value.
     * @return javax.swing.JButton
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    public javax.swing.JButton getDeleteToolBarButton() {
	if (ivjDeleteToolBarButton == null) {
	    try {
		ivjDeleteToolBarButton = new javax.swing.JButton();
		ivjDeleteToolBarButton.setName("DeleteToolBarButton");
		ivjDeleteToolBarButton.setIcon(PowerloomApp.getInstance().getImage("resources/images/delete.gif"));
		ivjDeleteToolBarButton.setMaximumSize(new java.awt.Dimension(20, 20));
		ivjDeleteToolBarButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
		ivjDeleteToolBarButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
		ivjDeleteToolBarButton.setPreferredSize(new java.awt.Dimension(20, 20));
		ivjDeleteToolBarButton.setMargin(new java.awt.Insets(0, 0, 0, 0));
		ivjDeleteToolBarButton.setMinimumSize(new java.awt.Dimension(20, 20));
		// user code begin {1}
		ivjDeleteToolBarButton.addActionListener(new java.awt.event.ActionListener() {
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
	return ivjDeleteToolBarButton;
    }
    /**
     * Return the JList1 property value.
     * @return javax.swing.JList
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    javax.swing.JList getJList() {
	if (ivjJList == null) {
	    try {
		ivjJList = new javax.swing.JList();
		ivjJList.setName("JList");
		ivjJList.setBounds(0, 0, 160, 120);
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjJList;
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
		getJScrollPane1().setViewportView(getJList());
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
     * Called whenever the part throws an exception.
     * @param exception java.lang.Throwable
     */
    private void handleException(java.lang.Throwable exception) {
	if (app != null) {
	    PowerloomApp.getInstance().handleException(exception);
	} else {
	    // System.out.println("--------- UNCAUGHT EXCEPTION ---------");
	    // exception.printStackTrace(System.out);
	}
    }
    /**
     * Initialize the class.
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void initialize(PLModule module) {
	try {
	    // user code begin {1}
	    // user code end
	    setName("AdderPanel2");
	    setLayout(new java.awt.BorderLayout());
	    setText("AdderPanel2");
	    setSize(372, 164);
	    add(getJToolBar1(), "North");
	    add(getJScrollPane1(), "Center");
	} catch (java.lang.Throwable ivjExc) {
	    handleException(ivjExc);
	}
	// user code begin {2}
	// setup listmodel in list.
	ArrayList supersList = new ArrayList();
	PLSurrogateContainer supers = new edu.isi.powerloom.gui.xmlobject.PLSurrogateContainer(supersList);
	PLListModel model = new PLListModel(module, supers);
	getJList().setModel(model);	
	getJList().setCellRenderer(new PLListRenderer());
	// user code end
    }

    /**
     * Insert the method's description here.
     * Creation date: (4/12/2002 1:02:21 PM)
     * @param object edu.isi.powerloom.gui.xmlobject.PLObject
     */
    public void removePLObject(PLObject object) {
	try {
	    PLListModel model = (PLListModel)(getJList().getModel());
	    model.removePLObject(object);
	} catch (Exception e) {
	    handleException(e);
	} 
    }

    /**
     * Insert the method's description here.
     * Creation date: (4/9/2002 7:29:32 PM)
     * @param newList javax.swing.JList
     */
    public void setList(javax.swing.JList newList) {
	ivjJList = newList;
	if (newList != null) {
	    getJScrollPane1().setViewportView(newList);
	    getJList().setCellRenderer(new PLListRenderer());
	}		
    }
}
