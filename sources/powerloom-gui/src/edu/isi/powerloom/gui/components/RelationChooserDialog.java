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


// Version: RelationChooserDialog.java,v 1.5 2010/02/04 05:19:14 hans Exp

package edu.isi.powerloom.gui.components;

import edu.isi.powerloom.gui.serverinterface.*;
import edu.isi.powerloom.gui.xmlobject.*;
import static edu.isi.powerloom.gui.common.Utils.*;

/**
 * Dialog for choosing a relation from a list.
 *
 * @since 4/2/2002 6:56:42 PM
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */
public class RelationChooserDialog extends ChooserDialog {
    private ChooserPanel ivjChooserPanel1 = null;
    private javax.swing.JPanel ivjJDialogContentPane = null;
    /**
     * ConceptChooserDialog constructor comment.
     */
    public RelationChooserDialog() {
	super();
	initialize();
    }
    /**
     * relationChooserDialog constructor comment.
     * @param owner java.awt.Dialog
     */
    public RelationChooserDialog(java.awt.Dialog owner) {
	super(owner);
    }
    /**
     * relationChooserDialog constructor comment.
     * @param owner java.awt.Dialog
     * @param title java.lang.String
     */
    public RelationChooserDialog(java.awt.Dialog owner, String title) {
	super(owner, title);
    }
    /**
     * relationChooserDialog constructor comment.
     * @param owner java.awt.Dialog
     * @param title java.lang.String
     * @param modal boolean
     */
    public RelationChooserDialog(java.awt.Dialog owner, String title, boolean modal) {
	super(owner, title, modal);
    }
    /**
     * relationChooserDialog constructor comment.
     * @param owner java.awt.Dialog
     * @param modal boolean
     */
    public RelationChooserDialog(java.awt.Dialog owner, boolean modal) {
	super(owner, modal);
    }
    /**
     * relationChooserDialog constructor comment.
     * @param owner java.awt.Frame
     */
    public RelationChooserDialog(java.awt.Frame owner) {
	super(owner);
    }
    /**
     * relationChooserDialog constructor comment.
     * @param owner java.awt.Frame
     * @param title java.lang.String
     */
    public RelationChooserDialog(java.awt.Frame owner, String title) {
	super(owner, title);
    }
    /**
     * relationChooserDialog constructor comment.
     * @param owner java.awt.Frame
     * @param title java.lang.String
     * @param modal boolean
     */
    public RelationChooserDialog(java.awt.Frame owner, String title, boolean modal) {
	super(owner, title, modal);
    }
    /**
     * relationChooserDialog constructor comment.
     * @param owner java.awt.Frame
     * @param modal boolean
     */
    public RelationChooserDialog(java.awt.Frame owner, boolean modal) {
	super(owner, modal);
    }
    /**
     * Return the ChooserPanel1 property value.
     * @return redesign.gui.components.ChooserPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    public ChooserPanel getChooserPanel() {
	if (ivjChooserPanel1 == null) {
	    try {
		ivjChooserPanel1 = new edu.isi.powerloom.gui.components.ChooserPanel();
		ivjChooserPanel1.setName("ChooserPanel1");
		ivjChooserPanel1.setDialog(this);
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjChooserPanel1;
    }
    /**
     * Return the ChooserPanel1 property value.
     * @return redesign.gui.components.ChooserPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private ChooserPanel getChooserPanel1() {
	if (ivjChooserPanel1 == null) {
	    try {
		ivjChooserPanel1 = new edu.isi.powerloom.gui.components.ChooserPanel();
		ivjChooserPanel1.setName("ChooserPanel1");
		ivjChooserPanel1.setDialog(this);
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjChooserPanel1;
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
		ivjJDialogContentPane.setBorder(new javax.swing.border.EmptyBorder(15,15,15,15));
		ivjJDialogContentPane.setLayout(new java.awt.BorderLayout());
		getJDialogContentPane().add(getChooserPanel1(), "Center");
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
	    setName("ConceptChooserDialog");
	    setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
	    setSize(383, 439);
	    setTitle("Choose Relation");
	    setContentPane(getJDialogContentPane());
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
	getChooserPanel().getEnterLabel().setText("Enter Relation Name:");	
	// user code end
    }
    /**
     * main entrypoint - starts the part when it is run as an application
     * @param args java.lang.String[]
     */
    public static void main(java.lang.String[] args) {
	try {
	    RelationChooserDialog aRelationChooserDialog;
	    aRelationChooserDialog = new RelationChooserDialog();
	    aRelationChooserDialog.setModal(true);
	    aRelationChooserDialog.addWindowListener(new java.awt.event.WindowAdapter() {
		    public void windowClosing(java.awt.event.WindowEvent e) {
			exitGui(0);
		    };
		});
	    aRelationChooserDialog.show();
	    java.awt.Insets insets = aRelationChooserDialog.getInsets();
	    aRelationChooserDialog.setSize(aRelationChooserDialog.getWidth() + insets.left + insets.right, aRelationChooserDialog.getHeight() + insets.top + insets.bottom);
	    aRelationChooserDialog.setVisible(true);
	} catch (Throwable exception) {
	    System.err.println("Exception occurred in main() of javax.swing.JDialog");
	    exception.printStackTrace(System.out);
	}
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/12/2002 12:05:46 PM)
     * @param module edu.isi.powerloom.gui.xmlobject.PLModule
     */
    public void setModule(PLModule module) {
	if (module == null) {
	    return;
	}
	try {
	    PLSurrogateContainer relations = KnowledgeManager.getInstance().getRelationsForModule(module).listifyTreeContainer();
	    getChooserPanel().setAllItems(module, relations);
	} catch (Exception e) {
	    handleException(e);
	} // end of try-catch	
    }
}
