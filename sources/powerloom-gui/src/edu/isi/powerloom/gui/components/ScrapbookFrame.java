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


// Version: ScrapbookFrame.java,v 1.7 2010/02/04 05:19:32 hans Exp

package edu.isi.powerloom.gui.components;

import edu.isi.powerloom.gui.xmlobject.*;
import static edu.isi.powerloom.gui.common.Utils.*;

/**
 * Frame for scrapbook.
 * 
 * @since 4/6/2002 3:26:11 PM
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */
public class ScrapbookFrame extends PLFrame {
    private javax.swing.JPanel ivjJInternalFrameContentPane = null;
    private javax.swing.JPanel ivjScrapbookPanel = null;
    /**
     * ScrapbookFrame constructor comment.
     */
    public ScrapbookFrame() {
	super();
	initialize();
    }
    /**
     * ScrapbookFrame constructor comment.
     * @param title java.lang.String
     */
    public ScrapbookFrame(String title) {
	super(title);
    }
    /**
     * ScrapbookFrame constructor comment.
     * @param title java.lang.String
     * @param resizable boolean
     */
    public ScrapbookFrame(String title, boolean resizable) {
	super(title, resizable);
    }
    /**
     * ScrapbookFrame constructor comment.
     * @param title java.lang.String
     * @param resizable boolean
     * @param closable boolean
     */
    public ScrapbookFrame(String title, boolean resizable, boolean closable) {
	super(title, resizable, closable);
    }
    /**
     * ScrapbookFrame constructor comment.
     * @param title java.lang.String
     * @param resizable boolean
     * @param closable boolean
     * @param maximizable boolean
     */
    public ScrapbookFrame(String title, boolean resizable, boolean closable, boolean maximizable) {
	super(title, resizable, closable, maximizable);
    }
    /**
     * ScrapbookFrame constructor comment.
     * @param title java.lang.String
     * @param resizable boolean
     * @param closable boolean
     * @param maximizable boolean
     * @param iconifiable boolean
     */
    public ScrapbookFrame(String title, boolean resizable, boolean closable, boolean maximizable, boolean iconifiable) {
	super(title, resizable, closable, maximizable, iconifiable);
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/6/2002 3:38:36 PM)
     * @param object java.lang.Object
     * @param x int
     * @param y int
     */
    public void addScrapbookItem(Object object, int x, int y) {
	ScrapbookItem item = new ScrapbookItem(object, getScrapbookPanel());
	getScrapbookPanel().add(item);
	java.awt.Point point = new java.awt.Point(x, y);
	item.setLocation(point);
	item.setVisible(true);
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
		ivjJInternalFrameContentPane.setLayout(new java.awt.BorderLayout());
		getJInternalFrameContentPane().add(getScrapbookPanel(), "Center");
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
     * Return the ScrapbookPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getScrapbookPanel() {
	if (ivjScrapbookPanel == null) {
	    try {
		ivjScrapbookPanel = new javax.swing.JPanel();
		ivjScrapbookPanel.setName("ScrapbookPanel");
		ivjScrapbookPanel.setLayout(null);
		ivjScrapbookPanel.setBackground(new java.awt.Color(198,255,210));
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjScrapbookPanel;
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
	    setName("ScrapbookFrame");
	    setResizable(true);
	    setClosable(true);
	    setIconifiable(true);
	    setSize(395, 375);
	    setTitle("Scrapbook");
	    setContentPane(getJInternalFrameContentPane());
	} catch (java.lang.Throwable ivjExc) {
	    handleException(ivjExc);
	}
	// user code begin {2}
	addScrapbookItem("Test Item", 100, 100);
	// user code end
    }
    /**
     * main entrypoint - starts the part when it is run as an application
     * @param args java.lang.String[]
     */
    public static void main(java.lang.String[] args) {
	try {
	    javax.swing.JFrame frame = new javax.swing.JFrame();
	    ScrapbookFrame aScrapbookFrame;
	    aScrapbookFrame = new ScrapbookFrame();
	    frame.setContentPane(aScrapbookFrame);
	    frame.setSize(aScrapbookFrame.getSize());
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
     * Implementation of PLFrame
     */
    public PLModule getModule() {
	return null;
    }
}
