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


// Version: BrowserFrame4.java,v 1.16 2010/02/04 05:16:42 hans Exp

package edu.isi.powerloom.gui.components;

import java.awt.*;
import java.awt.datatransfer.*;
import javax.swing.*;
import java.awt.event.*;
import java.text.*;

import edu.isi.powerloom.gui.xmlobject.*;
import edu.isi.powerloom.gui.serverinterface.*;
import static edu.isi.powerloom.gui.common.Utils.*;

/**
 * Internal Frame for the Powerloom Browser.
 *
 * @see edu.isi.powerloom.gui.components.BrowserPanel4 BrowserFrame4
 * @see edu.isi.powerloom.gui.components.NavigationPanel3 NavigationPanel3
 * @see edu.isi.powerloom.gui.components.PowerloomTrees PowerloomTrees
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */
public class BrowserFrame4 extends PLFrame {
    private javax.swing.JPanel ivjJInternalFrameContentPane = null;
    private BrowserPanel4 ivjBrowserPanel = null;
    private float modulePercnet;
    private float modulePercent;
    public float conceptPercent;
    public float relationPercent;
    public float instancePercent;
    public boolean proportionsAreSaved;
    private PowerloomApp parentApp;
    /**
     * BrowserFrame4 constructor comment.
     */
    public BrowserFrame4() {
	super();
	initialize();
    }
    /**
     * BrowserFrame4 constructor comment.
     * @param title java.lang.String
     */
    public BrowserFrame4(String title) {
	super(title);
    }
    /**
     * BrowserFrame4 constructor comment.
     * @param title java.lang.String
     * @param resizable boolean
     */
    public BrowserFrame4(String title, boolean resizable) {
	super(title, resizable);
    }
    /**
     * BrowserFrame4 constructor comment.
     * @param title java.lang.String
     * @param resizable boolean
     * @param closable boolean
     */
    public BrowserFrame4(String title, boolean resizable, boolean closable) {
	super(title, resizable, closable);
    }
    /**
     * BrowserFrame4 constructor comment.
     * @param title java.lang.String
     * @param resizable boolean
     * @param closable boolean
     * @param maximizable boolean
     */
    public BrowserFrame4(String title, boolean resizable, boolean closable, boolean maximizable) {
	super(title, resizable, closable, maximizable);
    }
    /**
     * BrowserFrame4 constructor comment.
     * @param title java.lang.String
     * @param resizable boolean
     * @param closable boolean
     * @param maximizable boolean
     * @param iconifiable boolean
     */
    public BrowserFrame4(String title, boolean resizable, boolean closable, boolean maximizable, boolean iconifiable) {
	super(title, resizable, closable, maximizable, iconifiable);
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/8/2002 5:01:39 PM)
     * @return redesign.gui.components.PowerloomApp
     */
    public PowerloomApp getApp() {
	return parentApp;
    }
    /**
     * Return the BrowserPanel property value.
     * @return redesign.gui.components.BrowserPanel4
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    BrowserPanel4 getBrowserPanel() {
	if (ivjBrowserPanel == null) {
	    try {
		ivjBrowserPanel = new edu.isi.powerloom.gui.components.BrowserPanel4(this);
		ivjBrowserPanel.setName("BrowserPanel");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjBrowserPanel;
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
		ivjJInternalFrameContentPane.add(getBrowserPanel(), "Center");
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
     * Insert the method's description here.
     * Creation date: (4/30/2002 7:20:21 PM)
     * @return redesign.gui.components.BrowserPanel4
     */
    public BrowserPanel4 getPubBrowserPanel() {
	return getBrowserPanel();
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
	    setName("BrowserFrame4");
	    setTitle("Browsing KB@localhost:8080");
	    setIconifiable(true);
	    setClosable(true);
	    setSize(1050, 750);
	    setResizable(true);
	    setContentPane(getJInternalFrameContentPane());
	} catch (java.lang.Throwable ivjExc) {
	    handleException(ivjExc);
	}
	// user code begin {2}
	savePanelProportions(.2f, .3f, .3f, .2f);
	getBrowserPanel().addComponentListener(new ComponentAdapter() {
		public void componentResized(ComponentEvent e) {
		    setDividerLocations();
		}});
	getBrowserPanel().setParentFrame(this);
	setMaximizable(true);
	setApp(PowerloomApp.getInstance());
	// user code end
    }

    public Icon getFrameIcon() {
	return PowerloomApp.getInstance().getImage("resources/images/browser.gif");
    }
    

    /**
     * main entrypoint - starts the part when it is run as an application
     * @param args java.lang.String[]
     */
    public static void main(java.lang.String[] args) {
	try {
	    javax.swing.JFrame frame = new javax.swing.JFrame();
	    BrowserFrame4 aBrowserFrame4;
	    aBrowserFrame4 = new BrowserFrame4();
	    frame.setContentPane(aBrowserFrame4);
	    frame.setSize(aBrowserFrame4.getSize());
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
     * Insert the method's description here.
     * Creation date: (4/8/2002 3:26:27 PM)
     */
    public void printstats() {
	debugPrintln(3, "~~~~~~~~~~~~~~~~~~~~");
	debugPrintln(3, "MCRIPR Divider location    : " + getBrowserPanel().getMCRIPRSplitPane().getDividerLocation());
	debugPrintln(3, "MCRI Divider location    : " + getBrowserPanel().getMCRISplitPane().getDividerLocation());
	debugPrintln(3, "ModuleConcept Divider location    : " + getBrowserPanel().getModuleConceptSplitPane().getDividerLocation());
	debugPrintln(3, "RelationInstance Divider location    : " + getBrowserPanel().getRelationInstanceSplitPane().getDividerLocation());
	debugPrintln(3, "PropRule Divier location: " + getBrowserPanel().getPropRuleSplitPane().getDividerLocation());
	debugPrintln(3, "Panel Bounds  : " + getBrowserPanel().getBounds());
	int moduleWidth = getBrowserPanel().getModulePanel().getWidth();
	int conceptWidth = getBrowserPanel().getConceptPanel().getWidth();
	int relationWidth = getBrowserPanel().getRelationPanel().getWidth();
	int instanceWidth = getBrowserPanel().getInstancePanel().getWidth();
	int totalWidth = moduleWidth + conceptWidth + instanceWidth + relationWidth;
	debugPrintln(3, "widths: [m=" + moduleWidth + " c=" + conceptWidth + " r=" + relationWidth + " i=" + instanceWidth + "]");
	debugPrintln(3, "proportions: [m=" + NumberFormat.getPercentInstance().format((double)moduleWidth/totalWidth) + " c=" + NumberFormat.getPercentInstance().format((double)conceptWidth/totalWidth) + " r=" + NumberFormat.getPercentInstance().format((double)relationWidth/totalWidth) + " i=" + NumberFormat.getPercentInstance().format((double)instanceWidth/totalWidth) + "]");
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/8/2002 3:23:39 PM)
     */
    void savePanelProportions() {}
    /**
     * Insert the method's description here.
     * Creation date: (4/8/2002 3:23:39 PM)
     */
    void savePanelProportions(float modulePercent, float conceptPercent, float relationPercent, float instancePercent) {
	this.modulePercent = modulePercent;
	this.conceptPercent = conceptPercent;
	this.relationPercent = relationPercent;
	this.instancePercent = instancePercent;
	proportionsAreSaved = true;
	//	System.out.println("saved panelProportions widths: [m=" + moduleWidth + " c=" + conceptWidth + " r=" + relationWidth + " i=" + instanceWidth + "]");

	debugPrintln(3, "saved proportions: [m=" + NumberFormat.getPercentInstance().format((double)modulePercent) + " c=" + NumberFormat.getPercentInstance().format((double)conceptPercent) + " r=" + NumberFormat.getPercentInstance().format((double)relationPercent) + " i=" + NumberFormat.getPercentInstance().format((double)instancePercent) + "]");
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/8/2002 5:01:39 PM)
     * @param newParentApp redesign.gui.components.PowerloomApp
     */
    public void setApp(PowerloomApp newParentApp) {
	parentApp = newParentApp;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/8/2002 5:01:20 PM)
     * @param cursorType int
     */
    public void setCursor(int cursorType) {
	getApp().setCursor(new java.awt.Cursor(cursorType));
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/8/2002 3:19:21 PM)
     */
    void setDividerLocations() {
	if (!proportionsAreSaved) {
	    return;
	}
	int dividerSize = getBrowserPanel().getRelationInstanceSplitPane().getDividerSize();
	// compute panel widths
	int panelWidth = getBrowserPanel().getWidth();
	int usableWidth = panelWidth - (dividerSize * 3);
	int moduleWidth = (int)(modulePercent * usableWidth);
	int conceptWidth = (int)(conceptPercent * usableWidth);
	int relationWidth = (int)(relationPercent * usableWidth);
	int instanceWidth = (int)(instancePercent * usableWidth);

	// compute divider locations
	int conceptModuleLocation = moduleWidth + 1;
	int relationInstanceLocation = relationWidth + 1;
	int mcriLocation = moduleWidth + relationWidth + dividerSize;

	// set the locations
	getBrowserPanel().getMCRISplitPane().setDividerLocation(mcriLocation);
	getBrowserPanel().getModuleConceptSplitPane().setDividerLocation(conceptModuleLocation);
	getBrowserPanel().getRelationInstanceSplitPane().setDividerLocation(relationInstanceLocation);
    }

    /**
     * Implementation of PLFrame
     */
    public PLModule getModule() {
	PowerloomTrees trees = getPubBrowserPanel().getPowerloomTrees();
	return trees.getMostRecentlyTouchedModule();
    }


    /**
     *  PLEditListener implementation
     */
    public void performEdit(PLEditEvent event) {
	getBrowserPanel().getPowerloomTrees().performEdit(event);
    }

    public void displayFrame() {
	super.displayFrame();
	setDividerLocations();
    }

}
