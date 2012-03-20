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


// Version: ScrapbookItem.java,v 1.5 2010/02/04 05:19:34 hans Exp

package edu.isi.powerloom.gui.components;

import static edu.isi.powerloom.gui.common.Utils.*;

/**
 * Item used for scrapbook.

 * @since 4/6/2002 3:33:34 PM
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */
public class ScrapbookItem extends javax.swing.JLabel {
    public javax.swing.JPanel scrapbookPanel;
    private java.lang.Object object;
    /**
     * ScrapbookLabel constructor comment.
     */
    public ScrapbookItem() {
	super();
	initialize();
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/6/2002 3:43:11 PM)
     * @param object java.lang.Object
     * @param scrapbookPanel javax.swing.JPanel
     */
    public ScrapbookItem(Object object, javax.swing.JPanel scrapbookPanel) {
	super();
	setObject(object);
	setScrapbookPanel(scrapbookPanel);
    }
    /**
     * ScrapbookLabel constructor comment.
     * @param text java.lang.String
     */
    public ScrapbookItem(String text) {
	super(text);
    }
    /**
     * ScrapbookLabel constructor comment.
     * @param text java.lang.String
     * @param horizontalAlignment int
     */
    public ScrapbookItem(String text, int horizontalAlignment) {
	super(text, horizontalAlignment);
    }
    /**
     * ScrapbookLabel constructor comment.
     * @param text java.lang.String
     * @param icon javax.swing.Icon
     * @param horizontalAlignment int
     */
    public ScrapbookItem(String text, javax.swing.Icon icon, int horizontalAlignment) {
	super(text, icon, horizontalAlignment);
    }
    /**
     * ScrapbookLabel constructor comment.
     * @param image javax.swing.Icon
     */
    public ScrapbookItem(javax.swing.Icon image) {
	super(image);
    }
    /**
     * ScrapbookLabel constructor comment.
     * @param image javax.swing.Icon
     * @param horizontalAlignment int
     */
    public ScrapbookItem(javax.swing.Icon image, int horizontalAlignment) {
	super(image, horizontalAlignment);
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/6/2002 3:37:13 PM)
     * @return java.lang.Object
     */
    public java.lang.Object getObject() {
	return object;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/6/2002 3:36:02 PM)
     * @return javax.swing.JPanel
     */
    public javax.swing.JPanel getScrapbookPanel() {
	return scrapbookPanel;
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
	    setName("ScrapbookItem");
	    setSize(93, 14);
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
	    ScrapbookItem aScrapbookItem;
	    aScrapbookItem = new ScrapbookItem();
	    frame.setContentPane(aScrapbookItem);
	    frame.setSize(aScrapbookItem.getSize());
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
	    System.err.println("Exception occurred in main() of redesign.gui.components.ScrapbookLabel");
	    exception.printStackTrace(System.out);
	}
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/6/2002 3:37:13 PM)
     * @param newObject java.lang.Object
     */
    public void setObject(java.lang.Object newObject) {
	object = newObject;
	setText(object.toString());
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/6/2002 3:36:02 PM)
     * @param newScrapbookPanel javax.swing.JPanel
     */
    public void setScrapbookPanel(javax.swing.JPanel newScrapbookPanel) {
	scrapbookPanel = newScrapbookPanel;
    }
}
