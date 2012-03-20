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


// Version: PLFrame.java,v 1.9 2010/02/04 05:18:24 hans Exp

package edu.isi.powerloom.gui.components;

import edu.isi.powerloom.gui.xmlobject.*;
import static edu.isi.powerloom.gui.common.Utils.*;

import java.awt.datatransfer.*;
import java.awt.event.*;
import java.awt.*;
import javax.swing.event.*;
import javax.swing.*;
import java.util.*;

/**
 * Abstract base class for interal frames which includes common functionality
 * such as edit listener management, clipboard management, open/close actions,
 * positioning, etc.
 *
 * @since Mon Sep 30 15:02:44 2002
 * @author <a href="mailto:melz@isi.edu">Eric Melz</a>
 */
	
public abstract class PLFrame extends JInternalFrame implements PLEditListener, PLClipboardOwnerParent {
    private PLClipboardOwner lastFocusedPLClipboardOwner;

    public PLFrame() {
	super();
	initializePLFrame();
    }
    public PLFrame(String title) {
	super(title);
    }

    public PLFrame(String title, boolean resizable) {
	super(title, resizable);
    }

    public PLFrame(String title, boolean resizable, boolean closable) {
	super(title, resizable, closable);
    }

    public PLFrame(String title, boolean resizable, boolean closable, boolean maximizable) {
	super(title, resizable, closable, maximizable);
    }

    public PLFrame(String title, boolean resizable, boolean closable, boolean maximizable, boolean iconifiable) {
	super(title, resizable, closable, maximizable, iconifiable);
    }

    protected void initializePLFrame() {
	addPLEditListener(this);
	// Add edit listener registering code,
	// on-open code (for adding to window menu (maybe..))
	// on-close code (for deleting from windows menu, unregistering edit listener))
	// positioning code, 
	// 
    }

    private void addPLEditListener(PLEditListener listener) {
	PowerloomApp.getInstance().addPLEditListener(listener);
    }

    protected void fireEditPerformed(PLEditEvent event) {
	PowerloomApp.getInstance().fireEditPerformed(event);
    }

    // Subclasses should override this
    public void performEdit(PLEditEvent e) {
	debugPrintln(3, "Perform edit is not yet implemented for this subclass...");
    }

    // Frame setup code
    public void displayFrame() {
	PowerloomApp.getInstance().getPubJDesktopPane().add(this);
	WindowSelectAction selectAction = new WindowSelectAction(this);
	JMenuItem item = new JMenuItem(selectAction);
	item.setIcon(getFrameIcon());
	PowerloomApp.getInstance().getWindowMenu().add(item);
	InternalFrameListener listener = new InternalFrameAdapter() {
		public void internalFrameClosing(InternalFrameEvent e) {
		    JInternalFrame closingFrame = (JInternalFrame)e.getSource();
		    String title = closingFrame.getTitle();
		    debugPrintln(3, "** internalframe: " + title + " is being closed.");
		    PowerloomApp.getInstance().removeFrameFromWindowMenu(closingFrame);
		    PowerloomApp.getInstance().removePLEditListener(PLFrame.this);
		}
	    };
	this.addInternalFrameListener(listener);
	PowerloomApp.getInstance().positionFrame(this);
	show();
    }

    // subclasses can override this...
    public Icon getFrameIcon() {
	return super.getFrameIcon();
    }

    /* I think these are obsolete... remove if so
    public void doCut() {
	System.out.println("doCut() is not implemented for this PLFrame");
    }

    public void doCopy() {
	System.out.println("doCopy() is not implemented for this PLFrame");
    }
    
    public void doPaste(Transferable transferable) {
	System.out.println("doPaste() is not implemented for this PLFrame");
    }

    public void lostOwnership(Clipboard c, Transferable t) {
	System.out.println("lostOwnerShip() is not implemented for this PLFrame");
    }

    */

    /**
     * Retrieve a child component which has the focus, or null if none exists.  I don't think this is used.
     */
    Component findComponentWithFocus() {
	return helpFindComponentWithFocus(this, 0);
    }

    Component helpFindComponentWithFocus(Component root, int indent) {
	for (int i = 0; i < indent; i++) {
	    System.out.print(" ");
	}
	debugPrintln(3, "examinining: " + root.getClass() + "(" + root.hashCode() + ") hasFocus = " + root.hasFocus());
	if (root.hasFocus()) {
	    return root;
	}
	if (root instanceof Container) {
	    Component[] children = ((Container)root).getComponents();
	    for (int i = 0; i < children.length; i++) {
		Component child = children[i];
		Component foundComponent = helpFindComponentWithFocus((Container)child, indent+2);
		if (foundComponent != null) {
		    return foundComponent;
		}
	    }
	}
	return null;
    }

    /**
     * New stuff
     */
    public void setLastFocusedPLClipboardOwner(PLClipboardOwner subComponent) {
	lastFocusedPLClipboardOwner = subComponent;
    }

    public PLClipboardOwner getLastFocusedPLClipboardOwner() {
	return 	lastFocusedPLClipboardOwner;
    }

    public abstract PLModule getModule();
}
