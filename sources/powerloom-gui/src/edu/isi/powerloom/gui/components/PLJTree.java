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


// Version: PLJTree.java,v 1.8 2010/02/05 00:24:13 hans Exp

package edu.isi.powerloom.gui.components;

import edu.isi.powerloom.gui.xmlobject.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.tree.*;
import java.awt.datatransfer.*;
import java.util.*;

/**
 * Subclass of JTree which supports cut/copy/paste/delete
 *
 * @since Thu Oct  3 16:17:55 2002
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */
public class PLJTree extends JTree implements PLClipboardOwner {
    protected PLClipboardOwnerSupport support;

    public PLJTree (PLTreeModel model, PLFrame parent, String name, List types) {
	super(model);
	support = new PLClipboardOwnerSupport(name, types, parent, this);
	initialize(parent);
    }
    
    private void initialize(PLFrame parent) {
	// Setup input maps to add datatransfer key bindings.
	ActionMap defaultActionMap = parent.getActionMap();
	ActionMap newActionMap = new ActionMap();
	newActionMap.setParent(defaultActionMap);
	KeyStroke copyKey = KeyStroke.getKeyStroke(KeyEvent.VK_C, InputEvent.CTRL_MASK, false);
	KeyStroke cutKey = KeyStroke.getKeyStroke(KeyEvent.VK_X, InputEvent.CTRL_MASK, false);
	KeyStroke pasteKey = KeyStroke.getKeyStroke(KeyEvent.VK_V, InputEvent.CTRL_MASK, false);
	newActionMap.put(copyKey, new CopyAction());
	newActionMap.put(cutKey, new CutAction());
	newActionMap.put(pasteKey, new PasteAction());
	InputMap defaultInputMap = parent.getInputMap();
	InputMap newInputMap = new InputMap();
	newInputMap.setParent(defaultInputMap);
	newInputMap.put(copyKey, newActionMap);
	newInputMap.put(cutKey, newActionMap);
	newInputMap.put(pasteKey, newActionMap);
	SwingUtilities.replaceUIInputMap(this, JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT, newInputMap);
	SwingUtilities.replaceUIActionMap(this, newActionMap);
    }

    public void doCut(Object object) {
	System.err.println("doCut is not implemented for this object: " + this);
    }

    public void doDelete(PLModule module, Object object) {
	System.err.println("doDelete is not implemented for this object: " + this);
    }

    public void doCopy(Object object) {
	System.err.println("doCopy is not implemented for this object: " + this);
    }

    public boolean supportsPLObjects() {
	return true;
    }

    public void lostOwnership(Clipboard c, Transferable t) {}

    public String getName() {
	return support.getName();
    }

    public List getTypes() {
	return support.getTypes();
    }

    public Object getSelectedObject() {
	TreePath path = getSelectionPath();
	if (path != null) {
	    return path.getLastPathComponent();
	}
	return null;
    }

    public PLModule getSelectedModule() {
	return support.getParent().getModule();
    }

    public void doPasteFromCut(PLModule module, Object object) {
	System.err.println("Warning: doPasteFromCut is not implemented.  Use a subclass to implement this method.");
    }

    public void doPasteFromCopy(PLModule module, Object object) {
	System.err.println("Warning: doPasteFromCopy is not implemented.  Use a subclass to implement this method.");
    }
}// PLJTree
