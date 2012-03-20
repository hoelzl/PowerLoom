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


// Version: PLClipboardOwnerSupport.java,v 1.7 2010/02/04 05:18:17 hans Exp

package edu.isi.powerloom.gui.components;

import javax.swing.*;
import java.awt.event.*;
import java.util.*;
import static edu.isi.powerloom.gui.common.Utils.*;

/**
 * Support class for classes which implmenet PLClipboardOwner.
 * Contains state (such as an objects name, its parent PLFrame, etc).
 * and provides utilities for registering the PLClipboardOwner, etc.
 * Collaborates with DataTransferManager, PLFrame.
 *
 * @since Thu Oct  3 16:11:10 2002
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */

public class PLClipboardOwnerSupport {
    private String name;
    private List types;
    private PLClipboardOwnerParent parent;

    public PLClipboardOwnerSupport (String name, List types, PLClipboardOwnerParent parent, PLClipboardOwner plClipboardOwner){
	this.name = name;
	this.parent = parent;
	this.types = types;
	DataTransferInfo.getInstance().registerTypes(name, types);
	registerPLClipboardOwner(parent, (JComponent)plClipboardOwner);
    }

    public void setParent(PLClipboardOwnerParent parent) {
	this.parent = parent;
    }

    public PLClipboardOwnerParent getParent() {
	return parent;
    }

    public void setName(String name) {
	this.name = name;
    }

    public String getName() {
	return name;
    }

    public void setTypes(List types) {
	this.types = types;
    }

    public List getTypes() {
	return types;
    }

    /**
     *  Register focus listener and communication between parent and plClipboardOwner.
     *  Assume (safely, I hope) that all PLClipboardOwners are JComponents.
     */
    public void registerPLClipboardOwner(final PLClipboardOwnerParent parent, final JComponent plClipboardOwner) {
	this.parent = parent;
	//debugPrintln(3, "registering component: " + getName() + ", parent = " + parent);
	FocusListener focusListener = new FocusListener() {
		public void focusGained(FocusEvent e) {
		    debugPrintln(3, "plClipboardOwner " + getName() + " is gaining focus, parent = " + parent);
		    parent.setLastFocusedPLClipboardOwner((PLClipboardOwner)plClipboardOwner);
		}

		public void focusLost(FocusEvent e) {
		    // do nothing... we don't want to wipe out the last focused component...
		    //debugPrintln(3, "plClipboardOwner " + getName() + " is losing focus");
		}
	    };
	plClipboardOwner.addFocusListener(focusListener);
    }
    
}// PLClipboardOwnerSupport
