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


// Version: EditInstanceCellPanel.java,v 1.5 2010/02/04 05:17:16 hans Exp

package edu.isi.powerloom.gui.components;

import javax.swing.*;
import java.awt.event.*;
import javax.swing.text.*;
import javax.swing.event.*;
import java.util.*;
import static edu.isi.powerloom.gui.common.Utils.*;

/**
 * Cell Editor used for editing instance names.
 *
 * @since Wed Sep 11 11:47:44 2002
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */
	
public class EditInstanceCellPanel extends ExpressionEditorPanel {
    public EditInstanceCellPanel() {
    }

    public EditInstanceCellPanel(PLFrame frame, String name, List types) {
	super(frame, name, types);
	/*
	System.err.println("Creating default editinstancepanel.  hashcode = " + hashCode());	
	try {
	    throw new Exception("Dummy Exception");
	} catch (Exception e) {
	    e.printStackTrace();
	}
	*/
	initialize();
    }

    public EditInstanceCellPanel(javax.swing.text.StyledDocument doc) {
	super(doc);
	/*
	System.err.println("Creating doc-based editinstancepanel.  hashcode = " + hashCode());	
	try {
	    throw new Exception("Dummy Exception");
	} catch (Exception e) {
	    e.printStackTrace();
	}
	*/
	initialize();
    }

    public void initialize() {
	super.initialize();
	addFocusListener(new FocusListener() {
		public void focusGained(FocusEvent e) {
		    //debugPrintln(3, "focusGained in cell editor: " + EditInstanceCellPanel.this.hashCode());
		    PowerloomApp.getInstance().setSelectedInstanceCellEditor(EditInstanceCellPanel.this);
		}
		public void focusLost(FocusEvent e) {
		    //debugPrintln(3, "focusLost in cell editor: " + EditInstanceCellPanel.this.hashCode());
		    PowerloomApp.getInstance().setSelectedInstanceCellEditor(null);
		}
	    });

	EditInstanceCellKeyMap.setupKeyMap(this);
    }

    

}
