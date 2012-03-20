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


// Version: DeleteObjectAction.java,v 1.4 2010/02/04 05:17:12 hans Exp

package edu.isi.powerloom.gui.components;

import edu.isi.powerloom.gui.xmlobject.*;
import edu.isi.powerloom.gui.serverinterface.*;
import static edu.isi.powerloom.gui.common.Utils.*;
import java.awt.event.*;
import javax.swing.*;

/**
 * Action to delete an object.
 *
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */

public class DeleteObjectAction extends PowerloomAction {
    public DeleteObjectAction (){
	super();
	putValue(Action.NAME, "Delete Object");
    }

    public DeleteObjectAction(PLModule module, PLObject object) {
	this();
	setModule(module);
	setObject(object);
    }
    
    public void actionPerformed(ActionEvent e) {
	debugPrintln(3, "deleting object...");
	PLObject object = (PLObject)getObject();
	if (object == null) {
	    PLObject theObject = parentApp.getMostRecentlyTouchedObject();
	    if (theObject instanceof PLObject) {
		object = (PLObject)theObject;
	    }
	}
	if (object != null) {
	    doDelete(module, object);
	} else {
	    PowerloomApp.getInstance().handleException(new Exception("Error: " + object + " is null."));
	}
    }

    private void doDelete(PLModule module, PLObject object) {
	try {
	    String typeString = "object";
	    if (object instanceof PLModule) {
		typeString = "module";
	    } else if (object instanceof PLConcept) {
		typeString = "concept";
	    } else if (object instanceof PLRelation) {
		typeString = "relation";
	    } else if(object instanceof PLInstance) {
		typeString = "instance";
	    }

	    int confirmResult = JOptionPane.showConfirmDialog(PowerloomApp.getInstance(), "Are you sure you want to delete the " + typeString + " " + object.getID() + "?", "Delete Confirm", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
	    if (confirmResult == JOptionPane.YES_OPTION) {
		KnowledgeManager.getInstance().destroyObject(module, object.getID());
		if (object instanceof PLModule) {
		    KnowledgeManager.getInstance().invalidateAllCaches();
		} else if (object instanceof PLConcept) {
		    KnowledgeManager.getInstance().invalidateConceptCaches();
		} else if (object instanceof PLRelation) {
		    KnowledgeManager.getInstance().invalidateRelationCaches();
		} else if(object instanceof PLInstance) {
		    KnowledgeManager.getInstance().invalidateInstanceCaches();
		}
		PowerloomApp.getInstance().fireEditPerformed(new PLEditEvent(this, object.getClass()));
	    }
	} catch (Exception e) {
	    PowerloomApp.getInstance().handleException(e);
	}
    }
}// DeleteObjectAction
