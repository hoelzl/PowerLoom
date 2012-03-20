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


// Version: EditObjectAction.java,v 1.11 2010/02/04 05:17:17 hans Exp

package edu.isi.powerloom.gui.components;

import java.util.*;
import javax.swing.*;
import java.awt.event.*;
import edu.isi.powerloom.gui.xmlobject.*;
import static edu.isi.powerloom.gui.common.Utils.*;

/**
 * Action to edit objects.
 *
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 * @since Tue Apr 09 13:52:07 2002
 */

public class EditObjectAction extends PowerloomAction {
    public EditObjectAction() {
	super();
	putValue(Action.SMALL_ICON, PowerloomApp.getInstance().getImage("resources/images/edit.gif"));
	putValue(Action.NAME, "Edit Object");
	putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_E, InputEvent.CTRL_MASK, false));
    }
    public EditObjectAction(PowerloomApp app) {
	super();
	parentApp = app;
	updateActionName();
    }
    public void actionPerformed(ActionEvent e) {
	PLObject theObject = null;
	if (object == null) {
	    // obsolete:
	    theObject = PowerloomApp.getInstance().getMostRecentlyTouchedObject();
	    /* candidate code if the above is no good:
	    JInternalFrame frame = PowerloomApp.getInstance().getTopmostBrowserFrame();
	    PLClipboardOwner source = ((PLFrame)frame).getLastFocusedPLClipboardOwner();
	    theObject = source.getSelectedObject();
	    */
	} else {
	    theObject = object;
	}
	//debugPrintln(3, "editing object: " + theObject + ", class = " + theObject.getClass());
	if (theObject instanceof PLModule) {
	    editModule((PLModule)theObject);
	} else if (theObject instanceof PLConcept) {
	    editClass(PowerloomApp.getInstance().getMostRecentlyTouchedModule(), (PLConcept)theObject);

	} else if (theObject instanceof PLRelation) {
	    editRelation(PowerloomApp.getInstance().getMostRecentlyTouchedModule(), (PLRelation)theObject);
	} else if (theObject instanceof PLInstance) {
	    editInstance(PowerloomApp.getInstance().getMostRecentlyTouchedModule(), (PLInstance)theObject);
	} else if (theObject instanceof PLProposition) {
	    editProposition(PowerloomApp.getInstance().getMostRecentlyTouchedModule(), (PLProposition)theObject);
	}
}
    // todo: add module to parameters for everything, using mostrecnetlytouchedmodule
    public void editClass(PLModule module, PLConcept concept) {
		// open editor with class...
		debugPrintln(3, "editing " + concept);
		ConceptFrame2 conceptFrame = new ConceptFrame2();
		PowerloomApp.getInstance().getConceptFrames().add(conceptFrame);
		conceptFrame.setConcept(module, concept);
		conceptFrame.displayFrame();
    }
    // todo: add module to parameters for everything, using mostrecnetlytouchedmodule
    public void editInstance(PLModule module, PLInstance instance) {
		// open editor with class...
		debugPrintln(3, "editing " + instance);
		InstanceFrame frame = new InstanceFrame();
		PowerloomApp.getInstance().getInstanceFrames().add(frame);
		frame.setInstance(module, instance);
		frame.displayFrame();
    }
    // todo: add module to parameters for everything, using mostrecnetlytouchedmodule
    public void editModule(PLModule module) {
		// open editor with class...
		debugPrintln(3, "editing " + module);
		ModuleFrame frame = new ModuleFrame();
		frame.setModule(module);
		frame.displayFrame();
    }
    // todo: add module to parameters for everything, using mostrecnetlytouchedmodule
    public void editRelation(PLModule module, PLRelation relation) {
		// open editor with class...
		debugPrintln(3, "editing " + relation);
		RelationFrame frame = new RelationFrame();
		PowerloomApp.getInstance().getRelationFrames().add(frame);
		frame.setRelation(module, relation);
		frame.displayFrame();
    }
    public void editProposition(PLModule module, PLProposition proposition) {
		// open editor with class...
		debugPrintln(3, "editing " + proposition);
		PropositionFrame2 frame = new PropositionFrame2();
		PowerloomApp.getInstance().getPropositionFrames().add(frame);
		frame.setProposition(module, proposition);
		frame.displayFrame();
    }
    public void updateActionName() {
	PLObject theObject = null;
	if (object == null) {
	    theObject = PowerloomApp.getInstance().getMostRecentlyTouchedObject();
	} else {
	    theObject = object;
	}
	if (PowerloomApp.getInstance().getMostRecentlyTouchedObject() != null) {
	    putValue(Action.NAME, "Edit " + theObject);
	} else {
	    putValue(Action.NAME, "Edit Object");
	} 
    }
}
