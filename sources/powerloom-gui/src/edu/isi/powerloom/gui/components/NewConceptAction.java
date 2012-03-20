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


// Version: NewConceptAction.java,v 1.11 2010/02/04 05:18:04 hans Exp

package edu.isi.powerloom.gui.components;

import javax.swing.*;
import java.awt.event.*;
import java.util.*;
import edu.isi.powerloom.gui.xmlobject.*;
import edu.isi.powerloom.gui.serverinterface.*;
import static edu.isi.powerloom.gui.common.Utils.*;

/**
 * Action to create a new concept.
 *
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 * @since Tue Apr 09 13:52:07 2002
 */
public class NewConceptAction extends PowerloomAction {
    public NewConceptAction() {
	super();
	putValue(Action.NAME, "Create New Concept");

	putValue(Action.SMALL_ICON, PowerloomApp.getInstance().getImage("resources/images/add.gif"));
    }

    public NewConceptAction(PowerloomApp app) {
	super();
	parentApp = app;
    }
    public void actionPerformed(ActionEvent e) {
		createNewConcept();
    }
    public void createNewConcept() {
	// open editor...
	debugPrintln(3, "creating new concept");
	ConceptFrame2 frame = new ConceptFrame2();
	List surrogateList = new ArrayList();
	try {
	    BrowserFrame4 topmostFrame = PowerloomApp.getInstance().getTopmostBrowserFrame();
	    if (topmostFrame != null) {
		PLConcept[] selectedConcepts = topmostFrame.getPubBrowserPanel().getPowerloomTrees().getSelectedConcepts();
		if (selectedConcepts != null) {
		    for (int i = 0; i< selectedConcepts.length; i++) {
			debugPrintln(3, "selected concept[" + i + "] = " + selectedConcepts[i].getID());
			PLSurrogate superSurrogate = KnowledgeManager.getInstance().findOrCreateSurrogate(PLConcept.class, selectedConcepts[i].getID());
			surrogateList.add(superSurrogate);
		    }
		}
	    }
	    frame.setupNewConcept("NEW-CONCEPT", new PLSurrogateContainer(surrogateList));
	    frame.displayFrame();
	} catch (Exception e) {
	    PowerloomApp.getInstance().handleException(e);
	} // end of try-catch
	

    }
}
