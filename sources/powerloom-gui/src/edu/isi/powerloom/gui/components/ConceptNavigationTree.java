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


// Version: ConceptNavigationTree.java,v 1.8 2010/02/04 05:16:53 hans Exp

package edu.isi.powerloom.gui.components;

import edu.isi.powerloom.gui.xmlobject.*;
import edu.isi.powerloom.gui.serverinterface.*;
import static edu.isi.powerloom.gui.common.Utils.*;
import java.util.*;

/**
 * JTree with special functionality for handling cut and paste for concepts.
 *
 * @since Fri Oct 4 12:03:54 2002
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */

public class ConceptNavigationTree extends PLJTree {
    public ConceptNavigationTree (PLTreeModel model, PLFrame parent, String name, List types) {
	super(model, parent, name, types);
    }

    private String createNewConceptDefinition(String oldSource, PLConcept[] newSupers, boolean doingCopy) {
	try {
	    debugPrintln(3, "creatingNewConceptDef: source = " + oldSource);
	    String supersString = "";
	    String oldSupersString = "";
	    // trim off defconcept and name
	    String lowerSource = oldSource.toLowerCase();
	    int bodyIndex = lowerSource.indexOf("defconcept") + "defconcept".length();
	    String bodyString = oldSource.substring(bodyIndex).trim();
	    int whiteSpaceIndex = bodyString.indexOf(' '); // after name
	    if (whiteSpaceIndex == -1) {
		whiteSpaceIndex = bodyString.length() - 1;
	    }
	    String name = bodyString.substring(0, whiteSpaceIndex).trim();
	    bodyString = bodyString.substring(whiteSpaceIndex).trim();
	    if (bodyString.startsWith("(")) {
		//scan for matching paren, assume well-formed
		int parenCount = 1;
		int afterParenIndex = -1;
		// start after the first paren
		for (int i = 1; i < bodyString.length(); i++) {  
		    char ch = bodyString.charAt(i);
		    if (ch == ')') {
			parenCount--;
		    } 
		    if (ch == '(') {
			parenCount++;
		    }
		    if (parenCount == 0) {
			afterParenIndex = i;
			break;
		    }
		} 
		debugPrintln(3, "afterParenIndex = " + afterParenIndex);
		debugPrintln(3, "parenCount = " + parenCount);
		oldSupersString = bodyString.substring(1, afterParenIndex - 1);
		bodyString = bodyString.substring(afterParenIndex);
	    }
	    for (int i = 0; i < newSupers.length; i++) {
		supersString += " " + newSupers[i];
	    }
	    if (doingCopy) {
		supersString = oldSupersString + supersString + ")";
	    } else {
		supersString = "(?s " + supersString + ")";
	    }
	    supersString += ")";

	    debugPrintln(3, "bodystring = " + bodyString);
	    debugPrintln(3, "oldSupersString= " + oldSupersString);
	    debugPrintln(3, "superString= " + supersString);

	    String definition = "(DEFCONCEPT " + name + " (" + supersString + ") " + bodyString;
	    debugPrintln(3, "definition = " + definition);
	    return definition;
	} catch (Exception e) {
	    PowerloomApp.getInstance().handleException(e);
	}
	return null;
    }


    void commitConcept(PLModule module, PLConcept oldConcept, String conceptDefinition) {
	PowerloomTrees trees = ((BrowserFrame4)support.getParent()).getPubBrowserPanel().getPowerloomTrees();
	try {
	    KnowledgeManager.getInstance().evaluateLogicCommand(module, conceptDefinition);	    
	    // todo: Update *all* frames interested in updates
	    trees.performEdit(new PLEditEvent(this, PLConcept.class));
	    trees.makeObjectVisible(module, oldConcept, true);
	} catch (Exception e) {
	    PowerloomApp.getInstance().handleException(e);
	} // end of try-catch
    }

    public void doPasteFromCutOrCopy(PLModule module, PLObject object, boolean doingCopy) {
	if (object instanceof PLConcept) {
	    PowerloomTrees trees = ((BrowserFrame4)support.getParent()).getPubBrowserPanel().getPowerloomTrees();
	    String sourceString = ((PLConcept)object).attrSourceString;
	    PLConcept[] concepts =trees.getSelectedConcepts();
	    if (PowerloomApp.getInstance().getDoingRightClickMenu()) {
		PLConcept rightClickedConcept = (PLConcept)PowerloomApp.getInstance().getMostRecentlyTouchedObject();
		concepts = new PLConcept[1];
		concepts[0] = rightClickedConcept;
	    }
	    String newDefinition = createNewConceptDefinition(sourceString, concepts, doingCopy);
	    debugPrintln(3, "new definition is: " + newDefinition);
	    commitConcept(module, (PLConcept)object, newDefinition);
	} else {
	    System.err.println("Error: You can't paste a non-concept to a ConceptNavigation Tree!");
	}
    }

    public void doPasteFromCut(PLModule module, Object object) {
	debugPrintln(3, "conceptnavtree: pastefromcut");
	doPasteFromCutOrCopy(module, (PLObject)object, false);
    }

    public void doPasteFromCopy(PLModule module, Object object) {
	debugPrintln(3, "conceptnavtree: pastefromcopy");
	doPasteFromCutOrCopy(module, (PLObject)object, true);
    }

    public void doDelete(PLModule module, Object object) {
	(new DeleteObjectAction(module, (PLObject)object)).actionPerformed(null);
    }

}// ConceptNavigationTree
