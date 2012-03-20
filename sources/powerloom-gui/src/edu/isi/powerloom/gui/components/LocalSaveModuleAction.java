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


// Version: LocalSaveModuleAction.java,v 1.4 2010/02/04 05:17:48 hans Exp

package edu.isi.powerloom.gui.components;

import java.util.*;
import javax.swing.*;
import java.awt.event.*;
import edu.isi.powerloom.gui.xmlobject.*;
import static edu.isi.powerloom.gui.common.Utils.*;

/**
 * Action to save modules to a local file system.
 *
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 * @since Tue Apr 09 13:52:07 2002
 */
public class LocalSaveModuleAction extends PowerloomAction {
    public LocalSaveModuleAction() {
	super();
	putValue(Action.NAME, "Save Module");
    }
    public LocalSaveModuleAction(PowerloomApp app) {
	super();
	parentApp = app;
	updateActionName();
    }
    public void actionPerformed(ActionEvent e) {
	PLObject theObject = parentApp.getMostRecentlyTouchedObject();
	debugPrintln(3, "editing object: " + theObject + ", class = " + theObject.getClass());
	if (theObject instanceof PLModule) {
	    saveModule((PLModule)theObject);
	} else {
	    PowerloomApp.getInstance().handleException(new Exception("Error: " + theObject + " is not a module."));
	}
}
    public void saveModule(PLModule module) {
	PLLocalFileBrowser fileBrowser = new PLLocalFileBrowser(parentApp, module);
	fileBrowser.showSaveKBDialog();
    }

    public void updateActionName() {
	if (parentApp.getMostRecentlyTouchedObject() != null) {
	    putValue(Action.NAME, "Save " + parentApp.getMostRecentlyTouchedObject() + " Locally");
	} else {
	    putValue(Action.NAME, "Save Module Locally");
	} 
    }
}
