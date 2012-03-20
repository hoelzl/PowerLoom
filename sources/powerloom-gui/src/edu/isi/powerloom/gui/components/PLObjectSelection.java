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


// Version: PLObjectSelection.java,v 1.8 2010/02/04 05:18:39 hans Exp

package edu.isi.powerloom.gui.components;

import edu.isi.powerloom.gui.serverinterface.*;
import edu.isi.powerloom.gui.xmlobject.*;
import static edu.isi.powerloom.gui.common.Utils.*;

import java.awt.*;
import java.awt.datatransfer.*;
import java.io.*;

/**
 * Transferable which holds a PLObject and associated information.
 */
public class PLObjectSelection implements Transferable,
    ClipboardOwner {
    static public DataFlavor plObjectFlavor, dataTransferRecordFlavor, stringFlavor;

    private DataFlavor[] flavors = {plObjectFlavor, dataTransferRecordFlavor, stringFlavor}; 
    private PLObject object;
    private DataTransferRecord dataTransferRecord;

    static {
	try {
	    dataTransferRecordFlavor = new DataFlavor(Class.forName("edu.isi.powerloom.gui.components.DataTransferRecord"), "DataTransfer Record");
	    plObjectFlavor = new DataFlavor(Class.forName("edu.isi.powerloom.gui.xmlobject.PLObject"), "Powerloom Object");
	    stringFlavor = new DataFlavor(Class.forName("java.lang.String"), "String");
	}
	catch(ClassNotFoundException e) {
	    PowerloomApp.getInstance().handleException(e);
	}
	catch(Exception e) {
            PowerloomApp.getInstance().handleException(e);
	}
    }
    
    public PLObjectSelection(PLModule module, PLObject object, PLClipboardOwner transferSource, int operation) {
	this.object = object;
	dataTransferRecord = new DataTransferRecord();
	dataTransferRecord.module = module;
	dataTransferRecord.transferSource = transferSource;
	dataTransferRecord.operation = operation;
	debugPrintln(3, "plobjectselection: set datatransferrecord = " + dataTransferRecord);
	// used for transfering concepts within a navigation pane.  Maybe this code should be put somewhere else? 
	if (object instanceof PLConcept) {
	    try {
		String source = KnowledgeManager.getInstance().getSourceForConcept(module, (PLConcept)object);
		((PLConcept)object).attrSourceString = source;
	    } catch (Exception e) {
		PowerloomApp.getInstance().handleException(e);
	    }
	}
    }
    public synchronized DataFlavor[] getTransferDataFlavors() {
	return flavors;
    }
    public boolean isDataFlavorSupported(DataFlavor flavor) {
	return (flavor.equals(plObjectFlavor) ||
		flavor.equals(stringFlavor) ||
		flavor.equals(dataTransferRecordFlavor));
    }

    public synchronized Object getTransferData(DataFlavor flavor) 
	throws UnsupportedFlavorException, IOException {
	debugPrintln(3, "plobjectselection: getting transferdata...");
	if (flavor.equals(plObjectFlavor)) {
	    return object;
	} else if (flavor.equals(dataTransferRecordFlavor)) {
	    debugPrintln(3, "plobjectselection: getting transferdata record:" + dataTransferRecord);
	    return dataTransferRecord;
	} else if (flavor.equals(stringFlavor)) {
	    return object.getID();
	} else {
	    throw new UnsupportedFlavorException(flavor);
	}
    }
    public void lostOwnership(Clipboard c, Transferable t) {
	System.err.println("LostOwnership on PLObjectSelection is not implemented, why is this called?");
    }
}
