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


// Version: DataTransferManager.java,v 1.9 2010/02/04 05:17:07 hans Exp

package edu.isi.powerloom.gui.components;

import edu.isi.powerloom.gui.xmlobject.*;
import static edu.isi.powerloom.gui.common.Utils.*;

import javax.swing.*;
import java.awt.datatransfer.*;

/**
 * Singleton class which is responsible for managing application-level cut/paste, drag/drop.
 *
 * @since Thu Oct  3 17:32:14 2002
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 * @see edu.isi.powerloom.gui.components.DataTransferInfo
 */

public class DataTransferManager {
    private static DataTransferManager theInstance = null;

    public static DataTransferManager getInstance() {
	if (theInstance == null) {
	    theInstance = new DataTransferManager();
	}
	return theInstance;
    }

    private DataTransferManager() {
    }

    private Transferable makeTransferable(PLModule module, PLObject object, PLClipboardOwner source, int operation) {
	return new PLObjectSelection(module, object, source, operation);
    }

    private Object getSelectedObject(PLClipboardOwner plClipboardOwner) {
	PLObject mostRecentlyTouchedObject = PowerloomApp.getInstance().getMostRecentlyTouchedObject();
	debugPrintln(3, "getsel: most recentlytouchedobj = " + mostRecentlyTouchedObject);
	Object selectedObject = plClipboardOwner.getSelectedObject();
	if (PowerloomApp.getInstance().getDoingRightClickMenu()) {
	    debugPrintln(3, "getsel: doing right click");
	    selectedObject = mostRecentlyTouchedObject;
	} else {
	    debugPrintln(3, "getsel: not doing right click");
	}
	return selectedObject;
    }

    private PLModule getSelectedModule(PLClipboardOwner plClipboardOwner) {
	PLModule mostRecentlyTouchedModule = PowerloomApp.getInstance().getMostRecentlyTouchedModule();
	PLModule selectedModule = plClipboardOwner.getSelectedModule();
	if (PowerloomApp.getInstance().getDoingRightClickMenu()) {
	    selectedModule = mostRecentlyTouchedModule;
	}
	return selectedModule;
    }

    /**
     * Assume the data returned from getSelectedObject() is of type PLObject.
     */
    private void putDataOnClipboard(PLClipboardOwner plClipboardOwner, int operation) {
	PLObject selection = (PLObject)getSelectedObject(plClipboardOwner);
	PLModule module = getSelectedModule(plClipboardOwner);
	Transferable transferable = makeTransferable(module, selection, plClipboardOwner, operation);
	debugPrintln(3, "putting " + selection + " on clipboard, transferable = " + transferable);

	Clipboard clipboard = PowerloomApp.getInstance().getToolkit().getSystemClipboard();
	clipboard.setContents(transferable, plClipboardOwner);

	// get appropriate message depending on cut/copy semantics ()
	if (operation == DataTransferInfo.CUT) {
	PowerloomApp.getInstance().getStatusMsg2().setText(selection.getID() + " will be moved if you do a paste command.");
	} else {
	    PowerloomApp.getInstance().getStatusMsg2().setText(selection.getID() + " will be copied if you do a paste command.");
	}

    }

    private DataTransferRecord getTransferRecordFromTransferable(Transferable transferable) {
	System.err.println("getTransferRecordFromTransferable: transferable = " + transferable);
	System.err.println("getTransferRecordFromTransferable: dataFlavorsSupported = " + transferable.getTransferDataFlavors());
	DataTransferRecord result = null;
	try {
	    if (transferable.isDataFlavorSupported(PLObjectSelection.dataTransferRecordFlavor)) {
		result = (DataTransferRecord)transferable.getTransferData(PLObjectSelection.dataTransferRecordFlavor);
	    } else {
		System.err.println("Warning: getTransferRecordFromTransferable: dataTransferRecordFlavor not supported.");
	    }
	} catch (Exception e) {
	    PowerloomApp.getInstance().handleException(e);
	}
	return result;
    }

    private Object getObjectFromTransferable(Transferable transferable) {
	Object object = null;
	try {
	    if (transferable.isDataFlavorSupported(PLObjectSelection.plObjectFlavor)) {
		object = transferable.getTransferData(PLObjectSelection.plObjectFlavor);
	    } else if (transferable.isDataFlavorSupported(PLObjectSelection.stringFlavor)) {
		object = transferable.getTransferData(PLObjectSelection.stringFlavor);
	    } else if (transferable.isDataFlavorSupported(DataFlavor.stringFlavor)) {
		object = transferable.getTransferData(DataFlavor.stringFlavor);
	    }
	} catch (Exception e) {
	    PowerloomApp.getInstance().handleException(e);
	}
	return object;
    }

    /**
     * Lookup cut/paste rules in tables maintained by DataTransferInfo to determine legality of an operation.
     */
    private boolean isLegalPaste(PLClipboardOwner source, PLClipboardOwner target, Object object, int operation) {
	CutPasteRecord searchRecord = new CutPasteRecord(source.getName(), target.getName(), operation);
	debugPrintln(3, "isLegalPaste: searchRecord = " + searchRecord);
	PLClipboardRecord record = DataTransferInfo.getInstance().getCutPasteTable().lookupFirst(searchRecord);
	debugPrintln(3, "isLegalPaste: retrieved: " + record);
	if (record != null) {  
	    // we've found a match, so the paste must be legal.
	    return true;
	}
	return false;
    }

    /**
     *  Lookup cut/copy legality from DataTransferInfo
     */
    private boolean isLegalCut(PLClipboardOwner source) {
	CutPasteRecord searchRecord = new CutPasteRecord(source.getName(), null, DataTransferInfo.CUT);
	debugPrintln(3, "isLegalCut: searchRecord = " + searchRecord);
	PLClipboardRecord record = DataTransferInfo.getInstance().getCutPasteTable().lookupFirst(searchRecord);
	debugPrintln(3, "isLegalCut: retrieved: " + record);
	return (record != null);
    }

    private boolean isLegalCopy(PLClipboardOwner source) {
	CutPasteRecord searchRecord = new CutPasteRecord(source.getName(), null, DataTransferInfo.COPY);
	debugPrintln(3, "isLegalCopy: searchRecord = " + searchRecord);
	PLClipboardRecord record = DataTransferInfo.getInstance().getCutPasteTable().lookupFirst(searchRecord);
	debugPrintln(3, "isLegalCopy: retrieved: " + record);
	return (record != null);
    }

    public void doCut() {
	JInternalFrame frame = PowerloomApp.getInstance().getTopmostFrame();
	if (frame == null) {
	    System.err.println("There is no topmost frame.");
	    return;
	}
	if (frame instanceof PLFrame) {
	    PLClipboardOwner source = ((PLFrame)frame).getLastFocusedPLClipboardOwner();
	    if (source != null) {
		boolean isLegalCut = isLegalCut(source);
		if (isLegalCut) {
		    Object selection = getSelectedObject(source);
		    PLModule module = null;
		    if (source.supportsPLObjects()) {
			putDataOnClipboard(source, DataTransferInfo.CUT);
			module = getSelectedModule(source);
		    }
		    source.doCut(selection);
		} else {
		    JOptionPane.showMessageDialog(PowerloomApp.getInstance(), 
						  "Sorry, you may not cut an object from a " + source.getName() + ".",
						  "Illegal Cut", JOptionPane.WARNING_MESSAGE);
		    return;
		}
	    } else {
		// todo: display warning?
		System.err.println("You can't cut: There is no selected subpane of the topmost window.");
	    }
	} else {
	    System.err.println("Error: We can't get data from a frame of type: " + frame.getClass());
	}
    }

    /**
     *  Like cut, but we don't put anything on the clipboard (and callees of doDelete shouldn't, either)
     */ 
    public void doDelete() {
	debugPrintln(3, " in dtm.dodelete");
	JInternalFrame frame = PowerloomApp.getInstance().getTopmostFrame();
	if (frame == null) {
	    System.err.println("There is no topmost frame.");
	    return;
	}
	if (frame instanceof PLFrame) {
	    PLClipboardOwner source = ((PLFrame)frame).getLastFocusedPLClipboardOwner();
	    if (source != null) {
		boolean isLegalDelete = DataTransferInfo.getInstance().isLegalDelete(source.getName());
		if (isLegalDelete) {
		    Object selection = getSelectedObject(source);
		    PLModule module = null;
		    if (source.supportsPLObjects()) {
			module = getSelectedModule(source);
		    }
		    source.doDelete(module, selection);
		} else {
		    JOptionPane.showMessageDialog(PowerloomApp.getInstance(), 
						  "Sorry, you may not delete an object from a " + source.getName() + ".",
						  "Illegal Delete", JOptionPane.WARNING_MESSAGE);
		    return;
		}
	    }
	} else {
	    // todo: display warning?
	    System.err.println("You can't delete: There is no selected subpane of the topmost window.");
	}
    }

    public void doCopy() {
	JInternalFrame frame = PowerloomApp.getInstance().getTopmostFrame();
	if (frame == null) {
	    System.err.println("There is no topmost frame.");
	    return;
	}
	if (frame instanceof PLFrame) {
	    PLClipboardOwner source = ((PLFrame)frame).getLastFocusedPLClipboardOwner();
	    if (source != null) {
		boolean isLegalCopy = isLegalCopy(source);
		if (isLegalCopy) {
		    Object selection = getSelectedObject(source);
		    PLModule module = null;
		    if (source.supportsPLObjects()) {
			putDataOnClipboard(source, DataTransferInfo.COPY);
			module = getSelectedModule(source);
		    }
		    source.doCopy(selection);
		} else {
		    JOptionPane.showMessageDialog(PowerloomApp.getInstance(), 
						  "Sorry, you may not copy an object from a " + source.getName() + ".",
						  "Illegal Copy", JOptionPane.WARNING_MESSAGE);
		    return;
		}
	    } else {
		System.err.println("You can't copy: There is no selected subpane of the topmost window.");
	    }
	} else {
	    System.err.println("Error: We can't get data from a frame of type: " + frame.getClass());
	}
    }

    public void doPaste() {
	Clipboard clipboard = PowerloomApp.getInstance().getToolkit().getSystemClipboard();
	Transferable transferable = clipboard.getContents(this);
	// Determine if data is available in the correct flavor 
	if (transferable != null) {
	    JInternalFrame frame = PowerloomApp.getInstance().getTopmostFrame();
	    if (frame == null) {
		debugPrintln(3, "There is no topmost frame.");
		return;
	    }
	    if (frame instanceof PLFrame) {
		if (transferable != null) {
		    PLClipboardOwner target = ((PLFrame)frame).getLastFocusedPLClipboardOwner();
		    Object object = getObjectFromTransferable(transferable);
		    int operation = -1;
		    PLModule module = null;
		    PLClipboardOwner source = null;
		    boolean isLegalPaste = true;
		    if (target.supportsPLObjects()) {
			DataTransferRecord transferRecord = getTransferRecordFromTransferable(transferable);
			if ((object == null) || (transferRecord == null)) {
			    // we can't do a paste here, e.g., we're trying to paste from text
			    // to a PLObject holder
			    System.err.println("Can't do paste: object = " + object + ", transferRecord = " + transferRecord);
			    return; 
			}
 			module = transferRecord.module;
			source = transferRecord.transferSource;
			operation = transferRecord.operation;
			// Check legality of paste using rules in DataTransferInfo tables
			isLegalPaste = isLegalPaste(source, target, object, operation);
		    }
		    if (!isLegalPaste) {
			JOptionPane.showMessageDialog(PowerloomApp.getInstance(), 
						      "Sorry, you may not paste an object that was " + 
						      ((operation == DataTransferInfo.CUT) ? "cut" : "copied") +
						      " from a " + source.getName() + " to a " + target.getName() + ".",
						      "Illegal Paste", JOptionPane.WARNING_MESSAGE);
			return;
		    }
		    if ((object != null)) {
			// todo: get operation from transferable (get DataTransferRecord)
			if (operation == DataTransferInfo.CUT) {
			    target.doPasteFromCut(module, object);
			    PowerloomApp.getInstance().getStatusMsg2().setText(((PLObject)object).getID() + " was successfuly moved.");
			} else if (operation == DataTransferInfo.COPY) {
			    target.doPasteFromCopy(module, object);
			    PowerloomApp.getInstance().getStatusMsg2().setText(((PLObject)object).getID() + " was successfuly pasted.");
			} else {
			    target.doPasteFromCopy(module, object);
			    if (object instanceof PLObject) {
				PowerloomApp.getInstance().getStatusMsg2().setText(object + " was successfuly pasted.");
			    }
			}
		    } else {
			System.err.println("Warning: You are pasting from a transferable that doesn't have a PLObject or a PLModule");
		    }
		}
	    } else {
		debugPrintln(3, "Error: We can't get data from a frame of type: " + frame.getClass());
	    }
	}
    }
}
