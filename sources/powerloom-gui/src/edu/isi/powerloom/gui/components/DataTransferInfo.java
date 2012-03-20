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


// Version: DataTransferInfo.java,v 1.11 2010/02/04 05:17:06 hans Exp

package edu.isi.powerloom.gui.components;

import java.util.*;
import static edu.isi.powerloom.gui.common.Utils.*;

/**
 * Singleton class which maintains static and dynamic information
 * relevant to cut/paste and drag/drop operations.
 *
 * @since Wed Oct  2 18:28:54 2002
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 * @see edu.isi.powerloom.gui.components.DataTransferManager
 * @see edu.isi.powerloom.gui.components.PLClipboardOwnerTable
 * @see edu.isi.powerloom.gui.components.CutPasteRecord
 */
	
public class DataTransferInfo {
    public static int CUT = 1;
    public static int COPY = 2;

    private static DataTransferInfo theInstance;
    private HashMap typeTable = new HashMap();
    private PLClipboardOwnerTable cutPasteTable;
    private PLClipboardOwnerTable dragDropTable;
    private Set legalDeleteSet = new HashSet();
    private Set allTypes = new HashSet();

    private DataTransferInfo (){
	initialize();
    }

    private void initialize() {
	initializeCutPasteTable();
	initializeDragDropTable();
	initializeLegalDeleteSet();
    }

    private void initializeLegalDeleteSet() {
	legalDeleteSet.add("Module Includes List");
	legalDeleteSet.add("Module Uses List");
	legalDeleteSet.add("Shadow List");
	legalDeleteSet.add("Concept Editor Supers List");
	legalDeleteSet.add("Concept Editor Relations List");
	legalDeleteSet.add("Concept Editor Propositions List");
	legalDeleteSet.add("Concept Editor Rules List");
	// for now, don't allow editing of relation editor supers
	//legalDeleteSet.add("Relation Editor Supers List");
	legalDeleteSet.add("Relation Editor Propositions List");
	legalDeleteSet.add("Relation Editor Rules List");
	legalDeleteSet.add("Instance Editor Supers List");
	legalDeleteSet.add("Instance Editor Propositions List");
	legalDeleteSet.add("Module Navigation Pane");
	legalDeleteSet.add("Concept Navigation Pane");
	legalDeleteSet.add("Relation Navigation Pane");
	legalDeleteSet.add("Instance Navigation Pane");
	legalDeleteSet.add("Proposition Navigation Pane");
	legalDeleteSet.add("Rule Navigation Pane");
	legalDeleteSet.add("Text Holder");
    }

    private void initializeCutPasteTable() {
	cutPasteTable = new PLClipboardOwnerTable();
	cutPasteTable.add(new CutPasteRecord("Concept Navigation Pane", 
					     "Concept Navigation Pane", CUT));
	cutPasteTable.add(new CutPasteRecord("Concept Editor Supers List",
					     "Concept Editor Supers List", CUT));
	cutPasteTable.add(new CutPasteRecord("Instance Editor Supers List",
					     "Instance Editor Supers List", CUT));
	cutPasteTable.add(new CutPasteRecord("Concept Holder", 
					     "Concept Holder", COPY));
	cutPasteTable.add(new CutPasteRecord("Concept Holder", 
					     "Concept Holder", COPY));
	/*
	cutPasteTable.add(new CutPasteRecord("Relation Holder", 
					     "Relation Editor Supers List", COPY));
	*/
	cutPasteTable.add(new CutPasteRecord("Module Includes List", 
					     "Module Includes List", CUT));
	cutPasteTable.add(new CutPasteRecord("Module Uses List", 
					     "Module Uses List", CUT));
	cutPasteTable.add(new CutPasteRecord("Module Includes List", 
					     "Module Uses List", CUT));
	cutPasteTable.add(new CutPasteRecord("Module Uses List", 
					     "Module Includes List", CUT));
	cutPasteTable.add(new CutPasteRecord("Module Holder", 
					     "Module Includes List", COPY));
	cutPasteTable.add(new CutPasteRecord("Module Holder", 
					     "Module Uses List", COPY));
	cutPasteTable.add(new CutPasteRecord("Shadow List", 
					     "Shadow List", CUT));
	cutPasteTable.add(new CutPasteRecord("Object Holder", 
					     "Text Holder", COPY));
	cutPasteTable.add(new CutPasteRecord("Text Holder", 
					     "Text Holder", CUT));
	cutPasteTable.add(new CutPasteRecord("Text Holder", 
					     "Text Holder", COPY));
    }
    private void initializeDragDropTable() {
	dragDropTable = new PLClipboardOwnerTable();
	// etc
    }

    public static DataTransferInfo getInstance() {
	if (theInstance == null) {
	    theInstance = new DataTransferInfo();
	}
	return theInstance;
    }

    /**
     * @return true if name is in legal delete set, or name's type is
     * in the legal delete set.
     */
    public boolean isLegalDelete(String name) {
	if (legalDeleteSet.contains(name)) {
	    return true;
	}
	Iterator iter = legalDeleteSet.iterator();
	while (iter.hasNext()) {
	    if (nameMatchesType(name, (String)iter.next())) {
		return true;
	    }
	}
	return false;
    }

    public PLClipboardOwnerTable getCutPasteTable() {
	return cutPasteTable;
    }

    public PLClipboardOwnerTable getDragDropTable() {
	return dragDropTable;
    }

    public boolean nameIsType(String name) {
	debugPrintln(3, "name is type, name = " + name + ", alltypes = " + allTypes);
	return allTypes.contains(name);
    }

    public void registerTypes(String name, List types) {
	typeTable.put(name, types);
	allTypes.addAll(types);
    }

    public List lookupTypes(String name) {
	return (List)typeTable.get(name);
    }

    public boolean typesAreCompatible(String name1, String name2) {
	List types1 = lookupTypes(name1);
	List types2 = lookupTypes(name2);
	types1.retainAll(types2);
	return (!types1.isEmpty());
    }

    public boolean nameMatchesType(String name, String typeName) {
	if (!nameIsType(typeName)) {
	    return false;
	}
	List typesForName = lookupTypes(name);
	debugPrintln(3, "nameMatchesType, name = " + name + ", typeName = " + typeName + " returning: " + typesForName.contains(typeName));
	return (typesForName.contains(typeName));
    }
	    
    // for testing
    public static void main(String args[]) {
	// Simulate creating concept navigation pane, concept editor supers list,
	// etc, which will register their types with DataTransferInfo
	DataTransferInfo transferInfo = DataTransferInfo.getInstance();
	String[] types = {"Concept Holder"};
	transferInfo.registerTypes("Concept Navigation Pane", Arrays.asList(types));
	String[] types2 = {"Concept Holder"};
	transferInfo.registerTypes("Concept Supers List", Arrays.asList(types2));
	String[] types3 = {"Module Holder"};
	transferInfo.registerTypes("Module Includes List", Arrays.asList(types3));
	CutPasteRecord searchRecord1 = new CutPasteRecord("Concept Navigation Pane", "Concept Navigation Pane", CUT);
	// expect this to return: Concept Nav, Concept Nav
	PLClipboardRecord result1 = transferInfo.getCutPasteTable().lookupFirst(searchRecord1);
	debugPrintln(3, "result1 = " + result1);
	CutPasteRecord searchRecord1a = new CutPasteRecord("Concept Navigation Pane", "Concept Navigation Pane", COPY);
	// expect this to return: null
	PLClipboardRecord result1a = transferInfo.getCutPasteTable().lookupFirst(searchRecord1a);
	debugPrintln(3, "result1a = " + result1a);
	CutPasteRecord searchRecord2 = new CutPasteRecord("Concept Navigation Pane", "Concept Supers List", COPY);
	PLClipboardRecord[] results2 = transferInfo.getCutPasteTable().lookupAll(searchRecord2);
	// expect this to return 1 record: Concept Nav, Concept Holder
	debugPrintln(3, "results2 are: ");
	for (int i = 0; i < results2.length; i++) {
	    debugPrintln(3, "  " + results2[i]);
	}

	CutPasteRecord searchRecord3 = new CutPasteRecord("Concept Supers List", "Concept Navigation Pane", COPY);
	PLClipboardRecord[] results3 = transferInfo.getCutPasteTable().lookupAll(searchRecord3);
	// expect this to return 0 records
	debugPrintln(3, "results3 are: ");
	for (int i = 0; i < results3.length; i++) {
	    debugPrintln(3, "  " + results3[i]);
	}
    }


}// DataTransferInfo
