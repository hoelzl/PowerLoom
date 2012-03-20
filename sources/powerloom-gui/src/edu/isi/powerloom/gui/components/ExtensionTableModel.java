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


// Version: ExtensionTableModel.java,v 1.12 2010/02/04 05:17:25 hans Exp

package edu.isi.powerloom.gui.components;

import java.util.*;
import javax.swing.table.*;
import javax.swing.event.*;

import edu.isi.powerloom.gui.xmlobject.*;
import edu.isi.powerloom.gui.serverinterface.*;
import static edu.isi.powerloom.gui.common.Utils.*;


/**
 * Table model for extension editor.
 *
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 * @since Thu May 23 12:31:31 2002
 */

public class ExtensionTableModel extends AbstractTableModel {
    private static final int ASCENDING = 1;
    private static final int DESCENDING = 2;
    private int sortPolarity = DESCENDING;
    int lastSortedColumn = -1;

    private PLQueryResult queryResult;
    private String columnNames[];
    private List sortedItems;
    private List oldTuples = new ArrayList();
    private List newTuples = new ArrayList();
    private int currentlyCompletingRow = 3;
    private int currentlyCompletingColumn = 0;
    private Object completionItem;
    private String completionFragment;
    private String relationName;
    private int savedRowHeight;

    public void resetOldAndNewTuples() {
	oldTuples = new ArrayList();
	newTuples = new ArrayList();
    }

    public ExtensionTableModel() {
    }
    public ExtensionTableModel(PLQueryResult queryResult) {
	this.queryResult = queryResult;
	sortedItems = (List)queryResult.elemPLTuple;
	sortColumn(0);
    }
    public ExtensionTableModel(PLQueryResult queryResult, String relationName, String[] columnNames) {
	this.relationName = relationName;
	if ((columnNames.length == 0) &&
	    (((List)queryResult.elemPLTuple).size() > 0)) {
	    // we have an ask result
	    columnNames = new String[]{ "TRUTH-VALUE" };
	}
	this.queryResult = queryResult;
	sortedItems = (List)queryResult.elemPLTuple;
	this.columnNames = columnNames;
	sortColumn(0);
	addBlankTuple();
    }

    public String getRelationName() {
	return relationName;
    }

    public PLTuple[] getOldTuples() {
	return (PLTuple[])oldTuples.toArray(new PLTuple[0]);
    }
    public PLTuple[] getNewTuples() {
	return (PLTuple[])newTuples.toArray(new PLTuple[0]);
    }
    public void sortColumn(int columnIndex) {
	if (columnIndex != lastSortedColumn) {
	    sortPolarity = ASCENDING;
	    lastSortedColumn = columnIndex;
	}
	Collections.sort(sortedItems, new TupleComparator(columnIndex));	    
	// toggle sort polarity
	if (sortPolarity == ASCENDING) {
	    sortPolarity = DESCENDING;
	} else {
	    sortPolarity = ASCENDING;
	}
    }

    public int getColumnCount() {
	if ((queryResult == null) || (queryResult.elemPLTuple == null)
	    || (queryResult.elemPLTuple.isEmpty())) {
	    return 0;
	}
	PLTuple firstTuple = (PLTuple)((List)queryResult.elemPLTuple).get(0);
	return firstTuple.elemPLObjectUnion.size();
    }
    public String getColumnName(int i) {
	if ((columnNames != null) && (i >= 0) && (i < columnNames.length)) {
	    return columnNames[i];
	}
	return "UNKNOWN" + i;
    }
    public int getRowCount() {
	if ((queryResult == null) || (queryResult.elemPLTuple == null)) {
	    return 0;
	}
	return queryResult.elemPLTuple.size();
    }

    public boolean isCellEditable(int row, int column) {
	return true;
    }

    public Object getValueAt(int row, int column) {
	if ((column < 0) || (column > getColumnCount())) {
	    System.err.println("***warning columnCount of " + column + " is not in range.");
	    return null;
	}
	if ((queryResult == null) || (queryResult.elemPLTuple == null)) {
	    return null;
	}
	PLTuple tuple = (PLTuple)sortedItems.get(row);
	PLObjectUnion plObjectUnion = (PLObjectUnion)((List)tuple.elemPLObjectUnion).get(column);
	return plObjectUnion.getObject();
    }

    // Remove row from table, add as candidate old tuple
    public void deleteRow(int row) {
	PLTuple deletedTuple = (PLTuple)sortedItems.get(row);
	oldTuples.add(deletedTuple);
	// remove the tuple
	sortedItems.remove(row);
	fireTableChanged(new TableModelEvent(this));
    }

    public void setValueAt(Object value, int row, int column) {
	debugPrintln(3, "setting value at row = " + row + ", col = " + column);
	try {
	    PLTuple tuple = (PLTuple)sortedItems.get(row);
	    PLObjectUnion union = (PLObjectUnion)((List)tuple.elemPLObjectUnion).get(column);
	    oldTuples.add(tuple.clone());
	    union.setObject(value);
	    newTuples.add(tuple);
	} catch (Exception e) {
	    PowerloomApp.getInstance().handleException(e);
	}

    }
    public void setQueryResult(PLQueryResult result) {
	this.queryResult = result;
    }

    public void addBlankTuple() {
	int arity = columnNames.length;
	PLTuple blankTuple = new PLTuple();
	blankTuple.elemPLObjectUnion = new ArrayList();
	for (int i = 0; i < arity; i++) {
	    PLObjectUnion union = new PLObjectUnion();
	    blankTuple.elemPLObjectUnion.add(union);
	}
	sortedItems.add(blankTuple);
    }

    private boolean lastTupleIsBlank() {
	if (sortedItems.size() > 0) {
	    PLTuple lastTuple = (PLTuple)sortedItems.get(sortedItems.size() - 1);
	    Iterator unionIter = lastTuple.elemPLObjectUnion.iterator();
	    while (unionIter.hasNext()) {
		if (((PLObjectUnion)unionIter.next()).getObject() != null) {
		    return false;
		}
	    }
	}
	return true;
    }

    public void maybeAddBlankTuple() {
	if (!lastTupleIsBlank()) {
	    addBlankTuple();
	}
    }

    class TupleComparator implements Comparator {
	int compareCol = -1;

	public TupleComparator(int compareCol) {
	    this.compareCol = compareCol;
	}
	
	public int compare(Object x, Object y) {
	    PLTuple tuple1 = null;
	    PLTuple tuple2 = null;
	    if (ExtensionTableModel.this.sortPolarity == ASCENDING) {
		tuple1 = (PLTuple)x;
		tuple2 = (PLTuple)y;
	    } else {
		tuple1 = (PLTuple)y;
		tuple2 = (PLTuple)x;
	    }
	    List unionList1 = (List)tuple1.elemPLObjectUnion;
	    List unionList2 = (List)tuple2.elemPLObjectUnion;
	    Object obj1 = ((PLObjectUnion)unionList1.get(compareCol)).getObject();
	    Object obj2 = ((PLObjectUnion)unionList2.get(compareCol)).getObject();
	    if (obj1 == null) {
		if (obj2 == null) {
		    return 0;
		} else {
		    return -1;
		}
	    }
	    if (obj2 == null) {
		if (obj1 == null) {
		    return 0;
		} else {
		    return 1;
		}
	    }
	    if (obj1 instanceof PLObject) {
		return (((PLObject)obj1).getID().compareTo(((PLObject)obj2).getID()));
	    } else if (obj1 instanceof Integer) {
		return (((Integer)obj1).compareTo((Integer)obj2));
	    } else if (obj1 instanceof Float) {
		return (((Float)obj1).compareTo((Float)obj2));
	    } else if (obj1 instanceof String) {
		return (((String)obj1).compareTo((String)obj2));
	    } else {
		System.err.println("error in tuplecomparator: unknown type: " + obj1.getClass());
	    }
	    return 0;
	}
    }

    public void setCurrentlyCompletingCell(int row, int column) {
	currentlyCompletingRow = row;
	currentlyCompletingColumn = column;
    }

    public boolean isCellBeingCompleted(int row, int column) {
	if ((row == currentlyCompletingRow) && (column == currentlyCompletingColumn)) {
	    return true;
	}
	return false;
    }

    public void setCompletionFragment(String text) {
	completionFragment = text;
    }

    public String getCompletionFragment() {
	return completionFragment;
    }

    public void setCompletionItem(Object item) {
	completionItem = item;
    }

    public Object getCompletionItem() {
	return completionItem;
    }

    public int getCompletingRow() {
	return currentlyCompletingRow;
    }

    public int getCompletingColumn() {
	return currentlyCompletingColumn;
    }

    public int getSavedRowHeight() {
	return savedRowHeight;
    }

    public void setSavedRowHeight(int rowHeight) {
	savedRowHeight = rowHeight;
    }
}
