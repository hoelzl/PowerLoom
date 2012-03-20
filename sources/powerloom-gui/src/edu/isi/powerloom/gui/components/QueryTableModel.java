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


// Version: QueryTableModel.java,v 1.9 2010/02/04 05:19:12 hans Exp

package edu.isi.powerloom.gui.components;

import java.util.*;
import javax.swing.table.*;
import edu.isi.powerloom.gui.xmlobject.*;
import static edu.isi.powerloom.gui.common.Utils.*;


/**
 * Table model used for displaying query results.
 *
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 * @since Thu May 23 12:31:31 2002
 */

public class QueryTableModel extends PLTableModel {
    // Sort Modes:
    private static final int ASCENDING = 1;
    private static final int DESCENDING = 2;
    private int sortPolarity = DESCENDING;
    int lastSortedColumn = -1;
    PLQueryResult queryResult;
    String columnNames[];
    List sortedItems;

    public QueryTableModel() {
    }
    public QueryTableModel(PLQueryResult queryResult) {
	this.queryResult = queryResult;
	sortedItems = (List)queryResult.elemPLTuple;
	sortColumn(0);
    }
    public QueryTableModel(PLQueryResult queryResult, String[] columnNames) {
	if ((columnNames.length == 0) &&
	    (((List)queryResult.elemPLTuple).size() > 0)) {
	    // we have an ask result
	    columnNames = new String[]{ "TRUTH-VALUE" };
	}
	this.queryResult = queryResult;
	//copy the list, leaving the original intact.  Necessary for rowToModelIndex
	sortedItems = new ArrayList(queryResult.elemPLTuple);
	this.columnNames = columnNames;
	sortColumn(0);
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
    public Object getValueAt(int row, int column) {
	if ((column < 0) || (column > getColumnCount())) {
	    debugPrintln(3, "***warning columnCount of " + column + " is not in range.");
	    return null;
	}
	if ((queryResult == null) || (queryResult.elemPLTuple == null)) {
	    return null;
	}
	PLTuple tuple = (PLTuple)sortedItems.get(row);
	PLObjectUnion plObjectUnion = (PLObjectUnion)((List)tuple.elemPLObjectUnion).get(column);
	return plObjectUnion.getObject();
    }
    public void setQueryResult(PLQueryResult result) {
	this.queryResult = result;
    }

    /**
     *  Convert row index to model index, i.e., row before the user sorts the results.
     */
    public int rowToModelIndex(int row) {
	List unsortedTuples = (List)queryResult.elemPLTuple;
	PLTuple currentTuple = (PLTuple)sortedItems.get(row);
	Iterator unsortedIter = unsortedTuples.iterator();
	int count = 0;
	while (unsortedIter.hasNext()) {
	    PLTuple candidate = (PLTuple)unsortedIter.next();
	    if (candidate == currentTuple) {
		return count;
	    }
	    count++;
	}
	return -1; // not found
    }

    class TupleComparator implements Comparator {
	int compareCol = -1;

	public TupleComparator(int compareCol) {
	    this.compareCol = compareCol;
	}
	
	public int compare(Object x, Object y) {
	    PLTuple tuple1 = null;
	    PLTuple tuple2 = null;
	    if (QueryTableModel.this.sortPolarity == ASCENDING) {
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
	    if ((obj1 instanceof PLObject) && (obj2 instanceof PLObject)) {
		return (((PLObject)obj1).getID().compareTo(((PLObject)obj2).getID()));
	    } else if ((obj1 instanceof Integer) && (obj2 instanceof Integer)) {
		return (((Integer)obj1).compareTo((Integer)obj2));
	    } else if ((obj1 instanceof Float) && (obj2 instanceof Float)) {
		return (((Float)obj1).compareTo((Float)obj2));
	    } else if ((obj1 instanceof String) && (obj2 instanceof String)) {
		return (((String)obj1).compareTo((String)obj2));
	    } else {
		debugPrintln(3, "error in tuplecomparator: unknown type: " + obj1.getClass());
	    }
	    return 0;
	}
    }

}
