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


// Version: QueryHistoryPanel.java,v 1.9 2010/02/05 00:11:58 hans Exp

package edu.isi.powerloom.gui.components;

import edu.isi.powerloom.gui.serverinterface.*;
import edu.isi.powerloom.gui.xmlobject.*;
import edu.isi.powerloom.gui.common.*;
import static edu.isi.powerloom.gui.common.Utils.*;

import java.awt.FlowLayout;
import javax.swing.*;
import java.util.*;

/**
 * Subpanel of the Query Dialog which holds the query history.
 *
 * @since Fri Oct 18 16:37:06 2002
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 * @see edu.isi.powerloom.gui.components.QueryFrame QueryFrame
 * @see edu.isi.powerloom.gui.components.QueryHistoryAndOptionsPanel QueryHistoryAndOptionsPanel
 */
public class QueryHistoryPanel extends JPanel {
    private List savedQueries = new ArrayList();
    private JComboBox queryComboBox;
    private JButton saveQueryButton;
    private PLQueryListModel queryListModel;

    private class PLQueryListModel extends DefaultComboBoxModel {
	public Object getElementAt(int i) {
	    return savedQueries.get(i);
	}

	public int getSize() {
	    return savedQueries.size();
	}

	public void fireContentsChanged() {
	    fireContentsChanged(this, 0, savedQueries.size() - 1);
	}
    }

    public QueryHistoryPanel (){
	super();
	initialize();
    }
    
    public void initialize() {
	loadSavedQueries();
	initComponents();
    }

    private void initComponents() {
	setLayout(new FlowLayout());
	queryListModel = new PLQueryListModel();
	queryComboBox = new JComboBox(queryListModel);
	if (queryListModel.getSize() > 0) {
	    queryComboBox.setSelectedItem(queryListModel.getElementAt(0));
	}
	add(queryComboBox);
	saveQueryButton = new JButton("Save Current Query");
	//saveQueryButton.setEnabled(false);
	add(saveQueryButton);
	javax.swing.border.TitledBorder title = new javax.swing.border.TitledBorder("Saved Queries");
	setBorder(title);
    }

    public void loadSavedQueries() {
	try {
	    Preferences prefs = Preferences.getInstance();
	    String savedQueriesXML = prefs.getProperty(Preferences.SAVED_QUERIES);
	    if (savedQueriesXML == null) {
		return;
	    }
	    debugPrintln(3, "savedQueriesXML = " + savedQueriesXML);
	    PLQueryList queryList = (PLQueryList)SoapSender.unmarshalXMLObject(savedQueriesXML);
	    debugPrintln(3, "queryList = " + queryList);
	    debugPrintln(3, "queryList.elemPLQuery = " + queryList.elemPLQuery);
	    if (queryList.elemPLQuery != null) {
		Iterator iter = queryList.elemPLQuery.iterator();
		savedQueries = new ArrayList();
		savedQueries.add(getDefaultQuery());
		while (iter.hasNext()) {
		    PLQuery query = (PLQuery)iter.next();
		    debugPrintln(3, "  " + query);
		    savedQueries.add(query);
		}
	    }
	} catch (Exception e) {
	    PowerloomApp.getInstance().handleException(e);
	}
    }

    public void saveQuery(PLQuery query) {
	try {
	    debugPrintln(3, "saving query: " + query.toFullString());
	    String name = javax.swing.JOptionPane. showInputDialog(this, "Enter Name:", "Enter Query Name",
							       javax.swing.JOptionPane.QUESTION_MESSAGE);
	    if (name != null) {
		query.attrQueryName = name;
		savedQueries.add(query);
		Preferences prefs = Preferences.getInstance();
		PLQueryList queryList = new PLQueryList();
		List savedQueriesWithoutDefault = new ArrayList(savedQueries);
		savedQueriesWithoutDefault.remove(savedQueries.get(0));
		queryList.elemPLQuery = savedQueriesWithoutDefault;
		String savedQueriesXML = SoapSender.marshalXMLObject(queryList);
		prefs.setProperty(Preferences.SAVED_QUERIES, savedQueriesXML);
		prefs.save();
		PLQueryListModel model = (PLQueryListModel)getQueryComboBox().getModel();
		model.fireContentsChanged();
	    } else {
		return;
	    }
	} catch (Exception e) {
	    PowerloomApp.getInstance().handleException(e);
	}
    }


    public JButton getSaveQueryButton() {
	return saveQueryButton;
    }

    /**
     * Used for clearing all parameter fields
     */
    private PLQuery getDefaultQuery() {
	PLQuery result = new PLQuery();
	result.attrQueryName = "<Default Query>";
	result.attrQuery = "";
	result.attrModule = "";
	result.attrInferenceLevel = "NORMAL";
	result.attrTimeout = "";
	result.attrMoveout = "";
	result.attrNumResults = "ALL";
	result.attrMinScore = "";
	result.attrMaxUnknowns = "";
	result.attrMatchMode = "FULL";
	result.attrMaximizeScore = "FALSE";
	result.attrDontOptimize = "FALSE";
	return result;
    }

    public JComboBox getQueryComboBox() {
	return queryComboBox;
    }

    public PLQuery getSelectedQuery() {
	// for testing
	return (PLQuery)queryComboBox.getSelectedItem();
    }
}// QueryHistory
