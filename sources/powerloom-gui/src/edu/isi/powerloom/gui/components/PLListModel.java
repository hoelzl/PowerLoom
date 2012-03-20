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


// Version: PLListModel.java,v 1.7 2010/02/04 05:18:35 hans Exp

package edu.isi.powerloom.gui.components;

import edu.isi.powerloom.gui.xmlobject.*;
import static edu.isi.powerloom.gui.common.Utils.*;

import javax.swing.*;
import javax.swing.event.*;
import java.util.*;

/**
 * List model for holding PLObjects.
 *
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 * @since Sun Mar 03 22:41:12 2002
 */
public class PLListModel extends AbstractListModel implements ComboBoxModel {
    private PLList theObject;
    private Object selectedItem;
    private PLModule module;

    public void setModule(PLModule module) {
	this.module = module;
	refreshList();
    }

    public PLModule getModule() {
	return module;
    }

    public PLListModel(PLModule module, PLList theObject) {
	this.module = module;
        this.theObject = theObject;
    }

    public PLListModel(PLList theObject) {
	this.theObject = theObject;
    } 

    public void addPLObject(PLObject object) {
	try {
	    getPLList().addPLObject(object);
	    fireIntervalAdded(this, getSize() - 1, getSize() - 1);
	} catch (Exception e) {
	    PowerloomApp.getInstance().handleException(e);
	} // end of try-catch
	

    }
    public Object getElementAt(int i) {
	if ((i >= 0) && (i < theObject.getSize())) {
	    return theObject.getElementAt(i);
	}
	return null;
    }
    public PLList getPLList() {
	return theObject;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/11/2002 6:06:23 PM)
     * @return java.lang.Object
     */
    public Object getSelectedItem() {
	return selectedItem;
    }
    public int getSize() {
        return theObject.getSize();
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/12/2002 1:24:46 PM)
     * @param object edu.isi.powerloom.gui.xmlobject.PLObject
     */
    public void removePLObject(PLObject object) {
	try {
	    int index = getPLList().removePLObject(object);
	    debugPrintln(3, "firing interval removed: " + index);
	    fireIntervalRemoved(this, index, index);
	} catch (Exception e) {
	    PowerloomApp.getInstance().handleException(e);
	} // end of try-catch
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/11/2002 6:04:41 PM)
     * @param object java.lang.Object
     */
    public void setSelectedItem(Object object) {
	selectedItem = object;
	Object[] listeners = listenerList.getListenerList();
	List dataListeners = new ArrayList();
	for (int i = 0; i < listeners.length; i++) {
	    if (listeners[i] instanceof ListDataListener) {
		dataListeners.add(listeners[i]);
	    }
	}
	Iterator iter = dataListeners.iterator();
	while (iter.hasNext()) {
	    ListDataListener listener = (ListDataListener)iter.next();
	    ListDataEvent lde = new ListDataEvent(this, ListDataEvent.CONTENTS_CHANGED, 0, getSize()-1);
	    listener.contentsChanged(lde);
	}
    }
    // force the list to re-render
    public void refreshList() {
	fireContentsChanged(this, 0, getSize() - 1);
    }
}
