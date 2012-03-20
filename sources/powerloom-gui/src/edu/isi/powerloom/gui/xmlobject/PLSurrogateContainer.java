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


// Version: PLSurrogateContainer.java,v 1.7 2010/02/04 05:21:20 hans Exp

package edu.isi.powerloom.gui.xmlobject;

import edu.isi.powerloom.gui.serverinterface.*;
import java.util.*;

/**
 * Hold a collection of surrogates.  Used for JTrees and JLists, also
 * used by KnowledgeManager to cache groups of objects.
 *
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 * @since Wed Mar 13 17:37:57 2002
 */

public class PLSurrogateContainer implements PLTreeNode, PLList, PLObject {
    private boolean undefined;
    private List surrogates;
    private String label;

    public PLSurrogateContainer() {
    }

    public PLSurrogateContainer(List surrogates) {
        this.surrogates = surrogatifyList(surrogates);;
	
    }

    // Convert all non-surrogates to surrogates pointing to anonymous PLObjects
    public static List surrogatifyList(List candidates) {
	ArrayList result = new ArrayList();
	Iterator iter = candidates.iterator();
	while (iter.hasNext()) {
	    Object candidate = iter.next(); 
	    if (!(candidate instanceof PLSurrogate)) {
		candidate = surrogatify(candidate);		    		
	    }
	    result.add(candidate);
	}
	return result;
    }

    public String getModule() {
	return null;
    }

    // Convert arbitrary object (usually string) to a surrogate pointing to an anonymous PLObject
    public static PLSurrogate surrogatify(Object object) {
	final String text = object.toString();
	PLSurrogate result = new PLSurrogate(text);
	PLObject newObject = new PLObject() {
		public String getID() {
		    return text;
		}
		public String getModule() {
		    return null;
		}
		public PLSurrogateContainer getChildSurrogates() {
		    return null;
		}
		public void setChildSurrogates(PLSurrogateContainer surrogates) {}

		public void setUndefined(boolean flag) {}
    
		public boolean isUndefined() {
		    return true;
		}
	    };
	result.setValue(newObject);
	return result;
    }

    public PLSurrogateContainer(String label, List surrogates) {
        this.label = label;
        this.surrogates = surrogates;
    }

    public void setSurrogates(List surrogates) {
        this.surrogates = surrogates;
    }

    public List getSurrogates() {
        return surrogates;
    }

    /*
     * PLTreeNode implementation
     */
    public void setChild(int index, PLTreeNode child) {
        if (child instanceof PLSurrogate) {
            PLSurrogate surrogate = (PLSurrogate)child;
            if (surrogates == null) {
                surrogates = new ArrayList();
            }
            surrogates.add(index, surrogate);
        } else {
            throw new IllegalArgumentException("child " + child + " must be of type Surrogate");
        } 
    }

    public PLTreeNode getChild(int index) {
        if ((surrogates == null) ||
            (index < 0) ||
            (index >= surrogates.size())) {
            throw new ArrayIndexOutOfBoundsException("index " + index + " is not in range.");
        }
        PLSurrogate surrogate = (PLSurrogate)surrogates.get(index);
        return (PLTreeNode)surrogate.getValue();
    }
    
    public int getIndexOfChild(PLTreeNode child) {
        if (surrogates == null) {
            return -1;
        }
        Iterator iter = surrogates.iterator();
        int count = 0;
        while (iter.hasNext()) {
            if (iter.next() == child) {
                return count;
            }
            count++;
        }
        return -1;
    }

    public int getChildCount() {
        if (surrogates == null) {
            return 0;
        }
        return surrogates.size();
    }

    public boolean isLeaf() {
        return ((surrogates == null) ||
                (surrogates.isEmpty()));
    }

    /*
     * PLList implementation
     */
    public Object getElementAt(int i) {
        return ((PLSurrogate)(surrogates).get(i)).getValue();
    }

    public int getSize() {
        if (surrogates == null) {
            return 0;
        }
        return surrogates.size();
    }

    public void addPLObject(PLObject object) {
	try {
	    Class theClass = object.getClass();
	    String id = object.getID();
	    PLSurrogate surrogate = KnowledgeManager.getInstance().findOrCreateSurrogate(theClass, id);
	    surrogates.add(surrogate);
	    // ensure that the possibly newly-interned surrogate has a value.  This shouldn't be
	    // necesssary in the real (non-prototype) system...
	    surrogate.setValue(object);
	} catch (Exception e) {
	    e.printStackTrace();
	    edu.isi.powerloom.gui.components.PowerloomApp.getInstance().handleException(e);
	} // end of try-catch
	
    }

    public void setLabel(String label) {
        this.label = label;
    }

    public String toString() {
        if (label != null) {
            return label;
        }
        return "[Surrogate Container: " + surrogates + "]";
    }

    // PLObject implementation
    /**
     * surrogate support
     */
    public String getID() {
        return label;
    }

    public PLSurrogateContainer getChildSurrogates() {
        return this;
    }

    public void setChildSurrogates(PLSurrogateContainer container) {
    }

    /**
     * Miscellaneous methods
     */
    // Return a surrogate container which flattens out a tree, so it can be used
    // in JLists and JComboBoxes.
    public PLSurrogateContainer listifyTreeContainer() {
	SortedSet sortedSurrogateSet = new TreeSet();
	Iterator iter = getSurrogates().iterator();
	while (iter.hasNext()) {
	    PLSurrogate root = (PLSurrogate)iter.next();
	    walkSurrogateTree(root, sortedSurrogateSet);
	}
	// collect into list
	iter = sortedSurrogateSet.iterator();
	List result = new ArrayList();
	while (iter.hasNext()) {
	    result.add(iter.next());
	}
	return new PLSurrogateContainer(result);
    }
    
    private void walkSurrogateTree(PLSurrogate node, SortedSet allSurrogates) {
	allSurrogates.add(node);
	PLObject value = (PLObject)node.getValue();
	PLSurrogateContainer children = value.getChildSurrogates();
	Iterator iter = children.getSurrogates().iterator();
	while (iter.hasNext()) {
	    PLSurrogate child = (PLSurrogate)iter.next();
	    walkSurrogateTree(child, allSurrogates);
	}
    }

/**
 * Insert the method's description here.
 * Creation date: (4/12/2002 1:27:09 PM)
 * @return int
 */
public int removePLObject(PLObject object) {
	int index = -1;
	try {
		PLModule module = null;  // this should be parameter
		PLSurrogate surrogate = KnowledgeManager.getInstance().findOrCreateSurrogate(object.getClass(), object.getID());
		index = surrogates.indexOf(surrogate);
		if (index >= 0) {
			surrogates.remove(surrogate);
		}
	} catch (Exception e) {
		e.printStackTrace();
		edu.isi.powerloom.gui.components.PowerloomApp.getInstance().handleException(e);
	}
	return index;
}

    public boolean isUndefined() {
	return undefined;
    }

    public void setUndefined(boolean flag) {
	undefined = flag;
    }

    /**
     * Return a new container which contains items that have a given prefix
     */
    public PLSurrogateContainer getFilteredSurrogateContainer(String prefix) {
	PLSurrogateContainer result = new PLSurrogateContainer();
	result.setLabel(label);
	List filteredSurrogates = new ArrayList();
	Iterator iter = surrogates.iterator();
	String downcasedPrefix = prefix.toLowerCase();
	while (iter.hasNext()) {
	    PLSurrogate surrogate = (PLSurrogate)iter.next();
	    if (surrogate.getID().toLowerCase().startsWith(downcasedPrefix)) {
		filteredSurrogates.add(surrogate);
	    }
	}
	result.setSurrogates(filteredSurrogates);
	return result;
    }

    public PLSurrogateContainer mergeSurrogateContainer(PLSurrogateContainer otherContainer) {
	surrogates.addAll(otherContainer.getSurrogates());
	return this;
    }
    
}
