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


// Version: PLTreeModel.java,v 1.4 2010/02/04 05:18:44 hans Exp

package edu.isi.powerloom.gui.components;

import edu.isi.powerloom.gui.xmlobject.*;
import static edu.isi.powerloom.gui.common.Utils.*;

import javax.swing.tree.*;
import javax.swing.event.*;
import java.util.Vector;

/**
 * Tree Model used for holding trees of PLObjects.
 *
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 * @since Sat Mar 02 14:02:00 2002
 */

public class PLTreeModel implements TreeModel {
    Vector listeners;
    PLTreeNode root;

    public PLTreeModel(PLTreeNode root) {
        /*
        if (root == null) {
            throw new IllegalArgumentException("root is null");
        }
        */
        this.root = root;
    }
    public void addTreeModelListener(TreeModelListener tml) {
        if (listeners == null) {
            listeners = new Vector();
        }
        listeners.addElement(tml);
    }
    protected void fireTreeNodesChanged(Object source, Object[] path, int[] ci, Object[] cc) {
        debugPrintln(3, "firing nodeschanged!");
        if (listeners != null) {
            TreeModelEvent tme = new TreeModelEvent(source, path, ci, cc);
            for (int i = 0; i < listeners.size(); i++) {
                ((TreeModelListener) listeners.elementAt(i)).treeNodesChanged(tme);
            } 
        }
    }
    // Implementation of TreeModel
    public Object getChild(Object node, int index) {
        if (node instanceof PLTreeNode) {
            return ((PLTreeNode)node).getChild(index);
        }
        return null;
    }
    public int getChildCount(Object parent) {
        if (parent instanceof PLTreeNode) {
            return ((PLTreeNode)parent).getChildCount();            
        }
        return 0;
    }
    public int getIndexOfChild(Object parent, Object child) {
        if (parent instanceof PLTreeNode) {
            if (child instanceof PLTreeNode) {
                return ((PLTreeNode)parent).getIndexOfChild((PLTreeNode)child);
            } else {
                throw new IllegalArgumentException("child is not of type PLTreeNode");
            }             
        } else {
            throw new IllegalArgumentException("parent is not of type PLTreeNode");
        } 
    }
    public Object getRoot() {
        return root;
    }
    public boolean isLeaf(Object node) {
        if (node instanceof PLTreeNode) {
            return ((PLTreeNode)node).isLeaf();
        } else {
            throw new IllegalArgumentException("Node must be of type PLTreeNode");
        } 
    }
    public void refresh(TreeExpansionEvent tee) {
        int[] ci = new int[] {-1};
        fireTreeNodesChanged(tee.getSource(), tee.getPath().getPath(), ci, null);
    }
    public void removeTreeModelListener(TreeModelListener tml) {
        if (listeners != null) {
            listeners.removeElement(tml);
        }
    }
    public void setRoot(PLTreeNode root) {
        this.root = root;
        Object[] path = new Object[] { root };
        TreePath tp = new TreePath(path);
        valueForPathChanged(tp, root);
    }
    public void valueForPathChanged(TreePath path, Object newValue) {
        Object[] p = path.getPath();
        Object[] pp = p;
        PLTreeNode node;
        int index;
        if (p.length == 1) { // editing root
            root = (PLTreeNode)newValue;
            node = (PLTreeNode)root;
            index = -1;
        } else {
            node = (PLTreeNode)p[p.length - 1];
            PLTreeNode parent = (PLTreeNode)p[p.length - 2];
            index = parent.getIndexOfChild(node);
            // replace the old object w/thenew object here

        } 
        int[] childIndices = new int[] { index };  // child indices in nparent
        Object [] childObjects = new Object[] { node }; // child objects in parent
        fireTreeNodesChanged(this, pp, childIndices, childObjects);
    }
}
