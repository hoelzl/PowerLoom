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


// Version: PowerloomTrees.java,v 1.45 2010/02/04 05:18:58 hans Exp

package edu.isi.powerloom.gui.components;

import java.awt.*;
import javax.swing.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.event.*;
import javax.swing.tree.*;
import edu.isi.powerloom.gui.common.*;
import static edu.isi.powerloom.gui.common.Utils.*;
import edu.isi.powerloom.gui.parser.*;
import edu.isi.powerloom.gui.xmlobject.*;
import edu.isi.powerloom.gui.serverinterface.*;

/**
 * PowerloomTrees is responsible for setting up views in the BrowserFrame
 * and managing selection events and updating.  There are several classes of
 * methods, including: <p>
 * <ul>
 * <li> <code>getSelectedXXX</code>, where XXX is the object type (concept, instance, etc).
 * These methods retrieve the currently selected items of the specified object type.  There
 * are two variations of this method.  One is parameterless, which just examines the current
 * selection state of the navigation pane.  The other takes a selection event as an argument.
 * This is so that the item that is being selected during a selection event can be retrieved-
 * it seems to be the case that examining the current selection state doesn't return the desired
 * results during event processing.
 * <li><code>installXXXPopup</code>, where XXX is the object type.  This installs the context-sensitive
 * (i.e., right-clickable) popups on each navigation pane.
 * <li><code>makeXXX{Tree | List }</code>.  These methods are called at startup time to create the initial
 * navigation panes. 
 * <li><code>processXXX{Tree | List }SelectionEvent.  These methods are called whenever a 
 * selection event occurs for a particular object.  It is the responsibility of these methods
 * to update dependent views.   E.g., when a module selection event occurs, concept,
 * relation, and instance navigation panes should be updated.
 * <li><code>refreshXXX{Tree | List}</code> refreshes a navigation pane.  This should be called
 *  after the underlying model for a navigation view has changed.  For example, after a concept has
 * been edited, the concept tree view should be refreshed.  These methods should invalidate and 
 * refresh applicable caches.  For example, refreshing the concept view should invalidate the
 * module->concept cache and all caches that are dependent on concepts that were affected
 * by an edit.  For example, if an edit changed the definitions of concepts A, B, and C,
 * instance, relation, proposition, and rule caches should be refreshed for all 3 concepts.
 * <li><code>updateXXXX{Tree | List}Model</code> updates the currently-shown model of
 * a given object type based on a selection event or the combined state of all current
 * selections in the BrowserFrame.
 * </ul>
 *
 * @since 3/3/2002 12:51:13 PM
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */
public class PowerloomTrees implements PLEditListener {
    public static final int SHOW_INHERITED_RELATIONS = 1;
    public static final int SHOW_DIRECT_RELATIONS = 2;
    public static final int SHOW_INHERITED_INSTANCES = 3;
    public static final int SHOW_DIRECT_INSTANCES = 4;
    private int relationFilter = SHOW_DIRECT_RELATIONS;
    private int instanceFilter = SHOW_INHERITED_INSTANCES;
    private BrowserPanel4 parent;
    private javax.swing.JTree moduleTree;
    private PLTreeModel moduleTreeModel;
    private java.util.List selectionHistory = new ArrayList();
    private int historyCursor = -1;
    private boolean navigating = false;
    private PLModule mostRecentlyTouchedModule;
    private PLObject mostRecentlyTouchedObject;

/**
 * Insert the method's description here.
 * Creation date: (3/3/2002 12:53:12 PM)
 * @param param edu.isi.powerloom.gui.components.PowerloomGUI
 * @exception edu.isi.powerloom.gui.common.AppException The exception description.
 */
public PowerloomTrees(BrowserPanel4 parent) throws AppException {
	this.parent = parent;
	initializeTrees();
}
    Object getFirstSelectedListItem(JList list, ListSelectionEvent lse) {
        return list.getSelectedValue();
    }


    /**
     * selection -getting methods
     */
    TreePath getFirstSelectedTreePath(JTree tree, TreeSelectionEvent tse, 
                                      boolean skipFirstRow) {
        boolean[] selected = new boolean[tree.getRowCount()];
        TreePath[] eventPaths = tse.getPaths();
        TreePath[] treePaths = tree.getSelectionPaths();
        // merge the two
        if (treePaths != null) {
            for (int i = 0 ; i < treePaths.length; i ++) {
                int row = tree.getRowForPath(treePaths[i]);
                if ((row >= 0) && (row < selected.length)) {
                    selected[row] = true;
                }
            } 
        }
        if (eventPaths != null) {
            for (int i = 0 ; i < eventPaths.length; i ++) {
                int row = tree.getRowForPath(eventPaths[i]);
                if ((row >= 0) && (row < selected.length)) {
                    if (tse.isAddedPath(eventPaths[i])) {
                        selected[row] = true;
                    } else {
                        selected[row] = false;
                    }
                } 
            } 
        }
        // return the first selected path
        for (int i = 0; i < selected.length; i ++) {
            if (selected[i]) {
                if ((i > 0) || !skipFirstRow) {
                    return tree.getPathForRow(i);
                }
            }
        } 
        // Kludge... getRowForPath doesn't always return a non-negative
        // number, if this is method called as a side-effect of a setSelectionPath
        // call (e.g., refreshConceptTree -> refreshSelectionPath).  Here, we
        // just pick the first selectionPath in the tree selection event if it
        // exists, otherwise we pick the first tree path
        // This could be dangerous!
        if (eventPaths != null) {
            for (int i = 0 ; i < eventPaths.length; i ++) {
                int row = tree.getRowForPath(eventPaths[i]);
                if ((row >= 0) && (row < selected.length)) {
                    if (tse.isAddedPath(eventPaths[i])) {
                        return eventPaths[i];
                    } else {
                        selected[row] = false;
                    }
                }
            }
        }
        if (treePaths != null) {
            return treePaths[0];
        }
        return null;
    }

    public int getRelationFilter() {
	return relationFilter;
    }
    public int getInstanceFilter() {
	return instanceFilter;
    }
    public PLConcept getSelectedConcept() {
        if (parent.getConceptTree() == null) {
            return null;
        }
        TreePath conceptPath = parent.getConceptTree().getLeadSelectionPath();
	// this is buggy! conceptPath returns a non-null value even if the
	// path has been *deselected*!!
	    //System.out.println("conceptPath = " + conceptPath);
        if (conceptPath == null) {
            return null;
        }
	PLConcept concept = null;
	Object object = conceptPath.getLastPathComponent();
	if (object instanceof PLConcept) {
	    concept = (PLConcept)object;
	}
        return concept;
    }
    PLConcept getSelectedConcept(TreeSelectionEvent tse) {
        if (tse == null) {
            return getSelectedConcept();
        }
        if (parent.getConceptTree() == null) {
            return null;
        }
        boolean skipFirstRow = true;
        TreePath conceptPath = getFirstSelectedTreePath(parent.getConceptTree(), 
                                                        tse, skipFirstRow);
        if (conceptPath == null) {
            return null;
        }
	Object object = conceptPath.getLastPathComponent();
        PLConcept concept = null;
	if (object instanceof PLConcept) {
	    concept = (PLConcept)object;
	}
        return concept;
    }
    public PLConcept[] getSelectedConcepts() {
        if (parent.getConceptTree() == null) {
            return null;
        }
        TreePath[] conceptPaths = parent.getConceptTree().getSelectionPaths();
        if (conceptPaths == null) {
	        return null;
        }
	ArrayList result = new ArrayList();
        for (int i = 0; i < conceptPaths.length; i++) {
	    if (parent.getConceptTree().isPathSelected(conceptPaths[i])) {
		result.add(conceptPaths[i].getLastPathComponent());
	    }
        }
	//System.out.println("getselectedconcepts result: " + result);
	return (PLConcept[])result.toArray(new PLConcept[0]);
    }
    public PLInstance getSelectedInstance() {
        if (parent.getInstanceList() == null) {
            return null;
        }
        PLInstance instance = (PLInstance)parent.getInstanceList().getSelectedValue();
        return instance;
    }
    PLInstance getSelectedInstance(ListSelectionEvent lse) {
        if (lse == null) {
            return getSelectedInstance();
        }
        if (parent.getInstanceList() == null) {
            return null;
        }
        PLInstance instance = (PLInstance)getFirstSelectedListItem(parent.getInstanceList(), lse);
        return instance;
    }
    public PLModule getPropositionViewModule() {
	return (PLModule)parent.getPropositionPanel().getModuleViewSelectorPanel().getModuleComboBox().getSelectedItem();
    }

    public PLModule getRuleViewModule() {
	return (PLModule)parent.getRulePanel().getModuleViewSelectorPanel().getModuleComboBox().getSelectedItem();
    }

    public PLModule getSelectedModule() {
        if (parent.getModuleTree() == null) {
            return null;
        }
        TreePath modulePath = parent.getModuleTree().getLeadSelectionPath();
        if ((modulePath == null) || (!parent.getModuleTree().isPathSelected(modulePath))) {
            return null;
        }
        PLModule module = (PLModule)modulePath.getLastPathComponent();
        return module;
    }
    PLModule getSelectedModule(TreeSelectionEvent tse) {
        if (tse == null) {
            return getSelectedModule();
        }
        if (parent.getModuleTree() == null) {
            return null;
        }
        boolean skipFirstRow = false;
        TreePath modulePath = getFirstSelectedTreePath(parent.getModuleTree(), 
                                                       tse, skipFirstRow);
        if (modulePath == null) {
            return null;
        }
        PLModule module = (PLModule)modulePath.getLastPathComponent();
        return module;
        
    }
    public PLProposition getSelectedProposition() {
        if (parent.getPropositionList() == null) {
            return null;
        }
        PLProposition proposition = (PLProposition)parent.getPropositionList().getSelectedValue();
        return proposition;
    }
    PLProposition getSelectedProposition(ListSelectionEvent lse) {
        if (lse == null) {
            return getSelectedProposition();
        }
        if (parent.getPropositionList() == null) {
            return null;
        }
        PLProposition proposition = (PLProposition)getFirstSelectedListItem(parent.getPropositionList(), lse);
        return proposition;
    }
    public PLRelation getSelectedRelation() {
        if (parent.getRelationTree() == null) {
            return null;
        }
        TreePath relationPath = parent.getRelationTree().getLeadSelectionPath();
        if ((relationPath == null) || (!parent.getRelationTree().isPathSelected(relationPath))) {
            return null;
        }
	//System.out.println("getSelectedrelation: relationPath = " + relationPath + ", isSelected = " + parent.getRelationTree().isPathSelected(relationPath));
        PLRelation relation = (PLRelation)relationPath.getLastPathComponent();
        return relation;
    }
    PLRelation getSelectedRelation(TreeSelectionEvent tse) {
        if (tse == null) {
            return getSelectedRelation();
        }
        if (parent.getRelationTree() == null) {
            return null;
        }
        boolean skipFirstRow = true;
        TreePath relationPath = getFirstSelectedTreePath(parent.getRelationTree(), tse,
                                                         skipFirstRow);
        if (relationPath == null) {
            return null;
        }
	Object object = relationPath.getLastPathComponent();
        PLRelation relation = null;
	if (object instanceof PLRelation) {
	    relation = (PLRelation)object;
	}
        return relation;
    }
    public PLProposition getSelectedRule() {
        if (parent.getRuleList() == null) {
            return null;
        }
        PLProposition rule = (PLProposition)parent.getRuleList().getSelectedValue();
        return rule;
    }
    PLProposition getSelectedRule(ListSelectionEvent lse) {
        if (lse == null) {
            return getSelectedRule();
        }
        if (parent.getRuleList() == null) {
            return null;
        }
        PLProposition rule = (PLProposition)getFirstSelectedListItem(parent.getRuleList(), lse);
        return rule;
    }
/**
 * Insert the method's description here.
 * Creation date: (3/3/2002 12:59:18 PM)
 */
private void initializeTrees() throws AppException {
	// Setup the module tree first
	try {
	    JTree newTree = makeModuleTree();
	    parent.setModuleTree(newTree);
	    parent.setModuleTree(newTree);
	    setupModuleViewListeners();
	    // record the empty selection to kick of the history
	    recordCurrentSelection();
	} catch (Exception e) {
            throw new AppException(e);
	}
}

    private void setupModuleViewListeners() {
	parent.getPropositionPanel().getModuleViewSelectorPanel().getModuleComboBox().addItemListener(new ItemListener() {
		public void itemStateChanged(ItemEvent e) {
		    debugPrintln(3, "detected prop module view state change: " + getPropositionViewModule());
		    try {
			updatePropositionListModel(null, null, null, null);
		    } catch (Exception e2) {
			PowerloomApp.getInstance().handleException(e2);
		    }
		}
	    });
	parent.getRulePanel().getModuleViewSelectorPanel().getModuleComboBox().addItemListener(new ItemListener() {
		public void itemStateChanged(ItemEvent e) {
		    debugPrintln(3, "detected rule module view state change: " + getRuleViewModule());
		    try {
			updateRuleListModel(null, null, null);
		    } catch (Exception e2) {
			PowerloomApp.getInstance().handleException(e2);
		    }
		}
	    });
    }


    private void installConceptPopup(final JTree tree) {
	Collection treeMenuItems = new ArrayList();
	Collection itemMenuItems = new ArrayList();
	Collection treeOnlyMenuItems = new ArrayList();
	//treeMenuItems.add(new JMenuItem("Filter Concepts..."));
	itemMenuItems.add(EditObjectAction.class);
	itemMenuItems.add(AddObjectAction.class);
	itemMenuItems.add(InstantiateAction.class);	
	itemMenuItems.add(EditExtensionAction.class);
	itemMenuItems.add(CutAction.class);
	itemMenuItems.add(CopyAction.class);
	itemMenuItems.add(PasteAction.class);
	itemMenuItems.add(DeleteAction.class);
	treeOnlyMenuItems.add(NewConceptAction.class);
	PopupUtils.installTreePopup(tree, treeMenuItems, itemMenuItems, treeOnlyMenuItems, "CONCEPT-ROOT", null);
    }

    private void installInstancePopup(final JList list) {
	Collection listMenuItems = new ArrayList();
	Collection itemMenuItems = new ArrayList();
	//listMenuItems.add(new JMenuItem("Filter Instances..."));
	//System.out.println("itemMenuItems = " + itemMenuItems);
	//System.out.println("parent = " + parent);
	//System.out.println("parentframe = " + parent.getParentFrame());
	itemMenuItems.add(EditObjectAction.class);
	itemMenuItems.add(CopyAction.class);
	itemMenuItems.add(DeleteAction.class);
	listMenuItems.add(NewInstanceAction.class);

	PopupUtils.ContextSensitiveMenuItemGenerator csmig = new PopupUtils.ContextSensitiveMenuItemGenerator() {
		public java.util.List getContextSensitiveMenuItems() {
		    java.util.List result = new ArrayList();
		    // we're selecting a concept, present filtering options
		    debugPrintln(3, "in instance csmig");
		    if (getSelectedConcepts() != null) {
			debugPrintln(3, "before init, getRelationFilter = " + getRelationFilter() + ", selectedConcept = " + getSelectedConcepts());
			if (getInstanceFilter() == PowerloomTrees.SHOW_INHERITED_INSTANCES) {
			    result.add(new FilterInstanceAction(PowerloomTrees.this, PowerloomTrees.SHOW_INHERITED_INSTANCES));
			} else {
			    result.add(new FilterInstanceAction(PowerloomTrees.this, PowerloomTrees.SHOW_DIRECT_INSTANCES));
			} 
		    } 
		    return result;
		}
	    };


	PopupUtils.installListPopup(getSelectedModule(), list, listMenuItems, itemMenuItems, null, null, csmig);
    }

    private void installModulePopup(final JTree moduleTree) {
	Collection treeMenuItems = new ArrayList();
	Collection itemMenuItems = new ArrayList();
	Collection treeOnlyMenuItems = new ArrayList();
	//treeMenuItems.add(new JMenuItem("Filter Modules..."));
	//System.out.println("itemMenuItems = " + itemMenuItems);
	//System.out.println("parent = " + parent);
	//System.out.println("parentframe = " + parent.getParentFrame());
	itemMenuItems.add(EditObjectAction.class);
	itemMenuItems.add(AddObjectAction.class);
	itemMenuItems.add(ClearModuleAction.class);
	itemMenuItems.add(SaveModuleAction.class);
	itemMenuItems.add(LocalSaveModuleAction.class);
	itemMenuItems.add(CopyAction.class);
	treeMenuItems.add(LoadModuleAction.class);
	treeMenuItems.add(LocalLoadModuleAction.class);
	PopupUtils.installTreePopup(moduleTree, treeMenuItems, itemMenuItems, treeOnlyMenuItems, "", null);
    }
    private void installPropositionPopup(final JList list) {
	Collection listMenuItems = new ArrayList();
	Collection itemMenuItems = new ArrayList();
	Collection subObjectMenuItems = new ArrayList();
	itemMenuItems.add(EditObjectAction.class);
	itemMenuItems.add(CopyAction.class);
	itemMenuItems.add(DeleteAction.class);
	subObjectMenuItems.add(NavigateAction.class);
	subObjectMenuItems.add(EditObjectAction.class);
	listMenuItems.add(NewPropositionAction.class);
	PopupUtils.installListPopup(getSelectedModule(), list, listMenuItems, itemMenuItems, null, subObjectMenuItems, null);
    }
    private void installRelationPopup(final JTree tree) {
	Collection treeMenuItems = new ArrayList();
	Collection treeOnlyMenuItems = new ArrayList();
	Collection itemMenuItems = new ArrayList();
	itemMenuItems.add(EditObjectAction.class);
	itemMenuItems.add(EditExtensionAction.class);
	itemMenuItems.add(CopyAction.class);
	itemMenuItems.add(DeleteAction.class);
	treeMenuItems.add(NewRelationAction.class);

	PopupUtils.ContextSensitiveMenuItemGenerator csmig = new PopupUtils.ContextSensitiveMenuItemGenerator() {
		public java.util.List getContextSensitiveMenuItems() {
		    java.util.List result = new ArrayList();
		    // we're selecting a concept, present filtering options
		    if (getSelectedConcepts() != null) {
			//System.out.println("before init, getRelationFilter = " + getRelationFilter() + ", selectedConcept = " + getSelectedConcepts());
			if (getRelationFilter() == PowerloomTrees.SHOW_INHERITED_RELATIONS) {
			    result.add(new FilterRelationAction(PowerloomTrees.this, PowerloomTrees.SHOW_INHERITED_RELATIONS));
			} else {
			    result.add(new FilterRelationAction(PowerloomTrees.this, PowerloomTrees.SHOW_DIRECT_RELATIONS));
			} 
		    } 
		    return result;
		}
	    };

	PopupUtils.installTreePopup(tree, treeMenuItems, itemMenuItems, treeOnlyMenuItems, "RELATION-ROOT", csmig);	
    }
    private void installRulePopup(final JList list) {
	Collection listMenuItems = new ArrayList();
	Collection itemMenuItems = new ArrayList();
	Collection subObjectMenuItems = new ArrayList();
	//listMenuItems.add(new JMenuItem("Filter Rules..."));
	//System.out.println("itemMenuItems = " + itemMenuItems);
	//System.out.println("parent = " + parent);
	//System.out.println("parentframe = " + parent.getParentFrame());
	itemMenuItems.add(EditObjectAction.class);
	itemMenuItems.add(CopyAction.class);
	itemMenuItems.add(DeleteAction.class);
	listMenuItems.add(NewRuleAction.class);
	subObjectMenuItems.add(NavigateAction.class);
	PopupUtils.installListPopup(getSelectedModule(), list, listMenuItems, itemMenuItems, null, subObjectMenuItems, null);
    }

    public JTree makeConceptTree(PLSurrogateContainer root) throws Exception {
        PLTreeModel conceptTreeModel = new PLTreeModel(root);
	String[] types = {"Concept Holder", "Object Holder"};
        JTree conceptTree = new ConceptNavigationTree(conceptTreeModel, parent.getParentFrame(), "Concept Navigation Pane", Arrays.asList(types));

	// for testing:
	TreeExpansionListener expansionListener = 
			  (new TreeExpansionListener() {
				  public void treeExpanded(TreeExpansionEvent e) {
				      //System.out.println("Tree expanded: " + e);
				  }
				  public void treeCollapsed(TreeExpansionEvent e) {
				      //System.out.println("Tree collapsed: " + e);
				  }
			      });
	
        TreeSelectionListener selectionListener = 
            (new TreeSelectionListener() { 
                public void valueChanged(TreeSelectionEvent tse) {
                    processConceptTreeSelectionEvent(tse); 
                }
                });
	conceptTree.addTreeExpansionListener(expansionListener);
        conceptTree.addTreeSelectionListener(selectionListener);
        //conceptTree.setExpandsSelectedPaths(true);
	installConceptPopup(conceptTree);
        setRenderer(conceptTree);
        return conceptTree;
    }
    public JList makeInstanceList(PLSurrogateContainer instances) throws Exception {
        PLListModel instanceListModel = new PLListModel(getSelectedModule(), instances);
	String[] types = {"Instance Holder", "Object Holder"};
        JList instanceList = new DeletableObjectList(instanceListModel, parent.getParentFrame(), "Instance Navigation Pane", Arrays.asList(types));
        
        ListSelectionListener selectionListener = 
            (new ListSelectionListener() { 
                public void valueChanged(ListSelectionEvent lse) {
                    processInstanceListSelectionEvent(lse); 
                }
                });
        instanceList.addListSelectionListener(selectionListener);
        setRenderer(instanceList);
		installInstancePopup(instanceList);
        return instanceList;
    }
    /**
     * Tree making methods
     */

    private JTree makeModuleTree() throws AppException {
        moduleTreeModel = new PLTreeModel(KnowledgeManager.getInstance().getRootModule());
	String[] types = {"Module Holder", "Object Holder"};
        JTree moduleTree = new ModuleNavigationTree(moduleTreeModel, parent.getParentFrame(), "Module Navigation Pane", Arrays.asList(types));
        //moduleTree.setExpandsSelectedPaths(true);

        TreeSelectionListener selectionListener = 
            (new TreeSelectionListener() { 
                public void valueChanged(TreeSelectionEvent tse) {
                    processModuleTreeSelectionEvent(tse); 
                }
                });
        moduleTree.addTreeSelectionListener(selectionListener);
        setRenderer(moduleTree);
	installModulePopup(moduleTree);
        return moduleTree;
    }
    public JList makePropositionList(PLSurrogateContainer propositions) throws Exception {
        PLListModel propositionListModel = new PLListModel(getSelectedModule(), propositions);
	String[] types = {"Proposition Holder", "Object Holder"};
        JList propositionList = new PLPropositionList(propositionListModel, parent.getParentFrame(), "Proposition Navigation Pane", Arrays.asList(types));
        
        ListSelectionListener selectionListener = 
            (new ListSelectionListener() { 
                public void valueChanged(ListSelectionEvent lse) {
                    processPropositionListSelectionEvent(lse); 
                }
                });
        propositionList.addListSelectionListener(selectionListener);
        setRenderer(propositionList);
		installPropositionPopup(propositionList);
        return propositionList;
    }
    public JTree makeRelationTree(PLSurrogateContainer root) throws Exception {
        PLTreeModel relationTreeModel = new PLTreeModel(root);
	String[] types = {"Relation Holder", "Object Holder"};
        JTree relationTree = new RelationNavigationTree(relationTreeModel, parent.getParentFrame(), "Relation Navigation Pane", Arrays.asList(types));
        //relationTree.setExpandsSelectedPaths(true);

        TreeSelectionListener selectionListener = 
            (new TreeSelectionListener() { 
                public void valueChanged(TreeSelectionEvent tse) {
                    processRelationTreeSelectionEvent(tse); 
                }
                });
        relationTree.addTreeSelectionListener(selectionListener);
	installRelationPopup(relationTree);
        setRenderer(relationTree);
        return relationTree;
    }
    public JList makeRuleList(PLSurrogateContainer propositions) throws Exception {
        PLListModel ruleListModel = new PLListModel(getSelectedModule(), propositions);
	String[] types = {"Rule Holder", "Object Holder"};
        JList ruleList = new PLPropositionList(ruleListModel, parent.getParentFrame(), "Rule Navigation Pane", Arrays.asList(types));
        
        ListSelectionListener selectionListener = 
            (new ListSelectionListener() { 
                public void valueChanged(ListSelectionEvent lse) {
                    processRuleListSelectionEvent(lse); 
                }
                });
        ruleList.addListSelectionListener(selectionListener);
        setRenderer(ruleList);
		installRulePopup(ruleList);
        return ruleList;
    }
/**
 * Insert the method's description here.
 * Creation date: (4/12/2002 9:52:05 PM)
 * @param e redesign.gui.components.PLEditEvent
 */
public void performEdit(PLEditEvent e) {
    if (e.getEditType() == null) {
	try {
	    debugPrintln(3, "performing edit in pltrees... refreshing everything.");
	    refreshEverything();
	} catch (Exception e2) {
	    handleException(e2);
	}
    }

	if (e.getEditType() == PLConcept.class) {
		try {
		    //debugPrintln(3, "performing edit in pltrees!");
		    debugPrintln(3, "  edit was perform with concept = " + e.getEditObject());
		    refreshConceptTree((PLConcept)e.getEditObject());
		} catch (Exception e2) {
		    handleException(e2);
		}
	}
	if (e.getEditType() == PLRelation.class) {
		try {
		    //debugPrintln(3, "performing rel edit in pltrees!");
			refreshRelationTree();
		} catch (Exception e2) {
		    handleException(e2);
		}
	}
	if (e.getEditType() == PLModule.class) {
		try {
		    //debugPrintln(3, "performing module edit in pltrees!");
		    refreshModuleTree();
		    makeObjectVisible((PLModule)e.getEditObject(), null, true);

		} catch (Exception e2) {
		    handleException(e2);
		}
	}
	if (e.getEditType() == PLInstance.class) {
		try {
		    //debugPrintln(3, "performing instance edit in pltrees!");
			refreshInstanceList();
		} catch (Exception e2) {
		    handleException(e2);
		}
	}
	if (e.getEditType() == PLProposition.class) {
		try {
		    //changes to propositions can affect anything... wipe out the entire cache...
		    refreshEverything();

		    //refreshConceptTree(getSelectedConcept());
		    //refreshPropositionList();
		    //refreshRuleList();
		    //refreshPropositionList();
		    //refreshModuleTree();
		    //debugPrintln(3, "performing prop edit in pltrees!");
		    //refreshPropositionList();
		    // for now, we don't have a separate data structure for props and rules...
		    // maybe this is a mistake...
		    //refreshRuleList();
		} catch (Exception e2) {
		    handleException(e2);
		}
	}
	
}
    private void processConceptTreeSelectionEvent(TreeSelectionEvent tse) {
        Cursor savedCursor = parent.getCursor();
        try {
            parent.setCursor(new Cursor(Cursor.WAIT_CURSOR));
            updateInstanceListModel(null, tse);
            updateRelationTreeModel(null, tse);
            updatePropositionListModel(null, tse, null, null);
            updateRuleListModel(null, tse, null);
	    PLConcept concept = getSelectedConcept(tse);
	    updateMostRecentlyTouchedObject(concept);
	    recordCurrentSelection(concept);
        } catch (Exception e) {
	    handleException(e);
        } finally {
            parent.setCursor(savedCursor);
        }
    }
    private void processInstanceListSelectionEvent(ListSelectionEvent lse) {
	if (lse.getValueIsAdjusting()) {
	    return;
	}
        Cursor savedCursor = parent.getCursor();
        try {
            parent.setCursor(new Cursor(Cursor.WAIT_CURSOR));
            updatePropositionListModel(null, null, lse, null);            
	    PLInstance instance = getSelectedInstance(lse);
	    recordCurrentSelection(instance);
	    updateMostRecentlyTouchedObject(instance);
        } catch (Exception e) {
	    handleException(e);
        } finally {
            parent.setCursor(savedCursor);
        }
    }
    private void processModuleTreeSelectionEvent(TreeSelectionEvent tse) {
	debugPrintln(3, "processmodtreeselectionevent");
        Cursor savedCursor = parent.getCursor();
        try {
            parent.setCursor(new Cursor(Cursor.WAIT_CURSOR));
            updateConceptTreeModel(tse);
            updateInstanceListModel(tse, null);
            updateRelationTreeModel(tse, null);
            updatePropositionListModel(tse, null, null, null);
            updateRuleListModel(tse, null, null);
	    PLModule module = getSelectedModule(tse);
	    updatePropositionModuleView(module);
	    updateRuleModuleView(module);
	    updateMostRecentlyTouchedObject(module);
            parent.setCursor(savedCursor);
	    recordCurrentSelection(module);
        } catch (Exception e) {
	    handleException(e);
        } finally {
            parent.setCursor(savedCursor);
        }
    }
    private void processPropositionListSelectionEvent(ListSelectionEvent lse) {
	if (lse.getValueIsAdjusting()) {
	    return;
	}
        Cursor savedCursor = parent.getCursor();
        try {
            parent.setCursor(new Cursor(Cursor.WAIT_CURSOR));
	    PLProposition proposition = (PLProposition)getSelectedProposition(lse);
	    updateMostRecentlyTouchedObject(proposition);
	    recordCurrentSelection(proposition, false);
        } catch (Exception e) {
	    handleException(e);
        } finally {
            parent.setCursor(savedCursor);
        }
    }
    private void processRelationTreeSelectionEvent(TreeSelectionEvent tse) {
        Cursor savedCursor = parent.getCursor();
        try {
            parent.setCursor(new Cursor(Cursor.WAIT_CURSOR));
            updatePropositionListModel(null, null, null, tse);
            updateRuleListModel(null, null, tse);
	    PLRelation relation = getSelectedRelation(tse);
	    updateMostRecentlyTouchedObject(relation);
	    recordCurrentSelection(relation);
        } catch (Exception e) {
	    handleException(e);
        } finally {
            parent.setCursor(savedCursor);
        }
    }
    private void processRuleListSelectionEvent(ListSelectionEvent lse) {
	if (lse.getValueIsAdjusting()) {
	    return;
	}
        Cursor savedCursor = parent.getCursor();
        try {
            parent.setCursor(new Cursor(Cursor.WAIT_CURSOR));
	    PLProposition rule = (PLProposition)getSelectedRule(lse);
	    updateMostRecentlyTouchedObject(rule);
	    recordCurrentSelection(rule, true);
        } catch (Exception e) {
	    handleException(e);
        } finally {
            parent.setCursor(savedCursor);
        }
    }
    
    /**
     * @return all paths which to 'object'.
     */
    TreePath[] getPathsForNode(JTree tree, PLObject object) {
        try {
            // Make sure we have a current object (i.e., not one that has become
            // obsolete because of a cache refresh).
            PLSurrogate surrogate = KnowledgeManager.getInstance().findOrCreateSurrogate(object.getClass(), object.getID());
            object = (PLObject)surrogate.getValue();
        
	    java.util.List pathSoFar = new ArrayList();
            java.util.List foundPaths = new ArrayList();
	    //System.out.println();
	    helpGetPathsForNode((PLTreeNode)((PLTreeModel)tree.getModel()).getRoot(), pathSoFar, foundPaths, 
				(PLTreeNode)object);
            return (TreePath[])foundPaths.toArray(new TreePath[0]);
        } catch (Exception e) {
	    handleException(e);
            return null;
        }
    }

    void helpGetPathsForNode(PLTreeNode node, java.util.List pathSoFar, java.util.List foundPaths, PLTreeNode searchNode) {
	//debugPrintln(3, " comparing node = " + node + ":" + node.hashCode() + " searchNode = " + searchNode + ":" + searchNode.hashCode());
	java.util.List newPathSoFar = (java.util.List)((ArrayList)pathSoFar).clone();
	newPathSoFar.add(node);
	if (node == searchNode) {
	    foundPaths.add(new TreePath(newPathSoFar.toArray()));
	    //debugPrintln(3, "found a path! -> " + (new TreePath(newPathSoFar.toArray())));
	}
	for (int i = 0; i < node.getChildCount(); i++) {
	    PLTreeNode child = node.getChild(i);
	    helpGetPathsForNode(child, newPathSoFar, foundPaths, searchNode);
	}
    }
    
    PLObject getCurrentObject(Class objectType, PLObject object) throws AppException {
	return KnowledgeManager.getInstance().getCurrentObject(objectType, object);
    }



    /**
     * Make concept, relation, or instance visible in Browser... This is used if we want
     * to select *only* the given object, all other selections are wiped out.
     */
	 public void makeObjectVisible(PLModule module, PLObject object, boolean selectPaths) {
	 try {
	     JTree tree = null;
	     JList list = null;

	     debugPrintln(3, "Going to make object " + object + ", module = " + module + " visible in " + tree);
	     if (module == null) {
		 return;
	     }

	     JTree moduleTree = parent.getModuleTree();

	     TreePath[] paths = getPathsForNode(moduleTree, module);
	     for (int i = 0; i < paths.length; i++) {
		 moduleTree.scrollPathToVisible(paths[i]);
	     }
	     moduleTree.setSelectionPaths(paths);

	     if (object != null) {
		 if (object instanceof PLConcept) {
		     object = getCurrentObject(PLConcept.class, object);
		     debugPrintln(3, "new concept object = " + object);
		     parent.getInstanceList().clearSelection();
		     parent.getRelationTree().clearSelection();
		     tree = parent.getConceptTree();
		 } else if (object instanceof PLRelation) {
		     getCurrentObject(PLRelation.class, object);
		     // clear concept selections
			    parent.getConceptTree().clearSelection();
		     parent.getInstanceList().clearSelection();
		     tree = parent.getRelationTree();
		 } else if (object instanceof PLInstance) {
		     object = getCurrentObject(PLInstance.class, object);
		     parent.getConceptTree().clearSelection();
		     parent.getRelationTree().clearSelection();
		     list = parent.getInstanceList();
		 }
		 if (tree != null) {
		     paths = getPathsForNode(tree, object);
		     debugPrintln(3, "selected tree: " + tree);
		     for (int i = 0; i < paths.length; i++) {
			 debugPrintln(3, "scrolling to path for node: " + paths[i]);
			 tree.scrollPathToVisible(paths[i]);
		     }
		     if (selectPaths) {
			 tree.setSelectionPaths(paths);
		     }
		 }
		 if (list != null) {
		     //debugPrintln(3, "going to select object: " + object);
		     list.setSelectedValue(object, true);
		 }
	     }
	 } catch (Exception e) {
	     handleException(e);
	 }
     }

    /**
     *  History and refesh-related methods
     */
    
    /**
     *  Record for storing selection state
     */
    class SelectionRecord {
	public PLModule module;
	public PLConcept concept;
	public PLRelation relation;
	public PLInstance instance;
	public PLProposition proposition;
	public PLProposition rule;

	public String toString() {
	    return "[Module = " + module + ", concept = " + concept + ", relation = " + relation + ", instance = " + instance + ", proposition = " + proposition + ", rule = " + rule + "]";
	}
    }
    

    public boolean isAtBeginningOfHistory() {
	if (selectionHistory.size() == 0) {
	    return true;
	}
	return (historyCursor == 0);
    }


    public boolean isAtEndOfHistory() {
	if (selectionHistory.size() == 0) {
	    return true;
	}
	return (historyCursor == (selectionHistory.size() -1));
    }


    public void recordCurrentSelection() {
	// don't record if we're in the middle of a navigation operation.
	if (isDoingNavigation()) {
	    return;
	}
	SelectionRecord selection = getAllSelections();
	//debugPrintln(3, "recording selection: " + selection.hashCode());
	selectionHistory.add(selection);
	if (historyCursor == (selectionHistory.size() - 2)) {
	    historyCursor = selectionHistory.size() - 1;
	}
	PowerloomApp.getInstance().updateNavigationMenuState(this);
    }

    public void recordCurrentSelection(PLModule module) {
	if (isDoingNavigation()) {
	    return;
	}
	recordCurrentSelection();
	SelectionRecord selection = (SelectionRecord)selectionHistory.get(selectionHistory.size() - 1);
	selection.module = module;
	//debugPrintln(3, "  selection module = " + selection.module);
    }

    public void recordCurrentSelection(PLConcept concept) {
	if (isDoingNavigation()) {
	    return;
	}
	recordCurrentSelection();
	SelectionRecord selection = (SelectionRecord)selectionHistory.get(selectionHistory.size() - 1);
	selection.concept = concept;
	//debugPrintln(3, "  selection concept = " + selection.concept);
    }

    public void recordCurrentSelection(PLRelation relation) {
	if (isDoingNavigation()) {
	    return;
	}
	recordCurrentSelection();
	SelectionRecord selection = (SelectionRecord)selectionHistory.get(selectionHistory.size() - 1);
	selection.relation = relation;
	//debugPrintln(3, "  selection relation = " + selection.relation);
    }

    public void recordCurrentSelection(PLInstance instance) {
	if (isDoingNavigation()) {
	    return;
	}
	recordCurrentSelection();
	SelectionRecord selection = (SelectionRecord)selectionHistory.get(selectionHistory.size() - 1);
	selection.instance = instance;
    }

    public void recordCurrentSelection(PLProposition proposition, boolean isRule) {
	if (isDoingNavigation()) {
	    return;
	}
	recordCurrentSelection();
	SelectionRecord selection = (SelectionRecord)selectionHistory.get(selectionHistory.size() - 1);
	if (isRule) {
	    selection.rule = proposition;
	} else {
	    selection.proposition = proposition;
	}
	//debugPrintln(3, "  selection " + (isRule ? "rule" : "proposition") + " = " + 
	//(isRule ? selection.rule : selection.proposition));
    }

    public SelectionRecord getSelectionAtCursor() {
	return (SelectionRecord)selectionHistory.get(historyCursor);
    }

    public void navigateToPreviousSelection() {
	historyCursor = Math.max(0, historyCursor - 1);
	doNavigation();
    }
    
    public void navigateToNextSelection() {
	historyCursor = Math.min(selectionHistory.size() - 1, historyCursor + 1);
	doNavigation();
    }

    private void doNavigation() {
	SelectionRecord selection = getSelectionAtCursor();
	setDoingNavigation();
	setAllSelections(selection);
	unsetDoingNavigation();
	debugPrintln(3, "navigated to selection: " + selection.hashCode() + ", " + selection);
	PowerloomApp.getInstance().updateNavigationMenuState(this);
    }

    private void setDoingNavigation() {
	navigating = true;
    }

    private void unsetDoingNavigation() {
	navigating = false;
    }

    private boolean isDoingNavigation() {
	return navigating;
    }
    
    /** Return a tuple of selected objects, which includes Module,
     *  Concept, Relation, Instance, Proposition, and Rule */
    public SelectionRecord getAllSelections() {
	SelectionRecord record = new SelectionRecord();
	record.module = getSelectedModule();
	record.concept = getSelectedConcept();
	record.relation = getSelectedRelation();
	record.instance = getSelectedInstance();
	record.proposition = getSelectedProposition();
	record.rule = getSelectedRule();
	return record;
    }

    /**
     *  Wipes out all caches, and refreshes all trees and selections
     */
    public void refreshEverything() {
	try {
	    SelectionRecord savedSelections = getAllSelections();
	    //KnowledgeManager.getInstance().invalidateAllCaches();
	    KnowledgeManager.getInstance().initializeModules();
	    refreshModuleTree();
	    setAllSelections(savedSelections);
	} catch (Exception e) {
	    PowerloomApp.getInstance().handleException(e);
	}
    }


    public void setAllSelections(SelectionRecord selections) {
	try {
	    PLModule selectedModule = selections.module;
	    debugPrintln(3, "  selected module = " + selectedModule);
	    if (selectedModule == null) {
		parent.getModuleTree().clearSelection();
		return;
	    }
	    if (selectedModule != null) {
		refreshModuleTree();
		JTree modTree = parent.getModuleTree();
		TreePath[] modPaths = getPathsForNode(modTree, getCurrentObject(PLModule.class, selectedModule));
		for (int i = 0; i < modPaths.length; i++) {
		    modTree.scrollPathToVisible(modPaths[i]);
		}
		modTree.setSelectionPaths(modPaths);

		PLConcept selectedConcept = selections.concept;
		//debugPrintln(3, "  selected concept = " + selectedConcept);
		if (selectedConcept != null) {
		    refreshConceptTree(selectedConcept);
		    JTree tree = parent.getConceptTree();
		    TreePath[] paths = getPathsForNode(tree, getCurrentObject(PLConcept.class, selectedConcept));
		    for (int i = 0; i < paths.length; i++) {
			tree.scrollPathToVisible(paths[i]);
		    }
		    tree.setSelectionPaths(paths);
		}
		PLRelation selectedRelation = selections.relation;
		//debugPrintln(3, "  selected relation = " + selectedRelation);
		if (selectedRelation != null) {
		    refreshRelationTree();
		    JTree tree = parent.getRelationTree();
		    TreePath[] paths = getPathsForNode(tree, getCurrentObject(PLRelation.class, selectedRelation));
		    for (int i = 0; i < paths.length; i++) {
			tree.scrollPathToVisible(paths[i]);
		    }
		    tree.setSelectionPaths(paths);
		}
		PLInstance selectedInstance = selections.instance;
		//debugPrintln(3, "  selected instance = " + selectedInstance);
		if (selectedInstance != null) {
		    refreshInstanceList();
		    JList list = parent.getInstanceList();
		    list.setSelectedValue(getCurrentObject(PLInstance.class, selectedInstance), true);
		}
		PLProposition selectedProposition = selections.proposition;
		//debugPrintln(3, "  selected proposition = " + selectedProposition);
		if (selectedProposition != null) {
		    refreshPropositionList();
		    JList list = parent.getPropositionList();
		    list.setSelectedValue(getCurrentObject(PLProposition.class, selectedProposition), true);
		}
		PLProposition selectedRule = selections.rule;
		//debugPrintln(3, "  selected rule = " + selectedRule);
		if (selectedRule != null) {
		    refreshRuleList();
		    JList list = parent.getRuleList();
		    list.setSelectedValue(getCurrentObject(PLProposition.class, selectedRule), true);
		}
	    }
	} catch (Exception e) {
	    PowerloomApp.getInstance().handleException(e);
	}
    }

    /**
     *  The refresh methods are called when some update to the kb
     *  has been performed which can affect the contents of a given
     *  navigation pane.
     */

    public void refreshConceptTree(PLConcept editedConcept) throws Exception {
        JTree tree = parent.getConceptTree();
		if (tree == null) {
		    return;
		}
        

        TreePath objectSelectionPath = tree.getLeadSelectionPath();
	debugPrintln(3, " concept selection path = " + objectSelectionPath);
        // Invalidating all caches is overkill.  I need to analyze
        // exactly which cachces need to be invalidated...
        KnowledgeManager.getInstance().invalidateAllCaches();
        
        updateConceptTreeModel((TreeSelectionEvent)null);
	if (editedConcept == null) {
	    // if this concept no longer exists, don't try to update selections.  Probably we
	    // should attempt to the best we can, e.g., expand to the parents of the deleted concept...
	    return;
	}
        tree = parent.getConceptTree();
	PLModule selectedModule = getSelectedModule();
	// don't know why I had this....
	debugPrintln(3, "Making " + editedConcept + " visible.");
	makeObjectVisible(selectedModule, editedConcept, true);
        if (refreshSelectionPath(tree, objectSelectionPath)) {
            debugPrintln(3, "---> concept refreshSelectionPath returned true.");
            // do stuff?
        } else {
            debugPrintln(3, "---> concept refreshSelectionPath returned false.");
            updateInstanceListModel((TreeSelectionEvent)null, (TreeSelectionEvent)null);
            updateRelationTreeModel((TreeSelectionEvent)null, (TreeSelectionEvent)null);
            updatePropositionListModel(null, null, null, null);
            updateRuleListModel(null, null, null);
        }
    }
    
    public void refreshInstanceList() throws Exception {
        JList list = parent.getInstanceList();
        if (list == null) return;
        Object selectedObject = list.getSelectedValue();
        KnowledgeManager.getInstance().invalidateInstanceCaches();
        updateInstanceListModel((TreeSelectionEvent)null, (TreeSelectionEvent)null);
        list = parent.getInstanceList();
        if (refreshSelectionPath(list, selectedObject)) {
            // do stuff?
        } else {
            updatePropositionListModel(null, null, null, null);
        }
    }
    public void refreshModuleTree() throws Exception {
        JTree tree = parent.getModuleTree();
        if (tree == null) return;
        TreePath moduleSelectionPath = tree.getLeadSelectionPath();
        updateModuleTreeModel();
        tree = parent.getModuleTree();
        if (refreshSelectionPath(tree, moduleSelectionPath)) {
	    debugPrintln(3, "refreshselection for moduletree returned true");
            // do stuff?
		      //debugPrintln(3, "refresh mod should to updates here...");
            updateConceptTreeModel((TreeSelectionEvent)null);
            updateInstanceListModel((TreeSelectionEvent)null, (TreeSelectionEvent)null);
            updateRelationTreeModel((TreeSelectionEvent)null, (TreeSelectionEvent)null);
            updatePropositionListModel(null, null, null, null);
            updateRuleListModel(null, null, null);

        } else {
            updateConceptTreeModel((TreeSelectionEvent)null);
            updateInstanceListModel((TreeSelectionEvent)null, (TreeSelectionEvent)null);
            updateRelationTreeModel((TreeSelectionEvent)null, (TreeSelectionEvent)null);
            updatePropositionListModel(null, null, null, null);
            updateRuleListModel(null, null, null);
        } 
    }
    public void refreshPropositionList() throws Exception {
        // since a proposition may have modified/created an instance...
        JList list = parent.getPropositionList();
        if (list == null) return;
        Object selectedObject = list.getSelectedValue();
        KnowledgeManager.getInstance().invalidatePropositionCaches();
	// whats this line doing here?
        //refreshInstanceList();         
        updatePropositionListModel(null, null, null, null);
        if (refreshSelectionPath(list, selectedObject)) {
            // do stuff?
        } else {
            // do stuff?
        }
    }
    public void refreshRelationTree() throws Exception {
        JTree tree = parent.getRelationTree();
        if (tree == null) return;
        TreePath objectSelectionPath = tree.getLeadSelectionPath();
	debugPrintln(3, " refreshrelationtree, selectionpath = " + objectSelectionPath);
        KnowledgeManager.getInstance().invalidateRelationCaches();
        updateRelationTreeModel((TreeSelectionEvent)null, (TreeSelectionEvent)null);
        tree = parent.getRelationTree();
        if (refreshSelectionPath(tree, objectSelectionPath)) {
            // do stuff?
        } else {
            updatePropositionListModel(null, null, null, null);
            updateRuleListModel(null, null, null);
        }
    }
    public void refreshRuleList() throws Exception {
        //debugPrintln(3, "-->in refreshRuleList");
        JList list = parent.getRuleList();
        if (list == null) return;
        Object selectedObject = list.getSelectedValue();
        KnowledgeManager.getInstance().invalidateRuleCaches();
        updateRuleListModel(null, null, null);
        if (refreshSelectionPath(list, selectedObject)) {
            // do stuff?
        } else {
            // do stuff?
        }
    }
    /**
     * Try to Reset the selection path for the new list to what it used to be.
     * @return a true if the old path still exists in the current tree
     */
    //todo: make this handle multiple selections...
    private boolean refreshSelectionPath(JList list, Object selectedObject) throws AppException {
        // there wasn't any selection to begin with.
        if (selectedObject == null) {
            return true;
        }
        PLObject plObject = (PLObject)selectedObject;
        PLSurrogate surrogate = KnowledgeManager.getInstance().findOrCreateSurrogate(plObject.getClass(), plObject.getID());
        PLObject newPLObject = (PLObject)surrogate.getValue();
        // the only way that the new object can be equal to the old object is
        // if the old object has been removed from the list... otherwise it would
        // have been replaced by a fresh object.
        if (plObject == newPLObject) {  
            return false;
        } else {
            for (int i = 0; i < list.getModel().getSize(); i++) {
                if (list.getModel().getElementAt(i) == newPLObject) {
                    list.setSelectedIndex(i);
                    return true;
                }
            } 
            // for some reason, we couldn't find the new object...
            return false;
        } 
    }
    /**
     *  Refresh methods (get data from server)
     */

    /**
     * Try to Reset the selection path for the new tree to what it used to be.
     * @return a boolean value indicating whether the old path still exists in the current tree
     */
    private boolean refreshSelectionPath(JTree tree, TreePath objectSelectionPath) throws Exception {
        // todo: implement for multiple selection paths
					    
        if (objectSelectionPath != null) {
            Object[] newSelectionPath = new Object[objectSelectionPath.getPath().length];
              for (int i = 0; i < objectSelectionPath.getPath().length; i++) {
                  PLObject currentObject = (PLObject)objectSelectionPath.getPath()[i];
                  PLSurrogate surrogate = KnowledgeManager.getInstance().findOrCreateSurrogate(objectSelectionPath.getPath()[i].getClass(), currentObject.getID());
                  PLObject newlySelectedObject = (PLObject)surrogate.getValue();
		  debugPrintln(3, "newlySelecteObject = " + newlySelectedObject);
		  if (newlySelectedObject == null) {
		      // someone must have moved or deleted this previously-selected object:
		      return false;
		  }
                  newSelectionPath[i] = newlySelectedObject;
              } 
              // for testing- screw up selectionpath
              //newSelectionPath[newSelectionPath.length-1] = new PLConcept();
              TreePath newTreePath = new TreePath(newSelectionPath);
              tree.setSelectionPath(newTreePath);                  
	      tree.scrollPathToVisible(newTreePath);
        } else {
            return true;
        } 
        // todo: figure out how see if all the objects in the path are in the new tree
        //  for now, always return true, meaning that nodes can't change or be removed.
        return true;
    }
    public void setRelationFilter(int relationFilter) {
	this.relationFilter = relationFilter;
	//debugPrintln(3, "setting relation filter to " + getRelationFilter());
    }
    public void setInstanceFilter(int instanceFilter) {
	this.instanceFilter = instanceFilter;
	//debugPrintln(3, "setting relation filter to " + getRelationFilter());
    }
    private void setRenderer(JList list) {
        list.setCellRenderer(new PLListRenderer());
        ToolTipManager.sharedInstance().registerComponent(list);
    }
    private void setRenderer(JTree tree) {
        tree.setCellRenderer(new PLTreeRenderer());
        ToolTipManager.sharedInstance().registerComponent(tree);
    }
    void updateConceptTreeModel(PLModule module) throws Exception {
        PLSurrogateContainer root = KnowledgeManager.getInstance().getConceptsForModule(module);
	//debugPrintln(3, "got concepts: " + root);
        JTree conceptTree = makeConceptTree(root);
        parent.setConceptTree(conceptTree);
        parent.getConceptPanel().getPubNavigationLabel().setText("Concepts for " + module.getID());
        parent.getConceptPanel().getPubNavigationLabel().setToolTipText("Concepts for " + module.getID());
    }
    void updateConceptTreeModel(TreeSelectionEvent moduleSelectionEvent) throws Exception {
        PLModule module = getSelectedModule(moduleSelectionEvent);
        if (module == null) {
            parent.clearConceptTree();
            parent.getConceptPanel().getPubNavigationLabel().setText("Concepts");
            parent.getConceptPanel().getPubNavigationLabel().setToolTipText("Concepts");
            return;
        } 
        updateConceptTreeModel(module);
    }
    void updateInstanceListModel(PLModule module) throws Exception {
        PLSurrogateContainer root = KnowledgeManager.getInstance().getInstancesForModule(module);
        JList instanceList = makeInstanceList(root);
        parent.setInstanceList(instanceList);
        parent.getInstancePanel().getPubNavigationLabel().setText("Instances for " + module.getID());        
        parent.getInstancePanel().getPubNavigationLabel().setToolTipText("Instances for " + module.getID());        
    }
    void updateInstanceListModel(PLModule module, PLConcept concept) throws Exception {
        PLSurrogateContainer root = null;

	if (getInstanceFilter() == PowerloomTrees.SHOW_INHERITED_INSTANCES) {
	    root = KnowledgeManager.getInstance().getInstancesForConcept(module, concept);
            parent.getInstancePanel().getPubNavigationLabel().setText("Derived Instances for " + concept.getID());
            parent.getInstancePanel().getPubNavigationLabel().setToolTipText("Derived Relations for " + concept.getID());
	} else {
	    root = KnowledgeManager.getInstance().getDirectInstancesForConcept(module, concept);
            parent.getInstancePanel().getPubNavigationLabel().setText("Direct Instances for " + concept.getID());
            parent.getInstancePanel().getPubNavigationLabel().setToolTipText("Direct Instances for " + concept.getID());
	} 

        JList instanceList = makeInstanceList(root);
        parent.setInstanceList(instanceList);

    }
    void updateInstanceListModel(TreeSelectionEvent moduleSelectionEvent, 
                                 TreeSelectionEvent conceptSelectionEvent) throws Exception {
        PLModule module = getSelectedModule(moduleSelectionEvent);
        if (module == null) {
            parent.clearInstanceList();
            parent.getInstancePanel().getPubNavigationLabel().setText("Instances");        
            parent.getInstancePanel().getPubNavigationLabel().setToolTipText("Instances");        
            return;
        } 
        PLConcept concept = getSelectedConcept(conceptSelectionEvent); 
        if (concept == null) {
            parent.clearInstanceList();
            updateInstanceListModel(module);
        } else {
            updateInstanceListModel(module, concept);
        } 
    }
    /**
     * tree/model updates
     */

    void updateModuleTreeModel() throws Exception {
        KnowledgeManager.getInstance().initializeModules();
        JTree moduleTree = makeModuleTree();
        parent.setModuleTree(moduleTree);
    }

    public PLObject getMostRecentlyTouchedObject() {
	return mostRecentlyTouchedObject;
    }

    public PLModule getMostRecentlyTouchedModule() {
	return mostRecentlyTouchedModule;
    }

    public void updateMostRecentlyTouchedObject(PLObject object) {
	mostRecentlyTouchedObject = object;
	PowerloomApp.getInstance().setMostRecentlyTouchedObject(object);
	if (object instanceof PLModule) {
	    mostRecentlyTouchedModule = (PLModule)object;
	    PowerloomApp.getInstance().setMostRecentlyTouchedModule((PLModule)object);
	}
    }
    void updatePropositionListModel(PLModule module, PLConcept concept) throws Exception {
        PLSurrogateContainer root = KnowledgeManager.getInstance().getPropositionsForConcept(module, concept);
        JList propositionList = makePropositionList(root);
        parent.setPropositionList(propositionList);
        parent.getPropositionPanel().getPubNavigationLabel().setText("Propositions for " + concept.getID());                        
        parent.getPropositionPanel().getPubNavigationLabel().setToolTipText("Propositions for " + concept.getID());                        
    }
    void updatePropositionListModel(PLModule module, PLInstance instance) throws Exception {
	debugPrintln(3, "in updateprop list model.  mod = " + module.getID() + ", instance = " + instance.getID());
        PLSurrogateContainer root = KnowledgeManager.getInstance().getPropositionsForInstance(module, instance);
        JList propositionList = makePropositionList(root);
        parent.setPropositionList(propositionList);
        parent.getPropositionPanel().getPubNavigationLabel().setText("Propositions for " + instance.getID());                        
        parent.getPropositionPanel().getPubNavigationLabel().setToolTipText("Propositions for " + instance.getID());                        
    }
    void updatePropositionListModel(PLModule module, PLInstance instance, PLRelation relation) throws Exception {
        PLSurrogateContainer root = KnowledgeManager.getInstance().getPropositionsForInstanceAndRelation(module, instance, relation);
        JList propositionList = makePropositionList(root);
        parent.setPropositionList(propositionList);
        parent.getPropositionPanel().getPubNavigationLabel().setText("Propositions for " + relation.getID() + " and " + instance.getID());
        parent.getPropositionPanel().getPubNavigationLabel().setToolTipText("Propositions for " + relation.getID() + " and " + instance.getID());
    }
    void updatePropositionListModel(PLModule module, PLRelation relation) throws Exception {
        PLSurrogateContainer root = KnowledgeManager.getInstance().getPropositionsForRelation(module, relation);
        JList propositionList = makePropositionList(root);
        parent.setPropositionList(propositionList);
        parent.getPropositionPanel().getPubNavigationLabel().setText("Propositions for " + relation.getID());                                
        parent.getPropositionPanel().getPubNavigationLabel().setToolTipText("Propositions for " + relation.getID());                                
    }
    void updatePropositionListModel(TreeSelectionEvent moduleSelectionEvent, 
                                    TreeSelectionEvent conceptSelectionEvent,
                                    ListSelectionEvent instanceSelectionEvent,
                                    TreeSelectionEvent relationSelectionEvent) throws Exception {
        //PLModule module = getSelectedModule(moduleSelectionEvent);
	PLModule module = getPropositionViewModule();
	if (module != null) {
	    debugPrintln(3, "in updatePropositionListModel. module = " + module.getID());
	} else {
	    debugPrintln(3, "in updatePropositionListModel. module = null");
	}

        if (module == null) {
            parent.clearPropositionList();
            parent.getPropositionPanel().getPubNavigationLabel().setText("Propositions");                                    
            parent.getPropositionPanel().getPubNavigationLabel().setToolTipText("Propositions");                                    
            return;
        } 
        PLConcept concept = getSelectedConcept(conceptSelectionEvent); 
        PLInstance instance = getSelectedInstance(instanceSelectionEvent);
        PLRelation relation = getSelectedRelation(relationSelectionEvent);

	debugPrintln(3, "  concept = " + ((concept == null) ? "null" : concept.getID()) + ", relation = " + ((relation == null) ? "null" : relation.getID()) + ", instance = " + ((instance == null) ? "null" : instance.getID()));

        if (relation == null) {
            if (instance == null) {
                if (concept == null) {
                    parent.clearPropositionList();
                    parent.getPropositionPanel().getPubNavigationLabel().setText("Propositions");                                                
                    parent.getPropositionPanel().getPubNavigationLabel().setToolTipText("Propositions");                                                
                } else {  // concept != null
                    updatePropositionListModel(module, concept);
                } 
            } else {  // instance != null, relation == null
                updatePropositionListModel(module, instance);
            } 
        } else { // relation != null
            if (instance == null) {
                updatePropositionListModel(module, relation);                
            } else {
                updatePropositionListModel(module, instance, relation);
            } 
        } 
    }
    void updateRelationTreeModel(PLModule module) throws Exception {
        PLSurrogateContainer root = KnowledgeManager.getInstance().getRelationsForModule(module);
        JTree relationTree = makeRelationTree(root);
        parent.setRelationTree(relationTree);
        parent.getRelationPanel().getPubNavigationLabel().setText("Relations for " + module.getID());        
        parent.getRelationPanel().getPubNavigationLabel().setToolTipText("Relations for " + module.getID());        
    }
    void updateRelationTreeModel(PLModule module, PLConcept concept) throws Exception {
	debugPrintln(3, "updating relation tree model, filterstate = " + getRelationFilter());
        PLSurrogateContainer root = null;
	if (getRelationFilter() == PowerloomTrees.SHOW_INHERITED_RELATIONS) {
	    root = KnowledgeManager.getInstance().getInheritedRelationsForConcept(module, concept);
            parent.getRelationPanel().getPubNavigationLabel().setText("Inherited Relations for " + concept.getID());                        
            parent.getRelationPanel().getPubNavigationLabel().setToolTipText("Inherited Relations for " + concept.getID());                        
	} else {
	    root = KnowledgeManager.getInstance().getRelationsForConcept(module, concept);
            parent.getRelationPanel().getPubNavigationLabel().setText("Direct Relations for " + concept.getID());                        
            parent.getRelationPanel().getPubNavigationLabel().setToolTipText("Direct Relations for " + concept.getID());                        
	} 
        JTree relationTree = makeRelationTree(root);
        parent.setRelationTree(relationTree);
    }
    void updateRelationTreeModel(TreeSelectionEvent moduleSelectionEvent, 
                                 TreeSelectionEvent conceptSelectionEvent) throws Exception {
        PLModule module = getSelectedModule(moduleSelectionEvent);
	//debugPrintln(3, "updatereltree, module = " + module);
        if (module == null) {
            parent.clearRelationTree();
            parent.getRelationPanel().getPubNavigationLabel().setText("Relations");        
            return;
        } else {
            PLConcept concept = getSelectedConcept(conceptSelectionEvent); 
	    //debugPrintln(3, "updatereltree, concept = " + concept);
            if (concept == null) {
                updateRelationTreeModel(module);
            } else {
                updateRelationTreeModel(module, concept);
            }
        } 
    }
    void updateRuleListModel(PLModule module, PLConcept concept) throws Exception {
        PLSurrogateContainer root = KnowledgeManager.getInstance().getRulesForConcept(module, concept);
        JList propositionList = makeRuleList(root);
        parent.setRuleList(propositionList);
        parent.getRulePanel().getPubNavigationLabel().setText("Rules for " + concept.getID());                        
        parent.getRulePanel().getPubNavigationLabel().setToolTipText("Rules for " + concept.getID());                        
    }
    void updateRuleListModel(PLModule module, PLRelation relation) throws Exception {
        PLSurrogateContainer root = KnowledgeManager.getInstance().getRulesForRelation(module, relation);
        JList propositionList = makeRuleList(root);
        parent.setRuleList(propositionList);
        parent.getRulePanel().getPubNavigationLabel().setText("Rules for " + relation.getID());                        
        parent.getRulePanel().getPubNavigationLabel().setToolTipText("Rules for " + relation.getID());                        
    }
    void updateRuleListModel(TreeSelectionEvent moduleSelectionEvent, 
                             TreeSelectionEvent conceptSelectionEvent,
                             TreeSelectionEvent relationSelectionEvent) throws Exception {
        //debugPrintln(3, "--->in updateRuleListModel.");
                                 
        //PLModule module = getSelectedModule(moduleSelectionEvent);
	PLModule module = getRuleViewModule();

        if (module == null) {
            //debugPrintln(3, "-->module is null!!");
            parent.clearRuleList();
            parent.getRulePanel().getPubNavigationLabel().setText("Rules");
            parent.getRulePanel().getPubNavigationLabel().setToolTipText("Rules");
            return;
        } 
        PLConcept concept = getSelectedConcept(conceptSelectionEvent); 
        PLRelation relation = getSelectedRelation(relationSelectionEvent);

        //debugPrintln(3, "-->concept=" + concept);
        //debugPrintln(3, "-->relation=" + relation);
        if (relation == null) {
            if (concept == null) {
                parent.clearRuleList();
                parent.getRulePanel().getPubNavigationLabel().setText("Rules");                                
                parent.getRulePanel().getPubNavigationLabel().setToolTipText("Rules");                                
            } else {
                updateRuleListModel(module, concept);
            }
        } else {
            updateRuleListModel(module, relation);
        }            
    }

    void updatePropositionModuleView(PLModule module) {
	parent.getPropositionPanel().getModuleViewSelectorPanel().setModule(module);
    }

    void updateRuleModuleView(PLModule module) {
	parent.getRulePanel().getModuleViewSelectorPanel().setModule(module);
    }

    void handleException(Throwable e) {
	PowerloomApp.getInstance().handleException(e);
    }
}
