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


// Version: BrowserPanel4.java,v 1.14 2010/02/04 05:16:44 hans Exp

package edu.isi.powerloom.gui.components;

import javax.swing.*;
import java.awt.event.*;

/**
 * Panel contained inside the BrowserFrame.  Contains all subpanes used for
 * navigation.
 * 
 * @see edu.isi.powerloom.gui.components.BrowserFrame4 BrowserFrame4
 * @see edu.isi.powerloom.gui.components.NavigationPanel3 NavigationPanel3
 * @see edu.isi.powerloom.gui.components.PowerloomTrees PowerloomTrees
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */
public class BrowserPanel4 extends javax.swing.JPanel {
    private NavigationPanel3 ivjConceptPanel = null;
    private NavigationPanel3 ivjModulePanel = null;
    private javax.swing.JSplitPane ivjPropRuleSplitPane = null;
    private javax.swing.JSplitPane ivjRelationInstanceSplitPane = null;
    private NavigationPanel3 ivjRulePanel = null;
    private javax.swing.JSplitPane ivjMCRISplitPane = null;
    private NavigationPanel3 ivjInstancePanel = null;
    private javax.swing.JSplitPane ivjModuleConceptSplitPane = null;
    private NavigationPanel3 ivjPropositionPanel = null;
    private NavigationPanel3 ivjRelationPanel = null;
    private javax.swing.JSplitPane ivjMCRIPRSplitPane = null;
    private javax.swing.JTree moduleTree;
    private javax.swing.JTree conceptTree;
    private javax.swing.JTree relationTree;
    private javax.swing.JList instanceList;
    private javax.swing.JList propositionList;
    private JList ruleList;
    private BrowserFrame4 parentFrame;
    public PowerloomTrees powerloomTrees;
    /**
     * BrowserPanel4 constructor comment.
     */
    public BrowserPanel4(BrowserFrame4 parent) {
	super();
	setParentFrame(parent);
	initialize();
    }
    /**
     * BrowserPanel4 constructor comment.
     * @param layout java.awt.LayoutManager
     */
    public BrowserPanel4(java.awt.LayoutManager layout) {
	super(layout);
    }
    /**
     * BrowserPanel4 constructor comment.
     * @param layout java.awt.LayoutManager
     * @param isDoubleBuffered boolean
     */
    public BrowserPanel4(java.awt.LayoutManager layout, boolean isDoubleBuffered) {
	super(layout, isDoubleBuffered);
    }
    /**
     * BrowserPanel4 constructor comment.
     * @param isDoubleBuffered boolean
     */
    public BrowserPanel4(boolean isDoubleBuffered) {
	super(isDoubleBuffered);
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/8/2002 4:22:04 PM)
     */
    public void clearConceptTree() {
	getConceptPanel().getNavigationScrollPane().setViewportView(new javax.swing.JList());
	setConceptTree(null);
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/8/2002 4:23:11 PM)
     */
    public void clearInstanceList() {
	getInstancePanel().getNavigationScrollPane().setViewportView(new javax.swing.JList());
	setInstanceList(null);
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/8/2002 4:08:52 PM)
     */
    public void clearModuleTree() {
	getModulePanel().getNavigationScrollPane().setViewportView(new javax.swing.JList());
	setModuleTree(null);
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/8/2002 4:23:41 PM)
     */
    public void clearPropositionList() {
	getPropositionPanel().getNavigationScrollPane().setViewportView(new javax.swing.JList());
	setPropositionList(null);
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/8/2002 4:22:38 PM)
     */
    public void clearRelationTree() {
	getRelationPanel().getNavigationScrollPane().setViewportView(new javax.swing.JList());
	setRelationTree(null);
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/8/2002 4:24:45 PM)
     */
    public void clearRuleList() {
	getRulePanel().getNavigationScrollPane().setViewportView(new javax.swing.JList());
	setRuleList(null);
    }
    /**
     * Return the ConceptPanel property value.
     * @return redesign.gui.components.NavigationPanel3
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    public NavigationPanel3 getConceptPanel() {
	if (ivjConceptPanel == null) {
	    try {
		ivjConceptPanel = new edu.isi.powerloom.gui.components.NavigationPanel3(new NewConceptAction());
		ivjConceptPanel.setName("ConceptPanel");
		ivjConceptPanel.setTitle("Concepts");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjConceptPanel;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/8/2002 4:15:44 PM)
     * @return javax.swing.JTree
     */
    public javax.swing.JTree getConceptTree() {
	return conceptTree;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/8/2002 4:16:14 PM)
     * @return javax.swing.JList
     */
    public javax.swing.JList getInstanceList() {
	return instanceList;
    }
    /**
     * Return the InstanceSplitPane property value.
     * @return redesign.gui.components.NavigationPanel3
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    public NavigationPanel3 getInstancePanel() {
	if (ivjInstancePanel == null) {
	    try {
		ivjInstancePanel = new edu.isi.powerloom.gui.components.NavigationPanel3(new NewInstanceAction());
		ivjInstancePanel.setName("InstancePanel");
		ivjInstancePanel.setTitle("Instances");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjInstancePanel;
    }
    /**
     * Return the MCRIRPSplitPane property value.
     * @return javax.swing.JSplitPane
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    public javax.swing.JSplitPane getMCRIPRSplitPane() {
	if (ivjMCRIPRSplitPane == null) {
	    try {
		ivjMCRIPRSplitPane = new javax.swing.JSplitPane(javax.swing.JSplitPane.VERTICAL_SPLIT);
		ivjMCRIPRSplitPane.setName("MCRIPRSplitPane");
		ivjMCRIPRSplitPane.setDividerLocation(300);
		ivjMCRIPRSplitPane.setOneTouchExpandable(true);
		getMCRIPRSplitPane().add(getMCRISplitPane(), "top");
		getMCRIPRSplitPane().add(getPropRuleSplitPane(), "bottom");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjMCRIPRSplitPane;
    }
    /**
     * Return the ModuleCRISplitPane property value.
     * @return javax.swing.JSplitPane
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    public javax.swing.JSplitPane getMCRISplitPane() {
	if (ivjMCRISplitPane == null) {
	    try {
		ivjMCRISplitPane = new javax.swing.JSplitPane(javax.swing.JSplitPane.HORIZONTAL_SPLIT);
		ivjMCRISplitPane.setName("MCRISplitPane");
		ivjMCRISplitPane.setOneTouchExpandable(true);
		ivjMCRISplitPane.setDividerLocation(350);
		getMCRISplitPane().add(getModuleConceptSplitPane(), "left");
		getMCRISplitPane().add(getRelationInstanceSplitPane(), "right");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjMCRISplitPane;
    }
    /**
     * Return the ConceptRISplitPane property value.
     * @return javax.swing.JSplitPane
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    public javax.swing.JSplitPane getModuleConceptSplitPane() {
	if (ivjModuleConceptSplitPane == null) {
	    try {
		ivjModuleConceptSplitPane = new javax.swing.JSplitPane(javax.swing.JSplitPane.HORIZONTAL_SPLIT);
		ivjModuleConceptSplitPane.setName("ModuleConceptSplitPane");
		ivjModuleConceptSplitPane.setOneTouchExpandable(true);
		ivjModuleConceptSplitPane.setDividerLocation(150);
		getModuleConceptSplitPane().add(getModulePanel(), "left");
		getModuleConceptSplitPane().add(getConceptPanel(), "right");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjModuleConceptSplitPane;
    }
    /**
     * Return the ModulePanel property value.
     * @return redesign.gui.components.NavigationPanel3
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    public NavigationPanel3 getModulePanel() {
	if (ivjModulePanel == null) {
	    try {
		ivjModulePanel = new edu.isi.powerloom.gui.components.NavigationPanel3(new NewModuleAction());
		ivjModulePanel.setName("ModulePanel");
		ivjModulePanel.setTitle("Modules");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjModulePanel;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/8/2002 4:05:05 PM)
     * @return javax.swing.JTree
     */
    public javax.swing.JTree getModuleTree() {
	return moduleTree;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/8/2002 4:58:24 PM)
     * @return redesign.gui.components.BrowserFrame4
     */
    public BrowserFrame4 getParentFrame() {
	return parentFrame;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/10/2002 6:58:40 PM)
     * @return redesign.gui.components.PowerloomTrees
     */
    public PowerloomTrees getPowerloomTrees() {
	return powerloomTrees;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/8/2002 4:16:34 PM)
     * @return javax.swing.JList
     */
    public javax.swing.JList getPropositionList() {
	return propositionList;
    }
    /**
     * Return the PropPanel property value.
     * @return redesign.gui.components.NavigationPanel3
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    public NavigationPanel3 getPropositionPanel() {
	if (ivjPropositionPanel == null) {
	    try {
		ivjPropositionPanel = new edu.isi.powerloom.gui.components.NavigationPanel3(true, new NewPropositionAction());
		ivjPropositionPanel.setName("PropositionPanel");
		ivjPropositionPanel.setTitle("Propositions");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjPropositionPanel;
    }
    /**
     * Return the PropRuleSplitPane property value.
     * @return javax.swing.JSplitPane
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    public javax.swing.JSplitPane getPropRuleSplitPane() {
	if (ivjPropRuleSplitPane == null) {
	    try {
		ivjPropRuleSplitPane = new javax.swing.JSplitPane(javax.swing.JSplitPane.VERTICAL_SPLIT);
		ivjPropRuleSplitPane.setName("PropRuleSplitPane");
		ivjPropRuleSplitPane.setOneTouchExpandable(true);
		ivjPropRuleSplitPane.setDividerLocation(200);
		getPropRuleSplitPane().add(getPropositionPanel(), "top");
		getPropRuleSplitPane().add(getRulePanel(), "bottom");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjPropRuleSplitPane;
    }
    /**
     * Return the RelationInstanceSplitPane property value.
     * @return javax.swing.JSplitPane
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    public javax.swing.JSplitPane getRelationInstanceSplitPane() {
	if (ivjRelationInstanceSplitPane == null) {
	    try {
		ivjRelationInstanceSplitPane = new javax.swing.JSplitPane(javax.swing.JSplitPane.HORIZONTAL_SPLIT);
		ivjRelationInstanceSplitPane.setName("RelationInstanceSplitPane");
		ivjRelationInstanceSplitPane.setOneTouchExpandable(true);
		ivjRelationInstanceSplitPane.setDividerLocation(200);
		getRelationInstanceSplitPane().add(getRelationPanel(), "left");
		getRelationInstanceSplitPane().add(getInstancePanel(), "right");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjRelationInstanceSplitPane;
    }
    /**
     * Return the RelationSplitPane property value.
     * @return redesign.gui.components.NavigationPanel3
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    public NavigationPanel3 getRelationPanel() {
	if (ivjRelationPanel == null) {
	    try {
		ivjRelationPanel = new edu.isi.powerloom.gui.components.NavigationPanel3(new NewRelationAction());
		ivjRelationPanel.setName("RelationPanel");
		ivjRelationPanel.setTitle("Relations");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjRelationPanel;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/8/2002 4:16:02 PM)
     * @return javax.swing.JTree
     */
    public javax.swing.JTree getRelationTree() {
	return relationTree;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/8/2002 4:16:50 PM)
     * @return int
     */
    public JList getRuleList() {
	return ruleList;
    }
    /**
     * Return the RulePanel property value.
     * @return redesign.gui.components.NavigationPanel3
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    public NavigationPanel3 getRulePanel() {
	if (ivjRulePanel == null) {
	    try {
		ivjRulePanel = new edu.isi.powerloom.gui.components.NavigationPanel3(true, new NewRuleAction());
		ivjRulePanel.setName("RulePanel");
		ivjRulePanel.setTitle("Rules");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjRulePanel;
    }
    /**
     * Called whenever the part throws an exception.
     * @param exception java.lang.Throwable
     */
    private void handleException(java.lang.Throwable exception) {
	PowerloomApp.getInstance().handleException(exception);
    }
    /**
     * Initialize the class.
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void initialize() {
	try {
	    // user code begin {1}
	    // user code end
	    setName("BrowserPanel4");
	    setLayout(new java.awt.BorderLayout());
	    setSize(800, 500);
	    add(getMCRIPRSplitPane(), "Center");
	} catch (java.lang.Throwable ivjExc) {
	    handleException(ivjExc);
	}
	// user code begin {2}
	try {
	    powerloomTrees = new PowerloomTrees(this);
	    // disable unused buttons
	    getPropositionPanel().getSearchButton().setEnabled(false);
	    getRulePanel().getSearchButton().setEnabled(false);
	    // bsetup listeners for active buttons
	    getModulePanel().getSearchButton().addActionListener(new ActionListener() {
                    public void actionPerformed(ActionEvent e) {
                        SearchFrame searchFrame = new SearchFrame(getParentFrame(), true, true, true);
			searchFrame.displayFrame();
                    }
                });
	    getConceptPanel().getSearchButton().addActionListener(new ActionListener() {
                    public void actionPerformed(ActionEvent e) {
                        SearchFrame searchFrame = new SearchFrame(getParentFrame(), true, false, false);
			searchFrame.displayFrame();
                    }
                });
	    getRelationPanel().getSearchButton().addActionListener(new ActionListener() {
                    public void actionPerformed(ActionEvent e) {
                        SearchFrame searchFrame = new SearchFrame(getParentFrame(), false, true, false);
			searchFrame.displayFrame();
                    }
                });
	    getInstancePanel().getSearchButton().addActionListener(new ActionListener() {
                    public void actionPerformed(ActionEvent e) {
                        SearchFrame searchFrame = new SearchFrame(getParentFrame(), false, false, true);
			searchFrame.displayFrame();
                    }
                });
	    getConceptPanel().getSearchButton().setToolTipText("Search Concepts");
	    getRelationPanel().getSearchButton().setToolTipText("Search Relations");
	    getInstancePanel().getSearchButton().setToolTipText("Search Instances");
	} catch (Exception e) {
	    PowerloomApp.getInstance().handleException(e);
	}
	// user code end
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/8/2002 4:15:44 PM)
     * @param newConceptTree javax.swing.JTree
     */
    public void setConceptTree(javax.swing.JTree newConceptTree) {
	conceptTree = newConceptTree;
        if (conceptTree != null) {
            getConceptPanel().getNavigationScrollPane().setViewportView(conceptTree);
        }		
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/8/2002 4:16:14 PM)
     * @param newInstanceList javax.swing.JList
     */
    public void setInstanceList(javax.swing.JList newInstanceList) {
	instanceList = newInstanceList;
        if (instanceList != null) {
            getInstancePanel().getNavigationScrollPane().setViewportView(instanceList);
        }		
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/8/2002 4:05:05 PM)
     * @param newModuleTree javax.swing.JTree
     */
    public void setModuleTree(javax.swing.JTree newModuleTree) {
	moduleTree = newModuleTree;
        if (moduleTree != null) {
            getModulePanel().getNavigationScrollPane().setViewportView(moduleTree);
        }	
	
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/8/2002 4:58:24 PM)
     * @param newParentFrame redesign.gui.components.BrowserFrame4
     */
    public void setParentFrame(BrowserFrame4 newParentFrame) {
	parentFrame = newParentFrame;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/10/2002 6:58:40 PM)
     * @param newPowerloomTrees redesign.gui.components.PowerloomTrees
     */
    public void setPowerloomTrees(PowerloomTrees newPowerloomTrees) {
	powerloomTrees = newPowerloomTrees;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/8/2002 4:16:34 PM)
     * @param newPropositionList javax.swing.JList
     */
    public void setPropositionList(javax.swing.JList newPropositionList) {
	propositionList = newPropositionList;
        if (propositionList != null) {
            getPropositionPanel().getNavigationScrollPane().setViewportView(propositionList);
        }		
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/8/2002 4:16:02 PM)
     * @param newRelationTree javax.swing.JTree
     */
    public void setRelationTree(javax.swing.JTree newRelationTree) {
	relationTree = newRelationTree;
        if (relationTree != null) {
            getRelationPanel().getNavigationScrollPane().setViewportView(relationTree);
        }		
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/8/2002 4:16:50 PM)
     * @param newRuleList int
     */
    public void setRuleList(JList newRuleList) {
	ruleList = newRuleList;
	if (ruleList != null) {
	    getRulePanel().getNavigationScrollPane().setViewportView(ruleList);
	}		
    }
}
