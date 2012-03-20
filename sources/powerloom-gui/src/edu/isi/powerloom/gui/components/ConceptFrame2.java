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


// Version: ConceptFrame2.java,v 1.23 2010/02/04 05:16:51 hans Exp

package edu.isi.powerloom.gui.components;

import java.util.*;
import javax.swing.*;
import javax.swing.event.*;

import edu.isi.powerloom.gui.xmlobject.*;
import edu.isi.powerloom.gui.serverinterface.*;
import static edu.isi.powerloom.gui.common.Utils.*;

/**
 * Frame for concept editor.
 *
 * @since 4/1/2002 7:52:32 PM
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */
public class ConceptFrame2 extends PLFrame {
    private PowerloomApp app;
    private ConceptPanel2 ivjConceptPanel = null;
    private javax.swing.JPanel ivjJInternalFrameContentPane = null;
    private java.awt.BorderLayout ivjJInternalFrameContentPaneBorderLayout = null;
    private edu.isi.powerloom.gui.xmlobject.PLModule savedModule;
    private java.util.Collection editListeners = new ArrayList();
    private edu.isi.powerloom.gui.xmlobject.PLConcept savedConcept;
    /**
     * ConceptFrame2 constructor comment.
     */
    public ConceptFrame2() {
	super();
	initialize();
    }
    /**
     * ConceptFrame2 constructor comment.
     * @param title java.lang.String
     */
    public ConceptFrame2(String title) {
	super(title);
    }
    /**
     * ConceptFrame2 constructor comment.
     * @param title java.lang.String
     * @param resizable boolean
     */
    public ConceptFrame2(String title, boolean resizable) {
	super(title, resizable);
    }
    /**
     * ConceptFrame2 constructor comment.
     * @param title java.lang.String
     * @param resizable boolean
     * @param closable boolean
     */
    public ConceptFrame2(String title, boolean resizable, boolean closable) {
	super(title, resizable, closable);
    }
    /**
     * ConceptFrame2 constructor comment.
     * @param title java.lang.String
     * @param resizable boolean
     * @param closable boolean
     * @param maximizable boolean
     */
    public ConceptFrame2(String title, boolean resizable, boolean closable, boolean maximizable) {
	super(title, resizable, closable, maximizable);
    }
    /**
     * ConceptFrame2 constructor comment.
     * @param title java.lang.String
     * @param resizable boolean
     * @param closable boolean
     * @param maximizable boolean
     * @param iconifiable boolean
     */
    public ConceptFrame2(String title, boolean resizable, boolean closable, boolean maximizable, boolean iconifiable) {
	super(title, resizable, closable, maximizable, iconifiable);
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/12/2002 10:32:45 PM)
     * @param listener redesign.gui.components.PLEditListener
     */
    public void addPLEditListener(PLEditListener listener) {
	editListeners.add(listener);
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/10/2002 5:05:01 PM)
     */
    protected void commitConcept() {
	try {
	    String conceptDefinition = createConceptDefinition();
	    debugPrintln(3, "concept definition: " + conceptDefinition);
	    PLModule module = (PLModule)getConceptPanel().pubGetModuleComboBox().getSelectedItem();
	    KnowledgeManager.getInstance().evaluateLogicCommand(module, conceptDefinition);	    
	    // Get the new concept from the server
	    PLConcept concept = (PLConcept)KnowledgeManager.getInstance().getPLObject(module, getConceptPanel().getNameTextField().getText());
	    // save the concept so that the PLEditEvent will record the new concept,
	    // and the powerloom trees will be expanded to the new concept.
	    savedConcept = concept;
	    PLEditEvent event = new PLEditEvent(this, PLConcept.class, savedConcept);
	    fireEditPerformed(event);
	} catch (Exception e) {
	    handleException(e);
	} // end of try-catch
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/10/2002 5:02:25 PM)
     * @return java.lang.String
     */
    String createConceptDefinition() {
	String name = getConceptPanel().getNameTextField().getText();
	String documentation = getConceptPanel().getDocumentationTextArea().getText();
	StringBuffer supers = new StringBuffer();
	PLListModel model = (PLListModel)getConceptPanel().getSupersPanel().getJList().getModel();
	supers.append("(");
	for (int i = 0; i < model.getSize(); i++) {
	    String superCon = model.getElementAt(i).toString();
	    String superExpression = " " + superCon;
	    supers.append(superExpression);
	}
	supers.append(")");
	String result = "(DEFCONCEPT " + name + " " + supers.toString();
	if ((documentation != null) && (!documentation.equals(""))) {
	    result += " :DOCUMENTATION \"" + documentation + "\"";
	} 
	result += ")";
	return result;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/1/2002 7:58:15 PM)
     * @return redesign.gui.components.PowerloomApp
     */
    public PowerloomApp getApp() {
	return app;
    }
    /**
     * Return the ConceptPanel property value.
     * @return redesign.gui.components.ConceptPanel2
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    ConceptPanel2 getConceptPanel() {
	if (ivjConceptPanel == null) {
	    try {
		ivjConceptPanel = new edu.isi.powerloom.gui.components.ConceptPanel2(this);
		ivjConceptPanel.setName("ConceptPanel");
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
     * Return the JInternalFrameContentPane property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getJInternalFrameContentPane() {
	if (ivjJInternalFrameContentPane == null) {
	    try {
		ivjJInternalFrameContentPane = new javax.swing.JPanel();
		ivjJInternalFrameContentPane.setName("JInternalFrameContentPane");
		ivjJInternalFrameContentPane.setBorder(new javax.swing.border.EmptyBorder(10,10,10,10));
		ivjJInternalFrameContentPane.setLayout(getJInternalFrameContentPaneBorderLayout());
		getJInternalFrameContentPane().add(getConceptPanel(), "Center");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjJInternalFrameContentPane;
    }
    /**
     * Return the JInternalFrameContentPaneBorderLayout property value.
     * @return java.awt.BorderLayout
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private java.awt.BorderLayout getJInternalFrameContentPaneBorderLayout() {
	java.awt.BorderLayout ivjJInternalFrameContentPaneBorderLayout = null;
	try {
	    /* Create part */
	    ivjJInternalFrameContentPaneBorderLayout = new java.awt.BorderLayout();
	    ivjJInternalFrameContentPaneBorderLayout.setVgap(0);
	    ivjJInternalFrameContentPaneBorderLayout.setHgap(0);
	} catch (java.lang.Throwable ivjExc) {
	    handleException(ivjExc);
	};
	return ivjJInternalFrameContentPaneBorderLayout;
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
    protected void initialize() {
	try {
	    // user code begin {1}
	    // user code end
	    setName("ConceptFrame2");
	    setTitle("Editing Concept yyy");
	    setClosable(true);
	    setIconifiable(true);
	    setMaximizable(true);
	    setSize(600, 570);
	    setResizable(true);
	    setContentPane(getJInternalFrameContentPane());
	    app = PowerloomApp.getInstance();
	    getConceptPanel().postCreateInitialize();
	} catch (java.lang.Throwable ivjExc) {
	    handleException(ivjExc);
	}
	// user code begin {2}
	getConceptPanel().setParentFrame(this);
	// Ensure name field gets focus when the frame is opened
	final JTextField nameField = getConceptPanel().getNameTextField();
	InternalFrameListener frameListener = new InternalFrameAdapter() {
		public void internalFrameActivated(InternalFrameEvent e) {
		    JInternalFrame internalFrame = (JInternalFrame)e.getSource();
		    debugPrintln(3, "activated concept frame " + internalFrame.hashCode());
		    nameField.requestFocus();
		}
	    };
	addInternalFrameListener(frameListener);
	setTitle("New Concept");
	// user code end
    }

    public Icon getFrameIcon() {
	return PowerloomApp.getInstance().getImage("resources/images/edit-concept.gif");
    }
    /**
     * main entrypoint - starts the part when it is run as an application
     * @param args java.lang.String[]
     */
    public static void main(java.lang.String[] args) {
	try {
	    javax.swing.JFrame frame = new javax.swing.JFrame();
	    ConceptFrame2 aConceptFrame2;
	    aConceptFrame2 = new ConceptFrame2();
	    frame.setContentPane(aConceptFrame2);
	    frame.setSize(aConceptFrame2.getSize());
	    frame.addWindowListener(new java.awt.event.WindowAdapter() {
		    public void windowClosing(java.awt.event.WindowEvent e) {
			exitGui(0);
		    };
		});
	    frame.show();
	    java.awt.Insets insets = frame.getInsets();
	    frame.setSize(frame.getWidth() + insets.left + insets.right, frame.getHeight() + insets.top + insets.bottom);
	    frame.setVisible(true);
	} catch (Throwable exception) {
	    System.err.println("Exception occurred in main() of javax.swing.JInternalFrame");
	    exception.printStackTrace(System.out);
	}
    }

    /**
     * Insert the method's description here.
     * Creation date: (4/15/2002 10:16:10 PM)
     */
    public void refreshPropositionsPanel() {
	try {
	    PLSurrogateContainer propositions = 
		KnowledgeManager.getInstance().getPropositionsForConcept(savedModule, savedConcept);
	    setPropositionsList(propositions);
	} catch (Exception e) {
	    handleException(e);
	}
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/15/2002 10:16:10 PM)
     */
    public void refreshRelationsPanel() {
	try {
	    PLSurrogateContainer relations = 
		KnowledgeManager.getInstance().getRelationsForConcept(savedModule, savedConcept);
	    setRelationsList(relations);
	} catch (Exception e) {
	    handleException(e);
	}
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/15/2002 10:16:10 PM)
     */
    public void refreshRulePanel() {
	try {
	    PLSurrogateContainer rules = 
		KnowledgeManager.getInstance().getRulesForConcept(savedModule, savedConcept);
	    setRulesList(rules);
	} catch (Exception e) {
	    handleException(e);
	}
    }

    /**
     * Insert the method's description here.
     * Creation date: (4/9/2002 8:39:35 PM)
     * @param concept edu.isi.powerloom.gui.xmlobject.PLConcept
     */
    public void setConcept(PLModule module, PLConcept concept) {
	setConceptName(concept.getID());
	savedModule = module;
	savedConcept = concept;
	try {
	    String documentation = 
		KnowledgeManager.getInstance().getDocumentationForConcept(module, concept);
	    PLSurrogateContainer supers = 
		KnowledgeManager.getInstance().getSuperConceptsForConcept(module, concept);
	    refreshRelationsPanel();
	    PLSurrogateContainer propositions = 
		KnowledgeManager.getInstance().getPropositionsForConcept(module, concept);
	    PLSurrogateContainer rules = 
		KnowledgeManager.getInstance().getRulesForConcept(module, concept);
	    setSupersList(supers);
	    setPropositionsList(propositions);
	    setRulesList(rules);
	    setDocumentation(documentation);
	} catch (Exception e) {
	    handleException(e);
	} // end of try-catch
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/9/2002 8:43:22 PM)
     * @param name java.lang.String
     */
    private void setConceptName(String name) {
	getConceptPanel().getNameTextField().setText(name);
	setTitle("Editing Concept " + name);
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/10/2002 3:39:59 PM)
     * @param documentation java.lang.String
     */
    private void setDocumentation(String documentation) {
	getConceptPanel().getDocumentationTextArea().setText(documentation);
	getConceptPanel().getDocumentationTextArea().getCaret().setDot(0);    
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/12/2002 10:31:56 PM)
     * @param newEditListeners java.util.Collection
     */
    public void setEditListeners(java.util.Collection newEditListeners) {
	editListeners = newEditListeners;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/12/2002 2:50:19 PM)
     * @param props edu.isi.powerloom.gui.xmlobject.PLSurrogateContainer
     */
    private void setPropositionsList(PLSurrogateContainer props) {
	String[] types = {"Proposition Holder", "Object Holder"};
	PLListModel model = new PLListModel(getConceptPanel().getModule(), props);
	JList newList = new PLPropositionList(model, this, "Concept Editor Propositions List", Arrays.asList(types));

	installPropositionPopup(getConceptPanel().getModule(), newList);
	getConceptPanel().getPropositionsPanel().setList(newList);
    }

    private void installPropositionPopup(PLModule module, final javax.swing.JList list) {
	Collection listMenuItems = new ArrayList();
	Collection itemMenuItems = new ArrayList();
	Collection subObjectMenuItems = new ArrayList();
	itemMenuItems.add(EditObjectAction.class);
	subObjectMenuItems.add(NavigateAction.class);
	subObjectMenuItems.add(EditObjectAction.class);
	itemMenuItems.add(CopyAction.class);
	itemMenuItems.add(DeleteAction.class);
	listMenuItems.add(NewPropositionAction.class);
	PopupUtils.installListPopup(module, list, listMenuItems, itemMenuItems, null, subObjectMenuItems, null);
    }

    private void setRelationsList(PLSurrogateContainer relations) {
	String[] types = {"Relation Holder", "Object Holder"};
	PLListModel model = new PLListModel(getConceptPanel().getModule(), relations);
	JList newList = new DeletableObjectList(model, this, "Concept Editor Relations List", Arrays.asList(types));

	installRelationPopup(getConceptPanel().getModule(), newList);
	getConceptPanel().getRelationPanel().setList(newList);
    }

    /**
     * Insert the method's description here.
     * Creation date: (4/12/2002 2:51:00 PM)
     * @param rules edu.isi.powerloom.gui.xmlobject.PLSurrogateContainer
     */
    private void setRulesList(PLSurrogateContainer rules) {
	String[] types = {"Rule Holder", "Object Holder"};
	PLListModel model = new PLListModel(getConceptPanel().getModule(), rules);
	JList newList = new PLPropositionList(model, this, "Concept Editor Rules List", Arrays.asList(types));

	installRulePopup(getConceptPanel().getModule(), newList);
	getConceptPanel().getRulePanel().setList(newList);
    }

    private void installRelationPopup(PLModule module, final JList list) {
	Collection listMenuItems = new ArrayList();
	Collection listOnlyMenuItems = new ArrayList();
	Collection itemMenuItems = new ArrayList();
	itemMenuItems.add(EditObjectAction.class);
	itemMenuItems.add(NavigateAction.class);
	itemMenuItems.add(EditExtensionAction.class);
	itemMenuItems.add(CopyAction.class);
	itemMenuItems.add(DeleteAction.class);
	listMenuItems.add(NewRelationAction.class);
	PopupUtils.installListPopup(module, list, listMenuItems, itemMenuItems, listOnlyMenuItems, null, null);
    }


    private void installConceptPopup(PLModule module, final JList list) {
	Collection listMenuItems = new ArrayList();
	Collection itemMenuItems = new ArrayList();
	Collection listOnlyMenuItems = new ArrayList();
	itemMenuItems.add(EditObjectAction.class);
	itemMenuItems.add(NavigateAction.class);
	itemMenuItems.add(AddObjectAction.class);
	itemMenuItems.add(InstantiateAction.class);	
	itemMenuItems.add(EditExtensionAction.class);
	itemMenuItems.add(CutAction.class);
	itemMenuItems.add(CopyAction.class);
	itemMenuItems.add(DeleteAction.class);
	listMenuItems.add(PasteAction.class);
	PopupUtils.installListPopup(module, list, listMenuItems, itemMenuItems, listOnlyMenuItems, null, null);
    }

    private void installRulePopup(PLModule module, final JList list) {
	Collection listMenuItems = new ArrayList();
	Collection itemMenuItems = new ArrayList();
	Collection subObjectMenuItems = new ArrayList();
	itemMenuItems.add(EditObjectAction.class);
	itemMenuItems.add(CopyAction.class);
	itemMenuItems.add(DeleteAction.class);
	listMenuItems.add(NewRuleAction.class);
	subObjectMenuItems.add(NavigateAction.class);
	PopupUtils.installListPopup(module, list, listMenuItems, itemMenuItems, null, subObjectMenuItems, null);
    }


    /**
     * Insert the method's description here.
     * Creation date: (4/9/2002 8:33:54 PM)
     */
    private void setSupersList(PLSurrogateContainer supers) {
	debugPrintln(3, "setting supers to " + supers.getSurrogates());
	PLListModel model = new PLListModel(getConceptPanel().getModule(), supers);
	String[] types = {"Concept Holder", "Object Holder"};
	JList newList = new PLJList(model, this, "Concept Editor Supers List", Arrays.asList(types));

	installConceptPopup(getConceptPanel().getModule(), newList);
	getConceptPanel().getSupersPanel().setList(newList);
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/29/2002 12:53:41 PM)
     * @param name java.lang.String
     * @param supers edu.isi.powerloom.gui.xmlobject.PLSurrogateContainer
     */
    public void setupNewConcept(String name, PLSurrogateContainer supers) {
	setConceptName(name);
	getConceptPanel().getNameTextField().selectAll();
	setSupersList(supers);
    }

    /**
     * Implementation of PLFrame
     */
    public PLModule getModule() {
	return getConceptPanel().getModule();
    }

    public void performEdit(PLEditEvent e) {
	if (e.getEditType() == PLRelation.class) {
	    refreshRelationsPanel();
	}
	if (e.getEditType() == PLProposition.class) {
	    refreshPropositionsPanel();
	    refreshRulePanel();
	}
    }
}
