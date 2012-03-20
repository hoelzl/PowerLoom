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


// Version: RelationFrame.java,v 1.17 2010/02/04 05:19:15 hans Exp

package edu.isi.powerloom.gui.components;

import java.util.*;
import java.awt.*;
import javax.swing.*;
import javax.swing.event.*;
import edu.isi.powerloom.gui.xmlobject.*;
import edu.isi.powerloom.gui.serverinterface.*;
import static edu.isi.powerloom.gui.common.Utils.*;

/**
 * Frame for editing relations
 *
 * @since 4/1/2002 7:52:32 PM
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 * @see edu.isi.powerloom.gui.components.RelationPanel RelationPanel
 */
public class RelationFrame extends PLFrame implements PLEditListener {
    private PowerloomApp app;
    private javax.swing.JPanel ivjJInternalFrameContentPane = null;
    private java.awt.BorderLayout ivjJInternalFrameContentPaneBorderLayout = null;
    private RelationPanel ivjRelationPanel = null;
    private java.util.Collection editListeners = new java.util.ArrayList();
    private edu.isi.powerloom.gui.xmlobject.PLModule savedModule;
    private edu.isi.powerloom.gui.xmlobject.PLRelation savedRelation;
    private int currentVariableCounter = 0;
    private String variablePrefix = "?var";
    /**
     * ConceptFrame2 constructor comment.
     */
    public RelationFrame() {
	super();
	initialize();
    }
    /**
     * ConceptFrame2 constructor comment.
     * @param title java.lang.String
     */
    public RelationFrame(String title) {
	super(title);
    }
    /**
     * ConceptFrame2 constructor comment.
     * @param title java.lang.String
     * @param resizable boolean
     */
    public RelationFrame(String title, boolean resizable) {
	super(title, resizable);
    }
    /**
     * ConceptFrame2 constructor comment.
     * @param title java.lang.String
     * @param resizable boolean
     * @param closable boolean
     */
    public RelationFrame(String title, boolean resizable, boolean closable) {
	super(title, resizable, closable);
    }
    /**
     * ConceptFrame2 constructor comment.
     * @param title java.lang.String
     * @param resizable boolean
     * @param closable boolean
     * @param maximizable boolean
     */
    public RelationFrame(String title, boolean resizable, boolean closable, boolean maximizable) {
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
    public RelationFrame(String title, boolean resizable, boolean closable, boolean maximizable, boolean iconifiable) {
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
    void commitRelation() {
	try {
	    String relationDefinition = createRelationDefinition();
	    debugPrintln(3, "relation definition: " + relationDefinition);
	    PLModule mod = (PLModule)getRelationPanel().getModuleComboBox().getSelectedItem();
	    if (mod == null) {
		//throw exception here
		debugPrintln(3, "** ERROR: YOU MUST SET THE MODULE BEFORE COMMITING THE RELATION");
		return;
	    }
	    KnowledgeManager.getInstance().evaluateLogicCommand(mod, relationDefinition);	    
	    KnowledgeManager.getInstance().invalidateRelationCaches();
	    PowerloomApp.getInstance().fireEditPerformed(new PLEditEvent(this, PLRelation.class));
	} catch (Exception e) {
	    handleException(e);
	} // end of try-catch
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/10/2002 5:02:25 PM)
     * @return java.lang.String
     */
    String createRelationDefinition() {
	String name = getRelationPanel().getNameTextField().getText();
	String documentation = getRelationPanel().getDocumentationTextArea().getText();
	boolean isFunction = getRelationPanel().getFunctionCheckBox().isSelected();
	boolean isClosed = getRelationPanel().getClosedCheckbox().isSelected();
	StringBuffer domainArguments = new StringBuffer("");
	javax.swing.table.TableModel tableModel = getRelationPanel().getArgumentsPanel().getTable().getModel();
	domainArguments.append("(");
	int argumentCount = isFunction ? tableModel.getRowCount() - 1 : tableModel.getRowCount();
	for (int i = 0; i < argumentCount; i++) {
	    String variable = (String)tableModel.getValueAt(i, 0);
	    String type = (String)tableModel.getValueAt(i, 1);
	    if (type != null) {
		domainArguments.append("(" + variable + " " + type + ") ");
	    } else {
		domainArguments.append(variable + " ");
	    }
	}
	domainArguments.append(")");
	String rangeArgument = null;
	if (isFunction) {
	    int lastRow = tableModel.getRowCount() - 1;
	    String variable = (String)tableModel.getValueAt(lastRow, 0);
	    rangeArgument = "(" + variable;
	    String type = (String)tableModel.getValueAt(lastRow, 1);
	    if (type != null) {
		rangeArgument += " " + type;
	    }
	    rangeArgument += ")";
	}
	StringBuffer supers = new StringBuffer("");
	PLListModel model = (PLListModel)getRelationPanel().getSupersPanel().getJList().getModel();
	if (model.getSize() > 0) {
	    supers.append(":SUPERS (");
	    for (int i = 0; i < model.getSize(); i++) {
		String superCon = model.getElementAt(i).toString();
		String superExpression = "(?p" + i + " " + superCon + ")";
		supers.append(superExpression);
	    }
	    supers.append(")");
	}
	String axioms = "";
	if (isClosed) {
	    axioms += "(CLOSED " + name + ")";
	}
	String result = "";
	if (isFunction) {
	    result += "(DEFFUNCTION " + name + " " + domainArguments + " :-> " + rangeArgument;
	} else {
	    result = "(DEFRELATION " + name + " " + domainArguments;
	}
	// ignore supers for now....
	// result += " " + supers;
	if ((documentation != null) && (!documentation.equals(""))) {
	    result += " :DOCUMENTATION \"" + documentation + "\"";
	} 
	if (axioms.length() > 0) {
	    result += " :AXIOMS " + axioms;
	} 
	result += ")";
	return result;
    }

    private java.util.List collectVariables() {
	java.util.List result = new ArrayList();
	javax.swing.table.TableModel tableModel = getRelationPanel().getArgumentsPanel().getTable().getModel();
	int argumentCount = tableModel.getRowCount();
	for (int i = 0; i < argumentCount; i++) {
	    String variable = (String)tableModel.getValueAt(i, 0);
	    result.add(variable);
	}
	return result;
    }

    private String generateVariable() {
	String result = variablePrefix + currentVariableCounter++;
	return result;
    }

    public String getUniqueVariable() {
	java.util.List existingVariables = collectVariables();
	String candidate = generateVariable();
	while (existingVariables.contains(candidate)) {
	    candidate = generateVariable();
	}
	return candidate;
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
     * Insert the method's description here.
     * Creation date: (4/13/2002 12:10:49 PM)
     * @return java.util.Collection
     */
    public java.util.Collection getEditListeners() {
	return editListeners;
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
		getJInternalFrameContentPane().add(getRelationPanel(), "Center");
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
     * Insert the method's description here.
     * Creation date: (4/22/2002 8:50:16 PM)
     * @return edu.isi.powerloom.gui.xmlobject.PLModule
     */
    public PLModule getModule() {
	return (PLModule)getRelationPanel().getPubModuleComboBox().getSelectedItem();
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/22/2002 8:45:21 PM)
     * @return edu.isi.powerloom.gui.xmlobject.PLRelation
     */
    public PLRelation getRelation() {
	try {
	    String relationName = getRelationPanel().getNameTextField().getText();
	    PLSurrogate relationSurrogate = KnowledgeManager.getInstance().findOrCreateSurrogate(PLRelation.class, relationName);
	    PLRelation relation = (PLRelation)relationSurrogate.getValue();
	    if (relation == null) {
		PLRelation newRelation = new PLRelation();
		newRelation.attrRelationName = relationName;
		newRelation.setUndefined(true);
		relationSurrogate.setValue(newRelation);
	    }
	    return (PLRelation)relationSurrogate.getValue();
	} catch (Exception e) {
	    handleException(e);
	}
	return null;
    }
    /**
     * Return the RelationPanel property value.
     * @return redesign.gui.components.RelationPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private RelationPanel getRelationPanel() {
	if (ivjRelationPanel == null) {
	    try {
		ivjRelationPanel = new edu.isi.powerloom.gui.components.RelationPanel(this);
		ivjRelationPanel.setName("RelationPanel");
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
	    setName("ConceptFrame2");
	    setResizable(true);
	    setClosable(true);
	    setMaximizable(true);
	    setIconifiable(true);
	    setSize(600, 570);
	    setTitle("Editing Relation rrr");
	    setContentPane(getJInternalFrameContentPane());
	    app = PowerloomApp.getInstance();
	    getRelationPanel().postCreateInitialize();

	    // Ensure name field gets focus when the frame is opened
	    final JTextField nameField = getRelationPanel().getNameTextField();
	    InternalFrameListener frameListener = new InternalFrameAdapter() {
		    public void internalFrameActivated(InternalFrameEvent e) {
			JInternalFrame internalFrame = (JInternalFrame)e.getSource();
			debugPrintln(3, "activated relation frame " + internalFrame.hashCode());
			nameField.requestFocus();
		    }
		};
	    addInternalFrameListener(frameListener);


	} catch (java.lang.Throwable ivjExc) {
	    handleException(ivjExc);
	}
	// user code begin {2}
	getRelationPanel().setParentFrame(this);
	setTitle("New Relation");
	// user code end
    }

    public Icon getFrameIcon() {
	return PowerloomApp.getInstance().getImage("resources/images/edit-relation.gif");
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
     * Creation date: (4/15/2002 10:13:34 PM)
     * @param e redesign.gui.components.PLEditEvent
     */
    public void performEdit(PLEditEvent e) {
	if (e.getEditType() == PLProposition.class) {
	    refreshPropositionsPanel();
	    refreshRulePanel();
	}
    }

    /**
     * Insert the method's description here.
     * Creation date: (4/15/2002 10:16:10 PM)
     */
    public void refreshPropositionsPanel() {
	if ((getRelation() == null) || (getModule() == null))
	    return;
	try {
	    PLSurrogateContainer propositions = 
		KnowledgeManager.getInstance().getPropositionsForRelation(getModule(), getRelation());
	    setPropositionsList(propositions);
	} catch (Exception e) {
	    handleException(e);
	}
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/15/2002 10:16:10 PM)
     */
    public void refreshRulePanel() {
	if ((getRelation() == null) || (getModule() == null))
	    return;
	try {
	    PLSurrogateContainer rules = 
		KnowledgeManager.getInstance().getRulesForRelation(getModule(), getRelation());
	    setRulesList(rules);
	} catch (Exception e) {
	    handleException(e);
	}
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/1/2002 7:58:15 PM)
     * @param newApp redesign.gui.components.PowerloomApp
     */
    public void setApp(PowerloomApp newApp) {
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/15/2002 5:16:31 PM)
     * @param isFunction boolean
     * @param isClosed boolean
     */
    private void setCheckboxes(boolean isFunction, boolean isClosed) {
	getRelationPanel().getFunctionCheckBox().setSelected(isFunction);
	getRelationPanel().getClosedCheckbox().setSelected(isClosed);
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/10/2002 3:39:59 PM)
     * @param documentation java.lang.String
     */
    private void setDocumentation(String documentation) {
	getRelationPanel().getDocumentationTextArea().setText(documentation);
	getRelationPanel().getDocumentationTextArea().getCaret().setDot(0);
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/13/2002 12:10:49 PM)
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
	PLListModel model = new PLListModel(getRelationPanel().getModule(), props);
	JList newList = new PLPropositionList(model, this, "Relation Editor Propositions List", Arrays.asList(types));

	installPropositionPopup(getRelationPanel().getModule(), newList);
	getRelationPanel().getPropositionsPanel().setList(newList);
    }

    private void installPropositionPopup(PLModule module, final javax.swing.JList list) {
	Collection listMenuItems = new ArrayList();
	Collection itemMenuItems = new ArrayList();
	Collection subObjectMenuItems = new ArrayList();
	itemMenuItems.add(EditObjectAction.class);
	itemMenuItems.add(CopyAction.class);
	itemMenuItems.add(DeleteAction.class);
	subObjectMenuItems.add(NavigateAction.class);
	subObjectMenuItems.add(EditObjectAction.class);
	listMenuItems.add(NewPropositionAction.class);
	PopupUtils.installListPopup(module, list, listMenuItems, itemMenuItems, null, subObjectMenuItems, null);
    }

    private void installRulePopup(PLModule module, final javax.swing.JList list) {
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
     * Creation date: (4/9/2002 8:39:35 PM)
     * @param concept edu.isi.powerloom.gui.xmlobject.PLConcept
     */
    public void setRelation(PLModule module, PLRelation relation) {
	setRelationName(relation.getID());
	savedModule = module;
	savedRelation = relation;
	try {
	    String documentation = 
		KnowledgeManager.getInstance().getDocumentationForRelation(module, relation);
	    PLSurrogateContainer supers = 
		KnowledgeManager.getInstance().getSuperRelationsForRelation(module, relation);
	    PLVariableList variables = KnowledgeManager.getInstance().getVariablesForRelation(module, relation);
	    PLSurrogateContainer propositions = 
		KnowledgeManager.getInstance().getPropositionsForRelation(module, relation);
	    PLSurrogateContainer rules = 
		KnowledgeManager.getInstance().getRulesForRelation(module, relation);
	    boolean isFunction = relation.attrIsFunction.equals("TRUE");
	    boolean isClosed = relation.attrIsClosed.equals("TRUE");
	    setSupersList(supers);
	    setVariablesList(variables);
	    setPropositionsList(propositions);
	    setRulesList(rules);
	    setDocumentation(documentation);
	    setCheckboxes(isFunction, isClosed);
	} catch (Exception e) {
	    handleException(e);
	} // end of try-catch
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/9/2002 8:43:22 PM)
     * @param name java.lang.String
     */
    private void setRelationName(String name) {
	getRelationPanel().getNameTextField().setText(name);
	setTitle("Editing Relation " + name);
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/12/2002 2:51:00 PM)
     * @param rules edu.isi.powerloom.gui.xmlobject.PLSurrogateContainer
     */
    private void setRulesList(PLSurrogateContainer rules) {
	String[] types = {"Rule Holder", "Object Holder"};
	PLListModel model = new PLListModel(getRelationPanel().getModule(), rules);
	JList newList = new PLPropositionList(model, this, "Relation Editor Rules List", Arrays.asList(types));

	installRulePopup(getRelationPanel().getModule(), newList);
	getRelationPanel().getRulePanel().setList(newList);
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/9/2002 8:33:54 PM)
     */
    private void setSupersList(PLSurrogateContainer supers) {
	debugPrintln(3, "setting supers to " + supers.getSurrogates());
	PLListModel model = new PLListModel(getRelationPanel().getModule(), supers);
	String[] types = {"Relation Holder", "Object Holder"};
	JList newList = new PLJList(model, this, "Relation Editor Supers List", Arrays.asList(types));
	getRelationPanel().getSupersPanel().setList(newList);
	// for now, disable super add/delete buttons, because, I don't know an easy
	// way to redefine a relation when its "supers" have changed: these are normally
	// part of the relations definition, which is a logical expression that is hard
	// to untangle.
	getRelationPanel().getSupersPanel().getAddToolBarButton().setEnabled(false);
	getRelationPanel().getSupersPanel().getDeleteToolBarButton().setEnabled(false);
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/19/2002 11:00:36 PM)
     * @param relName java.lang.String
     * @param domainVar java.lang.String
     * @param domainType java.lang.String
     */
    public void setupNewRelation(String relName, String domainVar, String domainType) {
	setTitle("New Relation");
	// disable prop and rule-adder button, since currently we can't safely add a prop
	// or rule until the relation is defined (get-propopsitions/rules-for-rel crashes)
	getRelationPanel().getPropositionsPanel().getAddToolBarButton().setEnabled(false);
	getRelationPanel().getPropositionsPanel().getDeleteToolBarButton().setEnabled(false);
	getRelationPanel().getRulePanel().getAddToolBarButton().setEnabled(false);
	getRelationPanel().getRulePanel().getDeleteToolBarButton().setEnabled(false);
	if (relName != null) {
	    getRelationPanel().getNameTextField().setText(relName);
	    getRelationPanel().getNameTextField().selectAll();	    
	}
	if ((domainVar != null) && (domainType != null)) {
	    PLVariable variable = new PLVariable();
	    variable.elemPLString = new PLString(domainVar);
	    variable.elemPLSurrogate = new PLSurrogate(domainType);
	    PLVariableList varList = new PLVariableList();
	    varList.elemPLVariable = new ArrayList();
	    varList.elemPLVariable.add(variable);
	    setVariablesList(varList);
	}
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/15/2002 2:49:50 PM)
     * @param variables edu.isi.powerloom.gui.xmlobject.PLVariableList
     */
    private void setVariablesList(PLVariableList variables) {
	javax.swing.table.DefaultTableModel tableModel = 
	    (javax.swing.table.DefaultTableModel)getRelationPanel().getArgumentsPanel().getTable().getModel();
	Iterator iter = variables.elemPLVariable.iterator();
	while (iter.hasNext()) {
	    PLVariable var = (PLVariable)iter.next();
	    Object[] row = new Object[2];
	    row[0] = var.elemPLString.getValue();
	    if (var.elemPLSurrogate != null) {
		row[1] = var.elemPLSurrogate.getID();
	    }
	    tableModel.addRow(row);
	}
    }
}
