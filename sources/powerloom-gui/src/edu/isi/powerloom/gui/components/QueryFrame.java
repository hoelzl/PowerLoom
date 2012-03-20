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


// Version: QueryFrame.java,v 1.34 2010/02/13 00:13:34 hans Exp

package edu.isi.powerloom.gui.components;

import edu.isi.powerloom.gui.serverinterface.*;
import edu.isi.powerloom.gui.xmlobject.*;
import edu.isi.powerloom.gui.parser.*;
import static edu.isi.powerloom.gui.common.Utils.*;

import java.awt.event.*;
import javax.swing.table.*;
import javax.swing.event.*;
import javax.swing.*;
import java.util.*;

/**
 * Frame for query dialog.
 *
 * @since 5/22/2002 1:32:35 PM
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 * @see edu.isi.powerloom.gui.components.QueryHistoryAndOptionsPanel QueryHistoryAndOptionsPanel
 * @see edu.isi.powerloom.gui.components.QueryHistoryPanel QueryHistoryPanel
 * @see edu.isi.powerloom.gui.components.QueryOptionsPanel QueryOptionsPanel
 * @see edu.isi.powerloom.gui.components.QueryTableModel QueryTableModel
 */
public class QueryFrame extends PLFrame implements ActionComponent {
    private javax.swing.JPanel ivjButtonPanel = null;
    private javax.swing.JButton ivjCancelButton = null;
    private javax.swing.JButton ivjContinueButton = null;
    private javax.swing.JButton ivjExecuteButton = null;
    private javax.swing.JButton ivjParseButton = null;
    private javax.swing.JPanel ivjJInternalFrameContentPane = null;
    private javax.swing.JLabel ivjJLabel1 = null;
    private javax.swing.JPanel ivjQueryGroupPanel = null;
    private javax.swing.JLabel ivjQueryLabel = null;
    private ExpressionEditorPanel ivjQueryPanel = null;
    private TableAdderPanel ivjVariableAdderPanel = null;
    private javax.swing.JPanel ivjVariableGroupPanel = null;
    private javax.swing.JScrollPane ivjJScrollPane2 = null;
    private javax.swing.JPanel ivjResultGroupPanel = null;
    private javax.swing.JLabel ivjResultsLabel = null;
    private javax.swing.JTable ivjResultsTable = null;
    private javax.swing.JPanel ivjVariableQueryResultGroupPanel = null;
    private javax.swing.BoxLayout ivjVariableQueryResultGroupPanelBoxLayout = null;
    private javax.swing.JComboBox ivjModuleComboBox = null;
    private javax.swing.JPanel ivjModulePanel = null;
    private PowerloomApp app;
    IvjEventHandler ivjEventHandler = new IvjEventHandler();
    private javax.swing.JScrollPane ivjJScrollPane1 = null;
    private BrowserFrame4 queryBrowser;
    private javax.swing.JPanel ivjControlPanel = null;
    private QueryHistoryAndOptionsPanel ivjQueryHistoryAndOptionsPanel = null;
    private final String queryID = UUID.randomUUID().toString();
    private boolean calledFromContinue;

    class IvjEventHandler implements java.awt.event.ActionListener {
	public void actionPerformed(java.awt.event.ActionEvent e) {
	    if (e.getSource() == QueryFrame.this.getVariableAdderPanel()) 
		connEtoC1(e);
	    if (e.getSource() == QueryFrame.this.getCancelButton()) 
		connEtoC2(e);
	    if (e.getSource() == QueryFrame.this.getExecuteButton()) 
		connEtoC3(e);
	    if (e.getSource() == QueryFrame.this.getParseButton()) 
		connEtoC4(e);
	    if (e.getSource() == QueryFrame.this.getContinueButton()) 
		connEtoC5(e);
	};
    };
    private javax.swing.JLabel ivjModuleLabel = null;
    /**
     * QueryFrame constructor comment.
     */
    public QueryFrame() {
	super();
	initialize();
    }
    /**
     * QueryFrame constructor comment.
     * @param title java.lang.String
     */
    public QueryFrame(String title) {
	super(title);
    }
    /**
     * QueryFrame constructor comment.
     * @param title java.lang.String
     * @param resizable boolean
     */
    public QueryFrame(String title, boolean resizable) {
	super(title, resizable);
    }
    /**
     * QueryFrame constructor comment.
     * @param title java.lang.String
     * @param resizable boolean
     * @param closable boolean
     */
    public QueryFrame(String title, boolean resizable, boolean closable) {
	super(title, resizable, closable);
    }
    /**
     * QueryFrame constructor comment.
     * @param title java.lang.String
     * @param resizable boolean
     * @param closable boolean
     * @param maximizable boolean
     */
    public QueryFrame(String title, boolean resizable, boolean closable, boolean maximizable) {
	super(title, resizable, closable, maximizable);
    }
    /**
     * QueryFrame constructor comment.
     * @param title java.lang.String
     * @param resizable boolean
     * @param closable boolean
     * @param maximizable boolean
     * @param iconifiable boolean
     */
    public QueryFrame(String title, boolean resizable, boolean closable, boolean maximizable, boolean iconifiable) {
	super(title, resizable, closable, maximizable, iconifiable);
    }
    /**
     * Comment
     */
    public void cancelButton_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	doDefaultCloseAction();
    }
    /**
     * connEtoC1:  (VariableAdderPanel.action.actionPerformed(java.awt.event.ActionEvent) --> QueryFrame.variableAdderPanel_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC1(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.variableAdderPanel_ActionPerformed(arg1);
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }
    /**
     * connEtoC2:  (CancelButton.action.actionPerformed(java.awt.event.ActionEvent) --> QueryFrame.cancelButton_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC2(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.cancelButton_ActionPerformed(arg1);
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }
    /**
     * connEtoC3:  (ExecuteButton.action.actionPerformed(java.awt.event.ActionEvent) --> QueryFrame.executeButton_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC3(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.executeButton_ActionPerformed(arg1);
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }


    private void connEtoC4(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.parseButton_ActionPerformed(arg1);
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }

    private void connEtoC5(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.continueButton_ActionPerformed(arg1);
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }
    public void continueButton_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	calledFromContinue = true;
	executeButton_ActionPerformed(actionEvent);
    }

    private PLQuery createPLQuery() {
	PLQuery result = null;
	String variables = "(";
	String[] varList = getFreeVariables();
	for (int i = 0; i < varList.length; i++) {
	    variables += " " + varList[i];
	}
	variables += ")";	
	String query = getQueryPanel().getText();
	String bodyString = null;
	if (varList.length == 0) {
	    bodyString = query;
	} else {
	    bodyString = variables + " " + query;
	}

	try {
	    result = ivjQueryHistoryAndOptionsPanel.getQueryOptionsPanel().getQueryOptions();
	    PLModule module = getModule();
	    if (module != null) {
		result.attrModule = module.getID();
	    } else {
		result.attrModule = "";
	    }
	    result.attrQuery = bodyString;
	    if (varList.length == 0) {
		result.attrIsAsk = "TRUE";
	    } else {
		result.attrIsAsk = "FALSE";
	    }
            // uniquely identify query for follow-up requests:
            result.attrQueryName = queryID;
	} catch (Exception e) {
	    handleException(e);
	}
	return result;
    }

    /**
     * Comment
     */

    // todo? run this in a separate thread....
    public void executeButton_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	java.awt.Cursor savedCursor = PowerloomApp.getInstance().getCursor();
	try {
	    setCursor(new java.awt.Cursor(java.awt.Cursor.WAIT_CURSOR));
	    PLModule module = (PLModule)getModuleComboBox().getSelectedItem();
	    if (module == null) {
		JOptionPane.showMessageDialog(app, "You must select a module before you execute a query.", "Select Module", JOptionPane.WARNING_MESSAGE);
		return;
	    }
	    String[] freeVariables = getFreeVariables();
	    if (freeVariables == null) {
		return; // parse error
	    }
	    PLQuery query = createPLQuery();
	    PLQueryResult queryResult = null;
	    try {
		queryResult = KnowledgeManager.getInstance().executeQuery(query, calledFromContinue);
	    } catch (Exception e) {
		throw e;
	    } finally {
		calledFromContinue = false;
	    }
	    // Results empty?
	    if ((queryResult.elemPLTuple == null) ||
		(queryResult.elemPLTuple.isEmpty())) {
		String labelMessage = "No Results were Retrieved";
		JLabel emptyLabel = new JLabel(labelMessage, SwingConstants.CENTER);
		getJScrollPane2().setViewportView(emptyLabel);
		PowerloomApp.getInstance().flashMessage(emptyLabel, labelMessage, 2, 200); 
		emptyLabel.setBackground(java.awt.Color.WHITE);
		emptyLabel.setBackground(java.awt.Color.RED);
	    } else {
		PLTuple firstTuple = (PLTuple)((List)queryResult.elemPLTuple).get(0);
		int tupleSize = firstTuple.elemPLObjectUnion.size();
		boolean isPartial = false;
		// If the tuple size is 1 greater than the normal, assume that
		// we've done a partial match, and the extra column is the match score
		if ((tupleSize > freeVariables.length) && 
		    ((query.attrIsAsk != null) &&
		     !query.attrIsAsk.equalsIgnoreCase("TRUE"))) {
		    isPartial = true;
		}
		getJScrollPane2().setViewportView(getResultsTable());
		String[] headers = freeVariables;
		if (isPartial) {
		    headers = new String[freeVariables.length + 1];
		    for (int i = 0; i < freeVariables.length; i++) {
			headers[i] = freeVariables[i];
		    }
		    headers[headers.length - 1] = "Match Score";
		}
		QueryTableModel tableModel = new QueryTableModel(queryResult, headers);
		getResultsTable().setModel(tableModel);
		tableModel.fireTableChanged(null);
		installQueryResultPopup(getResultsTable(), query);
		if (!query.attrNumResults.equals("ALL")) {
		    getContinueButton().setEnabled(true);
		}
	    }
	} catch (Exception e) {
	    handleException(e);
	} finally {
	    setCursor(savedCursor);
	}
    
    }

    public void parseButton_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	try {
	    PLElement parseTree = getQueryPanel().parseExpression();
	    parseTree.dump();
	    debugPrintln(3, "startOffset = " + parseTree.getStartOffset() + ", endOffset = " + parseTree.getEndOffset());
	    debugPrintln(3, "Text--->" + getQueryPanel().getText());
	    System.out.print  ("Child0->");
	    for (int i = 0; i < getQueryPanel().getText().length(); i++) {
		int index = parseTree.getElementIndex(i);
		System.out.print(index >= 0 ? Integer.toString(index) : " ");
	    }
	    System.out.println();
	    System.out.print  ("Child1->");
	    for (int i = 0; i < getQueryPanel().getText().length(); i++) {
		int index = parseTree.getElement(0).getElementIndex(i);
		System.out.print(index >= 0 ? Integer.toString(index) : " ");
	    }
	    System.out.println();
	    List vars = collectVariables(parseTree);
	    Iterator listIter = vars.iterator();
	    debugPrintln(3, "Free Variables:");
	    while (listIter.hasNext()) {
		debugPrintln(3, "  " + listIter.next());
	    }
	} catch (Exception e) {
	    PowerloomApp.getInstance().handleException(e);
	}
    }
    String[] getFreeVariables() {
	PLElement parseTree = null;
	try {
	    parseTree = getQueryPanel().parseExpression();
	} catch (Exception e) {
	    JOptionPane.showMessageDialog(PowerloomApp.getInstance(), "The expression \"" + getQueryPanel().getText() + "\" is not legal Powerloom syntax.", "Parse Error", JOptionPane.ERROR_MESSAGE);
	    return null;
	}
	List vars = collectVariables(parseTree);
	return (String[])vars.toArray(new String[0]);
    }

    List collectVariables(PLElement parseTree) {
	List boundVarList = new ArrayList();
	helpCollectBoundVariables(parseTree, boundVarList);
	debugPrintln(3, "Bound variables = " + boundVarList);
	List varList = new ArrayList();
	helpCollectVariables(parseTree, varList);
	debugPrintln(3, "All variables = " + varList);
	// exclude boundVars
	varList.removeAll(boundVarList);
	return varList;
    }

    void helpCollectBoundVariables(PLElement node, List varList) {
	Symbol symbol = node.getSymbol();
	if (symbol != null) {
	    if (symbol instanceof NonTerminal) {
		NonTerminal nonTerm = (NonTerminal)symbol;
		if (nonTerm.getName().equals("QUANTSENT")) {
		    // get Variable declarations
		    
		    PLElement varDecl = (PLElement)node.getElement(2);
		    debugPrintln(3, "found quantsent, vardecl = ");
		    varDecl.dump();
		    helpCollectVariables(varDecl, varList);
		    for (int i = 3; i < node.getElementCount(); i++) {
			helpCollectBoundVariables((PLElement)node.getElement(i), varList);
		    }
		} else { // something other than exists
		    for (int i = 0; i < node.getElementCount(); i++) {
			helpCollectBoundVariables((PLElement)node.getElement(i), varList);
		    }
		}
	    }
	}
    }

    // collect all indvars
    void helpCollectVariables(PLElement node, List varList) {
	Symbol symbol = node.getSymbol();
	if (symbol != null) {
	    if (symbol instanceof NonTerminal) {
		NonTerminal nonTerm = (NonTerminal)symbol;
		for (int i = 0; i < node.getElementCount(); i++) {
		    helpCollectVariables((PLElement)node.getElement(i), varList);
		}
	    } 
	    if (symbol instanceof Terminal) {
		Terminal term = (Terminal)symbol;
		if (term.getName().equals("indvar")) {
		    Yytoken token = node.getToken();
		    if (!varList.contains(token.getText())) {
			varList.add(token.getText());
		    }
		}
	    }
	} else {
	    debugPrintln(3, "***Err in helpCollectVariables: symbol is null");
	}
    }

    /**
     * Insert the method's description here.
     * Creation date: (5/22/2002 3:48:08 PM)
     * @return redesign.gui.components.PowerloomApp
     */
    public PowerloomApp getApp() {
	return app;
    }
    /**
     * Return the ButtonPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getButtonPanel() {
	if (ivjButtonPanel == null) {
	    try {
		ivjButtonPanel = new javax.swing.JPanel();
		ivjButtonPanel.setName("ButtonPanel");
		ivjButtonPanel.setLayout(new java.awt.FlowLayout());
		getButtonPanel().add(getExecuteButton(), getExecuteButton().getName());
		getButtonPanel().add(getContinueButton(), getContinueButton().getName());
		//getButtonPanel().add(getParseButton(), getParseButton().getName());
		getButtonPanel().add(getCancelButton(), getCancelButton().getName());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjButtonPanel;
    }

    private javax.swing.JPanel getControlPanel() {
	if (ivjControlPanel == null) {
	    try {
		ivjControlPanel = new javax.swing.JPanel();
		ivjControlPanel.setLayout(new java.awt.BorderLayout());
		ivjQueryHistoryAndOptionsPanel = new QueryHistoryAndOptionsPanel(this);
		ivjControlPanel.add(ivjQueryHistoryAndOptionsPanel, "Center");
		ivjControlPanel.add(getButtonPanel(), "South");

		final QueryOptionsPanel queryOptionsPanel = ivjQueryHistoryAndOptionsPanel.getQueryOptionsPanel();
		final JComboBox queryComboBox = ivjQueryHistoryAndOptionsPanel.getQueryHistoryPanel().getQueryComboBox();
		ItemListener listener = new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
			    try {
				PLQuery query = (PLQuery)queryComboBox.getSelectedItem();
				queryOptionsPanel.setQueryOptions(query);
				QueryFrame.this.getQueryPanel().setText(query.attrQuery);
				PLModule module = KnowledgeManager.getInstance().getModuleFromName(query.attrModule);
				if (module == null) {
				} else {
				    getModuleComboBox().setSelectedItem(module);
				}
			    } catch (Exception e2) {
				PowerloomApp.getInstance().handleException(e2);
			    }
			}
		    };
		queryComboBox.addItemListener(listener);

		ActionListener saveListener = new ActionListener() {
			public void actionPerformed(ActionEvent e) {
			    PLQuery query = queryOptionsPanel.getQueryOptions();
			    String queryBody = QueryFrame.this.getQueryPanel().getText();
			    PLModule module = getModule();
			    query.attrQuery = queryBody;
			    query.attrModule = module.getID();
			    ivjQueryHistoryAndOptionsPanel.getQueryHistoryPanel().saveQuery(query);
			}
		    };
		JButton saveButton = ivjQueryHistoryAndOptionsPanel.getQueryHistoryPanel().getSaveQueryButton();
		saveButton.addActionListener(saveListener);
	    } catch (java.lang.Throwable ivjExc) {
		handleException(ivjExc);
	    }
	}
	return ivjControlPanel;
    }

    /**
     * Return the CancelButton property value.
     * @return javax.swing.JButton
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JButton getCancelButton() {
	if (ivjCancelButton == null) {
	    try {
		ivjCancelButton = new javax.swing.JButton();
		ivjCancelButton.setName("CancelButton");
		ivjCancelButton.setText("Cancel");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjCancelButton;
    }
    /**
     * Return the ExecuteButton property value.
     * @return javax.swing.JButton
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JButton getExecuteButton() {
	if (ivjExecuteButton == null) {
	    try {
		ivjExecuteButton = new javax.swing.JButton();
		ivjExecuteButton.setName("ExecuteButton");
		ivjExecuteButton.setText("Execute");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjExecuteButton;
    }


    private javax.swing.JButton getContinueButton() {
	if (ivjContinueButton == null) {
	    try {
		ivjContinueButton = new javax.swing.JButton();
		ivjContinueButton.setName("ContinueButton");
		ivjContinueButton.setText("Continue");
		// user code begin {1}
		ivjContinueButton.setEnabled(false);
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjContinueButton;
    }

    private javax.swing.JButton getParseButton() {
	if (ivjParseButton == null) {
	    try {
		ivjParseButton = new javax.swing.JButton();
		ivjParseButton.setName("ParseButton");
		ivjParseButton.setText("Parse");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjParseButton;
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
		ivjJInternalFrameContentPane.setLayout(new java.awt.BorderLayout());
		getJInternalFrameContentPane().add(getVariableQueryResultGroupPanel(), "Center");
		//getJInternalFrameContentPane().add(getButtonPanel(), "South");
		getJInternalFrameContentPane().add(getControlPanel(), "South");
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
     * Return the JLabel1 property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getJLabel1() {
	if (ivjJLabel1 == null) {
	    try {
		ivjJLabel1 = new javax.swing.JLabel();
		ivjJLabel1.setName("JLabel1");
		ivjJLabel1.setText("Variables:");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjJLabel1;
    }
    /**
     * Return the JScrollPane1 property value.
     * @return javax.swing.JScrollPane
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JScrollPane getJScrollPane1() {
	if (ivjJScrollPane1 == null) {
	    try {
		ivjJScrollPane1 = new javax.swing.JScrollPane();
		ivjJScrollPane1.setName("JScrollPane1");
		getJScrollPane1().setViewportView(getQueryPanel());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjJScrollPane1;
    }
    /**
     * Return the JScrollPane2 property value.
     * @return javax.swing.JScrollPane
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JScrollPane getJScrollPane2() {
	if (ivjJScrollPane2 == null) {
	    try {
		ivjJScrollPane2 = new javax.swing.JScrollPane();
		ivjJScrollPane2.setName("JScrollPane2");
		ivjJScrollPane2.setVerticalScrollBarPolicy(javax.swing.JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
		ivjJScrollPane2.setHorizontalScrollBarPolicy(javax.swing.JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
		getJScrollPane2().setViewportView(getResultsTable());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjJScrollPane2;
    }
    /**
     * Return the ModuleComboBox property value.
     * @return javax.swing.JComboBox
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JComboBox getModuleComboBox() {
	if (ivjModuleComboBox == null) {
	    try {
		ivjModuleComboBox = new javax.swing.JComboBox();
		ivjModuleComboBox.setName("ModuleComboBox");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjModuleComboBox;
    }
    /**
     * Return the ModuleLabel property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getModuleLabel() {
	if (ivjModuleLabel == null) {
	    try {
		ivjModuleLabel = new javax.swing.JLabel();
		ivjModuleLabel.setName("ModuleLabel");
		ivjModuleLabel.setText("Module:");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjModuleLabel;
    }
    /**
     * Return the ModulePanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getModulePanel() {
	if (ivjModulePanel == null) {
	    try {
		ivjModulePanel = new javax.swing.JPanel();
		ivjModulePanel.setName("ModulePanel");
		ivjModulePanel.setLayout(new java.awt.FlowLayout());
		getModulePanel().add(getModuleLabel(), getModuleLabel().getName());
		getModulePanel().add(getModuleComboBox(), getModuleComboBox().getName());
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
     * Return the QueryGroupPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getQueryGroupPanel() {
	if (ivjQueryGroupPanel == null) {
	    try {
		ivjQueryGroupPanel = new javax.swing.JPanel();
		ivjQueryGroupPanel.setName("QueryGroupPanel");
		ivjQueryGroupPanel.setPreferredSize(new java.awt.Dimension(37, 100));
		ivjQueryGroupPanel.setLayout(new java.awt.BorderLayout());
		ivjQueryGroupPanel.setMinimumSize(new java.awt.Dimension(37, 50));
		getQueryGroupPanel().add(getQueryLabel(), "North");
		getQueryGroupPanel().add(getJScrollPane1(), "Center");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjQueryGroupPanel;
    }
    /**
     * Return the QueryLabel property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getQueryLabel() {
	if (ivjQueryLabel == null) {
	    try {
		ivjQueryLabel = new javax.swing.JLabel();
		ivjQueryLabel.setName("QueryLabel");
		ivjQueryLabel.setText("Query:");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjQueryLabel;
    }
    /**
     * Return the QueryPanel property value.
     * @return redesign.gui.components.ExpressionEditorPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private ExpressionEditorPanel getQueryPanel() {
	if (ivjQueryPanel == null) {
	    try {
		String[] types = {"Text Holder"};
		ivjQueryPanel = new edu.isi.powerloom.gui.components.ExpressionEditorPanel(this, "Query Frame Input Pane", Arrays.asList(types));
		ivjQueryPanel.setName("QueryPanel");
		ivjQueryPanel.setBounds(0, 0, 485, 10);
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjQueryPanel;
    }
    /**
     * Return the ResultGroupPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getResultGroupPanel() {
	if (ivjResultGroupPanel == null) {
	    try {
		ivjResultGroupPanel = new javax.swing.JPanel();
		ivjResultGroupPanel.setName("ResultGroupPanel");
		ivjResultGroupPanel.setLayout(new java.awt.BorderLayout());
		getResultGroupPanel().add(getResultsLabel(), "North");
		getResultGroupPanel().add(getJScrollPane2(), "Center");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjResultGroupPanel;
    }
    /**
     * Return the ResultsLabel property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getResultsLabel() {
	if (ivjResultsLabel == null) {
	    try {
		ivjResultsLabel = new javax.swing.JLabel();
		ivjResultsLabel.setName("ResultsLabel");
		ivjResultsLabel.setText("Results:");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjResultsLabel;
    }

    void navigateToSelection() {
	try {
	    int rowIndex = getResultsTable().getSelectedRow();
	    int colIndex = getResultsTable().getSelectedColumn();
	    if ((rowIndex > -1) && (colIndex > -1)) {
		TableModel tableModel = getResultsTable().getModel();
		if (tableModel.getValueAt(rowIndex, colIndex) instanceof PLObject) {
		    PLObject object = (PLObject)tableModel.getValueAt(rowIndex, colIndex);
		    debugPrintln(3, "You selected: " + object + ", row = "+ rowIndex + ", col = " + colIndex);
		    PLModule module = (PLModule)getModuleComboBox().getSelectedItem();
		    // retrieve object from server, to ensure we get module information
		    PLObject serverObject = KnowledgeManager.getInstance().getPLObject(module, object.getID());
		    debugPrintln(3, "module = " + module + ", serverObject = " + serverObject.getID() + " serverobject homeModule = " + serverObject.getModule());
		    PLModule homeModule = KnowledgeManager.getInstance().getModuleFromName(serverObject.getModule());
		    getQueryBrowser().getPubBrowserPanel().getPowerloomTrees().makeObjectVisible(homeModule, object, true);
		    //debugPrintln(3, "used browser: " + queryBrowser);
		}
	    }
	}  catch (Exception e) {
	    handleException(e);
	}
    }


    BrowserFrame4 getQueryBrowser() {
	if ((queryBrowser == null) || (!queryBrowser.isVisible())) {
	    getApp().browseMenuItem_ActionPerformed(null);
	    queryBrowser = getApp().getSelectedBrowserFrame();
	    debugPrintln(3, "created query browser: " + queryBrowser.hashCode());
	    /*
	      try {
	      //Thread.sleep(1000);
	      // make sure browser is displayed before other events happen, e.g., selection.
	      } catch (Exception e) {
	      e.printStackTrace();
	      }
	    */
	}
	return queryBrowser;
    }

    /**
     * Return the ResultsTable property value.
     * @return javax.swing.JTable
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTable getResultsTable() {
	if (ivjResultsTable == null) {
	    try {
		ivjResultsTable = new javax.swing.JTable();
		ivjResultsTable.setName("ResultsTable");
		getJScrollPane2().setColumnHeaderView(ivjResultsTable.getTableHeader());
		getJScrollPane2().getViewport().setBackingStoreEnabled(true);
		getJScrollPane2().setPreferredSize(new java.awt.Dimension(50, 150));
		ivjResultsTable.setBounds(0, 0, 100, 100);
		// user code begin {1}
		//ivjResultsTable.setPreferredSize(new java.awt.Dimension(50, 50));
		ivjResultsTable.setRowSelectionAllowed(true);
		ivjResultsTable.setColumnSelectionAllowed(true); 
		ivjResultsTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		ListSelectionListener selectionListener = 
		    (new ListSelectionListener() { 
			    public void valueChanged(ListSelectionEvent lse) {
				//debugPrintln(3, "Selection event: " + lse); 
				if (!lse.getValueIsAdjusting()) {
				    navigateToSelection();
				}
			    }
			});
		ivjResultsTable.getSelectionModel().addListSelectionListener(selectionListener);
		ivjResultsTable.getTableHeader().addMouseListener(new MouseListener() {
			public void mousePressed(MouseEvent e) { sortItems(e); }
			public void mouseClicked(MouseEvent e) {}
			public void mouseEntered(MouseEvent e) {}
			public void mouseExited(MouseEvent e) {}
			public void mouseReleased(MouseEvent e) {}
			private void sortItems(MouseEvent e) {
			    TableColumnModel colModel = (TableColumnModel)ivjResultsTable.getColumnModel();
			    int colIndex = colModel.getColumnIndexAtX(e.getX());
			    int modelIndex = ivjResultsTable.getColumnModel().getColumn(colIndex).getModelIndex();
			    debugPrintln(3, "You selected col #" + colIndex + ", modelindex = " + modelIndex);
			    ((QueryTableModel)ivjResultsTable.getModel()).sortColumn(modelIndex);
				    
			}
		    });

		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjResultsTable;
    }
    /**
     * Return the VariableAdderPanel property value.
     * @return redesign.gui.components.TableAdderPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private TableAdderPanel getVariableAdderPanel() {
	if (ivjVariableAdderPanel == null) {
	    try {
		ivjVariableAdderPanel = new edu.isi.powerloom.gui.components.TableAdderPanel();
		ivjVariableAdderPanel.setName("VariableAdderPanel");
		ivjVariableAdderPanel.setPreferredSize(new java.awt.Dimension(44, 50));
		ivjVariableAdderPanel.setText("VariableAdderPanel");
		ivjVariableAdderPanel.setMinimumSize(new java.awt.Dimension(44, 80));
		// user code begin {1}
		Object[] columnNames = {"Variable", "Type"};
		javax.swing.JTable table = new javax.swing.JTable();
		table.setModel(new javax.swing.table.DefaultTableModel(columnNames, 0));
		ivjVariableAdderPanel.setTable(table);
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjVariableAdderPanel;
    }

    private void installQueryResultPopup(JTable table, PLQuery query) {
	java.util.List tableMenuItems = new ArrayList();
	java.util.List itemMenuItems = new ArrayList();
	java.util.List tableOnlyMenuItems = new ArrayList();
	java.util.List rowMenuItems = new ArrayList();
	rowMenuItems.add(new ExplainAction());
	PopupUtils.installTablePopup(getModule(), table, query, tableMenuItems, itemMenuItems, tableOnlyMenuItems, 
				     rowMenuItems, null);
    }
    

    /**
     * Return the VariableGroupPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getVariableGroupPanel() {
	if (ivjVariableGroupPanel == null) {
	    try {
		ivjVariableGroupPanel = new javax.swing.JPanel();
		ivjVariableGroupPanel.setName("VariableGroupPanel");
		ivjVariableGroupPanel.setLayout(new java.awt.BorderLayout());
		getVariableGroupPanel().add(getJLabel1(), "North");
		getVariableGroupPanel().add(getVariableAdderPanel(), "Center");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjVariableGroupPanel;
    }
    /**
     * Return the VariableQueryGroupPanel property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getVariableQueryResultGroupPanel() {
	if (ivjVariableQueryResultGroupPanel == null) {
	    try {
		ivjVariableQueryResultGroupPanel = new javax.swing.JPanel();
		ivjVariableQueryResultGroupPanel.setName("VariableQueryResultGroupPanel");
		ivjVariableQueryResultGroupPanel.setLayout(getVariableQueryResultGroupPanelBoxLayout());
		//getVariableQueryResultGroupPanel().add(getVariableGroupPanel(), getVariableGroupPanel().getName());
		getVariableQueryResultGroupPanel().add(getQueryGroupPanel(), getQueryGroupPanel().getName());
		getVariableQueryResultGroupPanel().add(getModulePanel(), getModulePanel().getName());
		getVariableQueryResultGroupPanel().add(getResultGroupPanel(), getResultGroupPanel().getName());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjVariableQueryResultGroupPanel;
    }
    /**
     * Return the VariableQueryResultGroupPanelBoxLayout property value.
     * @return javax.swing.BoxLayout
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.BoxLayout getVariableQueryResultGroupPanelBoxLayout() {
	javax.swing.BoxLayout ivjVariableQueryResultGroupPanelBoxLayout = null;
	try {
	    /* Create part */
	    ivjVariableQueryResultGroupPanelBoxLayout = new javax.swing.BoxLayout(getVariableQueryResultGroupPanel(), javax.swing.BoxLayout.Y_AXIS);
	} catch (java.lang.Throwable ivjExc) {
	    handleException(ivjExc);
	};
	return ivjVariableQueryResultGroupPanelBoxLayout;
    }
    /**
     * Insert the method's description here.
     * Creation date: (5/23/2002 4:44:59 PM)
     * @return java.lang.String[]
     */
    private String[] getVariables() {
	DefaultTableModel model = (DefaultTableModel)getVariableAdderPanel().getTable().getModel();
	int rowCount = model.getRowCount();
	String result[] = new String[rowCount];
	for (int i = 0; i < rowCount; i++) {
	    result[i] = (String)model.getValueAt(i, 0);
	} 
	return result;
    }
    /**
     * Called whenever the part throws an exception.
     * @param exception java.lang.Throwable
     */
    private void handleException(java.lang.Throwable exception) {
	PowerloomApp.getInstance().handleException(exception);
    }
    /**
     * Initializes connections
     * @exception java.lang.Exception The exception description.
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void initConnections() throws java.lang.Exception {
	// user code begin {1}
	// user code end
	getVariableAdderPanel().addActionListener(ivjEventHandler);
	getCancelButton().addActionListener(ivjEventHandler);
	getExecuteButton().addActionListener(ivjEventHandler);
	getContinueButton().addActionListener(ivjEventHandler);
	getParseButton().addActionListener(ivjEventHandler);
    }
    /**
     * Initialize the class.
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void initialize() {
	try {
	    // user code begin {1}
	    addInternalFrameListener(new InternalFrameAdapter() {
		    public void internalFrameActivated(InternalFrameEvent e) {
			debugPrintln(3, "activated component: " + hashCode());
			PowerloomApp.getInstance().setSelectedActionComponent(QueryFrame.this);
		    }
		    public void internalFrameOpened(InternalFrameEvent e) {
			debugPrintln(3, "opeened component: " + hashCode());
			PowerloomApp.getInstance().setSelectedActionComponent(QueryFrame.this);
		    }
		});
	    app = PowerloomApp.getInstance();
	    edu.isi.powerloom.gui.xmlobject.PLSurrogateContainer modules = 
		KnowledgeManager.getInstance().getModules().listifyTreeContainer();
	    PLListModel model = new PLListModel(modules);
	    getModuleComboBox().setModel(model);
	    getModuleComboBox().addItemListener(new ItemListener () {
		    public void itemStateChanged(ItemEvent e) {
			getQueryPanel().setModule((PLModule)getModuleComboBox().getSelectedItem());
		    }
		});
	    PLModule selectedModule = getApp().getMostRecentlyTouchedModule();
	    if (selectedModule != null) {
		getModuleComboBox().setSelectedItem(selectedModule);
	    }
	    PLModule module = (PLModule)getModuleComboBox().getSelectedItem();
	    getQueryPanel().setModule(module);
	    debugPrintln(3, "set module in query panel to: " + getQueryPanel().getModule());
		
	    // temporary, used for testing.  Eventually, we will use QueryOptionsPanel.
	    //QueryHistoryPanel qh = new QueryHistoryPanel();


	    // user code end
	    setName("QueryFrame");
	    setTitle("Query");
	    setIconifiable(true);
	    setClosable(true);
	    setMaximum(false);
	    setSize(495, 396);
	    setMaximizable(true);
	    setResizable(true);
	    setContentPane(getJInternalFrameContentPane());
	    initConnections();
	    setupContinueButtonListeners();

	} catch (java.lang.Throwable ivjExc) {
	    handleException(ivjExc);
	}
	// user code begin {2}
	// user code end
    }

    /**
     * Set up all listeners which monitor the enable/disable state of the continue button.
     */
    private void setupContinueButtonListeners() {
	final JButton continueButton = getContinueButton();
	ivjQueryHistoryAndOptionsPanel.getQueryOptionsPanel().setupContinueButtonListeners(continueButton);
	final Action disableContinueAction = new AbstractAction() {
		public void actionPerformed(ActionEvent e) {
		    continueButton.setEnabled(false);
		}
	    };
	new ComboBoxValueTracker(getModuleComboBox(), disableContinueAction);
	new TextComponentValueTracker(getQueryPanel(), disableContinueAction);
    }

    /**
     * main entrypoint - starts the part when it is run as an application
     * @param args java.lang.String[]
     */
    public static void main(java.lang.String[] args) {
	try {
	    javax.swing.JFrame frame = new javax.swing.JFrame();
	    QueryFrame aQueryFrame;
	    aQueryFrame = new QueryFrame();
	    frame.setContentPane(aQueryFrame);
	    frame.setSize(aQueryFrame.getSize());
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
     * Comment
     */
    public void variableAdderPanel_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	if (actionEvent.getActionCommand().equals("add")) {
	    try {
		debugPrintln(3, "add button pressed.");
		VariableChooserDialog chooser = new VariableChooserDialog();
		chooser.setModal(true);
		chooser.setTitle("Choose variable");
		chooser.getChooserPanel().getEnterLabel().setText("Enter Concept Name:");
		PLModule module = (PLModule)getModuleComboBox().getSelectedItem();
		chooser.setModule(module);
		chooser.show();
		if ((chooser.getChooserPanel().getVariableResult() != null)) {
		    Object[] row = new Object[2];
		    row[0] = chooser.getChooserPanel().getVariableResult();
		    if (chooser.getChooserPanel().getConceptResult() != null) {
			PLSurrogate surrogate = KnowledgeManager.getInstance().
			    findOrCreateSurrogate(PLConcept.class, chooser.getChooserPanel().getConceptResult().getID());
			row[1] = surrogate.getID();
		    }
		    getVariableAdderPanel().addRow(row);
		    debugPrintln(3, "adding row: " + row[0] + ", " + row[1]); 
		}
	    } catch (Exception e) {
		handleException(e);
	    } // end of try-catch
	} else if (actionEvent.getActionCommand().equals("delete")) {
	    debugPrintln(3, "delete button pressed.");
	    getVariableAdderPanel().removeSelectedRow();
	} else {
	    debugPrintln(3, "Uknown action command: " + actionEvent.getActionCommand());
	}
    }

    // implemention of ActionComponent interface
    public void doAction() {
	executeButton_ActionPerformed(null);
    }

    /**
     * Implementation of PLFrame
     */
    public PLModule getModule() {
	PLModule module = (PLModule)getModuleComboBox().getSelectedItem();
	return module;
    }

    public Icon getFrameIcon() {
	return PowerloomApp.getInstance().getImage("resources/images/query.gif");
    }



}
