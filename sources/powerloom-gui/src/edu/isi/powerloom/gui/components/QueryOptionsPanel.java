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


// Version: QueryOptionsPanel.java,v 1.5 2010/02/04 05:19:11 hans Exp

package edu.isi.powerloom.gui.components;

import edu.isi.powerloom.gui.xmlobject.*;
import javax.swing.*;

/**
 * Subpanel of QueryHistoryAndOptionsPanel which contains the query options.
 *
 * @since October 18, 2002, 7:21 PM
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 * @see edu.isi.powerloom.gui.components.QueryHistoryAndOptionsPanel QueryHistoryAndOptionsPanel
 */
public class QueryOptionsPanel extends javax.swing.JPanel {
    StandardQueryOptionsPanel standardQueryOptionsPanel = new StandardQueryOptionsPanel();
    PLFrame parentFrame;
    AdvancedQueryOptionsPanel advancedQueryOptionsPanel;
    
    /** Creates new form QueryOptionsPanel */
    public QueryOptionsPanel(PLFrame frame) {
	parentFrame = frame;
        initComponents();
        initCustomComponents();
    }
    
    private void initCustomComponents() {
        topPanel.add(standardQueryOptionsPanel, "Center");
        advancedQueryOptionsPanel = new AdvancedQueryOptionsPanel(parentFrame, this);
    }
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    private void initComponents() {//GEN-BEGIN:initComponents
        jButton2 = new javax.swing.JButton();
        topPanel = new javax.swing.JPanel();
        bottomPanel = new javax.swing.JPanel();
        buttonPanel = new javax.swing.JPanel();
        jButton1 = new javax.swing.JButton();

        jButton2.setText("jButton2");

        setLayout(new java.awt.BorderLayout());

        setBorder(new javax.swing.border.TitledBorder("Query Options"));
        topPanel.setLayout(new java.awt.BorderLayout());

        add(topPanel, java.awt.BorderLayout.CENTER);

        jButton1.setText("Show Advanced Options");
        jButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });

        buttonPanel.add(jButton1);

        bottomPanel.add(buttonPanel);

        add(bottomPanel, java.awt.BorderLayout.SOUTH);

    }//GEN-END:initComponents

    private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton1ActionPerformed
        remove(bottomPanel);
        add(advancedQueryOptionsPanel, "South");
        parentFrame.pack();
    }//GEN-LAST:event_jButton1ActionPerformed
    
    public JPanel getBottomPanel() {
	return bottomPanel;
    }
    
    /**
     * @return PLQuery structure with options filled in.
     */
    public PLQuery getQueryOptions() {
	PLQuery result = new PLQuery();
	// todo: validate textfield results!... Add ValidationUtils class in common package.
	result.attrInferenceLevel = standardQueryOptionsPanel.getInferenceLevel();
	result.attrTimeout = standardQueryOptionsPanel.getTimeout();
	result.attrMoveout = standardQueryOptionsPanel.getMoveout();
	result.attrMatchMode = advancedQueryOptionsPanel.getMatchMode();
	result.attrNumResults = standardQueryOptionsPanel.isAllResultRadioButtonSelected() ? "ALL" : standardQueryOptionsPanel.getNResults();
	result.attrMinScore = advancedQueryOptionsPanel.getMinimumScore();
	result.attrMaxUnknowns = advancedQueryOptionsPanel.getMaximumUnknowns();
	result.attrMaximizeScore = advancedQueryOptionsPanel.getMaximizeScore() ? "TRUE" : "FALSE";
	result.attrDontOptimize = advancedQueryOptionsPanel.getDontOptimize() ? "TRUE" : "FALSE";
	return result;
    }

    public void setQueryOptions(PLQuery plQuery) {
	standardQueryOptionsPanel.setInferenceLevel(plQuery.attrInferenceLevel);
	standardQueryOptionsPanel.setTimeout(plQuery.attrTimeout);
	standardQueryOptionsPanel.setMoveout(plQuery.attrMoveout);
	advancedQueryOptionsPanel.setMatchMode(plQuery.attrMatchMode);
	if (plQuery.attrNumResults.equals("ALL")) {
	    standardQueryOptionsPanel.selectAllResultsRadioButton();
	    standardQueryOptionsPanel.setNResults("");
	} else {
	    standardQueryOptionsPanel.selectNResultsRadioButton();
	    standardQueryOptionsPanel.setNResults(plQuery.attrNumResults);
	}
	advancedQueryOptionsPanel.setMinimumScore(plQuery.attrMinScore);
	advancedQueryOptionsPanel.setMaximumUnknowns(plQuery.attrMaxUnknowns);
	advancedQueryOptionsPanel.setMaximizeScore((plQuery.attrMaximizeScore != null) &&
						   (plQuery.attrMaximizeScore.equals("TRUE")));
	advancedQueryOptionsPanel.setDontOptimize((plQuery.attrDontOptimize != null) &&
						  (plQuery.attrDontOptimize.equals("TRUE")));
    }

    /**
     * Delegated by QueryFrame
     */
    public void setupContinueButtonListeners(JButton continueButton) {
	standardQueryOptionsPanel.setupContinueButtonListeners(continueButton);
	advancedQueryOptionsPanel.setupContinueButtonListeners(continueButton);
    }


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButton2;
    private javax.swing.JButton jButton1;
    private javax.swing.JPanel buttonPanel;
    private javax.swing.JPanel topPanel;
    private javax.swing.JPanel bottomPanel;
    // End of variables declaration//GEN-END:variables

}
