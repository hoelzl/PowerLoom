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


// Version: PropositionFrame2.java,v 1.20 2010/02/04 05:19:04 hans Exp

package edu.isi.powerloom.gui.components;

import edu.isi.powerloom.gui.xmlobject.*;
import edu.isi.powerloom.gui.serverinterface.*;
import static edu.isi.powerloom.gui.common.Utils.*;

import java.awt.*;
import java.awt.datatransfer.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import java.util.*;

/**
 * Frame for editing propositions.
 *
 * @since August 19, 2002, 5:56 PM
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */
public class PropositionFrame2 extends PLFrame implements ActionComponent {
    
    /** Creates new form NewPropositionFrame */
    public PropositionFrame2() {
        initComponents();
        initCustomComponents();
	initialize();
    }
    
    private void initCustomComponents() {
	String[] types = {"Text Holder"};
        propositionPane = new ExpressionEditorPanel(this, "Proposition Editor Pane", Arrays.asList(types));
	Font font = new Font("Courier", Font.PLAIN, 12);
	propositionPane.setFont(font);
        jScrollPane1.setViewportView(propositionPane);
    }
        
    private void initialize() {
	try {
	    app = PowerloomApp.getInstance();
            edu.isi.powerloom.gui.xmlobject.PLSurrogateContainer modules = 
		    KnowledgeManager.getInstance().getModules().listifyTreeContainer();
            PLListModel model = new PLListModel(null, modules);
            moduleComboBox.setModel(model);
	    moduleComboBox.addItemListener(new ItemListener () {
		    public void itemStateChanged(ItemEvent e) {
			propositionPane.setModule((PLModule)moduleComboBox.getSelectedItem());
			app.setMostRecentlyTouchedModule((PLModule)moduleComboBox.getSelectedItem());
		    }
		});
            PLModule selectedModule = app.getMostRecentlyTouchedModule();
	    debugPrintln(3, "propframe2: setting module to: " + selectedModule);
            setModule(selectedModule);
	    // Ensure editor gets focus when the frame is opened
	    // Interesting: using internalFrameActivated does what I want, 
	    // but using FocusListener.focusGained doesn't do anything.
	    final JTextPane propPane = propositionPane;
	    InternalFrameListener frameListener = new InternalFrameAdapter() {
		    public void internalFrameActivated(InternalFrameEvent e) {
			JInternalFrame internalFrame = (JInternalFrame)e.getSource();
			debugPrintln(3, "activated propframe " + internalFrame.hashCode());
			propPane.requestFocus();
		    }
		};
	    addInternalFrameListener(frameListener);

	    addInternalFrameListener(new InternalFrameAdapter() {
		    public void internalFrameActivated(InternalFrameEvent e) {
			debugPrintln(3, "activated component: " + hashCode());
			PowerloomApp.getInstance().setSelectedActionComponent(PropositionFrame2.this);
		    }
		    public void internalFrameOpened(InternalFrameEvent e) {
			debugPrintln(3, "opeened component: " + hashCode());
			PowerloomApp.getInstance().setSelectedActionComponent(PropositionFrame2.this);
		    }
		});
	} catch (Exception e) {
	    handleException(e);
	} 
    }

    public Icon getFrameIcon() {
	return PowerloomApp.getInstance().getImage("resources/images/edit-proposition.gif");
    }


    public void setModule(edu.isi.powerloom.gui.xmlobject.PLModule selectedModule) {
	if (selectedModule != null) {
            moduleComboBox.setSelectedItem(selectedModule);
	}
        propositionPane.setModule(selectedModule);
    }
    
    public edu.isi.powerloom.gui.xmlobject.PLModule getModule() {
	return (PLModule)moduleComboBox.getSelectedItem();
    }    
    
    private void handleException(Exception e) {
        PowerloomApp.getInstance().handleException(e);
    }
    
    public void setProposition(edu.isi.powerloom.gui.xmlobject.PLModule module,
                           edu.isi.powerloom.gui.xmlobject.PLProposition proposition) {
        propositionPane.setText(proposition.attrPropositionName);
    }

    public void setProposition(PLModule module, String proposition) {
        propositionPane.setText(proposition);
    }

    public void setProposition(PLModule module, String proposition, int startSelection, int endSelection) {
        propositionPane.setText(proposition);
	propositionPane.select(startSelection, endSelection);
    }
        
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    private void initComponents() {//GEN-BEGIN:initComponents
        jScrollPane1 = new javax.swing.JScrollPane();
        jPanel1 = new javax.swing.JPanel();
        jToolBar1 = new javax.swing.JToolBar();
        assertButton = new javax.swing.JButton();
        denyButton = new javax.swing.JButton();
        retractButton = new javax.swing.JButton();
        jPanel3 = new javax.swing.JPanel();
        jPanel2 = new javax.swing.JPanel();
        CancelButton = new javax.swing.JButton();
        jPanel4 = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        moduleComboBox = new javax.swing.JComboBox();

        setClosable(true);
        setIconifiable(true);
        setMaximizable(true);
        setResizable(true);
        setTitle("Edit Proposition");
        getContentPane().add(jScrollPane1, java.awt.BorderLayout.CENTER);

        jToolBar1.setOrientation(javax.swing.SwingConstants.VERTICAL);
        assertButton.setIcon(new javax.swing.ImageIcon(PowerloomApp.class.getClassLoader().getResource("resources/images/assert.gif")));
        assertButton.setToolTipText("Assert");
        assertButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                assertButtonActionPerformed(evt);
            }
        });

        jToolBar1.add(assertButton);

        denyButton.setIcon(new javax.swing.ImageIcon(PowerloomApp.class.getClassLoader().getResource("resources/images/deny.gif")));
        denyButton.setToolTipText("Deny");
        denyButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                denyButtonActionPerformed(evt);
            }
        });

        jToolBar1.add(denyButton);

        retractButton.setIcon(new javax.swing.ImageIcon(PowerloomApp.class.getClassLoader().getResource("resources/images/retract.gif")));
        retractButton.setToolTipText("Retract");
        retractButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                retractButtonActionPerformed(evt);
            }
        });

        jToolBar1.add(retractButton);

        jPanel1.add(jToolBar1);

        getContentPane().add(jPanel1, java.awt.BorderLayout.WEST);

        jPanel3.setLayout(new java.awt.BorderLayout());

        CancelButton.setText("Cancel");
        CancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                CancelButtonActionPerformed(evt);
            }
        });

        jPanel2.add(CancelButton);

        jPanel3.add(jPanel2, java.awt.BorderLayout.SOUTH);

        jLabel1.setText("Module: ");
        jPanel4.add(jLabel1);

        jPanel4.add(moduleComboBox);

        jPanel3.add(jPanel4, java.awt.BorderLayout.NORTH);

        getContentPane().add(jPanel3, java.awt.BorderLayout.SOUTH);

        pack();
        java.awt.Dimension screenSize = java.awt.Toolkit.getDefaultToolkit().getScreenSize();
        setSize(new java.awt.Dimension(437, 244));
        setLocation((screenSize.width-437)/2,(screenSize.height-244)/2);
    }//GEN-END:initComponents

    private void retractButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_retractButtonActionPerformed
	try {
		String proposition = propositionPane.getText();
		String command = "(RETRACT " + proposition + ")";
		// for now, hardcode module
		PLSurrogate surrogate = KnowledgeManager.getInstance().findOrCreateSurrogate(PLModule.class, "AIRCRAFT-KB");
		PLModule module = getModule();
		if (module == null) {
			handleException(new Exception("***propositionframe.retract: module is null"));
			return; // todo throw exception
		}
		KnowledgeManager.getInstance().evaluateLogicCommand(module, command);
		doDefaultCloseAction();
		//setVisible(false);
		KnowledgeManager.getInstance().invalidatePropositionCaches();
		fireEditPerformed(PLProposition.class);
	} catch (Exception e) {
	    handleException(e);
	}
    }//GEN-LAST:event_retractButtonActionPerformed

    private void denyButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_denyButtonActionPerformed
	try {
		String proposition = propositionPane.getText();
		String command = "(DENY " + proposition + ")";
		// for now, hardcode module
		PLSurrogate surrogate = KnowledgeManager.getInstance().findOrCreateSurrogate(PLModule.class, "AIRCRAFT-KB");
		PLModule module = getModule();
		if (module == null) {
			handleException(new Exception("***propositionframe.deny: module is null"));
			return; // todo throw exception
		}
		KnowledgeManager.getInstance().evaluateLogicCommand(module, command);
		doDefaultCloseAction();
		//setVisible(false);
		KnowledgeManager.getInstance().invalidatePropositionCaches();
		fireEditPerformed(PLProposition.class);
	} catch (Exception e) {
	    handleException(e);
	}
    }//GEN-LAST:event_denyButtonActionPerformed

    private void assertButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_assertButtonActionPerformed
	try {
		String proposition = propositionPane.getText();
		String command = "(ASSERT " + proposition + ")";
		// for now, hardcode module
		PLSurrogate surrogate = KnowledgeManager.getInstance().findOrCreateSurrogate(PLModule.class, "AIRCRAFT-KB");
		PLModule module = getModule();
		if (module == null) {
	    JOptionPane.showMessageDialog(PowerloomApp.getInstance(), "You must first select a module before asserting a proposition or rule.", "Select Module", JOptionPane.WARNING_MESSAGE);
			return; // todo throw exception
		}
		KnowledgeManager.getInstance().evaluateLogicCommand(module, command);
		doDefaultCloseAction();
		//setVisible(false);
		KnowledgeManager.getInstance().invalidatePropositionCaches();
		fireEditPerformed(PLProposition.class);
	} catch (Exception e) {
	    handleException(e);
	}
    }//GEN-LAST:event_assertButtonActionPerformed

    private void CancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_CancelButtonActionPerformed
	doDefaultCloseAction();
        setVisible(false);
    }//GEN-LAST:event_CancelButtonActionPerformed

    /** Insert the method's description here.
     * Creation date: (4/12/2002 10:33:12 PM)
     */
    void fireEditPerformed(Class editType) {
	PLEditEvent event = new PLEditEvent(this, editType);
	PowerloomApp.getInstance().fireEditPerformed(event);
    }    
    
    /**
     * Implementation of PLClipboardOwner
     */
    public void doCut() {
	propositionPane.cut();
    }

    public void doCopy() {
	propositionPane.copy();
    }

    public void doPaste(Transferable transferable) {
	propositionPane.paste();
    }

    public void lostOwnership(Clipboard c, Transferable t) {
    }

    // implementation of ActionComponent
    public void doAction() {
	assertButtonActionPerformed(null);
    }


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JButton CancelButton;
    private javax.swing.JComboBox moduleComboBox;
    private javax.swing.JButton denyButton;
    private javax.swing.JButton assertButton;
    private javax.swing.JButton retractButton;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JToolBar jToolBar1;
    // End of variables declaration//GEN-END:variables
    private ExpressionEditorPanel propositionPane;
    private PowerloomApp app;
}
