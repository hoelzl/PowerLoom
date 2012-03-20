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


// Version: ModuleFrame.java,v 1.16 2010/02/04 05:17:52 hans Exp

package edu.isi.powerloom.gui.components;

import java.util.*;
import javax.swing.*;
import javax.swing.event.*;

import edu.isi.powerloom.gui.xmlobject.*;
import edu.isi.powerloom.gui.serverinterface.*;
import static edu.isi.powerloom.gui.common.Utils.*;

/**
 * Frame for editing modules.
 *
 * @since 4/1/2002 7:52:32 PM
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 * @see edu.isi.powerloom.gui.components.ModulePanel ModulePanel
 */
public class ModuleFrame extends PLFrame {
    private PowerloomApp app;
    private JPanel ivjJInternalFrameContentPane = null;
    private java.awt.BorderLayout ivjJInternalFrameContentPaneBorderLayout = null;
    private edu.isi.powerloom.gui.xmlobject.PLModule savedModule;
    private edu.isi.powerloom.gui.xmlobject.PLConcept savedConcept;
    private ModulePanel ivjModulePanel = null;
    private PLModule module = null;
    /**
     * ConceptFrame2 constructor comment.
     */
    public ModuleFrame() {
	super();
	initialize();
    }
    /**
     * ConceptFrame2 constructor comment.
     * @param title java.lang.String
     */
    public ModuleFrame(String title) {
	super(title);
    }
    /**
     * ConceptFrame2 constructor comment.
     * @param title java.lang.String
     * @param resizable boolean
     */
    public ModuleFrame(String title, boolean resizable) {
	super(title, resizable);
    }
    /**
     * ConceptFrame2 constructor comment.
     * @param title java.lang.String
     * @param resizable boolean
     * @param closable boolean
     */
    public ModuleFrame(String title, boolean resizable, boolean closable) {
	super(title, resizable, closable);
    }
    /**
     * ConceptFrame2 constructor comment.
     * @param title java.lang.String
     * @param resizable boolean
     * @param closable boolean
     * @param maximizable boolean
     */
    public ModuleFrame(String title, boolean resizable, boolean closable, boolean maximizable) {
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
    public ModuleFrame(String title, boolean resizable, boolean closable, boolean maximizable, boolean iconifiable) {
	super(title, resizable, closable, maximizable, iconifiable);
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/10/2002 5:05:01 PM)
     */
    void commitModule() {
	try {
	    String definition = createModuleDefinition();
	    String moduleName = getModuleName();
	    debugPrintln(3, "module definition: " + definition);
	    // arbitrarily choose a module
	    // first, make sure modules are loaded.
	    KnowledgeManager.getInstance().getModules();
	    PLSurrogate modSurrogate = KnowledgeManager.getInstance().findOrCreateSurrogate(PLModule.class, "PL-USER");
	    PLModule mod = (PLModule)modSurrogate.getValue();
	    if (mod == null) {
		debugPrintln(3, "theres a problem, can't find module PL-USER!");
		return;
	    }

	    KnowledgeManager.getInstance().evaluateLogicCommand(mod, definition);	    
	    // refresh modules so we can get the newly one
	    KnowledgeManager.getInstance().initializeModules();
	    
	    PLModule newMod = KnowledgeManager.getInstance().getModuleFromName(moduleName);
	    PLEditEvent event = new PLEditEvent(this, PLModule.class, newMod);
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
    private String getModuleName() {
	return getModulePanel().getPubNameTextField().getText();
    }

    String createModuleDefinition() {
	String result = "";
	String name = getModuleName();
	String documentation = getModulePanel().getPubDocumentationTextArea().getText();
	String cppPackage = getModulePanel().getPubCppNamespaceTextField().getText();
	String lispPackage = getModulePanel().getPubLispPackageTextField().getText();
	String javaPackage = getModulePanel().getPubJavaPackageTextField().getText();
	String javaCatchall = getModulePanel().getPubJavaCatchallTextField().getText();
	String includes = "";
	javax.swing.ListModel model = getModulePanel().getPubIncludesPanel().getJList().getModel();
	for (int i = 0; i < model.getSize(); i++) {
	    includes += " " + model.getElementAt(i);
	}
	String uses = "";
	model = getModulePanel().getPubUsesPanel().getJList().getModel();
	for (int i = 0; i < model.getSize(); i++) {
	    uses += " " + model.getElementAt(i);
	}
	String shadows = "";
	model = getModulePanel().getPubShadowPanel().getJList().getModel();
	for (int i = 0; i < model.getSize(); i++) {
	    shadows += " " + model.getElementAt(i);
	}
	if (includes.trim().length() > 0) {
	    includes = " :INCLUDES (" + includes.trim() + ")";
	}
	if (uses.trim().length() > 0) {
	    uses = " :USES (" + uses.trim() + ")";
	}
	if (shadows.trim().length() > 0) {
	    shadows = " :SHADOW (" + shadows.trim() + ")";
	}
	if (documentation.trim().length() > 0) {
	    documentation = " :DOCUMENTATION \"" + documentation + "\"";
	}
	if (javaPackage.trim().length() > 0) {
	    javaPackage = " :JAVA-PACKAGE \"" + javaPackage + "\"";
	}
	if (lispPackage.trim().length() > 0) {
	    lispPackage = " :LISP-PACKAGE \"" + lispPackage + "\"";
	}
	if (cppPackage.trim().length() > 0) {
	    cppPackage = " :CPP-PACKAGE \"" + cppPackage + "\"";
	}
	if (javaCatchall.trim().length() > 0) {
	    javaCatchall = " :JAVA-CATCHALL-CLASS \"" + javaCatchall + "\"";
	}
	result = "(DEFMODULE " + name + includes + uses + documentation + javaPackage + cppPackage
	    + lispPackage + shadows + javaCatchall + ")";
	
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
		getJInternalFrameContentPane().add(getModulePanel(), "Center");
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
     * Return the ModulePanel property value.
     * @return redesign.gui.components.ModulePanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private ModulePanel getModulePanel() {
	if (ivjModulePanel == null) {
	    try {
		ivjModulePanel = new edu.isi.powerloom.gui.components.ModulePanel(this);
		ivjModulePanel.setName("ModulePanel");
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
	    setName("ModuleFrame");
	    setTitle("New Module");
	    setClosable(true);
	    setIconifiable(true);
	    setMaximizable(true);
	    setSize(600, 520);
	    setResizable(true);
	    setContentPane(getJInternalFrameContentPane());
	} catch (java.lang.Throwable ivjExc) {
	    handleException(ivjExc);
	}
	// user code begin {2}
	getModulePanel().setParentFrame(this);
	setTitle("New Module");
	setShadowList(makeBlankSurrogateContainer());
	setUsesList(makeBlankSurrogateContainer());
	setIncludesList(makeBlankSurrogateContainer());
	app = PowerloomApp.getInstance();

	// Ensure name field gets focus when the frame is opened
	final JTextField nameField = getModulePanel().getPubNameTextField();
	InternalFrameListener frameListener = new InternalFrameAdapter() {
		public void internalFrameActivated(InternalFrameEvent e) {
		    JInternalFrame internalFrame = (JInternalFrame)e.getSource();
		    debugPrintln(3, "activated module frame " + internalFrame.hashCode());
		    nameField.requestFocus();
		}
	    };
	addInternalFrameListener(frameListener);
	// user code end
    }

    public Icon getFrameIcon() {
	return PowerloomApp.getInstance().getImage("resources/images/edit-module.gif");
    }


    private PLSurrogateContainer makeBlankSurrogateContainer() {
	List blankSurrogates = new ArrayList();
	PLSurrogateContainer blankSurrogateContainer = new PLSurrogateContainer(blankSurrogates);
	return blankSurrogateContainer;
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
     * Creation date: (4/9/2002 8:33:54 PM)
     */
    private void setIncludesList(PLSurrogateContainer supers) {
	String[] types = {"Module Holder", "Object Holder"};
	PLListModel model = new PLListModel(null, supers);
	JList newList = new PLJList(model, this, "Module Includes List", Arrays.asList(types));
	installModulePopup(getModule(), newList);
	getModulePanel().getPubIncludesPanel().setList(newList);
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/16/2002 4:28:10 PM)
     * @param module edu.isi.powerloom.gui.xmlobject.PLModule
     */
    public void setModule(PLModule module) {
	try {
	    this.module = module;
	    getModulePanel().getPubNameTextField().setText(module.attrModuleName);
	    setTitle("Editing Module " + module.attrModuleName);
	    getModulePanel().getPubDocumentationTextArea().setText(module.attrDocumentation);
	    getModulePanel().getPubDocumentationTextArea().getCaret().setDot(0);
	    getModulePanel().getPubCppNamespaceTextField().setText(module.attrCppPackage);
	    getModulePanel().getPubJavaPackageTextField().setText(module.attrJavaPackage);
	    getModulePanel().getPubLispPackageTextField().setText(module.attrLispPackage);
	    getModulePanel().getPubJavaCatchallTextField().setText(module.attrJavaCatchallClass);
	    if (module.attrAPI.equals("TRUE")) {
		getModulePanel().getPubAPICheckBox().setSelected(true);
	    } else {
		getModulePanel().getPubAPICheckBox().setSelected(false);
	    }	
	    if (module.attrCaseSensitive.equals("TRUE")) {
		getModulePanel().getPubCaseSensitiveCheckBox().setSelected(true);
	    } else {
		getModulePanel().getPubCaseSensitiveCheckBox().setSelected(false);
	    }
	    PLSurrogateContainer includes = KnowledgeManager.getInstance().getIncludesForModule(module);
	    setIncludesList(includes);
	    PLSurrogateContainer uses = KnowledgeManager.getInstance().getUsesForModule(module);
	    setUsesList(uses);
	    PLSurrogateContainer surrogates = KnowledgeManager.getInstance().getShadowedSurrogatesForModule(module);
	    setShadowList(surrogates);
	} catch (Exception e) {
	    handleException(e);
	}
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/9/2002 8:33:54 PM)
     */
    private void setShadowList(PLSurrogateContainer supers) {
	String[] types = {"Symbol Holder", "Object Holder"};
	PLListModel model = new PLListModel(null, supers);
	JList newList = new PLJList(model, this, "Shadow List", Arrays.asList(types));
	installShadowPopup(getModule(), newList);
	getModulePanel().getPubShadowPanel().setList(newList);
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/30/2002 5:19:48 PM)
     * @param name java.lang.String
     * @param supers edu.isi.powerloom.gui.xmlobject.PLSurrogateContainer
     */
    public void setupNewModule(String name, PLSurrogateContainer supers) {
	getModulePanel().getPubNameTextField().setText(name);
	setTitle("Editing Module " + name);
	getModulePanel().getPubNameTextField().selectAll();
	setIncludesList(supers);
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/9/2002 8:33:54 PM)
     */
    private void setUsesList(PLSurrogateContainer supers) {
	String[] types = {"Module Holder", "Object Holder"};
	PLListModel model = new PLListModel(null, supers);
	JList newList = new PLJList(model, this, "Module Uses List", Arrays.asList(types));
	installModulePopup(getModule(), newList);
	getModulePanel().getPubUsesPanel().setList(newList);
    }

    /**
     * Implementation of PLFrame
     */
    public PLModule getModule() {
	return module;
    }

    private void installModulePopup(PLModule module, final JList list) {
	Collection listMenuItems = new ArrayList();
	Collection itemMenuItems = new ArrayList();
	Collection listOnlyMenuItems = new ArrayList();
	itemMenuItems.add(NavigateAction.class);
	itemMenuItems.add(CopyAction.class);
	PopupUtils.installListPopup(module, list, listMenuItems, itemMenuItems, listOnlyMenuItems, null, null);
    }

    private void installShadowPopup(PLModule module, final JList list) {
	Collection listMenuItems = new ArrayList();
	Collection itemMenuItems = new ArrayList();
	Collection listOnlyMenuItems = new ArrayList();
	itemMenuItems.add(CutAction.class);
	itemMenuItems.add(CopyAction.class);
	itemMenuItems.add(DeleteAction.class);
	listOnlyMenuItems.add(PasteAction.class);
	PopupUtils.installListPopup(module, list, listMenuItems, itemMenuItems, listOnlyMenuItems, null, null);
    }
}
