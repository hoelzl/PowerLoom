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


// Version: InstanceFrame.java,v 1.22 2010/02/04 05:17:41 hans Exp

package edu.isi.powerloom.gui.components;

import javax.swing.*;
import javax.swing.event.*;
import java.util.*;
import java.net.*;

import edu.isi.powerloom.gui.xmlobject.*;
import edu.isi.powerloom.gui.serverinterface.*;
import edu.isi.powerloom.gui.common.*;
import static edu.isi.powerloom.gui.common.Utils.*;

/**
 * Frame for editing instances.
 *
 * @since 4/1/2002 7:52:32 PM
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 * @see edu.isi.powerloom.gui.components.InstancePanel InstancePanel
 */
public class InstanceFrame extends PLFrame {
    private PowerloomApp app;
    private JPanel ivjJInternalFrameContentPane = null;
    private java.awt.BorderLayout ivjJInternalFrameContentPaneBorderLayout = null;
    private InstancePanel ivjInstancePanel = null;
    public edu.isi.powerloom.gui.xmlobject.PLModule savedModule;
    public edu.isi.powerloom.gui.xmlobject.PLInstance savedInstance;
    private boolean newInstance;
    private java.util.List originalSupers = new ArrayList();
    private java.lang.String savedName;
    private java.lang.String savedDocumentation;
    /**
     * ConceptFrame2 constructor comment.
     */
    public InstanceFrame() {
	super();
	initialize();
    }
    /**
     * ConceptFrame2 constructor comment.
     * @param title java.lang.String
     */
    public InstanceFrame(String title) {
	super(title);
    }
    /**
     * ConceptFrame2 constructor comment.
     * @param title java.lang.String
     * @param resizable boolean
     */
    public InstanceFrame(String title, boolean resizable) {
	super(title, resizable);
    }
    /**
     * ConceptFrame2 constructor comment.
     * @param title java.lang.String
     * @param resizable boolean
     * @param closable boolean
     */
    public InstanceFrame(String title, boolean resizable, boolean closable) {
	super(title, resizable, closable);
    }
    /**
     * ConceptFrame2 constructor comment.
     * @param title java.lang.String
     * @param resizable boolean
     * @param closable boolean
     * @param maximizable boolean
     */
    public InstanceFrame(String title, boolean resizable, boolean closable, boolean maximizable) {
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
    public InstanceFrame(String title, boolean resizable, boolean closable, boolean maximizable, boolean iconifiable) {
	super(title, resizable, closable, maximizable, iconifiable);
    }

    /**
     * Insert the method's description here.
     * Creation date: (4/18/2002 3:07:55 PM)
     */
    public void commitInstance() {
	try {
	    String instanceDefinition = createInstanceDefinition();
	    debugPrintln(3, "instance definition: " + instanceDefinition);
	    boolean isRenamedInstance = !getInstancePanel().getPubNameTextField().getText().equals(savedName);
	    PLModule module = (PLModule)getInstancePanel().getPubModuleComboBox().getSelectedItem();
	    KnowledgeManager.getInstance().evaluateLogicCommand(module, instanceDefinition);
	    if (isRenamedInstance) {
		setNewInstance(true);
		debugPrintln(3, "***** popup dialog here to ask whether to copy props, delete old instance");
	    }
	    updateDocumentation();
	    updateTypePropositions();
	    // since props for this instance may have been updated
	    KnowledgeManager.getInstance().invalidatePropositionCaches();
	    PLEditEvent event = new PLEditEvent(this, PLInstance.class);
	    fireEditPerformed(event);
	} catch (Exception e) {
	    handleException(e);
	} // end of try-catch
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/18/2002 3:08:38 PM)
     * @return java.lang.String
     */
    public String createInstanceDefinition() {
	String name = getInstancePanel().getPubNameTextField().getText();
	// todo: assert documentation proposition
	String result = "(DEFOBJECT " + name + ")";
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
     * Insert the method's description here.
     * Creation date: (4/22/2002 8:45:21 PM)
     * @return edu.isi.powerloom.gui.xmlobject.PLRelation
     */
    private PLInstance getInstance() {
	try {
	    String name = getInstancePanel().getPubNameTextField().getText();
	    debugPrintln(3, " getInstance name = " + name);
	    PLSurrogate surrogate = KnowledgeManager.getInstance().findOrCreateSurrogate(PLInstance.class, name);
	    PLInstance instance = (PLInstance)surrogate.getValue();
	    debugPrintln(3, " getInstance instance = " + instance);
	    if (instance == null) {
		PLInstance newInstance = new PLInstance();
		debugPrintln(3, " getInstance newInstance = " + newInstance);
		newInstance.attrInstanceName = name;
		newInstance.setUndefined(true);
		surrogate.setValue(newInstance);
	    } else {
		debugPrintln(3, " getInstance instancehash = " + instance.hashCode());
	    }
	    debugPrintln(3, " getInstance surrgate val = " + surrogate.getValue());
	    return (PLInstance)surrogate.getValue();
	} catch (Exception e) {
	    handleException(e);  // todo handle correctly
	}
	return null;
    }
    /**
     * Return the InstancePanel property value.
     * @return redesign.gui.components.InstancePanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private InstancePanel getInstancePanel() {
	if (ivjInstancePanel == null) {
	    try {
		ivjInstancePanel = new edu.isi.powerloom.gui.components.InstancePanel(this);
		ivjInstancePanel.setName("InstancePanel");
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
		getJInternalFrameContentPane().add(getInstancePanel(), "Center");
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
	return (PLModule)getInstancePanel().getPubModuleComboBox().getSelectedItem();
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/17/2002 3:40:01 PM)
     * @return edu.isi.powerloom.gui.xmlobject.PLInstance
     */
    public edu.isi.powerloom.gui.xmlobject.PLInstance getSavedInstance() {
	return savedInstance;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/17/2002 3:39:49 PM)
     * @return edu.isi.powerloom.gui.xmlobject.PLModule
     */
    public edu.isi.powerloom.gui.xmlobject.PLModule getSavedModule() {
	return savedModule;
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
	    setName("InstanceFrame");
	    setTitle("Editing Instance yyy");
	    setClosable(true);
	    setIconifiable(true);
	    setMaximizable(true);
	    setSize(700, 800);
	    setResizable(true);
	    setContentPane(getJInternalFrameContentPane());
	} catch (java.lang.Throwable ivjExc) {
	    handleException(ivjExc);
	}
	// user code begin {2}
	getInstancePanel().setParentFrame(this);
	setTitle("New Instance");
	app = PowerloomApp.getInstance();
	getInstancePanel().postCreateInitialize();

	// Ensure name field gets focus when the frame is opened
	final JTextField nameField = getInstancePanel().getPubNameTextField();
	InternalFrameListener frameListener = new InternalFrameAdapter() {
		public void internalFrameActivated(InternalFrameEvent e) {
		    JInternalFrame internalFrame = (JInternalFrame)e.getSource();
		    debugPrintln(3, "activated instnace frame " + internalFrame.hashCode());
		    nameField.requestFocus();
		}
	    };
	addInternalFrameListener(frameListener);
	
	// experimental:

	//pack();
	// user code end
    }

    public Icon getFrameIcon() {
	return PowerloomApp.getInstance().getImage("resources/images/edit-instance.gif");
    }

    /**
     * Insert the method's description here.
     * Creation date: (5/1/2002 11:38:24 AM)
     * @return boolean
     */
    public boolean isNewInstance() {
	return newInstance;
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
	debugPrintln(3, "performing edit in instanceFrame");
	if (e.getEditType() == PLProposition.class) {
	    refreshPropositionsPanel();
	}
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/15/2002 10:16:10 PM)
     */
    public void refreshPropositionsPanel() {
	debugPrintln(3, "refresh prop panel: getinstnace = " + getInstance());
	if ((getInstance() == null) || (getModule() == null))
	    return;
	try {
	    PLSurrogateContainer propositions = 
		KnowledgeManager.getInstance().getPropositionsForInstance(getModule(), getInstance());
	    setPropositionsList(propositions);
	    setHTML(propositions);
	} catch (Exception e) {
	    handleException(e);
	}
    }

    /**
     * Set html pane to either a url proposition or a url-image proposition.  URL-propositions 
     * take precedence over url-image props.
     */
    private void setHTML(PLSurrogateContainer propositions) {
	try {
	    String urlString = findURLString(propositions);
	    if (urlString != null) {
		URL url = new URL(urlString);
		getInstancePanel().getHTMLPanel().setURL(url);
		getInstancePanel().getHTMLPanel().setPreferredSize(getInstancePanel().getHTMLPanel().getViewport().getView().getPreferredSize());
		revalidate();
		return;
	    }


	    String imageURLString = findImageURLString(propositions);
	    if (imageURLString != null) {
		String html = "<HTML><HEAD></HEAD><BODY><CENTER><IMG SRC=\"" + imageURLString + "\"></CENTER></BODY></HTML>";
		getInstancePanel().getHTMLPanel().setHTML(html);
		getInstancePanel().getHTMLPanel().setPreferredSize(getInstancePanel().getHTMLPanel().getViewport().getView().getPreferredSize());
		revalidate();
	    }
	} catch (Exception e) {
	    PowerloomApp.getInstance().handleException(e);
	}
    }

    private String findImageURLString(PLSurrogateContainer propositions) {
	return findURLStringHelper("image-url", propositions);
    }

    private String findURLString(PLSurrogateContainer propositions) {
	return findURLStringHelper("(url", propositions);
    }

    private String findURLStringHelper(String key, PLSurrogateContainer propositions) {
	List surrogates = propositions.getSurrogates();
	Iterator iter = surrogates.iterator();
	while(iter.hasNext()) {
	    PLProposition prop = (PLProposition)((PLSurrogate)iter.next()).getValue();
	    String propString = prop.getID().toLowerCase();
	    int keyIndex = propString.indexOf(key);
	    debugPrintln(3, "  keyindex = " + keyIndex);
	    if (keyIndex >= 0) {
		//assume prop is well-formed
		int firstQuoteIndex = propString.indexOf('"', keyIndex);
		int secondQuoteIndex = propString.indexOf('"', firstQuoteIndex + 1);
		String urlBodyString = prop.getID().substring(firstQuoteIndex+1, secondQuoteIndex);
		String result = null;
		boolean isAbsoluteURL = (propString.indexOf("http://") >= 0);
		if (isAbsoluteURL) {
		    result = urlBodyString;
		} else {
		    Preferences prefs = Preferences.getInstance();
		    String host = prefs.getProperty(Preferences.HOST);
		    String port = prefs.getProperty(Preferences.PORT);
		    result = "http://" + host + ":" + port + urlBodyString;
		}
		debugPrintln(3, "url: " + result);
		return result;
	    }
	}
	return null;
    }

    /**
     * Insert the method's description here.
     * Creation date: (4/17/2002 2:16:33 PM)
     * @param instance edu.isi.powerloom.gui.xmlobject.PLInstance
     */
    public void setInstance(PLModule module, edu.isi.powerloom.gui.xmlobject.PLInstance instance) {
	debugPrintln(3, "setting instance: " + instance + ", module = " + module);
	setInstanceName(instance.getID());
	setTitle("Editing Instance " + instance.getID());	
	savedModule = module;
	savedInstance = instance;
	try {
	    String documentation = 
		KnowledgeManager.getInstance().getDocumentationForInstance(module, instance);
	    savedDocumentation = documentation;
	    PLSurrogateContainer propositions = 
		KnowledgeManager.getInstance().getPropositionsForInstance(module, instance);
	    getInstancePanel().getPubDocumentationTextArea().setText(documentation);
	    getInstancePanel().getPubDocumentationTextArea().getCaret().setDot(0);
	    
	    String[] types = {"Proposition Holder", "Object Holder"};
	    PLListModel model = new PLListModel(module, propositions);
	    JList propList = new PLPropositionList(model, this, "Instance Editor Propositions List", Arrays.asList(types));

	    installPropositionPopup(module, propList);
	    getInstancePanel().getPubPropositionsPanel().setList(propList);
	    setHTML(propositions);
	    installHTMLBrowserPopup(getInstancePanel().getHTMLPanel());
	    PLSurrogateContainer supers = KnowledgeManager.getInstance().getTypesForInstance(module, instance);
	    setSupers(supers);
	} catch (Exception e) {
	    handleException(e);
	} // end of try-catch
    }

    private void installPropositionPopup(PLModule module, final JList list) {
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

    /**
     * Insert the method's description here.
     * Creation date: (5/1/2002 1:36:49 PM)
     * @param name java.lang.String
     */
    public void setInstanceName(String name) {
	getInstancePanel().getPubNameTextField().setText(name);
	getInstancePanel().getPubNameTextField().select(0, name.length());
	savedName = name;
    }
    /**
     * Insert the method's description here.
     * Creation date: (5/1/2002 11:38:24 AM)
     * @param newNewInstance boolean
     */
    public void setNewInstance(boolean newNewInstance) {
	newInstance = newNewInstance;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/12/2002 2:50:19 PM)
     * @param props edu.isi.powerloom.gui.xmlobject.PLSurrogateContainer
     */
    private void setPropositionsList(PLSurrogateContainer props) {
	String[] types = {"Proposition Holder", "Object Holder"};
	PLListModel model = new PLListModel(getModule(), props);
	JList newList = new PLPropositionList(model, this, "Instance Editor Propositions List", Arrays.asList(types));

	getInstancePanel().getPubPropositionsPanel().setList(newList);
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/17/2002 3:40:01 PM)
     * @param newSavedInstance edu.isi.powerloom.gui.xmlobject.PLInstance
     */
    public void setSavedInstance(edu.isi.powerloom.gui.xmlobject.PLInstance newSavedInstance) {
	savedInstance = newSavedInstance;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/17/2002 3:39:49 PM)
     * @param newSavedModule edu.isi.powerloom.gui.xmlobject.PLModule
     */
    public void setSavedModule(edu.isi.powerloom.gui.xmlobject.PLModule newSavedModule) {
	savedModule = newSavedModule;
    }
    /**
     * Insert the method's description here.
     * Creation date: (5/1/2002 12:31:10 PM)
     * @param supers edu.isi.powerloom.gui.xmlobject.PLSurrogateContainer
     */
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

    private void installHTMLBrowserPopup(final HTMLBrowserPane browser) {
	List menuItems = new ArrayList();
	menuItems.add(ShowBrowserAction.class);
	PopupUtils.installComponentPopup((JEditorPane)browser.getViewport().getView(), menuItems);
    }


    public void setSupers(PLSurrogateContainer supers) {
	//todo: implement next line...
	PLListModel model = new PLListModel(getModule(), supers);
	String[] types = {"Concept Holder", "Object Holder"};
	JList newList = new PLJList(model, this, "Instance Editor Supers List", Arrays.asList(types));

	installConceptPopup(getInstancePanel().getModule(), newList);
	getInstancePanel().getPubSupersPanel().setList(newList);
	Iterator iter = supers.getSurrogates().iterator();
	// save the supers that we initialized the editor with
	while (iter.hasNext()) {
	    PLConcept superType = (PLConcept)((PLSurrogate)iter.next()).getValue();
	    originalSupers.add(superType);
	}
    }
    /**
     * Insert the method's description here.
     * Creation date: (5/1/2002 11:39:55 AM)
     * @param instanceName java.lang.String
     * @param superTypes edu.isi.powerloom.gui.xmlobject.PLSurrogateContainer
     */
    public void setupNewInstance(String instanceName, PLSurrogateContainer superTypes) {
	setNewInstance(true);
	setInstanceName(instanceName);
	setSupers(superTypes);
    }
    /**
     * Insert the method's description here.
     * Creation date: (5/1/2002 3:13:57 PM)
     */
    public void updateDocumentation() {
	try {
	    PLModule module = (PLModule)getInstancePanel().getPubModuleComboBox().getSelectedItem();
	    String nameString = getInstancePanel().getPubNameTextField().getText();
	    if (!isNewInstance()) {
		String removeProp = "(RETRACT (DOCUMENTATION " + nameString + " \"" + savedDocumentation + "\"" + "))";
		KnowledgeManager.getInstance().evaluateLogicCommand(module, removeProp);
	    }
	    String docString = getInstancePanel().getPubDocumentationTextArea().getText();
	    String propString = "(ASSERT (DOCUMENTATION " + nameString + " \"" + docString + "\"" + "))";
	    KnowledgeManager.getInstance().evaluateLogicCommand(module, propString);
	} catch (Exception e) {
	    handleException(e);
	} 
    }
    /**
     * Insert the method's description here.
     * Creation date: (5/1/2002 12:21:35 PM)
     */
    public void updateTypePropositions() {
	// compare the original types with the new types for this instance,
	// remove old types, assert new types.
	PLListModel model = (PLListModel)getInstancePanel().getPubSupersPanel().getJList().getModel();
	List finalSupers = new ArrayList();
	for (int i = 0; i < model.getSize(); i++) {
	    finalSupers.add(model.getElementAt(i));
	}
	List unchangedSupers = new ArrayList(finalSupers);
	unchangedSupers.retainAll(originalSupers);  // intersection of new and original supers
	List oldSupers = new ArrayList();
	if (!isNewInstance()) {
	    oldSupers = new ArrayList(originalSupers);
	    oldSupers.removeAll(unchangedSupers);       // difference of original and unchanged
	}  // else... we're new, so there are no old supers
	List newSupers = new ArrayList(finalSupers);
	if (!isNewInstance()) {
	    newSupers.removeAll(unchangedSupers);       // difference of final and unchanged
	}
	Iterator iter;
	debugPrintln(3, "New Supers: " );
	iter = newSupers.iterator();
	while (iter.hasNext()) {
	    PLConcept type = (PLConcept)iter.next();
	    String prop = "(ASSERT (" + type.getID() + " " + getInstancePanel().getPubNameTextField().getText() + "))";
	    debugPrintln(3, "  " + prop);
	    try {
		PLModule module = (PLModule)getInstancePanel().getPubModuleComboBox().getSelectedItem();
		KnowledgeManager.getInstance().evaluateLogicCommand(module, prop);
	    } catch (Exception e) {
		handleException(e);
	    }
	}
	debugPrintln(3, "Old Supers: " );
	iter = oldSupers.iterator();
	while (iter.hasNext()) {
	    PLConcept type = (PLConcept)iter.next();
	    String prop = "(RETRACT (" + type.getID() + " " + getInstancePanel().getPubNameTextField().getText() + "))";
	    debugPrintln(3, "  " + prop);
	    try {
		PLModule module = (PLModule)getInstancePanel().getPubModuleComboBox().getSelectedItem();
		KnowledgeManager.getInstance().evaluateLogicCommand(module, prop);
	    } catch (Exception e) {
		handleException(e);
	    }
	}
	
    }
}
