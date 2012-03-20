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


// Version: PowerloomApp.java,v 1.62 2010/04/19 22:40:11 hans Exp

package edu.isi.powerloom.gui.components;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import java.util.*;
import java.io.*;
import java.net.URL;
import edu.isi.powerloom.gui.common.*;
import static edu.isi.powerloom.gui.common.Utils.*;
import edu.isi.powerloom.gui.xmlobject.*;
import edu.isi.powerloom.gui.serverinterface.*;

/**
 * Main class for the Powerloom GUI application.  Contains all application
 * frame code such as setting up menus, etc.  Also contains code and data which
 * is common to the entire application.
 *
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */
public class PowerloomApp extends JFrame {
    // In development mode we are not loading from a jar
    // and need to know where to find resources:
    public static final boolean DEVELOPMENT_MODE = false;  
    public static final String RESOURCE_BASE_PATH = "/nfs/topaz/melz/cvscheckout/powerloom/sources/powerloom-gui/";

    public static final String HELP_URL = "http://www.isi.edu/isd/LOOM/PowerLoom/documentation/index.html";

    // not used yet, but may be used eventually to customize l&f...
    //public static Color BACKGROUND_COLOR = new java.awt.Color(0xb6e0f4);
    //public static Color FOREGROUND_COLOR = new java.awt.Color(0x94cae8);

    public static final String POWERLOOM_LOGO_PATH = "resources/images/powerloom-logo-large.gif";
    public static final String POWERLOOM_WINDOW_ICON_PATH =
        "resources/images/powerloom-logo-tiny.gif";
    public static final String POWERLOOM_COPYRIGHT_HOLDER = "USC Information Sciences Institute";
    public static final String POWERLOOM_COPYRIGHT_YEARS = "1997-"
        + java.util.Calendar.getInstance().get(java.util.Calendar.YEAR);
    public static final String POWERLOOM_GUI_COPYRIGHT_YEARS = "2002-"
        + java.util.Calendar.getInstance().get(java.util.Calendar.YEAR);
    // Note: we don't have a POWERLOOM_VERSION string here, since the GUI might connect to
    // different servers potentially running different versions of PowerLoom; if we want that,
    // we should add an API method that retrieves the version string from the server.
    public static final String POWERLOOM_GUI_VERSION = "1.0.0";

    private PLObject mostRecentlyTouchedObject;
    private JMenuItem ivjAbout_BoxMenuItem = null;
    private JButton ivjCopyButton = null;
    private JMenuItem ivjCopyMenuItem = null;
    private JButton ivjCutButton = null;
    private JButton ivjDeleteButton = null;
    private JMenuItem ivjCutMenuItem = null;
    private JMenuItem ivjDeleteMenuItem = null;
    private JMenu ivjEditMenu = null;
    private JMenu ivjObjectMenu = null;
    private JMenu ivjQueryMenu = null;
    IvjEventHandler ivjEventHandler = new IvjEventHandler();
    private JMenuItem ivjExitMenuItem = null;
    private JMenuItem ivjFind_ReplaceMenuItem = null;
    private JMenuItem ivjHelp_TopicsMenuItem = null;
    private JMenu ivjHelpMenu = null;
    private JPanel ivjJFrameContentPane = null;
    private JSeparator ivjJSeparator1 = null;
    private JSeparator ivjJSeparator2 = null;
    private JSeparator ivjJSeparator3 = null;
    private JButton ivjPasteButton = null;
    private JMenuItem ivjPasteMenuItem = null;
    private JMenuBar ivjPowerloomAppJMenuBar = null;
    private JMenuItem ivjRedoMenuItem = null;
    private JMenuItem ivjSaveMenuItem = null;
    private JMenuItem ivjSelect_AllMenuItem = null;
    private JMenuItem ivjStatusbarMenuItem = null;
    private JPanel ivjStatusBarPane = null;
    private JLabel ivjStatusMsg1 = null;
    private JLabel ivjStatusMsg2 = null;
    private JMenuItem ivjToolbarMenuItem = null;
    private JMenuItem ivjRefreshMenuItem = null;
    private JMenuItem ivjBackMenuItem = null;
    private JMenuItem ivjForwardMenuItem = null;
    private JToolBar ivjToolBarPane = null;
    private JMenuItem ivjUndoMenuItem = null;
    private JMenu ivjViewMenu = null;
    private JMenuItem ivjBrowseMenuItem = null;
    private JMenuItem ivjConnectMenuItem = null;
    private JMenuItem ivjOpenConsoleMenuItem = null;
    private JSeparator ivjJSeparator4 = null;
    private JMenu ivjKBMenu = null;
    private JMenuItem ivjLoadMenuItem = null;
    private JMenuItem ivjScrapbookMenuItem = null;
    private JDesktopPane ivjJDesktopPane = null;
    private JSeparator ivjJSeparator5 = null;
    private JMenuItem ivjNewConceptMenuItem = null;
    private JMenuItem ivjNewInstanceMenuItem = null;
    private JMenuItem ivjNewModuleMenuItem = null;
    private JMenuItem ivjNewPropositionMenuItem = null;
    private JMenuItem ivjNewRelationMenuItem = null;
    private JMenuItem ivjNewRuleMenuItem = null;
    public edu.isi.powerloom.gui.xmlobject.PLModule mostRecentlyTouchedModule;
    private java.util.Collection browserFrames = new java.util.ArrayList();
    private java.util.Collection conceptFrames = new java.util.ArrayList();
    private java.util.Collection relationFrames = new java.util.ArrayList();
    private java.util.Collection instanceFrames = new java.util.ArrayList();
    private java.util.Collection propositionFrames = new java.util.ArrayList();
    private JMenuItem ivjClearModuleMenuItem = null;
    private JSeparator ivjJSeparator6 = null;
    private JMenuItem ivjPreferencesMenuItem = null;
    private int windowCount;
    private JSeparator ivjJSeparator7 = null;
    private JMenuItem ivjQueryMenuItem = null;
    private JMenuItem ivjExtensionMenuItem = null;
    private JMenuItem ivjSearchMenuItem = null;
    private JMenuItem ivjLocalLoadMenuItem = null;
    private JMenuItem ivjLocalSaveMenuItem = null;
    private JMenuItem ivjRemoteLoadMenuItem = null;
    private JMenuItem ivjRemoteSaveMenuItem = null;

    private static PowerloomApp aPowerloomApp;
    private ExpressionEditorPanel currentlySelectedEditor = null;
    private EditInstanceCellPanel currentlySelectedInstanceCellEditor = null;
    private ActionComponent currentlySelectedActionComponent = null;
    private static java.util.Hashtable cachedIcons = new java.util.Hashtable();
    private JMenu ivjNavigationMenu = null;
    private JMenu ivjWindowMenu = null;
    private Action ivjCutAction = null;
    private Action ivjDeleteAction = null;
    private Action ivjPasteAction = null;
    private Action ivjCopyAction = null;
    private Action ivjQueryAction = null;
    private JMenuItem ivjEditObjectMenuItem = null;
    private Action ivjEditObjectAction = null;
    private java.util.List editListeners = new ArrayList();
    private boolean doingRightClickMenu;
    private PLServerInfo serverInfo;
    private JMenuItem ivjMetalThemeMenuItem = null;
    private JMenuItem ivjDemoThemeMenuItem = null;
    private JMenuItem ivjBigThemeMenuItem = null;

    // Tricky: since the KeyMap, Parser, and Completor are all singleton objects, we need
    // to keep track of the curently-selected expression editor in order for
    // completions and key actions to be associated with the correct editor.

    public void setSelectedEditor(ExpressionEditorPanel editor) {
	currentlySelectedEditor = editor;
    }

    public ExpressionEditorPanel getSelectedEditor() {
	return currentlySelectedEditor;
    }

    public void setSelectedInstanceCellEditor(EditInstanceCellPanel editor) {
	currentlySelectedInstanceCellEditor = editor;
    }

    public EditInstanceCellPanel getSelectedInstanceCellEditor() {
	return currentlySelectedInstanceCellEditor;
    }


    public void setSelectedActionComponent(ActionComponent component) {
	currentlySelectedActionComponent = component;
    }

    public ActionComponent getSelectedActionComponent() {
	return currentlySelectedActionComponent;
    }

    public void doActionOnCurrentlySelectedComponent() {
	if (getSelectedActionComponent() != null) {
	    getSelectedActionComponent().doAction();
	}
    }


    class IvjEventHandler implements java.awt.event.ActionListener {
	public void actionPerformed(java.awt.event.ActionEvent e) {
	    if (e.getSource() == PowerloomApp.this.getExitMenuItem()) 
		connEtoM1(e);
	    if (e.getSource() == PowerloomApp.this.getToolbarMenuItem()) 
		connEtoC1(e);
	    if (e.getSource() == PowerloomApp.this.getStatusbarMenuItem()) 
		connEtoC2(e);
	    if (e.getSource() == PowerloomApp.this.getAbout_BoxMenuItem()) 
		connEtoC3(e);
	    if (e.getSource() == PowerloomApp.this.getBrowseMenuItem()) 
		connEtoC4(e);
	    if (e.getSource() == PowerloomApp.this.getNewPropositionMenuItem()) 
		connEtoC9(e);
	    if (e.getSource() == PowerloomApp.this.getNewRelationMenuItem()) 
		connEtoC10(e);
	    if (e.getSource() == PowerloomApp.this.getNewInstanceMenuItem()) 
		connEtoC11(e);
	    if (e.getSource() == PowerloomApp.this.getScrapbookMenuItem()) 
		connEtoC12(e);
	    if (e.getSource() == PowerloomApp.this.getNewModuleMenuItem()) 
		connEtoC13(e);
	    if (e.getSource() == PowerloomApp.this.getNewRuleMenuItem()) 
		connEtoC5(e);
	    if (e.getSource() == PowerloomApp.this.getNewConceptMenuItem()) 
		connEtoC6(e);
	    if (e.getSource() == PowerloomApp.this.getConnectMenuItem()) 
		connEtoC8(e);
	    if (e.getSource() == PowerloomApp.this.getClearModuleMenuItem()) 
		connEtoC15(e);
	    if (e.getSource() == PowerloomApp.this.getPreferencesMenuItem()) 
		connEtoC17(e);
	    if (e.getSource() == PowerloomApp.this.getQueryMenuItem()) 
		connEtoC7(e);
	    if (e.getSource() == PowerloomApp.this.getSearchMenuItem()) 
		connEtoC18(e);
	    if (e.getSource() == PowerloomApp.this.getRemoteLoadMenuItem()) 
		connEtoC19(e);
	    if (e.getSource() == PowerloomApp.this.getRemoteSaveMenuItem()) 
		connEtoC20(e);
	    if (e.getSource() == PowerloomApp.this.getExtensionMenuItem()) 
		connEtoC21(e);
	    if (e.getSource() == PowerloomApp.this.getLocalLoadMenuItem()) 
		connEtoC22(e);
	    if (e.getSource() == PowerloomApp.this.getLocalSaveMenuItem()) 
		connEtoC23(e);
	    if (e.getSource() == PowerloomApp.this.getRefreshMenuItem()) 
		connEtoC24(e);
	    if (e.getSource() == PowerloomApp.this.getBackMenuItem()) 
		connEtoC25(e);
	    if (e.getSource() == PowerloomApp.this.getForwardMenuItem()) 
		connEtoC26(e);
	    if (e.getSource() == PowerloomApp.this.getOpenConsoleMenuItem()) 
		connEtoC27(e);
	    if (e.getSource() == PowerloomApp.this.getHelp_TopicsMenuItem()) 
		connEtoC28(e);
	    if (e.getSource() == PowerloomApp.this.getMetalThemeMenuItem()) 
		connEtoC29(e);
	    if (e.getSource() == PowerloomApp.this.getDemoThemeMenuItem()) 
		connEtoC30(e);
	    if (e.getSource() == PowerloomApp.this.getBigThemeMenuItem()) 
		connEtoC31(e);

	};
    };
    /**
     * PowerloomApp constructor comment.
     */
    public PowerloomApp() {
	super();
	initialize();
    }
    /**
     * PowerloomApp constructor comment.
     * @param title java.lang.String
     */
    public PowerloomApp(String title) {
	super(title);
	initialize();
    }

    /**
     * Register, position, and show frame 
     */
    // NOTE: THIS MIGHT BE OBSOLETE!  TODO: CHECK THIS AND DELETE
    public void displayFrame(final JInternalFrame frame) {
	WindowSelectAction selectAction = new WindowSelectAction(frame);
	JMenuItem item = new JMenuItem(selectAction) {
		public boolean getState() {
		    if (getPubJDesktopPane() == null) {
			return false; 
		    } else {
			JInternalFrame selectedFrame = getPubJDesktopPane().getSelectedFrame();
			return ((selectedFrame != null) && (selectedFrame == frame));
		    }
		}
		public Icon getIcon() {
		    return frame.getFrameIcon();
		}
	    };
	getWindowMenu().add(item);
	InternalFrameListener listener = new InternalFrameAdapter() {
		public void internalFrameClosing(InternalFrameEvent e) {
		    JInternalFrame closingFrame = (JInternalFrame)e.getSource();
		    String title = closingFrame.getTitle();
		    debugPrintln(3, "internalframe: " + title + " is being closed.");
		    removeFrameFromWindowMenu(closingFrame);
		}
	    };
	frame.addInternalFrameListener(listener);
	positionFrame(frame);
	frame.show();
    }

    void removeFrameFromWindowMenu(JInternalFrame frame) {
	JMenu windowMenu = getWindowMenu();
	JMenuItem foundItem = null;
	for (int i = 0; i < windowMenu.getItemCount(); i++) {
	    if (((WindowSelectAction)windowMenu.getItem(i).getAction()).getFrame() == frame) {
		foundItem = windowMenu.getItem(i);
		break;
	    }
	}
	if (foundItem == null) {
	    return;
	}
	windowMenu.remove(foundItem);
    }

    /**
     * Comment
     */
    public void browseMenuItem_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	BrowserFrame4 frame = new BrowserFrame4();
	browserFrames.add(frame);
	Preferences prefs = Preferences.getInstance();
	String host = prefs.getProperty(prefs.HOST);
	String port = prefs.getProperty(prefs.PORT);
	frame.setTitle("Browsing Knowledge Base@" + host + ":" + port);
	InternalFrameListener listener = new InternalFrameAdapter() {
		public void internalFrameActivated(InternalFrameEvent e) {
		    JInternalFrame internalFrame = (JInternalFrame)e.getSource();
		    debugPrintln(3, "activated browserframe " + internalFrame.hashCode());
		    updateNavigationMenuState(((BrowserFrame4)internalFrame).getPubBrowserPanel().getPowerloomTrees());
		}
		public void internalFrameDeactivated(InternalFrameEvent e) {
		    JInternalFrame internalFrame = (JInternalFrame)e.getSource();
		    debugPrintln(3, "deactivated browserframe " + internalFrame.hashCode());
		    updateNavigationMenuState(null);
		}
		public void internalFrameClosed(InternalFrameEvent e) {
		    JInternalFrame internalFrame = (JInternalFrame)e.getSource();
		    debugPrintln(3, "closed browserframe " + internalFrame.hashCode());
		    updateNavigationMenuState(null);
		}
	    };
	frame.addInternalFrameListener(listener);
	frame.displayFrame();
    }
    /**
     * Comment
     */
    public void clearModuleMenuItem_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	KBClearFrame frame = new KBClearFrame();
	frame.displayFrame();
    }
    /**
     * Comment
     */
    public void connectMenuItem_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	ServerChooserDialog dialog = new ServerChooserDialog();
	dialog.setModal(true);
	dialog.show();
	String host = dialog.getHostResult();
	int port = dialog.getPortResult();
	boolean saveSettings = dialog.isSaveSettings();
	debugPrintln(3, "host = " + host + ", port = " + port + ", save = " + saveSettings);
	if ((host != null) && (port >=0)) {
	    Preferences prefs = Preferences.getInstance();
	    prefs.setProperty(Preferences.HOST, host);
	    prefs.setProperty(Preferences.PORT, Integer.toString(port));
            SoapSender.clearServerURL(); // clear any previously cached value
	    if (saveSettings) {
		prefs.save();
	    }
	}
    }

    public void openConsoleMenuItem_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	ConsoleFrame frame = new ConsoleFrame();
	frame.displayFrame();
    }

    public void help_TopicsMenuItem_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	try {
	    HTMLBrowserFrame frame = new HTMLBrowserFrame(new URL(HELP_URL));
	    frame.setTitle("Help Browser");
	    frame.displayFrame();
	} catch (Exception e) {
	    handleException(e);
	}
    }

    /**
     * connEtoC1:  (ToolbarMenuItem.action.actionPerformed(java.awt.event.ActionEvent) --> PowerloomApp.viewToolBar()V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC1(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.viewToolBar();
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }
    /**
     * connEtoC10:  (NewRelationMenuItem.action.actionPerformed(java.awt.event.ActionEvent) --> PowerloomApp.newRelationMenuItem_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC10(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.newRelationMenuItem_ActionPerformed(arg1);
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }
    /**
     * connEtoC11:  (NewInstanceMenuItem.action.actionPerformed(java.awt.event.ActionEvent) --> PowerloomApp.newInstanceMenuItem_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC11(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.newInstanceMenuItem_ActionPerformed(arg1);
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }
    /**
     * connEtoC12:  (ScrapbookMenuItem.action.actionPerformed(java.awt.event.ActionEvent) --> PowerloomApp.scrapbookMenuItem_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC12(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.scrapbookMenuItem_ActionPerformed(arg1);
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }
    /**
     * connEtoC13:  (NewModuleMenuItem.action.actionPerformed(java.awt.event.ActionEvent) --> PowerloomApp.newModuleMenuItem_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC13(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.newModuleMenuItem_ActionPerformed(arg1);
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }
    /**
     * connEtoC15:  (ClearModuleMenuItem.action.actionPerformed(java.awt.event.ActionEvent) --> PowerloomApp.clearModuleMenuItem_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC15(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.clearModuleMenuItem_ActionPerformed(arg1);
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }
    /**
     * connEtoC17:  (PreferencesMenuItem.action.actionPerformed(java.awt.event.ActionEvent) --> PowerloomApp.preferencesMenuItem_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC17(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.preferencesMenuItem_ActionPerformed(arg1);
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }
    /**
     * connEtoC18:  (SearchMenuItem.action.actionPerformed(java.awt.event.ActionEvent) --> PowerloomApp.searchMenuItem_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC18(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.searchMenuItem_ActionPerformed(arg1);
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }
    /**
     * connEtoC19:  (RemoteLoadMenuItem.action.actionPerformed(java.awt.event.ActionEvent) --> PowerloomApp.remoteLoadMenuItem_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC19(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.remoteLoadMenuItem_ActionPerformed(arg1);
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }
    /**
     * connEtoC20:  (RemoteSaveMenuItem.action.actionPerformed(java.awt.event.ActionEvent) --> PowerloomApp.remoteLoadMenuItem_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC20(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.remoteSaveMenuItem_ActionPerformed(arg1);
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }
    private void connEtoC21(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.extensionMenuItem_ActionPerformed(arg1);
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }


    private void connEtoC22(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.localLoadMenuItem_ActionPerformed(arg1);
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }

    private void connEtoC23(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.localSaveMenuItem_ActionPerformed(arg1);
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }

    private void connEtoC24(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.refreshMenuItem_ActionPerformed(arg1);
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }

    private void connEtoC25(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.backMenuItem_ActionPerformed(arg1);
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }


    private void connEtoC26(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.forwardMenuItem_ActionPerformed(arg1);
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }

    private void connEtoC27(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.openConsoleMenuItem_ActionPerformed(arg1);
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }

    private void connEtoC28(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.help_TopicsMenuItem_ActionPerformed(arg1);
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }

    private void connEtoC29(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.metalThemeMenuItem_ActionPerformed(arg1);
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }

    private void connEtoC30(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.demoThemeMenuItem_ActionPerformed(arg1);
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }

    private void connEtoC31(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.bigThemeMenuItem_ActionPerformed(arg1);
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }

    /**
     * connEtoC2:  (StatusbarMenuItem.action.actionPerformed(java.awt.event.ActionEvent) --> PowerloomApp.viewStatusBar()V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC2(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.viewStatusBar();
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }
    /**
     * connEtoC3:  (About_BoxMenuItem.action.actionPerformed(java.awt.event.ActionEvent) --> PowerloomApp.showAboutBox()V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC3(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.showAboutBox();
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }
    /**
     * connEtoC4:  (BrowseMenuItem.action.actionPerformed(java.awt.event.ActionEvent) --> PowerloomApp.browseMenuItem_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC4(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.browseMenuItem_ActionPerformed(arg1);
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }
    /**
     * connEtoC5:  (NewRuleMenuItem.action.actionPerformed(java.awt.event.ActionEvent) --> PowerloomApp.newRuleMenuItem_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC5(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.newRuleMenuItem_ActionPerformed(arg1);
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }
    /**
     * connEtoC6:  (EditMenuItem.action.actionPerformed(java.awt.event.ActionEvent) --> PowerloomApp.editMenuItem_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC6(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.newConcept2MenuItem_ActionPerformed(arg1);
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }
    /**
     * connEtoC7:  (EditMenuItem.action.actionPerformed(java.awt.event.ActionEvent) --> PowerloomApp.editMenuItem_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC7(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.queryMenuItem_ActionPerformed(arg1);
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }
    /**
     * connEtoC8:  (ConnectMenuItem.action.actionPerformed(java.awt.event.ActionEvent) --> PowerloomApp.connectMenuItem_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC8(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.connectMenuItem_ActionPerformed(arg1);
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }
    /**
     * connEtoC9:  (NewPropositionMenuItem.action.actionPerformed(java.awt.event.ActionEvent) --> PowerloomApp.newPropositionMenuItem_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC9(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.newPropositionMenuItem_ActionPerformed(arg1);
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }
    /**
     * connEtoM1:  (ExitMenuItem.action.actionPerformed(java.awt.event.ActionEvent) --> PowerloomApp.dispose()V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoM1(java.awt.event.ActionEvent arg1) {
	try {
	    // user code begin {1}
	    // user code end
	    this.dispose();
	    // user code begin {2}
	    // user code end
	} catch (java.lang.Throwable ivjExc) {
	    // user code begin {3}
	    // user code end
	    handleException(ivjExc);
	}
    }

    public PLServerInfo getServerInfo(boolean error) {
        // If `error' raise an exception if we can't connect, otherwise, fail quietly.
	try {
	    if (serverInfo == null) {
		serverInfo = edu.isi.powerloom.gui.serverinterface.KnowledgeManager.getInstance().getServerInfo();
	    }
	} catch (Exception e) {
            if (error)
                handleException(e);
	}
	debugPrintln(1, "Retrieved serverInfo: " + ((serverInfo != null) ? serverInfo : "none, server must be down"));
	return serverInfo;
    }

    /**
     * Load preferences and do startup actions.
     * Creation date: (4/29/2002 7:59:56 AM)
     */
    public void doStartup() {
        Preferences prefs = Preferences.getInstance();
        // hc: eventually, we want to separete BROWSE_ON_START into
        // CONNECT_ON_START and BROWSE_ON_CONNECT (the latter would
        // bring up a browser window whenever we connect to a new server).
	if (prefs.getBooleanProperty(prefs.BROWSE_ON_START)) {
	    try {
		// try to connect but fail quietly, since we can't
		// raise the error pane yet, we need to do that in
		// `main' once the app is initialized and visible;
                // eventually, do login here:
		getServerInfo(false);
                if (serverInfo != null)
                    browseMenuItem_ActionPerformed(new java.awt.event.ActionEvent(this, -1, ""));
	    } catch (Exception e) {
		handleException(e);
	    }
	}
	return;
    }

    public void refreshMenuItem_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	Iterator iter = browserFrames.iterator();
	while (iter.hasNext()) {
	    BrowserFrame4 browser = (BrowserFrame4)iter.next();
	    BrowserPanel4 panel = browser.getPubBrowserPanel();
	    PowerloomTrees trees = panel.getPowerloomTrees();
	    trees.refreshEverything();
	}
    }

    public void backMenuItem_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	BrowserFrame4 browser = (BrowserFrame4)getSelectedBrowserFrame();
	if (browser != null) {
	    BrowserPanel4 panel = browser.getPubBrowserPanel();
	    PowerloomTrees trees = panel.getPowerloomTrees();
	    trees.navigateToPreviousSelection();
	}
    }

    public void forwardMenuItem_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	BrowserFrame4 browser = (BrowserFrame4)getSelectedBrowserFrame();
	if (browser != null) {
	    BrowserPanel4 panel = browser.getPubBrowserPanel();
	    PowerloomTrees trees = panel.getPowerloomTrees();
	    trees.navigateToNextSelection();
	}
    }


    /**
     * Comment
     */
    public void editMenuItem_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
    }
    /**
     * Comment
     */
    public void expressionMenuItem_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
    }
    /**
     * Return the About_BoxMenuItem property value.
     * @return javax.swing.JMenuItem
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuItem getAbout_BoxMenuItem() {
	if (ivjAbout_BoxMenuItem == null) {
	    try {
		ivjAbout_BoxMenuItem = new javax.swing.JMenuItem();
		ivjAbout_BoxMenuItem.setName("About_BoxMenuItem");
		ivjAbout_BoxMenuItem.setText("About PowerLoom GUI");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjAbout_BoxMenuItem;
    }
    /**
     * Return the Save_AsMenuItem property value.
     * @return javax.swing.JMenuItem
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuItem getBrowseMenuItem() {
	if (ivjBrowseMenuItem == null) {
	    try {
		ivjBrowseMenuItem = new javax.swing.JMenuItem();
		ivjBrowseMenuItem.setName("BrowseMenuItem");
		ivjBrowseMenuItem.setText("Browse");
		ivjBrowseMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_B, InputEvent.CTRL_MASK, false));
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjBrowseMenuItem;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/10/2002 6:49:23 PM)
     * @return java.util.Collection
     */
    public java.util.Collection getBrowserFrames() {
	return browserFrames;
    }
    /**
     * Return the ClearModuleMenuItem property value.
     * @return javax.swing.JMenuItem
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuItem getClearModuleMenuItem() {
	if (ivjClearModuleMenuItem == null) {
	    try {
		ivjClearModuleMenuItem = new javax.swing.JMenuItem();
		ivjClearModuleMenuItem.setName("ClearModuleMenuItem");
		ivjClearModuleMenuItem.setText("Clear Module");
		ivjClearModuleMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_X, InputEvent.CTRL_MASK | InputEvent.SHIFT_MASK, false));
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjClearModuleMenuItem;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/15/2002 9:01:22 PM)
     * @return java.util.Collection
     */
    public java.util.Collection getConceptFrames() {
	return conceptFrames;
    }


    public java.util.Collection getPropositionFrames() {
	return propositionFrames;
    }
    /**
     * Return the NewMenuItem property value.
     * @return javax.swing.JMenuItem
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuItem getConnectMenuItem() {
	if (ivjConnectMenuItem == null) {
	    try {
		ivjConnectMenuItem = new javax.swing.JMenuItem();
		ivjConnectMenuItem.setName("ConnectMenuItem");
		ivjConnectMenuItem.setText("Connect to Server");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjConnectMenuItem;
    }
    private javax.swing.JMenuItem getOpenConsoleMenuItem() {
	if (ivjOpenConsoleMenuItem == null) {
	    try {
		ivjOpenConsoleMenuItem = new javax.swing.JMenuItem();
		ivjOpenConsoleMenuItem.setName("OpenConsoleMenuItem");
		ivjOpenConsoleMenuItem.setText("Open Powerloom Console");
		ivjOpenConsoleMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_P, InputEvent.CTRL_MASK, false));
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjOpenConsoleMenuItem;
    }
    /**
     * Return the CopyButton property value.
     * @return javax.swing.JButton
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JButton getCopyButton() {
	if (ivjCopyButton == null) {
	    try {
		ivjCopyButton = new javax.swing.JButton();
		ivjCopyButton.setName("CopyButton");
		ivjCopyButton.setIcon(getImage("resources/images/copy.gif"));
		ivjCopyButton.setText("");
		ivjCopyButton.setMargin(new java.awt.Insets(0, 0, 0, 0));
		ivjCopyButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
		ivjCopyButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjCopyButton;
    }

    public static javax.swing.ImageIcon getImage(String path) {
	javax.swing.ImageIcon cachedIcon = (javax.swing.ImageIcon)cachedIcons.get(path);
	if (cachedIcon != null) {
	    return cachedIcon;
	}
	if (DEVELOPMENT_MODE) {  // loading from non-jar
	    String absolutePath = RESOURCE_BASE_PATH + path;
	    javax.swing.ImageIcon result = new javax.swing.ImageIcon(absolutePath);
	    cachedIcons.put(path, result);
	    return result;
	} else { 
	    debugPrintln(3, "getting resource at " + path);
	    javax.swing.ImageIcon result = new javax.swing.ImageIcon(PowerloomApp.class.getClassLoader().getResource(path));
	    cachedIcons.put(path, result);
	    return result;
	}
    }

    /**
     * Return the CutButton property value.
     * @return javax.swing.JButton
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */

    private javax.swing.JMenu getNavigationMenu() {
	if (ivjNavigationMenu == null) {
	    try {
		ivjNavigationMenu = new javax.swing.JMenu();
		ivjNavigationMenu.setName("NavigationMenu");
		ivjNavigationMenu.setText("Navigate");
		ivjNavigationMenu.add(getBackMenuItem());
		ivjNavigationMenu.add(getForwardMenuItem());
		BrowserFrame4 selectedBrowser = getSelectedBrowserFrame();
		updateNavigationMenuState((selectedBrowser == null) ? null : selectedBrowser.getPubBrowserPanel().getPowerloomTrees());
	    } catch (Throwable e) {
		handleException(e);
	    }
	}
	return ivjNavigationMenu;
    }

    public javax.swing.JMenu getWindowMenu() {
	if (ivjWindowMenu == null) {
	    try {
		ivjWindowMenu = new javax.swing.JMenu();
		ivjWindowMenu.setName("WindowMenu");
		ivjWindowMenu.setText("Window");
	    } catch (Throwable e) {
		handleException(e);
	    }
	}
	return ivjWindowMenu;
    }


    /**
     * Return the EditMenu property value.
     * @return javax.swing.JMenu
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenu getEditMenu() {
	if (ivjEditMenu == null) {
	    try {
		ivjEditMenu = new javax.swing.JMenu();
		ivjEditMenu.setName("EditMenu");
		ivjEditMenu.setText("Edit");
		ivjEditMenu.add(getUndoMenuItem());
		ivjEditMenu.add(getRedoMenuItem());
		ivjEditMenu.add(getJSeparator2());
		ivjEditMenu.add(getCutMenuItem());
		ivjEditMenu.add(getCopyMenuItem());
		ivjEditMenu.add(getPasteMenuItem());
		ivjEditMenu.add(getDeleteMenuItem());
		//ivjEditMenu.add(getSelect_AllMenuItem());
		//ivjEditMenu.add(getFind_ReplaceMenuItem());
		ivjEditMenu.add(getJSeparator6());
		ivjEditMenu.add(getPreferencesMenuItem());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjEditMenu;
    }

    private javax.swing.JMenu getObjectMenu() {
	if (ivjObjectMenu == null) {
	    try {
		ivjObjectMenu = new javax.swing.JMenu();
		ivjObjectMenu.setName("ObjectMenu");
		ivjObjectMenu.setText("Objects");
		ivjObjectMenu.add(getEditObjectMenuItem());
		ivjObjectMenu.add(getNewModuleMenuItem());
		ivjObjectMenu.add(getNewConceptMenuItem());
		ivjObjectMenu.add(getNewRelationMenuItem());
		ivjObjectMenu.add(getNewInstanceMenuItem());
		ivjObjectMenu.add(getNewPropositionMenuItem());
		ivjObjectMenu.add(getNewRuleMenuItem());
		ivjObjectMenu.add(getJSeparator4());
		ivjObjectMenu.add(getScrapbookMenuItem());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjObjectMenu;
    }

    private javax.swing.JMenu getQueryMenu() {
	if (ivjQueryMenu == null) {
	    try {
		ivjQueryMenu = new javax.swing.JMenu();
		ivjQueryMenu.setName("QueryMenu");
		ivjQueryMenu.setText("Query");
		ivjQueryMenu.add(getQueryMenuItem());
		ivjQueryMenu.add(getSearchMenuItem());
		ivjQueryMenu.add(getExtensionMenuItem());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjQueryMenu;
    }


    /**
     * Return the ExitMenuItem property value.
     * @return javax.swing.JMenuItem
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuItem getExitMenuItem() {
	if (ivjExitMenuItem == null) {
	    try {
		ivjExitMenuItem = new javax.swing.JMenuItem();
		ivjExitMenuItem.setName("ExitMenuItem");
		ivjExitMenuItem.setText("Exit");
		ivjExitMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Q, InputEvent.CTRL_MASK, false));
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjExitMenuItem;
    }
    /**
     * Return the Find_ReplaceMenuItem property value.
     * @return javax.swing.JMenuItem
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuItem getFind_ReplaceMenuItem() {
	if (ivjFind_ReplaceMenuItem == null) {
	    try {
		ivjFind_ReplaceMenuItem = new javax.swing.JMenuItem();
		ivjFind_ReplaceMenuItem.setName("Find_ReplaceMenuItem");
		ivjFind_ReplaceMenuItem.setText("Find/Replace");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjFind_ReplaceMenuItem;
    }
    /**
     * Return the Help_TopicsMenuItem property value.
     * @return javax.swing.JMenuItem
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuItem getHelp_TopicsMenuItem() {
	if (ivjHelp_TopicsMenuItem == null) {
	    try {
		ivjHelp_TopicsMenuItem = new javax.swing.JMenuItem();
		ivjHelp_TopicsMenuItem.setName("Help_TopicsMenuItem");
		ivjHelp_TopicsMenuItem.setText("Help Topics");
		ivjHelp_TopicsMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F1, 0, false));
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjHelp_TopicsMenuItem;
    }


    /**
     * Return the HelpMenu property value.
     * @return javax.swing.JMenu
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenu getHelpMenu() {
	if (ivjHelpMenu == null) {
	    try {
		ivjHelpMenu = new javax.swing.JMenu();
		ivjHelpMenu.setName("HelpMenu");
		ivjHelpMenu.setText("Help");
		ivjHelpMenu.add(getHelp_TopicsMenuItem());
		ivjHelpMenu.add(getAbout_BoxMenuItem());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjHelpMenu;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/15/2002 9:04:49 PM)
     * @return java.util.Collection
     */
    public java.util.Collection getInstanceFrames() {
	return instanceFrames;
    }
    /**
     * Return the JDesktopPane property value.
     * @return javax.swing.JDesktopPane
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JDesktopPane getJDesktopPane() {
	if (ivjJDesktopPane == null) {
	    try {
		ivjJDesktopPane = new javax.swing.JDesktopPane();
		ivjJDesktopPane.setName("JDesktopPane");
		// user code begin {1}
		//ivjJDesktopPane.setBackground(BACKGROUND_COLOR);
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjJDesktopPane;
    }
    /**
     * Return the JFrameContentPane property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getJFrameContentPane() {
	if (ivjJFrameContentPane == null) {
	    try {
		ivjJFrameContentPane = new javax.swing.JPanel();
		ivjJFrameContentPane.setName("JFrameContentPane");
		ivjJFrameContentPane.setLayout(new java.awt.BorderLayout());
		getJFrameContentPane().add(getToolBarPane(), "North");
		getJFrameContentPane().add(getStatusBarPane(), "South");
		getJFrameContentPane().add(getJDesktopPane(), "Center");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjJFrameContentPane;
    }
    /**
     * Return the JSeparator1 property value.
     * @return javax.swing.JSeparator
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JSeparator getJSeparator1() {
	if (ivjJSeparator1 == null) {
	    try {
		ivjJSeparator1 = new javax.swing.JSeparator();
		ivjJSeparator1.setName("JSeparator1");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjJSeparator1;
    }
    /**
     * Return the JSeparator2 property value.
     * @return javax.swing.JSeparator
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JSeparator getJSeparator2() {
	if (ivjJSeparator2 == null) {
	    try {
		ivjJSeparator2 = new javax.swing.JSeparator();
		ivjJSeparator2.setName("JSeparator2");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjJSeparator2;
    }
    /**
     * Return the JSeparator3 property value.
     * @return javax.swing.JSeparator
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JSeparator getJSeparator3() {
	if (ivjJSeparator3 == null) {
	    try {
		ivjJSeparator3 = new javax.swing.JSeparator();
		ivjJSeparator3.setName("JSeparator3");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjJSeparator3;
    }
    /**
     * Return the JSeparator4 property value.
     * @return javax.swing.JSeparator
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JSeparator getJSeparator4() {
	if (ivjJSeparator4 == null) {
	    try {
		ivjJSeparator4 = new javax.swing.JSeparator();
		ivjJSeparator4.setName("JSeparator4");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjJSeparator4;
    }
    /**
     * Return the JSeparator5 property value.
     * @return javax.swing.JSeparator
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JSeparator getJSeparator5() {
	if (ivjJSeparator5 == null) {
	    try {
		ivjJSeparator5 = new javax.swing.JSeparator();
		ivjJSeparator5.setName("JSeparator5");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjJSeparator5;
    }
    /**
     * Return the JSeparator6 property value.
     * @return javax.swing.JSeparator
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JSeparator getJSeparator6() {
	if (ivjJSeparator6 == null) {
	    try {
		ivjJSeparator6 = new javax.swing.JSeparator();
		ivjJSeparator6.setName("JSeparator6");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjJSeparator6;
    }
    /**
     * Return the JSeparator7 property value.
     * @return javax.swing.JSeparator
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JSeparator getJSeparator7() {
	if (ivjJSeparator7 == null) {
	    try {
		ivjJSeparator7 = new javax.swing.JSeparator();
		ivjJSeparator7.setName("JSeparator7");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjJSeparator7;
    }
    /**
     * Return the FileMenu property value.
     * @return javax.swing.JMenu
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenu getKBMenu() {
	if (ivjKBMenu == null) {
	    try {
		ivjKBMenu = new javax.swing.JMenu();
		ivjKBMenu.setName("KBMenu");
		ivjKBMenu.setText("KB");
		ivjKBMenu.add(getConnectMenuItem());
		ivjKBMenu.add(getLocalLoadMenuItem());
		ivjKBMenu.add(getLocalSaveMenuItem());
		ivjKBMenu.add(getRemoteLoadMenuItem());
		ivjKBMenu.add(getRemoteSaveMenuItem());
		ivjKBMenu.add(getClearModuleMenuItem());
		ivjKBMenu.add(getOpenConsoleMenuItem());
		ivjKBMenu.add(getBrowseMenuItem());
		ivjKBMenu.add(getJSeparator7());
		ivjKBMenu.add(getExitMenuItem());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjKBMenu;
    }

    private javax.swing.JMenuItem getRemoteLoadMenuItem() {
        // hc: we probably want to replace this with loading server-side modules
        // from a predefined (PL:kbs;kbs.ste) list - which is what Eric's initial
        // idea was, I think.
	if (ivjRemoteLoadMenuItem == null) {
	    try {
		ivjRemoteLoadMenuItem = new javax.swing.JMenuItem();
		ivjRemoteLoadMenuItem.setName("NewRemoteLoadMenuItem");
		ivjRemoteLoadMenuItem.setText("Load Server Module");
		ivjRemoteLoadMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O, InputEvent.CTRL_MASK | InputEvent.SHIFT_MASK, false));
                // PROBLEM: this will break if server is not yet up....
		//ivjRemoteLoadMenuItem.setEnabled(getServerInfo().attrIsTrue(getServerInfo().attrAllowRemoteFileBrowsing));
                ivjRemoteLoadMenuItem.setEnabled(false);
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjRemoteLoadMenuItem;
    }

    private javax.swing.JMenuItem getRemoteSaveMenuItem() {
        // hc: we probably want to eliminate that, since saving remotely on the
        // server is too complicated, insecure and unnecessary.
	if (ivjRemoteSaveMenuItem == null) {
	    try {
		ivjRemoteSaveMenuItem = new javax.swing.JMenuItem();
		ivjRemoteSaveMenuItem.setName("RemoteSaveMenuItem");
		ivjRemoteSaveMenuItem.setText("Save Server Module");
		ivjRemoteSaveMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S, InputEvent.CTRL_MASK | InputEvent.SHIFT_MASK, false));
                ivjRemoteSaveMenuItem.setEnabled(false);
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjRemoteSaveMenuItem;
    }

    private javax.swing.JMenuItem getLocalLoadMenuItem() {
	if (ivjLocalLoadMenuItem == null) {
	    try {
		ivjLocalLoadMenuItem = new javax.swing.JMenuItem();
		ivjLocalLoadMenuItem.setName("NewLocalLoadMenuItem");
		ivjLocalLoadMenuItem.setText("Load Module");
		ivjLocalLoadMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O, InputEvent.CTRL_MASK, false));
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjLocalLoadMenuItem;
    }

    private javax.swing.JMenuItem getLocalSaveMenuItem() {
	if (ivjLocalSaveMenuItem == null) {
	    try {
		ivjLocalSaveMenuItem = new javax.swing.JMenuItem();
		ivjLocalSaveMenuItem.setName("LocalSaveMenuItem");
		ivjLocalSaveMenuItem.setText("Save Module");
		ivjLocalSaveMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S, InputEvent.CTRL_MASK, false));
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjLocalSaveMenuItem;
    }

    /**
     * Insert the method's description here.
     * Creation date: (4/10/2002 3:35:39 PM)
     * @return edu.isi.powerloom.gui.xmlobject.PLModule
     */
    public edu.isi.powerloom.gui.xmlobject.PLModule getMostRecentlyTouchedModule() {
	return mostRecentlyTouchedModule;
    }
    public PLObject getMostRecentlyTouchedObject() {
	return mostRecentlyTouchedObject;
    }
    /**
     * Return the NewConceptMenuItem property value.
     * @return javax.swing.JMenuItem
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuItem getNewConceptMenuItem() {
	if (ivjNewConceptMenuItem == null) {
	    try {
		ivjNewConceptMenuItem = new javax.swing.JMenuItem();
		ivjNewConceptMenuItem.setName("NewConceptMenuItem");
		ivjNewConceptMenuItem.setText("New Concept");
		ivjNewConceptMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C, InputEvent.CTRL_MASK | InputEvent.SHIFT_MASK, false));
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjNewConceptMenuItem;
    }
    /**
     * Return the NewInstanceMenuItem property value.
     * @return javax.swing.JMenuItem
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuItem getNewInstanceMenuItem() {
	if (ivjNewInstanceMenuItem == null) {
	    try {
		ivjNewInstanceMenuItem = new javax.swing.JMenuItem();
		ivjNewInstanceMenuItem.setName("NewInstanceMenuItem");
		ivjNewInstanceMenuItem.setText("New Instance");
		ivjNewInstanceMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_I, InputEvent.CTRL_MASK | InputEvent.SHIFT_MASK, false));
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjNewInstanceMenuItem;
    }
    /**
     * Return the NewModuleMenuItem property value.
     * @return javax.swing.JMenuItem
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuItem getNewModuleMenuItem() {
	if (ivjNewModuleMenuItem == null) {
	    try {
		ivjNewModuleMenuItem = new javax.swing.JMenuItem();
		ivjNewModuleMenuItem.setName("NewModuleMenuItem");
		ivjNewModuleMenuItem.setText("New Module");
		ivjNewModuleMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_M, InputEvent.CTRL_MASK | InputEvent.SHIFT_MASK, false));
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjNewModuleMenuItem;
    }
    /**
     * Return the NewPropositionMenuItem property value.
     * @return javax.swing.JMenuItem
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuItem getNewPropositionMenuItem() {
	if (ivjNewPropositionMenuItem == null) {
	    try {
		ivjNewPropositionMenuItem = new javax.swing.JMenuItem();
		ivjNewPropositionMenuItem.setName("NewPropositionMenuItem");
		ivjNewPropositionMenuItem.setText("New Proposition");
		ivjNewPropositionMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_P, InputEvent.CTRL_MASK | InputEvent.SHIFT_MASK, false));
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjNewPropositionMenuItem;
    }

    /**
     * Return the NewRelationMenuItem property value.
     * @return javax.swing.JMenuItem
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuItem getNewRelationMenuItem() {
	if (ivjNewRelationMenuItem == null) {
	    try {
		ivjNewRelationMenuItem = new javax.swing.JMenuItem();
		ivjNewRelationMenuItem.setName("NewRelationMenuItem");
		ivjNewRelationMenuItem.setText("New Relation");
		ivjNewRelationMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_R, InputEvent.CTRL_MASK | InputEvent.SHIFT_MASK, false));
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjNewRelationMenuItem;
    }
    /**
     * Return the NewRuleMenuItem property value.
     * @return javax.swing.JMenuItem
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuItem getNewRuleMenuItem() {
	if (ivjNewRuleMenuItem == null) {
	    try {
		ivjNewRuleMenuItem = new javax.swing.JMenuItem();
		ivjNewRuleMenuItem.setName("NewRuleMenuItem");
		ivjNewRuleMenuItem.setText("New Rule");
		ivjNewRuleMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_U, InputEvent.CTRL_MASK | InputEvent.SHIFT_MASK, false));
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjNewRuleMenuItem;
    }
    /**
     * Return the PasteButton property value.
     * @return javax.swing.JButton
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JButton getPasteButton() {
	if (ivjPasteButton == null) {
	    try {
		ivjPasteButton = new javax.swing.JButton();
		ivjPasteButton.setName("PasteButton");
		ivjPasteButton.setIcon(getImage("resources/images/paste.gif"));
		ivjPasteButton.setText("");
		ivjPasteButton.setMargin(new java.awt.Insets(0, 0, 0, 0));
		ivjPasteButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
		ivjPasteButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjPasteButton;
    }

    private Action getCutAction() {
	if (ivjCutAction == null) {
	    ivjCutAction = new CutAction();
	}
	return ivjCutAction;
    }

    private Action getCopyAction() {
	if (ivjCopyAction == null) {
	    ivjCopyAction = new CopyAction();
	}
	return ivjCopyAction;
    }

    private Action getPasteAction() {
	if (ivjPasteAction == null) {
	    ivjPasteAction = new PasteAction();
	}
	return ivjPasteAction;
    }

    private Action getDeleteAction() {
	if (ivjDeleteAction == null) {
	    ivjDeleteAction = new DeleteAction();
	}
	return ivjDeleteAction;
    }

    private Action getEditObjectAction() {
	if (ivjEditObjectAction == null) {
	    ivjEditObjectAction = new EditObjectAction();
	}
	return ivjEditObjectAction;
    }

    private Action getQueryAction() {
	if (ivjQueryAction == null) {
	    ivjQueryAction = new QueryAction();
	}
	return ivjQueryAction;
    }
    /**
     * Return the PowerloomAppJMenuBar property value.
     * @return javax.swing.JMenuBar
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuBar getPowerloomAppJMenuBar() {
	if (ivjPowerloomAppJMenuBar == null) {
	    try {
		ivjPowerloomAppJMenuBar = new javax.swing.JMenuBar();
		ivjPowerloomAppJMenuBar.setName("PowerloomAppJMenuBar");
		ivjPowerloomAppJMenuBar.add(getKBMenu());
		ivjPowerloomAppJMenuBar.add(getEditMenu());
		ivjPowerloomAppJMenuBar.add(getObjectMenu());
		ivjPowerloomAppJMenuBar.add(getQueryMenu());
		ivjPowerloomAppJMenuBar.add(getViewMenu());
		ivjPowerloomAppJMenuBar.add(getNavigationMenu());
		ivjPowerloomAppJMenuBar.add(getWindowMenu());
		ivjPowerloomAppJMenuBar.add(getHelpMenu());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjPowerloomAppJMenuBar;
    }
    /**
     * Return the PreferencesMenuItem property value.
     * @return javax.swing.JMenuItem
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuItem getPreferencesMenuItem() {
	if (ivjPreferencesMenuItem == null) {
	    try {
		ivjPreferencesMenuItem = new javax.swing.JMenuItem();
		ivjPreferencesMenuItem.setName("PreferencesMenuItem");
		ivjPreferencesMenuItem.setText("Preferences...");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjPreferencesMenuItem;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/16/2002 1:13:22 PM)
     * @return javax.swing.JDesktopPane
     */
    public JDesktopPane getPubJDesktopPane() {
	return getJDesktopPane();
    }
    private javax.swing.JMenuItem getExtensionMenuItem() {
	if (ivjExtensionMenuItem == null) {
	    try {
		ivjExtensionMenuItem = new javax.swing.JMenuItem();
		ivjExtensionMenuItem.setName("ExtensionMenuItem");
		ivjExtensionMenuItem.setText("Edit Extension");
		ivjExtensionMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_E, InputEvent.CTRL_MASK | InputEvent.SHIFT_MASK, false));

		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjExtensionMenuItem;
    }


    /**
     * Return the QueryMenuItem property value.
     * @return javax.swing.JMenuItem
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuItem getQueryMenuItem() {
	if (ivjQueryMenuItem == null) {
	    try {
		ivjQueryMenuItem = new javax.swing.JMenuItem();
		ivjQueryMenuItem.setName("QueryMenuItem");
		ivjQueryMenuItem.setText("Query");
		ivjQueryMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Q, InputEvent.CTRL_MASK | InputEvent.SHIFT_MASK, false));

		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjQueryMenuItem;
    }
    /**
     * Return the RedoMenuItem property value.
     * @return javax.swing.JMenuItem
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuItem getRedoMenuItem() {
	if (ivjRedoMenuItem == null) {
	    try {
		ivjRedoMenuItem = new javax.swing.JMenuItem();
		ivjRedoMenuItem.setName("RedoMenuItem");
		ivjRedoMenuItem.setText("Redo");
		// user code begin {1}
		ivjRedoMenuItem.setEnabled(false);
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjRedoMenuItem;
    }

    private javax.swing.JMenuItem getCutMenuItem() {
	if (ivjCutMenuItem == null) {
	    try {
		ivjCutMenuItem = new javax.swing.JMenuItem(getCutAction());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjCutMenuItem;
    }

    private javax.swing.JMenuItem getDeleteMenuItem() {
	if (ivjDeleteMenuItem == null) {
	    try {
		ivjDeleteMenuItem = new javax.swing.JMenuItem(getDeleteAction());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjDeleteMenuItem;
    }

    private javax.swing.JMenuItem getCopyMenuItem() {
	if (ivjCopyMenuItem == null) {
	    try {
		ivjCopyMenuItem = new javax.swing.JMenuItem(getCopyAction());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjCopyMenuItem;
    }

    private javax.swing.JMenuItem getEditObjectMenuItem() {
	if (ivjEditObjectMenuItem == null) {
	    try {
		ivjEditObjectMenuItem = new javax.swing.JMenuItem(getEditObjectAction());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjEditObjectMenuItem;
    }


    private javax.swing.JMenuItem getPasteMenuItem() {
	if (ivjPasteMenuItem == null) {
	    try {
		ivjPasteMenuItem = new javax.swing.JMenuItem(getPasteAction());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjPasteMenuItem;
    }

    /**
     * Insert the method's description here.
     * Creation date: (4/15/2002 9:03:50 PM)
     * @return java.util.Collection
     */
    public java.util.Collection getRelationFrames() {
	return relationFrames;
    }

    /**
     * Return the ScrapbookMenuItem property value.
     * @return javax.swing.JMenuItem
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuItem getScrapbookMenuItem() {
	if (ivjScrapbookMenuItem == null) {
	    try {
		ivjScrapbookMenuItem = new javax.swing.JMenuItem();
		ivjScrapbookMenuItem.setName("ScrapbookMenuItem");
		ivjScrapbookMenuItem.setText("Open Scrapbook");
		ivjScrapbookMenuItem.setEnabled(false);
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjScrapbookMenuItem;
    }
    /**
     * Return the SearchMenuItem property value.
     * @return javax.swing.JMenuItem
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuItem getSearchMenuItem() {
	if (ivjSearchMenuItem == null) {
	    try {
		ivjSearchMenuItem = new javax.swing.JMenuItem();
		ivjSearchMenuItem.setName("SearchMenuItem");
		ivjSearchMenuItem.setText("Search");
		ivjSearchMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F, InputEvent.CTRL_MASK, false));
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjSearchMenuItem;
    }
    /**
     * Return the Select_AllMenuItem property value.
     * @return javax.swing.JMenuItem
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuItem getSelect_AllMenuItem() {
	if (ivjSelect_AllMenuItem == null) {
	    try {
		ivjSelect_AllMenuItem = new javax.swing.JMenuItem();
		ivjSelect_AllMenuItem.setName("Select_AllMenuItem");
		ivjSelect_AllMenuItem.setText("Select All");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjSelect_AllMenuItem;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/30/2002 7:14:51 PM)
     * @return redesign.gui.BrowserFrame4
     */
    public BrowserFrame4 getSelectedBrowserFrame() {
	JInternalFrame selectedFrame = null;
	JInternalFrame frames[] = getJDesktopPane().getAllFrames();
	for (int i = 0; i < frames.length; i++) {
	    if (frames[i].isSelected()) {
		selectedFrame = frames[i];
	    }
	}
	if (selectedFrame instanceof BrowserFrame4) {
	    return (BrowserFrame4)selectedFrame;
	}
	return null;
    }

    public BrowserFrame4 getTopmostBrowserFrame() {
	int minLayer = 100000;
	JInternalFrame topmostFrame = null;
	JInternalFrame frames[] = getJDesktopPane().getAllFrames();
	for (int i = 0; i < frames.length; i++) {
	    if (frames[i] instanceof BrowserFrame4) {
		int layer = getJDesktopPane().getPosition(frames[i]);
		debugPrintln(3, "  looking at frame at layer " + layer);
		if (layer < minLayer) {
		    topmostFrame = frames[i];
		    minLayer = layer;
		}
	    }
	}
	return (BrowserFrame4)topmostFrame;
    }

    public JInternalFrame getTopmostFrame() {
	int minLayer = 100000;
	JInternalFrame topmostFrame = null;
	JInternalFrame frames[] = getJDesktopPane().getAllFrames();
	for (int i = 0; i < frames.length; i++) {
	    int layer = getJDesktopPane().getPosition(frames[i]);
	    if (layer < minLayer) {
		topmostFrame = frames[i];
		minLayer = layer;
	    }
	}
	return topmostFrame;
    }

    /**
     * Return the StatusbarMenuItem property value.
     * @return javax.swing.JMenuItem
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuItem getStatusbarMenuItem() {
	if (ivjStatusbarMenuItem == null) {
	    try {
		ivjStatusbarMenuItem = new javax.swing.JMenuItem();
		ivjStatusbarMenuItem.setName("StatusbarMenuItem");
		ivjStatusbarMenuItem.setText("Statusbar");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjStatusbarMenuItem;
    }
    /**
     * Return the StatusBarPane property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getStatusBarPane() {
	if (ivjStatusBarPane == null) {
	    try {
		ivjStatusBarPane = new javax.swing.JPanel();
		ivjStatusBarPane.setName("StatusBarPane");
		ivjStatusBarPane.setLayout(new java.awt.BorderLayout());
		getStatusBarPane().add(getStatusMsg1(), "West");
		getStatusBarPane().add(getStatusMsg2(), "Center");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjStatusBarPane;
    }
    /**
     * Return the StatusMsg1 property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getStatusMsg1() {
	if (ivjStatusMsg1 == null) {
	    try {
		ivjStatusMsg1 = new javax.swing.JLabel();
		ivjStatusMsg1.setName("StatusMsg1");
		ivjStatusMsg1.setBorder(new javax.swing.border.EtchedBorder());
		ivjStatusMsg1.setText("Current Module: <none>   ");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjStatusMsg1;
    }
    /**
     * Return the StatusMsg2 property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    public javax.swing.JLabel getStatusMsg2() {
	if (ivjStatusMsg2 == null) {
	    try {
		ivjStatusMsg2 = new javax.swing.JLabel();
		ivjStatusMsg2.setName("StatusMsg2");
		ivjStatusMsg2.setBorder(new javax.swing.border.EtchedBorder());
		ivjStatusMsg2.setText("");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjStatusMsg2;
    }
    /**
     * Return the ToolbarMenuItem property value.
     * @return javax.swing.JMenuItem
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuItem getToolbarMenuItem() {
	if (ivjToolbarMenuItem == null) {
	    try {
		ivjToolbarMenuItem = new javax.swing.JMenuItem();
		ivjToolbarMenuItem.setName("ToolbarMenuItem");
		ivjToolbarMenuItem.setText("Toolbar");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjToolbarMenuItem;
    }
    /**
     * Return the ToolbarMenuItem property value.
     * @return javax.swing.JMenuItem
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuItem getRefreshMenuItem() {
	if (ivjRefreshMenuItem == null) {
	    try {
		ivjRefreshMenuItem = new javax.swing.JMenuItem();
		ivjRefreshMenuItem.setName("RefreshMenuItem");
		ivjRefreshMenuItem.setText("Refresh");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjRefreshMenuItem;
    }


    private javax.swing.JMenuItem getBackMenuItem() {
	if (ivjBackMenuItem == null) {
	    try {
		ivjBackMenuItem = new javax.swing.JMenuItem();
		ivjBackMenuItem.setName("BackMenuItem");
		ivjBackMenuItem.setText("Back");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjBackMenuItem;
    }

    private javax.swing.JMenuItem getForwardMenuItem() {
	if (ivjForwardMenuItem == null) {
	    try {
		ivjForwardMenuItem = new javax.swing.JMenuItem();
		ivjForwardMenuItem.setName("ForwardMenuItem");
		ivjForwardMenuItem.setText("Forward");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjForwardMenuItem;
    }


    /**
     * Return the ToolBarPane property value.
     * @return javax.swing.JToolBar
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JToolBar getToolBarPane() {
	if (ivjToolBarPane == null) {
	    try {
		ivjToolBarPane = new javax.swing.JToolBar();
		ivjToolBarPane.add(getCutAction());
		ivjToolBarPane.add(getCopyAction());
		ivjToolBarPane.add(getPasteAction());
		ivjToolBarPane.add(getDeleteAction());
		ivjToolBarPane.add(getEditObjectAction());
		ivjToolBarPane.add(getQueryAction());
		// user code begin {1}
		((JComponent)ivjToolBarPane.getComponentAtIndex(0)).setToolTipText("Cut");
		((JComponent)ivjToolBarPane.getComponentAtIndex(1)).setToolTipText("Copy");
		((JComponent)ivjToolBarPane.getComponentAtIndex(2)).setToolTipText("Paste");
		((JComponent)ivjToolBarPane.getComponentAtIndex(3)).setToolTipText("Delete");
		((JComponent)ivjToolBarPane.getComponentAtIndex(4)).setToolTipText("Edit");
		((JComponent)ivjToolBarPane.getComponentAtIndex(5)).setToolTipText("Query");
		//ivjToolBarPane.setRollover(true);
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjToolBarPane;
    }
    /**
     * Return the UndoMenuItem property value.
     * @return javax.swing.JMenuItem
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuItem getUndoMenuItem() {
	if (ivjUndoMenuItem == null) {
	    try {
		ivjUndoMenuItem = new javax.swing.JMenuItem();
		ivjUndoMenuItem.setName("UndoMenuItem");
		ivjUndoMenuItem.setText("Undo");
		// user code begin {1}
		ivjUndoMenuItem.setEnabled(false);
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjUndoMenuItem;
    }

    private javax.swing.JMenuItem getMetalThemeMenuItem() {
	if (ivjMetalThemeMenuItem == null) {
	    try {
		ivjMetalThemeMenuItem = new javax.swing.JMenuItem();
		ivjMetalThemeMenuItem.setName("MetalThemeMenuItem");
		ivjMetalThemeMenuItem.setText("Metal Theme");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjMetalThemeMenuItem;
    }

    private javax.swing.JMenuItem getDemoThemeMenuItem() {
	if (ivjDemoThemeMenuItem == null) {
	    try {
		ivjDemoThemeMenuItem = new javax.swing.JMenuItem();
		ivjDemoThemeMenuItem.setName("DemoThemeMenuItem");
		ivjDemoThemeMenuItem.setText("Demo Theme");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjDemoThemeMenuItem;
    }

    private javax.swing.JMenuItem getBigThemeMenuItem() {
	if (ivjBigThemeMenuItem == null) {
	    try {
		ivjBigThemeMenuItem = new javax.swing.JMenuItem();
		ivjBigThemeMenuItem.setName("BigThemeMenuItem");
		ivjBigThemeMenuItem.setText("Big Theme");
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjBigThemeMenuItem;
    }

    /**
     * Return the ViewMenu property value.
     * @return javax.swing.JMenu
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenu getViewMenu() {
	if (ivjViewMenu == null) {
	    try {
		ivjViewMenu = new javax.swing.JMenu();
		ivjViewMenu.setName("ViewMenu");
		ivjViewMenu.setText("View");
		ivjViewMenu.add(getRefreshMenuItem());
		ivjViewMenu.add(new JSeparator());
		ivjViewMenu.add(getToolbarMenuItem());
		ivjViewMenu.add(getStatusbarMenuItem());
		ivjViewMenu.add(new JSeparator());
		ivjViewMenu.add(getMetalThemeMenuItem());
		ivjViewMenu.add(getDemoThemeMenuItem());
		ivjViewMenu.add(getBigThemeMenuItem());
		// user code begin {1}
		// user code end
	    } catch (java.lang.Throwable ivjExc) {
		// user code begin {2}
		// user code end
		handleException(ivjExc);
	    }
	}
	return ivjViewMenu;
    }

    public Object formatErrorMessage(String message) {
        // If `message' is too long (e.g., contains a long stack trace), wrap it
        // in a scrollable text area;  Otherwise, simply return `message'.
        if (message.length() <= 100)
            return message;
        else {
            JTextArea area = new JTextArea(message, 30, 80);
            JScrollPane pane = new JScrollPane(area);
            return pane;
        }
    }

    /**
     * Called whenever the part throws an exception.
     * @param exception java.lang.Throwable
     */
    public void handleException(java.lang.Throwable exception) {
	// silently eat InterruptedExceptions, e.g., this occurs when BasicDirectoryModel.LoadFileThread
	// gets intterupted by validateFileCache
	if (exception instanceof ClientException) {
	    Throwable nested = ((ClientException)exception).getNestedException();
	    if ((nested instanceof InterruptedException) ||
		(nested.getMessage().indexOf("InterruptedException") >= 0)) {
		debugPrintln(3, "Silently eating InterruptedException:");
		debugPrintStackTrace(3, nested);
		return;
	    } else {
		Preferences prefs = Preferences.getInstance();
		String host = prefs.getProperty(Preferences.HOST);
		String port = prefs.getProperty(Preferences.PORT);
		String hostString = host + ":" + port;
		String errorMessage = "Error while communicating with " + hostString + 
                                      ", maybe the server is unreachable";
		JOptionPane.showMessageDialog(this, errorMessage,
                                              "Application Error", JOptionPane.ERROR_MESSAGE);
		return;
	    }
	}
	StringWriter sw = new StringWriter();
	PrintWriter pw = new PrintWriter(sw);
	exception.printStackTrace(pw);
	String stackString = sw.toString();
	String errorMessage = exception.getMessage() + "\n" + stackString;
        JOptionPane.showMessageDialog(this, formatErrorMessage(errorMessage),
                                      "Application Error", JOptionPane.ERROR_MESSAGE);
	// print to System.err if required:
	debugPrintStackTrace(3, exception);
    }

    /**
     * Initializes connections
     * @exception java.lang.Exception The exception description.
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void initConnections() throws java.lang.Exception {
	// user code begin {1}
	// user code end
	getExitMenuItem().addActionListener(ivjEventHandler);
	getToolbarMenuItem().addActionListener(ivjEventHandler);
	getRefreshMenuItem().addActionListener(ivjEventHandler);
	getStatusbarMenuItem().addActionListener(ivjEventHandler);
	getAbout_BoxMenuItem().addActionListener(ivjEventHandler);
	getBrowseMenuItem().addActionListener(ivjEventHandler);
	getNewPropositionMenuItem().addActionListener(ivjEventHandler);
	getNewRelationMenuItem().addActionListener(ivjEventHandler);
	getNewInstanceMenuItem().addActionListener(ivjEventHandler);
	getScrapbookMenuItem().addActionListener(ivjEventHandler);
	getNewModuleMenuItem().addActionListener(ivjEventHandler);
	getNewRuleMenuItem().addActionListener(ivjEventHandler);
	getNewConceptMenuItem().addActionListener(ivjEventHandler);
	getConnectMenuItem().addActionListener(ivjEventHandler);
	getOpenConsoleMenuItem().addActionListener(ivjEventHandler);
	getLocalLoadMenuItem().addActionListener(ivjEventHandler);
	getLocalSaveMenuItem().addActionListener(ivjEventHandler);
	getRemoteLoadMenuItem().addActionListener(ivjEventHandler);
	getRemoteSaveMenuItem().addActionListener(ivjEventHandler);
	getClearModuleMenuItem().addActionListener(ivjEventHandler);
	getPreferencesMenuItem().addActionListener(ivjEventHandler);
	getQueryMenuItem().addActionListener(ivjEventHandler);
	getExtensionMenuItem().addActionListener(ivjEventHandler);
	getSearchMenuItem().addActionListener(ivjEventHandler);
	getBackMenuItem().addActionListener(ivjEventHandler);
	getForwardMenuItem().addActionListener(ivjEventHandler);
	getHelp_TopicsMenuItem().addActionListener(ivjEventHandler);
	getDemoThemeMenuItem().addActionListener(ivjEventHandler);
	getBigThemeMenuItem().addActionListener(ivjEventHandler);
	getMetalThemeMenuItem().addActionListener(ivjEventHandler);
    }

    /**
     * Sets the window icon of `window' to the standard PowerLoom icon.
     */
    public static void setPowerLoomWindowIcon(Window window) {
        window.setIconImage(new ImageIcon(window.getClass().getClassLoader().getResource(POWERLOOM_WINDOW_ICON_PATH)).getImage());
    }

    /**
     * Initialize the class.
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void initialize() {
	try {
	    // user code begin {1}
	    // user code end
	    setName("PowerloomApp");
	    setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
	    setJMenuBar(getPowerloomAppJMenuBar());
	    setSize(200, 200);
	    setContentPane(getJFrameContentPane());
	    initConnections();
	} catch (java.lang.Throwable ivjExc) {
	    handleException(ivjExc);
	}
	// user code begin {2}
	Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();		
	Dimension windowSize = new Dimension((int)screenSize.getWidth(), (int)screenSize.getHeight()-2);
	setSize(windowSize);
        setTitle("PowerLoom GUI");
        setPowerLoomWindowIcon(this);
	// user code end
    }

    public void changeMetalTheme(javax.swing.plaf.metal.MetalTheme theme) {
	debugPrintln(3, "activating theme: " + theme.getName());
	javax.swing.plaf.metal.MetalLookAndFeel.setCurrentTheme(theme);
	try {
	    javax.swing.UIManager.setLookAndFeel("javax.swing.plaf.metal.MetalLookAndFeel");
            SwingUtilities.updateComponentTreeUI(aPowerloomApp);
	} catch (Exception ex) {
	    debugPrintln(3, "Failed to activate theme");
            handleException(ex);
	}
    }

    public void metalThemeMenuItem_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
        changeMetalTheme(new javax.swing.plaf.metal.DefaultMetalTheme());
    }

    public void demoThemeMenuItem_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
        changeMetalTheme(new ZoomedMetalTheme(1.25f));
    }

    public void bigThemeMenuItem_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
        changeMetalTheme(new ZoomedMetalTheme(1.5f));
    }

    /**
     * Comment
     */
    public void remoteLoadMenuItem_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
        // disabled for now/forever (since we moved all the remote file support to the attic
        // since it was a major security hole and used Sun proprietary source code).
	//PLRemoteFileBrowser browser = new PLRemoteFileBrowser(this);
	//browser.showLoadKBDialog();
    }
    /**
     * Comment
     */
    public void remoteSaveMenuItem_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	NewKBSaverModuleFrame frame = new NewKBSaverModuleFrame(false);
	frame.displayFrame();
    }
    /**
     * Comment
     */
    public void localLoadMenuItem_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	PLLocalFileBrowser browser = new PLLocalFileBrowser(this);
	browser.showLoadKBDialog();
    }
    /**
     * Comment
     */
    public void localSaveMenuItem_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	NewKBSaverModuleFrame frame = new NewKBSaverModuleFrame(true);
	frame.displayFrame();
    }
    /**
     * Starts the application.
     * @param args an array of command-line arguments
     */
    public static void main(java.lang.String[] args) {
        debugLevel = 1;
	try {
            int nArgs = args.length;
            for (int i = 0; i < nArgs-1; i++) {
                if ("--host".equals(args[i]))
                    Preferences.getInstance().setProperty(Preferences.HOST, args[++i]);
                else if ("--port".equals(args[i]))
                    Preferences.getInstance().setProperty(Preferences.PORT, args[++i]);
                else if ("--server-url".equals(args[i]))
                    SoapSender.setServerURL(args[++i]);
                else if ("--debug".equals(args[i]))
                    debugLevel = Integer.parseInt(args[++i]);
                else
                    debugPrintln(0, "Unknown command line option: " + args[i]);
            }

            debugPrintln(1, "Starting PowerLoom GUI...");
            debugPrintln(1, "  java.vm.vendor=" + System.getProperty("java.vm.vendor"));
            debugPrintln(1, "  java.vm.version=" + System.getProperty("java.vm.version"));
            debugPrintln(1, "  java.vm.name=" + System.getProperty("java.vm.name"));
            debugPrintln(1, "  user.home=" + System.getProperty("user.home"));
            debugPrintln(1, "  user.dir=" + System.getProperty("user.dir"));

	    Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();		
	    PowerloomSplashScreen aPowerloomSplashScreen = new PowerloomSplashScreen();
	    aPowerloomSplashScreen.pack();

	    Dimension splashScreenSize = aPowerloomSplashScreen.getSize();
	    if (splashScreenSize.height > screenSize.height)
		splashScreenSize.height = screenSize.height;
	    if (splashScreenSize.width > screenSize.width)
		splashScreenSize.width = screenSize.width;
	    aPowerloomSplashScreen.setLocation((screenSize.width - splashScreenSize.width) / 2, (screenSize.height - splashScreenSize.height) / 2);
	    aPowerloomSplashScreen.setVisible(true);

	    /* Create the frame */
	    aPowerloomApp = new PowerloomApp();
	    /* Add a windowListener for the windowClosedEvent */
	    aPowerloomApp.addWindowListener(new java.awt.event.WindowAdapter() {
		    public void windowClosed(java.awt.event.WindowEvent e) {
			exitGui(0);
		    };
		});
	    /* Center frame on the screen */
	    /* Calculate the screen size */
	    screenSize = Toolkit.getDefaultToolkit().getScreenSize();		
	    Dimension frameSize = aPowerloomApp.getSize();
	    if (frameSize.height > screenSize.height)
		frameSize.height = screenSize.height;
	    if (frameSize.width > screenSize.width)
		frameSize.width = screenSize.width;
	    aPowerloomApp.setLocation((screenSize.width - frameSize.width) / 2, (screenSize.height - frameSize.height) / 2);

            aPowerloomApp.doStartup();

            // Without this, we don't always get all subcomponents updated properly
            // when the look-and-feel is switched:
	    UIManager.addPropertyChangeListener
                (new LookAndFeelChangeListener((JComponent)aPowerloomApp.getRootPane()));

            try {
                Thread.sleep(2000); // give the splash screen a bit more time
            } catch (InterruptedException ie) {};
	    aPowerloomSplashScreen.dispose();
	    aPowerloomApp.setVisible(true);

            if (aPowerloomApp. serverInfo == null &&
                Preferences.getInstance().getInstance().getBooleanProperty(Preferences.BROWSE_ON_START))
                JOptionPane.showMessageDialog(aPowerloomApp, "Error: can't connect to server: " +
                                              SoapSender.getServerURL(),
                                              "Server Error",
                                              JOptionPane.ERROR_MESSAGE);
	} catch (Throwable e) {
	    System.err.println("Exception in PowerloomApp.main():");
	    e.printStackTrace(System.out);
	}
    }

    /**
     * Ends the application (needed in embedded contexts).
     */
    public static void exit() {
        if (aPowerloomApp != null && aPowerloomApp.isVisible())
            aPowerloomApp.dispose();
    }


    /**
     * for debugging
     */
    private void printUIDefaults() {
	javax.swing.UIDefaults defaults = UIManager.getDefaults();
	Enumeration keys = defaults.keys();
	debugPrintln(3, "UIDefault keys: ");
	while (keys.hasMoreElements()) {
	    Object key = keys.nextElement();
	    debugPrintln(3, "  " + key);
	}
    }

    /**
     * Comment
     */
    public void newConcept2MenuItem_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	(new NewConceptAction()).actionPerformed(actionEvent);
    }
    /**
     * Comment
     */
    public void newInstanceMenuItem_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	InstanceFrame frame = new InstanceFrame();
	frame.displayFrame();

    }
    /**
     * Comment
     */
    public void newModuleMenuItem_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	ModuleFrame frame = new ModuleFrame();
	frame.displayFrame();
    }
    /**
     * Comment
     */
    public void newPropositionMenuItem_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	PropositionFrame2 propFrame = new PropositionFrame2();
	propFrame.displayFrame();
    }

    /**
     * Comment
     */
    public void newRelationMenuItem_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	RelationFrame relationFrame = new RelationFrame();
	relationFrame.displayFrame();
    }
    /**
     * Comment
     */
    public void newRuleMenuItem_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	PropositionFrame2 frame = new PropositionFrame2();
	frame.displayFrame();
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/29/2002 8:59:20 AM)
     * @param frame javax.swing.JInternalFrame
     */
    public void positionFrame(JInternalFrame frame) {
	int width = frame.getWidth();
	int height = frame.getHeight();
	int deskWidth = getJDesktopPane().getWidth();
	int deskHeight = getJDesktopPane().getHeight();
	int deltaMax = 100;
	int deltaX = (int)(Math.random() * deltaMax) - (deltaMax / 2);
	int deltaY = (int)(Math.random() * deltaMax) - (deltaMax / 2);
	int perfectX = (int)((deskWidth / 2) - (width / 2));
	int perfectY = (int)((deskHeight / 2) - (height / 2));
	//System.out.println("width = " + width + ", height = " + height + ", deskWidth = " + deskWidth + ", deskHeight = " + deskHeight + ", deltaX = " + deltaX + ", deltaY = " + deltaY + ", perfectX = " + perfectX + ", perfectY = " + perfectY);
	//System.out.println("desk isvisible = " + getJDesktopPane().isVisible());
	//System.out.println("desktop = " + getJDesktopPane());
	int x = (deskWidth == 0) ? 0 : Math.max(perfectX + deltaX, 0);
	int y = (deskHeight == 0) ? 0 : Math.max(perfectY + deltaY, 0);
	frame.setBounds(x, y, width, height);
	//frame.setBounds(30*(windowCount%10) + 30 * (int)Math.floor((double)windowCount / 10d), 30*(windowCount%10), width, height);
	windowCount++;
    }
    /**
     * Comment
     */
    public void preferencesMenuItem_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	PreferencesDialog dialog = new PreferencesDialog();
	dialog.setModal(true);
	dialog.show();
	PreferencesResult result = dialog.getResult();
	if (result != null) {
	    Preferences prefs = Preferences.getInstance();
	    prefs.setProperty(Preferences.BROWSE_ON_START, (new Boolean(result.isBrowseOnStartup())).toString());
	    if (result.isSavePreferences()) {
		prefs.save();
	    }
	}
    }
    /**
     * Comment
     */
    public void queryMenuItem_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	// temporary, for testing:
	//QueryOptionsFrame frame = new QueryOptionsFrame();
	QueryFrame frame = new QueryFrame();
	frame.pack();
	frame.displayFrame();
    }

    public void extensionMenuItem_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	ExtensionFrame frame = new ExtensionFrame();
	frame.displayFrame();
    }

    /**
     * Comment
     */
    public void scrapbookMenuItem_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	ScrapbookFrame frame = new ScrapbookFrame();
	frame.displayFrame();
    }
    /**
     * Comment
     */
    public void searchMenuItem_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	SearchFrame frame = new SearchFrame((BrowserFrame4)null, true, true, true);
	frame.displayFrame();
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/10/2002 6:49:23 PM)
     * @param newBrowserFrames java.util.Collection
     */
    public void setBrowserFrames(java.util.Collection newBrowserFrames) {
	browserFrames = newBrowserFrames;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/15/2002 9:01:22 PM)
     * @param newConceptFrames java.util.Collection
     */
    public void setConceptFrames(java.util.Collection newConceptFrames) {
	conceptFrames = newConceptFrames;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/15/2002 9:04:49 PM)
     * @param newInstanceFrames java.util.Collection
     */
    public void setInstanceFrames(java.util.Collection newInstanceFrames) {
	instanceFrames = newInstanceFrames;
    }
    /**
     * Insert the method's description here.
     * Creation date: (4/10/2002 3:35:39 PM)
     * @param newMostRecentlyTouchedModule edu.isi.powerloom.gui.xmlobject.PLModule
     */
    public void setMostRecentlyTouchedModule(edu.isi.powerloom.gui.xmlobject.PLModule newMostRecentlyTouchedModule) {
	mostRecentlyTouchedModule = newMostRecentlyTouchedModule;
	getStatusMsg1().setText("Current Module: " + mostRecentlyTouchedModule.getID() + "  ");
    }
    public void setMostRecentlyTouchedObject(PLObject object) {
	debugPrintln(3, "setting most recently touched object = " + object);
	mostRecentlyTouchedObject = object;
	if (object instanceof PLModule) {
	    mostRecentlyTouchedModule = (PLModule)object;
	}	
    }

    public void updateNavigationMenuState(PowerloomTrees trees) {
	JMenuItem forwardItem = getForwardMenuItem();
	JMenuItem backItem = getBackMenuItem();
	BrowserFrame4 selectedFrame = getSelectedBrowserFrame();
	if (selectedFrame == null) {
	    backItem.setEnabled(false);
	    forwardItem.setEnabled(false);
	    return;
	}
	//System.out.println("update navigation menu state.  tress = " + trees + ", beginning = " + trees.isAtBeginningOfHistory() + ", end = " + trees.isAtEndOfHistory());
	backItem.setEnabled((trees != null) && !(trees.isAtBeginningOfHistory()));
	forwardItem.setEnabled((trees != null) && !(trees.isAtEndOfHistory()));
    }

    /**
     * Insert the method's description here.
     * Creation date: (4/15/2002 9:03:50 PM)
     * @param newRelationFrames java.util.Collection
     */
    public void setRelationFrames(java.util.Collection newRelationFrames) {
	relationFrames = newRelationFrames;
    }
    public void showAboutBox() {
	/* Create the AboutBox dialog */
	PowerloomAppAboutBox aPowerloomAppAboutBox = new PowerloomAppAboutBox();
	Dimension dialogSize = aPowerloomAppAboutBox.getPreferredSize();
	Dimension frameSize = getSize();
	Point loc = getLocation();
	aPowerloomAppAboutBox.setLocation((frameSize.width - dialogSize.width) / 2 + loc.x, (frameSize.height - dialogSize.height) / 2 + loc.y);
	aPowerloomAppAboutBox.setModal(true);
	aPowerloomAppAboutBox.show();
    }
    public void viewStatusBar() {
	/* Hide or show the statusbar */
	getStatusBarPane().setVisible(!(getStatusBarPane().isVisible()));
    }
    public void viewToolBar() {
	/* Hide or show the toolbar */
	getToolBarPane().setVisible(!(getToolBarPane().isVisible()));
    }

    /**
     * Display flashing message in a JLabel, such as a status bar
     */
    public void flashMessage(final JLabel component, final String msg, final int flashTimes, final int flashDelay) {
	debugPrintln(3, "gonna flash msg: " + msg);
	Thread flashThread = new Thread() {
		public void run() {
		    for (int i = 0; i < flashTimes; i++) {
			try {
			    if ((i % 2) == 0) {
				component.setForeground(Color.RED);
				component.setText(msg);
			    } else {
				component.setText("");
			    }
			    sleep(flashDelay);
			    
			} catch (InterruptedException e) {
			    handleException(e);
			}
		    }
		    component.setForeground(Color.BLACK);
		    component.setText(msg);
		}
	    };
	flashThread.start();
    }

    public void flashStatusMessage2(String msg) {
	final int flashTimes = 20;
	final int flashDelay = 100;

	flashMessage(getStatusMsg2(), msg, flashTimes, flashDelay);
    }

    public void setDoingRightClickMenu(boolean doingIt) {
	doingRightClickMenu = doingIt;
    }

    public boolean getDoingRightClickMenu() {
	return doingRightClickMenu;
    }

    /** 
     * Singleton get method
     *
     */
    public static PowerloomApp getInstance() {
	return aPowerloomApp;
    }

    /**
     * Edit Listeners management 
     */
    public void addPLEditListener(PLEditListener listener) {
	debugPrintln(3, "PowerloomApp : added listener: " + listener);
	editListeners.add(listener);
    }

    public void removePLEditListener(PLEditListener listener) {
	editListeners.remove(listener);
    }

    protected void fireEditPerformed(PLEditEvent event) {
	debugPrintln(3, "PowerloomApp: fire editperformed...");
	Iterator iter = editListeners.iterator();
	while (iter.hasNext()) {
	    PLEditListener listener = (PLEditListener)iter.next();
	    listener.performEdit(event);
	}
    }

    public Collection getEditListeners() {
	return editListeners;
    }

}
