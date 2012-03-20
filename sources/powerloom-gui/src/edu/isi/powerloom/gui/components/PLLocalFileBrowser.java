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


// Version: PLLocalFileBrowser.java,v 1.9 2010/04/13 03:02:56 hans Exp

package edu.isi.powerloom.gui.components;

import java.util.*;
import java.io.*;
import javax.swing.*;
import javax.swing.filechooser.*;
import edu.isi.powerloom.gui.common.*;
import edu.isi.powerloom.gui.serverinterface.*;
import edu.isi.powerloom.gui.xmlobject.*;

/**
 * File browser for selecting files from local filesystem.
 *
 * @since  August 23, 2002, 6:01 PM
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */
public class PLLocalFileBrowser {
    public java.util.Collection editListeners = new ArrayList();
    private PowerloomApp app;
    private PLModule selectedModule;
    
    /** Creates a new instance of PLLocalFileBrowser */
    public PLLocalFileBrowser(PowerloomApp app) {
        setApp(app);
    }

    public PLLocalFileBrowser(PowerloomApp app, PLModule module) {
	this(app);
	this.selectedModule = module;
    }
    
    void showLoadKBDialog() {
        File defaultFile = new File("./");
	File f = null;
	try {
	    String loadPath = null;
	    if (selectedModule != null) {
		String modulePropName = Preferences.LOAD_PATH + "." + selectedModule.getID();
		loadPath = Preferences.getInstance().getProperty(modulePropName);
	    } else {
		loadPath = Preferences.getInstance().getProperty(Preferences.LOAD_PATH);
	    }
	    if (loadPath == null) {
		f = defaultFile;
	    } else {
		f = new File(loadPath);
	    }
	
	    JFileChooser chooser = new JFileChooser(f);
	    Preferences prefs = Preferences.getInstance();
	    String host = prefs.getProperty(Preferences.HOST);
	    String port = prefs.getProperty(Preferences.PORT);
	    chooser.setAcceptAllFileFilterUsed(true);
            chooser.addChoosableFileFilter(new PLFileFilter());
	    int returnVal = chooser.showOpenDialog(app);
	    String moduleName = "";
	    if(returnVal == JFileChooser.APPROVE_OPTION) {
		String name = chooser.getSelectedFile().getPath();
		moduleName = loadKB(name);
	    }
	    Preferences.getInstance().setProperty(Preferences.LOAD_PATH, chooser.getCurrentDirectory().getPath());
	    Preferences.getInstance().setProperty(Preferences.LOAD_PATH + "." + moduleName, chooser.getCurrentDirectory().getPath());
	    Preferences.getInstance().save();
	} catch (Exception e) {
            PowerloomApp.getInstance().handleException(e);
	}

    }
    
    void setApp(PowerloomApp app) {
        this.app = app;
	postCreateInitialize();
    }
    
    void showSaveKBDialog() {
        File defaultFile = new File("./");
	File f = null;
	try {
	    String modulePropName = Preferences.LOAD_PATH + "." + selectedModule.getID();
	    String savePath = Preferences.getInstance().getProperty(modulePropName);
	    if (savePath == null) {
		f = defaultFile;
	    } else {
		f = new File(savePath);
	    }
	    JFileChooser chooser = new JFileChooser(f);
	    Preferences prefs = Preferences.getInstance();
	    String host = prefs.getProperty(Preferences.HOST);
	    String port = prefs.getProperty(Preferences.PORT);
	    chooser.setAcceptAllFileFilterUsed(true);
            chooser.addChoosableFileFilter(new PLFileFilter());
	    int returnVal = chooser.showSaveDialog(app);
	    if(returnVal == JFileChooser.APPROVE_OPTION) {
		String name = chooser.getSelectedFile().getPath();
		saveKB(name);
	    }
	    Preferences.getInstance().setProperty(Preferences.LOAD_PATH, chooser.getCurrentDirectory().getPath());
	    Preferences.getInstance().setProperty(modulePropName, chooser.getCurrentDirectory().getPath());
	    Preferences.getInstance().save();
	} catch (Exception e) {
            PowerloomApp.getInstance().handleException(e);
	}
    }
        
    public void addPLEditListener(PLEditListener editListener) {
        editListeners.add(editListener);
    }

    /** Insert the method's description here.
     * Creation date: (4/24/2002 2:27:18 PM)
     */
    public void postCreateInitialize() {
        try {
            Iterator browserIter = app.getBrowserFrames().iterator();
            while (browserIter.hasNext()) {
                BrowserFrame4 frame = (BrowserFrame4)browserIter.next();
                PowerloomTrees trees = frame.getBrowserPanel().getPowerloomTrees();
                addPLEditListener(trees);
            }
        } catch (Exception e) {
            PowerloomApp.getInstance().handleException(e);
        }
    }

    /** Insert the method's description here.
     * Creation date: (4/12/2002 10:33:12 PM)
     */
    void fireEditPerformed() {
        Iterator iter = editListeners.iterator();
        while (iter.hasNext()) {
            PLEditListener listener = (PLEditListener)iter.next();
            PLEditEvent event = new PLEditEvent(this, PLModule.class);
            listener.performEdit(event);
        }
    }

    String loadKB(String path) {
	String result = "";
        try {
            result = KnowledgeManager.getInstance().loadKB2(path);
            fireEditPerformed();
        } catch (Exception e) {
	    PowerloomApp.getInstance().handleException(e);
        }
	return result;
    }

    void saveKB(String path) {
        try {
            KnowledgeManager.getInstance().saveKB2(selectedModule.getID(), path);
            fireEditPerformed();
        } catch (Exception e) {
	    PowerloomApp.getInstance().handleException(e);
        }
    }
    
}
