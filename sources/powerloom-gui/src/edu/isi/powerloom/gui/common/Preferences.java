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


// Version: Preferences.java,v 1.10 2010/02/18 22:09:46 hans Exp

package edu.isi.powerloom.gui.common;

import java.util.*;
import java.io.*;

import static edu.isi.powerloom.gui.common.Utils.*;

/**
 * This class provides facilities for loading and saving application 
 * preferences to and from a property file.
 *
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 * @since Tue Apr 23 15:53:36 2002
 */

public class Preferences {
    public final static String BROWSE_ON_START = "powerloom.gui.browseOnStart";
    public final static String HOST = "powerloom.gui.serverHost";
    public final static String PORT = "powerloom.gui.serverPort";
    public final static String LOAD_PATH = "powerloom.gui.loadPath";
    public final static String SAVE_PATH = "powerloom.gui.savePath";
    public final static String SAVED_QUERIES = "powerloom.gui.savedQueries";

    private String propertiesFile = System.getProperty("user.home") 
                                    + System.getProperty("file.separator") 
                                    + ".powerloom-gui";

    private static Preferences theInstance = null;

    private Properties properties = new Properties();

    private Preferences() {
	load();
    }

    /**
     * Insert the method's description here.
     * Creation date: (4/29/2002 8:03:49 AM)
     * @return boolean
     */
    public boolean getBooleanProperty() {
	return false;
    }

    /**
     * Insert the method's description here.
     * Creation date: (4/29/2002 8:03:49 AM)
     * @return boolean
     */
    public boolean getBooleanProperty(String property) {
	String value = getProperty(property);
	return Boolean.valueOf(value).booleanValue();
    }

    public static Preferences getInstance() {
	if (theInstance == null) {
	    theInstance = new Preferences();
	}
	return theInstance;
    }

    public String getProperty(String propName) {
	return properties.getProperty(propName);
    }

    private String getPropPath() {
	return propertiesFile;
    }

    public void load() {
	debugPrintln(3, "Loading " + getPropPath());
	try {
	    FileInputStream fis = null;
	    try {
		fis = new FileInputStream(getPropPath());
	    } catch (FileNotFoundException e) {
		try {
		    debugPrintln(3, "creating default properties file...");
		    InputStream is = Preferences.class.getClassLoader().getResourceAsStream("resources/conf/default.properties");
		    properties.load(is);
		    save();
		} catch (Exception e2) {
		    edu.isi.powerloom.gui.components.PowerloomApp.getInstance().handleException(e2);
		}
	    } // end of try-catch
	    fis = new FileInputStream(getPropPath());
	    properties.load(fis);
	} catch (Exception e) {
	    e.printStackTrace(); 
	    edu.isi.powerloom.gui.components.PowerloomApp.getInstance().handleException(e);
	} // end of try-catch
    }

    public static void main (String[] args) {
	Preferences prefs = Preferences.getInstance();
	String host = prefs.getProperty(Preferences.HOST);
	debugPrintln(3, "host is " + host);
	prefs.setProperty(Preferences.HOST, host + "x");
	prefs.save();
    }

    public void save() {
	debugPrintln(3, "Saving " + getPropPath());
	try {
	    FileOutputStream fos = new FileOutputStream(getPropPath());
	    String header = "Properties for the Powerloom GUI application.";
	    properties.store(fos, header);
	    fos.close();
	} catch (Exception e) {
	    e.printStackTrace(); // todo: handle exception
	    edu.isi.powerloom.gui.components.PowerloomApp.getInstance().handleException(e);
	} // end of try-catch
    }

    /**
     * Insert the method's description here.
     * Creation date: (4/29/2002 8:05:44 AM)
     * @param prop java.lang.String
     * @param value java.lang.String
     */
    public void setBooleanProperty(String prop, String value) {}

    /**
     * Insert the method's description here.
     * Creation date: (4/29/2002 8:05:44 AM)
     * @param prop java.lang.String
     * @param value java.lang.String
     */
    public void setBooleanProperty(String prop, boolean value) {
	String stringValue = (new Boolean(value)).toString();
	setProperty(prop, stringValue);
    }

    public void setProperty(String propName, String value) {
	properties.setProperty(propName, value);
    }
}
