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


// Version: HTMLBrowserFrame.java,v 1.7 2010/02/04 05:17:37 hans Exp

package edu.isi.powerloom.gui.components;

import java.net.URL;
import javax.swing.*;
import edu.isi.powerloom.gui.xmlobject.*;
import static edu.isi.powerloom.gui.common.Utils.*;

/**
 * Frame for displaying HTML pages.
 *
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 * @since Thu Oct 17 15:34:38 2002
 * @see edu.isi.powerloom.gui.components.HTMLBrowserPane HTMLBrowserPane
 */

public class HTMLBrowserFrame extends PLFrame {
    HTMLBrowserPane browserPane;
    JToolBar toolbar;
    BackAction backAction;
    ForwardAction forwardAction;
    ReloadAction reloadAction;

    private static final String protocolPathProp = "java.protocol.handler.pkgs";

    static {
	String protocolPaths = System.getProperty(protocolPathProp);
	String plguiProtocolPath = "edu.isi.powerloom.gui";
	if (protocolPaths == null) {
	    System.setProperty(protocolPathProp, plguiProtocolPath);
	} else {
	    System.setProperty(protocolPathProp, protocolPaths + ", " + plguiProtocolPath);
	}
    }

    public HTMLBrowserFrame (URL url){
	super("HTML Browser", true, true, true, true);
	initialize(url);
    }

    public HTMLBrowserFrame (String htmlText){
	super("HTML Browser", true, true, true, true);
	initialize(htmlText);
    }

    private void initialize(String htmlText) {
	browserPane = new HTMLBrowserPane(htmlText);
	browserPane.setParentFrame(this);
	getContentPane().add(getToolbar(), "North");
	getContentPane().add(browserPane, "Center");	
	setBounds( 200, 25, 800, 800);
    }

    private void initialize(URL url) {
	browserPane = new HTMLBrowserPane(url);
	browserPane.setParentFrame(this);
	getContentPane().add(getToolbar(), "North");
	getContentPane().add(browserPane, "Center");	
	setBounds( 200, 25, 800, 800);
    }

    private JToolBar getToolbar() {
	if (toolbar == null) {
	    backAction = new BackAction(this);
	    forwardAction = new ForwardAction(this);
	    reloadAction = new ReloadAction(this);
	    toolbar = new JToolBar();
	    toolbar.add(backAction);
	    toolbar.add(forwardAction);
	    toolbar.add(reloadAction);
	    ((JComponent)toolbar.getComponentAtIndex(0)).setToolTipText("Back");
	    ((JComponent)toolbar.getComponentAtIndex(1)).setToolTipText("Forward");
	    ((JComponent)toolbar.getComponentAtIndex(2)).setToolTipText("Reload");
	}      
	return toolbar;
    }

    public HTMLBrowserPane getBrowserPane() {
	return browserPane;
    }

    public void updateActionStatus(boolean isBackEnabled, boolean isForwardEnabled) {
	backAction.setEnabled(isBackEnabled);
	forwardAction.setEnabled(isForwardEnabled);
    }

    // Implementation of PLFrame
    public PLModule getModule() {
	return null;
    }

    private void testURL() {
	// for testing...
	try {

	    //URL url = new URL("plgui://getObject?module=AIRCRAFT-KB+concept=AIRPLANE");
	    URL url = new URL("http://www.isi.edu/getObject?module=AIRCRAFT-KB+concept=AIRPLANE");
	    debugPrintln(3, "url = " + url);
	    debugPrintln(3, "protocol = " + url.getProtocol());
	    debugPrintln(3, "path = " + url.getPath());
	    debugPrintln(3, "query = " + url.getQuery());

	    String protocolPaths = System.getProperty(protocolPathProp);
	    debugPrintln(3, "protocol paths = " + protocolPaths);
	    String plguiProtocolPath = "edu.isi.powerloom.gui";
	    if (protocolPaths == null) {
		System.setProperty(protocolPathProp, plguiProtocolPath);
	    } else {
		System.setProperty(protocolPathProp, protocolPaths + ", " + plguiProtocolPath);
	    }
	    protocolPaths = System.getProperty(protocolPathProp);
	    debugPrintln(3, "new protocol paths = " + protocolPaths);

	    url = new URL("plgui:navigateTo?module=AIRCRAFT-KB&type=CONCEPT&id=AIRPLANE");
	    debugPrintln(3, "url = " + url);
	    debugPrintln(3, "protocol = " + url.getProtocol());
	    debugPrintln(3, "path = " + url.getPath());
	    debugPrintln(3, "query = " + url.getQuery());

	} catch (Exception e) {
	    e.printStackTrace();
	}
    }

}// HTMLBrowserFrame
