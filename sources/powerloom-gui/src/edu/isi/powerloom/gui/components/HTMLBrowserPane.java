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


// Version: HTMLBrowserPane.java,v 1.8 2010/02/04 05:17:38 hans Exp

package edu.isi.powerloom.gui.components;

import edu.isi.powerloom.gui.xmlobject.*;
import edu.isi.powerloom.gui.serverinterface.*;
import static edu.isi.powerloom.gui.common.Utils.*;

import javax.swing.*;
import java.awt.Container;
import java.awt.Cursor;
import java.net.URL;
import java.net.MalformedURLException;
import java.io.*;
import javax.swing.text.*;
import javax.swing.event.*;
import java.util.*;

/**
 * Implements HTML browser functionality.
 *
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 * @see edu.isi.powerloom.gui.components.HTMLBrowserFrame
 */
public class HTMLBrowserPane extends JScrollPane implements HyperlinkListener {
    private static final int RELOAD = 0;
    private static final int FORWARD = 1;
    private static final int BACK = 2;
    private static final int NEW = 3;
    JEditorPane html;
    PLFrame parentFrame;
    History history = new History();

    class History {
	List urlHistory = new ArrayList();
	int cursor = -1;
	
	public boolean isAtEndOfHistory() {
	    return (urlHistory.isEmpty() || (cursor == (urlHistory.size() - 1)));
	}

	public boolean isAtBeginningOfHistory() {
	    return (urlHistory.isEmpty() || (cursor == 0));
	}

	/**
	 * Adds a URL to the current point in the history, truncating the rest of the history
	 */
	public void addURL(URL url) {
	    if (isAtEndOfHistory()) {
		urlHistory.add(url);
		cursor = urlHistory.size() - 1;
	    } else {
		List newHistory = new ArrayList();
		for (int i = 0; i <= cursor; i++) {
		    newHistory.add(urlHistory.get(i));
		}
		newHistory.add(url);
		urlHistory = newHistory;
		cursor = urlHistory.size() - 1;
	    }
	}
	
	public URL getCurrentURL() {
	    return (URL)urlHistory.get(cursor);
	}

	public URL back() {
	    if (cursor > 0) {
		cursor--;
	    }
	    return getCurrentURL();
	}

	public URL forward() {
	    if (cursor < (urlHistory.size() - 1)) {
		cursor++;
	    }
	    return getCurrentURL();
	}

	public void updateBrowserActionStatus(HTMLBrowserFrame browserFrame) {
	    browserFrame.updateActionStatus(!isAtBeginningOfHistory(), !isAtEndOfHistory());
	}
    }

    public JEditorPane getEditorPane() {
	return html;
    }

    public HTMLBrowserPane(String htmlText) {
	try {
	    debugPrintln(3, "setting HTMLBrowserPane text to: " + htmlText);
	    html = new JEditorPane("text/html", htmlText);
	    html.setEditable(false);
	    html.addHyperlinkListener(this);

	    JViewport vp = getViewport();
	    vp.add(html);
        } catch (Exception e) {
	    PowerloomApp.getInstance().handleException(e);
	}
    }

    public HTMLBrowserPane(URL url) {
	try {
	    html = new JEditorPane(url);
	    //	    html = new JEditorPane("text/html", "<HTML><HEAD></HEAD><BODY>Navigate to <A HREF=\"plgui:navigateTo?module=AIRCRAFT-KB&type=CONCEPT&id=STRATEGIC-BOMBING\">Strategic Bombing</A></BODY></HTML>");

	    html.setEditable(false);
	    html.addHyperlinkListener(this);

	    JViewport vp = getViewport();
	    vp.add(html);
	} catch (MalformedURLException e) {
	    debugPrintln(3, "Malformed URL: " + e);
	    return;
	} catch (IOException e) {
	    debugPrintln(3, "IOException: " + e);
	    return;
	} catch (Exception e) {
	    PowerloomApp.getInstance().handleException(e);
	}
	history.addURL(url);
    }

    public URL getURL() {
	return history.getCurrentURL();
    }

    public void setParentFrame(PLFrame frame) {
	this.parentFrame = frame;
    }

    public void reload() {
	linkActivated(history.getCurrentURL(), HTMLBrowserPane.RELOAD);
    }

    public void forward() {
	linkActivated(history.forward(), HTMLBrowserPane.FORWARD);
    }

    public void back() {
	linkActivated(history.back(), HTMLBrowserPane.BACK);
    }

    /**
     * Notification of a change relative to a 
     * hyperlink.
     */
    public void hyperlinkUpdate(HyperlinkEvent e) {
	if (e.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
	    linkActivated(e.getURL(), HTMLBrowserPane.NEW);
	}
    }

    public void setURL(URL url) {
	linkActivated(url, HTMLBrowserPane.NEW);
    }

    public void setHTML(String htmlString) {
	html.setText(htmlString);
    }

    /**
     * Follows the reference in an
     * link.  The given url is the requested reference.
     * By default this calls <a href="#setPage">setPage</a>,
     * and if an exception is thrown the original previous
     * document is restored and a beep sounded.  If an 
     * attempt was made to follow a link, but it represented
     * a malformed url, this method will be called with a
     * null argument.
     *
     * @param u the URL to follow
     */
    protected void linkActivated(URL u, int operation) {
	debugPrintln(3, "linkactivated, url = " + u);
	String protocol = u.getProtocol();
	if (protocol.equalsIgnoreCase("http")) {
	    activateHTTPLink(u, operation);
	} else if (protocol.equalsIgnoreCase("plgui")) {
	    activatePLGUILink(u, operation);
	}
    }

    protected void activateHTTPLink(URL url, int operation) {
	Cursor c = html.getCursor();
	Cursor waitCursor = Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR);
	html.setCursor(waitCursor);
	SwingUtilities.invokeLater(new PageLoader(parentFrame, history, operation, url, c));
    }

    protected void activatePLGUILink(URL url, int operation) {
	debugPrintln(3, "Activated plgui link: " + url);
	debugPrintln(3, "path = " + url.getPath());
	debugPrintln(3, "query = " + url.getQuery());
	Properties queryProps = parseURLQueryParameters(url.getQuery());
	String module = (String)queryProps.get("module");
	String type = (String)queryProps.get("type");
	String id = (String)queryProps.get("id");
	debugPrintln(3, "module = " + module);
	debugPrintln(3, "type   = " + type);
	debugPrintln(3, "id     = " + id);
	navigateToObject(module, type, id);
    }

    /**
     * temporary class that loads synchronously (although
     * later than the request so that a cursor change
     * can be done).
     */
    class PageLoader implements Runnable {
	PLFrame parentFrame;
	History history;
	int operation;
	
	PageLoader(PLFrame parentFrame, History history, int operation, URL u, Cursor c) {
	    this.parentFrame = parentFrame;
	    this.history = history;
	    this.operation = operation;
	    url = u;
	    cursor = c;
	}

        public void run() {
	    if (url == null) {
		// restore the original cursor
		html.setCursor(cursor);

		// PENDING(prinz) remove this hack when 
		// automatic validation is activated.
		Container parent = html.getParent();
		parent.repaint();
	    } else {
		Document doc = html.getDocument();
		try {
		    html.setPage(url);
		    // If we've made it here, we successfully loaded the page.
		    // Update the history if appropriate.
		    if (history != null) {
			if (operation == HTMLBrowserPane.NEW) {
			    history.addURL (url);
			}
			if (parentFrame != null) {
			    if (parentFrame instanceof HTMLBrowserFrame) {
				history.updateBrowserActionStatus((HTMLBrowserFrame)parentFrame);
			    }
			}
		    }
		} catch (IOException ioe) {
		    html.setDocument(doc);
		    getToolkit().beep();
		} finally {
		    // schedule the cursor to revert after
		    // the paint has happended.
		    url = null;
		    SwingUtilities.invokeLater(this);
		}
	    }
	}

	URL url;
	Cursor cursor;
    }

    protected void navigateToObject(String moduleName, String type, String objectName) {
	try {
	    PLModule module = KnowledgeManager.getInstance().getModuleFromName(moduleName);
	    if (module != null) {
		PLObject plObject = KnowledgeManager.getInstance().getPLObject(module, objectName);
		if (plObject != null) {
		    NavigateAction action = new NavigateAction();
		    action.setModule(module);
		    action.setObject(plObject);
		    action.actionPerformed(null);
		} else {
		    throw new Exception("unrecognized object: " + objectName);
		}
	    } else {
		throw new Exception("unrecognized module: " + moduleName);
	    }
	} catch (Exception e) {
	    PowerloomApp.getInstance().handleException(e);
	}

    }

    /**
     * Convert an "&"-separated list of URL GET-style parameters into a property list
     */
    public static Properties parseURLQueryParameters(String parameters) {
	Properties result = new Properties();
	StringTokenizer st = new StringTokenizer(parameters, "&");
	while (st.hasMoreTokens()) {
	    String token = st.nextToken();
	    int equalsIndex = token.indexOf('=');
	    String key = "";
	    String value = "";
	    if (equalsIndex >= 0) {
		key = token.substring(0, equalsIndex);
		value = token.substring(equalsIndex + 1);
	    }
	    if (key.trim().length() > 0) {
		result.put(key, value);
	    }
	}
	return result;
    }

}
