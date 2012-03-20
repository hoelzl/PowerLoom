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


// Version: ConsolePanel.java,v 1.4 2010/02/04 05:17:01 hans Exp

package edu.isi.powerloom.gui.components;

import java.awt.*;
import java.awt.event.*;
import javax.swing.text.*;
import javax.swing.*;

import edu.isi.powerloom.gui.serverinterface.*;
import edu.isi.powerloom.gui.xmlobject.*;
import static edu.isi.powerloom.gui.common.Utils.*;


/**
 * Implementation of powerloom console functionality.
 *
 * @since Fri Oct 11 12:19:41 2002
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */

public class ConsolePanel extends ExpressionEditorPanel implements ActionComponent {
    private static String promptString = ">";
    private static MutableAttributeSet promptAttributes;
    private static MutableAttributeSet plainAttributes;
    private static MutableAttributeSet responseAttributes;
    private static Style outputStyle;

    static {
	promptAttributes = new SimpleAttributeSet();
	StyleConstants.setForeground(promptAttributes, Color.GREEN);
	plainAttributes = new SimpleAttributeSet();
	StyleConstants.setForeground(plainAttributes, Color.BLACK);
	responseAttributes = new SimpleAttributeSet();
	StyleConstants.setForeground(responseAttributes, Color.RED);
    }

    public ConsolePanel (){
    }


    public ConsolePanel(PLFrame parent, String name, java.util.List types) {
	super(parent, name, types);
    }

    public void initialize() {
	super.initialize();
	addFocusListener(new FocusListener() {
		public void focusGained(FocusEvent e) {
		    debugPrintln(3, "setting current component to consolepanel");
		    PowerloomApp.getInstance().setSelectedActionComponent(ConsolePanel.this);
		    PowerloomApp.getInstance().flashStatusMessage2("Type CTRL-ENTER to execute command.");
		}
		public void focusLost(FocusEvent e) {
		    debugPrintln(3, "focus lost in consolepanel");
		    PowerloomApp.getInstance().getStatusMsg2().setText("");
		}
		
	    });
	showPrompt();
    }

    private void maybeInsertNewline() {	
	Document doc = getStyledDocument();
	try {
	    if (!(getText().length() == 0) && !(getText().charAt(getText().length() - 1) == '\n')) {
		doc.insertString(doc.getLength(), "\n", plainAttributes);
	    }
	} catch (BadLocationException e) {
	    PowerloomApp.getInstance().handleException(e);
	}
    }

    private void showPrompt() {
	maybeInsertNewline();
	Document doc = getStyledDocument();
	try {
	    doc.insertString(doc.getLength(), promptString, promptAttributes);
	    // kludgy way to switch styles.  Todo: use setLogicalStyle()
	    doc.insertString(doc.getLength(), " ", plainAttributes);
	} catch (BadLocationException e) {
	    PowerloomApp.getInstance().handleException(e);
	}
    }

    private String getCurrentCommand() {
	String text = getText();
	// assume the prompt hasn't been deleted.... 
	int promptPos = text.lastIndexOf(promptString);
	String command = null;
	if (promptPos >= 0) {
	  command = text.substring(promptPos+1);
	}
	debugPrintln(3, "command = " + command);
	return command;
    }

    private String executeCommand(String command) {
	String result = "";
	try {
	    PLModule module = getModule();
	    if (module == null) {
		JOptionPane.showMessageDialog(PowerloomApp.getInstance(), "You must select a module before you execute a command.", "Select Module", JOptionPane.WARNING_MESSAGE);
		return null;
	    }
	    result = KnowledgeManager.getInstance().evaluateLogicCommand(module, command);
	} catch (Exception e) {
	    PowerloomApp.getInstance().handleException(e);
	}
	return result;
    }

    private void showResponse(String response) {
	maybeInsertNewline();
	Document doc = getStyledDocument();
	try {
	    doc.insertString(doc.getLength(), response, responseAttributes);
	}  catch (BadLocationException e) {
	    PowerloomApp.getInstance().handleException(e);
	}
    }

    public void doAction() {
	debugPrintln(3, "You hit return!");
	String command = getCurrentCommand();
	String response = executeCommand(command);
	if (response != null) {
	    showResponse(response);
	    showPrompt();
	}
    }
    
}// ConsolePanel
