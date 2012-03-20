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


// Version: PLListRenderer.java,v 1.17 2010/02/04 05:18:37 hans Exp

package edu.isi.powerloom.gui.components;

import java.awt.*;
import javax.swing.*;
import javax.swing.text.*;
import java.util.*;

import edu.isi.powerloom.gui.xmlobject.*;
import static edu.isi.powerloom.gui.common.Utils.*;

/**
 * Custom list renderer for displaying PLObjects.
 *
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 * @since Sun Mar 03 20:33:15 2002
 */

public class PLListRenderer implements ListCellRenderer {
    private static JTextPane theComponent = null;
    // package protection so that UISwitchListener can modify it.
    static Font theFont = null;
    private PLModule module;

    private static JTextPane getComponent() {
	if (theComponent == null) {
	    theComponent = new ExpressionEditorPanel();
	}
	return theComponent;
    }

    private static Font getListFont() {
	if (theFont == null) {
	    // NOTE: Use the tree font, because the list font is bold (which sux).
	    Font currentListFont = (Font)UIManager.getDefaults().get("Tree.font");
	    //	    theFont = new Font("Dialog", Font.PLAIN, 12);
	    theFont = currentListFont;
	    debugPrintln(3, "---List font is: " + currentListFont);
	    //theFont = UIManager.getDefaults().getFont("List.font");
	}
	return theFont;
    }

    public PLListRenderer() {
    }

    public Component getListCellRendererComponent(JList list, Object value, int index, boolean selected, 
                                                  boolean cellHasFocus) {

	JTextPane result = getComponent();
	result.setText("");
	result.setBounds(1,1,1,1);
	StyleConstants.setItalic(result.getLogicalStyle(), false);
	if (value == null) {
	    return result;
	}
	Icon icon = null;
        if (value instanceof PLRelation) {
            if (((PLRelation)value).attrIsFunction.equals("TRUE")) {
                icon = PowerloomApp.getImage("resources/images/function.gif");
            } else {
                icon = PowerloomApp.getImage("resources/images/relation.gif");
            } 
        } else if (value instanceof PLModule) {
            icon = PowerloomApp.getImage("resources/images/module.gif");
        } else if (value instanceof PLConcept) {
            icon = PowerloomApp.getImage("resources/images/concept.gif");
        } else if (value instanceof PLInstance) {
            icon = PowerloomApp.getImage("resources/images/instance.gif");
        } else if (value instanceof PLProposition) {
            if (((PLProposition)value).attrIsRule.equals("TRUE")) {
                icon = PowerloomApp.getImage("resources/images/rule.gif");
            } else {
                icon = PowerloomApp.getImage("resources/images/proposition.gif");
            }
        }
        
	if (icon != null) {
	    result.insertIcon(icon);
	}
	result.setForeground(Color.BLACK);
	if (value instanceof PLProposition) {
	    if (!((PLProposition)value).attrIsAsserted.equals("TRUE")) {
		StyleConstants.setItalic(result.getLogicalStyle(), true);
	    }
	    if (!((PLProposition)value).attrIsStrict.equals("TRUE")) {
		result.setForeground(Color.GRAY);
	    }
	}
	result.select(1,1);
	String text = "<null>";
	if (value != null) {
	    if (value instanceof PLObject) {
		PLModule module = null;
		ListModel model = list.getModel();
		if (model instanceof PLListModel) {
		    module = ((PLListModel)model).getModule();
		}
		String moduleName = (module == null) ? "" : module.getID();
		text = RenderUtils.getModuleSensitiveFQN(moduleName, (PLObject)value);
	    } else {
		text = (String)value.toString();
	    }
	}
	result.replaceSelection(text);
        result.setFont(getListFont());
	Insets insets = new Insets(1,1,1,1);
	result.setMargin(insets);
	result.getHighlighter().removeAllHighlights();
	result.setBackground(Color.WHITE);
	if (selected) {
	    try {
		debugPrintln(3, " PLList: text = " + text + " is selected.");
		result.getHighlighter().addHighlight(((icon == null) ? 0 : 1),text.length()+1,new DefaultHighlighter.DefaultHighlightPainter(list.getSelectionBackground()));
	    } catch (Exception e) {
		PowerloomApp.getInstance().handleException(e);
	    }
	}
	int iconHeight = 12;
	if (icon != null) {
	    iconHeight = ((ImageIcon)icon).getImage().getHeight(null);
	}
	int fixedHeight = iconHeight + 7;
	// getPreferredSize is also a very expensive call: it takes up 15% of the time....
	int curPreferredWidth = (int)result.getPreferredSize().getWidth();
	int curMinWidth = (int)result.getMinimumSize().getWidth();
	int curMaxWidth = (int)result.getMaximumSize().getWidth();
	result.setPreferredSize(new Dimension(curPreferredWidth, fixedHeight));
	result.setMaximumSize(new Dimension(curMaxWidth, fixedHeight));
	result.setMinimumSize(new Dimension(curMinWidth, fixedHeight));
	result.setCaretPosition(0);  // avoid highlighting effect w/propositions and rules
        return result;
    }

    public String getToolTipText() {
	//todo: make tooltips work for lists...
	return "";
        //return getText();
    }
}
