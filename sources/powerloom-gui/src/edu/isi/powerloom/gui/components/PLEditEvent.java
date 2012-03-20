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


// Version: PLEditEvent.java,v 1.4 2010/02/04 05:18:21 hans Exp

package edu.isi.powerloom.gui.components;

import edu.isi.powerloom.gui.xmlobject.*;

/**
 * Event representing a change in the KB.  Editors fire this event,
 * and knowledge viewers listen for these events.
 *
 * @since 4/12/2002 9:43:45 PM
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */
public class PLEditEvent extends java.util.EventObject {
	private java.lang.Class editType;
        
        private edu.isi.powerloom.gui.xmlobject.PLObject editObject;
        
/**
 * PLEditEvent constructor comment.
 * @param source java.lang.Object
 */
public PLEditEvent(Object source) {
	super(source);
}
/**
 * Insert the method's description here.
 * Creation date: (4/12/2002 9:45:18 PM)
 * @param source java.lang.Object
 * @param editType java.lang.Class
 */
public PLEditEvent(Object source, Class editType) {
	super(source);
	this.editType = editType;
}

public PLEditEvent(Object source, Class editType, PLObject editObject) {
    super(source);
    this.editType = editType;
    this.editObject = editObject;
}

/*
public PLEditEvent(Object source, Class editType, PLObject editObject) {
}

 */
/**
 * Insert the method's description here.
 * Creation date: (4/12/2002 9:46:01 PM)
 * @return java.lang.Class
 */
public java.lang.Class getEditType() {
	return editType;
}
/**
 * Insert the method's description here.
 * Creation date: (4/12/2002 9:46:01 PM)
 * @param newEditType java.lang.Class
 */
public void setEditType(java.lang.Class newEditType) {
	editType = newEditType;
}

public edu.isi.powerloom.gui.xmlobject.PLObject getEditObject() {
    return editObject;
}

public void setEditObject(edu.isi.powerloom.gui.xmlobject.PLObject editObject) {
    this.editObject = editObject;
}

}
