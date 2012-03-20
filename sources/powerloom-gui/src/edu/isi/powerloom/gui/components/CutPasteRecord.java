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


// Version: CutPasteRecord.java,v 1.3 2010/02/04 05:17:04 hans Exp

package edu.isi.powerloom.gui.components;

/**
 * Record used for representing a legal cut/paste source and destination.
 *
 * @since Wed Oct  2 19:04:11 2002
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 * @see edu.isi.powerloom.gui.components.DataTransferInfo DataTransferInfo
 * @see edu.isi.powerloom.gui.components.DataTransferManager DataTransferManager
 */

public class CutPasteRecord extends PLClipboardRecord {
    public int operation;

    public CutPasteRecord (){
    }

    public CutPasteRecord(String sourceName, String targetName, int operation) {
	this.sourceName = sourceName;
	this.targetName = targetName;
	this.operation = operation;
    }

    public CutPasteRecord(String sourceName, String targetName) {
	this(sourceName, targetName, -1);
    }

    public String toString() {
	return "[source:" + sourceName + ", target: " + targetName + ", operation: " + ((operation == DataTransferInfo.CUT) ? "Cut" : "Copy") + "]";
    }

    public boolean equals(Object object) {
	//System.out.print("comparing " + this + ", " + object + " -> ");

	if (!(object instanceof CutPasteRecord)) {
	    //  System.out.println("false1");
	    return false;
	}
	CutPasteRecord otherRecord = (CutPasteRecord)object;
	DataTransferInfo transferInfo = DataTransferInfo.getInstance();

	boolean baseResult = super.equals(object);
	if (baseResult == false) {
	    //System.out.println("false2");
	    return false;
	}

	// Furthermore, check operation match
	if ((operation >= 0) && (otherRecord.operation >= 0)) {
	    if (operation != otherRecord.operation) {
		//System.out.println("false3");
		return false;
	    }
	}
	//System.out.println("true");
	return true;
    }
    
}// CutPasteRecord
