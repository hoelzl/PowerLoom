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


// Version: PLClipboardRecord.java,v 1.4 2010/02/04 05:18:20 hans Exp

package edu.isi.powerloom.gui.components;

/**
 * Record for representing a cut/copy source and a paste target.
 *
 * @since Wed Oct  2 18:49:50 2002
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 * @see edu.isi.powerloom.gui.components.DataTransferInfo DataTransferInfo
 * @see edu.isi.powerloom.gui.components.DataTransferManager DataTransferManager
 * @see edu.isi.powerloom.gui.components.PLClipboardOwnerTable PLClipboardOwnerTable
 */
public abstract class PLClipboardRecord {
    public String sourceName;
    public String targetName;

    public PLClipboardRecord (){
    }

    public boolean equals(Object object) {
	if (!(object instanceof PLClipboardRecord)) {
	    return false;
	}
	PLClipboardRecord otherRecord = (PLClipboardRecord)object;
	DataTransferInfo transferInfo = DataTransferInfo.getInstance();
	//	System.out.println("comparing : " + sourceName + " to " + otherRecord.sourceName);
	//	System.out.println("this.nameIsType = " + transferInfo.nameIsType(sourceName));
	//	System.out.println("other.nameIsType = " + transferInfo.nameIsType(otherRecord.sourceName));
	if ((sourceName != null) && (otherRecord.sourceName != null)) {
	    //	    System.out.println("1");
	    if (!(sourceName.equals(otherRecord.sourceName))) {
		//		System.out.println("2");
		if (transferInfo.nameIsType(sourceName)) {
		    //		    System.out.println("3");
		    if (transferInfo.nameIsType(otherRecord.sourceName)) {
			//			System.out.println("4");
			if (!transferInfo.typesAreCompatible(sourceName, otherRecord.sourceName)) {
			    //			    System.out.println("5");
			    return false;
			}
		    } else { // sourceName is type, other.sourceName is name
			//			System.out.println("6");
			if (!transferInfo.nameMatchesType(otherRecord.sourceName, sourceName)) {
			    //			    System.out.println("7");
			    return false;
			}
		    }
		} else { // sourceName is a name, not a type
		    //		    System.out.println("8");
		    if (transferInfo.nameIsType(otherRecord.sourceName)) {
			//			System.out.println("9");
			if (!transferInfo.nameMatchesType(sourceName, otherRecord.sourceName)) {
			    //			    System.out.println("10");
			    return false;
			}
		    } else {
			//			System.out.println("11");
			return false;  // we already know that names don't match
		    }
		}
	    }
	}

	// now do the same thing with the target
	if ((targetName != null) && (otherRecord.targetName != null)) {
	    if (!(targetName.equals(otherRecord.targetName))) {
		if (transferInfo.nameIsType(targetName)) {
		    if (transferInfo.nameIsType(otherRecord.targetName)) {
			if (!transferInfo.typesAreCompatible(targetName, otherRecord.targetName)) {
			    return false;
			}
		    } else { // targetName is type, other.targetName is name
			if (!transferInfo.nameMatchesType(otherRecord.targetName, targetName)) {
			    return false;
			}
		    }
		} else { // targetName is a name, not a type
		    if (transferInfo.nameIsType(otherRecord.targetName)) {
			if (!transferInfo.nameMatchesType(targetName, otherRecord.targetName)) {
			    return false;
			}
		    } else {
			return false;  // we already know that names don't match
		    }
		}
	    }
	}



	// we've made it this far, there must be a match (hopefully :)
	return true;
    }

}// PLClipboardRecord
