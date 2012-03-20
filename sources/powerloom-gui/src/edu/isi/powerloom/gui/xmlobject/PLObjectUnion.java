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


// Version: PLObjectUnion.java,v 1.9 2010/02/04 05:20:56 hans Exp

package edu.isi.powerloom.gui.xmlobject;

import edu.isi.powerloom.gui.serverinterface.*;
import static edu.isi.powerloom.gui.common.Utils.*;
import java.util.*;

/**
 * "Union" type which can hold many types of objects, including literals and PLObjects.
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */
public class PLObjectUnion extends XMLObject {
    public String attrType;
    public String attrLiteralValue;
    public PLSurrogate elemPLSurrogate;

    public PLObjectUnion copy() {
	PLObjectUnion result = new PLObjectUnion();
	result.attrType = attrType;
	result.attrLiteralValue = attrLiteralValue;
	result.elemPLSurrogate = elemPLSurrogate;
	return result;
    }

    // retrieve an object, which may either be a PLObject such as a concept
    // or a wrapped literal such as an INTEGER
    public Object getObject() {
	if (attrType == null) {
	    return null;
	}
	if (attrType.equals("INTEGER")) {
	    if (attrLiteralValue.equals("NULL-INTEGER")) {
		return null;
	    }
	    try {
		int result = Integer.parseInt(attrLiteralValue);
		return new Integer(result);
	    } catch (Exception e) {
		e.printStackTrace();
		edu.isi.powerloom.gui.components.PowerloomApp.getInstance().handleException(e);
	    } // end of try-catch
	} else if (attrType.equals("FLOAT")) {
	    if (attrLiteralValue.equals("NULL-FLOAT")) {
		return null;
	    }
	    try {
		float result = Float.parseFloat(attrLiteralValue);
		return new Float(result);
	    } catch (Exception e) {
		e.printStackTrace();
		edu.isi.powerloom.gui.components.PowerloomApp.getInstance().handleException(e);
	    } // end of try-catch
	} else if (attrType.equals("INSTANCE") ||
		   attrType.equals("CONCEPT") ||
		   attrType.equals("RELATION")) {
	    return elemPLSurrogate.getValue();
	} else if (attrType.equals("TRUTH-VALUE")) {
	    return attrLiteralValue;
	} else if (attrType.equals("STRING")) {
	    return attrLiteralValue;
	}
	return null;
    }

    public void setObject(Object object) throws Exception {
	String text = "";
	if (object != null) {
	    text = object.toString();
	}
	attrType = getTypeFromObject(object);
	elemPLSurrogate = null;
	attrLiteralValue = null;
	if (attrType.equals("INSTANCE")) {
	    elemPLSurrogate = KnowledgeManager.getInstance().findOrCreateSurrogate(PLInstance.class, text);
	} else if (attrType.equals("CONCEPT")) {
	    elemPLSurrogate = KnowledgeManager.getInstance().findOrCreateSurrogate(PLConcept.class, text);
	} else if (attrType.equals("RELATION")) {
	    elemPLSurrogate = KnowledgeManager.getInstance().findOrCreateSurrogate(PLRelation.class, text);
	} else if (attrType.equals("TRUTH-VALUE")) {
	    attrLiteralValue = text;
	} else if (attrType.equals("INTEGER")) {
	    if (text.equals("")) {
		attrLiteralValue = "NULL-INTEGER";
	    } else {
		attrLiteralValue = text;
	    }
	} else if (attrType.equals("FLOAT")) {
	    if (text.equals("")) {
		attrLiteralValue = "NULL-FLOAT";
	    } else {
		attrLiteralValue = text;
	    }
	} else if (attrType.equals("STRING")) {
	    attrLiteralValue = text;
	} else {
	    throw new Exception("**Error: can't get type for object: " + object);
	}
    }

    public static Object getObjectFromString(String text) throws Exception {
	PLSurrogate surrogate = KnowledgeManager.getInstance().findOrCreateSurrogate(PLInstance.class, text);
	PLInstance instance = (PLInstance)surrogate.getValue();
	if (instance != null) {
	    return instance;
	}
	surrogate = KnowledgeManager.getInstance().findOrCreateSurrogate(PLConcept.class, text);
	PLConcept concept = (PLConcept)surrogate.getValue();
	if (concept != null) {
	    return concept;
	}
	surrogate = KnowledgeManager.getInstance().findOrCreateSurrogate(PLRelation.class, text);
	PLRelation relation = (PLRelation)surrogate.getValue();
	if (relation != null) {
	    return relation;
	}
	Integer intVal = null;
	try {
	    intVal = new Integer(Integer.parseInt(text));
	} catch (Exception e) {
	    intVal = null;
	}
	if (intVal != null) {
	    return intVal;
	}
	Float floatVal = null;
	try {
	    floatVal = new Float(Float.parseFloat(text));
	} catch (Exception e) {
	    floatVal = null;
	}
	if (floatVal != null) {
	    return floatVal;
	}
	return text;
    }

    public static String getTypeFromObject(Object object) {
	if (object instanceof PLInstance) {
	    return "INSTANCE";
	}
	if (object instanceof PLConcept) {
	    return "CONCEPT";
	}
	if (object instanceof PLConcept) {
	    return "RELATION";
	}
	if (object instanceof Integer) {
	    return "INTEGER";
	}
	if (object instanceof Float) {
	    return "FLOAT";
	}
	if (object.equals("TRUE") ||
	    object.equals("FALSE") ||
	    object.equals("UNKNOWN")) {
	    return "TRUTH-VALUE";
	}
	return "STRING";
    }

    // used for generating objects that powerloom can read.
    public String toReadableString() {
	String result = "";
	if (attrType == null) {
	    return null;
	}
	if ((attrType.equals("INTEGER")) ||
	    (attrType.equals("FLOAT")) ||
	    (attrType.equals("STRING")) ||
	    (attrType.equals("TRUTH-VALUE"))) {
	    result += attrLiteralValue;
	}
	if (elemPLSurrogate != null) {
	    result += elemPLSurrogate.getID();
	}
	return result;
    }

    public String toString() {
	//	debugPrintln(3, "    attrType = " + attrType + ", literalValue = " + attrLiteralValue + ", surrogate = " + ((elemPLSurrogate != null) ? elemPLSurrogate.getID() : null));
	String result = "[" + attrType + ":";
	result += toReadableString();
	result += "]";
        return result;
    }
}
