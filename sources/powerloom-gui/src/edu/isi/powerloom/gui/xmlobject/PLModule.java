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


// Version: PLModule.java,v 1.4 2010/02/04 05:20:50 hans Exp

package edu.isi.powerloom.gui.xmlobject;

import java.util.*;

/**
 * XML Object representation of a module.
 *
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 */
public class PLModule extends XMLObject implements PLTreeNode, PLObject {
    private boolean undefined;

    // Attributes:
    public String attrModuleName;
    public String attrSourceString;
   	public String attrCppPackage;
	public String attrLispPackage;
	public String attrJavaPackage;
	public String attrJavaCatchallClass;
	public String attrDocumentation;
	public String attrAPI;
	public String attrCaseSensitive;
    public Collection elemPLModule;
    public Collection elemPLSurrogate;
    PLSurrogateContainer internedChildSurrogates;

    // todo: rename this to getChild
    public PLTreeNode getChild(int index) {
        return getChildSurrogates().getChild(index);
    }
    public int getChildCount() {
        return getChildSurrogates().getChildCount();
    }
    // return interned surrogates if they exist, otherwise return the uninterned ones
    public PLSurrogateContainer getChildSurrogates() {
        if (internedChildSurrogates != null ) {
            return internedChildSurrogates;
        } else {
            if (elemPLSurrogate == null) {
                elemPLSurrogate = new ArrayList();
            }
            return new PLSurrogateContainer((ArrayList)elemPLSurrogate);
        } 
    }
    // used for surrogate identification
    public String getID() {
        return attrModuleName;
    }

    public String getModule() {
	return null;
    }

    public int getIndexOfChild(PLTreeNode child) {
        return getChildSurrogates().getIndexOfChild(child);
    }
    public String getModuleName() {
        return attrModuleName;
    }
    public String getSourceString() {
        return attrSourceString;
    }
    public boolean isLeaf() {
        return getChildSurrogates().isLeaf();
    }
    /**
     * JTree support
     */
    public void setChild(int index, PLTreeNode child) {
        getChildSurrogates().setChild(index, child);
    }
    public void setChildSurrogates(PLSurrogateContainer container) {
        internedChildSurrogates = container;
    }
    public String toString() {
        return attrModuleName;
    }

    public boolean isUndefined() {
	return undefined;
    }

    public void setUndefined(boolean flag) {
	undefined = flag;
    }
}
