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
 | Portions created by the Initial Developer are Copyright (C) 2010           |
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


// Version: ExtensionFileFilter.java,v 1.1 2010/04/13 02:55:15 hans Exp

package edu.isi.powerloom.gui.components;

import java.util.*;
import java.io.File;
import javax.swing.filechooser.FileFilter;


/**
 * FileFilter for file chooser applications that filters files based
 * the extension component of their name.
 */
public class ExtensionFileFilter extends FileFilter {

    private java.util.Collection extensions = new ArrayList();
    private String description = "All files";

    /** Define a textual description for the files allowed by this filter.
     */
    public void setDescription(String desc) {
        description = desc;
    }

    /** Return the textual description for the files allowed by this filter.
     */
    public String getDescription() {
        return description;
    }

    /** Add `extension' to the list of extensions allowed by this filter
     *  (should not contain the dot).
     */
    public void add(String extension) {
        extensions.add(extension);
    }

    /** Remove `extension' from the list of extensions allowed by this filter
     *  (should not contain the dot).
     */
    public void remove(String extension) {
        extensions.remove(extension);
    }

    /** Return true if this filter accepts file `f'.
     */
    public boolean accept(File f) {
        if (f.isHidden())
            return false;
        if (extensions.isEmpty())
            // if no extensions have been registered, allow all files:
            return true;
        if (f.isDirectory())
            return true;
        String name = f.getName();
        int extensionStart = name.lastIndexOf('.');
        if (extensionStart < 0)
            return false;
        else
            return extensions.contains(name.substring(extensionStart + 1));
    }
}
