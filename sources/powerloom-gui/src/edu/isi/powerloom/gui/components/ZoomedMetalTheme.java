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


// Version: ZoomedMetalTheme.java,v 1.1 2010/02/04 23:40:06 hans Exp

package edu.isi.powerloom.gui.components;

import javax.swing.plaf.*;
import javax.swing.plaf.metal.*;
import javax.swing.*;
import javax.swing.border.*;
import java.awt.*;

// TO DO: Figure out how to preserve the color of the initial background, we
//        are getting a darker blue with any change of the theme.


/**
 * Class that zooms fonts and icons of the default Metal theme
 * according to a zoom factor.
 */
public class ZoomedMetalTheme extends DefaultMetalTheme {

    private static final int minSize = 8;
    private static final int maxSize = 32;

    private float zoom = 1.0f;
    
    public ZoomedMetalTheme(float zoom) {
        super();
        this.zoom = zoom;
        initializeDefaults();
    }
    
    public String getName() {
        return "Zoomed Metal Theme; zoom=" + zoom;
    }
    
    private static FontUIResource defaultControlTextFont = null;
    private static FontUIResource defaultMenuTextFont = null;
    private static FontUIResource defaultSubTextFont = null;
    private static FontUIResource defaultSystemTextFont = null;
    private static FontUIResource defaultUserTextFont = null;
    private static FontUIResource defaultWindowTitleFont = null;
    private static int defaultFrameIconSize  = 16;
    private static int defaultScrollBarWidth = 10;

    private void initializeDefaults() {
        // Initialize reference values that form the basis for zooming.
        if (defaultControlTextFont == null) {
            MetalTheme theme = new DefaultMetalTheme();
            defaultControlTextFont = theme.getControlTextFont();
            defaultMenuTextFont = theme.getMenuTextFont();
            defaultSubTextFont = theme.getSubTextFont();
            defaultSystemTextFont = theme.getSystemTextFont();
            defaultUserTextFont = theme.getUserTextFont();
            defaultWindowTitleFont = theme.getWindowTitleFont();
            defaultFrameIconSize =
                (UIManager.getDefaults().getIcon("InternalFrame.closeIcon")).getIconHeight();
            defaultScrollBarWidth = UIManager.getDefaults().getInt("ScrollBar.width");
        }
    }

    private int getZoomedSize(int baseSize, float zoom) {
        int zoomedSize = (int)(baseSize * zoom);
        if (zoomedSize < minSize)
            zoomedSize = minSize;
        if (zoomedSize > maxSize)
            zoomedSize = maxSize;
        return zoomedSize;
    }
    
    private FontUIResource getZoomedFont(FontUIResource baseFont, float zoom) {
        return new FontUIResource(baseFont.getFontName(), 
                                  baseFont.getStyle(),
                                  getZoomedSize(baseFont.getSize(), zoom));
    }
    
    public FontUIResource getControlTextFont() {
        return getZoomedFont(defaultControlTextFont, zoom);
    }

    public FontUIResource getMenuTextFont() {
        return getZoomedFont(defaultMenuTextFont, zoom);
    }

    public FontUIResource getSubTextFont() {
        return getZoomedFont(defaultSubTextFont, zoom);
    }

    public FontUIResource getSystemTextFont() {
        return getZoomedFont(defaultSystemTextFont, zoom);
    }

    public FontUIResource getUserTextFont() {
        return getZoomedFont(defaultUserTextFont, zoom);
    }

    public FontUIResource getWindowTitleFont() {
        return getZoomedFont(defaultWindowTitleFont, zoom);
    }

    public void addCustomEntriesToTable(UIDefaults table) {
        super.addCustomEntriesToTable(table);
        int zoomedSize = getZoomedSize(defaultFrameIconSize, zoom);
        table.put("InternalFrame.closeIcon",
                  MetalIconFactory.getInternalFrameCloseIcon(zoomedSize));
        table.put("InternalFrame.iconifyIcon",
                  MetalIconFactory.getInternalFrameMinimizeIcon(zoomedSize));
        table.put("InternalFrame.maximizeIcon",
                  MetalIconFactory.getInternalFrameMaximizeIcon(zoomedSize));
        table.put("InternalFrame.minimizeIcon",
                  MetalIconFactory.getInternalFrameAltMaximizeIcon(zoomedSize));
        table.put( "ScrollBar.width",
                   new Integer(getZoomedSize(defaultScrollBarWidth, zoom)));
    }
}
