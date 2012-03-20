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


// Version: PopupUtils.java,v 1.9 2010/02/04 05:18:48 hans Exp

package edu.isi.powerloom.gui.components;

import edu.isi.powerloom.gui.xmlobject.*;
import edu.isi.powerloom.gui.parser.*;
import edu.isi.powerloom.gui.serverinterface.*;
import static edu.isi.powerloom.gui.common.Utils.*;

import javax.swing.*;
import javax.swing.text.*;
import javax.swing.table.*;
import javax.swing.tree.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.event.*;

/**
 * Class with static methods for helping to create right-click popup menus.
 *
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 * @since Wed Sep 25 17:20:10 2002
 */
	
public class PopupUtils {

    /**
     * Interface used for dynamically generating menu items.  This is useful
     * for menu items that depend on the current runtime environment, such
     * as showing presenting a menu item to show inherited instances vs.
     * showing a menu item to show direct instances.
     */
    public interface ContextSensitiveMenuItemGenerator {
	public java.util.List getContextSensitiveMenuItems();
    }


    public static void updateMostRecentlyTouchedObject(PLObject object) {
	PowerloomApp.getInstance().setMostRecentlyTouchedObject(object);
	if (object instanceof PLModule) {
	    PowerloomApp.getInstance().setMostRecentlyTouchedModule((PLModule)object);
	}
    }
	    
    public static void installListPopup(final PLModule module,
					final JList list, final Collection listMenuItems,
					final Collection itemMenuItems, 
					final Collection listOnlyMenuItems, final Collection subObjectMenuItems,
					final ContextSensitiveMenuItemGenerator csmig) {
	final class ObjectAtPointResult {
	    int charPos;
	    PLObject object;
	    PLModule module;
	}

	final class ObjectFromElementResult {
	    PLObject object;
	    PLModule module;
	}


	list.addMouseListener(new MouseListener() {
		public void mousePressed(MouseEvent e) { checkPopup(e); }
		public void mouseClicked(MouseEvent e) { checkPopup(e); }
		public void mouseEntered(MouseEvent e) {}
		public void mouseExited(MouseEvent e) {}
		public void mouseReleased(MouseEvent e) { checkPopup(e); }
		private void checkPopup(MouseEvent e) {
		    if (e.isPopupTrigger()) {
			ObjectAtPointResult pointInfo = getObjectAtPoint(module, list, e, (subObjectMenuItems != null));

			int index = list.locationToIndex(new java.awt.Point(e.getX(), e.getY()));
			PLObject selectedObject = null;
			if ((index >= 0) && 
			    ((pointInfo == null) || (pointInfo.charPos >= 0))) {
			    selectedObject = (PLObject)list.getModel().getElementAt(index);
			    updateMostRecentlyTouchedObject(selectedObject);
			}
			// Ensure that the list has the focus, so that the cut/paste framework
			// works correctly
			list.requestFocus();
			JPopupMenu popup = createPopupMenu(selectedObject, pointInfo.object, pointInfo.module);
			popup.addFocusListener(new FocusListener() {
			    public void focusGained(FocusEvent e) {
				debugPrintln(3, "doing popup menu.");
			    }

			    public void focusLost(FocusEvent e) {
				PowerloomApp.getInstance().setDoingRightClickMenu(false);
				debugPrintln(3, "did popup menu.");
			    }
			    });
			PowerloomApp.getInstance().setDoingRightClickMenu(true);
			popup.show(list, e.getX(), e.getY());
		    }
		}

		private ObjectAtPointResult getObjectAtPoint(PLModule module, JList list, MouseEvent e, boolean getObject) {
		    ObjectAtPointResult result = new ObjectAtPointResult();
		    try {
			int index = list.locationToIndex(new java.awt.Point(e.getX(), e.getY()));
			//debugPrintln(3, "index = " + index);
			Object currentItem = list.getModel().getElementAt(index);
			if (currentItem == null) {
			    return result;
			}
			ExpressionEditorPanel itemPane = (ExpressionEditorPanel)list.getCellRenderer().getListCellRendererComponent
			    (list, currentItem, index, false, false);
			//debugPrintln(3, "the text of the selected component is: " + itemPane.getText());
			Rectangle cellRect = list.getCellBounds(index, index);
			int cellX = e.getX() - (int)cellRect.getX();
			int cellY = e.getY() - (int)cellRect.getY();
			int textPos = -1;
			if ((cellX >=0) && (cellY >= 0)) {
			    //debugPrintln(3, "cellX = " + cellX + ", cellY = " + cellY);
			    Point mousePoint = new Point(cellX, cellY);
			    //debugPrintln(3, "mousePoint = " + mousePoint);
			    itemPane.setBounds(cellRect);
			    //debugPrintln(3, "itemPane.width = " + itemPane.getWidth());
			    textPos = itemPane.viewToModel(mousePoint);
			    //debugPrintln(3, "the index of the selected char is: " + textPos + " (" + itemPane.getText(textPos, 1) + ")");
			    //debugPrintln(3, "length = " + itemPane.getText().length());
			    result.charPos = textPos;
			    if (textPos >= itemPane.getText().length()) {
				result.charPos = -1;
			    }
			    
			    if (getObject) {
				PLElement parseTree = itemPane.parseExpression();
				//parseTree.dump();
				PLElement leafElement = parseTree.getLeafElement(textPos);
				//debugPrintln(3, "leaf element is: "); leafElement.dump();
				PLObject theObject = null;
				if (leafElement != null) {
				    ObjectFromElementResult ofeResult = getPLObjectFromElement(module, leafElement);
				    result.object = ofeResult.object;
				    result.module = ofeResult.module;
				}
			    }
			    return result;
			} else {
			    System.err.println("error: cellX = " + cellX + ", cellY = " + cellY);
			}
		    } catch (Exception e2) {
			PowerloomApp.getInstance().handleException(e2);
		    }
		    return null;
		}

		private ObjectFromElementResult getPLObjectFromElement(PLModule module, PLElement element) throws Exception {
		    ObjectFromElementResult result = new ObjectFromElementResult();
		    Symbol symbol = element.getSymbol();
		    if (symbol.getName().equals("constant")) {
			PLElement parent = (PLElement)element.getParentElement();

			// new code:
			try {
			    String objectName = element.getToken().getText();
			    PLObject plObject = KnowledgeManager.getInstance().getPLObject(module, objectName);
			    if (plObject != null) {
				PLModule objectModule = KnowledgeManager.getInstance().getModuleFromName(plObject.getModule());
				result.object = plObject;
				result.module = objectModule;
				debugPrintln(3, "retrieved plObject = " + plObject);
				debugPrintln(3, " plObject id = " + plObject.getID());
				debugPrintln(3, " plObject module = " + plObject.getModule());
				debugPrintln(3, " module = " + objectModule);
				debugPrintln(3, " module id = " + objectModule.getID());
			    }
			} catch (Exception e) {
			    PowerloomApp.getInstance().handleException(e);
			}
		    }
		    return result;
		}

		// subModule is the module associated with the subobject
		private JPopupMenu createPopupMenu(PLObject object, PLObject subObject, PLModule subModule) {
		    JPopupMenu result = new JPopupMenu();
		    Iterator iter = listMenuItems.iterator();
		    while (iter.hasNext()) {
			Object iterObject = iter.next();
			if (iterObject instanceof JMenuItem) {
			    JMenuItem item = (JMenuItem)iterObject;
			    result.add(item);
			} else if (iterObject instanceof Action) {
			    Action action = (Action)iterObject;
			    result.add(action);
			} else if (iterObject instanceof Class) {
			    PowerloomAction action = null;
			    try {
				action = (PowerloomAction)((Class)iterObject).newInstance();
			    } catch (Exception e) {
				PowerloomApp.getInstance().handleException(e);
			    } // end of try-catch
			    if (action != null) {
				action.setApp(PowerloomApp.getInstance());
				result.add(action);				 
			    }
			} 
		    }

		    if (object != null) {
			iter = itemMenuItems.iterator();
			while (iter.hasNext()) {
			    Object iterObject = iter.next();
			    if (iterObject instanceof JMenuItem) {
				JMenuItem item = (JMenuItem)iterObject;
				result.add(item);
			    } else if (iterObject instanceof PowerloomAction) {
				PowerloomAction action = (PowerloomAction)iterObject;
				action.setApp(PowerloomApp.getInstance());
				result.add(action);
			    } else if (iterObject instanceof Class) {
				PowerloomAction action = null;
				try {
				    action = (PowerloomAction)((Class)iterObject).newInstance();
				} catch (Exception e) {
				    PowerloomApp.getInstance().handleException(e);
				} // end of try-catch
				if (action != null) {
				    action.setApp(PowerloomApp.getInstance());
				    result.add(action);				 
				}
			    } 
			}

			if ((subObject != null) && (subObjectMenuItems != null)) {
			    iter = subObjectMenuItems.iterator();
			    while (iter.hasNext()) {
				Object iterObject = iter.next();
				if (iterObject instanceof PowerloomAction) {
				    PowerloomAction action = (PowerloomAction)iterObject;
				    action.setApp(PowerloomApp.getInstance());
				    action.setObject(subObject);
				    // this could be dangerous...
				    if (subModule != null) {
					action.setModule(subModule);
				    } else {
					action.setModule(module);
				    }
				    result.add(action);
				}  else if (iterObject instanceof Class) {
				    PowerloomAction action = null;
				    try {
					action = (PowerloomAction)((Class)iterObject).newInstance();
				    } catch (Exception e) {
					PowerloomApp.getInstance().handleException(e);
				    } 
				    if (action != null) {
					action.setApp(PowerloomApp.getInstance());
					action.setObject(subObject);
					if (subModule != null) {
					    action.setModule(subModule);
					} else {
					    action.setModule(module);
					}
					result.add(action);
				    }
				}
			    }
			}
		    } else {  //object == null
			if (listOnlyMenuItems != null) {
			    iter = listOnlyMenuItems.iterator();
			    while (iter.hasNext()) {
				Object iterObject = iter.next();
				if (iterObject instanceof JMenuItem) {
				    JMenuItem item = (JMenuItem)iterObject;
				    result.add(item);
				} else if (iterObject instanceof PowerloomAction) {
				    PowerloomAction action = (PowerloomAction)iterObject;
				    action.setApp(PowerloomApp.getInstance());
				    result.add(action);
				} else if (iterObject instanceof Class) {
				    PowerloomAction action = null;
				    try {
					action = (PowerloomAction)((Class)iterObject).newInstance();
				    } catch (Exception e) {
					PowerloomApp.getInstance().handleException(e);
				    } // end of try-catch
				    if (action != null) {
					action.setApp(PowerloomApp.getInstance());
					result.add(action);				 
				    }
				}
			    }
			}
		    }
		    
		    if (csmig != null) {
			debugPrintln(3, "installing csmig = " + csmig);
			java.util.List contextSensitiveItems = csmig.getContextSensitiveMenuItems();
			iter = contextSensitiveItems.iterator();
			while (iter.hasNext()) {
			    Object iterObject = iter.next();
			    if (iterObject instanceof JMenuItem) {
				JMenuItem item = (JMenuItem)iterObject;
				result.add(item);
			    } else if (iterObject instanceof PowerloomAction) {
				PowerloomAction action = (PowerloomAction)iterObject;
				action.setApp(PowerloomApp.getInstance());
				result.add(action);
			    } else if (iterObject instanceof Class) {
				PowerloomAction action = null;
				try {
				    action = (PowerloomAction)((Class)iterObject).newInstance();
				} catch (Exception e) {
				    PowerloomApp.getInstance().handleException(e);
				} // end of try-catch
				    if (action != null) {
					action.setApp(PowerloomApp.getInstance());
					result.add(action);				 
				    }
			    }
			}
		    }
		    return result;
		}
	    });
    }

    public static void installTreePopup(final JTree tree, final Collection treeMenuItems,
					final Collection itemMenuItems, final Collection treeOnlyMenuItems, 
					final String ignorableRootName, 
					final PopupUtils.ContextSensitiveMenuItemGenerator csmig) {
	tree.addMouseListener(new MouseListener() {
		public void mousePressed(MouseEvent e) { checkPopup(e); }
		public void mouseClicked(MouseEvent e) { checkPopup(e); }
		public void mouseEntered(MouseEvent e) {}
		public void mouseExited(MouseEvent e) {}
		public void mouseReleased(MouseEvent e) { checkPopup(e); }
		private void checkPopup(MouseEvent e) {
		    if (e.isPopupTrigger()) {
			TreePath path = tree.getPathForLocation(e.getX(), e.getY());
			//debugPrintln(3, "path = " + path);
			PLObject selectedObject = null;
			if (path != null) {
			    selectedObject = (PLObject)path.getLastPathComponent();
			    updateMostRecentlyTouchedObject(selectedObject);
			} else {
				updateMostRecentlyTouchedObject(null);
			}
			JPopupMenu popup = createPopupMenu(selectedObject);
			popup.addFocusListener(new FocusListener() {
			    public void focusGained(FocusEvent e) {
				debugPrintln(3, "doing popup menu.");
			    }

			    public void focusLost(FocusEvent e) {
				PowerloomApp.getInstance().setDoingRightClickMenu(false);
				debugPrintln(3, "did popup menu.");
			    }
			    });
			PowerloomApp.getInstance().setDoingRightClickMenu(true);
			popup.show(tree, e.getX(), e.getY());
		    }
		}

		private JPopupMenu createPopupMenu(PLObject object) {
		    JPopupMenu result = new JPopupMenu();
		    Iterator iter = treeMenuItems.iterator();
		    while (iter.hasNext()) {
			Object iterObject = iter.next();
			if (iterObject instanceof JMenuItem) {
			    JMenuItem item = (JMenuItem)iterObject;
			    result.add(item);
			} else if (iterObject instanceof Action) {
			    Action action = (Action)iterObject;
			    result.add(action);
			} else if (iterObject instanceof Class) {
			    PowerloomAction action = null;
			    try {
				action = (PowerloomAction)((Class)iterObject).newInstance();
			    } catch (Exception e) {
				PowerloomApp.getInstance().handleException(e);
			    } // end of try-catch
			    if (action != null) {
				action.setApp(PowerloomApp.getInstance());
				result.add(action);				 
			    }
			}
		    }
            if ((object != null) && !(object.getID().equals(ignorableRootName))) {
			    iter = itemMenuItems.iterator();
			    while (iter.hasNext()) {
					Object iterObject = iter.next();
					if (iterObject instanceof JMenuItem) {
					    JMenuItem item = (JMenuItem)iterObject;
					    result.add(item);
					} else if (iterObject instanceof PowerloomAction) {
					    PowerloomAction action = (PowerloomAction)iterObject;
				 	  	action.setApp(PowerloomApp.getInstance());
				  	    result.add(action);
					} else if (iterObject instanceof Class) {
					    PowerloomAction action = null;
					  	try {
							action = (PowerloomAction)((Class)iterObject).newInstance();
			 	  	    } catch (Exception e) {
						PowerloomApp.getInstance().handleException(e);
				   	    } // end of try-catch
					    if (action != null) {
							action.setApp(PowerloomApp.getInstance());
							result.add(action);				 
			 			}
			  		}
			    }
		    } else { // object == null or is ignorable, we're clicking outside of the tree or on ignorable root
			    iter = treeOnlyMenuItems.iterator();
			    while (iter.hasNext()) {
					Object iterObject = iter.next();
					if (iterObject instanceof JMenuItem) {
					    JMenuItem item = (JMenuItem)iterObject;
					    result.add(item);
					} else if (iterObject instanceof PowerloomAction) {
					    PowerloomAction action = (PowerloomAction)iterObject;
				 	  	action.setApp(PowerloomApp.getInstance());
				  	    result.add(action);
					} else if (iterObject instanceof Class) {
					    PowerloomAction action = null;
					  	try {
							action = (PowerloomAction)((Class)iterObject).newInstance();
			 	  	    } catch (Exception e) {
						PowerloomApp.getInstance().handleException(e);
				   	    } // end of try-catch
					    if (action != null) {
							action.setApp(PowerloomApp.getInstance());
							result.add(action);				 
			 			}
			  		}
			    }
		    }

	    if (csmig != null) {
		java.util.List contextSensitiveItems = csmig.getContextSensitiveMenuItems();
		iter = contextSensitiveItems.iterator();
		while (iter.hasNext()) {
		    Object iterObject = iter.next();
		    if (iterObject instanceof JMenuItem) {
			JMenuItem item = (JMenuItem)iterObject;
			result.add(item);
		    } else if (iterObject instanceof PowerloomAction) {
			PowerloomAction action = (PowerloomAction)iterObject;
			action.setApp(PowerloomApp.getInstance());
			result.add(action);
		    } else if (iterObject instanceof Class) {
			PowerloomAction action = null;
			try {
			    action = (PowerloomAction)((Class)iterObject).newInstance();
			} catch (Exception e) {
			    PowerloomApp.getInstance().handleException(e);
			} // end of try-catch
			if (action != null) {
			    action.setApp(PowerloomApp.getInstance());
			    result.add(action);				 
			}
		    }
		}
	    }
	    return result;
		}
		});
    }

    public static void installTablePopup(final PLModule module,
					 final JTable table, 
					 final Object tableObject,
					 final java.util.List tableMenuItems,
					 final java.util.List itemMenuItems, 
					 final java.util.List tableOnlyMenuItems,
					 final java.util.List rowMenuItems,
					 final ContextSensitiveMenuItemGenerator csmig) {
	table.addMouseListener(new MouseListener() {
		public void mousePressed(MouseEvent e) { checkPopup(e); }
		public void mouseClicked(MouseEvent e) { checkPopup(e); }
		public void mouseEntered(MouseEvent e) {}
		public void mouseExited(MouseEvent e) {}
		public void mouseReleased(MouseEvent e) { checkPopup(e); }
		private void checkPopup(MouseEvent e) {
		    if (e.isPopupTrigger()) {
			int row = table.rowAtPoint(new Point(e.getX(), e.getY()));
			int col = table.columnAtPoint(new Point(e.getX(), e.getY()));
			debugPrintln(3, "you right-clicked row = " + row + ", col = " + col);
			Object object = null;
			if ((row >= 0) && (col >= 0)) {
			    object = table.getModel().getValueAt(row, col);
			    debugPrintln(3, "You right-clicked object: " + object);
			}
			int modelIndex = -1;
			TableModel model = table.getModel();
			if (model instanceof PLTableModel) {
			    modelIndex = ((PLTableModel)model).rowToModelIndex(row);
			}
			Object selectedObject = table.getModel().getValueAt(row, col);
			PLObject cellObject = null;
			if (selectedObject instanceof PLObject) {
			    cellObject = (PLObject)cellObject;
			}
			JPopupMenu popup = createPopupMenu(tableObject, cellObject, row, col, modelIndex);
			popup.addFocusListener(new FocusListener() {
			    public void focusGained(FocusEvent e) {
				debugPrintln(3, "doing popup menu.");
			    }

			    public void focusLost(FocusEvent e) {
				PowerloomApp.getInstance().setDoingRightClickMenu(false);
				debugPrintln(3, "did popup menu.");
			    }
			    });
			PowerloomApp.getInstance().setDoingRightClickMenu(true);
			popup.show(table, e.getX(), e.getY());

			
		    }
		}
		private JPopupMenu createPopupMenu(Object tableObject, PLObject cellObject, int row, int column, int modelIndex) {
		    JPopupMenu result = new JPopupMenu();
		    // todo: do similar for other lists
		    addObjectsToMenu(table, result, rowMenuItems, module, cellObject, tableObject, row, column, modelIndex);
		    return result;
		}

	    });

    }



    public static void installComponentPopup(final JComponent component,
					     final java.util.List menuItems) {
	component.addMouseListener(new MouseListener() {
		public void mousePressed(MouseEvent e) { checkPopup(e); }
		public void mouseClicked(MouseEvent e) { checkPopup(e); }
		public void mouseEntered(MouseEvent e) {}
		public void mouseExited(MouseEvent e) {}
		public void mouseReleased(MouseEvent e) { checkPopup(e); }
		private void checkPopup(MouseEvent e) {
		    if (e.isPopupTrigger()) {
			JPopupMenu popup = createPopupMenu();
			popup.addFocusListener(new FocusListener() {
			    public void focusGained(FocusEvent e) {
				//debugPrintln(3, "doing popup menu.");
			    }

			    public void focusLost(FocusEvent e) {
				PowerloomApp.getInstance().setDoingRightClickMenu(false);
				//debugPrintln(3, "did popup menu.");
			    }
			    });
			PowerloomApp.getInstance().setDoingRightClickMenu(true);
			popup.show(component, e.getX(), e.getY());
		    }
		}
		private JPopupMenu createPopupMenu() {
		    JPopupMenu result = new JPopupMenu();
		    addObjectsToMenu(component, result, menuItems, null, null);
		    return result;
		}

	    });

    }



    // Todo: migrate all code in this file to use these methods...

    private static void addObjectsToMenu(JComponent component, JPopupMenu menu, java.util.List list, PLModule actionModule, PLObject actionObject) {
	addObjectsToMenu(component,menu, list, actionModule, actionObject, null, -1, -1, -1);
    }

    private static void addObjectsToMenu(JComponent component, JPopupMenu menu, java.util.List list, PLModule actionModule, PLObject actionObject, Object tableObject, int row, int column, int modelIndex) {
	if (list == null) {
	    return;
	}
	Iterator iter = list.iterator();
	while(iter.hasNext()) {
	    Object iterObject = iter.next();
	    addObjectToMenu(component, menu, iterObject, actionModule, actionObject, tableObject, row, column, modelIndex);
	}
    }

    private static void addObjectToMenu(JComponent component, JPopupMenu menu, Object itemObject, PLModule actionModule, PLObject actionObject, Object tableObject, int row, int column, int modelIndex) {
	if (itemObject instanceof JMenuItem) {
	    JMenuItem item = (JMenuItem)itemObject;
	    menu.add(item);
	} else if (itemObject instanceof TableAction) {
	    TableAction action = (TableAction)itemObject;
	    action.setApp(PowerloomApp.getInstance());
	    if (actionModule != null) {
		action.setModule(actionModule);
	    }
	    if (actionObject != null) {
		action.setObject(actionObject);
	    }
	    if (tableObject != null) {
		action.setTableObject(tableObject);
	    }
	    if (row >= 0) {
		action.setRow(row);
	    }
	    if (column >= 0) {
		action.setColumn(column);
	    }
	    if (modelIndex >= 0) {
		action.setModelIndex(modelIndex);
	    }
	    menu.add(action);
	} else if (itemObject instanceof PowerloomAction) {
	    PowerloomAction action = (PowerloomAction)itemObject;
	    action.setApp(PowerloomApp.getInstance());
	    if (actionModule != null) {
		action.setModule(actionModule);
	    }
	    if (actionObject != null) {
		action.setObject(actionObject);
	    }
	    if (component != null) {
		action.setComponent(component);
	    }
	    menu.add(action);
	} else if (itemObject instanceof Action) {
	    Action action = (Action)itemObject;
	    menu.add(action);
	} else if (itemObject instanceof Class) {
	    PowerloomAction action = null;
	    try {
		action = (PowerloomAction)((Class)itemObject).newInstance();
	    } catch (Exception e) {
		PowerloomApp.getInstance().handleException(e);
	    } // end of try-catch
	    if (action != null) {
		action.setApp(PowerloomApp.getInstance());
		menu.add(action);				 
	    }
	}
    }



}
