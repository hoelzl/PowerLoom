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


// Version: PLElement.java,v 1.3 2010/02/04 05:20:03 hans Exp

package edu.isi.powerloom.gui.parser;

import javax.swing.text.*;
import java.util.*;

/**
 * Implementation of javax.swing.text.Element which represents a node in
 * a parse tree.  Eventually, this might be used for a special text widget.
 */
public class PLElement implements Element {
    public final String SYMBOL_ATTRIBUTE = "Symbol";
    public final String TOKEN_ATTRIBUTE = "Token";
    private AttributeSet attributes = new SimpleAttributeSet();
    private Document document;
    private List elements = new ArrayList();
    private Element parent;

    public AttributeSet getAttributes() {
	return attributes;
    }

    public void setAtrributes(AttributeSet attributes) {
	this.attributes = attributes;
    }

    public void addAttribute(Object name, Object value) {
	((MutableAttributeSet)attributes).addAttribute(name, value);
    }

    public Object getAttribute(Object name) {
	return attributes.getAttribute(name);
    }

    public void setAttribute(Object name, Object value) {
	((MutableAttributeSet)attributes).removeAttribute(StyleConstants.NameAttribute);
	((MutableAttributeSet)attributes).addAttribute(name, value);
    }

    public Document getDocument() {
	return document;
    }

    public Element getElement(int index) {
	return (Element)elements.get(index);
    }

    public void addElement(Element element) {
	elements.add(element);
    }

    public int getElementCount() {
	return elements.size();
    }

    public int getElementIndex(int offset) {
	Symbol symbol = getSymbol();
	if (symbol != null) {
	    if (symbol instanceof Terminal) {
		Terminal terminal = (Terminal)symbol;
		Yytoken token = (Yytoken)getAttribute(TOKEN_ATTRIBUTE);
		if ((offset >= token.getCharBegin()) &&
		    (offset < token.getCharEnd())) {
		    return offset;
		}
	    }
	    if (symbol instanceof NonTerminal) {
		NonTerminal nonTerminal = (NonTerminal)symbol;
		for (int i = 0; i < getElementCount(); i++) {
		    PLElement child = (PLElement)getElement(i);
		    int childOffset = child.getElementIndex(offset);
		    if (childOffset > -1) {
			return i;
		    }
		}
	    }
	}
	return -1;
    }

    // retrieve the leaf at offset
    public PLElement getLeafElement(int offset) {
	return helpGetLeafElement(this, offset);	
    }

    private PLElement helpGetLeafElement(PLElement node, int offset) {
	Symbol symbol = node.getSymbol();
	if (symbol != null) {
	    if (symbol instanceof Terminal) {
		Terminal terminal = (Terminal)symbol;
		Yytoken token = (Yytoken)node.getAttribute(TOKEN_ATTRIBUTE);
		//System.err.println("terminal = " + terminal + ", token = " + token);
		if ((offset >= token.getCharBegin()) &&
		    (offset < token.getCharEnd())) {
		    return node;
		} else {
		    return null;
		}
	    }
	    if (symbol instanceof NonTerminal) {
		NonTerminal nonTerminal = (NonTerminal)symbol;
		for (int i = 0; i < node.getElementCount(); i++) {
		    PLElement child = (PLElement)node.getElement(i);
		    PLElement searchResult = helpGetLeafElement(child, offset);
		    if (searchResult != null) {
			return searchResult;
		    }
		}
	    }
	}
	return null;
    }

    public int getEndOffset() {
	Symbol symbol = getSymbol();
	if (symbol != null) {
	    if (symbol instanceof Terminal) {
		Terminal terminal = (Terminal)symbol;
		Yytoken token = getToken();
		return token.getCharEnd();
	    }
	    if (symbol instanceof NonTerminal) {
		try {
		    PLElement rightChild = (PLElement)getElement(getElementCount() - 1);
		    return rightChild.getEndOffset();
		} catch (Exception e) {
		    //todo? throw exception
		    System.err.println("*** Err in getEndOffset: right child is null, symbol = " + symbol);
		    e.printStackTrace();
		    return -1;
		}
	    }
	}
	return -1;
    }

    public String getName() {
	return (String)attributes.getAttribute(StyleConstants.NameAttribute);
    }

    public void setName(String name) {
	setAttribute(StyleConstants.NameAttribute, name);
    }

    public Element getParentElement() {
	return parent;
    }

    public void setParentElement(Element parent) {
	this.parent = parent;
	if (parent instanceof PLElement) {
	    ((PLElement)parent).addElement(this);
	}
    }

    public Yytoken getToken() {
	return (Yytoken)getAttribute(TOKEN_ATTRIBUTE);
    }

    public void setToken(Yytoken token) {
	setAttribute(TOKEN_ATTRIBUTE, token);
    }

    public Symbol getSymbol() {
	return (Symbol)getAttribute(SYMBOL_ATTRIBUTE);
    }

    public void setSymbol(Symbol symbol) {
	setAttribute(SYMBOL_ATTRIBUTE, symbol);
    }

    public int getStartOffset() {
	Symbol symbol = getSymbol();
	if (symbol != null) {
	    if (symbol instanceof Terminal) {
		Terminal terminal = (Terminal)symbol;
		Yytoken token = getToken();
		return token.getCharBegin();
	    }
	    if (symbol instanceof NonTerminal) {
		try {
		    PLElement leftChild = (PLElement)getElement(0);
		    return leftChild.getStartOffset();
		} catch (Exception e) {
		    //todo? throw exception
				
		    System.err.println("*** Err in getStartOffset: left child is null, symbol = " + symbol);
		    e.printStackTrace();
		    return -1;
		}
	    }
	}
	return -1;
    }

    public boolean isLeaf() {
	Symbol symbol = getSymbol();
	if ((symbol != null) &&
	    (symbol instanceof Terminal)) {
	    return true;
	}
	return false;
    }
	
    public void dump() {
	dump(this, 0);
    }

    private void dump(Element root, int indent) {
	for (int i =0; i < indent; i++) {
	    System.out.print(" ");
	}
	System.out.println(root.getAttributes());
	for (int i = 0; i < root.getElementCount(); i++) {
	    dump(root.getElement(i), indent + 4);
	}
    }
}
