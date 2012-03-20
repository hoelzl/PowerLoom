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


// Version: KnowledgeManager.java,v 1.30 2010/02/04 05:20:27 hans Exp

package edu.isi.powerloom.gui.serverinterface;

import edu.isi.powerloom.gui.xmlobject.*;
import edu.isi.powerloom.gui.common.*;
import static edu.isi.powerloom.gui.common.Utils.*;
import java.util.*;

/**
 * Singleton Class which keeps track of knowledge imported from the server, including
 *   caching info.
 *
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 * @since Fri Mar 01 00:52:59 2002
 */

public class KnowledgeManager {
    private static KnowledgeManager theInstance = null;

    /**
     * concepts indexed on module
     */
    private HashMap moduleConceptTable; 
        
    /**
     * relations indexed on module
     */
    private HashMap moduleRelationTable; 

    /**
     * instances indexed on module
     */
    private HashMap moduleInstanceTable; 

    /**
     * instances indexed on concept
     */
    private HashMap conceptInstanceTable;

    /**
     * instances indexed on concept
     */
    private HashMap conceptDirectInstanceTable;

    /**
     * relations indexed on concept
     */
    private HashMap conceptRelationTable;

    /**
     * inherited relations indexed on concept
     */
    private HashMap conceptInheritedRelationTable;

    /**
     * instanceProposition tables indexed on modules
     */
    private HashMap moduleInstancePropositionTable;

    /**
     * propositions indexed on instance
     */
    // obsolete
    //    private HashMap instancePropositionTable; 

    /**
     * propositions indexed on module
     */
    private HashMap modulePropositionTable; 

    /**
     * rules indexed on module
     */
    private HashMap moduleRuleTable; 

    /**
     * rules indexed on relation
     */
    private HashMap moduleRelationRuleTable;

    /**
     * rules indexed on concept
     */
    private HashMap moduleConceptRuleTable;

    /**
     * propositions indexed on concept
     */
    private HashMap moduleConceptPropositionTable;

    /**
     * propositions indexed on relation
     */
    private HashMap moduleRelationPropositionTable;

    /**
     * propositions indexed on instance and relation
     */
    private HashMap moduleInstanceRelationPropositionTable;

    /**
     * name-surrogate tables index on surrogate type
     */
    private HashMap surrogateTable;

    /**
     * The root of the module hierarchy
     */
    private PLModule rootModule;

    /**
     * Modules
     */
    private PLSurrogateContainer modules;

    /**
     * Instance of knowledge server (can be local or remote)
     */
    ServerInterface server;

    public static KnowledgeManager getInstance() throws AppException {
        if (theInstance == null) {
            theInstance = new KnowledgeManager();
        }
        return theInstance;
    }

    private KnowledgeManager() throws AppException {
        initializeKnowledgeManager();
    }

    private void initializeKnowledgeManager() throws AppException {
        try {
            server = InterfaceFactory.getServerInterface();
            createTables();
            initializeModules();
        } catch (Exception e) {
            throw new AppException(e);
        }
    }

    private void createTables() {
        moduleConceptTable = new HashMap(); 
        moduleRelationTable = new HashMap(); 
        moduleInstanceTable = new HashMap(); 
        conceptInstanceTable = new HashMap();
        conceptDirectInstanceTable = new HashMap();

        modulePropositionTable = new HashMap(); 
        moduleRuleTable = new HashMap(); 
        conceptRelationTable = new HashMap();
        conceptInheritedRelationTable = new HashMap();
        surrogateTable = new HashMap();

        moduleConceptPropositionTable = new HashMap();
        moduleConceptRuleTable = new HashMap();
        moduleRelationPropositionTable = new HashMap();
        moduleRelationRuleTable = new HashMap();
	moduleInstancePropositionTable = new HashMap();
        moduleInstanceRelationPropositionTable = new HashMap();
    }

    private HashMap getConceptPropositionTable(PLModule module) {
	HashMap result = (HashMap)moduleConceptPropositionTable.get(module);
	if (result == null) {
	    result = new HashMap();
	    moduleConceptPropositionTable.put(module, result);
	}
	return result;
    }

    private HashMap getConceptRuleTable(PLModule module) {
	HashMap result = (HashMap)moduleConceptRuleTable.get(module);
	if (result == null) {
	    result = new HashMap();
	    moduleConceptRuleTable.put(module, result);
	}
	return result;
    }

    private HashMap getRelationPropositionTable(PLModule module) {
	HashMap result = (HashMap)moduleRelationPropositionTable.get(module);
	if (result == null) {
	    result = new HashMap();
	    moduleRelationPropositionTable.put(module, result);
	}
	return result;
    }

    private HashMap getRelationRuleTable(PLModule module) {
	HashMap result = (HashMap)moduleRelationRuleTable.get(module);
	if (result == null) {
	    result = new HashMap();
	    moduleRelationRuleTable.put(module, result);
	}
	return result;
    }

    private HashMap getInstancePropositionTable(PLModule module) {
	HashMap result = (HashMap)moduleInstancePropositionTable.get(module);
	if (result == null) {
	    result = new HashMap();
	    moduleInstancePropositionTable.put(module, result);
	}
	return result;
    }

    private HashMap getInstanceRelationPropositionTable(PLModule module) {
	HashMap result = (HashMap)moduleInstanceRelationPropositionTable.get(module);
	if (result == null) {
	    result = new HashMap();
	    moduleInstanceRelationPropositionTable.put(module, result);
	}
	return result;
    }


    /**
     * Surrogate management
     */
    private PLSurrogate getSurrogate(Class type, String id) {
        HashMap nameSurrogateTable = (HashMap)surrogateTable.get(type.getName());
        if (nameSurrogateTable == null) {
            return null;
        }
        PLSurrogate surrogate = (PLSurrogate)nameSurrogateTable.get(id);
        return surrogate;
    }

    private void internSurrogate(Class type, String id, PLSurrogate surrogate) {
        HashMap nameSurrogateTable = (HashMap)surrogateTable.get(type.getName());
        if (nameSurrogateTable == null) {
            nameSurrogateTable = new HashMap();
            surrogateTable.put(type.getName(), nameSurrogateTable);
        }
        nameSurrogateTable.put(id, surrogate);
    }

    public PLSurrogate findOrCreateSurrogate(Class type, String id) {
        PLSurrogate surrogate = getSurrogate(type, id);
        if (surrogate == null) {
            surrogate = new PLSurrogate(id);
        }
        internSurrogate(type, id, surrogate);
        return surrogate;
    }
    
    /**
     * Business object management
     */
    public void initializeModules() throws AppException {
        PLModuleContainer modContainer = server.getModules();
        modules = referencizeContainer("MODULE-ROOT", modContainer);
        // Assume there is a single root 
        rootModule = (PLModule)((PLSurrogate)modules.getSurrogates().get(0)).getValue();
	invalidateAllCaches();
    }

    /**
     * Create a dummy object (e.g., if we haven't seen it yet)
     */
    private PLObject createDummyObject(Class objectClass, String id) {
	PLObject dummyObj = null;
	if (objectClass == PLConcept.class) {
	    dummyObj = new PLConcept();
	    debugPrintln(3, " referencizing concept: " + id);
	    ((PLConcept)dummyObj).attrConceptName = id;
	} else if (objectClass == PLRelation.class) {
	    dummyObj = new PLRelation();
	    ((PLRelation)dummyObj).attrRelationName = id;
	} else if (objectClass == PLInstance.class) {
	    dummyObj = new PLInstance();
	    ((PLInstance)dummyObj).attrInstanceName = id;
	} else if (objectClass == PLModule.class) {
	    dummyObj = new PLModule();
	    ((PLModule)dummyObj).attrModuleName = id;
	} else {
	    debugPrintln(3, "***Error: unrecognized container class: " + objectClass);
	}
	dummyObj.setUndefined(true);
	return dummyObj;
    }

    /**
     * Make sure all surrogates in a container are properly interned
     */
    private PLSurrogateContainer referencizeContainer(String containerLabel, PLContainer container) {
        List allSurrogates = new ArrayList();
        List allChildSurrogates = new ArrayList();
        Collection xmlChildren = container.getXMLChildren();
        Iterator iter = (xmlChildren == null) ? (new ArrayList()).iterator() : xmlChildren.iterator();
        while (iter.hasNext()) {
            Object obj1 = iter.next();
            PLObject obj = (PLObject)obj1;

            PLSurrogate objSurrogate = findOrCreateSurrogate(container.getContaineeClass(), obj.getID());
            allSurrogates.add(objSurrogate);
            objSurrogate.setValue(obj);
            Collection children = obj.getChildSurrogates().getSurrogates();
            Iterator childIter = children.iterator();
            List childSurrogates = new ArrayList();
            while (childIter.hasNext()) {
                PLSurrogate uninternedSurrogate = (PLSurrogate)childIter.next();
                PLSurrogate internedSurrogate = findOrCreateSurrogate(container.getContaineeClass(), uninternedSurrogate.getID());
                // Create a dummy object if no real object was defined for this surrogate.
                if (internedSurrogate.getValue() == null) {
                    PLObject dummyObj = createDummyObject(container.getContaineeClass(), internedSurrogate.getID());
                    dummyObj.setUndefined(true);
                    internedSurrogate.setValue(dummyObj);
                                 
                }
                childSurrogates.add(internedSurrogate);

                allChildSurrogates.add(internedSurrogate);
            } 
            PLSurrogateContainer childrenContainer = new PLSurrogateContainer(childSurrogates);
            obj.setChildSurrogates(childrenContainer);
        }
        // Filter children to get just roots
        allSurrogates.removeAll(allChildSurrogates);
        PLSurrogateContainer result = new PLSurrogateContainer(containerLabel, allSurrogates);
        if (containerLabel != null) {
            PLSurrogate containerSurrogate = findOrCreateSurrogate(PLSurrogateContainer.class, containerLabel);
            containerSurrogate.setValue(result);
        }
        return result;
    }

    PLSurrogateContainer referencizeSurrogateCollection(PLSurrogateCollection collection, Class theClass, String label) {
	Iterator iter = ((collection.elemPLSurrogate != null) ? collection.elemPLSurrogate : new ArrayList()).iterator();
	List internedSurrogates = new ArrayList();
	while (iter.hasNext()) {
	    if (theClass == null) {
		internedSurrogates.add(PLSurrogateContainer.surrogatify(((PLSurrogate)iter.next()).getID()));
	    } else {
		PLSurrogate obj = (PLSurrogate)iter.next();
		PLSurrogate objSurrogate = findOrCreateSurrogate(theClass, obj.getID());
		internedSurrogates.add(objSurrogate);
		// if the surrogate doesn't have a value, create a dummy object, and mark it as undefined
		PLObject plObj = (PLObject)objSurrogate.getValue();
		if (plObj == null) {
		    try {
			PLObject newObj = (PLObject)theClass.newInstance();
			newObj.setUndefined(true);
			// kludgy.. should have setID() method on PLObject
			if (newObj instanceof PLModule) {
			    ((PLModule)newObj).attrModuleName = objSurrogate.getID();
			} else if (newObj instanceof PLConcept) {
			    ((PLConcept)newObj).attrConceptName = objSurrogate.getID();
			} else if (newObj instanceof PLRelation) {
			    ((PLRelation)newObj).attrRelationName = objSurrogate.getID();
			} else if (newObj instanceof PLInstance) {
			    ((PLInstance)newObj).attrInstanceName = objSurrogate.getID();
			} else if (newObj instanceof PLSymbol) {
			    ((PLSymbol)newObj).attrSymbolName = objSurrogate.getID();
			}
			objSurrogate.setValue(newObj);
		    } catch (Exception e) {
			edu.isi.powerloom.gui.components.PowerloomApp.getInstance().handleException(e);
		    } // end of try-catch
		
		}
	    }
	}
	PLSurrogateContainer result = new PLSurrogateContainer(label, internedSurrogates);
	collection.setChildSurrogates(result);
	return result;
    }

    void referencizeVariableList(PLVariableList variableList) {
	Iterator iter = ((variableList.elemPLVariable != null) ? variableList.elemPLVariable : new ArrayList()).iterator();
	List internedSurrogates = new ArrayList();
	while (iter.hasNext()) {
	    PLVariable variable = (PLVariable)iter.next();
	    PLSurrogate uninternedSurrogate = variable.elemPLSurrogate;
	    PLSurrogate internedSurrogate = findOrCreateSurrogate(PLConcept.class, uninternedSurrogate.getID());
	    variable.elemPLSurrogate = internedSurrogate;
	    internedSurrogates.add(internedSurrogate);
	}
    }

    /**
     * referencize all surrogates in query result.
     */
    private void referencizeQueryResult(PLQueryResult queryResult) throws AppException {
	Iterator tupleIter = queryResult.elemPLTuple.iterator();
	while (tupleIter.hasNext()) {
	    PLTuple tuple = (PLTuple)tupleIter.next();
	    Iterator unionIter = tuple.elemPLObjectUnion.iterator();
	    while (unionIter.hasNext()) {
		PLObjectUnion union = (PLObjectUnion)unionIter.next();
		referencizeUnion(union);
	    }
	    
	}
    }

    /**
     *  Reference union in PLSurrogate object
     */ 
    private void referencizeUnion(PLObjectUnion union) throws AppException {
	if (union.elemPLSurrogate != null) {
	    if (union.attrType.equals("INSTANCE")) {
		PLSurrogate internedSurrogate = findOrCreateSurrogate(PLInstance.class, union.elemPLSurrogate.getID());
		union.elemPLSurrogate = internedSurrogate;
		// if an instance object doesn't exist yet, create one 
		//   and mark it undefined.
		if (internedSurrogate.getValue() == null) {
		    PLInstance newInstance = new PLInstance();
		    newInstance.attrInstanceName = internedSurrogate.getID();
		    newInstance.setUndefined(true);
		    internedSurrogate.setValue(newInstance);
		}
	    } else if (union.attrType.equals("RELATION")) {
		PLSurrogate internedSurrogate = findOrCreateSurrogate(PLRelation.class, union.elemPLSurrogate.getID());
		union.elemPLSurrogate = internedSurrogate;
		// if an relation object doesn't exist yet, create one 
		//   and mark it undefined.
		if (internedSurrogate.getValue() == null) {
		    PLRelation newRelation = new PLRelation();
		    newRelation.attrRelationName = internedSurrogate.getID();
		    newRelation.setUndefined(true);
		    internedSurrogate.setValue(newRelation);
		}
	    } else if (union.attrType.equals("CONCEPT")) {
		PLSurrogate internedSurrogate = findOrCreateSurrogate(PLConcept.class, union.elemPLSurrogate.getID());
		union.elemPLSurrogate = internedSurrogate;
		// if an concept object doesn't exist yet, create one 
		//   and mark it undefined.
		if (internedSurrogate.getValue() == null) {
		    PLConcept newConcept = new PLConcept();
		    newConcept.attrConceptName = internedSurrogate.getID();
		    newConcept.setUndefined(true);
		    internedSurrogate.setValue(newConcept);
		}
	    } else {
		throw new AppException("Illegal type in union: " + union.attrType);
	    } 
	}
    }

    /**
     * referencize all surrogates in search result.
     */
    private void referencizeSearchResult(PLSearchResult searchResult) throws AppException {
	if (searchResult.elemPLSearchResultItem == null) {
	    searchResult.elemPLSearchResultItem = new ArrayList();
	}
	Iterator itemIter = searchResult.elemPLSearchResultItem.iterator();
	while (itemIter.hasNext()) {
	    PLSearchResultItem item = (PLSearchResultItem)itemIter.next();
	    String moduleName = item.attrModuleName;
	    PLObjectUnion union = (PLObjectUnion)item.elemPLObjectUnion;
	    referencizeUnion(union);
	}
    }


    public PLModule getRootModule() {
        return rootModule;
    }

    public PLSurrogateContainer getModules() {
        return modules;
    }

    /**
     * Return all modules under and including `module'.
     */
    public PLSurrogateContainer getModules(PLModule module) {
	PLSurrogate moduleSurrogate = getSurrogate(PLModule.class, module.getID());
	List surrogateList = new ArrayList();
	surrogateList.add(moduleSurrogate);
	PLSurrogateContainer result = new PLSurrogateContainer(surrogateList);
	return result;
    }


    /**
     *  STRATEGY: a 'null' as a hashtable value signifies that objects haven't yet been
     *  retrieved.  An empty container such as 'PLConceptContainer' signifies that objejcts
     *  have been retrieved but none are available.
     */
    public PLSurrogateContainer getConceptsForModule(PLModule module) throws AppException {
        PLSurrogate moduleSurrogate = findOrCreateSurrogate(PLModule.class, module.getID());
        PLSurrogateContainer cachedConcepts = (PLSurrogateContainer)moduleConceptTable.get(moduleSurrogate);
        if (cachedConcepts != null) {
            return cachedConcepts;
        }
        PLConceptContainer retrievedConcepts = server.getConceptsForModule(new PLString(module.getModuleName()));
        cachedConcepts = referencizeContainer("CONCEPT-ROOT", retrievedConcepts);
        moduleConceptTable.put(moduleSurrogate, cachedConcepts);
        return cachedConcepts;
    }

    public PLSurrogateContainer getInstancesForModule(PLModule module) throws AppException {
        PLSurrogate moduleSurrogate = findOrCreateSurrogate(PLModule.class, module.getID());
        PLSurrogateContainer cachedInstances = (PLSurrogateContainer)moduleInstanceTable.get(moduleSurrogate);
        if (cachedInstances != null) {
            return cachedInstances;
        }
        PLInstanceContainer retrievedInstances = server.getInstancesForModule(new PLString(module.getModuleName()));
        cachedInstances = referencizeContainer("INSTANCE-ROOT", retrievedInstances);
        moduleInstanceTable.put(moduleSurrogate, cachedInstances);
        return cachedInstances;
    }

    public PLSurrogateContainer getRelationsForModule(PLModule module) throws AppException {
        PLSurrogate moduleSurrogate = findOrCreateSurrogate(PLModule.class, module.getID());
        PLSurrogateContainer cachedRelations = (PLSurrogateContainer)moduleRelationTable.get(moduleSurrogate);
        if (cachedRelations != null) {
            return cachedRelations;
        }
        PLRelationContainer retrievedRelations = server.getRelationsForModule(new PLString(module.getModuleName()));
        cachedRelations = referencizeContainer("RELATION-ROOT", retrievedRelations);
        moduleRelationTable.put(moduleSurrogate, cachedRelations);
        return cachedRelations;
    }

    public PLSurrogateContainer getInstancesForConcept(PLModule module, PLConcept concept) throws AppException {
        PLSurrogate conceptSurrogate = findOrCreateSurrogate(PLConcept.class, concept.getID());
        PLSurrogateContainer cachedInstances = (PLSurrogateContainer)conceptInstanceTable.get(conceptSurrogate);
        if (cachedInstances != null) {
            return cachedInstances;
        }
        PLInstanceContainer retrievedInstances = server.
            getInstancesForConcept(new PLString(module.getModuleName()),
                                   new PLString(concept.getConceptName()));
        cachedInstances = referencizeContainer("INSTANCE-ROOT", retrievedInstances);
        conceptInstanceTable.put(conceptSurrogate, cachedInstances);
        return cachedInstances;
    }

    public PLSurrogateContainer getDirectInstancesForConcept(PLModule module, PLConcept concept) throws AppException {
        PLSurrogate conceptSurrogate = findOrCreateSurrogate(PLConcept.class, concept.getID());
        PLSurrogateContainer cachedInstances = (PLSurrogateContainer)conceptDirectInstanceTable.get(conceptSurrogate);
        if (cachedInstances != null) {
            return cachedInstances;
        }
        PLInstanceContainer retrievedInstances = server.
            getDirectInstancesForConcept(new PLString(module.getModuleName()),
					 new PLString(concept.getConceptName()));
        cachedInstances = referencizeContainer("INSTANCE-ROOT", retrievedInstances);
        conceptDirectInstanceTable.put(conceptSurrogate, cachedInstances);
        return cachedInstances;
    }

    public String getDocumentationForConcept(PLModule module, PLConcept concept) throws AppException {
        PLString docString = server.getDocumentationForConcept(new PLString(module.getModuleName()),
							       new PLString(concept.getConceptName()));
	return docString.attrValue;
    }

    public String getSourceForConcept(PLModule module, PLConcept concept) throws AppException {
        PLString string = server.getSourceForConcept(new PLString(module.getModuleName()),
						     new PLString(concept.getConceptName()));
	return string.attrValue;
    }

    public PLSurrogateContainer getSuperConceptsForConcept(PLModule module, PLConcept concept) throws AppException {
        PLSurrogateCollection superSurrogates = server.getSuperConceptsForConcept
	    (new PLString(module.getModuleName()),
	     new PLString(concept.getConceptName()));
	PLSurrogateContainer result = referencizeSurrogateCollection(superSurrogates, PLConcept.class, "SuperConcepts");
	return result;
    }

    public PLSurrogateContainer getPropositionsForInstance(PLModule module, PLInstance instance) throws AppException {        
	debugPrintln(3, "in knowledgemanager getPropositionsForInstance.  module = " + module.getID() + ", instance = " + instance.getID());

        PLSurrogate instanceSurrogate = findOrCreateSurrogate(PLInstance.class, instance.getID());
        PLSurrogateContainer cachedPropositions = (PLSurrogateContainer)getInstancePropositionTable(module).get(instanceSurrogate);
        if (cachedPropositions != null) {
            return cachedPropositions;
        }
        PLPropositionContainer retrievedPropositions = server.
            getPropositionsForInstance(new PLString(module.getModuleName()),
                                       new PLString(instance.getInstanceName()));
        cachedPropositions = referencizeContainer("PROPOSITION-ROOT", retrievedPropositions);
        getInstancePropositionTable(module).put(instanceSurrogate, cachedPropositions);
        return cachedPropositions;
    }

    public PLSurrogateContainer getRelationsForConcept(PLModule module, PLConcept concept) throws AppException {
        PLSurrogate conceptSurrogate = findOrCreateSurrogate(PLConcept.class, concept.getID());
        PLSurrogateContainer cachedRelations = (PLSurrogateContainer)conceptRelationTable.get(conceptSurrogate);
        if (cachedRelations != null) {
            return cachedRelations;
        }
        PLRelationContainer retrievedRelations = server.
            getRelationsForConcept(new PLString(module.getModuleName()),
                                     new PLString(concept.getConceptName()));
        cachedRelations = referencizeContainer("RELATION-ROOT", retrievedRelations);
        conceptRelationTable.put(conceptSurrogate, cachedRelations);
        return cachedRelations;
    }

    public PLSurrogateContainer getInheritedRelationsForConcept(PLModule module, PLConcept concept) throws AppException {
        PLSurrogate conceptSurrogate = findOrCreateSurrogate(PLConcept.class, concept.getID());
        PLSurrogateContainer cachedRelations = (PLSurrogateContainer)conceptInheritedRelationTable.get(conceptSurrogate);
        if (cachedRelations != null) {
            return cachedRelations;
        }
        PLRelationContainer retrievedRelations = server.
            getInheritedRelationsForConcept(new PLString(module.getModuleName()),
					    new PLString(concept.getConceptName()));
        cachedRelations = referencizeContainer("RELATION-ROOT", retrievedRelations);
        conceptInheritedRelationTable.put(conceptSurrogate, cachedRelations);
        return cachedRelations;
    }

    public PLSurrogateContainer getPropositionsForConcept(PLModule module, PLConcept concept) throws AppException {
        PLSurrogate conceptSurrogate = findOrCreateSurrogate(PLConcept.class, concept.getID());
        PLSurrogateContainer cachedPropositions = (PLSurrogateContainer)getConceptPropositionTable(module).get(conceptSurrogate);
        if (cachedPropositions != null) {
            return cachedPropositions;
        }
        PLPropositionContainer retrievedPropositions = server.
            getPropositionsForConcept(new PLString(module.getModuleName()),
                                      new PLString(concept.getConceptName()));
        cachedPropositions = referencizeContainer("PROPOSITION-ROOT", retrievedPropositions);
        getConceptPropositionTable(module).put(conceptSurrogate, cachedPropositions);
        return cachedPropositions;
    }

    public PLSurrogateContainer getPropositionsForRelation(PLModule module, PLRelation relation) throws AppException {
        PLSurrogate relationSurrogate = findOrCreateSurrogate(PLRelation.class, relation.getID());
        PLSurrogateContainer cachedPropositions = (PLSurrogateContainer)getRelationPropositionTable(module).get(relationSurrogate);
        if (cachedPropositions != null) {
            return cachedPropositions;
        }
        PLPropositionContainer retrievedPropositions = server.
            getPropositionsForRelation(new PLString(module.getModuleName()),
                                      new PLString(relation.getRelationName()));
        cachedPropositions = referencizeContainer("PROPOSITION-ROOT", retrievedPropositions);
        getRelationPropositionTable(module).put(relationSurrogate, cachedPropositions);
        return cachedPropositions;
    }

    public PLSurrogateContainer getRulesForConcept(PLModule module, PLConcept concept) throws AppException {
        PLSurrogate conceptSurrogate = findOrCreateSurrogate(PLConcept.class, concept.getID());
        PLSurrogateContainer cachedRules = (PLSurrogateContainer)getConceptRuleTable(module).get(conceptSurrogate);
        if (cachedRules != null) {
            return cachedRules;
        }         
        PLPropositionContainer retrievedRules = server.
            getRulesForConcept(new PLString(module.getModuleName()),
                                      new PLString(concept.getConceptName()));
        cachedRules = referencizeContainer("RULE-ROOT", retrievedRules);
        getConceptRuleTable(module).put(conceptSurrogate, cachedRules);
        return cachedRules;
    }

    public PLSurrogateContainer getRulesForRelation(PLModule module, PLRelation relation) throws AppException {
        PLSurrogate relationSurrogate = findOrCreateSurrogate(PLRelation.class, relation.getID());
        PLSurrogateContainer cachedRules = (PLSurrogateContainer)getRelationRuleTable(module).get(relationSurrogate);
        if (cachedRules != null) {
            return cachedRules;
        }
        PLPropositionContainer retrievedRules = server.
            getRulesForRelation(new PLString(module.getModuleName()),
                                      new PLString(relation.getRelationName()));
        cachedRules = referencizeContainer("RULE-ROOT", retrievedRules);
        getRelationRuleTable(module).put(relationSurrogate, cachedRules);
        return cachedRules;
    }

    public PLSurrogateContainer getPropositionsForInstanceAndRelation(PLModule mod, PLInstance instance, PLRelation relation) throws AppException {
        PLSurrogate instanceSurrogate = findOrCreateSurrogate(PLInstance.class, instance.getID());
        HashMap retrievedRelationPropositionTable = (HashMap)getInstanceRelationPropositionTable(mod).get(instanceSurrogate);
        if (retrievedRelationPropositionTable == null) {
            retrievedRelationPropositionTable = new HashMap();
            getInstanceRelationPropositionTable(mod).put(instanceSurrogate, retrievedRelationPropositionTable);
        } 
        

        PLSurrogate relationSurrogate = findOrCreateSurrogate(PLRelation.class, relation.getID());
        PLSurrogateContainer cachedPropositions = (PLSurrogateContainer)retrievedRelationPropositionTable.get(relationSurrogate);
        if (cachedPropositions == null) {
            PLPropositionContainer retrievedPropositions = 
                server.getPropositionsForInstanceAndRelation(new PLString(mod.getModuleName()), 
                                                             new PLString(instance.getInstanceName()), 
                                                             new PLString(relation.getRelationName()));
            cachedPropositions = referencizeContainer("INST-REL-PROPS", retrievedPropositions);
            retrievedRelationPropositionTable.put(relationSurrogate, cachedPropositions);
        }
        return cachedPropositions;
    }

    public String evaluateLogicCommand(PLModule mod, String command) throws AppException {
        PLString commandString = new PLString(command);
        PLString moduleString = new PLString(mod.getModuleName());
        PLString serverResult = server.evaluateLogicCommand(moduleString, commandString);
        return serverResult.getValue();
    }

    public PLQueryResult executeQuery(PLQuery query, boolean continueFlag) throws AppException {
	PLString continueFlagString = continueFlag ? (new PLString("TRUE")) : (new PLString("FALSE"));
        PLQueryResult queryResult = server.query(query, continueFlagString);
	referencizeQueryResult(queryResult);
        return queryResult;
    }

    public PLQueryResult getExtensionForRelation(PLModule mod, String relationName) throws AppException {
        PLString relationString = new PLString(relationName);
        PLString moduleString = new PLString(mod.getModuleName());
        PLQueryResult queryResult = server.getExtensionForRelation(moduleString, relationString);
	referencizeQueryResult(queryResult);
        return queryResult;
    }

    public PLSearchResult executeSearch(String modName, String searchString, 
					boolean isSearchConcept, 
					boolean isSearchRelation,
					boolean isSearchInstance,
					boolean isCaseSensitive) throws AppException {
	PLSearchParameter searchParameter = new PLSearchParameter();
	searchParameter.attrModuleName = modName;
	searchParameter.attrSearchString = searchString;
	searchParameter.attrSearchConcept = isSearchConcept ? "TRUE" : "FALSE";
	searchParameter.attrSearchRelation = isSearchRelation ? "TRUE" : "FALSE";
	searchParameter.attrSearchInstance = isSearchInstance ? "TRUE" : "FALSE";
	searchParameter.attrCaseSensitive = isCaseSensitive ? "TRUE" : "FALSE";

        PLSearchResult searchResult = server.search(searchParameter);
	debugPrintln(3, "search result is: " + searchResult);
	referencizeSearchResult(searchResult);
        return searchResult;
    }

    public PLSurrogateContainer getConceptCompletions(String modName, String prefix) throws AppException {
	PLConceptContainer concepts = server.getConceptCompletions(new PLString(modName), new PLString(prefix));
	return referencizeContainer("CONCEPT-ROOT", concepts);
    }

    public PLSurrogateContainer getRelationCompletions(String modName, String prefix) throws AppException {
	PLRelationContainer relations = server.getRelationCompletions(new PLString(modName), new PLString(prefix));
	return referencizeContainer("RELATION-ROOT", relations);
    }

    public PLSurrogateContainer getInstanceCompletions(String modName, String prefix) throws AppException {
	PLInstanceContainer instances = server.getInstanceCompletions(new PLString(modName), new PLString(prefix));
	return referencizeContainer("INSTANCE-ROOT", instances);
    }

    public PLSurrogateContainer getConceptAndRelationCompletions(String modName, String prefix) throws AppException {
	PLSurrogateContainer concepts = getConceptCompletions(modName, prefix);
	PLSurrogateContainer relations = getRelationCompletions(modName, prefix);
	return concepts.mergeSurrogateContainer(relations);
    }

    public PLSurrogateContainer getConceptAndRelationAndInstanceCompletions(String modName, String prefix) throws AppException {
	PLSurrogateContainer concepts = getConceptCompletions(modName, prefix);
	PLSurrogateContainer relations = getRelationCompletions(modName, prefix);
	PLSurrogateContainer instances = getInstanceCompletions(modName, prefix);
	concepts = concepts.mergeSurrogateContainer(relations);
	return concepts.mergeSurrogateContainer(instances);
    }

    public PLString destroyObject(PLModule module, String objectName) throws AppException {
        PLString moduleString = new PLString(module.getModuleName());
	PLString objectString = new PLString(objectName);
        PLString result = server.destroyObject(moduleString, objectString);
	return result;
    }

    public String getExplanationForQuery(PLQuery query, int resultNum) throws AppException {
        PLString resultNumString = new PLString(Integer.toString(resultNum));
        PLString result = server.getExplanationForQuery(query, resultNumString);
	return result.getValue();
    }

    public PLServerInfo getServerInfo() throws AppException {
	return server.getServerInfo();
    }

    /**
     * Cache management methods
     */
    public void invalidateConceptCaches() {
        moduleConceptTable.clear();
    }

    public void invalidateInstanceCaches() {
        moduleInstanceTable.clear();
        conceptInstanceTable.clear();
        conceptDirectInstanceTable.clear();
    }

    public void invalidateRelationCaches() {
        moduleRelationTable.clear();
        conceptRelationTable.clear();
        conceptInheritedRelationTable.clear();
    }

    public void invalidatePropositionCaches() {
        moduleConceptPropositionTable.clear();
        moduleRelationPropositionTable.clear();
        moduleInstancePropositionTable.clear();
        moduleInstanceRelationPropositionTable.clear();
    }

    public void invalidateRuleCaches() {
        moduleRuleTable.clear();
        moduleRelationRuleTable.clear();
        moduleConceptRuleTable.clear();
    }

    public void invalidateAllCaches() {
        moduleConceptTable.clear();
        moduleRelationTable.clear();
        moduleInstanceTable.clear();
        conceptInstanceTable.clear();
        conceptDirectInstanceTable.clear();
        conceptRelationTable.clear();
        conceptInheritedRelationTable.clear();
        moduleInstancePropositionTable.clear();
        modulePropositionTable.clear();
        moduleRuleTable.clear();
        moduleRelationRuleTable.clear();
        moduleConceptRuleTable.clear();
        moduleConceptPropositionTable.clear();
        moduleRelationPropositionTable.clear();
        moduleInstanceRelationPropositionTable.clear();
    }

    // Matching routines

    /**
     * @return an array of surrogate Id's whose beginning matches partialString
     */
    //todo? make search more efficient?
    public String[] matchAllSurrogates(String partialString) {
	partialString = partialString.toUpperCase();
	ArrayList result = new ArrayList();
	Iterator topIter = surrogateTable.values().iterator();
	while (topIter.hasNext()) {
	    HashMap idSurrogateTable = (HashMap)topIter.next();
	    Iterator nestedIter = idSurrogateTable.keySet().iterator();
	    while (nestedIter.hasNext()) {
		String id = ((String)nestedIter.next()).toUpperCase();
		if (id.startsWith(partialString)) {
		    result.add(id);
		}
	    }
	}
	Collections.sort(result);
	return ((String[])result.toArray(new String[0]));
    }

    public String getDocumentationForRelation(PLModule module, PLRelation relation) throws AppException {
	debugPrintln(3, "getting documentation for relation: module = " + module + ", relation = " + relation);
        PLString docString = server.getDocumentationForRelation(new PLString(module.getModuleName()),
							       new PLString(relation.getRelationName()));
	debugPrintln(3, "doc result = " + docString.attrValue);
	return docString.attrValue;
    }

    public PLSurrogateContainer getSuperRelationsForRelation(PLModule module, PLConcept concept) throws AppException {
        PLSurrogateCollection superSurrogates = server.getSuperConceptsForConcept
	    (new PLString(module.getModuleName()),
	     new PLString(concept.getConceptName()));
	PLSurrogateContainer result = referencizeSurrogateCollection(superSurrogates, PLConcept.class, "SuperConcepts");
	return result;
    }

    public PLSurrogateContainer getSuperRelationsForRelation(PLModule module, PLRelation relation) throws AppException {
        PLSurrogateCollection superSurrogates = server.getSuperRelationsForRelation
	    (new PLString(module.getModuleName()),
	     new PLString(relation.getRelationName()));
	PLSurrogateContainer result = referencizeSurrogateCollection(superSurrogates, PLRelation.class, "SuperRelations");
	return result;
    }

    public PLVariableList getVariablesForRelation(PLModule module, PLRelation relation) throws AppException {
        PLVariableList variableList = server.getVariablesForRelation
	    (new PLString(module.getModuleName()),
	     new PLString(relation.getRelationName()));
	referencizeVariableList(variableList);
	return variableList;
    }

    public PLSurrogateContainer getIncludesForModule(PLModule module) throws AppException {
        PLSurrogateCollection surrogates = server.getIncludesForModule
	    (new PLString(module.getModuleName()));
	PLSurrogateContainer result = referencizeSurrogateCollection(surrogates, PLModule.class, "IncludedModules");
	return result;
    }

    public PLSurrogateContainer getShadowedSurrogatesForModule(PLModule module) throws AppException {
        PLSurrogateCollection surrogates = server.getShadowedSurrogatesForModule
	    (new PLString(module.getModuleName()));
	// Kludgy: use non-standard class to intern symbols.... This could create problems....
	PLSurrogateContainer result = referencizeSurrogateCollection(surrogates, PLSymbol.class, "ShadowedSurrogates");
	return result;
    }

    public PLSurrogateContainer getUsesForModule(PLModule module) throws AppException {
        PLSurrogateCollection surrogates = server.getUsesForModule
	    (new PLString(module.getModuleName()));
	PLSurrogateContainer result = referencizeSurrogateCollection(surrogates, PLModule.class, "UsedModules");
	return result;
    }

    public String getDocumentationForInstance(PLModule module, PLInstance instance) throws AppException {
	debugPrintln(3, "getting documentation for relation: module = " + module + ", relation = " + instance);
        PLString docString = server.getDocumentationForInstance(new PLString(module.getModuleName()),
							       new PLString(instance.getInstanceName()));
	debugPrintln(3, "doc result = " + docString.attrValue);
	return docString.attrValue;
    }

    public Collection getLoadableKBs() throws AppException {
	debugPrintln(3, "getting loadable kbs...");
        PLModuleFileList fileList= server.getLoadableKBs();
	debugPrintln(3, "loadable kbs result = " + fileList.elemPLModuleFile);
	return fileList.elemPLModuleFile;
    }

    public void loadKB(String kbName) throws AppException {
		debugPrintln(3, "loading kb " + kbName + "...");
 	   	PLString result = server.loadKB(new PLString(kbName));
		debugPrintln(3, "loadable kbs result = " + result);
    }

    public String loadKB2(String fileName) throws AppException {
	PLString result = server.loadKB2(new PLString(fileName));
	return result.getValue();
    }

/**
 * Insert the method's description here.
 * Creation date: (4/24/2002 3:36:02 PM)
 * @param mod edu.isi.powerloom.gui.xmlobject.PLModule
 * @exception edu.isi.powerloom.gui.common.AppException The exception description.
 */
public void clearKB(PLModule mod) throws edu.isi.powerloom.gui.common.AppException {
	server.clearKB(mod);
}

    public PLSurrogateContainer getTypesForInstance(PLModule module, PLInstance instance) throws AppException {
        PLSurrogateCollection superSurrogates = server.getTypesForInstance
	    (new PLString(module.getModuleName()),
	     new PLString(instance.getID()));
	PLSurrogateContainer result = referencizeSurrogateCollection(superSurrogates, PLConcept.class, "InstanceTypes");
	return result;
    }

    public void saveKB(String modName, String kbName, String description) throws AppException {
		debugPrintln(3, "saving kb " + kbName + "...");
 	   	PLString result = server.saveKB(new PLString(modName), new PLString(kbName),
	 	   	                            new PLString(description));
		debugPrintln(3, "save kbs result = " + result);
    }

    public void saveKB2(String modName, String fileName) throws AppException {
		debugPrintln(3, "saving kb " + fileName + "...");
 	   	PLString result = server.saveKB2(new PLString(modName), new PLString(fileName));
		debugPrintln(3, "save kbs result = " + result);
    }

    public PLDirectoryContents getDefaultDirectoryListing(String extensionFilter) throws AppException {
	PLDirectoryContents result = server.getDefaultDirectoryListing(new PLString(extensionFilter));
	return result;
    }


    public PLDirectoryContents getDirectoryListing(String dirName, String extensionFilter) throws AppException {
	PLDirectoryContents result = server.getDirectoryListing(new PLString(dirName), 
								new PLString(extensionFilter));
	return result;
    }

    /**
     * Getxxx methods.  These methods always go to the server to retrieve the latest object.
     */
   
    public PLRelation getRelationObject(PLModule module, String relationName) throws AppException {
	 PLSurrogate surrogate = findOrCreateSurrogate(PLRelation.class, relationName);
	 PLRelation result = null;
	 PLRelationContainer container = server.getRelation(new PLString(module.getID()), new PLString(relationName));
	 if ((container.elemPLRelation != null) &&
	     (container.elemPLRelation.size() == 1)) {
	     result = (PLRelation)((List)container.elemPLRelation).get(0);
	 }
	 return result;
    }

    public PLConcept getConceptObject(PLModule module, String conceptName) throws AppException {
	 PLSurrogate surrogate = findOrCreateSurrogate(PLConcept.class, conceptName);
	 PLConcept result = null;
	 PLConceptContainer container = server.getConcept(new PLString(module.getID()), new PLString(conceptName));
	 if ((container.elemPLConcept != null) &&
	     (container.elemPLConcept.size() == 1)) {
	     result = (PLConcept)((List)container.elemPLConcept).get(0);
	 }
	 return result;
    }

    public PLInstance getInstanceObject(PLModule module, String instanceName) throws AppException {
	 PLSurrogate surrogate = findOrCreateSurrogate(PLInstance.class, instanceName);
	 PLInstance result = null;
	 PLInstanceContainer container = server.getInstance(new PLString(module.getID()), new PLString(instanceName));
	 if ((container.elemPLInstance != null) &&
	     (container.elemPLInstance.size() == 1)) {
	     result = (PLInstance)((List)container.elemPLInstance).get(0);
	 }
	 return result;
    }

    // Utility function to get the latest version of an object
    public PLObject getCurrentObject(Class objectType, PLObject object) throws AppException {
	return (PLObject)findOrCreateSurrogate(objectType, object.getID()).getValue();    
    }

    // given a name, try to get a concept, relation, and instance, in that order.
    public PLObject getPLObject(PLModule module, String objectName) throws AppException {
	// Some joker passed us a string...
	if (objectName.startsWith("\"")) {
	    return null;
	}
	PLObject result = getConceptObject(module, objectName);
	if (result != null) {
	    return result;
	}
	result = getRelationObject(module, objectName);
	if (result != null) {
	    return result;
	}
	result = getInstanceObject(module, objectName);
	return result;
    }

    /**
     * Get Module object given string name.  This handles module prefixes.
     */
    public PLModule getModuleFromName(String name) throws AppException {
	if (name.trim().length() == 0) {
	    return null;
	}
	int slashIndex = name.lastIndexOf('/');
	if (slashIndex == (name.length() - 1)) {
	    name = name.substring(0, name.length() - 1);
	    slashIndex = name.lastIndexOf('/');
	}
	if (slashIndex >= 0) {
	    name = name.substring(slashIndex+1);
	}
	PLSurrogate moduleSurrogate = getSurrogate(PLModule.class, name);
	if (moduleSurrogate == null) {
	    throw new AppException("Error: I don't know about a module named " + name);
	}
	PLModule module = (PLModule)moduleSurrogate.getValue();
	return module;
    }

}
