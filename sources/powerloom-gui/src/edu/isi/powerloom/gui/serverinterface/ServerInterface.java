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


// Version: ServerInterface.java,v 1.15 2010/02/04 05:20:33 hans Exp

package edu.isi.powerloom.gui.serverinterface;

import edu.isi.powerloom.gui.xmlobject.*;

import edu.isi.powerloom.gui.common.*;

/**
 * This specifies the contract between the client and server. Implementations
 * of this interface are produced by InterfaceFactory
 *
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 * @since Sun Feb 24 16:06:23 2002
 * @see edu.isi.powerloom.gui.serverinterface.InterfaceFactory InterfaceFactory
 */

public interface ServerInterface {
    public PLModuleContainer getModules() throws AppException;

    public PLConceptContainer getConceptsForModule(PLString modName) throws AppException;

    public PLInstanceContainer getInstancesForModule(PLString modName) throws AppException;

    public PLRelationContainer getRelationsForModule(PLString modName) throws AppException;

    public PLInstanceContainer getInstancesForConcept(PLString modName, PLString conceptName) throws AppException;

    public PLInstanceContainer getDirectInstancesForConcept(PLString modName, PLString conceptName) throws AppException;

    public PLString getDocumentationForConcept(PLString modName, PLString conceptName) throws AppException;

    public PLString getSourceForConcept(PLString modName, PLString conceptName) throws AppException;

    public PLSurrogateCollection getSuperConceptsForConcept(PLString modName, PLString conceptName) throws AppException;

    public PLPropositionContainer getPropositionsForInstance(PLString modName, PLString instanceName) throws AppException;

    public PLRelationContainer getRelationsForConcept(PLString modName, PLString conceptName) throws AppException;

    public PLRelationContainer getInheritedRelationsForConcept(PLString modName, PLString conceptName) throws AppException;

    public PLPropositionContainer getPropositionsForConcept(PLString modName, PLString conceptName) throws AppException;

    public PLPropositionContainer getPropositionsForRelation(PLString modName, PLString relationName) throws AppException;

    public PLPropositionContainer getRulesForConcept(PLString modName, PLString conceptName) throws AppException;

    public PLPropositionContainer getRulesForRelation(PLString modName, PLString relationName) throws AppException;

    public PLPropositionContainer getPropositionsForInstanceAndRelation(PLString modName, PLString instanceName, PLString relationName) throws AppException;

    public PLString evaluateLogicCommand(PLString modName, PLString command) throws AppException;

    PLString getDocumentationForRelation(PLString modName, PLString conceptName) throws edu.isi.powerloom.gui.common.AppException;

    PLSurrogateCollection getSuperRelationsForRelation(PLString modName, PLString conceptName) throws edu.isi.powerloom.gui.common.AppException;

    PLVariableList getVariablesForRelation(PLString modName, PLString conceptName) throws edu.isi.powerloom.gui.common.AppException;

/**
 * Insert the method's description here.
 * Creation date: (4/24/2002 3:35:10 PM)
 * @param mod edu.isi.powerloom.gui.xmlobject.PLModule
 * @exception edu.isi.powerloom.gui.common.AppException The exception description.
 */
void clearKB(PLModule mod) throws edu.isi.powerloom.gui.common.AppException;

    PLString getDocumentationForInstance(PLString modName, PLString conceptName) throws edu.isi.powerloom.gui.common.AppException;

/**
 * Insert the method's description here.
 * Creation date: (4/16/2002 5:45:30 PM)
 * @return edu.isi.powerloom.gui.xmlobject.PLSurrogateCollection
 * @param modName edu.isi.powerloom.gui.xmlobject.PLString
 * @exception edu.isi.powerloom.gui.common.AppException The exception description.
 */
PLSurrogateCollection getIncludesForModule(PLString modName) throws edu.isi.powerloom.gui.common.AppException;

/**
 * Insert the method's description here.
 * Creation date: (4/23/2002 8:22:13 PM)
 * @return edu.isi.powerloom.gui.xmlobject.PLModuleFileList
 * @exception edu.isi.powerloom.gui.common.AppException The exception description.
 */
PLModuleFileList getLoadableKBs() throws edu.isi.powerloom.gui.common.AppException;

/**
 * Insert the method's description here.
 * Creation date: (4/16/2002 5:46:13 PM)
 * @return edu.isi.powerloom.gui.xmlobject.PLSurrogateCollection
 * @param modName edu.isi.powerloom.gui.xmlobject.PLString
 * @exception edu.isi.powerloom.gui.common.AppException The exception description.
 */
PLSurrogateCollection getShadowedSurrogatesForModule(PLString modName) throws edu.isi.powerloom.gui.common.AppException;

    public PLSurrogateCollection getTypesForInstance(PLString modName, PLString instanceName) throws AppException;

/**
 * Insert the method's description here.
 * Creation date: (4/16/2002 5:45:56 PM)
 * @return edu.isi.powerloom.gui.xmlobject.PLSurrogateCollection
 * @param modName edu.isi.powerloom.gui.xmlobject.PLString
 * @exception edu.isi.powerloom.gui.common.AppException The exception description.
 */
PLSurrogateCollection getUsesForModule(PLString modName) throws edu.isi.powerloom.gui.common.AppException;

/**
 * Insert the method's description here.
 * Creation date: (4/24/2002 1:21:56 PM)
 * @return edu.isi.powerloom.gui.xmlobject.PLString
 * @param kbName edu.isi.powerloom.gui.xmlobject.PLString
 * @exception edu.isi.powerloom.gui.common.AppException The exception description.
 */
PLString loadKB(PLString kbName) throws edu.isi.powerloom.gui.common.AppException;

// Variant that can handle base-64 encoded content.
PLString loadKB2(PLString fileName) throws edu.isi.powerloom.gui.common.AppException;

/**
 * Insert the method's description here.
 * Creation date: (4/24/2002 1:22:28 PM)
 * @return edu.isi.powerloom.gui.xmlobject.PLString
 * @param modName edu.isi.powerloom.gui.xmlobject.PLString
 * @param kbName edu.isi.powerloom.gui.xmlobject.PLString
 * @param description edu.isi.powerloom.gui.xmlobject.PLString
 * @exception edu.isi.powerloom.gui.common.AppException The exception description.
 */
PLString saveKB(PLString modName, PLString kbName, PLString description) throws edu.isi.powerloom.gui.common.AppException;

// Variant that can handle base-64 encoded content.
PLString saveKB2(PLString modName, PLString fileName) throws edu.isi.powerloom.gui.common.AppException;

    PLQueryResult query(PLQuery query, PLString continueFlag) throws edu.isi.powerloom.gui.common.AppException;

    PLSearchResult search(PLSearchParameter searchParameter) throws edu.isi.powerloom.gui.common.AppException;

    PLDirectoryContents getDefaultDirectoryListing(PLString extensionFilter) throws edu.isi.powerloom.gui.common.AppException;

    PLDirectoryContents getDirectoryListing(PLString currentDir, PLString extensionFilter) throws edu.isi.powerloom.gui.common.AppException;

    PLQueryResult getExtensionForRelation(PLString modName, PLString relation) throws edu.isi.powerloom.gui.common.AppException;

    PLRelationContainer getRelation(PLString modName, PLString relation) throws edu.isi.powerloom.gui.common.AppException;

    PLConceptContainer getConcept(PLString modName, PLString concept) throws edu.isi.powerloom.gui.common.AppException;

    PLInstanceContainer getInstance(PLString modName, PLString instance) throws edu.isi.powerloom.gui.common.AppException;

    PLRelationContainer getRelationCompletions(PLString modName, PLString prefix) throws edu.isi.powerloom.gui.common.AppException;

    PLConceptContainer getConceptCompletions(PLString modName, PLString prefix) throws edu.isi.powerloom.gui.common.AppException;

    PLInstanceContainer getInstanceCompletions(PLString modName, PLString prefix) throws edu.isi.powerloom.gui.common.AppException;

    PLString destroyObject(PLString modName, PLString objectName) throws edu.isi.powerloom.gui.common.AppException;

    PLString getExplanationForQuery(PLQuery query, PLString resultNum) throws edu.isi.powerloom.gui.common.AppException;

    PLServerInfo getServerInfo() throws edu.isi.powerloom.gui.common.AppException;
}
