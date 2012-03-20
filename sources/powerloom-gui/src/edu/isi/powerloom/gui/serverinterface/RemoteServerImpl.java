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


// Version: RemoteServerImpl.java,v 1.17 2010/02/04 05:20:32 hans Exp

package edu.isi.powerloom.gui.serverinterface;

import edu.isi.powerloom.gui.xmlobject.*;
import edu.isi.powerloom.gui.common.*;
import static edu.isi.powerloom.gui.common.Utils.*;
import java.util.*;


/**
 * Interface responsible for communicating with a remote server.
 *
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 * @since Sun Feb 24 16:09:47 2002
 */

public class RemoteServerImpl implements ServerInterface {
    public PLModuleContainer getModules() throws AppException {
        String methodName = "SERVER-GET-MODULES";
        Collection args = new ArrayList();
        PLModuleContainer result = (PLModuleContainer)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLConceptContainer getConceptsForModule(PLString modName) throws AppException {
        String methodName = "SERVER-GET-CONCEPTS-FOR-MODULE";
        Collection args = new ArrayList();
        args.add(modName);
        PLConceptContainer result = (PLConceptContainer)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLInstanceContainer getInstancesForModule(PLString modName) throws AppException {
        String methodName = "SERVER-GET-INSTANCES-FOR-MODULE";
        Collection args = new ArrayList();
        args.add(modName);
        PLInstanceContainer result = (PLInstanceContainer)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLRelationContainer getRelationsForModule(PLString modName) throws AppException {
        String methodName = "SERVER-GET-RELATIONS-FOR-MODULE";
        Collection args = new ArrayList();
        args.add(modName);
        PLRelationContainer result = (PLRelationContainer)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLInstanceContainer getInstancesForConcept(PLString modName, PLString conceptName)
        throws AppException {
        String methodName = "SERVER-GET-INSTANCES-FOR-CONCEPT";
        Collection args = new ArrayList();
        args.add(modName);
        args.add(conceptName);
        PLInstanceContainer result = (PLInstanceContainer)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLInstanceContainer getDirectInstancesForConcept(PLString modName, PLString conceptName)
        throws AppException {
        String methodName = "SERVER-GET-DIRECT-INSTANCES-FOR-CONCEPT";
        Collection args = new ArrayList();
        args.add(modName);
        args.add(conceptName);
        PLInstanceContainer result = (PLInstanceContainer)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLString getDocumentationForConcept(PLString modName, PLString conceptName) throws AppException {
        String methodName = "SERVER-GET-DOCUMENTATION-FOR-CONCEPT";
        Collection args = new ArrayList();
        args.add(modName);
        args.add(conceptName);
        PLString result = (PLString)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLString getSourceForConcept(PLString modName, PLString conceptName) throws AppException {
        String methodName = "SERVER-GET-SOURCE-FOR-CONCEPT";
        Collection args = new ArrayList();
        args.add(modName);
        args.add(conceptName);
        PLString result = (PLString)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLString getDocumentationForRelation(PLString modName, PLString relationName) throws AppException {
        String methodName = "SERVER-GET-DOCUMENTATION-FOR-RELATION";
        Collection args = new ArrayList();
        args.add(modName);
        args.add(relationName);
        PLString result = (PLString)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLString getDocumentationForInstance(PLString modName, PLString instanceName) throws AppException {
        String methodName = "SERVER-GET-DOCUMENTATION-FOR-INSTANCE";
        Collection args = new ArrayList();
        args.add(modName);
        args.add(instanceName);
        PLString result = (PLString)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLSurrogateCollection getSuperConceptsForConcept(PLString modName, PLString conceptName) throws AppException {
        String methodName = "SERVER-GET-SUPERCONCEPTS-FOR-CONCEPT";
        Collection args = new ArrayList();
        args.add(modName);
        args.add(conceptName);
        PLSurrogateCollection result = (PLSurrogateCollection)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLSurrogateCollection getSuperRelationsForRelation(PLString modName, PLString relationName) throws AppException {
        String methodName = "SERVER-GET-SUPERRELATIONS-FOR-RELATION";
        Collection args = new ArrayList();
        args.add(modName);
        args.add(relationName);
        PLSurrogateCollection result = (PLSurrogateCollection)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLSurrogateCollection getIncludesForModule(PLString modName) throws AppException {
        String methodName = "SERVER-GET-INCLUDES-FOR-MODULE";
        Collection args = new ArrayList();
        args.add(modName);
        PLSurrogateCollection result = (PLSurrogateCollection)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLSurrogateCollection getUsesForModule(PLString modName) throws AppException {
        String methodName = "SERVER-GET-USES-FOR-MODULE";
        Collection args = new ArrayList();
        args.add(modName);
        PLSurrogateCollection result = (PLSurrogateCollection)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLSurrogateCollection getShadowedSurrogatesForModule(PLString modName) throws AppException {
        String methodName = "SERVER-GET-SHADOWED-SURROGATES-FOR-MODULE";
        Collection args = new ArrayList();
        args.add(modName);
        PLSurrogateCollection result = (PLSurrogateCollection)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLVariableList getVariablesForRelation(PLString modName, PLString relationName) throws AppException {
        String methodName = "SERVER-GET-VARIABLES-FOR-RELATION";
        Collection args = new ArrayList();
        args.add(modName);
        args.add(relationName);
        PLVariableList result = (PLVariableList)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLPropositionContainer getPropositionsForInstance(PLString modName, PLString instanceName)
        throws AppException {
        String methodName = "SERVER-GET-PROPOSITIONS-FOR-INSTANCE";
        Collection args = new ArrayList();
        args.add(modName);
        args.add(instanceName);
        PLPropositionContainer result = (PLPropositionContainer)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLSurrogateCollection getTypesForInstance(PLString modName, PLString instanceName)
        throws AppException {
        String methodName = "SERVER-GET-TYPES-FOR-INSTANCE";
        Collection args = new ArrayList();
        args.add(modName);
        args.add(instanceName);
        PLSurrogateCollection result = (PLSurrogateCollection)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLRelationContainer getRelationsForConcept(PLString modName, PLString conceptName) throws AppException {
        String methodName = "SERVER-GET-RELATIONS-FOR-CONCEPT";
        Collection args = new ArrayList();
        args.add(modName);
        args.add(conceptName);
        PLRelationContainer result = (PLRelationContainer)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLRelationContainer getInheritedRelationsForConcept(PLString modName, PLString conceptName) throws AppException {
        String methodName = "SERVER-GET-INHERITED-RELATIONS-FOR-CONCEPT";
        Collection args = new ArrayList();
        args.add(modName);
        args.add(conceptName);
        PLRelationContainer result = (PLRelationContainer)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLPropositionContainer getPropositionsForConcept(PLString modName, PLString conceptName) throws AppException {
        String methodName = "SERVER-GET-PROPOSITIONS-FOR-CONCEPT";
        Collection args = new ArrayList();
        args.add(modName);
        args.add(conceptName);
        PLPropositionContainer result = (PLPropositionContainer)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLPropositionContainer getPropositionsForRelation(PLString modName, PLString relationName) throws AppException {
        String methodName = "SERVER-GET-PROPOSITIONS-FOR-RELATION";
        Collection args = new ArrayList();
        args.add(modName);
        args.add(relationName);
        PLPropositionContainer result = (PLPropositionContainer)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLPropositionContainer getRulesForConcept(PLString modName, PLString conceptName) throws AppException {
        String methodName = "SERVER-GET-RULES-FOR-CONCEPT";
        Collection args = new ArrayList();
        args.add(modName);
        args.add(conceptName);
        PLPropositionContainer result = (PLPropositionContainer)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLPropositionContainer getRulesForRelation(PLString modName, PLString relationName) throws AppException {
        String methodName = "SERVER-GET-RULES-FOR-RELATION";
        Collection args = new ArrayList();
        args.add(modName);
        args.add(relationName);
        PLPropositionContainer result = (PLPropositionContainer)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLPropositionContainer getPropositionsForInstanceAndRelation(PLString modName, PLString instanceName, PLString relationName) throws AppException {
        String methodName = "SERVER-GET-PROPOSITIONS-FOR-INSTANCE-AND-RELATION";
        Collection args = new ArrayList();
        args.add(modName);
        args.add(instanceName);
        args.add(relationName);
        PLPropositionContainer result = (PLPropositionContainer)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLString evaluateLogicCommand(PLString modName, PLString command) throws AppException {
        String methodName = "SERVER-EVALUATE-LOGIC-COMMAND";
        Collection args = new ArrayList();
        args.add(modName);
        args.add(command);
        PLString result = (PLString)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLModuleFileList getLoadableKBs() throws AppException {
        String methodName = "SERVER-GET-LOADABLE-KBS";
        Collection args = new ArrayList();
        PLModuleFileList result = (PLModuleFileList)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLString loadKB(PLString kbName) throws AppException {
        String methodName = "SERVER-LOAD-KB";
        Collection args = new ArrayList();
        args.add(kbName);
        PLString result = (PLString)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLString loadKB2(PLString fileName) throws AppException {
        String methodName = "SERVER-LOAD-KB2";
        Collection args = new ArrayList();
        PLFile theFile = new PLFile();
        theFile.attrFileName = fileName.getValue();
        theFile.elemContent = Utils.fileToString(fileName.getValue());
        //theFile.elemContent = "*** some  ***  <test;>>content\nnext<line";
        args.add(theFile);
        PLString result = (PLString)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLString saveKB(PLString modName, PLString kbName, PLString description) throws AppException {
        String methodName = "SERVER-SAVE-KB";
        Collection args = new ArrayList();
        args.add(modName);
        args.add(kbName);
        args.add(description);
        PLString result = (PLString)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLString saveKB2(PLString modName, PLString fileName) throws AppException {
        String methodName = "SERVER-SAVE-KB2";
        Collection args = new ArrayList();
        args.add(modName);
        args.add(fileName);
        PLFile result = (PLFile)SoapSender.sendSoap(methodName, args);
        Utils.stringToFile(result.elemContent, result.attrFileName);
        return fileName;
    }

    public void clearKB(PLModule module) throws AppException {
        String methodName = "SERVER-CLEAR-KB";
        Collection args = new ArrayList();
        args.add(module);
        SoapSender.sendSoap(methodName, args);
    }


    public PLQueryResult query(PLQuery query, PLString continueFlag) throws edu.isi.powerloom.gui.common.AppException {
        String methodName = "SERVER-QUERY";
        Collection args = new ArrayList();
        args.add(query);
        args.add(continueFlag);
        PLQueryResult result = (PLQueryResult)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLSearchResult search(PLSearchParameter searchParameter) throws edu.isi.powerloom.gui.common.AppException {
        String methodName = "SERVER-SEARCH";
        Collection args = new ArrayList();
        args.add(searchParameter);
        PLSearchResult result = (PLSearchResult)SoapSender.sendSoap(methodName, args);
	debugPrintln(3, "Remote search result is: " + result);
        return result;
    }

    public PLDirectoryContents getDefaultDirectoryListing(PLString extensionFilter) throws edu.isi.powerloom.gui.common.AppException {
        String methodName = "SERVER-GET-DEFAULT-DIRECTORY-LISTING";
        Collection args = new ArrayList();
	args.add(extensionFilter);
        PLDirectoryContents result = (PLDirectoryContents)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLDirectoryContents getDirectoryListing(PLString currentDir, PLString extensionFilter) throws edu.isi.powerloom.gui.common.AppException {
        String methodName = "SERVER-GET-DIRECTORY-LISTING";
        Collection args = new ArrayList();
	args.add(currentDir);
	args.add(extensionFilter);
        PLDirectoryContents result = (PLDirectoryContents)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLQueryResult getExtensionForRelation(PLString modName, PLString relationName) throws edu.isi.powerloom.gui.common.AppException {
        String methodName = "SERVER-GET-EXTENSION-FOR-RELATION";
        Collection args = new ArrayList();
        args.add(modName);
        args.add(relationName);
        PLQueryResult result = (PLQueryResult)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLRelationContainer getRelation(PLString modName, PLString relation) throws edu.isi.powerloom.gui.common.AppException {
        String methodName = "SERVER-GET-RELATION";
        Collection args = new ArrayList();
        args.add(modName);
        args.add(relation);
        PLRelationContainer result = (PLRelationContainer)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLConceptContainer getConcept(PLString modName, PLString concept) throws edu.isi.powerloom.gui.common.AppException {
        String methodName = "SERVER-GET-CONCEPT";
        Collection args = new ArrayList();
        args.add(modName);
        args.add(concept);
        PLConceptContainer result = (PLConceptContainer)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLInstanceContainer getInstance(PLString modName, PLString instance) throws edu.isi.powerloom.gui.common.AppException {
        String methodName = "SERVER-GET-INSTANCE";
        Collection args = new ArrayList();
        args.add(modName);
        args.add(instance);
        PLInstanceContainer result = (PLInstanceContainer)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLRelationContainer getRelationCompletions(PLString modName, PLString prefix) throws edu.isi.powerloom.gui.common.AppException {
        String methodName = "SERVER-GET-RELATION-COMPLETIONS";
        Collection args = new ArrayList();
        args.add(modName);
        args.add(prefix);
        PLRelationContainer result = (PLRelationContainer)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLConceptContainer getConceptCompletions(PLString modName, PLString prefix) throws edu.isi.powerloom.gui.common.AppException {
        String methodName = "SERVER-GET-CONCEPT-COMPLETIONS";
        Collection args = new ArrayList();
        args.add(modName);
        args.add(prefix);
        PLConceptContainer result = (PLConceptContainer)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLInstanceContainer getInstanceCompletions(PLString modName, PLString prefix) throws edu.isi.powerloom.gui.common.AppException {
        String methodName = "SERVER-GET-INSTANCE-COMPLETIONS";
        Collection args = new ArrayList();
        args.add(modName);
        args.add(prefix);
        PLInstanceContainer result = (PLInstanceContainer)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLString destroyObject(PLString modName, PLString objectName) throws edu.isi.powerloom.gui.common.AppException {
        String methodName = "SERVER-DESTROY-OBJECT";
        Collection args = new ArrayList();
        args.add(modName);
        args.add(objectName);
        PLString result = (PLString)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLString getExplanationForQuery(PLQuery query, PLString resultNum) throws edu.isi.powerloom.gui.common.AppException {
        String methodName = "SERVER-GET-EXPLANATION-FOR-QUERY";
        Collection args = new ArrayList();
        args.add(query);
        args.add(resultNum);
        PLString result = (PLString)SoapSender.sendSoap(methodName, args);
        return result;
    }

    public PLServerInfo getServerInfo() throws edu.isi.powerloom.gui.common.AppException {
        String methodName = "SERVER-GET-INFO";
        Collection args = new ArrayList();
        PLServerInfo result = (PLServerInfo)SoapSender.sendSoap(methodName, args);
        return result;
    }

}
