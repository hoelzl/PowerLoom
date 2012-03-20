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


// Version: SoapSender.java,v 1.15 2010/02/13 00:14:48 hans Exp

package edu.isi.powerloom.gui.serverinterface;

import javax.xml.soap.*;
import java.io.*;
import java.util.*;
import java.net.*;

import edu.isi.powerloom.gui.xmlobject.*;
import edu.isi.powerloom.gui.common.*;
import static edu.isi.powerloom.gui.common.Utils.*;
import edu.isi.powerloom.gui.*;

import org.exolab.castor.mapping.Mapping;
import org.exolab.castor.xml.*;

/**
 * Utility class for sending SOAP to the server.  This leverages the castor framework
 * for marshaling/unmarshaling objects.
 *
 * @author <a href=mailto:eric@metrotech-consulting.com>Eric Melz</a>
 * @since Thu Feb 21 16:30:27 2002
 */

public class SoapSender {
    private static final String CASTOR_MAPPING_FILE = "resources/conf/castor-mapping.xml";

    private static String serverURL = null;
    private static String defaultWebServicePath = "ploom/soap-rpc/powerloom-gui-service";
    
    private static Mapping castorMapping = readMappingFile();

    public static void clearServerURL() {
        // Reset the server URL so it will be recomputed the next time around.
        serverURL = null;
    }

    public static void setServerURL(String url) throws ClientException {
        // Set the server URL used by the SOAP client to `url'.
        // If `url' is null, derive a default using the preferences file.
        if (url != null)
            serverURL = url;
        else
            try {
                Preferences prefs = Preferences.getInstance();
                String host = prefs.getProperty(Preferences.HOST);
                String port = prefs.getProperty(Preferences.PORT);
                serverURL = "http://" + host + ":" + port + "/" + defaultWebServicePath;
            } catch (Exception e) {
                throw new ClientException("Could not read host and/or port from properties file.");
            }
    }

    public static String getServerURL() throws ClientException {
        if (serverURL == null)
            setServerURL(null);
        return serverURL;
    }

    /** Marshal arguments and send soap to client */
    public static XMLObject sendSoap(String methodName, Collection arguments) throws AppServerException, ClientException {
        AppServerException serverException = null;
        ServerException xmlException = null;
        XMLObject xmlResult = null;
        try {
            SOAPConnectionFactory scFactory = 
                SOAPConnectionFactory.newInstance();
            SOAPConnection con = scFactory.createConnection();

            MessageFactory factory = MessageFactory.newInstance();
            SOAPMessage message = factory.createMessage();

            SOAPPart soapPart = message.getSOAPPart();
            SOAPEnvelope envelope = soapPart.getEnvelope();
            Name encodingName = envelope.createName("encodingStyle",
                                                    "soap-env", 
                                                    "http://schemas.xmlsoap.org/soap/envelope/");

            envelope.addAttribute(encodingName, "http://schemas.xmlsoap.org/soap/encoding/");

            SOAPHeader header = envelope.getHeader();
            SOAPBody body = envelope.getBody();
			 
            header.detachNode();

            Name bodyName = envelope.createName(methodName,
                                                "ns0", "http://hello.org/wsdl");
            SOAPBodyElement gltp = body.addBodyElement(bodyName);

            Collection marshalledArgs = marshallArguments(arguments);
            Iterator argIter = marshalledArgs.iterator();
            while (argIter.hasNext()) {
                // NOTE: for now we kludge it using strings, should really use actual SOAPElements:
                String arg = (String)argIter.next();
                Name name = envelope.createName("String_1");
                SOAPElement symbol = gltp.addChildElement(name);
                symbol.addTextNode(arg);
            }
            URL endpoint = new URL(getServerURL());
	    SOAPMessage response = con.call(message, endpoint);

            con.close();

            // for debugging
	    /*
            debugPrintln(3, "***SOAP Message: ");
            message.writeTo(System.out);

            FileOutputStream fos = new FileOutputStream ("C:/Data/Java Projects/jaxmclient/jaxmout.xml");
            response.writeTo(fos);
            fos.close();
	    */

            SOAPPart sp = response.getSOAPPart();
            
            SOAPEnvelope se = sp.getEnvelope();
            SOAPBody sb = se.getBody();
            Name bodyResponseName = envelope.createName("response");
            Name resultName = envelope.createName("result");
            Name exceptionName = envelope.createName("exception");

            Iterator it = sb.getChildElements(bodyResponseName);
            SOAPBodyElement bodyElement = (SOAPBodyElement)it.next();
            Iterator bodyIt = bodyElement.getChildElements(resultName);
            SOAPElement result = null;
            String resultValue = null;
            if (bodyIt.hasNext()) {
                result = (SOAPElement)bodyIt.next();
                resultValue = result.getValue();
                // todo: use log4j for this
                //debugPrintln(3, "Received XML: \n" + resultValue);
                // Unmarshall the result
                boolean doValidation = false;
		xmlResult = unmarshallResult(resultValue, doValidation);
            }
            bodyIt = bodyElement.getChildElements(exceptionName);
            if (bodyIt.hasNext()) {
                result = (SOAPElement)bodyIt.next();
                resultValue = result.getValue();
                // Unmarshall the result
                boolean doValidation = false;
                xmlException = (ServerException)unmarshallResult(resultValue, doValidation);
                serverException = new AppServerException(xmlException);
            }
        } catch (Exception e) {
            //e.printStackTrace();
            throw new ClientException(e);
        }
        if (serverException != null) {
            throw serverException;   
        }
        return xmlResult;
    }

    private static Collection marshallArguments(Collection args) throws Exception {
        Collection result = new ArrayList();
        Iterator iter = args.iterator();
        while (iter.hasNext()) {
            XMLObject arg = (XMLObject)iter.next();
            String marshalledArg = marshallArgument(arg);
            result.add(marshalledArg);
        }
        return result;
    }

    private static String marshallArgument(XMLObject arg) throws Exception {
	String castorResult = marshallCastorArgument(arg);
	return castorResult;
    }


    private static XMLObject unmarshallResult(String resultString, boolean doValidation) throws Exception {
	XMLObject castorResult = unmarshallCastorResult(resultString, doValidation);
	//debugPrintln(3, "unmarshalledresult = " + castorResult);
	return castorResult;
    }

    /*
     *   Castor XML binding framework support
     */
    private static Mapping readMappingFile() {
	Mapping mapping = null;
	try {
	    mapping = new Mapping();
            java.net.URL url;
            if (edu.isi.powerloom.gui.components.PowerloomApp.DEVELOPMENT_MODE) {
                url = new URL("file://" + edu.isi.powerloom.gui.components.PowerloomApp.RESOURCE_BASE_PATH + CASTOR_MAPPING_FILE);
            } else {
                url = SoapSender.class.getClassLoader().getResource(CASTOR_MAPPING_FILE);
            }
	    mapping.loadMapping(url);
	} catch (Exception e) {
	    edu.isi.powerloom.gui.components.PowerloomApp.getInstance().handleException(e);
	}
	return mapping;
    }

    private static String marshallCastorArgument(XMLObject arg) throws Exception {
	StringWriter sw = new StringWriter();
	Marshaller mappedMarshaller = new Marshaller(sw);
	mappedMarshaller.setMarshalAsDocument(false);
	mappedMarshaller.setMapping(castorMapping);
	mappedMarshaller.marshal(arg);
	return sw.toString();
    }

    private static XMLObject unmarshallCastorResult(String resultString, boolean doValidation) 
	throws Exception{
	StringReader sr = new StringReader(resultString);
	Unmarshaller mappedUnmarshaller = new Unmarshaller(castorMapping);
	XMLObject result = (XMLObject)mappedUnmarshaller.unmarshal(sr);
	if (result == null) {
	    debugPrintln(3, "*** Got null when unmarshalling castor xml: " + resultString);
	}
	return result;
    }

    /**
     *  Public interface to marshalling/unmarshalling xmlObjects.  Todo?  Move this to
     *  another class.
     */
    public static String marshalXMLObject(XMLObject object) throws Exception {
	return marshallCastorArgument(object);
    }

    public static XMLObject unmarshalXMLObject(String xmlString) throws Exception {
	return unmarshallCastorResult(xmlString, true);
    }
}
