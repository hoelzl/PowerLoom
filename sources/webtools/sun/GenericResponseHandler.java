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
 | The Original Code is the STELLA Programming Language.                      |
 |                                                                            |
 | The Initial Developer of the Original Code is                              |
 | UNIVERSITY OF SOUTHERN CALIFORNIA, INFORMATION SCIENCES INSTITUTE          |
 | 4676 Admiralty Way, Marina Del Rey, California 90292, U.S.A.               |
 |                                                                            |
 | Portions created by the Initial Developer are Copyright (C) 2009-2010      |
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


// Version: GenericResponseHandler.java,v 1.1 2010/02/11 01:23:11 hans Exp

package edu.isi.webtools.http.sun;

import java.io.*;
import java.net.URI;
import com.sun.net.httpserver.*;

import edu.isi.stella.PropertyList;
import edu.isi.stella.javalib.*;


public class GenericResponseHandler implements HttpHandler {

    public String defaultContentType = "text/html";
    public java.lang.reflect.Method handlerFunction = null;
    public PropertyList handlerOptions = null;

    public String getDefaultContentType() {
        return defaultContentType;
    }

    public void setDefaultContentType(String contentType) {
        defaultContentType = contentType;
    }

    public java.lang.reflect.Method getHandlerFunction() {
        return handlerFunction;
    }

    public void setHandlerFunction(java.lang.reflect.Method function) {
        handlerFunction = function;
    }

    public PropertyList getHandlerOptions() {
        return handlerOptions;
    }

    public void setHandlerOptions(PropertyList options) {
        handlerOptions = options;
    }

    public void handle(com.sun.net.httpserver.HttpExchange xch) throws IOException {
        // setup defaults:
        xch.getResponseHeaders().set("content-type", defaultContentType);

        // create STELLA exchange object:
        HttpExchangeSun exchange = HttpExchangeSun.newHttpExchangeSun();
        exchange.nativeExchange = xch;
        exchange.handlerOptions = handlerOptions;

        // call the STELLA handler:
        Native.funcall(handlerFunction, null, new Object[]{exchange});

        // create the response:
        int responseCode = exchange.responseCode;
        // make sure we use bytes here to properly handle binary files:
        byte[] responseBody = ((PrintStringStream)exchange.replyStream).toByteArray();
        xch.sendResponseHeaders(responseCode, responseBody.length);
        java.io.OutputStream response = xch.getResponseBody();
        response.write(responseBody);
        xch.close();
    }
}
