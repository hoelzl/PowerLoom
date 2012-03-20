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
 | Portions created by the Initial Developer are Copyright (C) 2005-2010      |
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


// Version: PowerLoomServlet.java,v 1.6 2010/10/11 21:42:37 hans Exp

// Powerloom Servlet

package edu.isi.webtools.http.tomcat;

import edu.isi.webtools.*;
import edu.isi.webtools.http.*;

import java.io.*;
import java.util.*;
import java.net.URLDecoder;
import javax.servlet.*;
import javax.servlet.http.*;

import edu.isi.powerloom.PLI;
import edu.isi.powerloom.logic.Logic;
import edu.isi.stella.Stella;
import edu.isi.stella.StringWrapper;
import edu.isi.stella.javalib.Native;
import edu.isi.stella.javalib.PrintStringStream;


/**  A top-level Servlet class for PowerLoom to work with the Tomcat or
 *   other servers which follow the javax servlet standards.  This needs
 *   to be used/compiled with the servlet API in the classpath, for example
 *   javac -classpath $TOMCAT_HOME/lib/servlet-api.jar:. ...
 *
 * @version PowerLoomServlet.java,v 1.6 2010/10/11 21:42:37 hans Exp
 *
 */
public class PowerLoomServlet extends HttpServlet {

    public ServletConfig config;
    public ServletContext context;

    /**  This will bootstrap PowerLoom so we can initialize other stuff.
     */
    public void initialize () throws ServletException {
        PLI.initialize();
        // define ploom as the PowerLoom root directory so
        // that we can easily find KBs and system definitions:
        String plRoot = context.getRealPath("/");
        synchronized (Logic.$POWERLOOM_LOCK$) {
            Stella.defineLogicalHostProperty("PL",
                                             Stella.KWD_ROOT_DIRECTORY,
                                             StringWrapper.wrapString(plRoot));
            context.log("PowerLoom: PL root=" + Stella.translateLogicalPathname("PL:"));
            Stella.addLoadPath("PL:kbs;");
        }
    }

    /** Configure additional systems and KBs to load.
     *  This should be called after initialize.
     */
    public void configure () throws ServletException {
        String configValue = null;
        synchronized (Logic.$POWERLOOM_LOCK$) {
            configValue = config.getInitParameter("LoadPath");
            if (configValue != null) {extendLoadPath(configValue);}
            context.log ("PowerLoom: load path=" +
                         Stella.replaceSubstrings(Native.stringify(Stella.getLoadPath()), "", "\""));

            configValue = config.getInitParameter("LoadSystems");
            if (configValue != null) {loadSystems(configValue);}

            configValue = config.getInitParameter("LoadKBs");
            if (configValue != null) {loadKBs(configValue);}
        }
    }

    public static String [] getParameters (String parameterString,
                                           String delimiters) throws Exception {
        StringTokenizer tok = new StringTokenizer (parameterString, delimiters);
        String [] parameters = new String [tok.countTokens()];
        for (int i = 0; i < parameters.length; i++) {
            parameters[i] = URLDecoder.decode(tok.nextToken());
        }
        return parameters;
    }

    void extendLoadPath (String pathList) throws ServletException {
        // TO DO: this needs to appropriately relativize paths if necessary.
        try {
            String[] paths = getParameters(pathList, " \r\n\t");
            for (int i = 0; i < paths.length; i++) {
                Stella.addLoadPath(paths[i]);
            }
        } catch (Exception e) {
            throw new ServletException("PowerLoom: processing LoadPath failed: "
                                       + e.getMessage(), e);
        }
    }

    void loadSystems (String systemList) throws ServletException {
        String system = null;
        try {
            String[] systems = getParameters(systemList, " \r\n\t");
            for (int i = 0; i < systems.length; i++) {
                system = systems[i];
                context.log("PowerLoom: loading system " + system);
                Stella.loadSystem(system, Stella.NIL);
            }
        } catch (Exception e) {
            throw new ServletException("PowerLoom: loading system " + system + " failed: "
                                       + e.getMessage(), e);
        }
    }

    void loadKBs (String KBList) throws ServletException {
        String kb = null;
        try {
            String[] KBs = getParameters(KBList, " \r\n\t");
            for (int i = 0; i < KBs.length; i++) {
                kb = KBs[i];
                context.log("PowerLoom: loading KB " + kb);
                PLI.load(kb, null);
            }
        } catch (Exception e) {
            throw new ServletException("PowerLoom: loading KB " + kb + " failed: "
                                       + e.getMessage(), e);
        }
    }

    /** Default init method
     */
    public void init (ServletConfig config) throws ServletException {
        this.config = config;
        this.context = config.getServletContext();
        initialize();
        configure();
        Http.publishRegisteredHttpHandlers();
        context.log("PowerLoom Servlet started");
    }

    /** Services a single GET request from the client
     * Calls `servletHandleRequest' which dispatches handlers based on the URL.
     * @param req the servlet request
     * @param res the servlet response
     * @exception ServletException when an exception has occurred
     */
    protected void doGet (HttpServletRequest req, HttpServletResponse res)
        throws ServletException ,IOException
    {
        context.log(req.getRemoteHost() + " GET " + req.getRequestURI());
        HttpExchangeTomcat xchg = 
            ((HttpServerTomcat)Http.$HTTP_SERVER_IMPLEMENTATION$).servletHandleRequest(req, res);
        // create the response:
        res.setStatus(xchg.responseCode);
        // make sure we use bytes here to properly handle binary files:
        byte[] body = ((PrintStringStream)xchg.replyStream).toByteArray();
        ServletOutputStream out = res.getOutputStream();
        out.write(body);
        out.flush();
        out.close();
    }

    /** Services a single POST request from the client.
     * Calls `servletHandleRequest' which dispatches handlers based on the URL.
     * @param req the servlet request
     * @param res the servlet response
     * @exception ServletException when an exception has occurred
     */
    protected void doPost (HttpServletRequest req, HttpServletResponse res)
        throws ServletException,IOException
    {
        context.log(req.getRemoteHost() + " POST " + req.getRequestURI());
        HttpExchangeTomcat xchg = 
            ((HttpServerTomcat)Http.$HTTP_SERVER_IMPLEMENTATION$).servletHandleRequest(req, res);
        // create the response:
        res.setStatus(xchg.responseCode);
        // make sure we use bytes here to properly handle binary files:
        byte[] body = ((PrintStringStream)xchg.replyStream).toByteArray();
        ServletOutputStream out = res.getOutputStream();
        out.write(body);
        out.flush();
        out.close();
    }
}
