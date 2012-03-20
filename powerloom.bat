@ECHO OFF

REM ########################### BEGIN LICENSE BLOCK ############################
REM                                                                            #
REM Version: MPL 1.1/GPL 2.0/LGPL 2.1                                          #
REM                                                                            #
REM The contents of this file are subject to the Mozilla Public License        #
REM Version 1.1 (the "License"); you may not use this file except in           #
REM compliance with the License. You may obtain a copy of the License at       #
REM http://www.mozilla.org/MPL/                                                #
REM                                                                            #
REM Software distributed under the License is distributed on an "AS IS" basis, #
REM WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   #
REM for the specific language governing rights and limitations under the       #
REM License.                                                                   #
REM                                                                            #
REM The Original Code is the PowerLoom KR&R System.                            #
REM                                                                            #
REM The Initial Developer of the Original Code is                              #
REM UNIVERSITY OF SOUTHERN CALIFORNIA, INFORMATION SCIENCES INSTITUTE          #
REM 4676 Admiralty Way, Marina Del Rey, California 90292, U.S.A.               #
REM                                                                            #
REM Portions created by the Initial Developer are Copyright (C) 1996-2010      #
REM the Initial Developer. All Rights Reserved.                                #
REM                                                                            #
REM Contributor(s):                                                            #
REM                                                                            #
REM Alternatively, the contents of this file may be used under the terms of    #
REM either the GNU General Public License Version 2 or later (the "GPL"), or   #
REM the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),   #
REM in which case the provisions of the GPL or the LGPL are applicable instead #
REM of those above. If you wish to allow use of your version of this file only #
REM under the terms of either the GPL or the LGPL, and not to allow others to  #
REM use your version of this file under the terms of the MPL, indicate your    #
REM decision by deleting the provisions above and replace them with the notice #
REM and other provisions required by the GPL or the LGPL. If you do not delete #
REM the provisions above, a recipient may use your version of this file under  #
REM the terms of any one of the MPL, the GPL or the LGPL.                      #
REM                                                                            #
REM ############################ END LICENSE BLOCK #############################

REM Version: powerloom.bat,v 1.3 2010/10/15 01:55:11 hans Exp
REM
REM Run the C++ or Java version of the PowerLoom KR&R system in
REM a Windows 2000/XP command prompt window.  If you double-click
REM on this file, it will lauch the Java GUI version of PowerLoom.
REM
REM Usage: powerloom [--c++|--java|--gui|--gui-only] [--help] [{-e|--eval} command] [--batch] ...
REM
REM If the first argument is `--c++' the C++ version of PowerLoom is run
REM (if installed), if it is `--java' the plain Java version is run (if
REM installed); if it is `--gui' the Java version is run and the PowerLoom
REM GUI is started from it connecting to an embedded HTTP server; if it
REM is `--gui-only', the GUI is run in standalone mode and can connect to
REM any available PowerLoom server.  If nothing is specified, the Java
REM version of PowerLoom is run.
REM
REM Example usage:
REM
REM    C:\powerloom> powerloom --batch -e "(demo """equations""" FALSE)"
REM    C:\powerloom> powerloom --gui
REM    C:\powerloom> powerloom --gui-only --host myhost --port 9999


SET POWERLOOM_ROOT=%~d0%~p0

cd %POWERLOOM_ROOT%

REM C++ environment:
SET POWERLOOM_CPP=%POWERLOOM_ROOT%native\cpp\powerloom\powerloom.exe

REM Java environment:
SET JAVA=java
SET JAVA_FLAGS=-Xmx256m
SET JAVA_LIBDIR=%POWERLOOM_ROOT%native\java\lib
SET STELLA_JAR=%JAVA_LIBDIR%\stella.jar
SET POWERLOOM_JAR=%JAVA_LIBDIR%\powerloom.jar
SET POWERLOOM_SERVER_JAR=%JAVA_LIBDIR%\powerloom-server.jar
SET POWERLOOM_GUI_JAR=%JAVA_LIBDIR%\powerloom-gui.jar
SET CASTOR_CORE_JAR=%JAVA_LIBDIR%\castor-core.jar
SET CASTOR_JAR=%JAVA_LIBDIR%\castor.jar
SET LOGGING_JAR=%JAVA_LIBDIR%\commons-logging.jar

SET GUI_CLASSPATH=%POWERLOOM_GUI_JAR%;%CASTOR_CORE_JAR%;%CASTOR_JAR%;%LOGGING_JAR%
SET CLASSPATH=%GUI_CLASSPATH%;%POWERLOOM_SERVER_JAR%;%POWERLOOM_JAR%;%STELLA_JAR%


IF "%1" == "--c++" (
   ECHO Running C++ version of PowerLoom...
   "%POWERLOOM_CPP%" %2 %3 %4 %5 %6 %7 %8 %9
) ELSE (
IF "%1" == "--java" (
   ECHO Running Java version of PowerLoom...
   %JAVA% %JAVA_FLAGS% -classpath "%CLASSPATH%" edu.isi.powerloom.PowerLoom %2 %3 %4 %5 %6 %7 %8 %9
) ELSE (
IF "%1" == "--gui" (
   ECHO Running Java GUI version of PowerLoom...
   %JAVA% %JAVA_FLAGS% -classpath "%CLASSPATH%" edu.isi.powerloom.PowerLoom --gui %2 %3 %4 %5 %6 %7 %8 %9
) ELSE (
IF "%1" == "--gui-only" (
   ECHO Running standalone PowerLoom GUI...
   %JAVA% %JAVA_FLAGS% -classpath "%GUI_CLASSPATH%" edu.isi.powerloom.gui.components.PowerloomApp %2 %3 %4 %5 %6 %7 %8 %9
) ELSE (
   ECHO Running Java version of PowerLoom...
   %JAVA% %JAVA_FLAGS% -classpath "%CLASSPATH%" edu.isi.powerloom.PowerLoom %*
))))
