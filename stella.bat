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
REM The Original Code is the STELLA Programming Language.                      #
REM                                                                            #
REM The Initial Developer of the Original Code is                              #
REM UNIVERSITY OF SOUTHERN CALIFORNIA, INFORMATION SCIENCES INSTITUTE          #
REM 4676 Admiralty Way, Marina Del Rey, California 90292, U.S.A.               #
REM                                                                            #
REM Portions created by the Initial Developer are Copyright (C) 1996-2006      #
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

REM Version: stella.bat,v 1.1 2006/05/16 06:32:57 hans Exp
REM
REM Run the C++ or Java version of the STELLA programming language
REM and translator in a Windows 2000/XP command prompt window.
REM
REM Usage: stella [c++ | java] [{-e|--eval} STELLA-EXPRESSION [--batch]
REM
REM If the first argument is `c++' the C++ version of STELLA is run
REM (if installed), if it is `java' the Java version is run (if
REM installed); otherwise, if the C++ version is installed it will run
REM that; if that is not installed, it will run the Java version.
REM
REM Example usage:
REM
REM    C:\stella> stella -e "(make-system """hello-world""" :cpp)"

SET STELLA_ROOT=%~d0%~p0

cd %STELLA_ROOT%

REM C++ environment:
SET STELLA_CPP=%STELLA_ROOT%native\cpp\stella\stella.exe

REM Java environment:
SET JAVA=java
SET JAVA_FLAGS=-Xmx256m
SET STELLA_JAR=%STELLA_ROOT%native\java\lib\stella.jar
SET CLASSPATH=%STELLA_JAR%

IF "%1" == "c++" (
   ECHO Running C++ version of STELLA...
   "%STELLA_CPP%" %2 %3 %4 %5 %6 %7 %8 %9
) ELSE (
IF "%1" == "java" (
   ECHO Running Java version of STELLA...
   %JAVA% %JAVA_FLAGS% -classpath "%CLASSPATH%" edu.isi.stella.Stella %2 %3 %4 %5 %6 %7 %8 %9
) ELSE (
IF EXIST "%STELLA_CPP%" (
   ECHO Running C++ version of STELLA...
   "%STELLA_CPP%" %*
) ELSE (
   ECHO Running Java version of STELLA...
   %JAVA% %JAVA_FLAGS% -classpath "%CLASSPATH%" edu.isi.stella.Stella %*
)))
