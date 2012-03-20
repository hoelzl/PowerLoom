# Makefile for STELLA manual(s)

############################ BEGIN LICENSE BLOCK #############################
#                                                                            #
# Version: MPL 1.1/GPL 2.0/LGPL 2.1                                          #
#                                                                            #
# The contents of this file are subject to the Mozilla Public License        #
# Version 1.1 (the "License"); you may not use this file except in           #
# compliance with the License. You may obtain a copy of the License at       #
# http://www.mozilla.org/MPL/                                                #
#                                                                            #
# Software distributed under the License is distributed on an "AS IS" basis, #
# WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   #
# for the specific language governing rights and limitations under the       #
# License.                                                                   #
#                                                                            #
# The Original Code is the STELLA Programming Language.                      #
#                                                                            #
# The Initial Developer of the Original Code is                              #
# UNIVERSITY OF SOUTHERN CALIFORNIA, INFORMATION SCIENCES INSTITUTE          #
# 4676 Admiralty Way, Marina Del Rey, California 90292, U.S.A.               #
#                                                                            #
# Portions created by the Initial Developer are Copyright (C) 1996-2003      #
# the Initial Developer. All Rights Reserved.                                #
#                                                                            #
# Contributor(s):                                                            #
#                                                                            #
# Alternatively, the contents of this file may be used under the terms of    #
# either the GNU General Public License Version 2 or later (the "GPL"), or   #
# the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),   #
# in which case the provisions of the GPL or the LGPL are applicable instead #
# of those above. If you wish to allow use of your version of this file only #
# under the terms of either the GPL or the LGPL, and not to allow others to  #
# use your version of this file under the terms of the MPL, indicate your    #
# decision by deleting the provisions above and replace them with the notice #
# and other provisions required by the GPL or the LGPL. If you do not delete #
# the provisions above, a recipient may use your version of this file under  #
# the terms of any one of the MPL, the GPL or the LGPL.                      #
#                                                                            #
############################# END LICENSE BLOCK ##############################

# Version: Makefile,v 1.10 2003/04/18 21:16:02 hans Exp
#
# Format a TeXINFO manual file and translate it into DVI, PS, INFO, and HTML
#
# Example usage:
#
#    % cd powerloom/logic/doc
#    % make -f ../../stella/doc/Makefile


# Basename of the manual file:
MANUAL = manual

# Formatters:
TEX = tex
PDFTEX = pdftex
TEXINDEX = texindex
DVIPS = dvips
EMACS = xemacs
MAKEINFO = makeinfo
TEXI2HTML = texi2html


all: ${MANUAL}.dvi ${MANUAL}.ps ${MANUAL}.info ${MANUAL}.html ${MANUAL}.pdf

SOURCES = ${MANUAL}.texi *.texi

${MANUAL}.dvi : ${SOURCES}
	${TEX} ${MANUAL}.texi
	${TEX} ${MANUAL}.texi
	${TEXINDEX} ${MANUAL}.fn ${MANUAL}.vr
	${TEX} ${MANUAL}.texi

${MANUAL}.pdf : ${MANUAL}.dvi
	${PDFTEX} ${MANUAL}.texi
	${PDFTEX} ${MANUAL}.texi
	${TEXINDEX} ${MANUAL}.fn ${MANUAL}.vr
	${PDFTEX} ${MANUAL}.texi

${MANUAL}.ps : ${MANUAL}.dvi
	${DVIPS} -o ${MANUAL}.ps ${MANUAL}.dvi

${MANUAL}.info : ${SOURCES}
	${MAKEINFO} ${MANUAL}.texi
	#${EMACS} -batch ${MANUAL}.texi -f texinfo-format-buffer -f save-buffer

${MANUAL}.html : ${SOURCES}
	${TEXI2HTML} -frames -split chapter ${MANUAL}.texi

# After converting to HTML rename ${MANUAL}.html and add frame anchors (don't
# do this anymore, since versions after texi2html-1.51 work differently):
${MANUAL}.html.old : ${SOURCES}
	${TEXI2HTML} ${MANUAL}.texi
	rm -f ${MANUAL}_toc.html.in ${MANUAL}_body.html.in \
	      ${MANUAL}_body.html
	mv ${MANUAL}_toc.html ${MANUAL}_toc.html.in
	mv ${MANUAL}.html ${MANUAL}_body.html.in
	sed -e 's/${MANUAL}.html/${MANUAL}_body.html/g' \
	    -e 's/${MANUAL}_body.html[^"]*"/& target="${MANUAL}"/g' \
	    ${MANUAL}_toc.html.in > ${MANUAL}_toc.html
	sed -e 's/${MANUAL}.html/${MANUAL}_body.html/g' \
	    -e 's/${MANUAL}_toc.html[^"]*"/& target="toc"/g' \
	    ${MANUAL}_body.html.in > ${MANUAL}_body.html
	cp ${MANUAL}.html.in ${MANUAL}.html

#
# Cleaning up
#

.PHONY: clean extraclean

clean extraclean:
	rm -f *.aux *.cp* *.fn* *.ilg *.ind *.ky* *.log \
	      *.pg* *.toc *.tp* *.vr* *~ .*~ \#* .\#*
