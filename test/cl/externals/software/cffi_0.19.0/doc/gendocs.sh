#!/bin/sh
# gendocs.sh -- generate a GNU manual in many formats.  This script is
#   mentioned in maintain.texi.  See the help message below for usage details.
# $Id: gendocs.sh,v 1.16 2005/05/15 00:00:08 karl Exp $
# 
# Copyright (C) 2003, 2004, 2005 Free Software Foundation, Inc.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, you can either send email to this
# program's maintainer or write to: The Free Software Foundation,
# Inc.; 51 Franklin Street, Fifth Floor; Boston, MA 02110-1301, USA.
#
# Original author: Mohit Agarwal.
# Send bug reports and any other correspondence to bug-texinfo@gnu.org.

#set -e

prog="`basename \"$0\"`"
srcdir=`pwd`

scripturl="https://github.com/cffi/cffi/blob/master/doc/gendocs.sh"
templateurl="http://savannah.gnu.org/cgi-bin/viewcvs/texinfo/texinfo/util/gendocs_template"

: ${MAKEINFO="makeinfo"}
: ${TEXI2DVI="texi2dvi -t @finalout"}
: ${DVIPS="dvips"}
: ${DOCBOOK2TXT="docbook2txt"}
: ${DOCBOOK2HTML="docbook2html"}
: ${DOCBOOK2PDF="docbook2pdf"}
: ${DOCBOOK2PS="docbook2ps"}
: ${GENDOCS_TEMPLATE_DIR="."}
unset CDPATH

rcs_revision='$Revision: 1.16 $'
rcs_version=`set - $rcs_revision; echo $2`
program=`echo $0 | sed -e 's!.*/!!'`
version="gendocs.sh $rcs_version

Copyright (C) 2005 Free Software Foundation, Inc.
There is NO warranty.  You may redistribute this software
under the terms of the GNU General Public License.
For more information about these matters, see the files named COPYING."

usage="Usage: $prog [OPTION]... PACKAGE MANUAL-TITLE

Generate various output formats from PACKAGE.texinfo (or .texi or .txi) source.
See the GNU Maintainers document for a more extensive discussion:
  http://www.gnu.org/prep/maintain_toc.html

Options:
  -o OUTDIR   write files into OUTDIR, instead of manual/.
  --docbook   convert to DocBook too (xml, txt, html, pdf and ps).
  --html ARG  pass indicated ARG to makeinfo for HTML targets.
  --help      display this help and exit successfully.
  --version   display version information and exit successfully.

Simple example: $prog emacs \"GNU Emacs Manual\"

Typical sequence:
  cd YOURPACKAGESOURCE/doc
  wget \"$scripturl\"
  wget \"$templateurl\"
  $prog YOURMANUAL \"GNU YOURMANUAL - One-line description\"

Output will be in a new subdirectory \"manual\" (by default, use -o OUTDIR
to override).  Move all the new files into your web CVS tree, as
explained in the Web Pages node of maintain.texi.

MANUAL-TITLE is included as part of the HTML <title> of the overall
manual/index.html file.  It should include the name of the package being
documented.  manual/index.html is created by substitution from the file
$GENDOCS_TEMPLATE_DIR/gendocs_template.  (Feel free to modify the
generic template for your own purposes.)

If you have several manuals, you'll need to run this script several
times with different YOURMANUAL values, specifying a different output
directory with -o each time.  Then write (by hand) an overall index.html
with links to them all.

You can set the environment variables MAKEINFO, TEXI2DVI, and DVIPS to
control the programs that get executed, and GENDOCS_TEMPLATE_DIR to
control where the gendocs_template file is looked for.

Email bug reports or enhancement requests to bug-texinfo@gnu.org.
"

calcsize()
{
  size="`ls -ksl $1 | awk '{print $1}'`"
  echo $size
}

outdir=manual
html=
PACKAGE=
MANUAL_TITLE=

while test $# -gt 0; do
  case $1 in
    --help) echo "$usage"; exit 0;;
    --version) echo "$version"; exit 0;;
    -o) shift; outdir=$1;;
    --docbook) docbook=yes;;
    --html) shift; html=$1;;
    -*)
      echo "$0: Unknown or ambiguous option \`$1'." >&2
      echo "$0: Try \`--help' for more information." >&2
      exit 1;;
    *)
      if test -z "$PACKAGE"; then
        PACKAGE=$1
      elif test -z "$MANUAL_TITLE"; then
        MANUAL_TITLE=$1
      else
        echo "$0: extra non-option argument \`$1'." >&2
        exit 1
      fi;;
  esac
  shift
done

if test -s $srcdir/$PACKAGE.texinfo; then
  srcfile=$srcdir/$PACKAGE.texinfo
elif test -s $srcdir/$PACKAGE.texi; then
  srcfile=$srcdir/$PACKAGE.texi
elif test -s $srcdir/$PACKAGE.txi; then
  srcfile=$srcdir/$PACKAGE.txi
else
  echo "$0: cannot find .texinfo or .texi or .txi for $PACKAGE in $srcdir." >&2
  exit 1
fi

if test ! -r $GENDOCS_TEMPLATE_DIR/gendocs_template; then
  echo "$0: cannot read $GENDOCS_TEMPLATE_DIR/gendocs_template." >&2
  echo "$0: it is available from $templateurl." >&2
  exit 1
fi

echo Generating output formats for $srcfile

cmd="${MAKEINFO} -o $PACKAGE.info $srcfile"
echo "Generating info files... ($cmd)"
eval $cmd
install-info $PACKAGE.info dir
mkdir -p $outdir/
tar czf $outdir/$PACKAGE.info.tar.gz $PACKAGE.info*
info_tgz_size="`calcsize $outdir/$PACKAGE.info.tar.gz`"
# do not mv the info files, there's no point in having them available
# separately on the web.

cmd="${TEXI2DVI} $srcfile"
echo "Generating dvi ... ($cmd)"
eval $cmd

# now, before we compress dvi:
echo Generating postscript...
${DVIPS} $PACKAGE -o
gzip -f -9 $PACKAGE.ps
ps_gz_size="`calcsize $PACKAGE.ps.gz`"
mv $PACKAGE.ps.gz $outdir/

# compress/finish dvi:
gzip -f -9 $PACKAGE.dvi
dvi_gz_size="`calcsize $PACKAGE.dvi.gz`"
mv $PACKAGE.dvi.gz $outdir/

cmd="${TEXI2DVI} --pdf $srcfile"
echo "Generating pdf ... ($cmd)"
eval $cmd
pdf_size="`calcsize $PACKAGE.pdf`"
mv $PACKAGE.pdf $outdir/

cmd="${MAKEINFO} -o $PACKAGE.txt --no-split --no-headers $srcfile"
echo "Generating ASCII... ($cmd)"
eval $cmd
ascii_size="`calcsize $PACKAGE.txt`"
gzip -f -9 -c $PACKAGE.txt >$outdir/$PACKAGE.txt.gz
ascii_gz_size="`calcsize $outdir/$PACKAGE.txt.gz`"
mv $PACKAGE.txt $outdir/

# Print a SED expression that will translate references to MANUAL to
# the proper page on gnu.org.  This is a horrible shell hack done
# because \| in sed regexps is a GNU extension.
monognuorg () {
    case "$1" in
	libtool) echo "s!$1.html!http://www.gnu.org/software/$1/manual.html!" ;;
	*) echo "s!$1.html!http://www.gnu.org/software/$1/manual/html_mono/$1.html!" ;;
    esac
}
polygnuorg () {
    case "$1" in
	libtool) echo 's!\.\./'"$1/.*\.html!http://www.gnu.org/software/$1/manual.html!" ;;
	*) echo 's!\.\./'"$1!http://www.gnu.org/software/$1/manual/html_node!" ;;
    esac
}

cmd="${MAKEINFO} --no-split --html -o $PACKAGE.html $html $srcfile"
echo "Generating monolithic html... ($cmd)"
rm -rf $PACKAGE.html  # in case a directory is left over
eval $cmd
sbcl --no-sysinit --no-userinit --load colorize-lisp-examples.lisp $PACKAGE.html
#fix libc/libtool xrefs
sed -e `monognuorg libc` -e `monognuorg libtool` $PACKAGE.html >$outdir/$PACKAGE.html
rm $PACKAGE.html
html_mono_size="`calcsize $outdir/$PACKAGE.html`"
gzip -f -9 -c $outdir/$PACKAGE.html >$outdir/$PACKAGE.html.gz
html_mono_gz_size="`calcsize $outdir/$PACKAGE.html.gz`"

cmd="${MAKEINFO} --html -o $PACKAGE.html $html $srcfile"
echo "Generating html by node... ($cmd)"
eval $cmd
split_html_dir=$PACKAGE.html
sbcl --no-userinit --no-sysinit --load colorize-lisp-examples.lisp "${split_html_dir}"/\*.html
(
  cd ${split_html_dir} || exit 1
  #fix libc xrefs
  for broken_file in *.html; do
      sed -e `polygnuorg libc` -e `polygnuorg libtool` "$broken_file" > "$broken_file".temp
      mv -f "$broken_file".temp "$broken_file"
  done
  tar -czf ../$outdir/${PACKAGE}.html_node.tar.gz -- *.html
)
html_node_tgz_size="`calcsize $outdir/${PACKAGE}.html_node.tar.gz`"
rm -f $outdir/html_node/*.html
mkdir -p $outdir/html_node/
mv ${split_html_dir}/*.html $outdir/html_node/
rmdir ${split_html_dir}

echo Making .tar.gz for sources...
srcfiles=`ls *.texinfo *.texi *.txi *.eps 2>/dev/null`
tar cvzfh $outdir/$PACKAGE.texi.tar.gz $srcfiles
texi_tgz_size="`calcsize $outdir/$PACKAGE.texi.tar.gz`"

if test -n "$docbook"; then
  cmd="${MAKEINFO} -o - --docbook $srcfile > ${srcdir}/$PACKAGE-db.xml"
  echo "Generating docbook XML... $(cmd)"
  eval $cmd
  docbook_xml_size="`calcsize $PACKAGE-db.xml`"
  gzip -f -9 -c $PACKAGE-db.xml >$outdir/$PACKAGE-db.xml.gz
  docbook_xml_gz_size="`calcsize $outdir/$PACKAGE-db.xml.gz`"
  mv $PACKAGE-db.xml $outdir/

  cmd="${DOCBOOK2HTML} -o $split_html_db_dir ${outdir}/$PACKAGE-db.xml"
  echo "Generating docbook HTML... ($cmd)"
  eval $cmd
  split_html_db_dir=html_node_db
  (
    cd ${split_html_db_dir} || exit 1
    tar -czf ../$outdir/${PACKAGE}.html_node_db.tar.gz -- *.html
  )
  html_node_db_tgz_size="`calcsize $outdir/${PACKAGE}.html_node_db.tar.gz`"
  rm -f $outdir/html_node_db/*.html
  mkdir -p $outdir/html_node_db
  mv ${split_html_db_dir}/*.html $outdir/html_node_db/
  rmdir ${split_html_db_dir}

  cmd="${DOCBOOK2TXT} ${outdir}/$PACKAGE-db.xml"
  echo "Generating docbook ASCII... ($cmd)"
  eval $cmd
  docbook_ascii_size="`calcsize $PACKAGE-db.txt`"
  mv $PACKAGE-db.txt $outdir/

  cmd="${DOCBOOK2PS} ${outdir}/$PACKAGE-db.xml"
  echo "Generating docbook PS... $(cmd)"
  eval $cmd
  gzip -f -9 -c $PACKAGE-db.ps >$outdir/$PACKAGE-db.ps.gz
  docbook_ps_gz_size="`calcsize $outdir/$PACKAGE-db.ps.gz`"
  mv $PACKAGE-db.ps $outdir/

  cmd="${DOCBOOK2PDF} ${outdir}/$PACKAGE-db.xml"
  echo "Generating docbook PDF... ($cmd)"
  eval $cmd
  docbook_pdf_size="`calcsize $PACKAGE-db.pdf`"
  mv $PACKAGE-db.pdf $outdir/
fi

echo Writing index file...
curdate="`date '+%B %d, %Y'`"
sed \
   -e "s!%%TITLE%%!$MANUAL_TITLE!g" \
   -e "s!%%DATE%%!$curdate!g" \
   -e "s!%%PACKAGE%%!$PACKAGE!g" \
   -e "s!%%HTML_MONO_SIZE%%!$html_mono_size!g" \
   -e "s!%%HTML_MONO_GZ_SIZE%%!$html_mono_gz_size!g" \
   -e "s!%%HTML_NODE_TGZ_SIZE%%!$html_node_tgz_size!g" \
   -e "s!%%INFO_TGZ_SIZE%%!$info_tgz_size!g" \
   -e "s!%%DVI_GZ_SIZE%%!$dvi_gz_size!g" \
   -e "s!%%PDF_SIZE%%!$pdf_size!g" \
   -e "s!%%PS_GZ_SIZE%%!$ps_gz_size!g" \
   -e "s!%%ASCII_SIZE%%!$ascii_size!g" \
   -e "s!%%ASCII_GZ_SIZE%%!$ascii_gz_size!g" \
   -e "s!%%TEXI_TGZ_SIZE%%!$texi_tgz_size!g" \
   -e "s!%%DOCBOOK_HTML_NODE_TGZ_SIZE%%!$html_node_db_tgz_size!g" \
   -e "s!%%DOCBOOK_ASCII_SIZE%%!$docbook_ascii_size!g" \
   -e "s!%%DOCBOOK_PS_GZ_SIZE%%!$docbook_ps_gz_size!g" \
   -e "s!%%DOCBOOK_PDF_SIZE%%!$docbook_pdf_size!g" \
   -e "s!%%DOCBOOK_XML_SIZE%%!$docbook_xml_size!g" \
   -e "s!%%DOCBOOK_XML_GZ_SIZE%%!$docbook_xml_gz_size!g" \
   -e "s,%%SCRIPTURL%%,$scripturl,g" \
   -e "s!%%SCRIPTNAME%%!$prog!g" \
$GENDOCS_TEMPLATE_DIR/gendocs_template >$outdir/index.html

echo "Done!  See $outdir/ subdirectory for new files."
