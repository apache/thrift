%-----------------------------------------------------------------------------
%
%               LaTeX Class/Style File
%
% Name:         sigplanconf.cls
% Purpose:      A LaTeX 2e class file for SIGPLAN conference proceedings.
%               This class file supercedes acm_proc_article-sp,
%               sig-alternate, and sigplan-proc.
%
% Author:       Paul C. Anagnostopoulos
%               Windfall Software
%               978 371-2316
%               paul@windfall.com
%
% Created:      12 September 2004
%
% Revisions:    See end of file.
%
%-----------------------------------------------------------------------------


\NeedsTeXFormat{LaTeX2e}[1995/12/01]
\ProvidesClass{sigplanconf}[2007/03/13 v1.5 ACM SIGPLAN Proceedings]

% The following few pages contain LaTeX programming extensions adapted
% from the ZzTeX macro package.

%                       Token Hackery
%                       ----- -------


\def \@expandaftertwice {\expandafter\expandafter\expandafter}
\def \@expandafterthrice {\expandafter\expandafter\expandafter\expandafter
                          \expandafter\expandafter\expandafter}

% This macro discards the next token.

\def \@discardtok #1{}%                                  token

% This macro removes the `pt' following a dimension.

{\catcode `\p = 12 \catcode `\t = 12

\gdef \@remover #1pt{#1}

} % \catcode

% This macro extracts the contents of a macro and returns it as plain text.
% Usage: \expandafter\@defof \meaning\macro\@mark

\def \@defof #1:->#2\@mark{#2}

%                       Control Sequence Names
%                       ------- -------- -----


\def \@name #1{%                                        {\tokens}
  \csname \expandafter\@discardtok \string#1\endcsname}

\def \@withname #1#2{%                                  {\command}{\tokens}
  \expandafter#1\csname \expandafter\@discardtok \string#2\endcsname}

%                       Flags (Booleans)
%                       ----- ----------

% The boolean literals \@true and \@false are appropriate for use with
% the \if command, which tests the codes of the next two characters.

\def \@true {TT}
\def \@false {FL}

\def \@setflag #1=#2{\edef #1{#2}}%              \flag = boolean

%                       IF and Predicates
%                       -- --- ----------

% A "predicate" is a macro that returns \@true or \@false as its value.
% Such values are suitable for use with the \if conditional.  For example:
%
%   \if \@oddp{\x} <then-clause> \else <else-clause> \fi

% A predicate can be used with \@setflag as follows:
%
%   \@setflag \flag = {<predicate>}

% Here are the predicates for TeX's repertoire of conditional
% commands.  These might be more appropriately interspersed with
% other definitions in this module, but what the heck.
% Some additional "obvious" predicates are defined.

\def \@eqlp   #1#2{\ifnum #1 = #2\@true \else \@false \fi}
\def \@neqlp  #1#2{\ifnum #1 = #2\@false \else \@true \fi}
\def \@lssp   #1#2{\ifnum #1 < #2\@true \else \@false \fi}
\def \@gtrp   #1#2{\ifnum #1 > #2\@true \else \@false \fi}
\def \@zerop  #1{\ifnum #1 = 0\@true \else \@false \fi}
\def \@onep   #1{\ifnum #1 = 1\@true \else \@false \fi}
\def \@posp   #1{\ifnum #1 > 0\@true \else \@false \fi}
\def \@negp   #1{\ifnum #1 < 0\@true \else \@false \fi}
\def \@oddp   #1{\ifodd #1\@true \else \@false \fi}
\def \@evenp  #1{\ifodd #1\@false \else \@true \fi}
\def \@rangep #1#2#3{\if \@orp{\@lssp{#1}{#2}}{\@gtrp{#1}{#3}}\@false \else
                                                          \@true \fi}
\def \@tensp  #1{\@rangep{#1}{10}{19}}

\def \@dimeqlp   #1#2{\ifdim #1 = #2\@true \else \@false \fi}
\def \@dimneqlp  #1#2{\ifdim #1 = #2\@false \else \@true \fi}
\def \@dimlssp   #1#2{\ifdim #1 < #2\@true \else \@false \fi}
\def \@dimgtrp   #1#2{\ifdim #1 > #2\@true \else \@false \fi}
\def \@dimzerop  #1{\ifdim #1 = 0pt\@true \else \@false \fi}
\def \@dimposp   #1{\ifdim #1 > 0pt\@true \else \@false \fi}
\def \@dimnegp   #1{\ifdim #1 < 0pt\@true \else \@false \fi}

\def \@vmodep     {\ifvmode \@true \else \@false \fi}
\def \@hmodep     {\ifhmode \@true \else \@false \fi}
\def \@mathmodep  {\ifmmode \@true \else \@false \fi}
\def \@textmodep  {\ifmmode \@false \else \@true \fi}
\def \@innermodep {\ifinner \@true \else \@false \fi}

\long\def \@codeeqlp #1#2{\if #1#2\@true \else \@false \fi}

\long\def \@cateqlp #1#2{\ifcat #1#2\@true \else \@false \fi}

\long\def \@tokeqlp  #1#2{\ifx #1#2\@true \else \@false \fi}
\long\def \@xtokeqlp #1#2{\expandafter\ifx #1#2\@true \else \@false \fi}

\long\def \@definedp #1{%
  \expandafter\ifx \csname \expandafter\@discardtok \string#1\endcsname
                   \relax \@false \else \@true \fi}

\long\def \@undefinedp #1{%
  \expandafter\ifx \csname \expandafter\@discardtok \string#1\endcsname
                   \relax \@true \else \@false \fi}

\def \@emptydefp #1{\ifx #1\@empty \@true \else \@false \fi}%       {\name}

\let \@emptylistp = \@emptydefp

\long\def \@emptyargp #1{%                               {#n}
  \@empargp #1\@empargq\@mark}
\long\def \@empargp #1#2\@mark{%
  \ifx #1\@empargq \@true \else \@false \fi}
\def \@empargq {\@empargq}

\def \@emptytoksp #1{%                                   {\tokenreg}
  \expandafter\@emptoksp \the#1\@mark}

\long\def \@emptoksp #1\@mark{\@emptyargp{#1}}

\def \@voidboxp #1{\ifvoid #1\@true \else \@false \fi}
\def \@hboxp #1{\ifhbox #1\@true \else \@false \fi}
\def \@vboxp #1{\ifvbox #1\@true \else \@false \fi}

\def \@eofp #1{\ifeof #1\@true \else \@false \fi}


% Flags can also be used as predicates, as in:
%
%   \if \flaga <then-clause> \else <else-clause> \fi


% Now here we have predicates for the common logical operators.

\def \@notp #1{\if #1\@false \else \@true \fi}

\def \@andp #1#2{\if #1%
                  \if #2\@true \else \@false \fi
                \else
                  \@false
                \fi}

\def \@orp #1#2{\if #1%
                 \@true
               \else
                 \if #2\@true \else \@false \fi
               \fi}

\def \@xorp #1#2{\if #1%
                  \if #2\@false \else \@true \fi
                \else
                  \if #2\@true \else \@false \fi
                \fi}

%                       Arithmetic
%                       ----------

\def \@increment #1{\advance #1 by 1\relax}%             {\count}

\def \@decrement #1{\advance #1 by -1\relax}%            {\count}

%                       Options
%                       -------


\@setflag \@blockstyle = \@false
\@setflag \@copyrightwanted = \@true
\@setflag \@explicitsize = \@false
\@setflag \@mathtime = \@false
\@setflag \@ninepoint = \@true
\newcount{\@numheaddepth} \@numheaddepth = 3
\@setflag \@onecolumn = \@false
\@setflag \@preprint = \@false
\@setflag \@reprint = \@false
\@setflag \@times = \@false

% Note that all the dangerous article class options are trapped.

\DeclareOption{9pt}{\@setflag \@ninepoint = \@true
                    \@setflag \@explicitsize = \@true}

\DeclareOption{10pt}{\PassOptionsToClass{10pt}{article}%
                     \@setflag \@ninepoint = \@false
                     \@setflag \@explicitsize = \@true}

\DeclareOption{11pt}{\PassOptionsToClass{11pt}{article}%
                     \@setflag \@ninepoint = \@false
                     \@setflag \@explicitsize = \@true}

\DeclareOption{12pt}{\@unsupportedoption{12pt}}

\DeclareOption{a4paper}{\@unsupportedoption{a4paper}}

\DeclareOption{a5paper}{\@unsupportedoption{a5paper}}

\DeclareOption{b5paper}{\@unsupportedoption{b5paper}}

\DeclareOption{blockstyle}{\@setflag \@blockstyle = \@true}

\DeclareOption{cm}{\@setflag \@times = \@false}

\DeclareOption{computermodern}{\@setflag \@times = \@false}

\DeclareOption{executivepaper}{\@unsupportedoption{executivepaper}}

\DeclareOption{indentedstyle}{\@setflag \@blockstyle = \@false}

\DeclareOption{landscape}{\@unsupportedoption{landscape}}

\DeclareOption{legalpaper}{\@unsupportedoption{legalpaper}}

\DeclareOption{letterpaper}{\@unsupportedoption{letterpaper}}

\DeclareOption{mathtime}{\@setflag \@mathtime = \@true}

\DeclareOption{nocopyrightspace}{\@setflag \@copyrightwanted = \@false}

\DeclareOption{notitlepage}{\@unsupportedoption{notitlepage}}

\DeclareOption{numberedpars}{\@numheaddepth = 4}

%%%\DeclareOption{onecolumn}{\@setflag \@onecolumn = \@true}

\DeclareOption{preprint}{\@setflag \@preprint = \@true}

\DeclareOption{reprint}{\@setflag \@reprint = \@true}

\DeclareOption{times}{\@setflag \@times = \@true}

\DeclareOption{titlepage}{\@unsupportedoption{titlepage}}

\DeclareOption{twocolumn}{\@setflag \@onecolumn = \@false}

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}

\ExecuteOptions{9pt,indentedstyle,times}
\@setflag \@explicitsize = \@false
\ProcessOptions

\if \@onecolumn
  \if \@notp{\@explicitsize}%
    \@setflag \@ninepoint = \@false
    \PassOptionsToClass{11pt}{article}%
  \fi
  \PassOptionsToClass{twoside,onecolumn}{article}
\else
  \PassOptionsToClass{twoside,twocolumn}{article}
\fi
\LoadClass{article}

\def \@unsupportedoption #1{%
  \ClassError{proc}{The standard '#1' option is not supported.}}

% This can be used with the 'reprint' option to get the final folios.

\def \setpagenumber #1{%
  \setcounter{page}{#1}}

\AtEndDocument{\label{sigplanconf@finalpage}}

%                       Utilities
%                       ---------


\newcommand{\setvspace}[2]{%
  #1 = #2
  \advance #1 by -1\parskip}

%                       Document Parameters
%                       -------- ----------


% Page:

\setlength{\hoffset}{-1in}
\setlength{\voffset}{-1in}

\setlength{\topmargin}{1in}
\setlength{\headheight}{0pt}
\setlength{\headsep}{0pt}

\if \@onecolumn
  \setlength{\evensidemargin}{.75in}
  \setlength{\oddsidemargin}{.75in}
\else
  \setlength{\evensidemargin}{.75in}
  \setlength{\oddsidemargin}{.75in}
\fi

% Text area:

\newdimen{\standardtextwidth}
\setlength{\standardtextwidth}{42pc}

\if \@onecolumn
  \setlength{\textwidth}{40.5pc}
\else
  \setlength{\textwidth}{\standardtextwidth}
\fi

\setlength{\topskip}{8pt}
\setlength{\columnsep}{2pc}
\setlength{\textheight}{54.5pc}

% Running foot:

\setlength{\footskip}{30pt}

% Paragraphs:

\if \@blockstyle
  \setlength{\parskip}{5pt plus .1pt minus .5pt}
  \setlength{\parindent}{0pt}
\else
  \setlength{\parskip}{0pt}
  \setlength{\parindent}{12pt}
\fi

\setlength{\lineskip}{.5pt}
\setlength{\lineskiplimit}{\lineskip}

\frenchspacing
\pretolerance = 400
\tolerance = \pretolerance
\setlength{\emergencystretch}{5pt}
\clubpenalty = 10000
\widowpenalty = 10000
\setlength{\hfuzz}{.5pt}

% Standard vertical spaces:

\newskip{\standardvspace}
\setvspace{\standardvspace}{5pt plus 1pt minus .5pt}

% Margin paragraphs:

\setlength{\marginparwidth}{36pt}
\setlength{\marginparsep}{2pt}
\setlength{\marginparpush}{8pt}


\setlength{\skip\footins}{8pt plus 3pt minus 1pt}
\setlength{\footnotesep}{9pt}

\renewcommand{\footnoterule}{%
  \hrule width .5\columnwidth height .33pt depth 0pt}

\renewcommand{\@makefntext}[1]{%
  \noindent \@makefnmark \hspace{1pt}#1}

% Floats:

\setcounter{topnumber}{4}
\setcounter{bottomnumber}{1}
\setcounter{totalnumber}{4}

\renewcommand{\fps@figure}{tp}
\renewcommand{\fps@table}{tp}
\renewcommand{\topfraction}{0.90}
\renewcommand{\bottomfraction}{0.30}
\renewcommand{\textfraction}{0.10}
\renewcommand{\floatpagefraction}{0.75}

\setcounter{dbltopnumber}{4}

\renewcommand{\dbltopfraction}{\topfraction}
\renewcommand{\dblfloatpagefraction}{\floatpagefraction}

\setlength{\floatsep}{18pt plus 4pt minus 2pt}
\setlength{\textfloatsep}{18pt plus 4pt minus 3pt}
\setlength{\intextsep}{10pt plus 4pt minus 3pt}

\setlength{\dblfloatsep}{18pt plus 4pt minus 2pt}
\setlength{\dbltextfloatsep}{20pt plus 4pt minus 3pt}

% Miscellaneous:

\errorcontextlines = 5

%                       Fonts
%                       -----


\if \@times
  \renewcommand{\rmdefault}{ptm}%
  \if \@mathtime
    \usepackage[mtbold,noTS1]{mathtime}%
  \else
%%%    \usepackage{mathptm}%
  \fi
\else
  \relax
\fi

\if \@ninepoint

\renewcommand{\normalsize}{%
  \@setfontsize{\normalsize}{9pt}{10pt}%
  \setlength{\abovedisplayskip}{5pt plus 1pt minus .5pt}%
  \setlength{\belowdisplayskip}{\abovedisplayskip}%
  \setlength{\abovedisplayshortskip}{3pt plus 1pt minus 2pt}%
  \setlength{\belowdisplayshortskip}{\abovedisplayshortskip}}

\renewcommand{\tiny}{\@setfontsize{\tiny}{5pt}{6pt}}

\renewcommand{\scriptsize}{\@setfontsize{\scriptsize}{7pt}{8pt}}

\renewcommand{\small}{%
  \@setfontsize{\small}{8pt}{9pt}%
  \setlength{\abovedisplayskip}{4pt plus 1pt minus 1pt}%
  \setlength{\belowdisplayskip}{\abovedisplayskip}%
  \setlength{\abovedisplayshortskip}{2pt plus 1pt}%
  \setlength{\belowdisplayshortskip}{\abovedisplayshortskip}}

\renewcommand{\footnotesize}{%
  \@setfontsize{\footnotesize}{8pt}{9pt}%
  \setlength{\abovedisplayskip}{4pt plus 1pt minus .5pt}%
  \setlength{\belowdisplayskip}{\abovedisplayskip}%
  \setlength{\abovedisplayshortskip}{2pt plus 1pt}%
  \setlength{\belowdisplayshortskip}{\abovedisplayshortskip}}

\renewcommand{\large}{\@setfontsize{\large}{11pt}{13pt}}

\renewcommand{\Large}{\@setfontsize{\Large}{14pt}{18pt}}

\renewcommand{\LARGE}{\@setfontsize{\LARGE}{18pt}{20pt}}

\renewcommand{\huge}{\@setfontsize{\huge}{20pt}{25pt}}

\renewcommand{\Huge}{\@setfontsize{\Huge}{25pt}{30pt}}

\fi

%                       Abstract
%                       --------


\renewenvironment{abstract}{%
  \section*{Abstract}%
  \normalsize}{%
  }

%                       Bibliography
%                       ------------


\renewenvironment{thebibliography}[1]
     {\section*{\refname
        \@mkboth{\MakeUppercase\refname}{\MakeUppercase\refname}}%
      \list{\@biblabel{\@arabic\c@enumiv}}%
           {\settowidth\labelwidth{\@biblabel{#1}}%
            \leftmargin\labelwidth
            \advance\leftmargin\labelsep
            \@openbib@code
            \usecounter{enumiv}%
            \let\p@enumiv\@empty
            \renewcommand\theenumiv{\@arabic\c@enumiv}}%
      \small
      \softraggedright%%%\sloppy
      \clubpenalty4000
      \@clubpenalty \clubpenalty
      \widowpenalty4000%
      \sfcode`\.\@m}
     {\def\@noitemerr
       {\@latex@warning{Empty `thebibliography' environment}}%
      \endlist}

%                       Categories
%                       ----------


\@setflag \@firstcategory = \@true

\newcommand{\category}[3]{%
  \if \@firstcategory
    \paragraph*{Categories and Subject Descriptors}%
    \@setflag \@firstcategory = \@false
  \else
    \unskip ;\hspace{.75em}%
  \fi
  \@ifnextchar [{\@category{#1}{#2}{#3}}{\@category{#1}{#2}{#3}[]}}

\def \@category #1#2#3[#4]{%
  {\let \and = \relax
   #1 [\textit{#2}]%
   \if \@emptyargp{#4}%
     \if \@notp{\@emptyargp{#3}}: #3\fi
   \else
     :\space
     \if \@notp{\@emptyargp{#3}}#3---\fi
     \textrm{#4}%
   \fi}}

%                       Copyright Notice
%                       --------- ------


\def \ftype@copyrightbox {8}
\def \@toappear {}
\def \@permission {}


\def \@copyrightspace {%
  \@float{copyrightbox}[b]%
  \vbox to 1in{%
    \vfill
    \parbox[b]{20pc}{%
      \scriptsize
      \if \@preprint
        [Copyright notice will appear here
         once 'preprint' option is removed.]\par
      \else
        \@toappear
      \fi
      \if \@reprint
        \noindent Reprinted from \@conferencename,
        \@proceedings,
        \@conferenceinfo,
        pp.~\number\thepage--\pageref{sigplanconf@finalpage}.\par
      \fi}}%
  \end@float}

\long\def \toappear #1{%
  \def \@toappear {#1}}

\toappear{%
  \noindent \@permission \par
  \vspace{2pt}
  \noindent \textsl{\@conferencename}\quad \@conferenceinfo \par
  \noindent Copyright \copyright\ \@copyrightyear\ ACM \@copyrightdata
    \dots \$5.00\par}

\newcommand{\permission}[1]{%
  \gdef \@permission {#1}}

\permission{%
  Permission to make digital or hard copies of all or
  part of this work for personal or classroom use is granted without
  fee provided that copies are not made or distributed for profit or
  commercial advantage and that copies bear this notice and the full
  citation on the first page.  To copy otherwise, to republish, to
  post on servers or to redistribute to lists, requires prior specific
  permission and/or a fee.}

% Here we have some alternate permission statements and copyright lines:

\newcommand{\ACMCanadapermission}{%
  \permission{%
    Copyright \@copyrightyear\ Association for Computing Machinery.
    ACM acknowledges that
    this contribution was authored or co-authored by an affiliate of the
    National Research Council of Canada (NRC).
    As such, the Crown in Right of
    Canada retains an equal interest in the copyright, however granting
    nonexclusive, royalty-free right to publish or reproduce this article,
    or to allow others to do so, provided that clear attribution
    is also given to the authors and the NRC.}}

\newcommand{\ACMUSpermission}{%
  \permission{%
    Copyright \@copyrightyear\ Association for
    Computing Machinery. ACM acknowledges that
    this contribution was authored or co-authored
    by a contractor or affiliate
    of the U.S. Government. As such, the Government retains a nonexclusive,
    royalty-free right to publish or reproduce this article,
    or to allow others to do so, for Government purposes only.}}

\newcommand{\authorpermission}{%
  \permission{%
    Copyright is held by the author/owner(s).}
  \toappear{%
    \noindent \@permission \par
    \vspace{2pt}
    \noindent \textsl{\@conferencename}\quad \@conferenceinfo \par
    ACM \@copyrightdata.}}

\newcommand{\Sunpermission}{%
  \permission{%
    Copyright is held by Sun Microsystems, Inc.}%
  \toappear{%
    \noindent \@permission \par
    \vspace{2pt}
    \noindent \textsl{\@conferencename}\quad \@conferenceinfo \par
    ACM \@copyrightdata.}}

\newcommand{\USpublicpermission}{%
  \permission{%
    This paper is authored by an employee(s) of the United States
    Government and is in the public domain.}%
  \toappear{%
    \noindent \@permission \par
    \vspace{2pt}
    \noindent \textsl{\@conferencename}\quad \@conferenceinfo \par
    ACM \@copyrightdata.}}

%                       Enunciations
%                       ------------


\def \@begintheorem #1#2{%                      {name}{number}
  \trivlist
  \item[\hskip \labelsep \textsc{#1 #2.}]%
  \itshape\selectfont
  \ignorespaces}

\def \@opargbegintheorem #1#2#3{%               {name}{number}{title}
  \trivlist
  \item[%
    \hskip\labelsep \textsc{#1\ #2}%
    \if \@notp{\@emptyargp{#3}}\nut (#3).\fi]%
  \itshape\selectfont
  \ignorespaces}

%                       Figures
%                       -------


\@setflag \@caprule = \@true

\long\def \@makecaption #1#2{%
  \addvspace{4pt}
  \if \@caprule
    \hrule width \hsize height .33pt
    \vspace{4pt}
  \fi
  \setbox \@tempboxa = \hbox{\@setfigurenumber{#1.}\nut #2}%
  \if \@dimgtrp{\wd\@tempboxa}{\hsize}%
    \noindent \@setfigurenumber{#1.}\nut #2\par
  \else
    \centerline{\box\@tempboxa}%
  \fi}

\newcommand{\nocaptionrule}{%
  \@setflag \@caprule = \@false}

\def \@setfigurenumber #1{%
  {\rmfamily \bfseries \selectfont #1}}

%                       Hierarchy
%                       ---------


\setcounter{secnumdepth}{\@numheaddepth}

\newskip{\@sectionaboveskip}
\setvspace{\@sectionaboveskip}{10pt plus 3pt minus 2pt}

\newskip{\@sectionbelowskip}
\if \@blockstyle
  \setlength{\@sectionbelowskip}{0.1pt}%
\else
  \setlength{\@sectionbelowskip}{4pt}%
\fi

\renewcommand{\section}{%
  \@startsection
    {section}%
    {1}%
    {0pt}%
    {-\@sectionaboveskip}%
    {\@sectionbelowskip}%
    {\large \bfseries \raggedright}}

\newskip{\@subsectionaboveskip}
\setvspace{\@subsectionaboveskip}{8pt plus 2pt minus 2pt}

\newskip{\@subsectionbelowskip}
\if \@blockstyle
  \setlength{\@subsectionbelowskip}{0.1pt}%
\else
  \setlength{\@subsectionbelowskip}{4pt}%
\fi

\renewcommand{\subsection}{%
  \@startsection%
    {subsection}%
    {2}%
    {0pt}%
    {-\@subsectionaboveskip}%
    {\@subsectionbelowskip}%
    {\normalsize \bfseries \raggedright}}

\renewcommand{\subsubsection}{%
  \@startsection%
    {subsubsection}%
    {3}%
    {0pt}%
    {-\@subsectionaboveskip}
    {\@subsectionbelowskip}%
    {\normalsize \bfseries \raggedright}}

\newskip{\@paragraphaboveskip}
\setvspace{\@paragraphaboveskip}{6pt plus 2pt minus 2pt}

\renewcommand{\paragraph}{%
  \@startsection%
    {paragraph}%
    {4}%
    {0pt}%
    {\@paragraphaboveskip}
    {-1em}%
    {\normalsize \bfseries \if \@times \itshape \fi}}

\renewcommand{\subparagraph}{%
  \@startsection%
    {subparagraph}%
    {4}%
    {0pt}%
    {\@paragraphaboveskip}
    {-1em}%
    {\normalsize \itshape}}

% Standard headings:

\newcommand{\acks}{\section*{Acknowledgments}}

\newcommand{\keywords}{\paragraph*{Keywords}}

\newcommand{\terms}{\paragraph*{General Terms}}

%                       Identification
%                       --------------


\def \@conferencename {}
\def \@conferenceinfo {}
\def \@copyrightyear {}
\def \@copyrightdata {[to be supplied]}
\def \@proceedings {[Unknown Proceedings]}


\newcommand{\conferenceinfo}[2]{%
  \gdef \@conferencename {#1}%
  \gdef \@conferenceinfo {#2}}

\newcommand{\copyrightyear}[1]{%
  \gdef \@copyrightyear {#1}}

\let \CopyrightYear = \copyrightyear

\newcommand{\copyrightdata}[1]{%
  \gdef \@copyrightdata {#1}}

\let \crdata = \copyrightdata

\newcommand{\proceedings}[1]{%
  \gdef \@proceedings {#1}}

%                       Lists
%                       -----


\setlength{\leftmargini}{13pt}
\setlength\leftmarginii{13pt}
\setlength\leftmarginiii{13pt}
\setlength\leftmarginiv{13pt}
\setlength{\labelsep}{3.5pt}

\setlength{\topsep}{\standardvspace}
\if \@blockstyle
  \setlength{\itemsep}{1pt}
  \setlength{\parsep}{3pt}
\else
  \setlength{\itemsep}{1pt}
  \setlength{\parsep}{3pt}
\fi

\renewcommand{\labelitemi}{{\small \centeroncapheight{\textbullet}}}
\renewcommand{\labelitemii}{\centeroncapheight{\rule{2.5pt}{2.5pt}}}
\renewcommand{\labelitemiii}{$-$}
\renewcommand{\labelitemiv}{{\Large \textperiodcentered}}

\renewcommand{\@listi}{%
  \leftmargin = \leftmargini
  \listparindent = 0pt}
%%%  \itemsep = 1pt
%%%  \parsep = 3pt}
%%%  \listparindent = \parindent}

\let \@listI = \@listi

\renewcommand{\@listii}{%
  \leftmargin = \leftmarginii
  \topsep = 1pt
  \labelwidth = \leftmarginii
  \advance \labelwidth by -\labelsep
  \listparindent = \parindent}

\renewcommand{\@listiii}{%
  \leftmargin = \leftmarginiii
  \labelwidth = \leftmarginiii
  \advance \labelwidth by -\labelsep
  \listparindent = \parindent}

\renewcommand{\@listiv}{%
  \leftmargin = \leftmarginiv
  \labelwidth = \leftmarginiv
  \advance \labelwidth by -\labelsep
  \listparindent = \parindent}

%                       Mathematics
%                       -----------


\def \theequation {\arabic{equation}}

%                       Miscellaneous
%                       -------------


\newcommand{\balancecolumns}{%
  \vfill\eject
  \global\@colht = \textheight
  \global\ht\@cclv = \textheight}

\newcommand{\nut}{\hspace{.5em}}

\newcommand{\softraggedright}{%
  \let \\ = \@centercr
  \leftskip = 0pt
  \rightskip = 0pt plus 10pt}

%                       Program Code
%                       ------- ----


\newcommand{\mono}[1]{%
  {\@tempdima = \fontdimen2\font
   \texttt{\spaceskip = 1.1\@tempdima #1}}}

%                       Running Heads and Feet
%                       ------- ----- --- ----


\def \@preprintfooter {}

\newcommand{\preprintfooter}[1]{%
  \gdef \@preprintfooter {#1}}

\if \@preprint

\def \ps@plain {%
  \let \@mkboth = \@gobbletwo
  \let \@evenhead = \@empty
  \def \@evenfoot {\scriptsize \textit{\@preprintfooter}\hfil \thepage \hfil
                   \textit{\@formatyear}}%
  \let \@oddhead = \@empty
  \let \@oddfoot = \@evenfoot}

\else\if \@reprint

\def \ps@plain {%
  \let \@mkboth = \@gobbletwo
  \let \@evenhead = \@empty
  \def \@evenfoot {\scriptsize \hfil \thepage \hfil}%
  \let \@oddhead = \@empty
  \let \@oddfoot = \@evenfoot}

\else

\let \ps@plain = \ps@empty
\let \ps@headings = \ps@empty
\let \ps@myheadings = \ps@empty

\fi\fi

\def \@formatyear {%
  \number\year/\number\month/\number\day}

%                       Special Characters
%                       ------- ----------


\DeclareRobustCommand{\euro}{%
  \protect{\rlap{=}}{\sf \kern .1em C}}

%                       Title Page
%                       ----- ----


\@setflag \@addauthorsdone = \@false

\def \@titletext {\@latex@error{No title was provided}{}}
\def \@subtitletext {}

\newcount{\@authorcount}

\newcount{\@titlenotecount}
\newtoks{\@titlenotetext}

\def \@titlebanner {}

\renewcommand{\title}[1]{%
  \gdef \@titletext {#1}}

\newcommand{\subtitle}[1]{%
  \gdef \@subtitletext {#1}}

\newcommand{\authorinfo}[3]{%           {names}{affiliation}{email/URL}
  \global\@increment \@authorcount
  \@withname\gdef {\@authorname\romannumeral\@authorcount}{#1}%
  \@withname\gdef {\@authoraffil\romannumeral\@authorcount}{#2}%
  \@withname\gdef {\@authoremail\romannumeral\@authorcount}{#3}}

\renewcommand{\author}[1]{%
  \@latex@error{The \string\author\space command is obsolete;
                use \string\authorinfo}{}}

\newcommand{\titlebanner}[1]{%
  \gdef \@titlebanner {#1}}

\renewcommand{\maketitle}{%
  \pagestyle{plain}%
  \if \@onecolumn
    {\hsize = \standardtextwidth
     \@maketitle}%
  \else
    \twocolumn[\@maketitle]%
  \fi
  \@placetitlenotes
  \if \@copyrightwanted \@copyrightspace \fi}

\def \@maketitle {%
  \begin{center}
  \@settitlebanner
  \let \thanks = \titlenote
  \noindent \LARGE \bfseries \@titletext \par
  \vskip 6pt
  \noindent \Large \@subtitletext \par
  \vskip 12pt
  \ifcase \@authorcount
    \@latex@error{No authors were specified for this paper}{}\or
    \@titleauthors{i}{}{}\or
    \@titleauthors{i}{ii}{}\or
    \@titleauthors{i}{ii}{iii}\or
    \@titleauthors{i}{ii}{iii}\@titleauthors{iv}{}{}\or
    \@titleauthors{i}{ii}{iii}\@titleauthors{iv}{v}{}\or
    \@titleauthors{i}{ii}{iii}\@titleauthors{iv}{v}{vi}\or
    \@titleauthors{i}{ii}{iii}\@titleauthors{iv}{v}{vi}%
                  \@titleauthors{vii}{}{}\or
    \@titleauthors{i}{ii}{iii}\@titleauthors{iv}{v}{vi}%
                  \@titleauthors{vii}{viii}{}\or
    \@titleauthors{i}{ii}{iii}\@titleauthors{iv}{v}{vi}%
                  \@titleauthors{vii}{viii}{ix}\or
    \@titleauthors{i}{ii}{iii}\@titleauthors{iv}{v}{vi}%
                  \@titleauthors{vii}{viii}{ix}\@titleauthors{x}{}{}\or
    \@titleauthors{i}{ii}{iii}\@titleauthors{iv}{v}{vi}%
                  \@titleauthors{vii}{viii}{ix}\@titleauthors{x}{xi}{}\or
    \@titleauthors{i}{ii}{iii}\@titleauthors{iv}{v}{vi}%
                  \@titleauthors{vii}{viii}{ix}\@titleauthors{x}{xi}{xii}%
  \else
    \@latex@error{Cannot handle more than 12 authors}{}%
  \fi
  \vspace{1.75pc}
  \end{center}}

\def \@settitlebanner {%
  \if \@andp{\@preprint}{\@notp{\@emptydefp{\@titlebanner}}}%
    \vbox to 0pt{%
      \vskip -32pt
      \noindent \textbf{\@titlebanner}\par
      \vss}%
    \nointerlineskip
  \fi}

\def \@titleauthors #1#2#3{%
  \if \@andp{\@emptyargp{#2}}{\@emptyargp{#3}}%
    \noindent \@setauthor{40pc}{#1}{\@false}\par
  \else\if \@emptyargp{#3}%
    \noindent \@setauthor{17pc}{#1}{\@false}\hspace{3pc}%
              \@setauthor{17pc}{#2}{\@false}\par
  \else
    \noindent \@setauthor{12.5pc}{#1}{\@false}\hspace{2pc}%
              \@setauthor{12.5pc}{#2}{\@false}\hspace{2pc}%
              \@setauthor{12.5pc}{#3}{\@true}\par
    \relax
  \fi\fi
  \vspace{20pt}}

\def \@setauthor #1#2#3{%                       {width}{text}{unused}
  \vtop{%
    \def \and {%
      \hspace{16pt}}
    \hsize = #1
    \normalfont
    \centering
    \large \@name{\@authorname#2}\par
    \vspace{5pt}
    \normalsize \@name{\@authoraffil#2}\par
    \vspace{4pt}
    \normalsize{\@name{\@authoremail#2}}\par}}

\def \@maybetitlenote #1{%
  \if \@andp{#1}{\@gtrp{\@authorcount}{3}}%
    \titlenote{See page~\pageref{@addauthors} for additional authors.}%
  \fi}

\newtoks{\@fnmark}

\newcommand{\titlenote}[1]{%
  \global\@increment \@titlenotecount
  \ifcase \@titlenotecount \relax \or
    \@fnmark = {\ast}\or
    \@fnmark = {\dagger}\or
    \@fnmark = {\ddagger}\or
    \@fnmark = {\S}\or
    \@fnmark = {\P}\or
    \@fnmark = {\ast\ast}%
  \fi
  \,$^{\the\@fnmark}$%
  \edef \reserved@a {\noexpand\@appendtotext{%
                       \noexpand\@titlefootnote{\the\@fnmark}}}%
  \reserved@a{#1}}

\def \@appendtotext #1#2{%
  \global\@titlenotetext = \expandafter{\the\@titlenotetext #1{#2}}}

\newcount{\@authori}

\iffalse
\def \additionalauthors {%
  \if \@gtrp{\@authorcount}{3}%
    \section{Additional Authors}%
    \label{@addauthors}%
    \noindent
    \@authori = 4
    {\let \\ = ,%
     \loop 
       \textbf{\@name{\@authorname\romannumeral\@authori}},
       \@name{\@authoraffil\romannumeral\@authori},
       email: \@name{\@authoremail\romannumeral\@authori}.%
       \@increment \@authori
     \if \@notp{\@gtrp{\@authori}{\@authorcount}} \repeat}%
    \par
  \fi
  \global\@setflag \@addauthorsdone = \@true}
\fi

\let \addauthorsection = \additionalauthors

\def \@placetitlenotes {
  \the\@titlenotetext}

%                       Utilities
%                       ---------


\newcommand{\centeroncapheight}[1]{%
  {\setbox\@tempboxa = \hbox{#1}%
   \@measurecapheight{\@tempdima}%         % Calculate ht(CAP) - ht(text)
   \advance \@tempdima by -\ht\@tempboxa   %           ------------------
   \divide \@tempdima by 2                 %                   2
   \raise \@tempdima \box\@tempboxa}}

\newbox{\@measbox}

\def \@measurecapheight #1{%                            {\dimen}
  \setbox\@measbox = \hbox{ABCDEFGHIJKLMNOPQRSTUVWXYZ}%
  #1 = \ht\@measbox}

\long\def \@titlefootnote #1#2{%
  \insert\footins{%
    \reset@font\footnotesize
    \interlinepenalty\interfootnotelinepenalty
    \splittopskip\footnotesep
    \splitmaxdepth \dp\strutbox \floatingpenalty \@MM
    \hsize\columnwidth \@parboxrestore
%%%    \protected@edef\@currentlabel{%
%%%       \csname p@footnote\endcsname\@thefnmark}%
    \color@begingroup
      \def \@makefnmark {$^{#1}$}%
      \@makefntext{%
        \rule\z@\footnotesep\ignorespaces#2\@finalstrut\strutbox}%
    \color@endgroup}}

%                       LaTeX Modifications
%                       ----- -------------

\def \@seccntformat #1{%
  \@name{\the#1}%
  \@expandaftertwice\@seccntformata \csname the#1\endcsname.\@mark
  \quad}

\def \@seccntformata #1.#2\@mark{%
  \if \@emptyargp{#2}.\fi}

%                       Revision History
%                       -------- -------


%  Date         Person  Ver.    Change
%  ----         ------  ----    ------

%  2004.09.12   PCA     0.1--5  Preliminary development.

%  2004.11.18   PCA     0.5     Start beta testing.

%  2004.11.19   PCA     0.6     Obsolete \author and replace with
%                               \authorinfo.
%                               Add 'nocopyrightspace' option.
%                               Compress article opener spacing.
%                               Add 'mathtime' option.
%                               Increase text height by 6 points.

%  2004.11.28   PCA     0.7     Add 'cm/computermodern' options.
%                               Change default to Times text.

%  2004.12.14   PCA     0.8     Remove use of mathptm.sty; it cannot
%                               coexist with latexsym or amssymb.

%  2005.01.20   PCA     0.9     Rename class file to sigplanconf.cls.

%  2005.03.05   PCA     0.91    Change default copyright data.

%  2005.03.06   PCA     0.92    Add at-signs to some macro names.

%  2005.03.07   PCA     0.93    The 'onecolumn' option defaults to '11pt',
%                               and it uses the full type width.

%  2005.03.15   PCA     0.94    Add at-signs to more macro names.
%                               Allow margin paragraphs during review.

%  2005.03.22   PCA     0.95    Implement \euro.
%                               Remove proof and newdef environments.

%  2005.05.06   PCA     1.0     Eliminate 'onecolumn' option.
%                               Change footer to small italic and eliminate
%                               left portion if no \preprintfooter.
%                               Eliminate copyright notice if preprint.
%                               Clean up and shrink copyright box.

%  2005.05.30   PCA     1.1     Add alternate permission statements.

%  2005.06.29   PCA     1.1     Publish final first edition of guide.

%  2005.07.14   PCA     1.2     Add \subparagraph.
%                               Use block paragraphs in lists, and adjust
%                               spacing between items and paragraphs.

%  2006.06.22   PCA     1.3     Add 'reprint' option and associated
%                               commands.

%  2006.08.24   PCA     1.4     Fix bug in \maketitle case command.

%  2007.03.13   PCA     1.5     The title banner only display with the
%                               'preprint' option.