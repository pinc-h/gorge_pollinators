%%
%% This is file `CUP-JNL-NMJ.cls',
%% generated with the docstrip utility.
%%
%% The original source files were:
%%
%% classes.dtx  (with options: `CUP-JNL-NMJ')
%%
%% This is a generated file.
%%
%% Copyright 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009
%% The LaTeX3 Project and any individual authors listed elsewhere
%% in this file.
%%
%% This file was generated from file(s) of the LaTeX base system.
%% --------------------------------------------------------------
%%
%% It may be distributed and/or modified under the
%% conditions of the LaTeX Project Public License, either version 1.3c
%% of this license or (at your option) any later version.
%% The latest version of this license is in
%%    http://www.latex-project.org/lppl.txt
%% and version 1.3c or later is part of all distributions of LaTeX
%% version 2005/12/01 or later.
%%
%% This file has the LPPL maintenance status "maintained".
%%
%% This file may only be distributed together with a copy of the LaTeX
%% base system. You may however distribute the LaTeX base system without
%% such generated files.
%%
%% The list of all files belonging to the LaTeX base distribution is
%% given in the file `manifest.txt'. See also `legal.txt' for additional
%% information.
%%
%% The list of derived (unpacked) files belonging to the distribution
%% and covered by LPPL is defined by the unpacking scripts (with
%% extension .ins) which are part of the distribution.
%% \CharacterTable
%%  {Upper-case    \A\B\C\D\E\F\G\H\I\J\K\L\M\N\O\P\Q\R\S\T\U\V\W\X\Y\Z
%%   Lower-case    \a\b\c\d\e\f\g\h\i\j\k\l\m\n\o\p\q\r\s\t\u\v\w\x\y\z
%%   Digits        \0\1\2\3\4\5\6\7\8\9
%%   Exclamation   \!     Double quote  \"     Hash (number) \#
%%   Dollar        \$     Percent       \%     Ampersand     \&
%%   Acute accent  \'     Left paren    \(     Right paren   \)
%%   Asterisk      \*     Plus          \+     Comma         \,
%%   Minus         \-     Point         \.     Solidus       \/
%%   Colon         \:     Semicolon     \;     Less than     \<
%%   Equals        \=     Greater than  \>     Question mark \?
%%   Commercial at \@     Left bracket  \[     Backslash     \\
%%   Right bracket \]     Circumflex    \^     Underscore    \_
%%   Grave accent  \`     Left brace    \{     Vertical bar  \|
%%   Right brace   \}     Tilde         \~}
\NeedsTeXFormat{LaTeX2e}[1995/12/01]
\ProvidesClass{CUP-JNL-NMJ}
              [2020/05/04 v0.1: An authoring template for CUP NMJ articles]

\newif\if@restonecol
\newif\if@titlepage   \@titlepagefalse
\newif\if@cropmarkson \@cropmarksonfalse

%% Template Options
\DeclareOption{a4paper}{\PassOptionsToPackage{a4}{crop}}
\DeclareOption{a3paper}{\PassOptionsToClass{a3paper}{article}\PassOptionsToPackage{a3}{crop}}
\DeclareOption{centre}{\PassOptionsToPackage{center}{crop}}
\DeclareOption{crop}{\PassOptionsToPackage{frame}{crop}\global\@cropmarksontrue}
\DeclareOption{nocrop}{\PassOptionsToPackage{off}{crop}\global\@cropmarksontrue}
\DeclareOption{info}{\PassOptionsToPackage{info}{crop}}
\DeclareOption{noinfo}{\PassOptionsToPackage{noinfo}{crop}}

%% Classfile Options
\DeclareOption{oneside}{\@twosidefalse \@mparswitchfalse}
\DeclareOption{twoside}{\@twosidetrue  \@mparswitchtrue}
\DeclareOption{draft}{\setlength\overfullrule{5pt}}
\DeclareOption{titlepage}{\@titlepagetrue}
\DeclareOption{notitlepage}{\@titlepagefalse}
\DeclareOption{onecolumn}{\@twocolumnfalse}
\DeclareOption{twocolumn}{\@twocolumntrue}
\DeclareOption{openbib}{\let\if@openbib\iftrue}

\ExecuteOptions{twosidecrop,crop,centre,info,croppage}
\ProcessOptions
\LoadClass[twoside,fleqn]{article}

%% General Packages Used
\RequirePackage{url}
\RequirePackage{soul}

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% Fonts & Sizes %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
\DeclareOldFontCommand{\rm}{\normalfont\rmfamily}{\mathrm}
\DeclareOldFontCommand{\sf}{\normalfont\sffamily}{\mathsf}
\DeclareOldFontCommand{\tt}{\normalfont\ttfamily}{\mathtt}
\DeclareOldFontCommand{\bf}{\normalfont\bfseries}{\mathbf}
\DeclareOldFontCommand{\it}{\normalfont\itshape}{\mathit}
\DeclareOldFontCommand{\sl}{\normalfont\slshape}{\@nomath\sl}
\DeclareOldFontCommand{\sc}{\normalfont\scshape}{\@nomath\sc}
\DeclareRobustCommand*\cal{\@fontswitch\relax\mathcal}
\DeclareRobustCommand*\mit{\@fontswitch\relax\mathnormal}

\renewcommand\normalsize{%
   \@setfontsize\normalsize{10.91bp}{14pt}
   \abovedisplayskip 7\p@ \@plus2\p@ \@minus0\p@
   \abovedisplayshortskip 6\p@ \@plus3\p@
   \belowdisplayshortskip 6\p@ \@plus3\p@ \@minus3\p@
   \belowdisplayskip \abovedisplayskip
   \let\@listi\@listI}
\normalsize
\newcommand\medsize{%
   \@setfontsize\small\@xipt{13}%
   \abovedisplayskip 5\p@ \@plus3\p@ \@minus4\p@
   \abovedisplayshortskip \z@ \@plus2\p@
   \belowdisplayshortskip 3\p@ \@plus2\p@ \@minus2\p@
   \def\@listi{\leftmargin\leftmargini
               \topsep 4\p@ \@plus2\p@ \@minus2\p@
               \parsep 2\p@ \@plus\p@ \@minus\p@
               \itemsep \parsep}%
   \belowdisplayskip \abovedisplayskip}
\renewcommand\small{%
   \@setfontsize\small{8bp}{10bp}%
   \abovedisplayskip 5.5\p@ \@plus3\p@ \@minus4\p@
   \abovedisplayshortskip \z@ \@plus2\p@
   \belowdisplayshortskip 5.5\p@ \@plus2\p@ \@minus2\p@
   \def\@listi{\leftmargin\leftmargini
               \topsep 4\p@ \@plus2\p@ \@minus2\p@
               \parsep 2\p@ \@plus\p@ \@minus\p@
               \itemsep \parsep}%
   \belowdisplayskip \abovedisplayskip
}
\renewcommand\footnotesize{%
   \@setfontsize\footnotesize{9bp}{10.75bp}%
   \abovedisplayskip 5\p@ \@plus2\p@ \@minus4\p@
   \abovedisplayshortskip \z@ \@plus\p@
   \belowdisplayshortskip 3\p@ \@plus\p@ \@minus2\p@
   \def\@listi{\leftmargin\leftmargini
               \topsep 3\p@ \@plus\p@ \@minus\p@
               \parsep 2\p@ \@plus\p@ \@minus\p@
               \itemsep \parsep}%
   \belowdisplayskip \abovedisplayskip
}
%
\renewcommand\scriptsize{\@setfontsize\scriptsize\@ixpt\@ixpt}
\newcommand\scrisize{\@setfontsize\scrisize{9.3}{9}}
\renewcommand\tiny{\@setfontsize\tiny\@vpt\@vipt}
\renewcommand\large{\@setfontsize\large{12}{14}}
\newcommand\larg{\@setfontsize\larg{11}{13}}
\renewcommand\Large{\@setfontsize\Large{16}{18}}
\renewcommand\LARGE{\@setfontsize\LARGE\@xviipt{22}}
\renewcommand\huge{\@setfontsize\huge\@xxpt{25}}
\renewcommand\Huge{\@setfontsize\Huge\@xxvpt{30}}
%
\DeclareMathSizes{\@ixpt}{\@ixpt}{7}{5}
\DeclareMathSizes{\@xpt}{\@xpt}{7}{5}
\DeclareMathSizes{\@xipt}{\@xipt}{7}{5}

\DeclareRobustCommand*\textsubscript[1]{%
  \@textsubscript{\selectfont#1}}%
\def\@textsubscript#1{%
  {\m@th\ensuremath{_{\mbox{\fontsize\sf@size\z@#1}}}}}

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% Layout Settings %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

\RequirePackage{geometry}

%% Regular Pages (for Opening page - redefined)
\geometry{%
    paperwidth=210truemm,
    paperheight=280truemm,
    top={86bp},
    headheight={7pt},
    headsep={16bp},
    textwidth={152mm},
    textheight={644pt},
    marginparsep=0mm,
    inner=65.25bp,
    marginparwidth=0mm,
    bindingoffset=6mm,
    footskip=0mm}
%

\setlength{\marginparsep}{0mm}%
\setlength{\marginparwidth}{0mm}%

\setlength\footskip{19pt}
\setlength\maxdepth{0pt}

\setlength{\footnotesep}{0\p@}
\setlength{\skip\footins}{14\p@ \@plus 2 \p@} % \@minus 4\p@

\setlength\parindent{12pt}
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% Pagination Settings %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Line spacing
\setlength\lineskip{1\p@}
\setlength\normallineskip{1\p@}
\renewcommand\baselinestretch{}
\parskip=0pt

% Page break penalties
\@lowpenalty   51
\@medpenalty  151
\@highpenalty 301

% Disallow widows and orphans
\clubpenalty 10000
\widowpenalty 10000

% Disable page breaks before equations, allow pagebreaks after
% equations and discourage widow lines before equations.
\displaywidowpenalty 100
\predisplaypenalty   10000
\postdisplaypenalty  0

% Set these global demerits
\doublehyphendemerits 1000000   % corresponds to badness 800
\finalhyphendemerits  1000000  % corresponds to badness 1000

% Allow loose lines rather than overfull lines
\vbadness=9999
\tolerance=9999

% Allow breaking the page in the middle of a paragraph
\interlinepenalty 0

% Disallow breaking the page after a hyphenated line
\brokenpenalty 10000

% Hyphenation; don't split words into less than three characters
\lefthyphenmin=3
\righthyphenmin=3

% Float placement parameters

% The total number of floats that can be allowed on a page.
\setcounter{totalnumber}{10}

% The maximum number of floats at the top and bottom of a page.
\setcounter{topnumber}{5}
\setcounter{bottomnumber}{5}

% The maximum part of the top or bottom of a text page that can be
% occupied by floats. This is set so that at least four lines of text
% fit on the page.
\renewcommand\topfraction{.921}
\renewcommand\bottomfraction{.921}

% The minimum amount of a text page that must be occupied by text.
% This should accomodate four lines of text.
\renewcommand\textfraction{.13}

% The minimum amount of a float page that must be occupied by floats.
\renewcommand\floatpagefraction{.887}

% The same parameters repeated for double column output
\renewcommand\dbltopfraction{.88}
\renewcommand\dblfloatpagefraction{.88}

% Space between floats
\setlength\floatsep    {18\p@ \@plus 2\p@ \@minus 2\p@}

% Space between floats and text
\setlength\textfloatsep{19\p@ \@plus 2\p@ \@minus 4\p@}

% Space above and below an inline figure
\setlength\intextsep   {18\p@ \@plus 2\p@ \@minus 2\p@}

% For double column floats
\setlength\dblfloatsep    {20\p@ \@plus 2\p@ \@minus 2\p@}
\setlength\dbltextfloatsep{18\p@ \@plus 2\p@ \@minus 4\p@}

\hyphenation{Figure Figures Table Tables Equation Equations Section Sections Appendix Theorem Lemma}

%
%%%%%%%%%%%%%%%%%%%%%%%%%%% Math Settings %%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% For above/below spacing  
\def\eqnarray{%
   \stepcounter{equation}%
   \def\@currentlabel{\p@equation\theequation}%
   \global\@eqnswtrue
   \m@th
   \global\@eqcnt\z@
   \tabskip\@centering
   \let\\\@eqncr
   $$\everycr{}\halign to\displaywidth\bgroup
       \hskip\@centering$\displaystyle\tabskip\z@skip{##}$\@eqnsel
      &\global\@eqcnt\@ne\hskip \tw@\arraycolsep \hfil${##}$\hfil
      &\global\@eqcnt\tw@ \hskip \tw@\arraycolsep
         $\displaystyle{##}$\hfil\tabskip\@centering
      &\global\@eqcnt\thr@@ \hb@xt@\z@\bgroup\hss##\egroup
         \tabskip\z@skip
      \cr
}
\def\endeqnarray{%
      \@@eqncr
      \egroup
      \global\advance\c@equation\m@ne
   $$\@ignoretrue
}
%
%%%%%%%%%%%%%%%%%%%%%%%%%%% Titles %%%%%%%%%%%%%%%%%%%%%%%%%%%
%

\renewcommand\contentsname{Contents}
\renewcommand\listfigurename{List of Figures}
\renewcommand\listtablename{List of Tables}
\renewcommand\refname{References}
\renewcommand\indexname{Index}
\renewcommand\figurename{Figure}
\renewcommand\tablename{Table}
\renewcommand\partname{Part}
\renewcommand\appendixname{Appendix}
\renewcommand\abstractname{Abstract}

%
%%%%%%%%%%%%%%%%%%%%%%%%%%% Article Front Matter %%%%%%%%%%%%%%%%%%%%%%%%%%%
%

%% Article Type
\sodef\letterspacetwty{}{.1em}{.4em plus1em}{.5em plus 1em minus1em }
\sodef\letterspacehalftitle{}{.12em}{.5em plus1em}{.5em plus 1em minus1em}%
\sodef\letterspaceseriestitle{}{.12em}{.5em plus1em}{.5em plus 1em minus1em}%
\sodef\letterspaceseriestitles{}{.1em}{.5em plus1em}{.5em plus 1em minus1em}%
\sodef\letterspacefmauthor{}{.095em}{.5em plus1em}{.5em plus 1em minus1em}%
\sodef\letterspacefmeditiontxt{}{.075em}{.5em plus1em}{.5em plus 1em minus1em}%
\sodef\letterspacepartname{}{.127em}{.5em plus1em}{.5em plus 1em minus1em}%
\sodef\letterspaceparttitle{}{.106em}{.5em plus1em}{.5em plus 1em minus1em}%
\sodef\letterspacechapname{}{.11em}{.5em plus1em}{.5em plus 1em minus1em}%
\sodef\letterspacethreehdname{}{.05em}{.5em plus1em}{.5em plus 1em minus1em}%
\sodef\letterspaceappthreehdname{}{.061em}{.5em plus1em}{.5em plus 1em minus1em}%
\sodef\letterspaceboxtwohdname{}{.08em}{.5em plus1em}{.5em plus 1em minus1em}%
\sodef\letterspacefivehdname{}{.072em}{.5em plus1em}{.5em plus 1em minus1em}%
\sodef\letterspaceappname{}{.11em}{.5em plus1em}{.5em plus 1em minus1em}%
\sodef\letterspacebibname{}{.095em}{.5em plus1em}{.5em plus 1em minus1em}%
\sodef\letterspacerhtext{}{.05em}{.5em plus1em}{.5em plus 1em minus1em}%
\sodef\letterspacecontrbauthor{}{.001em}{.35em}{.35em}%
\sodef\letterspaceabrvlabel{}{.06em}{.5em plus1em}{.5em plus 1em minus1em}%
\sodef\letterspacedialguename{}{.047em}{.5em}{.5em}%
\sodef\letterspacefigname{}{.0334em}{.5em plus1em}{.5em plus 1em}%
\sodef\letterspacetocpart{}{.05em}{.5em plus1em}{.5em plus 1em minus1em}%
\sodef\letterspacerunhead{}{.05em}{.5em plus1em}{.5em plus 1em minus1em}%

\def\articletype#1{\gdef\ArtType{\letterspacetwty{#1}}\gdef\@ArtType{#1}}%
\articletype{RESEARCH ARTICLE}%

%% Meta Info
\def\@artid{xx}
\def\@jname{xxx xxx}
\def\@jvol{xx}
\def\@jissue{xx}
\def\@jyear{{0000}}
\def\@jmonth{xx}
\def\@DOI{}
\def\@logourl{}
%
\def\artid#1{\gdef\@artid{#1}}
\def\jname#1{\markboth{\textit{#1}}{\textit{#1}}\gdef\@jname{#1}}
\def\jvol#1{\gdef\@jvol{#1}}
\def\jissue#1{\gdef\@jissue{#1}}
\def\jyear#1{\gdef\@jyear{#1}}
\def\jmonth#1{\gdef\@jmonth{#1}}
\def\jdoi#1{\gdef\@jdoi{\phantom{DOI \href{http://dx.doi.org/#1}{#1}}}}
%\jname{Nagoya Math. J.,}%
\def\jrarticle#1{\gdef\@jrarticle{#1}}%
\def\jrcopyrightline#1{\gdef\@jrcopyrightline{#1}}
\def\OpenAccess#1{\gdef\@OpenAccess{#1}}

%%% Font Def
\def\Titletxtfont{\fontsize{12pt}{14bp}\bfseries\selectfont\centering\mathversion{bold}}
\def\Authorfont{\reset@font\fontsize{10.91bp}{14bp}\scshape\selectfont\centering}
\def\addressfont{\fontsize{10bp}{12bp}\selectfont\centering}
\def\abstractheadfont{\fontsize{9pt}{11bp}\selectfont\bfseries}
\def\abstractfont{\fontsize{9pt}{12bp}\selectfont\leftskip58pt\rightskip58pt}
\def\keywordfont{\fontsize{8bp}{9bp}\selectfont}%

\def\abstractsubheadfont{\fontfamily{\rmdefault}\fontsize{9}{11}\fontseries{b}\selectfont}%

%% Article Title
%\def\title#1{\gdef\@title{#1}}

\renewcommand{\title}[2][]{%
\gdef\@checktitle{#1}\ifx\@checktitle\empty\gdef\@title{\MakeUppercase{#2}}%
\gdef\s@title{#2}\else\gdef\@title{\MakeUppercase{#2}}\gdef\s@title{#1}\fi}

%% Cross Link for Author & Address
\def\jmkLabel#1{\@bsphack\protected@write\@auxout{}{\string\Newlabel{#1}{\@currentlabel}}\@esphack}
\def\Newlabel#1#2{\expandafter\xdef\csname X@#1\endcsname{#2}}
\def\jmkRef#1{\@ifundefined{X@#1}{0}{\csname X@#1\endcsname}}

%% Article Author(s)
\let\sep\@empty%
\let\authorsep\@empty%
\newcount\aucount%
\newcount\punctcount%
\def\author{\advance\aucount by 1\@ifstar\@@corrauthor\@@author}%
\def\au@and{\ifnum\punctcount=2\ and\else\unskip,\advance\punctcount by -1 \fi}%
\def\artauthors{}
\newif\if@auemail%
\newcommand{\@@author}[2][]{\g@addto@macro\artauthors{%
    \ifnum\aucount=1%
    \global\@auemailtrue%
    \else%
    \global\@auemailfalse%
    \fi%
    \Authorfont%
    \def\baselinestretch{1}%
    \authorsep{\MakeUppercase{#2}}\unskip\ifx#1\empty\else\textsuperscript{\smash{{%
      \@for\@@affmark:=#1\do{\edef\affnum{\@ifundefined{X@\@@affmark}{1}{\jmkRef{\@@affmark}}}%
     \unskip\sep\affnum\let\sep=,}}}}\fi%
    \def\authorsep{\au@and~}%%% 
    \global\let\sep\@empty\global\let\@corref\@empty%
}}%

\newcommand{\@@corrauthor}[2][]{\g@addto@macro\artauthors{%
    \global\@auemailtrue%
    \Authorfont%
    \def\baselinestretch{1}%
    \authorsep{#2}\unskip\ifx#1\empty\else\textsuperscript{\smash{{%
      \@for\@@affmark:=#1\do{\edef\affnum{\@ifundefined{X@\@@affmark}{1}{\jmkRef{\@@affmark}}}%
     \unskip\sep\affnum\let\sep=,}}}}\fi%
    \def\authorsep{\au@and~}%%% 
    \global\let\sep\@empty\global\let\@corref\@empty%
}}%
%
\RequirePackage{ltxcmds}%
\def\authororcid#1{%
\ltx@ifpackageloaded{hyperref}{\href{https://orcid.org/#1}{\includegraphics{orcid_logo}}}{{\includegraphics{orcid_logo}}}%
}
\newcommand{\orcid}[1]{\protect\g@addto@macro\artauthors{%
\authororcid{#1}%
}}
%
%% Author Email
\newif\ifcorrauth
\def\addressrule{\kern-10\p@ \hrule \@height.15pt \@width\marginparwidth \kern 1.6\p@}%
\def\@email{}
\DeclareUrlCommand\myemail{}%
\def\email#1{ {\hbox{E-mail:}~}\ltx@ifpackageloaded{hyperref}{\textcolor{blue}}{}{\protect\myemail{#1}}}%
\def\affemail#1{\\\ltx@ifpackageloaded{hyperref}{\textcolor{blue}}{}{\normalfont\protect\myemail{#1}}}%



\let\nomail\relax

%% Corrseponding Address 
\def\@copycorthanks{}
\def\auaddress{}
\def\@auaddress{}
\newcounter{affn}
\renewcommand\theaffn{\arabic{affn}}
\def\address{\@ifstar\@@coraddress\@@address}
\newcount\addcount% To check the count of address
\newcommand{\@@coraddress}[2][]{%\advance\addcount by 1
\g@addto@macro\auaddress{%
     \stepcounter{affn}%
     \xdef\@currentlabel{\theaffn}%
     \jmkLabel{\theaffn}%
     {\textsuperscript{#1}#2.}\par}
}

%% Author Address 
\def\addsep{\par\vskip0pt}
\newcount\addcount% To check the count of address
\newcommand{\@@address}[2][]{\advance\addcount by 1\g@addto@macro\auaddress{%
     \stepcounter{affn}%
     \xdef\@currentlabel{\theaffn}%
     \jmkLabel{\theaffn}%
     {\textsuperscript{#1}#2.}\par}%\theaffn
}


%% Address tagging 
\newcommand{\orgdiv}[1]{#1}%
\newcommand{\orgname}[1]{#1}%
\newcommand{\orgaddress}[1]{#1}%
\newcommand{\street}[1]{#1}
\newcommand{\postcode}[1]{#1}
\newcommand{\city}[1]{#1}
\newcommand{\state}[1]{#1}
\newcommand{\country}[1]{#1}

%% Author name in Running Head
\def\@authormark{}%
%\def\leftmark{Nagoya Math. J.}%
\def\authormark#1{\gdef\@authormark{#1}}%

%% Optional Text
\def\artnotefont{\reset@font\fontfamily{\rmdefault}\fontsize{8.6}{13}\selectfont\raggedright}
\def\artnote#1{\gdef\@Artnote{#1}}\artnote{}%

%% Article Abstract
%\newcommand\abstracthead{\@startsection {section}{1}{\z@}{-22pt \@plus2ex \@minus0ex}{0.0001pt}{\abstractheadfont}}
%\newcommand\subabstracthead{\@startsection{subsection}{2}{\z@}{3pt \@plus0ex \@minus0ex}{-.5em}{\abstractsubheadfont}}

\def\@abstract{}%
\long\def\abstract#1{\def\@abstract{%
%\let\paragraph\subabstracthead%
\abstractfont%
{\noindent\abstractheadfont\abstractname.}\hskip8pt #1\par\vspace*{10pt}}}%

\def\printabstract{\ifx\@abstract\empty\else\@abstract\fi\par}%

\def\printkeywords{\ifx\@keywords\empty\else\@keywords\fi\par}%

%
%% Keywords
\def\keywordname{Keywords}
\newcommand{\keywords}[2][\keywordname]{%
\g@addto@macro\@keywords{\par\addvspace{2pt}{\keywordfont\hskip12pt{#1:}\space#2\par}}}
\def\@keywords{}
\newcommand{\codes}[2][Primary]{\textit{#1} -- #2}%

%% Article History

\newcommand{\received}[2][Received]{\g@addto@macro\@history{\hskip12pt{#1}\space#2}}%
\newcommand{\revised}[2][Revised]{\g@addto@macro\@history{. {#1}\space#2}}%
\newcommand{\accepted}[2][Accepted]{\g@addto@macro\@history{. {#1}\space#2}}%

%% Article Header Definition
\renewcommand{\@maketitle}
{{\vbox{\parindent0pt%
\ifx\@title\empty\else%
        \null\vskip8.5pt\nointerlineskip%
        \vskip21pt%
        {\Titletxtfont{\centering{\@title}}\par}
    \fi%
    %% Aritle Authors
    \ifnum\aucount>0
        \global\punctcount\aucount%
        \vskip12.43pt%
        \artauthors\par%%     authors and emails
     \fi}\par\addvspace{16.55pt}%
{\printabstract}
}\par}

\gdef \spilongpage {%
   \@ifstar
     {%
      \@spilongpage{\hbox{\kern\p@}}}%
     {%
      \@spilongpage\@empty}%
}
\gdef\@spilongpage#1#2{%
   \@tempskipa#2\relax
   \ifdim \@tempskipa>.5\maxdimen
     \@latexerr{Suggested\space extra\space height\space
                (\the\@tempskipa)\space dangerously\space
                large}\@eha
   \else
     \ifdim \vsize<.5\maxdimen
       \@bsphack
         \insert\@kludgeins{#1\vskip-\@tempskipa}%
       \@esphack
     \else
       \@latexerr{Page\space height\space already\space
                  too\space large}\@eha
     \fi
   \fi
}

%% Printing Article Header
\renewcommand\maketitle{\par%
  \@afterindentfalse%
  \begingroup
%    \gdef\UrlFont{\rmfamily}%
    \def\thanks##1{\gdef\afftext{\hspace*{10pt}##1\par}}%
    \long\def\@makefntext##1{{\hskip12pt\@textsuperscript{\normalfont\@thefnmark}}##1\vphantom{py}\par}%
    \renewcommand\thefootnote{\@fnsymbol\c@footnote}%
    \if@twocolumn
      \ifnum \col@number=\@ne%
        \twocolumn[\@maketitle]%
      \else
        \twocolumn[\@maketitle]%
      \fi%
    \else%
      \newpage%
      \global\@topnum\z@% Prevents figures from going at top of page.
      \@maketitle%
    \fi%
    \titlefoot%
    \thispagestyle{titlepage}%
    \spilongpage{10pt}
  \endgroup%
  \setcounter{footnote}{0}%
  %\global\let\thanks\relax%
  \global\let\maketitle\relax%
  \global\let\@maketitle\relax%
  %\global\let\@thanks\@empty%
  \global\let\@author\@empty%
  \global\let\@date\@empty%
  %\global\let\@title\@empty%
  \global\let\title\relax%
  \global\let\author\relax%
  \global\let\date\relax%
  \global\let\and\relax
  %%%print continuous abstract on next page
  \@afterheading}%


\newenvironment{Frontmatter}{\par}{\maketitle\par}

\newenvironment{Backmatter}{\par%
\def\sectionfont{\fontfamily{\rmdefault}\fontsize{10}{12}\fontseries{b}\selectfont\raggedright}
\def\paragraphfont{\fontfamily{\rmdefault}\fontsize{10.91bp}{14pt}\fontseries{b}\selectfont}
\renewcommand\section{\@startsection {section}{1}{\z@}%
                                   {-12pt \@plus -2ex \@minus 0ex}%
                                   {3pt}%
                                   {\sectionfont}}%
\renewcommand\paragraph{\@startsection{paragraph}{4}{\z@}%
                                    {2.25ex \@plus1ex \@minus.2ex}%
                                    {-.5em}%
                                    {\paragraphfont}}
\def\bibfont{\fontsize{9pt}{11bp}\selectfont}%
}{\par}
\def\bysame{\leavevmode\hbox to3em{\hrulefill}}

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Page Styles %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
\def\opheaderfont{\fontsize{7}{9}\selectfont}
\def\footerfont{\fontsize{7}{8}\selectfont}

\def\runheadstyle{\fontsize{6pt}{11}\selectfont}
\def\runfoliostyle{\fontsize{10pt}{13}\selectfont\selectfont}


%% Regular Page Style      
\def\ps@headings{\let\@mkboth\markboth%
   \let\@oddfoot\@empty%
   \let\@evenfoot\@empty%
   \def\@evenhead{\hbox to \textwidth{{\runfoliostyle\unskip\thepage}\hfill{\runheadstyle{\letterspacerunhead{\MakeUppercase{\@authormark}}}}\hfill}}
   \def\@oddhead{\hbox to \textwidth{\hfill{\runheadstyle{\letterspacerunhead{\MakeUppercase{\s@title}}}}\hfill{\runfoliostyle\thepage}}}%%\s@title
        \def\chaptermark##1{\markboth{\MakeUppercase{##1}}%\thechapter\hskip.97em
        {\MakeUppercase{##1}}}%\thechapter\hskip.97em
        \def\sectionmark##1{\markright{{\ifnum\c@secnumdepth>\z@\fi##1}}}%\thesection\hskip.91em
}%

\newdimen\opshortpage%

\def\printcopyright#1{#1}%%

\def\copytext#1{\def\@copyrighttext{\begin{minipage}[t]{\textwidth}\footerfont #1\end{minipage}}}
\copytext{\textcopyright\ The Author(s) 2022. This is an Open Access article, distributed under the terms of the Creative Commons Attribution licence (http://
creativecommons.org/licenses/by/4.0/), which permits unrestricted re-use, distribution, and reproduction in any medium, provided the original work is
properly cited.
}%

\newcommand{\amsclassification}[2][]{\gdef\@amsclassification{\def\sep{\ignorespaces,\space}\keywordfont\hskip12pt{{#1}:\space#2\par}}}
\gdef\@amsclassification{}

\AtEndDocument{\label{LastPage}}
\def\@opjournalheader{\textit{\@jname}\space\ {{(\@jyear),\ \textbf{\@jvol}:\@artid}} {\thepage{--}\pageref*{LastPage}}\\
{\@DOI}}%

%% Opening Page Style      
\let\firstmark\botmark
\def\afftext{}
\gdef\@correspond{}
\def\titlefoot{\footnotetext{{%
   \fontsize{9pt}{11bp}
   \selectfont\par\vskip-6.75pt%\vspace*{-6.75pt}
%   \@history.
}%
   \par
   %\vskip-2pt
   \ifx\@amsclassification\@empty\else\@amsclassification\fi%%
   \@keywords%%
   \par
   \@correspond
   \par
   \afftext
   }}

\newif\ifDpageNo%
\DpageNotrue%

\def\ps@titlepage{\let\@mkboth\markboth%
   \def\@oddfoot{\ifx\@OpenAccess\undefined
   \raisebox{-1.96pt}[0pt][0pt]{\vbox{\fontsize{7pt}{9bp}\selectfont\hspace*{.1pt}\phantom{\textcircled{c} \ifx\@jrcopyrightline\undefined\else{\itshape\@jrcopyrightline}\fi}\par}}
   \else
   \parbox{\textwidth}{{\fontsize{7pt}{9bp}\selectfont\hspace*{.1pt}\phantom{\textcircled{c} \ifx\@jrcopyrightline\undefined\else{\itshape\@jrcopyrightline\space\@OpenAccess\fi}}\par}}
   \fi%
   }%
   \def\@evenfoot{}
   \def\@evenhead{\raisebox{-5.5pt}[0pt][0pt]{\parbox{0.8\textwidth}{\fontsize{8pt}{9.5bp}\selectfont{\@jrarticle}, (\@jyear), \ifDpageNo XX--XX\else\thepage--\pageref*{LastPage}\fi\par%
   \@jdoi\par}}}%
   \def\@oddhead{\hbox to \textwidth{\raisebox{-5.5pt}[0pt][0pt]{\parbox{0.8\textwidth}{\fontsize{8pt}{9.5bp}\selectfont{\@jrarticle}, (\@jyear), \ifDpageNo XX--XX\else\thepage--\pageref*{LastPage}\fi\par%
   %{Volume \@jrvol},\space{Number \@jrissue},\space\@jrmonth\space\@jyear\par%
   \@jdoi\par}}%\hfill{\fontsize{10.75bp}{12bp}\selectfont}
   }}%
}%


%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Sections %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
\def\numbered{\setcounter{secnumdepth}{3}}
\def\unnumbered{\setcounter{secnumdepth}{0}}
\numbered%% default is unnumbered Sections

\newcounter {chapter}[part]% for bookmarks
\renewcommand\thepart         {\@Roman\c@part}
\renewcommand\thesection      {\@arabic\c@section}
\renewcommand\thesubsection   {\thesection.\@arabic\c@subsection}
\renewcommand\thesubsubsection{\thesubsection.\@arabic\c@subsubsection}
\renewcommand\theparagraph    {\thesubsubsection.\@arabic\c@paragraph}
\renewcommand\thesubparagraph {\theparagraph.\@arabic\c@subparagraph}
%%
\renewcommand\part{\par
   \addvspace{4ex}%
   \@afterindentfalse
   \secdef\@part\@spart}
%%
\def\@part[#1]#2{%
    \ifnum \c@secnumdepth >\m@ne
      \refstepcounter{part}%
      \addcontentsline{toc}{part}{\thepart\hspace{1em}#1}%
    \else
      \addcontentsline{toc}{part}{#1}%
    \fi
    {\parindent \z@ \raggedright
     \interlinepenalty \@M
     \normalfont
     \ifnum \c@secnumdepth >\m@ne
       \Large\bfseries \partname~\thepart
       \par\nobreak
     \fi
     \huge \bfseries #2%
     \markboth{}{}\par}%
    \nobreak
    \vskip 3ex
    \@afterheading}

\def\@spart#1{%
    {\parindent \z@ \raggedright
     \interlinepenalty \@M
     \normalfont
     \huge \bfseries #1\par}%
     \nobreak
     \vskip 3ex
     \@afterheading}

\def\@sect#1#2#3#4#5#6[#7]#8{%
  \ifnum #2>\c@secnumdepth
    \let\@svsec\@empty
  \else
    \refstepcounter{#1}%
    \protected@edef\@svsec{\@seccntformat{#1}\relax}%
  \fi
  \@tempskipa #5\relax
  \ifdim \@tempskipa>\z@
  \ifnum#2=1
  \begingroup
   #6{%
        \@hangfrom{\hskip #3\relax\ifnum#2=1\S\else\fi\@svsec}%
          \interlinepenalty \@M {#8}\@@par}%
    \endgroup
  \else
    \begingroup
      #6{%
        \@hangfrom{\hskip #3\relax\ifnum#2=3\else\hskip12pt\fi\@svsec}%
          \interlinepenalty \@M #8\@@par}%
    \endgroup
    \fi
    \csname #1mark\endcsname{#7}%
    \addcontentsline{toc}{#1}{%
      \ifnum #2>\c@secnumdepth \else
        \protect\numberline{\csname the#1\endcsname}%
      \fi
      #7}%
  \else
    \def\@svsechd{%
      #6{\hskip #3\relax
      \@svsec #8.}%
      \csname #1mark\endcsname{#7}%
      \addcontentsline{toc}{#1}{%
        \ifnum #2>\c@secnumdepth \else
          \protect\numberline{\csname the#1\endcsname}%
        \fi
        #7}}%
  \fi
  \@xsect{#5}}
%
\def\sectionfont{\fontsize{10.91bp}{14bp}\selectfont\bfseries\raggedright\mathversion{bold}}
\def\subsectionfont{\reset@font\fontsize{10.91bp}{14bp}\selectfont\bfseries\raggedright\mathversion{bold}}
\def\subsubsectionfont{\reset@font\fontsize{11pt}{14bp}\selectfont\raggedright}
\def\paragraphfont{\reset@font\rmfamily\fontsize{10bp}{12bp}\selectfont}
\def\subparagraphfont{\reset@font\rmfamily\fontsize{10bp}{12bp}\selectfont}

\def\@seccntformat#1{{\csname the#1\endcsname.}\hskip8pt}

\renewcommand\section{\@startsection {section}{1}{\z@}%
                                   {10pt \@plus 1pt}%
                                   {6pt}%
                                   {\sectionfont}}
\renewcommand\subsection{\@startsection{subsection}{2}{\z@}%
                                     {-9.3pt \@plus -1pt}%
                                     {.1pt}%
                                     {\subsectionfont}}
\renewcommand\subsubsection{\@startsection{subsubsection}{3}{\z@}%
                                     {12pt \@plus 1pt}%
                                     {5.1pt}%
                                     {\subsubsectionfont}}
\renewcommand\paragraph{\@startsection{paragraph}{4}{\z@}%
                                    {-14pt  \@plus -1pt}%
                                    {-3.5pt}%
                                    {\paragraphfont}}
\renewcommand\subparagraph{\@startsection{subparagraph}{5}{\z@}%
                                       {-6pt \@plus -1pt}%
                                       {0.001pt}%
                                     {\subparagraphfont}}
%
\def\@startsection#1#2#3#4#5#6{%
  \if@noskipsec \leavevmode \fi
  \par
  \@tempskipa #4\relax
  \@afterindenttrue
  \ifdim \@tempskipa <\z@
    \@tempskipa -\@tempskipa \ifnum#2=2\else\@afterindentfalse\fi%
  \fi
  \if@nobreak
    \everypar{}\ifnum#2=2\vskip0pt\else\ifnum#2=3\vskip1pt\else\ifnum#2=4\vskip1pt\fi\fi\fi%
  \else
    \addpenalty\@secpenalty\addvspace\@tempskipa
  \fi
  \@ifstar
    {\@ssect{#3}{#4}{#5}{#6}}%
    {\@dblarg{\@sect{#1}{#2}{#3}{#4}{#5}{#6}}}}
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Lists %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

\newdimen\labelwidthi
\newdimen\labelwidthii
\newdimen\labelwidthiii
\newdimen\labelwidthiv
\def\normal@labelsep{0.5em}
\labelsep\normal@labelsep
\settowidth{\labelwidthi}{(iii)}
\settowidth{\labelwidthii}{(d)}
\settowidth{\labelwidthiii}{(iii)}
\settowidth{\labelwidthiv}{(M)}
\leftmargini\labelwidthi    \advance\leftmargini\labelsep
\leftmarginii\labelwidthii  \advance\leftmarginii\labelsep
\leftmarginiii\labelwidthiii \advance\leftmarginiii\labelsep
\leftmarginiv\labelwidthiv  \advance\leftmarginiv\labelsep
\def\setleftmargin#1#2{\settowidth{\@tempdima}{#2}\labelsep\normal@labelsep
  \csname labelwidth#1\endcsname\@tempdima
  \@tempdimb\@tempdima \advance\@tempdimb\labelsep
  \csname leftmargin#1\endcsname\@tempdimb}
\def\@listI{\leftmargin\leftmargini
  \labelwidth\labelwidthi \labelsep\normal@labelsep
  \topsep 6pt \partopsep\z@ \parsep\z@ \itemsep\z@
  \listparindent 1em}
\def\@listii{\leftmargin\leftmarginii
  \labelwidth\labelwidthii \labelsep\normal@labelsep
  \topsep\z@ \partopsep\z@ \parsep\z@ \itemsep\z@
  \listparindent 1em}
\def\@listiii{\leftmargin\leftmarginiii
  \labelwidth\labelwidthiii \labelsep\normal@labelsep
  \topsep\z@ \partopsep\z@ \parsep\z@ \itemsep\z@
  \listparindent 1em}
\def\@listiv{\leftmargin\leftmarginiv
  \labelwidth\labelwidthiv \labelsep\normal@labelsep
  \topsep\z@ \partopsep\z@ \parsep\z@ \itemsep\z@
  \listparindent 1em}
\let\@listi\@listI
\@listi
%
\setlength  \labelsep  {.5em}
\setlength  \labelwidth{\leftmargini}
\addtolength\labelwidth{-\labelsep}
\@beginparpenalty -\@lowpenalty
\@endparpenalty   -\@lowpenalty
\@itempenalty     -\@lowpenalty
\def\labelitemi{$\bullet$}          \def\labelitemii{$\cdot$}
\def\labelenumi{\theenumi.}        \def\theenumi{\arabic{enumi}}
\def\labelenumii{(\alph{enumii})}   \def\theenumii{\theenumi\alph{enumii}}
\def\labelenumiii{(\roman{enumiii})}\def\theenumiii{\theenumii.\roman{enumiii}}
\def\labelenumiv{(\Alph{enumiv})}   \def\theenumiv{\theenumiii.\Alph{enumiv}}

%
%%%%%%%%%%%%%%%%%%%%%%%%%%% Ordered & Unordered List  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

\def\listfont{\normalsize}%

\def\enumargs{%
   \listfont%
   \leftmargini0pt%
   \topsep 7pt%
   \partopsep     \z@%
   \itemsep       \z@%
   \parsep        \z@%
   \labelsep      7.31\p@%
   \rightmargin   \z@%
   \raggedright%   
   \listparindent \parindent%
   \itemindent    \z@}%

\def\enumerate{%
    \@ifnextchar[{\@numerate}{\@numerate[0.]}}

\def\@numerate[#1]{\par%
     \ifnum \@enumdepth >3 \@toodeep\else
     \advance\@enumdepth \@ne
     \edef\@enumctr{enum\romannumeral\the\@enumdepth}
     \list{\csname label\@enumctr\endcsname}{%
       \enumargs%
       \setlength{\leftmargin}{\csname leftmargin\romannumeral\the\@enumdepth\endcsname}
       \usecounter{\@enumctr}
       \settowidth\labelwidth{\upshape#1}
       \addtolength{\leftmargin}{\labelwidth}
       \addtolength{\leftmargin}{\labelsep}
       \def\makelabel##1{\hbox to \labelwidth{\upshape##1}}}%
     \fi
   }
\let\endenumerate\endlist

%%Unnumbered list%%
\def\unenumargs{%
   \listfont%
   \leftmargini\parindent%
   \topsep7pt%
   \partopsep     \z@%
   \itemsep       2.5\p@%
   \parsep        \z@%
   \labelsep      0\p@%
   \rightmargin   \z@%
   \raggedright%
   \listparindent \parindent%
   \itemindent    -10pt}%

\def\unenumerate{%
    \@ifnextchar[{\@unenumerate}{\@unenumerate[0.]}}

\def\@unenumerate[#1]{\par%
     \ifnum \@enumdepth >3 \@toodeep\else
     \advance\@enumdepth \@ne
     \edef\@enumctr{enum\romannumeral\the\@enumdepth}
     \list{\csname label\@enumctr\endcsname}{%
       \unenumargs
       \setlength{\leftmargin}{\csname leftmargin\romannumeral\the\@enumdepth\endcsname}
       \usecounter{\@enumctr}
%       \settowidth\labelwidth{#1}
%       \addtolength{\leftmargin}{\labelwidth}
%       \addtolength{\leftmargin}{\labelsep}
       \def\makelabel##1{\hss\llap{##1}}}%
     \fi\listfont}
\let\endunenumerate\endlist%

\def\itemargs{%
   \listfont%
   \leftmargini-1.5pt%
   \topsep7pt%
   \partopsep     \z@%
   \itemsep       \z@%
   \parsep        \z@%
   \labelsep      3.5\p@%
   \rightmargin   \z@%
   \raggedright%
   \listparindent \parindent%
   \itemindent    \z@}%

\def\listtextbullet{{\fontencoding{OMS}\fontfamily{cmsy}\fontsize{11bp}{12bp}\selectfont\char110}}%
\AtBeginDocument{\def\textbullet{{\fontencoding{OMS}\fontfamily{cmsy}\fontshape{n}\fontsize{11bp}{12bp}\selectfont\char15}}}%
%
%\definecolor{ItemBullGray}{cmyk}{0,0,0,0.5}% 50% black

%\renewcommand\labelitemi{\textbullet}
%\renewcommand\labelitemii{\textendash}

\def\itemize{%
   \@ifnextchar[{\@itemize}{\@itemize[\listtextbullet]}}

\def\@itemize[#1]{\par%
     \ifnum \@itemdepth >3 \@toodeep\else
     \advance\@itemdepth \@ne
     \edef\@itemctr{item\romannumeral\the\@itemdepth}
     \list{\csname label\@itemctr\endcsname}{%
       \itemargs
       \setlength{\leftmargin}{\csname leftmargin\romannumeral\the\@itemdepth\endcsname}
       \settowidth\labelwidth{#1}
       \addtolength{\leftmargin}{\labelwidth}
       \addtolength{\leftmargin}{\labelsep}
       \def\makelabel##1{{##1}}}%
     \fi
   \listfont}
\let\enditemize\endlist
%
\def\verse{\let\\=\@centercr
  \list{}{\itemsep\z@
  \itemindent \z@
  \listparindent\z@
  \rightmargin 1em
  \leftmargin \rightmargin}\item[]}
\let\endverse\endlist
\def\quotation{\list{}{\vskip 8pt\itemindent\z@
 \leftmargin 1em \rightmargin \z@
  \parsep \z@ \@plus 1pt}\item[]}
\newcommand\e{\endlist\vskip 8pt}
\let\endquotation\e
\def\quote{\list{}{\itemindent\z@
   \leftmargin 1em \rightmargin \z@}%
\item[]}
\let\endquote\endlist
\def\descriptionlabel#1{\hspace\labelsep\bfseries #1}
\def\description{\list{}{\leftmargin=2em\labelwidth\z@
  \leftmargin \z@ \topsep6pt\itemindent-\leftmargin
  %\advance\itemindent-\labelsep
  \let\makelabel\descriptionlabel}}
\let\enddescription\endlist
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Float %%%%%%%%%%%%%%%%%%%%%%%%%%%%
\def\fignumstyle{\reset@font\rmfamily\fontsize{9pt}{12bp}\selectfont}
\def\figtextstyle{\reset@font\rmfamily\fontsize{9pt}{12bp}\selectfont}

\setlength\abovecaptionskip{3.7\p@}
\setlength\belowcaptionskip{6\p@}

\setlength\arraycolsep{2\p@}
\setlength\tabcolsep{6\p@}
\setlength\arrayrulewidth{.4\p@}
\setlength\doublerulesep{2\p@}
\setlength\tabbingsep{\labelsep}

\def\fnum@figure{{\figurename}\space\relax{\thefigure}.\hskip5pt}
\def\fnum@table{{\tablename}\space\relax{\thetable}.\hskip.5bp}

\def\FigName{figure}

\long\def\@makecaption#1#2{%
    \ifx\FigName\@captype
      \vskip\abovecaptionskip
        \@figurecaption{#1}{#2}
    \else
        \@tablecaption{#1}{#2}
      \vskip\belowcaptionskip
    \fi%
}

%% Figure

%\def\figurenumfont{\reset@font\fontfamily{\rmdefault}\fontsize{10}{12}\fontshape{it}\fontseries{b}\selectfont}
%\def\figurecaptionfont{\fontfamily{\rmdefault}\fontsize{10}{12}\fontshape{it}\selectfont}

\newdimen\figwidth%
\newdimen\figheight%
\newdimen\sidecapwidth
\newdimen\wrapcapline%
\newdimen\totalwrapline%
\newdimen\wraptotline%

%% Figures macro
\def\FIG#1#2{%
{\centering#1\par}
#2}

%% Figures caption
%\newbox\figcapbox
%\newbox\capbox
\long\def\@figurecaption#1#2{%
         \setbox\@tempboxa\hbox{\fignumstyle #1\space\figtextstyle#2}\let\source\figsource%%
         \ifdim \wd\@tempboxa >\hsize%
          \centering%
           \fignumstyle \textit{#1}\par%
           \figtextstyle#2\par%
          \else%
          \centering%
           \fignumstyle \textit{#1}\par%
           \figtextstyle#2\par%
          \fi%\vskip\abovecaptionskip%%
}%

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
\RequirePackage{booktabs}
\cmidrulewidth=.15pt%
%

\def\tblnumstyle{\reset@font\itshape\fontsize{9pt}{10bp}\selectfont}
\def\tbltextstyle{\reset@font\itshape\fontsize{9pt}{10bp}\selectfont}
\def\tblbdysize{\reset@font\rmfamily\fontsize{10bp}{12bp}\selectfont}
\def\tblnotesize{\reset@font\fontsize{8bp}{9bp}\selectfont}

%\def\tablenumfont{\reset@font\itshape\fontsize{9pt}{10bp}\selectfont}
%\def\tablecaptionfont{\reset@font\itshape\fontsize{9pt}{10bp}\selectfont}
%\def\tablebodyfont{\fontsize{10}{12}\selectfont}
%\def\tablecolheadfont{\fontsize{10}{12}\selectfont}
%\def\tablefootnotefont{\reset@font\fontsize{8bp}{9bp}\selectfont\raggedright}

\usepackage{threeparttable}
\newbox\mybox
%% Table Macro
\newbox\tablebox
%\newdimen\tabbodywidth
\long\def\TBL#1#2{%
\def\@tblfrstarg{#1}\tabcolsep0pt\def\arrayrulewidth{0.6bp}\def\arraystretch{1}%
\def\tnote##1{\protect\TPToverlap{\textsuperscript{\TPTtagStyle{##1}}}}%%
\setbox\mybox\hbox{{\tblbdysize#2}}%
\ifx\@tblfrstarg\@empty%
\fi
\let\footnote\tablefootnote%% for text footnote cited in Table.
\let\footnotemark\tnote%
\renewcommand\footnotetext[2][]{\ifx##1\empty\item[]##2\else\item[##1]##2\fi}\centering
\begin{threeparttable}
#1\vskip6pt\par%
{\tblbdysize#2}%
\end{threeparttable}
}

%% Table Caption
\newbox\tabcapbox
\newdimen\tabcapwidth
\long\def\@tablecaption#1#2{\vskip-3.7pt%
\let\source\tblsource%
        %\vskip\belowcaptionskip\vskip3.25pt
         \setbox\@tempboxa\hbox{{\tblnumstyle #1}\hskip0.4em\tbltextstyle#2}%
         \ifdim \wd\@tempboxa >\hsize%
           {{\tblnumstyle#1\hskip0.4em}{\tbltextstyle#2\par\vspace*{-3.5pt}}}%
         \else%
         %  \centering
           {\tblnumstyle #1}\hskip0.4em\tbltextstyle #2\par\vspace*{-3.5pt}%
         \fi%
}%

%% Table Column Heads
\newcommand\TCH[1]{{\def\seriesdefault{m}\rmfamily\fontseries{m}\fontsize{9bp}{11bp}\selectfont#1}}%

%% Table Footnotes
\newenvironment{fntable}[1][\columnwidth]{%
\def\@makefnmark{\hbox{\@textsuperscript{\fontsize{5}{5}\selectfont\@thefnmark}}}%
\begin{minipage}{#1}%
\def\thempfootnote{{\@arabic\c@mpfootnote}}
%%%%%%%%%%
\def\@xfootnotemark[##1]{%
   \begingroup
      %\c@footnote #1\relax
      \unrestored@protected@xdef\@thefnmark{##1}%
   \endgroup
   \@footnotemark}
%%%%%%%%%
\def\@xfootnotenext[##1]{%
  \begingroup
     %\csname c@\@mpfn\endcsname ##1\relax
     \unrestored@protected@xdef\@thefnmark{##1}%
  \endgroup
  \@footnotetext}
\renewcommand\@makefntext[1]{{%
%\tablefootnotefont%
\hskip0pt{\smash{\@makefnmark}}##1\par}}
\renewcommand\footnotesize{\@setfontsize\footnotesize{7}{8.5}}%
}%
{\vspace{-0.75\skip\footins}%
\renewcommand{\footnoterule}{}%
\end{minipage}}%

%% Table Rules
\heavyrulewidth=0.5bp
\lightrulewidth=0.5bp
\cmidrulewidth=0.5bp
\abovetopsep=0pt
\aboverulesep=0pt
\belowrulesep=0pt
\cmidrulekern=0em
%
\def\toprule{\noalign{\ifnum0=`}\fi
  \@aboverulesep=\abovetopsep
  \global\@belowrulesep=0.05pt %global cos for use in the next noalign
  \global\@thisruleclass=\@ne
  \@ifnextchar[{\@BTrule}{\@BTrule[0.6pt]}}
%
\def\midrule{\noalign{\ifnum0=`}\fi
  \@aboverulesep=0.5pt
  \global\@belowrulesep=\belowrulesep
  \global\@thisruleclass=\@ne
  \@ifnextchar[{\@BTrule}{\@BTrule[0.6pt]}}
\def\botrule{\noalign{\ifnum0=`}\fi
  \@aboverulesep=\aboverulesep
  \global\@belowrulesep=0pt%\belowbottomsep
  \global\@thisruleclass=\@ne
  \@ifnextchar[{\@BTrule}{\@BTrule[0.6pt]}}
%
\def\@@@cmidrule[#1-#2]#3#4{\global\@cmidla#1\relax
    \global\advance\@cmidla\m@ne
    \ifnum\@cmidla>0\global\let\@gtempa\@cmidrulea\else
    \global\let\@gtempa\@cmidruleb\fi
    \global\@cmidlb#2\relax
    \global\advance\@cmidlb-\@cmidla
    \global\@thisrulewidth=#3
    \@setrulekerning{#4}
    \ifnum\@lastruleclass=\z@\vskip 0pt\fi
    \ifnum0=`{\fi}\@gtempa
    \noalign{\ifnum0=`}\fi\futurenonspacelet\@tempa\@xcmidrule}
%
\def\@xcmidrule{%
   \ifx\@tempa\cmidrule
       \vskip-\@thisrulewidth
       \global\@lastruleclass=\@ne
   \else \ifx\@tempa\morecmidrules
       \vskip 0pt
       \global\@lastruleclass=\@ne\else
       \vskip 4.7pt%\belowrulesep
       \global\@lastruleclass=\z@
   \fi\fi
   \ifnum0=`{\fi}}
\let\cline\cmidrule

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%

\renewcommand\@pnumwidth{1.55em}
\renewcommand\@tocrmarg{2.55em}
\renewcommand\@dotsep{4.5}
\setcounter{tocdepth}{3}
\renewcommand\tableofcontents{%
    \section*{\contentsname
        \@mkboth{%
           \MakeUppercase\contentsname}{\MakeUppercase\contentsname}}%
    \@starttoc{toc}%
    }
\renewcommand*\l@part[2]{%
  \ifnum \c@tocdepth >-2\relax
    \addpenalty\@secpenalty
    \addvspace{2.25em \@plus\p@}%
    \begingroup
      \setlength\@tempdima{3em}%
      \parindent \z@ \rightskip \@pnumwidth
      \parfillskip -\@pnumwidth
      {\leavevmode
       \large \bfseries #1\hfil \hb@xt@\@pnumwidth{\hss #2}}\par
       \nobreak
       \if@compatibility
         \global\@nobreaktrue
         \everypar{\global\@nobreakfalse\everypar{}}%
      \fi
    \endgroup
  \fi}
\renewcommand*\l@section[2]{%
  \ifnum \c@tocdepth >\z@
    \addpenalty\@secpenalty
    \addvspace{0em}% \@plus\p@}%
    \setlength\@tempdima{1.5em}%
    \begingroup
      \parindent \z@ \rightskip \@pnumwidth
      \parfillskip -\@pnumwidth
      \leavevmode \bfseries
      \advance\leftskip\@tempdima
      \hskip -\leftskip
      #1\nobreak\hfil \nobreak\hb@xt@\@pnumwidth{\hss #2}\par
    \endgroup
  \fi}
\renewcommand*\l@subsection{\@dottedtocline{2}{1.5em}{2.3em}}
\renewcommand*\l@subsubsection{\@dottedtocline{3}{3.8em}{3.2em}}
\renewcommand*\l@paragraph{\@dottedtocline{4}{7.0em}{4.1em}}
\renewcommand*\l@subparagraph{\@dottedtocline{5}{10em}{5em}}
\renewcommand\listoffigures{%
    \section*{\listfigurename
      \@mkboth{\MakeUppercase\listfigurename}%
              {\MakeUppercase\listfigurename}}%
    \@starttoc{lof}%
    }
\renewcommand*\l@figure{\@dottedtocline{1}{1.5em}{2.3em}}
\renewcommand\listoftables{%
    \section*{\listtablename
      \@mkboth{%
          \MakeUppercase\listtablename}{\MakeUppercase\listtablename}}%
    \@starttoc{lot}%
    }
\let\l@table\l@figure

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% Mini-TOC %%%%%%%%%%%%%%%%%%%%%%%%%
%
\RequirePackage{etoc}
\etocarticlestylenomarks
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% Other Env. %%%%%%%%%%%%%%%%%%%%%%%%%
%

\renewenvironment{quote}
               {\list{}{\topsep=0pt\topsep6pt\leftmargin=1em\raggedright}%
                \item\relax}
               {\endlist}
               

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% Appendix %%%%%%%%%%%%%%%%%%%%%%%%%
%

\newif\ifbackmatter%
\newcommand{\backmatter}{\par\global\backmattertrue}
               
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% Appendix %%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% BackMatters %%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%


%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Doi-Links %%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%% From doi.sty %%

\DeclareRobustCommand{\doitext}{{doi}:}%

%% the meat of the code
%% the first command opens a group, and changes a few catcodes.
\newcommand*{\DOI}{%
  \begingroup
  \lccode`\~=`\#\relax
  \lowercase{\def~{\#}}%
  \lccode`\~=`\_\relax
  \lowercase{\def~{\_}}%
  \lccode`\~=`\<\relax
  \lowercase{\def~{\textless}}%
  \lccode`\~=`\>\relax
  \lowercase{\def~{\textgreater}}%
  \lccode`\~=0\relax
  \catcode`\#=\active
  \catcode`\_=\active
  \catcode`\<=\active
  \catcode`\>=\active
  \doitext\@mydoi
}

%% this is the actual command which processes the argument, with the catcodes
%% set in the previous command
%% it closes the group, and spits out the url.
\def\@mydoi#1{%
  \let\#\relax
  \let\_\relax
  \let\textless\relax
  \let\textgreater\relax
  \edef\x{\toks0={{#1}}}%
  \x
  \edef\#{\@percentchar23}%
  \edef\_{_}%
  \edef\textless{\@percentchar3C}% instead of {\string<} for Apple
  \edef\textgreater{\@percentchar3E}% instead of {\sting>} for Apple
  \edef\x{\toks1={\noexpand\href{http://dx.doi.org/#1}}}%
  \x
  \edef\x{\endgroup\the\toks1 \the\toks0}%
  \x
}
%
%%%%%%%%%%%%%%%%%%%%%%%%%%% Article History  %%%%%%%%%%%%%%%%%%%%
%
\def\@history{}
%\def\printhistory{{\par\addvspace{5pt}%
%\historyfont\noindent%
%\ifx\@history\empty\gdef\@history{Received xx xxx xxxx}\fi\@history\par}
%}%

%
\renewenvironment{theindex}
               {\if@twocolumn
                  \@restonecolfalse
                \else
                  \@restonecoltrue
                \fi
                \columnseprule \z@
                \columnsep 35\p@
                \twocolumn[\section*{\indexname}]%
                \@mkboth{\MakeUppercase\indexname}%
                        {\MakeUppercase\indexname}%
                \thispagestyle{plain}\parindent\z@
                \parskip\z@ \@plus .3\p@\relax
                \let\item\@idxitem}
               {\if@restonecol\onecolumn\else\clearpage\fi}
\renewcommand\@idxitem{\par\hangindent 40\p@}
\renewcommand\subitem{\@idxitem \hspace*{20\p@}}
\renewcommand\subsubitem{\@idxitem \hspace*{30\p@}}
\renewcommand\indexspace{\par \vskip 10\p@ \@plus5\p@ \@minus3\p@\relax}

%
%%%%%%%%%%%%%%%%%%%%%%% Footnotes %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

\renewcommand\footnoterule{\bgroup%
    \kern-3\p@
    \hrule \@height 0.5bp \@width 172bp
    \kern2.65\p@\egroup}%

\def\@hangfrom#1{\setbox\@tempboxa\hbox{{#1}}%
      \hangindent \wd\@tempboxa\noindent\box\@tempboxa}

\def\@makefnmark{\hbox{\textsuperscript{\fontsize{7.5}{9.5}\selectfont\@thefnmark}}}

\renewcommand\@makefntext[1]{%
    \parindent 10\p@%
    \vskip2.4pt%
    \noindent\@hangfrom{\hskip3pt\hbox{\smash{\@makefnmark}\hskip5pt}}%
    #1\vphantom{py}\par}
%
\AtBeginDocument{%
\@ifpackageloaded{hyperref}{%
\RequirePackage{hypcap}%
\renewcommand\UrlFont{\rmfamily}%
}{}%
%
\@ifpackageloaded{natbib}{%
\bibsep=\z@
  \renewcommand\bibsection{%
   \vskip7.27pt\noindent{\fontsize{11pt}{14}\scshape\selectfont\centerline{\refname}}\vspace*{-1.08pt}
  }%
}{}%
}%

\def\doi#1{\quad\url{http://dx.doi.org/#1}}
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

\pagestyle{headings}
\pagenumbering{arabic}
\sloppy
\frenchspacing
\flushbottom

%%% special parameters for TeX
\linepenalty=100

\RequirePackage{amsthm}
\let\proof\relax
\let\endproof\relax
\newif\ifproofthm
%\newdimen\afterenunheadspace%
%\setlength{\afterenunheadspace}{9.27pt plus2pt minus2pt}
\newdimen\afterproofheadspace%
\setlength{\afterproofheadspace}{9.81pt}
%
\def\@endtheorem{\ifproofthm\qed\global\proofthmfalse\fi\endtrivlist\@endpefalse}
%\def\@endtheorem{\ifproofthm\global\proofthmfalse\fi\endtrivlist\@endpefalse}

%%%%%%%%%%%%%%%%%% StyleOne Definition
\newtheoremstyle{cupplain}
{6pt plus2pt minus2pt}                  % Space above
{6pt plus2pt minus2pt}                  % Space below
{\normalfont\itshape}           % Body font
{\parindent}            % Indent amount
{\normalfont\scshape}             % Theorem head font
{}                     % Punctuation after theorem head
{9.27pt plus2pt minus2pt}                 % Space after theorem headi
{\@ifempty{#2}{%        % Optional head on unnumbered
    \@ifempty{#3}{\thmname{#1}\thmnumber{\@ifnotempty{#1}{ }\@upn{#2.}}}{\thmnote{{\the\thm@headfont#3}}}%
}{%                     % Main head on numbered
    \thmname{#1}\thmnumber{\@ifnotempty{#1}{ }\@upn{#2.}}\thmnote{ \the\thm@notefont\rm #3}%
}}%
%
%
%%%%%%%%%%%%%%%%%% StyleTwo Definition
\newtheoremstyle{cupdefinition}%
{6pt plus2pt minus2pt}                  % Space above
{6pt plus2pt minus2pt}                  % Space below
{\normalfont}           % Body font
{\parindent}            % Indent amount
{\normalfont\scshape}             % Theorem head font
{}                     % Punctuation after theorem head
{9.27pt plus2pt minus2pt}                 % Space after theorem headi
{\@ifempty{#2}{%        % Optional head on unnumbered
    \@ifempty{#3}{\thmname{#1}\thmnumber{\@ifnotempty{#1}{ }\@upn{#2}}}{\thmnote{{\the\thm@headfont#3}}}%
}{%                     % Main head on numbered
    \thmname{#1}\thmnumber{\@ifnotempty{#1}{ }\@upn{#2}}\thmnote{ (\the\thm@notefont\rm #3).}%
}}%
%
%%%%%%%%%%%%%%%%%% StyleThree Definition
\newtheoremstyle{cupremark}
{6pt plus2pt minus2pt}                  % Space above
{6pt plus2pt minus2pt}                  % Space below
{\normalfont}           % Body font
{\parindent}            % Indent amount
{\normalfont\scshape}             % Theorem head font
{}                     % Punctuation after theorem head
{9.27pt plus2pt minus2pt}                 % Space after theorem headi
{\@ifempty{#2}{%        % Optional head on unnumbered
    \@ifempty{#3}{\thmname{#1}\thmnumber{\@ifnotempty{#1}{ }\@upn{#2.}}}{\thmnote{{\the\thm@headfont#3}}}%
}{%                     % Main head on numbered
    \thmname{#1}\thmnumber{\@ifnotempty{#1}{ }\@upn{#2.}}\thmnote{ \the\thm@notefont\rm #3}%
}}%
%
%%%%%%%%%%%%%%%%%% StyleFour Definition
\newtheoremstyle{cupnote}
{6pt plus2pt minus2pt}                  % Space above
{6pt plus2pt minus2pt}                  % Space below
{\normalfont}           % Body font
{\parindent}            % Indent amount
{\normalfont\scshape}              % Theorem head font
{}                     % Punctuation after theorem head
{9.27pt plus2pt minus2pt}                 % Space after theorem headi
{\@ifempty{#2}{%        % Optional head on unnumbered
    \@ifempty{#3}{\thmname{#1}\thmnumber{\@ifnotempty{#1}{ }\@upn{#2.}}}{\thmnote{{\the\thm@headfont#3}}}%
}{%                     % Main head on numbered
    \thmname{#1}\thmnumber{\@ifnotempty{#1}{ }\@upn{#2.}}\thmnote{ \the\thm@notefont\rm #3}%
}}%
%
%%%%%%%%%%%%%%%%%% StyleFive Definition
\newtheoremstyle{cupproof}
{6pt plus2pt minus2pt}                  % Space above
{6pt plus2pt minus2pt}                  % Space below
{\normalfont}          % Body font
{\parindent}                  % Indent amount
{\itshape}             % Theorem head font
{.}                    % Punctuation after theorem head
{\afterproofheadspace}                % Space after theorem headi
{\global\proofthmtrue
    \@ifempty{#3}{\thmname{#1}}{\thmname{#3}}%
}%

\def\mysquare{\fboxsep0pt\fbox{\vbox to 8.50pt{\hbox to 4.58pt{}}}}
\renewcommand{\qed}{\hfill\mysquare}%\blacksquare



\newcounter{affilcnt}%
\newcounter{affaucnt}%
%
\newcounter{emailcnt}[affilcnt]
%
\newcommand\affiliation[1]{\let\email\affemail\stepcounter{affilcnt}\par{\reset@font\noindent\fontsize{10.91bp}{14bp}\selectfont\itshape\raggedright\noindent #1\par}}
\newcommand{\affauthor}[1]{\let\correspauone\relax\let\correspautwo\relax\let\correspauthree\relax\def\surname##1{##1}\def\authorprint##1{##1}\stepcounter{affaucnt}%
\par\ifnum\theaffaucnt=1\vskip34.1pt\else\vskip13.53pt\fi\noindent{\reset@font\fontsize{10.91bp}{12.5}\selectfont #1\par}}%



\endinput


