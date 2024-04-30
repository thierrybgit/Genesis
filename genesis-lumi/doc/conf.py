# -*- coding: utf-8 -*-
#
# GENESIS documentation build configuration file, created by
# sphinx-quickstart on Wed Feb 22 10:21:04 2017.
#
# This file is execfile()d with the current directory set to its
# containing dir.
#
# Note that not all possible configuration values are present in this
# autogenerated file.
#
# All configuration values have a default; values that are commented out
# serve to show the default.

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
import os
import sys
# sys.path.insert(0, os.path.abspath('.'))


# -- General configuration ------------------------------------------------

# If your documentation needs a minimal Sphinx version, state it here.
#
# needs_sphinx = '1.0'

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = ['sphinx.ext.imgmath', 'sphinxcontrib.bibtex']
bibtex_bibfiles = ['refs.bib']

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# The suffix(es) of source filenames.
# You can specify multiple suffix as a list of string:
#
# source_suffix = ['.rst', '.md']
source_suffix = '.rst'

# The master toctree document.
master_doc = 'index'

# General information about the project.
project = u'GENESIS'
copyright = u'2015-2023'
author = u'RIKEN'

# The version info for the project you're documenting, acts as replacement for
# |version| and |release|, also used in various other places throughout the
# built documents.
#
# The short X.Y version.
version = u'2.1.1'
# The full version, including alpha/beta/rc tags.
release = u'2.1.1'

# The language for content autogenerated by Sphinx. Refer to documentation
# for a list of supported languages.
#
# This is also used if you do content translation via gettext catalogs.
# Usually you set "language" from the command line for these cases.
language = None

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This patterns also effect to html_static_path and html_extra_path
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']

# The name of the Pygments (syntax highlighting) style to use.
#pygments_style = 'sphinx'
pygments_style = 'bw'

# If true, `todo` and `todoList` produce output, else they produce nothing.
todo_include_todos = False

# Put numbers to figures
numfig = True

# -- Options for HTML output ----------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
html_theme = 'alabaster'

# Theme options are theme-specific and customize the look and feel of a theme
# further.  For a list of options available for each theme, see the
# documentation.
#
# html_theme_options = {}

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ['_static']


# -- Options for HTMLHelp output ------------------------------------------

# Output file base name for HTML help builder.
htmlhelp_basename = 'GENESISdoc'


# -- Options for LaTeX output ---------------------------------------------

latex_elements = {
    # The paper size ('letterpaper' or 'a4paper').
    #
    # 'papersize': 'letterpaper',
    'papersize': 'a4paper',

    # The font size ('10pt', '11pt' or '12pt').
    #
    # 'pointsize': '10pt',
    'pointsize': '11pt',

    # Additional stuff for the LaTeX preamble.
    #
    # 'preamble': '',
    'preamble': r'''
\renewcommand{\releasename}{Version}
\let\OrigVerbatim\OriginalVerbatim
\renewcommand{\OriginalVerbatim}[1][1]{\OrigVerbatim[#1,frame=single]}
\pagestyle{normal}
\thispagestyle{normal}
\pagenumbering{arabic}
\usepackage{braket}
''',

    # Latex figure (float) alignment
    #
    # 'figure_align': 'htbp',

    # remove blank page
    'classoptions': ',oneside',
    'babel': '\\usepackage[english]{babel}',
    'date': '2017/02/21',

    'tableofcontents': r'''
%%%%%%%
\begin{center}
{\Huge GENESIS 2.1.1}
\end{center}

\vspace{2ex}

\begin{quote}
  Project Leader: Yuji Sugita (RIKEN)

  Current main developers:
  Jaewoon Jung (RIKEN), 
  Shingo Ito (RIKEN),
  Chigusa Kobayashi (RIKEN), 
  Takaharu Mori (RIKEN), 
  Hiraku Oshima (RIKEN), 
  Cheng Tan (RIKEN),
  Diego Ugarte (RIKEN),
  Kiyoshi Yagi (RIKEN)

  Other developers/contributors for older versions:
  Motoshi Kamiya (RIKEN/IMS), 
  Kento Kasahara (RIKEN/Osaka Univ.),
  Yasuhiro Matsunaga (RIKEN/Saitama Univ.),
  Daisuke Matsuoka (RIKEN/RIST), 
  Osamu Miyashita (RIKEN), 
  Suyong Re (RIKEN/NIBIOHN),
  Ai Shinobu (RIKEN), 
  Yosuke Sumiya (RIKEN),
  Florence Tama (RIKEN/Nagoya Univ.),
  Shoji Takada (Kyoto Univ.),
  Isseki Yu (RIKEN/Maebashi Institute of Technology),
  Tadashi Ando (RIKEN),
  Michael Feig (Michigan State University), 
  Raimondas Galvelis (RIKEN),
  Ryuhei Harada (RIKEN), 
  Takashi Imai (RIKEN), 
  Yasuaki Komuro (RIKEN), 
  Yasuhito Karino (RIKEN),
  Naoyuki Miyashita (RIKEN), 
  Wataru Nishima (RIKEN),
  Donatas Surblys (RIKEN), 
  Koichi Tamura (RIKEN), 
  Kenta Yamada (RIKEN),
  Takao Yoda (Nagahama Institute of Bio-Science and Technology)

  Acknowledgments: 
  Norio Takase (Isogo Soft), 
  Yasumasa Joti (RIKEN SPring8),
  Akira Naruse (NVIDIA), 
  Yukihiko Hirano (NVIDIA Japan),
  Hikaru Inoue (Fujitsu Ltd.), 
  Tomoyuki Noda (Fujitsu Ltd.),
  Kiyotaka Sakamoto (Fujitsu Ltd.), 
  Yoshinobu Akinaga (VINAS),
  Yoshitake Sakae (RIST),
  Nobuhiko Kato (ASTOM R\&D),
  Toru Shiozaki (QSimulate), 
  Klaas Gunst (QSimulate),
  Hideyo Yoshida (JSOL Corporation),
  Kenta Chaki (JSOL Corporation)

  Copyright \copyright 2014-2023 RIKEN. All Rights Reserved
\end{quote}

\vspace{3ex}

%%%%%%%
{\LARGE GENESIS website}

\begin{quote}
  \url{https://www.r-ccs.riken.jp/labs/cbrt/}
\end{quote}

\vspace{3ex}

%%%%%%%
{\LARGE Citation Information}

\begin{itemize}
 \item C. Kobayashi, J. Jung, Y. Matsunaga, T. Mori, T. Ando, K. Tamura, M. Kamiya, and Y. Sugita, "GENESIS 1.1: A hybrid-parallel molecular dynamics simulator with enhanced sampling algorithms on multiple computational platforms", J. Comput. Chem. 38, 2193-2206 (2017).

 \item J. Jung, T. Mori, C. Kobayashi, Y. Matsunaga, T. Yoda, M. Feig, and Y. Sugita, "GENESIS: A hybrid-parallel and multi-scale molecular dynamics simulator with enhanced sampling algorithms for biomolecular and cellular simulations", WIREs Computational Molecular Science 5, 310-323 (2015).
\end{itemize}

\vspace{3ex}

%%%%%%%
{\LARGE Copyright Notices}

\begin{quote}
  Copyright \copyright 2014-2023 RIKEN.

  GENESIS is free software; you can redistribute it and/or
  modify it provided that the following conditions are met:

  \begin{enumerate}
  \item All publications and commercial products using this software 
  should cite the above references, Jung et al (2015) and Kobayashi et al 
  (2017). 
  \item In addition, proper citations should be given for each specific method 
  used. Visit the website, 

  \url{https://www.r-ccs.riken.jp/labs/cbrt/citations/} 

  to see how to cite the original papers.

  \item We ask the users to make their best effort to cite the papers in the 
  \textbf{main text}. Although it is permitted to cite some of the papers in 
  the supporting information / supplementary materials due to the limitation of 
  article length, the name of the software, GENESIS, and at least one of the 
  papers should appear in the main text.
  \end{enumerate}

  GENESIS is released under the terms of the GNU Lesser General Public 
  License as published by the Free Software Foundation; either 
  version 3 of the License, or (at your option) any later version.
  
  GENESIS is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Lesser General Public License for more details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with GENESIS -- see the file COPYING and COPYING.LESSER.
  If not, see \url{https://www.gnu.org/licenses/}.
 
\end{quote}

It should be mentioned this package contains the following softwares
for convenience.  Please note that these are not covered by the license
under which a copy of GENESIS is licensed to you, while neither
composition nor distribution of any derivative work of GENESIS with
these software violates the terms of each license, provided that it meets
every condition of the respective licenses.

\vspace{1ex}

{\large SIMD-oriented Fast Mersenne Twister (SFMT)}

SFMT is a new variant of Mersenne Twister (MT) introduced by Mutsuo Saito and Makoto Matsumoto in 2006. The algorithm was reported at MCQMC 2006.
The routine is distributed under the New BSD License.

\begin{quote}
  Copyright \copyright 2006,2007 Mutsuo Saito, Makoto Matsumoto and Hiroshima University.
  Copyright \copyright 2012 Mutsuo Saito, Makoto Matsumoto, Hiroshima University and The University of Tokyo.
  All rights reserved.

  Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

  * Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.

  * Redistributions in binary form must reproduce the above
  copyright notice, this list of conditions and the following
  disclaimer in the documentation and/or other materials provided
  with the distribution.

  * Neither the names of Hiroshima University, The University of
  Tokyo nor the names of its contributors may be used to endorse
  or promote products derived from this software without specific
  prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
\end{quote}

%\vspace{2ex}
%
%{\large Mersenne Twister: A random number generator}
%
%A Mersenne Twister random number generator was originally written in C
%by Makoto Matsumoto and Takuji Nishimura, and later translated into
%Fortran by Hiroshi Takano and Richard Woloshyn. This routine is
%distributed under the GNU General Public License version 2.
%
%\begin{quote}
%  Copyright \copyright 1997 Makoto Matsumoto and Takuji Nishimura.
%
%  Copyright \copyright 1999 Hiroshi Takano.
%
%  Copyright \copyright 1999 Richard Woloshyn.
%
%  This library is free software; you can redistribute it and/or
%  modify it under the terms of the GNU Library General Public
%  License as published by the Free Software Foundation; either
%  version 2 of the License, or (at your option) any later
%  version.
%
%  This library is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
%  See the GNU Library General Public License for more details.
%  You should have received a copy of the GNU Library General
%  Public License along with this library; if not, write to the
%  Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%  02111-1307  USA
%\end{quote}

\vspace{2ex}

{\large FFTE: A Fast Fourier Transform Package}

FFTE (\url{http://www.ffte.jp/}) is written by Daisuke Takahashi (Tsukuba
University). 

\begin{quote}
  Copyright \copyright 2000-2004, 2008-2011 Daisuke Takahashi (Tsukuba
  University). 

  You may use, copy, modify this code for any purpose (include
  commercial use) and without fee. You may distribute this ORIGINAL
  package. 
\end{quote}

\vspace{2ex}

{\large Complementary error function: erfc04}

A Complementary error function routine (erfc04) is written by SunSoft, 
a Sun Microsystems, Inc. business. 

\begin{quote}
  Copyright \copyright 1993 Sun Microsystems, Inc.

  Developed at SunSoft, a Sun Microsystems, Inc. business. 
  Permission to use, copy, modify, and distribute this 
  software is freely granted, provided that this notice 
  is preserved (see math\_libs.fpp).
\end{quote}

\vspace{2ex}

{\large L-BFGS-B (version 3.0)}

L-BFGS-B (\url{http://users.iems.northwestern.edu/~nocedal/lbfgsb.html})
is written by C. Zhu, R. Byrd, J. Nocedal and J. L. Morales. 

\begin{quote}
   This software is freely available, but we expect that all publications 
   describing work using this software, or all commercial products using 
   it, quote at least one of the references given below. This software is 
   released under the "New BSD License" (aka "Modified BSD License" or 
   "3-clause license"). 

   R. H. Byrd, P. Lu and J. Nocedal. A Limited Memory Algorithm for Bound Constrained Optimization, (1995), SIAM Journal on Scientific and Statistical Computing, 16, 5, pp. 1190-1208.

   C. Zhu, R. H. Byrd and J. Nocedal. L-BFGS-B: Algorithm 778: L-BFGS-B, FORTRAN routines for large scale bound constrained optimization (1997), ACM Transactions on Mathematical Software, Vol 23, Num. 4, pp. 550-560.

   J.L. Morales and J. Nocedal. L-BFGS-B: Remark on Algorithm 778: L-BFGS-B, FORTRAN routines for large scale bound constrained optimization (2011), ACM Transactions on Mathematical Software, Vol 38, Num. 1, Article No. 7.

\end{quote}

\vspace{2ex}

{\large JSON-Fortran (version 8.2.5)}

JSON-Fortran (\url{https://github.com/jacobwilliams/json-fortran})
developed by J. Williams, 
is a user-friendly, thread-safe, and object-oriented API for reading and 
writing JSON files, written in modern Fortran

\begin{quote}
   The JSON-Fortran source code and related files and documentation are distributed 
   under a permissive free software license (BSD-style). See the LICENSE file 
   (src/lib/json-fortran/LICENSE) for more details.
\end{quote}

\vspace{3ex}

\tableofcontents
''',
}

# Grouping the document tree into LaTeX files. List of tuples
# (source start file, target name, title,
#  author, documentclass [howto, manual, or own class]).
#latex_documents = [
#    (master_doc, 'GENESIS.tex', u'GENESIS Documentation',
#     u'RIKEN', 'manual'),
#]
latex_documents = [
    (master_doc, 'GENESIS.tex', '',
     u'RIKEN', 'manual'),
]

latex_toplevel_sectioning = 'chapter'
latex_show_urls = 'inline'
latex_show_pagerrefs = False


# -- Options for manual page output ---------------------------------------

# One entry per manual page. List of tuples
# (source start file, name, description, authors, manual section).
man_pages = [
    (master_doc, 'genesis', u'GENESIS Documentation',
     [author], 1)
]


# -- Options for Texinfo output -------------------------------------------

# Grouping the document tree into Texinfo files. List of tuples
# (source start file, target name, title, author,
#  dir menu entry, description, category)
texinfo_documents = [
    (master_doc, 'GENESIS', u'GENESIS Documentation',
     author, 'GENESIS', 'One line description of project.',
     'Miscellaneous'),
]


