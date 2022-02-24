;;;
;;; Copyright © 2020, 2021 Lars-Dominik Braun <ldb@leibniz-psychology.org>
;;; Copyright © 2022 Ricardo Wurmus <rekado@elephly.net>
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guix-science packages cran)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system r)
  #:use-module (gnu packages)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages statistics))

;; Not upstreamable: Bundles lots of JavaScript libraries, minified bootstrap,
;; font-awesome, …
(define-public r-visnetwork
  (package
    (name "r-visnetwork")
    (version "2.0.9")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "visNetwork" version))
        (sha256
          (base32
            "0854r9znpjd9iy6j5bgrn20vj13dhp606gs3b6iy0rhym71ks2sy"))))
    (properties `((upstream-name . "visNetwork")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-htmltools" ,r-htmltools)
        ("r-htmlwidgets" ,r-htmlwidgets)
        ("r-jsonlite" ,r-jsonlite)
        ("r-magrittr" ,r-magrittr)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page "http://datastorm-open.github.io/visNetwork/")
    (synopsis "Network Visualization using vis.js Library")
    (description
      "This package provides an R interface to the vis.js JavaScript charting
library.  It allows an interactive visualization of networks.")
    (license license:expat)))

;; Depends on r-visnetwork
(define-public r-diagrammer
  (package
    (name "r-diagrammer")
    (version "1.0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "DiagrammeR" version))
        (sha256
          (base32
            "0gb7ccdrh7jlyqafdk8zs465ygczxxd25s05whn914in1994qkmy"))))
    (properties `((upstream-name . "DiagrammeR")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-downloader" ,r-downloader)
        ("r-dplyr" ,r-dplyr)
        ("r-glue" ,r-glue)
        ("r-htmltools" ,r-htmltools)
        ("r-htmlwidgets" ,r-htmlwidgets)
        ("r-igraph" ,r-igraph)
        ("r-influencer" ,r-influencer)
        ("r-magrittr" ,r-magrittr)
        ("r-purrr" ,r-purrr)
        ("r-rcolorbrewer" ,r-rcolorbrewer)
        ("r-readr" ,r-readr)
        ("r-rlang" ,r-rlang)
        ("r-rstudioapi" ,r-rstudioapi)
        ("r-scales" ,r-scales)
        ("r-stringr" ,r-stringr)
        ("r-tibble" ,r-tibble)
        ("r-tidyr" ,r-tidyr)
        ("r-viridis" ,r-viridis)
        ("r-visnetwork" ,r-visnetwork)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/rich-iannone/DiagrammeR")
    (synopsis "Graph/Network Visualization")
    (description
      "Build graph/network structures using functions for stepwise addition
and deletion of nodes and edges.  Work with data available in tables for bulk
addition of nodes, edges, and associated metadata.  Use graph selections and
traversals to apply changes to specific nodes or edges.  A wide selection of
graph algorithms allow for the analysis of graphs.  Visualize the graphs and
take advantage of any aesthetic properties assigned to nodes and edges.")
    (license license:expat)))

;; Depends on r-diagrammer
(define-public r-lavaanplot
  (package
    (name "r-lavaanplot")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "lavaanPlot" version))
        (sha256
          (base32
            "0x5iwx8rki17b1cdayjn42zfscdx9bm4m999pzn92l28gf55kmb6"))))
    (properties `((upstream-name . "lavaanPlot")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-diagrammer" ,r-diagrammer)
        ("r-lavaan" ,r-lavaan)
        ("r-stringr" ,r-stringr)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/alishinski/lavaanPlot")
    (synopsis "Path Diagrams for Lavaan Models via DiagrammeR")
    (description
      "Plots path diagrams from models in lavaan using the plotting
functionality from the DiagrammeR package.  DiagrammeR provides nice path
diagrams via Graphviz, and these functions make it easy to generate these
diagrams from a lavaan path model without having to write the DOT language
graph specification.")
    (license license:gpl2+)))

;; Depends on r-diagrammer.
(define-public r-piecewisesem
  (package
    (name "r-piecewisesem")
    (version "2.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "piecewiseSEM" version))
        (sha256
          (base32
            "1qxwc1drx26p4cgymrfjmg5i7ypl7gddv40virkpmc0qaag9pq36"))))
    (properties `((upstream-name . "piecewiseSEM")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-car" ,r-car)
        ("r-diagrammer" ,r-diagrammer)
        ("r-emmeans" ,r-emmeans)
        ("r-igraph" ,r-igraph)
        ("r-lme4" ,r-lme4)
        ("r-mass" ,r-mass)
        ("r-multcomp" ,r-multcomp)
        ("r-nlme" ,r-nlme)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page "http://jslefche.github.io/piecewiseSEM/")
    (synopsis
      "Piecewise Structural Equation Modeling")
    (description
      "Implements piecewise structural equation modeling from a single list of
structural equations, with new methods for non-linear, latent, and composite
variables, standardized coefficients, query-based prediction and indirect
effects.")
    (license license:gpl3)))

;; This contains a lot of minified JavaScript with no obvious source
;; files.
(define-public r-shinywidgets
  (package
    (name "r-shinywidgets")
    (version "0.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "shinyWidgets" version))
       (sha256
        (base32 "1j7pii40zc60ny1d5027pjagi1rrcnb9kxn4q9jmd9xv84y5b8sj"))))
    (properties `((upstream-name . "shinyWidgets")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bslib r-htmltools r-jsonlite r-sass r-shiny))
    (home-page "https://github.com/dreamRs/shinyWidgets")
    (synopsis "Custom inputs widgets for Shiny")
    (description
     "This package provides a collection of custom input controls and user
interface components for Shiny applications.")
    (license license:gpl3)))

;; Not upstreamable, contains precompiled jars.
(define-public r-xlsxjars
  (package
    (name "r-xlsxjars")
    (version "0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "xlsxjars" version))
        (sha256
          (base32
            "1rka5smm7yqnhhlblpihhciydfap4i6kjaa4a7isdg7qjmzm3h9p"))))
    (properties `((upstream-name . "xlsxjars")))
    (build-system r-build-system)
    (propagated-inputs `(("r-rjava" ,r-rjava)))
    (home-page
      "https://cran.r-project.org/web/packages/xlsxjars")
    (synopsis
      "Package required POI jars for the xlsx package")
    (description
      "The xlsxjars package collects all the external jars required for the
xlxs package.  This release corresponds to POI 3.10.1.")
    (license license:gpl3)))

;; Depends on r-xlsxjars
(define-public r-xlsx
  (package
    (name "r-xlsx")
    (version "0.6.5")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "xlsx" version))
        (sha256
          (base32
            "01r1ngdm51w18bdan8h94r91m731knkf04zal4g67mx3fpa5x31p"))))
    (properties `((upstream-name . "xlsx")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-rjava" ,r-rjava) ("r-xlsxjars" ,r-xlsxjars)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/colearendt/xlsx")
    (synopsis
      "Read, Write, Format Excel 2007 and Excel 97/2000/XP/2003 Files")
    (description
      "Provide R functions to read/write/format Excel 2007 and Excel
97/2000/XP/2003 file formats.")
    (license license:gpl3)))

;; Bundles udpipe, which is very hard to build, because it bundles many
;; libraries itself.
(define-public r-udpipe
  (package
    (name "r-udpipe")
    (version "0.8.5")
    (source
     (origin
      (method url-fetch)
      (uri (cran-uri "udpipe" version))
      (sha256
       (base32 "021n28jncfiv7492dj1ik6ylkhb3s2hpgjpc0y2zv4cdnl362zcx"))))
    (properties `((upstream-name . "udpipe")))
    (build-system r-build-system)
    ;(inputs `(("udpipe" ,udpipe)))
    (propagated-inputs
      `(("r-data-table" ,r-data-table)
        ("r-matrix" ,r-matrix)
        ("r-rcpp" ,r-rcpp)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page
      "https://bnosac.github.io/udpipe/en/index.html")
    (synopsis
      "R bindings for UDPipe NLP toolkit")
    (description
      "This natural language processing toolkit provides language-agnostic
'tokenization', 'parts of speech tagging', 'lemmatization' and 'dependency
parsing' of raw text.  Next to text parsing, the package also allows you to
train annotation models based on data of 'treebanks' in 'CoNLL-U' format as
provided at @url{https://universaldependencies.org/format.html}.  The techniques
are explained in detail in the paper: 'Tokenizing, POS Tagging, Lemmatizing and
Parsing UD 2.0 with UDPipe', available at @url{doi:10.18653/v1/K17-3009}.  The
toolkit also contains functionalities for commonly used data manipulations on
texts which are enriched with the output of the parser.  Namely functionalities
and algorithms for collocations, token co-occurrence, document term matrix
handling, term frequency inverse document frequency calculations, information
retrieval metrics (Okapi BM25), handling of multi-word expressions, keyword
detection (Rapid Automatic Keyword Extraction, noun phrase extraction,
syntactical patterns) sentiment scoring and semantic similarity analysis.")
    (license license:mpl2.0)))

;; Cannot upstream, because we are using a git version and the project
;; is kind of “special interest”, since its only purpose is to talk
;; to the formr.org web service.
(define-public r-formr
  (let ((commit "8f77582a70781f2ccb184e967ed120452478e0b5"))
    (package
     (name "r-formr")
     (version (string-append "0.7.4-" (string-take commit 7)))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rubenarslan/formr.git")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wjfrr37k4sj1fvny245skz2cnfy7mhl0kk88j07wsf4z70dvsvf"))))
     (build-system r-build-system)
     (propagated-inputs
       (list
         r-dplyr
         r-ggplot2
         r-scales
         r-haven
         r-tidyr
         r-knitr
         r-httr
         r-curl
         r-jsonlite
         r-lubridate
         r-commonmark
         r-rmarkdown
         r-keyring))
     (home-page
       "https://formr.org/")
     (synopsis
       "Helper functions for formr survey framework")
     (description
       "The formr R package provides a few convenience functions
that may be useful to the users of formr (formr.org), an online
survey framework which heavily relies on R via openCPU.
Some of the functions are for conveniently generating individual
feedback graphics, some are just shorthands to make certain common
operations in formr more palatable to R novices.")
     (license license:bsd-2))))

