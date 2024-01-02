;;;
;;; Copyright © 2020, 2021 Lars-Dominik Braun <ldb@leibniz-psychology.org>
;;; Copyright © 2022-2024, Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system r)
  #:use-module (gnu packages)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages web))

;; This contains minified JavaScript
(define-public r-bs4dash
  (package
    (name "r-bs4dash")
    (version "2.3.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "bs4Dash" version))
              (sha256
               (base32
                "09dzcpqzlkak9ilx79h2hpad20ck6857middbkn7j3mhrbqq0m51"))))
    (properties `((upstream-name . "bs4Dash")))
    (build-system r-build-system)
    (propagated-inputs (list r-bslib
                             r-fresh
                             r-htmltools
                             r-httpuv
                             r-httr
                             r-jsonlite
                             r-lifecycle
                             r-shiny
                             r-waiter))
    (native-inputs (list r-knitr))
    (home-page "https://rinterface.github.io/bs4Dash/index.html")
    (synopsis "Bootstrap 4 version of shinydashboard")
    (description
     "Make Bootstrap 4 Shiny dashboards.  Use the full power of
AdminLTE3, a dashboard template built on top of Bootstrap 4.")
    (license license:gpl2+)))

;; TODO: building these from source is difficult because of npm.
;; swagger/inst/dist3/swagger-ui.js
;; swagger/inst/dist3/swagger-ui-standalone-preset.js
;; swagger/inst/dist3/swagger-ui-bundle.js
(define-public r-swagger
  (package
    (name "r-swagger")
    (version "3.33.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "swagger" version))
              (sha256
               (base32
                "1ldgmy5vjzd11z5yl5a518wkw6y0l469b2zf0lp12hk19jq6k0sj"))))
    (properties `((upstream-name . "swagger")))
    (build-system r-build-system)
    (home-page "https://github.com/rstudio/swagger")
    (synopsis "Dynamically generate documentation from a Swagger compliant API")
    (description
     "This package provides a collection of HTML, JavaScript, and CSS assets
that dynamically generate beautiful documentation from a
@url{https://swagger.io/specification/,Swagger compliant API}.")
    (license license:asl2.0)))

;; TODO: unbundle these minified JavaScript files
;; Viz.js 1.8.2 (Graphviz 2.40.1, Expat 2.2.5, Emscripten 1.37.33)
;;   htmlwidgets/lib/viz/viz.js
(define-public r-diagrammer
  (package
    (name "r-diagrammer")
    (version "1.0.10")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "DiagrammeR" version))
       (sha256
        (base32
         "147q7zgwhd7vc0l134sqkkf6n6s6bznxvcmsrdx2f5df12bsixkj"))
       (snippet
        '(for-each delete-file
                   (list "inst/htmlwidgets/lib/d3/d3.min.js"
                         "inst/htmlwidgets/lib/dagre-d3/dagre-d3.min.js"
                         "inst/htmlwidgets/lib/mermaid/dist/mermaid.slim.min.js")))))
    (properties `((upstream-name . "DiagrammeR")))
    (build-system r-build-system)
    (arguments
     (list
      #:modules
      '((guix build r-build-system)
        (guix build minify-build-system)
        (guix build utils)
        (ice-9 match))
      #:imported-modules
      `(,@%r-build-system-modules
        (guix build minify-build-system))
      #:phases
      #~(modify-phases (@ (guix build r-build-system) %standard-phases)
          (add-after 'unpack 'replace-bundled-minified-JavaScript
            (lambda* (#:key inputs #:allow-other-keys)
              (with-directory-excursion "inst/htmlwidgets/lib"
                (for-each
                 (match-lambda
                   ((source . target)
                    (minify source #:target target)))
                 `((,(assoc-ref inputs "d3.v3.js")
                    . "d3/d3.min.js")
                   (,(search-input-file inputs "/dist/dagre-d3.js")
                    . "dagre-d3/dagre-d3.min.js")
                   (,(search-input-file inputs "/dist/mermaid.slim.js")
                    . "mermaid/dist/mermaid.slim.min.js")))))))))
    (propagated-inputs
     (list r-downloader
           r-dplyr
           r-glue
           r-htmltools
           r-htmlwidgets
           r-igraph
           r-magrittr
           r-purrr
           r-rcolorbrewer
           r-readr
           r-rlang
           r-rstudioapi
           r-scales
           r-stringr
           r-tibble
           r-tidyr
           r-viridis
           r-visnetwork))
    (native-inputs
     `(("esbuild" ,esbuild)
       ("r-knitr" ,r-knitr)
       ("d3.v3.js"
        ,(origin
           (method url-fetch)
           (uri "https://d3js.org/d3.v3.js")
           (sha256
            (base32
             "1arr7sr08vy7wh0nvip2mi7dpyjw4576vf3bm45rp4g5lc1k1x41"))))
       ;; dagre-d3 was added about 10 years ago.  Version 0.3.1 was
       ;; current at around that time.
       ("dagre-d3"
        ,(let ((version "0.3.1"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/dagrejs/dagre-d3/")
                   (commit (string-append "v" version))))
             (file-name (git-file-name "dagre-d3" version))
             (sha256
              (base32
               "0ys5ryamibbazrkb82ir72qfna66ajhg4n6jh9vk9m1z5qi59ahs")))))
       ;; DiagrammeR keeps trying to upgrade mermaid but they keep
       ;; reverting back to version 7.0.0.
       ("mermaid.js"
        ,(let ((version "7.0.0"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/mermaid-js/mermaid/")
                   (commit version)))
             (file-name (git-file-name "mermaid" version))
             (sha256
              (base32
               "0ihgmhpwy19ijngya707pcgaj83ja73ylbdx4wqf9drq8wkq7sza")))))))
    (home-page "https://github.com/rich-iannone/DiagrammeR")
    (synopsis "Graph/network visualization")
    (description
     "With the @code{DiagrammeR} package you can create, modify,
analyze, and visualize network graph diagrams.  The output can be
incorporated into @code{R Markdown} documents, integrated with Shiny
web apps, converted to other graph formats, or exported as image
files.")
    (license license:expat)))

;; Depends on r-diagrammer
(define-public r-lavaanplot
  (package
    (name "r-lavaanplot")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "lavaanPlot" version))
       (sha256
        (base32
         "03icyydr6sv4jkdfdjchsdrscr9lz9q74x5q17cx2iw3383j6lz5"))))
    (properties `((upstream-name . "lavaanPlot")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-diagrammer
           r-lavaan
           r-magrittr
           r-stringr))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/alishinski/lavaanPlot")
    (synopsis "Path Diagrams for Lavaan Models via DiagrammeR")
    (description
     "This package plots path diagrams from models in lavaan using the
plotting functionality from the DiagrammeR package.  DiagrammeR
provides nice path diagrams via Graphviz, and these functions make it
easy to generate these diagrams from a lavaan path model without
having to write the DOT language graph specification.")
    (license license:gpl2+)))

;; Depends on r-diagrammer.
(define-public r-piecewisesem
  (package
    (name "r-piecewisesem")
    (version "2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "piecewiseSEM" version))
       (sha256
        (base32
         "0f1ya6p85iai52cdm510zm6qvkpsfkn48948q8lz9v6ykz59v5h2"))))
    (properties `((upstream-name . "piecewiseSEM")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-car
           r-diagrammer
           r-emmeans
           r-igraph
           r-lme4
           r-mass
           r-multcomp
           r-mumin
           r-nlme
           r-performance))
    (native-inputs (list r-knitr))
    (home-page "http://jslefche.github.io/piecewiseSEM/")
    (synopsis "Piecewise structural equation modeling")
    (description
     "This package implements piecewise structural equation modeling
from a single list of structural equations, with new methods for
non-linear, latent, and composite variables, standardized
coefficients, query-based prediction and indirect effects.")
    (license license:gpl3)))

;; This depends on r-swagger which contains a lot of minified JS.
(define-public r-plumber
  (package
    (name "r-plumber")
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "plumber" version))
              (sha256
               (base32
                "1k0y7ylc8bld16imn86g0i0dmxmr3kmh9ax4ys0yrxqzrvji7z3g"))))
    (properties `((upstream-name . "plumber")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-crayon
           r-ellipsis
           r-httpuv
           r-jsonlite
           r-lifecycle
           r-magrittr
           r-mime
           r-promises
           r-r6
           r-rlang
           r-sodium
           r-stringi
           r-swagger
           r-webutils))
    (home-page "https://www.rplumber.io")
    (synopsis "API generator for R")
    (description
     "This package gives the ability to automatically generate and
serve an HTTP API from R functions using the annotations in the R
documentation around your functions.")
    (license license:expat)))

;; This contains a lot of minified JavaScript with no obvious source
;; files.
(define-public r-shinywidgets
  (package
    (name "r-shinywidgets")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "shinyWidgets" version))
       (sha256
        (base32 "1c5667ndrav8rfcd5cm2bwp21hy7msgnc3s5n4kzyijgdacfbs0s"))))
    (properties `((upstream-name . "shinyWidgets")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-anytime
           r-bslib
           r-htmltools
           r-jsonlite
           r-rlang
           r-sass
           r-shiny))
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
    (propagated-inputs (list r-rjava))
    (home-page "https://cran.r-project.org/web/packages/xlsxjars")
    (synopsis "POI jars for the xlsx package")
    (description
     "The xlsxjars package collects all the external jars required for
the xlxs package.  This release corresponds to POI 3.10.1.")
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
     (list r-rjava r-xlsxjars))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/colearendt/xlsx")
    (synopsis "Read, write, format Excel 2007 and Excel 97/2000/XP/2003 files")
    (description
     "This package provides R functions to read/write/format Excel
2007 and Excel 97/2000/XP/2003 file formats.")
    (license license:gpl3)))

;; Bundles udpipe, which is very hard to build, because it bundles many
;; libraries itself.
(define-public r-udpipe
  (package
    (name "r-udpipe")
    (version "0.8.11")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "udpipe" version))
       (sha256
        (base32 "0crjcfrpb0m7f58w7ksz7kvglvmc45axy9kbbvqz9w6i4kg00aaj"))))
    (properties `((upstream-name . "udpipe")))
    (build-system r-build-system)
    ;;(inputs `(("udpipe" ,udpipe)))
    (propagated-inputs
     (list r-data-table
           r-matrix
           r-rcpp))
    (native-inputs (list r-knitr))
    (home-page "https://bnosac.github.io/udpipe/en/index.html")
    (synopsis "R bindings for UDPipe NLP toolkit")
    (description
     "This natural language processing toolkit provides language-agnostic
tokenization, parts of speech tagging, lemmatization and dependency
parsing of raw text.  Next to text parsing, the package also allows you to
train annotation models based on data of treebanks in CoNLL-U format as
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

;; Contains minified JavaScript
(define-public r-reactr
  (package
    (name "r-reactr")
    (version "0.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "reactR" version))
       (sha256
        (base32 "15c0ij3xglsdavkpzgnkbyl5yb39jrn6zhlrdjiwp1m9cnrqzf2w"))))
    (properties `((upstream-name . "reactR")))
    (build-system r-build-system)
    (propagated-inputs (list r-htmltools))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/react-R/reactR")
    (synopsis "React helpers")
    (description
     "This package makes it easy to use React in R with htmlwidget
scaffolds, helper dependency functions, an embedded Babel transpiler,
and examples.")
    (license license:expat)))

;; Contains minified JavaScript
(define-public r-reactable
  (package
    (name "r-reactable")
    (version "0.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "reactable" version))
       (sha256
        (base32 "1wkamzyyl3k3772n5g4rjklkkhdb07jiax064r9alnnq5nzfdaml"))))
    (properties `((upstream-name . "reactable")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-digest r-htmltools r-htmlwidgets r-jsonlite r-reactr))
    (home-page "https://glin.github.io/reactable/")
    (synopsis "Interactive data tables based on React Table'")
    (description
     "This package provides interactive data tables for R, based on
the React Table JavaScript library.  It provides an HTML widget that
can be used in R Markdown documents and Shiny applications, or viewed
from an R console.")
    (license license:expat)))

;; Dependency of r-papaja
(define-public r-rmdfiltr
  (package
    (name "r-rmdfiltr")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "rmdfiltr" version))
        (sha256
          (base32 "0llnn4pdfznidalm4f7fpyxbhzsqv1096fkbsl1pgf4f7ll6w7a7"))))
    (properties `((upstream-name . "rmdfiltr")))
    (build-system r-build-system)
    (inputs (list pandoc))
    (propagated-inputs (list r-assertthat r-rmarkdown))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/crsh/rmdfiltr")
    (synopsis "'Lua'-Filters for R Markdown")
    (description
      "This package provides a collection of 'Lua' filters that extend the
functionality of R Markdown templates (e.g., count words or post-process
citations).")
    (license license:expat)))

;; Dependency of r-papaja
(define-public r-tinylabels
  (package
    (name "r-tinylabels")
    (version "0.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "tinylabels" version))
        (sha256
          (base32 "01pvnirma3wzfqnnz8zvyqajjyysjm3sd6813bgdhz199lcg7hhz"))))
    (properties `((upstream-name . "tinylabels")))
    (build-system r-build-system)
    (native-inputs (list r-knitr))
    (home-page "https://github.com/mariusbarth/tinylabels")
    (synopsis "Lightweight Variable Labels")
    (description
      "Assign, extract, or remove variable labels from R vectors.  Lightweight and
dependency-free.")
    (license license:expat)))

(define-public r-papaja
  (package
    (name "r-papaja")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "papaja" version))
        (sha256
          (base32 "08ydk9fws5x37q120cdjdx1dsib0y2g9bp826vxzxcaymlwx3dl7"))))
    (properties `((upstream-name . "papaja")))
    (build-system r-build-system)
    (propagated-inputs
      (list r-bookdown
            r-broom
            r-glue
            r-knitr
            r-rmarkdown
            r-rmdfiltr
            r-tinylabels
            r-yaml
            r-zip
            texlive-amsmath
            texlive-apa6
            texlive-booktabs
            texlive-caption
            texlive-csquotes
            texlive-endfloat
            texlive-environ
            texlive-etoolbox
            texlive-fancyhdr
            texlive-framed
            texlive-geometry
            texlive-graphics
            texlive-grfext
            texlive-hyperref
            texlive-lineno
            texlive-multirow
            texlive-threeparttable
            texlive-threeparttablex
            texlive-titlesec
            texlive-tools
            texlive-trimspaces
            texlive-was ; upgreek.sty
            texlive-xcolor
            texlive-xpatch
            (texlive-updmap.cfg (list
                                 texlive-amsfonts
                                 texlive-times
                                 texlive-lm))))
    (native-inputs (list r-knitr r-r-rsp))
    (home-page "https://github.com/crsh/papaja")
    (synopsis
      "Prepare American Psychological Association Journal Articles with R Markdown")
    (description
      "Tools to create dynamic, submission-ready manuscripts, which conform to American
ychological Association manuscript guidelines.  We provide R Markdown document
rmats for manuscripts (PDF and Word) and revision letters (PDF).  Helper
nctions facilitate reporting statistical analyses or create publication-ready
bles and plots.")
    (license license:expat)))

;; TODO: unbundle minified javascript.  Sources are in the git repo,
;; but they use webpack to minify.  See
;; https://github.com/JohnCoene/waiter/blob/master/package.json
(define-public r-waiter
  (package
    (name "r-waiter")
    (version "0.2.5")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "waiter" version))
              (sha256
               (base32
                "0ya92qr25ssfkzn888b7rr8rn0304f3gz4h4pnc2a95rknbmxhls"))))
    (properties `((upstream-name . "waiter")))
    (build-system r-build-system)
    (propagated-inputs (list r-htmltools r-r6 r-shiny))
    (native-inputs (list r-knitr))
    (home-page "https://waiter.john-coene.com/")
    (synopsis "Loading screen for Shiny")
    (description
     "This package provides full screen and partial loading screens
for Shiny with spinners, progress bars, and notifications.")
    (license license:expat)))

;; TODO: still have to remove these:
;; materialDesign-1.0/js/material.min.js
;; materialDesign-1.0/js/ripples.min.js
(define-public r-shinydashboardplus
  (package
    (name "r-shinydashboardplus")
    (version "2.0.3")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "shinydashboardPlus" version))
              (sha256
               (base32
                "10sdb1vddx2ij867pqijr63l4233hw1vnn7mzbs0z23g77x8ra29"))
              (modules '((guix build utils)))
              (snippet
               `(with-directory-excursion
                    ,(string-append "inst/shinydashboardPlus-" version
                                    "/js/")
                  (for-each delete-file-recursively
                            '("app.min.js"
                              "shinydashboardPlus.min.js"
                              "shinydashboardPlus.min.js.map"))))))
    (properties `((upstream-name . "shinydashboardPlus")))
    (build-system r-build-system)
    (arguments
     (list
      ;; The tests launch a shinyApp; they are interactive tests that
      ;; will block forever, so we just don't run them.
      #:tests? #false
      #:phases
      `(modify-phases %standard-phases
         (add-after 'unpack 'process-javascript
           (lambda _
             (with-directory-excursion
                 ,(string-append "inst/shinydashboardPlus-" version
                                 "/js/")
               (let ((mapping
                      `(("app.js" . "app.min.js")
                        ("shinydashboardPlus.js" . "shinydashboardPlus.min.js"))))
                 (for-each (lambda (source target)
                             (format #true "Processing ~a --> ~a~%"
                                     source target)
                             (invoke "esbuild" source "--minify"
                                     (string-append "--outfile=" target)))
                           (map car mapping)
                           (map cdr mapping)))))))))
    (propagated-inputs
     (list r-fresh
           r-htmltools
           r-lifecycle
           r-shiny
           r-shinydashboard
           r-waiter))
    (native-inputs
     (list esbuild r-knitr))
    (home-page "https://github.com/RinteRface/shinydashboardPlus")
    (synopsis "Add more AdminLTE2 components to shinydashboard")
    (description
     "This package extends shinydashboard with AdminLTE2 components.
AdminLTE2 is a Bootstrap 3 dashboard template.  Customize boxes, add
timelines and a lot more.")
    (license license:gpl2+)))
