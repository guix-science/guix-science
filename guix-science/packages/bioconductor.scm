;;;
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

(define-module (guix-science packages bioconductor)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system r)
  #:use-module (gnu packages)
  #:use-module (gnu packages bioconductor)
  #:use-module (gnu packages cran)
  #:use-module (guix-science packages cran)
  #:use-module (gnu packages statistics))

;; This package depends on r-shinywidgets, which contains a lot of
;; minified JavaScript.
(define-public r-isee
  (package
    (name "r-isee")
    (version "2.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.bioconductor.org/packages/iSEE")
                    (commit "9c0e8e01d657ea60696eca72514293d3f2720c4b")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1d58irf9ch1kfy1sbzldci71k8aqrfg8jn9qqdk973iwc7xsgfi2"))))
    (properties `((upstream-name . "iSEE")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-circlize
           r-colourpicker
           r-complexheatmap
           r-dt
           r-ggplot2
           r-ggrepel
           r-igraph
           r-mgcv
           r-rintrojs
           r-s4vectors
           r-shiny
           r-shinyace
           r-shinydashboard
           r-shinyjs
           r-shinywidgets
           r-singlecellexperiment
           r-summarizedexperiment
           r-vipor
           r-viridislite))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/iSEE/iSEE")
    (synopsis "Interactive SummarizedExperiment Explorer")
    (description
     "Create an interactive Shiny-based graphical user interface for
exploring data stored in SummarizedExperiment objects, including row-
and column-level metadata.  The interface supports transmission of
selections between plots and tables, code tracking, interactive tours,
interactive or programmatic initialization, preservation of app state,
and extensibility to new panel types via S4 classes.  Special
attention is given to single-cell data in a SingleCellExperiment
object with visualization of dimensionality reduction results.")
    (license license:expat)))
