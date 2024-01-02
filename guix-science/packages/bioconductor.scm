;;;
;;; Copyright © 2022-2024 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system r)
  #:use-module (gnu packages)
  #:use-module (gnu packages bioconductor)
  #:use-module (gnu packages cran)
  #:use-module (guix-science packages cran)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages web))

;; This package depends on r-shinywidgets, which contains a lot of
;; minified JavaScript.
(define-public r-isee
  (package
    (name "r-isee")
    (version "2.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "iSEE" version))
       (sha256
        (base32 "0khysw2nj83sn2vgzjn76qrqfkiqwn5yc11c6x5l2zpllvzfrcj4"))
       (snippet
        '(for-each delete-file
                   (list "inst/www/annyang.min.js"
                         "inst/www/bug-min.js")))))
    (properties `((upstream-name . "iSEE")))
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
          (add-after 'unpack 'process-javascript
            (lambda* (#:key inputs #:allow-other-keys)
              (with-directory-excursion "inst/www"
                (for-each (match-lambda
                            ((source . target)
                             (minify source #:target target)))
                          `((,(search-input-file inputs "/dist/annyang.js")
                             . "annyang.min.js")
                            (,(search-input-file inputs "/bug.js")
                             . "bug-min.js")))))))))
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
    (native-inputs
     (list esbuild
           r-knitr
           (let* ((commit "8eac7b7337604fb3b8c8f0be1116c8de0984057e")
                  (version (git-version "0" "0" commit)))
             (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/Auz/Bug")
                     (commit commit)))
               (file-name (git-file-name "bug" version))
               (sha256
                (base32
                 "0djxs794h64n7fjh5aya4sj9phkb5kfm49vvvdvfkv93r2kyb688"))))
           (let ((version "2.6.0"))
             (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/TalAter/annyang")
                     (commit (string-append "v" version))))
               (file-name (git-file-name "annyang" version))
               (sha256
                (base32
                 "109mfp8i45mb9m6b4nwjw0nm5r40jq4jpmik97bv8b97ycz3mgyz"))))))
    (home-page "https://github.com/iSEE/iSEE")
    (synopsis "Interactive SummarizedExperiment explorer")
    (description
     "Create an interactive Shiny-based graphical user interface for
exploring data stored in @code{SummarizedExperiment} objects,
including row- and column-level metadata.  The interface supports
transmission of selections between plots and tables, code tracking,
interactive tours, interactive or programmatic initialization,
preservation of app state, and extensibility to new panel types via S4
classes.  Special attention is given to single-cell data in a
@code{SingleCellExperiment} object with visualization of
dimensionality reduction results.")
    (license license:expat)))
