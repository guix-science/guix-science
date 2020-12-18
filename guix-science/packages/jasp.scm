;;;
;;; Copyright © 2020 Lars-Dominik Braun <ldb@leibniz-psychology.org>
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

;; JASP cannot be upstreamed, because it bundles several JavaScript libraries
;; in JASP-Desktop/html/js/ and Resources/Help/

(define-module (guix-science packages jasp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system r)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages base))

;; R dependencies not in Guix proper yet

(define-public r-kknn
  (package
    (name "r-kknn")
    (version "1.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "kknn" version))
        (sha256
          (base32
            "1nzkg3dxaiqp87p56wm895qx5xn86hv5hjr73qvl1yiaxiq0x112"))))
    (properties `((upstream-name . "kknn")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-igraph" ,r-igraph) ("r-matrix" ,r-matrix)))
    (home-page "https://github.com/KlausVigo/kknn")
    (synopsis "Weighted k-Nearest Neighbors")
    (description
      "Weighted k-Nearest Neighbors for Classification, Regression and Clustering.")
    (license license:gpl2+)))

(define-public r-contfrac
  (package
    (name "r-contfrac")
    (version "1.1-12")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "contfrac" version))
        (sha256
          (base32
            "0sq5c7ny235yrkv2xc4insgxby6rvzc1qsj8h301cd2if3lwbgwm"))))
    (properties `((upstream-name . "contfrac")))
    (build-system r-build-system)
    (home-page
      "https://github.com/RobinHankin/contfrac.git")
    (synopsis "Continued Fractions")
    (description
      "Various utilities for evaluating continued fractions.")
    (license license:gpl2)))

(define-public r-elliptic
  (package
    (name "r-elliptic")
    (version "1.4-0")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "elliptic" version))
        (sha256
          (base32
            "1dhba0yfxjd5rlqsxp5a7s2hclfkla9wigsr39dlma67l6qjjmxn"))))
    (properties `((upstream-name . "elliptic")))
    (build-system r-build-system)
    (inputs `(("pari-gp" ,pari-gp)))
    (propagated-inputs `(("r-mass" ,r-mass)))
    (home-page
      "https://github.com/RobinHankin/elliptic.git")
    (synopsis
      "Weierstrass and Jacobi Elliptic Functions")
    (description
      " A suite of elliptic and related functions including Weierstrass and Jacobi forms.  Also includes various tools for manipulating and visualizing complex functions.")
    (license license:gpl2)))

(define-public r-hypergeo
  (package
    (name "r-hypergeo")
    (version "1.2-13")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "hypergeo" version))
        (sha256
          (base32
            "13jdiy216znwhr91iqnh03mvkmyscw439syb3h4i67dd78sphnvd"))))
    (properties `((upstream-name . "hypergeo")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-contfrac" ,r-contfrac)
        ("r-desolve" ,r-desolve)
        ("r-elliptic" ,r-elliptic)))
    (home-page
      "https://cran.r-project.org/web/packages/hypergeo")
    (synopsis "The Gauss Hypergeometric Function")
    (description
      "The Gaussian hypergeometric function for complex numbers.")
    (license license:gpl2)))

(define-public r-logspline
  (package
    (name "r-logspline")
    (version "2.1.16")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "logspline" version))
        (sha256
          (base32
            "12hkdi77vkic05p2vhap025xdcg1n53ywm239v18713pihdlj63l"))))
    (properties `((upstream-name . "logspline")))
    (build-system r-build-system)
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page
      "https://cran.r-project.org/web/packages/logspline")
    (synopsis
      "Routines for Logspline Density Estimation")
    (description
      "Contains routines for logspline density estimation.  The function oldlogspline() uses the same algorithm as the logspline package version 1.0.x; i.e.  the Kooperberg and Stone (1992) algorithm (with an improved interface).  The recommended routine logspline() uses an algorithm from Stone et al (1997)  <DOI:10.1214/aos/1031594728>.")
    (license license:asl2.0)))

(define-public r-effects
  (package
    (name "r-effects")
    (version "4.2-0")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "effects" version))
        (sha256
          (base32
            "0iks79rd4knrd81x4kvcpgpclr69l0m0v6sfla8k3wzkcnydycv8"))))
    (properties `((upstream-name . "effects")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-cardata" ,r-cardata)
        ("r-colorspace" ,r-colorspace)
        ("r-estimability" ,r-estimability)
        ("r-insight" ,r-insight)
        ("r-lattice" ,r-lattice)
        ("r-lme4" ,r-lme4)
        ("r-nnet" ,r-nnet)
        ("r-survey" ,r-survey)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page "https://www.r-project.org")
    (synopsis
      "Effect Displays for Linear, Generalized Linear, and Other Models")
    (description
      " Graphical and tabular effect displays, e.g., of interactions, for various statistical models with linear predictors.")
    (license license:gpl2+)))

(define-public r-contfrac
  (package
    (name "r-contfrac")
    (version "1.1-12")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "contfrac" version))
        (sha256
          (base32
            "0sq5c7ny235yrkv2xc4insgxby6rvzc1qsj8h301cd2if3lwbgwm"))))
    (properties `((upstream-name . "contfrac")))
    (build-system r-build-system)
    (home-page
      "https://github.com/RobinHankin/contfrac.git")
    (synopsis "Continued Fractions")
    (description
      "Various utilities for evaluating continued fractions.")
    (license license:gpl2)))

(define-public r-elliptic
  (package
    (name "r-elliptic")
    (version "1.4-0")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "elliptic" version))
        (sha256
          (base32
            "1dhba0yfxjd5rlqsxp5a7s2hclfkla9wigsr39dlma67l6qjjmxn"))))
    (properties `((upstream-name . "elliptic")))
    (build-system r-build-system)
    (inputs `(("pari-gp" ,pari-gp)))
    (propagated-inputs `(("r-mass" ,r-mass)))
    (home-page
      "https://github.com/RobinHankin/elliptic.git")
    (synopsis
      "Weierstrass and Jacobi Elliptic Functions")
    (description
      " A suite of elliptic and related functions including Weierstrass and Jacobi forms.  Also includes various tools for manipulating and visualizing complex functions.")
    (license license:gpl2)))

(define-public r-hypergeo
  (package
    (name "r-hypergeo")
    (version "1.2-13")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "hypergeo" version))
        (sha256
          (base32
            "13jdiy216znwhr91iqnh03mvkmyscw439syb3h4i67dd78sphnvd"))))
    (properties `((upstream-name . "hypergeo")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-contfrac" ,r-contfrac)
        ("r-desolve" ,r-desolve)
        ("r-elliptic" ,r-elliptic)))
    (home-page
      "https://cran.r-project.org/web/packages/hypergeo")
    (synopsis "The Gauss Hypergeometric Function")
    (description
      "The Gaussian hypergeometric function for complex numbers.")
    (license license:gpl2)))

(define-public r-bayesfactor
  (package
    (name "r-bayesfactor")
    (version "0.9.12-4.2")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "BayesFactor" version))
        (sha256
          (base32
            "1z083v7is21gm0a458jm5ph3xfdm7mh29a6sg2r1njq82f8x3s3g"))))
    (properties `((upstream-name . "BayesFactor")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-coda" ,r-coda)
        ("r-gtools" ,r-gtools)
        ("r-hypergeo" ,r-hypergeo)
        ("r-matrix" ,r-matrix)
        ("r-matrixmodels" ,r-matrixmodels)
        ("r-mvtnorm" ,r-mvtnorm)
        ("r-pbapply" ,r-pbapply)
        ("r-rcpp" ,r-rcpp)
        ("r-rcppeigen" ,r-rcppeigen)
        ("r-stringr" ,r-stringr)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page
      "https://richarddmorey.github.io/BayesFactor/")
    (synopsis
      "Computation of Bayes Factors for Common Designs")
    (description
      "This package provides a suite of functions for computing various Bayes factors for simple designs, including contingency tables, one- and two-sample designs, one-way designs, general ANOVA designs, and linear regression.")
    (license license:gpl2)))

;; Other dependencies

(define-public readstat
  (package
    (name "readstat")
    (version "1.1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/WizardMac/ReadStat.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "188glrfkwfs19m2hki8zkhmwbwjdciahgnd6zk3mfxr9daypw82r"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gnu-gettext)
       ("libtool" ,libtool)))
    (inputs
     `(("zlib" ,zlib)))                 ; libz
    (synopsis "Convert SAS, Stata, and SPSS files")
    (description "Command-line tool and C library for reading files from
popular stats packages like SAS, Stata and SPSS.")
    (home-page "https://github.com/WizardMac/ReadStat")
    (license license:expat)))

(define-public jasp
  (package
    (name "jasp")
    (version "0.14.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jasp-stats/jasp-desktop.git")
                    (commit (string-append version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05r2gdq7lxmndxzg5h8nmahvrksv9v4xh40ls0azn0wfrpcsnrlq"))
              (patches
               (search-patches "jasp-0.14.0.0-noupdate.patch"))))
    (build-system qt-build-system)
    (arguments
     '(#:tests? #f ; There are tests in JASP-Tests, but they need special setup
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "R_HOME.pri"
                 (("\\$\\$_R_HOME/site-library/Rcpp/include")
                  (string-append (assoc-ref inputs "r-rcpp")
                                 "/site-library/Rcpp/include"))
                 (("/usr/include/R/")
                  (string-append (assoc-ref inputs "r-minimal")
                                 "/lib/R/include"))
                 (("R_EXE  = \\$\\$_R_HOME/bin/R")
                  (string-append "R_EXE = "
                                 (assoc-ref inputs "r-minimal")
                                 "/bin/R")))
               (substitute* "JASP.pri"
                 (("^GIT_BRANCH=.+") "GIT_BRANCH=master")
                 (("^GIT_COMMIT=.+") "GIT_COMMIT=undefined")
                 (("INSTALLPATH = /usr/bin")
                  (string-append "INSTALLPATH = " out "/lib/jasp")))
               (substitute* "JASP-Desktop/JASP-Desktop.pro"
                 (("target\\.path \\+= /usr/bin")
                  (string-append "target.path += " out "/lib/jasp")))
               #t)))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (setenv "R_HOME"
                       (string-append (assoc-ref inputs "r-minimal") "/lib/R"))
               (invoke "qmake"
                       (string-append "PREFIX=" out)
                       ;; Uncomment, if you want a debug build
                       ;;"CONFIG+=debug"
                       "JASP.pro"))))
         (add-after 'install 'copy-r-library
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Not copied by Makefile.
             (copy-recursively
               "R/library"
               (string-append (assoc-ref outputs "out") "/site-library"))
             #t))
         ;; Add before, so qt-wrap will add qt search paths
         (add-before 'qt-wrap 'webengine-wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((qtwebengineprocess
                    (string-append (assoc-ref inputs "qtwebengine")
                                   "/lib/qt5/libexec/QtWebEngineProcess"))
                   (out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/bin"))
               (symlink
                 (string-append out "/lib/jasp/jasp")
                 (string-append out "/bin/jasp"))
               (wrap-program (string-append out "/lib/jasp/jasp")
                 `("QTWEBENGINEPROCESS_PATH" =
                   (,qtwebengineprocess)))
               (wrap-program (string-append out "/lib/jasp/JASPEngine")
                 `("R_LIBS_SITE" ":" =
                   (,(string-append out "/site-library:" (getenv "R_LIBS_SITE"))))))
             #t)))))
    (native-inputs
     `(("qtbase" ,qtbase)))             ; qmake
    (inputs
     `(("qtwebengine" ,qtwebengine)
       ("qtsvg" ,qtsvg)
       ("qtwebchannel" ,qtwebchannel)
       ("qtquickcontrols" ,qtquickcontrols) ; qtquick styles
       ("qtquickcontrols2" ,qtquickcontrols2)
       ("qtdeclarative" ,qtdeclarative)
       ("qtgraphicaleffects" ,qtgraphicaleffects)
       ("boost" ,boost)
       ("libarchive" ,libarchive)
       ("zlib" ,zlib)
       ("readstat" ,readstat)
       ("r-minimal" ,r-minimal)
       ("r-rcpp" ,r-rcpp)
       ("r-kknn" ,r-kknn)
       ;; Required to build JASPgraphs.
       ("r-ggplot2" ,r-ggplot2)
       ("r-gridextra" ,r-gridextra)
       ("r-scales" ,r-scales)
       ("r-gtable" ,r-gtable)
       ("r-jsonlite" ,r-jsonlite)
       ("r-r6" ,r-r6)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-rlang" ,r-rlang)
       ("r-scales" ,r-scales)
       ("r-stringr" ,r-stringr)
       ("r-viridislite" ,r-viridislite)
       ;; Runtime deps.
       ("r-rjson" ,r-rjson)
       ("r-hypergeo" ,r-hypergeo)
       ("r-logspline" ,r-logspline)
       ("r-effects" ,r-effects)
       ("r-car" ,r-car)
       ("r-afex" ,r-afex)
       ("r-lme4" ,r-lme4)
       ("r-bayesfactor" ,r-bayesfactor)
       ("r-lmtest" ,r-lmtest)))
    (home-page "https://jasp-stats.org/")
    (synopsis "Statistical package for Bayesian and Frequentist statistical
methods")
    (description "Statistical package for both Bayesian and Frequentist
statistical methods, that is easy to use and familiar to users of SPSS.")
    (license
     (list
      license:agpl3                 ; The desktop client
      license:gpl2+                 ; The engine
      license:expat                 ; marked: Resources/Help/marked.js
                                        ; underscore.js: JASP-Desktop/html/js/underscore.js
                                        ; jquery ui: JASP-Desktop/html/js/jquery-ui-1.10.1.custom.min.js
                                        ; backbone.js: JASP-Desktop/html/js/backbone-1.1.2.js
      license:bsd-3 ; quilljs: JASP-Desktop/html/js/quill.js
                                        ; qwebchannel: JASP-Desktop/html/js/qwebchannel.js
      license:public-domain          ; jsoncpp: JASP-Common/lib_json/
      license:bsd-2                  ; libarchive: JASP-Common/libzip/
      license:boost1.0               ; boost: JASP-Common/boost/
      ))))

