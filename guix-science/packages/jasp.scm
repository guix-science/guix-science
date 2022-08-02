;;;
;;; Copyright © 2020 Lars-Dominik Braun <ldb@leibniz-psychology.org>
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
               (search-patches "jasp-0.14.0.0-noupdate.patch"
                               "jasp-0.14.0.0-includes.patch"))))
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
     `(("qtbase" ,qtbase-5)))             ; qmake
    (inputs
     `(("qtwebengine" ,qtwebengine-5)
       ("qtsvg" ,qtsvg-5)
       ("qtwebchannel" ,qtwebchannel-5)
       ("qtquickcontrols" ,qtquickcontrols-5) ; qtquick styles
       ("qtquickcontrols2" ,qtquickcontrols2-5)
       ("qtdeclarative" ,qtdeclarative-5)
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

