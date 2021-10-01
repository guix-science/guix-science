;;;
;;; Copyright © 2019, 2020 Lars-Dominik Braun <ldb@leibniz-psychology.org>
;;; Copyright © 2020 Roel Janssen <roel@gnu.org>
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

;; RStudio server and its dependencies. Only the first level of dependencies is
;; built from source right now. Also RStudio itself bundles a lot of external
;; libraries, some with custom modifications.
;; 
;; An older version is available at
;; https://github.com/BIMSBbioinfo/guix-bimsb/blob/master/bimsb/packages/staging.scm

(define-module (guix-science packages rstudio)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system ant)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages node)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (guix-science packages rstudio-node))

(define-public mathjax
  (package
    (name "mathjax")
    (version "2.7.9")
    ( source (origin
               (method url-fetch)
               (uri (string-append "https://github.com/mathjax/MathJax/archive/" version ".tar.gz"))
               (sha256
                (base32
                 "1pv51xiy31pbd3jhli2rrda07s9cl1rxa2lizzr4983xr1wp45n9"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("." "."))))
    (synopsis "JavaScript display engine for mathematics")
    (description
     "MathJax is an open-source JavaScript display engine for LaTeX, MathML,
and AsciiMath notation that works in all modern browsers, with built-in support
for assistive technology like screen readers.")
    (home-page "https://www.mathjax.org/")
    (license license:asl2.0)))

(define-public rstudio-server
  (package
    (name "rstudio-server")
    (version "2021.09.0+351")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rstudio/rstudio.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1lxncx6m2gakjpwzz3nflv5ljk69abwa8zizhvcysr7s192hsbv5"))
              (patches
               (search-patches "rstudio-server-2021.09.0+351-unbundle.patch"
                               "rstudio-server-1.4.1103-soci-searchpath.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file-recursively
                    ;; de-blob: windows tools
                    '("dependencies/windows/tools"
                      ;; auto-generated files
                      "src/cpp/session/include/session/SessionOptions.gen.hpp"
                      "src/cpp/server/include/server/ServerOptions.gen.hpp"))
                  #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:out-of-source? #f ; required by 'make-writable
       #:configure-flags
       (list
         "-DRSTUDIO_TARGET=Server"
         "-DCMAKE_BUILD_TYPE=Release"
         "-DRSTUDIO_USE_SYSTEM_YAML_CPP=1"
         "-DRSTUDIO_USE_SYSTEM_BOOST=1"
         "-DRSTUDIO_USE_SYSTEM_SOCI=1"
         ;; auto-detection seems to be broken with boost 1.72
         "-DRSTUDIO_BOOST_SIGNALS_VERSION=2")
       #:tests? #f ; no tests exist
       #:modules ((guix build cmake-build-system)
                  (guix build utils)
                  (ice-9 match))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-dictionary
           (lambda* (#:key inputs native-inputs #:allow-other-keys)
             (let ((dict-dir "dependencies/dictionaries"))
               (mkdir dict-dir)
               (invoke "unzip" "-qd" dict-dir (assoc-ref inputs "dict-source-tarball")))
             #t))
         ;; change the default paths for mathjax and pandoc and a hardcoded call to `which`
         (add-after 'unpack 'patch-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/cpp/session/session-options.json"
               (("resources/mathjax-27") (assoc-ref inputs "mathjax")))
             (substitute* "src/cpp/session/include/session/SessionConstants.hpp"
               ;; XXX: It looks like there’s a bug in RStudio right now, which
               ;; appends the binary name “pandoc” to this path, if it is an
               ;; absolute path.
               (("bin/pandoc") (string-append (assoc-ref inputs "pandoc") "/bin")))
             (substitute* "src/cpp/core/r_util/REnvironmentPosix.cpp"
               (("runCommand\\(\"which") (string-append "runCommand(\"" (assoc-ref inputs "which") "/bin/which")))
             (substitute* "src/cpp/session/modules/clang/CodeCompletion.cpp"
               (("/usr/bin/which") (string-append (assoc-ref inputs "which") "/bin/which")))
             (substitute* '("src/cpp/session/SessionConsoleProcess.cpp" "src/cpp/session/modules/SessionTerminalShell.cpp")
               (("/usr/bin/env") (string-append (assoc-ref inputs "coreutils") "/bin/env")))
             (substitute* '("src/cpp/session/modules/SessionFiles.R")
               (("\"zip\"") (string-append "\"" (assoc-ref inputs "zip") "/bin/zip\"")))
             (substitute* '("src/cpp/session/modules/SessionFiles.cpp")
               (("/usr/bin/unzip") (string-append (assoc-ref inputs "unzip") "/bin/unzip")))
             (substitute* "src/cpp/core/system/Architecture.cpp"
               (("/usr/bin/uname") (string-append (assoc-ref inputs "coreutils") "/bin/uname")))
             (substitute* "src/gwt/build.xml"
               ;; Fix path to node binary
               (("\"[^\"]+/bin/node\"")
                (string-append "\"" (assoc-ref inputs "node") "/bin/node\""))
               ;; For some reason this interferes with Guix’s NODE_PATH, so
               ;; remove it.
               (("<env key=\"NODE_PATH\" path=\"[^\"]+\"/>") ""))
             (substitute* "src/gwt/panmirror/src/editor/fuse.js"
               ;; This path is only used to copy prosemirror
               (("/opt/rstudio-tools/panmirror/node_modules")
                (string-append (assoc-ref inputs "node-prosemirror-dev-tools") "/lib/node_modules"))
               ;; Replace with current NODE_PATH, since there seems to be no
               ;; other way to make fuse-box respect that path.
               ;; (setenv "PROJECT_NODE_MODULES" (getenv "NODE_PATH"))
               ;; does not work, since that path does not support :-delimited strings
               (("\"\\./node_modules\"")
                (string-append "\"" (string-join (string-split (getenv "NODE_PATH") #\:) "\", \"") "\"")))
             #t))
         (add-after 'unpack 'set-environment-variables
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "JAVA_HOME" (assoc-ref inputs "jdk"))
             ;; set proper version information
             (match (string-split ,version (char-set #\. #\+))
               ((major minor patch suffix)
                (setenv "RSTUDIO_VERSION_MAJOR" major)
                (setenv "RSTUDIO_VERSION_MINOR" minor)
                (setenv "RSTUDIO_VERSION_PATCH" patch)
                (setenv "RSTUDIO_VERSION_SUFFIX" (string-append "+" suffix))))
             (setenv "PACKAGE_OS" "GNU Guix")
             ;; Otherwise fuse-box will try to write to /gnu/store/…/.fusebox.
             (setenv "FUSEBOX_TEMP_FOLDER" "/tmp/")
             #t))
         ;; Must be run after patch-source-shebangs, which changes
         ;; report-option.sh’s interpreter line.
         (add-before 'configure 'generate-server-options
           (lambda _
             ;; Generate *.gen.hpp files deleted by source snippet.
             (with-directory-excursion "src/cpp"
               (invoke "./generate-options.R"))))
         (add-before 'install 'make-writable
           (lambda _
             ;; This file is copied with permission bits from the store during
             ;; 'build and overwriting fails during 'install if not
             ;; writable.
             (make-file-writable "src/gwt/www/js/panmirror/prosemirror-dev-tools.min.js")
             #t)))))
    (native-inputs
     `(("unzip" ,unzip)
       ;; gwt-components are built using ant
       ("ant" ,ant)
       ("jdk" ,openjdk11 "jdk")
       ;; For building panmirror. XXX: Maybe put panmirror into its own package?
       ("node" ,node)
       ("node-fuse-box" ,node-fuse-box-3.7.1)
       ;; Copied to output
       ("node-prosemirror-dev-tools" ,node-prosemirror-dev-tools-2.1.1)
       ;; For src/cpp/generate-options.R
       ("r-jsonlite" ,r-jsonlite)
       ("r-stringi" ,r-stringi)
       ("r-stringr" ,r-stringr)
       ("r-tibble" ,r-tibble)
       ("jq" ,jq)))
    (inputs
     `(("boost" ,boost)
       ("zlib" ,zlib)
       ("linux-pam" ,linux-pam)
       ("yaml-cpp" ,yaml-cpp)
       ("r-minimal" ,r-minimal)
       ("openssl" ,openssl)
       ;; for libuuid
       ("util-linux" ,util-linux "lib")
       ("pandoc" ,pandoc)
       ("which" ,which)
       ("mathjax" ,mathjax) ; XXX: not sure this is the correct version, but
       ; any 2.7 patch release should work, right?
       ;; for `env`
       ("coreutils" ,coreutils)
       ;; File panel -> More -> Export
       ("zip" ,zip)
       ;; File panel -> Upload -> Upload zip file
       ("unzip" ,unzip)
       ;; For database support, uses only client library.
       ("postgresql" ,postgresql)
       ("sqlite" ,sqlite)
       ("soci" ,soci)
       ;; For panmirror.
       ("node-biblatex-csl-converter-1.9.5" ,node-biblatex-csl-converter-1.9.5)
       ("node-clipboard-2.0.6" ,node-clipboard-2.0.6)
       ("node-diff-match-patch-1.0.5" ,node-diff-match-patch-1.0.5)
       ("node-fuse-js-6.4.6" ,node-fuse-js-6.4.6)
       ("node-js-yaml-3.14.1" ,node-js-yaml-3.14.1)
       ("node-lodash-debounce-3.1.1" ,node-lodash-debounce-3.1.1)
       ("node-lodash-debounce-4.0.8" ,node-lodash-debounce-4.0.8)
       ("node-lodash-uniqby-4.7.0" ,node-lodash-uniqby-4.7.0)
       ("node-orderedmap-1.1.1" ,node-orderedmap-1.1.1)
       ("node-prosemirror-changeset-2.1.2" ,node-prosemirror-changeset-2.1.2)
       ("node-prosemirror-commands-1.1.6" ,node-prosemirror-commands-1.1.6)
       ("node-prosemirror-dev-tools-2.1.1" ,node-prosemirror-dev-tools-2.1.1)
       ("node-prosemirror-dropcursor-1.3.3" ,node-prosemirror-dropcursor-1.3.3)
       ("node-prosemirror-gapcursor-1.1.5" ,node-prosemirror-gapcursor-1.1.5)
       ("node-prosemirror-history-1.1.3" ,node-prosemirror-history-1.1.3)
       ("node-prosemirror-inputrules-1.1.3" ,node-prosemirror-inputrules-1.1.3)
       ("node-prosemirror-keymap-1.1.4" ,node-prosemirror-keymap-1.1.4)
       ("node-prosemirror-model-1.13.3" ,node-prosemirror-model-1.13.3)
       ("node-prosemirror-schema-list-1.1.4" ,node-prosemirror-schema-list-1.1.4)
       ("node-prosemirror-state-1.3.4" ,node-prosemirror-state-1.3.4)
       ("node-prosemirror-tables-1.1.1" ,node-prosemirror-tables-1.1.1)
       ("node-prosemirror-transform-1.2.11" ,node-prosemirror-transform-1.2.11)
       ("node-prosemirror-utils-0.9.6" ,node-prosemirror-utils-0.9.6)
       ("node-prosemirror-view-1.17.6" ,node-prosemirror-view-1.17.6)
       ("node-react-16.14.0" ,node-react-16.14.0)
       ("node-react-base16-styling-0.5.3" ,node-react-base16-styling-0.5.3)
       ("node-react-dock-0.2.4" ,node-react-dock-0.2.4)
       ("node-react-dom-16.14.0" ,node-react-dom-16.14.0)
       ("node-react-emotion-9.2.12" ,node-react-emotion-9.2.12)
       ("node-react-is-16.13.1" ,node-react-is-16.13.1)
       ("node-react-json-tree-0.11.2" ,node-react-json-tree-0.11.2)
       ("node-react-window-1.8.6" ,node-react-window-1.8.6)
       ("node-sentence-splitter-3.2.0" ,node-sentence-splitter-3.2.0)
       ("node-thenby-1.3.4" ,node-thenby-1.3.4)
       ("node-tlite-0.1.9" ,node-tlite-0.1.9)
       ("node-typescript-3.8.3" ,node-typescript-3.8.3)
       ("node-zenscroll-4.0.2" ,node-zenscroll-4.0.2)
       ;; External resources.
       ("dict-source-tarball"
        ,(origin
           (method url-fetch)
           (uri "https://s3.amazonaws.com/rstudio-buildtools/dictionaries/core-dictionaries.zip")
           (sha256
            (base32
             "153lg3ai97qzbqp6zjg10dh3sfvz80v42cjw45zwz7gv1risjha3"))))))
    (home-page "https://rstudio.com/products/rstudio/#rstudio-server")
    (synopsis "Integrated development environment (IDE) for R")
    (description "RStudio is an integrated development environment (IDE) for the R
programming language. Some of its features include: Customizable workbench
with all of the tools required to work with R in one place (console, source,
plots, workspace, help, history, etc.); syntax highlighting editor with code
completion; execute code directly from the source editor (line, selection, or
file); full support for authoring Sweave and TeX documents.  RStudio can also
be run as a server, enabling multiple users to access the RStudio IDE using a
web browser.")
    (license license:agpl3)))

(define-public rstudio-server-multi-version
  (package
    (inherit rstudio-server)
    (name "rstudio-server-multi-version")
    (source
      (origin
        (inherit (package-source rstudio-server))
        (patches
          (append
            (origin-patches (package-source rstudio-server))
            (search-patches
              "patches/rstudio-server-multi-version/0001-handleClientInit-Store-R-versions-in-sessionInfo.patch"
              "patches/rstudio-server-multi-version/0002-sessionProcessConfig-Configure-R-version-from-active.patch"
              "patches/rstudio-server-multi-version/0003-NewProjectWizard-Unhide-version-selector-widget.patch"
              "patches/rstudio-server-multi-version/0004-handleConnection-Switch-R-version-when-switching-pro.patch"
              "patches/rstudio-server-multi-version/0005-Add-version-switcher-widget-to-toolbar.patch"
              "patches/rstudio-server-multi-version/0006-Look-at-.local-share-rstudio-r-versions-for-custom-R.patch"
              "patches/rstudio-server-multi-version/0007-detectRLocationsUsingR-Restore-R_HOME-at-the-end.patch")))))
    (description "This fork of RStudio allows users to switch to
different versions of R from the toolbar or project settings.  R
versions can be recorded in @file{/etc/rstudio/r-versions} and in the
user's @file{~/.local/share/rstudio/r-versions}.")))

(define-public rstudio
  (package (inherit rstudio-server)
    (name "rstudio")
    (arguments
     (substitute-keyword-arguments (package-arguments rstudio-server)
       ((#:configure-flags flags)
        '(list "-DRSTUDIO_TARGET=Desktop"
               "-DCMAKE_BUILD_TYPE=Release"
               "-DRSTUDIO_USE_SYSTEM_YAML_CPP=1"
               "-DRSTUDIO_USE_SYSTEM_BOOST=1"
               "-DRSTUDIO_USE_SYSTEM_SOCI=1"
               ;; auto-detection seems to be broken with boost 1.72
               "-DRSTUDIO_BOOST_SIGNALS_VERSION=2"
               (string-append "-DQT_QMAKE_EXECUTABLE="
                              (assoc-ref %build-inputs "qtbase")
                              "/bin/qmake")))
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'install 'wrap-program
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin"))
                      (qtwebengine-path (string-append
                                         (assoc-ref inputs "qtwebengine")
                                         "/lib/qt5/libexec/QtWebEngineProcess")))
                 (wrap-program (string-append bin "/rstudio")
                   `("QTWEBENGINEPROCESS_PATH" ":" = (,qtwebengine-path))))
               #t))))))
    (inputs
     `(("qtbase" ,qtbase-5)
       ("qtdeclarative" ,qtdeclarative)
       ("qtlocation" ,qtlocation)
       ("qtsvg" ,qtsvg)
       ("qtsensors" ,qtsensors)
       ("qtxmlpatterns" ,qtxmlpatterns)
       ("qtwebchannel" ,qtwebchannel)
       ("qtwebengine" ,qtwebengine)
       ,@(package-inputs rstudio-server)))
    (synopsis "Integrated development environment (IDE) for R (desktop version)")))

