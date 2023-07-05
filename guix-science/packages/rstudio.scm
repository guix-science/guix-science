;;;
;;; Copyright © 2019, 2020 Lars-Dominik Braun <ldb@leibniz-psychology.org>
;;; Copyright © 2020 Roel Janssen <roel@gnu.org>
;;; Copyright © 2023 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system node)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages file)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages node)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages ssh)
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

(define-public js-panmirror
  ;; This is the latest commit of the branch mentioned in RStudio’s
  ;; dependencies/common/install-panmirror.
  (let ((commit "01345470a8f80becb1e128be24f59d2c34fb3a85")
        (revision "1"))
    (package
      (name "js-panmirror")
      ;; No releases, no versions, no tags.
      (version (git-version "0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                (url "https://github.com/quarto-dev/quarto.git")
                (commit commit)))
          (file-name (git-file-name "js-panmirror" version))
          (sha256
           (base32
            "1ngg95lh4n6yri3p4v2qps3vglnwcp054a54336rps16yi7814rb"))
          (patches (search-patches "js-panmirror-no-pinyin.patch"))))
      (build-system node-build-system)
      (arguments
       (list
        #:node node-lts
        #:imported-modules `(,@%node-build-system-modules
                             (guix build union))
        #:modules '((guix build node-build-system)
                    (guix build union)
                    (guix build utils)
                    (ice-9 ftw))
        #:tests? #false
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'chdir
              (lambda _
                (chdir "apps/panmirror")

                ;; There is a lot of duplication among the dependencies.
                ;; esbuild will inline each duplicate and so there will
                ;; be multiple instances of e.g. prosemirror-model
                ;; resulting in all sorts of runtime problems since
                ;; identical definitions don't share state.  The ugly
                ;; solution is to copy all node_modules from NODE_PATH
                ;; and manually hoist nested dependencies.
                (let ((local (string-append (getcwd) "/node_modules")))
                  ;; Build an editable copy of all node_modules
                  ;; directories.
                  (union-build local
                               (string-split (getenv "NODE_PATH") #\:)
                               #:symlink copy-file
                               #:create-all-directories? #t)

                  ;; The contents of every nested node_modules directory
                  ;; should be moved up to the top.
                  (with-directory-excursion local
                    (for-each make-file-writable (find-files "." ".*"))
                    (let ((package-names
                           (scandir "."
                                    (lambda (file)
                                      (not (member file (list "." "..")))))))
                      (for-each
                       (lambda (package-name)
                         (let ((name (string-append package-name "/node_modules")))
                           (when (file-exists? name)
                             (let ((dependencies
                                    (scandir name
                                             (lambda (file)
                                               (and (not (member file (list "." "..")))
                                                    (eq? 'directory
                                                         (stat:type
                                                          (stat (string-append name "/" file)))))))))
                               ;; Move the directory to the top level.
                               (for-each
                                (lambda (dir)
                                  (let ((source (string-append name "/" dir))
                                        (target (string-append local "/" dir)))
                                    (copy-recursively source target)
                                    (delete-file-recursively source)))
                                dependencies)))))
                       package-names)))
                  (setenv "NODE_PATH"
                          (string-append local ":../../packages")))))
            (delete 'configure)
            (replace 'build
              (lambda _
                (invoke "esbuild"
                        "--bundle"
                        "--minify"
                        "--outfile=dist/panmirror.js"
                        "--loader:.png=dataurl"
                        "--loader:.gif=dataurl"
                        ;; RStudio expects Panmirror to be available in window.Panmirror,
                        ;; esbuild’s global-name binds to <name>.default though, so we need the
                        ;; footer option to get the object into the right shape.
                        "--global-name=_internalPanmirror"
                        "--footer:js=window.Panmirror=_internalPanmirror.default"
                        "--define:process.env.NODE_ENV=\"production\""
                        "--define:process.env.DEBUG=\"\""
                        "--define:process.env.TERM=\"\""
                        "--define:process.platform=\"\""
                        "--platform=browser"
                        "--main-fields=browser,module,main"
                        "--conditions=browser,module"
                        "--format=iife"
                        "src/index.ts")))
            (replace 'install
              (lambda _
                (let ((target (string-append #$output "/share/javascript/panmirror")))
                  (install-file "dist/panmirror.js" target)
                  (install-file "dist/panmirror.css" target))))
            (delete 'avoid-node-gyp-rebuild))))
      (native-inputs
       (list esbuild))
      (inputs
       (list node-biblatex-csl-converter-2.1.0
             node-clipboard-2.0.11
             node-diff-match-patch-1.0.5
             node-fuse-js-6.6.2
             node-jayson-4.1.0
             node-js-yaml-4.1.0
             node-lodash-debounce-4.0.8
             node-lodash-orderby-4.6.0
             node-lodash-uniqby-4.7.0
             node-orderedmap-1.1.8
             ;; We cannot build node-jieba, which node-pinyin depends on. Instead, patch it out.
             ;;node-pinyin-2.11.2
             node-prosemirror-changeset-2.2.1
             node-prosemirror-commands-1.5.2
             node-prosemirror-dev-tools-3.1.0
             node-prosemirror-dropcursor-1.8.1
             node-prosemirror-gapcursor-1.3.2
             node-prosemirror-history-1.3.2
             node-prosemirror-inputrules-1.2.1
             node-prosemirror-keymap-1.2.2
             node-prosemirror-model-1.19.2
             node-prosemirror-schema-list-1.3.0
             node-prosemirror-state-1.4.3
             node-prosemirror-tables-1.3.2
             node-prosemirror-transform-1.7.3
             node-prosemirror-utils-0.9.6
             node-prosemirror-view-1.31.5
             node-react-18.2.0
             node-react-dom-18.2.0
             node-react-textarea-autosize-8.5.0
             node-react-window-1.8.9
             node-sentence-splitter-3.2.3
             node-thenby-1.3.4
             node-tlite-0.1.9
             node-transliteration-2.3.5
             node-typescript-4.9.5
             node-use-debounce-9.0.4
             node-vscode-languageserver-types-3.17.3
             node-zenscroll-4.0.2
             ;; For core
             node-markdown-it-13.0.1))
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
      (license license:agpl3))))

(define-public rstudio-server
  ;; Latest commit of the guix-science-v<version> branch.
  (let ((commit "a5a1fc0f64806e2e605cbf1ccd45f382d09ba5c1"))
    (package
      (name "rstudio-server")
      (version "2023.06.0+421")
      (source (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/guix-science/rstudio.git")
              (commit commit)))
        (file-name (git-file-name "rstudio" version))
        (sha256
         (base32
          "1i4vicq1fdk5ddij0zlwpakm75wwn4bikijv5brmi7j7xblvdd6k"))
        (modules '((guix build utils)))
        (snippet
         '(for-each delete-file-recursively
                    ;; de-blob: windows tools
                    '("dependencies/windows/tools"
                      ;; auto-generated files
                      "src/cpp/session/include/session/SessionOptions.gen.hpp"
                      "src/cpp/server/include/server/ServerOptions.gen.hpp")))))
      (build-system cmake-build-system)
      (arguments
       `(#:out-of-source? #f              ; required by 'make-writable
         #:configure-flags
         (list
          "-DRSTUDIO_TARGET=Server"
          "-DCMAKE_BUILD_TYPE=Release"
          "-DRSTUDIO_USE_SYSTEM_YAML_CPP=1"
          "-DRSTUDIO_USE_SYSTEM_BOOST=1"
          "-DRSTUDIO_USE_SYSTEM_SOCI=1"
          "-DQUARTO_ENABLED=0"
          ;; auto-detection seems to be broken with boost 1.72
          "-DRSTUDIO_BOOST_SIGNALS_VERSION=2")
         #:tests? #f                      ; no tests exist
         #:modules ((guix build cmake-build-system)
                    (guix build utils)
                    (ice-9 match))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'unpack-dictionary
             (lambda* (#:key inputs native-inputs #:allow-other-keys)
               (let ((dict-dir "dependencies/dictionaries"))
                 (mkdir dict-dir)
                 (invoke "unzip" "-qd" dict-dir (assoc-ref inputs "dict-source-tarball")))))
           ;; change the default paths for mathjax and pandoc and a hardcoded call to `which`
           (add-after 'unpack 'patch-paths
             (lambda* (#:key inputs #:allow-other-keys)
               ;; Don't build panmirror.  We already got it.
               (substitute* "src/gwt/build.xml"
                 (("target name=\"panmirror\"" m)
                  (string-append m " if=\"false\"")))
               (install-file (search-input-file inputs "/include/catch2/catch.hpp")
                             "src/cpp/tests/cpp/tests/vendor/")
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
               (substitute* "src/cpp/session/SessionModuleContext.cpp"
                 (("cmd\\(\"file\"\\)") (string-append "cmd(\"" (assoc-ref inputs "file") "/bin/file\")")))
               (substitute* "src/cpp/session/modules/SessionGit.cpp"
                 (("\"ssh-add") (string-append "\"" (assoc-ref inputs "openssh") "/bin/ssh-add"))
                 (("\"ssh-agent") (string-append "\"" (assoc-ref inputs "openssh") "/bin/ssh-agent")))
               (substitute* "src/cpp/session/modules/SessionSVN.cpp"
                 (("\"patch\"") (string-append "\"" (assoc-ref inputs "patch") "/bin/patch\"")))))
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
               (setenv "PACKAGE_OS" "GNU Guix")))
           ;; Must be run after patch-source-shebangs, which changes
           ;; report-option.sh’s interpreter line.
           (add-before 'configure 'generate-server-options
             (lambda _
               ;; Generate *.gen.hpp files deleted by source snippet.
               (with-directory-excursion "src/cpp"
                 (invoke "./generate-options.R"))))
           (add-after 'install 'install-panmirror
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((panmirror (string-append (assoc-ref inputs "js-panmirror")
                                                "/share/javascript/panmirror"))
                     (out (assoc-ref outputs "out")))
               (copy-recursively panmirror (string-append out "/www/js/panmirror"))))))))
      (native-inputs
       `(("unzip" ,unzip)
         ("catch2" ,catch2)
         ;; gwt-components are built using ant
         ("ant" ,ant)
         ("jdk" ,openjdk11 "jdk")
         ;; Copied to output
         ("js-panmirror" ,js-panmirror)
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
         ;; For `file` utility, binary file detection
         ("file" ,file)
         ;; For `patch` utility, apply SVN patches
         ("patch" ,patch)
         ;; For `ssh-add`/`ssh-agent`
         ("openssh" ,openssh)
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
      (license license:agpl3))))

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
               "-DQUARTO_ENABLED=0"
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
       ("qtdeclarative" ,qtdeclarative-5)
       ("qtlocation" ,qtlocation)
       ("qtsvg" ,qtsvg-5)
       ("qtsensors" ,qtsensors)
       ("qtxmlpatterns" ,qtxmlpatterns)
       ("qtwebchannel" ,qtwebchannel-5)
       ("qtwebengine" ,qtwebengine-5)
       ,@(package-inputs rstudio-server)))
    (synopsis "Integrated development environment (IDE) for R (desktop version)")))

