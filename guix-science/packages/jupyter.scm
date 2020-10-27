;;;
;;; Copyright © 2019, 2020 Lars-Dominik Braun <ldb@leibniz-psychology.org>
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

(define-module (guix-science packages jupyter)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu packages node)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xml)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix utils)
  #:use-module (guix build-system python)
  #:use-module (srfi srfi-1))

(define-public python-requests-unixsocket
  (package
    (name "python-requests-unixsocket")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "requests-unixsocket" version))
       (sha256
        (base32
         "1sn12y4fw1qki5gxy9wg45gmdrxhrndwfndfjxhpiky3mwh1lp4y"))))
    (build-system python-build-system)
    (native-inputs
     ;; pbr is required for setup only
     `(("python-pbr" ,python-pbr)))
    (propagated-inputs
     `(("python-requests" ,python-requests)
       ("python-urllib3" ,python-urllib3)))
    (arguments
     ;; tests depend on very specific package version, which are not available in guix
     '(#:tests? #f))
    (home-page
     "https://github.com/msabramo/requests-unixsocket")
    (synopsis
     "Use requests to talk HTTP via a UNIX domain socket")
    (description
     "Use requests to talk HTTP via a UNIX domain socket")
    (license license:asl2.0)))

(define-public python-nose-exclude
  (package
    (name "python-nose-exclude")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nose-exclude" version))
       (sha256
        (base32
         "0123x1lyv5b2p9civcfg8vilj2ga3q7p2ks1hq25z0gb3ssai3zp"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-nose" ,python-nose)))
    (home-page
     "https://github.com/kgrandis/nose-exclude")
    (synopsis
     "Exclude specific directories from nosetests runs.")
    (description
     "Exclude specific directories from nosetests runs.")
    (license license:lgpl2.0)))

(define-public python-nose-warnings-filters
  (package
    (name "python-nose-warnings-filters")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nose_warnings_filters" version))
       (sha256
        (base32
         "17dvfqfy2fm7a5cmiffw2dc3064kpx72fn5mlw01skm2rhn5nv25"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-nose" ,python-nose)))
    (home-page "")
    (synopsis
     "Allow to inject warning filters during ``nosetest``.")
    (description
     "Allow to inject warning filters during ``nosetest``.")
    (license #f)))

(define-public python-terminado-0.8.3
  (package
    (inherit python-terminado)
    (name "python-terminado")
    (version "0.8.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "terminado" version))
       (sha256
        (base32
         "1hmjdxpd9w8fl61f0adlmfh1s3rrqn9j2cppk9ynlc02z1saf128"))))))

(define-public python-testpath-0.4
  (package
    (name "python-testpath")
    (version "0.4.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "testpath" version))
        (sha256
          (base32
            "0zpcmq22dz79ipvvsfnw1ykpjcaj6xyzy7ws77s5b5ql3hka7q30"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-pathlib2" ,python-pathlib2)))
    (home-page "https://github.com/jupyter/testpath")
    (synopsis
      "Test utilities for code working with files and commands")
    (description
      "Test utilities for code working with files and commands")
    (license license:bsd-3)))

(define-public python-jupyter-core-4.6
  (package
    (name "python-jupyter-core")
    (version "4.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyter_core" version))
       (sha256
        (base32
         "0zly44i2b22bqxwya62xvsarzqqcl3wbv00qfihqhz3yg3fxakrr"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
            (if tests?
             (begin
              ; Some tests write to $HOME.
              (setenv "HOME" "/tmp")
              ; Some tests load the installed package.
              (add-installed-pythonpath inputs outputs)
              (invoke "pytest" "-vv")))))
         (add-after 'unpack 'patch-testsuite
           (lambda _
             ;; test_not_on_path() and test_path_priority() try to run a test
             ;; that loads jupyter_core, so we need PYTHONPATH
             (substitute* "jupyter_core/tests/test_command.py"
               (("env = \\{'PATH': ''\\}")
                "env = {'PATH': '', 'PYTHONPATH': os.environ['PYTHONPATH']}")
               (("env = \\{'PATH':  str\\(b\\)\\}")
                "env = {'PATH': str(b), 'PYTHONPATH': os.environ['PYTHONPATH']}"))
             #t)))))
    (propagated-inputs
     `(("python-traitlets" ,python-traitlets)))
    (native-inputs
     `(("python-six" ,python-six)
       ("python-pytest" ,python-pytest)))
    ;; This package provides the `jupyter` binary and thus, should also export the
    ;; search paths instead of the jupyter meta-package.
    (native-search-paths
     (list (search-path-specification
            (variable "JUPYTER_PATH")
            (files '("share/jupyter")))))
    (home-page "https://jupyter.org")
    (synopsis
     "Jupyter core package. A base package on which Jupyter projects rely.")
    (description
     "Jupyter core package. A base package on which Jupyter projects rely.")
    (license license:bsd-3)))

(define-public python-json-spec
  (package
    (name "python-json-spec")
    (version "0.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "json-spec" version))
        (sha256
          (base32
            "06dpbsq61ja9r89wpa2pzdii47qh3xri9ajdrgn1awfl102znchb"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-pathlib" ,python-pathlib)
        ("python-six" ,python-six)))
    (native-inputs
      `(("python-pytest" ,python-pytest)))
    (home-page "http://py.errorist.io/json-spec")
    (synopsis
      "Implements JSON Schema, JSON Pointer and JSON Reference.")
    (description
      "Implements JSON Schema, JSON Pointer and JSON Reference.")
    (license license:bsd-3)))

(define-public python-fastjsonschema
  (package
    (name "python-fastjsonschema")
    (version "2.14.5")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "fastjsonschema" version))
        (sha256
          (base32
            "1550bxk4r9z53c25da5cpwm25ng4282ik05adkj5cqzhamb27g5g"))))
    (build-system python-build-system)
    (native-inputs
      `(("python-colorama" ,python-colorama)
        ("python-json-spec" ,python-json-spec)
        ("python-jsonschema" ,python-jsonschema)
        ("python-pylint" ,python-pylint)
        ("python-pytest" ,python-pytest)
        ("python-pytest-benchmark"
         ,python-pytest-benchmark)
        ("python-pytest-cache" ,python-pytest-cache)
        ("python-validictory" ,python-validictory)))
    (home-page
      "https://github.com/seznam/python-fastjsonschema")
    (synopsis
      "Fastest Python implementation of JSON schema")
    (description
      "Fastest Python implementation of JSON schema")
    (license license:bsd-3)))

(define-public python-nbformat-5.0
  (package
    (name "python-nbformat")
    (version "5.0.8")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "nbformat" version))
        (sha256
          (base32
            "1y1h59q6z3hdlqn6z1zysk2jv3ibbbnqkzhzdg6gnnw670hv4igm"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
            (if tests?
             (begin
              (invoke "pytest" "-vv"))))))))
    (propagated-inputs
      `(("python-ipython-genutils"
         ,python-ipython-genutils)
        ("python-jsonschema" ,python-jsonschema)
        ("python-jupyter-core" ,python-jupyter-core-4.6)
        ("python-traitlets" ,python-traitlets)))
    (native-inputs
      `(("python-pytest" ,python-pytest)
        ("python-pytest-cov" ,python-pytest-cov)
        ("python-fastjsonschema" ,python-fastjsonschema) ; This is only active
        ; when setting NBFORMAT_VALIDATOR="fastjsonschema", so include it for
        ; testing only.
        ("python-testpath" ,python-testpath-0.4)))
    (home-page "http://jupyter.org")
    (synopsis "The Jupyter Notebook format")
    (description "The Jupyter Notebook format")
    (license license:bsd-3)))

(define-public python-jupyter-client-6.1
  (package
    (name "python-jupyter-client")
    (version "6.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyter_client" version))
       (sha256
        (base32
         "18bpjg81q8h567784s0vc2iz20xrky6s4kkh4ikj5d74dyrr1qs9"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-testsuite
           (lambda _
             ;; There is no such argument “encoding”.
             (substitute* "jupyter_client/tests/test_session.py"
               (("msgpack.unpackb\\(buf, encoding='utf8'\\)")
                "msgpack.unpackb(buf)"))
             ;; Test fails for unknown reason.
             (substitute* "jupyter_client/tests/test_session.py"
               (("import hmac" all) (string-append all "\nimport unittest"))
               (("(.+)(def test_tracking)" all indent def)
                (string-append indent "@unittest.skip('disabled by guix')\n"
                               indent def)))
             #t))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
            (if tests?
             (begin
               ;; Some tests try to write to $HOME.
               (setenv "HOME" "/tmp")
               (invoke "pytest" "-vv"))))))))
    (propagated-inputs
     `(("python-dateutil" ,python-dateutil)
       ("python-jupyter-core" ,python-jupyter-core-4.6)
       ("python-pyzmq" ,python-pyzmq)
       ("python-tornado" ,python-tornado)
       ("python-traitlets" ,python-traitlets)))
    (native-inputs
     `(("python-async-generator"
        ,python-async-generator)
       ("python-ipykernel" ,python-ipykernel-5.3-bootstrap)
       ("python-ipython" ,python-ipython-with-updates)
       ("python-mock" ,python-mock)
       ("python-msgpack" ,python-msgpack)
       ("python-pytest" ,python-pytest)
       ("python-pytest-asyncio" ,python-pytest-asyncio)
       ("python-pytest-timeout" ,python-pytest-timeout)))
    (home-page "https://jupyter.org")
    (synopsis
     "Jupyter protocol implementation and client libraries")
    (description
     "Jupyter protocol implementation and client libraries")
    (license license:bsd-3)))

;; Upstream version with dep to ipykernel removed.
(define-public python-jupyter-client-6.1-bootstrap
  (let ((base python-jupyter-client-6.1))
    (package
      (inherit base)
      (name "python-jupyter-client-bootstrap")
      (arguments
       `(#:tests? #f
         ,@(package-arguments base)))
      ;; Remove loop ipykernel <-> jupyter-client
      (native-inputs `()))))

(define-public python-ipython-with-updates
  (let ((parent python-ipython))
    (package
    (inherit parent)
    (propagated-inputs
      `(("python-terminado" ,python-terminado-0.8.3)
        ("python-nbformat" ,python-nbformat-5.0)
        ,@(fold alist-delete (package-propagated-inputs parent)
                 '("python-terminado" "python-nbformat"))))
    (native-inputs
      `(("python-testpath" ,python-testpath-0.4)
        ,@(fold alist-delete (package-native-inputs parent)
                 '("python-testpath")))))))

(define-public python-ipykernel-5.3
  (package
    (inherit python-ipykernel)
    (name "python-ipykernel")
    (version "5.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ipykernel" version))
       (sha256
        (base32
         "17g8pfmvj38kkmfklfacar7m7h0b0wnk0qhw4cdnm6072spm49lv"))))
    (propagated-inputs
     `(("python-ipython" ,python-ipython-with-updates)
       ("python-jupyter-client" ,python-jupyter-client-6.1)
       ("python-tornado" ,python-tornado)
       ("python-traitlets" ,python-traitlets)))
    (native-inputs
     `(("python-flaky" ,python-flaky)
       ("python-nose" ,python-nose)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)))))

;; Create a bootstrap variant, which can be used in python-jupyter-client-6.1’s
;; native-arguments
(define-public python-ipykernel-5.3-bootstrap
  (let ((parent python-ipykernel-5.3))
    (package
    (inherit parent)
    (name "python-ipykernel-bootstrap")
    (propagated-inputs
      `(("python-jupyter-client" ,python-jupyter-client-6.1-bootstrap)
        ,@(fold alist-delete (package-propagated-inputs parent)
               '("python-jupyter-client")))))))

(define-public python-jupyterlab-pygments
  (package
    (name "python-jupyterlab-pygments")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "jupyterlab_pygments" version))
        (sha256
          (base32
            "0ij14mmnc39nmf84i0av6j9glazjic7wzv1qyhr0j5966s3s1kfg"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-pygments" ,python-pygments)))
    (home-page "http://jupyter.org")
    (synopsis
      "Pygments theme using JupyterLab CSS variables")
    (description
      "Pygments theme using JupyterLab CSS variables")
    (license license:bsd-3)))

(define-public python-nbconvert-6.0
  (package
    (name "python-nbconvert")
    (version "6.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "nbconvert" version))
        (sha256
          (base32
            "00lhqaxn481qvk2w5568asqlsnvrw2fm61p1vssx3m7vdnl17g6b"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths-and-tests
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((pandoc (string-append (assoc-ref inputs "pandoc") "/bin/pandoc"))
                   (texlive-root (string-append (assoc-ref inputs "texlive")))
                   (xelatex (string-append texlive-root "/bin/xelatex"))
                   (bibtex (string-append texlive-root "/bin/bibtex")))
               ;; Use pandoc binary from input.
               (substitute* "nbconvert/utils/pandoc.py"
                 (("'pandoc'") (string-append "'" pandoc "'")))
               ;; Same for LaTeX.
               (substitute* "nbconvert/exporters/pdf.py"
                 (("\"xelatex\"") (string-append "\"" xelatex "\""))
                 (("\"bibtex\"") (string-append "\"" bibtex "\"")))
               ;; Make sure tests are not skipped.
               (substitute* (find-files "." "test_.+\\.py$")
                 (("@onlyif_cmds_exist\\(('(pandoc|xelatex)'(, )?)+\\)") ""))
              ;; Pandoc is never missing, disable test.
              (substitute* "nbconvert/utils/tests/test_pandoc.py"
                (("import os" all) (string-append all "\nimport pytest"))
                (("(.+)(def test_pandoc_available)" all indent def)
                (string-append indent "@pytest.mark.skip('disabled by guix')\n"
                               indent def)))
              ; Not installing pyppeteer, delete test.
              (delete-file "nbconvert/exporters/tests/test_webpdf.py")
              (substitute* "nbconvert/tests/test_nbconvertapp.py"
                (("(.+)(def test_webpdf_with_chromium)" all indent def)
                (string-append indent "@pytest.mark.skip('disabled by guix')\n"
                               indent def)))
             #t)))
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
            (if tests?
             (begin
              ; Some tests write to $HOME.
              (setenv "HOME" "/tmp")
              ; Tests depend on templates installed to output.
              (setenv "JUPYTER_PATH"
                      (string-append
                        (assoc-ref outputs "out")
                        "/share/jupyter:"
                        (getenv "JUPYTER_PATH")))
              ; Some tests invoke the installed nbconvert binary.
              (add-installed-pythonpath inputs outputs)
              (invoke "pytest" "-vv"))))))))
    (inputs
      `(("pandoc" ,pandoc)
        ; XXX: Disabled, needs substitute*.
        ;("inkscape" ,inkscape)
        ("texlive" ,texlive)))
    (propagated-inputs
      `(("python-bleach" ,python-bleach)
        ("python-defusedxml" ,python-defusedxml)
        ("python-entrypoints" ,python-entrypoints)
        ("python-jinja2" ,python-jinja2)
        ("python-jupyter-core" ,python-jupyter-core-4.6)
        ("python-jupyterlab-pygments"
         ,python-jupyterlab-pygments)
        ("python-mistune" ,python-mistune)
        ("python-nbclient" ,python-nbclient-0.5)
        ("python-nbformat" ,python-nbformat-5.0)
        ("python-pandocfilters" ,python-pandocfilters)
        ("python-pygments" ,python-pygments)
        ("python-testpath" ,python-testpath-0.4)
        ("python-traitlets" ,python-traitlets)))
    (native-inputs
      `(("python-ipykernel" ,python-ipykernel-5.3)
        ("python-ipywidgets" ,python-ipywidgets)
        ; XXX: Disabled, not in guix.
        ;("python-pyppeteer" ,python-pyppeteer)
        ("python-pytest" ,python-pytest)
        ("python-pytest-cov" ,python-pytest-cov)
        ("python-pytest-dependency"
         ,python-pytest-dependency)))
    (home-page "https://jupyter.org")
    (synopsis "Converting Jupyter Notebooks")
    (description "Converting Jupyter Notebooks")
    (license license:bsd-3)))

(define-public python-nbval-for-notebook
  (package
    (inherit python-nbval)
    (propagated-inputs
     `(("python-ipykernel" ,python-ipykernel-5.3)
       ("python-jupyter-client" ,python-jupyter-client-6.1)
       ("python-nbformat" ,python-nbformat-5.0)
       ("python-six" ,python-six)))))

(define-public python-notebook-6.1
  (package
    (name "python-notebook")
    (version "6.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "notebook" version))
       (sha256
        (base32
         "0cnyi4zd3byh7zixdj2q71axm31xgjiyfklh1c63c87acgwh2zb8"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-testsuite
           (lambda _
             ;; These tests try to delete a file using gio, which fails for
             ;; /tmp.
             (substitute* "notebook/services/contents/tests/test_contents_api.py"
               ;; Add import, so we can call skip() below.
               (("import io" all) (string-append all "\nimport unittest"))
               (("(.+)(def (test_checkpoints_follow_file|test_delete|test_delete_dirs|test_delete_non_empty_dir))" all indent def)
                (string-append indent "@unittest.skip('disabled by guix')\n"
                               indent def)))
             ;; Same here.
             (substitute* "notebook/services/contents/tests/test_manager.py"
               ;; Add import, so we can call skip() below.
               (("import os" all) (string-append all "\nimport unittest"))
               (("(.+)(def test_delete)" all indent def)
                (string-append indent "@unittest.skip('disabled by guix')\n"
                               indent def)))
             ;; These tests check paths.jupyter_path(). I don’t know what
             ;; the expected outcome would be for guix.
             (substitute* "notebook/tests/test_serverextensions.py"
               ;; Add import, so we can call skip() below.
               (("import os" all) (string-append all "\nimport unittest"))
               (("(.+)(def (test_disable|test_enable|test_merge_config|test_load_ordered))" all indent def)
                (string-append indent "@unittest.skip('disabled by guix')\n"
                               indent def)))
             #t))
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
            (if tests?
             (begin
               ;; Some tests try to write to $HOME.
               (setenv "HOME" "/tmp")
               (add-installed-pythonpath inputs outputs)
               ;; Some tests call `jupyter-notebook`.
               (setenv "PATH" (string-append (getenv "PATH") ":" (assoc-ref outputs "out") "/bin"))
               ;; Python tests.
               (invoke "nosetests" "-v" "--exclude-dir" "notebook/tests/selenium")
               ;; Browser tests. Fail with: No module named 'testpath'
               ;(invoke "pytest" "-sv" "notebook/tests/selenium")
             )))))))
    (propagated-inputs
     `(("python-argon2-cffi" ,python-argon2-cffi)
       ("python-ipykernel" ,python-ipykernel-5.3)
       ("python-ipython-genutils"
        ,python-ipython-genutils)
       ("python-jinja2" ,python-jinja2)
       ("python-jupyter-client" ,python-jupyter-client-6.1)
       ("python-jupyter-core" ,python-jupyter-core-4.6)
       ("python-nbconvert" ,python-nbconvert-6.0)
       ("python-nbformat" ,python-nbformat-5.0)
       ("python-prometheus-client"
        ,python-prometheus-client)
       ("python-pyzmq" ,python-pyzmq)
       ("python-send2trash" ,python-send2trash)
       ("python-terminado" ,python-terminado-0.8.3)
       ("python-tornado" ,python-tornado)
       ("python-traitlets" ,python-traitlets)))
    (native-inputs
     `(("python-coverage" ,python-coverage)
       ("python-nbval" ,python-nbval-for-notebook)
       ("python-nose" ,python-nose)
       ("python-nose-exclude" ,python-nose-exclude)
       ("python-nose-warnings-filters"
        ,python-nose-warnings-filters)
       ("python-requests" ,python-requests)
       ("python-requests-unixsocket"
        ,python-requests-unixsocket)))
    (home-page "http://jupyter.org")
    (synopsis
     "A web-based notebook environment for interactive computing")
    (description
     "A web-based notebook environment for interactive computing")
    (license license:bsd-3)))

(define-public python-json5-0.9.4
  (package
    (inherit python-json5)
    (name "python-json5")
    (version "0.9.4")
    (source
     (origin
       ;; sample.json5 is missing from PyPi source tarball
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dpranke/pyjson5.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14878npsn7f344pwkxcnw40lc0waqgpi8j25akd7qxlwd7nchy40"))))))

(define-public python-jupyterlab-server
  (package
    (name "python-jupyterlab-server")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyterlab_server" version))
       (sha256
        (base32
         "132xby7531rbrjg9bqvsx86birr1blynjxy8gi5kcnb6x7fxjcal"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-jinja2" ,python-jinja2)
       ("python-json5" ,python-json5)
       ("python-jsonschema" ,python-jsonschema)
       ("python-notebook" ,python-notebook-6.1)
       ("python-requests" ,python-requests)))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-ipykernel" ,python-ipykernel-5.3)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; python setup.py test does not invoke pytest?
         (replace 'check
           (lambda _
             (invoke "pytest" "-vv"))))))
    (home-page "https://jupyter.org")
    (synopsis "JupyterLab Server")
    (description "A set of server components for JupyterLab and JupyterLab like
applications")
    (license license:bsd-3)))

(define-public python-jupyterlab
  (package
    (name "python-jupyterlab")
    (version "2.2.9")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyterlab" version))
       (sha256
        (base32
         "1iixsfhvdh95f13lm0hz280wixdnphxns6wchgfm6dqpxbnzis1v"))
       (patches (search-patches "python-jupyterlab-copy-nometa.patch"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-jinja2" ,python-jinja2)
       ("python-jupyterlab-server"
        ,python-jupyterlab-server)
       ("python-notebook" ,python-notebook-6.1)
       ("python-tornado" ,python-tornado)
       ;; Required to rebuild assets.
       ("node" ,node)))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-pytest-check-links"
        ,python-pytest-check-links)
       ("python-requests" ,python-requests)
       ("python-ipykernel" ,python-ipykernel-5.3)))
    (arguments
     ;; testing requires npm, so disabled for now
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-syspath
           (lambda* (#:key outputs inputs configure-flags #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (substitute* "jupyterlab/commands.py"
                 ;; sys.prefix defaults to Python’s prefix in the store, not
                 ;; jupyterlab’s. Fix that.
                 (("sys\\.prefix")
                  (string-append "'" out "'"))))
             #t))
         ;; 'build does not respect configure-flags
         (replace 'build
           (lambda _
             (invoke "python" "setup.py" "build" "--skip-npm"))))
       #:configure-flags (list "--skip-npm")))
    (home-page "https://jupyter.org")
    (synopsis
     "The JupyterLab notebook server extension")
    (description
     "An extensible environment for interactive and reproducible computing,
based on the Jupyter Notebook and Architecture.")
    (license license:bsd-3)))

(define-public python-pytest-dependency
  (package
    (name "python-pytest-dependency")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pytest-dependency" version))
        (sha256
          (base32
            "0swl3mxca7nnjbb5grfzrm3fa2750h9vjsha0f2kyrljc6895a62"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-pytest" ,python-pytest)))
    (home-page
      "https://github.com/RKrahl/pytest-dependency")
    (synopsis "Manage dependencies of tests")
    (description "Manage dependencies of tests")
    (license #f)))

(define-public python-nest-asyncio
  (package
    (name "python-nest-asyncio")
    (version "1.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "nest_asyncio" version))
        (sha256
          (base32
            "00afn4h0gh2aa5lb1x3ql5lh6s0wqx5qjyccrzn2wnysmf9k2v5q"))))
    (build-system python-build-system)
    (home-page
      "https://github.com/erdewit/nest_asyncio")
    (synopsis
      "Patch asyncio to allow nested event loops")
    (description
      "Patch asyncio to allow nested event loops")
    (license license:bsd-3)))

(define-public python-nbclient-0.5
  (package
    (name "python-nbclient")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "nbclient" version))
        (sha256
          (base32
            "0l02zvadrzbj7zvl5l9ji1z58lql8pi8fjmpdpg2rbvfs4kdgqh1"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
            (if tests?
             (begin
              ;; Some tests write to homedir. IPython prints an unexpected
              ;; warning, which causes tests to fail.
              (setenv "HOME" "/tmp")
              (invoke "pytest" "-vv"))))))))
    (propagated-inputs
      `(("python-async-generator"
         ,python-async-generator)
        ("python-jupyter-client" ,python-jupyter-client-6.1)
        ("python-nbformat" ,python-nbformat-5.0)
        ("python-nest-asyncio" ,python-nest-asyncio)
        ("python-traitlets" ,python-traitlets)))
    (native-inputs
      `(("python-black" ,python-black)
        ("python-bumpversion" ,python-bumpversion)
        ("python-check-manifest" ,python-check-manifest)
        ("python-codecov" ,python-codecov)
        ("python-coverage" ,python-coverage)
        ("python-flake8" ,python-flake8)
        ("python-ipykernel" ,python-ipykernel)
        ("python-ipython" ,python-ipython)
        ("python-ipywidgets" ,python-ipywidgets)
        ("python-mypy" ,python-mypy)
        ("python-pytest" ,python-pytest)
        ("python-pytest-cov" ,python-pytest-cov)
        ("python-testpath" ,python-testpath-0.4) ; missing from setup.py
        ("python-tox" ,python-tox)
        ("python-twine" ,python-twine)
        ("python-wheel" ,python-wheel)
        ("python-xmltodict" ,python-xmltodict)))
    (home-page "https://jupyter.org")
    (synopsis
      "A client library for executing notebooks. Formally nbconvert's ExecutePreprocessor.")
    (description
      "A client library for executing notebooks. Formally nbconvert's ExecutePreprocessor.")
    (license license:bsd-3)))

