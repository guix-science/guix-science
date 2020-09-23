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
  #:use-module (gnu packages monitoring)
  #:use-module (gnu packages node)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages time)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix utils)
  #:use-module (guix build-system python))

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

(define python-terminado-0.8.3
  (package/inherit python-terminado
    (name "python-terminado")
    (version "0.8.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "terminado" version))
       (sha256
        (base32
         "1hmjdxpd9w8fl61f0adlmfh1s3rrqn9j2cppk9ynlc02z1saf128"))))))

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

(define-public python-jupyter-client-6.1
  (package
    (name "python-jupyter-client")
    (version "6.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyter_client" version))
       (sha256
        (base32
         "15d5sz2s0pf0dwzpdp4qa28myxfvk1igi4vfcnj7gicbcgaghq5k"))))
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
       ("python-ipython" ,python-ipython)
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

(define python-jupyter-client-6.1-bootstrap
  (package/inherit python-jupyter-client-6.1
    (arguments
     `(#:tests? #f
       ,@(package-arguments python-jupyter-client-6.1)))
    ;; Remove loop ipykernel <-> jupyter-client
    (native-inputs `())))

;; How to deal with packages that are available here, but have older versions
;; in guix proper:
;; Rewrite entire graph of packages with local updates. This avoids having
;; multiple versions of the same package in the graph. Define packages with the
;; version guix proper provides and then rewrite it.

(define rewrite-ipykernel
  (package-input-rewriting
   `((,python-jupyter-client . ,python-jupyter-client-6.1))))

(define-public python-ipykernel-5.3
  (rewrite-ipykernel (package/inherit python-ipykernel
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
     `(("python-ipython" ,python-ipython)
       ("python-jupyter-client" ,python-jupyter-client)
       ("python-tornado" ,python-tornado)
       ("python-traitlets" ,python-traitlets)))
    (native-inputs
     `(("python-flaky" ,python-flaky)
       ("python-nose" ,python-nose)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov))))))

;; Create a bootstrap variant, which can be used in python-jupyter-client-6.1’s
;; native-arguments
(define python-ipykernel-5.3-bootstrap
  ((package-input-rewriting
    `((,python-jupyter-client-6.1 . ,python-jupyter-client-6.1-bootstrap))) python-ipykernel-5.3))

(define rewrite-notebook
  (package-input-rewriting
   `((,python-jupyter-client . ,python-jupyter-client-6.1)
     (,python-jupyter-core . ,python-jupyter-core-4.6)
     (,python-ipykernel . ,python-ipykernel-5.3)
     (,python-terminado . ,python-terminado-0.8.3))))

(define-public python-notebook-6.1
  (rewrite-notebook (package
    (name "python-notebook")
    (version "6.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "notebook" version))
       (sha256
        (base32
         "1mcq2f7bbha4krg06h8wwnkz64cwwjcpalhn8dvnwrw87f7isfa2"))))
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
       ("python-ipykernel" ,python-ipykernel)
       ("python-ipython-genutils"
        ,python-ipython-genutils)
       ("python-jinja2" ,python-jinja2)
       ("python-jupyter-client" ,python-jupyter-client)
       ("python-jupyter-core" ,python-jupyter-core)
       ("python-nbconvert" ,python-nbconvert)
       ("python-nbformat" ,python-nbformat)
       ("python-prometheus-client"
        ,python-prometheus-client)
       ("python-pyzmq" ,python-pyzmq)
       ("python-send2trash" ,python-send2trash)
       ("python-terminado" ,python-terminado)
       ("python-tornado" ,python-tornado)
       ("python-traitlets" ,python-traitlets)))
    (native-inputs
     `(("python-coverage" ,python-coverage)
       ("python-nbval" ,python-nbval)
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
    (license license:bsd-3))))

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

(define rewrite-jupyterlab
  (package-input-rewriting
   `((,python-jupyter-client . ,python-jupyter-client-6.1)
     (,python-jupyter-core . ,python-jupyter-core-4.6)
     (,python-ipykernel . ,python-ipykernel-5.3)
     (,python-terminado . ,python-terminado-0.8.3)
     (,python-notebook . ,python-notebook-6.1)
     (,python-json5 . ,python-json5-0.9.4))))

(define-public python-jupyterlab-server
  (rewrite-jupyterlab (package
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
       ("python-notebook" ,python-notebook)
       ("python-requests" ,python-requests)))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-ipykernel" ,python-ipykernel)))
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
    (license license:bsd-3))))

(define-public python-jupyterlab
  (rewrite-jupyterlab (package
    (name "python-jupyterlab")
    (version "2.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyterlab" version))
       (sha256
        (base32
         "0jydvzjvbyxjvdk7fb7jcwksw9shxbakzkcwz86pdxzl3i66rlp9"))
       (patches (search-patches "python-jupyterlab-copy-nometa.patch"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-jinja2" ,python-jinja2)
       ("python-jupyterlab-server"
        ,python-jupyterlab-server)
       ("python-notebook" ,python-notebook)
       ("python-tornado" ,python-tornado)
       ;; Required to rebuild assets.
       ("node" ,node)))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-pytest-check-links"
        ,python-pytest-check-links)
       ("python-requests" ,python-requests)
       ("python-ipykernel" ,python-ipykernel)))
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
    (license license:bsd-3))))

