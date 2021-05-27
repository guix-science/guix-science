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
  #:use-module (gnu packages jupyter)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu packages node)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rdf)
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
       ("python-notebook" ,python-notebook)
       ("python-tornado" ,python-tornado-6)
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
    (license license:bsd-3)))

(define-public python-sparqlwrapper
  (package
    (name "python-sparqlwrapper")
    (version "1.8.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/RDFLib/sparqlwrapper/archive/"
                    version ".tar.gz"))
              (sha256
               (base32
                "0shc8y36bdyql9fzhggka88nb163h79pk965m9vqmb01y42zmigp"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (propagated-inputs
     `(("python-rdflib" ,python-rdflib)))
    (home-page "http://rdflib.github.io/sparqlwrapper")
    (synopsis "SPARQL Endpoint interface to Python")
    (description "SPARQL Endpoint interface to Python")
    (license license:w3c)))

(define-public python-sparqlkernel
  (package
    (name "python-sparqlkernel")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sparqlkernel" version))
              (sha256
               (base32
                "004v22nyi5cnpxq4fiws89p7i5wcnzv45n3n70axdd6prh6rkapx"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-traitlets" ,python-traitlets)
       ("python-notebook" ,python-notebook)
       ("python-ipykernel" ,python-ipykernel)
       ("python-html5lib" ,python-html5lib-0.9)))
    (propagated-inputs
     `(("python-sparqlwrapper" ,python-sparqlwrapper)
       ("python-pygments" ,python-pygments)))
    (home-page "https://github.com/paulovn/sparql-kernel")
    (synopsis "Jupyter kernel for SPARQL")
    (description "This package provides a Jupyter kernel for running SPARQL
queries.")
    (license license:bsd-3)))
