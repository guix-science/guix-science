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
  #:use-module (gnu packages databases)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages jupyter)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu packages node)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages serialization)
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
    (version "2.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyterlab_server" version))
       (sha256
        (base32
         "1adw3al6nn92555j3lvzhh5m093gqpfr5a06k34y5i3pk6svnl5q"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-babel python-entrypoints python-jinja2 python-json5 python-jsonschema
           python-packaging
           python-requests
           python-jupyter-server))
    (native-inputs
     (list python-pytest python-ipykernel))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         ;; python setup.py test does not invoke pytest?
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest" "-vv")))))))
    (home-page "https://jupyterlab-server.readthedocs.io/")
    (synopsis "JupyterLab Server")
    (description "A set of server components for JupyterLab and JupyterLab like
applications")
    (license license:bsd-3)))

(define-public python-nbclassic
  (package
    (name "python-nbclassic")
    (version "0.3.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "nbclassic" version))
        (sha256
          (base32 "0y2m1zr9x03ys8dz5slga9v51snfsg67c9id6gykiz0897phf2zh"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs (list python-jupyter-server python-notebook))
;    (native-inputs
;      (list python-pytest
;            python-pytest-console-scripts
;            python-pytest-tornasync))
    (home-page "https://github.com/jupyterlab/nbclassic")
    (synopsis "Jupyter Notebook as a Jupyter Server extension.")
    (description "Jupyter Notebook as a Jupyter Server extension.")
    (license #f)))

(define-public python-jupyterlab
  (package
    (name "python-jupyterlab")
    (version "3.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyterlab" version))
       (sha256
        (base32
         "0jl7c8asqdc0dfnfy9jvkdaz09jm70y9g0q50b92cn0g7d3qzcii"))
       ;(patches (search-patches "python-jupyterlab-copy-nometa.patch"))
      )
     )
    (build-system python-build-system)
    (propagated-inputs
     (list python-jinja2
           python-jupyterlab-server
           python-notebook
           python-tornado-6
           python-nbclassic
           ;; Required to rebuild assets.
           node))
    (native-inputs
     (list python-pytest python-pytest-check-links python-requests
           python-ipykernel python-jupyter-packaging))
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
         ;; Remove circular dependency on jupyterhub to keep 'sanity-check happy.
         (add-after 'unpack 'disable-jupyterhub
           (lambda _
             (substitute* "setup.cfg"
               ((".*jupyter-labhub = .*") ""))))
         (delete 'build)
         ;; Files are set to a timestamp in 1970, but ZIP only supports
         ;; >1980. Fortunately python-wheel respects this envvar.
         (add-before 'install 'set-SOURCE_DATE_EPOCH
           (lambda _
             (setenv "SOURCE_DATE_EPOCH" "1531865062")))
         ;; For some reason the setup.py-based installer is broken and does not install/build(?) assets.
         (replace 'install
           (lambda* (#:key outputs inputs configure-flags #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (setenv "HOME" "/tmp")
               (invoke "pip" "install" "--prefix" out "--no-build-isolation" ".")))))))
    (home-page "https://jupyter.org")
    (synopsis
     "The JupyterLab notebook server extension")
    (description
     "An extensible environment for interactive and reproducible computing,
based on the Jupyter Notebook and Architecture.")
    (license license:bsd-3)))

;; Cannot be upstreamed, bundles lots of JavaScript.
(define-public python-jupyterhub
  (package
   (name "python-jupyterhub")
   (version "1.4.1")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "jupyterhub" version))
            (sha256
             (base32
              "16aibgv34ndvkll3ax1an8m859jcf05ybqwnjwrhp3nvlhc0f6zf"))))
   (build-system python-build-system)
   (arguments
    `(#:tests? #f))
   (propagated-inputs
    `(("python-alembic" ,python-alembic)
      ("python-async-generator" ,python-async-generator)
      ("python-certipy" ,python-certipy)
      ("python-dateutil" ,python-dateutil)
      ("python-entrypoints" ,python-entrypoints)
      ("python-jinja2" ,python-jinja2)
      ("python-oauthlib" ,python-oauthlib)
      ("python-pamela" ,python-pamela)
      ("python-prometheus-client" ,python-prometheus-client)
      ("python-requests" ,python-requests)
      ("python-sqlalchemy" ,python-sqlalchemy)
      ("python-tornado" ,python-tornado)
      ("python-traitlets" ,python-traitlets)
      ("python-jupyter-telemetry" ,python-jupyter-telemetry)
      ("python-notebook" ,python-notebook)))
   (home-page "https://jupyter.org")
   (synopsis "JupyterHub: A multi-user server for Jupyter notebooks")
   (description "JupyterHub: A multi-user server for Jupyter notebooks")
   (license license:bsd-3)))

;; Cannot be upstreamed: Depends on JupyterHub
(define-public python-batchspawner
  (package
    (name "python-batchspawner")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "batchspawner" version))
              (sha256
               (base32
                "0fnxr6ayp9vzsv0c0bfrzl85liz5zb4kpk4flldb36xxq7vp5blv"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (propagated-inputs
     `(("python-jupyterhub" ,python-jupyterhub)
       ("python-pamela" ,python-pamela)))
    (home-page "http://jupyter.org")
    (synopsis "Add-on for Jupyterhub to spawn notebooks using batch systems")
    (description
     "This package provides a spawner for Jupyterhub to spawn notebooks using
batch resource managers.")
    (license license:bsd-3)))

(define-public python-jupyter-telemetry
  (package
    (name "python-jupyter-telemetry")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "jupyter_telemetry" version))
              (sha256
               (base32
                "052khyn6h97jxl3k5i2m81xvga5v6vwh5qixzrax4w6zwcx62p24"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-json-logger" ,python-json-logger)
       ("python-jsonschema" ,python-jsonschema)
       ("python-ruamel.yaml" ,python-ruamel.yaml)
       ("python-traitlets" ,python-traitlets)))
    (home-page "https://jupyter.org/")
    (synopsis "Jupyter telemetry library")
    (description "Jupyter telemetry library")
    (license license:bsd-3)))

;; Cannot be upstreamed: Depends on JupyterHub
(define-public python-jupyterhub-ldapauthenticator
  (package
    (name "python-jupyterhub-ldapauthenticator")
    (version "1.3.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "jupyterhub-ldapauthenticator" version))
              (sha256
               (base32
                "12xby5j7wmi6qsbb2fjd5qbckkcg5fmdij8qpc9n7ci8vfxq303m"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-jupyterhub" ,python-jupyterhub)
       ("python-jupyter-telemetry" ,python-jupyter-telemetry)
       ("python-ldap3" ,python-ldap3)
       ("python-tornado" ,python-tornado)
       ("python-traitlets" ,python-traitlets)))
    (home-page "https://github.com/yuvipanda/ldapauthenticator")
    (synopsis "LDAP Authenticator for JupyterHub")
    (description "LDAP Authenticator for JupyterHub")
    (license license:bsd-3)))

;; Cannot be upstreamed: Depends on JupyterHub
(define-public python-wrapspawner
  (package
    (name "python-wrapspawner")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "wrapspawner" version))
              (sha256
               (base32
                "1fqlzg06mpglfgkrh9117sx3dj4b2y3xqh476q89533j9v98qqyr"))))
    (inputs
     `(("jupyter" ,jupyter)
       ("python-tornado" ,python-tornado)
       ("python-jupyterhub" ,python-jupyterhub)))
    (build-system python-build-system)
    (home-page "https://github.com/jupyterhub/wrapspawner")
    (synopsis "Wrapspawner for JupyterHub")
    (description "This package includes @code{WrapSpawner} and
@code{ProfilesSpawner}, which provide mechanisms for runtime configuration of
spawners.  The inspiration for their development was to allow users to select
from a range of pre-defined batch job profiles, but their operation is
completely generic.")
    (license license:bsd-3)))
