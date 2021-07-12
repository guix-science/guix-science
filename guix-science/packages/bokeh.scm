;;;
;;; Copyright © 2021 Lars-Dominik Braun <lars@6xq.net>
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

(define-module (guix-science packages bokeh)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages node)
  #:use-module (gnu packages time)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz))

;; Cannot be upstreamed to Guix proper, because it bundles large amounts of
;; pre-built JavaScript.
(define-public python-bokeh
  (package
    (name "python-bokeh")
    (version "2.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "bokeh" version))
        (sha256
          (base32
            "0xxnvqf4sa7cvlx8bmcj8q8y9z3k14q738y5zi3i8mim30ccrzd5"))))
    (build-system python-build-system)
    (arguments
     ;; Alot of tests are broken, because the pypi distribution does not come
     ;; with conftest.py. But we also cannot use the git repository, because it
     ;; lacks a prebuilt bokehjs. Disable tests instead and pray.
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               ;; NPM needs a home directory.
               (setenv "HOME" "/tmp")
               (for-each delete-file
                         ;; Skip tests requiring selenium.
                         '("tests/unit/bokeh/embed/test_standalone.py"
                           "tests/unit/bokeh/io/test_export.py"
                           "tests/unit/bokeh/io/test_webdriver.py"
                           ;; NPM cannot fetch dependencies in a sandbox.
                           "tests/unit/bokeh/embed/test_bundle.py"
                           "tests/unit/bokeh/test_ext.py"))
               (add-installed-pythonpath inputs outputs)
               ;; Bokeh’s sampledata is in a terrible shape. First, it’s not
               ;; possible to download the entire thing. You have to fetch
               ;; files individually. Then it’s stored in a per-user directory
               ;; (~/.bokeh/data), which means we cannot install it globally.
               ;; Just ignore it and let the user handle it.
               (invoke "pytest" "-m" "not sampledata" "tests"))
             #t)))))
    (propagated-inputs
      `(("python-dateutil" ,python-dateutil)
        ("python-jinja2" ,python-jinja2)
        ("python-numpy" ,python-numpy)
        ("python-packaging" ,python-packaging)
        ("python-pillow" ,python-pillow)
        ("python-pyyaml" ,python-pyyaml)
        ("python-tornado" ,python-tornado-6)
        ("python-typing-extensions"
         ,python-typing-extensions)))
    (native-inputs
      `(("python-pandas" ,python-pandas)
        ("python-mock" ,python-mock)
        ("python-pytest" ,python-pytest)
        ("python-pytest-asyncio" ,python-pytest-asyncio)
        ("python-flaky" ,python-flaky)
        ("python-networkx" ,python-networkx)
        ("python-beautifulsoup4" ,python-beautifulsoup4)
        ("python-nbconvert" ,python-nbconvert)
        ("python-requests" ,python-requests)
        ("python-icalendar" ,python-icalendar)
        ("node" ,node-lts)))
    (home-page "https://github.com/bokeh/bokeh")
    (synopsis
      "Interactive Data Visualization in the browser for Python")
    (description
      "Bokeh is an interactive visualization library for modern web browsers.
It provides elegant, concise construction of versatile graphics, and affords
high-performance interactivity over large or streaming datasets.  Bokeh can
help anyone who would like to quickly and easily make interactive plots,
dashboards, and data applications.")
    (license license:bsd-3)))

