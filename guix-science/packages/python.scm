;;;
;;; Copyright © 2016, 2017, 2018, 2019, 2020, 2021 Roel Janssen <roel@gnu.org>
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

(define-module (guix-science packages python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages check)
  #:use-module (gnu packages)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public python-pytest-rerunfailures
  (package
    (name "python-pytest-rerunfailures")
    (version "10.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pytest-rerunfailures" version))
              (sha256
               (base32
                "14dksvc8mxmci3m3083xzwz70anis95a5s01apqvvqv2z48yc1ir"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pytest" ,python-pytest)
       ("python-setuptools" ,python-setuptools)))
    (home-page
     "https://github.com/pytest-dev/pytest-rerunfailures")
    (synopsis "Plugin for pytest to re-run tests to eliminate flaky failures")
    (description "This package provides a @code{pytest} plugin to re-run tests
to eliminate flaky failures.")
    (license license:mpl2.0)))

(define-public python-parameterizedtestcase
  (package
    (name "python-parameterizedtestcase")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "parameterizedtestcase" version))
              (sha256
               (base32
                "0zhjmsd16xacg4vd7zb75kw8q9khn52wvad634v1bvz7swaivk2c"))))
    (build-system python-build-system)
    (home-page
     "https://github.com/msabramo/python_unittest_parameterized_test_case")
    (synopsis "Parameterized tests for Python's unittest module")
    (description
     "Parameterized tests for Python's unittest module")
    (license license:expat)))

(define-public python-google-cloud-storage
  (package
    (name "python-google-cloud-storage")
    (version "1.38.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "google-cloud-storage" version))
              (sha256
               (base32
                "1i26jiasa9n10bm6lm1i299cqrh0mxfrlq1ng5fxrf34dzb1280n"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (home-page "https://github.com/googleapis/python-storage")
    (synopsis "Google Cloud Storage API client library")
    (description "Google Cloud Storage allows you to store data on Google
infrastructure with very high reliability, performance and availability,
and can be used to distribute large data objects to users via direct
download.")
    (license license:asl2.0)))

;; We might be missing a few Azure dependencies.
(define-public python-smart-open
  (package
   (name "python-smart-open")
   (version "5.1.0")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "smart_open" version))
            (sha256
             (base32
              "0bp9a832903zx5k9hqdjgd3ybm5zc7ry5r23wdchgvs0n9817p74"))))
   (build-system python-build-system)
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        (add-before 'check 'relax-moto-requirement
          (lambda* (#:key outputs inputs #:allow-other-keys)
            (substitute* (list "setup.py" "smart_open.egg-info/requires.txt")
              (("moto\\[server\\]==1.3.14") "moto")))))))
   (inputs
    `(("python-boto3" ,python-boto3)
      ("python-google-cloud-storage" ,python-google-cloud-storage)
      ("python-moto" ,python-moto)
      ("python-parameterizedtestcase" ,python-parameterizedtestcase)
      ("python-paramiko" ,python-paramiko)
      ("python-pathlib2" ,python-pathlib2)
      ("python-pytest" ,python-pytest)
      ("python-pytest-rerunfailures" ,python-pytest-rerunfailures)
      ("python-requests" ,python-requests)
      ("python-responses" ,python-responses)))
   (home-page "https://github.com/piskvorky/smart_open")
   (synopsis "Utilities for streaming large files")
   (description "This package provides a library for efficient streaming of
very large files from/to storages such as S3, GCS, HDFS, WebHDFS, HTTP, HTTPS,
SFTP, or local filesystem. It supports transparent, on-the-fly (de-)compression
for a variety of different formats.  This package is a drop-in replacement for
 Python's built-in @code{open()}: it can do anything @code{open()} can plus
lots of nifty extra stuff on top.")
   (license license:expat)))

(define-public python-morfessor
  (package
    (name "python-morfessor")
    (version "2.0.6")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "Morfessor" version))
              (sha256
               (base32
                "1cmsxyd7ymlqlgam9a6va0x3fqhz0w1mixj0yv2j85rl6k1flfxv"))))
    (build-system python-build-system)
    (home-page "http://morpho.aalto.fi")
    (synopsis "Morfessor")
    (description "Morfessor")
    (license license:bsd-3)))

(define-public python-torchfile
  (package
    (name "python-torchfile")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "torchfile" version))
              (sha256
               (base32
                "0vhklj6krl9r0kdynb4kcpwp8y1ihl2zw96byallay3k9c9zwgd5"))))
    (build-system python-build-system)
    (home-page "https://github.com/bshillingford/python-torchfile")
    (synopsis "Torch7 binary serialized file parser")
    (description "Torch7 binary serialized file parser")
    (license license:bsd-3)))

(define-public pybind11-2.6.1
  (package (inherit pybind11)
    (name "pybind11")
    (version "2.6.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pybind/pybind11")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "1wh5b1xnywzxwxkyac2wvyqwzmy1qxs341jjk820r7b825wn6yad"))
              (file-name (git-file-name name version))))))

(define-public python-nmslib
  (package
  (name "python-nmslib")
  (version "2.1.1")
  (source (origin
            (method url-fetch)
            (uri (pypi-uri "nmslib" version))
            (sha256
             (base32
              "084wl5kl2grr2yi3bibc6i2ak5s7vanwi21wssbwd4bgfskr84lp"))))
  (build-system python-build-system)
  (propagated-inputs
   `(("python-numpy" ,python-numpy)
     ("python-psutil" ,python-psutil)
     ("pybind11" ,pybind11-2.6.1)))
  (home-page "https://github.com/nmslib/nmslib")
  (synopsis "Non-Metric Space Library (NMSLIB)")
  (description "Non-Metric Space Library (NMSLIB)")
  (license license:asl2.0)))

(define-public python-pyemd
  (package
    (name "python-pyemd")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pyemd" version))
              (sha256
               (base32
                "0w3yw014760ncm09ymbh0wnw4wwz7ph773dvvxcyaww5dw8w50gw"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-numpy" ,python-numpy)))
    (home-page "http://github.com/wmayner/pyemd")
    (synopsis
     "Wrapper for Pele and Werman's implementation of the Earth Mover's Distance")
    (description
     "This package provides a Python wrapper for Ofir Pele and Michael Werman's
implementation of the Earth Mover's Distance.")
    (license license:expat)))

(define-public python-visdom
  (package
  (name "python-visdom")
  (version "0.1.8.9")
  (source (origin
            (method url-fetch)
            (uri (pypi-uri "visdom" version))
            (sha256
             (base32
              "09kiczx2i5asqsv214fz7sx8wlyldgbqvxwrd0alhjn24cvx4fn7"))))
  (build-system python-build-system)
  (propagated-inputs
   `(("python-jsonpatch" ,python-jsonpatch)
     ("python-numpy" ,python-numpy)
     ("python-pillow" ,python-pillow)
     ("python-pyzmq" ,python-pyzmq)
     ("python-requests" ,python-requests)
     ("python-scipy" ,python-scipy)
     ("python-six" ,python-six)
     ("python-torchfile" ,python-torchfile)
     ("python-tornado" ,python-tornado)
     ("python-websocket-client" ,python-websocket-client)))
  (home-page "https://github.com/facebookresearch/visdom")
  (synopsis "Visualize live, rich data for Torch and Numpy")
  (description "This package provides tools for visualizing live, rich data
for @code{Torch} and @code{Numpy}.")
  (license license:asl2.0)))

;; This package bundles 'multibuild'.
(define-public python-gensim
  (package
    (name "python-gensim")
    (version "4.0.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "gensim" version))
              (sha256
               (base32
                "1r5617m58xv6s5zha69pngzkkzvs1xg661lf0a28d5ln4xbbkl5l"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; Tests need an outdated version of Morfessor.
    (propagated-inputs
     `(("python-numpy" ,python-numpy)
       ("python-scipy" ,python-scipy)
       ("python-smart-open" ,python-smart-open)))
    (native-inputs
     `(("python-cython" ,python-cython)
       ("python-levenshtein" ,python-levenshtein)
       ("python-mock" ,python-mock)
       ;("python-morfessor" ,python-morfessor)
       ("python-nmslib" ,python-nmslib)
       ("python-pyemd" ,python-pyemd)
       ("python-pytest" ,python-pytest)
       ("python-testfixtures" ,python-testfixtures)
       ("python-visdom" ,python-visdom)))
    (home-page "https://radimrehurek.com/gensim/")
    (synopsis "Topic modelling for humans")
    (description "This package provides a Python library for topic modelling,
document indexing and similarity retrieval with large corpora.  The target
audience is the natural language processing and information retrieval
community.")
    (license license:lgpl2.1)))

(define-public python-parabam
  (package
    (name "python-parabam")
    (version "2.3.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "parabam" version))
              (sha256
               (base32
                "1iacm5vdk5xhra6xv4hzf9p9rjxc0w1mxw3ffi4g30n9wkfl6dkn"))))
    (build-system python-build-system)
    ;; Tests need 'argparse', but that's in the standard library.
    (arguments
     `(#:tests? #f))
    (propagated-inputs
     `(("python-numpy" ,python-numpy)
       ("python-pysam" ,python-pysam)))
    (home-page "")
    (synopsis "Parallel BAM File Analysis")
    (description "Parallel BAM File Analysis")
    (license license:gpl3)))

(define-public python-telomerecat
  (package
    (name "python-telomerecat")
    (version "3.4.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "telomerecat" version))
              (sha256
               (base32
                "1i59jdflc0m5kyq4b1brvvcyq6iid39pv3yw39w264i007yqgg0d"))))
    (build-system python-build-system)
    ;; Tests need 'argparse', but that's in the standard library.
    (arguments
     `(#:tests? #f))
    (propagated-inputs
     `(("python-numpy" ,python-numpy)
       ("python-pandas" ,python-pandas)
       ("python-parabam" ,python-parabam)
       ("python-pysam" ,python-pysam)))
    (home-page "https://pypi.org/project/telomerecat/")
    (synopsis "Telomere Computational Analysis Tool")
    (description "Telomerecat is a tool for estimating the average
telomere length for a paired end, whole genome sequencing sample.")
    (license license:gpl3)))

(define-public python-nanomath
  (package
    (name "python-nanomath")
    (version "0.23.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "nanomath" version))
              (sha256
               (base32
                "04b0n1qqyqq0id55zxp2dl3zj367gf59c8jilca406aqnjryv9sl"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-numpy" ,python-numpy)
       ("python-pandas" ,python-pandas)))
    (home-page "https://github.com/wdecoster/nanomath")
    (synopsis "Simple math function for other Oxford Nanopore scripts")
    (description "This package contains a few simple math function for other
Oxford Nanopore processing scripts.")
    (license license:expat)))

(define-public python-nanoget
  (package
    (name "python-nanoget")
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "nanoget" version))
              (sha256
               (base32
                "0cs5sc2i7mfbikgssfaia28bagvka2a8qpmdzbf6i27piv2c7kyz"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-biopython" ,python-biopython)
       ("python-nanomath" ,python-nanomath)
       ("python-numpy" ,python-numpy)
       ("python-pandas" ,python-pandas)
       ("python-pysam" ,python-pysam)))
    (home-page "https://github.com/wdecoster/nanoget")
    (synopsis "Functions for Oxford Nanopore sequencing data and alignments")
    (description "This package contains functions to extract information from
Oxford Nanopore sequencing data and alignments.")
    (license license:expat)))

(define-public python-nanostat
  (package
    (name "python-nanostat")
    (version "1.1.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "NanoStat" version))
              (sha256
               (base32
                "1mr81xl08qw1vyl552snnxafzmbg9rv9lskyzvzqg8dhm8baslya"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-nanoget" ,python-nanoget)
       ("python-nanomath" ,python-nanomath)))
    (home-page "https://github.com/wdecoster/nanostat")
    (synopsis "Statistics for Oxford Nanopore sequencing data and alignments")
    (description
     "This package provides procedures to calculate statistics for Oxford
Nanopore sequencing data and alignments.")
    (license license:expat)))

(define-public python-pycoqc
  (package
   (name "python-pycoqc")
   (version "2.5.0.21")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "pycoQC" version))
     (sha256
      (base32
       "02lqck381nk8bvczxjc8inr5ihhxziwwp7zdp1l43h8q2wix67k9"))))
   (build-system python-build-system)
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
       (add-after 'unpack 'downgrade-tqdm
        (lambda* (#:key inputs #:allow-other-keys)
          (substitute* "setup.py"
           (("tqdm==4.35") "tqdm>=4.19.6")
           (("pysam==0.15.3") "pysam>=0.15.0")
           (("jinja2==2.10.1") "jinja2>=2.10.1")
           (("h5py==2.9.0") "h5py>=2.8.0")
           (("plotly==4.1.0") "plotly>=3.9.0")
           (("pandas==0.25.1") "pandas>=0.24.2")
           (("scipy==1.3.1") "scipy>=1.3.1")
           (("numpy==1.17.1") "numpy>=1.15.4")))))))
   (propagated-inputs
    `(("python-h5py" ,python-h5py)
      ("python-jinja2" ,python-jinja2)
      ("python-numpy" ,python-numpy)
      ("python-pandas" ,python-pandas)
      ("python-plotly" ,python-plotly)
      ("python-scipy" ,python-scipy)
      ("python-tqdm" ,python-tqdm)
      ("python-pysam" ,python-pysam)))
   (home-page "https://github.com/a-slide/pycoQC")
   (synopsis "QC plots for Nanopore basecallers")
   (description "This package computes metrics and generates Interactive QC
plots from the sequencing summary report generated by Oxford Nanopore
technologies basecaller.")
   (license license:gpl3)))

(define-public python-nr.fs
  (package
    (name "python-nr.fs")
    (version "1.6.3")
    (source (origin
             (method url-fetch)
             (uri (pypi-uri "nr.fs" version))
             (sha256
              (base32
               "0jhjvzy5mdgvh1vx6fskpqrfjwh81k68rg25a9fgjhs19jha12kq"))))
    (build-system python-build-system)
    (propagated-inputs `(("python-six" ,python-six)))
    (home-page "https://git.niklasrosenstein.com/NiklasRosenstein/nr")
    (synopsis "Filesystem and path manipulation tools")
    (description "Filesystem and path manipulation tools.")
    (license license:expat)))

(define-public python-docspec-python
  (package
   (name "python-docspec-python")
   (version "0.2.0")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "docspec-python" version))
            (sha256
             (base32
              "0s04yn9hff5f68jbwxl2mrpcvghlir2jgqfjdr6wrl76np663cb1"))))
   (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; Needs old version of nr.pylang.utils
   (propagated-inputs
    `(("python-dataclasses" ,python-dataclasses)
      ("python-docspec" ,python-docspec)))
   (home-page "")
   (synopsis "Parser based on lib2to3 producing docspec data from Python code")
   (description "This package provides a parser based on lib2to3 producing
docspec data from Python source code.")
   (license license:expat)))

(define-public python-nr.utils.re
  (package
   (name "python-nr.utils.re")
   (version "0.2.0")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "nr.utils.re" version))
            (sha256
             (base32
              "11mscz791vyc5133xvrggaz3srf6fs35qb7zx3k0a9hwn43cd4xq"))))
   (build-system python-build-system)
   (native-inputs
    `(("python-pytest" ,python-pytest)))
   (home-page "https://git.niklasrosenstein.com/NiklasRosenstein/nr")
   (synopsis "Utility functions for applying regular expressions")
   (description "This module provides some utility functions for applying
regular expressions.")
   (license license:expat)))

(define-public python-nr.parsing.date
  (package
   (name "python-nr.parsing.date")
   (version "0.6.1")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "nr.parsing.date" version))
            (sha256
             (base32
              "1n2g6kc1zs924w10n8ya2iz3vlslkwxb1xlw0fg3yyirccm1xgqi"))))
   (build-system python-build-system)
   (arguments
    `(#:tests? #f)) ; Needs very old version of nr.utils.re.
   (home-page "https://git.niklasrosenstein.com/NiklasRosenstein/nr")
   (synopsis "Simple and fast date parsing library")
   (description "This package provides a simple and fast date parsing
library.  It uses dateutil for timezone offset support.")
   (license license:expat)))

(define-public python-nr.databind.json
  (package
   (name "python-nr.databind.json")
   (version "0.0.14")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "nr.databind.json" version))
     (sha256
      (base32
       "03pjfjjxw4mw5r0lv1zrkjrm2fmmzm88k2bbc463lw3gkykagir7"))))
   (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; Needs old version of nr.pylang.utils
   (propagated-inputs
    `(("python-nr.collections" ,python-nr.collections)
      ("python-nr.databind.core" ,python-nr.databind.core)
      ("python-nr.interface" ,python-nr.interface)
      ("python-nr.parsing.date" ,python-nr.parsing.date)
      ("python-nr.pylang.utils" ,python-nr.pylang.utils)))
   (home-page "https://git.niklasrosenstein.com/NiklasRosenstein/nr")
   (synopsis "Deserialize JSON into Python objects and reverse")
   (description "This package provides a module to deserialize JSON into
Python objects and reverse.")
   (license license:expat)))

(define-public python-nr.stream
  (package
   (name "python-nr.stream")
   (version "0.1.1")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "nr.stream" version))
            (sha256
             (base32
              "11wzmp8rpsl0vfvg6w6syfy50393zlmj13dhnbzzh1fhszdcjjaw"))))
   (build-system python-build-system)
   (native-inputs
    `(("python-pytest" ,python-pytest)))
   (propagated-inputs
    `(("python-nr.collections" ,python-nr.collections)
      ("python-nr.pylang.utils" ,python-nr.pylang.utils)
      ("python-six" ,python-six)))
   (home-page "https://git.niklasrosenstein.com/NiklasRosenstein/nr")
   (synopsis "Use iterators like Java streams")
   (description "This package provides a module to use iterators like
Java streams.")
   (license license:expat)))

(define-public python-nr.pylang.utils
  (package
   (name "python-nr.pylang.utils")
   (version "0.1.0")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "nr.pylang.utils" version))
            (sha256
             (base32
              "1gysjx9b80xp1981qix0iz4p9s82mk319rjfhq1dbl5i4jfmgygx"))))
   (build-system python-build-system)
   (native-inputs
    `(("python-pytest" ,python-pytest)))
   (propagated-inputs
    `(("python-nr.collections" ,python-nr.collections)))
   (home-page "https://git.niklasrosenstein.com/NiklasRosenstein/nr")
   (synopsis "")
   (description "")
   (license license:expat)))

(define-public python-nr.interface
  (package
   (name "python-nr.interface")
   (version "0.0.5")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "nr.interface" version))
            (sha256
             (base32
              "10yy380z8z9iz3hlky06g4nsmg5v6ch07qww55apa90k92al6q7d"))))
   (build-system python-build-system)
   (arguments
    `(#:tests? #f)) ; Needs old version of nr.pylang.utils
   (propagated-inputs
    `(("python-nr.collections" ,python-nr.collections)
      ("python-nr.metaclass" ,python-nr.metaclass)
      ("python-nr.pylang.utils" ,python-nr.pylang.utils)
      ("python-six" ,python-six)))
   (home-page "https://git.niklasrosenstein.com/NiklasRosenstein/nr")
   (synopsis "Interface definitions for Python")
   (description "This package provides a module to define interface
definitions for Python.")
   (license license:expat)))

(define-public python-nr.metaclass
  (package
   (name "python-nr.metaclass")
   (version "0.0.6")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "nr.metaclass" version))
            (sha256
             (base32
              "041ban9b3vvigjdc5wbcya8n3hy3jfas1r9mj55r9hp30gkghlix"))))
   (build-system python-build-system)
   (native-inputs
    `(("python-pytest" ,python-pytest)))
   (home-page "https://git.niklasrosenstein.com/NiklasRosenstein/nr")
   (synopsis "Metaclass utilities.")
   (description "Metaclass utilities.")
   (license license:expat)))

(define-public python-nr.collections
  (package
   (name "python-nr.collections")
   (version "0.1.1")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "nr.collections" version))
     (sha256
      (base32
       "0klspp025f2d7yzh61pcfb73i23xrx3xsqwacf74xzg3dlx5snv4"))))
   (build-system python-build-system)
   (native-inputs
    `(("python-pytest" ,python-pytest)))
   (propagated-inputs
    `(("python-nr.metaclass" ,python-nr.metaclass)
      ("python-nr.fs" ,python-nr.fs)
      ("python-six" ,python-six)))
   (home-page "https://git.niklasrosenstein.com/NiklasRosenstein/nr")
   (synopsis "Useful container datatypes for Python")
   (description "This package provides container datatypes for
Python 2 and 3.")
   (license license:expat)))

(define-public python-nr.databind.core
  (package
    (name "python-nr.databind.core")
    (version "0.0.22")
    (source (origin
             (method url-fetch)
             (uri (pypi-uri "nr.databind.core" version))
             (sha256
              (base32
               "0xsz0ws56xhg0gippnx8av50fla3av849lawl9wbi4fxv3bk5zl7"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; Needs old version of nr.pylang.utils
    (propagated-inputs
      `(("python-nr.collections" ,python-nr.collections)
        ("python-nr.interface" ,python-nr.interface)
        ("python-nr.pylang.utils" ,python-nr.pylang.utils)
        ("python-nr.stream" ,python-nr.stream)))
    (home-page
      "https://git.niklasrosenstein.com/NiklasRosenstein/nr")
    (synopsis "Bind structured data directly to typed objects")
    (description "This module provides a mechanism to bind structured data
directly to typed objects.")
    (license license:expat)))

(define-public python-docspec
  (package
   (name "python-docspec")
   (version "0.2.1")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "docspec" version))
     (sha256
      (base32
       "0afdl37yda2xsp2d1xaw2k9ki7hj57b4iir57s69b6pnrni4vwcg"))))
   (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; Needs old version of nr.pylang.utils
   (propagated-inputs
    `(("python-nr.databind.core" ,python-nr.databind.core)
      ("python-nr.databind.json" ,python-nr.databind.json)))
   (home-page "")
   (synopsis "Specification for representing API documentation")
   (description "This package provides a JSON object specification for
representing API documentation of programming languages.")
   (license license:expat)))

(define-public python-pydoc-markdown
  (package
   (name "python-pydoc-markdown")
   (version "3.13.0")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "pydoc-markdown" version))
            (sha256
             (base32
              "13yrwvq3wq2vkr9qkp998g4aa77118hgssaraf24wrjn4z8fn8qz"))))
   (build-system python-build-system)
   (arguments
    `(#:tests? #f)) ; Needs newer version of python-watchdog.
   (propagated-inputs
    `(("python-click" ,python-click)
      ("python-dataclasses" ,python-dataclasses)
      ("python-docspec" ,python-docspec)
      ("python-docspec-python" ,python-docspec-python)
      ("python-nr.collections" ,python-nr.collections)
      ("python-nr.databind.core" ,python-nr.databind.core)
      ("python-nr.databind.json" ,python-nr.databind.json)
      ("python-nr.fs" ,python-nr.fs)
      ("python-nr.interface" ,python-nr.interface)
      ("python-pyyaml" ,python-pyyaml)
      ("python-requests" ,python-requests)
      ("python-six" ,python-six)
      ("python-toml" ,python-toml)
      ("python-watchdog" ,python-watchdog)))
   (home-page "https://github.com/NiklasRosenstein/pydoc-markdown")
   (synopsis "Create Python API documentation in Markdown format.")
   (description
    "This package contains a module to create Python API documentation
using the Markdown format.")
   (license license:expat)))

(define-public python-py-make
  (package
   (name "python-py-make")
   (version "0.1.1")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "py-make" version))
            (sha256
             (base32
              "1sg848j1v65i636qr8d9p4b29ps4zpb1p7382cdyav5bglcm259j"))))
   (build-system python-build-system)
   (propagated-inputs
    `(("python-coverage" ,python-coverage)
      ("python-flake8" ,python-flake8)
      ("python-nose" ,python-nose)
      ("python-docopt" ,python-docopt)))
   (home-page "https://github.com/tqdm/pymake")
   (synopsis "Makefile execution powered by pure Python")
   (description "This package provides tools for Makefile execution powered
by pure Python.")
   (license license:mpl2.0)))
