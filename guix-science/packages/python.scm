;;;
;;; Copyright © 2016, 2017, 2018, 2019, 2020, 2021 Roel Janssen <roel@gnu.org>
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

(define-module (guix-science packages python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages c)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rpc)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix base32)
  #:use-module (guix search-paths)
  #:use-module (guix utils)
  #:use-module (guix-science packages bazel)
  #:use-module (ice-9 match))

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

(define-public eigen-for-python-ml-dtypes
  (let ((commit "7bf2968fed5f246c0589e1111004cb420fcd7c71")
        (revision "1"))
    (package
      (inherit eigen)
      (name "eigen-for-python-ml-dtypes")
      (version (git-version "3.4.90" revision commit))
      (source (origin
                (inherit (package-source eigen))
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.com/libeigen/eigen.git")
                      (commit commit)))
                (sha256
                 (base32
                  "0yq69h7pasbzq5r83d974xi031r0z2y2x0my1rz5crky54i1j0r7"))
                (patches '())
                (file-name (git-file-name name version))))
      (arguments
       (substitute-keyword-arguments (package-arguments eigen)
         ((#:tests? flag #f) #false))))))

(define-public python-ml-dtypes
  (package
    (name "python-ml-dtypes")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ml_dtypes" version))
       (sha256
        (base32 "04f61zkizfgmf2pqlsdgskj1r1gg6l5j1nj2p8v4yk2b36cqyxv0"))
       (modules '((guix build utils)))
       (snippet
        ;; Do not use bundled eigen.
        '(delete-file-recursively "third_party/eigen"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #false                   ;there are none
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'use-eigen-package
            (lambda _
              (substitute* "setup.py"
                (("third_party/eigen")
                 (string-append
                  #$(this-package-input "eigen-for-python-ml-dtypes")
                  "/include/eigen3"))))))))
    (inputs (list eigen-for-python-ml-dtypes))
    (propagated-inputs (list python-numpy))
    (native-inputs (list pybind11 python-absl-py python-pylint
                         python-pytest python-pytest-xdist))
    (home-page "https://github.com/jax-ml/ml_dtypes")
    (synopsis "NumPy dtype extensions used in machine learning")
    (description "This package is a stand-alone implementation of several
NumPy @code{dtype} extensions used in machine learning libraries, including:

@itemize
@item @code{bfloat16}: an alternative to the standard @code{float16} format
@item @code{float8_*}: several experimental 8-bit floating point
  representations including:
  @itemize
  @item @code{float8_e4m3b11fnuz}
  @item @code{float8_e4m3fn}
  @item @code{float8_e4m3fnuz}
  @item @code{float8_e5m2}
  @item @code{float8_e5m2fnuz}
  @end itemize
@item @code{int4} and @code{uint4}: low precision integer types.
@end itemize
")
    (license license:asl2.0)))

;; TODO: turn this into a proper bazel build system.
(define* (bazel-vendored-inputs
          #:key name version source
          hash search-paths inputs
          bazel-targets (bazel-arguments '())
          extra-configuration)
  (let ((name* (string-append name "-vendored-inputs-" version ".tar.xz")))
    (computed-file
     name*
     (with-imported-modules (source-module-closure '((guix build utils)
                                                     (guix build gnu-build-system)))
       #~(begin
           (use-modules (guix build utils)
                        (ice-9 match)
                        (ice-9 string-fun))
           (define input-directories '#$(map cadr inputs))
           (define %build-directory (getcwd))
           (define %bazel-out
             (string-append %build-directory "/output"))
           (define %bazel-user-root
             (string-append %build-directory "/tmp"))
           (setvbuf (current-output-port) 'line)
           (setvbuf (current-error-port) 'line)
           (set-path-environment-variable "PATH" '("bin" "sbin")
                                          (cons* #+openjdk11:jdk
                                                 #$(canonical-package which)
                                                 input-directories))
           (for-each (match-lambda
                       ((env-var (files ...) separator type pattern)
                        (set-path-environment-variable env-var files
                                                       input-directories
                                                       #:separator separator
                                                       #:type type
                                                       #:pattern pattern)))
                     '#$search-paths)

           ;; TODO: only works for directories
           (chdir #$source)
           (setenv "SOURCE_DATE_EPOCH" "1")
           (setenv "HOME" %build-directory)
           (setenv "USER" "homeless-shelter")
           (setenv "GIT_SSL_CAINFO"
                   (string-append #+nss-certs "/etc/ssl/certs/ca-bundle.crt"))
           (setenv "SSL_CERT_FILE"
                   (string-append #+nss-certs "/etc/ssl/certs/ca-bundle.crt"))

           (mkdir-p %bazel-out)
           #$extra-configuration
           (apply invoke "bazel"
                  "--batch"
                  (string-append "--output_base=" %bazel-out)
                  (string-append "--output_user_root=" %bazel-user-root)
                  "build" "--nobuild"
                  "--curses=no"
                  "--loading_phase_threads=1"
                  "--strategy=Genrule=standalone"
                  "--verbose_failures"
                  "--subcommands"
                  "--action_env=PATH"
                  "--action_env=LIBRARY_PATH"
                  "--action_env=C_INCLUDE_PATH"
                  "--action_env=CPLUS_INCLUDE_PATH"
                  "--action_env=GUIX_LOCPATH"
                  "--host_action_env=PATH"
                  "--host_action_env=LIBRARY_PATH"
                  "--host_action_env=C_INCLUDE_PATH"
                  "--host_action_env=CPLUS_INCLUDE_PATH"
                  "--host_action_env=GUIX_LOCPATH"

                  ;; Extra arguments
                  (append #$bazel-arguments
                          (list #$@bazel-targets)))

           (with-directory-excursion %bazel-out
             (for-each delete-file-recursively
                       (append
                        #;
                        (find-files "external" ;
                        "@?(bazel_tools|embedded_jdk|local_.*)(\\.marker)?")
                        (find-files "external"
                                    "^\\.(git|svn|hg)$")))
             ;; Erase markers
             (for-each (lambda (file)
                         (truncate-file file 0))
                       (find-files "external" "@.*\\.marker"))
             ;; Remove symlink references to the build directory.  These
             ;; will be rewritten to the current build directory by
             ;; users of this archive.
             (for-each (lambda (file)
                         (let ((new-target
                                (string-replace-substring
                                 (readlink file)
                                 %build-directory "GUIX_BUILD_TOP")))
                           (delete-file file)
                           (symlink new-target file)))
                       (find-files "external"
                                   (lambda (file-name stat)
                                     (and (eq? (stat:type stat) 'symlink)
                                          (string-contains (readlink file-name)
                                                           %build-directory)))
                                   #:stat lstat))
             (invoke "du" "-s" "external")
             (invoke "tar" "cfa" #$output
                     "--mtime=@1"
                     "--owner=0"
                     "--group=0"
                     "--numeric-owner"
                     "--sort=name"
                     "external"))))
     #:options
     `(#:hash-algo sha256
       #:hash
       ,(nix-base32-string->bytevector hash)))))

(define jaxlib-system-libs
  (list
   "absl_py"
   ;;"boringssl"
   "com_github_grpc_grpc"
   ;;"com_google_protobuf"
   "curl"
   "cython"
   ;;"dill_archive"
   "double_conversion"
   "flatbuffers"
   ;;"functools32_archive"
   "gast_archive"
   "gif"
   "hwloc"
   "icu"
   "jsoncpp_git"
   "libjpeg_turbo"
   "lmdb"
   "zlib"))

(define python-jaxlib/wheel
  (package
    (name "python-jaxlib")
    (version "0.4.18")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/jax")
             (commit (string-append "jaxlib-v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pfk7z3kkair6xi92yn0pvs3zlaxajhmk6r2yq020q13mwfxcfxc"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:modules
      '((guix build gnu-build-system)
        (guix build utils)
        (srfi srfi-1)
        (ice-9 string-fun))
      #:tests? #false                   ;there are none
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            (lambda* (#:key inputs #:allow-other-keys)
              (define bazel-out
                (string-append (getenv "NIX_BUILD_TOP") "/output"))
              (mkdir-p bazel-out)
              (with-directory-excursion bazel-out
                (invoke "tar" "xf" #$(bazel-vendored-inputs
                                      #:source (package-source this-package)
                                      #:name "jaxlib"
                                      #:version version
                                      #:inputs
                                      (append (standard-packages)
                                              (package-inputs this-package)
                                              (package-propagated-inputs this-package)
                                              (package-native-inputs this-package))
                                      #:search-paths
                                      (map search-path-specification->sexp
                                           (package-transitive-native-search-paths
                                            this-package))
                                      #:extra-configuration
                                      #~(begin
                                          (setenv "TF_SYSTEM_LIBS"
                                                  (string-join '#$jaxlib-system-libs ",")))
                                      #:bazel-targets
                                      (list "//jaxlib/tools:build_wheel"
                                            "@mkl_dnn_v1//:mkl_dnn")
                                      #:bazel-arguments
                                      #~(list "-c" "opt"
                                              "--config=avx_posix"
                                              "--config=mkl_open_source_only"
                                              (string-append "--define="
                                                             "PROTOBUF_INCLUDE_PATH="
                                                             #$static-protobuf
                                                             "/include"))
                                      #:hash
                                      "0jzk6d5s27s2rbzrrg8cmghkfyl99wgx3rfakhj0nshx1kra3c49")))

              ;; Rewrite dangling links to current build directory
              (for-each (lambda (file)
                          (let ((new-target
                                 (string-replace-substring
                                  (readlink file)
                                  "GUIX_BUILD_TOP" (getenv "NIX_BUILD_TOP"))))
                            (delete-file file)
                            (symlink new-target file)))
                        (find-files bazel-out
                                    (lambda (file-name stat)
                                      (and (eq? (stat:type stat) 'symlink)
                                           (string-contains (readlink file-name)
                                                            "GUIX_BUILD_TOP")))
                                    #:stat lstat))
              (setenv "HOME" (getenv "NIX_BUILD_TOP"))

              (invoke "python" "build/build.py" "--configure_only")

              ;; Bazel aborts when a source file includes a header
              ;; that isn't declared.  It prints something like this:
              ;;
              ;;    "this rule is missing dependency declarations for
              ;;    the following files included by..."
              ;;
              ;; Since we pass through C_INCLUDE_PATH and
              ;; CPLUS_INCLUDE_PATH there are many headers that are
              ;; visible to the toolchain, but that Bazel refuses to
              ;; allow.
              ;;
              ;; The biggest problem is that the kernel headers are
              ;; never declared as dependencies anywhere, so we need
              ;; to modify the toolchain declaration to allow headers
              ;; from this location.
              ;;
              ;; There are other headers that cause trouble, though,
              ;; such as those for zlib in the
              ;; @com_google_protobuf//:protobuf target.  There must
              ;; be some other mechanism to fix this (e.g. in the
              ;; protobuf target itself), but I find it easier to just
              ;; allow all locations that appear on these INCLUDE_PATH
              ;; variables.
              (substitute* (string-append bazel-out "/external/local_config_cc/BUILD")
                (("cxx_builtin_include_directories = \\[" m)
                 (string-append m
                                (string-join
                                 (map
                                  (lambda (dir) (string-append "\"" dir "\""))
                                  (append (parse-path (getenv "C_INCLUDE_PATH") '())
                                          (parse-path (getenv "CPLUS_INCLUDE_PATH") '())))
                                 "," 'suffix))))

              ;; XXX: Our version of protobuf leads to "File already
              ;; exists in database" when loading jax in Python.
              ;; Using the static library is what Nix does, but it
              ;; doesn't help us.
              (substitute* (string-append bazel-out "/external/xla/third_party/systemlibs/protobuf.BUILD")
                (("-lprotobuf") "-l:libprotobuf.a")
                (("-lprotoc") "-l:libprotoc.a"))))
          (replace 'build
            (lambda* (#:key parallel-build? #:allow-other-keys)
              (define %build-directory (getenv "NIX_BUILD_TOP"))
              (define %bazel-out
                (string-append %build-directory "/output"))
              (define %bazel-user-root
                (string-append %build-directory "/tmp"))
              ;; The version is automatically set to ".dev" if this
              ;; variable is not set.  See
              ;; https://github.com/google/jax/commit/e01f2617b85c5bdffc5ffb60b3d8d8ca9519a1f3
              (setenv "JAXLIB_RELEASE" "1")
              (setenv "BAZEL_USE_CPP_ONLY_TOOLCHAIN" "1")

              (call-with-output-file ".jax_configure.bazelrc"
                (lambda (port)
;; build --define PROTOBUF_INCLUDE_PATH=" #$(this-package-input "protobuf") "/include
                  (display (string-append "
build --strategy=Genrule=local
build --repo_env PYTHON_BIN_PATH=" #$(this-package-input "python-wrapper") "/bin/python
build --action_env=PYENV_ROOT
build --python_path=" #$(this-package-input "python-wrapper") "/bin/python
build --distinct_host_configuration=false
build --features=-layering_check
build --experimental_strict_java_deps=off
build --strict_proto_deps=off
build --config=mkl_open_source_only
build --toolchain_resolution_debug=\".*\"
build --local_ram_resources=HOST_RAM*.5
build --local_cpu_resources=HOST_CPUS*.75
")
                           port)))
              (setenv "USER" "homeless-shelter")
              (setenv "TF_SYSTEM_LIBS"
                      (string-join '#$jaxlib-system-libs ","))
              (apply invoke "bazel"
                     "--batch"
                     (string-append "--output_base=" %bazel-out)
                     (string-append "--output_user_root=" %bazel-user-root)
                     "run"
                     "--nofetch"
                     "--verbose_explanations"
                     "--curses=no"
                     "--verbose_failures"
                     "--subcommands"
                     "--action_env=PATH"
                     "--action_env=LIBRARY_PATH"
                     "--action_env=C_INCLUDE_PATH"
                     "--action_env=CPLUS_INCLUDE_PATH"
                     "--action_env=GUIX_LOCPATH"
                     "--action_env=TF_SYSTEM_LIBS"
                     "--host_action_env=TF_SYSTEM_LIBS"
                     "--host_action_env=PATH"
                     "--host_action_env=LIBRARY_PATH"
                     "--host_action_env=C_INCLUDE_PATH"
                     "--host_action_env=CPLUS_INCLUDE_PATH"
                     "--host_action_env=GUIX_LOCPATH"
                     "-c" "opt"
                     "--jobs"
                     (if parallel-build?
                         (number->string (parallel-job-count))
                         "1")
                     (list
                      "//jaxlib/tools:build_wheel"
                      "--"
                      (string-append "--output_path=" #$output)
                      (string-append "--cpu="
                                     #$(match (or (%current-target-system)
                                                  (%current-system))
                                         ("x86_64-linux"   "x86_64")
                                         ("i686-linux"     "i686")
                                         ("mips64el-linux" "mips64")
                                         ("aarch64-linux"  "aarch64")
                                         ;; Prevent errors when querying
                                         ;; this package on unsupported
                                         ;; platforms, e.g. when running
                                         ;; "guix package --search="
                                         (_                "UNSUPPORTED")))))))
          (delete 'install))))
    (inputs
     (list curl
           double-conversion
           flatbuffers
           giflib
           grpc
           hwloc
           icu4c
           jsoncpp
           libjpeg-turbo
           openssl
           ;; XXX: With our own version of Protobuf we see this error
           ;; on "import jax" (downstream of this package):
           ;;
           ;; [libprotobuf ERROR google/protobuf/descriptor_database.cc:642] File already exists in database: xla/xla_data.proto
           ;; [libprotobuf FATAL google/protobuf/descriptor.cc:1984] CHECK failed: GeneratedDatabase()->Add(encoded_file_descriptor, size): 
           ;; terminate called after throwing an instance of 'google::protobuf::FatalException'
           ;;   what():  CHECK failed: GeneratedDatabase()->Add(encoded_file_descriptor, size):
           ;;protobuf-3.20
           ;;`(,protobuf-3.20 "static")
           pybind11
           python-absl-py
           python-numpy
           python-scipy
           python-six
           python-wrapper
           ;; Wrong version of snappy?
           ;; external/tsl/tsl/platform/default/port.cc:328:11: error:
           ;; 'RawCompressFromIOVec' is not a member of 'snappy'; did
           ;; you mean 'RawUncompressToIOVec'?
           ;; snappy
           zlib))
    (propagated-inputs
     (list python-absl-py
           python-importlib-metadata
           python-gast
           python-ml-dtypes
           python-numpy
           python-opt-einsum
           python-protobuf-for-tensorflow-2
           python-scipy))
    (native-inputs
     (list python-pypa-build python-setuptools python-wheel
           bazel-6
           which
           `(,openjdk11 "jdk")))           ;for bazel
    (home-page "https://github.com/google/jax")
    (synopsis "Differentiate, compile, and transform Numpy code.")
    (description "JAX is Autograd and XLA, brought together for
high-performance numerical computing, including large-scale machine
learning research.  With its updated version of Autograd, JAX can
automatically differentiate native Python and NumPy functions. It can
differentiate through loops, branches, recursion, and closures, and it
can take derivatives of derivatives of derivatives. It supports
reverse-mode differentiation (a.k.a. backpropagation) via grad as well
as forward-mode differentiation, and the two can be composed
arbitrarily to any order.")
    (license license:asl2.0)))

(define-public python-jaxlib
  (package
    (inherit python-jaxlib/wheel)
    (source #f)
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #false
      #:phases
      #~(modify-phases %standard-phases
          (delete 'unpack)
          (replace 'build
            (lambda* (#:key inputs #:allow-other-keys)
              (mkdir-p "dist")
              (install-file
               (car (find-files (assoc-ref inputs "python-jaxlib") "\\.whl$"))
               "dist"))))))
    (native-inputs (list python-jaxlib/wheel))))

;; Keep in sync with jaxlib above
(define-public python-jax
  (package
    (name "python-jax")
    (version "0.4.18")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jax" version))
       (sha256
        (base32 "0cl1j8y7664i0rn7ckixk7372wkjm88azya5izlh620hj0wg6v3p"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #false))   ;unclear how to run them
    (propagated-inputs
     (list python-importlib-metadata python-jaxlib
           python-ml-dtypes
           python-numpy python-opt-einsum python-scipy))
    (home-page "https://github.com/google/jax")
    (synopsis "Differentiate, compile, and transform Numpy code")
    (description "JAX is Autograd and XLA, brought together for
high-performance numerical computing, including large-scale machine
learning research.  With its updated version of Autograd, JAX can
automatically differentiate native Python and NumPy functions. It can
differentiate through loops, branches, recursion, and closures, and it
can take derivatives of derivatives of derivatives. It supports
reverse-mode differentiation (a.k.a. backpropagation) via grad as well
as forward-mode differentiation, and the two can be composed
arbitrarily to any order.")
    (license license:asl2.0)))

(define tensorflow-system-libs
  (list
   ;;"absl_py"
   ;;"astor_archive"
   ;;"astunparse_archive"
   ;;"boringssl"
   ;;"com_github_googlecloudplatform_google_cloud_cpp"
   "com_github_grpc_grpc"
   ;;"com_google_absl"
   ;;"com_google_protobuf"
   ;;"com_googlesource_code_re2"
   "curl"
   "cython"
   ;;"dill_archive"
   "double_conversion"
   "flatbuffers"
   ;;"functools32_archive"
   "gast_archive"
   "gif"
   "hwloc"
   "icu"
   "jsoncpp_git"
   "libjpeg_turbo"
   "nasm"
   "nsync"
   "opt_einsum_archive"
   ;;"org_sqlite"
   "pasta"
   "png"
   ;;"pybind11" ;Our 2.8.1 does not support "const_name" attribute
   "six_archive"
   ;;"snappy"
   ;;"tblib_archive"
   "termcolor_archive"
   "typing_extensions_archive"
   "wrapt"
   "zlib"))

(define-public static-protobuf
  (package
    (inherit protobuf)
    (name "protobuf-static")
    (outputs (list "out"))
    (arguments
     (substitute-keyword-arguments (package-arguments protobuf)
       ((#:configure-flags _ #f)
        #~(list "-DBUILD_SHARED_LIBS=OFF"
                "-Dprotobuf_USE_EXTERNAL_GTEST=ON"))
       ((#:phases phases)
        #~(modify-phases #$phases
            (delete 'move-static-libraries)))))))

;; This one is self-contained to avoid problems when using a
;; mismatched version of the protobuf library.
(define-public python-protobuf-for-tensorflow-2
  (package
    (name "python-protobuf-for-tensorflow-2")
    ;; This matches the C++ version 3.21.9.  I don't make the rules.
    (version "4.21.9")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "protobuf" version))
       (sha256
        (base32
         "16asi3sdq3mqir2irlkixyshlmbjb08gmzl4rbwpfakdv69i9wk1"))))
    (build-system python-build-system)
    (home-page "https://github.com/google/protobuf")
    (synopsis "Protocol buffers is a data interchange format")
    (description
     "Protocol buffers are a language-neutral, platform-neutral extensible
mechanism for serializing structured data.")
    (license license:bsd-3)))

(define-public tensorflow
  (package
    (name "tensorflow")
    (version "2.13.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tensorflow/tensorflow/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09mfskmpvpbq919wibnw3bnhi1y3hkx3qrzm72gdr0gsivn1yb3w"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:modules
      '((guix build gnu-build-system)
        (guix build utils)
        (srfi srfi-1)
        (ice-9 string-fun))
      #:tests? #false                   ;there are none
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            (lambda* (#:key inputs #:allow-other-keys)
              (define bazel-out
                (string-append (getenv "NIX_BUILD_TOP") "/output"))
              (mkdir-p bazel-out)
              (with-directory-excursion bazel-out
                (invoke "tar" "xf" #$(bazel-vendored-inputs
                                      #:source (package-source this-package)
                                      #:name "tensorflow"
                                      #:version version
                                      #:inputs
                                      (append (standard-packages)
                                              (package-inputs this-package)
                                              (package-propagated-inputs this-package)
                                              (package-native-inputs this-package))
                                      #:search-paths
                                      (map search-path-specification->sexp
                                           (package-transitive-native-search-paths
                                            this-package))
                                      #:extra-configuration
                                      ;; TODO: is the union build *really* necessary?
                                      (with-imported-modules (source-module-closure '((guix build utils)
                                                                                      (guix build union)
                                                                                      (guix build gnu-build-system)))
                                        #~(begin
                                            (use-modules (guix build union))
                                            (union-build (string-append (getenv "NIX_BUILD_TOP") "/site-library")
                                                         (parse-path (getenv "GUIX_PYTHONPATH")))
                                            (setenv "PYTHON_LIB_PATH" (string-append (getenv "NIX_BUILD_TOP") "/site-library"))
                                            (setenv "PYTHON_BIN_PATH"
                                                    (string-append
                                                     #$(this-package-input "python-wrapper")
                                                     "/bin/python"))
                                            (setenv "TF_PYTHON_VERSION"
                                                    #$(version-major+minor
                                                       (package-version (this-package-input "python-wrapper"))))
                                            (setenv "TF_SYSTEM_LIBS"
                                                    (string-join '#$tensorflow-system-libs ","))))
                                      #:bazel-targets
                                      (list "//tensorflow/tools/pip_package:build_pip_package"
                                            "//tensorflow/tools/lib_package:libtensorflow")
                                      #:bazel-arguments
                                      #~(list
                                         "--extra_toolchains=@bazel_tools//tools/python:autodetecting_toolchain_nonstrict"
                                         "--action_env=PYTHON_LIB_PATH"
                                         "--host_action_env=PYTHON_LIB_PATH"
                                         "--action_env=PYTHON_BIN_PATH"
                                         "--host_action_env=PYTHON_BIN_PATH"
                                         ;; "--action_env=GUIX_PYTHONPATH"
                                         ;; "--action_env=PYTHONPATH"
                                         ;; "--host_action_env=GUIX_PYTHONPATH"
                                         ;; "--host_action_env=PYTHONPATH"
                                         (string-append "--python_path="
                                                        #$(this-package-input "python-wrapper")
                                                        "/bin/python"))
                                      #:hash
                                      "1fm0qjsbks8xz84bzv3ivyc8ba52vj5rz31n5w87glvsxijzq6jf")))

              ;; Rewrite dangling links to current build directory
              (for-each (lambda (file)
                          (let ((new-target
                                 (string-replace-substring
                                  (readlink file)
                                  "GUIX_BUILD_TOP" (getenv "NIX_BUILD_TOP"))))
                            (delete-file file)
                            (symlink new-target file)))
                        (find-files bazel-out
                                    (lambda (file-name stat)
                                      (and (eq? (stat:type stat) 'symlink)
                                           (string-contains (readlink file-name)
                                                            "GUIX_BUILD_TOP")))
                                    #:stat lstat))
              (setenv "HOME" (getenv "NIX_BUILD_TOP"))

              ;; Bazel aborts when a source file includes a header
              ;; that isn't declared.  It prints something like this:
              ;;
              ;;    "this rule is missing dependency declarations for
              ;;    the following files included by..."
              ;;
              ;; Since we pass through C_INCLUDE_PATH and
              ;; CPLUS_INCLUDE_PATH there are many headers that are
              ;; visible to the toolchain, but that Bazel refuses to
              ;; allow.
              ;;
              ;; The biggest problem is that the kernel headers are
              ;; never declared as dependencies anywhere, so we need
              ;; to modify the toolchain declaration to allow headers
              ;; from this location.
              ;;
              ;; There are other headers that cause trouble, though,
              ;; such as those for zlib in the
              ;; @com_google_protobuf//:protobuf target.  There must
              ;; be some other mechanism to fix this (e.g. in the
              ;; protobuf target itself), but I find it easier to just
              ;; allow all locations that appear on these INCLUDE_PATH
              ;; variables.
              (substitute* (string-append bazel-out "/external/local_config_cc/BUILD")
                (("cxx_builtin_include_directories = \\[" m)
                 (string-append m
                                (string-join
                                 (map
                                  (lambda (dir) (string-append "\"" dir "\""))
                                  (append (parse-path (getenv "C_INCLUDE_PATH") '())
                                          (parse-path (getenv "CPLUS_INCLUDE_PATH") '())))
                                 "," 'suffix))))
              ;; XXX: Our version of protobuf leads to "File already
              ;; exists in database" when loading in Python.
              (substitute* (string-append bazel-out "/external/tf_runtime/third_party/systemlibs/protobuf.BUILD")
                (("-lprotobuf") "-l:libprotobuf.a")
                (("-lprotoc") "-l:libprotoc.a"))
              ;; Do not mess with RUNPATH
              (substitute* "tensorflow/tools/pip_package/build_pip_package.sh"
                (("patchelf ") "echo -- "))))
          (replace 'build
            (lambda* (#:key parallel-build? #:allow-other-keys)
              (define %build-directory (getenv "NIX_BUILD_TOP"))
              (define %bazel-out
                (string-append %build-directory "/output"))
              (define %bazel-user-root
                (string-append %build-directory "/tmp"))
              (setenv "BAZEL_USE_CPP_ONLY_TOOLCHAIN" "1")
              (setenv "USER" "homeless-shelter")
              (setenv "TF_SYSTEM_LIBS"
                      (string-join '#$tensorflow-system-libs ","))
              (apply invoke "bazel"
                     "--batch"
                     (string-append "--output_base=" %bazel-out)
                     (string-append "--output_user_root=" %bazel-user-root)
                     "build"
                     "--nofetch"
                     "--distinct_host_configuration=false"
                     "--curses=no"
                     "--verbose_failures"
                     "--subcommands"
                     "--strategy=Genrule=local"
                     "--toolchain_resolution_debug=\".*\""
                     "--local_ram_resources=HOST_RAM*.5"
                     "--local_cpu_resources=HOST_CPUS*.75"
                     "--action_env=PATH"
                     "--action_env=GUIX_PYTHONPATH"
                     "--action_env=PYTHONPATH"
                     "--action_env=LIBRARY_PATH"
                     "--action_env=C_INCLUDE_PATH"
                     "--action_env=CPLUS_INCLUDE_PATH"
                     "--action_env=GUIX_LOCPATH"
                     "--action_env=TF_SYSTEM_LIBS"
                     "--host_action_env=TF_SYSTEM_LIBS"
                     "--host_action_env=PATH"
                     "--host_action_env=GUIX_PYTHONPATH"
                     "--host_action_env=PYTHONPATH"
                     "--host_action_env=LIBRARY_PATH"
                     "--host_action_env=C_INCLUDE_PATH"
                     "--host_action_env=CPLUS_INCLUDE_PATH"
                     "--host_action_env=GUIX_LOCPATH"
                     "-c" "opt"
                     "--jobs"
                     (if parallel-build?
                         (number->string (parallel-job-count))
                         "1")
                     (list "//tensorflow/tools/pip_package:build_pip_package"
                           "//tensorflow/tools/lib_package:libtensorflow"))))
          (replace 'install
            (lambda _
              ;; Install library
              (mkdir-p #$output)
              (invoke "tar" "-xf"
                      "bazel-bin/tensorflow/tools/lib_package/libtensorflow.tar.gz"
                      "-C" #$output)

              ;; Write pkgconfig file
              (mkdir-p (string-append #$output "/lib/pkgconfig"))
              (call-with-output-file (string-append #$output "/lib/pkgconfig/tensorflow.pc")
                (lambda (port)
                  (format port "\
Name: TensorFlow
Version: ~a
Description: Library for computation using data flow graphs for scalable machine learning
Requires:
Libs: -L~a/lib -ltensorflow
Cflags: -I~a/include/tensorflow
" #$version #$output #$output)))

              ;; Install python bindings
              ;; Build the source code, then copy it to the "python" output.
              ;;
              ;; TODO: build_pip_package includes symlinks so we must
              ;; dereference them.
              (let ((here (string-append (getcwd) "/dist")))
                (invoke "bazel-bin/tensorflow/tools/pip_package/build_pip_package"
                        "--src" here)
                (copy-recursively here #$output:python)))))))
    (outputs '("out" "python"))
    (inputs
     (list curl
           double-conversion
           flatbuffers-next
           giflib
           grpc
           hwloc
           icu4c
           jsoncpp
           libjpeg-turbo
           libpng
           nasm
           nsync
           openssl
           static-protobuf
           pybind11
           python-absl-py
           python-cython
           python-numpy
           python-scipy
           python-six
           python-wrapper
           ;; Wrong version of snappy?
           ;; external/tsl/tsl/platform/default/port.cc:328:11: error:
           ;; 'RawCompressFromIOVec' is not a member of 'snappy'; did
           ;; you mean 'RawUncompressToIOVec'?
           ;; snappy
           zlib))
    ;; TODO: these inputs probably should not be propagated.  They are
    ;; only needed for building the Python sources.
    (propagated-inputs
     (list python-absl-py
           python-cachetools
           python-certifi
           python-charset-normalizer
           python-flatbuffers
           python-gast
           python-google-pasta
           python-grpcio
           python-h5py
           python-idna
           python-jax
           python-markdown
           python-markupsafe
           python-ml-dtypes
           python-numpy
           python-oauthlib
           python-opt-einsum
           python-packaging
           python-portpicker
           python-protobuf-for-tensorflow-2
           python-psutil
           python-pyasn1
           python-requests
           python-requests-oauthlib
           python-rsa
           python-scipy
           python-six
           python-termcolor
           python-typing-extensions
           python-urllib3
           python-werkzeug
           python-wrapt))
    (native-inputs
     (list perl
           python-lit python-pypa-build python-setuptools python-wheel
           bazel-6
           which
           `(,openjdk11 "jdk")))           ;for bazel
    (home-page "https://tensorflow.org")
    (synopsis "Machine learning framework")
    (description "TensorFlow is a flexible platform for building and
training machine learning models.  It provides a library for high
performance numerical computation and includes high level Python APIs,
including both a sequential API for beginners that allows users to
build models quickly by plugging together building blocks and a
subclassing API with an imperative style for advanced research.")
    (license license:asl2.0)))

(define-public python-tensorflow
  (package
    (inherit tensorflow)
    (name "python-tensorflow")
    (source #f)
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #false
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda _
              (mkdir-p "source")
              (copy-recursively #$tensorflow:python "source")
              (chdir "source")
              ;; XXX: the "python" output of the tensorflow package
              ;; contains broken symlinks.
              (delete-file-recursively "third_party/eigen3")
              (mkdir-p "third_party/eigen3")
              (copy-recursively #$eigen-for-python-ml-dtypes
                                "third_party/eigen3")
              (with-output-to-file "third_party/eigen3/LICENSE"
                (lambda () (display "")))))
          (add-after 'unpack 'relax-dependencies
            (lambda _
              (substitute* "setup.py"
                ;; We don't have tensorflow-io yet
                (("'tensorflow-io-gcs-filesystem.*") "None,")
                (("'platform_system.*") "")
                ;; Versions above 0.4 break tests, but that's okay
                ;; because we aren't running them.
                (("gast >= 0.2.1, <= 0.4.0") "gast >= 0.2.1")
                ;; Drop all of tensorboard, keras, and tensorflow_estimator
                (("'(keras|tensorboard|tensorflow_estimator) >.*',") " None,")
                ;; Our clang bindings have a different name.
                (("libclang") "clang")
                ;; No tensorboard, sorry.
                (("standard_or_nightly\\('tensorboard = tensorboard.main:run_main', None\\),") "")
                (("'import_pb_to_tensorboard = tensorflow.python.tools.import_pb_to_tensorboard:main',") "")
                ;; We don't have tensorflow-estimator yet.
                (("'estimator_ckpt_converter = '") "")
                (("'tensorflow_estimator.python.estimator.tools.checkpoint_converter:main',") ""))))
          ;; XXX: this is really ugly, but many shared objects cannot
          ;; find libtensorflow_framework.so.2 and libbfloat16.so.so
          (add-after 'unpack 'find-tensorflow-libraries
            (lambda _
              ;; XXX: not all .so files need this.
              (let ((libraries (find-files "." ".*\\.so$")))
                (for-each (lambda (lib)
                            (make-file-writable lib)
                            (system (format #false
                                            "patchelf --set-rpath ~a:~a:$(patchelf --print-rpath ~a) ~a"
                                            ;; for libtensorflow_framework.so.2
                                            (string-append
                                             #$(this-package-input "tensorflow")
                                             "/lib")
                                            ;; for libbfloat16.so.so
                                            (string-append
                                             #$output
                                             "/lib/python3.10/site-packages/tensorflow/tsl/python/lib/core/")
                                            lib lib)))
                          libraries))))
          (add-after 'install 'install-missing-libraries
            (lambda _
              ;; libtensorflow_cc.so.2 is not installed.  See
              ;; https://github.com/tensorflow/tensorflow/issues/60326.
              (let ((dir (string-append #$output "/lib/python3.10/site-packages/tensorflow/"))
                    (lib (string-append "libtensorflow_cc.so." #$(package-version this-package))))
                (install-file (string-append "tensorflow/" lib) dir)
                (with-directory-excursion dir
                  (symlink lib "libtensorflow_cc.so.2"))))))))
    (outputs '("out"))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs tensorflow)
       (append python-clang-13)))
    (inputs (list tensorflow))
    (native-inputs
     (list eigen-for-python-ml-dtypes
           patchelf
           `(,tensorflow "python")))))

(define-public python-tensorflow-probability
  (package
    (name "python-tensorflow-probability")
    (version "0.22.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tensorflow/probability")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "04qva7cwplh0vj9k15nl7lr1m0jfa49j2cl6z2iaz8rnsm2kfz3k"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:modules
      '((guix build pyproject-build-system)
        (guix build utils)
        (srfi srfi-1)
        (ice-9 string-fun))
      #:tests? #false                   ;there are none
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'prepare-bazel-inputs
            (lambda* (#:key inputs #:allow-other-keys)
              (define bazel-out
                (string-append (getenv "NIX_BUILD_TOP") "/output"))
              (mkdir-p bazel-out)
              (with-directory-excursion bazel-out
                (invoke "tar" "xf" #$(bazel-vendored-inputs
                                      #:source (package-source this-package)
                                      #:name "tensorflow-probability"
                                      #:version version
                                      #:inputs
                                      (append (standard-packages)
                                              (package-inputs this-package)
                                              (package-propagated-inputs this-package)
                                              (package-native-inputs this-package))
                                      #:search-paths
                                      (map search-path-specification->sexp
                                           (package-transitive-native-search-paths
                                            this-package))
                                      #:bazel-targets
                                      (list "//tensorflow_probability")
                                      #:bazel-arguments
                                      '(list)
                                      #:hash
                                      "104m99n0ah9nxa296m3ryrfyqm40ay8ck4nsggfdmci935jigvwa")))

              ;; Rewrite dangling links to current build directory
              (for-each (lambda (file)
                          (let ((new-target
                                 (string-replace-substring
                                  (readlink file)
                                  "GUIX_BUILD_TOP" (getenv "NIX_BUILD_TOP"))))
                            (delete-file file)
                            (symlink new-target file)))
                        (find-files bazel-out
                                    (lambda (file-name stat)
                                      (and (eq? (stat:type stat) 'symlink)
                                           (string-contains (readlink file-name)
                                                            "GUIX_BUILD_TOP")))
                                    #:stat lstat))
              (setenv "HOME" (getenv "NIX_BUILD_TOP"))))
          (replace 'build
            (lambda _
              (define %build-directory (getenv "NIX_BUILD_TOP"))
              (define %bazel-out
                (string-append %build-directory "/output"))
              (define %bazel-user-root
                (string-append %build-directory "/tmp"))
              (setenv "BAZEL_USE_CPP_ONLY_TOOLCHAIN" "1")
              (setenv "USER" "homeless-shelter")
              (apply invoke "bazel"
                     "--batch"
                     (string-append "--output_base=" %bazel-out)
                     (string-append "--output_user_root=" %bazel-user-root)
                     "build"
                     "--nofetch"
                     "--distinct_host_configuration=false"
                     "--curses=no"
                     "--verbose_failures"
                     "--subcommands"
                     "--strategy=Genrule=local"
                     "--toolchain_resolution_debug=\".*\""
                     "--local_ram_resources=HOST_RAM*.5"
                     "--local_cpu_resources=HOST_CPUS*.75"
                     "--action_env=PATH"
                     "--action_env=GUIX_PYTHONPATH"
                     "--action_env=PYTHONPATH"
                     "--action_env=GUIX_LOCPATH"
                     "--host_action_env=PATH"
                     "--host_action_env=GUIX_PYTHONPATH"
                     "--host_action_env=PYTHONPATH"
                     "--host_action_env=GUIX_LOCPATH"
                     (list "//tensorflow_probability/python"
                           "//tensorflow_probability/python/internal/backend/jax:rewrite"
                           "//tensorflow_probability/substrates"
                           "//tensorflow_probability/substrates:jax"
                           "//tensorflow_probability/substrates:numpy"
                           ":pip_pkg"))
              (substitute* "bazel-bin/pip_pkg"
                (("\\$\\(date --utc \\+%Y%m%d\\)") "redacted"))
              (invoke "bazel-bin/pip_pkg" "dist" "--release"))))))
    (propagated-inputs
     (list python-absl-py
           python-cloudpickle
           python-decorator
           python-dm-tree
           python-jax
           python-jaxlib
           python-gast
           python-numpy
           python-six
           python-typing-extensions
           python-tensorflow
           ;;python-tensorflow-datasets ;TODO
           ))
    (native-inputs
     (list bazel-6
           `(,openjdk11 "jdk") ;for bazel
           which))
    (home-page "https://github.com/pyro-ppl/numpyro")
    (synopsis "Probabilistic reasoning and statistical analysis in TensorFlow")
    (description "TensorFlow Probability is a library for
probabilistic reasoning and statistical analysis in TensorFlow.  As
part of the TensorFlow ecosystem, TensorFlow Probability provides
integration of probabilistic methods with deep networks,
gradient-based inference via automatic differentiation, and
scalability to large datasets and models via hardware
acceleration (e.g., GPUs) and distributed computation.")
    (license license:asl2.0)))

(define-public python-numpyro
  (package
    (name "python-numpyro")
    (version "0.13.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "numpyro" version))
       (sha256
        (base32 "1s2fjmnzl93jc42rzinsfh9fm70wkh636vnzd27cg540a4ahnvsj"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(list
        ;; These require internet access
        "--ignore=test/test_example_utils.py"
        ;; Examples are not included in this distribution
        "--ignore=test/test_examples.py"
        "-k"
        (string-join
         ;; The hpdi test produces a result that is not within the
         ;; expected tolerances.
         (list "not test_hpdi"
               ;; Five of these tests fail due to deviations in the
               ;; expected values.
               "test_kl_univariate"
               ;; Sometimes the return value is not within the
               ;; expected tolerance.
               "test_zero_inflated_logits_probs_agree")
         " and not " 'infix))))
    (propagated-inputs
     (list python-funsor
           python-jax
           python-jaxlib
           python-multipledispatch
           python-numpy
           python-tensorflow-probability
           python-tqdm))
    (native-inputs
     (list python-black
           python-flake8
           python-graphviz
           python-importlib-metadata
           python-isort
           python-matplotlib
           python-pyro-api
           python-pytest
           python-pyyaml
           python-requests
           python-scipy))
    (home-page "https://github.com/pyro-ppl/numpyro")
    (synopsis "Pyro PPL on NumPy")
    (description "NumPyro is a lightweight probabilistic programming
library that provides a NumPy backend for Pyro.  It relies on JAX for
automatic differentiation and JIT compilation to GPU / CPU.")
    (license license:asl2.0)))
