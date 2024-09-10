;;;
;;; Copyright © 2016-2021 Roel Janssen <roel@gnu.org>
;;; Copyright © 2023, 2024 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages java)
  #:use-module (gnu packages jupyter)
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
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rpc)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages video)
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
  #:use-module (guix-science build-system bazel)
  #:use-module (guix-science packages machine-learning)
  #:use-module (ice-9 match))

(define-public python-optree
  (package
    (name "python-optree")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "optree" version))
       (sha256
        (base32 "13n98dzpyavzbj1cibkimxvki0whqx0x310p361m0dlpz608hznw"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; These all need a "helpers" module that is not included.
      '(list "--ignore=tests/test_ops.py"
             "--ignore=tests/test_typing.py"
             "--ignore=tests/test_treespec.py"
             "--ignore=tests/test_prefix_errors.py")))
    (propagated-inputs (list python-typing-extensions))
    (native-inputs
     (list cmake-minimal
           pybind11
           python-pytest
           python-pytest-cov
           python-pytest-xdist))
    (home-page "https://github.com/metaopt/optree")
    (synopsis "Optimized PyTree utilities")
    (description "This package provides optimized @code{PyTree}
utilities.")
    (license license:asl2.0)))

(define-public python-simple-parsing
  (package
    (name "python-simple-parsing")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "simple_parsing" version))
       (sha256
        (base32 "1s0s6hc6qz66x4rzfrnshf5vwdfk7d1015a1nrhyn2wgglr008ri"))))
    (build-system pyproject-build-system)
    ;; TODO: There is some sort of path issue, because most tests fail
    ;; outright with this error: ImportError: attempted relative
    ;; import with no known parent package
    (arguments (list #:tests? #false))
    (propagated-inputs
     (list python-docstring-parser python-typing-extensions))
    (native-inputs
     (list python-numpy python-pytest python-pytest-benchmark
           python-pytest-regressions python-pytest-xdist))
    (home-page "https://github.com/lebrice/SimpleParsing")
    (synopsis
     "Utility for simplifying and cleaning up argument parsing scripts")
    (description
     "This package provides a small utility for simplifying and
cleaning up argument parsing scripts.")
    (license license:expat)))

;; This package provides *independent* modules that are meant to be
;; imported selectively.  Each module has its own Bazel BUILD file,
;; but no separate pyproject.toml.  Bundling everything up as a single
;; package defeats the original purpose.  We should probably split
;; this up into as many packages as there are modules.
(define-public python-etils
  (package
    (name "python-etils")
    (version "1.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/etils/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xhnsr4n6dxsn25jiblf5qpk8jj9rgm4yb3gq4zyxffqxd1nlplg"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; The lazy imports require python-dataclass-array, but that
      ;; package needs etils at build time.
      '(list "--ignore=etils/epy/lazy_imports_utils_test.py"
             ;; This needs internet access
             "-k" "not test_public_access")))
    (inputs (list ffmpeg-5))            ;for mediapy
    (propagated-inputs
     (list jupyter
           python-absl-py
           python-chex
           python-dm-tree
           python-fsspec
           python-importlib-resources
           python-jax
           python-mediapy
           python-numpy
           python-optree
           python-packaging
           python-protobuf
           python-pylint
           python-pytorch
           python-simple-parsing
           python-tensorflow
           python-tqdm
           python-typing-extensions
           python-zipp))
    (native-inputs
     (list python-flit-core
           python-pytest
           python-pytest-subtests
           python-pytest-xdist))
    (home-page "https://github.com/google/etils/")
    (synopsis "Collection of common Python utils")
    (description "This is a collection of independent Python modules
providing utilities for various projects.")
    (license license:asl2.0)))

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
              (file-name (git-file-name name version))))
    (arguments
     (substitute-keyword-arguments (package-arguments pybind11)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'skip-failing-test
              (lambda _
                (substitute* "tests/test_exceptions.py"
                  ;; This test fails with Python 3.10; skip it.
                  (("^def test_python_alreadyset_in_destructor(.*)" _ rest)
                   (string-append
                    "def test_python_alreadyset_in_destructor"
                    rest "\n"
                    "    return\n")))))))))))

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
   (list python-numpy python-psutil pybind11-2.6.1))
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
     (list python-numpy))
    (home-page "http://github.com/wmayner/pyemd")
    (synopsis
     "Wrapper for Pele and Werman's implementation of the Earth Mover's Distance")
    (description
     "This package provides a Python wrapper for Ofir Pele and Michael Werman's
implementation of the Earth Mover's Distance.")
    (license license:expat)))

;; This package bundles 'multibuild'.
(define-public python-gensim
  (package
    (name "python-gensim")
    (version "4.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "gensim" version))
       (sha256
        (base32 "1wgf6kzm3jc3i39kcrdhw89bzqj75whi1b5a030lf7d3f0lvsplr"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; This fails for unknown reasons when trying to launch
      ;; visdom.server.
      '(list "-k" "not test_callback_update_graph"
             ;; This needs access to the internet
             "--ignore=gensim/test/test_api.py")
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'patch-build-system
           (lambda _
             (substitute* "setup.py"
               (("__builtins__.__NUMPY_SETUP__.*") ""))))
         (add-before 'check 'build-extensions
           (lambda _
             ;; Cython extensions have to be built before running the tests.
             (invoke "python" "setup.py" "build_ext" "--inplace")
             ;; Needed for some of the tests
             (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list python-pyemd
           python-numpy
           python-nmslib
           python-scipy
           python-smart-open))
    (native-inputs
     (list python-cython
           python-mock
           python-pytest
           python-pytest-cov
           python-testfixtures
           python-visdom))
    (home-page "https://radimrehurek.com/gensim/")
    (synopsis "Python framework for fast vector space modelling")
    (description "This package provides a Python library for topic modelling,
document indexing and similarity retrieval with large corpora.  The target
audience is the natural language processing and information retrieval
community.")
    (license license:lgpl2.1)))

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
     (list python-numpy python-pandas))
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
     (list python-biopython python-nanomath python-numpy python-pandas
           python-pysam))
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
     (list python-nanoget python-nanomath))
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
    (list python-h5py
          python-jinja2
          python-numpy
          python-pandas
          python-plotly
          python-scipy
          python-tqdm
          python-pysam))
   (home-page "https://github.com/a-slide/pycoQC")
   (synopsis "QC plots for Nanopore basecallers")
   (description "This package computes metrics and generates Interactive QC
plots from the sequencing summary report generated by Oxford Nanopore
technologies basecaller.")
   (license license:gpl3)))

(define-public python-nr.stream
  (deprecated-package "python-nr.stream" python-nr-stream))

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
    (list python-coverage python-flake8 python-nose python-docopt))
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
    (version "0.4.20")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/jax")
             (commit (string-append "jaxlib-v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15dmxmfjybg1289v822cmk9raagl9mcbkjk990xa0f91sx91gdjq"))))
    (build-system bazel-build-system)
    (arguments
     (list
      #:tests? #false                   ;there are none
      #:bazel-configuration
      #~(setenv "TF_SYSTEM_LIBS"
                (string-join '#$jaxlib-system-libs ","))
      #:fetch-targets
      '(list "//jaxlib/tools:build_wheel"
             "@mkl_dnn_v1//:mkl_dnn")
      #:build-targets
      '(list "//jaxlib/tools:build_wheel")
      #:run-command
      #~(list
         (string-append "--output_path=" #$output)
         (string-append "--cpu="
                        #$(match (or (%current-target-system)
                                     (%current-system))
                            ("x86_64-linux"   "x86_64")
                            ("i686-linux"     "i686")
                            ("mips64el-linux" "mips64")
                            ("aarch64-linux"  "aarch64")
                            ;; Prevent errors when querying this
                            ;; package on unsupported platforms,
                            ;; e.g. when running "guix package
                            ;; --search="
                            (_                "UNSUPPORTED"))))
      #:bazel-arguments
      #~(list "-c" "opt"
              ;; We need a more recent version of platforms, because the
              ;; included cpu package does not define cpu:wasm32.
              (string-append "--override_repository=platforms="
                             #$(this-package-native-input "bazel-platforms"))
              "--config=mkl_open_source_only"
              (string-append "--define="
                             "PROTOBUF_INCLUDE_PATH="
                             #$static-protobuf
                             "/include"))
      #:vendored-inputs-hash
      "1fa4f8qx0765zdwmqaz1jnc60nvb3j4qxqy0mxrpqj58qdclycfs"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack-vendored-inputs 'configure
            (lambda _
              ;; XXX: Our version of protobuf leads to "File already
              ;; exists in database" when loading jax in Python.
              ;; Using the static library is what Nix does, but it
              ;; doesn't help us.
              (let ((bazel-out
                     (string-append (getenv "NIX_BUILD_TOP") "/output")))
                #;
                (substitute* (string-append bazel-out "/external/xla/third_party/systemlibs/protobuf.BUILD")
                  (("-lprotobuf") "-l:libprotobuf.a")
                  (("-lprotoc") "-l:libprotoc.a"))
                ;; The version is automatically set to ".dev" if this
                ;; variable is not set.  See
                ;; https://github.com/google/jax/commit/e01f2617b85c5bdffc5ffb60b3d8d8ca9519a1f3
                (setenv "JAXLIB_RELEASE" "1")
                (setenv "BAZEL_USE_CPP_ONLY_TOOLCHAIN" "1")
                (setenv "TF_SYSTEM_LIBS"
                        (string-join '#$jaxlib-system-libs ","))
                (call-with-output-file ".jax_configure.bazelrc"
                  (lambda (port)
                    ;; build --define PROTOBUF_INCLUDE_PATH=" #$(this-package-input "protobuf") "/include
                    (display (string-append "
build --strategy=Genrule=standalone
build --repo_env PYTHON_BIN_PATH=" #$(this-package-input "python-wrapper") "/bin/python
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
                             port)))))))))
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
     `(("python-pypa-build" ,python-pypa-build)
       ("python-setuptools" ,python-setuptools)
       ("python-wheel" ,python-wheel)
       ("bazel-platforms"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/bazelbuild/platforms")
                 (commit "0.0.8")))
           (file-name (git-file-name "bazel-platforms" "0.0.8"))
           (sha256
            (base32
             "1wx2348w49vxr3z9kjfls5zsrwr0div6r3irbvdlawan87sx5yfs"))))))
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
    (version "0.4.20")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jax" version))
       (sha256
        (base32 "1b6j3svq35f06iygc8nh3k862d0nvss9l5fi7533gadim1isg5pa"))))
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

(define-public python-jmp
  (package
    (name "python-jmp")
    (version "0.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jmp" version))
       (sha256
        (base32 "0c0p7srwx19lzwvl0by016l35bmz1jfsn2pz1ykp57vsgkyv1zjx"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; This test fails. The error is "Unable to resolve runtime symbol:
      ;; `truncsfhf2', which looks more like a toolchain error.
      '(list "--ignore=jmp/_src/policy_test.py")))
    (propagated-inputs (list python-absl-py python-dataclasses python-jax
                             python-jaxlib python-numpy))
    (native-inputs (list python-pytest))
    (home-page "https://github.com/google-deepmind/jmp")
    (synopsis "JMP is a mixed precision library for JAX")
    (description
     "This library implements support for mixed precision training in JAX.
It provides two key abstractions.  These abstractions are mixed
precision policies and loss scaling.")
    (license license:asl2.0)))

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

;; Remember to update python-keras-for-tensorflow when upgrading this
;; package.  The versions must match.
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
    (build-system bazel-build-system)
    (arguments
     (list
      #:tests? #false                   ;there are none
      #:bazel-configuration
      ;; TODO: is the union build *really* necessary?
      (with-imported-modules (source-module-closure '((guix build utils)
                                                      (guix build union)
                                                      (guix build gnu-build-system)))
        #~(begin
            (use-modules (guix build union))
            (union-build (string-append (getenv "NIX_BUILD_TOP") "/site-packages")
                         (parse-path (getenv "GUIX_PYTHONPATH")))
            (setenv "PYTHON_LIB_PATH" (string-append (getenv "NIX_BUILD_TOP") "/site-packages"))
            (setenv "PYTHON_BIN_PATH"
                    (string-append
                     #$(this-package-input "python-wrapper")
                     "/bin/python"))
            (setenv "TF_PYTHON_VERSION"
                    #$(version-major+minor
                       (package-version (this-package-input "python-wrapper"))))
            (setenv "TF_SYSTEM_LIBS"
                    (string-join '#$tensorflow-system-libs ","))))
      #:fetch-targets
      '(list "//tensorflow/tools/pip_package:build_pip_package"
             "//tensorflow/tools/lib_package:libtensorflow")
      #:build-targets
      '(list "//tensorflow/tools/pip_package:build_pip_package"
             "//tensorflow/tools/lib_package:libtensorflow")
      #:bazel-arguments
      #~(list
         "--extra_toolchains=@bazel_tools//tools/python:autodetecting_toolchain_nonstrict"
         "--action_env=PYTHON_LIB_PATH"
         "--host_action_env=PYTHON_LIB_PATH"
         "--action_env=PYTHON_BIN_PATH"
         "--host_action_env=PYTHON_BIN_PATH"
         (string-append "--python_path="
                        #$(this-package-input "python-wrapper")
                        "/bin/python"))
      #:vendored-inputs-hash
      "0bqlwf9br68mrm5ambnm3dg31gnpsa12wfm2q2gqszknhmk1nyj8"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack-vendored-inputs 'configure
            (lambda _
              (define bazel-out
                (string-append (getenv "NIX_BUILD_TOP") "/output"))
              ;; XXX: Our version of protobuf leads to "File already
              ;; exists in database" when loading in Python.
              (substitute* (string-append bazel-out "/external/tf_runtime/third_party/systemlibs/protobuf.BUILD")
                (("-lprotobuf") "-l:libprotobuf.a")
                (("-lprotoc") "-l:libprotoc.a"))
              ;; Do not mess with RUNPATH
              (substitute* "tensorflow/tools/pip_package/build_pip_package.sh"
                (("patchelf ") "echo -- "))
              (setenv "BAZEL_USE_CPP_ONLY_TOOLCHAIN" "1")
              (setenv "USER" "homeless-shelter")
              (setenv "TF_SYSTEM_LIBS"
                      (string-join '#$tensorflow-system-libs ","))))
          (add-after 'build 'install
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
           flatbuffers-23.1
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
           python-lit python-pypa-build python-setuptools python-wheel))
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
                (("'typing_extensions>=3.6.6,<4.6.0'") "'typing_extensions>=3.6.6'")
                ;; Drop all of tensorboard and tensorflow_estimator
                (("'(tensorboard|tensorflow_estimator) >.*',") " None,")
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
       (append python-clang-13 python-keras-for-tensorflow)))
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
    (build-system bazel-build-system)
    (arguments
     (list
      #:modules
      '((guix-science build bazel-build-system)
        ((guix build pyproject-build-system) #:prefix pyproject:)
        (guix build utils))
      #:imported-modules
      `(,@%bazel-build-system-modules
        ,@%pyproject-build-system-modules)
      #:tests? #false                   ;there are none
      #:fetch-targets
      '(list "//tensorflow_probability")
      #:build-targets
      '(list "//tensorflow_probability/python"
             "//tensorflow_probability/python/internal/backend/jax:rewrite"
             "//tensorflow_probability/substrates"
             "//tensorflow_probability/substrates:jax"
             "//tensorflow_probability/substrates:numpy"
             ":pip_pkg")
      #:bazel-arguments '(list)
      #:vendored-inputs-hash
      "1pf6sws009bjg6gwkll9nl3v4b1j6ff5fphwdxb2pgsfwisr9mhw"
      #:phases
      #~(modify-phases (@ (guix-science build bazel-build-system) %standard-phases)
          (add-before 'build 'configure
            (lambda _
              (setenv "BAZEL_USE_CPP_ONLY_TOOLCHAIN" "1")
              (setenv "USER" "homeless-shelter")))
          (add-after 'build 'generate-python-package
            (lambda _
              (substitute* "bazel-bin/pip_pkg"
                (("\\$\\(date --utc \\+%Y%m%d\\)") "redacted"))
              (invoke "bazel-bin/pip_pkg" "dist" "--release")))
          (add-after 'generate-python-package 'install-python
            (assoc-ref pyproject:%standard-phases 'install))
          (add-after 'install-python 'create-entrypoints
            (assoc-ref pyproject:%standard-phases 'create-entrypoints))
          (add-after 'create-entrypoints 'compile-bytecode
            (assoc-ref pyproject:%standard-phases 'compile-bytecode)))))
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
    (inputs (list python-wrapper))
    (home-page "https://github.com/tensorflow/probability")
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
               ;; Fails with "[Dirichlet-<lambda>-params17]":
               ;; ValueError: diag input must be 1d or 2d
               "test_mean_var"
               ;; Sometimes the return value is not within the
               ;; expected tolerance.
               "test_zero_inflated_logits_probs_agree"
               ;; Accuracy failures.
               "test_gof"
               "test_gamma_poisson_log_prob"
               "test_log_prob_gradient")
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

(define-public python-chex
  (package
    (name "python-chex")
    ;; A newer version exists but is not compatible with our numpy.
    (version "0.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "chex" version))
       (sha256
        (base32 "0y9a2jjbjih2g4dn8q66y0b2b5kzxjrnir8x68cj0fcgnkyr80sr"))))
    (build-system pyproject-build-system)
    ;; Our version of np.testing.assert_array_equal does not support
    ;; the "strict" argument.
    (arguments
     (list
      #:test-flags
      '(list "-k" "not test_assert_trees_all_equal_strict_mode")
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'compatibility
           (lambda _
             (substitute* "chex/_src/asserts.py"
               (("strict=strict") "")))))))
    (propagated-inputs (list python-absl-py
                             python-jax
                             python-jaxlib
                             python-numpy
                             python-toolz
                             python-typing-extensions))
    (native-inputs
     (list python-cloudpickle
           python-dm-tree
           python-pytest))
    (home-page "https://github.com/deepmind/chex")
    (synopsis "Chex: Testing made fun, in JAX!")
    (description "Chex is a library of utilities for helping to write
reliable JAX code.  This includes utils to help:

@itemize
@item Instrument your code (e.g. assertions)
@item Debug (e.g. transforming @code{pmaps} in @code{vmaps} within a
  context manager).
@item Test JAX code across many @code{variants} (e.g. jitted vs
  non-jitted).
@end itemize
")
    (license license:asl2.0)))

(define-public python-optax
  (package
    (name "python-optax")
    ;; 0.1.6 needs a more recent numpy
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "optax" version))
       (sha256
        (base32 "0bhgaaxvqli3b2081zp9ycb2c2hqba0fcpigaqjxbnsidyspk8qa"))))
    (build-system pyproject-build-system)
    ;; Tests require haiku, tensorflow, and flax, but flax needs
    ;; optax.
    (arguments
     (list #:tests? #false))
    (propagated-inputs (list python-absl-py
                             python-chex
                             python-jax
                             python-jaxlib
                             python-numpy))
    (native-inputs
     (list python-dm-tree
           python-pytest))
    (home-page "https://github.com/google-deepmind/optax/")
    (synopsis "Gradient processing and optimization library for JAX")
    (description "Optax is a gradient processing and optimization
library for JAX.")
    (license license:asl2.0)))
