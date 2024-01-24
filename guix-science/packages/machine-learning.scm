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

(define-module (guix-science packages machine-learning)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages image)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages jupyter)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages machine-learning)
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
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages time)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix-science build-system bazel)
  #:use-module (guix-science packages bazel)
  #:use-module (guix-science packages python))

(define-public python-flax
  (package
    (name "python-flax")
    ;; Any version after this depends on a feature
    ;; (restore_with_serialized_types) that is not included in the
    ;; latest release of python-orbax-checkpoint.
    (version "0.6.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/flax")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f1r9r5w6kh3ncp07i5x9w1yr004zhxqx2kpyzzmzm7dhq4ryff7"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(list "--pyargs" "tests"
             ;; We don't have tensorboard
             "--ignore=tests/tensorboard_test.py"
             ;; These tests try to use a fixed number of CPUs that may
             ;; exceed the number of CPUs available at build time.
             "--ignore=tests/jax_utils_test.py")
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'ignore-deprecations
           (lambda _
             (substitute* "pyproject.toml"
               (("\"error\",") "")))))))
    (propagated-inputs
     (list python-einops
           python-jax
           python-optax
           python-orbax-checkpoint
           python-msgpack
           python-numpy
           python-pyyaml
           python-rich
           python-tensorstore
           python-typing-extensions))
    (native-inputs
     (list opencv
           python-nbstripout
           python-ml-collections
           python-mypy
           python-pytorch
           python-pytest
           python-pytest-cov
           python-pytest-xdist
           python-setuptools-scm
           python-tensorflow))
    (home-page "https://github.com/google/flax")
    (synopsis "Neural network library for JAX designed for flexibility")
    (description "Flax is a neural network library for JAX that is
designed for flexibility.")
    (license license:asl2.0)))

;; Keep in sync with tensorflow!
(define-public python-keras-for-tensorflow
  (package
    (name "python-keras")
    (version "2.13.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "keras" version))
       (sha256
        (base32 "0s6ciib94x5qinj4pdfr4774yx5jxv055d6xclds25d08712rwax"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #false                   ;needs tensorflow
      #:phases
      ;; We need this for tensorflow, so we can't have tensorflow
      ;; here, and this causes the sanity check to fail.  That fine,
      ;; because this is not sane.
      '(modify-phases %standard-phases
         (delete 'sanity-check))))
    (propagated-inputs (list python-absl-py
                             python-dm-tree
                             python-h5py
                             python-namex
                             python-numpy
                             python-rich))
    (home-page "https://github.com/keras-team/keras")
    (synopsis "Deep learning API")
    (description "Keras is a deep learning API written in Python,
running on top of the machine learning platform TensorFlow.  It was
developed with a focus on enabling fast experimentation and providing
a delightful developer experience.")
    (license license:asl2.0)))

(define-public python-orbax-checkpoint
  (package
    (name "python-orbax-checkpoint")
    (version "0.4.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/orbax")
             (commit "e68a1cd8c997caccc87fc5c1134847294be45ead")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02njj5jkcg5j58krj6z2y6sfi49zd9ic8r7v34fnbgkr648ay87q"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Tests require flax, but flax needs orbax.  Luckily there's a
      ;; goal to remove the dependency on flax as evidenced by this
      ;; comment in utils_test.py:
      ;;
      ;;    # TODO(b/275613424): Eliminate flax dependency in Orbax
      ;;    test suite.
      ;;
      ;; One can only hope.
      #:tests? #false
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _ (chdir "checkpoint"))))))
    (propagated-inputs
     (list python-absl-py
           python-cached-property
           python-etils
           python-importlib-resources
           python-jax
           python-jaxlib
           python-msgpack
           python-nest-asyncio
           python-numpy
           python-pyyaml
           python-tensorstore
           python-typing-extensions))
    (native-inputs
     (list python-flit-core
           python-pytest
           python-pytest-xdist))
    (home-page "https://github.com/google/orbax")
    (synopsis "Utility libraries for JAX users")
    (description "Orbax is a namespace providing common utility
libraries for JAX users.  Orbax also includes a serialization library
for JAX users, enabling the exporting of JAX models to the TensorFlow
SavedModel format.")
    (license license:asl2.0)))

(define tensorstore-python-packages
  (list
   "absl_py"
   ;;"alabaster"
   ;;"annotated_types"
   "appdirs"
   ;;"appnope"
   "asttokens"
   "attrs"
   "aws_sam_translator"
   "aws_xray_sdk"
   "babel"
   "blinker"
   "boto3"
   "botocore"
   "certifi"
   "cffi"
   "cfn_lint"
   "charset_normalizer"
   "click"
   "cloudpickle"
   "colorama"
   ;;"crc32c"
   "cryptography"
   "decorator"
   "docker"
   "docutils"
   "ecdsa"
   "exceptiongroup"
   "executing"
   "flask"
   "flask_cors"
   "googleapis_common_protos"
   "graphql_core"
   "grpcio"
   "idna"
   "imagesize"
   "importlib_metadata"
   "iniconfig"
   "ipython"
   "itsdangerous"
   "jedi"
   "jinja2"
   "jmespath"
   ;;"jschema_to_python"
   "jsondiff"
   "jsonpatch"
   "jsonpickle"
   "jsonpointer"
   "jsonschema"
   ;;"jsonschema_path"
   ;;"jsonschema_specifications"
   "junit_xml"
   "lazy_object_proxy"
   "markupsafe"
   "matplotlib_inline"
   "ml_dtypes"
   "moto"
   "mpmath"
   "networkx"
   "numpy"
   "openapi_schema_validator"
   "openapi_spec_validator"
   "packaging"
   "parso"
   ;;"pathable"
   "pbr"
   "pexpect"
   "platformdirs"
   "pluggy"
   "prompt_toolkit"
   "protobuf"
   "ptyprocess"
   "pure_eval"
   ;;"py_partiql_parser"
   "pyasn1"
   "pycparser"
   "pydantic"
   ;;"pydantic_core"
   ;;"pydantic_extra_types"
   "pygments"
   "pyparsing"
   "pytest"
   "pytest_asyncio"
   "python_dateutil"
   "python_jose"
   ;;"pywin32"
   "pyyaml"
   ;;"referencing"
   "regex"
   "requests"
   "requests_toolbelt"
   "responses"
   "rfc3339_validator"
   "rpds_py"
   "rsa"
   "s3transfer"
   "sarif_om"
   ;;"scalpl"
   "setuptools"
   "six"
   "snowballstemmer"
   "sphinx"
   ;;"sphinx_immaterial"
   "sphinxcontrib_applehelp"
   "sphinxcontrib_devhelp"
   "sphinxcontrib_htmlhelp"
   "sphinxcontrib_jsmath"
   "sphinxcontrib_qthelp"
   "sphinxcontrib_serializinghtml"
   "sshpubkeys"
   "stack_data"
   "sympy"
   "tomli"
   "traitlets"
   "typing_extensions"
   "urllib3"
   "wcwidth"
   "websocket_client"
   "werkzeug"
   "wrapt"
   "xmltodict"
   "yapf"
   "zipp"))

(define-public python-tensorstore
  (package
    (name "python-tensorstore")
    (version "0.1.52")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/google/tensorstore")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1hapkxnxcmn90xnk9ldb6nkszbnmb5zyw8x4m10wd605zxapmlhd"))))
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
      #:bazel bazel-6.4
      #:fetch-targets
      '(list "//python/tensorstore:_tensorstore__shared_objects")
      #:build-targets
      '(list "//python/tensorstore:_tensorstore__shared_objects")
      #:bazel-configuration
      ;; You can get the list of possible values by searching the
      ;; tensorstore checkout for system_build_file.  Any match is a
      ;; possible replacement.
      #~(begin
          (setenv "TENSORSTORE_SYSTEM_PYTHON_LIBS"
                  (string-join '#$tensorstore-python-packages
                               ","))
          (setenv "TENSORSTORE_SYSTEM_LIBS"
                  (string-join (list
                                ;; We don't seem to have a
                                ;; conventional library with headers,
                                ;; even though we use rust-blake3
                                ;; successfully in python-blake3.
                                ;;"blake3"
                                ;; when building with our variant we
                                ;; get this error: reference to
                                ;; ‘basic_json’ is ambiguous
                                ;;"com_github_nlohmann_json"
                                "com_github_pybind_pybind11"
                                ;;"com_google_boringssl"
                                "com_google_brotli"
                                "com_google_snappy"
                                "jpeg"
                                "libtiff"
                                "libwebp"
                                "nasm"
                                "net_zlib"
                                "net_zstd"
                                "org_aomedia_avif"
                                "org_blosc_cblosc"
                                "org_lz4"
                                "org_nghttp2"
                                "org_sourceware_bzip2"
                                "org_tukaani_xz"
                                "png"
                                "se_curl")
                               ",")))
      #:bazel-arguments
      #~(list
         "-c" "opt"
         ;; We need a more recent version of platforms, because the
         ;; included cpu package does not define cpu:wasm32.
         (string-append "--override_repository=platforms="
                        #$(this-package-native-input "bazel-platforms"))
         "--extra_toolchains=@bazel_tools//tools/python:autodetecting_toolchain_nonstrict"
         "--action_env=GUIX_PYTHONPATH"
         "--host_action_env=GUIX_PYTHONPATH"
         "--action_env=TENSORSTORE_SYSTEM_PYTHON_LIBS"
         "--host_action_env=TENSORSTORE_SYSTEM_PYTHON_LIBS"
         "--action_env=TENSORSTORE_SYSTEM_LIBS"
         "--host_action_env=TENSORSTORE_SYSTEM_LIBS"
         "--action_env=PYTHON_LIB_PATH"
         "--host_action_env=PYTHON_LIB_PATH"
         "--action_env=PYTHON_BIN_PATH"
         "--host_action_env=PYTHON_BIN_PATH"
         (string-append "--python_path="
                        #$(this-package-input "python-wrapper")
                        "/bin/python"))
      #:vendored-inputs-hash
      "02z84x7az26wp45k3mrk2cxgx0j3a2k7hfj08zv3mhkysck6jxfr"
      #:phases
      #~(modify-phases (@ (guix-science build bazel-build-system) %standard-phases)
          (add-after 'unpack 'patch-python-build-system
            (lambda _
              (substitute* "pyproject.toml"
                (("oldest-supported-numpy") "numpy"))
              ;; This rule expects that
              ;; _tensorstore.cpython-310-x86_64-linux-gnu.so exists,
              ;; but we've only built _tensorstore.so.
              (substitute* "setup.py"
                (("os.path.basename\\(ext_full_path\\)")
                 "'_tensorstore.so'")
                (("'fallback_version': '0.0.0'")
                 (string-append "'fallback_version': '" #$version "'")))))
          (add-after 'build 'prepare-python
            (lambda _
              (setenv "TENSORSTORE_PREBUILT_DIR"
                      (string-append (getcwd)
                                     "/bazel-bin/python/"))))
          (add-after 'prepare-python 'build-python
            (assoc-ref pyproject:%standard-phases 'build))
          (add-after 'build-python 'install-python
            (assoc-ref pyproject:%standard-phases 'install))
          (add-after 'install-python 'create-entrypoints
            (assoc-ref pyproject:%standard-phases 'create-entrypoints))
          (add-after 'create-entrypoints 'compile-bytecode
            (assoc-ref pyproject:%standard-phases 'compile-bytecode)))))
    (propagated-inputs
     (list python-absl-py
           python-appdirs
           python-asttokens
           python-attrs
           python-aws-sam-translator
           python-aws-xray-sdk
           python-babel
           python-blinker
           python-boto3
           python-botocore
           python-certifi
           python-cffi
           python-cfn-lint
           python-charset-normalizer
           python-click
           python-cloudpickle
           python-colorama
           python-cryptography
           python-dateutil
           python-decorator
           python-docker
           python-docutils
           python-ecdsa
           python-exceptiongroup
           python-executing
           python-flask
           python-flask-cors
           python-googleapis-common-protos
           python-graphql-core
           python-grpcio
           python-idna
           python-imagesize
           python-importlib-metadata
           python-iniconfig
           python-ipython
           python-itsdangerous
           python-jedi
           python-jinja2
           python-jmespath
           python-jose
           python-jsondiff
           python-jsonpatch
           python-jsonpickle
           python-jsonpointer
           python-jsonschema
           python-junit-xml
           python-lazy-object-proxy
           python-markupsafe
           python-matplotlib-inline
           python-ml-dtypes
           python-moto
           python-mpmath
           python-networkx
           python-numpy
           python-openapi-schema-validator
           python-openapi-spec-validator
           python-packaging
           python-parso
           python-pbr
           python-pexpect
           python-platformdirs
           python-pluggy
           python-prompt-toolkit
           python-protobuf
           python-ptyprocess
           python-pure-eval
           python-pyasn1
           python-pycparser
           ;;python-pydantic ;we don't have *core and *extra_types
           python-pygments
           python-pyparsing
           python-pytest
           python-pytest-asyncio
           python-pyyaml
           python-regex
           python-requests
           python-requests-toolbelt
           python-responses
           python-rfc3339-validator
           python-rpds-py
           python-rsa
           python-s3transfer
           python-sarif-om
           python-setuptools
           python-six
           python-snowballstemmer
           python-sphinx
           python-sphinxcontrib-applehelp
           python-sphinxcontrib-devhelp
           python-sphinxcontrib-htmlhelp
           python-sphinxcontrib-jsmath
           python-sphinxcontrib-qthelp
           python-sphinxcontrib-serializinghtml
           python-sshpubkeys
           python-stack-data
           python-sympy
           python-tomli
           python-traitlets
           python-typing-extensions
           python-urllib3
           python-wcwidth
           python-websocket-client
           python-werkzeug
           python-wrapt
           python-xmltodict
           python-yapf
           python-zipp))
    (inputs
     (list
      brotli
      c-blosc
      curl
      libavif
      libjpeg-turbo
      libpng
      libtiff
      libwebp
      lz4
      nasm
      nghttp2
      ;;nlohmann-json ;our version seems to be too old
      python-wrapper
      snappy
      xz
      `(,zstd "lib")))
    (native-inputs
     `(("pybind11" ,pybind11-2.10)
       ("python-pytest" ,python-pytest)
       ("python-setuptools" ,python-setuptools)
       ("python-setuptools-scm" ,python-setuptools-scm)
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
    (home-page "https://github.com/google/tensorstore")
    (synopsis "Library for reading and writing large multi-dimensional arrays")
    (description "TensorStore is a C++ and Python software library
designed for storage and manipulation of large multi-dimensional
arrays that:

@itemize
@item Provides advanced, fully composable indexing operations and
  virtual views.
@item Provides a uniform API for reading and writing multiple array
  formats, including zarr and N5.
@item Natively supports multiple storage systems, such as local and
  network filesystems, Google Cloud Storage, Amazon S3-compatible object
  stores, HTTP servers, and in-memory storage.
@item Offers an asynchronous API to enable high-throughput access even
  to high-latency remote storage.
@item Supports read caching and transactions, with strong atomicity,
  isolation, consistency, and durability (ACID) guarantees.
@item Supports safe, efficient access from multiple processes and
  machines via optimistic concurrency.
@end itemize
")
    (license license:asl2.0)))

(define-public python-tslearn
  (package
    (name "python-tslearn")
    (version "0.6.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tslearn-team/tslearn")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1f2bzl7vq0kqmcrw47nrhadmwj7ww7av9jc8wyqy7frl3x81wg18"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(list "-k"
             (string-append
              ;; XXX It's unclear why exactly these tests are failing.
              "not test_all_estimators[LearningShapelets-LearningShapelets]"
              " and not test_shapelets"))))
    (propagated-inputs (list python-cesium
                             python-h5py
                             python-joblib
                             python-numba
                             python-numpy
                             python-pandas
                             python-scipy
                             python-scikit-learn
                             python-tensorflow
                             python-wheel))
    (native-inputs (list python-pytest))
    (home-page "https://github.com/tslearn-team/tslearn")
    (synopsis "Machine learning toolkit for time series data")
    (description "This is a Python library for time series data mining.
It provides tools for time series classification, clustering
and forecasting.")
    (license license:bsd-2)))
