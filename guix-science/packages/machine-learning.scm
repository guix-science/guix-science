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
  #:use-module (gnu packages check)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix-science packages python))

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
