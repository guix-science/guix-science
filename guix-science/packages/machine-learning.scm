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
