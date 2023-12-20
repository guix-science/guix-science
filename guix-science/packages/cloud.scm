;;;
;;; Copyright © 2020-2021 Roel Janssen <roel@gnu.org>
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

(define-module (guix-science packages cloud)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix packages))

(define-public google-cloud-sdk
  (package
    (name "google-cloud-sdk")
    (version "337.0.0")
    (source (origin
             (method url-fetch)
             ;; A starting point for a proper package is here:
             ;; https://storage.googleapis.com/cloud-sdk-release/for_packagers
             ;; /linux/google-cloud-sdk_337.0.0.orig.tar.gz
             (uri (string-append
                   "https://dl.google.com/dl/cloudsdk/channels/rapid/downloads/"
                   "google-cloud-sdk-" version "-linux-x86_64.tar.gz"))
             (sha256
              (base32 "179hf34ld2qb7ll7qzflnci7bi6qyzsjszvryv8ml08vpmj510q4"))))
    ;; We use the GNU build system mainly for its patch-shebang phases.
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; This is just copying a binary, so no tests to perform.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; No configuration, just copying.
         (delete 'build)     ; No building, just copying.
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out      (assoc-ref outputs "out"))
                    (bin      (string-append out "/bin"))
                    (lib      (string-append out "/lib"))
                    (platform (string-append out "/platform"))
                    (share    (string-append out "/share/google-cloud-sdk")))
               (for-each mkdir-p (list out share))
               (copy-recursively "bin" bin)
               (copy-recursively "lib" lib)
               (copy-recursively "platform" platform)))))))
    (propagated-inputs
     (list python coreutils))
    (home-page "https://cloud.google.com/sdk")
    (synopsis "Google Cloud SDK")
    (description "This package provides the Google Cloud SDK which includes the
command-line programs gsutil and gcloud among others.")
    (license license:asl2.0)))

(define-public python-google-auth
  (package
    (name "python-google-auth")
    (version "1.30.1")
    (source (origin
              (method url-fetch)
	            (uri (pypi-uri "google-auth" version))
	            (sha256
	             (base32
                "0a1jbv12izisjycvq7rzahpk3yn1j7hk9h8wqzmzh4l0wnqq2k84"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (home-page "https://github.com/googleapis/google-auth-library-python")
    (synopsis "Google Authentication Library")
    (description "This library simplifies using Google’s various
server-to-server authentication mechanisms to access Google APIs.")
    (license license:asl2.0)))

(define-public python-google-api-core
  (package
    (name "python-google-api-core")
    (version "1.28.0")
    (source (origin
              (method url-fetch)
	            (uri (pypi-uri "google-api-core" version))
	            (sha256
	             (base32
                "0mxrz5fscsp23jd1nidcl74i89n16761vvj53zfi53kjpl1nhr02"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (home-page "https://github.com/googleapis/google-cloud-python")
    (synopsis "Google API client core library")
    (description "This library is not meant to stand-alone. Instead it defines
common helpers used by all Google API clients. For more information, see the
documentation.")
    (license license:asl2.0)))

(define-public python-google-cloud-core
  (package
    (name "python-google-cloud-core")
    (version "1.6.0")
    (source (origin
              (method url-fetch)
	            (uri (pypi-uri "google-cloud-core" version))
	            (sha256
	             (base32
                "00mvgh4vm4z5mjnlbiigmp674dwsrrsxxi7ghby7jlsl4y2v3ay6"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (home-page "https://github.com/googleapis/google-cloud-python")
    (synopsis "Google Cloud API client core library")
    (description "This library is not meant to stand-alone. Instead it defines
common helpers (e.g. base Client classes) used by all of the google-cloud-*
packages.")
    (license license:asl2.0)))


(define-public python-google-resumable-media
  (package
    (name "python-google-resumable-media")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
	            (uri (pypi-uri "google-resumable-media" version))
	            (sha256
	             (base32
                "01d34c66ymriav93faf2vf89mmbcifz693vcp38sm3yidl76a2h3"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-cachetools))
    (arguments
     `(#:tests? #f))
    (home-page "https://github.com/googleapis/google-resumable-media-python")
    (synopsis "Utilities for Google Media Downloads and Resumable Uploads")
    (description "This package provides utilities for Google media downloads
and resumable uploads. See the docs for examples and usage.")
    (license license:expat)))
