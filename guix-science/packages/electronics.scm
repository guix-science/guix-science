;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Cayetano Santos <csantosb@inventati.org>
;;;
;;; This file is NOT part of GNU Guix, but is supposed to be used with GNU
;;; Guix and thus has the same license.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guix-science packages electronics)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages llvm)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu))

(define-public gnat
  (package
    (name "gnat")
    (version "14.2.0-1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/alire-project/GNAT-FSF-builds/"
                           "releases/download/gnat-14.2.0-1/gnat-x86_64-linux-" version
                           ".tar.gz"))
       (sha256
        (base32 "08kpd3d7si73gsm2dfp5lmrhii9k96y972sw39h1sdvhgzpkvfq6"))))
    (build-system gnu-build-system)
    (arguments
     (list
      ;; Let's not publish or obtain substitutes for that.
      #:substitutable? #f
      #:strip-binaries? #f
      ;; XXX: This would check DT_RUNPATH, but patchelf populate DT_RPATH,
      ;; not DT_RUNPATH.
      #:validate-runpath? #f
      #:phases #~(modify-phases %standard-phases
                   (replace 'unpack
                     (lambda* (#:key inputs #:allow-other-keys)
                       (let ((source (assoc-ref inputs "source")))
                         (invoke "tar" "xvzf" source)
                         (chdir "gnat-x86_64-linux-14.2.0-1"))))
                   (delete 'configure)
                   (delete 'check)
                   (replace 'build
                     (lambda* (#:key inputs #:allow-other-keys)
                       (define libc
                         (assoc-ref inputs "libc"))
                       (define gcc-lib
                         (assoc-ref inputs "gcc:lib"))
                       (define ld.so
                         (search-input-file inputs
                                            #$(glibc-dynamic-linker)))
                       (define rpath
                         (string-join (list "$ORIGIN"
                                            (string-append #$output "/lib")
                                            (string-append #$output "/lib64")
                                            (string-append libc "/lib")
                                            (string-append gcc-lib "/lib"))
                                      ":"))

                       ;; patchelf procedure
                       (define (patch-elf file)
                         (make-file-writable file)

                         (unless (string-contains file ".so")
                           (unless (string-contains file ".o")
                             (format #t "Setting RPATH on '~a'...~%" file)
                             (invoke "patchelf" "--set-rpath" rpath
                                     "--force-rpath" file)))

                         (unless (string-contains file ".so")
                           (unless (string-contains file ".o")
                             (format #t "Setting interpreter on '~a'...~%"
                                     file)
                             (invoke "patchelf" "--set-interpreter" ld.so file))))

                       ;; patch files
                       (for-each (lambda (file)
                                   (when (elf-file? file)
                                     (patch-elf file)))
                                 (find-files "."
                                             (lambda (file stat)
                                               (eq? 'regular
                                                    (stat:type stat)))))))

                   (replace 'install
                     (lambda* _
                       (let ((bin (string-append #$output "/bin"))
                             (lib (string-append #$output "/lib"))
                             (lib64 (string-append #$output "/lib64"))
                             (libexec (string-append #$output "/libexec"))
                             (x86_64-pc-linux-gnu (string-append #$output
                                                                 "/x86_64-pc-linux-gnu")))
                         (mkdir-p #$output)
                         (copy-recursively "bin" bin)
                         (copy-recursively "lib" lib)
                         (copy-recursively "lib64" lib64)
                         (copy-recursively "libexec" libexec)
                         (copy-recursively "x86_64-pc-linux-gnu"
                                           x86_64-pc-linux-gnu)))))))
    (native-inputs (list patchelf))
    (inputs `(("gcc:lib" ,gcc-14 "lib")))
    (home-page "https://github.com/alire-project/GNAT-FSF-builds")
    (synopsis "Builds of the GNAT Ada compiler from Alire Project")
    (description
     "This package gathers GNAT binaries from FSF GCC releases of the Alire
Project.")
    (license (list license:expat)))) ; MIT license
