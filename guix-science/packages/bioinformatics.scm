;;;
;;; Copyright © 2016-2021 Roel Janssen <roel@gnu.org>
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

(define-module (guix-science packages bioinformatics)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages time)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages pcre)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix packages))

(define-public spades
  (package
   (name "spades")
   (version "3.11.1")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "http://cab.spbu.ru/files/release"
                  version "/SPAdes-" version ".tar.gz"))
            (sha256
             (base32 "0x5l4nkkjrkdn41ifz2baz9bp2r5blgrf77impc5nnbxpy35vf1s"))))
   (build-system cmake-build-system)
   ;; Reported under section 2 "Installation", "SPAdes requires a 64-bit
   ;; system": http://cab.spbu.ru/files/release3.10.1/manual.html
   (supported-systems '("x86_64-linux"))
   (arguments
    `(#:tests? #f ; There is no test target.
      #:phases
      (modify-phases %standard-phases
        (add-before 'configure 'move-to-source-dir
          (lambda _
            (chdir "src"))))))
   ;; TODO:  While this build works fine, SPAdes bundles samtools, bwa, and
   ;; boost.  These packages are also available in GNU Guix, so we should
   ;; unbundle them.
   (inputs
    `(("bzip2" ,bzip2)
      ("zlib" ,zlib)
      ("perl" ,perl)
      ("python-2" ,python-2)))
   (home-page "http://cab.spbu.ru/software/spades")
   (synopsis "Genome assembly toolkit")
   (description "SPAdes is an assembly toolkit containing various assembly
pipelines.")
   (license license:gpl2)))

(define-public assembly-stats
  (package
   (name "assembly-stats")
   (version "1.0.1")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/sanger-pathogens/assembly-stats/archive/v"
                  version ".tar.gz"))
            (file-name (string-append name "-" version ".tar.gz"))
            (sha256
             (base32 "0xc5ppmcs09d16f062nbb0mdb0cnfhbnkp0arlxnfi6jli6n3gh2"))))
   (build-system cmake-build-system)
   (arguments
    `(#:configure-flags (list (string-append
                               "-DINSTALL_DIR:PATH="
                               %output
                               "/bin"))))
   (home-page "https://github.com/sanger-pathogens")
   (synopsis "Tool to extract assembly statistics from FASTA and FASTQ files")
   (description "This package provides a tool to extract assembly statistics
from FASTA and FASTQ files.")
   (license license:gpl3)))

(define-public fastq-tools
  (package
   (name "fastq-tools")
   (version "0.8")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "http://homes.cs.washington.edu/~dcjones/fastq-tools/"
                  name "-" version ".tar.gz"))
            (sha256
             (base32
              "0jz1y40fs3x31bw10097a1nhm0vhbsyxmd4n7dwdsl275sc9l1nz"))))
   (build-system gnu-build-system)
   (arguments `(#:tests? #f))
   (inputs
    `(("pcre" ,pcre "bin")
      ("zlib" ,zlib)))
   (home-page "https://homes.cs.washington.edu/~dcjones/fastq-tools/")
   (synopsis "Tools to work with FASTQ files")
   (description "This packages provides a collection of small and efficient
programs for performing some common and uncommon tasks with FASTQ files.")
   (license license:gpl3)))

(define-public fast5
  (package
   (name "fast5")
   (version "0.6.5")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://github.com/mateidavid/fast5/archive/v"
                                version ".tar.gz"))
            (sha256
             (base32 "06pfg2ldra5g6d14xrxprn35y994w44g0zik2d7npddd0wncxcgq"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; There are no tests.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (replace 'build
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (setenv "HDF5_INCLUDE_DIR" (string-append (assoc-ref inputs "hdf5") "/include"))
            (setenv "HDF5_LIB_DIR" (string-append (assoc-ref inputs "hdf5") "/lib"))
            ;; TODO: Check for return value.
            (substitute* "python/Makefile"
              (("install: check_virtualenv") "install:"))
            (system* "make" "-C" "python" "install")))
        (replace 'install
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (python (assoc-ref inputs "python"))
                   (site-dir (string-append out "/lib/python3.6"
                                            "/site-packages/fast5/"))
                   (bin (string-append out "/bin"))
                   (include (string-append out "/include")))
              (mkdir-p site-dir)
              (mkdir-p bin)
              (install-file "python/fast5/fast5.pyx" site-dir)
              (install-file "python/bin/f5ls" bin)
              (install-file "python/bin/f5pack" bin)

              ;; Patch /usr/bin/env.
              (substitute* (list (string-append bin "/f5ls")
                                 (string-append bin "/f5pack"))
                (("/usr/bin/env python") (string-append python "/bin/python3")))

              ;; Wrap the environments of main programs so that
              ;; these work as expected.
              (wrap-program (string-append bin "/f5ls")
                `("PYTHONPATH" ":" prefix (,bin ,(getenv "PYTHONPATH")
                                                ,site-dir)))
              (wrap-program (string-append bin "/f5pack")
                `("PYTHONPATH" ":" prefix (,bin ,(getenv "PYTHONPATH")
                                                ,site-dir)))

              (for-each (lambda (file)
                          (install-file file include))
                        (find-files "src")))
            #t)))))
   (native-inputs
    `(("which" ,which)))
   (inputs
    `(("python" ,python-3)
      ("python-setuptools" ,python-setuptools)
      ("python-cython" ,python-cython)
      ("hdf5" ,hdf5)
      ("gcc" ,gcc-9)))
   (propagated-inputs
    `(("python-dateutil" ,python-dateutil)))
   (home-page "https://github.com/mateidavid/fast5")
   (synopsis "Library for accessing Oxford Nanopore sequencing data")
   (description "This package provides a lightweight C++ library for accessing
Oxford Nanopore Technologies sequencing data.")
   (license license:expat)))


