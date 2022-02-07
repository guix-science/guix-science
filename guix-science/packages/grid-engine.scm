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

(define-module (guix-science packages grid-engine)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages java)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls))

(define-public grid-engine-core
  (package
    (name "grid-engine-core")
    (version "8.1.9")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://arc.liv.ac.uk/downloads/SGE/releases/"
                    version "/sge-" version ".tar.gz"))
              (sha256
               (base32
                "0ra7m9mf09zzcf815y3zqzqkj95v9zm24nhhvzmdh2bsqgdmk59w"))
              (patches (search-patches "grid-engine-core-openssl-1.1.patch"
                                       "grid-engine-core-extern_qualifier.patch"))))
    (build-system gnu-build-system)
    (supported-systems '("x86_64-linux"))
    (inputs
     `(("bdb" ,bdb)
       ("tcsh" ,tcsh)
       ("inetutils" ,inetutils)
       ("hwloc" ,hwloc "lib")
       ("openssl" ,openssl)
       ("coreutils" ,coreutils)
       ("tcl" ,tcl)
       ("linux-pam" ,linux-pam)
       ("python" ,python-2.7)
       ("perl" ,perl)
       ("ruby" ,ruby)
       ("gawk" ,gawk)
       ("libtirpc" ,libtirpc)
       ("icedtea" ,icedtea-8)))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-various-stuff
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "source/aimk"
               (("/usr/bin/uname") "uname")
               (("uname") (string-append (assoc-ref inputs "coreutils")
                                         "/bin/uname")))
             (substitute* "source/dist/util/arch"
               (("/bin/uname") (string-append (assoc-ref inputs "coreutils")
                                              "/bin/uname"))
               (("/lib64/libc.so.6") (string-append (assoc-ref inputs "libc")
                                                    "/lib/libc.so.6"))
               (("awk") (string-append (assoc-ref inputs "gawk") "/bin/gawk"))
               (("head") (string-append (assoc-ref inputs "coreutils")
                                        "/bin/head")))
             (substitute* "source/aimk"
               (("= cc") (string-append "= gcc")))
             (substitute* "source/configure"
               (("SHELL=") (string-append "SHELL=" (assoc-ref inputs "bash")
                                          "/bin/sh #")))
             ))
         (replace 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             (chdir "source")
             (setenv "SGE_INPUT_LDFLAGS" "-ltirpc")
             (setenv "SGE_INPUT_CFLAGS"
                     (string-append
                      "-I" (assoc-ref inputs "openssl") "/include "
                      "-I" (assoc-ref inputs "libtirpc") "/include/tirpc"))
             (setenv "JAVA_HOME" (assoc-ref inputs "icedtea"))
             (system "scripts/bootstrap.sh")
             #t))
         (replace 'build
           (lambda _
             (system "./aimk -only-core -no-java -no-jni")
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             ;; The scripts/distinst would not work, so we copy the files
             ;; over manually.
             (chdir "LINUXAMD64")
             (let ((bin (string-append (assoc-ref outputs "out") "/bin"))
                   (lib (string-append (assoc-ref outputs "out") "/lib"))
                   (include (string-append (assoc-ref outputs "out")
                                           "/include")))
               (mkdir-p bin)
               (mkdir-p lib)
               (mkdir-p include)

               ;; Binaries
               (for-each (lambda (file)
                           (install-file file bin))
                         '("qacct" "qalter" "qconf" "qdel" "qevent" "qhost"
                           "qmod" "qping" "qquota" "qrdel" "qrstat" "qrsub"
                           "qsh" "qstat" "qsub" "sge_coshepherd" "sge_execd"
                           "sgepasswd" "sge_qmaster" "sge_shadowd"
                           "sge_share_mon" "sge_shepherd"))
               (system* "ln" "--symbolic"
                        (string-append bin "/qalter")
                        (string-append bin "/qhold"))
               (system* "ln" "--symbolic"
                        (string-append bin "/qalter")
                        (string-append bin "/qresub"))
               (system* "ln" "--symbolic"
                        (string-append bin "/qalter")
                        (string-append bin "/qrls"))
               (system* "ln" "--symbolic"
                        (string-append bin "/qsh")
                        (string-append bin "/qrsh"))
               (system* "ln" "--symbolic"
                        (string-append bin "/qstat")
                        (string-append bin "/qselect"))
               (system* "ln" "--symbolic"
                        (string-append bin "/qsh")
                        (string-append bin "/qlogin"))

               ;; Libraries
               (for-each (lambda (file)
                           (install-file file lib))
                         '("libdrmaa.so" "libjuti.so" "libspoolb.so"
                           "libspoolc.so" "pam_sge_authorize.so"
                           "pam_sge-qrsh-setup.so"))
               (system* "ln" "--symbolic"
                        (string-append lib "/libdrmaa.so")
                        (string-append lib "/libdrmaa.so.1"))
               (system* "ln" "--symbolic"
                        (string-append lib "/libdrmaa.so")
                        (string-append lib "/libdrmaa.so.1.0"))

               ;; Headers
               (install-file "../libs/japi/drmaa.h" include)
               (install-file "../libs/sched/sge_pqs_api.h" include)

               ;; Pkg-config file
               (mkdir-p (string-append lib "/pkgconfig"))
               (with-output-to-file (string-append lib "/pkgconfig/drmaa.pc")
                 (lambda _
                   (format #t "Name: drmaa~%Description: DRMAA interface~%Version: 8.1.9~%Requires:~%Libs: -L~a -ldrmaa~%Cflags: -I~a" lib include)))
               #t))))))
    (native-search-paths
     (list (search-path-specification
            (variable "LD_LIBRARY_PATH")
            (files '("lib")))
           (search-path-specification
            (variable "DRMAA_LIBRARY_PATH")
            (files '("lib/libdrmaa.so"))
            (file-type 'regular))))
    (home-page "https://arc.liv.ac.uk/trac/SGE")
    (synopsis "Implementation of a grid engine")
    (description "The Son of Grid Engine is a community project to continue
Sun's old gridengine project after Oracle shut down the site and stopped
contributing code.")
    (license (list license:asl2.0 license:gpl2+))))

(define-public qsub-slurm
  (let ((commit "1321c3bdaacf4b37b3bc20ed124fc88c1dfb38ef"))
    (package
     (name "qsub-slurm")
     (version "0.0.7")
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/roelj/qsub-slurm.git")
                    (commit commit)))
              (sha256
               (base32
                "0hm7c83ih1579nppc4s1s78ad3dzqjv7wzajzpqi1gr0gkn0566c"))))
     (build-system gnu-build-system)
     (arguments
      `(#:tests? #f))
     (native-inputs
      `(("autoconf" ,autoconf)
        ("automake" ,automake)
        ("pkg-config" ,pkg-config)))
     (inputs
      `(("slurm" ,slurm)
        ("guile" ,guile-3.0)))
     (home-page "https://github.com/roelj/qsub-slurm")
     (synopsis "Compatibility tool to move from SGE to SLURM.")
     (description "This package an alternative @code{qsub} command that
will submit jobs to SLURM.")
     (license license:gpl3+))))

(define-public qsub-local
  (let ((commit "3959ef2ed2e559798d8e7928579816c13404381b"))
    (package
     (name "qsub-local")
     (version "0.0.1")
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/roelj/qsub-local.git")
                    (commit commit)))
              (sha256
               (base32
                "1dxp3s311397jf6lm6jdik8fwx56kbghx5v97nb6kjz33ymycy9p"))))
     (build-system gnu-build-system)
     (arguments
      `(#:tests? #f))
     (native-inputs
      `(("autoconf" ,autoconf)
        ("automake" ,automake)
        ("pkg-config" ,pkg-config)))
     (inputs
      `(("bash" ,bash)
        ("guile" ,guile-3.0)))
     (home-page "https://github.com/roelj/qsub-local")
     (synopsis "Compatibility tool to run SGE pipelines locally.")
     (description "This package an alternative @code{qsub} command that
will directory run the script.")
     (license license:gpl3+))))
