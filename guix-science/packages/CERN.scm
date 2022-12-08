;;;
;;; Copyright © 2022 Emmanuel Medernach <Emmanuel.Medernach@iphc.cnrs.fr>
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


(define-module (guix-science packages CERN)
  #:use-module  ((guix licenses) #:prefix license:)
  #:use-module  (guix build-system trivial)
  #:use-module  (gnu packages algebra)	 ;; FFTW
  #:use-module  (gnu packages astronomy) ;; cfitsio
  #:use-module  (gnu packages backup)
  #:use-module  (gnu packages base) ;; gnu-make
  #:use-module  (gnu packages bash)
  #:use-module  (gnu packages check) 
  #:use-module  (gnu packages shells)
  #:use-module  (gnu packages base)
  #:use-module  (gnu packages bootstrap)
  #:use-module  (gnu packages commencement) ;; gcc-toolchain
  #:use-module  (gnu packages compression)  ;; zlib lz4
  #:use-module  (gnu packages digest)
  #:use-module  (gnu packages gcc)
  #:use-module  (gnu packages glib)
  #:use-module  (gnu packages fontutils) ;; freetype
  #:use-module  (gnu packages serialization)
  #:use-module  (gnu packages documentation)
  #:use-module  (gnu packages autotools)
  #:use-module  (gnu packages boost)
  #:use-module  (gnu packages less)
  #:use-module  (gnu packages image) ;; libjpeg
  #:use-module  (gnu packages linux)
  #:use-module  (gnu packages llvm)  ;; llvm clang
  #:use-module  (gnu packages maths) ;; openblas gsl
  #:use-module  (gnu packages pcre)
  #:use-module  (gnu packages perl)
  #:use-module  (gnu packages pkg-config)
  #:use-module  (gnu packages python)
  #:use-module  (gnu packages python-xyz) ;; numpy
  #:use-module  (gnu packages tbb)
  #:use-module  (gnu packages tls) ;; openssl
  #:use-module  (gnu packages version-control) ;; git
  #:use-module  (gnu packages xml)
  #:use-module  (gnu packages xorg) ;; libx11
  #:use-module  (gnu packages gl)
  #:use-module  (gnu packages adns)  ;; c-ares
  #:use-module  (gnu packages web)   ;; http-parser
  #:use-module  (gnu packages icu4c) 
  #:use-module  (gnu packages libevent) ;; libuv
  #:use-module  (gnu packages)
  #:use-module  (guix build-system cmake)
  #:use-module  (guix build-system gnu)
  #:use-module  (guix download)
  #:use-module  (guix packages)
  #:use-module  (guix utils)
  
  #:use-module  (ice-9 match)

  ;; TODO:
  ;; Verifier chaque paquet apres un guix pull
  
  #:export (;; Dependencies
	    llhttp-6.0.10
	    llhttp-8.1.0
	    c-ares-1.18.1
	    
	    ;; libcern
	    CLHEP-2.3.4         ;; Ok
	    nodejs-16.13.1      ;; Ok
	    OpenScientist-batch ;; TODO: deprecated but still needed
	    
	    ;; Deja fournie par Guix
	    ;; cairo
	    ;; clang
	    ;; cmake
	    ;; coin3d
	    ;; openmpi
	    
	    ;; Others
	    dcap-2.47.12  ;; Ok
	    davix-0.6.4	  ;; Failed
	    llvm-5	  ;; Failed
	    libAfterImage ;; Failed
	    vdt		  ;; Ok
	    Unuran-1.8.1  ;; Ok
	    ROOT-6.18.04  ;; Error
            ROOT-6.20.02  ;; Error

	    ))

(define-public llhttp-6.0.10
  (package
   (name "llhttp-6.0.10")
   (version "6.0.10")
   (source (origin
	    (method url-fetch)
            (uri "https://github.com/nodejs/llhttp/archive/refs/tags/release/v6.0.10.tar.gz")
	    (sha256
             (base32
	      "001r3p2jvsz7jvjxmzbdlly0ijngw4y3rggjzmwyv99574fw3c05"))))
   (build-system cmake-build-system)
   (inputs
    `(("bash" ,bash)
      ("gcc-toolchain" ,gcc-toolchain)))
   (arguments `(#:tests? #f))
   (home-page "https://llhttp.org/")
   (synopsis "llhttp-6.0.10")
   (description "Port of http_parser to llparse.")
   (license license:expat)))

(define-public llhttp-8.1.0
  (package
   (name "llhttp-8.1.0")
   (version "8.1.0")
   (source (origin
	    (method url-fetch)
            (uri "https://github.com/nodejs/llhttp/archive/refs/tags/release/v8.1.0.tar.gz")
	    (sha256
             (base32
	      "13k3hrz7ignnpby6a02jf257nldif3xxdi9b7g7l5qp8acsd584x"))))
   (build-system cmake-build-system)
   (inputs
    `(("bash" ,bash)
      ("gcc-toolchain" ,gcc-toolchain)))
   (arguments `(#:tests? #f))
   (home-page "https://llhttp.org/")
   (synopsis "llhttp-8.1.0")
   (description "Port of http_parser to llparse.")
   (license license:expat)))

(define-public c-ares-1.18.1
  (package
   (name "c-ares-1.18.1")
   (version "1.18.1")
   (source (origin
            (method url-fetch)
            (uri "https://c-ares.org/download/c-ares-1.18.1.tar.gz")
            (sha256
             (base32
	      "1kxviskwsaa7dcgscvssxa8ps88pdq7kq4z93gxvz7sam2l54z8s"))))
   (build-system gnu-build-system)
   (arguments
    '( ;; FIXME: Some tests require network access
      #:tests? #f
      #:phases
      (modify-phases %standard-phases
		     (add-before 'check 'filter-live-tests
				 (lambda _
				   ;; Filter tests that require internet access.
				   (setenv "GTEST_FILTER" "-*.Live*:*.FamilyV4*"))))))
   (native-inputs
    (list pkg-config))
   (home-page "https://c-ares.org/")
   (synopsis "C library for asynchronous DNS requests")
   (description
    "C-ares is a C library that performs DNS requests and name resolution
asynchronously.  It is intended for applications which need to perform DNS
queries without blocking, or need to perform multiple DNS queries in parallel.
The primary examples of such applications are servers which communicate with
multiple clients and programs with graphical user interfaces.")
   (license (license:x11-style "https://c-ares.org/license.html"))))

(define-public nodejs-16.13.1
  (package
   (name "nodejs-16.13.1")
   (version "16.13.1")
   (source
    (origin
     (method url-fetch)
     (uri
      (string-append
       "https://nodejs.org/dist/v" version
       "/node-v" version ".tar.xz"))
     (sha256
      (base32
       "1bb3rjb2xxwn6f4grjsa7m1pycp0ad7y6vz7v2d7kbsysx7h08sc"))
     (modules '((guix build utils)))
     (snippet
      '(begin
         ;; Patch for compatibility with ICU 68 and newer, which
         ;; removed the public TRUE and FALSE macros.
         (substitute* '("deps/v8/src/objects/intl-objects.cc"
                        "deps/v8/src/runtime/runtime-intl.cc")
                      (("TRUE") "true")
                      (("FALSE") "false"))

         ;; Remove bundled software.
         (for-each delete-file-recursively
                   '("deps/cares"
                     "deps/icu-small"
		     "deps/llhttp"
                     "deps/nghttp2"
                     "deps/openssl"
                     "deps/uv"
                     "deps/zlib"))
	 
         (substitute* "Makefile"
                      ;; Remove references to bundled software.
                      (("deps/uv/include/\\*.h") "")
                      (("deps/uv/uv.gyp") "")
		      (("deps/llhttp/llhttp.gyp") "")
                      (("deps/zlib/zlib.gyp") ""))))))
   (build-system gnu-build-system)
   (arguments
    `(#:configure-flags
      (let ((LLHTTP_DIR (assoc-ref %build-inputs "llhttp")))
	(list "--shared-http-parser-libname=llhttp"
              (string-append "--shared-http-parser-includes=" LLHTTP_DIR "/include")
              (string-append "--shared-http-parser-libpath=" LLHTTP_DIR "/lib")
              "--shared-cares"
              "--shared-http-parser"
              "--shared-libuv"
              "--shared-nghttp2"
              "--shared-openssl"
              "--shared-zlib"
              "--with-intl=system-icu"))

      #:tests? #f
      
      #:modules
      ((guix build gnu-build-system)
       (guix build utils)
       (srfi srfi-1)
       (ice-9 match))
      
      #:phases
      (modify-phases
       %standard-phases
       (add-before 'configure
		   'patch-hardcoded-program-references
		   (lambda*
		    (#:key inputs #:allow-other-keys)
		    (let ((LLHTTP_DIR (assoc-ref %build-inputs "llhttp")))
		      ;; Fix hardcoded /bin/sh references.
		      (substitute*
		       (let ((common
			      '("lib/child_process.js"
				"lib/internal/v8_prof_polyfill.js"
				"test/parallel/test-child-process-spawnsync-shell.js"
				"test/parallel/test-stdio-closed.js"
				"test/sequential/test-child-process-emfile.js"))
			     ;; not in bootstap node:
			     (sigxfsz "test/parallel/test-fs-write-sigxfsz.js"))
			 (if (file-exists? sigxfsz)
			     (cons sigxfsz common)
			     common))
		       (("'/bin/sh'")
			(string-append "'" (search-input-file inputs "/bin/sh") "'")))

		      ;; Fix hardcoded /usr/bin/env references.
		      (substitute* '("test/parallel/test-child-process-default-options.js"
				     "test/parallel/test-child-process-env.js"
				     "test/parallel/test-child-process-exec-env.js")
				   (("'/usr/bin/env'")
				    (string-append "'" (assoc-ref inputs "coreutils")
						   "/bin/env'"))))))
       
       (add-after 'patch-hardcoded-program-references
		  'delete-problematic-tests
		  (lambda*
		   (#:key inputs #:allow-other-keys)

		   ;; This requires a DNS resolver.
		   (delete-file "test/parallel/test-dns.js")

		   ;; FIXME: These tests fail on armhf-linux:
		   ;; https://github.com/nodejs/node/issues/31970
		   ,@(if (string-prefix? "arm" (%current-system))
			 '((for-each delete-file
				     '("test/parallel/test-zlib.js"
				       "test/parallel/test-zlib-brotli.js"
				       "test/parallel/test-zlib-brotli-flush.js"
				       "test/parallel/test-zlib-brotli-from-brotli.js"
				       "test/parallel/test-zlib-brotli-from-string.js"
				       "test/parallel/test-zlib-convenience-methods.js"
				       "test/parallel/test-zlib-random-byte-pipes.js"
				       "test/parallel/test-zlib-write-after-flush.js")))
			 '())

		   ;; These tests have an expiry date: they depend on the validity of
		   ;; TLS certificates that are bundled with the source.  We want this
		   ;; package to be reproducible forever, so remove those.
		   ;; TODO: Regenerate certs instead.
		   (for-each delete-file
			     '("test/parallel/test-tls-passphrase.js"
			       "test/parallel/test-tls-server-verify.js"))))

       (add-after 'patch-shebangs
		  'patch-nested-shebangs
		  (lambda* (#:key inputs outputs #:allow-other-keys)
			   ;; Based on the implementation of patch-shebangs
			   ;; from (guix build gnu-build-system).
			   (let ((path (append-map (match-lambda
						    ((_ . dir)
						     (list (string-append dir "/bin")
							   (string-append dir "/sbin")
							   (string-append dir "/libexec"))))
						   (append outputs inputs))))
			     (for-each
			      (lambda (file)
				(patch-shebang file path))
			      (find-files (search-input-directory outputs "lib/node_modules")
					  (lambda (file stat)
					    (executable-file? file))
					  #:stat lstat)))))

       (replace 'configure
		;; Node's configure script is actually a python script, so we can't
		;; run it with bash.
		(lambda* (#:key outputs (configure-flags '()) native-inputs inputs
			  #:allow-other-keys)
			 (let ((out (assoc-ref outputs "out"))
			       (PYTHON_DIR (assoc-ref %build-inputs "python@3.9")))
			   (format #t "build directory: ~s~%" (getcwd))
			   (format #t "configure flags: ~s~%" configure-flags)
			   (format #t "output directory: ~s~%" out)
			   ;; Node's configure script expects the CC environment variable to
			   ;; be set.
			   (setenv "CC_host" "gcc")
			   (setenv "CXX_host" "g++")
			   (setenv "CC" ,(cc-for-target))
			   (setenv "CXX" ,(cxx-for-target))
			   (setenv "PKG_CONFIG" ,(pkg-config-for-target))
			   (apply invoke
				  (string-append PYTHON_DIR "/bin/python3.9")
				  "configure.py"
				  (string-append "--prefix=" out)
				  configure-flags))))
       
       (add-after 'install
		  'install-npmrc
		  ;; Note: programs like node-gyp only receive these values if
		  ;; they are started via `npm` or `npx`.
		  ;; See: https://github.com/nodejs/node-gyp#npm-configuration
		  (lambda* (#:key outputs #:allow-other-keys)
			   (let* ((out (assoc-ref outputs "out")))
			     (with-output-to-file
				 ;; Use the config file "primarily for distribution
				 ;; maintainers" rather than "{prefix}/etc/npmrc",
				 ;; especially because node-build-system uses --prefix
				 ;; to install things to their store paths:
				 (string-append out "/lib/node_modules/npm/npmrc")
			       (lambda ()
				 ;; Tell npm (mostly node-gyp) where to find our
				 ;; installed headers so it doesn't try to
				 ;; download them from the internet:
				 (format #t "nodedir=~a\n" out)))))))))

   (native-search-paths
    (list (search-path-specification
           (variable "NODE_PATH")
           (files '("lib/node_modules")))))
   (inputs
    `(
      ("bash" ,bash)
      ;; fatal error: ares_nameser.h: No such file or directory
      ;; c-ares version >= 1.18.1
      ("c-ares" ,c-ares-1.18.1)
      ("coreutils" ,coreutils)
      ("findutils" ,findutils)
      ("gcc-toolchain" ,gcc-toolchain)
      ("grep" ,grep)
      ("icu4c" ,icu4c)
      ("libuv" ,libuv)
      ("llhttp" ,llhttp-6.0.10)
      ("make" ,gnu-make)
      ("nghttp2" ,nghttp2 "lib")
      ("openssl" ,openssl-1.1)
      ("perl" ,perl)
      ("pkg-config" ,pkg-config)
      ("procps" ,procps)
      ("python-wrapper" ,python-wrapper) ;; for node-gyp (supports python3)
      ("python@3.9" ,python-3.9)
      ("sed" ,sed)
      ("util-linux" ,util-linux)
      ("zlib" ,zlib)
      ;; ("http-parser" ,http-parser)
      ))

   (synopsis "Evented I/O for V8 JavaScript")
   (description
    "Node.js is a platform built on Chrome's JavaScript runtime
for easily building fast, scalable network applications.  Node.js uses an
event-driven, non-blocking I/O model that makes it lightweight and efficient,
perfect for data-intensive real-time applications that run across distributed
devices.")
   (home-page "https://nodejs.org/")
   (license license:expat)
   (properties '((max-silent-time . 7200) ;2h, needed on ARM
                 (timeout . 21600)	  ;6h
                 (cpe-name . "node.js")))))

;; TODO
(define-public OpenScientist-batch
  (package
   (name "OpenScientist-batch")
   (version "16.11.8")
   (source (origin
	    (method url-fetch)
            (uri "https://softinex.lal.in2p3.fr/down_load/OpenScientist/16.11.8/osc_batch_source_16.11.8.zip")
	    (sha256
             (base32
	      "0pn3y9v1q8x67v5nr2p17mv1qx1hhz0hz0sj1kn6y06av4xg83kl"))))
   (build-system trivial-build-system)
   (inputs
    `(("bash" ,bash)
      ("coreutils" ,coreutils) ;; dirname
      ("unzip" ,unzip)
      ("findutils" ,findutils)
      ("gcc-toolchain" ,gcc-toolchain)))
   (arguments
    `(#:modules ((guix build utils))
      #:builder
      (begin
        (use-modules (guix build utils))
	
        (define (echo-var var)
          (display (list var ": " (getenv var)))
          (newline))

	(define (set-path)
	  (let* ((packages (alist-delete "source" %build-inputs))
                 (packages-path (map cdr packages)))
            (setenv
             "PATH"
             (apply
              string-append
              (getenv "PATH") ":"
              (map (lambda (p) (string-append p "/bin:"))
                   packages-path)))))
	
	(define (patch-source-shebangs dir)
          (for-each
           patch-shebang
           (find-files dir
                       (lambda (file stat)
                         ;; Filter out symlinks.                                
                         (eq? 'regular (stat:type stat)))
                       #:stat lstat)))

	(let* ((source (assoc-ref %build-inputs "source"))
	       (coreutils-dir (assoc-ref %build-inputs "coreutils"))
	       (unzip-dir (assoc-ref %build-inputs "unzip"))
	       (unzip-bin (string-append unzip-dir "/bin/unzip"))
	       (GCC_DIR (assoc-ref %build-inputs "gcc-toolchain"))
	       )

	  (setenv "GUIX_LD_WRAPPER_ALLOW_IMPURITIES" "no")
	  (setenv "LIBRARY_PATH" (string-append GCC_DIR "/lib"))
	  
	  (set-path)
	  (setenv
           "CPLUS_INCLUDE_PATH"
	   (string-append
	    ;; Pour <linux/errno.h>
	    (string-append GCC_DIR "/include" ":")
	    ))
	
	  (display (list "build-inputs" %build-inputs)) (newline)
          (display (list "pwd: " (getcwd))) (newline)
	  (display (list "source: " source)) (newline)
          (display (list "env: " (environ))) (newline)

	  (invoke unzip-bin source)

	  (chdir "OpenScientist")
	  (substitute* '("osc_batch/obuild/sh/build"
			 "obuild/obuild/sh/build_app_owrap")
		       (("/bin/mkdir")
			(string-append coreutils-dir "/bin/mkdir"))
		       (("/bin/rm")
			(string-append coreutils-dir "/bin/rm")))
	  
	  (patch-source-shebangs (getcwd))
	  
	  (setenv "osc_home" (getcwd))
	  (setenv "OBUILD_PATH" (getcwd))

	  (display "Running BUILD") (newline)
	  
	  (chdir "osc_batch/obuild")
	  (invoke "sh/build" "-x")
	  
	  ))))

   (home-page "https://softinex.lal.in2p3.fr/OpenScientist/16.11.8_1/index.html")
   (synopsis "OpenScientist is now deprecated")
   (description "OpenScientist is an integration of open source products working together to do scientific visualization and data analysis, in particular for high energy physics (HEP).")
   (license license:gpl2)))

(define-public CLHEP-2.3.4
  (package
   (name "CLHEP-2.3.4")
   (version "2.3.4.3")
   (source
    (origin
     (method url-fetch)
     (uri "https://gitlab.cern.ch/CLHEP/CLHEP/-/archive/CLHEP_2_3_4_3/CLHEP-CLHEP_2_3_4_3.tar.gz")
     (sha256
      (base32
       "0h1mpc795lzq0z562jiq8w7rv0l1kwjjb98ynydqmxb0djnsmsli"))))
   (build-system cmake-build-system)
   (inputs
    `())
   
   (home-page "https://proj-clhep.web.cern.ch/proj-clhep/")
   (synopsis "HEP-specific foundation and utility classes")
   (description "HEP-specific foundation and utility classes such as random generators, physics vectors, geometry and linear algebra. CLHEP is structured in a set of packages independent of any external package")
   (license license:gpl3+)))

;; ---------------------------------------- ;; 

(define-public dcap-2.47.12 ;; Ok
  (package
   (name "dcap")
   (version "2.47.12")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/dCache/dcap/archive/"
			 version ".tar.gz"))
     (sha256
      (base32
       "1b04r0paazdc8004ijcy3pk40ff4mgwqc9fpsmcg7as1q8h8s2h5"))))
   (build-system gnu-build-system)
   (inputs
    `(("bash" ,bash)
      ("gcc-toolchain" ,gcc-toolchain)
      ("python" ,python-2.7)
      ("autoconf" ,autoconf)
      ("automake" ,automake)
      ("cunit" ,cunit)
      ("zlib" ,zlib)
      ("zsh" ,zsh)
      ("libtool" ,libtool)
      ("pkg-config" ,pkg-config)))
   
   (arguments
    `(#:phases
      (modify-phases
       %standard-phases
       (add-before 'configure 'bootstrap
		   (lambda args
		     (invoke "sh" "./bootstrap.sh"))))))
   
   (home-page "https://www.dcache.org/downloads/dcap/")
   (synopsis "DCAP")
   (description "dCache access protocol client library ")
   (license license:lgpl2.1+)))

;; ;; Error: No package 'davix_copy' found
;;
;; (define-public gfal2-2.17.2
;;   (package
;;    (name "gfal2")
;;    (version "2.17.2")
;;    (source
;;     (origin
;;      (method url-fetch)
;;      (uri (string-append "https://gitlab.cern.ch/dmc/gfal2/-/archive/v"
;; 			 version "/gfal2-v" version 
;; 			 ".tar.gz"))
;;      (sha256
;;       (base32 "00pg8c1dc26skd65yy8v2p6sc9q2kygs4hh6sblhg9laqma4d2vx"))))
;;    (build-system cmake-build-system)
;;    (inputs
;;     `(("coreutils" ,coreutils)
;;       ("gcc-toolchain" ,gcc-toolchain)
;;       ("glib" ,glib)
;;       ("davix" ,davix-0.6.4)
;;       ("pkg-config" ,pkg-config)
;;       ("jsoncpp" ,jsoncpp)
;;       ("doxygen" ,doxygen)
;;       ("pugixml" ,pugixml)
;;       ("dcap" ,dcap-2.47.12)
;;       ))
;;    ;; (arguments ...)
;;    (home-page "https://dmc.web.cern.ch/projects/gfal-2/home")
;;    (synopsis "GFAL2")
;;    (description "GFAL2 is a C library providing an abstraction layer of the grid storage system complexity.")
;;    (license license:asl2.0)))

;; TODO
(define-public davix-0.6.4
  (package
   (name "davix")
   (version "0.6.4")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/cern-fts/davix/archive/R_"
			 (string-replace-substring version "." "_")
			 ".tar.gz"))
     (sha256
      (base32 "1yh3dpg4iajpwbswv9y56s1jy5j687ampsdf1j746ac1drwkanly"))))
   (build-system cmake-build-system)
   (inputs
    `(("coreutils" ,coreutils)
      ("gcc-toolchain" ,gcc-toolchain)
      ("python" ,python-2.7)
      ("libxml2" ,libxml2)
      ("openssl" ,openssl-1.0)
      ("libuuid" ,util-linux+udev)
      ("boost" ,boost)
      ))
   ;; (arguments ...)
   (home-page "https://davix.web.cern.ch/")
   (synopsis "Davix")
   (description "The davix project aims to make file management over HTTP-based protocols simple.")
   (license license:lgpl2.1+)))

;; Failed
(define-public llvm-5
  (package
   (inherit llvm-7)
   (version "5.0.2")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://releases.llvm.org/"
				version "/llvm-"
				version ".src.tar.xz"))
            (sha256
             (base32
	      "0g1bbj2n6xv4p1n6hh17vj3vpvg56wacipc81dgwga9mg2lys8nm"))))))

;; Failed
(define-public libAfterImage
  (package
   (name "libAfterImage")
   (version "1.20")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
	   "ftp://ftp.afterstep.org/stable/libAfterImage/"
	   name "-" version ".tar.gz"))
     (sha256
      (base32 "125y119fbr3g389nr7yls4i7x5zd5pz7h8qn12k8b21b4xk1h6y5"))))
   (build-system gnu-build-system)
   (inputs
    `(("coreutils" ,coreutils)
      ("gcc-toolchain" ,gcc-toolchain)
      ("zlib" ,zlib)))
   (arguments 
    `(#:tests? #f)) ; no tests in Makefile
   (home-page "http://www.afterstep.org/afterimage/")
   (synopsis "LibAfterImage")
   (description "LibAfterImage")
   (license license:lgpl2.1+)))

;; Ok
(define-public vdt
  (package
   (name "vdt")
   (version "0.4.3")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
	   "http://lcgpackages.web.cern.ch/lcgpackages/tarFiles/sources/"
	   name "-" version ".tar.gz"))
     (sha256
      (base32 "1qdc10p4j6jl0as3a8pfvrygxdry2x6izxm8clmihp5v5rhp8mkh"))))
   (build-system cmake-build-system)
   (inputs
    `(("coreutils" ,coreutils)
      ("gcc-toolchain" ,gcc-toolchain)
      ("python" ,python-2.7)))
   (arguments 
    `(#:tests? #f)) ; no tests
   (home-page "https://root.cern/root/html606/md_math_vdt_ReadMe.html")
   (synopsis "VDT")
   (description "VDT")
   (license license:lgpl2.1+)))

;; Ok
(define-public Unuran-1.8.1
  (package
   (name "Unuran")
   (version "1.8.1")
   (source
    (origin
     (method url-fetch)
     (uri "http://statistik.wu-wien.ac.at/unuran/unuran-1.8.1.tar.gz")
     (sha256
      (base32
       "14si7jqq6mfk5whfbnvdhm97ylg0cpw3s12hcjndnmvqhnbaww62"))))
   (build-system gnu-build-system)
   (inputs
    `())

   (arguments
    '(#:configure-flags '("--with-pic")))
   
   (home-page "http://statistik.wu-wien.ac.at/unuran/")
   (synopsis "Universal Non-Uniform RAndom Number generator")
   (description "UNU.RAN (Universal Non-Uniform RAndom Number
generator) is a collection of algorithms for generating non-uniform
pseudorandom variates")
   (license license:gpl2)))

;; Erreur de compilation :
;; error while loading shared libraries: libLLVMCoroutines.so.5
(define-public ROOT-6.18.04
  (package
   (name "ROOT")
   (version "6.18.04")
   (source (origin
	    (method url-fetch)
	    (uri (string-append "https://root.cern/download/root_v"
				version ".source.tar.gz"))
	    (sha256
	     (base32
	      "196ghma6g5a7sqz52wyjkgvmh4hj4vqwppm0zwdypy33hgy8anii"))
	    (file-name (string-append name "-" version ".tar.gz"))
	    (modules '((guix build utils)))
	    ))

   ;; 

   (build-system cmake-build-system)
   (inputs
    `(

      ;; Package to build to avoid ROOT downloading them
      
      ("dcap" ,dcap-2.47.12)
      ("davix" ,davix-0.6.4)
      ("libAfterImage" ,libAfterImage)
      ("vdt" ,vdt)

      ("unuran" ,Unuran-1.8.1)
      
      ;; Dependencies
      
      ("binutils" ,binutils)
      ("cfitsio" ,cfitsio)
      ("coreutils" ,coreutils)
      ("gcc-lib" ,gcc "lib")
      ("gcc-toolchain" ,gcc-toolchain)
      ("git" ,git)
      ("glibc" ,glibc)
      ("liblzma" ,xz)
      ("less" ,less)
      ("libc" ,glibc)
      ("libc-debug" ,glibc "debug")
      ("libc-static" ,glibc "static")
      ("libpthread-stubs" ,libpthread-stubs)
      ("libcxx" ,libcxx)
      ("libx11" ,libx11)
      ("libxext" ,libxext)
      ("libxft" ,libxft)
      ("libxml2" ,libxml2)
      ("libxpm" ,libxpm)
      ("llvm-5" ,llvm-5)
      ("lz4" ,lz4)
      ("openblas" ,openblas)
      ("openssl" ,openssl)
      ("pcre" ,pcre)
      ("perl" ,perl)
      ("pkg-config" ,pkg-config)
      ("python" ,python-2.7)
      ("python-numpy" ,python-numpy)
      ("tbb" ,tbb)
      ("xxhash" ,xxhash)
      ("zlib" ,zlib)
      ("zstd" ,zstd)
      ;; ("libjpeg" ,libjpeg) ;; Deprecated
      ("libjpeg-turbo" ,libjpeg-turbo) 
      ("gsl" ,gsl)
      ("glu" ,glu)
      ("mesa" ,mesa)
      ("fftw" ,fftw)))

   (arguments 
    `(#:configure-flags 
      
      (list

       ;; To avoid error "fatal error: module file not found"
       ;; "-DCMAKE_INSTALL_PREFIX=/opt/root/"  ;; error

       (let ((out (assoc-ref %outputs "out")))
	 (string-append "-DCMAKE_INSTALL_PREFIX=" out))

       ;; From https://root.cern.ch/building-root
       "-Dgnuinstall=ON"
       
       ;; To avoid downloading clad, llvm, davix
       "-Dclad=OFF"
       "-Dbuiltin_llvm=OFF"
       "-Dbuiltin_davix=OFF"

       ;; sinon configure: error: cannot run /bin/sh ./config.sub
       "-Dbuiltin_afterimage=OFF"

       "-Dminuit2=ON"
       "-Dunuran=ON"
       
       ;; "-Dbuiltin_clang=OFF" ;; error
       
       ;; Due to ROOT-specific patches you need a special
       ;; version of clang.  You cannot use vanilla clang.

       ;; Test pour erreur libLLVMCoroutines.so.5
       ;; Non ca ne marche pas 
       ;; (string-append "-DLD_LIBRARY_PATH="
       ;;                (assoc-ref %build-inputs "llvm-5")
       ;;                "/lib")

       ;; From llvm.scm
       (string-append "-DC_INCLUDE_DIRS="
		      (assoc-ref %build-inputs "libc")
		      "/include"))

      ;; To avoid "depends on .. which cannot be found in RUNPATH"
      #:validate-runpath? #f

      #:phases
      (modify-phases
       %standard-phases
       (add-before
        ;; avec build LD_LIBRARY_PATH est vide pour G__Core.cxx
        'configure 'fix-library-path
        ;; Sinon error while loading shared libraries: libLLVMTableGen.so.5
        (lambda*
         (#:key inputs outputs #:allow-other-keys)
         
         (define (add-libraries libpath inputs)
           (let ((inputs (alist-delete "source" inputs)))
             (apply string-append
                    libpath
                    (map (lambda (p)
                           (string-append (cdr p) "/lib:"))
                         inputs))))

         (let* ((libpath (getenv "LD_LIBRARY_PATH"))
                (libpath (if libpath (string-append libpath ":") ""))
                (libpath (add-libraries libpath inputs)))
           
           ;; (display (list "LD_LIBRARY_PATH" libpath)) (newline)
           (setenv "LD_LIBRARY_PATH" libpath)
           #t))))
      
      ))

   ;; From llvm.scm
   (native-search-paths
    (list (search-path-specification
	   (variable "CPATH")
	   (files '("include")))
	  (search-path-specification
	   (variable "LIBRARY_PATH")
	   (files '("lib" "lib64")))))
   
   (home-page "https://root.cern.ch/")
   (synopsis "ROOT: Data Analysis Framework")
   (description
    "A modular scientific software toolkit.  It provides all the
functionalities needed to deal with big data processing, statistical
analysis, visualisation and storage.  It is mainly written in C++ but
integrated with other languages such as Python and R.")
   (license license:lgpl2.1+)))

;; Error
(define-public ROOT-6.20.02
  (package
   (name "ROOT")
   (version "6.20.02")
   (source (origin
	    (method url-fetch)
	    (uri (string-append "https://root.cern/download/root_v"
				version ".source.tar.gz"))
	    (sha256
	     (base32
	      "19nbgl4cixd39c8ic8i66x16mszmxgybzv88dyyazh4py1mmi5q9"))
	    (file-name (string-append name "-" version ".tar.gz"))
	    (modules '((guix build utils)))
	    ))
   (build-system cmake-build-system)
   (inputs
    `(

      ;; Package to build to avoid ROOT downloading them
      
      ("dcap" ,dcap-2.47.12)
      ("davix" ,davix-0.6.4)
      ("libAfterImage" ,libAfterImage)
      ("vdt" ,vdt)

      ;; Dependencies
      
      ("binutils" ,binutils)
      ("cfitsio" ,cfitsio)
      ("coreutils" ,coreutils)
      ("freetype" ,freetype)
      ("gcc-lib" ,gcc "lib")
      ("gcc-toolchain" ,gcc-toolchain)
      ("git" ,git)
      ("glibc" ,glibc)
      ("liblzma" ,xz)
      ("less" ,less)
      ("libc" ,glibc)
      ("libc-debug" ,glibc "debug")
      ("libc-static" ,glibc "static")
      ("libpthread-stubs" ,libpthread-stubs)
      ("libcxx" ,libcxx)
      ("libx11" ,libx11)
      ("libxext" ,libxext)
      ("libxft" ,libxft)
      ("libxml2" ,libxml2)
      ("libxpm" ,libxpm)
      ("llvm-5" ,llvm-5)
      ("lz4" ,lz4)
      ("openblas" ,openblas)
      ("openssl" ,openssl)
      ("pcre" ,pcre)
      ("perl" ,perl)
      ("pkg-config" ,pkg-config)
      ("python" ,python-2.7)
      ("python-numpy" ,python-numpy)
      ("tbb" ,tbb)
      ("xxhash" ,xxhash)
      ("zlib" ,zlib)
      ("zstd" ,zstd)
      ;; ("libjpeg" ,libjpeg) ;; Deprecated
      ("libjpeg-turbo" ,libjpeg-turbo) 
      ("gsl" ,gsl)
      ("glu" ,glu)
      ("mesa" ,mesa)
      ("fftw" ,fftw)))

   (arguments 
    `(#:configure-flags 
      
      (list

       ;; To avoid error "fatal error: module file not found"
       ;; "-DCMAKE_INSTALL_PREFIX=/opt/root/"  ;; error

       (let ((out (assoc-ref %outputs "out")))
	 (string-append "-DCMAKE_INSTALL_PREFIX=" out))

       ;; From https://root.cern.ch/building-root
       "-Dgnuinstall=ON"
       
       ;; To avoid downloading clad, llvm, davix
       "-Dclad=OFF"
       "-Dbuiltin_llvm=OFF"
       "-Dbuiltin_davix=OFF"

       ;; "-Dbuiltin_clang=OFF" ;; error
       
       ;; Due to ROOT-specific patches you need a special
       ;; version of clang.  You cannot use vanilla clang.
       
       ;; From llvm.scm
       (string-append "-DC_INCLUDE_DIRS="
		      (assoc-ref %build-inputs "libc")
		      "/include"))

      ;; To avoid "depends on .. which cannot be found in RUNPATH"
      #:validate-runpath? #f

      #:tests? #f
      
      #:phases
      (modify-phases
       %standard-phases
       (add-before
        ;; avec build LD_LIBRARY_PATH est vide pour G__Core.cxx
        'configure 'fix-library-path
        ;; Sinon error while loading shared libraries: libLLVMTableGen.so.5
        (lambda*
         (#:key inputs outputs #:allow-other-keys)
         
         (define (add-libraries libpath inputs)
           (let ((inputs (alist-delete "source" inputs)))
             (apply string-append
                    libpath
                    (map (lambda (p)
                           (string-append (cdr p) "/lib:"))
                         inputs))))

         (let* ((libpath (getenv "LD_LIBRARY_PATH"))
                (libpath (if libpath (string-append libpath ":") ""))
                (libpath (add-libraries libpath inputs)))
           
           ;; (display (list "LD_LIBRARY_PATH" libpath)) (newline)
           (setenv "LD_LIBRARY_PATH" libpath)
           #t))))
      
      ))

   ;; From llvm.scm
   (native-search-paths
    (list (search-path-specification
	   (variable "CPATH")
	   (files '("include")))
	  (search-path-specification
	   (variable "LIBRARY_PATH")
	   (files '("lib" "lib64")))))
   
   (home-page "https://root.cern.ch/")
   (synopsis "ROOT: Data Analysis Framework")
   (description
    "A modular scientific software toolkit.  It provides all the
functionalities needed to deal with big data processing, statistical
analysis, visualisation and storage.  It is mainly written in C++ but
integrated with other languages such as Python and R.")
   (license license:lgpl2.1+)))




  
