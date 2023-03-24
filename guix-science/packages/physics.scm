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


(define-module (guix-science packages physics)
  #:use-module  ((guix licenses) #:prefix license:)
  #:use-module  (gnu packages adns)  ;; c-ares
  #:use-module  (gnu packages algebra)	 ;; FFTW
  #:use-module  (gnu packages astronomy) ;; cfitsio
  #:use-module  (gnu packages autotools)
  #:use-module  (gnu packages backup)
  #:use-module  (gnu packages base)
  #:use-module  (gnu packages base) ;; gnu-make
  #:use-module  (gnu packages bash)
  #:use-module  (gnu packages boost)
  #:use-module  (gnu packages bootstrap)
  #:use-module  (gnu packages check) 
  #:use-module  (gnu packages commencement) ;; gcc-toolchain
  #:use-module  (gnu packages compression)  ;; zlib lz4
  #:use-module  (gnu packages curl) 
  #:use-module  (gnu packages databases)
  #:use-module  (gnu packages digest)
  #:use-module  (gnu packages documentation)
  #:use-module  (gnu packages file) 
  #:use-module  (gnu packages fontutils) ;; fontconfig
  #:use-module  (gnu packages fontutils) ;; freetype
  #:use-module  (gnu packages gcc)
  #:use-module  (gnu packages geo)
  #:use-module  (gnu packages gl)
  #:use-module  (gnu packages glib)
  #:use-module  (gnu packages hunspell)
  #:use-module  (gnu packages icu4c) 
  #:use-module  (gnu packages image) ;; libjpeg
  #:use-module  (gnu packages less)
  #:use-module  (gnu packages libevent) ;; libuv
  #:use-module  (gnu packages libreoffice) ;; hunspell
  #:use-module  (gnu packages linux)
  #:use-module  (gnu packages llvm)  ;; llvm clang
  #:use-module  (gnu packages maths) ;; openblas gsl
  #:use-module  (gnu packages monitoring)
  #:use-module  (gnu packages openstack)
  #:use-module  (gnu packages pcre)
  #:use-module  (gnu packages pdf) ;; poppler-qt5
  #:use-module  (gnu packages perl)
  #:use-module  (gnu packages pkg-config)
  #:use-module  (gnu packages python)
  #:use-module  (gnu packages python-build)
  #:use-module  (gnu packages python-crypto) ;; python-cryptography
  #:use-module  (gnu packages python-web) ;; python-oauthlib
  #:use-module  (gnu packages python-xyz) ;; numpy 
  #:use-module  (gnu packages qt)
  #:use-module  (gnu packages serialization)
  #:use-module  (gnu packages shells)
  #:use-module  (gnu packages shells)
  #:use-module  (gnu packages tbb)
  #:use-module  (gnu packages time)
  #:use-module  (gnu packages tls) ;; openssl
  #:use-module  (gnu packages version-control) ;; git
  #:use-module  (gnu packages web)   ;; http-parser
  #:use-module  (gnu packages xml)
  #:use-module  (gnu packages xorg) ;; libx11
  #:use-module  (gnu packages)
  #:use-module  (guix build-system cmake)
  #:use-module  (guix build-system gnu)
  #:use-module  (guix build-system python)  
  #:use-module  (guix build-system trivial)
  #:use-module  (guix download)
  #:use-module  (guix gexp)
  #:use-module  (guix git-download)
  #:use-module  (guix packages)
  #:use-module  (guix utils)
  
  #:use-module  (ice-9 match)
  #:use-module  (ice-9 regex)

  ;; TODO: Verify if still Ok after guix pull
  
  #:export (

	    ;; Concerning versions, we have to keep several
	    ;; versions at the same time because users want
	    ;; to be able to install a specific version of
	    ;; each tool.
	    
	    ;; Dependencies
	    llhttp-6.0.10   ;; Ok
	    llhttp-8.1.0    ;; Ok
	    c-ares-1.18.1   ;; Ok
	    
	    ;; libcern
	    CLHEP-2.3.4         ;; Ok
	    nodejs-16.13.1      ;; Ok
	    TALYS-1.96          ;; Ok
	    texworks-0.6.7      ;; Ok
	    timing-gen-0.9.8    ;; Ok

	    ;; Others: 
	    
	    Unuran-1.8.1       ;; Ok
	    clang-9.0.1        ;; Ok
	    davix-0.8.3        ;; Ok
	    dcap-2.47.12       ;; Ok
	    libAfterImage-1.20 ;; Ok
	    llvm-5             ;; Ok
	    llvm-9.0.1         ;; Ok
	    vdt-0.4.3          ;; Ok

	    ;; -----
	    
	    davix-0.6.4	                ;; TODO Failed
	    cubix-3.0                   ;; TODO depends on ROOT
	    dawn-3.91a                  ;; TODO configure is an interactive script
	    dirac-8.0.6                 ;; TODO needs rucio-clients etc.
	    GATE                        ;; TODO depends on ROOT
	    gammaware-AGATAD_P2_COM_001 ;; TODO depends on ROOT

	    OpenScientist-batch ;; TODO deprecated but still needed

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

(define-public CLHEP-2.1.2.5
  (package
   (name "CLHEP-2.1.2.5")
   (version "2.1.2.5")
   (source
    (origin
     (method url-fetch)
     (uri
      "https://gitlab.cern.ch/CLHEP/CLHEP/-/archive/CLHEP_2_1_2_5/CLHEP-CLHEP_2_1_2_5.tar.gz")
     (sha256
      (base32
       "0a975ad65fiz4m1in4ivfwirhcr52yq8lrv3qpnshb1y4pm0gacz"))))
   (build-system cmake-build-system)
   (inputs
    `())
   
   (home-page "https://proj-clhep.web.cern.ch/proj-clhep/")
   (synopsis "HEP-specific foundation and utility classes")
   (description "HEP-specific foundation and utility classes such as random generators, physics vectors, geometry and linear algebra. CLHEP is structured in a set of packages independent of any external package")
   (license license:gpl3+)))

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

(define-public CLHEP-2.4.6.0
  (package
   (name "CLHEP-2.4.6.0")
   (version "2.4.6.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://gitlab.cern.ch/CLHEP/CLHEP/-/archive/CLHEP_2_4_6_0/CLHEP-CLHEP_2_4_6_0.tar.gz")
     (sha256
      (base32
       "1cqcrbdaanvc76sjc513b8hn47pz5mdspcgvjsrpc4i2si3grpzj"))))
   (build-system cmake-build-system)
   (inputs
    `())
   
   (home-page "https://proj-clhep.web.cern.ch/proj-clhep/")
   (synopsis "HEP-specific foundation and utility classes")
   (description "HEP-specific foundation and utility classes such as random generators, physics vectors, geometry and linear algebra. CLHEP is structured in a set of packages independent of any external package")
   (license license:gpl3+)))

(define-public CLHEP-2.4.6.2
  (package
   (name "CLHEP-2.4.6.2")
   (version "2.4.6.2")
   (source
    (origin
     (method url-fetch)
     (uri "https://gitlab.cern.ch/CLHEP/CLHEP/-/archive/CLHEP_2_4_6_2/CLHEP-CLHEP_2_4_6_2.tar.gz")
     (sha256
      (base32
       "18sm14ikdz8hym5b2c9yb5l4hvzjk77jfasahb8zs41fcx6r9gwp"))))
   (build-system cmake-build-system)
   (inputs
    `())
   
   (home-page "https://proj-clhep.web.cern.ch/proj-clhep/")
   (synopsis "HEP-specific foundation and utility classes")
   (description "HEP-specific foundation and utility classes such as random generators, physics vectors, geometry and linear algebra. CLHEP is structured in a set of packages independent of any external package")
   (license license:gpl3+)))

;; TODO depend on ROOT
(define-public cubix-3.0
  (package
   (name "cubix-3.0")
   (version "3.0")
   (source (origin
	    (method url-fetch)
            (uri "https://gitlab.in2p3.fr/dudouet/Cubix/-/archive/Cubix-3.0/Cubix-Cubix-3.0.tar.gz")
	    (sha256
             (base32
	      "1vpymb22k727h62n0bzb30bc2h85kh7qimjwnd7zik6q32vhi2mr"))))
   (build-system cmake-build-system)
   (inputs
    `(
      ;; ("bash" ,bash)
      ;; ("gcc-toolchain" ,gcc-toolchain)
      ("ROOT" ,ROOT-6.26.10)
      
      ))
   (arguments `())
   (home-page "https://gitlab.in2p3.fr/dudouet/Cubix")
   (synopsis "Cubix")
   (description "Cubix")
   (license license:expat)))

;; https://geant4.kek.jp/~tanaka/DAWN/About_DAWN.html
;; https://twiki.cern.ch/twiki/bin/view/CLIC/DawnVisualization
;; TODO: configure is an interactive script !
(define-public dawn-3.91a
  (package
   (name "dawn-3.91a")
   (version "3.91a")
   (source (origin
	    (method url-fetch)
            (uri "http://geant4.kek.jp/~tanaka/src/dawn_3_91a.tgz")
	    (sha256
             (base32
	      "1x7mpi77jylsv8mzsqs0ppchbq147azd0b94i2qq2xhis7m5bn41"))))
   (build-system cmake-build-system)
   (inputs
    `(("bash" ,bash)
      ("gcc-toolchain" ,gcc-toolchain)))
   (arguments `())
   (home-page "http://geant4.kek.jp/~tanaka")
   (synopsis "Dawn")
   (description "Fukui Renderer DAWN (Drawer for Academic WritiNgs)")
   (license license:expat)))

(define-public python-authlib
  (package
   (name "python-authlib")
   (version "1.2.0")
   (source (origin
	    (method url-fetch)
            (uri "https://github.com/lepture/authlib/archive/refs/tags/v1.2.0.tar.gz")
	    (sha256
             (base32
	      "1r3mk46g7b0x6p32js1wh6qwgg67m8knbk6dp6hxzl66yd83i6h4"))))
   (build-system python-build-system)
   (inputs
    `(("python-3.9" ,python-3.9)
      ("python-cryptography" ,python-cryptography)
      ))
   (arguments `(#:tests? #f))
   (home-page "https://github.com/lepture/authlib")
   (synopsis "Python authlib")
   (description "The ultimate Python library in building OAuth and OpenID Connect servers. JWS, JWK, JWA, JWT are included.")
   (license license:expat)))

(define-public python-dominate
  (package
   (name "python-dominate")
   (version "2.7.0")
   (source (origin
	    (method url-fetch)
            (uri "https://github.com/Knio/dominate/archive/refs/tags/2.7.0.tar.gz")
	    (sha256
             (base32
	      "03y2xlmf3fd01x7gds27kx09274sz9h1a3yj6bbbj98fq493zgzj"))))
   (build-system python-build-system)
   (inputs
    `(("python-3.9" ,python-3.9)
      ))
   (arguments `(#:tests? #f))
   (home-page "https://github.com/Knio/dominate")
   (synopsis "Dominate")
   (description "Dominate is a Python library for creating and manipulating HTML documents using an elegant DOM API. It allows you to write HTML pages in pure Python very concisely, which eliminates the need to learn another template language, and lets you take advantage of the more powerful features of Python.")
   (license license:gpl3+)))

(define-public python-typing-extensions-4.4.0
  (package
   (name "python-typing-extensions-4.4.0")
   (version "4.4.0")
   (source (origin
	    (method url-fetch)
            (uri "https://github.com/python/typing_extensions/archive/refs/tags/4.4.0.tar.gz")
	    (sha256
             (base32
	      "0rbfvnri7g43jn05izx1lfay0aa7a22rxvwlr8npwsl5p1pm6hqq"))))
   (build-system python-build-system)
   (inputs
    `(("python-3.9" ,python-3.9)
      ;; ("python-wheel" ,python-wheel)
      ("python-pypa-build" ,python-pypa-build)
      ("python-flit-core" ,python-flit-core)
      ))
   (arguments
     (list
      #:tests? #f       ;requires Python's test module, not available in Guix   
      #:phases
      #~(modify-phases %standard-phases
        ;; (add-after 'unpack 'enter-source-directory
	;; 	     (lambda _ (chdir "src") #t))
          ;; XXX: PEP 517 manual build copied from python-isort.                
          (replace 'build
            (lambda _
              (invoke "python" "-m" "build" "--wheel" "--no-isolation" ".")))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "python" "src/test_typing_extensions.py"))))
          (replace 'install
            (lambda _
              (let ((whl (car (find-files "dist" "\\.whl$"))))
                (invoke "pip" "--no-cache-dir" "--no-input"
                        "install" "--no-deps" "--prefix" #$output whl)))))))
   (home-page "https://github.com/python/typing_extensions")
   (synopsis "Typing Extensions")
   (description "The typing_extensions module serves two related purposes:
    - Enable use of new type system features on older Python versions. For example, typing.TypeGuard is new in Python 3.10, but typing_extensions allows users on previous Python versions to use it too.
    - Enable experimentation with new type system PEPs before they are accepted and added to the typing module.")
   (license license:psfl)))

;; TODO
(define-public dirac-8.0.6
  (package
   (name "dirac-8.0.6")
   (version "8.0.6")
   (source (origin
	    (method url-fetch)
            (uri "https://github.com/DIRACGrid/DIRAC/archive/refs/tags/v8.0.6.tar.gz")
	    (sha256
             (base32
	      "1f7r9d843xfcq1jbm5csajdmhndrghdli9nwkhzv20s0qg6rm8cv"))))
   (build-system python-build-system)
   (inputs
    `(
      ("python-3.9" ,python-3.9)
      ("python-authlib" ,python-authlib)
      ("python-boto3" ,python-boto3)
      ("python-botocore" ,python-botocore)
      ("python-cachetools" ,python-cachetools)
      ("python-certifi" ,python-certifi)
      ("python-dateutil" ,python-dateutil)
      ("python-dominate" ,python-dominate)
      ("python-importlib-metadata" ,python-importlib-metadata)
      ("python-m2crypto" ,python-m2crypto)
      ("python-pexpect" ,python-pexpect)
      ("python-prompt-toolkit" ,python-prompt-toolkit)
      ("python-psutil" ,python-psutil)
      ("python-pyasn1" ,python-pyasn1)
      ("python-pyasn1-modules" ,python-pyasn1-modules)
      ("python-pyjwt" ,python-pyjwt)
      ("python-pyparsing" ,python-pyparsing)
      ("python-pytz" ,python-pytz)
      ("python-requests" ,python-requests)
      ("python-setuptools" ,python-setuptools)
      ("python-sqlalchemy" ,python-sqlalchemy)
      ("python-typing-extensions" ,python-typing-extensions-4.4.0) ;; >= 4.3.0 !
      ("python-wheel" ,python-wheel)

      ("rucio" ,rucio-1.30.0)

      ;; Dependencies in environment.yml
      ;; TODO: diraccfg db12 fts3 gfal2-python 

      ))
   (arguments `(#:tests? #f ;; No network 
		))
   (home-page "http://diracgrid.org/")
   (synopsis "DIRAC")
   (description "DIRAC provides a complete solution to one or more user community requiring access to distributed resources. DIRAC builds a layer between the users and the resources offering a common interface to a number of heterogeneous providers, integrating them in a seamless manner, providing interoperability, at the same time as an optimized, transparent and reliable usage of the resources.")
   (license license:gpl3+)))

;; Fispact
;; https://www.oecd-nea.org/tools/abstract/detail/nea-1890/
;; http://fispact.ukaea.uk/nuclear-data/downloads/
;; Sources are behind authorization page

(define-public TALYS-1.96
  (package
   (name "TALYS-1.96")
   (version "1.96")
   (source
    (origin
     (method url-fetch)
     (uri "https://tendl.web.psi.ch/tendl_2019/talys/talys.tar")
     (sha256
      (base32 "1g8mj7jj8ld5mzj8k79hbypfhhzksx5k0b8fqmavlrlvx424v9zz"))))
   (build-system trivial-build-system)
   (inputs
    `(("bash" ,bash)
      ("coreutils" ,coreutils)
      ("sed" ,sed)
      ("tar" ,tar)
      ("gzip" ,gzip)
      ("gfortran-toolchain" ,gfortran-toolchain)
      
      ))
   (arguments
    `(#:modules ((guix build utils))
      #:builder
      (begin
        (use-modules (guix build utils))

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
	
	(let* ((source (assoc-ref %build-inputs "source"))
	       (out (assoc-ref %outputs "out"))
	       (BASH (string-append (assoc-ref %build-inputs "bash") "/bin/bash"))
	       (TAR  (string-append (assoc-ref %build-inputs "tar") "/bin/tar"))
	       )

	  (set-path)

	  (mkdir-p (string-append out "/bin"))
	  
	  (invoke TAR "xvf" source)
	  
	  (chdir "talys")
	  (invoke BASH "talys.setup")

	  ))))
	  

   (home-page "https://tendl.web.psi.ch/tendl_2021/talys.html")
   (synopsis "TALYS")
   (description "TALYS is an open source software package (GPL license)
for the simulation of nuclear reactions.")
   (license license:gpl3+)))

(define-public texworks-0.6.7
  (package
   (name "texworks-0.6.7")
   (version "0.6.7")
   (source
    (origin
     (method url-fetch)
     (uri "https://github.com/TeXworks/texworks/archive/release-0.6.7.zip")
     (sha256
      (base32 "0vwxq3i120ngr9z2hbdgrav91x64k94kss7wyxxpr48bv209pwlm"))))
  (build-system cmake-build-system)
  (inputs
   `(("unzip" ,unzip)
     ("qt5" ,qtbase-5)
     ("qttools" ,qttools-5)
     ("qtdeclarative-5" ,qtdeclarative-5)
     ("qtscript" ,qtscript)
     ("zlib" ,zlib)
     ("pkg-config" ,pkg-config)
     ("hunspell" ,hunspell)
     ("poppler-qt5" ,poppler-qt5)
     ("fontconfig" ,fontconfig)
     ))
  (arguments `(#:tests? #f))
  (home-page "https://www.tug.org/texworks/")
  (synopsis "TeXworks")
  (description "The TeXworks project is an effort to build a simple TeX front-end program (working environment) that will be available for all today's major desktop operating systems")
  (license license:gpl2+)))

(define-public timing-gen-0.9.8
  (package
   (name "timing-gen-0.9.8")
   (version "0.9.8")
   (source
    (origin
     (method url-fetch)
     (uri "https://sourceforge.net/projects/timing-gen/files/timing-gen-0.9.8.tgz")
     (sha256
      (base32 "1k9x9rcbqg9xax5z8w1fglycf5z8wiaym5kp3naia6m9ny959i3z"))))
   (build-system trivial-build-system)
   (inputs `(("gzip" ,gzip)
	     ("tar" ,tar)))

   (arguments
    `(#:modules ((guix build utils))
      #:builder
      (begin
        (use-modules (guix build utils))

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
	
	(let* ((source (assoc-ref %build-inputs "source"))
	       (out (assoc-ref %outputs "out"))
	       (TAR (string-append (assoc-ref %build-inputs "tar") "/bin/tar"))
	       )

	  (setenv "GUIX_LD_WRAPPER_ALLOW_IMPURITIES" "no")

	  (set-path)
	  (mkdir-p (string-append out "/bin"))
	  (mkdir-p (string-append out "/share/doc"))
	  (mkdir-p (string-append out "/share/samples"))
	  
	  (invoke TAR "zxvf" source)
	  (chdir "timing-gen-0.9.8")
	  (copy-recursively "doc" (string-append out "/share/doc"))
	  (copy-recursively "samples" (string-append out "/share/samples"))
	  (copy-file "timing-gen" (string-append out "/bin/timing-gen"))
	  (copy-file "timing-gen-viewer" (string-append out "/bin/timing-gen-viewer"))
	  
	  ))))

   (home-page "https://sourceforge.net/projects/timing-gen/")
   (synopsis "Timing Gen")
   (description "Timing-gen is a tool to generate high quality Postscript timing
diagrams from text input files.")
   (license license:expat)))

;; TODO: depends on ROOT
(define-public gammaware-AGATAD_P2_COM_001
  (package
   (name "gammaware-AGATAD_P2_COM_001")
   (version "AGATAD_P2_COM_001")
   (source
    (origin
     (method url-fetch)
     (uri "https://gitlab.in2p3.fr/IPNL_GAMMA/gammaware/-/archive/AGATAD_P2_COM_001/gammaware-AGATAD_P2_COM_001.tar.gz")
     (sha256
      (base32
       "1gmnrrnw7sg7d9y80761y89a35z01568jbj59sdi4mjigffijd3x"))))
   (build-system cmake-build-system)
   (inputs
    `(("bash" ,bash)
      ("gcc-toolchain" ,gcc-toolchain)
      ))

   (home-page "https://gitlab.in2p3.fr/IPNL_GAMMA/gammaware")
   (synopsis "gammaware")
   (description "General Tools to play with gamma-ray spectroscopy related data")
   (license license:gpl2+)))

'(define-public GATE
  "http://www.opengatecollaboration.org/"
  ;; Gate 10.1:
  ;; - Geant 4 10.7
  ;; - ROOT 6
  ;; - gcc 4.8 to 7.3
  ;; - cmake >= 3.3
  )

;; TODO: Pargate (parallel Gate)

;; TODO: gf3 -> Radware

;; TODO: fluka: source code behind a registration page
;; https://fluka.cern/download/latest-fluka-release
;; https://flukafiles.web.cern.ch/flukafiles/fluka-4-3.0_src/fluka-4-3.0.src.tgz

(define-public OscProb-1.4.1 ;; TODO: depends on ROOT
  (package
   (name "OscProb-1.4.1")
   (version "1.4.1")
   (source (origin
            (method url-fetch)
            (uri "https://github.com/joaoabcoelho/OscProb/archive/refs/tags/v1.4.1.tar.gz")
            (sha256
             (base32 "1fgzkc7l9c3bh0m9bzikcfffsr2hvz9clczws646mk8sgy1dpgjb"))))
   (build-system gnu-build-system)
   ;; No configure, only Makefile
   (home-page "https://github.com/joaoabcoelho/OscProb")
   (synopsis "OscProb")
   (description
    "OscProb is a small set of classes aimed at computing exact neutrino
oscillation probabilities with a few different models.")
   (license license:expat)))

(define-public globes-3.0.11
  (package
   (name "globes-3.0.11")
   (version "3.0.11")
   (source (origin
            (method url-fetch)
            (uri "https://www.mpi-hd.mpg.de/personalhomes/globes/download/globes-3.0.11.tar.gz")
            (sha256
             (base32 "1lbwdbh1bn3ayn85p08ifzvxrjmyi1vdxcj48v9zl20g39ssvypb"))))
   (build-system gnu-build-system)
   (inputs
    `(("bash" ,bash)
      ("gcc-toolchain" ,gcc-toolchain)
      ("gsl" ,gsl)
      ("OscProb" ,OscProb-1.4.1)
      ))
   (home-page "https://www.mpi-hd.mpg.de/personalhomes/globes")
   (synopsis "Globes")
   (description
    "Globes: General Long Baseline Experiment Simulator. GLoBES is a
sophisticated software package for the simulation of long baseline
neutrino oscillation experiments. ")
   (license license:gpl2+)))

(define-public python-jmespath-0.7.1
  (package
   (name "python-jmespath-0.7.1")
   (version "0.7.1")
   (source
    (origin
     (method url-fetch)
     (uri "https://github.com/jmespath/jmespath.py/archive/refs/tags/0.7.1.tar.gz")
     (sha256
      (base32
       "1yg99hqq56064365sj0k4dyiyyqxh2096h39c5pj37k2nf3ql5bx"))))
   (build-system python-build-system)
   (native-inputs
    (list python-nose))
   (synopsis "JSON Matching Expressions")
   (description "JMESPath (pronounced james path) is a Python
library that allows one to declaratively specify
how to extract elements from a JSON document.")
   (home-page "https://github.com/jmespath/jmespath.py")
   (license license:expat)))

(define-public python-botocore-local
  ;; Note: When updating botocore, also make sure that boto3 and awscli
  ;; are compatible.
  (package
   (name "python-botocore-local")
   (version "1.24.35")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "botocore" version))
     (sha256
      (base32
       "0rv8mvhq5s373zdjs2yb45hzvqcqdh2lp2rbb21jjc8ciwnl5d9n"))))
   (build-system python-build-system)
   (arguments
    ;; FIXME: Many tests are failing.
    '(#:tests? #f))
   (propagated-inputs
    (list python-dateutil
	  python-jmespath-0.7.1
	  python-urllib3))
   (home-page "https://github.com/boto/botocore")
   (synopsis "Low-level interface to AWS")
   (description "Botocore is a Python library that provides a low-level
interface to the Amazon Web Services (AWS) API.")
   (license license:asl2.0)))

(define-public python-boto3-1.21.13
  (package
   (name "python-boto3-1.21.13")
   (version "1.21.13")
   (home-page "https://github.com/boto/boto3")
   (source (origin
            (method url-fetch)
	    (uri "https://github.com/boto/boto3/archive/refs/tags/1.21.13.tar.gz")
            (sha256
             (base32
              "1ad6zmkphx59kgilc4c6ah6w9zyaxdkn5kdcy1x4dn95dvbwb81y"))))
   (arguments `(#:tests? #f))
   (build-system python-build-system)
   (native-inputs
    (list python-nose python-mock python-pytest))
   (propagated-inputs
    (list python-botocore-local
	  python-s3transfer))
   (inputs `(("python-jmespath-0.7.1" ,python-jmespath-0.7.1)
	     ))
   (synopsis "AWS SDK for Python")
   (description
    "Boto3 is a Python library for writing programs that interact with                       
@acronym{AWS,Amazon Web Services}.")
   (license license:asl2.0)))

(define-public python-prometheus-client-0.13.1
  (package
   (name "python-prometheus-client-0.13.1")
   (version "0.13.1")
   (source
    (origin
     (method url-fetch)
     (uri "https://github.com/prometheus/client_python/archive/refs/tags/v0.13.1.tar.gz")
     (sha256
      (base32
       "10515ihb6p1qpjxzf2f8f7w4abdqfiy7rl9il73gk52gfriqkmjj"))))
   (build-system python-build-system)
   (arguments '(#:tests? #f))
   (propagated-inputs (list python-twisted))
   (home-page "https://github.com/prometheus/client_python")
   (synopsis "Python client for the Prometheus monitoring system")
   (description
    "The prometheus_client package supports exposing
metrics from software written in Python, so that
they can be scraped by a Prometheus service.
Metrics can be exposed through a standalone web
server, or through Twisted, WSGI and the node
exporter textfile collector.")
   (license license:asl2.0)))

(define-public python-beaker-1.12.0
(package
   (name "python-beaker-1.12.0")
   (version "1.12.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://github.com/bbangert/beaker/archive/refs/tags/1.12.0.tar.gz")
     (sha256
      (base32
       "09kzimi2p8r18a9r7x1f302xfxnppf2h4pl2cj1nda41dsz1g1z8"))))
   (build-system python-build-system)
   (arguments '(#:tests? #f))
   (inputs
    `(("python-3.9" ,python-3.9)
      ))
   (home-page "https://github.com/bbangert/beaker")
   (synopsis "Python beaker")
   (description
    "Beaker is a web session and general caching
library that includes WSGI middleware for use in
web applications.")
   (license license:expat)))

(define-public python-pyjwkest-1.4.0
(package
   (name "python-pyjwkest-1.4.0")
   (version "1.4.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://github.com/IdentityPython/pyjwkest/archive/refs/tags/v1.4.0.tar.gz")
     (sha256
      (base32
       "086lprhhjhx4hi7f966m34izqrdjwxm4n2x54l2hbphvl87jlz6s"))))
   (build-system python-build-system)
   (arguments '(#:tests? #f))
   (inputs
    `(("python-3.9" ,python-3.9)
      ("python-future" ,python-future)
      ("python-six" ,python-six)
      ("python-requests" ,python-requests)
      ("python-pycryptodomex" ,python-pycryptodomex)
      ))
   (home-page "https://github.com/IdentityPython/pyjwkest")
   (synopsis "Python pyjwkest")
   (description
    "Implementation of JWT, JWS, JWE and JWK")
   (license license:asl2.0)))  

(define-public python-pyoidc-1.3.0
  (package
   (name "python-pyoidc-1.3.0")
   (version "1.3.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://github.com/OpenIDC/pyoidc/archive/refs/tags/1.3.0.tar.gz")
     (sha256
      (base32
       "10bdaiiisiha4sxg6irq9nwiy2h6si356bq4ml9pp888ajcyp9n8"))))
   (build-system python-build-system)
   (arguments '(#:tests? #f))
   (inputs
    `(("python-3.9" ,python-3.9)
      ("python-typing-extensions-4.4.0" ,python-typing-extensions-4.4.0)
      ("python-defusedxml" ,python-defusedxml)
      ("python-cryptography" ,python-cryptography)
      ("python-beaker-1.12.0" ,python-beaker-1.12.0)
      ("python-mako" ,python-mako)
      ("python-pyjwkest-1.4.0" ,python-pyjwkest-1.4.0)
      ("python-requests" ,python-requests)
      ("python-pycryptodomex" ,python-pycryptodomex)
      ("python-future" ,python-future)
      ))
   (home-page "https://github.com/OpenIDC/pyoidc")
   (synopsis "Python pyoidc")
   (description
    "This is a complete implementation of OpenID
Connect as specified in the OpenID Connect Core
specification. And as a side effect, a complete
implementation of OAuth2.0 too.")
   (license license:asl2.0)))

(define-public python-flask-2.0.3
  (package
    (name "python-flask-2.0.3")
    (version "2.0.3")
    (source (origin
              (method url-fetch)
              (uri "https://github.com/pallets/flask/archive/refs/tags/2.0.3.tar.gz")
              (sha256
               (base32
		"090wxkld1151yb2jf5b365qihk21v0sfx0ldn7n7m2d68z0gfhr9"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest" "-vv" "tests")))))))
    (native-inputs
     (list python-pytest))
    (propagated-inputs
     (list python-asgiref               ;async extra                                          
           python-click
           python-importlib-metadata
           python-itsdangerous
           python-jinja2
           python-werkzeug))
    (home-page "https://palletsprojects.com/p/flask/")
    (synopsis "Microframework based on Werkzeug, Jinja2 and good intentions")
    (description "Flask is a micro web framework based on the Werkzeug toolkit                
and Jinja2 template engine.  It is called a micro framework because it does not               
presume or force a developer to use a particular tool or library.")
    (license license:bsd-3)))

(define-public python-redis-4.1.4
  (package
    (name "python-redis-4.1.4")
    (version "4.1.4")
    (source
     (origin
       (method url-fetch)
       (uri "https://github.com/redis/redis-py/archive/refs/tags/v4.1.4.tar.gz")
       (sha256
        (base32
	 "1jw44i8ly74hddxgiqm6ldhq25wxs0h7yrz0rxw110h0j7c93xjv"))))
    (build-system python-build-system)
    ;; Tests require a running Redis server.
    (arguments '(#:tests? #f))
    (inputs
     `(("python-3.9" ,python-3.9)
       ;; packaging>=20.4
       ("python-packaging" ,python-packaging)
       ;; deprecated>=1.2.3
       ("python-deprecated" ,python-deprecated)
       ))
    ;; As long as we are not running test, we do not need this input :-)
    ;;(native-inputs
    ;; `(("python-pytest" ,python-pytest)))
    (home-page "https://github.com/andymccurdy/redis-py")
    (synopsis "Redis Python client")
    (description
     "This package provides a Python interface to the Redis key-value store.")
    (license license:expat)))

(define-public python-google-auth-2.6.0
  (package
   (name "python-google-auth-2.6.0")
   (version "2.6.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://github.com/googleapis/google-auth-library-python/archive/refs/tags/v2.6.0.tar.gz")
     (sha256
      (base32
       "1zgjjmsp5daavsqfwlfrnidmnjfpknb9nnfascgs41nm39hp0z17"))))
   (build-system python-build-system)
   (arguments
    `(#:tests? #f

      #:phases
      (modify-phases %standard-phases
		     (replace 'check
			      (lambda* (#:key tests? #:allow-other-keys)
				       (when tests?
					 (invoke "pytest")))))))
   (propagated-inputs
    (list python-cachetools
          python-cryptography
          python-pyasn1-modules
          python-rsa
          python-six))
   (native-inputs
    (list python-flask
          python-freezegun
          python-oauth2client
          python-pyopenssl
          python-pytest
          python-pytest-localserver
          python-pyu2f
          python-requests
          python-responses))
   (home-page "https://github.com/googleapis/google-auth-library-python")
   (synopsis "Google Authentication Library")
   (description "This library simplifies using Google's various
server-to-server authentication mechanisms to access Google APIs.")
   (license license:asl2.0)))

(define-public python-geoip2-4.5.0
  (package
   (name "python-geoip2-4.5.0")
   (version "4.5.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://github.com/maxmind/GeoIP2-python/archive/refs/tags/v4.5.0.tar.gz")
     (sha256
      (base32
       "0hy4s73w2ldy55h58dbardbswv4v81ng5xhb5j0vc7lnzhk61qdh"))))
   (build-system python-build-system)
   (arguments
    `(#:tests? #f ;; Tests require a copy of the maxmind database

      #:phases
      (modify-phases
       %standard-phases
       (add-before 'build ;; For python-build-system
		   'patches
		   (lambda*
		    (#:key inputs #:allow-other-keys)
		    (substitute*
		     "geoip2/__init__.py"
		     (("4.4.0") "4.5.0"))
		    )))))
   (inputs
    `(("python-3.9" ,python-3.9)
      ;; Requirement.parse('aiohttp<4.0.0,>=3.6.2')
      ("python-aiohttp" ,python-aiohttp)
      ;; maxminddb<3.0.0,>=2.2.0
      ("python-maxminddb" ,python-maxminddb)
      ;; urllib3<2.0.0,>=1.25.2
      ("python-urllib3" ,python-urllib3)
      ;; requests<3.0.0,>=2.24.0
      ("python-requests" ,python-requests)
      ))
   (home-page "https://www.maxmind.com/")
   (synopsis "MaxMind GeoIP2 API")
   (description "Provides an API for the GeoIP2 web services and databases.
The API also works with MaxMind’s free GeoLite2
databases.")
   (license license:asl2.0)))

(define-public python-stomp-6.1.1
  (package
   (name "python-stomp-6.1.1")
   (version "6.1.1")
   (source
    (origin
     (method url-fetch)
     (uri "https://github.com/jasonrbriggs/stomp.py/archive/refs/tags/v6.1.1.tar.gz")
     (sha256
      (base32
       "005vjc5nasp1z75mrvr3gl6yd9mld8q1mim6mjv0brq9ldnbwd67"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f

      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (add-before
	'build
	'patches-stomp
	(lambda*
	 _
	 (substitute*
	  "Makefile"
	  (("python") "python3")
	  (("poetry update") "# poetry update")
	  (("all: test install") "all: install")
	  (("install: updateversion test") "install: updateversion")
	  )
	 ))
       (add-before
	'build
	'home
	(lambda*
	 (#:key outputs #:allow-other-keys)
	 ;; To avoid this error:
	 ;; virtualenv: error: argument dest: the destination . is not write-able at /
	 (setenv
	  "HOME"
	  (assoc-ref outputs "out"))))

;; $ python
;; Python 3.9.9 (main, Jan  1 1970, 00:00:01) 
;; [GCC 10.3.0] on linux
;; Type "help", "copyright", "credits" or "license" for more information.
;; >>> import stomp
;; >>> print(stomp.__file__)
;; .../python3.9/site-packages/stomp/__init__.py

;; $ python3
;; Python 3.9.9 (main, Jan  1 1970, 00:00:01) 
;; [GCC 10.3.0] on linux
;; Type "help", "copyright", "credits" or "license" for more information.
;; >>> import stomp
;; >>> stomp.__file__
;; '.../.guix-profile/lib/python3.9/site-packages/stomp/__init__.py'
;; >>> stomp.__version__
;; (6, 1, 1)
       
       (add-after
	'install
	'install-stomp
	(lambda*
	 (#:key outputs #:allow-other-keys)
	 (let* ((DIR (assoc-ref outputs "out"))
		(suffix "/lib/python3.9/site-packages/stomp")
		(INSTALL (string-append DIR suffix)))

	   (substitute*
	    "stomp/__init__.py"
	    (("__version__ = \\(6, 1, 1\\)")
	     "__version__ = \"6.1.1\""))
	 
	   (mkdir-p INSTALL)
	   (copy-recursively "stomp" INSTALL))))
       
       )))
   (inputs
    `(("python-3.9" ,python-3.9)
      ("which" ,which)
      ("poetry" ,poetry)
      ;; docopt<0.7.0,>=0.6.2 
      ("python-docopt" ,python-docopt)
      ;; websocket-client<2.0.0,>=1.2.3
      ("python-websocket-client" ,python-websocket-client)
      ))
   (home-page "https://github.com/jasonrbriggs/stomp.py")
   (synopsis "Stomp")
   (description "stomp.py is a Python client library for accessing messaging
servers (such as ActiveMQ, Artemis or RabbitMQ) using the
STOMP protocol (STOMP v1.0, STOMP v1.1 and STOMP v1.2). It
can also be run as a standalone, command-line client for
testing. NOTE: Stomp.py has officially ended support for
Python2.x. See python3statement.org for more information.")
   (license license:asl2.0)))

(define-public python-pymemcache-3.5.2
  (package
   (name "python-pymemcache-3.5.2")
   (version "3.5.2")
   (source
    (origin
     (method url-fetch)
     (uri "https://github.com/pinterest/pymemcache/archive/refs/tags/v3.5.2.tar.gz")
     (sha256
      (base32
       "1rajzwp5grmq9yy1qinq7wzd3ilqnvq5sxkvi95agclb2m7pgzi9"))))
   (build-system python-build-system)
   (arguments
    `(#:tests? #f))
   (inputs
    `(("python-3.9" ,python-3.9)
      ("python-six" ,python-six)
      ))
   (home-page "https://github.com/pinterest/pymemcache")
   (synopsis "pymemcache")
   (description "A comprehensive, fast, pure-Python memcached client.")
   (license license:asl2.0)))

(define-public python-sqlalchemy-1.4.31
  (package
    (name "python-sqlalchemy-1.4.31")
    (version "1.4.31")
    (source
     (origin
      (method url-fetch)
      (uri "https://github.com/sqlalchemy/sqlalchemy/archive/refs/tags/rel_1_4_31.tar.gz")
      (sha256
       (base32 "0vn70i1frr2mi9x0xc4ymczhfi736gvmngrfm2snrd4fildvqfvd"))))
    (build-system python-build-system)
    (native-inputs
     (list python-cython ; for C extensions
           python-pytest python-mock python-pytest-xdist)) ; for tests
    (propagated-inputs
     (list python-greenlet))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "pytest" "-vv"
                        "-n" (number->string (parallel-job-count))
                        ;; The memory usage tests are very expensive and run in
                        ;; sequence; skip them.
                        "-k" "not test_memusage.py")))))))
    (home-page "https://www.sqlalchemy.org")
    (synopsis "Database abstraction library")
    (description
     "SQLAlchemy is the Python SQL toolkit and Object Relational Mapper that
gives application developers the full power and flexibility of SQL.  It
provides a full suite of well known enterprise-level persistence patterns,
designed for efficient and high-performing database access, adapted into a
simple and Pythonic domain language.")
    (license license:x11)))

(define-public python-magic-0.4.25
  (package
    (name "python-magic-0.4.25")
    (version "0.4.25")
    (home-page "https://github.com/ahupp/python-magic")
    (source
     (origin
       (method url-fetch)
       (uri "https://github.com/ahupp/python-magic/archive/refs/tags/0.4.25.tar.gz")
       (sha256
        (base32
         "0ab3wcfwxg1lmxp916ss7am44bqdhsv8ngqhd3rgyzq6jlwlh7qc"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  ;; Replace a specific method call with a hard-coded
                  ;; path to the necessary libmagic.so file in the
                  ;; store.  If we don't do this, then the method call
                  ;; will fail to find the libmagic.so file, which in
                  ;; turn will cause any application using
                  ;; python-magic to fail.
                  (add-before 'build 'hard-code-path-to-libmagic
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((magic (search-input-file inputs "/lib/libmagic.so")))
                        (substitute*
			 "magic/loader.py"
			 (("find_library\\('magic'\\)")
			  (string-append "'" magic "'")))
			)))
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      ;; The test suite mandates this variable.
                      (setenv "LC_ALL" "en_US.UTF-8")
                      (if tests?
                          (with-directory-excursion "test"
                            (invoke "python" "./test.py")
                            (invoke "python" "./libmagic_test.py"))
                          (format #t "test suite not run~%")))))))
    (native-inputs
     (list which))
    (inputs
     ;; python-magic needs to be able to find libmagic.so.
     ;; Use a newer version because 5.39 returns bogus for some archives
     ;; (notably Chromium .crx extensions), which breaks e.g. 'diffoscope'.
     (list file-next))
    (synopsis "File type identification using libmagic")
    (description
     "This module uses ctypes to access the libmagic file type
identification library.  It makes use of the local magic database and
supports both textual and MIME-type output.  Note that this module and
the python-file module both provide a \"magic.py\" file; these two
modules, which are different and were developed separately, both serve
the same purpose: to provide Python bindings for libmagic.")
    (license license:expat)))

(define-public python-argcomplete-1.12.3
  (package
   (name "python-argcomplete-1.12.3")
   (version "1.12.3")
   (source
    (origin
     (method url-fetch)
     (uri "https://github.com/kislyuk/argcomplete/archive/refs/tags/v1.12.3.tar.gz")
     (sha256
      (base32
       "104n9hkc32zbc2cdfdhh9c6l15fl2vdsfkl8iv4qw7ip15m52jvm"))
     ;; (patches (search-patches "python-argcomplete-1.11.1-fish31.patch"))
     ))
   (build-system python-build-system)
   (native-inputs
    (list python-coverage
          python-flake8
          python-pexpect
          python-wheel
          tcsh
          fish
          bash))       ;full Bash for 'test_file_completion'
   (home-page "https://github.com/kislyuk/argcomplete")
   (synopsis "Shell tab completion for Python argparse")
   (description "argcomplete provides extensible command line tab completion
     of arguments and options for Python scripts using @code{argparse}.  It's
     particularly useful for programs with many options or sub-parsers that can
     dynamically suggest completions ; for example, when browsing resources over the
     network.")
   (license license:asl2.0)))

(define-public python-urllib3-1.26.8
  (package
   (name "python-urllib3-1.26.8")
   (version "1.26.8")
   (source
    (origin
     (method url-fetch)
     (uri "https://github.com/urllib3/urllib3/archive/refs/tags/1.26.8.tar.gz")
     (sha256
      (base32
       "1m4bspbls4pvhqidsacbllgpa4jd6va2li044k3hkslmx7jbbjx5"))))
   (build-system python-build-system)
   (arguments `(#:tests? #f))
   (propagated-inputs
    (list ;; These 5 inputs are used to build urrlib3[secure]
     python-certifi
     python-cryptography
     python-idna
     python-pyopenssl
     python-pysocks))
   (home-page "https://urllib3.readthedocs.io/")
   (synopsis "HTTP library with thread-safe connection pooling")
   (description
    "Urllib3 supports features left out of urllib and urllib2 libraries.  It
can reuse the same socket connection for multiple requests, it can POST files,
supports url redirection and retries, and also gzip and deflate decoding.")
   (license license:expat)))

(define-public python-charset-normalizer-2.0.0
  (package
   (name "python-charset-normalizer-2.0.0")
   (version "2.0.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://github.com/Ousret/charset_normalizer/archive/refs/tags/2.0.0.tar.gz")
     (sha256
      (base32 "1clizvyb2rps3l02d4g5brsg5bzirqx0fa98arjs43sj0bpaj134"))))
   (build-system python-build-system)
   (arguments
    (list #:phases
          #~(modify-phases %standard-phases
			   ;; This package provides a 'normalizer' executable that only
			   ;; depends on Python, so customize the wrap phase to avoid
			   ;; adding pytest and friends in order to save size.
			   ;; (See also <https://bugs.gnu.org/25235>.)
			   (replace 'wrap
				    (lambda* (#:key inputs outputs #:allow-other-keys)
					     (let* ((sitedir (site-packages inputs outputs))
						    (python (dirname (dirname
								      (search-input-file
								       inputs "bin/python"))))
						    (python-sitedir
						     (string-append python "/lib/python"
								    (python-version python)
								    "/site-packages")))
					       (wrap-program (string-append #$output "/bin/normalizer")
							     `("GUIX_PYTHONPATH" ":" suffix
							       ,(list sitedir python-sitedir)))))))))
   (native-inputs
    (list python-pytest))
   (home-page "https://github.com/ousret/charset_normalizer")
   (synopsis "Universal Charset Detector, alternative to Chardet")
   (description "This library helps you read text from an unknown charset
encoding.  Motivated by @code{chardet}, it tries to resolve the issue by
taking a new approach.  All IANA character set names for which the Python core
library provides codecs are supported.")
   (license license:expat)))

(define-public python-requests-2.27.1
  (package
   (name "python-requests-2.27.1")
   (version "2.27.1")
   (source (origin
            (method url-fetch)
            (uri "https://github.com/psf/requests/archive/refs/tags/v2.27.1.tar.gz")
            (sha256
             (base32
              "16a2prri2wrhiq87mh64kkx720jh1vna9c0ds1vgc56y78m5k45j"))))
   (build-system python-build-system)
   (propagated-inputs
    (list python-certifi
          python-charset-normalizer-2.0.0
          python-idna
          python-urllib3))
   (arguments
    ;; FIXME: Some tests require network access.
    '(#:tests? #f))
   (home-page "http://python-requests.org/")
   (synopsis "Python HTTP library")
   (description
    "Requests is a Python HTTP client library.  It aims to be easier to use
than Python’s urllib2 library.")
   (license license:asl2.0)))

(define-public python-dogpile.cache-1.1.5
  (package
   (name "python-dogpile.cache-1.1.5")
   (version "1.1.5")
   (source (origin
            (method url-fetch)
            (uri "https://github.com/sqlalchemy/dogpile.cache/archive/refs/tags/rel_1_1_5.tar.gz")
            (sha256
             (base32
              "1jiqsz3s2a5q1j499pr6cz23d18anqsnvd5i7qjkf46hjx11r2ah"))))
   (build-system python-build-system)
   (arguments
    '(#:phases
      (modify-phases %standard-phases
		     (replace 'check
			      (lambda* (#:key tests? #:allow-other-keys)
				       (when tests?
					 (invoke "pytest")))))))
   (native-inputs (list python-mako python-pytest))
   (propagated-inputs (list python-decorator python-stevedore))
   (home-page "https://github.com/sqlalchemy/dogpile.cache")
   (synopsis "Caching front-end based on the Dogpile lock")
   (description "@code{dogpile.cache} is a caching API which provides a
generic interface to caching backends of any variety, and additionally
provides API hooks which integrate these cache backends with the locking
mechanism of @code{dogpile}.")
   (license license:expat)))

(define-public python-jsonschema-3.2.0
  (package
   (name "python-jsonschema-3.2.0")
   (version "3.2.0")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "jsonschema" version))
            (sha256
             (base32
              "0ykr61yiiizgvm3bzipa3l73rvj49wmrybbfwhvpgk3pscl5pa68"))))
   (build-system python-build-system)
   (arguments
    '(#:phases
      (modify-phases %standard-phases
		     (replace 'check
			      (lambda* (#:key inputs outputs tests? #:allow-other-keys)
				       (when tests?
					 (setenv "JSON_SCHEMA_TEST_SUITE" "json")
					 (invoke "trial" "jsonschema")))))))
   (native-inputs
    `(("python-setuptools_scm" ,python-setuptools-scm)
      ("python-twisted" ,python-twisted)))
   (propagated-inputs
    (list python-attrs python-pyrsistent python-six))
   (home-page "https://github.com/Julian/jsonschema")
   (synopsis "Implementation of JSON Schema for Python")
   (description
    "Jsonschema is an implementation of JSON Schema for Python.")
   (license license:expat)))

(define-public python-paramiko-2.11.0
  (package
   (name "python-paramiko-2.11.0")
   (version "2.11.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://github.com/paramiko/paramiko/archive/refs/tags/2.11.0.tar.gz")
     (sha256
      (base32 "0vdq40qddm739hf8r7v02as1xws7hkvlw8zvry06x6yrf0z6vkxw"))))
   (build-system python-build-system)
   (arguments
    `( ;; FIXME: Tests require many unpackaged libraries, see dev-requirements.txt.
      #:tests? #f))
   (propagated-inputs
    (list python-bcrypt python-pyasn1 python-pynacl python-cryptography))
   (home-page "https://www.paramiko.org/")
   (synopsis "SSHv2 protocol library")
   (description "Paramiko is a python implementation of the SSHv2 protocol,
providing both client and server functionality.  While it leverages a Python C
extension for low level cryptography (PyCrypto), Paramiko itself is a pure
Python interface around SSH networking concepts.")
   (license license:lgpl2.1+)))

(define-public python-alembic-1.7.6
  (package
    (name "python-alembic-1.7.6")
    (version "1.7.6")
    (source
     (origin
       (method url-fetch)
       (uri "https://github.com/sqlalchemy/alembic/archive/refs/tags/rel_1_7_6.tar.gz")
       (sha256
        (base32 "169wb8yl4svznnrj9dwc8fnmaa6x1cc9vlflssy6imhy35hf5vww"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda _
                      (invoke "pytest" "-vv"))))))
    (native-inputs
     (list python-mock python-pytest-cov))
    (propagated-inputs
     (list python-dateutil python-sqlalchemy python-mako python-editor))
    (home-page "https://bitbucket.org/zzzeek/alembic")
    (synopsis "Database migration tool for SQLAlchemy")
    (description
     "Alembic is a lightweight database migration tool for usage with the                      
SQLAlchemy Database Toolkit for Python.")
    (license license:expat)))

(define-public rucio-1.30.0
  (package
   (name "rucio-1.30.0")
   (version "1.30.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://github.com/rucio/rucio/archive/refs/tags/1.30.0.tar.gz")
     (sha256
      (base32 "1pwxm5flqqcin6xsh9v07drbjjdn28gi7m0j7m5z900cnxryi1bm"))))
      (build-system python-build-system)
   (propagated-inputs
    `(("python-urllib3" ,python-urllib3-1.26.8)
      ("python-charset-normalizer-2.0.0" ,python-charset-normalizer-2.0.0)
      ))
   (inputs
    `(("python-3.9" ,python-3.9)
      ("bash" ,bash)
      ("python-boto3" ,python-boto3-1.21.13) ;;
      ("python-prometheus-client-0.13.1" ,python-prometheus-client-0.13.1)
      ("python-pyoidc-1.3.0" ,python-pyoidc-1.3.0)
      ("python-flask-2.0.3" ,python-flask-2.0.3)
      ("python-redis-4.1.4" ,python-redis-4.1.4)
      ("python-google-auth-2.6.0" ,python-google-auth-2.6.0)
      ("python-typing-extensions-4.4.0" ,python-typing-extensions-4.4.0)
      ("python-defusedxml" ,python-defusedxml)
      ("python-beaker-1.12.0" ,python-beaker-1.12.0)
      ("python-pyjwkest-1.4.0" ,python-pyjwkest-1.4.0)
      ("python-pycryptodomex" ,python-pycryptodomex)
      ("python-future" ,python-future)

      ;; Requirement.parse('aiohttp<4.0.0,>=3.6.2')
      ("python-aiohttp" ,python-aiohttp)
      ;; maxminddb<3.0.0,>=2.2.0
      ("python-maxminddb" ,python-maxminddb)
      ;; deprecated>=1.2.3
      ("python-deprecated" ,python-deprecated)
      ;; packaging>=20.4
      ("python-packaging" ,python-packaging)
      ;; geoip2==4.5.0
      ("python-geoip2-4.5.0" ,python-geoip2-4.5.0)
      ;; statsd==3.3.0
      ("python-statsd" ,python-statsd)
      ;; stomp.py==6.1.1
      ("python-stomp-6.1.1" ,python-stomp-6.1.1)
      ;; pymemcache==3.5.2
      ("python-pymemcache-3.5.2" ,python-pymemcache-3.5.2)
      ;; alembic~=1.7.6
      ("python-alembic-1.7.6" ,python-alembic-1.7.6)
      ;; SQLAlchemy==1.4.31
      ("python-sqlalchemy-1.4.31" ,python-sqlalchemy-1.4.31)
      ;; python-magic~=0.4.25
      ("python-magic-0.4.25" ,python-magic-0.4.25)
      ;; argcomplete~=1.12.3
      ("python-argcomplete-1.12.3" ,python-argcomplete-1.12.3)
      ;; paramiko~=2.11.0
      ("python-paramiko-2.11.0" ,python-paramiko-2.11.0)
      ;; jsonschema~=3.2.0
      ("python-jsonschema-3.2.0" ,python-jsonschema-3.2.0)
      ;; tabulate~=0.8.0
      ("python-tabulate" ,python-tabulate)
      ;; dogpile.cache<=1.1.5,>=1.1.2
      ("python-dogpile.cache" ,python-dogpile.cache-1.1.5) 
      ;; urllib3<=1.26.8,>=1.24.2
      ("python-urllib3" ,python-urllib3-1.26.8)
      ;; requests<=2.27.1,>=2.20.0
      ("python-requests-2.27.1" ,python-requests-2.27.1)
      ;; charset_normalizer~=2.0.0
      ("python-charset-normalizer-2.0.0" ,python-charset-normalizer-2.0.0)
      ))
   (arguments
    `(#:tests? #f

      #:phases
      (modify-phases
       %standard-phases
       (add-before 'build ;; For python-build-system
		   'patches
		   (lambda*
		    (#:key inputs #:allow-other-keys)
		    (let ((BASH_DIR (assoc-ref inputs "bash")))
		    (substitute*
		     '("setuputil.py"
		       "tools/generate_version.py"
		       "tools/prepare-commit-msg"
		       "tools/test/oracle_setup.sh"
		       "tools/test/run_tests.py"
		       "tools/add_header")
		     (("/bin/sh")
                      (string-append BASH_DIR "/bin/sh")))
		    (substitute*
		     "requirements.txt"
		     ;; DistributionNotFound(Requirement.parse('stomp.py==6.1.1'), {'rucio'})
		     (("stomp.py==6.1.1") "# stomp.py==6.1.1")
		     ;; ContextualVersionConflict(alembic 1.7.6.dev0 ...)
		     (("alembic~=1.7.6") "# alembic~=1.7.6")
		     ;; ContextualVersionConflict(SQLAlchemy 1.4.31.dev0
		     (("SQLAlchemy==1.4.31") "# SQLAlchemy==1.4.31")
		     )))))))
   
   (home-page "http://rucio.cern.ch/")
   (synopsis "Rucio")
   (description "Rucio is a software framework that provides functionality
to organize, manage, and access large volumes of scientific
data using customisable policies. The data can be spread
across globally distributed locations and across
heterogeneous data centers, uniting different storage and
network technologies as a single federated entity. Rucio
offers advanced features such as distributed data recovery
or adaptive replication, and is highly scalable, modular,
and extensible. Rucio has been originally developed to meet
the requirements of the high-energy physics experiment
ATLAS, and is continuously extended to support LHC
experiments and other diverse scientific communities.")
   (license license:asl2.0)))

;; Problem: rucio-webui has no tags yet
(define-public rucio-1.30.1
  (package
   (name "rucio-1.30.1")
   (version "1.30.1")
   (source
    (origin
     (method url-fetch)
     (uri "https://github.com/rucio/rucio/archive/refs/tags/1.30.1.tar.gz")
     (sha256
      (base32 "1iw6njbnwjs7chqr25mylnxi5qkx35wdj8z7b90g1j5b8a884k6a"))))
      (build-system python-build-system)
   (inputs
    `(("python-3.9" ,python-3.9)
      ("bash" ,bash)
      ("python-boto3" ,python-boto3-1.21.13) ;;
      ("python-prometheus-client-0.13.1" ,python-prometheus-client-0.13.1)
      ("python-pyoidc-1.3.0" ,python-pyoidc-1.3.0)
      ("python-flask-2.0.3" ,python-flask-2.0.3)
      ("python-redis-4.1.4" ,python-redis-4.1.4)
      ("python-google-auth-2.6.0" ,python-google-auth-2.6.0)
      ("python-typing-extensions-4.4.0" ,python-typing-extensions-4.4.0)
      ("python-defusedxml" ,python-defusedxml)
      ("python-beaker-1.12.0" ,python-beaker-1.12.0)
      ("python-pyjwkest-1.4.0" ,python-pyjwkest-1.4.0)
      ("python-pycryptodomex" ,python-pycryptodomex)
      ("python-future" ,python-future)

      ;; Requirement.parse('aiohttp<4.0.0,>=3.6.2')
      ("python-aiohttp" ,python-aiohttp)
      ;; maxminddb<3.0.0,>=2.2.0
      ("python-maxminddb" ,python-maxminddb)
      ;; deprecated>=1.2.3
      ("python-deprecated" ,python-deprecated)
      ;; packaging>=20.4
      ("python-packaging" ,python-packaging)
      ;; geoip2==4.5.0
      ("python-geoip2-4.5.0" ,python-geoip2-4.5.0)
      ;; statsd==3.3.0
      ("python-statsd" ,python-statsd)
      ;; stomp.py==6.1.1
      ("python-stomp-6.1.1" ,python-stomp-6.1.1)
      ;; pymemcache==3.5.2
      ("python-pymemcache-3.5.2" ,python-pymemcache-3.5.2)
      ;; alembic~=1.7.6
      ("python-alembic-1.7.6" ,python-alembic-1.7.6)
      ;; SQLAlchemy==1.4.31
      ("python-sqlalchemy-1.4.31" ,python-sqlalchemy-1.4.31)
      ;; python-magic~=0.4.25
      ("python-magic-0.4.25" ,python-magic-0.4.25)
      ;; argcomplete~=1.12.3
      ("python-argcomplete-1.12.3" ,python-argcomplete-1.12.3)
      ;; paramiko~=2.11.0
      ("python-paramiko-2.11.0" ,python-paramiko-2.11.0)
      ;; jsonschema~=3.2.0
      ("python-jsonschema-3.2.0" ,python-jsonschema-3.2.0)
      ;; tabulate~=0.8.0
      ("python-tabulate" ,python-tabulate)
      ;; dogpile.cache<=1.1.5,>=1.1.2
      ("python-dogpile.cache" ,python-dogpile.cache-1.1.5) 
      ;; urllib3<=1.26.8,>=1.24.2
      ("python-urllib3" ,python-urllib3-1.26.8)
      ;; requests<=2.27.1,>=2.20.0
      ("python-requests-2.27.1" ,python-requests-2.27.1)
      ;; charset_normalizer~=2.0.0
      ("python-charset-normalizer-2.0.0" ,python-charset-normalizer-2.0.0)
      ))
   (arguments
    `(#:tests? #f

      #:phases
      (modify-phases
       %standard-phases
       (add-before 'build ;; For python-build-system
		   'patches
		   (lambda*
		    (#:key inputs #:allow-other-keys)
		    (let ((BASH_DIR (assoc-ref inputs "bash")))
		    (substitute*
		     '("setuputil.py"
		       "tools/generate_version.py"
		       "tools/prepare-commit-msg"
		       "tools/test/oracle_setup.sh"
		       "tools/test/run_tests.py"
		       "tools/add_header")
		     (("/bin/sh")
                      (string-append BASH_DIR "/bin/sh")))
		    (substitute*
		     "requirements.txt"
		     ;; DistributionNotFound(Requirement.parse('stomp.py==6.1.1'), {'rucio'})
		     (("stomp.py==6.1.1") "# stomp.py==6.1.1")
		     ;; ContextualVersionConflict(alembic 1.7.6.dev0 ...)
		     (("alembic~=1.7.6") "# alembic~=1.7.6")
		     ;; ContextualVersionConflict(SQLAlchemy 1.4.31.dev0
		     (("SQLAlchemy==1.4.31") "# SQLAlchemy==1.4.31")
		     )))))))
   
   (home-page "http://rucio.cern.ch/")
   (synopsis "Rucio")
   (description "Rucio is a software framework that provides functionality
to organize, manage, and access large volumes of scientific
data using customisable policies. The data can be spread
across globally distributed locations and across
heterogeneous data centers, uniting different storage and
network technologies as a single federated entity. Rucio
offers advanced features such as distributed data recovery
or adaptive replication, and is highly scalable, modular,
and extensible. Rucio has been originally developed to meet
the requirements of the high-energy physics experiment
ATLAS, and is continuously extended to support LHC
experiments and other diverse scientific communities.")
   (license license:asl2.0)))
  
  


;; ---------------------------------------- ;; 

(define-public dcap-2.47.12 ;; Ok
  (package
   (name "dcap-2.47.12")
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
   (name "davix-0.6.4")
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
      ("openssl" ,openssl-1.1)
      ("libuuid" ,util-linux+udev)
      ("boost" ,boost)
      ))
   ;; (arguments ...)
   (home-page "https://davix.web.cern.ch/")
   (synopsis "Davix")
   (description "The davix project aims to make file management over HTTP-based protocols simple.")
   (license license:lgpl2.1+)))

;; https://github.com/cern-fts/davix/archive/refs/tags/R_0_8_3.tar.gz
(define-public davix-0.8.3
  (package
   (name "davix-0.8.3")
   (version "0.8.3")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/cern-fts/davix")
           (commit (string-append "R_" (string-replace-substring version "." "_")))
           (recursive? #t))) ;; submodules
     (sha256
      (base32
       "1q17063rjw1yl8fxvwk1kvzd1i5m37jfks4hwal5mr62b9v2fv64"))))
   (build-system cmake-build-system)
   (inputs
    `(("coreutils" ,coreutils)
      ("gcc-toolchain" ,gcc-toolchain-8)
      ("python" ,python-2.7)
      ("libxml2" ,libxml2)
      ("openssl" ,openssl-1.1)
      ("libuuid" ,util-linux+udev)
      ("boost" ,boost)
      ("util-linux" ,util-linux "lib") ;; Pour #include <uuid/uuid.h>
      ("curl" ,curl)
      ;; ("googletest" ,googletest)
      ))
   (arguments
    `(#:phases
      (modify-phases
       %standard-phases
       (add-before
	'configure
      	'patch
      	(lambda args
          '(substitute*
	    "src/fileops/AzureIO.cpp"
	    (("#include <uuid/uuid.h>") "#include <linux/uuid.h>")
	    (("uuid_t uuid") "guid_t uuid")
	    (("uuid\\[i\\]") "uuid.b[i]") 
      	    ))))

      #:configure-flags
      (list
       "-DEMBEDDED_LIBCURL=FALSE"
       "-DLIBCURL_BACKEND_BY_DEFAULT=TRUE"
       )))
      
   (home-page "https://davix.web.cern.ch/")
   (synopsis "Davix")
   (description "The davix project aims to make file management over HTTP-based protocols simple.")
   (license license:lgpl2.1+)))


;; Failed
;; In file included from /tmp/guix-build-llvm-5.0.2.drv-0/llvm-5.0.2.src/tools/lli/lli.cpp:30:
;; /tmp/guix-build-llvm-5.0.2.drv-0/llvm-5.0.2.src/include/llvm/ExecutionEngine/Orc/OrcRemoteTarg
;; etClient.h: In member function <E2><80><98>llvm::Expected<std::vector<char> > llvm::orc::remot
;; e::OrcRemoteTargetClient<ChannelT>::readMem(char*, llvm::JITTargetAddress, uint64_t)<E2><80>
;; <99>:
;; /tmp/guix-build-llvm-5.0.2.drv-0/llvm-5.0.2.src/include/llvm/ExecutionEngine/Orc/OrcRemoteTarg
;; etClient.h:722:26: error: could not convert <E2><80><98>((llvm::orc::remote::OrcRemoteTargetCl
;; ient<ChannelT>*)this)->callB<llvm::orc::remote::OrcRemoteTargetRPCAPI::ReadMem>(Src, Size)<E2>
;; <80><99> from <E2><80><98>Expected<vector<unsigned char,allocator<unsigned char>>><E2><80><99>
;;  to <E2><80><98>Expected<vector<char,allocator<char>>><E2><80><99>
;;   722 |     return callB<ReadMem>(Src, Size);
;;       |            ~~~~~~~~~~~~~~^~~~~~~~~~~
;;       |                          |
;;       |                          Expected<vector<unsigned char,allocator<unsigned char>>>


(define-public llvm-5
  (package
   (inherit llvm-7)
   (name "llvm-5")
   (version "5.0.2")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://releases.llvm.org/"
			 version "/llvm-"
			 version ".src.tar.xz"))
     (sha256
      (base32
       "0g1bbj2n6xv4p1n6hh17vj3vpvg56wacipc81dgwga9mg2lys8nm"))
     (modules '((guix build utils)))
     (snippet '(substitute* ;; https://bugs.gentoo.org/655140
		"include/llvm/ExecutionEngine/Orc/OrcRemoteTargetClient.h"
		(("Expected<std::vector<char>> readMem")
		 "Expected<std::vector<uint8_t>> readMem")
		))
     ))))

(define %llvm-release-monitoring-url
  "https://github.com/llvm/llvm-project/releases")

(define (llvm-uri component version)
  ;; LLVM release candidate file names are formatted 'tool-A.B.C-rcN/tool-A.B.CrcN.src.tar.xz'
  ;; so we specify the version as A.B.C-rcN and delete the hyphen when referencing the file name.
  (string-append "https://github.com/llvm/llvm-project/releases/download"
                 "/llvmorg-" version "/" component "-" (string-delete #\- version) ".src.tar.xz"))

(define-public llvm-9.0.1
  (package
   (inherit llvm-10)
   (name "llvm-9.0.1")
   (version "9.0.1")
   (source
    (origin
     (method url-fetch)
     (uri (llvm-uri "llvm" version))
     (sha256
      (base32
       "16hwp3qa54c3a3v7h8nlw0fh5criqh0hlr1skybyk0cz70gyx880"))
     (modules '((guix build utils)))
     (snippet '(substitute*
		;; https://github.com/root-project/cling/issues/438
		;; Error:
		;; llvm::orc::LegacyRTDyldObjectLinkingLayer::LinkedObjects is private within this context
		"include/llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
		(("std::map<VModuleKey, std::unique_ptr<LinkedObject>> LinkedObjects;")
		 "public:\n std::map<VModuleKey, std::unique_ptr<LinkedObject>> LinkedObjects;\n  private:\n")))
		;; A verifier dans le code si le substitute* a bien marche
     (patches (search-patches
               "llvm-9-fix-bitcast-miscompilation.patch"
               "llvm-9-fix-scev-miscompilation.patch"
               "llvm-9-fix-lpad-miscompilation.patch"))))
   (arguments
    (if (target-riscv64?)
	(substitute-keyword-arguments (package-arguments llvm-10)
				      ((#:phases phases)
				       `(modify-phases ,phases
						       (add-after 'unpack 'patch-dsymutil-link
								  (lambda _
								    (substitute* "tools/dsymutil/CMakeLists.txt"
										 (("endif\\(APPLE\\)")
										  (string-append
										   "endif(APPLE)\n\n"
										   "if (CMAKE_HOST_SYSTEM_PROCESSOR MATCHES \"riscv64\")\n"
										   "  target_link_libraries(dsymutil PRIVATE atomic)\n"
										   "endif()"))))))))
	(package-arguments llvm-10)))))

(define* (clang-runtime-from-llvm llvm
                                  #:optional
                                  hash
                                  (patches '()))
  (package
   (name "clang-runtime")
   (version (package-version llvm))
   (source
    (if hash
        (origin
         (method url-fetch)
         (uri (llvm-uri "compiler-rt" version))
         (sha256 (base32 hash))
         (patches (map search-patch patches)))
        (llvm-monorepo (package-version llvm))))
   (build-system cmake-build-system)
   (native-inputs (package-native-inputs llvm))
   (inputs
    (list llvm))
   (arguments
    `( ;; Don't use '-g' during the build to save space.
      #:build-type "Release"
      #:tests? #f                      ; Tests require gtest
      #:modules ((srfi srfi-1)
                 (ice-9 match)
                 ,@%cmake-build-system-modules)
      #:phases (modify-phases (@ (guix build cmake-build-system) %standard-phases)
			      (add-after 'set-paths 'hide-glibc
					 ;; Work around https://issues.guix.info/issue/36882.  We need to
					 ;; remove glibc from CPLUS_INCLUDE_PATH so that the one hardcoded
					 ;; in GCC, at the bottom of GCC include search-path is used.
					 (lambda* (#:key inputs #:allow-other-keys)
						  (let* ((filters '("libc"))
							 (input-directories
							  (filter-map (lambda (input)
									(match input
									       ((name . dir)
										(and (not (member name filters))
										     dir))))
								      inputs)))
						    (set-path-environment-variable "CPLUS_INCLUDE_PATH"
										   '("include")
										   input-directories)
						    #t))))))
   (home-page "https://compiler-rt.llvm.org")
   (synopsis "Runtime library for Clang/LLVM")
   (description
    "The \"clang-runtime\" library provides the implementations of run-time
functions for C and C++ programs.  It also provides header files that allow C
and C++ source code to interface with the \"sanitization\" passes of the clang
compiler.  In LLVM this library is called \"compiler-rt\".")
   (license (package-license llvm))
   (properties `((release-monitoring-url . ,%llvm-release-monitoring-url)
                 (upstream-name . "compiler-rt")))

   ;; <https://compiler-rt.llvm.org/> doesn't list MIPS as supported.
   (supported-systems (delete "mips64el-linux" %supported-systems))))

(define (clang-properties version)
  "Return package properties for Clang VERSION."
  `((compiler-cpu-architectures
     ("x86_64"
      ;; This list was obtained by running:
      ;;
      ;;   guix shell clang -- llc -march=x86-64 -mattr=help
      ;;
      ;; filtered from uninteresting entries such as "i686" and "pentium".
      ,@(if (version>=? version "10.0")	;TODO: refine
            '("atom"
              "barcelona"
              "bdver1"
              "bdver2"
              "bdver3"
              "bdver4"
              "bonnell"
              "broadwell"
              "btver1"
              "btver2"
              "c3"
              "c3-2"
              "cannonlake"
              "cascadelake"
              "cooperlake"
              "core-avx-i"
              "core-avx2"
              "core2"
              "corei7"
              "corei7-avx"
              "generic"
              "geode"
              "goldmont"
              "goldmont-plus"
              "haswell"
              "icelake-client"
              "icelake-server"
              "ivybridge"
              "k8"
              "k8-sse3"
              "knl"
              "knm"
              "lakemont"
              "nehalem"
              "nocona"
              "opteron"
              "opteron-sse3"
              "sandybridge"
              "silvermont"
              "skx"
              "skylake"
              "skylake-avx512"
              "slm"
              "tigerlake"
              "tremont"
              "westmere"
              "x86-64"
              "x86-64-v2"
              "x86-64-v3"
              "x86-64-v4"
              "znver1"
              "znver2"
              "znver3")
            '())))))

(define* (clang-from-llvm llvm clang-runtime
                          #:optional hash
                          #:key (patches '()) tools-extra
                          (properties
                           (append `((release-monitoring-url
                                      . ,%llvm-release-monitoring-url))
                                   (clang-properties (package-version llvm))))
                          (legacy-build-shared-libs? #f))
  "Produce Clang with dependencies on LLVM and CLANG-RUNTIME, and applying the
given PATCHES.  When TOOLS-EXTRA is given, it must point to the
'clang-tools-extra' tarball, which contains code for 'clang-tidy', 'pp-trace',
'modularize', and other tools.  LEGACY-BUILD-SHARED-LIBS? is used to configure
the package to use the legacy BUILD_SHARED_LIBS CMake option, which was used
until LLVM/Clang 14."
  (package
   (name "clang")
   (version (package-version llvm))
   (source
    (if hash
        (origin
         (method url-fetch)
         (uri (llvm-uri (if (version>=? version "9.0.1")
                            "clang"
                            "cfe")
                        version))
         (sha256 (base32 hash))
         (patches (map search-patch patches)))
        (llvm-monorepo (package-version llvm))))
   ;; Using cmake allows us to treat llvm as an external library.  There
   ;; doesn't seem to be any way to do this with clang's autotools-based
   ;; build system.
   (build-system cmake-build-system)
   (native-inputs (package-native-inputs llvm))
   (inputs
    `(("libxml2" ,libxml2)
      ("gcc-lib" ,gcc "lib")
      ("clang-runtime" ,clang-runtime) ;; Added to avoid error (assoc-ref inputs "clang-runtime") = #f
      ,@(package-inputs llvm)
      ,@(if tools-extra
            `(("clang-tools-extra" ,tools-extra))
            '())))
   (propagated-inputs
    (list llvm clang-runtime))
   (arguments
    `(#:configure-flags
      (list "-DCLANG_INCLUDE_TESTS=True"

            ;; Find libgcc_s, crtbegin.o, and crtend.o.
            (string-append "-DGCC_INSTALL_PREFIX="
                           (assoc-ref %build-inputs "gcc-lib"))

            ;; Use a sane default include directory.
            (string-append "-DC_INCLUDE_DIRS="
                           (assoc-ref %build-inputs "libc")
                           "/include")
            ,@(if (target-riscv64?)
                  (list "-DLIBOMP_LIBFLAGS=-latomic"
                        "-DCMAKE_SHARED_LINKER_FLAGS=-latomic")
                  `())
            ,@(if legacy-build-shared-libs?
                  '()
                  (list "-DCLANG_LINK_CLANG_DYLIB=ON")))

      ,@(if (target-riscv64?)
            `(#:make-flags '("LDFLAGS=-latomic"))
            '())

      ;; Don't use '-g' during the build to save space.
      #:build-type "Release"

      #:phases (modify-phases %standard-phases
			      ,@(if tools-extra
				    `((add-after 'unpack 'add-tools-extra
						 (lambda* (#:key inputs #:allow-other-keys)
							  ;; Unpack the 'clang-tools-extra' tarball under
							  ;; tools/.
							  (let ((extra (assoc-ref inputs
										  "clang-tools-extra")))
							    (invoke "tar" "xf" extra)
							    (rename-file ,(string-append
									   "clang-tools-extra-"
									   (string-delete #\- (package-version llvm))
									   ".src")
									 "tools/extra")
							    ,@(if legacy-build-shared-libs?
								  ;; Build and link to shared libraries.
								  '((substitute* "cmake/modules/AddClang.cmake"
										 (("BUILD_SHARED_LIBS") "True")))
								  '())
							    #t))))
				    '())
			      (add-after 'unpack 'add-missing-triplets
					 (lambda _
					   ;; Clang iterates through known triplets to search for
					   ;; GCC's headers, but does not recognize some of the
					   ;; triplets that are used in Guix.
					   (substitute* ,@(if (version>=? version "6.0")
							      '("lib/Driver/ToolChains/Gnu.cpp")
							      '("lib/Driver/ToolChains.cpp"))
							(("\"aarch64-linux-gnu\"," all)
							 (string-append "\"aarch64-unknown-linux-gnu\", "
									all))
							(("\"arm-linux-gnueabihf\"," all)
							 (string-append all
									" \"arm-unknown-linux-gnueabihf\","))
							(("\"i686-pc-linux-gnu\"," all)
							 (string-append "\"i686-unknown-linux-gnu\", "
									all)))
					   #t))
			      (add-after 'unpack 'set-glibc-file-names
					 (lambda* (#:key inputs #:allow-other-keys)
						  (let ((libc (assoc-ref inputs "libc"))
							(compiler-rt (assoc-ref inputs "clang-runtime"))
							(gcc (assoc-ref inputs "gcc")))
						    ,@(cond
						       ((version>=? version "6.0")
							`( ;; Link to libclang_rt files from clang-runtime.
							  (substitute* "lib/Driver/ToolChain.cpp"
								       (("getDriver\\(\\)\\.ResourceDir")
									(string-append "\"" compiler-rt "\"")))

							  ;; Make "LibDir" refer to <glibc>/lib so that it
							  ;; uses the right dynamic linker file name.
							  (substitute* "lib/Driver/ToolChains/Linux.cpp"
								       (("(^[[:blank:]]+LibDir = ).*" _ declaration)
									(string-append declaration "\"" libc "/lib\";\n"))

								       ;; Make clang look for libstdc++ in the right
								       ;; location.
								       (("LibStdCXXIncludePathCandidates\\[\\] = \\{")
									(string-append
									 "LibStdCXXIncludePathCandidates[] = { \"" gcc
									 "/include/c++\","))

								       ;; Make sure libc's libdir is on the search path, to
								       ;; allow crt1.o & co. to be found.
								       (("@GLIBC_LIBDIR@")
									(string-append libc "/lib")))))
						       (else
							`((substitute* "lib/Driver/Tools.cpp"
								       ;; Patch the 'getLinuxDynamicLinker' function so that
								       ;; it uses the right dynamic linker file name.
								       (("/lib64/ld-linux-x86-64.so.2")
									(string-append libc
										       ,(glibc-dynamic-linker))))

							  ;; Link to libclang_rt files from clang-runtime.
							  ;; This substitution needed slight adjustment in 3.8.
							  ,@(if (version>=? version "3.8")
								'((substitute* "lib/Driver/Tools.cpp"
									       (("TC\\.getDriver\\(\\)\\.ResourceDir")
										(string-append "\"" compiler-rt "\""))))
								'((substitute* "lib/Driver/ToolChain.cpp"
									       (("getDriver\\(\\)\\.ResourceDir")
										(string-append "\"" compiler-rt "\"")))))

							  ;; Make sure libc's libdir is on the search path, to
							  ;; allow crt1.o & co. to be found.
							  (substitute* "lib/Driver/ToolChains.cpp"
								       (("@GLIBC_LIBDIR@")
									(string-append libc "/lib"))))))
						    #t)))
			      ;; Awkwardly, multiple phases added after the same phase,
			      ;; e.g. unpack, get applied in the reverse order.  In other
			      ;; words, adding 'change-directory last means it occurs
			      ;; first after the unpack phase.
			      ,@(if (version>=? version "14")
				    '((add-after 'unpack 'change-directory
						 (lambda _
						   (chdir "clang"))))
				    '())
			      ,@(if (version>=? version "10")
				    `((add-after 'install 'adjust-cmake-file
						 (lambda* (#:key outputs #:allow-other-keys)
							  (let ((out (assoc-ref outputs "out")))
							    ;; Clang generates a CMake file with "targets"
							    ;; for each installed library file.  Downstream
							    ;; consumers of the CMake interface can use this
							    ;; to get absolute library locations.  Including
							    ;; this file will needlessly assert that _all_
							    ;; libraries are available, which causes problems
							    ;; in Guix because some are removed (see the
							    ;; move-extra-tools phase).  Thus, remove the
							    ;; asserts so that the main functionality works.
							    (substitute*
							     (string-append
							      out
							      "/lib/cmake/clang/ClangTargets-release.cmake")
							     (("list\\(APPEND _IMPORT_CHECK_TARGETS.*" all)
							      (string-append "# Disabled by Guix.\n#" all)))
							    #t))))
				    '())
			      ,@(if (version>? version "3.8")
				    `((add-after 'install 'symlink-cfi_ignorelist
						 (lambda* (#:key inputs outputs #:allow-other-keys)
							  (let* ((out (assoc-ref outputs "out"))
								 (lib-share (string-append out "/lib/clang/"
											   ,version "/share"))
								 (compiler-rt (assoc-ref inputs "clang-runtime"))
								 (file-name ,(if (version>=? version "13")
										 "cfi_ignorelist.txt"
										 "cfi_blacklist.txt"))
								 ;; The location varies between Clang versions.
								 (cfi-ignorelist
								  (cond
								   ((file-exists?
								     (string-append compiler-rt "/" file-name))
								    (string-append compiler-rt "/" file-name))
								   (else (string-append compiler-rt
											"/share/" file-name)))))
							    (mkdir-p lib-share)
							    ;; Symlink the ignorelist to where Clang expects
							    ;; to find it.
							    ;; Not all architectures support CFI.
							    ;; see: compiler-rt/cmake/config-ix.cmake
							    (when (file-exists? cfi-ignorelist)
							      (symlink cfi-ignorelist
								       (string-append lib-share "/" file-name)))))))
				    '())
			      (add-after 'install 'install-clean-up-/share/clang
					 (lambda* (#:key outputs #:allow-other-keys)
						  (let* ((out (assoc-ref outputs "out"))
							 (compl-dir (string-append
								     out "/etc/bash_completion.d")))
						    (with-directory-excursion (string-append out
											     "/share/clang")
									      (for-each
									       (lambda (file)
										 (when (file-exists? file)
										   (delete-file file)))
									       ;; Delete extensions for proprietary text editors.
									       '("clang-format-bbedit.applescript"
										 "clang-format-sublime.py"
										 ;; Delete Emacs extensions: see their respective Emacs
										 ;; Guix package instead.
										 "clang-rename.el" "clang-format.el"))
									      ;; Install bash completion.
									      (when (file-exists?  "bash-autocomplete.sh")
										(mkdir-p compl-dir)
										(rename-file "bash-autocomplete.sh"
											     (string-append compl-dir "/clang")))))
						  #t)))))

   ;; Clang supports the same environment variables as GCC.
   (native-search-paths
    (list (search-path-specification
           (variable "C_INCLUDE_PATH")
           (files '("include")))
          (search-path-specification
           (variable "CPLUS_INCLUDE_PATH")
           (files '("include/c++" "include")))
          (search-path-specification
           (variable "OBJC_INCLUDE_PATH")
           (files '("include")))
          (search-path-specification
           (variable "LIBRARY_PATH")
           (files '("lib" "lib64")))))

   (home-page "https://clang.llvm.org")
   (synopsis "C language family frontend for LLVM")
   (description
    "Clang is a compiler front end for the C, C++, Objective-C and
Objective-C++ programming languages.  It uses LLVM as its back end.  The Clang
project includes the Clang front end, the Clang static analyzer, and several
code analysis tools.")
   (properties properties)
   (license (if (version>=? version "9.0")
                license:asl2.0         ;with LLVM exceptions
                license:ncsa))))

(define-public clang-runtime-9.0.1
  (package
   (inherit
    (clang-runtime-from-llvm
     llvm-9.0.1
     "0xwh79g3zggdabxgnd0bphry75asm1qz7mv3hcqihqwqr6aspgy2"
     '("clang-runtime-9-libsanitizer-mode-field.patch")))
   (name "clang-runtime-9.0.1")))

(define-public clang-9.0.1
  (package
   (inherit
    (clang-from-llvm
     llvm-9.0.1
     clang-runtime-9.0.1
     "0ls2h3iv4finqyflyhry21qhc9cm9ga7g1zq21020p065qmm2y2p"
     #:legacy-build-shared-libs? #t
     #:patches '("clang-9.0-libc-search-path.patch")))
   (name "clang-9.0.1")))

(define-public libAfterImage-1.20
  (package
   (name "libAfterImage-1.20")
   (version "1.20")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
	   "ftp://ftp.afterstep.org/stable/libAfterImage/"
	   name "-" version ".tar.gz"))
     (sha256
      (base32 "125y119fbr3g389nr7yls4i7x5zd5pz7h8qn12k8b21b4xk1h6y5"))
     (modules '((guix build utils)))
     (snippet
      '(begin
         (substitute*
	  "Makefile.in"
	  (("ar clq") "ar cq")
	  )))))
   (build-system gnu-build-system)
   (inputs
    `(("coreutils" ,coreutils)
      ("zlib" ,zlib)))
   (arguments 
    `(#:tests? #f)) ; no tests in Makefile
   (home-page "http://www.afterstep.org/afterimage/")
   (synopsis "LibAfterImage")
   (description "libAfterImage is a generic image manipulation library. It
was initially implemented to address AfterStep Window
Manager's needs for image handling, but it evolved into
extremely powerful and flexible software, suitable for
virtually any project that has needs for loading,
manipulating, displaying images, as well as writing images
in files. Most of the popular image formats are supported
using standard libraries, with XCF, XPM, PPM/PNM, BMP, ICO,
TGA and GIF being supported internally.")
   (license license:lgpl2.1+)))

;; Ok
(define-public vdt-0.4.3
  (package
   (name "vdt-0.4.3")
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
   (name "Unuran-1.8.1")
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



  
