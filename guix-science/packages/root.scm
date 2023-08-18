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


;; (define-module (guix-science packages physics root)
(define-module (guix-science packages root)
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

  #:use-module  (guix-science packages physics)
  
  #:use-module  (ice-9 match)
  #:use-module  (ice-9 regex)

  #:export (

	    ;; Concerning versions, we have to keep several
	    ;; versions at the same time because users want
	    ;; to be able to install a specific version of
	    ;; each tool.

	    ;; ROOT
	    
	    ;; ROOT-5.28     ;; TODO
	    ROOT-6.18.04     ;; TODO Ko
	    ROOT-6.20.02     ;; TODO Ko: LLVM version different from ROOT supported, please try 5.0.x
	    ROOT-6.26.10     ;; Ok (thisroot.sh is not even needed)
	    ROOT-6.28.00     ;; TODO Ko: Could not open file for write in copy operation /gnu/store/579xrf7vz85ynjlk62njaczgnayam1ar-llvm-13.0.1/lib/cmake/cling/ClingConfig.cmake.tmp
	    
	    ))

(define-public ROOT-6.18.04
  (package
   (name "ROOT-6.18.04")
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
   (propagated-inputs
    `(
      ;; https://issues.guix.gnu.org/41038
      ;; ("binutils" ,binutils)
      ;; ("libc" ,glibc)
      ;; ("libc-debug" ,glibc "debug")
      ;; ("libc-static" ,glibc "static")

      ))
   (inputs
    `(

      ;; Package to build to avoid ROOT downloading them
      
      ("dcap" ,dcap-2.47.12)
      ("davix" ,davix-0.8.3)
      ("vdt" ,vdt-0.4.3)

      ;; Dependencies
      
      ("gcc-lib" ,gcc "lib")
      ("libAfterImage" ,libAfterImage-1.20)
      ("libcxx" ,libcxx)
      ("libjpeg-turbo" ,libjpeg-turbo) 
      ("liblzma" ,xz)
      ("libpthread-stubs" ,libpthread-stubs)
      ("libx11" ,libx11)
      ("libxext" ,libxext)
      ("libxft" ,libxft)
      ("libxml2" ,libxml2)
      ("libxpm" ,libxpm)
      ("pcre" ,pcre)
      ("zlib" ,zlib)
      
      ("cfitsio" ,cfitsio)
      ("coreutils" ,coreutils)
      ("fftw" ,fftw)
      ("freetype" ,freetype)
      ("gcc-toolchain" ,gcc-toolchain)
      ("git" ,git)
      ("glu" ,glu)
      ("gsl" ,gsl)
      ("less" ,less)
      ("clang" ,clang-9.0.1)
      ("llvm-9" ,llvm-9.0.1)
      ("lz4" ,lz4)
      ("mesa" ,mesa)
      ("openblas" ,openblas)
      ("openssl" ,openssl-1.1)
      ;; ("openssl" ,openssl)
      ("perl" ,perl)
      ("pkg-config" ,pkg-config)
      ("python@3.9" ,python-3.9)
      ("python@2.7" ,python-2.7)
      ("python-numpy" ,python-numpy)
      ("tbb" ,tbb)
      ("xxhash" ,xxhash)
      ("zstd" ,zstd)
      ;; ("libjpeg" ,libjpeg) ;; Deprecated
      ))
   
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

(define-public ROOT-6.20.02
  (package
   (name "ROOT-6.20.02")
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
   ;; 

   (build-system cmake-build-system)
   (propagated-inputs
    `(
      ;; https://issues.guix.gnu.org/41038
      ;; ("binutils" ,binutils)
      ;; ("libc" ,glibc)
      ;; ("libc-debug" ,glibc "debug")
      ;; ("libc-static" ,glibc "static")

      ))
   (inputs
    `(

      ;; Package to build to avoid ROOT downloading them
      
      ("dcap" ,dcap-2.47.12)
      ("davix" ,davix-0.8.3)
      ("vdt" ,vdt-0.4.3)

      ;; Dependencies
      
      ("gcc-lib" ,gcc "lib")
      ("libAfterImage" ,libAfterImage-1.20)
      ("libcxx" ,libcxx)
      ("libjpeg-turbo" ,libjpeg-turbo) 
      ("liblzma" ,xz)
      ("libpthread-stubs" ,libpthread-stubs)
      ("libx11" ,libx11)
      ("libxext" ,libxext)
      ("libxft" ,libxft)
      ("libxml2" ,libxml2)
      ("libxpm" ,libxpm)
      ("pcre" ,pcre)
      ("zlib" ,zlib)
      
      ("cfitsio" ,cfitsio)
      ("coreutils" ,coreutils)
      ("fftw" ,fftw)
      ("freetype" ,freetype)
      ("gcc-toolchain" ,gcc-toolchain)
      ("git" ,git)
      ("glu" ,glu)
      ("gsl" ,gsl)
      ("less" ,less)
      ("clang" ,clang-9.0.1)
      ("llvm-5" ,llvm-5)
      ("lz4" ,lz4)
      ("mesa" ,mesa)
      ("openblas" ,openblas)
      ("perl" ,perl)
      ("pkg-config" ,pkg-config)
      ("python@3.9" ,python-3.9)
      ("python@2.7" ,python-2.7)
      ("python-numpy" ,python-numpy)
      ("tbb" ,tbb)
      ("xxhash" ,xxhash)
      ("zstd" ,zstd)
      ;; ("libjpeg" ,libjpeg) ;; Deprecated
      ))

   (arguments 
    `(#:configure-flags 
      
      (list

       ;; To avoid error "fatal error: module file not found"
       ;; "-DCMAKE_INSTALL_PREFIX=/opt/root/"  ;; error

       (let ((out (assoc-ref %outputs "out")))
	 (string-append "-DCMAKE_INSTALL_PREFIX=" out))

       ;; From https://root.cern.ch/building-root
       "-Dgnuinstall=ON"
       "-Drpath=ON"
       "-DCMAKE_POSITION_INDEPENDENT_CODE=ON"
       
       ;; To avoid downloading clad, llvm, davix
       "-Dclad=OFF"
       ;; "-Dbuiltin_clang=OFF" ;; Too many cling errors
       "-Dbuiltin_llvm=OFF"
       "-Dbuiltin_davix=OFF"
       
       "-DCMAKE_INSTALL_LIBDIR=lib"
       (string-append "-DCLANG_LIBRARY_DIR="
		      (assoc-ref %build-inputs "clang")
		      "/lib")
       (string-append "-DCMAKE_C_FLAGS="
		      (let ((inputs (alist-delete "source" %build-inputs)))
			(apply string-append
			       (map (lambda (p)
				      (string-append "-L" (cdr p) "/lib "))
				    inputs))))
       "-Dastiff=ON"
       "-Dbuiltin_afterimage=OFF"
       "-Dbuiltin_ftgl=OFF"
       "-Dbuiltin_glew=OFF"
       "-Dbuiltin_gsl=OFF"
       "-Dbuiltin_zlib=OFF"
       "-Dcfitsio=ON"
       "-Ddavix=ON"
       "-Dhttp=ON"
       "-Djemmaloc=ON"
       "-Dmathmore=ON"
       "-Dminuit2=ON"
       "-Dopengl=ON"
       "-Dpythia6=ON"
       "-Dpythia6_nolink=ON"
       "-Dpythia8=OFF"
       "-Droofit=ON"
       "-Drpath=ON"
       "-Dshadowpw=OFF"
       "-Dsoversion=ON"
       "-Dtmva=OFF"
       "-Dvdt=ON"
       "-Dx11=ON"
       
       (string-append "-DOPENGL_INCLUDE_DIR="
		      (assoc-ref %build-inputs "mesa")
		      "/include")
       (string-append "-DOPENGL_gl_LIBRARY="
		      (assoc-ref %build-inputs "mesa")
		      "/lib/libGL.so")
       (string-append "-DOPENGL_glu_LIBRARY="
		      (assoc-ref %build-inputs "glu")
		      "/lib/libGLU.so")

       ;; From llvm.scm
       (string-append "-DC_INCLUDE_DIRS="
		      (assoc-ref %build-inputs "libc")
		      "/include")
       (string-append "-DCMAKE_LIBRARY_PATH="
		      (let ((inputs (alist-delete "source" %build-inputs)))
			(apply string-append
			       (map (lambda (p)
				      (string-append (cdr p) "/lib:"))
				    inputs)))))
		     
      ;; To avoid "depends on .. which cannot be found in RUNPATH"
      #:validate-runpath? #f

      ;; #:tests? #f
      
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

	 ;; Set LD_LIBRARY_PATH to find shared libraries
	 ;; during compilation
	 (let* ((libpath (getenv "LD_LIBRARY_PATH"))
		 (libpath (if libpath (string-append libpath ":") ""))
		 (libpath (add-libraries libpath inputs)))
           
	    (display (list "LD_LIBRARY_PATH" libpath)) (newline)
	    (setenv "LD_LIBRARY_PATH" libpath))

	 #t )))
      
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

;; Note: In case of the folowing error:
;; collect2: fatal error: ld terminated with signal 9 [Killed]
;; You may not have enough RAM to compile ROOT (> 16G)
(define-public ROOT-6.26.10 ;; Ok
  (package
   (name "ROOT-6.26.10")
   (version "6.26.10")
   (source (origin
	    (method url-fetch)
	    (uri (string-append
		  "https://root.cern/download/root_v"
		  version ".source.tar.gz"))
	    (sha256
	     (base32
	      "0pv2ppxc83x78c7vpcz09x3k98d1wx6mbszrajm1fh0hjz1vwmlf"))
	    (file-name (string-append name "-" version ".tar.gz"))
	    (modules '((guix build utils)))
	    (snippet
	     '(begin
		(substitute*
		 "core/clingutils/CMakeLists.txt"
		 (("set\\(CLANG_RESOURCE_DIR_STEM \\$\\{LLVM_LIBRARY_DIR\\}/clang\\)")
		  "set(CLANG_RESOURCE_DIR_STEM ${CLANG_LIBRARY_DIR}/clang)"))

		;; Preprocessor::LookupFile changed its signature
		;; (use-modules (ice-9 regex))
		(substitute*
		 "interpreter/cling/lib/Interpreter/AutoloadCallback.cpp"
		 (("/\\*SkipCache\\*/ false,") "/*SkipCache*/ false);")
		 (("/\\*OpenFile\\*/ false, /\\*CacheFail\\*/ true\\);") ""))

		(substitute*
		 "interpreter/cling/lib/Utils/Diagnostics.cpp"
		 (("m_Diags.Reset(true)") "m_Diags.Reset()"))
		
		;; https://github.com/root-project/cling/issues/449
		(substitute*
		 "interpreter/cling/lib/Interpreter/DynamicLibraryManagerSymbol.cpp"
		 (("if \\(Dyn.d_un.d_val & llvm::ELF::DF_1_PIE\\)")
		  "if (Dyn.d_un.d_val & 0x08000000)"))
		
		))))
   (build-system cmake-build-system)
   (propagated-inputs
    `(
      ;; https://issues.guix.gnu.org/41038
      ;; ("binutils" ,binutils)
      ;; ("libc" ,glibc)
      ;; ("libc-debug" ,glibc "debug")
      ;; ("libc-static" ,glibc "static")

      ))
   (inputs
    `(

      ;; Package to build to avoid ROOT downloading them
      
      ("dcap" ,dcap-2.47.12)
      ("davix" ,davix-0.8.3)
      ("vdt" ,vdt-0.4.3)

      ;; Dependencies
      
      ("gcc-lib" ,gcc "lib")
      ("libAfterImage" ,libAfterImage-1.20)
      ("libcxx" ,libcxx)
      ("libjpeg-turbo" ,libjpeg-turbo) 
      ("liblzma" ,xz)
      ("libpthread-stubs" ,libpthread-stubs)
      ("libx11" ,libx11)
      ("libxext" ,libxext)
      ("libxft" ,libxft)
      ("libxml2" ,libxml2)
      ("libxpm" ,libxpm)
      ("pcre" ,pcre)
      ("zlib" ,zlib)
      
      ("cfitsio" ,cfitsio)
      ("coreutils" ,coreutils)
      ("fftw" ,fftw)
      ("freetype" ,freetype)
      ("gcc-toolchain" ,gcc-toolchain)
      ("git" ,git)
      ("glu" ,glu)
      ("gsl" ,gsl)
      ("less" ,less)
      ("clang" ,clang-9.0.1)
      ("llvm-9" ,llvm-9.0.1)
      ("lz4" ,lz4)
      ("mesa" ,mesa)
      ("openblas" ,openblas)
      ;; ("openssl" ,openssl)
      ("openssl" ,openssl-1.1)
      ("perl" ,perl)
      ("pkg-config" ,pkg-config)
      ("python@3.9" ,python-3.9)
      ("python@2.7" ,python-2.7)
      ("python-numpy" ,python-numpy)
      ("tbb" ,tbb)
      ("xxhash" ,xxhash)
      ("zstd" ,zstd)
      ;; ("libjpeg" ,libjpeg) ;; Deprecated
      ))

   (arguments 
    `(#:configure-flags 
      
      (list

       ;; To avoid error "fatal error: module file not found"
       ;; "-DCMAKE_INSTALL_PREFIX=/opt/root/"  ;; error

       (let ((out (assoc-ref %outputs "out")))
	 (string-append "-DCMAKE_INSTALL_PREFIX=" out))

       ;; From https://root.cern.ch/building-root
       "-Dgnuinstall=ON"
       "-Drpath=ON"
       "-DCMAKE_POSITION_INDEPENDENT_CODE=ON"
       
       ;; To avoid downloading clad, llvm, davix
       "-Dclad=OFF"
       ;; "-Dbuiltin_clang=OFF" ;; Too many cling errors
       "-Dbuiltin_llvm=OFF"
       "-Dbuiltin_davix=OFF"
       
       "-DCMAKE_INSTALL_LIBDIR=lib"
       (string-append "-DCLANG_LIBRARY_DIR="
		      (assoc-ref %build-inputs "clang")
		      "/lib")
       (string-append "-DCMAKE_C_FLAGS="
		      (let ((inputs (alist-delete "source" %build-inputs)))
			(apply string-append
			       (map (lambda (p)
				      (string-append "-L" (cdr p) "/lib "))
				    inputs))))
       "-Dastiff=ON"
       "-Dbuiltin_afterimage=OFF"
       "-Dbuiltin_ftgl=OFF"
       "-Dbuiltin_glew=OFF"
       "-Dbuiltin_gsl=OFF"
       "-Dbuiltin_zlib=OFF"
       "-Dcfitsio=ON"
       "-Ddavix=ON"
       "-Dhttp=ON"
       "-Djemmaloc=ON"
       "-Dmathmore=ON"
       "-Dminuit2=ON"
       "-Dopengl=ON"
       "-Dpythia6=ON"
       "-Dpythia6_nolink=ON"
       "-Dpythia8=OFF"
       "-Droofit=ON"
       "-Drpath=ON"
       "-Dshadowpw=OFF"
       "-Dsoversion=ON"
       "-Dtmva=OFF"
       "-Dvdt=ON"
       "-Dx11=ON"
       
       (string-append "-DOPENGL_INCLUDE_DIR="
		      (assoc-ref %build-inputs "mesa")
		      "/include")
       (string-append "-DOPENGL_gl_LIBRARY="
		      (assoc-ref %build-inputs "mesa")
		      "/lib/libGL.so")
       (string-append "-DOPENGL_glu_LIBRARY="
		      (assoc-ref %build-inputs "glu")
		      "/lib/libGLU.so")

       ;; From llvm.scm
       (string-append "-DC_INCLUDE_DIRS="
		      (assoc-ref %build-inputs "libc")
		      "/include")
       (string-append "-DCMAKE_LIBRARY_PATH="
		      (let ((inputs (alist-delete "source" %build-inputs)))
			(apply string-append
			       (map (lambda (p)
				      (string-append (cdr p) "/lib:"))
				    inputs)))))
		     
      ;; To avoid "depends on .. which cannot be found in RUNPATH"
      #:validate-runpath? #f

      ;; #:tests? #f
      
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

	 ;; Set LD_LIBRARY_PATH to find shared libraries
	 ;; during compilation
	 (let* ((libpath (getenv "LD_LIBRARY_PATH"))
		 (libpath (if libpath (string-append libpath ":") ""))
		 (libpath (add-libraries libpath inputs)))
           
	    (display (list "LD_LIBRARY_PATH" libpath)) (newline)
	    (setenv "LD_LIBRARY_PATH" libpath))

	 #t )))
      
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

(define-public ROOT-6.28.00
  (package
   (inherit ROOT-6.26.10)
   (name "ROOT-6.28.00")
   (version "6.28.00")
   (source (origin
	    (method url-fetch)
	    (uri (string-append
		  "https://root.cern/download/root_v"
		  version ".source.tar.gz"))
	    (sha256
	     (base32
	      "1140blxxcgh1b7jyfn53c6cv04mbm574fbj9p4f42589dp0cb8dg"))
	    (file-name (string-append name "-" version ".tar.gz"))
	    (modules '((guix build utils)))
	    (snippet
	     '(begin
		(substitute*
		 "core/clingutils/CMakeLists.txt"
		 (("set\\(CLANG_RESOURCE_DIR_STEM \\$\\{LLVM_LIBRARY_DIR\\}/clang\\)")
		  "set(CLANG_RESOURCE_DIR_STEM ${CLANG_LIBRARY_DIR}/clang)"))

		;; Preprocessor::LookupFile changed its signature
		;; (use-modules (ice-9 regex))
		(substitute*
		 "interpreter/cling/lib/Interpreter/AutoloadCallback.cpp"
		 (("/\\*SkipCache\\*/ false,") "/*SkipCache*/ false);")
		 (("/\\*OpenFile\\*/ false, /\\*CacheFail\\*/ true\\);") ""))

		(substitute*
		 "interpreter/cling/lib/Utils/Diagnostics.cpp"
		 (("m_Diags.Reset(true)") "m_Diags.Reset()"))
		
		;; https://github.com/root-project/cling/issues/449
		(substitute*
		 "interpreter/cling/lib/Interpreter/DynamicLibraryManagerSymbol.cpp"
		 (("if \\(Dyn.d_un.d_val & llvm::ELF::DF_1_PIE\\)")
		  "if (Dyn.d_un.d_val & 0x08000000)"))
		
		))))

      (inputs
    `(

      ;; Package to build to avoid ROOT downloading them
      
      ("dcap" ,dcap-2.47.12)
      ("davix" ,davix-0.8.3)
      ("vdt" ,vdt-0.4.3)

      ;; Dependencies
      
      ("gcc-lib" ,gcc "lib")
      ("libAfterImage" ,libAfterImage-1.20)
      ("libcxx" ,libcxx)
      ("libjpeg-turbo" ,libjpeg-turbo) 
      ("liblzma" ,xz)
      ("libpthread-stubs" ,libpthread-stubs)
      ("libx11" ,libx11)
      ("libxext" ,libxext)
      ("libxft" ,libxft)
      ("libxml2" ,libxml2)
      ("libxpm" ,libxpm)
      ("pcre" ,pcre)
      ("zlib" ,zlib)
      
      ("cfitsio" ,cfitsio)
      ("coreutils" ,coreutils)
      ("fftw" ,fftw)
      ("freetype" ,freetype)
      ("gcc-toolchain" ,gcc-toolchain)
      ("git" ,git)
      ("glu" ,glu)
      ("gsl" ,gsl)
      ("less" ,less)
      ("clang" ,clang-9.0.1)
      ("llvm-13" ,llvm-13) ;; LLVM version different from ROOT supported, please try 13.0.x
      ("lz4" ,lz4)
      ("mesa" ,mesa)
      ("openblas" ,openblas)
      ("openssl" ,openssl)
      ("perl" ,perl)
      ("pkg-config" ,pkg-config)
      ("python@3.9" ,python-3.9)
      ("python@2.7" ,python-2.7)
      ("python-numpy" ,python-numpy)
      ("tbb" ,tbb)
      ("xxhash" ,xxhash)
      ("zstd" ,zstd)
      ;; ("libjpeg" ,libjpeg) ;; Deprecated
      ))
   ))
