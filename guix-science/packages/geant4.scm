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


;; (define-module (guix-science packages physics geant4)
(define-module (guix-science packages geant4)
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
	    
	    ;; Geant 4
	    GEANT4-9.6.p04   ;; Ok
	    GEANT4-10.04.p03 ;; Ok
	    GEANT4-10.05.p01 ;; Ok	    
	    GEANT4-10.06.p01 ;; Ok
	    GEANT4-10.07.p04 ;; Ok
	    GEANT4-11.1.0    ;; Ok
	    GEANT4-11.1.1    ;; Ok
	    
	    ;; Geant 4 data
	    ;; G4NDL-4.6         
	    ;; G4NDL-4.7         
	    ;; G4EMLOW-7.9.1
	    ;; G4EMLOW-8.2
	    ;; G4PhotonEvaporation-5.5
	    ;; G4PhotonEvaporation-5.7
	    ;; G4RadioactiveDecay-5.4
	    ;; G4RadioactiveDecay-5.6
	    ;; G4PARTICLEXS-2.1
	    ;; G4PARTICLEXS-4.0
	    ;; G4PII-1.3
	    ;; G4RealSurface-2.1.1
	    ;; G4RealSurface-2.2
	    ;; G4SAIDDATA-2.0
	    ;; G4ABLA-3.1
	    ;; G4INCL-1.0
	    ;; G4ENSDFSTATE-2.2
	    ;; G4ENSDFSTATE-2.3
	    ;; G4TENDL-1.4

	    ))

;; -- Geant 4 --

;; https://cern.ch/geant4-data/datasets/G4NDL.4.6.tar.gz
(define-public G4NDL-4.6
  (let ((version "4.6"))
    (origin
     (method url-fetch)
     (uri (string-append
           "https://cern.ch/geant4-data/datasets/G4NDL."
           version ".tar.gz"))
     (sha256
      (base32 "022l2jjhi57frfdv9nk6s6q23gmr9zkix06fmni8gf0gmvr7qa4x")))))

;; https://cern.ch/geant4-data/datasets/G4NDL.4.7.tar.gz
(define-public G4NDL-4.7
  (let ((version "4.7"))
    (origin
     (method url-fetch)
     (uri (string-append
           "https://cern.ch/geant4-data/datasets/G4NDL."
           version ".tar.gz"))
     (sha256
      (base32 "0283cwylajyjm4267ngfc2bd3452623r5bakywaccb8h44k3szby")))))

;; https://cern.ch/geant4-data/datasets/G4EMLOW.7.9.1.tar.gz
(define-public G4EMLOW-7.9.1
  (let ((version "7.9.1"))
    (origin
     (method url-fetch)
     (uri (string-append
           "https://cern.ch/geant4-data/datasets/G4EMLOW."
           version ".tar.gz"))
     (sha256
      (base32 "1jrw0izw732bywq1k1srs3x2z0m3y2h377kcvwbwcr0wa1p10342")))))

;; https://cern.ch/geant4-data/datasets/G4EMLOW.8.2.tar.gz
(define-public G4EMLOW-8.2
  (let ((version "8.2"))
    (origin
     (method url-fetch)
     (uri (string-append
           "https://cern.ch/geant4-data/datasets/G4EMLOW."
           version ".tar.gz"))
     (sha256
      (base32 "09z4m3hq6895s7vwiaham7zbfq0ww6xh8xh8jv5kp9gm9wk6hxrx")))))

;; https://cern.ch/geant4-data/datasets/G4PhotonEvaporation.5.5.tar.gz
(define-public G4PhotonEvaporation-5.5
  (let ((version "5.5"))
    (origin
     (method url-fetch)
     (uri (string-append
           "https://cern.ch/geant4-data/datasets/G4PhotonEvaporation."
           version ".tar.gz"))
     (sha256
      (base32 "1mvnbs7yvkii41blks6bkqr8qhxgnj3xxvv1i3vdg2y14shxv5ar")))))

;; https://cern.ch/geant4-data/datasets/G4PhotonEvaporation.5.7.tar.gz
(define-public G4PhotonEvaporation-5.7
  (let ((version "5.7"))
    (origin
     (method url-fetch)
     (uri (string-append
           "https://cern.ch/geant4-data/datasets/G4PhotonEvaporation."
           version ".tar.gz"))
     (sha256
      (base32 "1rg7fygfxx06h98ywlci6b0b9ih74q8diygr76c3vppxdzjl47kn")))))

;; https://cern.ch/geant4-data/datasets/G4RadioactiveDecay.5.4.tar.gz
(define-public G4RadioactiveDecay-5.4
  (let ((version "5.4"))
    (origin
     (method url-fetch)
     (uri (string-append
           "https://cern.ch/geant4-data/datasets/G4RadioactiveDecay."
           version ".tar.gz"))
     (sha256
      (base32 "0qaark6mqzxr3lqawv6ai8z5211qihlp5x2hn86vzx8kgpd7j1r4")))))

;; https://cern.ch/geant4-data/datasets/G4RadioactiveDecay.5.6.tar.gz
(define-public G4RadioactiveDecay-5.6
  (let ((version "5.6"))
    (origin
     (method url-fetch)
     (uri (string-append
           "https://cern.ch/geant4-data/datasets/G4RadioactiveDecay."
           version ".tar.gz"))
     (sha256
      (base32 "1w8d9zzc4ss7sh1f8cxv5pmrx2b74p1y26377rw9hnlfkiy0g1iq")))))

;; https://cern.ch/geant4-data/datasets/G4PARTICLEXS.2.1.tar.gz
(define-public G4PARTICLEXS-2.1
  (let ((version "2.1"))
    (origin
     (method url-fetch)
     (uri (string-append
           "https://cern.ch/geant4-data/datasets/G4PARTICLEXS."
           version ".tar.gz"))
     (sha256
      (base32 "0h8ba8jk197npbd9lzq2qlfiklbjgqwk45m1cc6piy5vf8ri0k89")))))

;; https://cern.ch/geant4-data/datasets/G4PARTICLEXS.4.0.tar.gz
(define-public G4PARTICLEXS-4.0
  (let ((version "4.0"))
    (origin
     (method url-fetch)
     (uri (string-append
           "https://cern.ch/geant4-data/datasets/G4PARTICLEXS."
           version ".tar.gz"))
     (sha256
      (base32 "15fa6c8jh6g3nj82ychc13wlz2rc58v9jjdb6vyv1wn30fbh70ck")))))

;; https://cern.ch/geant4-data/datasets/G4PII.1.3.tar.gz
(define-public G4PII-1.3
  (let ((version "1.3"))
    (origin
     (method url-fetch)
     (uri (string-append
           "https://cern.ch/geant4-data/datasets/G4PII."
           version ".tar.gz"))
     (sha256
      (base32 "09p92rk1sj837m6n6yd9k9a8gkh6bby2bfn6k0f3ix3m4s8as9b2")))))

;; https://cern.ch/geant4-data/datasets/G4RealSurface.2.1.1.tar.gz
(define-public G4RealSurface-2.1.1
  (let ((version "2.1.1"))
    (origin
     (method url-fetch)
     (uri (string-append
           "https://cern.ch/geant4-data/datasets/G4RealSurface."
           version ".tar.gz"))
     (sha256
      (base32 "0l3gs0nlp10cjlwiln3f72zfch0av2g1r8m2ny9afgvwgbwiyj4h")))))

;; https://cern.ch/geant4-data/datasets/G4RealSurface.2.2.tar.gz
(define-public G4RealSurface-2.2
  (let ((version "2.2"))
    (origin
     (method url-fetch)
     (uri (string-append
           "https://cern.ch/geant4-data/datasets/G4RealSurface."
           version ".tar.gz"))
     (sha256
      (base32 "08382y1258ifs7nap6zaaazvabg72blr0dkqgwk32lrg07hdwm4r")))))

;; https://cern.ch/geant4-data/datasets/G4SAIDDATA.2.0.tar.gz
(define-public G4SAIDDATA-2.0
  (let ((version "2.0"))
    (origin
     (method url-fetch)
     (uri (string-append
           "https://cern.ch/geant4-data/datasets/G4SAIDDATA."
           version ".tar.gz"))
     (sha256
      (base32 "149fqy801n1pj2g6lcai2ziyvdz8cxdgbfarax6y8wdakgksh9hx")))))

;; https://cern.ch/geant4-data/datasets/G4ABLA.3.1.tar.gz
(define-public G4ABLA-3.1
  (let ((version "3.1"))
    (origin
     (method url-fetch)
     (uri (string-append
           "https://cern.ch/geant4-data/datasets/G4ABLA."
           version ".tar.gz"))
     (sha256
      (base32 "1v97q28g1xqwnav0lwzwk7hc3b87yrmbvkgadf4bkwcbnm9b163n")))))

;; https://cern.ch/geant4-data/datasets/G4INCL.1.0.tar.gz
(define-public G4INCL-1.0
  (let ((version "1.0"))
    (origin
     (method url-fetch)
     (uri (string-append
           "https://cern.ch/geant4-data/datasets/G4INCL."
           version ".tar.gz"))
     (sha256
      (base32 "0z9nqk125vvf4f19lhgb37jy60jf9zrjqg5zbxbd1wz93a162qbi")))))

;; https://cern.ch/geant4-data/datasets/G4ENSDFSTATE.2.2.tar.gz
(define-public G4ENSDFSTATE-2.2
  (let ((version "2.2"))
    (origin
     (method url-fetch)
     (uri (string-append
           "https://cern.ch/geant4-data/datasets/G4ENSDFSTATE."
           version ".tar.gz"))
     (sha256
      (base32 "19p0sq0rmyg48j9hddqy24dn99md7ddiyq09lyj381q7cbpjfznx")))))

;; https://cern.ch/geant4-data/datasets/G4ENSDFSTATE.2.3.tar.gz
(define-public G4ENSDFSTATE-2.3
  (let ((version "2.3"))
    (origin
     (method url-fetch)
     (uri (string-append
           "https://cern.ch/geant4-data/datasets/G4ENSDFSTATE."
           version ".tar.gz"))
     (sha256
      (base32 "00wjir59rrrlk0a12vi8rsnhyya71rdi1kmark9sp487hbhcai4l")))))

;; https://cern.ch/geant4-data/datasets/G4TENDL.1.4.tar.gz
(define-public G4TENDL-1.4
  (let ((version "1.4"))
    (origin
     (method url-fetch)
     (uri (string-append
           "https://cern.ch/geant4-data/datasets/G4TENDL."
           version ".tar.gz"))
     (sha256
      (base32 "1q11jxfy5kjwb0jrvwv6dgdxr3h85s6g2bl9kdbfvd681h178wjb")))))

(define-public GEANT4-10.06.p01
  (package
   (name "GEANT4-10.06.p01")
   (version "10.06.p01")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "http://cern.ch/geant4-data/releases/geant4."
                         version
                         ".tar.gz"))
     (sha256
      (base32 "0ssxg7dd7vxljb3fdyb0llg7gsxack21qjfsb3n23k107a19yibk"))))
   (build-system cmake-build-system)
   (inputs
    `(("coreutils" ,coreutils)
      ("gcc-toolchain" ,gcc-toolchain)
      ("xerces-c" ,xerces-c) ;; pour GDML
      ("expat" ,expat)
      ("CLHEP" ,CLHEP-2.3.4)
      ("python2" ,python-2)
      ("python3" ,python-3.9)
      ("perl" ,perl)
      ("tcsh" ,tcsh)
      ))

   (arguments
    `(#:configure-flags
      (let ((out (assoc-ref %outputs "out")))
	(list

         (string-append "-DCMAKE_INSTALL_PREFIX=" out)

	 ;; Downloading data is not allowed with guix
	 ;; "-DGEANT4_INSTALL_DATA=ON"
       
	 ;; "-DGEANT4_BUILD_CXXSTD=c++11"
	 
	 "-DCMAKE_INSTALL_LIBDIR=lib"
	 "-DGEANT4_BUILD_MULTITHREADED=ON"
	 "-DGEANT4_ENABLE_TESTING=OFF"
	 "-DGEANT4_INSTALL_DATA=OFF"
	 "-DGEANT4_USE_GDML=ON" ;; xerces-c is needed for GDML
	 "-DGEANT4_USE_SYSTEM_CLHEP=ON"
	 "-DGEANT4_USE_SYSTEM_EXPAT=ON"

	 (let ((datadir (string-append out "/share/geant4/data")))
	   (string-append "-DGEANT4_INSTALL_DATADIR="
			  datadir
			  "/share/geant4/data"))))

	#:phases
	(modify-phases
	 %standard-phases
	 (add-after
          'install 'install-data
          (lambda*
           (#:key inputs outputs #:allow-other-keys)
           (let ((G4NDL (assoc-ref inputs "G4NDL"))
		 (G4EMLOW (assoc-ref inputs "G4EMLOW"))
		 (G4PhotonEvaporation (assoc-ref inputs "G4PhotonEvaporation"))
		 (G4RadioactiveDecay (assoc-ref inputs "G4RadioactiveDecay"))
		 (G4PARTICLEXS (assoc-ref inputs "G4PARTICLEXS"))
		 (G4PII (assoc-ref inputs "G4PII"))
		 (G4RealSurface (assoc-ref inputs "G4RealSurface"))
		 (G4SAIDDATA (assoc-ref inputs "G4SAIDDATA"))
		 (G4ABLA (assoc-ref inputs "G4ABLA"))
		 (G4INCL (assoc-ref inputs "G4INCL"))
		 (G4ENSDFSTATE (assoc-ref inputs "G4ENSDFSTATE"))
		 (G4TENDL (assoc-ref inputs "G4TENDL"))
		 
		 (datadir (string-append
                           (assoc-ref outputs "out")
                           "/share/geant4/data")))

	     (display (list "Data archives:"
			    G4NDL
			    G4EMLOW
			    G4PhotonEvaporation
			    G4RadioactiveDecay 
			    G4PARTICLEXS 
			    G4PII
			    G4RealSurface
			    G4SAIDDATA 
			    G4ABLA 
			    G4INCL 
			    G4ENSDFSTATE))
	     (newline)
	     
             (mkdir-p datadir)
             (invoke "tar" "xvf" G4NDL "-C" datadir)
             (invoke "tar" "xvf" G4EMLOW "-C" datadir)
             (invoke "tar" "xvf" G4PhotonEvaporation "-C" datadir)
             (invoke "tar" "xvf" G4RadioactiveDecay "-C" datadir)
             (invoke "tar" "xvf" G4PARTICLEXS "-C" datadir)
             (invoke "tar" "xvf" G4PII "-C" datadir)
             (invoke "tar" "xvf" G4RealSurface "-C" datadir)
             (invoke "tar" "xvf" G4SAIDDATA "-C" datadir)
             (invoke "tar" "xvf" G4ABLA "-C" datadir)
             (invoke "tar" "xvf" G4INCL "-C" datadir)
             (invoke "tar" "xvf" G4ENSDFSTATE "-C" datadir)
	     (invoke "tar" "xvf" G4TENDL "-C" datadir)
             ))))
      
	;; no tests in Makefile
	#:tests? #f))

   (native-inputs
    `(("G4NDL" ,G4NDL-4.6)
      ("G4EMLOW" ,G4EMLOW-7.9.1)
      ("G4PhotonEvaporation" ,G4PhotonEvaporation-5.5)
      ("G4RadioactiveDecay" ,G4RadioactiveDecay-5.4)
      ("G4PARTICLEXS" ,G4PARTICLEXS-2.1)
      ("G4PII" ,G4PII-1.3)
      ("G4RealSurface" ,G4RealSurface-2.1.1)
      ("G4SAIDDATA" ,G4SAIDDATA-2.0)
      ("G4ABLA" ,G4ABLA-3.1)
      ("G4INCL" ,G4INCL-1.0)
      ("G4ENSDFSTATE" ,G4ENSDFSTATE-2.3)
      ("G4TENDL" ,G4TENDL-1.4)
      ))
   
   (home-page "https://geant4.web.cern.ch/")
   (synopsis "Platform for the simulation of the passage of particles through matter using Monte Carlo methods")
   (description "Geant4 is a toolkit for the simulation of the passage of particles through matter.  Its areas of application include high energy, nuclear and accelerator physics, as well as studies in medical and space science.")
   (license (license:non-copyleft "https://geant4.web.cern.ch/download/license"))
   ))

(define-public GEANT4-9.6.p04 ;; http://cern.ch/geant4-data/releases/geant4.9.6.p04.tar.gz
  (package
   (inherit GEANT4-10.06.p01)
   (name "GEANT4-9.6.p04")
   (version "9.6.p04")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "http://cern.ch/geant4-data/releases/geant4."
                         version
                         ".tar.gz"))
     (sha256
      (base32
       "1z9r1wxgp0x5xbg1fk9471izv88a6sldbirzaf7sqhva72jj0wlr"))
     (modules '((guix build utils)))
     (snippet '(begin
		 (substitute*
		  "source/processes/hadronic/models/low_energy/src/G4LElastic.cc"
		  (("<<std::cout") "<< std::endl"))
		 (substitute*
		  "source/visualization/gMocren/src/G4GMocrenIO.cc"
		  (("<< ofile") ""))

		 ))
     ))
   
   (native-inputs
    `(("G4NDL" ,G4NDL-4.7)
      ("G4EMLOW" ,G4EMLOW-8.2)
      ("G4PhotonEvaporation" ,G4PhotonEvaporation-5.7)
      ("G4RadioactiveDecay" ,G4RadioactiveDecay-5.6)
      ("G4PARTICLEXS" ,G4PARTICLEXS-4.0)
      ("G4PII" ,G4PII-1.3)
      ("G4RealSurface" ,G4RealSurface-2.2)
      ("G4SAIDDATA" ,G4SAIDDATA-2.0)
      ("G4ABLA" ,G4ABLA-3.1)
      ("G4INCL" ,G4INCL-1.0)
      ("G4ENSDFSTATE" ,G4ENSDFSTATE-2.3)
      ("G4TENDL" ,G4TENDL-1.4)
      ))

   (inputs
    `(("coreutils" ,coreutils)
      ("gcc-toolchain" ,gcc-toolchain)
      ("xerces-c" ,xerces-c) ;; pour GDML
      ("expat" ,expat)
      ("CLHEP" ,CLHEP-2.1.2.5)
      ("python2" ,python-2)
      ("python3" ,python-3.9)
      ("perl" ,perl)
      ("tcsh" ,tcsh)
      ))   

   (arguments
    `(#:configure-flags
      (let ((out (assoc-ref %outputs "out")))
	(list

         (string-append "-DCMAKE_INSTALL_PREFIX=" out)

	 ;; Downloading data is not allowed with guix
	 ;; "-DGEANT4_INSTALL_DATA=ON"
       
	 "-DGEANT4_BUILD_CXXSTD=c++11" ;; Necessary for GEANT4-9.6
	 
	 "-DCMAKE_INSTALL_LIBDIR=lib"
	 "-DGEANT4_BUILD_MULTITHREADED=ON"
	 "-DGEANT4_ENABLE_TESTING=OFF"
	 "-DGEANT4_INSTALL_DATA=OFF"
	 "-DGEANT4_USE_GDML=ON" ;; xerces-c is needed for GDML
	 "-DGEANT4_USE_SYSTEM_CLHEP=ON"
	 "-DGEANT4_USE_SYSTEM_EXPAT=ON"

	 (let ((datadir (string-append out "/share/geant4/data")))
	   (string-append "-DGEANT4_INSTALL_DATADIR="
			  datadir
			  "/share/geant4/data"))))

      #:phases
      (modify-phases
       %standard-phases
       (add-after
        'install 'install-data
        (lambda*
         (#:key inputs outputs #:allow-other-keys)
         (let ((G4NDL (assoc-ref inputs "G4NDL"))
	       (G4EMLOW (assoc-ref inputs "G4EMLOW"))
	       (G4PhotonEvaporation (assoc-ref inputs "G4PhotonEvaporation"))
	       (G4RadioactiveDecay (assoc-ref inputs "G4RadioactiveDecay"))
	       (G4PARTICLEXS (assoc-ref inputs "G4PARTICLEXS"))
	       (G4PII (assoc-ref inputs "G4PII"))
	       (G4RealSurface (assoc-ref inputs "G4RealSurface"))
	       (G4SAIDDATA (assoc-ref inputs "G4SAIDDATA"))
	       (G4ABLA (assoc-ref inputs "G4ABLA"))
	       (G4INCL (assoc-ref inputs "G4INCL"))
	       (G4ENSDFSTATE (assoc-ref inputs "G4ENSDFSTATE"))
	       (G4TENDL (assoc-ref inputs "G4TENDL"))
		 
	       (datadir (string-append
                         (assoc-ref outputs "out")
                         "/share/geant4/data")))

	   (display (list "Data archives:"
			  G4NDL
			  G4EMLOW
			  G4PhotonEvaporation
			  G4RadioactiveDecay 
			  G4PARTICLEXS 
			  G4PII
			  G4RealSurface
			  G4SAIDDATA 
			  G4ABLA 
			  G4INCL 
			  G4ENSDFSTATE))
	   (newline)
	     
           (mkdir-p datadir)
           (invoke "tar" "xvf" G4NDL "-C" datadir)
           (invoke "tar" "xvf" G4EMLOW "-C" datadir)
           (invoke "tar" "xvf" G4PhotonEvaporation "-C" datadir)
           (invoke "tar" "xvf" G4RadioactiveDecay "-C" datadir)
           (invoke "tar" "xvf" G4PARTICLEXS "-C" datadir)
           (invoke "tar" "xvf" G4PII "-C" datadir)
           (invoke "tar" "xvf" G4RealSurface "-C" datadir)
           (invoke "tar" "xvf" G4SAIDDATA "-C" datadir)
           (invoke "tar" "xvf" G4ABLA "-C" datadir)
           (invoke "tar" "xvf" G4INCL "-C" datadir)
           (invoke "tar" "xvf" G4ENSDFSTATE "-C" datadir)
	   (invoke "tar" "xvf" G4TENDL "-C" datadir)
           ))))
      
      ;; no tests in Makefile
      #:tests? #f))
   
   ))

(define-public GEANT4-10.04.p03
  (package
   (inherit GEANT4-10.06.p01)
   (name "GEANT4-10.04.p03")
   (version "10.04.p03")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "http://cern.ch/geant4-data/releases/geant4."
                         version
                         ".tar.gz"))
     (sha256
      (base32
       "1j1921hkv48i27b46nivb9z6qfcy4x8shgxxqlz8pdc69hvm591i"))))
   
   ))

(define-public GEANT4-10.07.p04
  (package
   (inherit GEANT4-10.06.p01)
   (name "GEANT4-10.07.p04")
   (version "10.07.p04") ;; geant4.10.07.p04.tar.gz
   (source
    (origin
     (method url-fetch)
     (uri (string-append "http://cern.ch/geant4-data/releases/geant4."
                         version
                         ".tar.gz"))
     (sha256
      (base32
       "0a9yc2dbj1a88ly9wbhdyf2mpi08k6w3b6f26hh14pmg88q2dfv6"))))
   
   (native-inputs
    `(("G4NDL" ,G4NDL-4.7)
      ("G4EMLOW" ,G4EMLOW-8.2)
      ("G4PhotonEvaporation" ,G4PhotonEvaporation-5.7)
      ("G4RadioactiveDecay" ,G4RadioactiveDecay-5.6)
      ("G4PARTICLEXS" ,G4PARTICLEXS-4.0)
      ("G4PII" ,G4PII-1.3)
      ("G4RealSurface" ,G4RealSurface-2.2)
      ("G4SAIDDATA" ,G4SAIDDATA-2.0)
      ("G4ABLA" ,G4ABLA-3.1)
      ("G4INCL" ,G4INCL-1.0)
      ("G4ENSDFSTATE" ,G4ENSDFSTATE-2.3)
      ("G4TENDL" ,G4TENDL-1.4)
      ))

   (inputs
    `(("coreutils" ,coreutils)
      ("gcc-toolchain" ,gcc-toolchain)
      ("xerces-c" ,xerces-c) ;; pour GDML
      ("expat" ,expat)
      ("CLHEP" ,CLHEP-2.4.6.2)
      ("python2" ,python-2)
      ("python3" ,python-3.9)
      ("perl" ,perl)
      ("tcsh" ,tcsh)
      ))
   
   ))

(define-public GEANT4-10.05.p01 ;; geant4.10.05.p01.tar.gz
  (package
   (inherit GEANT4-10.06.p01)
   (name "GEANT4-10.05.p01")
   (version "10.05.p01") 
   (source
    (origin
     (method url-fetch)
     (uri (string-append "http://cern.ch/geant4-data/releases/geant4."
                         version
                         ".tar.gz"))
     (sha256
      (base32
       "1zxx4cszc1y75mblwa4npvn10d3fx59k3kk705zd3yh00li958pl"))))
   
   (native-inputs
    `(("G4NDL" ,G4NDL-4.7)
      ("G4EMLOW" ,G4EMLOW-8.2)
      ("G4PhotonEvaporation" ,G4PhotonEvaporation-5.7)
      ("G4RadioactiveDecay" ,G4RadioactiveDecay-5.6)
      ("G4PARTICLEXS" ,G4PARTICLEXS-4.0)
      ("G4PII" ,G4PII-1.3)
      ("G4RealSurface" ,G4RealSurface-2.2)
      ("G4SAIDDATA" ,G4SAIDDATA-2.0)
      ("G4ABLA" ,G4ABLA-3.1)
      ("G4INCL" ,G4INCL-1.0)
      ("G4ENSDFSTATE" ,G4ENSDFSTATE-2.3)
      ("G4TENDL" ,G4TENDL-1.4)
      ))

   (inputs
    `(("coreutils" ,coreutils)
      ("gcc-toolchain" ,gcc-toolchain)
      ("xerces-c" ,xerces-c) ;; pour GDML
      ("expat" ,expat)
      ("CLHEP" ,CLHEP-2.4.6.2)
      ("python2" ,python-2)
      ("python3" ,python-3.9)
      ("perl" ,perl)
      ("tcsh" ,tcsh)
      ))
   
   ))

(define-public GEANT4-11.1.0
  (package
   (inherit GEANT4-10.06.p01)
   (name "GEANT4-11.1.0")
   (version "11.1.0")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "http://cern.ch/geant4-data/releases/geant4-v"
                         version
                         ".tar.gz"))
     (sha256
      (base32
       "11drs02imp50gnvbf7ngv5d1gr69y3kgzga68zvp7hsjc43467h5"))))
   
   (native-inputs
    `(("G4NDL" ,G4NDL-4.7)
      ("G4EMLOW" ,G4EMLOW-8.2)
      ("G4PhotonEvaporation" ,G4PhotonEvaporation-5.7)
      ("G4RadioactiveDecay" ,G4RadioactiveDecay-5.6)
      ("G4PARTICLEXS" ,G4PARTICLEXS-4.0)
      ("G4PII" ,G4PII-1.3)
      ("G4RealSurface" ,G4RealSurface-2.2)
      ("G4SAIDDATA" ,G4SAIDDATA-2.0)
      ("G4ABLA" ,G4ABLA-3.1)
      ("G4INCL" ,G4INCL-1.0)
      ("G4ENSDFSTATE" ,G4ENSDFSTATE-2.3)
      ("G4TENDL" ,G4TENDL-1.4)
      ))

   (inputs
    `(("coreutils" ,coreutils)
      ("gcc-toolchain" ,gcc-toolchain)
      ("xerces-c" ,xerces-c) ;; pour GDML
      ("expat" ,expat)
      ("CLHEP" ,CLHEP-2.4.6.2)
      ("python2" ,python-2)
      ("python3" ,python-3.9)
      ("perl" ,perl)
      ("tcsh" ,tcsh)
      ))
   
   ))

(define-public GEANT4-11.1.1
  (package
   (inherit GEANT4-10.06.p01)
   (name "GEANT4-11.1.1")
   (version "11.1.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "http://cern.ch/geant4-data/releases/geant4-v"
                         version
                         ".tar.gz"))
     (sha256
      (base32
       "0sg45nd47ajqhzncw5fahrrx8a9s54x1l31l6xa73xsx5bmj54fj"))))
   
   (native-inputs
    `(("G4NDL" ,G4NDL-4.7)
      ("G4EMLOW" ,G4EMLOW-8.2)
      ("G4PhotonEvaporation" ,G4PhotonEvaporation-5.7)
      ("G4RadioactiveDecay" ,G4RadioactiveDecay-5.6)
      ("G4PARTICLEXS" ,G4PARTICLEXS-4.0)
      ("G4PII" ,G4PII-1.3)
      ("G4RealSurface" ,G4RealSurface-2.2)
      ("G4SAIDDATA" ,G4SAIDDATA-2.0)
      ("G4ABLA" ,G4ABLA-3.1)
      ("G4INCL" ,G4INCL-1.0)
      ("G4ENSDFSTATE" ,G4ENSDFSTATE-2.3)
      ("G4TENDL" ,G4TENDL-1.4)
      ))

   (inputs
    `(("coreutils" ,coreutils)
      ("gcc-toolchain" ,gcc-toolchain)
      ("xerces-c" ,xerces-c) ;; pour GDML
      ("expat" ,expat)
      ("CLHEP" ,CLHEP-2.4.6.2)
      ("python2" ,python-2)
      ("python3" ,python-3.9)
      ("perl" ,perl)
      ("tcsh" ,tcsh)
      ))
   
   ))


