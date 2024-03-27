;;; Copyright (C) 2022 Emmanuel Medernach <Emmanuel.Medernach@iphc.cnrs.fr>
;;; Copyright (C) 2023, 2024 Jake Forster <jakecameron.forster@gmail.com>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; This work started from a PR submitted by Emmanuel Medernach to guix-science.
;;; https://github.com/guix-science/guix-science/pull/25
;;;
;;; Original author EM.
;;; Contributions by JF:
;;; - Add geant4-vis package that supports visualisation using OGL and Qt.
;;; - Add package for the DAWN visualisation tool.
;;; - Switch CLHEP and Geant4 sources from archive files to version control
;;;   systems so they can be archived in Software Heritage.
;;; - Use Guix-style package names and version strings.

;;; Geant4 documentation:
;;; - Release notes: https://geant4.web.cern.ch/download/all
;;; - Installation guide: https://geant4.web.cern.ch/docs/

(define-module (guix-science packages geant4)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages adns) ;c-ares
  #:use-module (gnu packages algebra) ;FFTW
  #:use-module (gnu packages astronomy) ;cfitsio
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base) ;gnu-make, make
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages check)
  #:use-module (gnu packages commencement) ;gcc-toolchain
  #:use-module (gnu packages compression) ;zlib lz4
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages digest)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages file)
  #:use-module (gnu packages fontutils) ;fontconfig
  #:use-module (gnu packages fontutils) ;freetype
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages gl) ;mesa
  #:use-module (gnu packages glib)
  #:use-module (gnu packages hunspell)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image) ;libjpeg
  #:use-module (gnu packages less)
  #:use-module (gnu packages libevent) ;libuv
  #:use-module (gnu packages libreoffice) ;hunspell
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm) ;llvm clang
  #:use-module (gnu packages maths) ;openblas gsl
  #:use-module (gnu packages monitoring)
  #:use-module (gnu packages openstack)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pdf) ;poppler-qt5
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto) ;python-cryptography
  #:use-module (gnu packages python-web) ;python-oauthlib
  #:use-module (gnu packages python-xyz) ;numpy
  #:use-module (gnu packages qt) ;qtbase
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls) ;openssl
  #:use-module (gnu packages version-control) ;git
  #:use-module (gnu packages web) ;http-parser
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg) ;libx11
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages tcl) ;tk
  #:use-module (gnu packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (gnu packages cmake))

(define (replace-char old-char new-char str)
  (string-map (lambda (char)
                (if (char=? char old-char) new-char char)) str))

;; -- CLHEP library --

(define clhep-2.4.6.2
  (package
    (name "clhep")
    (version "2.4.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.cern.ch/CLHEP/CLHEP")
             (commit (string-append "CLHEP_"
                                    (replace-char #\. #\_ version)))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06mpvpb5ywx8j5cpddh9sb415h02mim3ip1sd895q94pxci2jr92"))))
    (build-system cmake-build-system)
    (home-page "https://proj-clhep.web.cern.ch/proj-clhep/")
    (synopsis "HEP-specific foundation and utility classes")
    (description
     "HEP-specific foundation and utility classes such as random generators,
physics vectors, geometry and linear algebra.
CLHEP is structured in a set of packages independent of any external package.")
    (license license:gpl3+)))

(define clhep-2.4.7.1
  (package
    (inherit clhep-2.4.6.2)
    (version "2.4.7.1")
    (source
     (origin
       (inherit (package-source clhep-2.4.6.2))
       (uri (git-reference
             (url "https://gitlab.cern.ch/CLHEP/CLHEP")
             (commit (string-append "CLHEP_"
                                    (replace-char #\. #\_ version)))))
       (sha256
        (base32 "13la49jnadsi5f2a70qagv87j7qzcpdccmbp2lyscaxlkmzy253z"))))))

;; -- Geant4 datasets--

;; G4NDL
;;
(define g4ndl-4.7
  (let ((version "4.7"))
    (origin
      (method url-fetch)
      (uri (string-append "https://cern.ch/geant4-data/datasets/G4NDL."
                          version ".tar.gz"))
      (sha256 (base32 "0283cwylajyjm4267ngfc2bd3452623r5bakywaccb8h44k3szby")))))

;; G4EMLOW
;;
(define g4emlow-8.2
  (let ((version "8.2"))
    (origin
      (method url-fetch)
      (uri (string-append "https://cern.ch/geant4-data/datasets/G4EMLOW."
                          version ".tar.gz"))
      (sha256 (base32 "09z4m3hq6895s7vwiaham7zbfq0ww6xh8xh8jv5kp9gm9wk6hxrx")))))

(define g4emlow-8.5
  (let ((version "8.5"))
    (origin
      (method url-fetch)
      (uri (string-append "https://cern.ch/geant4-data/datasets/G4EMLOW."
                          version ".tar.gz"))
      (sha256 (base32 "0wzgpklx776f14crhriyh08ya9b24vxv89f122nf4iaxmi4wmfk6")))))

;; PhotonEvaporation
;;
(define photon-evaporation-5.7
  (let ((version "5.7"))
    (origin
      (method url-fetch)
      (uri (string-append
            "https://cern.ch/geant4-data/datasets/G4PhotonEvaporation."
            version ".tar.gz"))
      (sha256 (base32 "1rg7fygfxx06h98ywlci6b0b9ih74q8diygr76c3vppxdzjl47kn")))))

;; RadioactiveDecay
;;
(define radioactive-decay-5.6
  (let ((version "5.6"))
    (origin
      (method url-fetch)
      (uri (string-append
            "https://cern.ch/geant4-data/datasets/G4RadioactiveDecay." version
            ".tar.gz"))
      (sha256 (base32 "1w8d9zzc4ss7sh1f8cxv5pmrx2b74p1y26377rw9hnlfkiy0g1iq")))))

;; G4PARTICLEXS
;;
(define g4particlexs-4.0
  (let ((version "4.0"))
    (origin
      (method url-fetch)
      (uri (string-append "https://cern.ch/geant4-data/datasets/G4PARTICLEXS."
            version ".tar.gz"))
      (sha256 (base32 "15fa6c8jh6g3nj82ychc13wlz2rc58v9jjdb6vyv1wn30fbh70ck")))))

;; G4PII
;;
(define g4pii-1.3
  (let ((version "1.3"))
    (origin
      (method url-fetch)
      (uri (string-append "https://cern.ch/geant4-data/datasets/G4PII."
                          version ".tar.gz"))
      (sha256 (base32 "09p92rk1sj837m6n6yd9k9a8gkh6bby2bfn6k0f3ix3m4s8as9b2")))))

;; RealSurface
;;
(define real-surface-2.2
  (let ((version "2.2"))
    (origin
      (method url-fetch)
      (uri (string-append
            "https://cern.ch/geant4-data/datasets/G4RealSurface." version
            ".tar.gz"))
      (sha256 (base32 "08382y1258ifs7nap6zaaazvabg72blr0dkqgwk32lrg07hdwm4r")))))

;; G4SAIDDATA
;;
(define g4saiddata-2.0
  (let ((version "2.0"))
    (origin
      (method url-fetch)
      (uri (string-append "https://cern.ch/geant4-data/datasets/G4SAIDDATA."
                          version ".tar.gz"))
      (sha256 (base32 "149fqy801n1pj2g6lcai2ziyvdz8cxdgbfarax6y8wdakgksh9hx")))))

;; G4ABLA
;;
(define g4abla-3.1
  (let ((version "3.1"))
    (origin
      (method url-fetch)
      (uri (string-append "https://cern.ch/geant4-data/datasets/G4ABLA."
                          version ".tar.gz"))
      (sha256 (base32 "1v97q28g1xqwnav0lwzwk7hc3b87yrmbvkgadf4bkwcbnm9b163n")))))

(define g4abla-3.3
  (let ((version "3.3"))
    (origin
      (method url-fetch)
      (uri (string-append "https://cern.ch/geant4-data/datasets/G4ABLA."
                          version ".tar.gz"))
      (sha256 (base32 "1cd25vckckxkhyx3pvz5swral0rkd4z7akv2dn4fz77fa8r1n10y")))))

;; G4INCL
;;
(define g4incl-1.0
  (let ((version "1.0"))
    (origin
      (method url-fetch)
      (uri (string-append "https://cern.ch/geant4-data/datasets/G4INCL."
                          version ".tar.gz"))
      (sha256 (base32 "0z9nqk125vvf4f19lhgb37jy60jf9zrjqg5zbxbd1wz93a162qbi")))))

(define g4incl-1.2
  (let ((version "1.2"))
    (origin
      (method url-fetch)
      (uri (string-append "https://cern.ch/geant4-data/datasets/G4INCL."
                          version ".tar.gz"))
      (sha256 (base32 "0zhs1vnrc0vhb1y4q3bscz9y2k9dsnk7ccjg97br42pffdhb307q")))))

;; G4ENSDFSTATE
;;
(define g4ensdfstate-2.3
  (let ((version "2.3"))
    (origin
      (method url-fetch)
      (uri (string-append "https://cern.ch/geant4-data/datasets/G4ENSDFSTATE."
            version ".tar.gz"))
      (sha256 (base32 "00wjir59rrrlk0a12vi8rsnhyya71rdi1kmark9sp487hbhcai4l")))))

;; G4TENDL
;;
(define g4tendl-1.4
  (let ((version "1.4"))
    (origin
      (method url-fetch)
      (uri (string-append "https://cern.ch/geant4-data/datasets/G4TENDL."
                          version ".tar.gz"))
      (sha256 (base32 "1q11jxfy5kjwb0jrvwv6dgdxr3h85s6g2bl9kdbfvd681h178wjb")))))

;; -- Geant4 version 11.1.x --
;;
;; -- geant4: Geant4 without visualisation -- 

;; (propagated-inputs (list cmake gnu-make gcc-toolchain))
(define-public geant4-11-1-1
  (package
    (name "geant4")
    (version "11.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.cern.ch/geant4/geant4")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "141fhmh0w8sbp6cckccf3dswn596ds4vgqwc3gz6i53ypyxmv2fw"))))
    (build-system cmake-build-system)
    (inputs (list coreutils
                  gcc-toolchain
                  xerces-c ;pour GDML
                  expat
                  clhep-2.4.6.2
                  python-2
                  python-3.10
                  perl
                  tcsh))
    (arguments
     `(#:configure-flags (let ((out (assoc-ref %outputs "out")))
                           (list (string-append "-DCMAKE_INSTALL_PREFIX=" out)
                                 "-DCMAKE_INSTALL_LIBDIR=lib"
                                 "-DGEANT4_BUILD_MULTITHREADED=ON"
                                 "-DGEANT4_ENABLE_TESTING=OFF"
                                 "-DGEANT4_INSTALL_DATA=OFF"
                                 "-DGEANT4_USE_GDML=ON" ;xerces-c is needed for GDML
                                 "-DGEANT4_USE_SYSTEM_CLHEP=ON"
                                 "-DGEANT4_USE_SYSTEM_EXPAT=ON"
                                 (let ((datadir (string-append out
                                                 "/share/geant4/data")))
                                   (string-append "-DGEANT4_INSTALL_DATADIR="
                                                  datadir "/share/geant4/data"))))
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'install-data
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((G4NDL (assoc-ref inputs "G4NDL"))
                            (G4EMLOW (assoc-ref inputs "G4EMLOW"))
                            (G4PhotonEvaporation (assoc-ref inputs
                                                  "G4PhotonEvaporation"))
                            (G4RadioactiveDecay (assoc-ref inputs
                                                 "G4RadioactiveDecay"))
                            (G4PARTICLEXS (assoc-ref inputs "G4PARTICLEXS"))
                            (G4PII (assoc-ref inputs "G4PII"))
                            (G4RealSurface (assoc-ref inputs "G4RealSurface"))
                            (G4SAIDDATA (assoc-ref inputs "G4SAIDDATA"))
                            (G4ABLA (assoc-ref inputs "G4ABLA"))
                            (G4INCL (assoc-ref inputs "G4INCL"))
                            (G4ENSDFSTATE (assoc-ref inputs "G4ENSDFSTATE"))
                            (G4TENDL (assoc-ref inputs "G4TENDL"))
                            (datadir (string-append (assoc-ref outputs "out")
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
                        (invoke "tar" "xvf" G4TENDL "-C" datadir)))))
       ;; no tests in Makefile
       #:tests? #f))
    (native-inputs `(("G4NDL" ,g4ndl-4.7)
                     ("G4EMLOW" ,g4emlow-8.2)
                     ("G4PhotonEvaporation" ,photon-evaporation-5.7)
                     ("G4RadioactiveDecay" ,radioactive-decay-5.6)
                     ("G4PARTICLEXS" ,g4particlexs-4.0)
                     ("G4PII" ,g4pii-1.3)
                     ("G4RealSurface" ,real-surface-2.2)
                     ("G4SAIDDATA" ,g4saiddata-2.0)
                     ("G4ABLA" ,g4abla-3.1)
                     ("G4INCL" ,g4incl-1.0)
                     ("G4ENSDFSTATE" ,g4ensdfstate-2.3)
                     ("G4TENDL" ,g4tendl-1.4)))
    (home-page "https://geant4.web.cern.ch")
    (synopsis "Monte Carlo particle track simulations")
    (description
     "Geant4 is a toolkit for the simulation of the passage of particles
through matter.  Its areas of application include high energy,
nuclear and accelerator physics, as well as studies
in medical and space science.

Note this package does not support visualisation -- you
can use @code{geant4-vis} for that.")
    (license (license:non-copyleft
              "https://geant4.web.cern.ch/download/license"))))

(define-public geant4-11-1-2
  (package
    (inherit geant4-11-1-1)
    (version "11.1.2")
    (source
     (origin
       (inherit (package-source geant4-11-1-1))
       (uri (git-reference
             (url "https://gitlab.cern.ch/geant4/geant4")
             (commit (string-append "v" version))))
       (sha256
        (base32 "03dbq48sycw8bfsf2vm6kk9f7ri663lfiw3blwigck0g4f1vdmhd"))))))

(define-public geant4-11-1-3
  (package
    (inherit geant4-11-1-1)
    (version "11.1.3")
    (source
     (origin
       (inherit (package-source geant4-11-1-1))
       (uri (git-reference
             (url "https://gitlab.cern.ch/geant4/geant4")
             (commit (string-append "v" version))))
       (sha256
        (base32 "0w616j4w8gmmn5815pnyylm9sv0gpdicpjbvxwpyhzqlssmm2bjl"))))))

;; -- geant4-vis: Geant4 with visualisation using OGL + Qt -- 

;; (propagated-inputs (list cmake gnu-make gcc-toolchain mesa))
(define-public geant4-vis-11-1-1
  (package
    (inherit geant4-11-1-1)
    (name "geant4-vis")
    (inputs (list coreutils
                  gcc-toolchain
                  xerces-c
                  expat
                  clhep-2.4.6.2
                  python-2
                  python-3.10
                  perl
                  tcsh
                  qtbase-5
                  libxmu
                  libxt))
    (arguments
     `(#:configure-flags (let* ((out (assoc-ref %outputs "out"))
                                (qt-path (string-append (assoc-ref
                                                         %build-inputs
                                                         "qtbase")
                                                        "/lib/cmake/Qt5")))
                           (list (string-append "-DCMAKE_INSTALL_PREFIX=" out)
                                 (string-append "-DCMAKE_PREFIX_PATH=" qt-path)
                                 "-DCMAKE_INSTALL_LIBDIR=lib"
                                 "-DGEANT4_BUILD_MULTITHREADED=ON"
                                 "-DGEANT4_ENABLE_TESTING=OFF"
                                 "-DGEANT4_INSTALL_DATA=OFF"
                                 "-DGEANT4_USE_GDML=ON" ;xerces-c is needed for GDML
                                 "-DGEANT4_USE_SYSTEM_CLHEP=ON"
                                 "-DGEANT4_USE_SYSTEM_EXPAT=ON"
                                 "-DGEANT4_USE_OPENGL_X11=ON"
                                 "-DGEANT4_USE_QT=ON"
                                 (let ((datadir (string-append out
                                                 "/share/geant4/data")))
                                   (string-append "-DGEANT4_INSTALL_DATADIR="
                                                  datadir "/share/geant4/data"))))
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'install-data
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((G4NDL (assoc-ref inputs "G4NDL"))
                            (G4EMLOW (assoc-ref inputs "G4EMLOW"))
                            (G4PhotonEvaporation (assoc-ref inputs
                                                  "G4PhotonEvaporation"))
                            (G4RadioactiveDecay (assoc-ref inputs
                                                 "G4RadioactiveDecay"))
                            (G4PARTICLEXS (assoc-ref inputs "G4PARTICLEXS"))
                            (G4PII (assoc-ref inputs "G4PII"))
                            (G4RealSurface (assoc-ref inputs "G4RealSurface"))
                            (G4SAIDDATA (assoc-ref inputs "G4SAIDDATA"))
                            (G4ABLA (assoc-ref inputs "G4ABLA"))
                            (G4INCL (assoc-ref inputs "G4INCL"))
                            (G4ENSDFSTATE (assoc-ref inputs "G4ENSDFSTATE"))
                            (G4TENDL (assoc-ref inputs "G4TENDL"))
                            (datadir (string-append (assoc-ref outputs "out")
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
                        (invoke "tar" "xvf" G4TENDL "-C" datadir)))))
       ;; no tests in Makefile
       #:tests? #f))
    (description
     "Geant4 is a toolkit for the simulation of the passage of particles
through matter.  Its areas of application include high energy,
nuclear and accelerator physics, as well as studies
in medical and space science.

This package supports visualisation with OpenGL and Qt.")))

(define-public geant4-vis-11-1-2
  (package
    (inherit geant4-vis-11-1-1)
    (version "11.1.2")
    (source
     (origin
       (inherit (package-source geant4-vis-11-1-1))
       (uri (git-reference
             (url "https://gitlab.cern.ch/geant4/geant4")
             (commit (string-append "v" version))))
       (sha256
        (base32 "03dbq48sycw8bfsf2vm6kk9f7ri663lfiw3blwigck0g4f1vdmhd"))))))

(define-public geant4-vis-11-1-3
  (package
    (inherit geant4-vis-11-1-1)
    (version "11.1.3")
    (source
     (origin
       (inherit (package-source geant4-vis-11-1-1))
       (uri (git-reference
             (url "https://gitlab.cern.ch/geant4/geant4")
             (commit (string-append "v" version))))
       (sha256
        (base32 "0w616j4w8gmmn5815pnyylm9sv0gpdicpjbvxwpyhzqlssmm2bjl"))))))

;; -- Geant4 version 11.2.x --
;; bump versions of CLHEP, G4EMLOW, G4ABLA, G4INCL

(define-public geant4-11-2-0
  (package
    (inherit geant4-11-1-1)
    (version "11.2.0")
    (source
     (origin
       (inherit (package-source geant4-11-1-1))
       (uri (git-reference
             (url "https://gitlab.cern.ch/geant4/geant4")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1nblqmpp45k080rkrpb214q02c0d7qpwbqr9phr38vvaqndd710n"))))
    (inputs (list coreutils
                  gcc-toolchain
                  xerces-c ;pour GDML
                  expat
                  clhep-2.4.7.1
                  python-2
                  python-3.10
                  perl
                  tcsh))
    (native-inputs `(("G4NDL" ,g4ndl-4.7)
                     ("G4EMLOW" ,g4emlow-8.5)
                     ("G4PhotonEvaporation" ,photon-evaporation-5.7)
                     ("G4RadioactiveDecay" ,radioactive-decay-5.6)
                     ("G4PARTICLEXS" ,g4particlexs-4.0)
                     ("G4PII" ,g4pii-1.3)
                     ("G4RealSurface" ,real-surface-2.2)
                     ("G4SAIDDATA" ,g4saiddata-2.0)
                     ("G4ABLA" ,g4abla-3.3)
                     ("G4INCL" ,g4incl-1.2)
                     ("G4ENSDFSTATE" ,g4ensdfstate-2.3)
                     ("G4TENDL" ,g4tendl-1.4)))))

(define-public geant4-11-2-1
  (package
    (inherit geant4-11-2-0)
    (version "11.2.1")
    (source
     (origin
       (inherit (package-source geant4-11-2-0))
       (uri (git-reference
             (url "https://gitlab.cern.ch/geant4/geant4")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1pdg5pf5izd2i05w34jclg0h4hrn3s4mjnr2dvq24lspjisc9r81"))))))

(define-public geant4-vis-11-2-0
  (package
    (inherit geant4-vis-11-1-1)
    (version "11.2.0")
    (source
     (origin
       (inherit (package-source geant4-vis-11-1-1))
       (uri (git-reference
             (url "https://gitlab.cern.ch/geant4/geant4")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1nblqmpp45k080rkrpb214q02c0d7qpwbqr9phr38vvaqndd710n"))))
    (inputs (list coreutils
                  gcc-toolchain
                  xerces-c
                  expat
                  clhep-2.4.7.1
                  python-2
                  python-3.10
                  perl
                  tcsh
                  qtbase-5
                  libxmu
                  libxt))
    (native-inputs `(("G4NDL" ,g4ndl-4.7)
                     ("G4EMLOW" ,g4emlow-8.5)
                     ("G4PhotonEvaporation" ,photon-evaporation-5.7)
                     ("G4RadioactiveDecay" ,radioactive-decay-5.6)
                     ("G4PARTICLEXS" ,g4particlexs-4.0)
                     ("G4PII" ,g4pii-1.3)
                     ("G4RealSurface" ,real-surface-2.2)
                     ("G4SAIDDATA" ,g4saiddata-2.0)
                     ("G4ABLA" ,g4abla-3.3)
                     ("G4INCL" ,g4incl-1.2)
                     ("G4ENSDFSTATE" ,g4ensdfstate-2.3)
                     ("G4TENDL" ,g4tendl-1.4)))))

(define-public geant4-vis-11-2-1
  (package
    (inherit geant4-vis-11-2-0)
    (version "11.2.1")
    (source
     (origin
       (inherit (package-source geant4-vis-11-2-0))
       (uri (git-reference
             (url "https://gitlab.cern.ch/geant4/geant4")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1pdg5pf5izd2i05w34jclg0h4hrn3s4mjnr2dvq24lspjisc9r81"))))))

;; -- dawn: DAWN visualisation tool for Geant4 -- 

(define-public dawn
  (package
    (name "dawn")
    (version "3.91a")
    (source
     (origin
       (method url-fetch)
       (uri "https://geant4.kek.jp/~tanaka/src/dawn_3_91a.tgz")
       (sha256
        (base32 "1x7mpi77jylsv8mzsqs0ppchbq147azd0b94i2qq2xhis7m5bn41"))))
    (build-system gnu-build-system)
    (native-inputs `(("gcc-toolchain" ,gcc-toolchain)
                     ("make" ,gnu-make)))
    (propagated-inputs `(("imagemagick" ,imagemagick)
                         ("tk" ,tk)))
    (arguments
     `(#:make-flags (list (string-append "INSTALL_DIR=" %output "/bin"))
       #:phases (modify-phases %standard-phases
                  (replace 'configure
                    (lambda* (#:key outputs #:allow-other-keys)
                      #t))
                  (replace 'check
                    (lambda* (#:key outputs #:allow-other-keys)
                      #t))
                  (add-after 'build 'create-bin-directory
                    (lambda* (#:key outputs #:allow-other-keys)
                      (mkdir-p (string-append %output "/bin")) #t))
                  (add-after 'unpack 'change-install-directory
                    (lambda* (#:key outputs #:allow-other-keys)
                      (substitute* "Makefile"
                        (("/usr/local/bin")
                         (string-append %output "/bin"))) #t)))))
    (synopsis "Visualize 3D data generated by Geant4 simulation")
    (description
     "Fukui Renderer DAWN (Drawer for Academic WritiNgs) is a renderer,
which reads 3D geometrical data and visualize them. It is a vectorized
3D PostScript processor with analytical hidden line/surface removal.
It aims at precise technical drawing of complex geometries.")
    (home-page "https://geant4.kek.jp/~tanaka/DAWN/About_DAWN.html")
    (license license:expat)))
