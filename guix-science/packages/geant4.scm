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

;; (define-module (guix-science-nonfree packages geant4)
(define-module (guix-science packages geant4)
  #:use-module ((guix licenses)
                #:prefix license:)
  ;; #:use-module ((guix-science-nonfree licenses) #:prefix nonfree:)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

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

;; -- Geant4 datasets --

(define (geant4-dataset name version hash)
    (origin
      (method url-fetch)
    (uri (string-append "https://cern.ch/geant4-data/datasets/" name "."
                          version ".tar.gz"))
    (sha256 (base32 hash))))

;; G4NDL
;;
(define g4ndl-4.7
  (geant4-dataset "G4NDL" "4.7"
                  "0283cwylajyjm4267ngfc2bd3452623r5bakywaccb8h44k3szby"))

;; G4EMLOW
;;
(define g4emlow-8.2
  (geant4-dataset "G4EMLOW" "8.2"
                  "09z4m3hq6895s7vwiaham7zbfq0ww6xh8xh8jv5kp9gm9wk6hxrx"))

(define g4emlow-8.5
  (geant4-dataset "G4EMLOW" "8.5"
                  "0wzgpklx776f14crhriyh08ya9b24vxv89f122nf4iaxmi4wmfk6"))

;; PhotonEvaporation
;;
(define photon-evaporation-5.7
  (geant4-dataset "G4PhotonEvaporation" "5.7"
                  "1rg7fygfxx06h98ywlci6b0b9ih74q8diygr76c3vppxdzjl47kn"))

;; RadioactiveDecay
;;
(define radioactive-decay-5.6
  (geant4-dataset "G4RadioactiveDecay" "5.6"
                  "1w8d9zzc4ss7sh1f8cxv5pmrx2b74p1y26377rw9hnlfkiy0g1iq"))

;; G4PARTICLEXS
;;
(define g4particlexs-4.0
  (geant4-dataset "G4PARTICLEXS" "4.0"
                  "15fa6c8jh6g3nj82ychc13wlz2rc58v9jjdb6vyv1wn30fbh70ck"))

;; G4PII
;;
(define g4pii-1.3
  (geant4-dataset "G4PII" "1.3"
                  "09p92rk1sj837m6n6yd9k9a8gkh6bby2bfn6k0f3ix3m4s8as9b2"))

;; RealSurface
;;
(define real-surface-2.2
  (geant4-dataset "G4RealSurface" "2.2"
                  "08382y1258ifs7nap6zaaazvabg72blr0dkqgwk32lrg07hdwm4r"))

;; G4SAIDDATA
;;
(define g4saiddata-2.0
  (geant4-dataset "G4SAIDDATA" "2.0"
                  "149fqy801n1pj2g6lcai2ziyvdz8cxdgbfarax6y8wdakgksh9hx"))

;; G4ABLA
;;
(define g4abla-3.1
  (geant4-dataset "G4ABLA" "3.1"
                  "1v97q28g1xqwnav0lwzwk7hc3b87yrmbvkgadf4bkwcbnm9b163n"))

(define g4abla-3.3
  (geant4-dataset "G4ABLA" "3.3"
                  "1cd25vckckxkhyx3pvz5swral0rkd4z7akv2dn4fz77fa8r1n10y"))

;; G4INCL
;;
(define g4incl-1.0
  (geant4-dataset "G4INCL" "1.0"
                  "0z9nqk125vvf4f19lhgb37jy60jf9zrjqg5zbxbd1wz93a162qbi"))

(define g4incl-1.2
  (geant4-dataset "G4INCL" "1.2"
                  "0zhs1vnrc0vhb1y4q3bscz9y2k9dsnk7ccjg97br42pffdhb307q"))

;; G4ENSDFSTATE
;;
(define g4ensdfstate-2.3
  (geant4-dataset "G4ENSDFSTATE" "2.3"
                  "00wjir59rrrlk0a12vi8rsnhyya71rdi1kmark9sp487hbhcai4l"))

;; G4TENDL
;;
(define g4tendl-1.4
  (geant4-dataset "G4TENDL" "1.4"
                  "1q11jxfy5kjwb0jrvwv6dgdxr3h85s6g2bl9kdbfvd681h178wjb"))

;; -- Geant4 --

;; The Geant4 license has an anti-patent clause that might make it nonfree.
(define-public geant4-11-1
  (package
    (name "geant4")
    (version "11.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.cern.ch/geant4/geant4")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0w616j4w8gmmn5815pnyylm9sv0gpdicpjbvxwpyhzqlssmm2bjl"))))
    (build-system cmake-build-system)
    (inputs (list clhep-2.4.6.2 expat xerces-c)) ;xerces-c is for GDML
    (arguments
     (list
      #:configure-flags #~(list (string-append "-DCMAKE_INSTALL_PREFIX="
                                               #$output)
                                 "-DCMAKE_INSTALL_LIBDIR=lib"
                                 "-DGEANT4_BUILD_MULTITHREADED=ON"
                                 "-DGEANT4_ENABLE_TESTING=OFF"
                                 "-DGEANT4_INSTALL_DATA=OFF"
                                "-DGEANT4_USE_GDML=ON"
                                 "-DGEANT4_USE_SYSTEM_CLHEP=ON"
                                 "-DGEANT4_USE_SYSTEM_EXPAT=ON"
                                   (string-append "-DGEANT4_INSTALL_DATADIR="
                                               #$output "/share/geant4/data"))
      #:phases #~(modify-phases %standard-phases
                  (add-after 'install 'install-data
                     (lambda _
                       (let ((datadir (string-append #$output
                                                    "/share/geant4/data")))
                        (mkdir-p datadir)
                         (for-each (lambda (archive)
                                     (invoke "tar" "xvf" archive "-C" datadir))
                                   (list #$(this-package-native-input "G4NDL")
                                         #$(this-package-native-input
                                            "G4EMLOW")
                                         #$(this-package-native-input
                                            "G4PhotonEvaporation")
                                         #$(this-package-native-input
                                            "G4RadioactiveDecay")
                                         #$(this-package-native-input
                                            "G4PARTICLEXS")
                                         #$(this-package-native-input "G4PII")
                                         #$(this-package-native-input
                                            "G4RealSurface")
                                         #$(this-package-native-input
                                            "G4SAIDDATA")
                                         #$(this-package-native-input "G4ABLA")
                                         #$(this-package-native-input "G4INCL")
                                         #$(this-package-native-input
                                            "G4ENSDFSTATE")
                                         #$(this-package-native-input
                                            "G4TENDL")))))))
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

Note this package does not support visualization -- you
can use @code{geant4-vis} for that.")
    ;; (license (nonfree:nonfree "https://geant4.web.cern.ch/download/license"))
    (license (license:non-copyleft
              "https://geant4.web.cern.ch/download/license"))))

(define-public geant4-vis-11-1
  (package
    (inherit geant4-11-1)
    (name "geant4-vis")
    (inputs (modify-inputs (package-inputs geant4-11-1)
              (append qtbase-5 libxmu libxt)))
    (arguments
     (substitute-keyword-arguments (package-arguments geant4-11-1)
       ((#:configure-flags flags)
        #~(append (list (string-append "-DCMAKE_PREFIX_PATH="
                                       (string-append #$(this-package-input
                                                         "qtbase")
                                                      "/lib/cmake/Qt5"))
                        "-DGEANT4_USE_OPENGL_X11=ON" "-DGEANT4_USE_QT=ON")
                  #$flags))))
    (description
     "Geant4 is a toolkit for the simulation of the passage of particles
through matter.  Its areas of application include high energy,
nuclear and accelerator physics, as well as studies
in medical and space science.

This package supports visualization with OpenGL and Qt.")))

(define-public geant4-11-2
  (package
    (inherit geant4-11-1)
    (version "11.2.1")
    (source
     (origin
       (inherit (package-source geant4-11-1))
       (uri (git-reference
             (url "https://gitlab.cern.ch/geant4/geant4")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1pdg5pf5izd2i05w34jclg0h4hrn3s4mjnr2dvq24lspjisc9r81"))))
    (inputs (modify-inputs (package-inputs geant4-11-1)
              (replace "clhep" clhep-2.4.7.1)))
    (native-inputs (modify-inputs (package-native-inputs geant4-11-1)
                     (replace "G4EMLOW" g4emlow-8.5)
                     (replace "G4ABLA" g4abla-3.3)
                     (replace "G4INCL" g4incl-1.2)))))

(define-public geant4-vis-11-2
  (package
    (inherit geant4-11-2)
    (name "geant4-vis")
    (inputs (modify-inputs (package-inputs geant4-11-2)
              (append qtbase-5 libxmu libxt)))
    (arguments
     (substitute-keyword-arguments (package-arguments geant4-11-2)
       ((#:configure-flags flags)
        #~(append (list (string-append "-DCMAKE_PREFIX_PATH="
                                       (string-append #$(this-package-input
                                                         "qtbase")
                                                      "/lib/cmake/Qt5"))
                        "-DGEANT4_USE_OPENGL_X11=ON" "-DGEANT4_USE_QT=ON")
                  #$flags))))
    (description
     "Geant4 is a toolkit for the simulation of the passage of particles
through matter.  Its areas of application include high energy,
nuclear and accelerator physics, as well as studies
in medical and space science.

This package supports visualization with OpenGL and Qt.")))
