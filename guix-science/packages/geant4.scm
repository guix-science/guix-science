;;; Copyright © 2021 BambooGeek <nju@git.nju.edu.cn>
;;;
;;; This file is NOT part of GNU Guix.
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



(define-module (guix-science packages geant4)
  ;;#:use-module ((guix licenses) #:prefix license:) ; NO needed Licence in (guix licenses)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  
  #:use-module (gnu packages)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages graphics) ; coin3D
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages gl) ; glu
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages python)
  #:use-module (gnu packages shells) ; tcsh
  #:use-module (gnu packages perl)
  #:use-module (gnu packages fontutils)
)


(define (geant4-releases-urls pkgtarver)
  "Return a list of URLS for geant4 version." ;https://geant4-data.web.cern.ch/releases/geant4.10.07.p01.tar.gz
  (list (string-append "https://geant4-data.web.cern.ch/releases/" "geant4" "." pkgtarver ".tar.gz"))
)

(define (geant4-datasets-urls pkgtarname version)
  "Return a list of URLS for geant4 version." ;https://geant4-data.web.cern.ch/datasets/G4SAIDDATA.2.0.tar.gz
  (list (string-append "https://geant4-data.web.cern.ch/datasets/" pkgtarname "." version ".tar.gz"))
)



;; start define geant4-datasets

(define (geant4-datasets pkgtarname version hash)
  (package
    (name (string-append "Geant4-" pkgtarname))
    (version version)
    (source (origin
              (method url-fetch)
              (uri (geant4-datasets-urls pkgtarname version))
              (sha256 (base32 hash))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `(("../" "./" #:exclude ("environment-variables"))) )
    )
    (synopsis (string-append "Dataset " pkgtarname " for Geant4 toolkit"))
    (description (string-append "Dataset " pkgtarname " for Geant4, a toolkit for the simulation of the passage of particles through matter."))
    (home-page "https://geant4.web.cern.ch/")
    ;;(license license:g4sl1.0) ; to be added in licences
))



(define-public geant4-g4ndl-4.6 (geant4-datasets ; 500-600 MiB
  "G4NDL" "4.6"
  "022l2jjhi57frfdv9nk6s6q23gmr9zkix06fmni8gf0gmvr7qa4x"))

(define-public geant4-g4ndl geant4-g4ndl-4.6)


(define-public geant4-g4emlow-7.13 (geant4-datasets ; 200-300 MiB
  "G4EMLOW" "7.13"
  "0scczd4ismvd4g3vfshbvwv92bzkdjz0ma7y21n6qxxy96v9cj1p"))

(define-public geant4-g4emlow geant4-g4emlow-7.13)


(define-public geant4-g4photonevaporation-5.7  (geant4-datasets ; 10 MiB
  "G4PhotonEvaporation" "5.7"
  "1rg7fygfxx06h98ywlci6b0b9ih74q8diygr76c3vppxdzjl47kn"))

(define-public geant4-g4photonevaporation geant4-g4photonevaporation-5.7)


(define-public geant4-g4radioactivedecay-5.6 (geant4-datasets ; 1 MiB
  "G4RadioactiveDecay" "5.6"
  "1w8d9zzc4ss7sh1f8cxv5pmrx2b74p1y26377rw9hnlfkiy0g1iq")) 

(define-public geant4-g4radioactivedecay geant4-g4radioactivedecay-5.6)


(define-public geant4-g4saiddata-2.0 (geant4-datasets ; 0.05 MiB
  "G4SAIDDATA" "2.0"
  "149fqy801n1pj2g6lcai2ziyvdz8cxdgbfarax6y8wdakgksh9hx"))

(define-public geant4-g4saiddata geant4-g4saiddata-2.0)


(define-public geant4-g4particlexs-3.1.1 (geant4-datasets ; 10 MiB
  "G4PARTICLEXS" "3.1.1"
  "1nmgy8w1s196php7inrkbsi0f690qa2dsyj9s1sp75mndkfpxhb6"))

(define-public geant4-g4particlexs geant4-g4particlexs-3.1.1)


(define-public geant4-g4abla-3.1 (geant4-datasets ; 0.1 MiB
  "G4ABLA" "3.1"
  "1v97q28g1xqwnav0lwzwk7hc3b87yrmbvkgadf4bkwcbnm9b163n"))

(define-public geant4-g4abla geant4-g4abla-3.1)


(define-public geant4-g4incl-1.0 (geant4-datasets ; 0.1 MiB
  "G4INCL" "1.0"
  "0z9nqk125vvf4f19lhgb37jy60jf9zrjqg5zbxbd1wz93a162qbi"))

(define-public geant4-g4incl geant4-g4incl-1.0)


(define-public geant4-g4pii-1.3 (geant4-datasets
  "G4PII" "1.3"
  "09p92rk1sj837m6n6yd9k9a8gkh6bby2bfn6k0f3ix3m4s8as9b2"))

(define-public geant4-g4pii geant4-g4pii-1.3)


(define-public geant4-g4ensdfstate-2.3 (geant4-datasets 
  "G4ENSDFSTATE" "2.3"
  "00wjir59rrrlk0a12vi8rsnhyya71rdi1kmark9sp487hbhcai4l"))

(define-public geant4-g4ensdfstate geant4-g4ensdfstate-2.3)


(define-public geant4-g4realsurface-2.2 (geant4-datasets ; optional
  "G4RealSurface" "2.2"
  "08382y1258ifs7nap6zaaazvabg72blr0dkqgwk32lrg07hdwm4r"))

(define-public geant4-g4realsurface geant4-g4realsurface-2.2)


(define-public geant4-g4tendl-1.3.2 (geant4-datasets ; optional
  "G4TENDL" "1.3.2"
  "0jdqmz3rz5a7yrq1mli6dj3bnmn73igf4fdxwfbl3rxywg38fa9v"))

(define-public geant4-g4tendl geant4-g4tendl-1.3.2)



;; start define geant4-releases using Qt5

(define (geant4-releases pkgtarver dispver hash)
  (package
    (name "Geant4")
    (version dispver)
    (source (origin
              (method url-fetch)
              (uri (geant4-releases-urls pkgtarver))
              (sha256 (base32 hash))))
    (build-system cmake-build-system)
    (arguments `(
      #:tests? #f
      #:configure-flags `(
        "-DGEANT4_INSTALL_DATA=OFF" ; default OFF, enforce to ensure reproducity
        "-DGEANT4_INSTALL_DATASETS_TENDL=OFF" ; default OFF
        "-DGEANT4_BUILD_MULTITHREADED=ON" ; default OFF on non-unix-like system
        "-DGEANT4_USE_INVENTOR_QT=OFF" ; default OFF, needs Coin3D+SoQt, Qt5:lib+headers
        "-DGEANT4_USE_QT=ON" ; default OFF, needs Qt5, OpenGL
        "-DGEANT4_USE_SYSTEM_CLHEP=OFF"
        "-DGEANT4_USE_SYSTEM_EXPAT=OFF" ; default ON, see 'expat' below in (inputs) for reason
        "-DGEANT4_USE_SYSTEM_ZLIB=OFF"
        "-DGEANT4_USE_TBB=ON" ; needs TBB
        "-DGEANT4_USE_OPENGL_X11=ON" ; default OFF
        "-DGEANT4_USE_OPENGL_WIN32=OFF" ; default OFF
        "-DGEANT4_INSTALL_PACKAGE_CACHE=ON" ; default ON, may be set to OFF when packaging
        ;;"-DGEANT4_INSTALL_DATADIR=share" ; default CMAKE_INSTALL_DATAROOTDIR 
        ;;"-DCMAKE_INSTALL_DATAROOTDIR=share" ; default share
        ;; need data under /gnu/store/xxx...xxx-geant4-10.07.p01/share/Geant4-10.7.1/data
        ) ; end of configure-flags
      #:phases
      (modify-phases %standard-phases
          (add-before 'configure 'copy-datasets
          (lambda* (#:key inputs outputs (name ,name) (version ,version) #:allow-other-keys) 
            (let* (
                  (outdir (assoc-ref outputs "out"))
                  (datadir (string-append outdir "/share/" name "-" version "/data"))
                  ) ; end of let ( bindings
              (mkdir-p (dirname datadir))
              (copy-recursively (assoc-ref inputs "geant4-g4ndl")   datadir)
              (copy-recursively (assoc-ref inputs "geant4-g4emlow") datadir)
              (copy-recursively (assoc-ref inputs "geant4-g4photonevaporation") datadir)
              (copy-recursively (assoc-ref inputs "geant4-g4radioactivedecay")  datadir)
              (copy-recursively (assoc-ref inputs "geant4-g4saiddata")    datadir)
              (copy-recursively (assoc-ref inputs "geant4-g4particlexs")  datadir)
              (copy-recursively (assoc-ref inputs "geant4-g4abla")  datadir)
              (copy-recursively (assoc-ref inputs "geant4-g4incl")  datadir)
              (copy-recursively (assoc-ref inputs "geant4-g4pii")   datadir)
              (copy-recursively (assoc-ref inputs "geant4-g4ensdfstate")  datadir)
              (copy-recursively (assoc-ref inputs "geant4-g4realsurface") datadir)
              (copy-recursively (assoc-ref inputs "geant4-g4tendl") datadir)
            #t) ; end of (let bodys under lambda
            ) ; end of (lambda
          )) ; end of (modify-phases (add-before
      )) ; end of arguments
    (inputs `(
      ;;("expat" ,expat) ; use Geant4 built-in expat, 'expat' imported from both (gix licenses) and (gnu packages xml) conflicts
      ("geant4-g4ndl" ,geant4-g4ndl) 
      ("geant4-g4emlow" ,geant4-g4emlow)
      ("geant4-g4photonevaporation" ,geant4-g4photonevaporation)
      ("geant4-g4radioactivedecay" ,geant4-g4radioactivedecay)
      ("geant4-g4saiddata" ,geant4-g4saiddata) 
      ("geant4-g4particlexs" ,geant4-g4particlexs)
      ("geant4-g4abla" ,geant4-g4abla)
      ("geant4-g4incl" ,geant4-g4incl)
      ("geant4-g4pii" ,geant4-g4pii)
      ("geant4-g4ensdfstate" ,geant4-g4ensdfstate)
      ("geant4-g4realsurface" ,geant4-g4realsurface)
      ("geant4-g4tendl" ,geant4-g4tendl)
      )) ; end of inputs
    (native-inputs `(
      ("pkg-config" ,pkg-config)
      )) ; end of native-inputs
    (propagated-inputs `(
      ("tbb" ,tbb) ("glu" ,glu) ("xerces-c" ,xerces-c) 
      ("tcsh" ,tcsh) ("perl" ,perl)
      ("python3" ,python-3) ("python2" ,python-2)
      ("qtbase" ,qtbase) ("qttools" ,qttools) ("qtdatavis3d" ,qtdatavis3d) 
      ("qtx11extras" ,qtx11extras) ("qtdeclarative" ,qtdeclarative)
      ("libqglviewer" ,libqglviewer) ; Qt3D?
      ("wget" ,wget) ("freetype" ,freetype)
      ;;("expat" ,expat) ; 'expat' imported from both (gix licenses) and (gnu packages xml)
      ("soqt" ,soqt)  ("coin3D" ,coin3D-4) ; coin3D-v4
      ("libx11" ,libx11) ("libxmu" ,libxmu) ("libxt" ,libxt) ("libxau" ,libxau) ("libxaw" ,libxaw)
      ("libxfont" ,libxfont) ("xtrans" ,xtrans)
      )) ; end of propagated
    (synopsis "Geant4 toolkit from CERN")
    (description "Geant4 is a toolkit for the simulation of the passage of particles through matter.")
    (home-page "https://geant4.web.cern.ch/")
    ;;(license license:g4sl1.0) ; to be added in (guix-science licences 
    ;; Geant4 Software License Version 1.0,  28 June 2006
    ;; Copyright (c) Copyright Holders of the Geant4 Collaboration, 1994-2006.
    ;; See http://cern.ch/geant4/license for details on the copyright holders.
    ;; All rights not expressly granted under this license are reserved.
    ;; This software includes voluntary contributions made to Geant4. See http://cern.ch/geant4 for more information on Geant4.
)) ; end of define geant4-releases



(define-public geant4-10.7.1 ; https://geant4-data.web.cern.ch/releases/geant4.10.07.p01.tar.gz
  (geant4-releases "10.07.p01" "10.7.1"
  "07if874aljizkjyp21qj6v193pmyifyfmwi5kg8jm71x79sn2laj"))

(define-public geant4-10.7  
  (geant4-releases "10.07" "10.7"
  "0jmdxb8z20d4l6sf2w0gk9ska48kylm38yngy3mzyvyj619a8vkp"))

(define-public geant4-stable geant4-10.7.1)
