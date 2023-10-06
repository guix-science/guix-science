;;; Copyright © 2016-2021 Roel Janssen <roel@gnu.org>
;;; Copyright © 2022, 2023 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages bioconductor)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages django)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages java)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system r)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix-science packages cran)
  #:use-module (guix-science packages grid-engine))

(define-public spades
  (package
   (name "spades")
   (version "3.15.2")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "http://cab.spbu.ru/files/release"
                  version "/SPAdes-" version ".tar.gz"))
            (sha256
             (base32 "03cxz4m1n4rc81lqb4p1pz2ammms7f31wvi4daywfkc13aal6fz9"))))
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
    (list bzip2
          zlib
          perl
          python-2))
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
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/mateidavid/fast5")
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "1dsq3x1662ck1bcmcmqhblnhmypfppgysblgj2xr4lr6fl4si4pk"))))
   (build-system pyproject-build-system)
   (arguments
    (list
     #:tests? #f ;There are no tests.
     #:phases
     #~(modify-phases %standard-phases
         (add-after 'unpack 'use-system-hdf5
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (setenv "HDF5_INCLUDE_DIR"
                     (string-append #$(this-package-input "hdf5") "/include"))
             (setenv "HDF5_LIB_DIR"
                     (string-append #$(this-package-input "hdf5") "/lib"))))
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "python"))))))
   (inputs
    (list hdf5 python-cython))
   (propagated-inputs
    (list python-dateutil))
   (home-page "https://github.com/mateidavid/fast5")
   (synopsis "Library for accessing Oxford Nanopore sequencing data")
   (description "This package provides a lightweight C++ library for accessing
Oxford Nanopore Technologies sequencing data.")
   (license license:expat)))

(define-public fastqc-bin
  (package
    (name "fastqc")
    (version "0.11.9")
    (source (origin
      (method url-fetch)
      (uri (string-append
            "http://www.bioinformatics.babraham.ac.uk/projects/fastqc/fastqc_v"
            version ".zip"))
      (sha256
       (base32 "0s8k6ac68xx6bsivkzcc4qcb57shav2wl5xp4l1y967pdqbhll8m"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; No tests for binary release.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; No configure phase for binary release.
         (delete 'build) ; No build phase for binary release.
         (replace 'install
           (lambda _
             (let* ((out (assoc-ref %outputs "out"))
                    (bin (string-append out "/bin"))
                    (create-and-copy
                     (lambda (dir)
                       (mkdir (string-append bin "/" dir))
                       (copy-recursively dir (string-append bin "/" dir)))))
               (install-file "cisd-jhdf5.jar" bin)
               (install-file "jbzip2-0.9.jar" bin)
               (install-file "sam-1.103.jar" bin)
               (map create-and-copy '("net" "org" "uk" "Templates" "Help"
                                      "Configuration"))
               (install-file "fastqc" bin)
               ;; Make the script executable.
               (chmod (string-append bin "/fastqc") #o555)))))))
    (propagated-inputs
     `(("perl" ,perl) ; Used for a runner script for the Java program.
       ("jdk" ,icedtea-7)))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "http://www.bioinformatics.babraham.ac.uk/projects/fastqc/")
    (synopsis "A quality control tool for high throughput sequence data")
    (description
     "FastQC aims to provide a QC report which can spot problems which originate
either in the sequencer or in the starting library material.  It can either run
as a stand alone interactive application for the immediate analysis of small
numbers of FastQ files, or it can be run in a non-interactive mode where it
would be suitable for integrating into a larger analysis pipeline for the
systematic processing of large numbers of files.")
    ;; FastQC is licensed GPLv3+, but one of its dependencies (JHDF5) is
    ;; licensed ASL2.0.
    (license (list license:gpl3+ license:asl2.0))))

(define-public freec
  (package
    (name "freec")
    (version "11.6b")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/BoevaLab/FREEC")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "10ipf49b5ybsphi00hx626v1w5abc1nkizkivy9ckwsabswxvhfq"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-before 'build 'move-to-src-dir
            (lambda _
              (chdir "src")))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((bin (string-append #$output "/bin"))
                    (share (string-append #$output "/share/freec")))
                (mkdir-p bin)
                (mkdir-p share)
                (copy-recursively "../scripts" share)
                (install-file "freec" bin)))))))
    (inputs
     (list perl))
    (propagated-inputs
     (list r-rtracklayer))
    (home-page "http://bioinfo-out.curie.fr/projects/freec/")
    (synopsis "Tool for detection of copy-number changes and allelic imbalances
(including LOH) using deep-sequencing data")
    (description "Control-FREEC automatically computes, normalizes, segments
copy number and beta allele frequency (BAF) profiles, then calls copy number
alterations and LOH.  The control (matched normal) sample is optional for whole
genome sequencing data but mandatory for whole exome or targeted sequencing
data.  For whole genome sequencing data analysis, the program can also use
mappability data (files created by GEM).")
    (license license:gpl2+)))

;; This is a dependency for strelka.
(define-public codemin
  (package
   (name "codemin")
   (version "1.0.5")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/Illumina/strelka/raw/"
                  "5a993884687f2d92f794109e171d0bdeb95e504d"
                  "/redist/CodeMin-1.0.5.tar.bz2"))
            (sha256
             (base32 "1y8wsli1q626i80p3dmrc65p77ch164hj2sbxv497i9y89kvk35s"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; There are no tests.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (delete 'build)
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((include-dir (string-append
                                (assoc-ref outputs "out") "/include")))
              (mkdir-p include-dir)
              (copy-recursively "include" include-dir)))))))
   (home-page "https://github.com/Illumina/strelka/tree/master/redist")
   (synopsis "Set of lightweight minimization functions.")
   (description "The CodeMin minimization library provides a set of lightweight
minimization functions originally developed for the CodeAxe phylogenetic
analysis package.")
   ;; MIT license.
   (license license:expat)))

;; Depends on r-diagrammer, which is not in Guix proper yet due to
;; minified JavaScript.
(define-public r-nichenetr
  (package
    (name "r-nichenetr")
    (version "2.0.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/saeyslab/nichenetr")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "08d4y99kxv9lgsh39qvg24x2h7rwbhda907cz76k46lq49yyny44"))))
    (properties `((upstream-name . "nichenetr")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-caret
           r-catools
           r-circlize
           r-complexheatmap
           r-cowplot
           r-data-table
           r-diagrammer         ;<--- this is why this package is here
           r-dicekriging
           r-dplyr
           r-e1071
           r-emoa
           r-fdrtool
           r-ggforce
           r-ggnewscale
           r-ggplot2
           r-ggpubr
           r-hmisc
           r-igraph
           r-limma
           r-magrittr
           r-matrix
           r-mlrmbo
           r-parallelmap
           r-purrr
           r-randomforest
           r-readr
           r-rocr
           r-seurat
           r-shadowtext
           r-tibble
           r-tidyr))
    (native-inputs (list r-knitr))
	(home-page "https://github.com/saeyslab/nichenetr")
    (synopsis "R implementation of the NicheNet method")
    (description
     "The goal of NicheNet is to study intercellular communication
from a computational perspective.  NicheNet uses human or mouse gene
expression data of interacting cells as input and combines this with a
prior model that integrates existing knowledge on ligand-to-target
signaling paths.  This allows to predict ligand-receptor interactions
that might drive gene expression changes in cells of interest.")
    (license license:gpl3)))

;; This is here because r-nichenetr is also here.
(define-public r-scriabin
  (let ((commit "313d15e9150413e6bcad0947215da3a09c0257f5")
        (revision "1"))
    (package
      (name "r-scriabin")
      (version (git-version "0.0.0.9000" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/BlishLab/scriabin")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0k8khwcr3a281pa33h6l3wdk5igyqc4xknydiazk7qyf5w25g9zg"))))
      (properties `((upstream-name . "scriabin")))
      (build-system r-build-system)
      (propagated-inputs (list r-ade4
                               r-cellid
                               r-circlize
                               r-clipr
                               r-complexheatmap
                               r-complexheatmap
                               r-cowplot
                               r-dplyr
                               r-factoextra
                               r-fsa
                               r-genefilter
                               r-ggalluvial
                               r-ggfittext
                               r-ggplot2
                               r-ggsci
                               r-limma
                               r-magrittr
                               r-matrix
                               r-matrixstats
                               r-networkd3
                               r-nichenetr
                               r-pbapply
                               r-qlcmatrix
                               r-scales
                               r-scater
                               r-seurat
                               r-tibble
                               r-tidyft
                               r-wgcna))
      (native-inputs (list r-knitr))
	  (home-page "https://github.com/BlishLab/scriabin")
      (synopsis "Single-cell resolved interaction analysis through binning")
      (description
       "Scriabin aims to provide a comprehensive view of cell-cell
communication (CCC).  It achieves this without requiring subsampling
or aggregation.")
      (license license:expat))))

;; Seqan 3.0.3 removed a few deprecated features.
(define-public seqan-3.0.2
  (package
    (name "seqan")
    (version "3.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/seqan/seqan3/releases/"
                                  "download/" version "/seqan3-"
                                  version "-Source.tar.xz"))
              (sha256
               (base32
                "1s9fnvg26scm8g2z9bgx8cn6vgy185dpmyp089l4iz811k6skcds"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "ctest" "test" "--output-on-failure")))))))
    (native-inputs
     (list bzip2 cereal zlib))
    (home-page "https://www.seqan.de")
    (synopsis "Library for nucleotide sequence analysis")
    (description
     "SeqAn is a C++ library of efficient algorithms and data structures for
the analysis of sequences with the focus on biological data.  It contains
algorithms and data structures for string representation and their
manipulation, online and indexed string search, efficient I/O of
bioinformatics file formats, sequence alignment, and more.")
    (license license:bsd-3)))

(define-public strelka-2.9.2
  (package
   (name "strelka")
   (version "2.9.2")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/Illumina/strelka/releases/download/v"
                  version "/strelka-" version ".release_src.tar.bz2"))
            (sha256
             (base32 "19bq2wzlxmnv8rx112y8z0sfvgsajnd0m945njmfy9p170qjqr27"))
            (patches
             (list (search-patch "strelka2-unbundle-dependencies.patch")))))
   (build-system cmake-build-system)
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'unbundle-dependencies
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (substitute* "redist/CMakeLists.txt"
              ;; HTSlib
              (("superset\\(HTSLIB_DIR \"\\$\\{CMAKE_CURRENT_BINARY_DIR\\}/\\$\\{HTSLIB_PREFIX\\}\"\\)")
               (format #f "superset(HTSLIB_DIR \"~a/bin\")" (assoc-ref inputs "htslib")))
              (("superset\\(HTSLIB_LIBRARY \"\\$\\{HTSLIB_DIR\\}/libhts.a\"\\)")
               (format #f "superset(HTSLIB_LIBRARY \"~a/lib/libhts.so\")"
                       (assoc-ref inputs "htslib")))
              ;; SAMtools
              (("set\\(SAMTOOLS_DIR \"\\$\\{CMAKE_CURRENT_BINARY_DIR}/\\$\\{SAMTOOLS_PREFIX\\}\"\\)")
               (format #f "set(SAMTOOLS_DIR \"~a/bin\")"
                       (assoc-ref inputs "samtools")))
              (("set\\(SAMTOOLS_LIBRARY \"\\$\\{SAMTOOLS_DIR\\}/libbam.a\"\\)")
               (format #f "set(SAMTOOLS_LIBRARY \"~a/lib/libbam.a\")"
                       (assoc-ref inputs "samtools"))))))
        (add-after 'install 'install-shared-libraries
          (lambda* (#:key inputs outputs  #:allow-other-keys)
            (let ((libdir (string-append (assoc-ref outputs "out") "/lib")))
              (mkdir-p libdir)
              (map (lambda (file)
                     (copy-file file (string-append libdir "/" (basename file))))
                   (find-files "." "\\.so")))))
        (add-after 'install 'patch-python-bin
          (lambda* (#:key inputs outputs  #:allow-other-keys)
            (let ((patch-path (string-append (assoc-ref outputs "out") "/lib/python")))
              (substitute* (list (string-append patch-path "/makeRunScript.py")
                                 (string-append patch-path "/pyflow/pyflow.py"))
                (("/usr/bin/env python")
                 (string-append (assoc-ref inputs "python") "/bin/python")))))))))
   (inputs
    `(("boost" ,boost)
      ("perl" ,perl)
      ("bash" ,bash)
      ("zlib" ,zlib)
      ("samtools" ,samtools)
      ("rapidjson" ,rapidjson)
      ("codemin" ,codemin)
      ("curl" ,curl)
      ("xz" ,xz)
      ("openssl" ,openssl)
      ("samtools" ,samtools)
      ("zlib" ,zlib)
      ("python" ,python)))
   (native-inputs
    `(("bash" ,bash)
      ("python" ,python-2)
      ("doxygen" ,doxygen)
      ("graphviz" ,graphviz)))
   (propagated-inputs
    `(("vcftools" ,vcftools)
      ("htslib" ,htslib)))
   (native-search-paths (package-native-search-paths perl))
   (home-page "https://github.com/Illumina/strelka")
   (synopsis "Small variant caller")
   (description "Strelka2 is a fast and accurate small variant caller optimized
for analysis of germline variation in small cohorts and somatic variation in
tumor/normal sample pairs.  The germline caller employs an efficient tiered
haplotype model to improve accuracy and provide read-backed phasing, adaptively
selecting between assembly and a faster alignment-based haplotyping approach at
each variant locus.  The germline caller also analyzes input sequencing data
using a mixture-model indel error estimation method to improve robustness to
indel noise.  The somatic calling model improves on the original Strelka method
for liquid and late-stage tumor analysis by accounting for possible tumor cell
contamination in the normal sample.  A final empirical variant re-scoring step
using random forest models trained on various call quality features has been
added to both callers to further improve precision.")
   (license license:gpl3+)))

(define-public star-2.4.2a
  (package
    (name "star")
    (version "2.4.2a")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/alexdobin/STAR/archive/"
                                  "STAR_" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1c3rnm7r5l0kl3d04gl1g7938xqf1c2l0mla87rlplqg1hcns5mc"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "source/Makefile"
                    (("/bin/rm") "rm"))
                  ;; Remove pre-built binaries and bundled htslib sources.
                  (delete-file-recursively "bin/MacOSX_x86_64")
                  (delete-file-recursively "bin/Linux_x86_64")
                  (delete-file-recursively "source/htslib")
                  #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ;no check target
       #:make-flags '("STAR")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-source-dir
           (lambda _ (chdir "source") #t))
         (add-after 'enter-source-dir 'do-not-use-bundled-htslib
           (lambda _
             (substitute* "Makefile"
               (("(Depend.list: \\$\\(SOURCES\\) parametersDefault\\.xxd) htslib"
                 _ prefix) prefix))
             (substitute* '("BAMfunctions.cpp"
                            "signalFromBAM.h"
                            ;"bam_cat.h"
                            "bam_cat.c"
                            "STAR.cpp"
                            "bamRemoveDuplicates.cpp")
               (("#include \"htslib/([^\"]+\\.h)\"" _ header)
                (string-append "#include <" header ">")))
             (substitute* "IncludeDefine.h"
               (("\"htslib/(htslib/[^\"]+.h)\"" _ header)
                (string-append "<" header ">")))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin/")))
               (install-file "STAR" bin))
             #t))
         (delete 'configure))))
    (native-inputs
     `(("vim" ,vim))) ; for xxd
    (inputs
     `(("htslib" ,htslib)
       ("zlib" ,zlib)))
    (home-page "https://github.com/alexdobin/STAR")
    (synopsis "Universal RNA-seq aligner")
    (description
     "The Spliced Transcripts Alignment to a Reference (STAR) software is
based on a previously undescribed RNA-seq alignment algorithm that uses
sequential maximum mappable seed search in uncompressed suffix arrays followed
by seed clustering and stitching procedure.  In addition to unbiased de novo
detection of canonical junctions, STAR can discover non-canonical splices and
chimeric (fusion) transcripts, and is also capable of mapping full-length RNA
sequences.")
    ;; STAR is licensed under GPLv3 or later; htslib is MIT-licensed.
    (license license:gpl3+)))

;; Dependency of star-fusion
(define-public perl-set-intervaltree
  (package
   (name "perl-set-intervaltree")
   (version "0.10")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "mirror://cpan/authors/id/B/BE/BENBOOTH/Set-IntervalTree-"
           version ".tar.gz"))
     (sha256
      (base32 "1g1yam3fwl11wvy489yhhfzrfdlqaj1dh7pgks3myjq71p7rrgg3"))))
   (build-system perl-build-system)
   (home-page "http://search.cpan.org/dist/Set-IntervalTree")
   (synopsis "Perform range-based lookups on sets of ranges.")
   (description "")
   (license license:gpl1+)))

(define-public star-fusion
  (package
   (name "star-fusion")
   (version "1.0.0")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/STAR-Fusion/STAR-Fusion/releases/"
                  "download/v" version "/STAR-Fusion-v" version
                  ".FULL.tar.gz"))
            (sha256
             (base32 "19p5lwq2f95hgii7fdidz03845nkhf3pjfvp8v3midrsb0s6p7df"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; There is no test phase.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure) ; There is nothing to configure.
        (delete 'build) ; There is nothing to compile/build.
        (add-before 'install 'patch-external-tools
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((samtools (string-append (assoc-ref inputs "samtools") "/bin/samtools"))
                  (gunzip (string-append (assoc-ref inputs "gzip") "/bin/gunzip"))
                  (zcat (string-append (assoc-ref inputs "gzip") "/bin/zcat"))
                  (cat (string-append (assoc-ref inputs "coreutils") "/bin/cat"))
                  (wc (string-append (assoc-ref inputs "coreutils") "/bin/wc"))
                  (sort (string-append (assoc-ref inputs "coreutils") "/bin/sort"))
                  (mkdir (string-append (assoc-ref inputs "coreutils") "/bin/mkdir")))
              (substitute* "util/append_breakpoint_junction_info.pl"
                (("samtools") samtools))
              (substitute* "util/incorporate_FFPM_into_final_report.pl"
                (("gunzip") gunzip))
              (substitute* "util/STAR-Fusion.predict" (("gunzip") gunzip))
              (substitute* "util/incorporate_FFPM_into_final_report.pl" (("wc") wc))
              (substitute* "util/convert_to_FFPM.pl" (("wc") wc))
              (substitute* "util/incorporate_FFPM_into_final_report.pl"
                (("cat \\$fq_file") (string-append cat " $fq_file")))
              (substitute* "util/partition_FUSION_EVIDENCE_fastqs_by_fusion.pl"
                (("sort \\$tmp_paired") (string-append sort " $tmp_paired")))
              (substitute* "util/convert_to_FFPM.pl"
                (("\"cat \\$fq_filename") (string-append "\"" cat " $fq_filename")))
              (substitute* "util/convert_to_FFPM.pl"
                (("zcat \\$fq_filename") (string-append zcat " $fq_filename")))
              (substitute* "util/partition_FUSION_EVIDENCE_fastqs_by_fusion.pl"
                (("mkdir") mkdir))
              (substitute* "util/STAR-Fusion.filter" (("mkdir") mkdir))
              (substitute* "util/STAR-Fusion.predict" (("mkdir") mkdir)))))
        (replace 'install
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
              (mkdir-p bin)
              (install-file "STAR-Fusion" bin)
              (copy-recursively "PerlLib" (string-append bin "/PerlLib"))
              (copy-recursively "util" (string-append bin "/util"))
              (copy-recursively "FusionFilter"
                                (string-append bin "/FusionFilter"))))))))
   (inputs
    `(("perl" ,perl)
      ("samtools" ,samtools)
      ("coreutils" ,coreutils)
      ("gzip" ,gzip)))
   (propagated-inputs
    `(("perl-carp" ,perl-carp)
      ("perl-pathtools" ,perl-pathtools)
      ("perl-db-file" ,perl-db-file)
      ("perl-uri" ,perl-uri)
      ("perl-set-intervaltree" ,perl-set-intervaltree)))
   (home-page "https://github.com/STAR-Fusion/STAR-Fusion/")
   (synopsis "Fusion detection based on STAR")
   (description "This package provides a component of the Trinity Cancer
Transcriptome Analysis Toolkit (CTAT).  It uses the STAR aligner to identify
candidate fusion transcripts supported by Illumina reads.  It further
processes the output generated by the STAR aligner to map junction reads and
spanning reads to a reference annotation set.")
   (license license:bsd-3)))

(define-public primer3-1.1.4
  (package
    (name "primer3")
    (version "1.1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/primer3-org/primer3/archive/v"
                    version ".tar.gz"))
              (sha256
               (base32
                "1nkxyw811xbb7gid0dbcw4k7yg3q1mw6hv96076xx0j10ishmh1w"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'change-directory
           (lambda _ (chdir "src")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (mkdir-p bin)
               (for-each (lambda (file) (install-file file bin))
                         '("primer3_core" "oligotm" "ntdpal"))))))))
    (inputs
     `(("perl" ,perl)))
    (home-page "https://github.com/primer3-org/primer3")
    (synopsis "Tool to design PCR primers")
    (description "Design PCR primers from DNA sequence.  From mispriming
libraries to sequence quality data to the generation of internal oligos,
primer3 does it.")
    (license license:gpl2)))

;; The submodules contain JavaScript that we don't build from source.
(define-public python-anvio
  (package
    (name "python-anvio")
    (version "7.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/merenlab/anvio")
                    (commit (string-append "v" version))
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0c41gni87ian6p9vdzp0f8j1w7w5n3r7xabbgwkw83v26wybqvrw"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* "requirements.txt"
               ;; This is questionable.  Pandas 0.25 is really quite old.  Using
               ;; version 1.4.x is a gamble.
               (("pandas==.*") "pandas\n")
               ;; We use the same major version; should be fine.
               (("snakemake==5.*") "snakemake\n")))))
      #:test-flags
      ;; These fail because the test files are not in the expected directory.
      '(list "-k" "not test_fasta_splitting \
and not test_more_parts_than_sequences \
and not test_single_fasta_gives_one_split")))
    (propagated-inputs
     (list python-bottle
           python-colored
           python-django
           python-ete3
           python-illumina-utils
           python-matplotlib
           python-mistune
           python-multiprocess
           python-numba
           python-numpy
           python-pandas
           python-paste
           python-plotext
           python-psutil
           python-pyani
           python-pysam
           python-requests
           python-scikit-learn
           python-scipy
           python-six
           python-statsmodels
           python-tabulate
           snakemake))
    (native-inputs (list python-pytest))
    (home-page "https://anvio.org")
    (synopsis "Analysis and visualization platform for 'omics data")
    (description
     "Anvi’o is a comprehensive platform that brings together many aspects of
today’s computational strategies of data-enabled microbiology, including
genomics, metagenomics, metatranscriptomics, pangenomics, metapangenomics,
phylogenomics, and microbial population genetics in an integrated and
easy-to-use fashion through extensive interactive visualization
capabilities.")
    (license license:gpl3+)))

(define-public gnomad-sv-sites-2.1
  (package
   (name "gnomad-sv-sites")
   (version "2.1")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://storage.googleapis.com/gnomad-public/"
                  "papers/2019-sv/gnomad_v" version "_sv.sites.vcf.gz"))
            (sha256
             (base32
              "18gxfnar8n5r06mj0ykyq4fkw3q3qqbrfnprgi18db0xzf6lh94k"))))
   (build-system trivial-build-system)
   (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((gzip     (string-append (assoc-ref %build-inputs "gzip") "/bin/gzip"))
               (sv-sites (assoc-ref %build-inputs "source"))
               (out      (string-append %output "/share/gnomad")))
           (mkdir-p out)
           (with-directory-excursion out
             (zero? (system
                     (string-append
                      gzip " -d " sv-sites
                      " -c > gnomad_v2.1_sv.sites.vcf"))))))))
   (inputs `(("gzip" ,gzip)))
   (home-page "https://gnomad.broadinstitute.org")
   (synopsis "gnomAD structural variant sites")
   (description "This package provides in uncompressed version of the gnomAD
 structural variant sites.")
   (license license:cc0)))

(define-public sharc
  (package
   (name "sharc")
   (version "1.0-slurm")
   (source (origin
            (method url-fetch)
            (uri "https://github.com/UMCUGenetics/SHARC/archive/1.0.tar.gz")
            (file-name (string-append name "-" version ".tar.gz"))
            (sha256
             (base32
              "05121h8nrrsd4j9xk0dg92p1js6m849x8p2vj5mss1fzf50cdyv7"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; There are no tests
      #:phases
      (modify-phases %standard-phases
                     (delete 'configure)
                     (delete 'build)
                     (add-after 'unpack 'patch-external-programs
                      (lambda* (#:key inputs outputs #:allow-other-keys)
                        (let* ((out        (assoc-ref outputs "out"))
                               (share      (string-append out "/share/sharc"))
                               (venvdir    (string-append share "/venv/bin"))
                               (scriptsdir (string-append share "/scripts"))
                               (primerdir  (string-append scriptsdir "/primers"))
                               (stepsdir   (string-append share "/steps"))
                               (filesdir   (string-append share "/files")))
                          (substitute* "sharc.sh"
                           (("/hpc/cog_bioinf/cuppen/personal_data/jvalleinclan/tools_kloosterman/minimap2_v2.12/minimap2")
                            (string-append (assoc-ref inputs "minimap2") "/bin/minimap2"))
                           (("SHARCDIR=\\$\\(dirname \\$\\{BASH_SOURCE\\[0\\]\\}\\)")
                            (string-append "SHARCDIR='" out "'"))
                           (("VENV=\\$SHARCDIR/venv/bin/activate")
                            (string-append "VENV=" venvdir "/activate"))
                           (("STEPSDIR=\\$SHARCDIR/steps")
                            (string-append "STEPSDIR=" stepsdir))
                           (("SCRIPTSDIR=\\$SHARCDIR/scripts")
                            (string-append "SCRIPTSDIR=" scriptsdir))
                           (("FILESDIR=\\$SHARCDIR/files")
                            (string-append "FILESDIR=" filesdir))
                           (("python \\$PON_SCRIPT") "PY2 $PON_SCRIPT")
                           (("\\$PRIMER_DESIGN_DIR/primer3/src/primer3_core")
                            (string-append (assoc-ref inputs "primer3") "/bin/primer3_core"))
                           (("\\$\\{FILESDIR\\}/gnomad_v2.1_sv.sites.vcf")
                            (string-append (assoc-ref inputs "gnomad-sv-sites")
                                           "/share/gnomad/gnomad_v2.1_sv.sites.vcf")))

                          (substitute* '("steps/vcf_fasta.sh"
                                         "steps/vcf_primer_filter.sh"
                                         "steps/somatic_ranking.sh"
                                         "steps/top20_report.sh"
                                         "steps/somatic_feature_selection.sh"
                                         "steps/randomForest.sh"
                                         "steps/randomForest.sh"
                                         "steps/randomForest.sh"
                                         "steps/bed_annotation.sh")
                           (("/hpc/cog_bioinf/cuppen/project_data/Jose_SHARC/sharc/scripts")
                            scriptsdir))

                          (substitute* '("steps/vcf_fasta.sh"
                                         "steps/nanosv.sh"
                                         "steps/vcf_primer_filter.sh"
                                         "steps/somatic_ranking.sh"
                                         "steps/top20_report.sh"
                                         "steps/somatic_feature_selection.sh"
                                         "steps/randomForest.sh"
                                         "steps/bed_annotation.sh")
                           (("VENV='/hpc/cog_bioinf/cuppen/project_data/Jose_SHARC/sharc/venv/bin/activate'")
                            (string-append venvdir "/activate")))

                          (substitute* '("steps/calculate_coverage.sh"
                                         "steps/nanosv.sh"
                                         "steps/somatic_feature_selection.sh")
                           (("/hpc/cog_bioinf/cuppen/project_data/Jose_SHARC/sharc/files")
                            filesdir))

                           (substitute* "steps/minimap2.sh"
                            (("/hpc/cog_bioinf/cuppen/personal_data/jvalleinclan/tools_kloosterman/minimap2_v2.12/minimap2")
                             (string-append (assoc-ref inputs "minimap2") "/bin/minimap2")))

                           (substitute* '("steps/bed_annotation.sh"
                                          "steps/calculate_coverage.sh"
                                          "steps/create_bed_annotation_jobs.sh"
                                          "steps/minimap2.sh"
                                          "steps/nanosv.sh"
                                          "steps/primer_design.sh"
                                          "steps/primer_ranking.sh"
                                          "steps/randomForest.sh"
                                          "steps/sharc_filter.sh"
                                          "steps/somatic_feature_selection.sh"
                                          "steps/somatic_ranking.sh"
                                          "steps/top20_report.sh"
                                          "steps/vcf_fasta.sh"
                                          "steps/vcf_filter.sh"
                                          "steps/vcf_primer_filter.sh"
                                          "steps/vcf_split.sh"
                                          "sharc.sh")
                            (("/hpc/local/CentOS7/cog_bioinf/sambamba_v0.6.5/sambamba")
                             (string-append (assoc-ref inputs "sambamba") "/bin/sambamba"))
                            (("#!/bin/bash")
                             (format #f "#!~a/bin/bash~%~%~{export ~:a~%~}"
                                     (assoc-ref inputs "bash")
                                     `(,(let ((python-inputs
                                               (delete #f
                                                (map (lambda (pair)
                                                       (if (string-prefix? "python-" (car pair))
                                                           (format #f "~a/lib/python~a/site-packages"
                                                                   (cdr pair) "3.8")
                                                           #f))
                                                     inputs))))
                                          (format #f "PYTHONPATH=\"~a~{:~a~}\""
                                                  (car python-inputs)
                                                  (cdr python-inputs)))
                                       ,(format #f "R_LIBS_SITE=~s" (getenv "R_LIBS_SITE")))))
                            (("Rscript")
                             (string-append (assoc-ref inputs "r") "/bin/Rscript"))
                            (("qsub")
                             (string-append (assoc-ref inputs "grid-engine-core") "/bin/qsub -V"))
                            (("python ")
                             (string-append (assoc-ref inputs "python") "/bin/python3 "))
                            (("PY2")
                             (string-append (assoc-ref inputs "python-2") "/bin/python"))
                            (("NanoSV ")
                             (string-append (assoc-ref inputs "nanosv") "/bin/NanoSV "))
                            (("module load R") ""))

                           (substitute* "steps/create_bed_annotation_jobs.sh"
                            (("bash \\$STEPSDIR")
                             (string-append (assoc-ref inputs "bash") "/bin/bash $STEPSDIR")))

                           (substitute* "scripts/run_randomForest.R"
                            (("/hpc/cog_bioinf/kloosterman/common_scripts/sharc/scripts")
                             scriptsdir)
                            (("randomforest_vl_v3_3overlap_p96_r99.5_pc0.39.Rdata")
                             "randomforest_v3_3overlap_p96_r99.5_pc0.39.Rdata"))

                           ;; Use Guix's Python.
                           (substitute* '("scripts/add_predict_annotation.py"
                                          "scripts/create_features_table.py"
                                          "scripts/get_closest_feature.py"
                                          "scripts/primer_ranking.py"
                                          "scripts/somatic_feature_selection.py"
                                          "scripts/somatic_ranking.py"
                                          "scripts/top20_report.py"
                                          "scripts/vcf_primer_filter.py"
                                          "scripts/vcf_to_fasta.py")
                            (("/usr/bin/python") (string-append
                                                  (assoc-ref inputs "python")
                                                  "/bin/python3")))

                           (substitute* '("scripts/primers/primerBATCH1"
                                          "scripts/primers/amplicons3.pl"
                                          "scripts/primers/format_primers1.pl")
                            (("/usr/bin/perl")
                             (string-append (assoc-ref inputs "perl") "/bin/perl")))

                           (substitute* "scripts/annotate_sv_vcf_file.py"
                            (("/usr/bin/python")
                             (string-append
                              (assoc-ref inputs "python-2")
                              "/bin/python")))

                           (substitute* "scripts/primers/primerBATCH1"
                            (("/hpc/cuppen/projects/TP0001_General/COLO/analysis/jvalleinclan/bin/tools_kloosterman/primer3/primers")
                             primerdir))

                           (substitute* "scripts/primers/amplicons3.pl"
                            (("eprimer3 ")
                             (string-append (assoc-ref inputs "emboss")
                                            "/bin/eprimer3 ")))

                           #t)))

                     (replace 'install
                      (lambda* (#:key inputs outputs #:allow-other-keys)
                        (let* ((out        (assoc-ref outputs "out"))
                               (bin        (string-append out "/bin"))
                               (share      (string-append out "/share/sharc"))
                               (venvdir    (string-append share "/venv/bin"))
                               (scriptsdir (string-append share "/scripts"))
                               (stepsdir   (string-append share "/steps"))
                               (filesdir   (string-append share "/files")))
                          (mkdir-p bin)
                          (mkdir-p venvdir)
                          (mkdir-p scriptsdir)
                          (mkdir-p stepsdir)
                          (mkdir-p filesdir)
                          (copy-recursively "scripts" scriptsdir)
                          (copy-recursively "steps" stepsdir)
                          (copy-recursively "files" filesdir)

                          ;; Create an empty virtual environment
                          (call-with-output-file (string-append venvdir "/activate")
                            (lambda (port)
                              (format port "export DEACTIVATE_PATH=$PATH~%")
                              (format port "export PATH=$PATH:~s~%" venvdir)
                              (format port "printf \"Environment activated.\\n\";~%")))
                          (let ((deactivate (string-append venvdir "/deactivate")))
                            (call-with-output-file deactivate
                              (lambda (port)
                                (format port "#!~a/bin/bash~%" (assoc-ref inputs "bash"))
                                (format port "export PATH=${DEACTIVATE_PATH}~%")
                                (format port "printf \"Environment deactivated.\\n\";~%exit 0;~%")))
                            (chmod deactivate #o555))
                          (install-file "sharc.sh" bin)
                          (with-directory-excursion bin
                                                    (symlink "sharc.sh" "sharc"))))))))
   (inputs
    `(("awk" ,gawk)
      ("bash" ,bash)
      ("coreutils" ,coreutils)
      ("emboss" ,emboss)
      ("grep" ,grep)
      ("grid-engine-core" ,qsub-slurm)
      ("minimap2" ,minimap2)
      ("primer3" ,primer3-1.1.4)
      ("perl" ,perl)
      ("python" ,python)
      ("python-aniso8601" ,python-aniso8601)
      ("python-certifi" ,python-certifi)
      ("python-chardet" ,python-chardet)
      ("python-configparser" ,python-configparser)
      ("python-flask" ,python-flask)
      ("python-flask-restful" ,python-flask-restful)
      ("python-idna" ,python-idna)
      ("python-itsdangerous" ,python-itsdangerous)
      ("python-jinja2" ,python-jinja2)
      ("python-markupsafe" ,python-markupsafe)
      ("nanosv" ,nanosv)
      ("python-pymongo" ,python-pymongo)
      ("python-pysam" ,python-pysam)
      ("python-pytz" ,python-pytz)
      ("python-pyvcf" ,python-pyvcf)
      ("python-requests" ,python-requests)
      ("python-six" ,python-six)
      ("python-urllib3" ,python-urllib3)
      ("python-werkzeug" ,python-werkzeug)
      ("python-2" ,python-2)
      ("r" ,r-minimal)
      ("r-ggplot2" ,r-ggplot2)
      ("r-randomforest", r-randomforest)
      ("r-rocr" ,r-rocr)
      ("sambamba" ,sambamba)
      ("sed" ,sed)
      ("gnomad-sv-sites" ,gnomad-sv-sites-2.1)))
   (native-search-paths
    (append (package-native-search-paths bash)
            (package-native-search-paths python)
            (package-native-search-paths perl)
            (package-native-search-paths r)))
   (search-paths native-search-paths)
   (home-page "https://github.com/UMCUGenetics/SHARC")
   (synopsis "Somatic SV pipeline for tumor-only Nanopore sequencing data")
   (description "SHARC is a pipeline for somatic SV calling and filtering
from tumor-only Nanopore sequencing data. It performs mapping, SV calling,
SV filtering, random forest classification, blacklist filtering and SV
prioritization, followed by automated primer design for PCR amplicons of
80-120 bp that are useful to track cancer ctDNA molecules in liquid
biopsies.")
   (license license:gpl3)))

(define-public sharc-local
  (package (inherit sharc)
     (name "sharc")
     (version "1.0-local")
     (inputs
      `(("awk" ,gawk)
        ("bash" ,bash)
        ("coreutils" ,coreutils)
        ("emboss" ,emboss)
        ("grep" ,grep)
        ("grid-engine-core" ,qsub-local)
        ("minimap2" ,minimap2)
        ("primer3" ,primer3-1.1.4)
        ("perl" ,perl)
        ("python" ,python)
        ("python-aniso8601" ,python-aniso8601)
        ("python-certifi" ,python-certifi)
        ("python-chardet" ,python-chardet)
        ("python-configparser" ,python-configparser)
        ("python-flask" ,python-flask)
        ("python-flask-restful" ,python-flask-restful)
        ("python-idna" ,python-idna)
        ("python-itsdangerous" ,python-itsdangerous)
        ("python-jinja2" ,python-jinja2)
        ("python-markupsafe" ,python-markupsafe)
        ("nanosv" ,nanosv)
        ("python-pymongo" ,python-pymongo)
        ("python-pysam" ,python-pysam)
        ("python-pytz" ,python-pytz)
        ("python-pyvcf" ,python-pyvcf)
        ("python-requests" ,python-requests)
        ("python-six" ,python-six)
        ("python-urllib3" ,python-urllib3)
        ("python-werkzeug" ,python-werkzeug)
        ("python-2" ,python-2)
        ("r" ,r-minimal)
        ("r-ggplot2" ,r-ggplot2)
        ("r-randomforest", r-randomforest)
        ("r-rocr" ,r-rocr)
        ("sambamba" ,sambamba)
        ("sed" ,sed)
        ("gnomad-sv-sites" ,gnomad-sv-sites-2.1)))))

(define-public sharc-sge
  (package (inherit sharc)
     (name "sharc")
     (version "1.0-sge")
     (inputs
      `(("awk" ,gawk)
        ("bash" ,bash)
        ("coreutils" ,coreutils)
        ("emboss" ,emboss)
        ("grep" ,grep)
        ("grid-engine-core" ,grid-engine-core)
        ("minimap2" ,minimap2)
        ("primer3" ,primer3-1.1.4)
        ("perl" ,perl)
        ("python" ,python)
        ("python-aniso8601" ,python-aniso8601)
        ("python-certifi" ,python-certifi)
        ("python-chardet" ,python-chardet)
        ("python-configparser" ,python-configparser)
        ("python-flask" ,python-flask)
        ("python-flask-restful" ,python-flask-restful)
        ("python-idna" ,python-idna)
        ("python-itsdangerous" ,python-itsdangerous)
        ("python-jinja2" ,python-jinja2)
        ("python-markupsafe" ,python-markupsafe)
        ("nanosv" ,nanosv)
        ("python-pymongo" ,python-pymongo)
        ("python-pysam" ,python-pysam)
        ("python-pytz" ,python-pytz)
        ("python-pyvcf" ,python-pyvcf)
        ("python-requests" ,python-requests)
        ("python-six" ,python-six)
        ("python-urllib3" ,python-urllib3)
        ("python-werkzeug" ,python-werkzeug)
        ("python-2" ,python-2)
        ("r" ,r-minimal)
        ("r-ggplot2" ,r-ggplot2)
        ("r-randomforest", r-randomforest)
        ("r-rocr" ,r-rocr)
        ("sambamba" ,sambamba)
        ("sed" ,sed)
        ("gnomad-sv-sites" ,gnomad-sv-sites-2.1)))))

(define-public allelecount
  (package
    (name "allelecount")
    (version "4.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cancerit/alleleCount")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "117r2xwl7vx6axfz7wlrpdsmxylbn7vr1gi974jag7r5xsd3jlzs"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-before 'build 'chdir
            (lambda _ (chdir "perl")))
          (replace 'build
            (lambda _
              (invoke "perl" "Makefile.PL"
                      (string-append "PREFIX=" #$output))
              (invoke "make")
              ;; Build the C alleleCounter program.
              (with-directory-excursion "../c"
                (mkdir-p "bin")
                (substitute* "src/bam_access.c"
                  (("\\#include <cram\\/cram.h>") "#include <htslib/cram.h>"))
                (invoke "make"))))
          (add-after 'install 'install-allelecounter
            (lambda _
              (install-file "../c/bin/alleleCounter"
                            (string-append #$output "/bin")))))))
    (propagated-inputs
     (list perl-const-fast
           perl-sub-exporter-progressive
           perl-bio-db-hts
           bioperl-minimal))
    (inputs
     (list zlib
           htslib
           perl-pod-coverage
           perl-file-which
           perl-test-fatal
           perl-try-tiny
           samtools))
    (native-inputs
     (list perl-module-build perl))
    (home-page "https://github.com/cancerit/alleleCount")
    (synopsis "Support code for NGS copy number algorithms")
    (description "This package primarily exists to prevent code duplication
between some other projects, specifically AscatNGS and Battenburg.")
    (license license:agpl3+)))

;; libmaus and biobambam are obsolete and have newer versions in Guix proper
;; (libmaus2/biobambam2).
(define-public libmaus
  (package
   (name "libmaus")
   (version "0.0.196")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/gt1/libmaus/archive/"
                  version "-release-20150326095654.tar.gz"))
            (sha256
             (base32
              "0g92bl37ci8pzkgi2xnn2bck7y655jwcb1bm3mg42mj5lf5x2i5b"))))
   (build-system gnu-build-system)
   (native-inputs
    (list pkg-config))
   (inputs
    (list zlib))
   (home-page "https://github.com/gt1/libmaus")
   (synopsis "Collection of bioinformatics data structures and algorithms")
   (description "This package contains a collection of bioinformatics data
structures and algorithms.  It provides I/O classes, bitio classes, text
indexing classes and BAM sequence alignment functionality.")
   (license license:gpl3+)))

(define-public biobambam
  (package
   (name "biobambam")
   (version "0.0.191")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/gt1/biobambam/archive/" version
                  "-release-20150401083643.tar.gz"))
            (file-name (string-append name "-" version ".tar.gz"))
            (sha256
             (base32 "065fcwdh5sb6dg3mf5qk9w2818jxm27pvbv976qc00y7np2y2nqz"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f))
   (inputs
    (list libmaus zlib))
   (native-inputs
    (list pkg-config))
   (home-page "https://github.com/gt1/biobambam")
   (synopsis "Collection of tools to work with BAM files")
   (description "This package contains the following programs: bamcollate2,
bammarkduplicates, bammaskflags, bamrecompress, bamsort, bamtofastq.")
   (license license:gpl3+)))

(define-public pcap-core
  (package
   (name "pcap-core")
   (version "3.5.0")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/ICGC-TCGA-PanCancer/PCAP-core/archive/v"
                  version ".tar.gz"))
            (file-name (string-append name "-" version ".tar.gz"))
            (sha256
             (base32 "06im5lf00jyghwmqjzb3dpglgjx7pi5ysda75fw8ygmj1fi5q8kj"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        (replace 'configure
          (lambda* (#:key outputs #:allow-other-keys)
            (system* "perl" "Makefile.PL"
                     (string-append "PREFIX=" (assoc-ref outputs "out"))))))))
   (propagated-inputs
    `(("bwa" ,bwa)
      ("samtools" ,samtools)
      ("biobambam" ,biobambam)))
   (native-inputs
    `(("perl-module-install" ,perl-module-install)
      ("perl-module-build" ,perl-module-build)
      ("perl-file-sharedir-install" ,perl-file-sharedir-install)
      ("perl" ,perl)
      ("perltidy" ,perltidy)))
   (home-page "https://github.com/ICGC-TCGA-PanCancer/PCAP-core")
   (synopsis "NGS reference implementations and helper code for the ICGC/TCGA
Pan-Cancer Analysis Project")
   (description "")
   (license license:gpl2+)))

(define-public perl-bio-pipeline-comparison
  (package
    (name "perl-bio-pipeline-comparison")
    (version "1.123050")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AJ/AJPAGE/"
                           "Bio-Pipeline-Comparison-" version ".tar.gz"))
       (sha256
        (base32
         "081kn3zyi7zcwkaxrk5w52nkx7jrp0pwjcr8sai25l45711xli49"))))
    (build-system perl-build-system)
    ;; Only one test fails.
    (arguments `(#:tests? #f))
    (propagated-inputs
     `(("htslib" ,htslib)
       ("which" ,which)))
    (native-inputs
     `(("perl-env-path" ,perl-env-path)
       ("perl-test-most" ,perl-test-most)))
    (inputs
     `(("bioperl-minimal" ,bioperl-minimal)
       ("perl-exception-class" ,perl-exception-class)
       ("perl-file-which" ,perl-file-which)
       ("perl-moose" ,perl-moose)
       ("perl-try-tiny" ,perl-try-tiny)))
    (home-page "http://search.cpan.org/dist/Bio-Pipeline-Comparison")
    (synopsis "Comparative assesment of variant calling (CAVar)")
    (description "")
    (license license:gpl3+)))

(define-public perl-cgpvcf
  (package
   (name "perl-cgpvcf")
   (version "2.0.4")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://github.com/cancerit/cgpVcf/archive/v"
                                version ".tar.gz"))
            (file-name (string-append name "-" version ".tar.gz"))
            (sha256
             (base32 "009vpq2l1pxqfsvckapzxav5xr6kcjvg3krrfdx40qammcr4q1ak"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        (replace 'configure
          (lambda* (#:key outputs #:allow-other-keys)
            (system* "perl" "Makefile.PL"
                     (string-append "PREFIX=" (assoc-ref outputs "out"))))))))
   (propagated-inputs
    `(("perl-bio-pipeline-comparison" ,perl-bio-pipeline-comparison)
      ("perl-const-fast" ,perl-const-fast)
      ("perl-data-uuid" ,perl-data-uuid)
      ("perl-datetime" ,perl-datetime)))
   (native-inputs
    `(("perl-module-install" ,perl-module-install)
      ("perl-module-build" ,perl-module-build)
      ("perl" ,perl)
      ("perltidy" ,perltidy)))
   (home-page "https://cancerit.github.io/cgpVcf/")
   (synopsis "Set of common Perl utilities for generating VCF headers")
   (description "This package contains a set of common Perl utilities for
generating consistent Vcf headers.  It primarily exists to prevent code
duplication between some other projects.")
   (license license:agpl3+)))

(define-public caveman
  (package
   (name "caveman")
   (version "1.15.3")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/cancerit/CaVEMan")
                  (commit version)))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0qrkdlz7g475xj4iwmbzp8dk9b09kmrgkyzf8pmzblnj7bwapgfx"))))
   (build-system gnu-build-system)
   (arguments
    (list
     #:make-flags
     #~(list (string-append
              "HTSLOC=" #$(this-package-input "htslib")))
      #:tests? #f ; Tests require a network connection.
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-before 'build 'patch-out-tests
            (lambda _
              (substitute* "Makefile"
                (("CC\\?=gcc") (string-append "CC=" #$(cc-for-target)))
                (("copyscript test") "copyscript"))))
          (replace 'install
            (lambda _
              (let ((bin (string-append #$output "/bin")))
                (install-file "bin/caveman" bin)
                (install-file "bin/generateCavemanUMNormVCF" bin)
                (install-file "bin/mergeCavemanResults" bin)))))))
   (inputs
    (list curl htslib linasm perl zlib))
   (home-page "http://cancerit.github.io/CaVEMan/")
   (synopsis "Implementation of an SNV expectation maximisation algorithm for
calling single base substitutions in paired data")
   (description "This package provides an implementation of the
CaVEMan program.  It uses an expectation maximisation approach to
calling single base substitutions in paired data.  It is designed for
use with a compute cluster.  Most steps in the program make use of an
index parameter.  The split step is designed to divide the genome into
chunks of adjustable size to optimise for runtime/memory usage
requirements.")
   (license license:agpl3+)))

(define-public perl-forks
  (package
  (name "perl-forks")
  (version "0.36")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "mirror://cpan/authors/id/R/RY/RYBSKEJ/forks-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "14srnq51n98aizdlg6lhzpzdqyjvxf5nfm431qiylvsc9zj29gk1"))))
  (build-system perl-build-system)
  (propagated-inputs
    `(("perl-acme-damn" ,perl-acme-damn)
      ("perl-devel-symdump" ,perl-devel-symdump)
      ("perl-list-moreutils" ,perl-list-moreutils)
      ("perl-sys-sigaction" ,perl-sys-sigaction)))
  (home-page "http://search.cpan.org/dist/forks")
  (synopsis "forks - emulate threads with fork")
  (description "")
  (license (package-license perl))))

(define-public perl-acme-damn
  (package
   (name "perl-acme-damn")
   (version "0.08")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://cpan.metacpan.org/authors/id/I/IB/IBB/Acme-Damn-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "03kykdsz3fk5ppb9g92pvnif67zlk501finrwi1csbcizw1js39i"))))
   (build-system perl-build-system)
   (inputs
    `(("perl-test-exception" ,perl-test-exception)))
   (home-page "http://search.cpan.org/dist/Acme-Damn")
   (synopsis "'Unbless' Perl objects.")
   (description "")
   (license license:perl-license)))

(define-public perl-sys-sigaction
  (package
   (name "perl-sys-sigaction")
   (version "0.23")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "mirror://cpan/authors/id/L/LB/LBAXTER/Sys-SigAction-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "0lykjlq5dsf7z927lpllzixd953izi3w7bg2pgy32h2k8n9nrvy4"))))
   (build-system perl-build-system)
   (home-page
    "http://search.cpan.org/dist/Sys-SigAction")
   (synopsis
    "Perl extension for Consistent Signal Handling")
   (description "")
   (license (package-license perl))))

(define-public cgp-cavemanpostprocessing
  (package
    (name "cgp-cavemanpostprocessing")
    (version "1.8.9")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/cancerit/cgpCaVEManPostProcessing/"
                    "archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "01h2vd8vz8vd4sdgjh13sy2kb98w2lgrqamqpw65ivvhb96yg3qf"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'install)
         ;; The Perl in Guix does not support threads.
         ;; The forks module is a drop-in replacement for it, so it
         ;; is easier to use that instead of recompiling Perl.
         (add-after 'unpack 'enable-threads
           (lambda _
             (substitute* "bin/cgpFlagCaVEMan.pl"
               (("use strict;") "use forks;\nuse strict;"))))
         (replace 'build
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((bin-dir (string-append (assoc-ref outputs "out") "/bin"))
                   (lib-dir (string-append (assoc-ref outputs "out")
                            "/lib/perl5/site_perl/5.28.0"))
                   (config-dir (string-append (assoc-ref outputs "out") "/config")))
               (mkdir-p bin-dir)
               (mkdir-p lib-dir)
               (mkdir-p config-dir)
               (install-file "bin/cgpFlagCaVEMan.pl" bin-dir)
               (copy-recursively "lib" lib-dir)
               (copy-recursively "config" config-dir)
               #t))))))
    (propagated-inputs
     `(("perl-file-path" ,perl-file-path)
       ("perl-file-which", perl-file-which)
       ("perl-const-fast", perl-const-fast)
       ("perl-capture-tiny", perl-capture-tiny)
       ("perl-ipc-system-simple", perl-ipc-system-simple)
       ("perl-try-tiny", perl-try-tiny)
       ("perl-carp", perl-carp)
       ("perl-forks", perl-forks)
       ("perl-attribute-util", perl-attribute-util)
       ("perl-config-inifiles", perl-config-inifiles)
       ("perl-set-intervaltree", perl-set-intervaltree)
       ("perl-libwww", perl-libwww)
       ("pcap-core" ,pcap-core)
       ("perl-cgpvcf", perl-cgpvcf)
       ("perl-bio-db-hts", perl-bio-db-hts)
       ("bioperl-minimal", bioperl-minimal)
       ("perl" ,perl)))
    (home-page "https://github.com/cancerit/cgpCaVEManPostProcessing")
    (synopsis "Flagging add-on to CaVEMan")
    (description "This package is used to apply filtering on raw VCF calls
generated using CaVEMan.")
    (license license:agpl3+)))

(define-public perl-term-ui
  (package
    (name "perl-term-ui")
    (version "0.50")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/B/BI/BINGOS/Term-UI-"
             version ".tar.gz"))
       (sha256
        (base32
         "0g8n4jjqriw11c00dn8jbfj3cnj841dwcgqkf39qi2qm9inxvgv0"))))
    (build-system perl-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs
     `(("perl-log-message-simple" ,perl-log-message-simple)))
    (home-page "http://search.cpan.org/dist/Term-UI")
    (synopsis "User interfaces via Term::ReadLine made easy")
    (description "")
    (license (package-license perl))))

(define-public perl-devel-cover
  (package
    (name "perl-devel-cover")
    (version "1.40")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/P/PJ/PJCJ/Devel-Cover-"
             version ".tar.gz"))
       (sha256
        (base32
         "1a0iwypjp3ddbjbsnh3g43zhjz06hhxzhlpka4wgyyygzcqz9qi6"))))
    (build-system perl-build-system)
    (arguments
     `(#:tests? #f))
    (native-inputs
     (list perl-test-differences))
    ;; These are recommended inputs
    (propagated-inputs
     (list perl-browser-open
           perl-capture-tiny
           perl-class-xsaccessor
           perl-moo
           perl-namespace-clean
           perl-parallel-iterator
           perl-pod-coverage
           perl-ppi-html
           perl-template-toolkit
           perl-test-differences
           perltidy))
    (home-page
     "http://search.cpan.org/dist/Devel-Cover")
    (synopsis "Code coverage metrics for Perl")
    (description "This module provides code coverage metrics for Perl.
Code coverage metrics describe how thoroughly tests exercise code.  By
using @code{Devel::Cover} you can discover areas of code not exercised
by your tests and determine which tests to create to increase
coverage.")
    (license (package-license perl))))

(define-public perl-parallel-iterator
  (package
    (name "perl-parallel-iterator")
    (version "1.00")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/A/AN/ANDYA/Parallel-Iterator-"
             version ".tar.gz"))
       (sha256
        (base32
         "1x252cqzcyxkmf8p5dw34ais47ci1ldv2ds02m7a2ijpryam0jg8"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-install" ,perl-module-install)
       ("perl-module-build" ,perl-module-build)
       ("perl-file-sharedir-install" ,perl-file-sharedir-install)))
    (home-page
     "http://search.cpan.org/dist/Parallel-Iterator")
    (synopsis "Simple parallel execution")
    (description "")
    (license (package-license perl))))

(define-public perl-ppi
  (package
    (name "perl-ppi")
    (version "1.236")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/M/MI/MITHALDU/PPI-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1v4a622h19d2d6m070jcxn47siqma9g9ly4fcrnrlz8bkhs4nry6"))))
    (build-system perl-build-system)
    (arguments
     `(#:tests? #f))
    (native-inputs
     `(("perl-class-inspector" ,perl-class-inspector)
       ("perl-file-remove" ,perl-file-remove)
       ("perl-test-nowarnings" ,perl-test-nowarnings)
       ("perl-test-object" ,perl-test-object)
       ("perl-test-subcalls" ,perl-test-subcalls)))
    (inputs
     `(("perl-clone" ,perl-clone)
       ("perl-io-string" ,perl-io-string)
       ("perl-list-moreutils" ,perl-list-moreutils)
       ("perl-params-util" ,perl-params-util)
       ("perl-task-weaken" ,perl-task-weaken)))
    (home-page "http://search.cpan.org/dist/PPI")
    (synopsis "Parse, Analyze and Manipulate Perl (without perl)")
    (description "")
    (license (package-license perl))))

(define-public perl-ppi-html
  (package
    (name "perl-ppi-html")
    (version "1.08")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/A/AD/ADAMK/PPI-HTML-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "04f5sfrb6ckfdd3lnyipmky9mdgsxr5b724sp1xaszx86d09c9l4"))))
    (build-system perl-build-system)
    (arguments
     `(#:tests? #f))
    (propagated-inputs
     `(("perl-module-install" ,perl-module-install)))
    (inputs
     `(("perl-css-tiny" ,perl-css-tiny)
       ("perl-params-util" ,perl-params-util)
       ("perl-ppi" ,perl-ppi)))
    (home-page
     "http://search.cpan.org/dist/PPI-HTML")
    (synopsis
     "Generate syntax-hightlighted HTML for Perl using PPI")
    (description "")
    (license (package-license perl))))

(define-public perl-css-tiny
  (package
    (name "perl-css-tiny")
    (version "1.20")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/C/CH/CHORNY/CSS-Tiny-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1yjjn4li8v3d51l7jgrbbkhjdpfm9mmira2xfgf3s58wlkk9vx38"))))
    (build-system perl-build-system)
    (home-page
     "http://search.cpan.org/dist/CSS-Tiny")
    (synopsis
     "Read/Write .css files with as little code as possible")
    (description "")
    (license (package-license perl))))

(define-public cgp-pindel
  (package
    (name "cgp-pindel")
    (version "2.2.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/cancerit/cgpPindel/archive/v"
                    version ".tar.gz"))
              (sha256
               (base32
                "1vadlyffabqj696k9nnzqprxn5avf0a5iykpqjxmw8n2180lppvw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'move-to-subdirectory
           (lambda _
             (chdir "perl")))
         (replace 'build
           (lambda* (#:key outputs #:allow-other-keys)
             (system* "perl" "Makefile.PL"
                      (string-append "PREFIX=" (assoc-ref outputs "out")))
             (system* "make"))))))
    (propagated-inputs
     `(("perl" ,perl)
       ("pcap-core" ,pcap-core)
       ("perl-cgpvcf" ,perl-cgpvcf)
       ("bioperl-minimal" ,bioperl-minimal)
       ("perl-bio-db-hts" ,perl-bio-db-hts)
       ("perl-const-fast" ,perl-const-fast)
       ("perl-file-which" ,perl-file-which)
       ("perl-pod-coverage" ,perl-pod-coverage)
       ("perl-list-moreutils" ,perl-list-moreutils)
       ("perl-test-fatal" ,perl-test-fatal)
       ("perl-try-tiny" ,perl-try-tiny)
       ("perl-capture-tiny" ,perl-capture-tiny)
       ("perl-term-ui" ,perl-term-ui)
       ("perl-log-message" ,perl-log-message)
       ("perl-ipc-system-simple" ,perl-ipc-system-simple)
       ("perl-sub-exporter-progressive" ,perl-sub-exporter-progressive)
       ("perl-devel-cover" ,perl-devel-cover)))
    (native-inputs
     `(("perl-module-install" ,perl-module-install)
       ("perl-module-build" ,perl-module-build)
       ("perl-file-sharedir-install" ,perl-file-sharedir-install)))
    (home-page "https://github.com/cancerit/cgpPindel")
    (synopsis "Cancer Genome Projects workflow for Pindel.")
    (description "")
    (license license:agpl3+)))

;; Cannot be upstreamed: Binary package.
(define-public score-client
  (package
   (name "score-client")
   (version "5.0.0")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://artifacts.oicr.on.ca/artifactory/dcc-release/bio/"
                  "overture/score-client/" version "/score-client-" version
                  "-dist.tar.gz"))
            (sha256
             (base32 "05pvffd43aqdh92g1p37p9p00wciqxp45n5gyybxvpgs1cfdqsfm"))))
   ;; We use the GNU build system mainly for its patch-shebang phases.
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; This is just copying a binary, so no tests to perform.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure) ; No configuration, just copying.
        (delete 'build)     ; No building, just copying.
        (replace 'install
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (etc (string-append out "/etc/score-client"))
                   (bin (string-append out "/bin"))
                   (lib (string-append out "/lib")))

              (for-each mkdir-p (list out etc bin lib))

              (substitute* "bin/score-client"
               (("`dirname \\$0`/..") out)
               (("\\$\\(cd \\$\\{BASE_DIR\\} && pwd -P\\)") out)
               (("exec java") (string-append
                               "exec " (assoc-ref inputs "openjdk")
                               "/bin/java"))
               (("-Dlogging.path=\\$\\{BASE_DIR\\}/logs")
                "-Dlogging.path=${HOME}")
               (("type -p java")
                (string-append "type -p "
                               (assoc-ref inputs "openjdk")
                               "/bin/java"))
               (("_java=java")
                (string-append "_java="
                               (assoc-ref inputs "openjdk")
                               "/bin/java"))
               (("\\$\\{CLIENT_DIR\\}/conf") etc))

              (copy-recursively "bin" bin)
              (copy-recursively "conf" etc)
              (copy-recursively "lib" lib)

              (wrap-program (string-append out "/bin/score-client")
                `("_JAVA_OPTIONS" ":" = (,(string-append
                                           "-Djavax.net.ssl.trustStore="
                                           (assoc-ref inputs "openjdk")
                                           "/lib/security/cacerts"))))))))))
   (inputs
    `(("openjdk" ,openjdk11)))
   (home-page "https://docs.icgc.org/software/download/#score-client")
   (synopsis "Tool to view ICGC data")
   (description "This package provides a tool to download or view data in
the cloud environments of ICGC.")
   (license license:gpl3)))

(define-public metamaps
  (let ((commit "e23f8a8688159ff0d092557a40305dbc7acc2342"))
    (package
     (name "metamaps")
     (version (string-append "0.0-" (string-take commit 7)))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/DiltheyLab/MetaMaps.git")
                    (commit commit)))
              (sha256
               (base32
                "0h9ahkv7axw4qzgbvhsz4r699swiv64hlwjy6h8s11vjls2dslrp"))))
     (build-system gnu-build-system)
     (arguments
      `(#:configure-flags (list (string-append
                                 "--with-boost="
                                 (assoc-ref %build-inputs "boost")))
        #:tests? #f
        #:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'shared-boost
            (lambda _
              (substitute* "configure.ac"
               (("libboost_math_c99.a") "libboost_math_c99.so")))))))
     (native-inputs
      `(("autoconf" ,autoconf)))
     (inputs
      `(("boost" ,boost)
        ("zlib" ,zlib)
        ("gsl" ,gsl)))
     (home-page "https://github.com/DiltheyLab/MetaMaps")
     (synopsis "Long-read metagenomic analysis")
     (description "MetaMaps is tool specifically developed for the analysis
of long-read (PacBio/Oxford Nanopore) metagenomic datasets.")
     (license license:public-domain))))

(define-public igv
  (package
    (name "igv")
    (version "2.16.2")
    (source
     (origin
       (method url-fetch)
       ;; There is no version available without jdk, so use it and deblob.
       ;; The source code is available at https://github.com/igvteam/igv/
       (uri (string-append
             "http://data.broadinstitute.org/igv/projects/downloads/"
             (version-major+minor version)
             "/IGV_Linux_" version "_WithJava.zip"))
       (sha256
        (base32
         "1rr7cmalxrhwprj7kwfq2bjwjs5ng3j0fpwrpl7189wab58r6yhb"))
       (modules '((guix build utils)))
       (snippet
        '(delete-file-recursively "jdk-11"))))
    (build-system gnu-build-system)
    (propagated-inputs
     (list openjdk11))
    (native-inputs
     (list unzip))
    (arguments
     (list
      #:tests? #f                       ; No tests available.
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)         ; Nothing to configure.
          (delete 'build)             ; This is a binary package only.
          (replace 'install
            (lambda _
              (let ((bin (string-append #$output "/bin"))
                    (lib (string-append #$output "/lib"))
                    (share (string-append #$output "/share/igv")))
                (mkdir-p share)
                (mkdir-p lib)
                (mkdir-p bin)
                (copy-recursively "lib" lib)
                (substitute* "igv.sh"
                  (("prefix=")
                   (string-append "prefix=" lib " # "))
                  (("\\$\\{prefix\\}/igv.args")
                   (string-append share "/igv.args"))
                  (("--module-path=\"\\$\\{prefix\\}/lib\"")
                   (string-append "--module-path=" lib))
                  ;; Always use Guix JDK; don't test for deleted
                  ;; jdk-11 directory.
                  (("\"\\$\\{prefix\\}/jdk-11\"")
                   "\"${prefix}\"")
                  (("JAVA_HOME=.*")
                   (string-append "JAVA_HOME=" #$(this-package-input "openjdk") "\n")))
                (install-file "igv.args" share)
                (install-file "igv.sh" bin)))))))
   (home-page "https://www.broadinstitute.org/software/igv/")
   (synopsis "Integrative Genomics Viewer")
   (description "The Integrative Genomics Viewer (IGV) is a high-performance
visualization tool for interactive exploration of large, integrated
genomic datasets.  It supports a wide variety of data types, including
array-based and next-generation sequence data, and genomic
annotations.")
   (license license:expat)))

(define-public iq-tree
  (package
    (name "iq-tree")
    (version "2.0.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/Cibiv/IQ-TREE/archive/v"
                    version ".tar.gz"))
              (sha256
               (base32 "119s3zw2y8432gryj5rg1vgdgmvf00hb1gzgqn2qgrjmfrnahp2k"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DIQTREE_FLAGS=omp")
       #:tests? #f))
    (inputs
     (list boost eigen zlib))
    (home-page "http://www.iqtree.org/")
    (synopsis "Efficient software for phylogenomic inference")
    (description
     "This package provides software for phylogenomic inference.")
    (license license:gpl2)))

(define-public last
  (package
   (name "last")
   (version "1080")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://last.cbrc.jp/last-" version ".zip"))
            (sha256
               (base32
                "0az6xiqkbdcq858m1dlwvf7f7pa5fjldckkawcj8a38a2fq9drds"))))
   (build-system gnu-build-system)
   (native-inputs
    `(("unzip" ,unzip)
      ("sed" ,sed)))
   (inputs
    `(("zlib" ,zlib)))
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (add-after 'unpack 'set-c-compiler
         (lambda* (#:key outputs #:allow-other-keys)
           (substitute* "src/makefile"
            (("# -Wconversion") "CC=gcc"))
           (substitute* "makefile"
            (("prefix = /usr/local")
             (string-append
              "prefix = " (assoc-ref outputs "out")))))))))
   (home-page "http://last.cbrc.jp/")
   (synopsis "Genome-scale sequence comparison")
   (description "")
   (license license:gpl3)))

(define-public cat
  (package
   (name "cat")
   (version "5.2.3")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/dutilh/CAT/archive/v"
                  version".tar.gz"))
            (sha256
             (base32
              "16wjl8ng6dfz60phvdrj1bq1mp8nm42x61mh0idgrlzbq0lfpa0r"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (delete 'build)
        (replace 'install
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((bin (string-append (assoc-ref outputs "out") "/bin"))
                  (share (string-append (assoc-ref outputs "out") "/share/CAT")))
              (mkdir-p bin)
              (mkdir-p share)
              (with-directory-excursion "CAT_pack"
                (install-file "CAT" bin)

                ;; Don't pollute the "bin" directory with Python libraries.
                (map (lambda (file)
                       (when (string-suffix? ".py" file)
                         (install-file file share)))
                     (find-files "."))

                ;; Make sure CAT can find its Python libraries.
                (wrap-program (string-append bin "/CAT")
                 `("PYTHONPATH" ":" = (,share "$PYTHONPATH"))))))))))
   (inputs
    `(("diamond" ,diamond)
      ("prodigal" ,prodigal)
      ("python" ,python)))
   (home-page "https://github.com/dutilh/CAT")
   (synopsis "Tool for taxonomic classification of contigs and metagenome-assembled genomes")
   (description "Contig Annotation Tool (CAT) and Bin Annotation Tool (BAT)
are pipelines for the taxonomic classification of long DNA sequences and
metagenome assembled genomes (MAGs/bins) of both known and (highly) unknown
microorganisms, as generated by contemporary metagenomics studies.  The core
algorithm of both programs involves gene calling, mapping of predicted ORFs
against the nr protein database, and voting-based classification of the entire
contig / MAG based on classification of the individual ORFs.  CAT and BAT can
be run from intermediate steps if files are formated appropriately")
   (license license:expat)))

(define-public hiddendomains
  (package
   (name "hiddendomains")
   (version "3.1")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "mirror://sourceforge/hiddendomains/hiddenDomains."
                  version ".tar.gz"))
            (sha256
             (base32 "0kpdfz7z014aax26ga8b29s924jb8csy06lxkg3jwjdq7amy2bbw"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (delete 'build)
        (replace 'install
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((bin   (string-append (assoc-ref outputs "out") "/bin"))
                  (share (string-append (assoc-ref outputs "out")
                                        "/share/hiddenDomains")))
              (mkdir-p bin)
              (for-each (lambda (file) (install-file file bin))
               '("binReads.pl"     "centersToGEM.pl" "domainsMergeToBed.pl"
                 "domainsToBed.pl" "hiddenDomains"   "peakCenters"))
              (mkdir-p share)
              (install-file "hiddenDomains.R" share)))))))
   (inputs
    `(("perl" ,perl)))
   (propagated-inputs
    `(("r-depmixS4" ,r-depmixs4)
      ("r-hiddenmarkov" ,r-hiddenmarkov)))
   (home-page "http://hiddendomains.sourceforge.net")
   (synopsis "Programs used to identify enrichment of ChIP-seq reads")
   (description "hiddenDomains is a suite of programs used to identify
significant enrichment of ChIP-seq reads that span large domains, like
HK27me3.  The input data can be in BAM format, or in a tab-delimited
'reads per bin' format described below.  The output is a BED formatted
file the lists the enriched domains and their posterior probabilities.")
   (license license:gpl2)))

(define-public perl-findbin-libs
  (package
    (name "perl-findbin-libs")
    (version "2.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/L/LE/LEMBARK/FindBin-libs-"
             version ".tar.gz"))
       (sha256
        (base32
         "0306g1lpxfpv0r6491y6njjc312jx01zh2qqqa4cwkc0ya4jpdpn"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/FindBin-libs")
    (synopsis "")
    (description "")
    (license (package-license perl))))

(define-public perl-prokka
  (package
    (name "perl-prokka")
    (version "1.14.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tseemann/prokka/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "03dp48d7gb1n7dp3c75mv8qis1vlzmvn6x0y5649w6hzsqn6z7yi"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #false ;there are none
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (delete 'build)
          (replace 'install
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* '("bin/prokka"
                             "bin/prokka-abricate_to_fasta_db"
                             "bin/prokka-biocyc_to_fasta_db"
                             "bin/prokka-build_kingdom_dbs"
                             "bin/prokka-cdd_to_hmm"
                             "bin/prokka-clusters_to_hmm"
                             "bin/prokka-genbank_to_fasta_db"
                             "bin/prokka-genpept_to_fasta_db"
                             "bin/prokka-hamap_to_hmm"
                             "bin/prokka-make_tarball"
                             "bin/prokka-tigrfams_to_hmm"
                             "bin/prokka-uniprot_to_fasta_db")
                (("\"parallel ")
                 (string-append "\"" (search-input-file inputs "/bin/parallel") " "))
                (("(aragorn|blastp|cmpress|cmscan|hmmscan|hmmpress|makeblastdb|minced|prodigal) " _ bin)
                 (string-append (search-input-file inputs (string-append "/bin/" bin)) " ")))

              (let ((bin (string-append #$output "/bin"))
                    (share (string-append #$output "/share/prokka/db")))
                (mkdir-p bin)
                (copy-recursively "bin" bin)
                (mkdir-p share)
                (copy-recursively "db" share)
                (for-each (lambda (program)
                            (wrap-program program
                              `("PERL5LIB" ":" = (,(getenv "PERL5LIB") "$PERL5LIB"))))
                          (find-files bin))))))))
    (inputs
     (list aragorn
           bioperl-minimal
           blast+
           findutils
           hmmer
           infernal
           minced
           parallel
           perl
           perl-data-dumper
           perl-digest-md5
           perl-findbin-libs
           perl-module-build
           perl-scalar-list-utils
           perl-time-piece
           perl-xml-simple
           prodigal))
    (native-search-paths
     (list (search-path-specification
            (variable "PROKKA_DBDIR")
            (files '("share/prokka/db")))))
    (home-page "https://github.com/tseemann/prokka")
    (synopsis "Rapid prokaryotic genome annotation")
    (description "This package provides tools for rapid prokaryotic
genome annotation.")
    (license license:gpl3)))

(define-public cgp-cavemanwrapper-1.15.2
  (package
   (name "cgp-cavemanwrapper")
   (version "1.15.2")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/cancerit/cgpCaVEManWrapper/archive/"
                  version ".tar.gz"))
            (file-name (string-append name "-" version ".tar.gz"))
            (patches (list (search-patch "cgp-cavemanwrapper-fix-script.patch")))
            (sha256
             (base32
              "017b03j1sm64v6barbxx69420d3s1gx5nlcqg7rzn6mczj47g264"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
       (delete 'configure)
       (delete 'install)
       ;; The Perl in Guix does not support threads.
       ;; The forks module is a drop-in replacement for it, so it
       ;; is easier to use that instead of recompiling Perl.
       (add-after 'unpack 'enable-threads
        (lambda _
          (substitute* "bin/caveman_merge_results.pl"
           (("use strict;") "use forks;\nuse strict;"))))
       (replace 'build
        (lambda* (#:key inputs outputs #:allow-other-keys)
          (let ((bin-dir (string-append (assoc-ref outputs "out") "/bin"))
                (lib-dir (string-append (assoc-ref outputs "out")
                                        "/lib/perl5/site_perl/5.28.0")))
            (mkdir-p bin-dir)
            (mkdir-p lib-dir)
            (install-file "bin/caveman_merge_results.pl" bin-dir)
            (copy-recursively "lib/Sanger" lib-dir)
            #t))))))
   (propagated-inputs
    `(("perl-file-path" ,perl-file-path)
      ("perl-file-which", perl-file-which)
      ("perl-const-fast", perl-const-fast)
      ("perl-capture-tiny", perl-capture-tiny)
      ("perl-ipc-system-simple", perl-ipc-system-simple)
      ("perl-try-tiny", perl-try-tiny)
      ("perl-carp", perl-carp)
      ("perl-forks", perl-forks)
      ("pcap-core" ,pcap-core)
      ("perl" ,perl)))
   (home-page "https://github.com/cancerit/cgpCaVEManWrapper")
   (synopsis "Reference implementation of CGP workflow for CaVEMan")
   (description "This package provides the reference implementation of CGP
workflow for CaVEMan SNV analysis.")
   (license license:agpl3+)))

