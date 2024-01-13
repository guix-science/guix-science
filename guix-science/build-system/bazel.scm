;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Ricardo Wurmus <rekado@elephly.net>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guix-science build-system bazel)
  #:use-module (guix base32)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix monads)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages base) ;canonical-package
  #:use-module (gnu packages certs) ;nss-certs
  #:export (%bazel-build-system-modules
            bazel-build
            bazel-build-system))

;; Commentary:
;;
;; Standard build procedure for packages using Bazel.
;;
;; Code:

(define %bazel-build-system-modules
  ;; Build-side modules imported by default.
  `((guix-science build bazel-build-system)
    ,@%gnu-build-system-modules))

(define %default-modules
  `((ice-9 ftw)
    (ice-9 match)
    (ice-9 string-fun)
    (guix build utils)
    (guix-science build bazel-build-system)))

(define (default-bazel)
  "Return the default Bazel package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((bazel-mod (resolve-interface '(guix-science packages bazel))))
    (module-ref bazel-mod 'bazel-6)))

(define (default-jdk)
  "Return the default JDK package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((java-mod (resolve-interface '(gnu packages java))))
    (module-ref java-mod 'openjdk11)))

(define* (bazel-vendored-inputs
          #:key name source hash
          search-paths inputs
          fetch-targets (bazel-arguments '())
          bazel-configuration)
  (computed-file
   name
   (with-imported-modules (source-module-closure '((guix build utils)
                                                   (guix-science build bazel-build-system)))
     #~(begin
         (use-modules (guix build utils)
                      (ice-9 ftw)
                      (ice-9 match)
                      (ice-9 string-fun))
         (define input-directories '#$(map cadr inputs))
         (define %build-directory (getcwd))
         (define %bazel-out
           (string-append %build-directory "/output"))
         (define %bazel-user-root
           (string-append %build-directory "/tmp"))
         (setvbuf (current-output-port) 'line)
         (setvbuf (current-error-port) 'line)
         (set-path-environment-variable "PATH" '("bin" "sbin")
                                        input-directories)
         (for-each (match-lambda
                     ((env-var (files ...) separator type pattern)
                      (set-path-environment-variable env-var files
                                                     input-directories
                                                     #:separator separator
                                                     #:type type
                                                     #:pattern pattern)))
                   '#$search-paths)

         ;; TODO: only works for directories
         (chdir #$source)
         (setenv "SOURCE_DATE_EPOCH" "1")
         (setenv "HOME" %build-directory)
         (setenv "USER" "homeless-shelter")
         (let ((cert-bundle (string-append #+nss-certs
                                           "/etc/ssl/certs/ca-bundle.crt")))
           (setenv "GIT_SSL_CAINFO" cert-bundle)
           (setenv "SSL_CERT_FILE" cert-bundle))

         (mkdir-p %bazel-out)
         #$bazel-configuration
         (apply invoke "bazel"
                "--batch"
                (string-append "--output_base=" %bazel-out)
                (string-append "--output_user_root=" %bazel-user-root)
                "build" "--nobuild"
                "--curses=no"
                "--loading_phase_threads=1"
                "--strategy=Genrule=standalone"
                "--verbose_failures"
                "--subcommands"
                "--action_env=PATH"
                "--action_env=LIBRARY_PATH"
                "--action_env=C_INCLUDE_PATH"
                "--action_env=CPLUS_INCLUDE_PATH"
                "--action_env=GUIX_LOCPATH"
                "--host_action_env=PATH"
                "--host_action_env=LIBRARY_PATH"
                "--host_action_env=C_INCLUDE_PATH"
                "--host_action_env=CPLUS_INCLUDE_PATH"
                "--host_action_env=GUIX_LOCPATH"
                (append #$bazel-arguments
                        #$fetch-targets))

         (with-directory-excursion %bazel-out
           (for-each (lambda (file)
                       (delete-file (string-append "external/" file)))
                     (scandir "external"
                              (lambda (file)
                                (or (member file '("@bazel_tools.marker"
                                                   "@embedded_jdk.marker"))
                                    (and (string-prefix? "@local_" file)
                                         (string-suffix? ".marker" file))))))
           (for-each delete-file-recursively
                     (append
                      (list "external/bazel_tools")
                      (map (lambda (file)
                             (string-append "external/" file))
                           (scandir "external"
                                    (lambda (file)
                                      (and (string-prefix? "local_" file)
                                           (eq? 'directory
                                                (stat:type
                                                 (stat (string-append "external/" file))))))))
                      (find-files "external"
                                  "^\\.(git|svn|hg)$"
                                  #:directories? #true)))

           ;; Clear markers
           (for-each (lambda (file)
                       (truncate-file file 0))
                     (find-files "external" "@.*\\.marker"))

           ;; Remove top-level symlinks along with their markers.
           ;; This is needed because they sometimes point to
           ;; temporary locations.
           (for-each (lambda (file)
                       (delete-file (string-append "external/" file))
                       (let ((marker
                              (format #false "external/@~a.marker" (basename file))))
                         (false-if-exception
                          (delete-file marker))))
                     (scandir "external"
                              (lambda (file)
                                (eq? 'symlink
                                     (stat:type
                                      (lstat (string-append "external/" file)))))))

           ;; Remove symlink references to the build directory.  These
           ;; will be rewritten to the current build directory by
           ;; users of this archive.
           (for-each (lambda (file)
                       (let ((new-target
                              (string-replace-substring
                               (readlink file)
                               %build-directory "GUIX_BUILD_TOP")))
                         (delete-file file)
                         (symlink new-target file)))
                     (find-files "external"
                                 (lambda (file-name stat)
                                   (and (eq? (stat:type stat) 'symlink)
                                        (string-contains (readlink file-name)
                                                         %build-directory)))
                                 #:stat lstat))
           (invoke "du" "-s" "external")
           (invoke "tar" "cfa" #$output
                   "--mtime=@1"
                   "--owner=0"
                   "--group=0"
                   "--numeric-owner"
                   "--sort=name"
                   "external"))))
   #:options
   `(#:hash-algo sha256
     #:hash
     ,(nix-base32-string->bytevector hash))))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (jdk (default-jdk))
                (bazel (default-bazel))
                (fetch-targets '())
                (build-targets '())
                (bazel-arguments '())
                (bazel-configuration '())
                vendored-inputs-hash
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    `(#:inputs #:native-inputs #:outputs
      ,@(if target '() '(#:target))))

  (bag
    (name name)
    (system system) (target target)
    (build-inputs `(,@(if source
                         `(("source" ,source))
                         '())
                    ("jdk" ,jdk "jdk")
                    ("bazel" ,bazel)
                    ("which" ,(canonical-package which))
                    ,@native-inputs
                    ,@(if target '() inputs)
                    ,@(standard-packages)))
    (host-inputs (if target inputs '()))
    ;; Keep the standard inputs of 'gnu-buid-system'.
    (target-inputs (if target
                       (standard-cross-packages target 'target)
                       '()))
    (outputs outputs)
    (build (if target bazel-cross-build bazel-build))
    (arguments (strip-keyword-arguments private-keywords arguments))))

(define* (bazel-cross-build #:key name inputs #:allow-other-keys)
  (error "cross-builds not implemented"))

(define* (bazel-build name inputs
                      #:key
                      guile source
                      (tests? #t)
                      (fetch-targets '())
                      (build-targets '())
                      (bazel-arguments '())
                      (bazel-configuration '())
                      (run-command '())
                      vendored-inputs-hash
                      (parallel-build? #t)
                      (phases '%standard-phases)
                      (outputs '("out"))
                      (search-paths '())
                      (system (%current-system))
                      (imported-modules %bazel-build-system-modules)
                      (modules %default-modules)
                      (substitutable? #t)
                      allowed-references
                      disallowed-references)
  "Build SOURCE with INPUTS."
  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))
          #$(with-build-variables inputs outputs
              #~(bazel-build #:name #$name
                             #:source #+source
                             #:vendored-inputs
                             #$(bazel-vendored-inputs
                                #:name (string-append name "-bazel-deps.tar.xz")
                                #:source source
                                #:hash vendored-inputs-hash
                                #:search-paths
                                (map search-path-specification->sexp
                                     search-paths)
                                #:inputs inputs
                                #:fetch-targets fetch-targets
                                #:bazel-arguments bazel-arguments
                                #:bazel-configuration bazel-configuration)
                             #:system #$system
                             #:tests? #$tests?
                             #:parallel-build? #$parallel-build?
                             #:build-targets #$build-targets
                             #:run-command #$run-command
                             #:phases #$phases
                             #:inputs %build-inputs
                             #:outputs %outputs
                             #:search-paths '#$(sexp->gexp
                                                (map search-path-specification->sexp
                                                     search-paths)))))))

  (mlet %store-monad  ((guile (package->derivation (or guile (default-guile))
                                                   system #:graft? #f)))
    (gexp->derivation name builder
                      #:system system
                      #:target #f
                      #:graft? #f
                      #:substitutable? substitutable?
                      #:allowed-references allowed-references
                      #:disallowed-references disallowed-references
                      #:guile-for-build guile)))

(define bazel-build-system
  (build-system
    (name 'bazel)
    (description "The Bazel build system")
    (lower lower)))

;;; bazel.scm ends here
