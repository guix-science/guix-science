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

(define-module (guix-science build bazel-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:export (%standard-phases
            bazel-build))

;; Commentary:
;;
;; Builder-side code of the standard build procedure for Bazel
;; packages.
;;
;; Code:

(define* (unpack-vendored-inputs #:key vendored-inputs #:allow-other-keys)
  "Unpack VENDORED-INPUTS, a tarball resulting form a previous run of
`bazel build --nobuild'.  Restore symlinks after unpacking."
  (define bazel-out
    (string-append (getenv "NIX_BUILD_TOP") "/output"))
  (mkdir-p bazel-out)
  (with-directory-excursion bazel-out
    (invoke "tar" "xf" vendored-inputs))
  ;; Rewrite dangling links to current build directory
  (for-each (lambda (file)
              (let ((new-target
                     (string-replace-substring
                      (readlink file)
                      "GUIX_BUILD_TOP" (getenv "NIX_BUILD_TOP"))))
                (delete-file file)
                (symlink new-target file)))
            (find-files bazel-out
                        (lambda (file-name stat)
                          (and (eq? (stat:type stat) 'symlink)
                               (string-contains (readlink file-name)
                                                "GUIX_BUILD_TOP")))
                        #:stat lstat))
  (setenv "HOME" (getenv "NIX_BUILD_TOP")))

(define* (build #:key
                parallel-build?
                build-targets
                (run-command '()) #:allow-other-keys)
  (define %build-directory (getenv "NIX_BUILD_TOP"))
  (define %bazel-out
    (string-append %build-directory "/output"))
  (define %bazel-user-root
    (string-append %build-directory "/tmp"))
  (setenv "USER" "homeless-shelter")
  (apply invoke "bazel"
         "--batch"
         (string-append "--output_base=" %bazel-out)
         (string-append "--output_user_root=" %bazel-user-root)
         (if (null? run-command) "build" "run")
         "--curses=no"
         "--verbose_failures"
         "--subcommands"
         "--action_env=PATH"
         "--action_env=LIBRARY_PATH"
         "--action_env=C_INCLUDE_PATH"
         "--action_env=CPLUS_INCLUDE_PATH"
         "--action_env=GUIX_LOCPATH"
         "--action_env=TF_SYSTEM_LIBS"
         "--host_action_env=TF_SYSTEM_LIBS"
         "--host_action_env=PATH"
         "--host_action_env=LIBRARY_PATH"
         "--host_action_env=C_INCLUDE_PATH"
         "--host_action_env=CPLUS_INCLUDE_PATH"
         "--host_action_env=GUIX_LOCPATH"
         "-c" "opt"
         "--jobs"
         (if parallel-build?
             (number->string (parallel-job-count))
             "1")
         (match run-command
           (() build-targets)
           (_
            `(,@build-targets "--" ,@run-command)))))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (add-after 'unpack 'unpack-vendored-inputs unpack-vendored-inputs)
    (delete 'configure)
    (replace 'build build)
    (delete 'install)))

(define* (bazel-build #:key inputs (phases %standard-phases)
                      #:allow-other-keys #:rest args)
  "Build the given Bazel package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

;;; bazel-build-system.scm ends here
