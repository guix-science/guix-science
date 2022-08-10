;;; Copyright © 2022 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (guix-science services rstudio)
  #:use-module (gnu system pam)
  #:use-module (gnu system shadow)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages statistics)
  #:use-module (guix-science packages rstudio)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:export (rstudio-server-configuration
            rstudio-server-configuration?
            rstudio-server-configuration-package
            rstudio-server-configuration-default-r
            rstudio-server-configuration-server-user
            rstudio-server-configuration-pam
            rstudio-server-configuration-auth-none?
            rstudio-server-configuration-www-address
            rstudio-server-configuration-www-port

            rstudio-server-service-type))

(define-record-type* <rstudio-server-configuration>
  rstudio-server-configuration make-rstudio-server-configuration
  rstudio-server-configuration?
  (package     rstudio-server-configuration-package
               (default rstudio-server))
  (default-r   rstudio-server-configuration-default-r
               (default r-minimal))
  (server-user rstudio-server-configuration-server-user
               (default "rstudio-server"))
  (pam         rstudio-server-configuration-pam
               (default (pam-entry
                         (control "required")
                         (module "pam_unix.so"))))
  (auth-none?  rstudio-server-configuration-auth-none?
               (default #false))
  (www-address rstudio-server-configuration-www-address
               (default "127.0.0.1"))
  (www-port    rstudio-server-configuration-www-port
               (default "8899")))

(define (rstudio-server-accounts config)
  (let ((username (rstudio-server-configuration-server-user config)))
    (list (user-group
           (name username)
           (system? #t))
          (user-account
           (name username)
           (group username)
           (system? #t)
           (comment "System user for running the RStudio server")
           (home-directory "/var/empty")
           (shell (file-append shadow "/sbin/nologin"))))))

(define (rstudio-server-pam config)
  (let ((pam (rstudio-server-configuration-pam config)))
    (list (pam-service
           (name "rstudio")
           (account (list pam))
           (auth (list pam))
           (password (list pam))
           (session (list pam))))))

(define (rstudio-server-shepherd-services config)
  (define environment
    #~(list "LC_ALL=en_US.utf8"
            (string-append "GUIX_LOCPATH=" #$glibc-utf8-locales
                           "/lib/locale")))

  (match config
    (($ <rstudio-server-configuration>
        package default-r
        server-user pam
        auth-none?
        www-address www-port)
     (list (shepherd-service
            (provision '(rstudio-server))
            (documentation "Run RStudio Server.")
            (requirement '(networking))
            (start #~(make-forkexec-constructor
                      `(#$(file-append package "/bin/rserver")
                        ,(string-append "--rsession-which-r="
                                        #$(file-append default-r "/bin/R"))
                        ,@(if #$auth-none?
                              '("--auth-none=1")
                              '())
                        ,(string-append "--www-address=" #$www-address)
                        ,(string-append "--www-port=" #$www-port)
                        ,(string-append "--server-user=" #$server-user)
                        "--server-daemonize=0")
                      #:environment-variables #$environment))
            (stop #~(make-kill-destructor)))))))

(define rstudio-server-service-type
  (service-type
   (name 'rstudio-server)
   (extensions
    (list (service-extension account-service-type
                             rstudio-server-accounts)
          (service-extension shepherd-root-service-type
                             rstudio-server-shepherd-services)
          (service-extension pam-root-service-type
                             rstudio-server-pam)))
   (description
    "Run RStudio Server.")
   (default-value
     (rstudio-server-configuration))))
