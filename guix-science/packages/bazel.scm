;;; Copyright © 2023 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (guix-science packages bazel)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages java)
  #:use-module (gnu packages java-compression)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rpc))

;; TODO: all these jars need to be replaced with packages from Guix.
#;
(let ((files
       '(("netty" . #f)
         ("gson" . "java-gson")
         ("tomcat_annotations_api" . #f)
         ("allocation_instrumenter" . #f)
         ("protobuf_java" . #f)
         ("protobuf_java_util" . #f)
         ("apache_commons_collections" . "java-commons-collections")
         ("apache_commons_lang" . "java-commons-lang3")
         ("apache_commons_compress" . "java-commons-compress")
         ("apache_commons_pool2" . "java-commons-pool")
         ("apache_velocity" . #f)
         ("asm" . "java-asm")
         ("jackson2" . "java-fasterxml-jackson-core")
         ("jcip_annotations" . #f)
         ("jsr305" . "java-jsr305")
         ("javapoet" . #f)
         ("jaxb" . #f) ;maybe java-fasterxml-jackson-modules-base-jaxb?
         ("xz" . "java-xz")
         ("javax_annotations" . #f)
         ("android_common_25_0_0_lite" . #f)
         ("guava" . "java-guava")
         ("TODO:jacoco" . #f)
         ("escapevelocity" . #f)
         ("auto_common" . #f)
         ("auto_service_lib" . #f)
         ("auto_value_value" . #f)
         ("api_client" . #f)
         ("auth" . #f)
         ("error_prone_annotations" . #f)
         ("java-diff-utils" . #f)
         ("opencensus-api" . #f)
         ("perfmark-api" . #f)
         ("flogger" . #f)
         ("checker_framework_annotations" . #f)
         ("grpc-jar" . #f)
         ("junit4" . "java-junit")
         ("reactive_streams" . #f)
         ("rxjava3" . #f)
         ("caffeine" . #f))))
  #t)

(define-public bazel-6
  (package
    (name "bazel")
    (version "6.3.2")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append "https://github.com/bazelbuild/bazel/"
                                  "releases/download/" version
                                  "/bazel-" version "-dist.zip"))
              (sha256
               (base32
                "0j190j7vjknlw1cgynb3r8vlv0j6i9lac6s5payf4fqrb2ngxmwc"))
              (patches
               (search-patches "patches/bazel-mock-repos.patch"
                               "patches/bazel-workspace.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #false                  ;there are none
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (replace 'build
            (lambda _
              ;; Use local JDK only
              (setenv "EXTRA_BAZEL_ARGS"
                      (string-append
                       "\
--tool_java_runtime_version=local_jdk \
--noremote_accept_cached \
--verbose_failures \
--subcommands \
--action_env=PATH \
--action_env=LIBRARY_PATH \
--action_env=C_INCLUDE_PATH \
--action_env=CPLUS_INCLUDE_PATH \
--action_env=GUIX_LOCPATH \
--host_action_env=PATH \
--host_action_env=LIBRARY_PATH \
--host_action_env=C_INCLUDE_PATH \
--host_action_env=CPLUS_INCLUDE_PATH \
--host_action_env=GUIX_LOCPATH \
--define=distribution=debian
--override_repository=com_google_protobuf=" (getcwd) "/tools/distributions/debian/protobuf \
--override_repository=remote_java_tools_linux=" (getcwd) "/mock_repos/remote_java_tools_linux \
--override_repository=io_bazel_skydoc=" (getcwd) "/mock_repos/bazel_skydoc \
--override_repository=rules_cc=" (getcwd) "/mock_repos/rules_cc \
--override_repository=rules_java=" (getcwd) "/mock_repos/rules_java \
--override_repository=rules_proto=" (getcwd) "/mock_repos/rules_proto \
--override_repository=platforms=" (getcwd) "/mock_repos/platforms \
"))
              (substitute* "tools/distributions/debian/deps.bzl"
                (("/usr/include/google/protobuf")
                 (string-append #$(this-package-native-input "protobuf")
                                "/include/google/protobuf"))
                ;; TODO: /usr/bin/grpc_java_plugin
                ;; TODO: add all the java things
                (("\"grpc_java_plugin.*") "")
                (("/usr/bin/protoc")
                 (string-append #$(this-package-native-input "protobuf")
                                "/bin/protoc"))
                (("/usr/bin/grpc_cpp_plugin")
                 (string-append #$(this-package-input "grpc")
                                "/bin/grpc_cpp_plugin"))
                ;; Symlink the bundled jars in derived/jars to where
                ;; we can use them in debian_java.BUILD.
                (("\"java\": \"/usr/share/java\",")
                 (string-append "\"jars\": \"" (getcwd)
                                "/derived/jars\",")))
              ;; XXX: do not override any jar locations, because we
              ;; don't have all of them and we don't have any of them
              ;; in the locations that debian uses.
              (substitute* "tools/distributions/distribution_rules.bzl"
                (("if \"debian\"")
                 "if \"shmebian\""))
              ;; Make this work with our version of protobuf.
              (substitute* "third_party/grpc-java/compiler/src/java_plugin/cpp/java_generator.cpp"
                (("google/protobuf/compiler/java/java_names.h")
                 "google/protobuf/compiler/java/names.h"))
              ;; XXX: override the overrides for protobuf-util.jar and
              ;; protobuf.jar, because we don't have them.
              (substitute* "tools/distributions/debian/debian_java.BUILD"
                (("\"java/protobuf-util.jar\",")
                 (string-append "\""
                                "jars/com_google_protobuf/java/util/libutil.jar"
                                "\","))
                (("\"java/protobuf.jar\",")
                 (string-append "\""
                                "jars/com_google_protobuf/java/core/libcore.jar"
                                "\","))
                (("jars = \\[\"java/protobuf-util.jar\"")
                 (string-append "jars = [\""
                                "jars/com_google_protobuf/java/util/libutil.jar"
                                "\""))
                (("jars = \\[\"java/protobuf.jar\"")
                 (string-append "jars = [\""
                                "jars/com_google_protobuf/java/core/libcore.jar"
                                "\", \""
                                "jars/com_google_protobuf/java/core/liblite.jar"
                                "\",")))
              ;; Make sure that "cp" is found.
              (substitute* "third_party/grpc/build_defs.bzl"
                (("command = \"cp" m)
                 (string-append "use_default_shell_env = True,\n" m)))
              ;; XXX: we don't have a BUILD file in protobuf:/java/util
              (substitute* "src/main/java/com/google/devtools/build/lib/BUILD"
                (("\"@com_google_protobuf//java/util\",") ""))
              ;; XXX: these platforms are undefined
              (substitute* '("src/conditions/BUILD"
                             "src/conditions/BUILD.tools")
                (("@platforms//cpu:riscv64") "@platforms//cpu:x86_64")
                (("@platforms//cpu:mips64") "@platforms//cpu:x86_64"))
              ;; Ensure that "cat" can be found.
              (substitute* "src/main/java/com/google/devtools/build/lib/runtime/commands/license/merge_licenses.bzl"
                (("\"OUT\": ctx.outputs.out.path," m)
                 (string-append m "\n\"PATH\": \"" (getenv "PATH") "\",")))
              (substitute* "scripts/bootstrap/compile.sh"
                (("#!/bin/sh")
                 (string-append "#!" (which "sh"))))

              ;; XXX: We need to have /bin/bash and /usr/bin/env.  We
              ;; can't easily override this, so we run everything
              ;; inside of proot.
              (invoke "proot" "-b"
                      (string-append
                       #$static-bash "/bin/bash"
                       ":/bin/bash")
                      "-b"
                      (string-append
                       #$coreutils "/bin/env"
                       ":/usr/bin/env")
                      "bash" "compile.sh")))
          (replace 'install
            (lambda _
              (install-file "output/bazel"
                            (string-append #$output "/bin")))))))
    (inputs
     (list `(,openjdk11 "jdk")
           grpc
           python
           ;; We aren't yet using the jars provided by these packages.
           java-asm
           java-commons-collections
           java-commons-compress
           java-commons-lang3
           java-commons-pool
           java-fasterxml-jackson-core
           java-guava
           java-gson
           java-jsr305
           java-junit
           java-xz
           zlib))
    (native-inputs
     (list proot ;yikes!
           protobuf
           unzip
           util-linux
           which
           zip))
    (home-page "https://bazel.build")
    (synopsis "Build and test tool")
    (description
     "Bazel is a build and test tool similar to Make, Maven, and
Gradle.  It uses a human-readable, high-level build language.  Bazel
supports projects in multiple languages and builds outputs for
multiple platforms.  Bazel supports large codebases across multiple
repositories, and large numbers of users.")
    (license license:asl2.0)))
