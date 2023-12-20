;;;
;;; Copyright © 2021 Lars-Dominik Braun <ldb@leibniz-psychology.org>
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

;; Packages in this module have been auto-imported by the npm-binary importer.

;; Hints to future me:
;; - Find duplicates with
;;   > grep define-public guix-science/packages/rstudio-node.scm | sort | uniq -c -d
;; - node-fsevents is macos only.
;; - Get all deps:
;;   jq -r '.dependencies | to_entries | map(.key + " " + .value) | join("\n")' < src/gwt/panmirror/src/editor/package.json| grep -v '^@'
;; - For failures like this one:
;;   npm ERR! > orderedmap@2.1.1 build
;;   npm ERR! > rollup -c
;;   npm ERR! sh: line 1: rollup: command not found
;;   track down orderedmap and remove rollup from package.json

(define-module (guix-science packages rstudio-node)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system node)
  #:use-module (gnu packages node-xyz))

(define-public node-abbrev-1.1.1
  (package
    (name "node-abbrev")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/abbrev/-/abbrev-1.1.1.tgz")
              (sha256
               (base32
                "0vdsff38rgn0qylyj6x42n13bnxfqxb9ql34bzs4z9grlli9vh8c"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("tap")))))))
    (home-page "https://github.com/isaacs/abbrev-js#readme")
    (synopsis "Like ruby's abbrev module, but in js")
    (description "Like ruby's abbrev module, but in js")
    (license license:isc)))

(define-public node-accepts-1.3.8
  (package
    (name "node-accepts")
    (version "1.3.8")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/accepts/-/accepts-1.3.8.tgz")
              (sha256
               (base32
                "0xbds0r4v51s5lprkr6snag2xr5ssbavh9hmqygj4y427z59l65z"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("deep-equal" "eslint"
                                             "eslint-config-standard"
                                             "eslint-plugin-import"
                                             "eslint-plugin-markdown"
                                             "eslint-plugin-node"
                                             "eslint-plugin-promise"
                                             "eslint-plugin-standard"
                                             "mocha"
                                             "nyc")))))))
    (inputs (list node-negotiator-0.6.3 node-mime-types-2.1.35))
    (home-page "https://github.com/jshttp/accepts#readme")
    (synopsis "Higher-level content negotiation")
    (description "Higher-level content negotiation")
    (license license:expat)))

(define-public node-acorn-5.7.4
  (package
    (inherit node-acorn)
    (name "node-acorn")
    (version "5.7.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/acornjs/acorn")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "10krd3s0fzglwvk0khb4px82amjmf30qalv789i3vbfvln26ar3y"))))
    (arguments
     (substitute-keyword-arguments (package-arguments node-acorn)
       ((#:phases phases
         '%standard-phases)
        `(modify-phases ,phases
           (delete 'change-directory)
           (add-after 'patch-dependencies 'delete-dev-dependencies
             (lambda _
               (delete-dependencies '("eslint" "eslint-config-standard"
                                      "eslint-plugin-import"
                                      "eslint-plugin-node"
                                      "eslint-plugin-promise"
                                      "eslint-plugin-standard"
                                      "rollup"
                                      "rollup-plugin-buble"
                                      "test262"
                                      "test262-parser-runner"
                                      "unicode-11.0.0"))))))))))

(define-public node-acorn-jsx-4.1.1
  (package
    (name "node-acorn-jsx")
    (version "4.1.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/acorn-jsx/-/acorn-jsx-4.1.1.tgz")
              (sha256
               (base32
                "1rzhpi0ak9pynyrvb5pmzvh0pypzpnx9xxrs2y7b0cgg093r5imm"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("chai" "mocha")))))))
    (inputs (list node-acorn-5.7.4))
    (home-page "https://github.com/RReverser/acorn-jsx")
    (synopsis "Alternative, faster React.js JSX parser")
    (description "Alternative, faster React.js JSX parser")
    (license license:expat)))

(define-public node-agent-base-6.0.2
  (package
    (name "node-agent-base")
    (version "6.0.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/agent-base/-/agent-base-6.0.2.tgz")
              (sha256
               (base32
                "0cg85gngrap12xzz8ibdjw98hcfhgcidg2w7ll7whsrf59ps0vdw"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@types/debug" "@types/mocha"
                                             "@types/node"
                                             "@types/semver"
                                             "@types/ws"
                                             "@typescript-eslint/eslint-plugin"
                                             "@typescript-eslint/parser"
                                             "async-listen"
                                             "cpy-cli"
                                             "eslint"
                                             "eslint-config-airbnb"
                                             "eslint-config-prettier"
                                             "eslint-import-resolver-typescript"
                                             "eslint-plugin-import"
                                             "eslint-plugin-jsx-a11y"
                                             "eslint-plugin-react"
                                             "mocha"
                                             "rimraf"
                                             "semver"
                                             "typescript"
                                             "ws")))))))
    (inputs (list node-debug-4.3.4))
    (home-page "https://github.com/TooTallNate/node-agent-base#readme")
    (synopsis "Turn a function into an `http.Agent` instance")
    (description "Turn a function into an `http.Agent` instance")
    (license license:expat)))

(define-public node-ajax-request-1.2.3
  (package
    (name "node-ajax-request")
    (version "1.2.3")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/ajax-request/-/ajax-request-1.2.3.tgz")
              (sha256
               (base32
                "0pkzvkblcmj8wxar7bw61jxfghzki12awi15gm1v2qsyfa42blva"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("body-parser" "express" "grunt"
                                             "grunt-contrib-jshint"
                                             "serve-static")))))))
    (inputs (list node-utils-extend-1.0.8 node-file-system-2.2.2))
    (home-page "https://github.com/douzi8/ajax-request#readme")
    (synopsis "Http request for nodejs, and it also support file download")
    (description "Http request for nodejs, and it also support file download")
    (license license:isc)))

(define-public node-ajv-6.12.6
  (package
    (name "node-ajv")
    (version "6.12.6")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/ajv/-/ajv-6.12.6.tgz")
              (sha256
               (base32
                "0jhk2dnzrk188p3micnkh7126lhdbkj9iip0pywhky6vh1dk8xcr"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ajv-async" "bluebird"
                                             "brfs"
                                             "browserify"
                                             "chai"
                                             "coveralls"
                                             "del-cli"
                                             "dot"
                                             "eslint"
                                             "gh-pages-generator"
                                             "glob"
                                             "if-node-version"
                                             "js-beautify"
                                             "jshint"
                                             "json-schema-test"
                                             "karma"
                                             "karma-chrome-launcher"
                                             "karma-mocha"
                                             "karma-sauce-launcher"
                                             "mocha"
                                             "nyc"
                                             "pre-commit"
                                             "require-globify"
                                             "typescript"
                                             "uglify-js"
                                             "watch")))))))
    (inputs (list node-uri-js-4.4.1 node-json-schema-traverse-0.4.1
                  node-fast-json-stable-stringify-2.1.0
                  node-fast-deep-equal-3.1.3))
    (home-page "https://github.com/ajv-validator/ajv")
    (synopsis "Another JSON Schema Validator")
    (description "Another JSON Schema Validator")
    (license license:expat)))

(define-public node-ansi-0.3.1
  (package
    (name "node-ansi")
    (version "0.3.1")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/ansi/-/ansi-0.3.1.tgz")
              (sha256
               (base32
                "0jbf21lf5jsqs63ki1dqzx4llqr7wvlc4jy8hhdyy7qvhglsj187"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://github.com/TooTallNate/ansi.js#readme")
    (synopsis "Advanced ANSI formatting tool for Node.js")
    (description "Advanced ANSI formatting tool for Node.js")
    (license license:expat)))

(define-public node-ansi-escapes-3.2.0
  (package
    (name "node-ansi-escapes")
    (version "3.2.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/ansi-escapes/-/ansi-escapes-3.2.0.tgz")
              (sha256
               (base32
                "13ags8v3fa8ijgkhajaqf6nc5s5xhhxkkn9c44npgnwhdcx93v1f"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "xo")))))))
    (home-page "https://github.com/sindresorhus/ansi-escapes#readme")
    (synopsis "ANSI escape codes for manipulating the terminal")
    (description "ANSI escape codes for manipulating the terminal")
    (license license:expat)))

(define-public node-ansi-regex-3.0.1
  (package
    (name "node-ansi-regex")
    (version "3.0.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/ansi-regex/-/ansi-regex-3.0.1.tgz")
              (sha256
               (base32
                "0mv4lz09i6n3l427cq83ac590hy3ah6dr6w91n4pwygq5940m74q"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "xo")))))))
    (home-page "https://github.com/chalk/ansi-regex#readme")
    (synopsis "Regular expression for matching ANSI escape codes")
    (description "Regular expression for matching ANSI escape codes")
    (license license:expat)))

(define-public node-ansi-regex-5.0.1
  (package
    (name "node-ansi-regex")
    (version "5.0.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/ansi-regex/-/ansi-regex-5.0.1.tgz")
              (sha256
               (base32
                "1ng0r2k4mcz7b2bfr6g1dschnxm0vifaslsvv2smv06smb6ss3hf"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "tsd" "xo")))))))
    (home-page "https://github.com/chalk/ansi-regex#readme")
    (synopsis "Regular expression for matching ANSI escape codes")
    (description "Regular expression for matching ANSI escape codes")
    (license license:expat)))

(define-public node-ansi-styles-3.2.1
  (package
    (name "node-ansi-styles")
    (version "3.2.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/ansi-styles/-/ansi-styles-3.2.1.tgz")
              (sha256
               (base32
                "1wqd08glq159q724kvpi6nnf87biajr749a7r9c84xm639g6463k"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "babel-polyfill"
                                             "svg-term-cli" "xo")))))))
    (inputs (list node-color-convert-1.9.3))
    (home-page "https://github.com/chalk/ansi-styles#readme")
    (synopsis "ANSI escape codes for styling strings in the terminal")
    (description "ANSI escape codes for styling strings in the terminal")
    (license license:expat)))

(define-public node-ansi-styles-4.3.0
  (package
    (name "node-ansi-styles")
    (version "4.3.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/ansi-styles/-/ansi-styles-4.3.0.tgz")
              (sha256
               (base32
                "0zwqsx67hr7m4a8dpd0jzkp2rjm5v7938x4rhcqh7djsv139llrc"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@types/color-convert" "ava"
                                             "svg-term-cli" "tsd" "xo")))))))
    (inputs (list node-color-convert-2.0.1))
    (home-page "https://github.com/chalk/ansi-styles#readme")
    (synopsis "ANSI escape codes for styling strings in the terminal")
    (description "ANSI escape codes for styling strings in the terminal")
    (license license:expat)))

(define-public node-anymatch-1.3.2
  (package
    (name "node-anymatch")
    (version "1.3.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/anymatch/-/anymatch-1.3.2.tgz")
              (sha256
               (base32
                "1lr9c0ki26rl2xnbiqj8smgw0b5r5ns3jxmr41s4cwbkrh6jjfxs"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("coveralls" "istanbul" "mocha")))))))
    (inputs (list node-normalize-path-2.1.1 node-micromatch-2.3.11))
    (home-page "https://github.com/es128/anymatch")
    (synopsis
     "Matches strings against configurable strings, globs, regular expressions, and/or functions")
    (description
     "Matches strings against configurable strings, globs, regular expressions, and/or functions")
    (license license:isc)))

(define-public node-app-root-path-1.4.0
  (package
    (name "node-app-root-path")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/app-root-path/-/app-root-path-1.4.0.tgz")
              (sha256
               (base32
                "00ccw98bvkbrzrwf40ykq6jlckvyxbag1sl8answjqg86h2xs3lb"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("codecov" "coveralls"
                                             "cracks"
                                             "cz-conventional-changelog"
                                             "ghooks"
                                             "istanbul"
                                             "mocha"
                                             "mocha-lcov-reporter"
                                             "nyc"
                                             "semantic-release"
                                             "validate-commit-msg")))))))
    (home-page "https://github.com/inxilpro/node-app-root-path")
    (synopsis "Determine an app's root path from anywhere inside the app")
    (description "Determine an app's root path from anywhere inside the app")
    (license license:expat)))

(define-public node-app-root-path-2.2.1
  (package
    (name "node-app-root-path")
    (version "2.2.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/app-root-path/-/app-root-path-2.2.1.tgz")
              (sha256
               (base32
                "0qjv4w1qhkqwri9iscvzh2fhvc4qv25mj58anqd61nbrh5n1la54"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("codecov" "coveralls"
                                             "cracks"
                                             "cz-conventional-changelog"
                                             "ghooks"
                                             "istanbul"
                                             "mocha"
                                             "mocha-lcov-reporter"
                                             "mockery"
                                             "nyc"
                                             "semantic-release"
                                             "validate-commit-msg")))))))
    (home-page "https://github.com/inxilpro/node-app-root-path")
    (synopsis "Determine an app's root path from anywhere inside the app")
    (description "Determine an app's root path from anywhere inside the app")
    (license license:expat)))

(define-public node-aproba-2.0.0
  (package
    (name "node-aproba")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/aproba/-/aproba-2.0.0.tgz")
              (sha256
               (base32
                "1x9b0j6z15bnmhbw8fca0xnafj9ci1y091r0l0ms72rbbqb4zchc"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("standard" "tap")))))))
    (home-page "https://github.com/iarna/aproba")
    (synopsis
     "A ridiculously light-weight argument validator (now browser friendly)")
    (description
     "A ridiculously light-weight argument validator (now browser friendly)")
    (license license:isc)))

(define-public node-are-we-there-yet-2.0.0
  (package
    (name "node-are-we-there-yet")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/are-we-there-yet/-/are-we-there-yet-2.0.0.tgz")
              (sha256
               (base32
                "0dnd6fpia4mm58zlilx5wk2dqssmv157ckqj6vagkdks7jrxxn7m"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@npmcli/eslint-config"
                                             "@npmcli/template-oss" "eslint"
                                             "eslint-plugin-node" "tap")))))))
    (inputs (list node-readable-stream-3.6.2 node-delegates-1.0.0))
    (home-page "https://github.com/npm/are-we-there-yet")
    (synopsis
     "Keep track of the overall completion of many disparate processes")
    (description
     "Keep track of the overall completion of many disparate processes")
    (license license:isc)))

(define-public node-argparse-2.0.1
  (package
    (name "node-argparse")
    (version "2.0.1")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/argparse/-/argparse-2.0.1.tgz")
              (sha256
               (base32
                "133jjyhcr25rf4vy7bca7x06dfmsyy819s1kbbyfc5c2zi3ki417"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@babel/eslint-parser"
                                             "@babel/plugin-syntax-class-properties"
                                             "eslint" "mocha" "nyc")))))))
    (home-page "https://github.com/nodeca/argparse#readme")
    (synopsis "CLI arguments parser. Native port of python's argparse.")
    (description "CLI arguments parser. Native port of python's argparse.")
    (license #f)))

(define-public node-arr-diff-2.0.0
  (package
    (name "node-arr-diff")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/arr-diff/-/arr-diff-2.0.0.tgz")
              (sha256
               (base32
                "180fm2zz2qqygr715i4dwdpqypvf2gap4wds1crdhpdlbc3c2wpd"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("array-differ" "array-slice"
                                             "benchmarked" "chalk" "mocha"
                                             "should")))))))
    (inputs (list node-arr-flatten-1.1.0))
    (home-page "https://github.com/jonschlinkert/arr-diff")
    (synopsis
     "Returns an array with only the unique values from the first array, by excluding all values from additional arrays using strict equality for comparisons.")
    (description
     "Returns an array with only the unique values from the first array, by excluding all values from additional arrays using strict equality for comparisons.")
    (license license:expat)))

(define-public node-arr-diff-4.0.0
  (package
    (name "node-arr-diff")
    (version "4.0.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/arr-diff/-/arr-diff-4.0.0.tgz")
              (sha256
               (base32
                "1735byq6vmrvqkv9n5400494mh9vd6x4knzkybgr55km5dgpdcyx"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ansi-bold" "arr-flatten"
                                             "array-differ"
                                             "benchmarked"
                                             "gulp-format-md"
                                             "minimist"
                                             "mocha")))))))
    (home-page "https://github.com/jonschlinkert/arr-diff")
    (synopsis
     "Returns an array with only the unique values from the first array, by excluding all values from additional arrays using strict equality for comparisons.")
    (description
     "Returns an array with only the unique values from the first array, by excluding all values from additional arrays using strict equality for comparisons.")
    (license license:expat)))

(define-public node-arr-flatten-1.1.0
  (package
    (name "node-arr-flatten")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/arr-flatten/-/arr-flatten-1.1.0.tgz")
              (sha256
               (base32
                "0amnq01y6y8j49rdcib9yh8n95wk2ajs3qcckccvv1x6bf6nfvay"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ansi-bold" "array-flatten"
                                             "array-slice"
                                             "benchmarked"
                                             "compute-flatten"
                                             "flatit"
                                             "flatten"
                                             "flatten-array"
                                             "glob"
                                             "gulp-format-md"
                                             "just-flatten-it"
                                             "lodash.flattendeep"
                                             "m_flattened"
                                             "mocha"
                                             "utils-flatten"
                                             "write")))))))
    (home-page "https://github.com/jonschlinkert/arr-flatten")
    (synopsis "Recursively flatten an array or arrays.")
    (description "Recursively flatten an array or arrays.")
    (license license:expat)))

(define-public node-arr-union-3.1.0
  (package
    (name "node-arr-union")
    (version "3.1.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/arr-union/-/arr-union-3.1.0.tgz")
              (sha256
               (base32
                "1jrcfq6xnx3lbvnpl9pzfvc2v3bcgyir1vwcc7p0slhf6gglzd3p"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ansi-bold" "array-union"
                                             "array-unique"
                                             "benchmarked"
                                             "gulp-format-md"
                                             "minimist"
                                             "mocha"
                                             "should")))))))
    (home-page "https://github.com/jonschlinkert/arr-union")
    (synopsis
     "Combines a list of arrays, returning a single array with unique values, using strict equality for comparisons.")
    (description
     "Combines a list of arrays, returning a single array with unique values, using strict equality for comparisons.")
    (license license:expat)))

(define-public node-array-flatten-1.1.1
  (package
    (name "node-array-flatten")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/array-flatten/-/array-flatten-1.1.1.tgz")
              (sha256
               (base32
                "1v96cj6w6f7g61c7fjfkxpkbbfkxl2ksh5zm7y5mfp96xivi5jhs"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("istanbul" "mocha" "pre-commit"
                                             "standard")))))))
    (home-page "https://github.com/blakeembrey/array-flatten")
    (synopsis "Flatten an array of nested arrays into a single flat array")
    (description "Flatten an array of nested arrays into a single flat array")
    (license license:expat)))

(define-public node-array-unique-0.2.1
  (package
    (name "node-array-unique")
    (version "0.2.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/array-unique/-/array-unique-0.2.1.tgz")
              (sha256
               (base32
                "12j74kq24w0xpj6zxdqj0zlgr97ls02yqph2vbbk05q7bgbkckvy"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("array-uniq" "benchmarked"
                                             "mocha" "should")))))))
    (home-page "https://github.com/jonschlinkert/array-unique")
    (synopsis
     "Return an array free of duplicate values. Fastest ES5 implementation.")
    (description
     "Return an array free of duplicate values. Fastest ES5 implementation.")
    (license license:expat)))

(define-public node-array-unique-0.3.2
  (package
    (name "node-array-unique")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/array-unique/-/array-unique-0.3.2.tgz")
              (sha256
               (base32
                "0bkrb481qri7qad5h4bzw9hq161wfqgxdj5g11a5bnlfylqczg9g"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("array-uniq" "benchmarked"
                                             "gulp-format-md" "mocha" "should")))))))
    (home-page "https://github.com/jonschlinkert/array-unique")
    (synopsis
     "Remove duplicate values from an array. Fastest ES5 implementation.")
    (description
     "Remove duplicate values from an array. Fastest ES5 implementation.")
    (license license:expat)))

(define-public node-asn1-0.2.6
  (package
    (name "node-asn1")
    (version "0.2.6")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/asn1/-/asn1-0.2.6.tgz")
              (sha256
               (base32
                "0l72iwxdhwl7h4qafw9pxrxvas4r5ck3knhb66p83xp337znzjhp"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("istanbul" "faucet" "tape"
                                             "eslint" "eslint-plugin-joyent")))))))
    (inputs (list node-safer-buffer-2.1.2))
    (home-page "https://github.com/joyent/node-asn1#readme")
    (synopsis
     "Contains parsers and serializers for ASN.1 (currently BER only)")
    (description
     "Contains parsers and serializers for ASN.1 (currently BER only)")
    (license license:expat)))

(define-public node-assert-plus-1.0.0
  (package
    (name "node-assert-plus")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/assert-plus/-/assert-plus-1.0.0.tgz")
              (sha256
               (base32
                "1srkj0nyslz3rbfncj59sqbsllavmwik8gphd7jxwjshf52mras7"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("tape" "faucet")))))))
    (home-page "https://github.com/mcavage/node-assert-plus#readme")
    (synopsis "Extra assertions on top of node's assert module")
    (description "Extra assertions on top of node's assert module")
    (license license:expat)))

(define-public node-assign-symbols-1.0.0
  (package
    (name "node-assign-symbols")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/assign-symbols/-/assign-symbols-1.0.0.tgz")
              (sha256
               (base32
                "1lpcb6gzhdl4l9843kifsz9z48qwpk5gw6b1h6f6n7h5d8qp2xab"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("mocha")))))))
    (home-page "https://github.com/jonschlinkert/assign-symbols")
    (synopsis
     "Assign the enumerable es6 Symbol properties from an object (or objects) to the first object passed on the arguments. Can be used as a supplement to other extend, assign or merge methods as a polyfill for the Symbols part of the es6 Object.assign method.")
    (description
     "Assign the enumerable es6 Symbol properties from an object (or objects) to the first object passed on the arguments. Can be used as a supplement to other extend, assign or merge methods as a polyfill for the Symbols part of the es6 Object.assign method.")
    (license license:expat)))

(define-public node-async-each-1.0.6
  (package
    (name "node-async-each")
    (version "1.0.6")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/async-each/-/async-each-1.0.6.tgz")
              (sha256
               (base32
                "0gwpwdm80y3bj88m63yjjsp7v8xvsmmm8d6wm1hmnp0gk4wsb82b"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://github.com/paulmillr/async-each/")
    (synopsis
     "No-bullshit, ultra-simple, 35-lines-of-code async parallel forEach / map function for JavaScript.")
    (description
     "No-bullshit, ultra-simple, 35-lines-of-code async parallel forEach / map function for JavaScript.")
    (license license:expat)))

(define-public node-asynckit-0.4.0
  (package
    (name "node-asynckit")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/asynckit/-/asynckit-0.4.0.tgz")
              (sha256
               (base32
                "1kvxnmjbjwqc8gvp4ms7d8w8x7y41rcizmz4898694h7ywq4y9cc"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("browserify"
                                             "browserify-istanbul"
                                             "coveralls"
                                             "eslint"
                                             "istanbul"
                                             "obake"
                                             "phantomjs-prebuilt"
                                             "pre-commit"
                                             "reamde"
                                             "rimraf"
                                             "size-table"
                                             "tap-spec"
                                             "tape")))))))
    (home-page "https://github.com/alexindigo/asynckit#readme")
    (synopsis "Minimal async jobs utility library, with streams support")
    (description "Minimal async jobs utility library, with streams support")
    (license license:expat)))

(define-public node-atob-2.1.2
  (package
    (name "node-atob")
    (version "2.1.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/atob/-/atob-2.1.2.tgz")
              (sha256
               (base32
                "1nmrnfpzg9a99i4p88knw3470d5vcqmm3hyhavlln96wnza2lbg5"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://git.coolaj86.com/coolaj86/atob.js.git")
    (synopsis
     "atob for Node.JS and Linux / Mac / Windows CLI (it's a one-liner)")
    (description
     "atob for Node.JS and Linux / Mac / Windows CLI (it's a one-liner)")
    (license #f)))

(define-public node-aws-sign2-0.7.0
  (package
    (name "node-aws-sign2")
    (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/aws-sign2/-/aws-sign2-0.7.0.tgz")
              (sha256
               (base32
                "12bjw01pgh0nfyxi7vw6lvsapyvnazrnyn4qf87lclardjaz9yd8"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://github.com/mikeal/aws-sign#readme")
    (synopsis
     "AWS signing. Originally pulled from LearnBoost/knox, maintained as vendor in request, now a standalone module.")
    (description
     "AWS signing. Originally pulled from LearnBoost/knox, maintained as vendor in request, now a standalone module.")
    (license license:asl2.0)))

(define-public node-aws4-1.12.0
  (package
    (name "node-aws4")
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/aws4/-/aws4-1.12.0.tgz")
              (sha256
               (base32
                "073xjq7cq7x4wqpjay0bzzd3ssbyp0salb5d4wh1mksaa9xl2ccz"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("mocha" "should")))))))
    (home-page "https://github.com/mhart/aws4#readme")
    (synopsis "Signs and prepares requests using AWS Signature Version 4")
    (description "Signs and prepares requests using AWS Signature Version 4")
    (license license:expat)))

(define-public node-babel-code-frame-7.22.5
  (package
    (name "node-babel-code-frame")
    (version "7.22.5")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/@babel/code-frame/-/code-frame-7.22.5.tgz")
              (sha256
               (base32
                "0kcqhh0lkb3cvjrhvw0jm10l0bj6y06il15vdd06dcv8fn02v1ld"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("strip-ansi")))))))
    (inputs (list node-babel-highlight-7.22.5))
    (home-page "https://babel.dev/docs/en/next/babel-code-frame")
    (synopsis
     "Generate errors that contain a code frame that point to source locations.")
    (description
     "Generate errors that contain a code frame that point to source locations.")
    (license license:expat)))

(define-public node-babel-helper-module-imports-7.22.5
  (package
    (name "node-babel-helper-module-imports")
    (version "7.22.5")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/@babel/helper-module-imports/-/helper-module-imports-7.22.5.tgz")
              (sha256
               (base32
                "053s7572rif04kxf2vfs76bwzv3xl0fp69zbjn31f2bpr2hsxir5"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@babel/core" "@babel/traverse")))))))
    (inputs (list node-babel-types-7.22.5))
    (home-page "https://babel.dev/docs/en/next/babel-helper-module-imports")
    (synopsis "Babel helper functions for inserting module loads")
    (description "Babel helper functions for inserting module loads")
    (license license:expat)))

(define-public node-babel-helper-string-parser-7.22.5
  (package
    (name "node-babel-helper-string-parser")
    (version "7.22.5")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/@babel/helper-string-parser/-/helper-string-parser-7.22.5.tgz")
              (sha256
               (base32
                "18ynmnd6m6nsrbkcvvzl6gf5xid4m8xv17n906iajxjzpazy8fb8"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("charcodes")))))))
    (home-page "https://babel.dev/docs/en/next/babel-helper-string-parser")
    (synopsis "A utility package to parse strings")
    (description "A utility package to parse strings")
    (license license:expat)))

(define-public node-babel-helper-validator-identifier-7.22.5
  (package
    (name "node-babel-helper-validator-identifier")
    (version "7.22.5")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/@babel/helper-validator-identifier/-/helper-validator-identifier-7.22.5.tgz")
              (sha256
               (base32
                "1hfnmqk486lq54i5k9i72rx3xr3xdpnfkfrba0083wsvsfs1yyhl"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@unicode/unicode-15.0.0"
                                             "charcodes")))))))
    (home-page
     "https://www.npmjs.com/package/node-babel-helper-validator-identifier")
    (synopsis "Validate identifier/keywords name")
    (description "Validate identifier/keywords name")
    (license license:expat)))

(define-public node-babel-highlight-7.22.5
  (package
    (name "node-babel-highlight")
    (version "7.22.5")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/@babel/highlight/-/highlight-7.22.5.tgz")
              (sha256
               (base32
                "0wf1h5bz0gg8mmqqmf1gwm5s4vv780r0hsicqn3wm26a5rg5499f"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@types/chalk" "strip-ansi")))))))
    (inputs (list node-js-tokens-4.0.0 node-chalk-2.4.2
                  node-babel-helper-validator-identifier-7.22.5))
    (home-page "https://babel.dev/docs/en/next/babel-highlight")
    (synopsis "Syntax highlight JavaScript strings for output in terminals.")
    (description
     "Syntax highlight JavaScript strings for output in terminals.")
    (license license:expat)))

(define-public node-babel-plugin-emotion-9.2.11
  (package
    (name "node-babel-plugin-emotion")
    (version "9.2.11")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/babel-plugin-emotion/-/babel-plugin-emotion-9.2.11.tgz")
              (sha256
               (base32
                "160acyl7sxcg285y63cwlji8fqa11bra8wbdlhxcqx42ikpcsj74"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("babel-check-duplicated-nodes"
                                             "babel-core")))))))
    (inputs (list node-touch-2.0.2
                  node-source-map-0.5.7
                  node-mkdirp-0.5.6
                  node-find-root-1.1.0
                  node-convert-source-map-1.9.0
                  node-babel-plugin-syntax-jsx-6.18.0
                  node-babel-plugin-macros-2.8.0
                  node-emotion-stylis-0.7.1
                  node-emotion-memoize-0.6.6
                  node-emotion-hash-0.6.6
                  node-emotion-babel-utils-0.6.10
                  node-babel-helper-module-imports-7.22.5))
    (home-page "https://emotion.sh")
    (synopsis
     "A recommended babel preprocessing plugin for emotion, The Next Generation of CSS-in-JS.")
    (description
     "A recommended babel preprocessing plugin for emotion, The Next Generation of CSS-in-JS.")
    (license license:expat)))

(define-public node-babel-plugin-macros-2.8.0
  (package
    (name "node-babel-plugin-macros")
    (version "2.8.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/babel-plugin-macros/-/babel-plugin-macros-2.8.0.tgz")
              (sha256
               (base32
                "03ymharkmanc9xclccgh17v7m7i2366jcraqwqzb7gc5bvpmrrb2"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@babel/core" "@babel/parser"
                                             "@babel/types"
                                             "ast-pretty-print"
                                             "babel-plugin-tester"
                                             "babel-plugin-transform-es2015-modules-commonjs"
                                             "cpy"
                                             "kcd-scripts")))))))
    (inputs (list node-resolve-1.22.3 node-cosmiconfig-6.0.0
                  node-babel-runtime-7.22.5))
    (home-page "https://github.com/kentcdodds/babel-plugin-macros#readme")
    (synopsis "Allows you to build compile-time libraries")
    (description "Allows you to build compile-time libraries")
    (license license:expat)))

(define-public node-babel-plugin-macros-3.1.0
  (package
    (name "node-babel-plugin-macros")
    (version "3.1.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/babel-plugin-macros/-/babel-plugin-macros-3.1.0.tgz")
              (sha256
               (base32
                "086k5i9wmw2lhly3lawl3bfjvsm9crnr7aj95zbr9njiwaw84c6h"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@babel/core" "@babel/parser"
                                             "@babel/types"
                                             "ast-pretty-print"
                                             "babel-plugin-tester"
                                             "babel-plugin-transform-es2015-modules-commonjs"
                                             "cpy"
                                             "kcd-scripts")))))))
    (inputs (list node-resolve-1.22.3 node-cosmiconfig-7.1.0
                  node-babel-runtime-7.22.5))
    (home-page "https://github.com/kentcdodds/babel-plugin-macros#readme")
    (synopsis "Allows you to build compile-time libraries")
    (description "Allows you to build compile-time libraries")
    (license license:expat)))

(define-public node-babel-plugin-syntax-jsx-6.18.0
  (package
    (name "node-babel-plugin-syntax-jsx")
    (version "6.18.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/babel-plugin-syntax-jsx/-/babel-plugin-syntax-jsx-6.18.0.tgz")
              (sha256
               (base32
                "0nlwixy37wj48m275k0w66cq8cpdnwxa5fnv4hav83nvamqz6a1g"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://www.npmjs.com/package/node-babel-plugin-syntax-jsx")
    (synopsis "Allow parsing of jsx")
    (description "Allow parsing of jsx")
    (license license:expat)))

(define-public node-babel-runtime-6.26.0
  (package
    (name "node-babel-runtime")
    (version "6.26.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/babel-runtime/-/babel-runtime-6.26.0.tgz")
              (sha256
               (base32
                "0swl6f8bw62qydhkq6qq53c8afxai4cqpd4rg7270jvl8s4lilhl"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("babel-helpers"
                                             "babel-plugin-transform-runtime")))))))
    (inputs (list node-regenerator-runtime-0.11.1 node-core-js-2.6.12))
    (home-page "https://www.npmjs.com/package/node-babel-runtime")
    (synopsis "babel selfContained runtime")
    (description "babel selfContained runtime")
    (license license:expat)))

(define-public node-babel-runtime-7.22.5
  (package
    (name "node-babel-runtime")
    (version "7.22.5")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/@babel/runtime/-/runtime-7.22.5.tgz")
              (sha256
               (base32
                "0d5bm0wcvz1s44xj051p9xsq2s6cfdgsm441a0frcnyxrqzbpilg"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (inputs (list node-regenerator-runtime-0.13.11))
    (home-page "https://babel.dev/docs/en/next/babel-runtime")
    (synopsis "babel's modular runtime helpers")
    (description "babel's modular runtime helpers")
    (license license:expat)))

(define-public node-babel-runtime-corejs3-7.22.5
  (package
    (name "node-babel-runtime-corejs3")
    (version "7.22.5")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/@babel/runtime-corejs3/-/runtime-corejs3-7.22.5.tgz")
              (sha256
               (base32
                "01969ykcjg3yzzv0f7ywaqwhsmlnrm4afxlgcysk4dmzx49m55nq"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (inputs (list node-regenerator-runtime-0.13.11 node-core-js-pure-3.30.2))
    (home-page "https://www.npmjs.com/package/node-babel-runtime-corejs3")
    (synopsis "babel's modular runtime helpers with core-js@@3 polyfilling")
    (description "babel's modular runtime helpers with core-js@@3 polyfilling")
    (license license:expat)))

(define-public node-babel-types-7.22.5
  (package
    (name "node-babel-types")
    (version "7.22.5")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/@babel/types/-/types-7.22.5.tgz")
              (sha256
               (base32
                "14irmg5qq107fw63n27qr11kawh8sycpk0jln4rrrgbbycgqj5wa"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@babel/generator"
                                             "@babel/parser" "chalk" "glob")))))))
    (inputs (list node-to-fast-properties-2.0.0
                  node-babel-helper-validator-identifier-7.22.5
                  node-babel-helper-string-parser-7.22.5))
    (home-page "https://babel.dev/docs/en/next/babel-types")
    (synopsis "Babel Types is a Lodash-esque utility library for AST nodes")
    (description "Babel Types is a Lodash-esque utility library for AST nodes")
    (license license:expat)))

(define-public node-balanced-match-1.0.2
  (package
    (name "node-balanced-match")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/balanced-match/-/balanced-match-1.0.2.tgz")
              (sha256
               (base32
                "1hdwrr7qqb37plj7962xbwjx1jvjz7ahl7iqrwh82yhcvnmzfm6q"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("matcha" "tape")))))))
    (home-page "https://github.com/juliangruber/balanced-match")
    (synopsis "Match balanced character pairs, like \"{\" and \"}\"")
    (description "Match balanced character pairs, like \"{\" and \"}\"")
    (license license:expat)))

(define-public node-base-0.11.2
  (package
    (name "node-base")
    (version "0.11.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/base/-/base-0.11.2.tgz")
              (sha256
               (base32
                "0wh3b37238q4x15diq4vp2vx6d6zqh9z0559p0kk6xh0v9b95ca0"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp" "gulp-eslint"
                                             "gulp-format-md"
                                             "gulp-istanbul"
                                             "gulp-mocha"
                                             "helper-coverage"
                                             "mocha"
                                             "should"
                                             "through2"
                                             "verb-generate-readme")))))))
    (inputs (list node-pascalcase-0.1.1
                  node-mixin-deep-1.3.2
                  node-isobject-3.0.1
                  node-define-property-1.0.0
                  node-component-emitter-1.3.0
                  node-class-utils-0.3.6
                  node-cache-base-1.0.1))
    (home-page "https://github.com/node-base/base")
    (synopsis
     "base is the foundation for creating modular, unit testable and highly pluggable node.js applications, starting with a handful of common methods, like `set`, `get`, `del` and `use`.")
    (description
     "base is the foundation for creating modular, unit testable and highly pluggable node.js applications, starting with a handful of common methods, like `set`, `get`, `del` and `use`.")
    (license license:expat)))

(define-public node-base16-1.0.0
  (package
    (name "node-base16")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/base16/-/base16-1.0.0.tgz")
              (sha256
               (base32
                "07jqwqxly1ia87yki3kr0cv25jslkasmad55i3yxfkd9w16dxx59"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("babel" "babel-core"
                                             "babel-eslint"
                                             "eslint"
                                             "eslint-config-airbnb"
                                             "eslint-plugin-react"
                                             "rimraf")))))))
    (home-page "https://github.com/gaearon/base16-js")
    (synopsis "Base16 themes as JS objects")
    (description "Base16 themes as JS objects")
    (license license:expat)))

(define-public node-base64-img-1.0.4
  (package
    (name "node-base64-img")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/base64-img/-/base64-img-1.0.4.tgz")
              (sha256
               (base32
                "0f8x6x2dpq80kn97yfw4nk26b8mskaysv13hi93h03qlhwyx69jq"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("grunt" "grunt-contrib-jshint")))))))
    (inputs (list node-file-system-2.2.2 node-ajax-request-1.2.3))
    (home-page "https://github.com/douzi8/base64-img")
    (synopsis "Convert img or svg to base64, or convert base64 to img")
    (description "Convert img or svg to base64, or convert base64 to img")
    (license license:isc)))

(define-public node-base64-js-1.5.1
  (package
    (name "node-base64-js")
    (version "1.5.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/base64-js/-/base64-js-1.5.1.tgz")
              (sha256
               (base32
                "118a46skxnrgx5bdd68ny9xxjcvyb7b1clj2hf82d196nm2skdxi"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("babel-minify" "benchmark"
                                             "browserify" "standard" "tape")))))))
    (home-page "https://github.com/beatgammit/base64-js")
    (synopsis "Base64 encoding/decoding in pure JS")
    (description "Base64 encoding/decoding in pure JS")
    (license license:expat)))

(define-public node-bcrypt-pbkdf-1.0.2
  (package
    (name "node-bcrypt-pbkdf")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/bcrypt-pbkdf/-/bcrypt-pbkdf-1.0.2.tgz")
              (sha256
               (base32
                "09kqy1rjj0b1aavdssglrjj8ayf9vxvnnvlh5ah270j3bngrwgp1"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (inputs (list node-tweetnacl-0.14.5))
    (home-page "https://github.com/joyent/node-bcrypt-pbkdf#readme")
    (synopsis "Port of the OpenBSD bcrypt_pbkdf function to pure JS")
    (description "Port of the OpenBSD bcrypt_pbkdf function to pure JS")
    (license license:bsd-3)))

(define-public node-biblatex-csl-converter-2.1.0
  (package
    (name "node-biblatex-csl-converter")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/biblatex-csl-converter/-/biblatex-csl-converter-2.1.0.tgz")
              (sha256
               (base32
                "0z4vdxmwsbimg7zky9i47gymn27kig4c0ci05683mdsgr94z21n6"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@babel/core"
                                             "@babel/plugin-transform-runtime"
                                             "@babel/preset-env"
                                             "@babel/runtime"
                                             "@rollup/plugin-babel"
                                             "@rollup/plugin-commonjs"
                                             "@rollup/plugin-json"
                                             "@rollup/plugin-node-resolve"
                                             "@rollup/plugin-typescript"
                                             "@typescript-eslint/eslint-plugin"
                                             "@typescript-eslint/parser"
                                             "babel-plugin-istanbul"
                                             "benchmark"
                                             "chai"
                                             "coveralls"
                                             "eslint"
                                             "eslint-config-prettier"
                                             "eslint-plugin-prettier"
                                             "mocha"
                                             "nyc"
                                             "prettier"
                                             "prettier-cli"
                                             "rollup"
                                             "rollup-plugin-dts"
                                             "rollup-plugin-istanbul"
                                             "rollup-plugin-node-globals"
                                             "rollup-plugin-polyfill-node"
                                             "rollup-plugin-terser"
                                             "tslib"
                                             "typescript"
                                             "updates")))))))
    (inputs (list node-xregexp-5.1.1))
    (home-page "https://github.com/fiduswriter/biblatex-csl-converter#readme")
    (synopsis
     "a set of converters: biblatex => json, json => biblatex, json => CSL")
    (description
     "a set of converters: biblatex => json, json => biblatex, json => CSL")
    (license license:lgpl3)))

(define-public node-binary-extensions-1.13.1
  (package
    (name "node-binary-extensions")
    (version "1.13.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/binary-extensions/-/binary-extensions-1.13.1.tgz")
              (sha256
               (base32
                "172h5sjqa46zwblrzbjqdr0v3jjcasfvsgm6zqq5jgd6xxjq2kkh"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava")))))))
    (home-page "https://github.com/sindresorhus/binary-extensions#readme")
    (synopsis "List of binary file extensions")
    (description "List of binary file extensions")
    (license license:expat)))

(define-public node-bindings-1.5.0
  node-bindings)

(define-public node-body-parser-1.20.1
  (package
    (name "node-body-parser")
    (version "1.20.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/body-parser/-/body-parser-1.20.1.tgz")
              (sha256
               (base32
                "0mhv12a925px3sy8nalsxfvnxc8ms007wj83im5r47lpg40qcldk"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("eslint" "eslint-config-standard"
                                             "eslint-plugin-import"
                                             "eslint-plugin-markdown"
                                             "eslint-plugin-node"
                                             "eslint-plugin-promise"
                                             "eslint-plugin-standard"
                                             "methods"
                                             "mocha"
                                             "nyc"
                                             "safe-buffer"
                                             "supertest")))))))
    (inputs (list node-unpipe-1.0.0
                  node-type-is-1.6.18
                  node-raw-body-2.5.1
                  node-qs-6.11.0
                  node-on-finished-2.4.1
                  node-iconv-lite-0.4.24
                  node-http-errors-2.0.0
                  node-destroy-1.2.0
                  node-depd-2.0.0
                  node-debug-2.6.9
                  node-content-type-1.0.5
                  node-bytes-3.1.2))
    (home-page "https://github.com/expressjs/body-parser#readme")
    (synopsis "Node.js body parsing middleware")
    (description "Node.js body parsing middleware")
    (license license:expat)))

(define-public node-boundary-1.0.1
  (package
    (name "node-boundary")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/boundary/-/boundary-1.0.1.tgz")
              (sha256
               (base32
                "0wv8vbfv73yywd7s4slzhbfc7bvxm5kqbw96yf3df5sv5l9yfmmc"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp" "gulp-6to5"
                                             "gulp-bump"
                                             "gulp-espower"
                                             "gulp-filter"
                                             "gulp-git"
                                             "gulp-mocha"
                                             "gulp-sourcemaps"
                                             "gulp-tag-version"
                                             "power-assert")))))))
    (home-page "https://github.com/Constellation/boundary")
    (synopsis "Provides boundary functions, (upper-bound and lower-bound).")
    (description "Provides boundary functions, (upper-bound and lower-bound).")
    (license #f)))

(define-public node-bowser-2.11.0
  (package
    (name "node-bowser")
    (version "2.11.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/bowser/-/bowser-2.11.0.tgz")
              (sha256
               (base32
                "1mrsgjann2d0lnwivfqprh9x1dg48k1vq9mff1cwz8zgc1mbkrm7"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@babel/cli" "@babel/core"
                                             "@babel/polyfill"
                                             "@babel/preset-env"
                                             "@babel/register"
                                             "ava"
                                             "babel-eslint"
                                             "babel-loader"
                                             "babel-plugin-add-module-exports"
                                             "babel-plugin-istanbul"
                                             "compression-webpack-plugin"
                                             "coveralls"
                                             "docdash"
                                             "eslint"
                                             "eslint-config-airbnb-base"
                                             "eslint-plugin-ava"
                                             "eslint-plugin-import"
                                             "gh-pages"
                                             "jsdoc"
                                             "nyc"
                                             "sinon"
                                             "testem"
                                             "webpack"
                                             "webpack-bundle-analyzer"
                                             "webpack-cli"
                                             "yamljs")))))))
    (home-page "https://github.com/lancedikson/bowser")
    (synopsis "Lightweight browser detector")
    (description "Lightweight browser detector")
    (license license:expat)))

(define-public node-brace-expansion-1.1.11
  (package
    (name "node-brace-expansion")
    (version "1.1.11")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/brace-expansion/-/brace-expansion-1.1.11.tgz")
              (sha256
               (base32
                "1nlmjvlwlp88knblnayns0brr7a9m2fynrlwq425lrpb4mcn9gc4"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("matcha" "tape")))))))
    (inputs (list node-concat-map-0.0.1 node-balanced-match-1.0.2))
    (home-page "https://github.com/juliangruber/brace-expansion")
    (synopsis "Brace expansion as known from sh/bash")
    (description "Brace expansion as known from sh/bash")
    (license license:expat)))

(define-public node-braces-1.8.5
  (package
    (name "node-braces")
    (version "1.8.5")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/braces/-/braces-1.8.5.tgz")
              (sha256
               (base32
                "1593ijg9gjm1sgyk1cpqc71ac57md8izkhg4n5p18vnbri0xqzww"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("benchmarked" "brace-expansion"
                                             "chalk"
                                             "gulp-format-md"
                                             "minimatch"
                                             "minimist"
                                             "mocha"
                                             "should")))))))
    (inputs (list node-repeat-element-1.1.4 node-preserve-0.2.0
                  node-expand-range-1.8.2))
    (home-page "https://github.com/jonschlinkert/braces")
    (synopsis
     "Fastest brace expansion for node.js, with the most complete support for the Bash 4.3 braces specification.")
    (description
     "Fastest brace expansion for node.js, with the most complete support for the Bash 4.3 braces specification.")
    (license license:expat)))

(define-public node-braces-2.3.2
  (package
    (name "node-braces")
    (version "2.3.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/braces/-/braces-2.3.2.tgz")
              (sha256
               (base32
                "10608dfl1pxajw0nwrsh69769q659yjyw88b0mrlprnn0z1wya1l"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ansi-cyan" "benchmarked"
                                             "brace-expansion"
                                             "cross-spawn"
                                             "gulp"
                                             "gulp-eslint"
                                             "gulp-format-md"
                                             "gulp-istanbul"
                                             "gulp-mocha"
                                             "gulp-unused"
                                             "is-windows"
                                             "minimatch"
                                             "mocha"
                                             "noncharacters"
                                             "text-table"
                                             "time-diff"
                                             "yargs-parser")))))))
    (inputs (list node-to-regex-3.0.2
                  node-split-string-3.1.0
                  node-snapdragon-node-2.1.1
                  node-snapdragon-0.8.2
                  node-repeat-element-1.1.4
                  node-isobject-3.0.1
                  node-fill-range-4.0.0
                  node-extend-shallow-2.0.1
                  node-array-unique-0.3.2
                  node-arr-flatten-1.1.0))
    (home-page "https://github.com/micromatch/braces")
    (synopsis
     "Bash-like brace expansion, implemented in JavaScript. Safer than other brace expansion libs, with complete support for the Bash 4.3 braces specification, without sacrificing speed.")
    (description
     "Bash-like brace expansion, implemented in JavaScript. Safer than other brace expansion libs, with complete support for the Bash 4.3 braces specification, without sacrificing speed.")
    (license license:expat)))

(define-public node-buffer-from-1.1.2
  (package
    (name "node-buffer-from")
    (version "1.1.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/buffer-from/-/buffer-from-1.1.2.tgz")
              (sha256
               (base32
                "0hz3cbll0m805g22c5pnwdgpi1xavmrp5q1734x4d3yakvah6aww"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("standard")))))))
    (home-page "https://github.com/LinusU/buffer-from#readme")
    (synopsis
     "A [ponyfill](https://ponyfill.com) for `Buffer.from`, uses native implementation if available.")
    (description
     "A [ponyfill](https://ponyfill.com) for `Buffer.from`, uses native implementation if available.")
    (license license:expat)))

(define-public node-bufferutil-4.0.7
  (package
    (name "node-bufferutil")
    (version "4.0.7")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/bufferutil/-/bufferutil-4.0.7.tgz")
              (sha256
               (base32
                "1yj39zhjk1hf1dbbaznxpcainhv4n3r687d86f7fc2mfww6hf65n"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("mocha" "node-gyp" "prebuildify")))))))
    (inputs (list node-node-gyp-build-4.6.0))
    (home-page "https://github.com/websockets/bufferutil")
    (synopsis "WebSocket buffer utils")
    (description "WebSocket buffer utils")
    (license license:expat)))

(define-public node-bytes-3.1.2
  (package
    (name "node-bytes")
    (version "3.1.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/bytes/-/bytes-3.1.2.tgz")
              (sha256
               (base32
                "10f5wgg4izi14lc425v7ljr1ayk28ycdjckfxpm4bnj0bankfpl3"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("eslint" "eslint-plugin-markdown"
                                             "mocha" "nyc")))))))
    (home-page "https://github.com/visionmedia/bytes.js#readme")
    (synopsis "Utility to parse a string bytes to bytes and vice-versa")
    (description "Utility to parse a string bytes to bytes and vice-versa")
    (license license:expat)))

(define-public node-cache-base-1.0.1
  (package
    (name "node-cache-base")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/cache-base/-/cache-base-1.0.1.tgz")
              (sha256
               (base32
                "1iny3winp8x5ac39hx52baq60g2hsp4pcx95a634c83klawdaw78"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp-format-md" "mocha")))))))
    (inputs (list node-unset-value-1.0.0
                  node-union-value-1.0.1
                  node-to-object-path-0.3.0
                  node-set-value-2.0.1
                  node-isobject-3.0.1
                  node-has-value-1.0.0
                  node-get-value-2.0.6
                  node-component-emitter-1.3.0
                  node-collection-visit-1.0.0))
    (home-page "https://github.com/jonschlinkert/cache-base")
    (synopsis
     "Basic object cache with `get`, `set`, `del`, and `has` methods for node.js/javascript projects.")
    (description
     "Basic object cache with `get`, `set`, `del`, and `has` methods for node.js/javascript projects.")
    (license license:expat)))

(define-public node-call-bind-1.0.2
  (package
    (name "node-call-bind")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/call-bind/-/call-bind-1.0.2.tgz")
              (sha256
               (base32
                "06il2ki0bprw4s0a12bwnzwjc8gwwcw6f0cda0jyvj46sj56z5f3"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@ljharb/eslint-config" "aud"
                                             "auto-changelog"
                                             "eslint"
                                             "nyc"
                                             "safe-publish-latest"
                                             "tape")))))))
    (inputs (list node-get-intrinsic-1.2.1 node-function-bind-1.1.1))
    (home-page "https://github.com/ljharb/call-bind#readme")
    (synopsis "Robustly `.call.bind()` a function")
    (description "Robustly `.call.bind()` a function")
    (license license:expat)))

(define-public node-callsites-3.1.0
  (package
    (name "node-callsites")
    (version "3.1.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/callsites/-/callsites-3.1.0.tgz")
              (sha256
               (base32
                "17f8wf2bxv2s4k36ld1x4y2rbkh9a8vsmbhwab470vmz6ayl40hp"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "tsd" "xo")))))))
    (home-page "https://github.com/sindresorhus/callsites#readme")
    (synopsis "Get callsites from the V8 stack trace API")
    (description "Get callsites from the V8 stack trace API")
    (license license:expat)))

(define-public node-caseless-0.12.0
  (package
    (name "node-caseless")
    (version "0.12.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/caseless/-/caseless-0.12.0.tgz")
              (sha256
               (base32
                "165fzm8s6qxapxk8xlb548q58xjav55k5nnychr234282irb2zjd"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("tape")))))))
    (home-page "https://github.com/mikeal/caseless#readme")
    (synopsis
     "Caseless object set/get/has, very useful when working with HTTP headers.")
    (description
     "Caseless object set/get/has, very useful when working with HTTP headers.")
    (license license:asl2.0)))

(define-public node-chain-able-1.0.1
  (package
    (name "node-chain-able")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/chain-able/-/chain-able-1.0.1.tgz")
              (sha256
               (base32
                "085qmxbrkf71bn4xslrj099yz8wb9k9ciq6vb1kk86d3zmnh67py"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:modules ((guix build node-build-system)
                  (srfi srfi-1)
                  (ice-9 match)
                  (guix build utils))
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  ;; XXX: Copied from Guix.
                  (add-after 'unpack 'add-deps
                    (lambda _
                      (with-atomic-json-file-replacement "package.json"
                                                         (match-lambda
                                                           (((quote @) . pkg-meta-alist)
                                                            (cons '@
                                                                  (cons '("dependencies"
                                                                          @
                                                                          ("inspector-gadget" . "^1.0.0"))
                                                                   pkg-meta-alist)))))))
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "babel-cli"
                                             "babel-core"
                                             "babel-jest"
                                             "babel-plugin-transform-flow-strip-types"
                                             "babel-preset-env"
                                             "flow-remove-types"
                                             "fuse-box"
                                             ;; This is a runtime dependency.
                                             ;"inspector-gadget"
                                             "rollup"
                                             "rollup-plugin-commonjs"
                                             "rollup-plugin-node-resolve")))))))
    (inputs (list node-inspector-gadget-1.0.0))
    (home-page "https://github.com/fluents/chain-able#readme")
    (synopsis "next level chaining.")
    (description "next level chaining.")
    (license license:expat)))

(define-public node-chain-able-3.0.0
  (package
    (name "node-chain-able")
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/chain-able/-/chain-able-3.0.0.tgz")
              (sha256
               (base32
                "0pk2szx36nv5lx3ndfiai36jxqwkd28cyyhycy964lqrrlmf8930"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "babel-cli"
                                             "babel-core"
                                             "babel-jest"
                                             "babel-plugin-istanbul"
                                             "babel-plugin-transform-flow-strip-types"
                                             "babel-preset-env"
                                             "babel-preset-es2015"
                                             "buble"
                                             "chainsaw"
                                             "coveralls"
                                             "deepmerge"
                                             "eslint-config-chain-able"
                                             "fliplog"
                                             "flow-remove-types"
                                             "immutable"
                                             "inspector-gadget"
                                             "mobx"
                                             "module-alias"
                                             "nyc"
                                             "optimize-js"
                                             "rollup"
                                             "rollup-plugin-babili"
                                             "rollup-plugin-buble"
                                             "rollup-plugin-cleanup"
                                             "rollup-plugin-commonjs"
                                             "rollup-plugin-filesize"
                                             "rollup-plugin-node-resolve"
                                             "rollup-plugin-replace"
                                             "rollup-plugin-uglify"
                                             "traverse"
                                             "uglify-es"
                                             "uglify-js"
                                             "validator")))))))
    (home-page "https://github.com/fluents/chain-able#readme")
    (synopsis "interfaces that describe their intentions.")
    (description "interfaces that describe their intentions.")
    (license license:expat)))

(define-public node-chalk-2.4.2
  (package
    (name "node-chalk")
    (version "2.4.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/chalk/-/chalk-2.4.2.tgz")
              (sha256
               (base32
                "0wf6hln5gcjb2n8p18gag6idghl6dfq4if6pxa6s1jqnwr94x26h"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "coveralls"
                                             "execa"
                                             "flow-bin"
                                             "import-fresh"
                                             "matcha"
                                             "nyc"
                                             "resolve-from"
                                             "typescript"
                                             "xo")))))))
    (inputs (list node-supports-color-5.5.0 node-escape-string-regexp-1.0.5
                  node-ansi-styles-3.2.1))
    (home-page "https://github.com/chalk/chalk#readme")
    (synopsis "Terminal string styling done right")
    (description "Terminal string styling done right")
    (license license:expat)))

(define-public node-chardet-0.4.2
  (package
    (name "node-chardet")
    (version "0.4.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/chardet/-/chardet-0.4.2.tgz")
              (sha256
               (base32
                "1p2mq4rv3vkwd9k8ipadambn1jw7jzf7fxcrvg16fm3d6slsk7sf"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("github-publish-release" "mocha")))))))
    (home-page "https://github.com/runk/node-chardet")
    (synopsis "Character detector")
    (description "Character detector")
    (license license:expat)))

(define-public node-chokidar-1.7.0
  (package
    (name "node-chokidar")
    (version "1.7.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/chokidar/-/chokidar-1.7.0.tgz")
              (sha256
               (base32
                "0dxk0wm89l9smx45bgh828x0cf9mis4mahk6wgd98pfryx01y9cp"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("chai" "coveralls"
                                             "graceful-fs"
                                             "istanbul"
                                             "mocha"
                                             "rimraf"
                                             "sinon"
                                             "sinon-chai")))))))
    (inputs (list node-readdirp-2.2.1
                  node-path-is-absolute-1.0.1
                  node-is-glob-2.0.1
                  node-is-binary-path-1.0.1
                  node-inherits-2.0.4
                  node-glob-parent-2.0.0
                  node-async-each-1.0.6
                  node-anymatch-1.3.2))
    (home-page "https://github.com/paulmillr/chokidar")
    (synopsis
     "A neat wrapper around node.js fs.watch / fs.watchFile / fsevents.")
    (description
     "A neat wrapper around node.js fs.watch / fs.watchFile / fsevents.")
    (license license:expat)))

(define-public node-chownr-2.0.0
  (package
    (name "node-chownr")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/chownr/-/chownr-2.0.0.tgz")
              (sha256
               (base32
                "177wsdfmn1d2f12wy8m875b5y9a74ibfdh33jarlv3a0zrbmvqlv"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("mkdirp" "rimraf" "tap")))))))
    (home-page "https://github.com/isaacs/chownr#readme")
    (synopsis "like `chown -R`")
    (description "like `chown -R`")
    (license license:isc)))

(define-public node-class-utils-0.3.6
  (package
    (name "node-class-utils")
    (version "0.3.6")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/class-utils/-/class-utils-0.3.6.tgz")
              (sha256
               (base32
                "0567alh9j9bl7rj91frmjdpb4q5zig9rydzkdm9pmij85h7sffjh"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp" "gulp-eslint"
                                             "gulp-format-md"
                                             "gulp-istanbul"
                                             "gulp-mocha"
                                             "mocha"
                                             "should"
                                             "through2")))))))
    (inputs (list node-static-extend-0.1.2 node-isobject-3.0.1
                  node-define-property-0.2.5 node-arr-union-3.1.0))
    (home-page "https://github.com/jonschlinkert/class-utils")
    (synopsis
     "Utils for working with JavaScript classes and prototype methods.")
    (description
     "Utils for working with JavaScript classes and prototype methods.")
    (license license:expat)))

(define-public node-clean-css-4.2.4
  (package
    (name "node-clean-css")
    (version "4.2.4")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/clean-css/-/clean-css-4.2.4.tgz")
              (sha256
               (base32
                "1k5dixs7dh2ms3kzf0b6sd1wbk435jf4cqdxkgh8wgky6xmg06pv"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("browserify" "http-proxy"
                                             "jshint"
                                             "nock"
                                             "server-destroy"
                                             "uglify-js"
                                             "vows")))))))
    (inputs (list node-source-map-0.6.1))
    (home-page "https://github.com/jakubpawlowicz/clean-css")
    (synopsis "A well-tested CSS minifier")
    (description "A well-tested CSS minifier")
    (license license:expat)))

(define-public node-cli-cursor-2.1.0
  (package
    (name "node-cli-cursor")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/cli-cursor/-/cli-cursor-2.1.0.tgz")
              (sha256
               (base32
                "034sp34k37dacwr16x78c86d9xvrb2z14j88mfwv054bzgkgn9pz"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "xo")))))))
    (inputs (list node-restore-cursor-2.0.0))
    (home-page "https://github.com/sindresorhus/cli-cursor#readme")
    (synopsis "Toggle the CLI cursor")
    (description "Toggle the CLI cursor")
    (license license:expat)))

(define-public node-cli-width-2.2.1
  (package
    (name "node-cli-width")
    (version "2.2.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/cli-width/-/cli-width-2.2.1.tgz")
              (sha256
               (base32
                "0vkir7lsf8psvqrb3s7zzkq7c5ng1mj2zksxcy46km03gr8ldrgg"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("coveralls" "nyc"
                                             "standard-version" "tap-spec"
                                             "tape")))))))
    (home-page "https://github.com/knownasilya/cli-width")
    (synopsis
     "Get stdout window width, with two fallbacks, tty and then a default.")
    (description
     "Get stdout window width, with two fallbacks, tty and then a default.")
    (license license:isc)))

(define-public node-clipboard-2.0.11
  (package
    (name "node-clipboard")
    (version "2.0.11")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/clipboard/-/clipboard-2.0.11.tgz")
              (sha256
               (base32
                "0gv53gyv1f6sj3j18fqzdv79jx80723p4wjzxl0n89n7k8g1npvk"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@babel/core" "@babel/preset-env"
                                             "babel-loader"
                                             "chai"
                                             "cross-env"
                                             "eslint"
                                             "eslint-config-airbnb-base"
                                             "eslint-config-prettier"
                                             "eslint-plugin-import"
                                             "eslint-plugin-prettier"
                                             "husky"
                                             "karma"
                                             "karma-chai"
                                             "karma-chrome-launcher"
                                             "karma-mocha"
                                             "karma-sinon"
                                             "karma-webpack"
                                             "lint-staged"
                                             "mocha"
                                             "prettier"
                                             "sinon"
                                             "tsd"
                                             "uglifyjs-webpack-plugin"
                                             "webpack"
                                             "webpack-cli")))))))
    (inputs (list node-tiny-emitter-2.1.0 node-select-1.1.2
                  node-good-listener-1.2.2))
    (home-page "https://clipboardjs.com")
    (synopsis "Modern copy to clipboard. No Flash. Just 2kb")
    (description "Modern copy to clipboard. No Flash. Just 2kb")
    (license license:expat)))

(define-public node-cliui-8.0.1
  (package
    (name "node-cliui")
    (version "8.0.1")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/cliui/-/cliui-8.0.1.tgz")
              (sha256
               (base32
                "13cc8rvmzcvvlvf2prwxj1zjyxhybsznl1nirnlrwd4abvid3yny"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:modules ((guix build node-build-system)
                  (srfi srfi-1)
                  (ice-9 match)
                  (guix build utils))
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-before 'configure 'avoid-prepare-scripts
                    (lambda _
                      ;; We need to remove the prepare script from "package.json", as
                      ;; it would try to use the build environment and would block the
                      ;; automatic building by other packages making use of node-acorn.
                      ;; TODO: Add utility function
                      (with-atomic-json-file-replacement "package.json"
                                                         (match-lambda
                                                           (((quote @) . pkg-meta-alist)
                                                            (cons '@
                                                                  (map (match-lambda
                                                                         (("scripts" @ . scripts-alist) `
                                                                          ("scripts"
                                                                           @
                                                                           ,@(filter (match-lambda
                                                                                       
                                                                                       (("prepare" . _)
                                                                                        #f)
                                                                                       
                                                                                       (_
                                                                                        #t))
                                                                              scripts-alist)))
                                                                         (other
                                                                          other))
                                                                   pkg-meta-alist)))))))
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@types/node"
                                             "@typescript-eslint/eslint-plugin"
                                             "@typescript-eslint/parser"
                                             "c8"
                                             "chai"
                                             "chalk"
                                             "cross-env"
                                             "eslint"
                                             "eslint-plugin-import"
                                             "eslint-plugin-node"
                                             "gts"
                                             "mocha"
                                             "rimraf"
                                             "rollup"
                                             "rollup-plugin-ts"
                                             "standardx"
                                             "typescript")))))))
    (inputs (list node-wrap-ansi-7.0.0 node-strip-ansi-6.0.1
                  node-string-width-4.2.3))
    (home-page "https://github.com/yargs/cliui#readme")
    (synopsis "easily create complex multi-column command-line-interfaces")
    (description "easily create complex multi-column command-line-interfaces")
    (license license:isc)))

(define-public node-collection-visit-1.0.0
  (package
    (name "node-collection-visit")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/collection-visit/-/collection-visit-1.0.0.tgz")
              (sha256
               (base32
                "062q78a3fjfvk4vplkay9hd4rx6f1xw4r0438sa0hbv62jf5bc46"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("clone-deep" "gulp"
                                             "gulp-eslint"
                                             "gulp-format-md"
                                             "gulp-istanbul"
                                             "gulp-mocha"
                                             "mocha")))))))
    (inputs (list node-object-visit-1.0.1 node-map-visit-1.0.0))
    (home-page "https://github.com/jonschlinkert/collection-visit")
    (synopsis
     "Visit a method over the items in an object, or map visit over the objects in an array.")
    (description
     "Visit a method over the items in an object, or map visit over the objects in an array.")
    (license license:expat)))

(define-public node-color-convert-1.9.3
  (package
    (name "node-color-convert")
    (version "1.9.3")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/color-convert/-/color-convert-1.9.3.tgz")
              (sha256
               (base32
                "1ahbdssv1qgwlzvhv7731hpfgz8wny0619x97b7n5x9lckj17i0j"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("chalk" "xo")))))))
    (inputs `(("node-color-name" ,node-color-name-1.1.3)))
    (home-page "https://github.com/Qix-/color-convert#readme")
    (synopsis "Plain color conversion functions")
    (description "Plain color conversion functions")
    (license license:expat)))

(define-public node-color-convert-2.0.1
  (package
    (name "node-color-convert")
    (version "2.0.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/color-convert/-/color-convert-2.0.1.tgz")
              (sha256
               (base32
                "1qbw9rwfzcp7y0cpa8gmwlj7ccycf9pwn15zvf2s06f070ss83wj"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("chalk" "xo")))))))
    (inputs (list node-color-name-1.1.4))
    (home-page "https://github.com/Qix-/color-convert#readme")
    (synopsis "Plain color conversion functions")
    (description "Plain color conversion functions")
    (license license:expat)))

(define-public node-color-name-1.1.3
  node-color-name)

(define-public node-color-name-1.1.4
  (package
    (name "node-color-name")
    (version "1.1.4")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/color-name/-/color-name-1.1.4.tgz")
              (sha256
               (base32
                "020p7x7k8rlph38lhsqpqvkx0b70lzlmk6mgal9r9sz8c527qysh"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://github.com/colorjs/color-name")
    (synopsis "A list of color names and its values")
    (description "A list of color names and its values")
    (license license:expat)))

(define-public node-color-support-1.1.3
  (package
    (name "node-color-support")
    (version "1.1.3")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/color-support/-/color-support-1.1.3.tgz")
              (sha256
               (base32
                "0mdpbqki4iz5fmm4rpviwvw8ra0wjp0ccqf3cyvwh8nxwanlxhv1"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("tap")))))))
    (home-page "https://github.com/isaacs/color-support#readme")
    (synopsis
     "A module which will endeavor to guess your terminal's level of color support.")
    (description
     "A module which will endeavor to guess your terminal's level of color support.")
    (license license:isc)))

(define-public node-combined-stream-1.0.8
  (package
    (name "node-combined-stream")
    (version "1.0.8")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/combined-stream/-/combined-stream-1.0.8.tgz")
              (sha256
               (base32
                "04hm5rrkwda2qgy1afwhrz42asmflw5hxkbpxddn741ywnmmmgmn"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("far")))))))
    (inputs (list node-delayed-stream-1.0.0))
    (home-page "https://github.com/felixge/node-combined-stream")
    (synopsis "A stream that emits multiple other streams one after another.")
    (description
     "A stream that emits multiple other streams one after another.")
    (license license:expat)))

(define-public node-commander-1.1.1
  (package
    (name "node-commander")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/commander/-/commander-1.1.1.tgz")
              (sha256
               (base32
                "17rss32w98jn6h9sy7880h784pa9a9dxiklpl2wwfr099sbl5yvl"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("should")))))))
    (inputs (list node-keypress-0.1.0))
    (home-page "https://www.npmjs.com/package/node-commander")
    (synopsis "the complete solution for node.js command-line programs")
    (description "the complete solution for node.js command-line programs")
    (license #f)))

(define-public node-commander-2.20.3
  (package
    (name "node-commander")
    (version "2.20.3")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/commander/-/commander-2.20.3.tgz")
              (sha256
               (base32
                "0m9bcmcwgn2zj6hdqydnyx6d1c5y014ysazyqd6qva8pw0yr00iw"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@types/node" "eslint"
                                             "should"
                                             "sinon"
                                             "standard"
                                             "ts-node"
                                             "typescript")))))))
    (home-page "https://github.com/tj/commander.js#readme")
    (synopsis "the complete solution for node.js command-line programs")
    (description "the complete solution for node.js command-line programs")
    (license license:expat)))

(define-public node-component-emitter-1.3.0
  (package
    (name "node-component-emitter")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/component-emitter/-/component-emitter-1.3.0.tgz")
              (sha256
               (base32
                "0qc1qx0ngvah8lmkn0d784vlxmya2kr5gpjcpfikxlpwx5v3wr0h"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("mocha" "should")))))))
    (home-page "https://github.com/component/emitter#readme")
    (synopsis "Event emitter")
    (description "Event emitter")
    (license license:expat)))

(define-public node-concat-map-0.0.1
  (package
    (name "node-concat-map")
    (version "0.0.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/concat-map/-/concat-map-0.0.1.tgz")
              (sha256
               (base32
                "0qa2zqn9rrr2fqdki44s4s2dk2d8307i4556kv25h06g43b2v41m"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("tape")))))))
    (home-page "https://github.com/substack/node-concat-map")
    (synopsis "concatenative mapdashery")
    (description "concatenative mapdashery")
    (license license:expat)))

(define-public node-concat-stream-1.6.2
  (package
    (name "node-concat-stream")
    (version "1.6.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/concat-stream/-/concat-stream-1.6.2.tgz")
              (sha256
               (base32
                "1wa3gka91z4mdwi9yz2lri8lb2b1vhimkr6zckcjdj4bjxcw2iya"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("tape")))))))
    (inputs (list node-typedarray-0.0.6 node-readable-stream-2.3.8
                  node-inherits-2.0.4 node-buffer-from-1.1.2))
    (home-page "https://github.com/maxogden/concat-stream#readme")
    (synopsis
     "writable stream that concatenates strings or binary data and calls a callback with the result")
    (description
     "writable stream that concatenates strings or binary data and calls a callback with the result")
    (license license:expat)))

(define-public node-concat-stream-2.0.0
  (package
    (name "node-concat-stream")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/concat-stream/-/concat-stream-2.0.0.tgz")
              (sha256
               (base32
                "030m3v08q6mfh4vsdv5jc4w5c4q09il5lgw6mhaij8p0gix9jm6c"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("tape")))))))
    (inputs (list node-typedarray-0.0.6 node-readable-stream-3.6.2
                  node-inherits-2.0.4 node-buffer-from-1.1.2))
    (home-page "https://github.com/maxogden/concat-stream#readme")
    (synopsis
     "writable stream that concatenates strings or binary data and calls a callback with the result")
    (description
     "writable stream that concatenates strings or binary data and calls a callback with the result")
    (license license:expat)))

(define-public node-console-control-strings-1.1.0
  (package
    (name "node-console-control-strings")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/console-control-strings/-/console-control-strings-1.1.0.tgz")
              (sha256
               (base32
                "0dl328x4g2dqhf4lbfajf4c7vlb6m7ff8adpk1ngig7s1i49pm7h"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("standard" "tap")))))))
    (home-page "https://github.com/iarna/console-control-strings#readme")
    (synopsis
     "A library of cross-platform tested terminal/console command strings for doing things like color and cursor positioning.  This is a subset of both ansi and vt100.  All control codes included work on both Windows & Unix-like OSes, except where noted.")
    (description
     "A library of cross-platform tested terminal/console command strings for doing things like color and cursor positioning.  This is a subset of both ansi and vt100.  All control codes included work on both Windows & Unix-like OSes, except where noted.")
    (license license:isc)))

(define-public node-content-disposition-0.5.4
  (package
    (name "node-content-disposition")
    (version "0.5.4")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/content-disposition/-/content-disposition-0.5.4.tgz")
              (sha256
               (base32
                "0ljl6r5vqhyscjb723f8ddlzrzg66zlh9q01xahjrhlanskwi574"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("deep-equal" "eslint"
                                             "eslint-config-standard"
                                             "eslint-plugin-import"
                                             "eslint-plugin-markdown"
                                             "eslint-plugin-node"
                                             "eslint-plugin-promise"
                                             "eslint-plugin-standard"
                                             "istanbul"
                                             "mocha")))))))
    (inputs (list node-safe-buffer-5.2.1))
    (home-page "https://github.com/jshttp/content-disposition#readme")
    (synopsis "Create and parse Content-Disposition header")
    (description "Create and parse Content-Disposition header")
    (license license:expat)))

(define-public node-content-type-1.0.5
  (package
    (name "node-content-type")
    (version "1.0.5")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/content-type/-/content-type-1.0.5.tgz")
              (sha256
               (base32
                "1j0jpnlxjrdpbnq7s1h1xga2n8562j5g6612f7fl40jz82cd0cdc"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("deep-equal" "eslint"
                                             "eslint-config-standard"
                                             "eslint-plugin-import"
                                             "eslint-plugin-node"
                                             "eslint-plugin-promise"
                                             "eslint-plugin-standard"
                                             "mocha"
                                             "nyc")))))))
    (home-page "https://github.com/jshttp/content-type#readme")
    (synopsis "Create and parse HTTP Content-Type header")
    (description "Create and parse HTTP Content-Type header")
    (license license:expat)))

(define-public node-convert-source-map-1.9.0
  (package
    (name "node-convert-source-map")
    (version "1.9.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/convert-source-map/-/convert-source-map-1.9.0.tgz")
              (sha256
               (base32
                "0l5hs8bf8dy2gkw5i4gfr7w1rhhla2hclmms819j7i4i5x9bjwm1"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("inline-source-map" "tap")))))))
    (home-page "https://github.com/thlorenz/convert-source-map")
    (synopsis
     "Converts a source-map from/to  different formats and allows adding/changing properties.")
    (description
     "Converts a source-map from/to  different formats and allows adding/changing properties.")
    (license license:expat)))

(define-public node-cookie-0.5.0
  (package
    (name "node-cookie")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/cookie/-/cookie-0.5.0.tgz")
              (sha256
               (base32
                "0mnpmzpda4l2xjr4zgkkmjx0cjzqywvibr0m8y1bhmvyc3f5m2ap"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("beautify-benchmark" "benchmark"
                                             "eslint"
                                             "eslint-plugin-markdown"
                                             "mocha"
                                             "nyc"
                                             "safe-buffer"
                                             "top-sites")))))))
    (home-page "https://github.com/jshttp/cookie#readme")
    (synopsis "HTTP server cookie parsing and serialization")
    (description "HTTP server cookie parsing and serialization")
    (license license:expat)))

(define-public node-cookie-signature-1.0.6
  (package
    (name "node-cookie-signature")
    (version "1.0.6")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/cookie-signature/-/cookie-signature-1.0.6.tgz")
              (sha256
               (base32
                "04sk9ma5a8xb4jib4wmsdj8pz5bk36yzavzbj3k0drdy9bi4bww9"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("mocha" "should")))))))
    (home-page "https://github.com/visionmedia/node-cookie-signature")
    (synopsis "Sign and unsign cookies")
    (description "Sign and unsign cookies")
    (license license:expat)))

(define-public node-copy-descriptor-0.1.1
  (package
    (name "node-copy-descriptor")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/copy-descriptor/-/copy-descriptor-0.1.1.tgz")
              (sha256
               (base32
                "1dmlg6g04hfn1kmjklw6l33va255gsml1mrlz07jfv2a4alz4470"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp-format-md" "mocha")))))))
    (home-page "https://github.com/jonschlinkert/copy-descriptor")
    (synopsis "Copy a descriptor from object A to object B")
    (description "Copy a descriptor from object A to object B")
    (license license:expat)))

(define-public node-core-js-2.6.12
  (package
    (name "node-core-js")
    (version "2.6.12")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/core-js/-/core-js-2.6.12.tgz")
              (sha256
               (base32
                "0idawjihpabdgpq1w460phfls3wrmgkl3idll6h68cy48k2z6bw7"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("LiveScript"
                                             "es-observable-tests"
                                             "eslint"
                                             "eslint-plugin-import"
                                             "grunt"
                                             "grunt-cli"
                                             "grunt-contrib-clean"
                                             "grunt-contrib-copy"
                                             "grunt-contrib-uglify"
                                             "grunt-contrib-watch"
                                             "grunt-karma"
                                             "grunt-livescript"
                                             "karma"
                                             "karma-qunit"
                                             "karma-chrome-launcher"
                                             "karma-firefox-launcher"
                                             "karma-ie-launcher"
                                             "karma-phantomjs-launcher"
                                             "phantomjs-prebuilt"
                                             "promises-aplus-tests"
                                             "qunit"
                                             "temp"
                                             "webpack")))))))
    (home-page "https://github.com/zloirock/core-js#readme")
    (synopsis "Standard library")
    (description "Standard library")
    (license license:expat)))

(define-public node-core-js-pure-3.30.2
  (package
    (name "node-core-js-pure")
    (version "3.30.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/core-js-pure/-/core-js-pure-3.30.2.tgz")
              (sha256
               (base32
                "12f4wp0m1sby4apz0chd60kifmwac4dxijrni1rqz01qiqdz2w5z"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://github.com/zloirock/core-js#readme")
    (synopsis "Standard library")
    (description "Standard library")
    (license license:expat)))

(define-public node-core-util-is-1.0.2
  (package
    (name "node-core-util-is")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/core-util-is/-/core-util-is-1.0.2.tgz")
              (sha256
               (base32
                "164k94d9bdzw1335kzakj7hflhnnixpx4n6ydbhf7vbrcnmlv954"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("tap")))))))
    (home-page "https://github.com/isaacs/core-util-is#readme")
    (synopsis "The `util.is*` functions introduced in Node v0.12.")
    (description "The `util.is*` functions introduced in Node v0.12.")
    (license license:expat)))

(define-public node-core-util-is-1.0.3
  (package
    (name "node-core-util-is")
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/core-util-is/-/core-util-is-1.0.3.tgz")
              (sha256
               (base32
                "032dwykfbxff1q7s0kgqdkwwzmm25mlrlfqijzibbwrc3z3zsc24"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("tap")))))))
    (home-page "https://github.com/isaacs/core-util-is#readme")
    (synopsis "The `util.is*` functions introduced in Node v0.12.")
    (description "The `util.is*` functions introduced in Node v0.12.")
    (license license:expat)))

(define-public node-cosmiconfig-6.0.0
  (package
    (name "node-cosmiconfig")
    (version "6.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/cosmiconfig/-/cosmiconfig-6.0.0.tgz")
              (sha256
               (base32
                "1qr66zb1wgmx8rxksjcrbpvv8zmpxhrm1wbbykdxm20bmipkg9w1"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@babel/cli" "@babel/core"
                                             "@babel/preset-env"
                                             "@babel/preset-typescript"
                                             "@types/jest"
                                             "@types/node"
                                             "@typescript-eslint/eslint-plugin"
                                             "@typescript-eslint/parser"
                                             "cross-env"
                                             "del"
                                             "del-cli"
                                             "eslint"
                                             "eslint-config-davidtheclark-node"
                                             "eslint-config-prettier"
                                             "eslint-plugin-import"
                                             "eslint-plugin-jest"
                                             "eslint-plugin-node"
                                             "husky"
                                             "jest"
                                             "lint-staged"
                                             "make-dir"
                                             "parent-module"
                                             "prettier"
                                             "remark-preset-davidtheclark"
                                             "typescript")))))))
    (inputs (list node-yaml-1.10.2 node-path-type-4.0.0 node-parse-json-5.2.0
                  node-import-fresh-3.3.0 node-types-parse-json-4.0.0))
    (home-page "https://github.com/davidtheclark/cosmiconfig#readme")
    (synopsis
     "Find and load configuration from a package.json property, rc file, or CommonJS module")
    (description
     "Find and load configuration from a package.json property, rc file, or CommonJS module")
    (license license:expat)))

(define-public node-cosmiconfig-7.1.0
  (package
    (name "node-cosmiconfig")
    (version "7.1.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/cosmiconfig/-/cosmiconfig-7.1.0.tgz")
              (sha256
               (base32
                "1mfnjsnkk7y2fxbh8jx42plszrn5xg5vyiq56axigsli25hpsirb"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@babel/cli" "@babel/core"
                                             "@babel/preset-env"
                                             "@babel/preset-typescript"
                                             "@types/jest"
                                             "@types/node"
                                             "@typescript-eslint/eslint-plugin"
                                             "@typescript-eslint/parser"
                                             "cross-env"
                                             "del"
                                             "del-cli"
                                             "eslint"
                                             "eslint-config-davidtheclark-node"
                                             "eslint-config-prettier"
                                             "eslint-plugin-import"
                                             "eslint-plugin-jest"
                                             "eslint-plugin-node"
                                             "husky"
                                             "jest"
                                             "lint-staged"
                                             "make-dir"
                                             "parent-module"
                                             "prettier"
                                             "remark-preset-davidtheclark"
                                             "typescript")))))))
    (inputs (list node-yaml-1.10.2 node-path-type-4.0.0 node-parse-json-5.2.0
                  node-import-fresh-3.3.0 node-types-parse-json-4.0.0))
    (home-page "https://github.com/davidtheclark/cosmiconfig#readme")
    (synopsis
     "Find and load configuration from a package.json property, rc file, or CommonJS module")
    (description
     "Find and load configuration from a package.json property, rc file, or CommonJS module")
    (license license:expat)))

(define-public node-create-emotion-9.2.12
  (package
    (name "node-create-emotion")
    (version "9.2.12")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/create-emotion/-/create-emotion-9.2.12.tgz")
              (sha256
               (base32
                "0a4bsws1wv8kisjcbvv1j6zg0r0sgrb92r7gjs7h9h7yk46fdf2w"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@types/react"
                                             "babel-plugin-transform-define"
                                             "dtslint")))))))
    (inputs (list node-stylis-rule-sheet-0.0.10
                  node-stylis-3.5.4
                  node-csstype-2.6.21
                  node-emotion-unitless-0.6.7
                  node-emotion-stylis-0.7.1
                  node-emotion-memoize-0.6.6
                  node-emotion-hash-0.6.6))
    (home-page "https://emotion.sh")
    (synopsis "The Next Generation of CSS-in-JS.")
    (description "The Next Generation of CSS-in-JS.")
    (license license:expat)))

(define-public node-create-emotion-styled-9.2.8
  (package
    (name "node-create-emotion-styled")
    (version "9.2.8")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/create-emotion-styled/-/create-emotion-styled-9.2.8.tgz")
              (sha256
               (base32
                "1yjh96lc5alha7vd9aqxbn1jbqrgygbrgmlyy63jycnm2mx41s0k"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@types/react" "dtslint"
                                             "prop-types")))))))
    (inputs (list node-emotion-is-prop-valid-0.6.8 node-prop-types-15.8.1))
    (home-page "https://emotion.sh")
    (synopsis "The styled API for emotion")
    (description "The styled API for emotion")
    (license license:expat)))

(define-public node-create-react-context-0.1.6
  (package
    (name "node-create-react-context")
    (version "0.1.6")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/create-react-context/-/create-react-context-0.1.6.tgz")
              (sha256
               (base32
                "05rgnfqb99xkwh4ckk0lilxdhdl2yvhphhkz0f9j31zhaq9jdmd9"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("babel-cli"
                                             "babel-plugin-add-module-exports"
                                             "babel-plugin-transform-class-properties"
                                             "babel-preset-env"
                                             "babel-preset-flow"
                                             "babel-preset-react"
                                             "enzyme"
                                             "enzyme-adapter-react-16"
                                             "enzyme-to-json"
                                             "flow-bin"
                                             "husky"
                                             "jest"
                                             "lint-staged"
                                             "prettier"
                                             "prop-types"
                                             "raf"
                                             "react"
                                             "react-dom")))))))
    (home-page "https://github.com/thejameskyle/create-react-context#readme")
    (synopsis "Polyfill for the proposed React context API")
    (description "Polyfill for the proposed React context API")
    (license license:expat)))

(define-public node-csstype-2.6.21
  (package
    (name "node-csstype")
    (version "2.6.21")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/csstype/-/csstype-2.6.21.tgz")
              (sha256
               (base32
                "1g5mlmsymw369yp44aidkczzljlsky3k6vl2j0c81zvyyyqbfgdg"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@types/chokidar" "@types/jest"
                                             "@types/jsdom"
                                             "@types/node"
                                             "@types/prettier"
                                             "chalk"
                                             "chokidar"
                                             "fast-glob"
                                             "flow-bin"
                                             "jest"
                                             "jsdom"
                                             "mdn-browser-compat-data"
                                             "mdn-data"
                                             "prettier"
                                             "sync-request"
                                             "ts-node"
                                             "tslint"
                                             "tslint-config-prettier"
                                             "turndown"
                                             "typescript")))))))
    (home-page "https://github.com/frenic/csstype#readme")
    (synopsis "Strict TypeScript and Flow types for style based on MDN data")
    (description
     "Strict TypeScript and Flow types for style based on MDN data")
    (license license:expat)))

(define-public node-csstype-3.1.2
  (package
    (name "node-csstype")
    (version "3.1.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/csstype/-/csstype-3.1.2.tgz")
              (sha256
               (base32
                "05nrmfqwl47hrgq5ci5idn2nrs8ki3dqma5bd6xfdg8n5vlp7qq0"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@types/chokidar"
                                             "@types/css-tree"
                                             "@types/jest"
                                             "@types/jsdom"
                                             "@types/node"
                                             "@types/prettier"
                                             "@types/request"
                                             "@types/turndown"
                                             "@typescript-eslint/eslint-plugin"
                                             "@typescript-eslint/parser"
                                             "chalk"
                                             "chokidar"
                                             "eslint"
                                             "css-tree"
                                             "eslint-config-prettier"
                                             "eslint-plugin-prettier"
                                             "fast-glob"
                                             "flow-bin"
                                             "jest"
                                             "jsdom"
                                             "mdn-browser-compat-data"
                                             "mdn-data"
                                             "prettier"
                                             "request"
                                             "ts-jest"
                                             "ts-node"
                                             "turndown"
                                             "typescript")))))))
    (home-page "https://github.com/frenic/csstype#readme")
    (synopsis "Strict TypeScript and Flow types for style based on MDN data")
    (description
     "Strict TypeScript and Flow types for style based on MDN data")
    (license license:expat)))

(define-public node-dashdash-1.14.1
  (package
    (name "node-dashdash")
    (version "1.14.1")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/dashdash/-/dashdash-1.14.1.tgz")
              (sha256
               (base32
                "0h2kaml5wgx5x430wlbnjz3j6q1ppvndqckylfmi13xa33gfnycb"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("nodeunit")))))))
    (inputs (list node-assert-plus-1.0.0))
    (home-page "https://github.com/trentm/node-dashdash#readme")
    (synopsis "A light, featureful and explicit option parsing library.")
    (description "A light, featureful and explicit option parsing library.")
    (license license:expat)))

(define-public node-debug-2.6.9
  (package
    (name "node-debug")
    (version "2.6.9")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/debug/-/debug-2.6.9.tgz")
              (sha256
               (base32
                "160wvc74r8aypds7pym3hq4qpa786hpk4vif58ggiwcqcv34ibil"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("browserify" "chai"
                                             "concurrently"
                                             "coveralls"
                                             "eslint"
                                             "istanbul"
                                             "karma"
                                             "karma-chai"
                                             "karma-mocha"
                                             "karma-phantomjs-launcher"
                                             "karma-sinon"
                                             "mocha"
                                             "mocha-lcov-reporter"
                                             "rimraf"
                                             "sinon"
                                             "sinon-chai")))))))
    (inputs (list node-ms-2.0.0))
    (home-page "https://github.com/visionmedia/debug#readme")
    (synopsis "small debugging utility")
    (description "small debugging utility")
    (license license:expat)))

(define-public node-debug-4.3.4
  (package
    (name "node-debug")
    (version "4.3.4")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/debug/-/debug-4.3.4.tgz")
              (sha256
               (base32
                "1kwbyb5m63bz8a2bvhy4gsnsma6ks5wa4w5qya6qb9ip5sdjr4h4"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("brfs" "browserify"
                                             "coveralls"
                                             "istanbul"
                                             "karma"
                                             "karma-browserify"
                                             "karma-chrome-launcher"
                                             "karma-mocha"
                                             "mocha"
                                             "mocha-lcov-reporter"
                                             "xo")))))))
    (inputs (list node-ms-2.1.2))
    (home-page "https://github.com/debug-js/debug#readme")
    (synopsis "Lightweight debugging utility for Node.js and the browser")
    (description "Lightweight debugging utility for Node.js and the browser")
    (license license:expat)))

(define-public node-decode-uri-component-0.2.2
  (package
    (name "node-decode-uri-component")
    (version "0.2.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/decode-uri-component/-/decode-uri-component-0.2.2.tgz")
              (sha256
               (base32
                "0qqjrlqr1k8fga8i9g8x4wb0qv9fi411fd07gqr78m3anxwyn8y1"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "coveralls" "nyc" "xo")))))))
    (home-page "https://github.com/SamVerschueren/decode-uri-component#readme")
    (synopsis "A better decodeURIComponent")
    (description "A better decodeURIComponent")
    (license license:expat)))

(define-public node-deep-is-0.1.4
  (package
    (name "node-deep-is")
    (version "0.1.4")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/deep-is/-/deep-is-0.1.4.tgz")
              (sha256
               (base32
                "0g5z206z33f41cdh220vf7kpj61cfcnmlnrcrk1ffs5zqs1sl0qc"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("tape")))))))
    (home-page "https://github.com/thlorenz/deep-is#readme")
    (synopsis
     "node's assert.deepEqual algorithm except for NaN being equal to NaN")
    (description
     "node's assert.deepEqual algorithm except for NaN being equal to NaN")
    (license license:expat)))

(define-public node-define-property-0.2.5
  (package
    (name "node-define-property")
    (version "0.2.5")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/define-property/-/define-property-0.2.5.tgz")
              (sha256
               (base32
                "1r2gws87mpwv0i1rl3l79bw8psgpz44vwyd9va9cr90bvkada5yk"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("mocha" "should")))))))
    (inputs (list node-is-descriptor-0.1.6))
    (home-page "https://github.com/jonschlinkert/define-property")
    (synopsis "Define a non-enumerable property on an object.")
    (description "Define a non-enumerable property on an object.")
    (license license:expat)))

(define-public node-define-property-1.0.0
  (package
    (name "node-define-property")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/define-property/-/define-property-1.0.0.tgz")
              (sha256
               (base32
                "1547m4v074hgd28jzv4k82ig43pl7rgfnp0y832swxmlff4ra6m6"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp-format-md" "mocha")))))))
    (inputs (list node-is-descriptor-1.0.2))
    (home-page "https://github.com/jonschlinkert/define-property")
    (synopsis "Define a non-enumerable property on an object.")
    (description "Define a non-enumerable property on an object.")
    (license license:expat)))

(define-public node-define-property-2.0.2
  (package
    (name "node-define-property")
    (version "2.0.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/define-property/-/define-property-2.0.2.tgz")
              (sha256
               (base32
                "0m8x3myy76d3w777c1jq94gafphxqrpj7sy3myxcvksfk0p8vpha"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp-format-md" "mocha")))))))
    (inputs (list node-isobject-3.0.1 node-is-descriptor-1.0.2))
    (home-page "https://github.com/jonschlinkert/define-property")
    (synopsis
     "Define a non-enumerable property on an object. Uses Reflect.defineProperty when available, otherwise Object.defineProperty.")
    (description
     "Define a non-enumerable property on an object. Uses Reflect.defineProperty when available, otherwise Object.defineProperty.")
    (license license:expat)))

(define-public node-delay-5.0.0
  (package
    (name "node-delay")
    (version "5.0.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/delay/-/delay-5.0.0.tgz")
              (sha256
               (base32
                "1bs6r1j2m1fd3igfdbcdz2i5f52mac2prm5hzbc83fygn2x0gd8k"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("abort-controller" "ava"
                                             "currently-unhandled"
                                             "in-range"
                                             "time-span"
                                             "tsd"
                                             "xo")))))))
    (home-page "https://github.com/sindresorhus/delay#readme")
    (synopsis "Delay a promise a specified amount of time")
    (description "Delay a promise a specified amount of time")
    (license license:expat)))

(define-public node-delayed-stream-1.0.0
  (package
    (name "node-delayed-stream")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/delayed-stream/-/delayed-stream-1.0.0.tgz")
              (sha256
               (base32
                "1lr98585rayrc5xfj599hg6mxqvks38diir74ivivyvx47jgqf5c"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("fake" "far")))))))
    (home-page "https://github.com/felixge/node-delayed-stream")
    (synopsis
     "Buffers events from a stream until you are ready to handle them.")
    (description
     "Buffers events from a stream until you are ready to handle them.")
    (license license:expat)))

(define-public node-delegate-3.2.0
  (package
    (name "node-delegate")
    (version "3.2.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/delegate/-/delegate-3.2.0.tgz")
              (sha256
               (base32
                "1ys85812sy02lmz6r4gc5987hr5bnsiagiil8ncggaiaxk76j0vy"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("browserify" "chai"
                                             "karma"
                                             "karma-browserify"
                                             "karma-chai"
                                             "karma-mocha"
                                             "karma-phantomjs-launcher"
                                             "karma-sinon"
                                             "mocha"
                                             "phantomjs-polyfill"
                                             "simulant"
                                             "sinon")))))))
    (home-page "https://github.com/zenorocha/delegate#readme")
    (synopsis "Lightweight event delegation")
    (description "Lightweight event delegation")
    (license license:expat)))

(define-public node-delegates-1.0.0
  (package
    (name "node-delegates")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/delegates/-/delegates-1.0.0.tgz")
              (sha256
               (base32
                "0qb4rw56fbxcpc2iwj1x0qxzakwwqigxgggggd6ajl7d27sdpvxz"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("mocha" "should")))))))
    (home-page "https://github.com/visionmedia/node-delegates#readme")
    (synopsis "delegate methods and accessors to another property")
    (description "delegate methods and accessors to another property")
    (license license:expat)))

(define-public node-depd-2.0.0
  (package
    (name "node-depd")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/depd/-/depd-2.0.0.tgz")
              (sha256
               (base32
                "19yl2piwl0ci2lvn5j5sk0z4nbldj6apsrqds3ql2d09aqh8m998"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("benchmark" "beautify-benchmark"
                                             "eslint"
                                             "eslint-config-standard"
                                             "eslint-plugin-import"
                                             "eslint-plugin-markdown"
                                             "eslint-plugin-node"
                                             "eslint-plugin-promise"
                                             "eslint-plugin-standard"
                                             "istanbul"
                                             "mocha"
                                             "safe-buffer"
                                             "uid-safe")))))))
    (home-page "https://github.com/dougwilson/nodejs-depd#readme")
    (synopsis "Deprecate all the things")
    (description "Deprecate all the things")
    (license license:expat)))

(define-public node-destroy-1.2.0
  (package
    (name "node-destroy")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/destroy/-/destroy-1.2.0.tgz")
              (sha256
               (base32
                "1a6gf6hn9zc4g6v3dqdcsc3v1n22qbv1s5xmdakljjgmdkl2gzcd"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("eslint" "eslint-config-standard"
                                             "eslint-plugin-import"
                                             "eslint-plugin-node"
                                             "eslint-plugin-promise"
                                             "eslint-plugin-standard"
                                             "mocha"
                                             "nyc")))))))
    (home-page "https://github.com/stream-utils/destroy#readme")
    (synopsis "destroy a stream if possible")
    (description "destroy a stream if possible")
    (license license:expat)))

(define-public node-detect-libc-2.0.1
  (package
    (name "node-detect-libc")
    (version "2.0.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/detect-libc/-/detect-libc-2.0.1.tgz")
              (sha256
               (base32
                "1j2nlrp28b2yig0vf96fbdz9favscl60ljz8rx2l9fzhyn5ipvk6"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "nyc" "proxyquire"
                                             "semistandard")))))))
    (home-page "https://github.com/lovell/detect-libc#readme")
    (synopsis
     "Node.js module to detect the C standard library (libc) implementation family and version")
    (description
     "Node.js module to detect the C standard library (libc) implementation family and version")
    (license license:asl2.0)))

(define-public node-diff-match-patch-1.0.5
  (package
    (name "node-diff-match-patch")
    (version "1.0.5")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/diff-match-patch/-/diff-match-patch-1.0.5.tgz")
              (sha256
               (base32
                "1av5phlznfsvj4yr01hfh83b5fv6izs6810jq7l9q927yr5d8mpa"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("testit")))))))
    (home-page "https://github.com/JackuB/diff-match-patch#readme")
    (synopsis "npm package for https://github.com/google/diff-match-patch")
    (description "npm package for https://github.com/google/diff-match-patch")
    (license license:asl2.0)))

(define-public node-ecc-jsbn-0.1.2
  (package
    (name "node-ecc-jsbn")
    (version "0.1.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/ecc-jsbn/-/ecc-jsbn-0.1.2.tgz")
              (sha256
               (base32
                "0x39lihzphr0h1fvh9p65k86vx3p7z6jrxgv4b402lvdrifd56k0"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (inputs (list node-safer-buffer-2.1.2 node-jsbn-0.1.1))
    (home-page "https://github.com/quartzjer/ecc-jsbn")
    (synopsis "ECC JS code based on JSBN")
    (description "ECC JS code based on JSBN")
    (license license:expat)))

(define-public node-ee-first-1.1.1
  (package
    (name "node-ee-first")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/ee-first/-/ee-first-1.1.1.tgz")
              (sha256
               (base32
                "175r500n567a04qmswzw5hkgdnika3dvn63n284jlar2gvmyhj2i"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("istanbul" "mocha")))))))
    (home-page "https://github.com/jonathanong/ee-first")
    (synopsis "return the first event in a set of ee/event pairs")
    (description "return the first event in a set of ee/event pairs")
    (license license:expat)))

(define-public node-emoji-regex-8.0.0
  (package
    (name "node-emoji-regex")
    (version "8.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/emoji-regex/-/emoji-regex-8.0.0.tgz")
              (sha256
               (base32
                "01xi3ikahnlj77h2gqs3jb7kmnxn1nsb9dmnpvpqw288zgxxkk5m"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@babel/cli" "@babel/core"
                                             "@babel/plugin-proposal-unicode-property-regex"
                                             "@babel/preset-env"
                                             "mocha"
                                             "regexgen"
                                             "unicode-12.0.0")))))))
    (home-page "https://mths.be/emoji-regex")
    (synopsis
     "A regular expression to match all Emoji-only symbols as per the Unicode Standard.")
    (description
     "A regular expression to match all Emoji-only symbols as per the Unicode Standard.")
    (license license:expat)))

(define-public node-emotion-9.2.12
  (package
    (name "node-emotion")
    (version "9.2.12")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/emotion/-/emotion-9.2.12.tgz")
              (sha256
               (base32
                "0ps3zzpnaid2mg1bwcw9dpr2vxamnmgrgal1ia3jl2g841h6adf5"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@types/react"
                                             "babel-plugin-transform-define"
                                             "dtslint")))))))
    (inputs (list node-create-emotion-9.2.12 node-babel-plugin-emotion-9.2.11))
    (home-page "https://emotion.sh")
    (synopsis "The Next Generation of CSS-in-JS.")
    (description "The Next Generation of CSS-in-JS.")
    (license license:expat)))

(define-public node-emotion-babel-plugin-11.11.0
  (package
    (name "node-emotion-babel-plugin")
    (version "11.11.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/@emotion/babel-plugin/-/babel-plugin-11.11.0.tgz")
              (sha256
               (base32
                "1ms9g5n138pgvq601m1blgg5vg77znhzqi74ygbfykx9iymv5ka4"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@babel/core"
                                             "babel-check-duplicated-nodes")))))))
    (inputs (list node-stylis-4.2.0
                  node-source-map-0.5.7
                  node-find-root-1.1.0
                  node-escape-string-regexp-4.0.0
                  node-convert-source-map-1.9.0
                  node-babel-plugin-macros-3.1.0
                  node-emotion-serialize-1.1.2
                  node-emotion-memoize-0.8.1
                  node-emotion-hash-0.9.1
                  node-babel-runtime-7.22.5
                  node-babel-helper-module-imports-7.22.5))
    (home-page "https://emotion.sh")
    (synopsis
     "A recommended babel preprocessing plugin for emotion, The Next Generation of CSS-in-JS.")
    (description
     "A recommended babel preprocessing plugin for emotion, The Next Generation of CSS-in-JS.")
    (license license:expat)))

(define-public node-emotion-babel-utils-0.6.10
  (package
    (name "node-emotion-babel-utils")
    (version "0.6.10")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/@emotion/babel-utils/-/babel-utils-0.6.10.tgz")
              (sha256
               (base32
                "02s0nks0krdzxwbn8x1k7isl4vbi9y9pyps6kkisskz3pcxry6wf"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (inputs (list node-source-map-0.7.4
                  node-find-root-1.1.0
                  node-convert-source-map-1.9.0
                  node-emotion-serialize-0.9.1
                  node-emotion-memoize-0.6.6
                  node-emotion-hash-0.6.6))
    (home-page "https://www.npmjs.com/package/node-emotion-babel-utils")
    (synopsis "Internal Babel utils used in various macros")
    (description "Internal Babel utils used in various macros")
    (license license:expat)))

(define-public node-emotion-cache-11.11.0
  (package
    (name "node-emotion-cache")
    (version "11.11.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/@emotion/cache/-/cache-11.11.0.tgz")
              (sha256
               (base32
                "1xnkdybz2zjp9djrmaxydmn7n7h7dfdl1n8lqkc523ia67k4xcpz"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@definitelytyped/dtslint"
                                             "@emotion/hash" "typescript")))))))
    (inputs (list node-stylis-4.2.0 node-emotion-weak-memoize-0.3.1
                  node-emotion-utils-1.2.1 node-emotion-sheet-1.2.2
                  node-emotion-memoize-0.8.1))
    (home-page "https://github.com/emotion-js/emotion/tree/main#readme")
    (synopsis "emotion's cache")
    (description "emotion's cache")
    (license license:expat)))

(define-public node-emotion-css-11.11.2
  (package
    (name "node-emotion-css")
    (version "11.11.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/@emotion/css/-/css-11.11.2.tgz")
              (sha256
               (base32
                "1hgasqz87dkrfpn6s98wq7hm0x3y4gs0v2drplqnfaxlz25hslsc"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@definitelytyped/dtslint"
                                             "typescript")))))))
    (inputs (list node-emotion-utils-1.2.1 node-emotion-sheet-1.2.2
                  node-emotion-serialize-1.1.2 node-emotion-cache-11.11.0
                  node-emotion-babel-plugin-11.11.0))
    (home-page "https://emotion.sh")
    (synopsis "The Next Generation of CSS-in-JS.")
    (description "The Next Generation of CSS-in-JS.")
    (license license:expat)))

(define-public node-emotion-hash-0.6.6
  (package
    (name "node-emotion-hash")
    (version "0.6.6")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/@emotion/hash/-/hash-0.6.6.tgz")
              (sha256
               (base32
                "1cbb9m6j76xa4nvnk5kg12q18vad9r3kxgl1b4skr9zbsj6f6h5x"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://www.npmjs.com/package/node-emotion-hash")
    (synopsis "A MurmurHash2 implementation")
    (description "A MurmurHash2 implementation")
    (license license:expat)))

(define-public node-emotion-hash-0.9.1
  (package
    (name "node-emotion-hash")
    (version "0.9.1")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/@emotion/hash/-/hash-0.9.1.tgz")
              (sha256
               (base32
                "0874yb5q67xwxkdg7l2wvxqrcp86vni7k3jnm3p8h76jnv2ilb2k"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@definitelytyped/dtslint"
                                             "typescript")))))))
    (home-page "https://github.com/emotion-js/emotion/tree/main#readme")
    (synopsis "A MurmurHash2 implementation")
    (description "A MurmurHash2 implementation")
    (license license:expat)))

(define-public node-emotion-is-prop-valid-0.6.8
  (package
    (name "node-emotion-is-prop-valid")
    (version "0.6.8")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/@emotion/is-prop-valid/-/is-prop-valid-0.6.8.tgz")
              (sha256
               (base32
                "1yw9j5jg06wp15qd61d0crm7h8icj5h7bjhaig54f1jdhyxjw239"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (inputs (list node-emotion-memoize-0.6.6))
    (home-page "https://www.npmjs.com/package/node-emotion-is-prop-valid")
    (synopsis
     "A function to check whether a prop is valid for HTML and SVG elements")
    (description
     "A function to check whether a prop is valid for HTML and SVG elements")
    (license license:expat)))

(define-public node-emotion-is-prop-valid-1.2.1
  (package
    (name "node-emotion-is-prop-valid")
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/@emotion/is-prop-valid/-/is-prop-valid-1.2.1.tgz")
              (sha256
               (base32
                "024g2q21mpnqxbgx4532sdyx7bz3h75j1rqy289dqjs6c4silnr5"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@definitelytyped/dtslint"
                                             "typescript")))))))
    (inputs (list node-emotion-memoize-0.8.1))
    (home-page "https://github.com/emotion-js/emotion/tree/main#readme")
    (synopsis
     "A function to check whether a prop is valid for HTML and SVG elements")
    (description
     "A function to check whether a prop is valid for HTML and SVG elements")
    (license license:expat)))

(define-public node-emotion-memoize-0.6.6
  (package
    (name "node-emotion-memoize")
    (version "0.6.6")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/@emotion/memoize/-/memoize-0.6.6.tgz")
              (sha256
               (base32
                "0fgj8kjn7ihiwbq2p5nbcv5a12zpy0x2smxlgfhk5xqj3ln1zgbn"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://www.npmjs.com/package/node-emotion-memoize")
    (synopsis "emotion's memoize utility")
    (description "emotion's memoize utility")
    (license license:expat)))

(define-public node-emotion-memoize-0.8.1
  (package
    (name "node-emotion-memoize")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/@emotion/memoize/-/memoize-0.8.1.tgz")
              (sha256
               (base32
                "1fpl01xzxcd9ygydxxd19f1nww284jcdzicqcz5rz8q01d4x7vp1"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@definitelytyped/dtslint"
                                             "typescript")))))))
    (home-page "https://github.com/emotion-js/emotion/tree/main#readme")
    (synopsis "emotion's memoize utility")
    (description "emotion's memoize utility")
    (license license:expat)))

(define-public node-emotion-react-11.11.1
  (package
    (name "node-emotion-react")
    (version "11.11.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/@emotion/react/-/react-11.11.1.tgz")
              (sha256
               (base32
                "0gz2azzfhchrnlx7kpwd47dwbsb4g59lran05r0q1abk78qxjj7i"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@definitelytyped/dtslint"
                                             "@emotion/css"
                                             "@emotion/css-prettifier"
                                             "@emotion/server"
                                             "@emotion/styled"
                                             "html-tag-names"
                                             "react"
                                             "svg-tag-names"
                                             "typescript")))))))
    (inputs (list node-hoist-non-react-statics-3.3.2
                  node-emotion-weak-memoize-0.3.1
                  node-emotion-utils-1.2.1
                  node-emotion-use-insertion-effect-with-fallbacks-1.0.1
                  node-emotion-serialize-1.1.2
                  node-emotion-cache-11.11.0
                  node-emotion-babel-plugin-11.11.0
                  node-babel-runtime-7.22.5
                  node-react-18.2.0))
    (home-page "https://github.com/emotion-js/emotion/tree/main#readme")
    (synopsis "> Simple styling in React.")
    (description "> Simple styling in React.")
    (license license:expat)))

(define-public node-emotion-serialize-0.9.1
  (package
    (name "node-emotion-serialize")
    (version "0.9.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/@emotion/serialize/-/serialize-0.9.1.tgz")
              (sha256
               (base32
                "1x60cb0nkfh93db0k0ci26466nnij0hvsx9zff8s1p6jkm2pzdls"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (inputs (list node-emotion-utils-0.8.2 node-emotion-unitless-0.6.7
                  node-emotion-memoize-0.6.6 node-emotion-hash-0.6.6))
    (home-page "https://www.npmjs.com/package/node-emotion-serialize")
    (synopsis "serialization utils for emotion")
    (description "serialization utils for emotion")
    (license license:expat)))

(define-public node-emotion-serialize-1.1.2
  (package
    (name "node-emotion-serialize")
    (version "1.1.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/@emotion/serialize/-/serialize-1.1.2.tgz")
              (sha256
               (base32
                "06w3mhkim9ipdiqf9aw0z5w9ak5yib7a9qzm8mlylmyr9zc96rs8"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@definitelytyped/dtslint"
                                             "typescript")))))))
    (inputs (list node-csstype-3.1.2 node-emotion-utils-1.2.1
                  node-emotion-unitless-0.8.1 node-emotion-memoize-0.8.1
                  node-emotion-hash-0.9.1))
    (home-page "https://github.com/emotion-js/emotion/tree/main#readme")
    (synopsis "serialization utils for emotion")
    (description "serialization utils for emotion")
    (license license:expat)))

(define-public node-emotion-sheet-1.2.2
  (package
    (name "node-emotion-sheet")
    (version "1.2.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/@emotion/sheet/-/sheet-1.2.2.tgz")
              (sha256
               (base32
                "0cirjbx6db4mdgyvf27wrwafzmhxq4ccjj41qsj57q010l2w1l1h"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@definitelytyped/dtslint"
                                             "typescript")))))))
    (home-page "https://github.com/emotion-js/emotion/tree/main#readme")
    (synopsis "emotion's stylesheet")
    (description "emotion's stylesheet")
    (license license:expat)))

(define-public node-emotion-styled-11.11.0
  (package
    (name "node-emotion-styled")
    (version "11.11.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/@emotion/styled/-/styled-11.11.0.tgz")
              (sha256
               (base32
                "1ghjs7sr5bs5v0pxgfmpqwr4zy0n5s8aq14ajxcs3vqgi1r87m24"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@definitelytyped/dtslint"
                                             "@emotion/react" "react"
                                             "typescript")))))))
    (inputs (list node-emotion-utils-1.2.1
                  node-emotion-use-insertion-effect-with-fallbacks-1.0.1
                  node-emotion-serialize-1.1.2
                  node-emotion-is-prop-valid-1.2.1
                  node-emotion-babel-plugin-11.11.0
                  node-babel-runtime-7.22.5
                  node-react-18.2.0
                  node-emotion-react-11.11.1))
    (home-page "https://github.com/emotion-js/emotion/tree/main#readme")
    (synopsis "styled API for emotion")
    (description "styled API for emotion")
    (license license:expat)))

(define-public node-emotion-stylis-0.7.1
  (package
    (name "node-emotion-stylis")
    (version "0.7.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/@emotion/stylis/-/stylis-0.7.1.tgz")
              (sha256
               (base32
                "1g038f0pbq4l6hrddicnxxff4z79khwwfn88bxpzgn0mz96frckn"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("dtslint" "jscodeshift" "request"
                                             "request-promise-native" "stylis")))))))
    (home-page "https://www.npmjs.com/package/node-emotion-stylis")
    (synopsis "A custom build of Stylis")
    (description "A custom build of Stylis")
    (license license:expat)))

(define-public node-emotion-unitless-0.6.7
  (package
    (name "node-emotion-unitless")
    (version "0.6.7")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/@emotion/unitless/-/unitless-0.6.7.tgz")
              (sha256
               (base32
                "07jprfn958gmsnphyblvn81lmk3g9r0jqp612km87n0397kb56lw"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://www.npmjs.com/package/node-emotion-unitless")
    (synopsis
     "An object of css properties that don't accept values with units")
    (description
     "An object of css properties that don't accept values with units")
    (license license:expat)))

(define-public node-emotion-unitless-0.8.1
  (package
    (name "node-emotion-unitless")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/@emotion/unitless/-/unitless-0.8.1.tgz")
              (sha256
               (base32
                "0cbv53lkv474h8pxqwi3izpgm9vbqgcrk3vvk7algi39rz38gvaw"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://github.com/emotion-js/emotion/tree/main#readme")
    (synopsis
     "An object of css properties that don't accept values with units")
    (description
     "An object of css properties that don't accept values with units")
    (license license:expat)))

(define-public node-emotion-use-insertion-effect-with-fallbacks-1.0.1
  (package
    (name "node-emotion-use-insertion-effect-with-fallbacks")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/@emotion/use-insertion-effect-with-fallbacks/-/use-insertion-effect-with-fallbacks-1.0.1.tgz")
              (sha256
               (base32
                "1l9fz545s95zbzx81p43xrhyd6k0s32b7ka086cmxpdwqzfjvxvm"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("react")))))))
    (home-page "https://github.com/emotion-js/emotion/tree/main#readme")
    (synopsis
     "A wrapper package that uses `useInsertionEffect` or a fallback for it")
    (description
     "A wrapper package that uses `useInsertionEffect` or a fallback for it")
    (license license:expat)))

(define-public node-emotion-utils-0.8.2
  (package
    (name "node-emotion-utils")
    (version "0.8.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/@emotion/utils/-/utils-0.8.2.tgz")
              (sha256
               (base32
                "1bpc42kvbmpyc68p519ck9bwddd3a6962wrj67awayqwrifg7pgh"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("dtslint")))))))
    (home-page "https://www.npmjs.com/package/node-emotion-utils")
    (synopsis "internal utils for emotion")
    (description "internal utils for emotion")
    (license license:expat)))

(define-public node-emotion-utils-1.2.1
  (package
    (name "node-emotion-utils")
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/@emotion/utils/-/utils-1.2.1.tgz")
              (sha256
               (base32
                "1nyd2h3j41ig4lsi8a0g1yxkps9iixa9pfaglbrdmbpc7nhjnsnr"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@definitelytyped/dtslint"
                                             "typescript")))))))
    (home-page "https://github.com/emotion-js/emotion/tree/main#readme")
    (synopsis "internal utils for emotion")
    (description "internal utils for emotion")
    (license license:expat)))

(define-public node-emotion-weak-memoize-0.3.1
  (package
    (name "node-emotion-weak-memoize")
    (version "0.3.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/@emotion/weak-memoize/-/weak-memoize-0.3.1.tgz")
              (sha256
               (base32
                "12aznp6q17wdb3vp0p8d50kyb7ph741l8ksahf86r4nd4szj7g0h"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@definitelytyped/dtslint"
                                             "typescript")))))))
    (home-page "https://github.com/emotion-js/emotion/tree/main#readme")
    (synopsis "A memoization function that uses a WeakMap")
    (description "A memoization function that uses a WeakMap")
    (license license:expat)))

(define-public node-encodeurl-1.0.2
  (package
    (name "node-encodeurl")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/encodeurl/-/encodeurl-1.0.2.tgz")
              (sha256
               (base32
                "13afvicx42ha4k29571sg0i4b76xjggyxvmmmibm258ipf6mjinb"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("eslint" "eslint-config-standard"
                                             "eslint-plugin-import"
                                             "eslint-plugin-node"
                                             "eslint-plugin-promise"
                                             "eslint-plugin-standard"
                                             "istanbul"
                                             "mocha")))))))
    (home-page "https://github.com/pillarjs/encodeurl#readme")
    (synopsis
     "Encode a URL to a percent-encoded form, excluding already-encoded sequences")
    (description
     "Encode a URL to a percent-encoded form, excluding already-encoded sequences")
    (license license:expat)))

(define-public node-encoding-0.1.13
  (package
    (name "node-encoding")
    (version "0.1.13")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/encoding/-/encoding-0.1.13.tgz")
              (sha256
               (base32
                "116gipr1y0hc9zvflsvd39psbbf2j62zvpnw099pqf7fl0nb5xbc"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("nodeunit")))))))
    (inputs (list node-iconv-lite-0.6.3))
    (home-page "https://github.com/andris9/encoding#readme")
    (synopsis "Convert encodings, uses iconv-lite")
    (description "Convert encodings, uses iconv-lite")
    (license license:expat)))

(define-public node-entities-3.0.1
  (package
    (name "node-entities")
    (version "3.0.1")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/entities/-/entities-3.0.1.tgz")
              (sha256
               (base32
                "1xixnv7ydywnn3lnf387fchlpsddxwa8nqcj24pannym246cmpdx"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:modules ((guix build node-build-system)
                  (srfi srfi-1)
                  (ice-9 match)
                  (guix build utils))
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-before 'configure 'avoid-prepare-scripts
                    (lambda _
                      (with-atomic-json-file-replacement "package.json"
                                                         (match-lambda
                                                           (((quote @) . pkg-meta-alist)
                                                            (cons '@
                                                                  (map (match-lambda
                                                                         (("scripts" @ . scripts-alist) `
                                                                          ("scripts"
                                                                           @
                                                                           ,@(filter (match-lambda
                                                                                       
                                                                                       (("prepare" . _)
                                                                                        #f)
                                                                                       
                                                                                       (_
                                                                                        #t))
                                                                              scripts-alist)))
                                                                         (other
                                                                          other))
                                                                   pkg-meta-alist)))))))
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@types/jest" "@types/node"
                                             "@typescript-eslint/eslint-plugin"
                                             "@typescript-eslint/parser"
                                             "eslint"
                                             "eslint-config-prettier"
                                             "eslint-plugin-node"
                                             "jest"
                                             "prettier"
                                             "ts-jest"
                                             "typescript")))))))
    (home-page "https://github.com/fb55/entities#readme")
    (synopsis "Encode & decode XML and HTML entities with ease")
    (description "Encode & decode XML and HTML entities with ease")
    (license license:bsd-2)))

(define-public node-error-ex-1.3.2
  (package
    (name "node-error-ex")
    (version "1.3.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/error-ex/-/error-ex-1.3.2.tgz")
              (sha256
               (base32
                "12gyrmh6iqpx838bnb5iwcqm2447rnbxx1bvqn76l40fvr1aichs"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("coffee-script" "coveralls"
                                             "istanbul" "mocha" "should" "xo")))))))
    (inputs (list node-is-arrayish-0.2.1))
    (home-page "https://github.com/qix-/node-error-ex#readme")
    (synopsis "Easy error subclassing and stack customization")
    (description "Easy error subclassing and stack customization")
    (license license:expat)))

(define-public node-es6-object-assign-1.1.0
  (package
    (name "node-es6-object-assign")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/es6-object-assign/-/es6-object-assign-1.1.0.tgz")
              (sha256
               (base32
                "0qqnv6adfh52zd49jay2kiav4pjwn8v9b8g6khhpybp6sxq6m4kv"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("browserify" "uglify-js")))))))
    (home-page "https://github.com/rubennorte/es6-object-assign")
    (synopsis "ECMAScript 2015 (ES6) Object.assign polyfill and ponyfill")
    (description "ECMAScript 2015 (ES6) Object.assign polyfill and ponyfill")
    (license license:expat)))

(define-public node-es6-promise-4.2.8
  (package
    (name "node-es6-promise")
    (version "4.2.8")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/es6-promise/-/es6-promise-4.2.8.tgz")
              (sha256
               (base32
                "19v6k3c1035x0ba9qpfjjp14qicd86s8amfz0nsxf9w3dx0xqfpa"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("babel-plugin-transform-es2015-arrow-functions"
                                             "babel-plugin-transform-es2015-block-scoping"
                                             "babel-plugin-transform-es2015-classes"
                                             "babel-plugin-transform-es2015-computed-properties"
                                             "babel-plugin-transform-es2015-constants"
                                             "babel-plugin-transform-es2015-destructuring"
                                             "babel-plugin-transform-es2015-parameters"
                                             "babel-plugin-transform-es2015-shorthand-properties"
                                             "babel-plugin-transform-es2015-spread"
                                             "babel-plugin-transform-es2015-template-literals"
                                             "babel6-plugin-strip-class-callcheck"
                                             "broccoli-babel-transpiler"
                                             "broccoli-concat"
                                             "broccoli-merge-trees"
                                             "broccoli-rollup"
                                             "broccoli-stew"
                                             "broccoli-uglify-js"
                                             "broccoli-watchify"
                                             "ember-cli"
                                             "ember-cli-dependency-checker"
                                             "git-repo-version"
                                             "json3"
                                             "mocha"
                                             "promises-aplus-tests-phantom")))))))
    (home-page "https://github.com/stefanpenner/es6-promise")
    (synopsis
     "A lightweight library that provides tools for organizing asynchronous code")
    (description
     "A lightweight library that provides tools for organizing asynchronous code")
    (license license:expat)))

(define-public node-es6-promisify-5.0.0
  (package
    (name "node-es6-promisify")
    (version "5.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/es6-promisify/-/es6-promisify-5.0.0.tgz")
              (sha256
               (base32
                "1p87k4ny1ccbs1r92fzskc6hgrp6n7gfqxia888vzkq2w5g9dkbc"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("babel-preset-es2015" "eslint"
                                             "gulp" "gulp-babel" "nodeunit")))))))
    (inputs (list node-es6-promise-4.2.8))
    (home-page "https://github.com/digitaldesignlabs/es6-promisify#readme")
    (synopsis "Converts callback-based functions to ES6 Promises")
    (description "Converts callback-based functions to ES6 Promises")
    (license license:expat)))

(define-public node-escalade-3.1.1
  (package
    (name "node-escalade")
    (version "3.1.1")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/escalade/-/escalade-3.1.1.tgz")
              (sha256
               (base32
                "17d7icirlarj6mjx321z04klpvjsxwga8c8svai2547bid131y30"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("bundt" "esm" "uvu")))))))
    (home-page "https://github.com/lukeed/escalade#readme")
    (synopsis
     "A tiny (183B to 210B) and fast utility to ascend parent directories")
    (description
     "A tiny (183B to 210B) and fast utility to ascend parent directories")
    (license license:expat)))

(define-public node-escape-html-1.0.3
  (package
    (name "node-escape-html")
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/escape-html/-/escape-html-1.0.3.tgz")
              (sha256
               (base32
                "0rh35dvab1wbp87dy1m6rynbcb9rbs5kry7jk17ixyxx7if1a0d1"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("benchmark" "beautify-benchmark")))))))
    (home-page "https://github.com/component/escape-html")
    (synopsis "Escape string for use in HTML")
    (description "Escape string for use in HTML")
    (license license:expat)))

(define-public node-escape-string-regexp-1.0.5
  (package
    (name "node-escape-string-regexp")
    (version "1.0.5")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/escape-string-regexp/-/escape-string-regexp-1.0.5.tgz")
              (sha256
               (base32
                "0iy3jirnnslnfwk8wa5xkg56fnbmg7bsv5v2a1s0qgbnfqp7j375"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "xo")))))))
    (home-page "https://github.com/sindresorhus/escape-string-regexp")
    (synopsis "Escape RegExp special characters")
    (description "Escape RegExp special characters")
    (license license:expat)))

(define-public node-escape-string-regexp-4.0.0
  (package
    (name "node-escape-string-regexp")
    (version "4.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/escape-string-regexp/-/escape-string-regexp-4.0.0.tgz")
              (sha256
               (base32
                "06xi6f77ybg7cl673bawycpp6984h2ridzy2hl2kqylqlr6s2i2b"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "tsd" "xo")))))))
    (home-page "https://github.com/sindresorhus/escape-string-regexp#readme")
    (synopsis "Escape RegExp special characters")
    (description "Escape RegExp special characters")
    (license license:expat)))

(define-public node-escodegen-1.14.3
  (package
    (name "node-escodegen")
    (version "1.14.3")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/escodegen/-/escodegen-1.14.3.tgz")
              (sha256
               (base32
                "12rngj20cpcnayic21ky3rkmkldbcvrzxzsr9y6a9diaf9lgldzj"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("acorn" "bluebird"
                                             "bower-registry-client"
                                             "chai"
                                             "commonjs-everywhere"
                                             "gulp"
                                             "gulp-eslint"
                                             "gulp-mocha"
                                             "semver")))))))
    (inputs (list node-source-map-0.6.1 node-optionator-0.8.3
                  node-esprima-4.0.1 node-esutils-2.0.3 node-estraverse-4.3.0))
    (home-page "http://github.com/estools/escodegen")
    (synopsis "ECMAScript code generator")
    (description "ECMAScript code generator")
    (license license:bsd-2)))

(define-public node-esprima-4.0.1
  (package
    (name "node-esprima")
    (version "4.0.1")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/esprima/-/esprima-4.0.1.tgz")
              (sha256
               (base32
                "0x6cjgh4452wa28yz562b4c2dad78rn3fxfzqns9bk5ykh7938fq"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("codecov.io" "escomplex-js"
                                             "everything.js"
                                             "glob"
                                             "istanbul"
                                             "json-diff"
                                             "karma"
                                             "karma-chrome-launcher"
                                             "karma-detect-browsers"
                                             "karma-edge-launcher"
                                             "karma-firefox-launcher"
                                             "karma-ie-launcher"
                                             "karma-mocha"
                                             "karma-safari-launcher"
                                             "karma-safaritechpreview-launcher"
                                             "karma-sauce-launcher"
                                             "lodash"
                                             "mocha"
                                             "node-tick-processor"
                                             "regenerate"
                                             "temp"
                                             "tslint"
                                             "typescript"
                                             "typescript-formatter"
                                             "unicode-8.0.0"
                                             "webpack")))))))
    (home-page "http://esprima.org")
    (synopsis "ECMAScript parsing infrastructure for multipurpose analysis")
    (description "ECMAScript parsing infrastructure for multipurpose analysis")
    (license license:bsd-2)))

(define-public node-estraverse-4.3.0
  (package
    (name "node-estraverse")
    (version "4.3.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/estraverse/-/estraverse-4.3.0.tgz")
              (sha256
               (base32
                "1bpip5qvpq6f6prpn5dsxnqsaw3czx6hn8mps04v5vpqgla2n9kz"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("babel-preset-env"
                                             "babel-register"
                                             "chai"
                                             "espree"
                                             "gulp"
                                             "gulp-bump"
                                             "gulp-filter"
                                             "gulp-git"
                                             "gulp-tag-version"
                                             "jshint"
                                             "mocha")))))))
    (home-page "https://github.com/estools/estraverse")
    (synopsis "ECMAScript JS AST traversal functions")
    (description "ECMAScript JS AST traversal functions")
    (license license:bsd-2)))

(define-public node-esutils-2.0.3
  (package
    (name "node-esutils")
    (version "2.0.3")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/esutils/-/esutils-2.0.3.tgz")
              (sha256
               (base32
                "03v4y32k50mbxwv70prr7ghwg59vd5gyxsdsbdikqnj919rvvbf5"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("chai" "coffee-script" "jshint"
                                             "mocha" "regenerate"
                                             "unicode-9.0.0")))))))
    (home-page "https://github.com/estools/esutils")
    (synopsis "utility box for ECMAScript language tools")
    (description "utility box for ECMAScript language tools")
    (license license:bsd-2)))

(define-public node-etag-1.8.1
  (package
    (name "node-etag")
    (version "1.8.1")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/etag/-/etag-1.8.1.tgz")
              (sha256
               (base32
                "1bqgznlsrqcmxnhmnqkhwzcrqfaalxmfxzly1ikaplkkm5w6ragn"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("beautify-benchmark" "benchmark"
                                             "eslint"
                                             "eslint-config-standard"
                                             "eslint-plugin-import"
                                             "eslint-plugin-markdown"
                                             "eslint-plugin-node"
                                             "eslint-plugin-promise"
                                             "eslint-plugin-standard"
                                             "istanbul"
                                             "mocha"
                                             "safe-buffer"
                                             "seedrandom")))))))
    (home-page "https://github.com/jshttp/etag#readme")
    (synopsis "Create simple HTTP ETags")
    (description "Create simple HTTP ETags")
    (license license:expat)))

(define-public node-exec-sh-0.2.2
  (package
    (name "node-exec-sh")
    (version "0.2.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/exec-sh/-/exec-sh-0.2.2.tgz")
              (sha256
               (base32
                "1493miwgm1g0f76xzgn2bxg1zaqliih2d4lmfpzxwk4xaq6d2aiq"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("coveralls" "istanbul"
                                             "jsdoc"
                                             "jshint"
                                             "mocha"
                                             "sinon"
                                             "standard")))))))
    (inputs (list node-merge-1.2.1))
    (home-page "https://github.com/tsertkov/exec-sh#readme")
    (synopsis "Execute shell command forwarding all stdio.")
    (description "Execute shell command forwarding all stdio.")
    (license license:expat)))

(define-public node-expand-brackets-0.1.5
  (package
    (name "node-expand-brackets")
    (version "0.1.5")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/expand-brackets/-/expand-brackets-0.1.5.tgz")
              (sha256
               (base32
                "1fzgbcp2faxj7d6i41iybacbxaig8gpkx329dvcysic2yy2rn1c3"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp-format-md" "mocha" "should")))))))
    (inputs (list node-is-posix-bracket-0.1.1))
    (home-page "https://github.com/jonschlinkert/expand-brackets")
    (synopsis
     "Expand POSIX bracket expressions (character classes) in glob patterns.")
    (description
     "Expand POSIX bracket expressions (character classes) in glob patterns.")
    (license license:expat)))

(define-public node-expand-brackets-2.1.4
  (package
    (name "node-expand-brackets")
    (version "2.1.4")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/expand-brackets/-/expand-brackets-2.1.4.tgz")
              (sha256
               (base32
                "0csxpxfx1xkf2dp10vpifynkrx8csx7md7gysvhy5xr094hdr3nq"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("bash-match" "gulp-format-md"
                                             "helper-changelog"
                                             "minimatch"
                                             "mocha"
                                             "multimatch"
                                             "yargs-parser")))))))
    (inputs (list node-to-regex-3.0.2
                  node-snapdragon-0.8.2
                  node-regex-not-1.0.2
                  node-posix-character-classes-0.1.1
                  node-extend-shallow-2.0.1
                  node-define-property-0.2.5
                  node-debug-2.6.9))
    (home-page "https://github.com/jonschlinkert/expand-brackets")
    (synopsis
     "Expand POSIX bracket expressions (character classes) in glob patterns.")
    (description
     "Expand POSIX bracket expressions (character classes) in glob patterns.")
    (license license:expat)))

(define-public node-expand-range-1.8.2
  (package
    (name "node-expand-range")
    (version "1.8.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/expand-range/-/expand-range-1.8.2.tgz")
              (sha256
               (base32
                "1d78rg12y81sy87jfdr5i9a2dq7smg47w5s249f7bfgwlcn848zl"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("benchmarked" "brace-expansion"
                                             "glob" "gulp-format-md"
                                             "minimatch" "mocha")))))))
    (inputs (list node-fill-range-2.2.4))
    (home-page "https://github.com/jonschlinkert/expand-range")
    (synopsis
     "Fast, bash-like range expansion. Expand a range of numbers or letters, uppercase or lowercase. See the benchmarks. Used by micromatch.")
    (description
     "Fast, bash-like range expansion. Expand a range of numbers or letters, uppercase or lowercase. See the benchmarks. Used by micromatch.")
    (license license:expat)))

(define-public node-express-4.18.2
  (package
    (name "node-express")
    (version "4.18.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/express/-/express-4.18.2.tgz")
              (sha256
               (base32
                "0zn3j5gwif18gh0zqj70ag6zdkrarvdhjrr5crcvs9mkgycnjjla"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("after" "connect-redis"
                                             "cookie-parser"
                                             "cookie-session"
                                             "ejs"
                                             "eslint"
                                             "express-session"
                                             "hbs"
                                             "marked"
                                             "method-override"
                                             "mocha"
                                             "morgan"
                                             "multiparty"
                                             "nyc"
                                             "pbkdf2-password"
                                             "supertest"
                                             "vhost")))))))
    (inputs (list node-vary-1.1.2
                  node-utils-merge-1.0.1
                  node-type-is-1.6.18
                  node-statuses-2.0.1
                  node-setprototypeof-1.2.0
                  node-serve-static-1.15.0
                  node-send-0.18.0
                  node-safe-buffer-5.2.1
                  node-range-parser-1.2.1
                  node-qs-6.11.0
                  node-proxy-addr-2.0.7
                  node-path-to-regexp-0.1.7
                  node-parseurl-1.3.3
                  node-on-finished-2.4.1
                  node-methods-1.1.2
                  node-merge-descriptors-1.0.1
                  node-http-errors-2.0.0
                  node-fresh-0.5.2
                  node-finalhandler-1.2.0
                  node-etag-1.8.1
                  node-escape-html-1.0.3
                  node-encodeurl-1.0.2
                  node-depd-2.0.0
                  node-debug-2.6.9
                  node-cookie-signature-1.0.6
                  node-cookie-0.5.0
                  node-content-type-1.0.5
                  node-content-disposition-0.5.4
                  node-body-parser-1.20.1
                  node-array-flatten-1.1.1
                  node-accepts-1.3.8))
    (home-page "http://expressjs.com/")
    (synopsis "Fast, unopinionated, minimalist web framework")
    (description "Fast, unopinionated, minimalist web framework")
    (license license:expat)))

(define-public node-extend-3.0.2
  (package
    (name "node-extend")
    (version "3.0.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/extend/-/extend-3.0.2.tgz")
              (sha256
               (base32
                "1ckjrzapv4awrafybcvq3n5rcqm6ljswfdx97wibl355zaqd148x"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@ljharb/eslint-config" "covert"
                                             "eslint" "jscs" "tape")))))))
    (home-page "https://github.com/justmoon/node-extend#readme")
    (synopsis "Port of jQuery.extend for node.js and the browser")
    (description "Port of jQuery.extend for node.js and the browser")
    (license license:expat)))

(define-public node-extend-shallow-2.0.1
  (package
    (name "node-extend-shallow")
    (version "2.0.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/extend-shallow/-/extend-shallow-2.0.1.tgz")
              (sha256
               (base32
                "09baxpl8w1rw3qmqmp7w23kyqrxks1hhbvpac0j82gbsgimrlz0v"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("array-slice" "benchmarked"
                                             "chalk"
                                             "for-own"
                                             "glob"
                                             "is-plain-object"
                                             "kind-of"
                                             "minimist"
                                             "mocha"
                                             "should")))))))
    (inputs (list node-is-extendable-0.1.1))
    (home-page "https://github.com/jonschlinkert/extend-shallow")
    (synopsis
     "Extend an object with the properties of additional objects. node.js/javascript util.")
    (description
     "Extend an object with the properties of additional objects. node.js/javascript util.")
    (license license:expat)))

(define-public node-extend-shallow-3.0.2
  (package
    (name "node-extend-shallow")
    (version "3.0.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/extend-shallow/-/extend-shallow-3.0.2.tgz")
              (sha256
               (base32
                "02bickcbljfrxfix2rbvq5j2cdlwm0dqflxc2ky4l6jp97bcl6m0"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("array-slice" "benchmarked"
                                             "for-own"
                                             "gulp-format-md"
                                             "is-plain-object"
                                             "kind-of"
                                             "minimist"
                                             "mocha"
                                             "object-assign")))))))
    (inputs (list node-is-extendable-1.0.1 node-assign-symbols-1.0.0))
    (home-page "https://github.com/jonschlinkert/extend-shallow")
    (synopsis
     "Extend an object with the properties of additional objects. node.js/javascript util.")
    (description
     "Extend an object with the properties of additional objects. node.js/javascript util.")
    (license license:expat)))

(define-public node-external-editor-2.2.0
  (package
    (name "node-external-editor")
    (version "2.2.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/external-editor/-/external-editor-2.2.0.tgz")
              (sha256
               (base32
                "0l3g4am7l1li9aznhc82wfkan4qcq8lxm2y5lqfmi0vzmfj0jiii"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("chai" "coffee-script"
                                             "coffeelint" "mocha")))))))
    (inputs (list node-tmp-0.0.33 node-iconv-lite-0.4.24 node-chardet-0.4.2))
    (home-page "https://github.com/mrkmg/node-external-editor#readme")
    (synopsis
     "Edit a string with the users preferred text editor using $VISUAL or $ENVIRONMENT")
    (description
     "Edit a string with the users preferred text editor using $VISUAL or $ENVIRONMENT")
    (license license:expat)))

(define-public node-extglob-0.3.2
  (package
    (name "node-extglob")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/extglob/-/extglob-0.3.2.tgz")
              (sha256
               (base32
                "161asrmll909bda6hzz8ayfym5sp35yp45s25dcbfs3d9qcz0m98"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ansi-green" "micromatch"
                                             "minimatch"
                                             "minimist"
                                             "mocha"
                                             "should"
                                             "success-symbol")))))))
    (inputs (list node-is-extglob-1.0.0))
    (home-page "https://github.com/jonschlinkert/extglob")
    (synopsis
     "Convert extended globs to regex-compatible strings. Add (almost) the expressive power of regular expressions to glob patterns.")
    (description
     "Convert extended globs to regex-compatible strings. Add (almost) the expressive power of regular expressions to glob patterns.")
    (license license:expat)))

(define-public node-extglob-2.0.4
  (package
    (name "node-extglob")
    (version "2.0.4")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/extglob/-/extglob-2.0.4.tgz")
              (sha256
               (base32
                "1wza438hvcr83b28zic65h8jq8k0p0v9iw9b1lfk1zhb5xrkp8sy"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("bash-match" "for-own"
                                             "gulp"
                                             "gulp-eslint"
                                             "gulp-format-md"
                                             "gulp-istanbul"
                                             "gulp-mocha"
                                             "gulp-unused"
                                             "helper-changelog"
                                             "is-windows"
                                             "micromatch"
                                             "minimatch"
                                             "minimist"
                                             "mocha"
                                             "multimatch")))))))
    (inputs (list node-to-regex-3.0.2
                  node-snapdragon-0.8.2
                  node-regex-not-1.0.2
                  node-fragment-cache-0.2.1
                  node-extend-shallow-2.0.1
                  node-expand-brackets-2.1.4
                  node-define-property-1.0.0
                  node-array-unique-0.3.2))
    (home-page "https://github.com/micromatch/extglob")
    (synopsis
     "Extended glob support for JavaScript. Adds (almost) the expressive power of regular expressions to glob patterns.")
    (description
     "Extended glob support for JavaScript. Adds (almost) the expressive power of regular expressions to glob patterns.")
    (license license:expat)))

(define-public node-extsprintf-1.3.0
  (package
    (name "node-extsprintf")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/extsprintf/-/extsprintf-1.3.0.tgz")
              (sha256
               (base32
                "0i6hmr7mkg76rgrxs7f0xny48kha2xi03wj43mfik77m0lk3k6yg"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://github.com/davepacheco/node-extsprintf")
    (synopsis "extended POSIX-style sprintf")
    (description "extended POSIX-style sprintf")
    (license license:expat)))

(define-public node-extsprintf-1.4.1
  (package
    (name "node-extsprintf")
    (version "1.4.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/extsprintf/-/extsprintf-1.4.1.tgz")
              (sha256
               (base32
                "1k3s5pdmrc0j52xzh9bbna3fsyplg1626k898laaq4ibr3axcks5"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://github.com/davepacheco/node-extsprintf#readme")
    (synopsis "extended POSIX-style sprintf")
    (description "extended POSIX-style sprintf")
    (license license:expat)))

(define-public node-eyes-0.1.8
  (package
    (name "node-eyes")
    (version "0.1.8")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/eyes/-/eyes-0.1.8.tgz")
              (sha256
               (base32
                "0d9pjsk4gmjms6drymbq92yfyxisxgbj5ihil8wzn9lr5j7xp9jg"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://www.npmjs.com/package/node-eyes")
    (synopsis "a customizable value inspector")
    (description "a customizable value inspector")
    (license #f)))

(define-public node-fast-deep-equal-3.1.3
  (package
    (name "node-fast-deep-equal")
    (version "3.1.3")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/fast-deep-equal/-/fast-deep-equal-3.1.3.tgz")
              (sha256
               (base32
                "13vvwib6za4zh7054n3fg86y127ig3jb0djqz31qsqr71yca06dh"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("coveralls" "dot"
                                             "eslint"
                                             "mocha"
                                             "nyc"
                                             "pre-commit"
                                             "react"
                                             "react-test-renderer"
                                             "sinon"
                                             "typescript")))))))
    (home-page "https://github.com/epoberezkin/fast-deep-equal#readme")
    (synopsis "Fast deep equal")
    (description "Fast deep equal")
    (license license:expat)))

(define-public node-fast-json-stable-stringify-2.1.0
  (package
    (name "node-fast-json-stable-stringify")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/fast-json-stable-stringify/-/fast-json-stable-stringify-2.1.0.tgz")
              (sha256
               (base32
                "11qnzlan5yd2hg9nqi9hdv48bq6kwvw9pxsxir22n2iyqhighb8y"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("benchmark" "coveralls"
                                             "eslint"
                                             "fast-stable-stringify"
                                             "faster-stable-stringify"
                                             "json-stable-stringify"
                                             "nyc"
                                             "pre-commit"
                                             "tape")))))))
    (home-page "https://github.com/epoberezkin/fast-json-stable-stringify")
    (synopsis
     "deterministic `JSON.stringify()` - a faster version of substack's json-stable-strigify without jsonify")
    (description
     "deterministic `JSON.stringify()` - a faster version of substack's json-stable-strigify without jsonify")
    (license license:expat)))

(define-public node-fast-levenshtein-2.0.6
  (package
    (name "node-fast-levenshtein")
    (version "2.0.6")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/fast-levenshtein/-/fast-levenshtein-2.0.6.tgz")
              (sha256
               (base32
                "0g5zgdlp38dli94qbbm8vhvmj90fh48sxpggfn2083wbdcq50jxv"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("chai" "grunt"
                                             "grunt-benchmark"
                                             "grunt-cli"
                                             "grunt-contrib-jshint"
                                             "grunt-contrib-uglify"
                                             "grunt-mocha-test"
                                             "grunt-npm-install"
                                             "load-grunt-tasks"
                                             "lodash"
                                             "mocha")))))))
    (home-page "https://github.com/hiddentao/fast-levenshtein#readme")
    (synopsis
     "Efficient implementation of Levenshtein algorithm  with locale-specific collator support.")
    (description
     "Efficient implementation of Levenshtein algorithm  with locale-specific collator support.")
    (license license:expat)))

(define-public node-figures-2.0.0
  (package
    (name "node-figures")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/figures/-/figures-2.0.0.tgz")
              (sha256
               (base32
                "1xyyajzjnv409rhscns6s5cvg519awj445x7cpx7v8g31pqirk5s"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "markdown-table"
                                             "require-uncached" "xo")))))))
    (inputs (list node-escape-string-regexp-1.0.5))
    (home-page "https://github.com/sindresorhus/figures#readme")
    (synopsis "Unicode symbols with Windows CMD fallbacks")
    (description "Unicode symbols with Windows CMD fallbacks")
    (license license:expat)))

(define-public node-file-match-1.0.2
  (package
    (name "node-file-match")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/file-match/-/file-match-1.0.2.tgz")
              (sha256
               (base32
                "07xax9x1x8zxkss8i0hmcqkpsz0fd53pl68xvg8x7d6d1s61nks1"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("grunt" "grunt-contrib-jshint"
                                             "mocha")))))))
    (inputs (list node-utils-extend-1.0.8))
    (home-page "https://github.com/douzi8/file-match")
    (synopsis
     "Match filepath is validated, or exclude filepath that don't need.")
    (description
     "Match filepath is validated, or exclude filepath that don't need.")
    (license license:isc)))

(define-public node-file-system-2.2.2
  (package
    (name "node-file-system")
    (version "2.2.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/file-system/-/file-system-2.2.2.tgz")
              (sha256
               (base32
                "08d1xdfkhx4j1gv2ipwzdxkyv0wpin0317k5vmllhdb3k3v3wdg6"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("grunt" "grunt-contrib-jshint"
                                             "mocha")))))))
    (inputs (list node-utils-extend-1.0.8 node-file-match-1.0.2))
    (home-page "https://github.com/douzi8/file-system")
    (synopsis "Strengthen the ability of file system")
    (description "Strengthen the ability of file system")
    (license license:isc)))

(define-public node-filename-regex-2.0.1
  (package
    (name "node-filename-regex")
    (version "2.0.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/filename-regex/-/filename-regex-2.0.1.tgz")
              (sha256
               (base32
                "0swl7rv3vxrrmpjfr9i5fdbghif1s2yj9mdxmx6c27mg2kx88ya2"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp-format-md")))))))
    (home-page "https://github.com/regexhq/filename-regex")
    (synopsis
     "Regular expression for matching file names, with or without extension.")
    (description
     "Regular expression for matching file names, with or without extension.")
    (license license:expat)))

(define-public node-fill-range-2.2.4
  (package
    (name "node-fill-range")
    (version "2.2.4")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/fill-range/-/fill-range-2.2.4.tgz")
              (sha256
               (base32
                "183q5x2pvh9xwbpm1lrhazqbm5pg400nxi6rirpks4980fiph7fi"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("benchmarked" "chalk"
                                             "gulp-format-md" "should")))))))
    (inputs (list node-repeat-string-1.6.1 node-repeat-element-1.1.4
                  node-randomatic-3.1.1 node-isobject-2.1.0
                  node-is-number-2.1.0))
    (home-page "https://github.com/jonschlinkert/fill-range")
    (synopsis
     "Fill in a range of numbers or letters, optionally passing an increment or multiplier to use.")
    (description
     "Fill in a range of numbers or letters, optionally passing an increment or multiplier to use.")
    (license license:expat)))

(define-public node-fill-range-4.0.0
  (package
    (name "node-fill-range")
    (version "4.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/fill-range/-/fill-range-4.0.0.tgz")
              (sha256
               (base32
                "14kaakn1yhkfsclqds0pg1g87aibi8nkrwn0axvfasj495hv2wzx"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ansi-cyan" "benchmarked"
                                             "gulp-format-md" "minimist"
                                             "mocha")))))))
    (inputs (list node-to-regex-range-2.1.1 node-repeat-string-1.6.1
                  node-is-number-3.0.0 node-extend-shallow-2.0.1))
    (home-page "https://github.com/jonschlinkert/fill-range")
    (synopsis
     "Fill in a range of numbers or letters, optionally passing an increment or `step` to use, or create a regex-compatible range with `options.toRegex`")
    (description
     "Fill in a range of numbers or letters, optionally passing an increment or `step` to use, or create a regex-compatible range with `options.toRegex`")
    (license license:expat)))

(define-public node-finalhandler-1.2.0
  (package
    (name "node-finalhandler")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/finalhandler/-/finalhandler-1.2.0.tgz")
              (sha256
               (base32
                "0dcjp6zdn5pyv1sjvz3qmr9wkwjm2yd8bfd3ri3nv7kjd8i4ngjq"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("eslint" "eslint-config-standard"
                                             "eslint-plugin-import"
                                             "eslint-plugin-markdown"
                                             "eslint-plugin-node"
                                             "eslint-plugin-promise"
                                             "eslint-plugin-standard"
                                             "mocha"
                                             "nyc"
                                             "readable-stream"
                                             "safe-buffer"
                                             "supertest")))))))
    (inputs (list node-unpipe-1.0.0
                  node-statuses-2.0.1
                  node-parseurl-1.3.3
                  node-on-finished-2.4.1
                  node-escape-html-1.0.3
                  node-encodeurl-1.0.2
                  node-debug-2.6.9))
    (home-page "https://github.com/pillarjs/finalhandler#readme")
    (synopsis "Node.js final http responder")
    (description "Node.js final http responder")
    (license license:expat)))

(define-public node-find-root-1.1.0
  (package
    (name "node-find-root")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/find-root/-/find-root-1.1.0.tgz")
              (sha256
               (base32
                "1m0lf8903ffmlzxhy8nss7lpkyjyhf8samyy0himcpjxknpkrrq6"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("chai" "mocha" "moquire"
                                             "standard")))))))
    (home-page "https://github.com/js-n/find-root#readme")
    (synopsis "find the closest package.json")
    (description "find the closest package.json")
    (license license:expat)))

(define-public node-fliplog-0.3.13
  (package
    (name "node-fliplog")
    (version "0.3.13")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/fliplog/-/fliplog-0.3.13.tgz")
              (sha256
               (base32
                "16c8f09313gdm9m2jwlakkrb26zr7n8cb2vif3yv25jly1wbdi72"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (inputs (list node-chain-able-1.0.1))
    (home-page "https://github.com/fliphub/fliplog")
    (synopsis
     "fluent logging with verbose insight, colors, tables, emoji, filtering, spinners, progress bars, timestamps, capturing, stack traces, clearing, & presets")
    (description
     "fluent logging with verbose insight, colors, tables, emoji, filtering, spinners, progress bars, timestamps, capturing, stack traces, clearing, & presets")
    (license license:expat)))

(define-public node-for-in-1.0.2
  (package
    (name "node-for-in")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/for-in/-/for-in-1.0.2.tgz")
              (sha256
               (base32
                "0pm8dx9gvp9p91my9fqivajq7yhnxmn8scl391pgcv6d8h6s6zaf"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp-format-md" "mocha")))))))
    (home-page "https://github.com/jonschlinkert/for-in")
    (synopsis
     "Iterate over the own and inherited enumerable properties of an object, and return an object with properties that evaluate to true from the callback. Exit early by returning `false`. JavaScript/Node.js")
    (description
     "Iterate over the own and inherited enumerable properties of an object, and return an object with properties that evaluate to true from the callback. Exit early by returning `false`. JavaScript/Node.js")
    (license license:expat)))

(define-public node-for-own-0.1.5
  (package
    (name "node-for-own")
    (version "0.1.5")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/for-own/-/for-own-0.1.5.tgz")
              (sha256
               (base32
                "0bxjf90a7n1r0l5vm93zrz00qzip9s4g4c3pzdg3grn2fw53byph"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp-format-md" "mocha")))))))
    (inputs (list node-for-in-1.0.2))
    (home-page "https://github.com/jonschlinkert/for-own")
    (synopsis
     "Iterate over the own enumerable properties of an object, and return an object with properties that evaluate to true from the callback. Exit early by returning `false`. JavaScript/Node.js.")
    (description
     "Iterate over the own enumerable properties of an object, and return an object with properties that evaluate to true from the callback. Exit early by returning `false`. JavaScript/Node.js.")
    (license license:expat)))

(define-public node-forever-agent-0.6.1
  (package
    (name "node-forever-agent")
    (version "0.6.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/forever-agent/-/forever-agent-0.6.1.tgz")
              (sha256
               (base32
                "1i86r2ip6ryrnpg3v7pf0ywddhsdlr809xycd3zm9gq7zphn5a7c"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://github.com/mikeal/forever-agent")
    (synopsis
     "HTTP Agent that keeps socket connections alive between keep-alive requests. Formerly part of mikeal/request, now a standalone module.")
    (description
     "HTTP Agent that keeps socket connections alive between keep-alive requests. Formerly part of mikeal/request, now a standalone module.")
    (license license:asl2.0)))

(define-public node-form-data-2.3.3
  (package
    (name "node-form-data")
    (version "2.3.3")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/form-data/-/form-data-2.3.3.tgz")
              (sha256
               (base32
                "1j1ka178syqqaycr1m3vqahbb3bi7qsks0mp0iqbd6y7yj1wz7p3"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("browserify"
                                             "browserify-istanbul"
                                             "coveralls"
                                             "cross-spawn"
                                             "eslint"
                                             "fake"
                                             "far"
                                             "formidable"
                                             "in-publish"
                                             "is-node-modern"
                                             "istanbul"
                                             "obake"
                                             "phantomjs-prebuilt"
                                             "pkgfiles"
                                             "pre-commit"
                                             "request"
                                             "rimraf"
                                             "tape")))))))
    (inputs (list node-mime-types-2.1.35 node-combined-stream-1.0.8
                  node-asynckit-0.4.0))
    (home-page "https://github.com/form-data/form-data#readme")
    (synopsis
     "A library to create readable \"multipart/form-data\" streams. Can be used to submit forms and file uploads to other web applications.")
    (description
     "A library to create readable \"multipart/form-data\" streams. Can be used to submit forms and file uploads to other web applications.")
    (license license:expat)))

(define-public node-forwarded-0.2.0
  (package
    (name "node-forwarded")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/forwarded/-/forwarded-0.2.0.tgz")
              (sha256
               (base32
                "168w8dhfqp12llh2w802dm3z1fcarsacsksyvdccnpxqbzlmsnlv"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("beautify-benchmark" "benchmark"
                                             "deep-equal"
                                             "eslint"
                                             "eslint-config-standard"
                                             "eslint-plugin-import"
                                             "eslint-plugin-node"
                                             "eslint-plugin-promise"
                                             "eslint-plugin-standard"
                                             "mocha"
                                             "nyc")))))))
    (home-page "https://github.com/jshttp/forwarded#readme")
    (synopsis "Parse HTTP X-Forwarded-For header")
    (description "Parse HTTP X-Forwarded-For header")
    (license license:expat)))

(define-public node-fragment-cache-0.2.1
  (package
    (name "node-fragment-cache")
    (version "0.2.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/fragment-cache/-/fragment-cache-0.2.1.tgz")
              (sha256
               (base32
                "114jfm5qpvz52127hf4ny4vz1qs7a597hql5lj0g2ncixn8sqg76"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp" "gulp-eslint"
                                             "gulp-format-md" "gulp-istanbul"
                                             "gulp-mocha" "mocha")))))))
    (inputs (list node-map-cache-0.2.2))
    (home-page "https://github.com/jonschlinkert/fragment-cache")
    (synopsis "A cache for managing namespaced sub-caches")
    (description "A cache for managing namespaced sub-caches")
    (license license:expat)))

(define-public node-fresh-0.5.2
  (package
    (name "node-fresh")
    (version "0.5.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/fresh/-/fresh-0.5.2.tgz")
              (sha256
               (base32
                "0k44badcxkwy202kz404w078l660f65jaijg473xv38ay3wpdri5"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("beautify-benchmark" "benchmark"
                                             "eslint"
                                             "eslint-config-standard"
                                             "eslint-plugin-import"
                                             "eslint-plugin-markdown"
                                             "eslint-plugin-node"
                                             "eslint-plugin-promise"
                                             "eslint-plugin-standard"
                                             "istanbul"
                                             "mocha")))))))
    (home-page "https://github.com/jshttp/fresh#readme")
    (synopsis "HTTP response freshness testing")
    (description "HTTP response freshness testing")
    (license license:expat)))

(define-public node-fs-extra-7.0.1
  (package
    (name "node-fs-extra")
    (version "7.0.1")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/fs-extra/-/fs-extra-7.0.1.tgz")
              (sha256
               (base32
                "1f08bng4dgkdrwhd977f4xfch9419b7fbwvwwn1qpz3gy4zgja4b"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("coveralls" "istanbul"
                                             "klaw"
                                             "klaw-sync"
                                             "minimist"
                                             "mocha"
                                             "proxyquire"
                                             "read-dir-files"
                                             "rimraf"
                                             "secure-random"
                                             "semver"
                                             "standard"
                                             "standard-markdown")))))))
    (inputs (list node-universalify-0.1.2 node-jsonfile-4.0.0
                  node-graceful-fs-4.2.11))
    (home-page "https://github.com/jprichardson/node-fs-extra")
    (synopsis
     "fs-extra contains methods that aren't included in the vanilla Node.js fs package. Such as mkdir -p, cp -r, and rm -rf.")
    (description
     "fs-extra contains methods that aren't included in the vanilla Node.js fs package. Such as mkdir -p, cp -r, and rm -rf.")
    (license license:expat)))

(define-public node-fs-minipass-2.1.0
  (package
    (name "node-fs-minipass")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/fs-minipass/-/fs-minipass-2.1.0.tgz")
              (sha256
               (base32
                "029kdjb6h8gp0gfx7rx6yzwbv7pnd7i119gn563ynv0dqx02p5gx"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("mutate-fs" "tap")))))))
    (inputs (list node-minipass-3.3.6))
    (home-page "https://github.com/npm/fs-minipass#readme")
    (synopsis "fs read and write streams based on minipass")
    (description "fs read and write streams based on minipass")
    (license license:isc)))

(define-public node-fs-realpath-1.0.0
  (package
    (name "node-fs-realpath")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/fs.realpath/-/fs.realpath-1.0.0.tgz")
              (sha256
               (base32
                "174g5vay9jnd7h5q8hfdw6dnmwl1gdpn4a8sz0ysanhj2f3wp04y"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://github.com/isaacs/fs.realpath#readme")
    (synopsis
     "Use node's fs.realpath, but fall back to the JS implementation if the native one fails")
    (description
     "Use node's fs.realpath, but fall back to the JS implementation if the native one fails")
    (license license:isc)))

(define-public node-function-bind-1.1.1
  (package
    (name "node-function-bind")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/function-bind/-/function-bind-1.1.1.tgz")
              (sha256
               (base32
                "10p0s9ypggwmazik4azdhywjnnayagnjxk10cjzsrhxlk1y2wm9d"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@ljharb/eslint-config" "covert"
                                             "eslint" "jscs" "tape")))))))
    (home-page "https://github.com/Raynos/function-bind")
    (synopsis "Implementation of Function.prototype.bind")
    (description "Implementation of Function.prototype.bind")
    (license license:expat)))

(define-public node-fuse-box-3.7.1
  (package
    (name "node-fuse-box")
    (version "3.7.1")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/fuse-box/-/fuse-box-3.7.1.tgz")
              (sha256
               (base32
                "0rn9i8a9rxbhrzaf8w6fg2s8kjgps45vl58f2q9ih8bvag0ixj7h"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@babel/core" "@babel/preset-env"
                                             "@types/node"
                                             "@types/styled-components"
                                             "babel-core"
                                             "babel-generator"
                                             "babel-plugin-transform-es2015-modules-commonjs"
                                             "babel-preset-latest"
                                             "babylon"
                                             "buble"
                                             "cheerio"
                                             "coffee-script"
                                             "commitizen"
                                             "consolidate"
                                             "conventional-changelog-cli"
                                             "cross-env"
                                             "cz-conventional-changelog"
                                             "fuse-box"
                                             "fuse-box-test-using-old-version"
                                             "fuse-box-testcase1"
                                             "fuse-box-testcase3"
                                             "fuse-test-runner"
                                             "gulp"
                                             "gulp-bump"
                                             "gulp-clean"
                                             "gulp-concat"
                                             "gulp-header"
                                             "gulp-rename"
                                             "gulp-replace"
                                             "gulp-sourcemaps"
                                             "gulp-typescript"
                                             "gulp-uglify"
                                             "gulp-wrap"
                                             "homedir"
                                             "husky"
                                             "jsdom"
                                             "less"
                                             "marked"
                                             "node-sass"
                                             "npm-run-all"
                                             "postcss-selector-parser"
                                             "prettier"
                                             "pug"
                                             "react"
                                             "react-dom"
                                             "reflect-metadata"
                                             "run-sequence"
                                             "should"
                                             "styled-components"
                                             "stylus"
                                             "terser"
                                             "typescript"
                                             "typescript-plugin-styled-components"
                                             "uglify-es"
                                             "uglify-js"
                                             "validate-commit-msg"
                                             "vue"
                                             "vue-class-component"
                                             "vue-hot-reload-api"
                                             "vue-server-renderer"
                                             "vue-template-compiler"
                                             "vue-template-es2015-compiler"
                                             "wires-reactive")))))))
    (inputs (list node-ws-1.1.5
                  node-watch-1.0.2
                  node-tslib-1.14.1
                  node-sourcemap-blender-1.0.5
                  node-stream-browserify-2.0.2
                  node-source-map-0.7.4
                  node-shorthash-0.0.2
                  node-request-2.88.2
                  node-regexpu-core-4.8.0
                  node-realm-utils-1.0.9
                  node-prettysize-0.0.3
                  node-pretty-time-0.2.0
                  node-postcss-6.0.23
                  node-mustache-2.3.2
                  node-lego-api-1.0.8
                  node-inquirer-3.3.0
                  node-ieee754-1.2.1
                  node-glob-7.2.3
                  node-getopts-2.3.0
                  node-fuse-concat-with-sourcemaps-1.0.5
                  node-fs-extra-7.0.1
                  node-fliplog-0.3.13
                  node-express-4.18.2
                  node-escodegen-1.14.3
                  node-clean-css-4.2.4
                  node-chokidar-1.7.0
                  node-bowser-2.11.0
                  node-base64-js-1.5.1
                  node-base64-img-1.0.4
                  node-app-root-path-2.2.1
                  node-ansi-0.3.1
                  node-acorn-jsx-4.1.1
                  node-acorn-5.7.4))
    (home-page "https://github.com/fuse-box/fuse-box#readme")
    (synopsis "Fuse-Box a bundler that does it right")
    (description "Fuse-Box a bundler that does it right")
    (license license:expat)))

(define-public node-fuse-concat-with-sourcemaps-1.0.5
  (package
    (name "node-fuse-concat-with-sourcemaps")
    (version "1.0.5")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/fuse-concat-with-sourcemaps/-/fuse-concat-with-sourcemaps-1.0.5.tgz")
              (sha256
               (base32
                "1sam6dbh6zv6hgpc9h4vyv15myl28y1n5pzmcgrxzk0jb16chx3k"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("jshint" "tape" "istanbul"
                                             "faucet" "coveralls")))))))
    (inputs (list node-source-map-0.6.1))
    (home-page "http://github.com/floridoo/concat-with-sourcemaps")
    (synopsis
     "Concatenate file contents with a custom separator and generate a source map")
    (description
     "Concatenate file contents with a custom separator and generate a source map")
    (license license:isc)))

(define-public node-fuse-js-6.6.2
  (package
    (name "node-fuse-js")
    (version "6.6.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/fuse.js/-/fuse.js-6.6.2.tgz")
              (sha256
               (base32
                "0hba84b6s1xg3k81lpa3sf73wylb9q36b6bv2vz24lnkqi2xpc26"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@babel/cli" "@babel/core"
                                             "@babel/eslint-parser"
                                             "@babel/plugin-proposal-object-rest-spread"
                                             "@babel/preset-env"
                                             "@babel/preset-typescript"
                                             "@commitlint/cli"
                                             "@commitlint/config-conventional"
                                             "@rollup/plugin-babel"
                                             "@rollup/plugin-node-resolve"
                                             "@rollup/plugin-replace"
                                             "@types/jest"
                                             "@vuepress/plugin-google-analytics"
                                             "@vuepress/plugin-register-components"
                                             "babel-loader"
                                             "codemirror"
                                             "eslint"
                                             "eslint-config-prettier"
                                             "eslint-plugin-vue"
                                             "faker"
                                             "husky"
                                             "jest"
                                             "prettier"
                                             "replace-in-file"
                                             "rimraf"
                                             "rollup"
                                             "rollup-plugin-copy"
                                             "standard-version"
                                             "terser-webpack-plugin"
                                             "typescript"
                                             "vue-codemirror"
                                             "vue-eslint-parser"
                                             "vuepress"
                                             "vuepress-plugin-element-tabs"
                                             "vuepress-plugin-google-adsense"
                                             "vuepress-plugin-smooth-scroll"
                                             "vuepress-plugin-social-share"
                                             "webpack"
                                             "webpack-cli")))))))
    (home-page "http://fusejs.io")
    (synopsis "Lightweight fuzzy-search")
    (description "Lightweight fuzzy-search")
    (license license:asl2.0)))

(define-public node-gauge-3.0.2
  (package
    (name "node-gauge")
    (version "3.0.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/gauge/-/gauge-3.0.2.tgz")
              (sha256
               (base32
                "0mx3v0ar34dh0p88dm9xfh19f3jjx40139q58z5ibnb6cnpywxw8"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("readable-stream"
                                             "require-inject" "standard" "tap"
                                             "through2")))))))
    (inputs (list node-wide-align-1.1.5
                  node-strip-ansi-6.0.1
                  node-string-width-4.2.3
                  node-signal-exit-3.0.7
                  node-object-assign-4.1.1
                  node-has-unicode-2.0.1
                  node-console-control-strings-1.1.0
                  node-color-support-1.1.3
                  node-aproba-2.0.0))
    (home-page "https://github.com/npm/gauge")
    (synopsis "A terminal based horizontal guage")
    (description "A terminal based horizontal guage")
    (license license:isc)))

(define-public node-get-caller-file-2.0.5
  (package
    (name "node-get-caller-file")
    (version "2.0.5")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/get-caller-file/-/get-caller-file-2.0.5.tgz")
              (sha256
               (base32
                "0pwk4r8iyyq2j0xpxavdm3ldb4h4k4s4xb74m8dlrzs9374f24vv"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:modules ((guix build node-build-system)
                  (srfi srfi-1)
                  (ice-9 match)
                  (guix build utils))
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-before 'configure 'avoid-prepare-scripts
                    (lambda _
                      ;; We need to remove the prepare script from "package.json", as
                      ;; it would try to use the build environment and would block the
                      ;; automatic building by other packages making use of node-acorn.
                      ;; TODO: Add utility function
                      (with-atomic-json-file-replacement "package.json"
                                                         (match-lambda
                                                           (((quote @) . pkg-meta-alist)
                                                            (cons '@
                                                                  (map (match-lambda
                                                                         (("scripts" @ . scripts-alist) `
                                                                          ("scripts"
                                                                           @
                                                                           ,@(filter (match-lambda
                                                                                       
                                                                                       (("prepare" . _)
                                                                                        #f)
                                                                                       
                                                                                       (_
                                                                                        #t))
                                                                              scripts-alist)))
                                                                         (other
                                                                          other))
                                                                   pkg-meta-alist)))))))
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@types/chai"
                                             "@types/ensure-posix-path"
                                             "@types/mocha"
                                             "@types/node"
                                             "chai"
                                             "ensure-posix-path"
                                             "mocha"
                                             "typescript")))))))
    (home-page "https://github.com/stefanpenner/get-caller-file#readme")
    (synopsis
     "[![Build Status](https://travis-ci.org/stefanpenner/get-caller-file.svg?branch=master)](https://travis-ci.org/stefanpenner/get-caller-file) [![Build status](https://ci.appveyor.com/api/projects/status/ol2q94g1932cy14a/branch/master?svg=true)](https://ci.a")
    (description
     "[![Build Status](https://travis-ci.org/stefanpenner/get-caller-file.svg?branch=master)](https://travis-ci.org/stefanpenner/get-caller-file) [![Build status](https://ci.appveyor.com/api/projects/status/ol2q94g1932cy14a/branch/master?svg=true)](https://ci.a")
    (license license:isc)))

(define-public node-get-intrinsic-1.2.1
  (package
    (name "node-get-intrinsic")
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/get-intrinsic/-/get-intrinsic-1.2.1.tgz")
              (sha256
               (base32
                "0pffwzq9hhajnpv2zng5vix5qnxry41nk94m2wbmwgw5v7wsmy0c"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@ljharb/eslint-config" "aud"
                                             "auto-changelog"
                                             "call-bind"
                                             "es-abstract"
                                             "es-value-fixtures"
                                             "eslint"
                                             "evalmd"
                                             "for-each"
                                             "gopd"
                                             "make-async-function"
                                             "make-async-generator-function"
                                             "make-generator-function"
                                             "mock-property"
                                             "npmignore"
                                             "nyc"
                                             "object-inspect"
                                             "safe-publish-latest"
                                             "tape")))))))
    (inputs (list node-has-symbols-1.0.3 node-has-proto-1.0.1 node-has-1.0.3
                  node-function-bind-1.1.1))
    (home-page "https://github.com/ljharb/get-intrinsic#readme")
    (synopsis
     "Get and robustly cache all JS language-level intrinsics at first require time")
    (description
     "Get and robustly cache all JS language-level intrinsics at first require time")
    (license license:expat)))

(define-public node-get-value-2.0.6
  (package
    (name "node-get-value")
    (version "2.0.6")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/get-value/-/get-value-2.0.6.tgz")
              (sha256
               (base32
                "057wz0ya7zlgz59m08zbaasvzfdxjzgd6bzphd2xwsydhjwnw02l"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ansi-bold" "arr-reduce"
                                             "benchmarked"
                                             "dot-prop"
                                             "getobject"
                                             "gulp"
                                             "gulp-eslint"
                                             "gulp-format-md"
                                             "gulp-istanbul"
                                             "gulp-mocha"
                                             "isobject"
                                             "matched"
                                             "minimist")))))))
    (home-page "https://github.com/jonschlinkert/get-value")
    (synopsis
     "Use property paths (`a.b.c`) to get a nested value from an object.")
    (description
     "Use property paths (`a.b.c`) to get a nested value from an object.")
    (license license:expat)))

(define-public node-getopts-2.3.0
  (package
    (name "node-getopts")
    (version "2.3.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/getopts/-/getopts-2.3.0.tgz")
              (sha256
               (base32
                "0qfb7pyqvg5by55xnznvnbliywgqd4hh6vyaaifn2s1d3as7qnw5"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:modules ((guix build node-build-system)
                  (srfi srfi-1)
                  (ice-9 match)
                  (guix build utils))
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-before 'configure 'avoid-prepare-scripts
                    (lambda _
                      ;; We need to remove the prepare script from "package.json", as
                      ;; it would try to use the build environment and would block the
                      ;; automatic building by other packages making use of node-acorn.
                      ;; TODO: Add utility function
                      (with-atomic-json-file-replacement "package.json"
                                                         (match-lambda
                                                           (((quote @) . pkg-meta-alist)
                                                            (cons '@
                                                                  (map (match-lambda
                                                                         (("scripts" @ . scripts-alist) `
                                                                          ("scripts"
                                                                           @
                                                                           ,@(filter (match-lambda
                                                                                       
                                                                                       (("prepare" . _)
                                                                                        #f)
                                                                                       
                                                                                       (_
                                                                                        #t))
                                                                              scripts-alist)))
                                                                         (other
                                                                          other))
                                                                   pkg-meta-alist)))))))
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("c8" "twist")))))))
    (home-page "https://github.com/jorgebucaran/getopts#readme")
    (synopsis "Parse CLI arguments.")
    (description "Parse CLI arguments.")
    (license license:expat)))

(define-public node-getpass-0.1.7
  (package
    (name "node-getpass")
    (version "0.1.7")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/getpass/-/getpass-0.1.7.tgz")
              (sha256
               (base32
                "0ifl7rdzhkbwzb2pmi6mxvv92qd2ihbfbfkipw9nqvbn22x140wg"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (inputs (list node-assert-plus-1.0.0))
    (home-page "https://github.com/arekinath/node-getpass#readme")
    (synopsis "getpass for node.js")
    (description "getpass for node.js")
    (license license:expat)))

(define-public node-glob-7.2.3
  (package
    (name "node-glob")
    (version "7.2.3")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/glob/-/glob-7.2.3.tgz")
              (sha256
               (base32
                "10a336nxv867xkjs3ipgbharwdzp5lnz7wr8viawn1lc66qqx8zh"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("memfs" "mkdirp" "rimraf" "tap"
                                             "tick")))))))
    (inputs (list node-path-is-absolute-1.0.1
                  node-once-1.4.0
                  node-minimatch-3.1.2
                  node-inherits-2.0.4
                  node-inflight-1.0.6
                  node-fs-realpath-1.0.0))
    (home-page "https://github.com/isaacs/node-glob#readme")
    (synopsis "a little globber")
    (description "a little globber")
    (license license:isc)))

(define-public node-glob-base-0.3.0
  (package
    (name "node-glob-base")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/glob-base/-/glob-base-0.3.0.tgz")
              (sha256
               (base32
                "19d8xlkbibp313hidn9ldliqx9n7qlscf7pgxwi5ld1rak5r7an7"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("mocha" "should")))))))
    (inputs (list node-is-glob-2.0.1 node-glob-parent-2.0.0))
    (home-page "https://github.com/jonschlinkert/glob-base")
    (synopsis
     "Returns an object with the (non-glob) base path and the actual pattern.")
    (description
     "Returns an object with the (non-glob) base path and the actual pattern.")
    (license license:expat)))

(define-public node-glob-parent-2.0.0
  (package
    (name "node-glob-parent")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/glob-parent/-/glob-parent-2.0.0.tgz")
              (sha256
               (base32
                "0kc5f3hd4n5aj58gvaslcfqnadx3zwk1qwmilgggqf5jv4wc0cb3"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("coveralls" "istanbul" "mocha")))))))
    (inputs (list node-is-glob-2.0.1))
    (home-page "https://github.com/es128/glob-parent")
    (synopsis "Strips glob magic from a string to provide the parent path")
    (description "Strips glob magic from a string to provide the parent path")
    (license license:isc)))

(define-public node-good-listener-1.2.2
  (package
    (name "node-good-listener")
    (version "1.2.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/good-listener/-/good-listener-1.2.2.tgz")
              (sha256
               (base32
                "0xbzaxs3xvmh1sf3qbvd0slp82inlb0gz5fx60vkj94gn31l8s4x"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("browserify" "chai"
                                             "karma"
                                             "karma-browserify"
                                             "karma-chai"
                                             "karma-mocha"
                                             "karma-phantomjs-launcher"
                                             "karma-sinon"
                                             "mocha"
                                             "phantomjs-polyfill"
                                             "phantomjs-prebuilt"
                                             "simulant"
                                             "sinon"
                                             "watchify")))))))
    (inputs (list node-delegate-3.2.0))
    (home-page "https://github.com/zenorocha/good-listener#readme")
    (synopsis "A more versatile way of adding & removing event listeners")
    (description "A more versatile way of adding & removing event listeners")
    (license license:expat)))

(define-public node-graceful-fs-4.2.11
  (package
    (name "node-graceful-fs")
    (version "4.2.11")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/graceful-fs/-/graceful-fs-4.2.11.tgz")
              (sha256
               (base32
                "1709vla02prpbf34xqsvkqngvsmp5ypnljvg1pcgxrk1l553fq9r"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("import-fresh" "mkdirp" "rimraf"
                                             "tap")))))))
    (home-page "https://github.com/isaacs/node-graceful-fs#readme")
    (synopsis "A drop-in replacement for fs, making various improvements.")
    (description "A drop-in replacement for fs, making various improvements.")
    (license license:isc)))

(define-public node-har-schema-2.0.0
  (package
    (name "node-har-schema")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/har-schema/-/har-schema-2.0.0.tgz")
              (sha256
               (base32
                "09myh5q5225c53v39mw9n3a2kgf2pk0z9dfwbmm7rbb70npq8yrf"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ajv" "codeclimate-test-reporter"
                                             "cz-conventional-changelog"
                                             "echint"
                                             "semantic-release"
                                             "snazzy"
                                             "tap")))))))
    (home-page "https://github.com/ahmadnassri/har-schema")
    (synopsis "JSON Schema for HTTP Archive (HAR)")
    (description "JSON Schema for HTTP Archive (HAR)")
    (license license:isc)))

(define-public node-har-validator-5.1.5
  (package
    (name "node-har-validator")
    (version "5.1.5")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/har-validator/-/har-validator-5.1.5.tgz")
              (sha256
               (base32
                "02vymdr8s3x1cbsv15m9fq6bnbiajyjy8vdz0hl9vrv8xi5ay27f"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("tap")))))))
    (inputs (list node-har-schema-2.0.0 node-ajv-6.12.6))
    (home-page "https://github.com/ahmadnassri/node-har-validator")
    (synopsis "Extremely fast HTTP Archive (HAR) validator using JSON Schema")
    (description
     "Extremely fast HTTP Archive (HAR) validator using JSON Schema")
    (license license:expat)))

(define-public node-has-1.0.3
  (package
    (name "node-has")
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/has/-/has-1.0.3.tgz")
              (sha256
               (base32
                "0wsmn2vcbqb23xpbzxipjd7xcdljid2gwnwl7vn5hkp0zkpgk363"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@ljharb/eslint-config" "eslint"
                                             "tape")))))))
    (inputs (list node-function-bind-1.1.1))
    (home-page "https://github.com/tarruda/has")
    (synopsis "Object.prototype.hasOwnProperty.call shortcut")
    (description "Object.prototype.hasOwnProperty.call shortcut")
    (license license:expat)))

(define-public node-has-flag-3.0.0
  (package
    (name "node-has-flag")
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/has-flag/-/has-flag-3.0.0.tgz")
              (sha256
               (base32
                "1sp0m48zavms86q7vkf90mwll9z2bqi11hk3s01aw8nw40r72jzd"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "xo")))))))
    (home-page "https://github.com/sindresorhus/has-flag#readme")
    (synopsis "Check if argv has a specific flag")
    (description "Check if argv has a specific flag")
    (license license:expat)))

(define-public node-has-proto-1.0.1
  (package
    (name "node-has-proto")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/has-proto/-/has-proto-1.0.1.tgz")
              (sha256
               (base32
                "0lgnq6pcakprcxq176803bhv901sdjp23nii1rfh4q1dd5nxa457"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@ljharb/eslint-config" "aud"
                                             "auto-changelog"
                                             "eslint"
                                             "in-publish"
                                             "npmignore"
                                             "safe-publish-latest"
                                             "tape")))))))
    (home-page "https://github.com/inspect-js/has-proto#readme")
    (synopsis
     "Does this environment have the ability to get the [[Prototype]] of an object on creation with `__proto__`?")
    (description
     "Does this environment have the ability to get the [[Prototype]] of an object on creation with `__proto__`?")
    (license license:expat)))

(define-public node-has-symbols-1.0.3
  (package
    (name "node-has-symbols")
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/has-symbols/-/has-symbols-1.0.3.tgz")
              (sha256
               (base32
                "19p65j4wxry2y75capz52i4miva1z5m6b2qs1b8aj4hy5xl1gk6x"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@ljharb/eslint-config" "aud"
                                             "auto-changelog"
                                             "core-js"
                                             "eslint"
                                             "get-own-property-symbols"
                                             "nyc"
                                             "safe-publish-latest"
                                             "tape")))))))
    (home-page "https://github.com/ljharb/has-symbols#readme")
    (synopsis
     "Determine if the JS environment has Symbol support. Supports spec, or shams.")
    (description
     "Determine if the JS environment has Symbol support. Supports spec, or shams.")
    (license license:expat)))

(define-public node-has-unicode-2.0.1
  (package
    (name "node-has-unicode")
    (version "2.0.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/has-unicode/-/has-unicode-2.0.1.tgz")
              (sha256
               (base32
                "1b7c6mrpncz2x7s6r1v9kcmqw6hix039kbkbzqz0czma56gxsqfh"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("require-inject" "tap")))))))
    (home-page "https://github.com/iarna/has-unicode")
    (synopsis "Try to guess if your terminal supports unicode")
    (description "Try to guess if your terminal supports unicode")
    (license license:isc)))

(define-public node-has-value-0.3.1
  (package
    (name "node-has-value")
    (version "0.3.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/has-value/-/has-value-0.3.1.tgz")
              (sha256
               (base32
                "1h4dnzr9rszpj0a015529r1c9g8ysy4wym6ay99vc6dls02jhmny"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp-format-md" "mocha")))))))
    (inputs (list node-isobject-2.1.0 node-has-values-0.1.4
                  node-get-value-2.0.6))
    (home-page "https://github.com/jonschlinkert/has-value")
    (synopsis
     "Returns true if a value exists, false if empty. Works with deeply nested values using object paths.")
    (description
     "Returns true if a value exists, false if empty. Works with deeply nested values using object paths.")
    (license license:expat)))

(define-public node-has-value-1.0.0
  (package
    (name "node-has-value")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/has-value/-/has-value-1.0.0.tgz")
              (sha256
               (base32
                "1ikpbff1imw3nqqp9q9siwh7p93r1pdigiv6kgryx6l8hwiwbxr9"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp-format-md" "mocha")))))))
    (inputs (list node-isobject-3.0.1 node-has-values-1.0.0
                  node-get-value-2.0.6))
    (home-page "https://github.com/jonschlinkert/has-value")
    (synopsis
     "Returns true if a value exists, false if empty. Works with deeply nested values using object paths.")
    (description
     "Returns true if a value exists, false if empty. Works with deeply nested values using object paths.")
    (license license:expat)))

(define-public node-has-values-0.1.4
  (package
    (name "node-has-values")
    (version "0.1.4")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/has-values/-/has-values-0.1.4.tgz")
              (sha256
               (base32
                "1lnp2ww5w0lxxiqrm3scyaddj7csdrd4ldsmsvapys4rdfgnx0m6"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp-format-md" "mocha")))))))
    (home-page "https://github.com/jonschlinkert/has-values")
    (synopsis
     "Returns true if any values exist, false if empty. Works for booleans, functions, numbers, strings, nulls, objects and arrays. ")
    (description
     "Returns true if any values exist, false if empty. Works for booleans, functions, numbers, strings, nulls, objects and arrays. ")
    (license license:expat)))

(define-public node-has-values-1.0.0
  (package
    (name "node-has-values")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/has-values/-/has-values-1.0.0.tgz")
              (sha256
               (base32
                "19ggazh85imjwxnvvhabypdpmyl0q4mgm3frwfx2dx3ffq1g1bwc"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp-format-md" "mocha")))))))
    (inputs (list node-kind-of-4.0.0 node-is-number-3.0.0))
    (home-page "https://github.com/jonschlinkert/has-values")
    (synopsis
     "Returns true if any values exist, false if empty. Works for booleans, functions, numbers, strings, nulls, objects and arrays. ")
    (description
     "Returns true if any values exist, false if empty. Works for booleans, functions, numbers, strings, nulls, objects and arrays. ")
    (license license:expat)))

(define-public node-hoist-non-react-statics-3.3.2
  (package
    (name "node-hoist-non-react-statics")
    (version "3.3.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/hoist-non-react-statics/-/hoist-non-react-statics-3.3.2.tgz")
              (sha256
               (base32
                "1fn8c8a85nszglnyygpc1dzg2h7k7xwylqg0l88if5sp5jsy8yhg"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@babel/core"
                                             "@babel/plugin-proposal-class-properties"
                                             "@babel/preset-env"
                                             "@babel/preset-react"
                                             "@babel/register"
                                             "chai"
                                             "coveralls"
                                             "create-react-class"
                                             "eslint"
                                             "mocha"
                                             "nyc"
                                             "pre-commit"
                                             "prop-types"
                                             "react"
                                             "rimraf"
                                             "rollup"
                                             "rollup-plugin-babel"
                                             "rollup-plugin-commonjs"
                                             "rollup-plugin-node-resolve"
                                             "rollup-plugin-terser")))))))
    (inputs (list node-react-is-16.13.1))
    (home-page "https://github.com/mridgway/hoist-non-react-statics#readme")
    (synopsis
     "Copies non-react specific statics from a child component to a parent component")
    (description
     "Copies non-react specific statics from a child component to a parent component")
    (license license:bsd-3)))

(define-public node-html-1.0.0
  (package
    (name "node-html")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/html/-/html-1.0.0.tgz")
              (sha256
               (base32
                "1h0zw5lz45aaz17aq27la0pg0lkc6b8l0hvngcgq6vkm67vkij7l"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (inputs (list node-concat-stream-1.6.2))
    (home-page "https://github.com/maxogden/commonjs-html-prettyprinter")
    (synopsis "HTML pretty printer CLI utility (based on jsbeautifier)")
    (description "HTML pretty printer CLI utility (based on jsbeautifier)")
    (license #f)))

(define-public node-http-errors-2.0.0
  (package
    (name "node-http-errors")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/http-errors/-/http-errors-2.0.0.tgz")
              (sha256
               (base32
                "1dypd936i09cvjyxx338da0nimbm4cqi2rrxhjch3ix2wmwx6ky1"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("eslint" "eslint-config-standard"
                                             "eslint-plugin-import"
                                             "eslint-plugin-markdown"
                                             "eslint-plugin-node"
                                             "eslint-plugin-promise"
                                             "eslint-plugin-standard"
                                             "mocha"
                                             "nyc")))))))
    (inputs (list node-toidentifier-1.0.1 node-statuses-2.0.1
                  node-setprototypeof-1.2.0 node-inherits-2.0.4
                  node-depd-2.0.0))
    (home-page "https://github.com/jshttp/http-errors#readme")
    (synopsis "Create HTTP error objects")
    (description "Create HTTP error objects")
    (license license:expat)))

(define-public node-http-signature-1.2.0
  (package
    (name "node-http-signature")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/http-signature/-/http-signature-1.2.0.tgz")
              (sha256
               (base32
                "1y856b84kxhq6wc9yiqcfhd4187nizr7lhxi9z69mwzavmpnvgk6"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("tap" "uuid")))))))
    (inputs (list node-sshpk-1.17.0 node-jsprim-1.4.2 node-assert-plus-1.0.0))
    (home-page "https://github.com/joyent/node-http-signature/")
    (synopsis "Reference implementation of Joyent's HTTP Signature scheme.")
    (description "Reference implementation of Joyent's HTTP Signature scheme.")
    (license license:expat)))

(define-public node-https-proxy-agent-5.0.1
  (package
    (name "node-https-proxy-agent")
    (version "5.0.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/https-proxy-agent/-/https-proxy-agent-5.0.1.tgz")
              (sha256
               (base32
                "0690f44jaazg21wii9vxzva1lk4kma0gh6yj1g9ybwii8fs6z8bd"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@types/debug" "@types/node"
                                             "@typescript-eslint/eslint-plugin"
                                             "@typescript-eslint/parser"
                                             "eslint"
                                             "eslint-config-airbnb"
                                             "eslint-config-prettier"
                                             "eslint-import-resolver-typescript"
                                             "eslint-plugin-import"
                                             "eslint-plugin-jsx-a11y"
                                             "eslint-plugin-react"
                                             "mocha"
                                             "proxy"
                                             "rimraf"
                                             "typescript")))))))
    (inputs (list node-debug-4.3.4 node-agent-base-6.0.2))
    (home-page "https://github.com/TooTallNate/node-https-proxy-agent#readme")
    (synopsis "An HTTP(s) proxy `http.Agent` implementation for HTTPS")
    (description "An HTTP(s) proxy `http.Agent` implementation for HTTPS")
    (license license:expat)))

(define-public node-iconv-lite-0.4.24
  (package
    (name "node-iconv-lite")
    (version "0.4.24")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/iconv-lite/-/iconv-lite-0.4.24.tgz")
              (sha256
               (base32
                "0da6ff7dlx6lfhdafsd9sv0h09sicpfakms8bqylrm4f17r68v2p"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("mocha" "request"
                                             "unorm"
                                             "errto"
                                             "async"
                                             "istanbul"
                                             "semver"
                                             "iconv")))))))
    (inputs (list node-safer-buffer-2.1.2))
    (home-page "https://github.com/ashtuchkin/iconv-lite")
    (synopsis "Convert character encodings in pure javascript.")
    (description "Convert character encodings in pure javascript.")
    (license license:expat)))

(define-public node-iconv-lite-0.6.3
  (package
    (name "node-iconv-lite")
    (version "0.6.3")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/iconv-lite/-/iconv-lite-0.6.3.tgz")
              (sha256
               (base32
                "1x681ziwavjjn09j4858fl3h3xi90vf512k5zwg06kwriwafq9vi"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("async" "c8"
                                             "errto"
                                             "iconv"
                                             "mocha"
                                             "request"
                                             "semver"
                                             "unorm")))))))
    (inputs (list node-safer-buffer-2.1.2))
    (home-page "https://github.com/ashtuchkin/iconv-lite")
    (synopsis "Convert character encodings in pure javascript.")
    (description "Convert character encodings in pure javascript.")
    (license license:expat)))

(define-public node-ie-array-find-polyfill-1.1.0
  (package
    (name "node-ie-array-find-polyfill")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/ie-array-find-polyfill/-/ie-array-find-polyfill-1.1.0.tgz")
              (sha256
               (base32
                "0a6hnxnv5s45fkl1y50lz84c2vm54wp4sdj5nxgsynh6jl72hyrk"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://github.com/abdalla/ie-array-find-polyfill#readme")
    (synopsis "Polyfill to provide array.find on IE.")
    (description "Polyfill to provide array.find on IE.")
    (license license:expat)))

(define-public node-ieee754-1.2.1
  (package
    (name "node-ieee754")
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/ieee754/-/ieee754-1.2.1.tgz")
              (sha256
               (base32
                "1b4xiyr6fmgl05cjgc8fiyfk2jagf7xq2y5rknw9scvy76dlpwcf"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("airtap" "standard" "tape")))))))
    (home-page "https://github.com/feross/ieee754#readme")
    (synopsis
     "Read/write IEEE754 floating point numbers from/to a Buffer or array-like object")
    (description
     "Read/write IEEE754 floating point numbers from/to a Buffer or array-like object")
    (license license:bsd-3)))

(define-public node-import-fresh-3.3.0
  (package
    (name "node-import-fresh")
    (version "3.3.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/import-fresh/-/import-fresh-3.3.0.tgz")
              (sha256
               (base32
                "1chk0qimpnkrd2bn072ywnlhvy69cjyndgbij59m2b9jf4rxp945"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "heapdump" "tsd" "xo")))))))
    (inputs (list node-resolve-from-4.0.0 node-parent-module-1.0.1))
    (home-page "https://github.com/sindresorhus/import-fresh#readme")
    (synopsis "Import a module while bypassing the cache")
    (description "Import a module while bypassing the cache")
    (license license:expat)))

(define-public node-inflight-1.0.6
  (package
    (name "node-inflight")
    (version "1.0.6")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/inflight/-/inflight-1.0.6.tgz")
              (sha256
               (base32
                "16w864087xsh3q7f5gm3754s7bpsb9fq3dhknk9nmbvlk3sxr7ss"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("tap")))))))
    (inputs (list node-wrappy-1.0.2 node-once-1.4.0))
    (home-page "https://github.com/isaacs/inflight")
    (synopsis "Add callbacks to requests in flight to avoid async duplication")
    (description
     "Add callbacks to requests in flight to avoid async duplication")
    (license license:isc)))

(define-public node-inherits-2.0.4
  node-inherits)

(define-public node-inquirer-3.3.0
  (package
    (name "node-inquirer")
    (version "3.3.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/inquirer/-/inquirer-3.3.0.tgz")
              (sha256
               (base32
                "0vjqv6xk5cw6p1kq563nvyb7117gyia8ry30r6a45c57kjv7s8bi"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("chai" "cmdify"
                                             "eslint"
                                             "eslint-config-xo-space"
                                             "gulp"
                                             "gulp-codacy"
                                             "gulp-coveralls"
                                             "gulp-eslint"
                                             "gulp-exclude-gitignore"
                                             "gulp-istanbul"
                                             "gulp-line-ending-corrector"
                                             "gulp-mocha"
                                             "gulp-nsp"
                                             "gulp-plumber"
                                             "mocha"
                                             "mockery"
                                             "sinon")))))))
    (inputs (list node-through-2.3.8
                  node-strip-ansi-4.0.0
                  node-string-width-2.1.1
                  node-rx-lite-aggregates-4.0.8
                  node-rx-lite-4.0.8
                  node-run-async-2.4.1
                  node-mute-stream-0.0.7
                  node-lodash-4.17.21
                  node-figures-2.0.0
                  node-external-editor-2.2.0
                  node-cli-width-2.2.1
                  node-cli-cursor-2.1.0
                  node-chalk-2.4.2
                  node-ansi-escapes-3.2.0))
    (home-page "https://github.com/SBoudrias/Inquirer.js#readme")
    (synopsis
     "A collection of common interactive command line user interfaces.")
    (description
     "A collection of common interactive command line user interfaces.")
    (license license:expat)))

(define-public node-inspector-gadget-1.0.0
  (package
    (name "node-inspector-gadget")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/inspector-gadget/-/inspector-gadget-1.0.0.tgz")
              (sha256
               (base32
                "0v8dxzlq2hrbz5g5vh4dhhsqhrj2na27a9jc75mlpqi0373cgi9f"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (inputs (list node-javascript-stringify-1.6.0))
    (home-page "https://github.com/fliphub/fliphub#readme")
    (synopsis
     "preconfigured nodejs util for inspecting, and customizing inspecting")
    (description
     "preconfigured nodejs util for inspecting, and customizing inspecting")
    (license #f)))

(define-public node-ipaddr-js-1.9.1
  (package
    (name "node-ipaddr-js")
    (version "1.9.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/ipaddr.js/-/ipaddr.js-1.9.1.tgz")
              (sha256
               (base32
                "1vlg9vgdlx13dvh6h6sg3rgdbp04lkljmn6gxih43zk77xidjhbl"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("coffee-script" "nodeunit"
                                             "uglify-js")))))))
    (home-page "https://github.com/whitequark/ipaddr.js#readme")
    (synopsis
     "A library for manipulating IPv4 and IPv6 addresses in JavaScript.")
    (description
     "A library for manipulating IPv4 and IPv6 addresses in JavaScript.")
    (license license:expat)))

(define-public node-is-accessor-descriptor-0.1.6
  (package
    (name "node-is-accessor-descriptor")
    (version "0.1.6")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/is-accessor-descriptor/-/is-accessor-descriptor-0.1.6.tgz")
              (sha256
               (base32
                "1j9kc4m771w28kdrph25q8q62yfiazg2gi6frnbfd66xfmimhmi3"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("mocha" "should")))))))
    (inputs (list node-kind-of-3.2.2))
    (home-page "https://github.com/jonschlinkert/is-accessor-descriptor")
    (synopsis
     "Returns true if a value has the characteristics of a valid JavaScript accessor descriptor.")
    (description
     "Returns true if a value has the characteristics of a valid JavaScript accessor descriptor.")
    (license license:expat)))

(define-public node-is-accessor-descriptor-1.0.0
  (package
    (name "node-is-accessor-descriptor")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/is-accessor-descriptor/-/is-accessor-descriptor-1.0.0.tgz")
              (sha256
               (base32
                "18ybls0d0q6y7gp8mzhypyr0vbrx1vr5agm07s201byvc5dfmxql"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp-format-md" "mocha")))))))
    (inputs (list node-kind-of-6.0.3))
    (home-page "https://github.com/jonschlinkert/is-accessor-descriptor")
    (synopsis
     "Returns true if a value has the characteristics of a valid JavaScript accessor descriptor.")
    (description
     "Returns true if a value has the characteristics of a valid JavaScript accessor descriptor.")
    (license license:expat)))

(define-public node-is-arrayish-0.2.1
  (package
    (name "node-is-arrayish")
    (version "0.2.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/is-arrayish/-/is-arrayish-0.2.1.tgz")
              (sha256
               (base32
                "13734x7w9924g9pch6ywgz741hs5ir612k3578k9fy247vcib3c4"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("coffee-script" "coveralls"
                                             "istanbul" "mocha" "should" "xo")))))))
    (home-page "https://github.com/qix-/node-is-arrayish#readme")
    (synopsis "Determines if an object can be used as an array")
    (description "Determines if an object can be used as an array")
    (license license:expat)))

(define-public node-is-binary-path-1.0.1
  (package
    (name "node-is-binary-path")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/is-binary-path/-/is-binary-path-1.0.1.tgz")
              (sha256
               (base32
                "05ylaj9wal46fvlzz34fbfmb0v870bc69k1h9l0lqgkbs1091vpl"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava")))))))
    (inputs (list node-binary-extensions-1.13.1))
    (home-page "https://github.com/sindresorhus/is-binary-path")
    (synopsis "Check if a filepath is a binary file")
    (description "Check if a filepath is a binary file")
    (license license:expat)))

(define-public node-is-buffer-1.1.6
  (package
    (name "node-is-buffer")
    (version "1.1.6")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/is-buffer/-/is-buffer-1.1.6.tgz")
              (sha256
               (base32
                "03l8f9r41xy0lq5zjm790jg758r8wv3fcsfwsd8331w6l30dh6ix"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("standard" "tape" "zuul")))))))
    (home-page "https://github.com/feross/is-buffer#readme")
    (synopsis "Determine if an object is a Buffer")
    (description "Determine if an object is a Buffer")
    (license license:expat)))

(define-public node-is-core-module-2.12.1
  (package
    (name "node-is-core-module")
    (version "2.12.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/is-core-module/-/is-core-module-2.12.1.tgz")
              (sha256
               (base32
                "1bci8wjbkq2j5gqv4hzp4g85y0qvl36bsn085ikwfsrd3381j4hn"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@ljharb/eslint-config" "aud"
                                             "auto-changelog"
                                             "eslint"
                                             "in-publish"
                                             "mock-property"
                                             "npmignore"
                                             "nyc"
                                             "safe-publish-latest"
                                             "semver"
                                             "tape")))))))
    (inputs (list node-has-1.0.3))
    (home-page "https://github.com/inspect-js/is-core-module")
    (synopsis "Is this specifier a node.js core module?")
    (description "Is this specifier a node.js core module?")
    (license license:expat)))

(define-public node-is-data-descriptor-0.1.4
  (package
    (name "node-is-data-descriptor")
    (version "0.1.4")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/is-data-descriptor/-/is-data-descriptor-0.1.4.tgz")
              (sha256
               (base32
                "0grcjmph4r8s17dd24mbq3ax09q9qgm6vrpjwkmhsc87nv42m9s3"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("mocha" "should")))))))
    (inputs (list node-kind-of-3.2.2))
    (home-page "https://github.com/jonschlinkert/is-data-descriptor")
    (synopsis
     "Returns true if a value has the characteristics of a valid JavaScript data descriptor.")
    (description
     "Returns true if a value has the characteristics of a valid JavaScript data descriptor.")
    (license license:expat)))

(define-public node-is-data-descriptor-1.0.0
  (package
    (name "node-is-data-descriptor")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/is-data-descriptor/-/is-data-descriptor-1.0.0.tgz")
              (sha256
               (base32
                "0b51kish7r330jy625yaz424kvawrh8vxnpajmkx364pw9hm9hi8"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp-format-md" "mocha")))))))
    (inputs (list node-kind-of-6.0.3))
    (home-page "https://github.com/jonschlinkert/is-data-descriptor")
    (synopsis
     "Returns true if a value has the characteristics of a valid JavaScript data descriptor.")
    (description
     "Returns true if a value has the characteristics of a valid JavaScript data descriptor.")
    (license license:expat)))

(define-public node-is-descriptor-0.1.6
  (package
    (name "node-is-descriptor")
    (version "0.1.6")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/is-descriptor/-/is-descriptor-0.1.6.tgz")
              (sha256
               (base32
                "0smh1f833y06l7m1qasjms9kv9qjr11bzyaslpz0wq9qmbkl5mdh"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp-format-md" "mocha")))))))
    (inputs (list node-kind-of-5.1.0 node-is-data-descriptor-0.1.4
                  node-is-accessor-descriptor-0.1.6))
    (home-page "https://github.com/jonschlinkert/is-descriptor")
    (synopsis
     "Returns true if a value has the characteristics of a valid JavaScript descriptor. Works for data descriptors and accessor descriptors.")
    (description
     "Returns true if a value has the characteristics of a valid JavaScript descriptor. Works for data descriptors and accessor descriptors.")
    (license license:expat)))

(define-public node-is-descriptor-1.0.2
  (package
    (name "node-is-descriptor")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/is-descriptor/-/is-descriptor-1.0.2.tgz")
              (sha256
               (base32
                "1bbfdklsskykp1qw9h2mpxjk7hjh6685s1raq73lxbn2b6iyfppy"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp-format-md" "mocha")))))))
    (inputs (list node-kind-of-6.0.3 node-is-data-descriptor-1.0.0
                  node-is-accessor-descriptor-1.0.0))
    (home-page "https://github.com/jonschlinkert/is-descriptor")
    (synopsis
     "Returns true if a value has the characteristics of a valid JavaScript descriptor. Works for data descriptors and accessor descriptors.")
    (description
     "Returns true if a value has the characteristics of a valid JavaScript descriptor. Works for data descriptors and accessor descriptors.")
    (license license:expat)))

(define-public node-is-dotfile-1.0.3
  (package
    (name "node-is-dotfile")
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/is-dotfile/-/is-dotfile-1.0.3.tgz")
              (sha256
               (base32
                "13k7xhnmgbirvqs0lrggm7knqhkghm73pdha2sb5fq6ys9l6bili"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("benchmarked" "dotfile-regex"
                                             "gulp-format-md" "mocha")))))))
    (home-page "https://github.com/jonschlinkert/is-dotfile")
    (synopsis
     "Return true if a file path is (or has) a dotfile. Returns false if the path is a dot directory.")
    (description
     "Return true if a file path is (or has) a dotfile. Returns false if the path is a dot directory.")
    (license license:expat)))

(define-public node-is-equal-shallow-0.1.3
  (package
    (name "node-is-equal-shallow")
    (version "0.1.3")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/is-equal-shallow/-/is-equal-shallow-0.1.3.tgz")
              (sha256
               (base32
                "1scqzxqgasmdyzw658nayiaykw4ba82rqkz83svrcdvjvp68phql"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("mocha" "should")))))))
    (inputs (list node-is-primitive-2.0.0))
    (home-page "https://github.com/jonschlinkert/is-equal-shallow")
    (synopsis
     "Does a shallow comparison of two objects, returning false if the keys or values differ.")
    (description
     "Does a shallow comparison of two objects, returning false if the keys or values differ.")
    (license license:expat)))

(define-public node-is-extendable-0.1.1
  (package
    (name "node-is-extendable")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/is-extendable/-/is-extendable-0.1.1.tgz")
              (sha256
               (base32
                "12f91w1hcv9hw2jlrxf3831zhw7fb0bmzdybzsqb71h5phyjnd7b"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("mocha")))))))
    (home-page "https://github.com/jonschlinkert/is-extendable")
    (synopsis
     "Returns true if a value is any of the object types: array, regexp, plain object, function or date. This is useful for determining if a value can be extended, e.g. \"can the value have keys?\"")
    (description
     "Returns true if a value is any of the object types: array, regexp, plain object, function or date. This is useful for determining if a value can be extended, e.g. \"can the value have keys?\"")
    (license license:expat)))

(define-public node-is-extendable-1.0.1
  (package
    (name "node-is-extendable")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/is-extendable/-/is-extendable-1.0.1.tgz")
              (sha256
               (base32
                "0n1610rd5qv9h43zkr464dvk6hns1afmfpzg1g7b7z6as3axkss2"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp-format-md" "mocha")))))))
    (inputs (list node-is-plain-object-2.0.4))
    (home-page "https://github.com/jonschlinkert/is-extendable")
    (synopsis "Returns true if a value is a plain object, array or function.")
    (description
     "Returns true if a value is a plain object, array or function.")
    (license license:expat)))

(define-public node-is-extglob-1.0.0
  (package
    (name "node-is-extglob")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/is-extglob/-/is-extglob-1.0.0.tgz")
              (sha256
               (base32
                "17arbivg9gky2l24xr5jjr2zpqslq73amb7sgp1flnadqcxmcgj7"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("mocha" "should")))))))
    (home-page "https://github.com/jonschlinkert/is-extglob")
    (synopsis "Returns true if a string has an extglob.")
    (description "Returns true if a string has an extglob.")
    (license license:expat)))

(define-public node-is-fullwidth-code-point-2.0.0
  (package
    (name "node-is-fullwidth-code-point")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/is-fullwidth-code-point/-/is-fullwidth-code-point-2.0.0.tgz")
              (sha256
               (base32
                "0sx0mg720hlpxdcg3rpf5ck93bwzkvb5883686v2iwvbxvnx1l2c"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "xo")))))))
    (home-page
     "https://github.com/sindresorhus/is-fullwidth-code-point#readme")
    (synopsis
     "Check if the character represented by a given Unicode code point is fullwidth")
    (description
     "Check if the character represented by a given Unicode code point is fullwidth")
    (license license:expat)))

(define-public node-is-fullwidth-code-point-3.0.0
  (package
    (name "node-is-fullwidth-code-point")
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/is-fullwidth-code-point/-/is-fullwidth-code-point-3.0.0.tgz")
              (sha256
               (base32
                "0jmw03rxmbwbrkx0a8wq05qsjhdrx9jn3vns88dhy1y6bnp5shbg"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "tsd-check" "xo")))))))
    (home-page
     "https://github.com/sindresorhus/is-fullwidth-code-point#readme")
    (synopsis
     "Check if the character represented by a given Unicode code point is fullwidth")
    (description
     "Check if the character represented by a given Unicode code point is fullwidth")
    (license license:expat)))

(define-public node-is-glob-2.0.1
  (package
    (name "node-is-glob")
    (version "2.0.1")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/is-glob/-/is-glob-2.0.1.tgz")
              (sha256
               (base32
                "1bchcw1g5fdi2mrz362hjhlxvrbbg2ppyz7dcqkldpxjl5x2n777"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("mocha")))))))
    (inputs (list node-is-extglob-1.0.0))
    (home-page "https://github.com/jonschlinkert/is-glob")
    (synopsis
     "Returns `true` if the given string looks like a glob pattern or an extglob pattern. This makes it easy to create code that only uses external modules like node-glob when necessary, resulting in much faster code execution and initialization time, and a bet")
    (description
     "Returns `true` if the given string looks like a glob pattern or an extglob pattern. This makes it easy to create code that only uses external modules like node-glob when necessary, resulting in much faster code execution and initialization time, and a bet")
    (license license:expat)))

(define-public node-is-number-2.1.0
  (package
    (name "node-is-number")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/is-number/-/is-number-2.1.0.tgz")
              (sha256
               (base32
                "07v7lz3vb6iq9f36ksnbf60gy91dggaa32l0m8026zpa5ccrx8r2"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("benchmarked" "chalk" "mocha")))))))
    (inputs (list node-kind-of-3.2.2))
    (home-page "https://github.com/jonschlinkert/is-number")
    (synopsis "Returns true if the value is a number. comprehensive tests.")
    (description "Returns true if the value is a number. comprehensive tests.")
    (license license:expat)))

(define-public node-is-number-3.0.0
  (package
    (name "node-is-number")
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/is-number/-/is-number-3.0.0.tgz")
              (sha256
               (base32
                "19rpbi5ryx3y28bh0pwm99az1mridh0p2sinfdxkcbpbxfx5zbf4"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("benchmarked" "chalk"
                                             "gulp-format-md" "mocha")))))))
    (inputs (list node-kind-of-3.2.2))
    (home-page "https://github.com/jonschlinkert/is-number")
    (synopsis "Returns true if the value is a number. comprehensive tests.")
    (description "Returns true if the value is a number. comprehensive tests.")
    (license license:expat)))

(define-public node-is-number-4.0.0
  (package
    (name "node-is-number")
    (version "4.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/is-number/-/is-number-4.0.0.tgz")
              (sha256
               (base32
                "1f47qljw0w6ajdkln18gxzv1qbhbxr88lrs6w485vk0g1b3wqnsy"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("benchmarked" "chalk"
                                             "gulp-format-md" "mocha")))))))
    (home-page "https://github.com/jonschlinkert/is-number")
    (synopsis "Returns true if the value is a number. comprehensive tests.")
    (description "Returns true if the value is a number. comprehensive tests.")
    (license license:expat)))

(define-public node-is-plain-object-2.0.4
  (package
    (name "node-is-plain-object")
    (version "2.0.4")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/is-plain-object/-/is-plain-object-2.0.4.tgz")
              (sha256
               (base32
                "1ipx9y0c1kmq6irjxix6vcxfax6ilnns9pkgjc6cq8ygnyagv4s8"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("browserify" "chai"
                                             "gulp-format-md"
                                             "mocha"
                                             "mocha-phantomjs"
                                             "phantomjs"
                                             "uglify-js")))))))
    (inputs (list node-isobject-3.0.1))
    (home-page "https://github.com/jonschlinkert/is-plain-object")
    (synopsis
     "Returns true if an object was created by the `Object` constructor.")
    (description
     "Returns true if an object was created by the `Object` constructor.")
    (license license:expat)))

(define-public node-is-posix-bracket-0.1.1
  (package
    (name "node-is-posix-bracket")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/is-posix-bracket/-/is-posix-bracket-0.1.1.tgz")
              (sha256
               (base32
                "01yzwk96c7zvc0zlh7wg200ajpk1av5aahcgc1avxizwp53m1343"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp-format-md" "mocha")))))))
    (home-page "https://github.com/jonschlinkert/is-posix-bracket")
    (synopsis
     "Returns true if the given string is a POSIX bracket expression (POSIX character class).")
    (description
     "Returns true if the given string is a POSIX bracket expression (POSIX character class).")
    (license license:expat)))

(define-public node-is-primitive-2.0.0
  (package
    (name "node-is-primitive")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/is-primitive/-/is-primitive-2.0.0.tgz")
              (sha256
               (base32
                "1s57swffqif8vd6zkbw3d5lg25c9236r9m1s8c7c616dmx3c4bnd"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("mocha" "should")))))))
    (home-page "https://github.com/jonschlinkert/is-primitive")
    (synopsis "Returns `true` if the value is a primitive. ")
    (description "Returns `true` if the value is a primitive. ")
    (license license:expat)))

(define-public node-is-typedarray-1.0.0
  (package
    (name "node-is-typedarray")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/is-typedarray/-/is-typedarray-1.0.0.tgz")
              (sha256
               (base32
                "0i9qr2b79d0chhvpd1fc5pcp9bvirpg37f99d40alciqffmrfp0d"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("tape")))))))
    (home-page "https://github.com/hughsk/is-typedarray")
    (synopsis "Detect whether or not an object is a Typed Array")
    (description "Detect whether or not an object is a Typed Array")
    (license license:expat)))

(define-public node-is-windows-1.0.2
  (package
    (name "node-is-windows")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/is-windows/-/is-windows-1.0.2.tgz")
              (sha256
               (base32
                "18iihzz6fs6sfrgq1bpvgkfk3cza6r9wrrgn7ahcbbra1lkjh4bv"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp-format-md" "mocha")))))))
    (home-page "https://github.com/jonschlinkert/is-windows")
    (synopsis
     "Returns true if the platform is windows. UMD module, works with node.js, commonjs, browser, AMD, electron, etc.")
    (description
     "Returns true if the platform is windows. UMD module, works with node.js, commonjs, browser, AMD, electron, etc.")
    (license license:expat)))

(define-public node-isarray-1.0.0
  (package
    (name "node-isarray")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/isarray/-/isarray-1.0.0.tgz")
              (sha256
               (base32
                "11qcjpdzigcwcprhv7nyarlzjcwf3sv5i66q75zf08jj9zqpcg72"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("tape")))))))
    (home-page "https://github.com/juliangruber/isarray")
    (synopsis "Array#isArray for older browsers")
    (description "Array#isArray for older browsers")
    (license license:expat)))

(define-public node-isobject-2.1.0
  (package
    (name "node-isobject")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/isobject/-/isobject-2.1.0.tgz")
              (sha256
               (base32
                "14df1spjczhml90421sq645shkxwggrbbkgp1qgal29kslc7av32"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp-format-md" "mocha")))))))
    (inputs (list node-isarray-1.0.0))
    (home-page "https://github.com/jonschlinkert/isobject")
    (synopsis
     "Returns true if the value is an object and not an array or null.")
    (description
     "Returns true if the value is an object and not an array or null.")
    (license license:expat)))

(define-public node-isobject-3.0.1
  (package
    (name "node-isobject")
    (version "3.0.1")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/isobject/-/isobject-3.0.1.tgz")
              (sha256
               (base32
                "0dvx6rhjj5b9q7fcjg24lfy2nr3a1d2ypqy9zf9lqr2s00mwkiiw"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp-format-md" "mocha")))))))
    (home-page "https://github.com/jonschlinkert/isobject")
    (synopsis
     "Returns true if the value is an object and not an array or null.")
    (description
     "Returns true if the value is an object and not an array or null.")
    (license license:expat)))

(define-public node-isomorphic-ws-4.0.1
  (package
    (name "node-isomorphic-ws")
    (version "4.0.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/isomorphic-ws/-/isomorphic-ws-4.0.1.tgz")
              (sha256
               (base32
                "1p1b7ca7qr36bz763dwj077mxxblvic315nqvshf82wbwjr4ljlk"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ws")))))))
    (home-page "https://github.com/heineiuo/isomorphic-ws#readme")
    (synopsis "Isomorphic implementation of WebSocket")
    (description "Isomorphic implementation of WebSocket")
    (license license:expat)))

(define-public node-isstream-0.1.2
  (package
    (name "node-isstream")
    (version "0.1.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/isstream/-/isstream-0.1.2.tgz")
              (sha256
               (base32
                "0i0br6synccpj2ian2z5fnnna99qq4w73dbp46vnyi53l9w47bkr"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("tape" "core-util-is" "isarray"
                                             "string_decoder" "inherits")))))))
    (home-page "https://github.com/rvagg/isstream")
    (synopsis "Determine if an object is a Stream")
    (description "Determine if an object is a Stream")
    (license license:expat)))

(define-public node-javascript-stringify-1.6.0
  (package
    (name "node-javascript-stringify")
    (version "1.6.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/javascript-stringify/-/javascript-stringify-1.6.0.tgz")
              (sha256
               (base32
                "0g2ph2sskxdczc623g7m2988x0j9hz1byymbzkg188pzakn8qkjf"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("chai" "istanbul" "mocha")))))))
    (home-page "https://github.com/blakeembrey/javascript-stringify#readme")
    (synopsis "Stringify is to `eval` as `JSON.stringify` is to `JSON.parse`")
    (description
     "Stringify is to `eval` as `JSON.stringify` is to `JSON.parse`")
    (license license:expat)))

(define-public node-jayson-4.1.0
  (package
    (name "node-jayson")
    (version "4.1.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/jayson/-/jayson-4.1.0.tgz")
              (sha256
               (base32
                "0pzskha2h2kc7ibba7fw1pgnbbr7pz13xmhi1prm1r3q97krvyag"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@types/express-serve-static-core"
                                             "body-parser"
                                             "connect"
                                             "coveralls"
                                             "es6-promise"
                                             "express"
                                             "ink-docstrap"
                                             "jsdoc"
                                             "jshint"
                                             "mocha"
                                             "node-fetch"
                                             "nyc"
                                             "pass-stream"
                                             "should"
                                             "superagent"
                                             "typescript")))))))
    (inputs (list node-ws-7.5.9
                  node-uuid-8.3.2
                  node-jsonstream-1.3.5
                  node-json-stringify-safe-5.0.1
                  node-isomorphic-ws-4.0.1
                  node-eyes-0.1.8
                  node-es6-promisify-5.0.0
                  node-delay-5.0.0
                  node-commander-2.20.3
                  node-types-ws-7.4.7
                  node-types-node-12.20.55
                  node-types-connect-3.4.35))
    (home-page "https://jayson.tedeh.net")
    (synopsis "JSON-RPC 1.0/2.0 compliant server and client")
    (description "JSON-RPC 1.0/2.0 compliant server and client")
    (license license:expat)))

(define-public node-js-tokens-4.0.0
  (package
    (name "node-js-tokens")
    (version "4.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/js-tokens/-/js-tokens-4.0.0.tgz")
              (sha256
               (base32
                "0lrw3qvcfmxrwwi7p7ng4r17yw32ki7jpnbj2a65ddddv2icg16q"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("coffeescript" "esprima"
                                             "everything.js" "mocha")))))))
    (home-page "https://github.com/lydell/js-tokens#readme")
    (synopsis "A regex that tokenizes JavaScript.")
    (description "A regex that tokenizes JavaScript.")
    (license license:expat)))

(define-public node-js-yaml-4.1.0
  (package
    (name "node-js-yaml")
    (version "4.1.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/js-yaml/-/js-yaml-4.1.0.tgz")
              (sha256
               (base32
                "1jpj5j4aiyh9sbcw7y8jjkwkyc6qmwrffw7a4qfb48ngb4jk7bhd"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@rollup/plugin-commonjs"
                                             "@rollup/plugin-node-resolve"
                                             "ansi"
                                             "benchmark"
                                             "codemirror"
                                             "eslint"
                                             "fast-check"
                                             "gh-pages"
                                             "mocha"
                                             "nyc"
                                             "rollup"
                                             "rollup-plugin-node-polyfills"
                                             "rollup-plugin-terser"
                                             "shelljs")))))))
    (inputs (list node-argparse-2.0.1))
    (home-page "https://github.com/nodeca/js-yaml#readme")
    (synopsis "YAML 1.2 parser and serializer")
    (description "YAML 1.2 parser and serializer")
    (license license:expat)))

(define-public node-jsbn-0.1.1
  (package
    (name "node-jsbn")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/jsbn/-/jsbn-0.1.1.tgz")
              (sha256
               (base32
                "08r3wxx18yixax4w9rs18ya1ggw6kgzjhw5vbsj7sb8a974lpi2s"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://github.com/andyperlitch/jsbn#readme")
    (synopsis
     "The jsbn library is a fast, portable implementation of large-number math in pure JavaScript, enabling public-key crypto and other applications on desktop and mobile browsers.")
    (description
     "The jsbn library is a fast, portable implementation of large-number math in pure JavaScript, enabling public-key crypto and other applications on desktop and mobile browsers.")
    (license license:expat)))

(define-public node-jsesc-0.5.0
  (package
    (name "node-jsesc")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/jsesc/-/jsesc-0.5.0.tgz")
              (sha256
               (base32
                "0x365wjgs65s1xa7zad9rbg5izayxbh2mfgsjgqm7zl0gi3116iy"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("coveralls" "grunt"
                                             "grunt-shell"
                                             "grunt-template"
                                             "istanbul"
                                             "qunit-extras"
                                             "qunitjs"
                                             "regenerate"
                                             "requirejs")))))))
    (home-page "http://mths.be/jsesc")
    (synopsis
     "A JavaScript library for escaping JavaScript strings while generating the shortest possible valid output.")
    (description
     "A JavaScript library for escaping JavaScript strings while generating the shortest possible valid output.")
    (license #f)))

(define-public node-json-parse-even-better-errors-2.3.1
  (package
    (name "node-json-parse-even-better-errors")
    (version "2.3.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/json-parse-even-better-errors/-/json-parse-even-better-errors-2.3.1.tgz")
              (sha256
               (base32
                "0s95hppvdm62vq9ax92bl7dpgs28dp97j6xlrka1q25bc797g1ra"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("tap")))))))
    (home-page "https://github.com/npm/json-parse-even-better-errors#readme")
    (synopsis "JSON.parse with context information on error")
    (description "JSON.parse with context information on error")
    (license license:expat)))

(define-public node-json-schema-0.4.0
  (package
    (name "node-json-schema")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/json-schema/-/json-schema-0.4.0.tgz")
              (sha256
               (base32
                "0jdkfpin57qi9h45afiznjan7m1xm51jn0f0g1y91i1szzrgya1b"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("vows")))))))
    (home-page "https://github.com/kriszyp/json-schema#readme")
    (synopsis "JSON Schema validation and specifications")
    (description "JSON Schema validation and specifications")
    (license #f)))

(define-public node-json-schema-traverse-0.4.1
  (package
    (name "node-json-schema-traverse")
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/json-schema-traverse/-/json-schema-traverse-0.4.1.tgz")
              (sha256
               (base32
                "0rf0pvm62k8g81vs7n7zx080p6sfylwk52vc149jx1216vcssdgp"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("coveralls" "eslint" "mocha"
                                             "nyc" "pre-commit")))))))
    (home-page "https://github.com/epoberezkin/json-schema-traverse#readme")
    (synopsis "Traverse JSON Schema passing each schema object to callback")
    (description "Traverse JSON Schema passing each schema object to callback")
    (license license:expat)))

(define-public node-json-stringify-safe-5.0.1
  (package
    (name "node-json-stringify-safe")
    (version "5.0.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/json-stringify-safe/-/json-stringify-safe-5.0.1.tgz")
              (sha256
               (base32
                "12ljc7ipy7cprz5zxzzds20ykw6z5616763ca5zx9xmzq1jvzyxp"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("mocha" "must" "sinon")))))))
    (home-page "https://github.com/isaacs/json-stringify-safe")
    (synopsis "Like JSON.stringify, but doesn't blow up on circular refs.")
    (description "Like JSON.stringify, but doesn't blow up on circular refs.")
    (license license:isc)))

(define-public node-jsondiffpatch-0.3.11
  (package
    (name "node-jsondiffpatch")
    (version "0.3.11")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/jsondiffpatch/-/jsondiffpatch-0.3.11.tgz")
              (sha256
               (base32
                "0zv6a35v83zh2fkw634vsj256xr1qxxk0mh1dy2wq3cq2wm6031p"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@istanbuljs/nyc-config-babel"
                                             "babel-core"
                                             "babel-eslint"
                                             "babel-plugin-external-helpers"
                                             "babel-plugin-istanbul"
                                             "babel-plugin-transform-object-rest-spread"
                                             "babel-polyfill"
                                             "babel-preset-env"
                                             "babel-preset-stage-0"
                                             "babel-preset-stage-1"
                                             "babel-preset-stage-2"
                                             "chai"
                                             "codeclimate-test-reporter"
                                             "eslint"
                                             "eslint-config-standard"
                                             "eslint-plugin-import"
                                             "eslint-plugin-node"
                                             "eslint-plugin-promise"
                                             "eslint-plugin-standard"
                                             "istanbul"
                                             "mkdirp"
                                             "mocha"
                                             "nodemon"
                                             "nyc"
                                             "prettier"
                                             "rollup"
                                             "rollup-plugin-babel"
                                             "rollup-plugin-commonjs"
                                             "rollup-plugin-istanbul"
                                             "rollup-plugin-node-resolve"
                                             "rollup-plugin-replace"
                                             "rollup-plugin-visualizer")))))))
    (inputs (list node-diff-match-patch-1.0.5 node-chalk-2.4.2))
    (home-page "https://github.com/benjamine/jsondiffpatch")
    (synopsis "Diff & Patch for Javascript objects")
    (description "Diff & Patch for Javascript objects")
    (license license:expat)))

(define-public node-jsondiffpatch-0.4.1
  (package
    (name "node-jsondiffpatch")
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/jsondiffpatch/-/jsondiffpatch-0.4.1.tgz")
              (sha256
               (base32
                "1imrla6g4i4nbk1mvddai3xg767wja5lp9qjh16zwgqzg7kq9m67"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@istanbuljs/nyc-config-babel"
                                             "babel-core"
                                             "babel-eslint"
                                             "babel-plugin-external-helpers"
                                             "babel-plugin-istanbul"
                                             "babel-plugin-transform-object-rest-spread"
                                             "babel-polyfill"
                                             "babel-preset-env"
                                             "babel-preset-stage-0"
                                             "babel-preset-stage-1"
                                             "babel-preset-stage-2"
                                             "chai"
                                             "codeclimate-test-reporter"
                                             "eslint"
                                             "eslint-config-standard"
                                             "eslint-plugin-import"
                                             "eslint-plugin-node"
                                             "eslint-plugin-promise"
                                             "eslint-plugin-standard"
                                             "istanbul"
                                             "mkdirp"
                                             "mocha"
                                             "nodemon"
                                             "npm-check"
                                             "nyc"
                                             "prettier"
                                             "rollup"
                                             "rollup-plugin-babel"
                                             "rollup-plugin-commonjs"
                                             "rollup-plugin-istanbul"
                                             "rollup-plugin-node-resolve"
                                             "rollup-plugin-replace"
                                             "rollup-plugin-visualizer")))))))
    (inputs (list node-diff-match-patch-1.0.5 node-chalk-2.4.2))
    (home-page "https://github.com/benjamine/jsondiffpatch")
    (synopsis "Diff & Patch for Javascript objects")
    (description "Diff & Patch for Javascript objects")
    (license license:expat)))

(define-public node-jsonfile-4.0.0
  (package
    (name "node-jsonfile")
    (version "4.0.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/jsonfile/-/jsonfile-4.0.0.tgz")
              (sha256
               (base32
                "1s701cy3mlbvgyhhyy2ypqcy064w5990sk8x81gv0200yybrbfaz"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("mocha" "rimraf" "standard")))))))
    (inputs (list node-graceful-fs-4.2.11))
    (home-page "https://github.com/jprichardson/node-jsonfile#readme")
    (synopsis "Easily read/write JSON files.")
    (description "Easily read/write JSON files.")
    (license license:expat)))

(define-public node-jsonparse-1.3.1
  (package
    (name "node-jsonparse")
    (version "1.3.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/jsonparse/-/jsonparse-1.3.1.tgz")
              (sha256
               (base32
                "1kpz1flq77jsw3r0w6il48abclsq3sslcv5vx85hhxwg7bfjwv6g"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("tape" "tap")))))))
    (home-page "https://github.com/creationix/jsonparse#readme")
    (synopsis "This is a pure-js JSON streaming parser for node.js")
    (description "This is a pure-js JSON streaming parser for node.js")
    (license license:expat)))

(define-public node-jsonstream-1.3.5
  (package
    (name "node-jsonstream")
    (version "1.3.5")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/JSONStream/-/JSONStream-1.3.5.tgz")
              (sha256
               (base32
                "1214f66g7084rn4xiclfkkn17d05g650gcrcirlbnvy78lnfffzg"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("it-is" "assertions" "render"
                                             "trees" "event-stream" "tape")))))))
    (inputs (list node-through-2.3.8 node-jsonparse-1.3.1))
    (home-page "http://github.com/dominictarr/JSONStream")
    (synopsis "rawStream.pipe(JSONStream.parse()).pipe(streamOfObjects)")
    (description "rawStream.pipe(JSONStream.parse()).pipe(streamOfObjects)")
    (license #f)))

(define-public node-jsprim-1.4.2
  (package
    (name "node-jsprim")
    (version "1.4.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/jsprim/-/jsprim-1.4.2.tgz")
              (sha256
               (base32
                "1qnwv3p5r2xs7r8mdf28nb9xbammmjrl4zhigmx0p3022n8gk5d2"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (inputs (list node-verror-1.10.0 node-json-schema-0.4.0
                  node-extsprintf-1.3.0 node-assert-plus-1.0.0))
    (home-page "https://github.com/joyent/node-jsprim#readme")
    (synopsis "utilities for primitive JavaScript types")
    (description "utilities for primitive JavaScript types")
    (license license:expat)))

(define-public node-keypress-0.1.0
  (package
    (name "node-keypress")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/keypress/-/keypress-0.1.0.tgz")
              (sha256
               (base32
                "1r9vz0j83pm0fb13yp95gvvjv07aya9ld971fkw7d4gikmzdchrf"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://www.npmjs.com/package/node-keypress")
    (synopsis "Make any Node ReadableStream emit \"keypress\" events")
    (description "Make any Node ReadableStream emit \"keypress\" events")
    (license license:expat)))

(define-public node-kind-of-3.2.2
  (package
    (name "node-kind-of")
    (version "3.2.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/kind-of/-/kind-of-3.2.2.tgz")
              (sha256
               (base32
                "0isxns331nf5f4h8yj0vb4rj626bscxh1rh7j92vk8lbzcs2x93q"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ansi-bold" "benchmarked"
                                             "browserify"
                                             "glob"
                                             "gulp-format-md"
                                             "mocha"
                                             "type-of"
                                             "typeof")))))))
    (inputs (list node-is-buffer-1.1.6))
    (home-page "https://github.com/jonschlinkert/kind-of")
    (synopsis "Get the native type of a value.")
    (description "Get the native type of a value.")
    (license license:expat)))

(define-public node-kind-of-4.0.0
  (package
    (name "node-kind-of")
    (version "4.0.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/kind-of/-/kind-of-4.0.0.tgz")
              (sha256
               (base32
                "0afwg6007r7l0c9r7jghdk8pwvfffmykhybpwrs0nlks59d2ziym"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ansi-bold" "benchmarked"
                                             "browserify"
                                             "glob"
                                             "gulp-format-md"
                                             "mocha"
                                             "type-of"
                                             "typeof")))))))
    (inputs (list node-is-buffer-1.1.6))
    (home-page "https://github.com/jonschlinkert/kind-of")
    (synopsis "Get the native type of a value.")
    (description "Get the native type of a value.")
    (license license:expat)))

(define-public node-kind-of-5.1.0
  (package
    (name "node-kind-of")
    (version "5.1.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/kind-of/-/kind-of-5.1.0.tgz")
              (sha256
               (base32
                "0wkp161zwgg2jp5rjnkw7d7lgczbxj6bwvqvh4s8j68ghk0c8q2d"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ansi-bold" "benchmarked"
                                             "browserify"
                                             "gulp-format-md"
                                             "matched"
                                             "mocha"
                                             "type-of"
                                             "typeof")))))))
    (home-page "https://github.com/jonschlinkert/kind-of")
    (synopsis "Get the native type of a value.")
    (description "Get the native type of a value.")
    (license license:expat)))

(define-public node-kind-of-6.0.3
  (package
    (name "node-kind-of")
    (version "6.0.3")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/kind-of/-/kind-of-6.0.3.tgz")
              (sha256
               (base32
                "1nk31q65n9hcmbp16mbn55siqnf44wn1x71rrqyjv9bcbcxl893c"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("benchmarked" "browserify"
                                             "gulp-format-md" "mocha" "write")))))))
    (home-page "https://github.com/jonschlinkert/kind-of")
    (synopsis "Get the native type of a value.")
    (description "Get the native type of a value.")
    (license license:expat)))

(define-public node-lego-api-1.0.8
  (package
    (name "node-lego-api")
    (version "1.0.8")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/lego-api/-/lego-api-1.0.8.tgz")
              (sha256
               (base32
                "0q1y63fyki5ss4f9y01q93bk39wli46fjvvf9wlbn8w53my05iqw"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "eslint-config-chain-able"
                                             "eslint-plugin-prettier"
                                             "fliplog"
                                             "flow-remove-types"
                                             "prettier"
                                             "prettier-eslint")))))))
    (inputs (list node-chain-able-3.0.0))
    (home-page "https://github.com/fuse-box/lego-api#readme")
    (synopsis "lego api")
    (description "lego api")
    (license license:expat)))

(define-public node-levn-0.3.0
  (package
    (name "node-levn")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/levn/-/levn-0.3.0.tgz")
              (sha256
               (base32
                "094nf5lc3jk3gv0hs01nzgq2vw0h2y9cxaqsbgs0xc45in2kw0ds"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("livescript" "mocha" "istanbul")))))))
    (inputs (list node-type-check-0.3.2 node-prelude-ls-1.1.2))
    (home-page "https://github.com/gkz/levn")
    (synopsis
     "Light ECMAScript (JavaScript) Value Notation - human written, concise, typed, flexible")
    (description
     "Light ECMAScript (JavaScript) Value Notation - human written, concise, typed, flexible")
    (license license:expat)))

(define-public node-lines-and-columns-1.2.4
  (package
    (name "node-lines-and-columns")
    (version "1.2.4")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/lines-and-columns/-/lines-and-columns-1.2.4.tgz")
              (sha256
               (base32
                "174sl99srzs7c0a9l7q73dqmlczgbji9aqh028y2y6c54g8r47s7"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@types/jest" "@types/node"
                                             "@typescript-eslint/eslint-plugin"
                                             "@typescript-eslint/parser"
                                             "esbuild"
                                             "esbuild-runner"
                                             "eslint"
                                             "eslint-config-prettier"
                                             "eslint-plugin-prettier"
                                             "is-ci-cli"
                                             "jest"
                                             "prettier"
                                             "semantic-release"
                                             "typescript")))))))
    (home-page "https://github.com/eventualbuddha/lines-and-columns#readme")
    (synopsis "Maps lines and columns to character offsets and back.")
    (description "Maps lines and columns to character offsets and back.")
    (license license:expat)))

(define-public node-linkify-it-4.0.1
  (package
    (name "node-linkify-it")
    (version "4.0.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/linkify-it/-/linkify-it-4.0.1.tgz")
              (sha256
               (base32
                "14zpf636kd43knh0n2kpi0nvcxw33x70g2f2q0jqqa5dqp9nn5mv"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ansi" "autoprefixer-stylus"
                                             "benchmark"
                                             "browserify"
                                             "eslint"
                                             "gh-pages"
                                             "mdurl"
                                             "mocha"
                                             "ndoc"
                                             "nyc"
                                             "pug-cli"
                                             "shelljs"
                                             "shx"
                                             "stylus"
                                             "tlds")))))))
    (inputs (list node-uc-micro-1.0.6))
    (home-page "https://github.com/markdown-it/linkify-it#readme")
    (synopsis "Links recognition library with FULL unicode support")
    (description "Links recognition library with FULL unicode support")
    (license license:expat)))

(define-public node-lodash--getnative-3.9.1
  (package
    (name "node-lodash--getnative")
    (version "3.9.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/lodash._getnative/-/lodash._getnative-3.9.1.tgz")
              (sha256
               (base32
                "0q3rny7pjcaxp74p7y95nhq4w1gxz72d32nrcjddlb2h7w8l1wzp"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://lodash.com/")
    (synopsis
     "The modern build of lodashâs internal `getNative` as a module.")
    (description
     "The modern build of lodashâs internal `getNative` as a module.")
    (license license:expat)))

(define-public node-lodash-4.17.21
  (package
    (name "node-lodash")
    (version "4.17.21")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/lodash/-/lodash-4.17.21.tgz")
              (sha256
               (base32
                "017qragyfl5ifajdx48lvz46wr0jc1llikgvc2fhqakhwp4pl23a"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://lodash.com/")
    (synopsis "Lodash modular utilities.")
    (description "Lodash modular utilities.")
    (license license:expat)))

(define-public node-lodash-curry-4.1.1
  (package
    (name "node-lodash-curry")
    (version "4.1.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/lodash.curry/-/lodash.curry-4.1.1.tgz")
              (sha256
               (base32
                "01yih63d4chjld2w5y32n15x6ql6qpglyj7cr7175acpyz6mkpz1"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://lodash.com/")
    (synopsis "The lodash method `_.curry` exported as a module.")
    (description "The lodash method `_.curry` exported as a module.")
    (license license:expat)))

(define-public node-lodash-debounce-3.1.1
  (package
    (name "node-lodash-debounce")
    (version "3.1.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/lodash.debounce/-/lodash.debounce-3.1.1.tgz")
              (sha256
               (base32
                "0rd56z9jzp7yack2g7jb0kcsfdfd27c8da5ghkbbyfs2f3xdl9ki"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (inputs (list node-lodash--getnative-3.9.1))
    (home-page "https://lodash.com/")
    (synopsis "The modern build of lodashâs `_.debounce` as a module.")
    (description "The modern build of lodashâs `_.debounce` as a module.")
    (license license:expat)))

(define-public node-lodash-debounce-4.0.8
  (package
    (name "node-lodash-debounce")
    (version "4.0.8")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/lodash.debounce/-/lodash.debounce-4.0.8.tgz")
              (sha256
               (base32
                "180bk3h6nm2kvnn4f6phvcr0qyg5kj30ixrgflng352ndx63h655"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://lodash.com/")
    (synopsis "The lodash method `_.debounce` exported as a module.")
    (description "The lodash method `_.debounce` exported as a module.")
    (license license:expat)))

(define-public node-lodash-flow-3.5.0
  (package
    (name "node-lodash-flow")
    (version "3.5.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/lodash.flow/-/lodash.flow-3.5.0.tgz")
              (sha256
               (base32
                "1p1h7s6zbs2ca7vh7xwa21qlcd5vz65hpbrhr47m39zv6jn9gqir"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://lodash.com/")
    (synopsis "The lodash method `_.flow` exported as a module.")
    (description "The lodash method `_.flow` exported as a module.")
    (license license:expat)))

(define-public node-lodash-orderby-4.6.0
  (package
    (name "node-lodash-orderby")
    (version "4.6.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/lodash.orderby/-/lodash.orderby-4.6.0.tgz")
              (sha256
               (base32
                "111qi7i10a9c2yn5p0xgz566rh1ldpa5v6wdv2a4629wlrnpmzfj"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://lodash.com/")
    (synopsis "The lodash method `_.orderBy` exported as a module.")
    (description "The lodash method `_.orderBy` exported as a module.")
    (license license:expat)))

(define-public node-lodash-uniqby-4.7.0
  (package
    (name "node-lodash-uniqby")
    (version "4.7.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/lodash.uniqby/-/lodash.uniqby-4.7.0.tgz")
              (sha256
               (base32
                "1zwa038vr31hfyalgv49xrszhwdkfjq0rqi5fx5cpsjxry5pspa8"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://lodash.com/")
    (synopsis "The lodash method `_.uniqBy` exported as a module.")
    (description "The lodash method `_.uniqBy` exported as a module.")
    (license license:expat)))

(define-public node-loose-envify-1.4.0
  (package
    (name "node-loose-envify")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/loose-envify/-/loose-envify-1.4.0.tgz")
              (sha256
               (base32
                "1p5b3ca0b2jkxalyg7h9bss6aspa8plkh0ak1mrlz2jkjc58660j"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("browserify" "envify" "tap")))))))
    (inputs (list node-js-tokens-4.0.0))
    (home-page "https://github.com/zertosh/loose-envify")
    (synopsis
     "Fast (and loose) selective `process.env` replacer using js-tokens instead of an AST")
    (description
     "Fast (and loose) selective `process.env` replacer using js-tokens instead of an AST")
    (license license:expat)))

(define-public node-lru-cache-6.0.0
  (package
    (name "node-lru-cache")
    (version "6.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/lru-cache/-/lru-cache-6.0.0.tgz")
              (sha256
               (base32
                "0pnziizgv8jpg708ykywcjby0syjz1l2ll1j727rdxhw0gmhvr2w"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("benchmark" "tap")))))))
    (inputs (list node-yallist-4.0.0))
    (home-page "https://github.com/isaacs/node-lru-cache#readme")
    (synopsis "A cache object that deletes the least-recently-used items.")
    (description "A cache object that deletes the least-recently-used items.")
    (license license:isc)))

(define-public node-make-dir-3.1.0
  (package
    (name "node-make-dir")
    (version "3.1.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/make-dir/-/make-dir-3.1.0.tgz")
              (sha256
               (base32
                "1p3larbzfz9nny2m83x9isf5sng6gdc0x1vf6kyn24abln5anm7c"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@types/graceful-fs"
                                             "@types/node"
                                             "ava"
                                             "codecov"
                                             "graceful-fs"
                                             "nyc"
                                             "path-type"
                                             "tempy"
                                             "tsd"
                                             "xo")))))))
    (inputs (list node-semver-6.3.0))
    (home-page "https://github.com/sindresorhus/make-dir#readme")
    (synopsis "Make a directory and its parents if needed - Think `mkdir -p`")
    (description
     "Make a directory and its parents if needed - Think `mkdir -p`")
    (license license:expat)))

(define-public node-map-cache-0.2.2
  (package
    (name "node-map-cache")
    (version "0.2.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/map-cache/-/map-cache-0.2.2.tgz")
              (sha256
               (base32
                "0gag01y8x17l2ffdmi5rgll24bw4cmyi3wksk45xpghirlkrkhj1"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp-format-md" "should")))))))
    (home-page "https://github.com/jonschlinkert/map-cache")
    (synopsis "Basic cache object for storing key-value pairs.")
    (description "Basic cache object for storing key-value pairs.")
    (license license:expat)))

(define-public node-map-visit-1.0.0
  (package
    (name "node-map-visit")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/map-visit/-/map-visit-1.0.0.tgz")
              (sha256
               (base32
                "19fhyr0jmskx32s1s9b6p0jb96gmbxxnbmzx4sc42sxzwkfmqvpi"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("clone-deep" "extend-shallow"
                                             "gulp-format-md" "lodash" "mocha")))))))
    (inputs (list node-object-visit-1.0.1))
    (home-page "https://github.com/jonschlinkert/map-visit")
    (synopsis "Map `visit` over an array of objects.")
    (description "Map `visit` over an array of objects.")
    (license license:expat)))

(define-public node-mapbox-node-pre-gyp-1.0.10
  (package
    (name "node-mapbox-node-pre-gyp")
    (version "1.0.10")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/@mapbox/node-pre-gyp/-/node-pre-gyp-1.0.10.tgz")
              (sha256
               (base32
                "1p72rwbv37p194hd04bfg7ka3rbvj2f2qp431l32x5pfi0aygs3y"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@mapbox/cloudfriend"
                                             "@mapbox/eslint-config-mapbox"
                                             "aws-sdk"
                                             "codecov"
                                             "eslint"
                                             "eslint-plugin-node"
                                             "mock-aws-s3"
                                             "nock"
                                             "node-addon-api"
                                             "nyc"
                                             "tape"
                                             "tar-fs")))))))
    (inputs (list node-tar-6.1.15
                  node-semver-7.5.1
                  node-rimraf-3.0.2
                  node-npmlog-5.0.1
                  node-nopt-5.0.0
                  node-node-fetch-2.6.11
                  node-make-dir-3.1.0
                  node-https-proxy-agent-5.0.1
                  node-detect-libc-2.0.1))
    (home-page "https://github.com/mapbox/node-pre-gyp#readme")
    (synopsis "Node.js native addon binary install tool")
    (description "Node.js native addon binary install tool")
    (license license:bsd-3)))

(define-public node-markdown-it-13.0.1
  (package
    (name "node-markdown-it")
    (version "13.0.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/markdown-it/-/markdown-it-13.0.1.tgz")
              (sha256
               (base32
                "13yzl03dfwgnb9p4h9qhgai730kg0hyc7hyq0k42pimgn9h0hvrm"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@rollup/plugin-commonjs"
                                             "@rollup/plugin-json"
                                             "@rollup/plugin-node-resolve"
                                             "ansi"
                                             "autoprefixer-stylus"
                                             "benchmark"
                                             "chai"
                                             "coveralls"
                                             "eslint"
                                             "express"
                                             "gh-pages"
                                             "highlight.js"
                                             "jest-worker"
                                             "markdown-it-abbr"
                                             "markdown-it-container"
                                             "markdown-it-deflist"
                                             "markdown-it-emoji"
                                             "markdown-it-footnote"
                                             "markdown-it-for-inline"
                                             "markdown-it-ins"
                                             "markdown-it-mark"
                                             "markdown-it-sub"
                                             "markdown-it-sup"
                                             "markdown-it-testgen"
                                             "mocha"
                                             "ndoc"
                                             "needle"
                                             "nyc"
                                             "pug-cli"
                                             "rollup"
                                             "rollup-plugin-node-polyfills"
                                             "rollup-plugin-terser"
                                             "shelljs"
                                             "stylus"
                                             "supertest")))))))
    (inputs (list node-uc-micro-1.0.6 node-mdurl-1.0.1 node-linkify-it-4.0.1
                  node-entities-3.0.1 node-argparse-2.0.1))
    (home-page "https://github.com/markdown-it/markdown-it#readme")
    (synopsis "Markdown-it - modern pluggable markdown parser.")
    (description "Markdown-it - modern pluggable markdown parser.")
    (license license:expat)))

(define-public node-math-random-1.0.4
  (package
    (name "node-math-random")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/math-random/-/math-random-1.0.4.tgz")
              (sha256
               (base32
                "0882pbcf4m1irc214h2ddi3faspwah70813ryvxl14h3ar0jnqjx"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("array-unique" "dexy")))))))
    (home-page "https://github.com/michaelrhodes/math-random#readme")
    (synopsis
     "math-random is an drop-in replacement for Math.random that uses cryptographically secure random number generation, where available. It works in both browser and node environments.")
    (description
     "math-random is an drop-in replacement for Math.random that uses cryptographically secure random number generation, where available. It works in both browser and node environments.")
    (license license:expat)))

(define-public node-mdurl-1.0.1
  (package
    (name "node-mdurl")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/mdurl/-/mdurl-1.0.1.tgz")
              (sha256
               (base32
                "17hy8nfhys0rvxxdb68wi1n9my78a29walz1qnfwm5qghp7sn5n1"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("mocha" "eslint"
                                             "eslint-plugin-nodeca" "istanbul")))))))
    (home-page "https://github.com/markdown-it/mdurl#readme")
    (synopsis "URL utilities for markdown-it")
    (description "URL utilities for markdown-it")
    (license license:expat)))

(define-public node-media-typer-0.3.0
  (package
    (name "node-media-typer")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/media-typer/-/media-typer-0.3.0.tgz")
              (sha256
               (base32
                "07vlmddn91j0bbrxr2br320dnkxw96dp7hqmvidj5ydl84adiyid"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("istanbul" "mocha" "should")))))))
    (home-page "https://github.com/jshttp/media-typer")
    (synopsis "Simple RFC 6838 media type parser and formatter")
    (description "Simple RFC 6838 media type parser and formatter")
    (license license:expat)))

(define-public node-memoize-one-5.2.1
  (package
    (name "node-memoize-one")
    (version "5.2.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/memoize-one/-/memoize-one-5.2.1.tgz")
              (sha256
               (base32
                "1rd5c7c5mmgqjppj9r5hhn2xgkc9bqpi6ihzgxzg261963chiwig"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@size-limit/preset-small-lib"
                                             "@types/jest"
                                             "@types/lodash.isequal"
                                             "@typescript-eslint/eslint-plugin"
                                             "@typescript-eslint/parser"
                                             "benchmark"
                                             "cross-env"
                                             "eslint"
                                             "eslint-config-prettier"
                                             "eslint-plugin-jest"
                                             "eslint-plugin-prettier"
                                             "jest"
                                             "lodash.isequal"
                                             "prettier"
                                             "rimraf"
                                             "rollup"
                                             "rollup-plugin-replace"
                                             "rollup-plugin-terser"
                                             "rollup-plugin-typescript"
                                             "size-limit"
                                             "ts-jest"
                                             "ts-node"
                                             "tslib"
                                             "typescript")))))))
    (home-page "https://github.com/alexreardon/memoize-one#readme")
    (synopsis
     "A memoization library which only remembers the latest invocation")
    (description
     "A memoization library which only remembers the latest invocation")
    (license license:expat)))

(define-public node-merge-1.2.1
  (package
    (name "node-merge")
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/merge/-/merge-1.2.1.tgz")
              (sha256
               (base32
                "1jkairlra9950qzmfiglyagxahccd9an27i2gjlkp6say0s04m5x"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://github.com/yeikos/js.merge")
    (synopsis
     "Merge multiple objects into one, optionally creating a new cloned object. Similar to the jQuery.extend but more flexible. Works in Node.js and the browser.")
    (description
     "Merge multiple objects into one, optionally creating a new cloned object. Similar to the jQuery.extend but more flexible. Works in Node.js and the browser.")
    (license license:expat)))

(define-public node-merge-descriptors-1.0.1
  (package
    (name "node-merge-descriptors")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/merge-descriptors/-/merge-descriptors-1.0.1.tgz")
              (sha256
               (base32
                "02d4fqgiz4cc33vbcdlah9rafj5vc2z0iy05sc9wpfpzri3kn2l8"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("istanbul" "mocha")))))))
    (home-page "https://github.com/component/merge-descriptors")
    (synopsis "Merge objects using descriptors")
    (description "Merge objects using descriptors")
    (license license:expat)))

(define-public node-methods-1.1.2
  (package
    (name "node-methods")
    (version "1.1.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/methods/-/methods-1.1.2.tgz")
              (sha256
               (base32
                "0g50ci0gc8r8kq1i06q078gw7azkakp7j3yw5qfi6gq2qk8hdlnz"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("istanbul" "mocha")))))))
    (home-page "https://github.com/jshttp/methods")
    (synopsis "HTTP methods that node supports")
    (description "HTTP methods that node supports")
    (license license:expat)))

(define-public node-micromatch-2.3.11
  (package
    (name "node-micromatch")
    (version "2.3.11")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/micromatch/-/micromatch-2.3.11.tgz")
              (sha256
               (base32
                "1gz4w1hn7sh17wv82j53d97pr5fli3747sv28fb00r7ghbn5zxla"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("benchmarked" "chalk"
                                             "gulp"
                                             "gulp-eslint"
                                             "gulp-format-md"
                                             "gulp-istanbul"
                                             "gulp-mocha"
                                             "minimatch"
                                             "minimist"
                                             "mocha"
                                             "multimatch"
                                             "should"
                                             "write")))))))
    (inputs (list node-regex-cache-0.4.4
                  node-parse-glob-3.0.4
                  node-object-omit-2.0.1
                  node-normalize-path-2.1.1
                  node-kind-of-3.2.2
                  node-is-glob-2.0.1
                  node-is-extglob-1.0.0
                  node-filename-regex-2.0.1
                  node-extglob-0.3.2
                  node-expand-brackets-0.1.5
                  node-braces-1.8.5
                  node-array-unique-0.2.1
                  node-arr-diff-2.0.0))
    (home-page "https://github.com/jonschlinkert/micromatch")
    (synopsis
     "Glob matching for javascript/node.js. A drop-in replacement and faster alternative to minimatch and multimatch.")
    (description
     "Glob matching for javascript/node.js. A drop-in replacement and faster alternative to minimatch and multimatch.")
    (license license:expat)))

(define-public node-micromatch-3.1.10
  (package
    (name "node-micromatch")
    (version "3.1.10")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/micromatch/-/micromatch-3.1.10.tgz")
              (sha256
               (base32
                "1j4m1y8x2sib8i5rfilml7yzpgs5njsg2nxzxwlb63997qlpb7p5"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("bash-match" "for-own"
                                             "gulp"
                                             "gulp-format-md"
                                             "gulp-istanbul"
                                             "gulp-mocha"
                                             "gulp-unused"
                                             "is-windows"
                                             "minimatch"
                                             "minimist"
                                             "mocha"
                                             "multimatch")))))))
    (inputs (list node-to-regex-3.0.2
                  node-snapdragon-0.8.2
                  node-regex-not-1.0.2
                  node-object-pick-1.3.0
                  node-nanomatch-1.2.13
                  node-kind-of-6.0.3
                  node-fragment-cache-0.2.1
                  node-extglob-2.0.4
                  node-extend-shallow-3.0.2
                  node-define-property-2.0.2
                  node-braces-2.3.2
                  node-array-unique-0.3.2
                  node-arr-diff-4.0.0))
    (home-page "https://github.com/micromatch/micromatch")
    (synopsis
     "Glob matching for javascript/node.js. A drop-in replacement and faster alternative to minimatch and multimatch.")
    (description
     "Glob matching for javascript/node.js. A drop-in replacement and faster alternative to minimatch and multimatch.")
    (license license:expat)))

(define-public node-mime-1.6.0
  (package
    (name "node-mime")
    (version "1.6.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/mime/-/mime-1.6.0.tgz")
              (sha256
               (base32
                "16iprk4h6nh780mvfv0p93k3yvj7jrq2qs92niaw6yk11qwi0li1"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:modules ((guix build node-build-system)
                  (srfi srfi-1)
                  (ice-9 match)
                  (guix build utils))
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-before 'configure 'avoid-prepare-scripts
                    (lambda _
                      ;; We need to remove the prepare script from "package.json", as
                      ;; it would try to use the build environment and would block the
                      ;; automatic building by other packages making use of node-acorn.
                      ;; TODO: Add utility function
                      (with-atomic-json-file-replacement "package.json"
                                                         (match-lambda
                                                           (((quote @) . pkg-meta-alist)
                                                            (cons '@
                                                                  (map (match-lambda
                                                                         (("scripts" @ . scripts-alist) `
                                                                          ("scripts"
                                                                           @
                                                                           ,@(filter (match-lambda
                                                                                       
                                                                                       (("prepare" . _)
                                                                                        #f)
                                                                                       
                                                                                       (_
                                                                                        #t))
                                                                              scripts-alist)))
                                                                         (other
                                                                          other))
                                                                   pkg-meta-alist)))))))
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("github-release-notes" "mime-db"
                                             "mime-score")))))))
    (home-page "https://github.com/broofa/node-mime#readme")
    (synopsis "A comprehensive library for mime-type mapping")
    (description "A comprehensive library for mime-type mapping")
    (license license:expat)))

(define-public node-mime-db-1.52.0
  (package
    (name "node-mime-db")
    (version "1.52.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/mime-db/-/mime-db-1.52.0.tgz")
              (sha256
               (base32
                "0fwyiyqi3w03w3xwy2jhm8rsa0y9wgkc0j6q3q6mvk9asns0prxq"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("bluebird" "co"
                                             "cogent"
                                             "csv-parse"
                                             "eslint"
                                             "eslint-config-standard"
                                             "eslint-plugin-import"
                                             "eslint-plugin-markdown"
                                             "eslint-plugin-node"
                                             "eslint-plugin-promise"
                                             "eslint-plugin-standard"
                                             "gnode"
                                             "media-typer"
                                             "mocha"
                                             "nyc"
                                             "raw-body"
                                             "stream-to-array")))))))
    (home-page "https://github.com/jshttp/mime-db#readme")
    (synopsis "Media Type Database")
    (description "Media Type Database")
    (license license:expat)))

(define-public node-mime-types-2.1.35
  (package
    (name "node-mime-types")
    (version "2.1.35")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/mime-types/-/mime-types-2.1.35.tgz")
              (sha256
               (base32
                "1hyi043kcqyfz82w19s357klvj54f0s94d40rymbms86i74lyws9"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("eslint" "eslint-config-standard"
                                             "eslint-plugin-import"
                                             "eslint-plugin-markdown"
                                             "eslint-plugin-node"
                                             "eslint-plugin-promise"
                                             "eslint-plugin-standard"
                                             "mocha"
                                             "nyc")))))))
    (inputs (list node-mime-db-1.52.0))
    (home-page "https://github.com/jshttp/mime-types#readme")
    (synopsis "The ultimate javascript content-type utility.")
    (description "The ultimate javascript content-type utility.")
    (license license:expat)))

(define-public node-mimic-fn-1.2.0
  (package
    (name "node-mimic-fn")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/mimic-fn/-/mimic-fn-1.2.0.tgz")
              (sha256
               (base32
                "0fkbqr66pl1vzbqph4m8qc0ss1wr7hhnxymdhj32klsxlvbiz4xi"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "xo")))))))
    (home-page "https://github.com/sindresorhus/mimic-fn#readme")
    (synopsis "Make a function mimic another one")
    (description "Make a function mimic another one")
    (license license:expat)))

(define-public node-minimatch-3.1.2
  (package
    (name "node-minimatch")
    (version "3.1.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/minimatch/-/minimatch-3.1.2.tgz")
              (sha256
               (base32
                "0kd3h6q90kvmzzw1v7cc3dr911gjkb9s547cdvfncfqanq84p5hk"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("tap")))))))
    (inputs (list node-brace-expansion-1.1.11))
    (home-page "https://github.com/isaacs/minimatch#readme")
    (synopsis "a glob matcher in javascript")
    (description "a glob matcher in javascript")
    (license license:isc)))

(define-public node-minimist-1.2.8
  (package
    (name "node-minimist")
    (version "1.2.8")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/minimist/-/minimist-1.2.8.tgz")
              (sha256
               (base32
                "10yfwkrl00d8gy9z622yrklg1jax3qk38j354jfw34xk2p0pc2im"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@ljharb/eslint-config" "aud"
                                             "auto-changelog"
                                             "eslint"
                                             "in-publish"
                                             "npmignore"
                                             "nyc"
                                             "safe-publish-latest"
                                             "tape")))))))
    (home-page "https://github.com/minimistjs/minimist")
    (synopsis "parse argument options")
    (description "parse argument options")
    (license license:expat)))

(define-public node-minipass-3.3.6
  (package
    (name "node-minipass")
    (version "3.3.6")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/minipass/-/minipass-3.3.6.tgz")
              (sha256
               (base32
                "0pcyrxs22fnr6n1910qa7mpvdwfnxjvwnkwf5j04f6d28ds91lks"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@types/node" "end-of-stream"
                                             "prettier"
                                             "tap"
                                             "through2"
                                             "ts-node"
                                             "typescript")))))))
    (inputs (list node-yallist-4.0.0))
    (home-page "https://github.com/isaacs/minipass#readme")
    (synopsis "minimal implementation of a PassThrough stream")
    (description "minimal implementation of a PassThrough stream")
    (license license:isc)))

(define-public node-minipass-5.0.0
  (package
    (name "node-minipass")
    (version "5.0.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/minipass/-/minipass-5.0.0.tgz")
              (sha256
               (base32
                "0gyp0rswjzj01mqapa714zw9h5m2jbynfcfz01g107wgdhc5hwdy"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:modules ((guix build node-build-system)
                  (srfi srfi-1)
                  (ice-9 match)
                  (guix build utils))
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-before 'configure 'avoid-prepare-scripts
                    (lambda _
                      ;; We need to remove the prepare script from "package.json", as
                      ;; it would try to use the build environment and would block the
                      ;; automatic building by other packages making use of node-acorn.
                      ;; TODO: Add utility function
                      (with-atomic-json-file-replacement "package.json"
                                                         (match-lambda
                                                           (((quote @) . pkg-meta-alist)
                                                            (cons '@
                                                                  (map (match-lambda
                                                                         (("scripts" @ . scripts-alist) `
                                                                          ("scripts"
                                                                           @
                                                                           ,@(filter (match-lambda
                                                                                       
                                                                                       (("prepare" . _)
                                                                                        #f)
                                                                                       
                                                                                       (_
                                                                                        #t))
                                                                              scripts-alist)))
                                                                         (other
                                                                          other))
                                                                   pkg-meta-alist)))))))
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@types/node" "end-of-stream"
                                             "node-abort-controller"
                                             "prettier"
                                             "tap"
                                             "through2"
                                             "ts-node"
                                             "typedoc"
                                             "typescript")))))))
    (home-page "https://github.com/isaacs/minipass#readme")
    (synopsis "minimal implementation of a PassThrough stream")
    (description "minimal implementation of a PassThrough stream")
    (license license:isc)))

(define-public node-minizlib-2.1.2
  (package
    (name "node-minizlib")
    (version "2.1.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/minizlib/-/minizlib-2.1.2.tgz")
              (sha256
               (base32
                "1vffn3i5ys3w74s8m8n4l7vvzijddi44flpaxdfv96q85n513va4"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("tap")))))))
    (inputs (list node-yallist-4.0.0 node-minipass-3.3.6))
    (home-page "https://github.com/isaacs/minizlib#readme")
    (synopsis
     "A small fast zlib stream built on [minipass](http://npm.im/minipass) and Node.js's zlib binding.")
    (description
     "A small fast zlib stream built on [minipass](http://npm.im/minipass) and Node.js's zlib binding.")
    (license license:expat)))

(define-public node-mixin-deep-1.3.2
  (package
    (name "node-mixin-deep")
    (version "1.3.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/mixin-deep/-/mixin-deep-1.3.2.tgz")
              (sha256
               (base32
                "0kdw3h2r5cpfazjdfjzh9yac7c2gb1vrl3nxm0bhl7pb3yh276iq"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp-format-md" "mocha" "should")))))))
    (inputs (list node-is-extendable-1.0.1 node-for-in-1.0.2))
    (home-page "https://github.com/jonschlinkert/mixin-deep")
    (synopsis
     "Deeply mix the properties of objects into the first object. Like merge-deep, but doesn't clone.")
    (description
     "Deeply mix the properties of objects into the first object. Like merge-deep, but doesn't clone.")
    (license license:expat)))

(define-public node-mkdirp-0.5.6
  (package
    (name "node-mkdirp")
    (version "0.5.6")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/mkdirp/-/mkdirp-0.5.6.tgz")
              (sha256
               (base32
                "0sdmma4bm86i69b5p1didy3cass87rb0zjv35z0xs8sb0ld2ja8v"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("tap")))))))
    (inputs (list node-minimist-1.2.8))
    (home-page "https://github.com/substack/node-mkdirp#readme")
    (synopsis "Recursively mkdir, like `mkdir -p`")
    (description "Recursively mkdir, like `mkdir -p`")
    (license license:expat)))

(define-public node-mkdirp-1.0.4
  (package
    (name "node-mkdirp")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/mkdirp/-/mkdirp-1.0.4.tgz")
              (sha256
               (base32
                "06nqac14zbpar89jc7s574l1qpmamr1kzy0dr3qyhvxg8570f5qx"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("require-inject" "tap")))))))
    (home-page "https://github.com/isaacs/node-mkdirp#readme")
    (synopsis "Recursively mkdir, like `mkdir -p`")
    (description "Recursively mkdir, like `mkdir -p`")
    (license license:expat)))

(define-public node-ms-2.0.0
  (package
    (name "node-ms")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/ms/-/ms-2.0.0.tgz")
              (sha256
               (base32
                "1jrysw9zx14av3jdvc3kywc3xkjqxh748g4s6p1iy634i2mm489n"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("eslint" "expect.js" "husky"
                                             "lint-staged" "mocha")))))))
    (home-page "https://github.com/zeit/ms#readme")
    (synopsis "Tiny milisecond conversion utility")
    (description "Tiny milisecond conversion utility")
    (license license:expat)))

(define-public node-ms-2.1.2
  (package
    (name "node-ms")
    (version "2.1.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/ms/-/ms-2.1.2.tgz")
              (sha256
               (base32
                "0j7vrqxzg2fxip3q0cws360wk3cz2nprr8zkragipziz1piscmqi"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("eslint" "expect.js" "husky"
                                             "lint-staged" "mocha")))))))
    (home-page "https://github.com/zeit/ms#readme")
    (synopsis "Tiny millisecond conversion utility")
    (description "Tiny millisecond conversion utility")
    (license license:expat)))

(define-public node-ms-2.1.3
  node-ms)

(define-public node-mustache-2.3.2
  (package
    (name "node-mustache")
    (version "2.3.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/mustache/-/mustache-2.3.2.tgz")
              (sha256
               (base32
                "0mpjnc0h2aplfgbz74j1hhghh43sbyra5x8gm7ri4syh0nfmsx9a"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("chai" "eslint"
                                             "jshint"
                                             "mocha"
                                             "uglify-js"
                                             "zuul"
                                             "zuul-ngrok")))))))
    (home-page "https://github.com/janl/mustache.js")
    (synopsis "Logic-less {{mustache}} templates with JavaScript")
    (description "Logic-less {{mustache}} templates with JavaScript")
    (license license:expat)))

(define-public node-mute-stream-0.0.7
  (package
    (name "node-mute-stream")
    (version "0.0.7")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/mute-stream/-/mute-stream-0.0.7.tgz")
              (sha256
               (base32
                "1mzb26ahrc1p99gqhbfdigigl473yjjpi1q9qpnjhqz24dzi0mvg"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("tap")))))))
    (home-page "https://github.com/isaacs/mute-stream#readme")
    (synopsis "Bytes go in, but they don't come out (when muted).")
    (description "Bytes go in, but they don't come out (when muted).")
    (license license:isc)))

(define-public node-nan-2.17.0
  (package
    (name "node-nan")
    (version "2.17.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/nan/-/nan-2.17.0.tgz")
              (sha256
               (base32
                "0xlxw2zkgxhkw09fcpr28yarm877fc8fs9lpzsh5pjwjljsmixih"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("bindings" "commander"
                                             "glob"
                                             "request"
                                             "node-gyp"
                                             "readable-stream"
                                             "tap"
                                             "xtend")))))))
    (home-page "https://github.com/nodejs/nan#readme")
    (synopsis
     "Native Abstractions for Node.js: C++ header for Node 0.8 -> 18 compatibility")
    (description
     "Native Abstractions for Node.js: C++ header for Node 0.8 -> 18 compatibility")
    (license license:expat)))

(define-public node-nanoid-2.1.11
  (package
    (name "node-nanoid")
    (version "2.1.11")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/nanoid/-/nanoid-2.1.11.tgz")
              (sha256
               (base32
                "1hx929j296iqkysnngbwsmlp57mr5di88ivfr1nwcjq002vi0wa0"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://github.com/ai/nanoid#readme")
    (synopsis
     "A tiny (119 bytes), secure URL-friendly unique string ID generator")
    (description
     "A tiny (119 bytes), secure URL-friendly unique string ID generator")
    (license license:expat)))

(define-public node-nanomatch-1.2.13
  (package
    (name "node-nanomatch")
    (version "1.2.13")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/nanomatch/-/nanomatch-1.2.13.tgz")
              (sha256
               (base32
                "1f4fxk7azvglyi6gfbkxmh91pd3n6i7av03y9bpizfn37xjf7g0z"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("bash-match" "for-own"
                                             "gulp"
                                             "gulp-format-md"
                                             "gulp-istanbul"
                                             "gulp-mocha"
                                             "helper-changelog"
                                             "minimatch"
                                             "minimist"
                                             "mocha"
                                             "multimatch")))))))
    (inputs (list node-to-regex-3.0.2
                  node-snapdragon-0.8.2
                  node-regex-not-1.0.2
                  node-object-pick-1.3.0
                  node-kind-of-6.0.3
                  node-is-windows-1.0.2
                  node-fragment-cache-0.2.1
                  node-extend-shallow-3.0.2
                  node-define-property-2.0.2
                  node-array-unique-0.3.2
                  node-arr-diff-4.0.0))
    (home-page "https://github.com/micromatch/nanomatch")
    (synopsis
     "Fast, minimal glob matcher for node.js. Similar to micromatch, minimatch and multimatch, but complete Bash 4.3 wildcard support only (no support for exglobs, posix brackets or braces)")
    (description
     "Fast, minimal glob matcher for node.js. Similar to micromatch, minimatch and multimatch, but complete Bash 4.3 wildcard support only (no support for exglobs, posix brackets or braces)")
    (license license:expat)))

(define-public node-nanoseconds-0.1.0
  (package
    (name "node-nanoseconds")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/nanoseconds/-/nanoseconds-0.1.0.tgz")
              (sha256
               (base32
                "0ap9dmnqj0k5mn7bxcj6icbw20sh15j8i1d5zfsyi2la8ic5mn2a"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("mocha")))))))
    (home-page "https://github.com/jonschlinkert/nanoseconds")
    (synopsis
     "Convert the process.hrtime array to a single nanoseconds value.")
    (description
     "Convert the process.hrtime array to a single nanoseconds value.")
    (license license:expat)))

(define-public node-negotiator-0.6.3
  (package
    (name "node-negotiator")
    (version "0.6.3")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/negotiator/-/negotiator-0.6.3.tgz")
              (sha256
               (base32
                "04sjfqwmsamf29a67zwrjdi3h62avc3y6a9y6a74zsgpl1xnhbli"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("eslint" "eslint-plugin-markdown"
                                             "mocha" "nyc")))))))
    (home-page "https://github.com/jshttp/negotiator#readme")
    (synopsis "HTTP content negotiation")
    (description "HTTP content negotiation")
    (license license:expat)))

(define-public node-node-addon-api-3.2.1
  (package
    (name "node-node-addon-api")
    (version "3.2.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/node-addon-api/-/node-addon-api-3.2.1.tgz")
              (sha256
               (base32
                "0b8c9dx5rks26c3vv2zl4zwlg16i464mjk8aasn80jcdf5fn748v"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("benchmark" "bindings"
                                             "clang-format" "fs-extra"
                                             "pre-commit" "safe-buffer")))))))
    (home-page "https://github.com/nodejs/node-addon-api")
    (synopsis "Node.js API (Node-API)")
    (description "Node.js API (Node-API)")
    (license license:expat)))

(define-public node-node-fetch-2.6.11
  (package
    (name "node-node-fetch")
    (version "2.6.11")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/node-fetch/-/node-fetch-2.6.11.tgz")
              (sha256
               (base32
                "1xn85dq7fqpyswya37fhasgipxpkirp7fa6wi8q8frr7p4cjwsp2"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:modules ((guix build node-build-system)
                  (srfi srfi-1)
                  (ice-9 match)
                  (guix build utils))
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-before 'configure 'avoid-prepare-scripts
                    (lambda _
                      ;; We need to remove the prepare script from "package.json", as
                      ;; it would try to use the build environment and would block the
                      ;; automatic building by other packages making use of node-acorn.
                      ;; TODO: Add utility function
                      (with-atomic-json-file-replacement "package.json"
                                                         (match-lambda
                                                           (((quote @) . pkg-meta-alist)
                                                            (cons '@
                                                                  (map (match-lambda
                                                                         (("scripts" @ . scripts-alist) `
                                                                          ("scripts"
                                                                           @
                                                                           ,@(filter (match-lambda
                                                                                       
                                                                                       (("prepare" . _)
                                                                                        #f)
                                                                                       
                                                                                       (_
                                                                                        #t))
                                                                              scripts-alist)))
                                                                         (other
                                                                          other))
                                                                   pkg-meta-alist)))))))
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@ungap/url-search-params"
                                             "abort-controller"
                                             "abortcontroller-polyfill"
                                             "babel-core"
                                             "babel-plugin-istanbul"
                                             "babel-plugin-transform-async-generator-functions"
                                             "babel-polyfill"
                                             "babel-preset-env"
                                             "babel-register"
                                             "chai"
                                             "chai-as-promised"
                                             "chai-iterator"
                                             "chai-string"
                                             "codecov"
                                             "cross-env"
                                             "form-data"
                                             "is-builtin-module"
                                             "mocha"
                                             "nyc"
                                             "parted"
                                             "promise"
                                             "resumer"
                                             "rollup"
                                             "rollup-plugin-babel"
                                             "string-to-arraybuffer"
                                             "teeny-request"
                                             "encoding")))))))
    (inputs (list node-whatwg-url-5.0.0 node-encoding-0.1.13))
    (home-page "https://github.com/bitinn/node-fetch")
    (synopsis "A light-weight module that brings window.fetch to node.js")
    (description "A light-weight module that brings window.fetch to node.js")
    (license license:expat)))

(define-public node-node-gyp-build-4.6.0
  (package
    (name "node-node-gyp-build")
    (version "4.6.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/node-gyp-build/-/node-gyp-build-4.6.0.tgz")
              (sha256
               (base32
                "0k8mys7cyizra77bzck45ci248lyb74xvfjq40m1s63hyyc4pvjw"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("array-shuffle" "standard" "tape")))))))
    (home-page "https://github.com/prebuild/node-gyp-build")
    (synopsis
     "Build tool and bindings loader for node-gyp that supports prebuilds")
    (description
     "Build tool and bindings loader for node-gyp that supports prebuilds")
    (license license:expat)))

(define-public node-nodejieba-2.5.2
  (package
    (name "node-nodejieba")
    (version "2.5.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/nodejieba/-/nodejieba-2.5.2.tgz")
              (sha256
               (base32
                "0pi41pk8y9whdyyx2j6giqkf5rn85fzjjad2yv2cfg0ah0gvxg9b"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'unpack 'unpack-prebuilt
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((prebuilt (assoc-ref inputs "prebuilt")))
                        (invoke "tar" "-xf" prebuilt)
                        (rename-file "Release/nodejieba.node" "nodejieba.node")
                        (substitute* "package.json"
                          (("--fallback-to-build")
                           "")))))
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("coveralls" "istanbul" "mocha"
                                             "node-pre-gyp-github" "should"
                                             "typescript")))))))
    (inputs `(("node-node-addon-api" ,node-node-addon-api-3.2.1)
              ("node-mapbox-node-pre-gyp" ,node-mapbox-node-pre-gyp-1.0.10)
              ("prebuilt" ,(origin
                             (method url-fetch)
                             (uri
                              "https://github.com/yanyiwu/nodejieba/releases/download/v2.5.2/nodejieba-v2.5.2-node-v88-linux-x64.tar.gz")
                             (sha256 (base32
                                      "0id29d3zkzsaic16d4r9vimh1533562nmyzyslv8i2rrvcm68dm2"))))))
    (home-page "https://github.com/yanyiwu/nodejieba#readme")
    (synopsis "chinese word segmentation for node")
    (description "chinese word segmentation for node")
    (license license:expat)))

(define-public node-nopt-1.0.10
  (package
    (name "node-nopt")
    (version "1.0.10")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/nopt/-/nopt-1.0.10.tgz")
              (sha256
               (base32
                "09s96giczznyly80l061wl0nr35pwnnpadf8x9cw1gzv7fa64ra2"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (inputs (list node-abbrev-1.1.1))
    (home-page "https://www.npmjs.com/package/node-nopt")
    (synopsis
     "Option parsing for Node, supporting types, shorthands, etc. Used by npm.")
    (description
     "Option parsing for Node, supporting types, shorthands, etc. Used by npm.")
    (license license:expat)))

(define-public node-nopt-5.0.0
  (package
    (name "node-nopt")
    (version "5.0.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/nopt/-/nopt-5.0.0.tgz")
              (sha256
               (base32
                "1h4pv5dd3yd8zvf4z61sg9ydqvwimdws1w2dm581kj93j35sxivz"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("tap")))))))
    (inputs (list node-abbrev-1.1.1))
    (home-page "https://github.com/npm/nopt#readme")
    (synopsis
     "Option parsing for Node, supporting types, shorthands, etc. Used by npm.")
    (description
     "Option parsing for Node, supporting types, shorthands, etc. Used by npm.")
    (license license:isc)))

(define-public node-normalize-path-2.1.1
  (package
    (name "node-normalize-path")
    (version "2.1.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/normalize-path/-/normalize-path-2.1.1.tgz")
              (sha256
               (base32
                "1d82jyqqyqgk8qkzb4sk7vnz6sgf8xznlm55mazlp43fc6w100cj"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("benchmarked" "gulp-format-md"
                                             "minimist" "mocha")))))))
    (inputs (list node-remove-trailing-separator-1.1.0))
    (home-page "https://github.com/jonschlinkert/normalize-path")
    (synopsis
     "Normalize file path slashes to be unix-like forward slashes. Also condenses repeat slashes to a single slash and removes and trailing slashes unless disabled.")
    (description
     "Normalize file path slashes to be unix-like forward slashes. Also condenses repeat slashes to a single slash and removes and trailing slashes unless disabled.")
    (license license:expat)))

(define-public node-npmlog-5.0.1
  (package
    (name "node-npmlog")
    (version "5.0.1")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/npmlog/-/npmlog-5.0.1.tgz")
              (sha256
               (base32
                "1wk8s79vxn6r2d2c4gj6z81jl7dskl3aghgrjvbwjv1s3lh5glgs"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@npmcli/lint" "tap")))))))
    (inputs (list node-set-blocking-2.0.0 node-gauge-3.0.2
                  node-console-control-strings-1.1.0
                  node-are-we-there-yet-2.0.0))
    (home-page "https://github.com/npm/npmlog#readme")
    (synopsis "logger for npm")
    (description "logger for npm")
    (license license:isc)))

(define-public node-oauth-sign-0.9.0
  (package
    (name "node-oauth-sign")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/oauth-sign/-/oauth-sign-0.9.0.tgz")
              (sha256
               (base32
                "1g6rl2pv86pxcx4mv25qqv0w265mc5ardp3vxd2hqg80c4bsy5h0"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://github.com/mikeal/oauth-sign#readme")
    (synopsis
     "OAuth 1 signing. Formerly a vendor lib in mikeal/request, now a standalone module.")
    (description
     "OAuth 1 signing. Formerly a vendor lib in mikeal/request, now a standalone module.")
    (license license:asl2.0)))

(define-public node-object-assign-4.1.1
  (package
    (name "node-object-assign")
    (version "4.1.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/object-assign/-/object-assign-4.1.1.tgz")
              (sha256
               (base32
                "1v999sycxcp74j2pikdhyinm2d80p2bsy4nnrrnb59rv4rm74bbq"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "lodash" "matcha" "xo")))))))
    (home-page "https://github.com/sindresorhus/object-assign#readme")
    (synopsis "ES2015 `Object.assign()` ponyfill")
    (description "ES2015 `Object.assign()` ponyfill")
    (license license:expat)))

(define-public node-object-copy-0.1.0
  (package
    (name "node-object-copy")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/object-copy/-/object-copy-0.1.0.tgz")
              (sha256
               (base32
                "1b59pq32z0kdzhjkwphyk5h0m59gcc4qvmkvq7zb49yla00w5f0w"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp-format-md" "mocha")))))))
    (inputs (list node-kind-of-3.2.2 node-define-property-0.2.5
                  node-copy-descriptor-0.1.1))
    (home-page "https://github.com/jonschlinkert/object-copy")
    (synopsis
     "Copy static properties, prototype properties, and descriptors from one object to another.")
    (description
     "Copy static properties, prototype properties, and descriptors from one object to another.")
    (license license:expat)))

(define-public node-object-inspect-1.12.3
  (package
    (name "node-object-inspect")
    (version "1.12.3")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/object-inspect/-/object-inspect-1.12.3.tgz")
              (sha256
               (base32
                "1ai21szcxddav1648fb78rdkhvkgpn6k2d8il9s7agk3lvgais2a"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@ljharb/eslint-config"
                                             "@pkgjs/support"
                                             "aud"
                                             "auto-changelog"
                                             "core-js"
                                             "error-cause"
                                             "es-value-fixtures"
                                             "eslint"
                                             "for-each"
                                             "functions-have-names"
                                             "has-tostringtag"
                                             "in-publish"
                                             "make-arrow-function"
                                             "mock-property"
                                             "npmignore"
                                             "nyc"
                                             "safe-publish-latest"
                                             "string.prototype.repeat"
                                             "tape")))))))
    (home-page "https://github.com/inspect-js/object-inspect")
    (synopsis "string representations of objects in node and the browser")
    (description "string representations of objects in node and the browser")
    (license license:expat)))

(define-public node-object-omit-2.0.1
  (package
    (name "node-object-omit")
    (version "2.0.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/object.omit/-/object.omit-2.0.1.tgz")
              (sha256
               (base32
                "1zyjhjfjgmqbnszcxqlvncg7kahc045sp5w1j8f0z92cq9y25zws"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp-format-md" "mocha" "should")))))))
    (inputs (list node-is-extendable-0.1.1 node-for-own-0.1.5))
    (home-page "https://github.com/jonschlinkert/object.omit")
    (synopsis
     "Return a copy of an object excluding the given key, or array of keys. Also accepts an optional filter function as the last argument.")
    (description
     "Return a copy of an object excluding the given key, or array of keys. Also accepts an optional filter function as the last argument.")
    (license license:expat)))

(define-public node-object-pick-1.3.0
  (package
    (name "node-object-pick")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/object.pick/-/object.pick-1.3.0.tgz")
              (sha256
               (base32
                "02zfyg9vkizb5vanjy3d976cnbjnx4qrcjrd92z2ylyl4ih24040"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp-format-md" "mocha" "vinyl")))))))
    (inputs (list node-isobject-3.0.1))
    (home-page "https://github.com/jonschlinkert/object.pick")
    (synopsis
     "Returns a filtered copy of an object with only the specified keys, similar to `_.pick` from lodash / underscore.")
    (description
     "Returns a filtered copy of an object with only the specified keys, similar to `_.pick` from lodash / underscore.")
    (license license:expat)))

(define-public node-object-values-0.1.2
  (package
    (name "node-object-values")
    (version "0.1.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/object_values/-/object_values-0.1.2.tgz")
              (sha256
               (base32
                "0sck8v3vx0qs0gyniwa9brk67i6fjnd926av2i4zhabav5cnzsmw"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://github.com/KhaledElAnsari/Object.values#readme")
    (synopsis "polyfill for es8 Object.values")
    (description "polyfill for es8 Object.values")
    (license license:expat)))

(define-public node-object-visit-1.0.1
  (package
    (name "node-object-visit")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/object-visit/-/object-visit-1.0.1.tgz")
              (sha256
               (base32
                "05qpsh7jyq40dk2mqm85hbcaapb4g4hyjcb4z6b2kcziqfiynpsl"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp" "gulp-eslint"
                                             "gulp-format-md" "gulp-istanbul"
                                             "gulp-mocha" "mocha")))))))
    (inputs (list node-isobject-3.0.1))
    (home-page "https://github.com/jonschlinkert/object-visit")
    (synopsis "Call a specified method on each value in the given object.")
    (description "Call a specified method on each value in the given object.")
    (license license:expat)))

(define-public node-on-finished-2.4.1
  (package
    (name "node-on-finished")
    (version "2.4.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/on-finished/-/on-finished-2.4.1.tgz")
              (sha256
               (base32
                "02mxvpahgv07xaih7lmpn8wic9v4jph3fir0qpd6qf4w0kql4kgn"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("eslint" "eslint-config-standard"
                                             "eslint-plugin-import"
                                             "eslint-plugin-markdown"
                                             "eslint-plugin-node"
                                             "eslint-plugin-promise"
                                             "eslint-plugin-standard"
                                             "mocha"
                                             "nyc")))))))
    (inputs (list node-ee-first-1.1.1))
    (home-page "https://github.com/jshttp/on-finished#readme")
    (synopsis "Execute a callback when a request closes, finishes, or errors")
    (description
     "Execute a callback when a request closes, finishes, or errors")
    (license license:expat)))

(define-public node-once-1.4.0
  node-once)

(define-public node-onetime-2.0.1
  (package
    (name "node-onetime")
    (version "2.0.1")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/onetime/-/onetime-2.0.1.tgz")
              (sha256
               (base32
                "0zmzcy1cg2qdi0g0nvp5picz2gxg6c3xcx7q1rfigfdfyjwha2yr"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "xo")))))))
    (inputs (list node-mimic-fn-1.2.0))
    (home-page "https://github.com/sindresorhus/onetime#readme")
    (synopsis "Ensure a function is only called once")
    (description "Ensure a function is only called once")
    (license license:expat)))

(define-public node-optionator-0.8.3
  (package
    (name "node-optionator")
    (version "0.8.3")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/optionator/-/optionator-0.8.3.tgz")
              (sha256
               (base32
                "0gdxsryh0g0vrbjqrgg8bvzjj2m98hf1rg8sksjmslspkr4fdvsi"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("livescript" "mocha" "istanbul")))))))
    (inputs (list node-fast-levenshtein-2.0.6
                  node-levn-0.3.0
                  node-type-check-0.3.2
                  node-word-wrap-1.2.3
                  node-deep-is-0.1.4
                  node-prelude-ls-1.1.2))
    (home-page "https://github.com/gkz/optionator")
    (synopsis "option parsing and help generation")
    (description "option parsing and help generation")
    (license license:expat)))

(define-public node-options-0.0.6
  (package
    (name "node-options")
    (version "0.0.6")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/options/-/options-0.0.6.tgz")
              (sha256
               (base32
                "1wn9bq0lbkn0md7ma7cccjfdw7z9ia8smls8ab0475rpnxqh08dy"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("mocha")))))))
    (home-page "https://github.com/einaros/options.js")
    (synopsis "A very light-weight in-code option parsers for node.js.")
    (description "A very light-weight in-code option parsers for node.js.")
    (license #f)))

(define-public node-orderedmap-1.1.8
  (package
    (name "node-orderedmap")
    (version "1.1.8")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/orderedmap/-/orderedmap-1.1.8.tgz")
              (sha256
               (base32
                "1g7dbll33jn21ppsad1mdx02pnahyqfjcgqv08rdw42ys0dhww68"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:modules ((guix build node-build-system)
                  (srfi srfi-1)
                  (ice-9 match)
                  (guix build utils))
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  ;; XXX: Copied from Guix.
                  (add-before 'configure 'avoid-prepare-scripts
                    (lambda _
                      (with-atomic-json-file-replacement "package.json"
                                                         (match-lambda
                                                           (((quote @) . pkg-meta-alist)
                                                            (cons '@
                                                                  (map (match-lambda
                                                                         (("scripts" @ . scripts-alist) `
                                                                          ("scripts"
                                                                           @
                                                                           ,@(filter (match-lambda
                                                                                       
                                                                                       (("prepare" . _)
                                                                                        #f)
                                                                                       
                                                                                       (_
                                                                                        #t))
                                                                              scripts-alist)))
                                                                         (other
                                                                          other))
                                                                   pkg-meta-alist)))))))
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("rollup")))))))
    (home-page "https://github.com/marijnh/orderedmap#readme")
    (synopsis "Persistent ordered mapping from strings")
    (description "Persistent ordered mapping from strings")
    (license license:expat)))

(define-public node-orderedmap-2.1.1
  (package
    (name "node-orderedmap")
    (version "2.1.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/orderedmap/-/orderedmap-2.1.1.tgz")
              (sha256
               (base32
                "138n5kqr3rsl6xzlzi84ara82dlb5hfpqb93yap6s5f4daz8mncs"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:modules ((guix build node-build-system)
                  (srfi srfi-1)
                  (ice-9 match)
                  (guix build utils))
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  ;; XXX: Copied from Guix.
                  (add-before 'configure 'avoid-prepare-scripts
                    (lambda _
                      (with-atomic-json-file-replacement "package.json"
                                                         (match-lambda
                                                           (((quote @) . pkg-meta-alist)
                                                            (cons '@
                                                                  (map (match-lambda
                                                                         (("scripts" @ . scripts-alist) `
                                                                          ("scripts"
                                                                           @
                                                                           ,@(filter (match-lambda
                                                                                       
                                                                                       (("prepare" . _)
                                                                                        #f)
                                                                                       
                                                                                       (_
                                                                                        #t))
                                                                              scripts-alist)))
                                                                         (other
                                                                          other))
                                                                   pkg-meta-alist)))))))
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("rollup")))))))
    (home-page "https://github.com/marijnh/orderedmap#readme")
    (synopsis "Persistent ordered mapping from strings")
    (description "Persistent ordered mapping from strings")
    (license license:expat)))

(define-public node-os-tmpdir-1.0.2
  (package
    (name "node-os-tmpdir")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/os-tmpdir/-/os-tmpdir-1.0.2.tgz")
              (sha256
               (base32
                "12ddjb45wq0swr2159wiaxl2balnli8127if7sc89h3psz125rqk"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "xo")))))))
    (home-page "https://github.com/sindresorhus/os-tmpdir#readme")
    (synopsis "Node.js os.tmpdir() ponyfill")
    (description "Node.js os.tmpdir() ponyfill")
    (license license:expat)))

(define-public node-parent-module-1.0.1
  (package
    (name "node-parent-module")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/parent-module/-/parent-module-1.0.1.tgz")
              (sha256
               (base32
                "01z4k8a3y21gqmpcwyf7gq9v8v4k2y5180f5g7qgswd4gq986zk0"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "execa" "xo")))))))
    (inputs (list node-callsites-3.1.0))
    (home-page "https://github.com/sindresorhus/parent-module#readme")
    (synopsis "Get the path of the parent module")
    (description "Get the path of the parent module")
    (license license:expat)))

(define-public node-parse-glob-3.0.4
  (package
    (name "node-parse-glob")
    (version "3.0.4")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/parse-glob/-/parse-glob-3.0.4.tgz")
              (sha256
               (base32
                "1pj6awfzkdl4i9ljmfcqgr16qh8pp11hqb1jsx5lq4rhw12laxmh"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("browserify" "lodash" "mocha")))))))
    (inputs (list node-is-glob-2.0.1 node-is-extglob-1.0.0
                  node-is-dotfile-1.0.3 node-glob-base-0.3.0))
    (home-page "https://github.com/jonschlinkert/parse-glob")
    (synopsis "Parse a glob pattern into an object of tokens.")
    (description "Parse a glob pattern into an object of tokens.")
    (license license:expat)))

(define-public node-parse-json-5.2.0
  (package
    (name "node-parse-json")
    (version "5.2.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/parse-json/-/parse-json-5.2.0.tgz")
              (sha256
               (base32
                "1388gc6zhgygqc68vkmqxafk6r9kxgdqqwbbm96k9hpxw223ddq6"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "nyc" "xo")))))))
    (inputs (list node-lines-and-columns-1.2.4
                  node-json-parse-even-better-errors-2.3.1 node-error-ex-1.3.2
                  node-babel-code-frame-7.22.5))
    (home-page "https://github.com/sindresorhus/parse-json#readme")
    (synopsis "Parse JSON with more helpful errors")
    (description "Parse JSON with more helpful errors")
    (license license:expat)))

(define-public node-parseurl-1.3.3
  (package
    (name "node-parseurl")
    (version "1.3.3")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/parseurl/-/parseurl-1.3.3.tgz")
              (sha256
               (base32
                "06h2bx1rilkdir3v9jlg94r1q2fn895s0vxjjs0wx5z027x4pvsn"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("beautify-benchmark" "benchmark"
                                             "eslint"
                                             "eslint-config-standard"
                                             "eslint-plugin-import"
                                             "eslint-plugin-node"
                                             "eslint-plugin-promise"
                                             "eslint-plugin-standard"
                                             "fast-url-parser"
                                             "istanbul"
                                             "mocha")))))))
    (home-page "https://github.com/pillarjs/parseurl#readme")
    (synopsis "parse a url with memoization")
    (description "parse a url with memoization")
    (license license:expat)))

(define-public node-pascalcase-0.1.1
  (package
    (name "node-pascalcase")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/pascalcase/-/pascalcase-0.1.1.tgz")
              (sha256
               (base32
                "0hd2yjrsfhw3183dxzs5045xnas07in2ww5sl0m5jx035zcrqv2m"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("mocha" "should")))))))
    (home-page "https://github.com/jonschlinkert/pascalcase")
    (synopsis "Convert a string to pascal-case.")
    (description "Convert a string to pascal-case.")
    (license license:expat)))

(define-public node-path-is-absolute-1.0.1
  (package
    (name "node-path-is-absolute")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/path-is-absolute/-/path-is-absolute-1.0.1.tgz")
              (sha256
               (base32
                "0p7p04xxd8q495qhxmxydyjgzcf762dp1hp2wha2b52n3agp0vbf"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("xo")))))))
    (home-page "https://github.com/sindresorhus/path-is-absolute#readme")
    (synopsis "Node.js 0.12 path.isAbsolute() ponyfill")
    (description "Node.js 0.12 path.isAbsolute() ponyfill")
    (license license:expat)))

(define-public node-path-parse-1.0.7
  (package
    (name "node-path-parse")
    (version "1.0.7")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/path-parse/-/path-parse-1.0.7.tgz")
              (sha256
               (base32
                "18vkai53yyiv1c1rimsh2whiymxnz9xj6a39c6b65097ly61jym0"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://github.com/jbgutierrez/path-parse#readme")
    (synopsis "Node.js path.parse() ponyfill")
    (description "Node.js path.parse() ponyfill")
    (license license:expat)))

(define-public node-path-to-regexp-0.1.7
  (package
    (name "node-path-to-regexp")
    (version "0.1.7")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/path-to-regexp/-/path-to-regexp-0.1.7.tgz")
              (sha256
               (base32
                "0dlgr61ahgydnmsp1pprc1m52qnylkb3pdpvn1s5p5x8d7qn4m6y"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("mocha" "istanbul")))))))
    (home-page "https://github.com/component/path-to-regexp#readme")
    (synopsis "Express style path to RegExp utility")
    (description "Express style path to RegExp utility")
    (license license:expat)))

(define-public node-path-type-4.0.0
  (package
    (name "node-path-type")
    (version "4.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/path-type/-/path-type-4.0.0.tgz")
              (sha256
               (base32
                "15wvcgwg053hr2h11ja5swvdz3vvxciqq5aad0ara9qmzgwfh9f0"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "nyc" "tsd-check" "xo")))))))
    (home-page "https://github.com/sindresorhus/path-type#readme")
    (synopsis "Check if a path is a file, directory, or symlink")
    (description "Check if a path is a file, directory, or symlink")
    (license license:expat)))

(define-public node-performance-now-2.1.0
  (package
    (name "node-performance-now")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/performance-now/-/performance-now-2.1.0.tgz")
              (sha256
               (base32
                "0ich517fgk1nhmcjs2mfv4dp70ppqvj3xgmv3syl25zixzfrk3q6"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("bluebird" "call-delayed"
                                             "chai"
                                             "chai-increasing"
                                             "coffee-script"
                                             "mocha"
                                             "pre-commit")))))))
    (home-page "https://github.com/braveg1rl/performance-now")
    (synopsis "Implements performance.now (based on process.hrtime).")
    (description "Implements performance.now (based on process.hrtime).")
    (license license:expat)))

(define-public node-pinyin-2.11.2
  (package
    (name "node-pinyin")
    (version "2.11.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/pinyin/-/pinyin-2.11.2.tgz")
              (sha256
               (base32
                "1s3a58jrlav131psi2aa31b9xb2yfj7dij22cf4l947m8kpwhc55"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("aurl" "aws-sdk"
                                             "benchmark"
                                             "dumi"
                                             "eslint"
                                             "expect.js"
                                             "gh-pages"
                                             "istanbul"
                                             "mocha"
                                             "mock-aws-s3"
                                             "nock"
                                             "nyc"
                                             "react-json-view"
                                             "request")))))))
    (inputs (list node-nodejieba-2.5.2 node-object-assign-4.1.1
                  node-commander-1.1.1))
    (home-page "http://pinyin.hotoo.me/")
    (synopsis "æ±è¯­æ¼é³è½¬æ¢å·¥å·ã")
    (description "æ±è¯­æ¼é³è½¬æ¢å·¥å·ã")
    (license license:expat)))

(define-public node-posix-character-classes-0.1.1
  (package
    (name "node-posix-character-classes")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/posix-character-classes/-/posix-character-classes-0.1.1.tgz")
              (sha256
               (base32
                "08c07ib7iaj34d1mnjhll0bq0yh3kb98q4mf334js2l4166y9rcr"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp-format-md" "mocha")))))))
    (home-page "https://github.com/jonschlinkert/posix-character-classes")
    (synopsis "POSIX character classes for creating regular expressions.")
    (description "POSIX character classes for creating regular expressions.")
    (license license:expat)))

(define-public node-postcss-6.0.23
  (package
    (name "node-postcss")
    (version "6.0.23")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/postcss/-/postcss-6.0.23.tgz")
              (sha256
               (base32
                "1r6pmafj6i6qysgmkl5yjcjjxfbbylx4yf1fb82r3bsp3acx2m8k"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (inputs (list node-supports-color-5.5.0 node-source-map-0.6.1
                  node-chalk-2.4.2))
    (home-page "https://postcss.org/")
    (synopsis "Tool for transforming styles with JS plugins")
    (description "Tool for transforming styles with JS plugins")
    (license license:expat)))

(define-public node-prelude-ls-1.1.2
  (package
    (name "node-prelude-ls")
    (version "1.1.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/prelude-ls/-/prelude-ls-1.1.2.tgz")
              (sha256
               (base32
                "0msvwq9la3w6wm51s2p3j2dv6634sj7iydyx3iqw4y67vkj8zzgs"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("livescript" "uglify-js" "mocha"
                                             "istanbul" "browserify" "sinon")))))))
    (home-page "http://preludels.com")
    (synopsis
     "prelude.ls is a functionally oriented utility library. It is powerful and flexible. Almost all of its functions are curried. It is written in, and is the recommended base library for, LiveScript.")
    (description
     "prelude.ls is a functionally oriented utility library. It is powerful and flexible. Almost all of its functions are curried. It is written in, and is the recommended base library for, LiveScript.")
    (license #f)))

(define-public node-preserve-0.2.0
  (package
    (name "node-preserve")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/preserve/-/preserve-0.2.0.tgz")
              (sha256
               (base32
                "10lk61d1axbbh6j2cglvbcxamn7vh6dy5mmgkc2agmj05nlmlkr9"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("benchmarked" "chalk"
                                             "js-beautify" "mocha" "should")))))))
    (home-page "https://github.com/jonschlinkert/preserve")
    (synopsis
     "Temporarily substitute tokens in the given `string` with placeholders, then put them back after transforming the string.")
    (description
     "Temporarily substitute tokens in the given `string` with placeholders, then put them back after transforming the string.")
    (license license:expat)))

(define-public node-pretty-time-0.2.0
  (package
    (name "node-pretty-time")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/pretty-time/-/pretty-time-0.2.0.tgz")
              (sha256
               (base32
                "0i1qjipzd1dd5m1zrngx9awxyikvmfjl62bl5c1b5lbvf6k0qb46"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp" "gulp-istanbul"
                                             "gulp-jshint" "gulp-mocha"
                                             "jshint-stylish" "mocha")))))))
    (inputs (list node-nanoseconds-0.1.0 node-is-number-2.1.0))
    (home-page "https://github.com/jonschlinkert/pretty-time")
    (synopsis
     "Easily format the time from node.js `process.hrtime`. Works with timescales ranging from weeks to nanoseconds.")
    (description
     "Easily format the time from node.js `process.hrtime`. Works with timescales ranging from weeks to nanoseconds.")
    (license license:expat)))

(define-public node-prettysize-0.0.3
  (package
    (name "node-prettysize")
    (version "0.0.3")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/prettysize/-/prettysize-0.0.3.tgz")
              (sha256
               (base32
                "01kgjq1p0dr3k2gj2vlk2qba66l0bwr79z17f9ah2q939y6bwdwg"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("vows" "yui-lint" "jshint"
                                             "istanbul")))))))
    (home-page "https://www.npmjs.com/package/node-prettysize")
    (synopsis "Convert bytes to other sizes for prettier logging")
    (description "Convert bytes to other sizes for prettier logging")
    (license #f)))

(define-public node-process-nextick-args-2.0.1
  (package
    (name "node-process-nextick-args")
    (version "2.0.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/process-nextick-args/-/process-nextick-args-2.0.1.tgz")
              (sha256
               (base32
                "16w8m2ycy5s4ykgdfg97qxa67gfvkh6x3vdwfsncafyj4p3zhns2"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("tap")))))))
    (home-page "https://github.com/calvinmetcalf/process-nextick-args")
    (synopsis "process.nextTick but always with args")
    (description "process.nextTick but always with args")
    (license license:expat)))

(define-public node-prop-types-15.8.1
  (package
    (name "node-prop-types")
    (version "15.8.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/prop-types/-/prop-types-15.8.1.tgz")
              (sha256
               (base32
                "0mx539ks64wvzzll94jjldpmqj755hpx17afd8l2qilabnq9wwqw"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("babel-jest" "babel-preset-react"
                                             "browserify"
                                             "bundle-collapser"
                                             "eslint"
                                             "in-publish"
                                             "jest"
                                             "react"
                                             "uglifyify"
                                             "uglifyjs")))))))
    (inputs (list node-react-is-16.13.1 node-object-assign-4.1.1
                  node-loose-envify-1.4.0))
    (home-page "https://facebook.github.io/react/")
    (synopsis "Runtime type checking for React props and similar objects.")
    (description "Runtime type checking for React props and similar objects.")
    (license license:expat)))

(define-public node-prosemirror-changeset-2.2.1
  (package
    (name "node-prosemirror-changeset")
    (version "2.2.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/prosemirror-changeset/-/prosemirror-changeset-2.2.1.tgz")
              (sha256
               (base32
                "1n8fq4l91w8wz5h8gs8ygf1nvr3i1j5b3xqgws9vmic6485ivxs3"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@prosemirror/buildhelper"
                                             "prosemirror-model"
                                             "prosemirror-test-builder")))))))
    (inputs (list node-prosemirror-transform-1.7.3))
    (home-page "https://github.com/prosemirror/prosemirror-changeset#readme")
    (synopsis
     "Distills a series of editing steps into deleted and added ranges")
    (description
     "Distills a series of editing steps into deleted and added ranges")
    (license license:expat)))

(define-public node-prosemirror-commands-1.5.2
  (package
    (name "node-prosemirror-commands")
    (version "1.5.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/prosemirror-commands/-/prosemirror-commands-1.5.2.tgz")
              (sha256
               (base32
                "0nh7mps3vmzrfzsrrzxp744p44wdkc2yb8596cgvvnmfz375cixk"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@prosemirror/buildhelper"
                                             "prosemirror-test-builder")))))))
    (inputs (list node-prosemirror-state-1.4.3
                  node-prosemirror-transform-1.7.3
                  node-prosemirror-model-1.19.2))
    (home-page "https://github.com/prosemirror/prosemirror-commands#readme")
    (synopsis "Editing commands for ProseMirror")
    (description "Editing commands for ProseMirror")
    (license license:expat)))

(define-public node-prosemirror-dev-tools-3.1.0
  (package
    (name "node-prosemirror-dev-tools")
    (version "3.1.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/prosemirror-dev-tools/-/prosemirror-dev-tools-3.1.0.tgz")
              (sha256
               (base32
                "01mmdwvsyibfciwa1w3ycj1j6p204ycz156fyakbw0hz1vbkv8ml"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@babel/cli" "@babel/core"
                                             "@babel/plugin-proposal-class-properties"
                                             "@babel/plugin-transform-runtime"
                                             "@babel/preset-env"
                                             "@babel/preset-react"
                                             "@vitejs/plugin-react-refresh"
                                             "commitizen"
                                             "conventional-github-releaser"
                                             "cross-env"
                                             "cz-conventional-changelog"
                                             "lint-staged"
                                             "pmm"
                                             "pre-commit"
                                             "preact"
                                             "preact-compat"
                                             "prettier"
                                             "prosemirror-example-setup"
                                             "prosemirror-schema-basic"
                                             "prosemirror-view"
                                             "react"
                                             "react-dom"
                                             "rimraf"
                                             "vite")))))))
    (inputs (list node-unstated-2.1.1
                  node-react-json-tree-0.11.2
                  node-react-dock-0.2.4
                  node-prosemirror-state-1.4.3
                  node-prosemirror-model-1.19.2
                  node-prop-types-15.8.1
                  node-nanoid-2.1.11
                  node-jsondiffpatch-0.4.1
                  node-html-1.0.0
                  node-emotion-styled-11.11.0
                  node-emotion-react-11.11.1
                  node-emotion-css-11.11.2
                  node-babel-runtime-7.22.5
                  node-react-dom-18.2.0
                  node-react-18.2.0))
    (home-page "https://github.com/d4rkr00t/prosemirror-dev-tools#readme")
    (synopsis "Dev Tools for ProseMirror")
    (description "Dev Tools for ProseMirror")
    (license license:expat)))

(define-public node-prosemirror-dropcursor-1.8.1
  (package
    (name "node-prosemirror-dropcursor")
    (version "1.8.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/prosemirror-dropcursor/-/prosemirror-dropcursor-1.8.1.tgz")
              (sha256
               (base32
                "1x6df2plh12m96f5r72p3dx5696px6xhiin70d8rwrbakp19kgc6"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@prosemirror/buildhelper")))))))
    (inputs (list node-prosemirror-transform-1.7.3
                  node-prosemirror-view-1.31.5 node-prosemirror-state-1.4.3))
    (home-page "https://github.com/prosemirror/prosemirror-dropcursor#readme")
    (synopsis "Drop cursor plugin for ProseMirror")
    (description "Drop cursor plugin for ProseMirror")
    (license license:expat)))

(define-public node-prosemirror-gapcursor-1.3.2
  (package
    (name "node-prosemirror-gapcursor")
    (version "1.3.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/prosemirror-gapcursor/-/prosemirror-gapcursor-1.3.2.tgz")
              (sha256
               (base32
                "0qj3d9s8yw8b0073gsnjzzw8qnw1ilsvsypkwinakyq1fqj3imhl"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@prosemirror/buildhelper")))))))
    (inputs (list node-prosemirror-view-1.31.5 node-prosemirror-state-1.4.3
                  node-prosemirror-model-1.19.2 node-prosemirror-keymap-1.2.2))
    (home-page "https://github.com/prosemirror/prosemirror-gapcursor#readme")
    (synopsis
     "ProseMirror plugin for cursors at normally impossible-to-reach positions")
    (description
     "ProseMirror plugin for cursors at normally impossible-to-reach positions")
    (license license:expat)))

(define-public node-prosemirror-history-1.3.2
  (package
    (name "node-prosemirror-history")
    (version "1.3.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/prosemirror-history/-/prosemirror-history-1.3.2.tgz")
              (sha256
               (base32
                "12c1vlw01qgi4glwvrs49h12nf4774kqynp9j7yh9bkisnw0q6vq"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@prosemirror/buildhelper"
                                             "prosemirror-test-builder")))))))
    (inputs (list node-rope-sequence-1.3.4 node-prosemirror-view-1.31.5
                  node-prosemirror-transform-1.7.3
                  node-prosemirror-state-1.4.3))
    (home-page "https://github.com/prosemirror/prosemirror-history#readme")
    (synopsis "Undo history for ProseMirror")
    (description "Undo history for ProseMirror")
    (license license:expat)))

(define-public node-prosemirror-inputrules-1.2.1
  (package
    (name "node-prosemirror-inputrules")
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/prosemirror-inputrules/-/prosemirror-inputrules-1.2.1.tgz")
              (sha256
               (base32
                "16mbl1dlcfgyh84ad34ffxs72ay5vzddjb2mapdnggv2n2q5h87k"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@prosemirror/buildhelper")))))))
    (inputs (list node-prosemirror-transform-1.7.3
                  node-prosemirror-state-1.4.3))
    (home-page "https://github.com/prosemirror/prosemirror-inputrules#readme")
    (synopsis "Automatic transforms on text input for ProseMirror")
    (description "Automatic transforms on text input for ProseMirror")
    (license license:expat)))

(define-public node-prosemirror-keymap-1.2.2
  (package
    (name "node-prosemirror-keymap")
    (version "1.2.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/prosemirror-keymap/-/prosemirror-keymap-1.2.2.tgz")
              (sha256
               (base32
                "098q0ggwqsgv1ppfimld371yy1v5qffsmhc0b0zmyh7324s69dx1"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:modules ((guix build node-build-system)
                  (srfi srfi-1)
                  (ice-9 match)
                  (guix build utils))
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-before 'configure 'avoid-prepare-scripts
                    (lambda _
                      ;; We need to remove the prepare script from "package.json", as
                      ;; it would try to use the build environment and would block the
                      ;; automatic building by other packages making use of node-acorn.
                      ;; TODO: Add utility function
                      (with-atomic-json-file-replacement "package.json"
                                                         (match-lambda
                                                           (((quote @) . pkg-meta-alist)
                                                            (cons '@
                                                                  (map (match-lambda
                                                                         (("scripts" @ . scripts-alist) `
                                                                          ("scripts"
                                                                           @
                                                                           ,@(filter (match-lambda
                                                                                       
                                                                                       (("prepare" . _)
                                                                                        #f)
                                                                                       
                                                                                       (_
                                                                                        #t))
                                                                              scripts-alist)))
                                                                         (other
                                                                          other))
                                                                   pkg-meta-alist)))))))
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@prosemirror/buildhelper"
                                             "prosemirror-test-builder")))))))
    (inputs (list node-prosemirror-state-1.4.3 node-w3c-keyname-2.2.8))
    (home-page "https://github.com/prosemirror/prosemirror-keymap#readme")
    (synopsis "Keymap plugin for ProseMirror")
    (description "Keymap plugin for ProseMirror")
    (license license:expat)))

(define-public node-prosemirror-model-1.19.2
  (package
    (name "node-prosemirror-model")
    (version "1.19.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/prosemirror-model/-/prosemirror-model-1.19.2.tgz")
              (sha256
               (base32
                "0fv062qchz3wlav9v2k4pxzcngg5pj4jmqhfn1pnzazhl8sa4y6q"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:modules ((guix build node-build-system)
                  (srfi srfi-1)
                  (ice-9 match)
                  (guix build utils))
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  ;; XXX: Copied from Guix.
                  (add-before 'configure 'avoid-prepare-scripts
                    (lambda _
                      (with-atomic-json-file-replacement "package.json"
                                                         (match-lambda
                                                           (((quote @) . pkg-meta-alist)
                                                            (cons '@
                                                                  (map (match-lambda
                                                                         (("scripts" @ . scripts-alist) `
                                                                          ("scripts"
                                                                           @
                                                                           ,@(filter (match-lambda
                                                                                       
                                                                                       (("prepare" . _)
                                                                                        #f)
                                                                                       
                                                                                       (_
                                                                                        #t))
                                                                              scripts-alist)))
                                                                         (other
                                                                          other))
                                                                   pkg-meta-alist)))))))
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@prosemirror/buildhelper"
                                             "jsdom"
                                             "prosemirror-test-builder")))))))
    (inputs (list node-orderedmap-2.1.1))
    (home-page "https://github.com/prosemirror/prosemirror-model#readme")
    (synopsis "ProseMirror's document model")
    (description "ProseMirror's document model")
    (license license:expat)))

(define-public node-prosemirror-schema-list-1.3.0
  (package
    (name "node-prosemirror-schema-list")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/prosemirror-schema-list/-/prosemirror-schema-list-1.3.0.tgz")
              (sha256
               (base32
                "0ihv76fffw1nilvvpl61zm9i1k1fw84zrygc2xzpv7ifw83njvs2"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("prosemirror-state"
                                             "@prosemirror/buildhelper"
                                             "prosemirror-test-builder")))))))
    (inputs (list node-prosemirror-state-1.4.3
                  node-prosemirror-transform-1.7.3
                  node-prosemirror-model-1.19.2))
    (home-page "https://github.com/prosemirror/prosemirror-schema-list#readme")
    (synopsis "List-related schema elements and commands for ProseMirror")
    (description "List-related schema elements and commands for ProseMirror")
    (license license:expat)))

(define-public node-prosemirror-state-1.4.3
  (package
    (name "node-prosemirror-state")
    (version "1.4.3")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/prosemirror-state/-/prosemirror-state-1.4.3.tgz")
              (sha256
               (base32
                "122sp7qx20sfmg1j0p7z90qi1qzgp3brk3zslqkksf9py6mf5d86"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:modules ((guix build node-build-system)
                  (srfi srfi-1)
                  (ice-9 match)
                  (guix build utils))
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  ;; XXX: Copied from Guix.
                  (add-before 'configure 'avoid-prepare-scripts
                    (lambda _
                      (with-atomic-json-file-replacement "package.json"
                                                         (match-lambda
                                                           (((quote @) . pkg-meta-alist)
                                                            (cons '@
                                                                  (map (match-lambda
                                                                         (("scripts" @ . scripts-alist) `
                                                                          ("scripts"
                                                                           @
                                                                           ,@(filter (match-lambda
                                                                                       
                                                                                       (("prepare" . _)
                                                                                        #f)
                                                                                       
                                                                                       (_
                                                                                        #t))
                                                                              scripts-alist)))
                                                                         (other
                                                                          other))
                                                                   pkg-meta-alist)))))))
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@prosemirror/buildhelper"
                                             "prosemirror-test-builder"
                                             ;; Remove to avoid circular reference
                                             "prosemirror-view")))))))
    (inputs (list ;("node-prosemirror-view" ,node-prosemirror-view-1.31.5)
                  node-prosemirror-transform-1.7.3
                  node-prosemirror-model-1.19.2))
    (home-page "https://github.com/prosemirror/prosemirror-state#readme")
    (synopsis "ProseMirror editor state")
    (description "ProseMirror editor state")
    (license license:expat)))

(define-public node-prosemirror-tables-1.3.2
  (package
    (name "node-prosemirror-tables")
    (version "1.3.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/prosemirror-tables/-/prosemirror-tables-1.3.2.tgz")
              (sha256
               (base32
                "1vllpzrclnyq7wcykxfc661rlvzlixa6i69kx0n99p5446l9jvby"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@typescript-eslint/eslint-plugin"
                                             "@typescript-eslint/parser"
                                             "builddocs"
                                             "eslint"
                                             "eslint-plugin-jest"
                                             "ist"
                                             "prettier"
                                             "prosemirror-commands"
                                             "prosemirror-example-setup"
                                             "prosemirror-menu"
                                             "prosemirror-schema-basic"
                                             "prosemirror-test-builder"
                                             "tsup"
                                             "typescript"
                                             "vite"
                                             "vitest")))))))
    (inputs (list node-prosemirror-view-1.31.5
                  node-prosemirror-transform-1.7.3
                  node-prosemirror-state-1.4.3 node-prosemirror-model-1.19.2
                  node-prosemirror-keymap-1.2.2))
    (home-page "https://www.npmjs.com/package/node-prosemirror-tables")
    (synopsis "ProseMirror's rowspan/colspan tables component")
    (description "ProseMirror's rowspan/colspan tables component")
    (license license:expat)))

(define-public node-prosemirror-transform-1.7.3
  (package
    (name "node-prosemirror-transform")
    (version "1.7.3")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/prosemirror-transform/-/prosemirror-transform-1.7.3.tgz")
              (sha256
               (base32
                "1b0ipxk24smla430ljjls8llbq7dbd6imm1nrlxqmqf71sqll8km"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:modules ((guix build node-build-system)
                  (srfi srfi-1)
                  (ice-9 match)
                  (guix build utils))
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  ;; XXX: Copied from Guix.
                  (add-before 'configure 'avoid-prepare-scripts
                    (lambda _
                      (with-atomic-json-file-replacement "package.json"
                                                         (match-lambda
                                                           (((quote @) . pkg-meta-alist)
                                                            (cons '@
                                                                  (map (match-lambda
                                                                         (("scripts" @ . scripts-alist) `
                                                                          ("scripts"
                                                                           @
                                                                           ,@(filter (match-lambda
                                                                                       
                                                                                       (("prepare" . _)
                                                                                        #f)
                                                                                       
                                                                                       (_
                                                                                        #t))
                                                                              scripts-alist)))
                                                                         (other
                                                                          other))
                                                                   pkg-meta-alist)))))))
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@prosemirror/buildhelper"
                                             "prosemirror-test-builder")))))))
    (inputs (list node-prosemirror-model-1.19.2))
    (home-page "https://github.com/prosemirror/prosemirror-transform#readme")
    (synopsis "ProseMirror document transformations")
    (description "ProseMirror document transformations")
    (license license:expat)))

(define-public node-prosemirror-utils-0.9.6
  (package
    (name "node-prosemirror-utils")
    (version "0.9.6")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/prosemirror-utils/-/prosemirror-utils-0.9.6.tgz")
              (sha256
               (base32
                "095p3n1ak3j3lvmdymyp92sw4w3arkig4n55cnbdf5ara017cnj2"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("babel-core" "babel-jest"
                                             "babel-preset-env"
                                             "builddocs"
                                             "codecov"
                                             "husky"
                                             "jest"
                                             "jest-diff"
                                             "lint-staged"
                                             "prettier"
                                             "prosemirror-model"
                                             "prosemirror-schema-basic"
                                             "prosemirror-state"
                                             "prosemirror-tables"
                                             "prosemirror-test-builder"
                                             "prosemirror-view"
                                             "rollup"
                                             "rollup-plugin-babel")))))))
    (home-page "https://github.com/atlassian/prosemirror-utils#readme")
    (synopsis "Utils library for ProseMirror")
    (description "Utils library for ProseMirror")
    (license license:asl2.0)))

(define-public node-prosemirror-view-1.31.5
  (package
    (name "node-prosemirror-view")
    (version "1.31.5")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/prosemirror-view/-/prosemirror-view-1.31.5.tgz")
              (sha256
               (base32
                "116sgdgq6qx3qj1j96rp5xskrc3760vdavrnmlksk85virn8lspg"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:modules ((guix build node-build-system)
                  (srfi srfi-1)
                  (ice-9 match)
                  (guix build utils))
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  ;; XXX: Copied from Guix.
                  (add-before 'configure 'avoid-prepare-scripts
                    (lambda _
                      (with-atomic-json-file-replacement "package.json"
                                                         (match-lambda
                                                           (((quote @) . pkg-meta-alist)
                                                            (cons '@
                                                                  (map (match-lambda
                                                                         (("scripts" @ . scripts-alist) `
                                                                          ("scripts"
                                                                           @
                                                                           ,@(filter (match-lambda
                                                                                       
                                                                                       (("prepare" . _)
                                                                                        #f)
                                                                                       
                                                                                       (_
                                                                                        #t))
                                                                              scripts-alist)))
                                                                         (other
                                                                          other))
                                                                   pkg-meta-alist)))))))
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@prosemirror/buildhelper"
                                             "prosemirror-test-builder")))))))
    (inputs (list node-prosemirror-transform-1.7.3
                  node-prosemirror-state-1.4.3 node-prosemirror-model-1.19.2))
    (home-page "https://github.com/prosemirror/prosemirror-view#readme")
    (synopsis "ProseMirror's view component")
    (description "ProseMirror's view component")
    (license license:expat)))

(define-public node-proxy-addr-2.0.7
  (package
    (name "node-proxy-addr")
    (version "2.0.7")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/proxy-addr/-/proxy-addr-2.0.7.tgz")
              (sha256
               (base32
                "1na6xrmlga7qjd55gfhnp7m8qg43nynzg5ds54s76kkd9zrvdld0"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("benchmark" "beautify-benchmark"
                                             "deep-equal"
                                             "eslint"
                                             "eslint-config-standard"
                                             "eslint-plugin-import"
                                             "eslint-plugin-markdown"
                                             "eslint-plugin-node"
                                             "eslint-plugin-promise"
                                             "eslint-plugin-standard"
                                             "mocha"
                                             "nyc")))))))
    (inputs (list node-ipaddr-js-1.9.1 node-forwarded-0.2.0))
    (home-page "https://github.com/jshttp/proxy-addr#readme")
    (synopsis "Determine address of proxied request")
    (description "Determine address of proxied request")
    (license license:expat)))

(define-public node-psl-1.9.0
  (package
    (name "node-psl")
    (version "1.9.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/psl/-/psl-1.9.0.tgz")
              (sha256
               (base32
                "1qbng513j3yn8pjkbq4j5mr2ywz5gx4wwd3pzgyk3rwzkp77q0nc"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("browserify"
                                             "browserslist-browserstack"
                                             "browserstack-local"
                                             "chai"
                                             "commit-and-pr"
                                             "eslint"
                                             "JSONStream"
                                             "mocha"
                                             "porch"
                                             "request"
                                             "selenium-webdriver"
                                             "serve-handler"
                                             "uglify-js"
                                             "watchify")))))))
    (home-page "https://github.com/lupomontero/psl#readme")
    (synopsis "Domain name parser based on the Public Suffix List")
    (description "Domain name parser based on the Public Suffix List")
    (license license:expat)))

(define-public node-punycode-2.3.0
  (package
    (name "node-punycode")
    (version "2.3.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/punycode/-/punycode-2.3.0.tgz")
              (sha256
               (base32
                "0v449nngp3gdvic0x0i2p83mwwsr357mb6c84nrdf3gvhr78insg"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("codecov" "istanbul" "mocha")))))))
    (home-page "https://mths.be/punycode")
    (synopsis
     "A robust Punycode converter that fully complies to RFC 3492 and RFC 5891, and works on nearly all JavaScript platforms.")
    (description
     "A robust Punycode converter that fully complies to RFC 3492 and RFC 5891, and works on nearly all JavaScript platforms.")
    (license license:expat)))

(define-public node-pure-color-1.3.0
  (package
    (name "node-pure-color")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/pure-color/-/pure-color-1.3.0.tgz")
              (sha256
               (base32
                "1z6bacqcxn1pgxzlw0sw3abpixzi05dw3xpvj0fpjh0rqq4hgdqm"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://github.com/WickyNilliams/pure-color#readme")
    (synopsis "Pure functions for color conversion and parsing")
    (description "Pure functions for color conversion and parsing")
    (license license:expat)))

(define-public node-qs-6.11.0
  (package
    (name "node-qs")
    (version "6.11.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/qs/-/qs-6.11.0.tgz")
              (sha256
               (base32
                "0dg21qavayja9ij6vcl2kzfb1fszpribr5jg63fmjhmv9pj1rn81"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@ljharb/eslint-config" "aud"
                                             "browserify"
                                             "eclint"
                                             "eslint"
                                             "evalmd"
                                             "for-each"
                                             "has-symbols"
                                             "iconv-lite"
                                             "in-publish"
                                             "mkdirp"
                                             "npmignore"
                                             "nyc"
                                             "object-inspect"
                                             "qs-iconv"
                                             "safe-publish-latest"
                                             "safer-buffer"
                                             "tape")))))))
    (inputs (list node-side-channel-1.0.4))
    (home-page "https://github.com/ljharb/qs")
    (synopsis
     "A querystring parser that supports nesting and arrays, with a depth limit")
    (description
     "A querystring parser that supports nesting and arrays, with a depth limit")
    (license license:bsd-3)))

(define-public node-qs-6.5.3
  (package
    (name "node-qs")
    (version "6.5.3")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/qs/-/qs-6.5.3.tgz")
              (sha256
               (base32
                "09kz5lli2a4nl62i415wplzjgrnw82zpbm1a6crg15ar3g6d9cql"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@ljharb/eslint-config" "aud"
                                             "browserify"
                                             "eclint"
                                             "eslint"
                                             "evalmd"
                                             "iconv-lite"
                                             "in-publish"
                                             "mkdirp"
                                             "nyc"
                                             "qs-iconv"
                                             "safe-publish-latest"
                                             "safer-buffer"
                                             "tape")))))))
    (home-page "https://github.com/ljharb/qs")
    (synopsis
     "A querystring parser that supports nesting and arrays, with a depth limit")
    (description
     "A querystring parser that supports nesting and arrays, with a depth limit")
    (license license:bsd-3)))

(define-public node-randomatic-3.1.1
  (package
    (name "node-randomatic")
    (version "3.1.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/randomatic/-/randomatic-3.1.1.tgz")
              (sha256
               (base32
                "1jawbnz6qqy98w7v45yp1wby00ykwrpmai5bmvbkw1nw9q56cy0p"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ansi-bold" "benchmarked" "glob"
                                             "gulp-format-md" "mocha")))))))
    (inputs (list node-math-random-1.0.4 node-kind-of-6.0.3
                  node-is-number-4.0.0))
    (home-page "https://github.com/jonschlinkert/randomatic")
    (synopsis
     "Generate randomized strings of a specified length using simple character sequences. The original generate-password.")
    (description
     "Generate randomized strings of a specified length using simple character sequences. The original generate-password.")
    (license license:expat)))

(define-public node-range-parser-1.2.1
  (package
    (name "node-range-parser")
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/range-parser/-/range-parser-1.2.1.tgz")
              (sha256
               (base32
                "09prs852snwqr9cfcrybm7ysl0z1wka9dh4dwc4v1415cvi6cllh"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("deep-equal" "eslint"
                                             "eslint-config-standard"
                                             "eslint-plugin-markdown"
                                             "eslint-plugin-import"
                                             "eslint-plugin-node"
                                             "eslint-plugin-promise"
                                             "eslint-plugin-standard"
                                             "mocha"
                                             "nyc")))))))
    (home-page "https://github.com/jshttp/range-parser#readme")
    (synopsis "Range header field string parser")
    (description "Range header field string parser")
    (license license:expat)))

(define-public node-raw-body-2.5.1
  (package
    (name "node-raw-body")
    (version "2.5.1")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/raw-body/-/raw-body-2.5.1.tgz")
              (sha256
               (base32
                "08kbp3a2i8pmccvxz1j2g9lqqlrpfplrq062fwl1d75jpy6rr8s7"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("bluebird" "eslint"
                                             "eslint-config-standard"
                                             "eslint-plugin-import"
                                             "eslint-plugin-markdown"
                                             "eslint-plugin-node"
                                             "eslint-plugin-promise"
                                             "eslint-plugin-standard"
                                             "mocha"
                                             "nyc"
                                             "readable-stream"
                                             "safe-buffer")))))))
    (inputs (list node-unpipe-1.0.0 node-iconv-lite-0.4.24
                  node-http-errors-2.0.0 node-bytes-3.1.2))
    (home-page "https://github.com/stream-utils/raw-body#readme")
    (synopsis "Get and validate the raw body of a readable stream.")
    (description "Get and validate the raw body of a readable stream.")
    (license license:expat)))

(define-public node-react-16.14.0
  (package
    (name "node-react")
    (version "16.14.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/react/-/react-16.14.0.tgz")
              (sha256
               (base32
                "1qx52jqad4jxavq83x08gypw2jg7cidwm2zpc5gxkjshbqvdqmzy"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (inputs (list node-prop-types-15.8.1 node-object-assign-4.1.1
                  node-loose-envify-1.4.0))
    (home-page "https://reactjs.org/")
    (synopsis "React is a JavaScript library for building user interfaces.")
    (description "React is a JavaScript library for building user interfaces.")
    (license license:expat)))

(define-public node-react-18.2.0
  (package
    (name "node-react")
    (version "18.2.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/react/-/react-18.2.0.tgz")
              (sha256
               (base32
                "0z19cq1ywbhsj2py4jgz27yl38dbzhfnyvvs5gj605wixk5gfld1"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (inputs (list node-loose-envify-1.4.0))
    (home-page "https://reactjs.org/")
    (synopsis "React is a JavaScript library for building user interfaces.")
    (description "React is a JavaScript library for building user interfaces.")
    (license license:expat)))

(define-public node-react-base16-styling-0.5.3
  (package
    (name "node-react-base16-styling")
    (version "0.5.3")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/react-base16-styling/-/react-base16-styling-0.5.3.tgz")
              (sha256
               (base32
                "1w1xiymxqaa7ybyb4il9dg9nqv4bpaal3wj8ydq830501yf1n6nc"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "babel-cli"
                                             "babel-core"
                                             "babel-eslint"
                                             "babel-plugin-transform-flow-strip-types"
                                             "babel-plugin-transform-runtime"
                                             "babel-preset-es2015"
                                             "babel-preset-stage-0"
                                             "eslint"
                                             "eslint-plugin-babel"
                                             "eslint-plugin-flowtype"
                                             "flow-bin"
                                             "pre-commit")))))))
    (inputs (list node-pure-color-1.3.0 node-lodash-flow-3.5.0
                  node-lodash-curry-4.1.1 node-base16-1.0.0))
    (home-page "https://github.com/alexkuz/react-base16-styling#readme")
    (synopsis "React styling with base16 color scheme support")
    (description "React styling with base16 color scheme support")
    (license license:expat)))

(define-public node-react-dock-0.2.4
  (package
    (name "node-react-dock")
    (version "0.2.4")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/react-dock/-/react-dock-0.2.4.tgz")
              (sha256
               (base32
                "0s8q2j9iy046700rsy2xa7z58pz9v2a6baxr1jwjfrfizq5ba3w6"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("babel" "babel-cli"
                                             "babel-core"
                                             "babel-eslint"
                                             "babel-loader"
                                             "babel-plugin-transform-decorators-legacy"
                                             "babel-plugin-transform-runtime"
                                             "babel-preset-es2015"
                                             "babel-preset-react"
                                             "babel-preset-stage-0"
                                             "chai"
                                             "eslint"
                                             "eslint-loader"
                                             "eslint-plugin-babel"
                                             "eslint-plugin-react"
                                             "imports-loader"
                                             "json-loader"
                                             "mocha"
                                             "radium"
                                             "raw-loader"
                                             "react-bootstrap"
                                             "react-dom"
                                             "react-hot-loader"
                                             "react-pure-render"
                                             "webpack"
                                             "webpack-dev-server"
                                             "babel-runtime"
                                             "react")))))))
    (inputs (list node-prop-types-15.8.1 node-lodash-debounce-3.1.1
                  node-babel-runtime-6.26.0 node-react-18.2.0))
    (home-page "https://github.com/alexkuz/react-dock")
    (synopsis "Resizable dockable react component")
    (description "Resizable dockable react component")
    (license license:expat)))

(define-public node-react-dom-16.14.0
  (package
    (name "node-react-dom")
    (version "16.14.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/react-dom/-/react-dom-16.14.0.tgz")
              (sha256
               (base32
                "0dikgr72j7qc9gz60kvlqgmddwf2lnyzr47f1bayhk8gy638bl31"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("react")))))))
    (inputs (list node-scheduler-0.19.1 node-prop-types-15.8.1
                  node-object-assign-4.1.1 node-loose-envify-1.4.0
                  node-react-16.14.0))
    (home-page "https://reactjs.org/")
    (synopsis "React package for working with the DOM.")
    (description "React package for working with the DOM.")
    (license license:expat)))

(define-public node-react-dom-18.2.0
  (package
    (name "node-react-dom")
    (version "18.2.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/react-dom/-/react-dom-18.2.0.tgz")
              (sha256
               (base32
                "1vz263mx0vpykp53v04s7m6krimkqy2jlwzkh73n1y4ins31x9r0"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("react")))))))
    (inputs (list node-scheduler-0.23.0 node-loose-envify-1.4.0
                  node-react-18.2.0))
    (home-page "https://reactjs.org/")
    (synopsis "React package for working with the DOM.")
    (description "React package for working with the DOM.")
    (license license:expat)))

(define-public node-react-emotion-9.2.12
  (package
    (name "node-react-emotion")
    (version "9.2.12")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/react-emotion/-/react-emotion-9.2.12.tgz")
              (sha256
               (base32
                "0h8ap7ppsq9zmmwvv2gx52m00il52hf28bzgr0wx7qp1gml4pq6k"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@types/react" "dtslint"
                                             "emotion" "react")))))))
    (inputs (list node-create-emotion-styled-9.2.8
                  node-babel-plugin-emotion-9.2.11 node-react-16.14.0
                  node-emotion-9.2.12))
    (home-page "https://emotion.sh")
    (synopsis "The Next Generation of CSS-in-JS, for React projects.")
    (description "The Next Generation of CSS-in-JS, for React projects.")
    (license license:expat)))

(define-public node-react-is-16.13.1
  (package
    (name "node-react-is")
    (version "16.13.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/react-is/-/react-is-16.13.1.tgz")
              (sha256
               (base32
                "13zfzbq3z5cmh7fzvbzwm2ivcqg2crv97ww7162kxhfxfgabbnqb"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://reactjs.org/")
    (synopsis "Brand checking of React Elements.")
    (description "Brand checking of React Elements.")
    (license license:expat)))

(define-public node-react-json-tree-0.11.2
  (package
    (name "node-react-json-tree")
    (version "0.11.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/react-json-tree/-/react-json-tree-0.11.2.tgz")
              (sha256
               (base32
                "0bnpdp1cj1gbxp0z664jb2j6api6h6g8g4y8bkvrwvnnyynf7hb1"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:modules ((guix build node-build-system)
                  (srfi srfi-1)
                  (ice-9 match)
                  (guix build utils))
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-before 'configure 'avoid-prepare-scripts
                    (lambda _
                      ;; We need to remove the prepare script from "package.json", as
                      ;; it would try to use the build environment and would block the
                      ;; automatic building by other packages making use of node-acorn.
                      ;; TODO: Add utility function
                      (with-atomic-json-file-replacement "package.json"
                                                         (match-lambda
                                                           (((quote @) . pkg-meta-alist)
                                                            (cons '@
                                                                  (map (match-lambda
                                                                         (("scripts" @ . scripts-alist) `
                                                                          ("scripts"
                                                                           @
                                                                           ,@(filter (match-lambda
                                                                                       
                                                                                       (("prepare" . _)
                                                                                        #f)
                                                                                       
                                                                                       (_
                                                                                        #t))
                                                                              scripts-alist)))
                                                                         (other
                                                                          other))
                                                                   pkg-meta-alist)))))))
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("babel-cli" "babel-core"
                                             "babel-eslint"
                                             "babel-loader"
                                             "babel-plugin-transform-class-properties"
                                             "babel-plugin-transform-es3-member-expression-literals"
                                             "babel-plugin-transform-es3-property-literals"
                                             "babel-plugin-transform-object-rest-spread"
                                             "babel-plugin-transform-runtime"
                                             "babel-preset-env"
                                             "babel-preset-react"
                                             "eslint"
                                             "eslint-config-prettier"
                                             "eslint-config-standard"
                                             "eslint-plugin-babel"
                                             "eslint-plugin-import"
                                             "eslint-plugin-node"
                                             "eslint-plugin-prettier"
                                             "eslint-plugin-promise"
                                             "eslint-plugin-react"
                                             "eslint-plugin-standard"
                                             "expect"
                                             "husky"
                                             "isparta"
                                             "lint-staged"
                                             "mocha"
                                             "prettier"
                                             "react"
                                             "react-dom"
                                             "react-test-renderer"
                                             "rimraf"
                                             "terser-webpack-plugin"
                                             "webpack"
                                             "webpack-cli")))))))
    (inputs (list node-react-base16-styling-0.5.3 node-prop-types-15.8.1
                  node-babel-runtime-6.26.0 node-react-dom-16.14.0
                  node-react-16.14.0))
    (home-page "https://github.com/reduxjs/redux-devtools")
    (synopsis "React JSON Viewer Component, Extracted from redux-devtools")
    (description "React JSON Viewer Component, Extracted from redux-devtools")
    (license license:expat)))

(define-public node-react-textarea-autosize-8.5.0
  (package
    (name "node-react-textarea-autosize")
    (version "8.5.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/react-textarea-autosize/-/react-textarea-autosize-8.5.0.tgz")
              (sha256
               (base32
                "1vwkirh1594kzbw041bgmp3zfcqlq1xvpkaqb4k3jqj01p85c91y"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@babel/core"
                                             "@babel/plugin-proposal-object-rest-spread"
                                             "@babel/plugin-transform-runtime"
                                             "@babel/preset-env"
                                             "@babel/preset-react"
                                             "@babel/preset-typescript"
                                             "@changesets/changelog-github"
                                             "@changesets/cli"
                                             "@preconstruct/cli"
                                             "@testing-library/jest-dom"
                                             "@testing-library/react"
                                             "@types/react"
                                             "@types/react-dom"
                                             "@typescript-eslint/eslint-plugin"
                                             "@typescript-eslint/parser"
                                             "babel-eslint"
                                             "bytes"
                                             "cross-env"
                                             "eslint"
                                             "eslint-config-prettier"
                                             "eslint-plugin-prettier"
                                             "eslint-plugin-react"
                                             "husky"
                                             "jest"
                                             "jest-environment-jsdom"
                                             "lint-staged"
                                             "parcel"
                                             "prettier"
                                             "react"
                                             "react-dom"
                                             "rimraf"
                                             "terser"
                                             "typescript")))))))
    (inputs (list node-use-latest-1.2.1 node-use-composed-ref-1.3.0
                  node-babel-runtime-7.22.5 node-react-18.2.0))
    (home-page "https://github.com/Andarist/react-textarea-autosize#readme")
    (synopsis "textarea component for React which grows with content")
    (description "textarea component for React which grows with content")
    (license license:expat)))

(define-public node-react-window-1.8.9
  (package
    (name "node-react-window")
    (version "1.8.9")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/react-window/-/react-window-1.8.9.tgz")
              (sha256
               (base32
                "1n30n24qp7yili81rrr66n37qz9ai2d59072gcdd47pz818m67j9"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@babel/core"
                                             "@babel/plugin-proposal-class-properties"
                                             "@babel/plugin-transform-runtime"
                                             "@babel/preset-env"
                                             "@babel/preset-flow"
                                             "babel-core"
                                             "babel-eslint"
                                             "babel-plugin-annotate-pure-calls"
                                             "cross-env"
                                             "del-cli"
                                             "eslint"
                                             "eslint-config-prettier"
                                             "eslint-config-react-app"
                                             "eslint-config-standard"
                                             "eslint-config-standard-react"
                                             "eslint-plugin-flowtype"
                                             "eslint-plugin-import"
                                             "eslint-plugin-jsx-a11y"
                                             "eslint-plugin-node"
                                             "eslint-plugin-prettier"
                                             "eslint-plugin-promise"
                                             "eslint-plugin-react"
                                             "eslint-plugin-standard"
                                             "flow-bin"
                                             "gh-pages"
                                             "lint-staged"
                                             "prettier"
                                             "react"
                                             "react-dom"
                                             "react-is"
                                             "react-scripts"
                                             "react-test-renderer"
                                             "rollup"
                                             "rollup-plugin-babel"
                                             "rollup-plugin-commonjs"
                                             "rollup-plugin-node-resolve"
                                             "rollup-plugin-replace"
                                             "rollup-plugin-terser")))))))
    (inputs (list node-memoize-one-5.2.1 node-babel-runtime-7.22.5
                  node-react-dom-18.2.0 node-react-18.2.0))
    (home-page "http://react-window.now.sh/")
    (synopsis
     "React components for efficiently rendering large, scrollable lists and tabular data")
    (description
     "React components for efficiently rendering large, scrollable lists and tabular data")
    (license license:expat)))

(define-public node-readable-stream-2.3.8
  (package
    (name "node-readable-stream")
    (version "2.3.8")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/readable-stream/-/readable-stream-2.3.8.tgz")
              (sha256
               (base32
                "0cm5g4a5mfqb5im98mzv5y3dpv377bws6wlgpv5wk2pn90wn2j0n"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("assert" "babel-polyfill"
                                             "buffer"
                                             "lolex"
                                             "nyc"
                                             "tap"
                                             "tape")))))))
    (inputs (list node-util-deprecate-1.0.2
                  node-string-decoder-1.1.1
                  node-safe-buffer-5.1.2
                  node-process-nextick-args-2.0.1
                  node-isarray-1.0.0
                  node-inherits-2.0.4
                  node-core-util-is-1.0.3))
    (home-page "https://github.com/nodejs/readable-stream#readme")
    (synopsis "Streams3, a user-land copy of the stream library from Node.js")
    (description
     "Streams3, a user-land copy of the stream library from Node.js")
    (license license:expat)))

(define-public node-readable-stream-3.6.2
  (package
    (name "node-readable-stream")
    (version "3.6.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/readable-stream/-/readable-stream-3.6.2.tgz")
              (sha256
               (base32
                "0pdb0mrh95ks672ikgj8frx9nh078bfyngknj70ak2iibv06dn7d"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@babel/cli" "@babel/core"
                                             "@babel/polyfill"
                                             "@babel/preset-env"
                                             "airtap"
                                             "assert"
                                             "bl"
                                             "deep-strict-equal"
                                             "events.once"
                                             "glob"
                                             "gunzip-maybe"
                                             "hyperquest"
                                             "lolex"
                                             "nyc"
                                             "pump"
                                             "rimraf"
                                             "tap"
                                             "tape"
                                             "tar-fs"
                                             "util-promisify")))))))
    (inputs (list node-util-deprecate-1.0.2 node-string-decoder-1.3.0
                  node-inherits-2.0.4))
    (home-page "https://github.com/nodejs/readable-stream#readme")
    (synopsis "Streams3, a user-land copy of the stream library from Node.js")
    (description
     "Streams3, a user-land copy of the stream library from Node.js")
    (license license:expat)))

(define-public node-readdirp-2.2.1
  (package
    (name "node-readdirp")
    (version "2.2.1")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/readdirp/-/readdirp-2.2.1.tgz")
              (sha256
               (base32
                "18vnhdirs6khbbzwfkclgff6yqdhb55gs0bp3p9k41lpq2fk18n9"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("nave" "proxyquire" "tap"
                                             "through2")))))))
    (inputs (list node-readable-stream-2.3.8 node-micromatch-3.1.10
                  node-graceful-fs-4.2.11))
    (home-page "https://github.com/paulmillr/readdirp")
    (synopsis "Recursive version of fs.readdir with streaming api.")
    (description "Recursive version of fs.readdir with streaming api.")
    (license license:expat)))

(define-public node-realm-utils-1.0.9
  (package
    (name "node-realm-utils")
    (version "1.0.9")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/realm-utils/-/realm-utils-1.0.9.tgz")
              (sha256
               (base32
                "06vl01rm2sxbjmdfwr4m736khcppq4nc0wn2apcg8dqgcvad09y9"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("babel-preset-es2015" "gulp"
                                             "gulp-babel"
                                             "gulp-bump"
                                             "gulp-concat"
                                             "gulp-rename"
                                             "gulp-sourcemaps"
                                             "gulp-typescript"
                                             "gulp-util"
                                             "merge2"
                                             "mocha"
                                             "run-sequence"
                                             "should"
                                             "ts-universal")))))))
    (inputs (list node-mkdirp-0.5.6 node-app-root-path-1.4.0))
    (home-page "https://github.com/realm-js/realm-utils#readme")
    (synopsis
     "Realm-js has a set of functionality that helps solving many problems or impediments related to Promises. Utilities live in this repository, apart from realm-js library. Typings included ### Install")
    (description
     "Realm-js has a set of functionality that helps solving many problems or impediments related to Promises. Utilities live in this repository, apart from realm-js library. Typings included ### Install")
    (license license:isc)))

(define-public node-regenerate-1.4.2
  (package
    (name "node-regenerate")
    (version "1.4.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/regenerate/-/regenerate-1.4.2.tgz")
              (sha256
               (base32
                "07km6in83r5da81jn43ps2gll0gpdykcy0aa94v3vlwh83ygqy33"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("codecov" "grunt"
                                             "grunt-shell"
                                             "istanbul"
                                             "qunit-extras"
                                             "qunitjs"
                                             "requirejs")))))))
    (home-page "https://mths.be/regenerate")
    (synopsis
     "Generate JavaScript-compatible regular expressions based on a given set of Unicode symbols or code points.")
    (description
     "Generate JavaScript-compatible regular expressions based on a given set of Unicode symbols or code points.")
    (license license:expat)))

(define-public node-regenerate-unicode-properties-9.0.0
  (package
    (name "node-regenerate-unicode-properties")
    (version "9.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/regenerate-unicode-properties/-/regenerate-unicode-properties-9.0.0.tgz")
              (sha256
               (base32
                "0k5m9a5fq2ddc0qrdzjw5i339nc3bhz7i576zfisnnwgmlbij9nd"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "fs-extra" "jsesc"
                                             "@unicode/unicode-14.0.0"
                                             "unicode-canonical-property-names-ecmascript")))))))
    (inputs (list node-regenerate-1.4.2))
    (home-page
     "https://github.com/mathiasbynens/regenerate-unicode-properties")
    (synopsis "Regenerate sets for Unicode properties and values.")
    (description "Regenerate sets for Unicode properties and values.")
    (license license:expat)))

(define-public node-regenerator-runtime-0.11.1
  (package
    (name "node-regenerator-runtime")
    (version "0.11.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/regenerator-runtime/-/regenerator-runtime-0.11.1.tgz")
              (sha256
               (base32
                "0frppih4qlrkvl1xgyd9zpgxs3fp0sarpcywwn7v1dmkhidrin6d"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://www.npmjs.com/package/node-regenerator-runtime")
    (synopsis
     "Runtime for Regenerator-compiled generator and async functions.")
    (description
     "Runtime for Regenerator-compiled generator and async functions.")
    (license license:expat)))

(define-public node-regenerator-runtime-0.13.11
  (package
    (name "node-regenerator-runtime")
    (version "0.13.11")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/regenerator-runtime/-/regenerator-runtime-0.13.11.tgz")
              (sha256
               (base32
                "131b9kq9m176s787asppci2kihmpvmhdqxfp2zbjqgdqrr4si84n"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://github.com/facebook/regenerator/tree/main#readme")
    (synopsis
     "Runtime for Regenerator-compiled generator and async functions.")
    (description
     "Runtime for Regenerator-compiled generator and async functions.")
    (license license:expat)))

(define-public node-regex-cache-0.4.4
  (package
    (name "node-regex-cache")
    (version "0.4.4")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/regex-cache/-/regex-cache-0.4.4.tgz")
              (sha256
               (base32
                "100lg733jk0j378l77n72w5qfqzyrnacf744y2icp170wwsrvm36"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ansi-bold" "benchmarked"
                                             "gulp-format-md" "micromatch"
                                             "should")))))))
    (inputs (list node-is-equal-shallow-0.1.3))
    (home-page "https://github.com/jonschlinkert/regex-cache")
    (synopsis
     "Memoize the results of a call to the RegExp constructor, avoiding repetitious runtime compilation of the same string and options, resulting in surprising performance improvements.")
    (description
     "Memoize the results of a call to the RegExp constructor, avoiding repetitious runtime compilation of the same string and options, resulting in surprising performance improvements.")
    (license license:expat)))

(define-public node-regex-not-1.0.2
  (package
    (name "node-regex-not")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/regex-not/-/regex-not-1.0.2.tgz")
              (sha256
               (base32
                "0xpjprkrk0c9fn6yhqay3bm8vw44k197smcfby89g3sfjsxlhi7s"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp-format-md" "mocha")))))))
    (inputs (list node-safe-regex-1.1.0 node-extend-shallow-3.0.2))
    (home-page "https://github.com/jonschlinkert/regex-not")
    (synopsis
     "Create a javascript regular expression for matching everything except for the given string.")
    (description
     "Create a javascript regular expression for matching everything except for the given string.")
    (license license:expat)))

(define-public node-regexpu-core-4.8.0
  (package
    (name "node-regexpu-core")
    (version "4.8.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/regexpu-core/-/regexpu-core-4.8.0.tgz")
              (sha256
               (base32
                "0wsk7lkkaf8rs3lmmm7mjvjr6bcnsfsvyd607b15xyrk32k6k8hv"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("codecov" "istanbul"
                                             "jsesc"
                                             "lodash"
                                             "mocha"
                                             "regexpu-fixtures"
                                             "@unicode/unicode-14.0.0")))))))
    (inputs (list node-unicode-match-property-value-ecmascript-2.1.0
                  node-unicode-match-property-ecmascript-2.0.0
                  node-regjsparser-0.7.0
                  node-regjsgen-0.5.2
                  node-regenerate-unicode-properties-9.0.0
                  node-regenerate-1.4.2))
    (home-page "https://mths.be/regexpu")
    (synopsis
     "regexpuâs core functionality (i.e. `rewritePattern(pattern, flag)`), capable of translating ES6 Unicode regular expressions to ES5.")
    (description
     "regexpuâs core functionality (i.e. `rewritePattern(pattern, flag)`), capable of translating ES6 Unicode regular expressions to ES5.")
    (license license:expat)))

(define-public node-regjsgen-0.5.2
  (package
    (name "node-regjsgen")
    (version "0.5.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/regjsgen/-/regjsgen-0.5.2.tgz")
              (sha256
               (base32
                "1rcqz05jgsfq3v23aaryhfxkk2fcj4d22ah46dyg91zgjhpii74r"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("codecov" "nyc" "regjsparser"
                                             "request")))))))
    (home-page "https://github.com/bnjmnt4n/regjsgen")
    (synopsis "Generate regular expressions from regjsparserâs AST.")
    (description "Generate regular expressions from regjsparserâs AST.")
    (license license:expat)))

(define-public node-regjsparser-0.7.0
  (package
    (name "node-regjsparser")
    (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/regjsparser/-/regjsparser-0.7.0.tgz")
              (sha256
               (base32
                "08x6i1yqccwik7ryk8n3vx6b3nb2bskp5ap4xkznfz2y1fqsf9ad"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("regenerate" "unicode-11.0.0")))))))
    (inputs (list node-jsesc-0.5.0))
    (home-page "https://github.com/jviereck/regjsparser")
    (synopsis "Parsing the JavaScript's RegExp in JavaScript.")
    (description "Parsing the JavaScript's RegExp in JavaScript.")
    (license license:bsd-2)))

(define-public node-remove-trailing-separator-1.1.0
  (package
    (name "node-remove-trailing-separator")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/remove-trailing-separator/-/remove-trailing-separator-1.1.0.tgz")
              (sha256
               (base32
                "08b3msz9s5kw1alivgn9saaz6w04grjqppkdk3qbr7blk38l04sf"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "coveralls" "nyc" "xo")))))))
    (home-page "https://github.com/darsain/remove-trailing-separator#readme")
    (synopsis "Removes separators from the end of the string.")
    (description "Removes separators from the end of the string.")
    (license license:isc)))

(define-public node-repeat-element-1.1.4
  (package
    (name "node-repeat-element")
    (version "1.1.4")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/repeat-element/-/repeat-element-1.1.4.tgz")
              (sha256
               (base32
                "0iy9kfkd7dj9lh1pk7ffq8yxh4v0kb96azkj7yg0g9p34bab0vrn"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("benchmarked" "chalk" "glob"
                                             "gulp-format-md" "minimist"
                                             "mocha")))))))
    (home-page "https://github.com/jonschlinkert/repeat-element")
    (synopsis "Create an array by repeating the given value n times.")
    (description "Create an array by repeating the given value n times.")
    (license license:expat)))

(define-public node-repeat-string-1.6.1
  (package
    (name "node-repeat-string")
    (version "1.6.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/repeat-string/-/repeat-string-1.6.1.tgz")
              (sha256
               (base32
                "1zmlk22rp97i5yfxqlb9hix87zlznngd60pm8qwhcg6bssacpq8b"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ansi-cyan" "benchmarked"
                                             "gulp-format-md"
                                             "isobject"
                                             "mocha"
                                             "repeating"
                                             "text-table"
                                             "yargs-parser")))))))
    (home-page "https://github.com/jonschlinkert/repeat-string")
    (synopsis
     "Repeat the given string n times. Fastest implementation for repeating a string.")
    (description
     "Repeat the given string n times. Fastest implementation for repeating a string.")
    (license license:expat)))

(define-public node-request-2.88.2
  (package
    (name "node-request")
    (version "2.88.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/request/-/request-2.88.2.tgz")
              (sha256
               (base32
                "0hj2f9qqn3hpzpvhsnbwhzjyn5f8aicjz5wn00q0mfc4824awvg8"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("bluebird" "browserify"
                                             "browserify-istanbul"
                                             "buffer-equal"
                                             "codecov"
                                             "coveralls"
                                             "function-bind"
                                             "karma"
                                             "karma-browserify"
                                             "karma-cli"
                                             "karma-coverage"
                                             "karma-phantomjs-launcher"
                                             "karma-tap"
                                             "nyc"
                                             "phantomjs-prebuilt"
                                             "rimraf"
                                             "server-destroy"
                                             "standard"
                                             "tape"
                                             "taper")))))))
    (inputs (list node-uuid-3.4.0
                  node-tunnel-agent-0.6.0
                  node-tough-cookie-2.5.0
                  node-safe-buffer-5.2.1
                  node-qs-6.5.3
                  node-performance-now-2.1.0
                  node-oauth-sign-0.9.0
                  node-mime-types-2.1.35
                  node-json-stringify-safe-5.0.1
                  node-isstream-0.1.2
                  node-is-typedarray-1.0.0
                  node-http-signature-1.2.0
                  node-har-validator-5.1.5
                  node-form-data-2.3.3
                  node-forever-agent-0.6.1
                  node-extend-3.0.2
                  node-combined-stream-1.0.8
                  node-caseless-0.12.0
                  node-aws4-1.12.0
                  node-aws-sign2-0.7.0))
    (home-page "https://github.com/request/request#readme")
    (synopsis "Simplified HTTP request client.")
    (description "Simplified HTTP request client.")
    (license license:asl2.0)))

(define-public node-require-directory-2.1.1
  (package
    (name "node-require-directory")
    (version "2.1.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/require-directory/-/require-directory-2.1.1.tgz")
              (sha256
               (base32
                "1j46ydacaai73mx5krskl0k78r32lnjx94l79bz860rn8h4fwfvh"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("jshint" "mocha")))))))
    (home-page "https://github.com/troygoode/node-require-directory/")
    (synopsis
     "Recursively iterates over specified directory, require()'ing each file, and returning a nested hash structure containing those modules.")
    (description
     "Recursively iterates over specified directory, require()'ing each file, and returning a nested hash structure containing those modules.")
    (license license:expat)))

(define-public node-resolve-1.22.3
  (package
    (name "node-resolve")
    (version "1.22.3")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/resolve/-/resolve-1.22.3.tgz")
              (sha256
               (base32
                "1dxpqlkldla6jrwxp5bip4kr9521fwf3xb316j7wnjxb89mxyfs4"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@ljharb/eslint-config"
                                             "array.prototype.map"
                                             "aud"
                                             "copy-dir"
                                             "eclint"
                                             "eslint"
                                             "in-publish"
                                             "mkdirp"
                                             "mv"
                                             "npmignore"
                                             "object-keys"
                                             "rimraf"
                                             "safe-publish-latest"
                                             "semver"
                                             "tap"
                                             "tape"
                                             "tmp")))))))
    (inputs (list node-supports-preserve-symlinks-flag-1.0.0
                  node-path-parse-1.0.7 node-is-core-module-2.12.1))
    (home-page "https://github.com/browserify/resolve#readme")
    (synopsis
     "resolve like require.resolve() on behalf of files asynchronously and synchronously")
    (description
     "resolve like require.resolve() on behalf of files asynchronously and synchronously")
    (license license:expat)))

(define-public node-resolve-from-4.0.0
  (package
    (name "node-resolve-from")
    (version "4.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/resolve-from/-/resolve-from-4.0.0.tgz")
              (sha256
               (base32
                "1p11030pz8qdm9x2d9q0qi2p329447i2bb7a5j7hbsxxqbs2hhi4"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "xo")))))))
    (home-page "https://github.com/sindresorhus/resolve-from#readme")
    (synopsis
     "Resolve the path of a module like `require.resolve()` but from a given path")
    (description
     "Resolve the path of a module like `require.resolve()` but from a given path")
    (license license:expat)))

(define-public node-resolve-url-0.2.1
  (package
    (name "node-resolve-url")
    (version "0.2.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/resolve-url/-/resolve-url-0.2.1.tgz")
              (sha256
               (base32
                "090qal7agjs8d6x98jrf0wzgx5j85ksbkb3c85f14wv3idbwpsc8"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("testling" "jshint" "tape")))))))
    (home-page "https://github.com/lydell/resolve-url")
    (synopsis "Like Node.jsâ `path.resolve`/`url.resolve` for the browser.")
    (description
     "Like Node.jsâ `path.resolve`/`url.resolve` for the browser.")
    (license license:expat)))

(define-public node-restore-cursor-2.0.0
  (package
    (name "node-restore-cursor")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/restore-cursor/-/restore-cursor-2.0.0.tgz")
              (sha256
               (base32
                "18vaswnzkljbawjl88k4pr0ww4mqmmc0x8l2xrpfkvyh7jpck2cr"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (inputs (list node-signal-exit-3.0.7 node-onetime-2.0.1))
    (home-page "https://github.com/sindresorhus/restore-cursor#readme")
    (synopsis "Gracefully restore the CLI cursor on exit")
    (description "Gracefully restore the CLI cursor on exit")
    (license license:expat)))

(define-public node-ret-0.1.15
  (package
    (name "node-ret")
    (version "0.1.15")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/ret/-/ret-0.1.15.tgz")
              (sha256
               (base32
                "1zk9xw3jzs7di9b31sxg3fi0mljac8w09k0q6m6y311i1fsn4x2a"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("istanbul" "vows")))))))
    (home-page "https://github.com/fent/ret.js#readme")
    (synopsis "Tokenizes a string that represents a regular expression.")
    (description "Tokenizes a string that represents a regular expression.")
    (license license:expat)))

(define-public node-rimraf-3.0.2
  (package
    (name "node-rimraf")
    (version "3.0.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/rimraf/-/rimraf-3.0.2.tgz")
              (sha256
               (base32
                "0lkzjyxjij6ssh5h2l3ncp0zx00ylzhww766dq2vf1s7v07w4xjq"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("mkdirp" "tap")))))))
    (inputs (list node-glob-7.2.3))
    (home-page "https://github.com/isaacs/rimraf#readme")
    (synopsis "A deep deletion module for node (like `rm -rf`)")
    (description "A deep deletion module for node (like `rm -rf`)")
    (license license:isc)))

(define-public node-rope-sequence-1.3.4
  (package
    (name "node-rope-sequence")
    (version "1.3.4")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/rope-sequence/-/rope-sequence-1.3.4.tgz")
              (sha256
               (base32
                "0z063y557n638082cx5lmy7wv8ka5qbcm6x2cfdva8as5vh3wj1a"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:modules ((guix build node-build-system)
                  (srfi srfi-1)
                  (ice-9 match)
                  (guix build utils))
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-before 'configure 'avoid-prepare-scripts
                    (lambda _
                      ;; We need to remove the prepare script from "package.json", as
                      ;; it would try to use the build environment and would block the
                      ;; automatic building by other packages making use of node-acorn.
                      ;; TODO: Add utility function
                      (with-atomic-json-file-replacement "package.json"
                                                         (match-lambda
                                                           (((quote @) . pkg-meta-alist)
                                                            (cons '@
                                                                  (map (match-lambda
                                                                         (("scripts" @ . scripts-alist) `
                                                                          ("scripts"
                                                                           @
                                                                           ,@(filter (match-lambda
                                                                                       
                                                                                       (("prepare" . _)
                                                                                        #f)
                                                                                       
                                                                                       (_
                                                                                        #t))
                                                                              scripts-alist)))
                                                                         (other
                                                                          other))
                                                                   pkg-meta-alist)))))))
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@rollup/plugin-buble" "rollup")))))))
    (home-page "https://github.com/marijnh/rope-sequence#readme")
    (synopsis "Rope-based persistent sequence type")
    (description "Rope-based persistent sequence type")
    (license license:expat)))

(define-public node-run-async-2.4.1
  (package
    (name "node-run-async")
    (version "2.4.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/run-async/-/run-async-2.4.1.tgz")
              (sha256
               (base32
                "0kwddnvq40f38jzgvz9cxs0jk730nx3gfcdb3xvm7ma05sf3mp0f"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("mocha")))))))
    (home-page "https://github.com/SBoudrias/run-async#readme")
    (synopsis
     "Utility method to run function either synchronously or asynchronously using the common `this.async()` style.")
    (description
     "Utility method to run function either synchronously or asynchronously using the common `this.async()` style.")
    (license license:expat)))

(define-public node-rx-lite-4.0.8
  (package
    (name "node-rx-lite")
    (version "4.0.8")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/rx-lite/-/rx-lite-4.0.8.tgz")
              (sha256
               (base32
                "1w1vdy83nwglrai37qsapwrrixvc5yr490rch6xzkjgcbw0cdpjp"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://github.com/Reactive-Extensions/RxJS")
    (synopsis
     "Lightweight library for composing asynchronous and event-based operations in JavaScript")
    (description
     "Lightweight library for composing asynchronous and event-based operations in JavaScript")
    (license #f)))

(define-public node-rx-lite-aggregates-4.0.8
  (package
    (name "node-rx-lite-aggregates")
    (version "4.0.8")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/rx-lite-aggregates/-/rx-lite-aggregates-4.0.8.tgz")
              (sha256
               (base32
                "05z44a4zcwfi2yby9d3ik4msjh23nmpq79py80w6wvzd73iysj4q"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (inputs (list node-rx-lite-4.0.8))
    (home-page "https://github.com/Reactive-Extensions/RxJS")
    (synopsis
     "Lightweight library with aggregate functions for composing asynchronous and event-based operations in JavaScript")
    (description
     "Lightweight library with aggregate functions for composing asynchronous and event-based operations in JavaScript")
    (license #f)))

(define-public node-safe-buffer-5.1.2
  (package
    (name "node-safe-buffer")
    (version "5.1.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/safe-buffer/-/safe-buffer-5.1.2.tgz")
              (sha256
               (base32
                "08ma0a2a9j537bxl7qd2dn6sjcdhrclpdbslr19bkbyc1z30d4p0"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("standard" "tape")))))))
    (home-page "https://github.com/feross/safe-buffer")
    (synopsis "Safer Node.js Buffer API")
    (description "Safer Node.js Buffer API")
    (license license:expat)))

(define-public node-safe-buffer-5.2.1
  node-safe-buffer)

(define-public node-safe-regex-1.1.0
  (package
    (name "node-safe-regex")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/safe-regex/-/safe-regex-1.1.0.tgz")
              (sha256
               (base32
                "1lkcz58mjp3nfdiydh4iynpcfgxhf5mr339swzi5k05sikx8j4k2"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("tape")))))))
    (inputs (list node-ret-0.1.15))
    (home-page "https://github.com/substack/safe-regex")
    (synopsis
     "detect possibly catastrophic, exponential-time regular expressions")
    (description
     "detect possibly catastrophic, exponential-time regular expressions")
    (license license:expat)))

(define-public node-safer-buffer-2.1.2
  (package
    (name "node-safer-buffer")
    (version "2.1.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/safer-buffer/-/safer-buffer-2.1.2.tgz")
              (sha256
               (base32
                "1cx383s7vchfac8jlg3mnb820hkgcvhcpfn9w4f0g61vmrjjz0bq"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("standard" "tape")))))))
    (home-page "https://github.com/ChALkeR/safer-buffer#readme")
    (synopsis "Modern Buffer API polyfill without footguns")
    (description "Modern Buffer API polyfill without footguns")
    (license license:expat)))

(define-public node-scheduler-0.19.1
  (package
    (name "node-scheduler")
    (version "0.19.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/scheduler/-/scheduler-0.19.1.tgz")
              (sha256
               (base32
                "1s9igfdyabpnmcn9pk7f5p5wlvhprnrib07d2fa0qppiq0j5jxly"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (inputs (list node-object-assign-4.1.1 node-loose-envify-1.4.0))
    (home-page "https://reactjs.org/")
    (synopsis "Cooperative scheduler for the browser environment.")
    (description "Cooperative scheduler for the browser environment.")
    (license license:expat)))

(define-public node-scheduler-0.20.2
  (package
    (name "node-scheduler")
    (version "0.20.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/scheduler/-/scheduler-0.20.2.tgz")
              (sha256
               (base32
                "1ihlq27pq5m19g5h389c0vk7dm98d8jw7n4w42xl2pikpl7zpnpg"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (inputs (list node-object-assign-4.1.1 node-loose-envify-1.4.0))
    (home-page "https://reactjs.org/")
    (synopsis "Cooperative scheduler for the browser environment.")
    (description "Cooperative scheduler for the browser environment.")
    (license license:expat)))

(define-public node-scheduler-0.23.0
  (package
    (name "node-scheduler")
    (version "0.23.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/scheduler/-/scheduler-0.23.0.tgz")
              (sha256
               (base32
                "17wi8ln2hcgpfab5c92hzissf25lm0zv1waw7hfpdpsj6q4dq32g"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (inputs (list node-loose-envify-1.4.0))
    (home-page "https://reactjs.org/")
    (synopsis "Cooperative scheduler for the browser environment.")
    (description "Cooperative scheduler for the browser environment.")
    (license license:expat)))

(define-public node-select-1.1.2
  (package
    (name "node-select")
    (version "1.1.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/select/-/select-1.1.2.tgz")
              (sha256
               (base32
                "159mk5g9gjv7qvxwk4yp3h0y3s2wawvvh0ms66fr2z79zdzgi6v6"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("browserify" "chai"
                                             "karma"
                                             "karma-browserify"
                                             "karma-chai"
                                             "karma-mocha"
                                             "karma-phantomjs-launcher"
                                             "mocha"
                                             "phantomjs")))))))
    (home-page "https://github.com/zenorocha/select#readme")
    (synopsis "Programmatically select the text of a HTML element")
    (description "Programmatically select the text of a HTML element")
    (license license:expat)))

(define-public node-semver-6.3.0
  (package
    (name "node-semver")
    (version "6.3.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/semver/-/semver-6.3.0.tgz")
              (sha256
               (base32
                "0ib79jd27krndmai3hg1rzr6nzwcj4crbnff4qjvchrq6grlmwbb"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("tap")))))))
    (home-page "https://github.com/npm/node-semver#readme")
    (synopsis "The semantic version parser used by npm.")
    (description "The semantic version parser used by npm.")
    (license license:isc)))

(define-public node-semver-7.5.1
  (package
    (name "node-semver")
    (version "7.5.1")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/semver/-/semver-7.5.1.tgz")
              (sha256
               (base32
                "1iqrkrbkd1ddf8nzkpkdm8vw4xl1qg04j1118mhgcrsdjgpd0rkl"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@npmcli/eslint-config"
                                             "@npmcli/template-oss" "tap")))))))
    (inputs (list node-lru-cache-6.0.0))
    (home-page "https://github.com/npm/node-semver#readme")
    (synopsis "The semantic version parser used by npm.")
    (description "The semantic version parser used by npm.")
    (license license:isc)))

(define-public node-send-0.18.0
  (package
    (name "node-send")
    (version "0.18.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/send/-/send-0.18.0.tgz")
              (sha256
               (base32
                "050vp002qp8myzhjkb1fjafvhb3hz59pgli5k5kc8z7d16gp56pn"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("after" "eslint"
                                             "eslint-config-standard"
                                             "eslint-plugin-import"
                                             "eslint-plugin-markdown"
                                             "eslint-plugin-node"
                                             "eslint-plugin-promise"
                                             "eslint-plugin-standard"
                                             "mocha"
                                             "nyc"
                                             "supertest")))))))
    (inputs (list node-statuses-2.0.1
                  node-range-parser-1.2.1
                  node-on-finished-2.4.1
                  node-ms-2.1.3
                  node-mime-1.6.0
                  node-http-errors-2.0.0
                  node-fresh-0.5.2
                  node-etag-1.8.1
                  node-escape-html-1.0.3
                  node-encodeurl-1.0.2
                  node-destroy-1.2.0
                  node-depd-2.0.0
                  node-debug-2.6.9))
    (home-page "https://github.com/pillarjs/send#readme")
    (synopsis
     "Better streaming static file server with Range and conditional-GET support")
    (description
     "Better streaming static file server with Range and conditional-GET support")
    (license license:expat)))

(define-public node-sentence-splitter-3.2.3
  (package
    (name "node-sentence-splitter")
    (version "3.2.3")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/sentence-splitter/-/sentence-splitter-3.2.3.tgz")
              (sha256
               (base32
                "1cxn5sggc4y9ildpr778gxkpfn6m2vhfr9836di9gqwppasxdi4f"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@parcel/transformer-typescript-tsc"
                                             "@snowpack/plugin-typescript"
                                             "@textlint/markdown-to-ast"
                                             "@types/mocha"
                                             "@types/node"
                                             "@types/structured-source"
                                             "husky"
                                             "lint-staged"
                                             "mocha"
                                             "power-assert"
                                             "prettier"
                                             "ts-node"
                                             "ts-node-test-register"
                                             "typescript"
                                             "vite")))))))
    (inputs (list node-structured-source-3.0.2 node-object-values-0.1.2
                  node-concat-stream-2.0.0 node-textlint-ast-node-types-4.4.3))
    (home-page "https://github.com/azu/sentence-splitter")
    (synopsis "split {japanese, english} text into sentences.")
    (description "split {japanese, english} text into sentences.")
    (license license:expat)))

(define-public node-serve-static-1.15.0
  (package
    (name "node-serve-static")
    (version "1.15.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/serve-static/-/serve-static-1.15.0.tgz")
              (sha256
               (base32
                "0nwn940rkg5dpdd3s5mq0lglg9s68j9wmibidl3c88gx47psdxs9"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("eslint" "eslint-config-standard"
                                             "eslint-plugin-import"
                                             "eslint-plugin-markdown"
                                             "eslint-plugin-node"
                                             "eslint-plugin-promise"
                                             "eslint-plugin-standard"
                                             "mocha"
                                             "nyc"
                                             "safe-buffer"
                                             "supertest")))))))
    (inputs (list node-send-0.18.0 node-parseurl-1.3.3 node-escape-html-1.0.3
                  node-encodeurl-1.0.2))
    (home-page "https://github.com/expressjs/serve-static#readme")
    (synopsis "Serve static files")
    (description "Serve static files")
    (license license:expat)))

(define-public node-set-blocking-2.0.0
  (package
    (name "node-set-blocking")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/set-blocking/-/set-blocking-2.0.0.tgz")
              (sha256
               (base32
                "0gb9mvv8bjfavsxlzq56189qis7z2lrp893px04xl2cyvgkswd6r"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("chai" "coveralls" "mocha" "nyc"
                                             "standard" "standard-version")))))))
    (home-page "https://github.com/yargs/set-blocking#readme")
    (synopsis
     "set blocking stdio and stderr ensuring that terminal output does not truncate")
    (description
     "set blocking stdio and stderr ensuring that terminal output does not truncate")
    (license license:isc)))

(define-public node-set-value-2.0.1
  (package
    (name "node-set-value")
    (version "2.0.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/set-value/-/set-value-2.0.1.tgz")
              (sha256
               (base32
                "0qbp2ndx2qmmn6i7y92lk8jaj79cv36gpv9lq29lgw52wr6jbrl0"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp-format-md" "mocha")))))))
    (inputs (list node-split-string-3.1.0 node-is-plain-object-2.0.4
                  node-is-extendable-0.1.1 node-extend-shallow-2.0.1))
    (home-page "https://github.com/jonschlinkert/set-value")
    (synopsis
     "Create nested values and any intermediaries using dot notation (`'a.b.c'`) paths.")
    (description
     "Create nested values and any intermediaries using dot notation (`'a.b.c'`) paths.")
    (license license:expat)))

(define-public node-setprototypeof-1.2.0
  (package
    (name "node-setprototypeof")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/setprototypeof/-/setprototypeof-1.2.0.tgz")
              (sha256
               (base32
                "1qnzx8bl8h1vga28pf59mjd52wvh1hf3ma18d4zpwmijlrpcqfy8"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("mocha" "standard")))))))
    (home-page "https://github.com/wesleytodd/setprototypeof")
    (synopsis "A small polyfill for Object.setprototypeof")
    (description "A small polyfill for Object.setprototypeof")
    (license license:isc)))

(define-public node-shorthash-0.0.2
  (package
    (name "node-shorthash")
    (version "0.0.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/shorthash/-/shorthash-0.0.2.tgz")
              (sha256
               (base32
                "1lb884bdh1m6b7vyb3afm70vacip84i9ql4gdcl5f1wiqd5fpnfz"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://www.npmjs.com/package/node-shorthash")
    (synopsis "Node.js module to get a short unique hash of a string")
    (description "Node.js module to get a short unique hash of a string")
    (license license:expat)))

(define-public node-side-channel-1.0.4
  (package
    (name "node-side-channel")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/side-channel/-/side-channel-1.0.4.tgz")
              (sha256
               (base32
                "0dfp0r5j0i847w1q88riq2yp4mc0wrs0a6ys57j1m21nlny88arm"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@ljharb/eslint-config" "aud"
                                             "auto-changelog"
                                             "eslint"
                                             "nyc"
                                             "safe-publish-latest"
                                             "tape")))))))
    (inputs (list node-object-inspect-1.12.3 node-get-intrinsic-1.2.1
                  node-call-bind-1.0.2))
    (home-page "https://github.com/ljharb/side-channel#readme")
    (synopsis
     "Store information about any JS value in a side channel. Uses WeakMap if available.")
    (description
     "Store information about any JS value in a side channel. Uses WeakMap if available.")
    (license license:expat)))

(define-public node-signal-exit-3.0.7
  (package
    (name "node-signal-exit")
    (version "3.0.7")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/signal-exit/-/signal-exit-3.0.7.tgz")
              (sha256
               (base32
                "1a10ixkiak24yy6s7p9m7c6v9jkz2fm7wxgc2l3614dbdbx275j3"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("chai" "coveralls" "nyc"
                                             "standard-version" "tap")))))))
    (home-page "https://github.com/tapjs/signal-exit")
    (synopsis "when you want to fire an event no matter how a process exits.")
    (description
     "when you want to fire an event no matter how a process exits.")
    (license license:isc)))

(define-public node-snapdragon-0.8.2
  (package
    (name "node-snapdragon")
    (version "0.8.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/snapdragon/-/snapdragon-0.8.2.tgz")
              (sha256
               (base32
                "1anpibb0ajgw2yv400aq30bvvpcyi3yzyrp7fac2ia6r7ysxcgvq"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp" "gulp-eslint"
                                             "gulp-format-md"
                                             "gulp-istanbul"
                                             "gulp-mocha"
                                             "gulp-unused"
                                             "mocha")))))))
    (inputs (list node-use-3.1.1
                  node-source-map-resolve-0.5.3
                  node-source-map-0.5.7
                  node-map-cache-0.2.2
                  node-extend-shallow-2.0.1
                  node-define-property-0.2.5
                  node-debug-2.6.9
                  node-base-0.11.2))
    (home-page "https://github.com/jonschlinkert/snapdragon")
    (synopsis "Fast, pluggable and easy-to-use parser-renderer factory.")
    (description "Fast, pluggable and easy-to-use parser-renderer factory.")
    (license license:expat)))

(define-public node-snapdragon-node-2.1.1
  (package
    (name "node-snapdragon-node")
    (version "2.1.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/snapdragon-node/-/snapdragon-node-2.1.1.tgz")
              (sha256
               (base32
                "0idm24bf2jvwgqi8fx1fkn1w78ylqnpm137s2m0g0ni43y19i97j"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp" "gulp-eslint"
                                             "gulp-format-md"
                                             "gulp-istanbul"
                                             "gulp-mocha"
                                             "mocha"
                                             "snapdragon")))))))
    (inputs (list node-snapdragon-util-3.0.1 node-isobject-3.0.1
                  node-define-property-1.0.0))
    (home-page "https://github.com/jonschlinkert/snapdragon-node")
    (synopsis
     "Snapdragon utility for creating a new AST node in custom code, such as plugins.")
    (description
     "Snapdragon utility for creating a new AST node in custom code, such as plugins.")
    (license license:expat)))

(define-public node-snapdragon-util-3.0.1
  (package
    (name "node-snapdragon-util")
    (version "3.0.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/snapdragon-util/-/snapdragon-util-3.0.1.tgz")
              (sha256
               (base32
                "0c4fcrilagmpsrhh4mjfj7ah0vdxwkm9a4h5658bj8m7machxm2f"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("define-property" "gulp"
                                             "gulp-eslint"
                                             "gulp-format-md"
                                             "gulp-istanbul"
                                             "gulp-mocha"
                                             "isobject"
                                             "mocha"
                                             "snapdragon"
                                             "snapdragon-node")))))))
    (inputs (list node-kind-of-3.2.2))
    (home-page "https://github.com/jonschlinkert/snapdragon-util")
    (synopsis "Utilities for the snapdragon parser/compiler.")
    (description "Utilities for the snapdragon parser/compiler.")
    (license license:expat)))

(define-public node-source-map-0.5.7
  (package
    (name "node-source-map")
    (version "0.5.7")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/source-map/-/source-map-0.5.7.tgz")
              (sha256
               (base32
                "0rvb24j4kfib26w3cjyl6yan2dxvw1iy7d0wl404y5ckqjdjipp1"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("doctoc" "webpack")))))))
    (home-page "https://github.com/mozilla/source-map")
    (synopsis "Generates and consumes source maps")
    (description "Generates and consumes source maps")
    (license license:bsd-3)))

(define-public node-source-map-0.6.1
  (package
    (name "node-source-map")
    (version "0.6.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/source-map/-/source-map-0.6.1.tgz")
              (sha256
               (base32
                "11ib173i7xf5sd85da9jfrcbzygr48pppz5csl15hnpz2w6s3g5x"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("doctoc" "webpack")))))))
    (home-page "https://github.com/mozilla/source-map")
    (synopsis "Generates and consumes source maps")
    (description "Generates and consumes source maps")
    (license license:bsd-3)))

(define-public node-source-map-0.7.4
  (package
    (name "node-source-map")
    (version "0.7.4")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/source-map/-/source-map-0.7.4.tgz")
              (sha256
               (base32
                "0gbxjirhdjafc3w5w133b2rrybvq6hfr59j7bbg0mvv3i7iagpk5"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("doctoc" "eslint"
                                             "live-server"
                                             "npm-run-all"
                                             "nyc"
                                             "watch"
                                             "webpack"
                                             "webpack-cli")))))))
    (home-page "https://github.com/mozilla/source-map")
    (synopsis "Generates and consumes source maps")
    (description "Generates and consumes source maps")
    (license license:bsd-3)))

(define-public node-source-map-resolve-0.5.3
  (package
    (name "node-source-map-resolve")
    (version "0.5.3")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/source-map-resolve/-/source-map-resolve-0.5.3.tgz")
              (sha256
               (base32
                "1a9ykfyqnzxmna8i3f20g9pqcjzwx1f3wqwaa0bn6jwgmvwcvdj4"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("Base64" "jshint" "setimmediate"
                                             "simple-asyncify" "tape")))))))
    (inputs (list node-urix-0.1.0 node-source-map-url-0.4.1
                  node-resolve-url-0.2.1 node-decode-uri-component-0.2.2
                  node-atob-2.1.2))
    (home-page "https://github.com/lydell/source-map-resolve#readme")
    (synopsis "Resolve the source map and/or sources for a generated file.")
    (description "Resolve the source map and/or sources for a generated file.")
    (license license:expat)))

(define-public node-source-map-url-0.4.1
  (package
    (name "node-source-map-url")
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/source-map-url/-/source-map-url-0.4.1.tgz")
              (sha256
               (base32
                "10c201qd0ik7n9dyliaypczj4jraalg6lj3ky27y052kyngqa1cc"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("mocha" "expect.js" "jshint")))))))
    (home-page "https://github.com/lydell/source-map-url#readme")
    (synopsis "Tools for working with sourceMappingURL comments.")
    (description "Tools for working with sourceMappingURL comments.")
    (license license:expat)))

(define-public node-sourcemap-blender-1.0.5
  (package
    (name "node-sourcemap-blender")
    (version "1.0.5")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/sourcemap-blender/-/sourcemap-blender-1.0.5.tgz")
              (sha256
               (base32
                "1n80zmkdnw5gn8d51x8av4wd0dby18vp2xzjzqg5x29dlw63kpy1"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@types/node" "acorn" "escodegen"
                                             "fuse-box" "fuse-test-runner"
                                             "typescript")))))))
    (inputs (list node-source-map-0.7.4))
    (home-page "https://www.npmjs.com/package/node-sourcemap-blender")
    (synopsis "")
    (description "")
    (license license:isc)))

(define-public node-split-string-3.1.0
  (package
    (name "node-split-string")
    (version "3.1.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/split-string/-/split-string-3.1.0.tgz")
              (sha256
               (base32
                "1bx5n7bga42bd5d804w7y06wx96qk524gylxl4xi3i8nb8ch86na"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp-format-md" "mocha")))))))
    (inputs (list node-extend-shallow-3.0.2))
    (home-page "https://github.com/jonschlinkert/split-string")
    (synopsis
     "Split a string on a character except when the character is escaped.")
    (description
     "Split a string on a character except when the character is escaped.")
    (license license:expat)))

(define-public node-sshpk-1.17.0
  (package
    (name "node-sshpk")
    (version "1.17.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/sshpk/-/sshpk-1.17.0.tgz")
              (sha256
               (base32
                "1wjq0428xaji0i62iz8v809mjnd7l1qck4d3r99kqn56mcw3iavv"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("tape" "benchmark" "sinon" "temp")))))))
    (inputs (list node-bcrypt-pbkdf-1.0.2
                  node-ecc-jsbn-0.1.2
                  node-tweetnacl-0.14.5
                  node-jsbn-0.1.1
                  node-safer-buffer-2.1.2
                  node-getpass-0.1.7
                  node-dashdash-1.14.1
                  node-assert-plus-1.0.0
                  node-asn1-0.2.6))
    (home-page "https://github.com/arekinath/node-sshpk#readme")
    (synopsis "A library for finding and using SSH public keys")
    (description "A library for finding and using SSH public keys")
    (license license:expat)))

(define-public node-static-extend-0.1.2
  (package
    (name "node-static-extend")
    (version "0.1.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/static-extend/-/static-extend-0.1.2.tgz")
              (sha256
               (base32
                "1hwg7diq3kg6q7d2ymj423562yx6nfdqlym538s6ca02zxjgi7nl"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp-format-md" "mocha")))))))
    (inputs (list node-object-copy-0.1.0 node-define-property-0.2.5))
    (home-page "https://github.com/jonschlinkert/static-extend")
    (synopsis
     "Adds a static `extend` method to a class, to simplify inheritance. Extends the static properties, prototype properties, and descriptors from a `Parent` constructor onto `Child` constructors.")
    (description
     "Adds a static `extend` method to a class, to simplify inheritance. Extends the static properties, prototype properties, and descriptors from a `Parent` constructor onto `Child` constructors.")
    (license license:expat)))

(define-public node-statuses-2.0.1
  (package
    (name "node-statuses")
    (version "2.0.1")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/statuses/-/statuses-2.0.1.tgz")
              (sha256
               (base32
                "0nig6ygf53sj8vcqvbcwrzm4ln986rcz16kn5qjv1y4s9m1l164i"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("csv-parse" "eslint"
                                             "eslint-config-standard"
                                             "eslint-plugin-import"
                                             "eslint-plugin-markdown"
                                             "eslint-plugin-node"
                                             "eslint-plugin-promise"
                                             "eslint-plugin-standard"
                                             "mocha"
                                             "nyc"
                                             "raw-body"
                                             "stream-to-array")))))))
    (home-page "https://github.com/jshttp/statuses#readme")
    (synopsis "HTTP status utility")
    (description "HTTP status utility")
    (license license:expat)))

(define-public node-stream-browserify-2.0.2
  (package
    (name "node-stream-browserify")
    (version "2.0.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/stream-browserify/-/stream-browserify-2.0.2.tgz")
              (sha256
               (base32
                "0rqljbnx9mhx3cwm8pfl5vvvm0v8igcfk5r3c2lgg4gnxrb755qv"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("safe-buffer" "tape" "typedarray")))))))
    (inputs (list node-readable-stream-2.3.8 node-inherits-2.0.4))
    (home-page "https://github.com/browserify/stream-browserify")
    (synopsis "the stream module from node core for browsers")
    (description "the stream module from node core for browsers")
    (license license:expat)))

(define-public node-string-decoder-1.1.1
  (package
    (name "node-string-decoder")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/string_decoder/-/string_decoder-1.1.1.tgz")
              (sha256
               (base32
                "0fln2r91b8gj845j7jl76fvsp7nij13fyzvz82985yh88m1n50mg"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("babel-polyfill" "core-util-is"
                                             "inherits" "tap")))))))
    (inputs (list node-safe-buffer-5.1.2))
    (home-page "https://github.com/nodejs/string_decoder")
    (synopsis "The string_decoder module from Node core")
    (description "The string_decoder module from Node core")
    (license license:expat)))

(define-public node-string-decoder-1.3.0
  node-string-decoder)

(define-public node-string-width-2.1.1
  (package
    (name "node-string-width")
    (version "2.1.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/string-width/-/string-width-2.1.1.tgz")
              (sha256
               (base32
                "0b3rb6pbkyg411hvnzb5v5w2vckasgxvslwwijh0p410x46dqz12"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "xo")))))))
    (inputs (list node-strip-ansi-4.0.0 node-is-fullwidth-code-point-2.0.0))
    (home-page "https://github.com/sindresorhus/string-width#readme")
    (synopsis
     "Get the visual width of a string - the number of columns required to display it")
    (description
     "Get the visual width of a string - the number of columns required to display it")
    (license license:expat)))

(define-public node-string-width-4.2.3
  (package
    (name "node-string-width")
    (version "4.2.3")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/string-width/-/string-width-4.2.3.tgz")
              (sha256
               (base32
                "0d19spdisrqxd6311fc7z1yg34ww6rwh1zxdk6pnk03fnaqlzfxd"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "tsd" "xo")))))))
    (inputs (list node-strip-ansi-6.0.1 node-is-fullwidth-code-point-3.0.0
                  node-emoji-regex-8.0.0))
    (home-page "https://github.com/sindresorhus/string-width#readme")
    (synopsis
     "Get the visual width of a string - the number of columns required to display it")
    (description
     "Get the visual width of a string - the number of columns required to display it")
    (license license:expat)))

(define-public node-strip-ansi-4.0.0
  (package
    (name "node-strip-ansi")
    (version "4.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/strip-ansi/-/strip-ansi-4.0.0.tgz")
              (sha256
               (base32
                "0b90ys7pxxbavph56rhfmlymla8f8vaq7fy2pa91dq4r6r3sic5a"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "xo")))))))
    (inputs (list node-ansi-regex-3.0.1))
    (home-page "https://github.com/chalk/strip-ansi#readme")
    (synopsis "Strip ANSI escape codes")
    (description "Strip ANSI escape codes")
    (license license:expat)))

(define-public node-strip-ansi-6.0.1
  (package
    (name "node-strip-ansi")
    (version "6.0.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/strip-ansi/-/strip-ansi-6.0.1.tgz")
              (sha256
               (base32
                "1jh81jj6cn1lli1c7m6xi0ynra9zdghb1g63v1nib7zlpz87bnwv"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "tsd" "xo")))))))
    (inputs (list node-ansi-regex-5.0.1))
    (home-page "https://github.com/chalk/strip-ansi#readme")
    (synopsis "Strip ANSI escape codes from a string")
    (description "Strip ANSI escape codes from a string")
    (license license:expat)))

(define-public node-structured-source-3.0.2
  (package
    (name "node-structured-source")
    (version "3.0.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/structured-source/-/structured-source-3.0.2.tgz")
              (sha256
               (base32
                "1m6kmhbdccff9gcg355cbclbi2nhhqmh1ahacwa1wgfzawg3vplr"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp" "gulp-6to5"
                                             "gulp-bump"
                                             "gulp-espower"
                                             "gulp-filter"
                                             "gulp-git"
                                             "gulp-mocha"
                                             "gulp-sourcemaps"
                                             "gulp-tag-version"
                                             "power-assert")))))))
    (inputs (list node-boundary-1.0.1))
    (home-page "https://github.com/Constellation/structured-source")
    (synopsis
     "Provides StructuredSource and functionality for converting range and loc vice versa.")
    (description
     "Provides StructuredSource and functionality for converting range and loc vice versa.")
    (license #f)))

(define-public node-stylis-3.5.4
  (package
    (name "node-stylis")
    (version "3.5.4")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/stylis/-/stylis-3.5.4.tgz")
              (sha256
               (base32
                "0l79wch35jcrkrd2069qgmnrhrly06asrps540lda2zmgqkqwdwf"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:modules ((guix build node-build-system)
                  (srfi srfi-1)
                  (ice-9 match)
                  (guix build utils))
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-before 'configure 'avoid-prepare-scripts
                    (lambda _
                      (with-atomic-json-file-replacement "package.json"
                                                         (match-lambda
                                                           (((quote @) . pkg-meta-alist)
                                                            (cons '@
                                                                  (map (match-lambda
                                                                         (("scripts" @ . scripts-alist) `
                                                                          ("scripts"
                                                                           @
                                                                           ,@(filter (match-lambda
                                                                                       
                                                                                       (("prepare" . _)
                                                                                        #f)
                                                                                       
                                                                                       (_
                                                                                        #t))
                                                                              scripts-alist)))
                                                                         (other
                                                                          other))
                                                                   pkg-meta-alist)))))))
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("uglify-js")))))))
    (home-page "https://github.com/thysultan/stylis.js")
    (synopsis "light - weight css preprocessor")
    (description "light - weight css preprocessor")
    (license license:expat)))

(define-public node-stylis-4.2.0
  (package
    (name "node-stylis")
    (version "4.2.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/stylis/-/stylis-4.2.0.tgz")
              (sha256
               (base32
                "11ajla40nz9n1qshlr9y34fql5f1gybwzdcragy9vi4api0cncvm"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:modules ((guix build node-build-system)
                  (srfi srfi-1)
                  (ice-9 match)
                  (guix build utils))
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-before 'configure 'avoid-prepare-scripts
                    (lambda _
                      (with-atomic-json-file-replacement "package.json"
                                                         (match-lambda
                                                           (((quote @) . pkg-meta-alist)
                                                            (cons '@
                                                                  (map (match-lambda
                                                                         (("scripts" @ . scripts-alist) `
                                                                          ("scripts"
                                                                           @
                                                                           ,@(filter (match-lambda
                                                                                       
                                                                                       (("prepare" . _)
                                                                                        #f)
                                                                                       
                                                                                       (_
                                                                                        #t))
                                                                              scripts-alist)))
                                                                         (other
                                                                          other))
                                                                   pkg-meta-alist)))))))

                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("chai" "eslint"
                                             "esm"
                                             "mocha"
                                             "nyc"
                                             "rimraf"
                                             "rollup"
                                             "rollup-plugin-size"
                                             "rollup-plugin-terser"
                                             "stylis")))))))
    (home-page "https://github.com/thysultan/stylis.js")
    (synopsis "A Lightâweight CSS Preprocessor")
    (description "A Lightâweight CSS Preprocessor")
    (license license:expat)))

(define-public node-stylis-rule-sheet-0.0.10
  (package
    (name "node-stylis-rule-sheet")
    (version "0.0.10")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/stylis-rule-sheet/-/stylis-rule-sheet-0.0.10.tgz")
              (sha256
               (base32
                "09ipsi2bzzh7xl9pg2j7gh1dqbly93yf65ifix9adr918kw6f3dx"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("stylis")))))))
    (home-page "https://github.com/thysultan/stylis.js")
    (synopsis
     "stylis plugin to extract individual rules to use with insertRule API")
    (description
     "stylis plugin to extract individual rules to use with insertRule API")
    (license license:expat)))

(define-public node-supports-color-5.5.0
  (package
    (name "node-supports-color")
    (version "5.5.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/supports-color/-/supports-color-5.5.0.tgz")
              (sha256
               (base32
                "1ap0lk4n0m3948cnkfmyz71pizqlzjdfrhs0f954pksg4jnk52h5"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "import-fresh" "xo")))))))
    (inputs (list node-has-flag-3.0.0))
    (home-page "https://github.com/chalk/supports-color#readme")
    (synopsis "Detect whether a terminal supports color")
    (description "Detect whether a terminal supports color")
    (license license:expat)))

(define-public node-supports-preserve-symlinks-flag-1.0.0
  (package
    (name "node-supports-preserve-symlinks-flag")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/supports-preserve-symlinks-flag/-/supports-preserve-symlinks-flag-1.0.0.tgz")
              (sha256
               (base32
                "1ygp0kk7p6df5p6p2q1csdshilklhfbh49g7h90m6i2kmxh13nlp"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@ljharb/eslint-config" "aud"
                                             "auto-changelog"
                                             "eslint"
                                             "nyc"
                                             "safe-publish-latest"
                                             "semver"
                                             "tape")))))))
    (home-page
     "https://github.com/inspect-js/node-supports-preserve-symlinks-flag#readme")
    (synopsis
     "Determine if the current node version supports the `--preserve-symlinks` flag.")
    (description
     "Determine if the current node version supports the `--preserve-symlinks` flag.")
    (license license:expat)))

(define-public node-tar-6.1.15
  (package
    (name "node-tar")
    (version "6.1.15")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/tar/-/tar-6.1.15.tgz")
              (sha256
               (base32
                "06dh95c86j1lsccb3xm4cxlr3i4bsmpfs5a8dyv3bginmb1y0zg2"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@npmcli/eslint-config"
                                             "@npmcli/template-oss"
                                             "chmodr"
                                             "end-of-stream"
                                             "events-to-array"
                                             "mutate-fs"
                                             "nock"
                                             "rimraf"
                                             "tap")))))))
    (inputs (list node-yallist-4.0.0
                  node-mkdirp-1.0.4
                  node-minizlib-2.1.2
                  node-minipass-5.0.0
                  node-fs-minipass-2.1.0
                  node-chownr-2.0.0))
    (home-page "https://github.com/isaacs/node-tar#readme")
    (synopsis "tar for node")
    (description "tar for node")
    (license license:isc)))

(define-public node-textlint-ast-node-types-4.4.3
  (package
    (name "node-textlint-ast-node-types")
    (version "4.4.3")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/@textlint/ast-node-types/-/ast-node-types-4.4.3.tgz")
              (sha256
               (base32
                "01p6fxgr329vskhz4rcg4h2dba817y8x768hrl4vmb2cf790jqbv"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("cross-env" "mocha" "rimraf"
                                             "ts-node" "ts-node-test-register"
                                             "typescript")))))))
    (home-page "https://github.com/textlint/textlint#readme")
    (synopsis "textlint AST node type definition.")
    (description "textlint AST node type definition.")
    (license license:expat)))

(define-public node-thenby-1.3.4
  (package
    (name "node-thenby")
    (version "1.3.4")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/thenby/-/thenby-1.3.4.tgz")
              (sha256
               (base32
                "0n4gw01z6snr70gcssazh0ll0g21wmplykdfvd5n8vinnnsp616p"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("chai" "gulp"
                                             "gulp-insert"
                                             "gulp-mocha"
                                             "gulp-rename"
                                             "gulp-replace"
                                             "gulp-uglify"
                                             "gulp-umd"
                                             "performance-now")))))))
    (home-page "https://github.com/Teun/thenBy.js")
    (synopsis
     "Micro library for sorting arrays using the firstBy().thenBy().thenBy() syntax")
    (description
     "Micro library for sorting arrays using the firstBy().thenBy().thenBy() syntax")
    (license license:asl2.0)))

(define-public node-through-2.3.8
  (package
    (name "node-through")
    (version "2.3.8")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/through/-/through-2.3.8.tgz")
              (sha256
               (base32
                "0gjpaj9lwd6s356z2lljj2yj0pxwvdr8sckb6lkmfgmi1y67mchn"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("stream-spec" "tape" "from")))))))
    (home-page "https://github.com/dominictarr/through")
    (synopsis "simplified stream construction")
    (description "simplified stream construction")
    (license license:expat)))

(define-public node-tiny-emitter-2.1.0
  (package
    (name "node-tiny-emitter")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/tiny-emitter/-/tiny-emitter-2.1.0.tgz")
              (sha256
               (base32
                "0fki07f4gfncqj8h3zv5rz0cnza0yc0bk6v7bkc6ih0y1jiix32k"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@tap-format/spec" "browserify"
                                             "tape" "testling" "uglify-js")))))))
    (home-page "https://github.com/scottcorgan/tiny-emitter#readme")
    (synopsis "A tiny (less than 1k) event emitter library")
    (description "A tiny (less than 1k) event emitter library")
    (license license:expat)))

(define-public node-tlite-0.1.9
  (package
    (name "node-tlite")
    (version "0.1.9")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/tlite/-/tlite-0.1.9.tgz")
              (sha256
               (base32
                "1n84qafxyf1ph6d74mc2c51pncj9rz8h7ijknqbnfsjxs77svzl3"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("jasmine-node" "uglify-js")))))))
    (home-page "https://github.com/chrisdavies/tlite")
    (synopsis "Tiny tooltip utility")
    (description "Tiny tooltip utility")
    (license license:expat)))

(define-public node-tmp-0.0.33
  (package
    (name "node-tmp")
    (version "0.0.33")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/tmp/-/tmp-0.0.33.tgz")
              (sha256
               (base32
                "1ifn24mp4ds8yx9i79739gsdsj5ml6vzx8v3w5vg0m74dig22fyj"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("vows")))))))
    (inputs (list node-os-tmpdir-1.0.2))
    (home-page "http://github.com/raszi/node-tmp")
    (synopsis "Temporary file and directory creator")
    (description "Temporary file and directory creator")
    (license license:expat)))

(define-public node-to-fast-properties-2.0.0
  (package
    (name "node-to-fast-properties")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/to-fast-properties/-/to-fast-properties-2.0.0.tgz")
              (sha256
               (base32
                "10q99rgk8nfl8k7q0aqmik4wkbm8zp4z0rpwbm8b0gr4pi4gw4y7"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava")))))))
    (home-page "https://github.com/sindresorhus/to-fast-properties#readme")
    (synopsis "Force V8 to use fast properties for an object")
    (description "Force V8 to use fast properties for an object")
    (license license:expat)))

(define-public node-to-object-path-0.3.0
  (package
    (name "node-to-object-path")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/to-object-path/-/to-object-path-0.3.0.tgz")
              (sha256
               (base32
                "01n40v8xlqm635rp6cyz0jpw6295wm9fkr6m85nqcf457rfbqc68"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("base" "mocha")))))))
    (inputs (list node-kind-of-3.2.2))
    (home-page "https://github.com/jonschlinkert/to-object-path")
    (synopsis "Create an object path from a list or array of strings.")
    (description "Create an object path from a list or array of strings.")
    (license license:expat)))

(define-public node-to-regex-3.0.2
  (package
    (name "node-to-regex")
    (version "3.0.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/to-regex/-/to-regex-3.0.2.tgz")
              (sha256
               (base32
                "039l28qygjrjy10jz9cm3j066nw5fkfimzkp5ipq47w2pl2ii0w6"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp-format-md" "mocha")))))))
    (inputs (list node-safe-regex-1.1.0 node-regex-not-1.0.2
                  node-extend-shallow-3.0.2 node-define-property-2.0.2))
    (home-page "https://github.com/jonschlinkert/to-regex")
    (synopsis "Generate a regex from a string or array of strings.")
    (description "Generate a regex from a string or array of strings.")
    (license license:expat)))

(define-public node-to-regex-range-2.1.1
  (package
    (name "node-to-regex-range")
    (version "2.1.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/to-regex-range/-/to-regex-range-2.1.1.tgz")
              (sha256
               (base32
                "0rw8mjvncwxhyg5m7mzwqg16ddpyq5qzdwds0v0jnskqhklh14bq"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("fill-range" "gulp-format-md"
                                             "mocha" "text-table" "time-diff")))))))
    (inputs (list node-repeat-string-1.6.1 node-is-number-3.0.0))
    (home-page "https://github.com/micromatch/to-regex-range")
    (synopsis
     "Pass two numbers, get a regex-compatible source string for matching ranges. Validated against more than 2.78 million test assertions.")
    (description
     "Pass two numbers, get a regex-compatible source string for matching ranges. Validated against more than 2.78 million test assertions.")
    (license license:expat)))

(define-public node-toidentifier-1.0.1
  (package
    (name "node-toidentifier")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/toidentifier/-/toidentifier-1.0.1.tgz")
              (sha256
               (base32
                "021fp42m51qbqbqabwhxky8bkfkkwza65lqiz7d2gqwd91vwqvqq"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("eslint" "eslint-config-standard"
                                             "eslint-plugin-import"
                                             "eslint-plugin-markdown"
                                             "eslint-plugin-node"
                                             "eslint-plugin-promise"
                                             "eslint-plugin-standard"
                                             "mocha"
                                             "nyc")))))))
    (home-page "https://github.com/component/toidentifier#readme")
    (synopsis "Convert a string of words to a JavaScript identifier")
    (description "Convert a string of words to a JavaScript identifier")
    (license license:expat)))

(define-public node-touch-2.0.2
  (package
    (name "node-touch")
    (version "2.0.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/touch/-/touch-2.0.2.tgz")
              (sha256
               (base32
                "1cf0q0cpgqchs6qhpnhzicvpsf1miamz3h8pfg7wxkav0h5qrsws"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("mutate-fs" "tap")))))))
    (inputs (list node-nopt-1.0.10))
    (home-page "https://github.com/isaacs/node-touch#readme")
    (synopsis "like touch(1) in node")
    (description "like touch(1) in node")
    (license license:isc)))

(define-public node-tough-cookie-2.5.0
  (package
    (name "node-tough-cookie")
    (version "2.5.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/tough-cookie/-/tough-cookie-2.5.0.tgz")
              (sha256
               (base32
                "0knsdm6l5mn88rh78hajzr2rrydal6wf97l2pbpqjq8ws4w8gazh"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("async" "genversion" "nyc"
                                             "string.prototype.repeat" "vows")))))))
    (inputs (list node-punycode-2.3.0 node-psl-1.9.0))
    (home-page "https://github.com/salesforce/tough-cookie")
    (synopsis "RFC6265 Cookies and Cookie Jar for node.js")
    (description "RFC6265 Cookies and Cookie Jar for node.js")
    (license license:bsd-3)))

(define-public node-tr46-0.0.3
  (package
    (name "node-tr46")
    (version "0.0.3")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/tr46/-/tr46-0.0.3.tgz")
              (sha256
               (base32
                "02ia19bsjr545jlkgv35psmzzr5avic96zxw3dam78yf6bmy2jhn"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("mocha" "request")))))))
    (home-page "https://github.com/Sebmaster/tr46.js#readme")
    (synopsis "An implementation of the Unicode TR46 spec")
    (description "An implementation of the Unicode TR46 spec")
    (license license:expat)))

(define-public node-transliteration-2.3.5
  (package
    (name "node-transliteration")
    (version "2.3.5")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/transliteration/-/transliteration-2.3.5.tgz")
              (sha256
               (base32
                "0kgn4fxf7xmwmsi537cnhvgs6yag51ppx2cnpjhmdmradkarvgv6"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@babel/core"
                                             "@babel/plugin-proposal-class-properties"
                                             "@babel/plugin-proposal-object-rest-spread"
                                             "@babel/preset-env"
                                             "@babel/preset-typescript"
                                             "@types/tape"
                                             "@types/yargs"
                                             "codecov"
                                             "core-js"
                                             "coveralls"
                                             "eslint-config-prettier"
                                             "json5"
                                             "nyc"
                                             "prettier"
                                             "rimraf"
                                             "rollup"
                                             "rollup-plugin-babel"
                                             "rollup-plugin-commonjs"
                                             "rollup-plugin-hashbang"
                                             "rollup-plugin-sourcemaps"
                                             "rollup-plugin-terser"
                                             "rollup-plugin-typescript2"
                                             "rollup-plugin-uglify"
                                             "tap-spec"
                                             "tape"
                                             "ts-loader"
                                             "ts-node"
                                             "tslint"
                                             "tslint-config-prettier"
                                             "typescript")))))))
    (inputs (list node-yargs-17.7.2))
    (home-page "https://github.com/dzcpy/transliteration#readme")
    (synopsis
     "Unicode to ACSII transliteration / slugify module for node.js, browser, Web Worker, ReactNative and CLI.")
    (description
     "Unicode to ACSII transliteration / slugify module for node.js, browser, Web Worker, ReactNative and CLI.")
    (license license:expat)))

(define-public node-tslib-1.14.1
  (package
    (name "node-tslib")
    (version "1.14.1")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/tslib/-/tslib-1.14.1.tgz")
              (sha256
               (base32
                "1ywidp09jjarr031l03jg042zsfys2y2idwdcvmqhr7g54p32fvk"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://www.typescriptlang.org/")
    (synopsis "Runtime library for TypeScript helper functions")
    (description "Runtime library for TypeScript helper functions")
    (license license:bsd-0)))

(define-public node-tunnel-agent-0.6.0
  (package
    (name "node-tunnel-agent")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/tunnel-agent/-/tunnel-agent-0.6.0.tgz")
              (sha256
               (base32
                "04jhbjld99zavh1rvik2bayrgxwj2zx69xsbcm0gmlnna15c1qyk"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (inputs (list node-safe-buffer-5.2.1))
    (home-page "https://github.com/mikeal/tunnel-agent#readme")
    (synopsis
     "HTTP proxy tunneling agent. Formerly part of mikeal/request, now a standalone module.")
    (description
     "HTTP proxy tunneling agent. Formerly part of mikeal/request, now a standalone module.")
    (license license:asl2.0)))

(define-public node-tweetnacl-0.14.5
  (package
    (name "node-tweetnacl")
    (version "0.14.5")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/tweetnacl/-/tweetnacl-0.14.5.tgz")
              (sha256
               (base32
                "1mnzrxlww1sqwv493gn6ph9ak7n8l9w5qrahsa5kzn4vgbb37skc"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("browserify" "eslint"
                                             "faucet"
                                             "tap-browser-color"
                                             "tape"
                                             "tape-run"
                                             "tweetnacl-util"
                                             "uglify-js")))))))
    (home-page "https://tweetnacl.js.org")
    (synopsis "Port of TweetNaCl cryptographic library to JavaScript")
    (description "Port of TweetNaCl cryptographic library to JavaScript")
    (license license:unlicense)))

(define-public node-type-check-0.3.2
  (package
    (name "node-type-check")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/type-check/-/type-check-0.3.2.tgz")
              (sha256
               (base32
                "05iwxwj2sdbnxwp5lkxrdkbdcdvmkvq6zbq7lmsg20zbw3pyy56l"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("livescript" "mocha" "istanbul"
                                             "browserify")))))))
    (inputs (list node-prelude-ls-1.1.2))
    (home-page "https://github.com/gkz/type-check")
    (synopsis
     "type-check allows you to check the types of JavaScript values at runtime with a Haskell like type syntax.")
    (description
     "type-check allows you to check the types of JavaScript values at runtime with a Haskell like type syntax.")
    (license license:expat)))

(define-public node-type-is-1.6.18
  (package
    (name "node-type-is")
    (version "1.6.18")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/type-is/-/type-is-1.6.18.tgz")
              (sha256
               (base32
                "1bn3gl9vd67cq3wl2cvq686zskl2xx6lxz5kp9w47qc06f2vbnll"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("eslint" "eslint-config-standard"
                                             "eslint-plugin-import"
                                             "eslint-plugin-markdown"
                                             "eslint-plugin-node"
                                             "eslint-plugin-promise"
                                             "eslint-plugin-standard"
                                             "mocha"
                                             "nyc")))))))
    (inputs (list node-mime-types-2.1.35 node-media-typer-0.3.0))
    (home-page "https://github.com/jshttp/type-is#readme")
    (synopsis "Infer the content-type of a request.")
    (description "Infer the content-type of a request.")
    (license license:expat)))

(define-public node-typedarray-0.0.6
  (package
    (name "node-typedarray")
    (version "0.0.6")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/typedarray/-/typedarray-0.0.6.tgz")
              (sha256
               (base32
                "022101ap05mryhpyw33phwyamk1i139qqpn2rs2lq72qm5slnciz"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("tape")))))))
    (home-page "https://github.com/substack/typedarray")
    (synopsis "TypedArray polyfill for old browsers")
    (description "TypedArray polyfill for old browsers")
    (license license:expat)))

(define-public node-types-connect-3.4.35
  (package
    (name "node-types-connect")
    (version "3.4.35")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/@types/connect/-/connect-3.4.35.tgz")
              (sha256
               (base32
                "0s28qcmdv0g0hylq19s1g6lds18c4acksad5zjkh1ky6d88lrh5l"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (inputs (list node-types-node-20.3.1))
    (home-page
     "https://github.com/DefinitelyTyped/DefinitelyTyped/tree/master/types/connect")
    (synopsis "TypeScript definitions for connect")
    (description "TypeScript definitions for connect")
    (license license:expat)))

(define-public node-types-node-12.20.55
  (package
    (name "node-types-node")
    (version "12.20.55")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/@types/node/-/node-12.20.55.tgz")
              (sha256
               (base32
                "1wdxsgc3npbqxyvi1gacnj1mqhqc1y6vv8bh2pq3pm8ysnp6d4p2"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page
     "https://github.com/DefinitelyTyped/DefinitelyTyped/tree/master/types/node")
    (synopsis "TypeScript definitions for Node.js")
    (description "TypeScript definitions for Node.js")
    (license license:expat)))

(define-public node-types-node-20.3.1
  (package
    (name "node-types-node")
    (version "20.3.1")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/@types/node/-/node-20.3.1.tgz")
              (sha256
               (base32
                "08fadblp5i9yzi1gysvvpl6q4abbw2bpfxmgv8qpmfkqmmyw428g"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page
     "https://github.com/DefinitelyTyped/DefinitelyTyped/tree/master/types/node")
    (synopsis "TypeScript definitions for Node.js")
    (description "TypeScript definitions for Node.js")
    (license license:expat)))

(define-public node-types-parse-json-4.0.0
  (package
    (name "node-types-parse-json")
    (version "4.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/@types/parse-json/-/parse-json-4.0.0.tgz")
              (sha256
               (base32
                "0cbn421abbsbvwp0zm5ykdhnqwdimn02lfbznjg3s6cw8035k7yf"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://www.npmjs.com/package/node-types-parse-json")
    (synopsis "TypeScript definitions for parse-json")
    (description "TypeScript definitions for parse-json")
    (license license:expat)))

(define-public node-types-ws-7.4.7
  (package
    (name "node-types-ws")
    (version "7.4.7")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/@types/ws/-/ws-7.4.7.tgz")
              (sha256
               (base32
                "17yr5zpww5agzcdc1vvicmfhv94zzynskq52cyvardnkcwrvqsql"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (inputs (list node-types-node-20.3.1))
    (home-page
     "https://github.com/DefinitelyTyped/DefinitelyTyped/tree/master/types/ws")
    (synopsis "TypeScript definitions for ws")
    (description "TypeScript definitions for ws")
    (license license:expat)))

(define-public node-typescript-3.8.3
  (package
    (name "node-typescript")
    (version "3.8.3")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/typescript/-/typescript-3.8.3.tgz")
              (sha256
               (base32
                "11vv2h8lsja6vf9kvpm45ayswsw08zvydgk4hq97nrqaqlqhwf6p"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@octokit/rest"
                                             "@types/browserify"
                                             "@types/chai"
                                             "@types/convert-source-map"
                                             "@types/glob"
                                             "@types/gulp"
                                             "@types/gulp-concat"
                                             "@types/gulp-newer"
                                             "@types/gulp-rename"
                                             "@types/gulp-sourcemaps"
                                             "@types/jake"
                                             "@types/merge2"
                                             "@types/microsoft__typescript-etw"
                                             "@types/minimatch"
                                             "@types/minimist"
                                             "@types/mkdirp"
                                             "@types/mocha"
                                             "@types/ms"
                                             "@types/node"
                                             "@types/node-fetch"
                                             "@types/q"
                                             "@types/source-map-support"
                                             "@types/through2"
                                             "@types/travis-fold"
                                             "@types/xml2js"
                                             "@typescript-eslint/eslint-plugin"
                                             "@typescript-eslint/experimental-utils"
                                             "@typescript-eslint/parser"
                                             "async"
                                             "azure-devops-node-api"
                                             "browser-resolve"
                                             "browserify"
                                             "chai"
                                             "chalk"
                                             "convert-source-map"
                                             "del"
                                             "eslint"
                                             "eslint-formatter-autolinkable-stylish"
                                             "eslint-plugin-import"
                                             "eslint-plugin-jsdoc"
                                             "eslint-plugin-no-null"
                                             "fancy-log"
                                             "fs-extra"
                                             "glob"
                                             "gulp"
                                             "gulp-concat"
                                             "gulp-insert"
                                             "gulp-newer"
                                             "gulp-rename"
                                             "gulp-sourcemaps"
                                             "istanbul"
                                             "merge2"
                                             "minimist"
                                             "mkdirp"
                                             "mocha"
                                             "mocha-fivemat-progress-reporter"
                                             "ms"
                                             "node-fetch"
                                             "plugin-error"
                                             "pretty-hrtime"
                                             "prex"
                                             "q"
                                             "remove-internal"
                                             "source-map-support"
                                             "through2"
                                             "travis-fold"
                                             "typescript"
                                             "vinyl"
                                             "vinyl-sourcemaps-apply"
                                             "xml2js")))))))
    (home-page "https://www.typescriptlang.org/")
    (synopsis
     "TypeScript is a language for application scale JavaScript development")
    (description
     "TypeScript is a language for application scale JavaScript development")
    (license license:asl2.0)))

(define-public node-typescript-4.9.5
  (package
    (name "node-typescript")
    (version "4.9.5")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/typescript/-/typescript-4.9.5.tgz")
              (sha256
               (base32
                "01gmbf7ngk4as4qvn81yhp24vnn0rvq0g8z846lpl169g7halbvf"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@octokit/rest" "@types/chai"
                                             "@types/fancy-log"
                                             "@types/fs-extra"
                                             "@types/glob"
                                             "@types/gulp"
                                             "@types/gulp-concat"
                                             "@types/gulp-newer"
                                             "@types/gulp-rename"
                                             "@types/gulp-sourcemaps"
                                             "@types/merge2"
                                             "@types/microsoft__typescript-etw"
                                             "@types/minimist"
                                             "@types/mkdirp"
                                             "@types/mocha"
                                             "@types/ms"
                                             "@types/node"
                                             "@types/source-map-support"
                                             "@types/which"
                                             "@types/xml2js"
                                             "@typescript-eslint/eslint-plugin"
                                             "@typescript-eslint/parser"
                                             "@typescript-eslint/utils"
                                             "azure-devops-node-api"
                                             "chai"
                                             "chalk"
                                             "del"
                                             "diff"
                                             "eslint"
                                             "eslint-formatter-autolinkable-stylish"
                                             "eslint-plugin-import"
                                             "eslint-plugin-jsdoc"
                                             "eslint-plugin-local"
                                             "eslint-plugin-no-null"
                                             "fancy-log"
                                             "fs-extra"
                                             "glob"
                                             "gulp"
                                             "gulp-concat"
                                             "gulp-insert"
                                             "gulp-newer"
                                             "gulp-rename"
                                             "gulp-sourcemaps"
                                             "merge2"
                                             "minimist"
                                             "mkdirp"
                                             "mocha"
                                             "mocha-fivemat-progress-reporter"
                                             "ms"
                                             "node-fetch"
                                             "source-map-support"
                                             "typescript"
                                             "vinyl"
                                             "which"
                                             "xml2js")))))))
    (home-page "https://www.typescriptlang.org/")
    (synopsis
     "TypeScript is a language for application scale JavaScript development")
    (description
     "TypeScript is a language for application scale JavaScript development")
    (license license:asl2.0)))

(define-public node-uc-micro-1.0.6
  (package
    (name "node-uc-micro")
    (version "1.0.6")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/uc.micro/-/uc.micro-1.0.6.tgz")
              (sha256
               (base32
                "143wjdg3v5wqc894mbpm28pp8v9n8xjm7qlq8cdqxzp9yf5ikl94"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("mocha" "shelljs"
                                             "unicode-11.0.0")))))))
    (home-page "https://github.com/markdown-it/uc.micro#readme")
    (synopsis "Micro subset of unicode data files for markdown-it projects.")
    (description
     "Micro subset of unicode data files for markdown-it projects.")
    (license license:expat)))

(define-public node-ultron-1.0.2
  (package
    (name "node-ultron")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/ultron/-/ultron-1.0.2.tgz")
              (sha256
               (base32
                "056l6xqq23iijac63myrqinwhldixzsvmyi4lkfnawd0w4fagl67"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("assume" "eventemitter3"
                                             "istanbul" "mocha" "pre-commit")))))))
    (home-page "https://github.com/unshiftio/ultron")
    (synopsis
     "Ultron is high-intelligence robot. It gathers intel so it can start improving upon his rudimentary design")
    (description
     "Ultron is high-intelligence robot. It gathers intel so it can start improving upon his rudimentary design")
    (license license:expat)))

(define-public node-unicode-canonical-property-names-ecmascript-2.0.0
  (package
    (name "node-unicode-canonical-property-names-ecmascript")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/unicode-canonical-property-names-ecmascript/-/unicode-canonical-property-names-ecmascript-2.0.0.tgz")
              (sha256
               (base32
                "11k4z0098c9r7c0mignfdbislxn3b9ds0mwbgsmrpr3liwa0zw3j"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava")))))))
    (home-page
     "https://github.com/mathiasbynens/unicode-canonical-property-names-ecmascript")
    (synopsis
     "The set of canonical Unicode property names supported in ECMAScript RegExp property escapes.")
    (description
     "The set of canonical Unicode property names supported in ECMAScript RegExp property escapes.")
    (license license:expat)))

(define-public node-unicode-match-property-ecmascript-2.0.0
  (package
    (name "node-unicode-match-property-ecmascript")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/unicode-match-property-ecmascript/-/unicode-match-property-ecmascript-2.0.0.tgz")
              (sha256
               (base32
                "13bzxipaxf66i2xvlmmfv8yhc2db6ndv05xgrxinspypiq3z469a"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava")))))))
    (inputs (list node-unicode-property-aliases-ecmascript-2.1.0
                  node-unicode-canonical-property-names-ecmascript-2.0.0))
    (home-page
     "https://github.com/mathiasbynens/unicode-match-property-ecmascript")
    (synopsis
     "Match a Unicode property or property alias to its canonical property name per the algorithm used for RegExp Unicode property escapes in ECMAScript.")
    (description
     "Match a Unicode property or property alias to its canonical property name per the algorithm used for RegExp Unicode property escapes in ECMAScript.")
    (license license:expat)))

(define-public node-unicode-match-property-value-ecmascript-2.1.0
  (package
    (name "node-unicode-match-property-value-ecmascript")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/unicode-match-property-value-ecmascript/-/unicode-match-property-value-ecmascript-2.1.0.tgz")
              (sha256
               (base32
                "04rsxphw84cbc7qqywcr7xh39ryhswfrzix0ycf8h3qnrm1x9p5m"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "jsesc"
                                             "unicode-property-value-aliases-ecmascript")))))))
    (home-page
     "https://github.com/mathiasbynens/unicode-match-property-value-ecmascript")
    (synopsis
     "Match a Unicode property or property alias to its canonical property name per the algorithm used for RegExp Unicode property escapes in ECMAScript.")
    (description
     "Match a Unicode property or property alias to its canonical property name per the algorithm used for RegExp Unicode property escapes in ECMAScript.")
    (license license:expat)))

(define-public node-unicode-property-aliases-ecmascript-2.1.0
  (package
    (name "node-unicode-property-aliases-ecmascript")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/unicode-property-aliases-ecmascript/-/unicode-property-aliases-ecmascript-2.1.0.tgz")
              (sha256
               (base32
                "12dd3gla04q1cxssqf2bzqqwdvvz5fir4kjral3hvm7g9p49rm3n"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "jsesc"
                                             "unicode-canonical-property-names-ecmascript")))))))
    (home-page
     "https://github.com/mathiasbynens/unicode-property-aliases-ecmascript")
    (synopsis
     "Unicode property alias mappings in JavaScript format for property names that are supported in ECMAScript RegExp property escapes.")
    (description
     "Unicode property alias mappings in JavaScript format for property names that are supported in ECMAScript RegExp property escapes.")
    (license license:expat)))

(define-public node-union-value-1.0.1
  (package
    (name "node-union-value")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/union-value/-/union-value-1.0.1.tgz")
              (sha256
               (base32
                "00rjw4hvxnj5vrji9qzbxn6y9rx6av1q3nv8ilyxpska3q0zpj75"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp-format-md" "mocha" "should")))))))
    (inputs (list node-set-value-2.0.1 node-is-extendable-0.1.1
                  node-get-value-2.0.6 node-arr-union-3.1.0))
    (home-page "https://github.com/jonschlinkert/union-value")
    (synopsis
     "Set an array of unique values as the property of an object. Supports setting deeply nested properties using using object-paths/dot notation.")
    (description
     "Set an array of unique values as the property of an object. Supports setting deeply nested properties using using object-paths/dot notation.")
    (license license:expat)))

(define-public node-universalify-0.1.2
  (package
    (name "node-universalify")
    (version "0.1.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/universalify/-/universalify-0.1.2.tgz")
              (sha256
               (base32
                "0lykbpkmvjkjg0sqngrn086rxlyddgjkfnsi22r8hgixxzxb2alc"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("colortape" "coveralls" "nyc"
                                             "standard" "tape")))))))
    (home-page "https://github.com/RyanZim/universalify#readme")
    (synopsis
     "Make a callback- or promise-based function support both promises and callbacks.")
    (description
     "Make a callback- or promise-based function support both promises and callbacks.")
    (license license:expat)))

(define-public node-unpipe-1.0.0
  (package
    (name "node-unpipe")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/unpipe/-/unpipe-1.0.0.tgz")
              (sha256
               (base32
                "1dnzbqfmchls4jyvkw0wnkc09pig98y66zzsy3lizgyls435xyrd"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("istanbul" "mocha"
                                             "readable-stream")))))))
    (home-page "https://github.com/stream-utils/unpipe")
    (synopsis "Unpipe a stream from all destinations")
    (description "Unpipe a stream from all destinations")
    (license license:expat)))

(define-public node-unset-value-1.0.0
  (package
    (name "node-unset-value")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/unset-value/-/unset-value-1.0.0.tgz")
              (sha256
               (base32
                "11jj8ggkz8c54sf0zyqwlnwv89i5gj572ywbry7ispy6lqa84qsk"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp-format-md" "mocha" "should")))))))
    (inputs (list node-isobject-3.0.1 node-has-value-0.3.1))
    (home-page "https://github.com/jonschlinkert/unset-value")
    (synopsis "Delete nested properties from an object using dot notation.")
    (description "Delete nested properties from an object using dot notation.")
    (license license:expat)))

(define-public node-unstated-2.1.1
  (package
    (name "node-unstated")
    (version "2.1.1")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/unstated/-/unstated-2.1.1.tgz")
              (sha256
               (base32
                "1yaphbvwdgzvxp5vpsnk59lvfjznd0m8q9hbvsshg9wmpqpjybvy"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@types/react" "babel-core"
                                             "babel-plugin-transform-class-properties"
                                             "babel-preset-env"
                                             "babel-preset-flow"
                                             "babel-preset-react"
                                             "babel-register"
                                             "flow-bin"
                                             "flow-copy-source"
                                             "husky"
                                             "jest"
                                             "jsdom"
                                             "lint-staged"
                                             "parcel-bundler"
                                             "prettier"
                                             "prop-types"
                                             "react"
                                             "react-dom"
                                             "react-test-renderer"
                                             "rollup"
                                             "rollup-plugin-babel"
                                             "typescript")))))))
    (inputs (list node-create-react-context-0.1.6 node-react-16.14.0))
    (home-page "https://github.com/thejameskyle/unstated#readme")
    (synopsis "State so simple, it goes without saying")
    (description "State so simple, it goes without saying")
    (license license:expat)))

(define-public node-uri-js-4.4.1
  (package
    (name "node-uri-js")
    (version "4.4.1")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/uri-js/-/uri-js-4.4.1.tgz")
              (sha256
               (base32
                "0bcdxkngap84iv7hpfa4r18i3a3allxfh6dmcqzafgg8mx9dw4jn"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("babel-cli"
                                             "babel-plugin-external-helpers"
                                             "babel-preset-latest"
                                             "mocha"
                                             "mocha-qunit-ui"
                                             "rollup"
                                             "rollup-plugin-babel"
                                             "rollup-plugin-node-resolve"
                                             "sorcery"
                                             "typescript"
                                             "uglify-js")))))))
    (inputs (list node-punycode-2.3.0))
    (home-page "https://github.com/garycourt/uri-js")
    (synopsis
     "An RFC 3986/3987 compliant, scheme extendable URI/IRI parsing/validating/resolving library for JavaScript.")
    (description
     "An RFC 3986/3987 compliant, scheme extendable URI/IRI parsing/validating/resolving library for JavaScript.")
    (license license:bsd-2)))

(define-public node-urix-0.1.0
  (package
    (name "node-urix")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/urix/-/urix-0.1.0.tgz")
              (sha256
               (base32
                "19qmq8cra96cf7ji8d4ljfcgnazzrvw4lcxlf91jk1816ndx1pbm"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("mocha" "jshint")))))))
    (home-page "https://github.com/lydell/urix")
    (synopsis "Makes Windows-style paths more unix and URI friendly.")
    (description "Makes Windows-style paths more unix and URI friendly.")
    (license license:expat)))

(define-public node-use-3.1.1
  (package
    (name "node-use")
    (version "3.1.1")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/use/-/use-3.1.1.tgz")
              (sha256
               (base32
                "1nqrazqb927s0nma2qi2c3aambpy34krz8cgl6610n456zg5vjin"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("base-plugins" "define-property"
                                             "extend-shallow"
                                             "gulp"
                                             "gulp-eslint"
                                             "gulp-format-md"
                                             "gulp-istanbul"
                                             "gulp-mocha"
                                             "mocha")))))))
    (home-page "https://github.com/jonschlinkert/use")
    (synopsis "Easily add plugin support to your node.js application.")
    (description "Easily add plugin support to your node.js application.")
    (license license:expat)))

(define-public node-use-composed-ref-1.3.0
  (package
    (name "node-use-composed-ref")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/use-composed-ref/-/use-composed-ref-1.3.0.tgz")
              (sha256
               (base32
                "0isbz71zzc58r4wicxpx486qbc6myy9pqqr52nimrsl8nhxrg69l"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:modules ((guix build node-build-system)
                  (srfi srfi-1)
                  (ice-9 match)
                  (guix build utils))
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-before 'configure 'avoid-prepare-scripts
                    (lambda _
                      (with-atomic-json-file-replacement "package.json"
                                                         (match-lambda
                                                           (((quote @) . pkg-meta-alist)
                                                            (cons '@
                                                                  (map (match-lambda
                                                                         (("scripts" @ . scripts-alist) `
                                                                          ("scripts"
                                                                           @
                                                                           ,@(filter (match-lambda
                                                                                       
                                                                                       (("prepare" . _)
                                                                                        #f)
                                                                                       
                                                                                       (_
                                                                                        #t))
                                                                              scripts-alist)))
                                                                         (other
                                                                          other))
                                                                   pkg-meta-alist)))))))
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@babel/core"
                                             "@babel/plugin-transform-modules-commonjs"
                                             "@babel/preset-env"
                                             "@babel/preset-typescript"
                                             "@types/react"
                                             "husky"
                                             "jest"
                                             "lint-staged"
                                             "prettier"
                                             "react"
                                             "rimraf"
                                             "rollup"
                                             "rollup-plugin-babel"
                                             "typescript")))))))
    (home-page "https://github.com/Andarist/use-composed-ref#readme")
    (synopsis
     "React hook which creates a ref function from given refs. Useful when using forwardRef.")
    (description
     "React hook which creates a ref function from given refs. Useful when using forwardRef.")
    (license license:expat)))

(define-public node-use-debounce-9.0.4
  (package
    (name "node-use-debounce")
    (version "9.0.4")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/use-debounce/-/use-debounce-9.0.4.tgz")
              (sha256
               (base32
                "1n7r6civqqqawzpj1rlrrwf958hvjysh4ic81nh3ici08688pa35"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@size-limit/preset-small-lib"
                                             "@types/enzyme"
                                             "@types/enzyme-adapter-react-16"
                                             "@types/jest"
                                             "@types/node"
                                             "@types/react"
                                             "@types/react-dom"
                                             "@typescript-eslint/eslint-plugin"
                                             "@typescript-eslint/parser"
                                             "enzyme"
                                             "enzyme-adapter-react-16"
                                             "eslint"
                                             "eslint-config-prettier"
                                             "eslint-config-standard"
                                             "eslint-plugin-import"
                                             "eslint-plugin-node"
                                             "eslint-plugin-prettier"
                                             "eslint-plugin-promise"
                                             "eslint-plugin-react-hooks"
                                             "eslint-plugin-standard"
                                             "jest"
                                             "microbundle"
                                             "prettier"
                                             "react"
                                             "react-dom"
                                             "size-limit"
                                             "ts-jest"
                                             "typescript")))))))
    (home-page "https://github.com/xnimorz/use-debounce#readme")
    (synopsis "Debounce hook for react")
    (description "Debounce hook for react")
    (license license:expat)))

(define-public node-use-isomorphic-layout-effect-1.1.2
  (package
    (name "node-use-isomorphic-layout-effect")
    (version "1.1.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/use-isomorphic-layout-effect/-/use-isomorphic-layout-effect-1.1.2.tgz")
              (sha256
               (base32
                "1v21imbx03qlzkhgx21wykfxaigxsj111yd90993q0w0gn6ip1p2"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:modules ((guix build node-build-system)
                  (srfi srfi-1)
                  (ice-9 match)
                  (guix build utils))
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-before 'configure 'avoid-prepare-scripts
                    (lambda _
                      (with-atomic-json-file-replacement "package.json"
                                                         (match-lambda
                                                           (((quote @) . pkg-meta-alist)
                                                            (cons '@
                                                                  (map (match-lambda
                                                                         (("scripts" @ . scripts-alist) `
                                                                          ("scripts"
                                                                           @
                                                                           ,@(filter (match-lambda
                                                                                       
                                                                                       (("prepare" . _)
                                                                                        #f)
                                                                                       
                                                                                       (_
                                                                                        #t))
                                                                              scripts-alist)))
                                                                         (other
                                                                          other))
                                                                   pkg-meta-alist)))))))
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@babel/core" "@babel/preset-env"
                                             "@babel/preset-typescript"
                                             "@preconstruct/cli"
                                             "@types/react"
                                             "cpy-cli"
                                             "husky"
                                             "lint-staged"
                                             "prettier"
                                             "react"
                                             "typescript")))))))
    (home-page
     "https://github.com/Andarist/use-isomorphic-layout-effect#readme")
    (synopsis
     "A React helper hook for scheduling a layout effect with a fallback to a regular effect for environments where layout effects should not be used (such as server-side rendering).")
    (description
     "A React helper hook for scheduling a layout effect with a fallback to a regular effect for environments where layout effects should not be used (such as server-side rendering).")
    (license license:expat)))

(define-public node-use-latest-1.2.1
  (package
    (name "node-use-latest")
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/use-latest/-/use-latest-1.2.1.tgz")
              (sha256
               (base32
                "1my0k3dihszlanvkh517d163w1fvhcipbiz2pwp0pnm2pr4xzzji"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:modules ((guix build node-build-system)
                  (srfi srfi-1)
                  (ice-9 match)
                  (guix build utils))
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-before 'configure 'avoid-prepare-scripts
                    (lambda _
                      (with-atomic-json-file-replacement "package.json"
                                                         (match-lambda
                                                           (((quote @) . pkg-meta-alist)
                                                            (cons '@
                                                                  (map (match-lambda
                                                                         (("scripts" @ . scripts-alist) `
                                                                          ("scripts"
                                                                           @
                                                                           ,@(filter (match-lambda
                                                                                       
                                                                                       (("prepare" . _)
                                                                                        #f)
                                                                                       
                                                                                       (_
                                                                                        #t))
                                                                              scripts-alist)))
                                                                         (other
                                                                          other))
                                                                   pkg-meta-alist)))))))
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@babel/core" "@babel/preset-env"
                                             "@babel/preset-typescript"
                                             "@preconstruct/cli"
                                             "@types/react"
                                             "husky"
                                             "lint-staged"
                                             "prettier"
                                             "react"
                                             "typescript")))))))
    (inputs (list node-use-isomorphic-layout-effect-1.1.2 node-react-18.2.0))
    (home-page "https://github.com/Andarist/use-latest#readme")
    (synopsis
     "A React helper hook for storing latest value in ref object (updated in useEffect's callback).")
    (description
     "A React helper hook for storing latest value in ref object (updated in useEffect's callback).")
    (license license:expat)))

(define-public node-utf-8-validate-5.0.10
  (package
    (name "node-utf-8-validate")
    (version "5.0.10")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/utf-8-validate/-/utf-8-validate-5.0.10.tgz")
              (sha256
               (base32
                "12hjjrqm7r8fk87wlln4yzn9c58vhzz6jnqwzh1pcadccds81q3j"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("mocha" "node-gyp" "prebuildify")))))))
    (inputs (list node-node-gyp-build-4.6.0))
    (home-page "https://github.com/websockets/utf-8-validate")
    (synopsis "Check if a buffer contains valid UTF-8")
    (description "Check if a buffer contains valid UTF-8")
    (license license:expat)))

(define-public node-utf-8-validate-6.0.3
  (package
    (name "node-utf-8-validate")
    (version "6.0.3")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/utf-8-validate/-/utf-8-validate-6.0.3.tgz")
              (sha256
               (base32
                "14m4gs53fb420cx4jixcliz3agwnjiivzmzp2l5j08cc6hzgxv32"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("mocha" "node-gyp" "prebuildify"
                                             "prebuildify-cross")))))))
    (inputs (list node-node-gyp-build-4.6.0))
    (home-page "https://github.com/websockets/utf-8-validate")
    (synopsis "Check if a buffer contains valid UTF-8")
    (description "Check if a buffer contains valid UTF-8")
    (license license:expat)))

(define-public node-util-deprecate-1.0.2
  node-util-deprecate)

(define-public node-utils-extend-1.0.8
  (package
    (name "node-utils-extend")
    (version "1.0.8")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/utils-extend/-/utils-extend-1.0.8.tgz")
              (sha256
               (base32
                "0049dvkvlbzdvqyd51mga7kaj4b2v0gxrxsdxss835a28lp7s19j"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("grunt" "grunt-contrib-jshint"
                                             "mocha")))))))
    (home-page "https://github.com/douzi8/utils-extend")
    (synopsis "Extend nodejs util api, and it is light weight and simple")
    (description "Extend nodejs util api, and it is light weight and simple")
    (license license:isc)))

(define-public node-utils-merge-1.0.1
  (package
    (name "node-utils-merge")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/utils-merge/-/utils-merge-1.0.1.tgz")
              (sha256
               (base32
                "0djhmrfzxpdhscg4pkgnsd39cddpwpkkw1w2f8mp2xfsxn7mvnfy"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("make-node" "mocha" "chai")))))))
    (home-page "https://github.com/jaredhanson/utils-merge#readme")
    (synopsis "merge() utility function")
    (description "merge() utility function")
    (license license:expat)))

(define-public node-uuid-3.4.0
  (package
    (name "node-uuid")
    (version "3.4.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/uuid/-/uuid-3.4.0.tgz")
              (sha256
               (base32
                "1pgldpxvxyy2a9h437v0mflqxyc4w91b37iya2pcfd5wdlqcjxxs"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:modules ((guix build node-build-system)
                  (srfi srfi-1)
                  (ice-9 match)
                  (guix build utils))
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-before 'configure 'avoid-prepare-scripts
                    (lambda _
                      ;; We need to remove the prepare script from "package.json", as
                      ;; it would try to use the build environment and would block the
                      ;; automatic building by other packages making use of node-acorn.
                      ;; TODO: Add utility function
                      (with-atomic-json-file-replacement "package.json"
                                                         (match-lambda
                                                           (((quote @) . pkg-meta-alist)
                                                            (cons '@
                                                                  (map (match-lambda
                                                                         (("scripts" @ . scripts-alist) `
                                                                          ("scripts"
                                                                           @
                                                                           ,@(filter (match-lambda
                                                                                       
                                                                                       (("prepare" . _)
                                                                                        #f)
                                                                                       
                                                                                       (_
                                                                                        #t))
                                                                              scripts-alist)))
                                                                         (other
                                                                          other))
                                                                   pkg-meta-alist)))))))
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@commitlint/cli"
                                             "@commitlint/config-conventional"
                                             "eslint"
                                             "husky"
                                             "mocha"
                                             "runmd"
                                             "standard-version")))))))
    (home-page "https://github.com/uuidjs/uuid#readme")
    (synopsis "RFC4122 (v1, v4, and v5) UUIDs")
    (description "RFC4122 (v1, v4, and v5) UUIDs")
    (license license:expat)))

(define-public node-uuid-8.3.2
  (package
    (name "node-uuid")
    (version "8.3.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/uuid/-/uuid-8.3.2.tgz")
              (sha256
               (base32
                "0jljg4g6m0znnqm93ng9xwj9g63csbwdqbjlb9rir0m58wv70c7n"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@babel/cli" "@babel/core"
                                             "@babel/preset-env"
                                             "@commitlint/cli"
                                             "@commitlint/config-conventional"
                                             "@rollup/plugin-node-resolve"
                                             "babel-eslint"
                                             "bundlewatch"
                                             "eslint"
                                             "eslint-config-prettier"
                                             "eslint-config-standard"
                                             "eslint-plugin-import"
                                             "eslint-plugin-node"
                                             "eslint-plugin-prettier"
                                             "eslint-plugin-promise"
                                             "eslint-plugin-standard"
                                             "husky"
                                             "jest"
                                             "lint-staged"
                                             "npm-run-all"
                                             "optional-dev-dependency"
                                             "prettier"
                                             "random-seed"
                                             "rollup"
                                             "rollup-plugin-terser"
                                             "runmd"
                                             "standard-version")))))))
    (home-page "https://github.com/uuidjs/uuid#readme")
    (synopsis "RFC4122 (v1, v4, and v5) UUIDs")
    (description "RFC4122 (v1, v4, and v5) UUIDs")
    (license license:expat)))

(define-public node-vary-1.1.2
  (package
    (name "node-vary")
    (version "1.1.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/vary/-/vary-1.1.2.tgz")
              (sha256
               (base32
                "0wbf4kmfyzc23dc0vjcmymkd1ks50z5gvv23lkkkayipf438cy3k"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("beautify-benchmark" "benchmark"
                                             "eslint"
                                             "eslint-config-standard"
                                             "eslint-plugin-import"
                                             "eslint-plugin-markdown"
                                             "eslint-plugin-node"
                                             "eslint-plugin-promise"
                                             "eslint-plugin-standard"
                                             "istanbul"
                                             "mocha"
                                             "supertest")))))))
    (home-page "https://github.com/jshttp/vary#readme")
    (synopsis "Manipulate the HTTP Vary header")
    (description "Manipulate the HTTP Vary header")
    (license license:expat)))

(define-public node-verror-1.10.0
  (package
    (name "node-verror")
    (version "1.10.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/verror/-/verror-1.10.0.tgz")
              (sha256
               (base32
                "0swyg46nvq95xlrrjjbhhmhjrdxg19yrc1aj69zipck0vi24b6q1"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (inputs (list node-extsprintf-1.4.1 node-core-util-is-1.0.2
                  node-assert-plus-1.0.0))
    (home-page "https://github.com/davepacheco/node-verror")
    (synopsis "richer JavaScript errors")
    (description "richer JavaScript errors")
    (license license:expat)))

(define-public node-vscode-languageserver-types-3.17.3
  (package
    (name "node-vscode-languageserver-types")
    (version "3.17.3")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/vscode-languageserver-types/-/vscode-languageserver-types-3.17.3.tgz")
              (sha256
               (base32
                "1999qlc1dyaii73z5w114caachmlw0arkax5jckrzzql9baphq71"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page
     "https://github.com/Microsoft/vscode-languageserver-node#readme")
    (synopsis "Types used by the Language server for node")
    (description "Types used by the Language server for node")
    (license license:expat)))

(define-public node-w3c-keyname-2.2.8
  (package
    (name "node-w3c-keyname")
    (version "2.2.8")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/w3c-keyname/-/w3c-keyname-2.2.8.tgz")
              (sha256
               (base32
                "0dmb5lq40grcaqnjkzsx3l7xcrhy74g1hhy88bc2s7vd0a219lh5"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:modules ((guix build node-build-system)
                  (srfi srfi-1)
                  (ice-9 match)
                  (guix build utils))
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-before 'configure 'avoid-prepare-scripts
                    (lambda _
                      ;; We need to remove the prepare script from "package.json", as
                      ;; it would try to use the build environment and would block the
                      ;; automatic building by other packages making use of node-acorn.
                      ;; TODO: Add utility function
                      (with-atomic-json-file-replacement "package.json"
                                                         (match-lambda
                                                           (((quote @) . pkg-meta-alist)
                                                            (cons '@
                                                                  (map (match-lambda
                                                                         (("scripts" @ . scripts-alist) `
                                                                          ("scripts"
                                                                           @
                                                                           ,@(filter (match-lambda
                                                                                       
                                                                                       (("prepare" . _)
                                                                                        #f)
                                                                                       
                                                                                       (_
                                                                                        #t))
                                                                              scripts-alist)))
                                                                         (other
                                                                          other))
                                                                   pkg-meta-alist)))))))
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("rollup")))))))
    (home-page "https://github.com/marijnh/w3c-keyname#readme")
    (synopsis "Get a KeyboardEvent.key-style string from an event")
    (description "Get a KeyboardEvent.key-style string from an event")
    (license license:expat)))

(define-public node-watch-1.0.2
  (package
    (name "node-watch")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/watch/-/watch-1.0.2.tgz")
              (sha256
               (base32
                "1kzs1vvv0r1bns3bacj2rc8k829rv60hqvd7nvdr30vsgf54irw5"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (inputs (list node-minimist-1.2.8 node-exec-sh-0.2.2))
    (home-page "https://github.com/mikeal/watch")
    (synopsis "Utilities for watching file trees.")
    (description "Utilities for watching file trees.")
    (license license:asl2.0)))

(define-public node-webidl-conversions-3.0.1
  (package
    (name "node-webidl-conversions")
    (version "3.0.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/webidl-conversions/-/webidl-conversions-3.0.1.tgz")
              (sha256
               (base32
                "1a1dwb1ga1cj2s7av9r46b4xmx11vsk5zncc0gq2qz4l815w7pz4"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("mocha")))))))
    (home-page "https://github.com/jsdom/webidl-conversions#readme")
    (synopsis
     "Implements the WebIDL algorithms for converting to and from JavaScript values")
    (description
     "Implements the WebIDL algorithms for converting to and from JavaScript values")
    (license license:bsd-2)))

(define-public node-whatwg-url-5.0.0
  (package
    (name "node-whatwg-url")
    (version "5.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/whatwg-url/-/whatwg-url-5.0.0.tgz")
              (sha256
               (base32
                "1lvyrf4ry4bgl2jgpim2pkdmrbv2vb0vh0irmkp7da3kymqw97dh"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("eslint" "istanbul" "mocha"
                                             "recast" "request" "webidl2js")))))))
    (inputs (list node-webidl-conversions-3.0.1 node-tr46-0.0.3))
    (home-page "https://github.com/jsdom/whatwg-url#readme")
    (synopsis
     "An implementation of the WHATWG URL Standard's URL API and parsing machinery")
    (description
     "An implementation of the WHATWG URL Standard's URL API and parsing machinery")
    (license license:expat)))

(define-public node-wide-align-1.1.5
  (package
    (name "node-wide-align")
    (version "1.1.5")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/wide-align/-/wide-align-1.1.5.tgz")
              (sha256
               (base32
                "0k2zsixfhs4bahdn22bjjbbnxldlaxaxczwzaxv4wflf1f4h2n7c"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("tap")))))))
    (inputs (list node-string-width-4.2.3))
    (home-page "https://github.com/iarna/wide-align#readme")
    (synopsis
     "A wide-character aware text alignment function for use on the console or with fixed width fonts.")
    (description
     "A wide-character aware text alignment function for use on the console or with fixed width fonts.")
    (license license:isc)))

(define-public node-word-wrap-1.2.3
  (package
    (name "node-word-wrap")
    (version "1.2.3")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/word-wrap/-/word-wrap-1.2.3.tgz")
              (sha256
               (base32
                "1ngw3nglmfh9a90b4ckay43yw96h61mbxmhm5g1qvk1j6h1dmyv4"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("gulp-format-md" "mocha")))))))
    (home-page "https://github.com/jonschlinkert/word-wrap")
    (synopsis "Wrap words to a specified length.")
    (description "Wrap words to a specified length.")
    (license license:expat)))

(define-public node-wrap-ansi-7.0.0
  (package
    (name "node-wrap-ansi")
    (version "7.0.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/wrap-ansi/-/wrap-ansi-7.0.0.tgz")
              (sha256
               (base32
                "0bx4bkwfli34343rl97qy5pdg3zmskinsf8mlkv3isfj1d8v7587"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ava" "chalk" "coveralls"
                                             "has-ansi" "nyc" "xo")))))))
    (inputs (list node-strip-ansi-6.0.1 node-string-width-4.2.3
                  node-ansi-styles-4.3.0))
    (home-page "https://github.com/chalk/wrap-ansi#readme")
    (synopsis "Wordwrap a string with ANSI escape codes")
    (description "Wordwrap a string with ANSI escape codes")
    (license license:expat)))

(define-public node-wrappy-1.0.2
  node-wrappy)

(define-public node-ws-1.1.5
  (package
    (name "node-ws")
    (version "1.1.5")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/ws/-/ws-1.1.5.tgz")
              (sha256
               (base32
                "1vh6gflkpl9fga4kzcxjdb7zp81x9687bnihs5fyvp7xc6s2zm51"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("ansi" "benchmark"
                                             "bufferutil"
                                             "expect.js"
                                             "istanbul"
                                             "mocha"
                                             "should"
                                             "tinycolor"
                                             "utf-8-validate")))))))
    (inputs (list node-ultron-1.0.2 node-options-0.0.6))
    (home-page "https://github.com/websockets/ws")
    (synopsis
     "Simple to use, blazing fast and thoroughly tested websocket client and server for Node.js")
    (description
     "Simple to use, blazing fast and thoroughly tested websocket client and server for Node.js")
    (license license:expat)))

(define-public node-ws-7.5.9
  (package
    (name "node-ws")
    (version "7.5.9")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/ws/-/ws-7.5.9.tgz")
              (sha256
               (base32
                "06yy4mgyn4d75h2j5y226rb8pwq3igm18qym0qmbddg822z36iws"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("benchmark" "bufferutil"
                                             "eslint"
                                             "eslint-config-prettier"
                                             "eslint-plugin-prettier"
                                             "mocha"
                                             "nyc"
                                             "prettier"
                                             "utf-8-validate")))))))
    (home-page "https://github.com/websockets/ws")
    (synopsis
     "Simple to use, blazing fast and thoroughly tested websocket client and server for Node.js")
    (description
     "Simple to use, blazing fast and thoroughly tested websocket client and server for Node.js")
    (license license:expat)))

(define-public node-ws-8.13.0
  (package
    (name "node-ws")
    (version "8.13.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/ws/-/ws-8.13.0.tgz")
              (sha256
               (base32
                "0kyagsk1bgxhc43fxsc2g9xays9gzm2pcj4aj11wvvhzabyy02yq"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("benchmark" "bufferutil"
                                             "eslint"
                                             "eslint-config-prettier"
                                             "eslint-plugin-prettier"
                                             "mocha"
                                             "nyc"
                                             "prettier"
                                             "utf-8-validate")))))))
    (home-page "https://github.com/websockets/ws")
    (synopsis
     "Simple to use, blazing fast and thoroughly tested websocket client and server for Node.js")
    (description
     "Simple to use, blazing fast and thoroughly tested websocket client and server for Node.js")
    (license license:expat)))

(define-public node-xregexp-5.1.1
  (package
    (name "node-xregexp")
    (version "5.1.1")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/xregexp/-/xregexp-5.1.1.tgz")
              (sha256
               (base32
                "1lglgf3ij7vpm9ylas5pfvi84wgxgzsm2iwpkbj4pab8ay0vkx00"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@babel/cli" "@babel/core"
                                             "@babel/plugin-proposal-unicode-property-regex"
                                             "@babel/plugin-transform-runtime"
                                             "@babel/preset-env"
                                             "@unicode/unicode-14.0.0"
                                             "babel-plugin-add-module-exports"
                                             "babel-plugin-array-includes"
                                             "babel-plugin-transform-xregexp"
                                             "browserify"
                                             "dtslint"
                                             "eslint"
                                             "jasmine"
                                             "jsesc"
                                             "nyc"
                                             "unicode-property-value-aliases")))))))
    (inputs (list node-babel-runtime-corejs3-7.22.5))
    (home-page "http://xregexp.com/")
    (synopsis "Extended regular expressions")
    (description "Extended regular expressions")
    (license license:expat)))

(define-public node-y18n-5.0.8
  (package
    (name "node-y18n")
    (version "5.0.8")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/y18n/-/y18n-5.0.8.tgz")
              (sha256
               (base32
                "0sd7mpg2c0fxqq4q2574hvm26fgy6byp1gxnsgskmjx7sfx46dyl"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:modules ((guix build node-build-system)
                  (srfi srfi-1)
                  (ice-9 match)
                  (guix build utils))
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-before 'configure 'avoid-prepare-scripts
                    (lambda _
                      ;; We need to remove the prepare script from "package.json", as
                      ;; it would try to use the build environment and would block the
                      ;; automatic building by other packages making use of node-acorn.
                      ;; TODO: Add utility function
                      (with-atomic-json-file-replacement "package.json"
                                                         (match-lambda
                                                           (((quote @) . pkg-meta-alist)
                                                            (cons '@
                                                                  (map (match-lambda
                                                                         (("scripts" @ . scripts-alist) `
                                                                          ("scripts"
                                                                           @
                                                                           ,@(filter (match-lambda
                                                                                       
                                                                                       (("prepare" . _)
                                                                                        #f)
                                                                                       
                                                                                       (_
                                                                                        #t))
                                                                              scripts-alist)))
                                                                         (other
                                                                          other))
                                                                   pkg-meta-alist)))))))
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@types/node"
                                             "@wessberg/rollup-plugin-ts"
                                             "c8"
                                             "chai"
                                             "cross-env"
                                             "gts"
                                             "mocha"
                                             "rimraf"
                                             "rollup"
                                             "standardx"
                                             "ts-transform-default-export"
                                             "typescript")))))))
    (home-page "https://github.com/yargs/y18n")
    (synopsis "the bare-bones internationalization library used by yargs")
    (description "the bare-bones internationalization library used by yargs")
    (license license:isc)))

(define-public node-yallist-4.0.0
  (package
    (name "node-yallist")
    (version "4.0.0")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/yallist/-/yallist-4.0.0.tgz")
              (sha256
               (base32
                "0jadz9mh1lzfk19bvqqlrg40ggfk2yyfyrpgj5c62dk54ym7h358"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("tap")))))))
    (home-page "https://github.com/isaacs/yallist#readme")
    (synopsis "Yet Another Linked List")
    (description "Yet Another Linked List")
    (license license:isc)))

(define-public node-yaml-1.10.2
  (package
    (name "node-yaml")
    (version "1.10.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/yaml/-/yaml-1.10.2.tgz")
              (sha256
               (base32
                "13g0whx1h2bd2swvmwjwaqd1lhwsbngw1x1vjmi0ivgcnrd07krx"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@babel/core"
                                             "@babel/plugin-proposal-class-properties"
                                             "@babel/preset-env"
                                             "@rollup/plugin-babel"
                                             "babel-eslint"
                                             "babel-jest"
                                             "babel-plugin-trace"
                                             "common-tags"
                                             "cross-env"
                                             "eslint"
                                             "eslint-config-prettier"
                                             "fast-check"
                                             "jest"
                                             "prettier"
                                             "rollup"
                                             "typescript")))))))
    (home-page "https://eemeli.org/yaml/v1/")
    (synopsis "JavaScript parser and stringifier for YAML")
    (description "JavaScript parser and stringifier for YAML")
    (license license:isc)))

(define-public node-yargs-17.7.2
  (package
    (name "node-yargs")
    (version "17.7.2")
    (source (origin
              (method url-fetch)
              (uri "https://registry.npmjs.org/yargs/-/yargs-17.7.2.tgz")
              (sha256
               (base32
                "1jdfhg5pvp8834zsbm0vibjxx9rpwargqw1619i61mxm21gr7v28"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:modules ((guix build node-build-system)
                  (srfi srfi-1)
                  (ice-9 match)
                  (guix build utils))
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-before 'configure 'avoid-prepare-scripts
                    (lambda _
                      ;; We need to remove the prepare script from "package.json", as
                      ;; it would try to use the build environment and would block the
                      ;; automatic building by other packages making use of node-acorn.
                      ;; TODO: Add utility function
                      (with-atomic-json-file-replacement "package.json"
                                                         (match-lambda
                                                           (((quote @) . pkg-meta-alist)
                                                            (cons '@
                                                                  (map (match-lambda
                                                                         (("scripts" @ . scripts-alist) `
                                                                          ("scripts"
                                                                           @
                                                                           ,@(filter (match-lambda
                                                                                       
                                                                                       (("prepare" . _)
                                                                                        #f)
                                                                                       
                                                                                       (_
                                                                                        #t))
                                                                              scripts-alist)))
                                                                         (other
                                                                          other))
                                                                   pkg-meta-alist)))))))
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@types/chai" "@types/mocha"
                                             "@types/node"
                                             "c8"
                                             "chai"
                                             "chalk"
                                             "coveralls"
                                             "cpr"
                                             "cross-env"
                                             "cross-spawn"
                                             "eslint"
                                             "gts"
                                             "hashish"
                                             "mocha"
                                             "rimraf"
                                             "rollup"
                                             "rollup-plugin-cleanup"
                                             "rollup-plugin-terser"
                                             "rollup-plugin-ts"
                                             "typescript"
                                             "which"
                                             "yargs-test-extends")))))))
    (inputs (list node-yargs-parser-21.1.1
                  node-y18n-5.0.8
                  node-string-width-4.2.3
                  node-require-directory-2.1.1
                  node-get-caller-file-2.0.5
                  node-escalade-3.1.1
                  node-cliui-8.0.1))
    (home-page "https://yargs.js.org/")
    (synopsis "yargs the modern, pirate-themed, successor to optimist.")
    (description "yargs the modern, pirate-themed, successor to optimist.")
    (license license:expat)))

(define-public node-yargs-parser-21.1.1
  (package
    (name "node-yargs-parser")
    (version "21.1.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/yargs-parser/-/yargs-parser-21.1.1.tgz")
              (sha256
               (base32
                "0lcif2zbzw2jfj63paq2wdgc1hs9fd8i2xzjcidjcwjagm97sarl"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:modules ((guix build node-build-system)
                  (srfi srfi-1)
                  (ice-9 match)
                  (guix build utils))
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (add-before 'configure 'avoid-prepare-scripts
                    (lambda _
                      ;; We need to remove the prepare script from "package.json", as
                      ;; it would try to use the build environment and would block the
                      ;; automatic building by other packages making use of node-acorn.
                      ;; TODO: Add utility function
                      (with-atomic-json-file-replacement "package.json"
                                                         (match-lambda
                                                           (((quote @) . pkg-meta-alist)
                                                            (cons '@
                                                                  (map (match-lambda
                                                                         (("scripts" @ . scripts-alist) `
                                                                          ("scripts"
                                                                           @
                                                                           ,@(filter (match-lambda
                                                                                       
                                                                                       (("prepare" . _)
                                                                                        #f)
                                                                                       
                                                                                       (_
                                                                                        #t))
                                                                              scripts-alist)))
                                                                         (other
                                                                          other))
                                                                   pkg-meta-alist)))))))
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (delete-dependencies '("@types/chai" "@types/mocha"
                                             "@types/node"
                                             "@typescript-eslint/eslint-plugin"
                                             "@typescript-eslint/parser"
                                             "c8"
                                             "chai"
                                             "cross-env"
                                             "eslint"
                                             "eslint-plugin-import"
                                             "eslint-plugin-node"
                                             "gts"
                                             "mocha"
                                             "puppeteer"
                                             "rimraf"
                                             "rollup"
                                             "rollup-plugin-cleanup"
                                             "rollup-plugin-ts"
                                             "serve"
                                             "standardx"
                                             "start-server-and-test"
                                             "ts-transform-default-export"
                                             "typescript")))))))
    (home-page "https://github.com/yargs/yargs-parser#readme")
    (synopsis "the mighty option parser used by yargs")
    (description "the mighty option parser used by yargs")
    (license license:isc)))

(define-public node-zenscroll-4.0.2
  (package
    (name "node-zenscroll")
    (version "4.0.2")
    (source (origin
              (method url-fetch)
              (uri
               "https://registry.npmjs.org/zenscroll/-/zenscroll-4.0.2.tgz")
              (sha256
               (base32
                "14626z9wqrc0b2vm7f6q0irdb4zxqxs0rbg3l626kqhfzx7dy8kr"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://zengabor.github.io/zenscroll/")
    (synopsis
     "A module to smooth-scroll web pages and scrollable elements (like DIVs)")
    (description
     "A module to smooth-scroll web pages and scrollable elements (like DIVs)")
    (license license:unlicense)))

