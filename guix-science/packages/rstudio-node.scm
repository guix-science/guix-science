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

;; Packages in this module have been auto-imported by the npm-binary importer
;; from wip-node-14.
;; Find duplicates with
;; > grep define-public guix-science/packages/rstudio-node.scm | sort | uniq -c -d

(define-module (guix-science packages rstudio-node)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system node))

(define-public node-acorn-5.7.4
  (package
    (name "node-acorn")
    (version "5.7.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/acorn/-/acorn-5.7.4.tgz")
        (sha256
          (base32
            "1fsw5vbp6p6bqcy1h0g35vgbjb6bjpr65sjs0ln3q212fpkxsz3g"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://github.com/acornjs/acorn")
    (synopsis "ECMAScript parser")
    (description "ECMAScript parser")
    (license license:expat)))

(define-public node-acorn-jsx-4.1.1
  (package
    (name "node-acorn-jsx")
    (version "4.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/acorn-jsx/-/acorn-jsx-4.1.1.tgz")
        (sha256
          (base32
            "1rzhpi0ak9pynyrvb5pmzvh0pypzpnx9xxrs2y7b0cgg093r5imm"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-acorn" ,node-acorn-5.7.4)))
    (home-page
      "https://github.com/RReverser/acorn-jsx")
    (synopsis
      "Alternative, faster React.js JSX parser")
    (description
      "Alternative, faster React.js JSX parser")
    (license license:expat)))

(define-public node-ansi-0.3.1
  (package
    (name "node-ansi")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/ansi/-/ansi-0.3.1.tgz")
        (sha256
          (base32
            "0jbf21lf5jsqs63ki1dqzx4llqr7wvlc4jy8hhdyy7qvhglsj187"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/TooTallNate/ansi.js#readme")
    (synopsis
      "Advanced ANSI formatting tool for Node.js")
    (description
      "Advanced ANSI formatting tool for Node.js")
    (license license:expat)))

(define-public node-app-root-path-2.2.1
  (package
    (name "node-app-root-path")
    (version "2.2.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/app-root-path/-/app-root-path-2.2.1.tgz")
        (sha256
          (base32
            "0qjv4w1qhkqwri9iscvzh2fhvc4qv25mj58anqd61nbrh5n1la54"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/inxilpro/node-app-root-path")
    (synopsis
      "Determine an app's root path from anywhere inside the app")
    (description
      "Determine an app's root path from anywhere inside the app")
    (license license:expat)))

(define-public node-file-system-2.2.2
  (package
    (name "node-file-system")
    (version "2.2.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/file-system/-/file-system-2.2.2.tgz")
        (sha256
          (base32
            "08d1xdfkhx4j1gv2ipwzdxkyv0wpin0317k5vmllhdb3k3v3wdg6"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-utils-extend" ,node-utils-extend-1.0.8)
        ("node-file-match" ,node-file-match-1.0.2)))
    (home-page
      "https://github.com/douzi8/file-system")
    (synopsis
      "Strengthen the ability of file system")
    (description
      "Strengthen the ability of file system")
    (license license:isc)))

(define-public node-utils-extend-1.0.8
  (package
    (name "node-utils-extend")
    (version "1.0.8")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/utils-extend/-/utils-extend-1.0.8.tgz")
        (sha256
          (base32
            "0049dvkvlbzdvqyd51mga7kaj4b2v0gxrxsdxss835a28lp7s19j"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/douzi8/utils-extend")
    (synopsis
      "Extend nodejs util api, and it is light weight and simple")
    (description
      "Extend nodejs util api, and it is light weight and simple")
    (license license:isc)))

(define-public node-ajax-request-1.2.3
  (package
    (name "node-ajax-request")
    (version "1.2.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/ajax-request/-/ajax-request-1.2.3.tgz")
        (sha256
          (base32
            "0pkzvkblcmj8wxar7bw61jxfghzki12awi15gm1v2qsyfa42blva"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-utils-extend" ,node-utils-extend-1.0.8)
        ("node-file-system" ,node-file-system-2.2.2)))
    (home-page
      "https://github.com/douzi8/ajax-request#readme")
    (synopsis
      "Http request for nodejs, and it also support file download")
    (description
      "Http request for nodejs, and it also support file download")
    (license license:isc)))

(define-public node-file-match-1.0.2
  (package
    (name "node-file-match")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/file-match/-/file-match-1.0.2.tgz")
        (sha256
          (base32
            "07xax9x1x8zxkss8i0hmcqkpsz0fd53pl68xvg8x7d6d1s61nks1"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-utils-extend" ,node-utils-extend-1.0.8)))
    (home-page
      "https://github.com/douzi8/file-match")
    (synopsis
      "Match filepath is validated, or exclude filepath that don't need.")
    (description
      "Match filepath is validated, or exclude filepath that don't need.")
    (license license:isc)))

(define-public node-base64-img-1.0.4
  (package
    (name "node-base64-img")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/base64-img/-/base64-img-1.0.4.tgz")
        (sha256
          (base32
            "0f8x6x2dpq80kn97yfw4nk26b8mskaysv13hi93h03qlhwyx69jq"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-file-system" ,node-file-system-2.2.2)
        ("node-ajax-request" ,node-ajax-request-1.2.3)))
    (home-page
      "https://github.com/douzi8/base64-img")
    (synopsis
      "Convert img or svg to base64, or convert base64 to img")
    (description
      "Convert img or svg to base64, or convert base64 to img")
    (license license:isc)))

(define-public node-base64-js-1.5.1
  (package
    (name "node-base64-js")
    (version "1.5.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/base64-js/-/base64-js-1.5.1.tgz")
        (sha256
          (base32
            "118a46skxnrgx5bdd68ny9xxjcvyb7b1clj2hf82d196nm2skdxi"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/beatgammit/base64-js")
    (synopsis "Base64 encoding/decoding in pure JS")
    (description
      "Base64 encoding/decoding in pure JS")
    (license license:expat)))

(define-public node-bowser-2.11.0
  (package
    (name "node-bowser")
    (version "2.11.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/bowser/-/bowser-2.11.0.tgz")
        (sha256
          (base32
            "1mrsgjann2d0lnwivfqprh9x1dg48k1vq9mff1cwz8zgc1mbkrm7"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/lancedikson/bowser")
    (synopsis "Lightweight browser detector")
    (description "Lightweight browser detector")
    (license license:expat)))

(define-public node-arr-flatten-1.1.0
  (package
    (name "node-arr-flatten")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/arr-flatten/-/arr-flatten-1.1.0.tgz")
        (sha256
          (base32
            "0amnq01y6y8j49rdcib9yh8n95wk2ajs3qcckccvv1x6bf6nfvay"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jonschlinkert/arr-flatten")
    (synopsis
      "Recursively flatten an array or arrays.")
    (description
      "Recursively flatten an array or arrays.")
    (license license:expat)))

(define-public node-arr-diff-2.0.0
  (package
    (name "node-arr-diff")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/arr-diff/-/arr-diff-2.0.0.tgz")
        (sha256
          (base32
            "180fm2zz2qqygr715i4dwdpqypvf2gap4wds1crdhpdlbc3c2wpd"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-arr-flatten" ,node-arr-flatten-1.1.0)))
    (home-page
      "https://github.com/jonschlinkert/arr-diff")
    (synopsis
      "Returns an array with only the unique values from the first array, by excluding all values from additional arrays using strict equality for comparisons.")
    (description
      "Returns an array with only the unique values from the first array, by excluding all values from additional arrays using strict equality for comparisons.")
    (license license:expat)))

(define-public node-array-unique-0.2.1
  (package
    (name "node-array-unique")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/array-unique/-/array-unique-0.2.1.tgz")
        (sha256
          (base32
            "12j74kq24w0xpj6zxdqj0zlgr97ls02yqph2vbbk05q7bgbkckvy"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jonschlinkert/array-unique")
    (synopsis
      "Return an array free of duplicate values. Fastest ES5 implementation.")
    (description
      "Return an array free of duplicate values. Fastest ES5 implementation.")
    (license #f)))

(define-public node-is-number-2.1.0
  (package
    (name "node-is-number")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-number/-/is-number-2.1.0.tgz")
        (sha256
          (base32
            "07v7lz3vb6iq9f36ksnbf60gy91dggaa32l0m8026zpa5ccrx8r2"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-kind-of" ,node-kind-of-3.2.2)))
    (home-page
      "https://github.com/jonschlinkert/is-number")
    (synopsis
      "Returns true if the value is a number. comprehensive tests.")
    (description
      "Returns true if the value is a number. comprehensive tests.")
    (license license:expat)))

(define-public node-is-number-4.0.0
  (package
    (name "node-is-number")
    (version "4.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-number/-/is-number-4.0.0.tgz")
        (sha256
          (base32
            "1f47qljw0w6ajdkln18gxzv1qbhbxr88lrs6w485vk0g1b3wqnsy"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jonschlinkert/is-number")
    (synopsis
      "Returns true if the value is a number. comprehensive tests.")
    (description
      "Returns true if the value is a number. comprehensive tests.")
    (license license:expat)))

(define-public node-math-random-1.0.4
  (package
    (name "node-math-random")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/math-random/-/math-random-1.0.4.tgz")
        (sha256
          (base32
            "0882pbcf4m1irc214h2ddi3faspwah70813ryvxl14h3ar0jnqjx"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/michaelrhodes/math-random#readme")
    (synopsis
      "math-random is an drop-in replacement for Math.random that uses cryptographically secure random number generation, where available. It works in both browser and node environments.")
    (description
      "math-random is an drop-in replacement for Math.random that uses cryptographically secure random number generation, where available. It works in both browser and node environments.")
    (license license:expat)))

(define-public node-randomatic-3.1.1
  (package
    (name "node-randomatic")
    (version "3.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/randomatic/-/randomatic-3.1.1.tgz")
        (sha256
          (base32
            "1jawbnz6qqy98w7v45yp1wby00ykwrpmai5bmvbkw1nw9q56cy0p"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-math-random" ,node-math-random-1.0.4)
        ("node-kind-of" ,node-kind-of-6.0.3)
        ("node-is-number" ,node-is-number-4.0.0)))
    (home-page
      "https://github.com/jonschlinkert/randomatic")
    (synopsis
      "Generate randomized strings of a specified length using simple character sequences. The original generate-password.")
    (description
      "Generate randomized strings of a specified length using simple character sequences. The original generate-password.")
    (license license:expat)))

(define-public node-repeat-string-1.6.1
  (package
    (name "node-repeat-string")
    (version "1.6.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/repeat-string/-/repeat-string-1.6.1.tgz")
        (sha256
          (base32
            "1zmlk22rp97i5yfxqlb9hix87zlznngd60pm8qwhcg6bssacpq8b"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jonschlinkert/repeat-string")
    (synopsis
      "Repeat the given string n times. Fastest implementation for repeating a string.")
    (description
      "Repeat the given string n times. Fastest implementation for repeating a string.")
    (license license:expat)))

(define-public node-fill-range-2.2.4
  (package
    (name "node-fill-range")
    (version "2.2.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/fill-range/-/fill-range-2.2.4.tgz")
        (sha256
          (base32
            "183q5x2pvh9xwbpm1lrhazqbm5pg400nxi6rirpks4980fiph7fi"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-repeat-string" ,node-repeat-string-1.6.1)
        ("node-repeat-element"
         ,node-repeat-element-1.1.3)
        ("node-randomatic" ,node-randomatic-3.1.1)
        ("node-isobject" ,node-isobject-2.1.0)
        ("node-is-number" ,node-is-number-2.1.0)))
    (home-page
      "https://github.com/jonschlinkert/fill-range")
    (synopsis
      "Fill in a range of numbers or letters, optionally passing an increment or multiplier to use.")
    (description
      "Fill in a range of numbers or letters, optionally passing an increment or multiplier to use.")
    (license license:expat)))

(define-public node-expand-range-1.8.2
  (package
    (name "node-expand-range")
    (version "1.8.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/expand-range/-/expand-range-1.8.2.tgz")
        (sha256
          (base32
            "1d78rg12y81sy87jfdr5i9a2dq7smg47w5s249f7bfgwlcn848zl"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-fill-range" ,node-fill-range-2.2.4)))
    (home-page
      "https://github.com/jonschlinkert/expand-range")
    (synopsis
      "Fast, bash-like range expansion. Expand a range of numbers or letters, uppercase or lowercase. See the benchmarks. Used by micromatch.")
    (description
      "Fast, bash-like range expansion. Expand a range of numbers or letters, uppercase or lowercase. See the benchmarks. Used by micromatch.")
    (license license:expat)))

(define-public node-preserve-0.2.0
  (package
    (name "node-preserve")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/preserve/-/preserve-0.2.0.tgz")
        (sha256
          (base32
            "10lk61d1axbbh6j2cglvbcxamn7vh6dy5mmgkc2agmj05nlmlkr9"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jonschlinkert/preserve")
    (synopsis
      "Temporarily substitute tokens in the given `string` with placeholders, then put them back after transforming the string.")
    (description
      "Temporarily substitute tokens in the given `string` with placeholders, then put them back after transforming the string.")
    (license #f)))

(define-public node-braces-1.8.5
  (package
    (name "node-braces")
    (version "1.8.5")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/braces/-/braces-1.8.5.tgz")
        (sha256
          (base32
            "1593ijg9gjm1sgyk1cpqc71ac57md8izkhg4n5p18vnbri0xqzww"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-repeat-element"
         ,node-repeat-element-1.1.3)
        ("node-preserve" ,node-preserve-0.2.0)
        ("node-expand-range" ,node-expand-range-1.8.2)))
    (home-page
      "https://github.com/jonschlinkert/braces")
    (synopsis
      "Fastest brace expansion for node.js, with the most complete support for the Bash 4.3 braces specification.")
    (description
      "Fastest brace expansion for node.js, with the most complete support for the Bash 4.3 braces specification.")
    (license license:expat)))

(define-public node-is-posix-bracket-0.1.1
  (package
    (name "node-is-posix-bracket")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-posix-bracket/-/is-posix-bracket-0.1.1.tgz")
        (sha256
          (base32
            "01yzwk96c7zvc0zlh7wg200ajpk1av5aahcgc1avxizwp53m1343"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jonschlinkert/is-posix-bracket")
    (synopsis
      "Returns true if the given string is a POSIX bracket expression (POSIX character class).")
    (description
      "Returns true if the given string is a POSIX bracket expression (POSIX character class).")
    (license license:expat)))

(define-public node-expand-brackets-0.1.5
  (package
    (name "node-expand-brackets")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/expand-brackets/-/expand-brackets-0.1.5.tgz")
        (sha256
          (base32
            "1fzgbcp2faxj7d6i41iybacbxaig8gpkx329dvcysic2yy2rn1c3"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-is-posix-bracket"
         ,node-is-posix-bracket-0.1.1)))
    (home-page
      "https://github.com/jonschlinkert/expand-brackets")
    (synopsis
      "Expand POSIX bracket expressions (character classes) in glob patterns.")
    (description
      "Expand POSIX bracket expressions (character classes) in glob patterns.")
    (license license:expat)))

(define-public node-extglob-0.3.2
  (package
    (name "node-extglob")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/extglob/-/extglob-0.3.2.tgz")
        (sha256
          (base32
            "161asrmll909bda6hzz8ayfym5sp35yp45s25dcbfs3d9qcz0m98"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-is-extglob" ,node-is-extglob-1.0.0)))
    (home-page
      "https://github.com/jonschlinkert/extglob")
    (synopsis
      "Convert extended globs to regex-compatible strings. Add (almost) the expressive power of regular expressions to glob patterns.")
    (description
      "Convert extended globs to regex-compatible strings. Add (almost) the expressive power of regular expressions to glob patterns.")
    (license license:expat)))

(define-public node-filename-regex-2.0.1
  (package
    (name "node-filename-regex")
    (version "2.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/filename-regex/-/filename-regex-2.0.1.tgz")
        (sha256
          (base32
            "0swl7rv3vxrrmpjfr9i5fdbghif1s2yj9mdxmx6c27mg2kx88ya2"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/regexhq/filename-regex")
    (synopsis
      "Regular expression for matching file names, with or without extension.")
    (description
      "Regular expression for matching file names, with or without extension.")
    (license license:expat)))

(define-public node-is-glob-2.0.1
  (package
    (name "node-is-glob")
    (version "2.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-glob/-/is-glob-2.0.1.tgz")
        (sha256
          (base32
            "1bchcw1g5fdi2mrz362hjhlxvrbbg2ppyz7dcqkldpxjl5x2n777"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-is-extglob" ,node-is-extglob-1.0.0)))
    (home-page
      "https://github.com/jonschlinkert/is-glob")
    (synopsis
      "Returns `true` if the given string looks like a glob pattern or an extglob pattern. This makes it easy to create code that only uses external modules like node-glob when necessary, resulting in much faster code execution and initialization time, and a bet")
    (description
      "Returns `true` if the given string looks like a glob pattern or an extglob pattern. This makes it easy to create code that only uses external modules like node-glob when necessary, resulting in much faster code execution and initialization time, and a bet")
    (license license:expat)))

(define-public node-normalize-path-2.1.1
  (package
    (name "node-normalize-path")
    (version "2.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/normalize-path/-/normalize-path-2.1.1.tgz")
        (sha256
          (base32
            "1d82jyqqyqgk8qkzb4sk7vnz6sgf8xznlm55mazlp43fc6w100cj"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-remove-trailing-separator"
         ,node-remove-trailing-separator-1.1.0)))
    (home-page
      "https://github.com/jonschlinkert/normalize-path")
    (synopsis
      "Normalize file path slashes to be unix-like forward slashes. Also condenses repeat slashes to a single slash and removes and trailing slashes unless disabled.")
    (description
      "Normalize file path slashes to be unix-like forward slashes. Also condenses repeat slashes to a single slash and removes and trailing slashes unless disabled.")
    (license license:expat)))

(define-public node-for-in-1.0.2
  (package
    (name "node-for-in")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/for-in/-/for-in-1.0.2.tgz")
        (sha256
          (base32
            "0pm8dx9gvp9p91my9fqivajq7yhnxmn8scl391pgcv6d8h6s6zaf"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jonschlinkert/for-in")
    (synopsis
      "Iterate over the own and inherited enumerable properties of an object, and return an object with properties that evaluate to true from the callback. Exit early by returning `false`. JavaScript/Node.js")
    (description
      "Iterate over the own and inherited enumerable properties of an object, and return an object with properties that evaluate to true from the callback. Exit early by returning `false`. JavaScript/Node.js")
    (license license:expat)))

(define-public node-for-own-0.1.5
  (package
    (name "node-for-own")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/for-own/-/for-own-0.1.5.tgz")
        (sha256
          (base32
            "0bxjf90a7n1r0l5vm93zrz00qzip9s4g4c3pzdg3grn2fw53byph"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-for-in" ,node-for-in-1.0.2)))
    (home-page
      "https://github.com/jonschlinkert/for-own")
    (synopsis
      "Iterate over the own enumerable properties of an object, and return an object with properties that evaluate to true from the callback. Exit early by returning `false`. JavaScript/Node.js.")
    (description
      "Iterate over the own enumerable properties of an object, and return an object with properties that evaluate to true from the callback. Exit early by returning `false`. JavaScript/Node.js.")
    (license license:expat)))

(define-public node-object-omit-2.0.1
  (package
    (name "node-object-omit")
    (version "2.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/object.omit/-/object.omit-2.0.1.tgz")
        (sha256
          (base32
            "1zyjhjfjgmqbnszcxqlvncg7kahc045sp5w1j8f0z92cq9y25zws"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-is-extendable" ,node-is-extendable-0.1.1)
        ("node-for-own" ,node-for-own-0.1.5)))
    (home-page
      "https://github.com/jonschlinkert/object.omit")
    (synopsis
      "Return a copy of an object excluding the given key, or array of keys. Also accepts an optional filter function as the last argument.")
    (description
      "Return a copy of an object excluding the given key, or array of keys. Also accepts an optional filter function as the last argument.")
    (license license:expat)))

(define-public node-glob-base-0.3.0
  (package
    (name "node-glob-base")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/glob-base/-/glob-base-0.3.0.tgz")
        (sha256
          (base32
            "19d8xlkbibp313hidn9ldliqx9n7qlscf7pgxwi5ld1rak5r7an7"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-is-glob" ,node-is-glob-2.0.1)
        ("node-glob-parent" ,node-glob-parent-2.0.0)))
    (home-page
      "https://github.com/jonschlinkert/glob-base")
    (synopsis
      "Returns an object with the (non-glob) base path and the actual pattern.")
    (description
      "Returns an object with the (non-glob) base path and the actual pattern.")
    (license #f)))

(define-public node-is-dotfile-1.0.3
  (package
    (name "node-is-dotfile")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-dotfile/-/is-dotfile-1.0.3.tgz")
        (sha256
          (base32
            "13k7xhnmgbirvqs0lrggm7knqhkghm73pdha2sb5fq6ys9l6bili"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jonschlinkert/is-dotfile")
    (synopsis
      "Return true if a file path is (or has) a dotfile. Returns false if the path is a dot directory.")
    (description
      "Return true if a file path is (or has) a dotfile. Returns false if the path is a dot directory.")
    (license license:expat)))

(define-public node-parse-glob-3.0.4
  (package
    (name "node-parse-glob")
    (version "3.0.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/parse-glob/-/parse-glob-3.0.4.tgz")
        (sha256
          (base32
            "1pj6awfzkdl4i9ljmfcqgr16qh8pp11hqb1jsx5lq4rhw12laxmh"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-is-glob" ,node-is-glob-2.0.1)
        ("node-is-extglob" ,node-is-extglob-1.0.0)
        ("node-is-dotfile" ,node-is-dotfile-1.0.3)
        ("node-glob-base" ,node-glob-base-0.3.0)))
    (home-page
      "https://github.com/jonschlinkert/parse-glob")
    (synopsis
      "Parse a glob pattern into an object of tokens.")
    (description
      "Parse a glob pattern into an object of tokens.")
    (license license:expat)))

(define-public node-is-primitive-2.0.0
  (package
    (name "node-is-primitive")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-primitive/-/is-primitive-2.0.0.tgz")
        (sha256
          (base32
            "1s57swffqif8vd6zkbw3d5lg25c9236r9m1s8c7c616dmx3c4bnd"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jonschlinkert/is-primitive")
    (synopsis
      "Returns `true` if the value is a primitive. ")
    (description
      "Returns `true` if the value is a primitive. ")
    (license #f)))

(define-public node-is-equal-shallow-0.1.3
  (package
    (name "node-is-equal-shallow")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-equal-shallow/-/is-equal-shallow-0.1.3.tgz")
        (sha256
          (base32
            "1scqzxqgasmdyzw658nayiaykw4ba82rqkz83svrcdvjvp68phql"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-is-primitive" ,node-is-primitive-2.0.0)))
    (home-page
      "https://github.com/jonschlinkert/is-equal-shallow")
    (synopsis
      "Does a shallow comparison of two objects, returning false if the keys or values differ.")
    (description
      "Does a shallow comparison of two objects, returning false if the keys or values differ.")
    (license license:expat)))

(define-public node-regex-cache-0.4.4
  (package
    (name "node-regex-cache")
    (version "0.4.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/regex-cache/-/regex-cache-0.4.4.tgz")
        (sha256
          (base32
            "100lg733jk0j378l77n72w5qfqzyrnacf744y2icp170wwsrvm36"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-is-equal-shallow"
         ,node-is-equal-shallow-0.1.3)))
    (home-page
      "https://github.com/jonschlinkert/regex-cache")
    (synopsis
      "Memoize the results of a call to the RegExp constructor, avoiding repetitious runtime compilation of the same string and options, resulting in surprising performance improvements.")
    (description
      "Memoize the results of a call to the RegExp constructor, avoiding repetitious runtime compilation of the same string and options, resulting in surprising performance improvements.")
    (license license:expat)))

(define-public node-micromatch-2.3.11
  (package
    (name "node-micromatch")
    (version "2.3.11")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/micromatch/-/micromatch-2.3.11.tgz")
        (sha256
          (base32
            "1gz4w1hn7sh17wv82j53d97pr5fli3747sv28fb00r7ghbn5zxla"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-regex-cache" ,node-regex-cache-0.4.4)
        ("node-parse-glob" ,node-parse-glob-3.0.4)
        ("node-object-omit" ,node-object-omit-2.0.1)
        ("node-normalize-path"
         ,node-normalize-path-2.1.1)
        ("node-kind-of" ,node-kind-of-3.2.2)
        ("node-is-glob" ,node-is-glob-2.0.1)
        ("node-is-extglob" ,node-is-extglob-1.0.0)
        ("node-filename-regex"
         ,node-filename-regex-2.0.1)
        ("node-extglob" ,node-extglob-0.3.2)
        ("node-expand-brackets"
         ,node-expand-brackets-0.1.5)
        ("node-braces" ,node-braces-1.8.5)
        ("node-array-unique" ,node-array-unique-0.2.1)
        ("node-arr-diff" ,node-arr-diff-2.0.0)))
    (home-page
      "https://github.com/jonschlinkert/micromatch")
    (synopsis
      "Glob matching for javascript/node.js. A drop-in replacement and faster alternative to minimatch and multimatch.")
    (description
      "Glob matching for javascript/node.js. A drop-in replacement and faster alternative to minimatch and multimatch.")
    (license license:expat)))

(define-public node-remove-trailing-separator-1.1.0
  (package
    (name "node-remove-trailing-separator")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/remove-trailing-separator/-/remove-trailing-separator-1.1.0.tgz")
        (sha256
          (base32
            "08b3msz9s5kw1alivgn9saaz6w04grjqppkdk3qbr7blk38l04sf"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/darsain/remove-trailing-separator#readme")
    (synopsis
      "Removes separators from the end of the string.")
    (description
      "Removes separators from the end of the string.")
    (license license:isc)))

(define-public node-anymatch-1.3.2
  (package
    (name "node-anymatch")
    (version "1.3.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/anymatch/-/anymatch-1.3.2.tgz")
        (sha256
          (base32
            "1lr9c0ki26rl2xnbiqj8smgw0b5r5ns3jxmr41s4cwbkrh6jjfxs"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-normalize-path"
         ,node-normalize-path-2.1.1)
        ("node-micromatch" ,node-micromatch-2.3.11)))
    (home-page "https://github.com/es128/anymatch")
    (synopsis
      "Matches strings against configurable strings, globs, regular expressions, and/or functions")
    (description
      "Matches strings against configurable strings, globs, regular expressions, and/or functions")
    (license license:isc)))

(define-public node-async-each-1.0.3
  (package
    (name "node-async-each")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/async-each/-/async-each-1.0.3.tgz")
        (sha256
          (base32
            "0qv3hchym99lkq4jsikkdirwxzs3kifdfpyrinazc543hipbhxla"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/paulmillr/async-each/")
    (synopsis
      "No-bullshit, ultra-simple, 35-lines-of-code async parallel forEach / map function for JavaScript.")
    (description
      "No-bullshit, ultra-simple, 35-lines-of-code async parallel forEach / map function for JavaScript.")
    (license license:expat)))

(define-public node-glob-parent-2.0.0
  (package
    (name "node-glob-parent")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/glob-parent/-/glob-parent-2.0.0.tgz")
        (sha256
          (base32
            "0kc5f3hd4n5aj58gvaslcfqnadx3zwk1qwmilgggqf5jv4wc0cb3"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-is-glob" ,node-is-glob-2.0.1)))
    (home-page
      "https://github.com/es128/glob-parent")
    (synopsis
      "Strips glob magic from a string to provide the parent path")
    (description
      "Strips glob magic from a string to provide the parent path")
    (license license:isc)))

(define-public node-inherits-2.0.4
  (package
    (name "node-inherits")
    (version "2.0.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/inherits/-/inherits-2.0.4.tgz")
        (sha256
          (base32
            "1bxg4igfni2hymabg8bkw86wd3qhhzhsswran47sridk3dnbqkfr"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/isaacs/inherits#readme")
    (synopsis
      "Browser-friendly inheritance fully compatible with standard node.js inherits()")
    (description
      "Browser-friendly inheritance fully compatible with standard node.js inherits()")
    (license license:isc)))

(define-public node-binary-extensions-1.13.1
  (package
    (name "node-binary-extensions")
    (version "1.13.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/binary-extensions/-/binary-extensions-1.13.1.tgz")
        (sha256
          (base32
            "172h5sjqa46zwblrzbjqdr0v3jjcasfvsgm6zqq5jgd6xxjq2kkh"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/binary-extensions#readme")
    (synopsis "List of binary file extensions")
    (description "List of binary file extensions")
    (license license:expat)))

(define-public node-is-binary-path-1.0.1
  (package
    (name "node-is-binary-path")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-binary-path/-/is-binary-path-1.0.1.tgz")
        (sha256
          (base32
            "05ylaj9wal46fvlzz34fbfmb0v870bc69k1h9l0lqgkbs1091vpl"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-binary-extensions"
         ,node-binary-extensions-1.13.1)))
    (home-page
      "https://github.com/sindresorhus/is-binary-path")
    (synopsis "Check if a filepath is a binary file")
    (description
      "Check if a filepath is a binary file")
    (license license:expat)))

(define-public node-is-extglob-1.0.0
  (package
    (name "node-is-extglob")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-extglob/-/is-extglob-1.0.0.tgz")
        (sha256
          (base32
            "17arbivg9gky2l24xr5jjr2zpqslq73amb7sgp1flnadqcxmcgj7"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jonschlinkert/is-extglob")
    (synopsis
      "Returns true if a string has an extglob.")
    (description
      "Returns true if a string has an extglob.")
    (license license:expat)))

(define-public node-graceful-fs-4.2.6
  (package
    (name "node-graceful-fs")
    (version "4.2.6")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/graceful-fs/-/graceful-fs-4.2.6.tgz")
        (sha256
          (base32
            "0dz5rck3zvvblkxq2234654axjslp6ackixnb5dsh9nzxm8l4cr5"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/isaacs/node-graceful-fs#readme")
    (synopsis
      "A drop-in replacement for fs, making various improvements.")
    (description
      "A drop-in replacement for fs, making various improvements.")
    (license license:isc)))

(define-public node-to-regex-range-2.1.1
  (package
    (name "node-to-regex-range")
    (version "2.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/to-regex-range/-/to-regex-range-2.1.1.tgz")
        (sha256
          (base32
            "0rw8mjvncwxhyg5m7mzwqg16ddpyq5qzdwds0v0jnskqhklh14bq"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-repeat-string" ,node-repeat-string-1.6.1)
        ("node-is-number" ,node-is-number-3.0.0)))
    (home-page
      "https://github.com/micromatch/to-regex-range")
    (synopsis
      "Pass two numbers, get a regex-compatible source string for matching ranges. Validated against more than 2.78 million test assertions.")
    (description
      "Pass two numbers, get a regex-compatible source string for matching ranges. Validated against more than 2.78 million test assertions.")
    (license license:expat)))

(define-public node-fill-range-4.0.0
  (package
    (name "node-fill-range")
    (version "4.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/fill-range/-/fill-range-4.0.0.tgz")
        (sha256
          (base32
            "14kaakn1yhkfsclqds0pg1g87aibi8nkrwn0axvfasj495hv2wzx"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-to-regex-range"
         ,node-to-regex-range-2.1.1)
        ("node-repeat-string" ,node-repeat-string-1.6.1)
        ("node-is-number" ,node-is-number-3.0.0)
        ("node-extend-shallow"
         ,node-extend-shallow-2.0.1)))
    (home-page
      "https://github.com/jonschlinkert/fill-range")
    (synopsis
      "Fill in a range of numbers or letters, optionally passing an increment or `step` to use, or create a regex-compatible range with `options.toRegex`")
    (description
      "Fill in a range of numbers or letters, optionally passing an increment or `step` to use, or create a regex-compatible range with `options.toRegex`")
    (license license:expat)))

(define-public node-repeat-element-1.1.3
  (package
    (name "node-repeat-element")
    (version "1.1.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/repeat-element/-/repeat-element-1.1.3.tgz")
        (sha256
          (base32
            "1p71vsqclms9dxadcpnaajh8jkp3acxlkv6xic7f9x98dpdss8g6"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jonschlinkert/repeat-element")
    (synopsis
      "Create an array by repeating the given value n times.")
    (description
      "Create an array by repeating the given value n times.")
    (license license:expat)))

(define-public node-kind-of-3.2.2
  (package
    (name "node-kind-of")
    (version "3.2.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/kind-of/-/kind-of-3.2.2.tgz")
        (sha256
          (base32
            "0isxns331nf5f4h8yj0vb4rj626bscxh1rh7j92vk8lbzcs2x93q"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-is-buffer" ,node-is-buffer-1.1.6)))
    (home-page
      "https://github.com/jonschlinkert/kind-of")
    (synopsis "Get the native type of a value.")
    (description "Get the native type of a value.")
    (license license:expat)))

(define-public node-snapdragon-util-3.0.1
  (package
    (name "node-snapdragon-util")
    (version "3.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/snapdragon-util/-/snapdragon-util-3.0.1.tgz")
        (sha256
          (base32
            "0c4fcrilagmpsrhh4mjfj7ah0vdxwkm9a4h5658bj8m7machxm2f"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-kind-of" ,node-kind-of-3.2.2)))
    (home-page
      "https://github.com/jonschlinkert/snapdragon-util")
    (synopsis
      "Utilities for the snapdragon parser/compiler.")
    (description
      "Utilities for the snapdragon parser/compiler.")
    (license license:expat)))

(define-public node-snapdragon-node-2.1.1
  (package
    (name "node-snapdragon-node")
    (version "2.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/snapdragon-node/-/snapdragon-node-2.1.1.tgz")
        (sha256
          (base32
            "0idm24bf2jvwgqi8fx1fkn1w78ylqnpm137s2m0g0ni43y19i97j"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-snapdragon-util"
         ,node-snapdragon-util-3.0.1)
        ("node-isobject" ,node-isobject-3.0.1)
        ("node-define-property"
         ,node-define-property-1.0.0)))
    (home-page
      "https://github.com/jonschlinkert/snapdragon-node")
    (synopsis
      "Snapdragon utility for creating a new AST node in custom code, such as plugins.")
    (description
      "Snapdragon utility for creating a new AST node in custom code, such as plugins.")
    (license license:expat)))

(define-public node-split-string-3.1.0
  (package
    (name "node-split-string")
    (version "3.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/split-string/-/split-string-3.1.0.tgz")
        (sha256
          (base32
            "1bx5n7bga42bd5d804w7y06wx96qk524gylxl4xi3i8nb8ch86na"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-extend-shallow"
         ,node-extend-shallow-3.0.2)))
    (home-page
      "https://github.com/jonschlinkert/split-string")
    (synopsis
      "Split a string on a character except when the character is escaped.")
    (description
      "Split a string on a character except when the character is escaped.")
    (license license:expat)))

(define-public node-braces-2.3.2
  (package
    (name "node-braces")
    (version "2.3.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/braces/-/braces-2.3.2.tgz")
        (sha256
          (base32
            "10608dfl1pxajw0nwrsh69769q659yjyw88b0mrlprnn0z1wya1l"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-to-regex" ,node-to-regex-3.0.2)
        ("node-split-string" ,node-split-string-3.1.0)
        ("node-snapdragon-node"
         ,node-snapdragon-node-2.1.1)
        ("node-snapdragon" ,node-snapdragon-0.8.2)
        ("node-repeat-element"
         ,node-repeat-element-1.1.3)
        ("node-isobject" ,node-isobject-3.0.1)
        ("node-fill-range" ,node-fill-range-4.0.0)
        ("node-extend-shallow"
         ,node-extend-shallow-2.0.1)
        ("node-array-unique" ,node-array-unique-0.3.2)
        ("node-arr-flatten" ,node-arr-flatten-1.1.0)))
    (home-page
      "https://github.com/micromatch/braces")
    (synopsis
      "Bash-like brace expansion, implemented in JavaScript. Safer than other brace expansion libs, with complete support for the Bash 4.3 braces specification, without sacrificing speed.")
    (description
      "Bash-like brace expansion, implemented in JavaScript. Safer than other brace expansion libs, with complete support for the Bash 4.3 braces specification, without sacrificing speed.")
    (license license:expat)))

(define-public node-debug-2.6.9
  (package
    (name "node-debug")
    (version "2.6.9")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/debug/-/debug-2.6.9.tgz")
        (sha256
          (base32
            "160wvc74r8aypds7pym3hq4qpa786hpk4vif58ggiwcqcv34ibil"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-ms" ,node-ms-2.0.0)))
    (home-page
      "https://github.com/visionmedia/debug#readme")
    (synopsis "small debugging utility")
    (description "small debugging utility")
    (license license:expat)))

(define-public node-posix-character-classes-0.1.1
  (package
    (name "node-posix-character-classes")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/posix-character-classes/-/posix-character-classes-0.1.1.tgz")
        (sha256
          (base32
            "08c07ib7iaj34d1mnjhll0bq0yh3kb98q4mf334js2l4166y9rcr"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jonschlinkert/posix-character-classes")
    (synopsis
      "POSIX character classes for creating regular expressions.")
    (description
      "POSIX character classes for creating regular expressions.")
    (license license:expat)))

(define-public node-expand-brackets-2.1.4
  (package
    (name "node-expand-brackets")
    (version "2.1.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/expand-brackets/-/expand-brackets-2.1.4.tgz")
        (sha256
          (base32
            "0csxpxfx1xkf2dp10vpifynkrx8csx7md7gysvhy5xr094hdr3nq"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-to-regex" ,node-to-regex-3.0.2)
        ("node-snapdragon" ,node-snapdragon-0.8.2)
        ("node-regex-not" ,node-regex-not-1.0.2)
        ("node-posix-character-classes"
         ,node-posix-character-classes-0.1.1)
        ("node-extend-shallow"
         ,node-extend-shallow-2.0.1)
        ("node-define-property"
         ,node-define-property-0.2.5)
        ("node-debug" ,node-debug-2.6.9)))
    (home-page
      "https://github.com/jonschlinkert/expand-brackets")
    (synopsis
      "Expand POSIX bracket expressions (character classes) in glob patterns.")
    (description
      "Expand POSIX bracket expressions (character classes) in glob patterns.")
    (license license:expat)))

(define-public node-extglob-2.0.4
  (package
    (name "node-extglob")
    (version "2.0.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/extglob/-/extglob-2.0.4.tgz")
        (sha256
          (base32
            "1wza438hvcr83b28zic65h8jq8k0p0v9iw9b1lfk1zhb5xrkp8sy"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-to-regex" ,node-to-regex-3.0.2)
        ("node-snapdragon" ,node-snapdragon-0.8.2)
        ("node-regex-not" ,node-regex-not-1.0.2)
        ("node-fragment-cache"
         ,node-fragment-cache-0.2.1)
        ("node-extend-shallow"
         ,node-extend-shallow-2.0.1)
        ("node-expand-brackets"
         ,node-expand-brackets-2.1.4)
        ("node-define-property"
         ,node-define-property-1.0.0)
        ("node-array-unique" ,node-array-unique-0.3.2)))
    (home-page
      "https://github.com/micromatch/extglob")
    (synopsis
      "Extended glob support for JavaScript. Adds (almost) the expressive power of regular expressions to glob patterns.")
    (description
      "Extended glob support for JavaScript. Adds (almost) the expressive power of regular expressions to glob patterns.")
    (license license:expat)))

(define-public node-arr-diff-4.0.0
  (package
    (name "node-arr-diff")
    (version "4.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/arr-diff/-/arr-diff-4.0.0.tgz")
        (sha256
          (base32
            "1735byq6vmrvqkv9n5400494mh9vd6x4knzkybgr55km5dgpdcyx"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jonschlinkert/arr-diff")
    (synopsis
      "Returns an array with only the unique values from the first array, by excluding all values from additional arrays using strict equality for comparisons.")
    (description
      "Returns an array with only the unique values from the first array, by excluding all values from additional arrays using strict equality for comparisons.")
    (license license:expat)))

(define-public node-array-unique-0.3.2
  (package
    (name "node-array-unique")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/array-unique/-/array-unique-0.3.2.tgz")
        (sha256
          (base32
            "0bkrb481qri7qad5h4bzw9hq161wfqgxdj5g11a5bnlfylqczg9g"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jonschlinkert/array-unique")
    (synopsis
      "Remove duplicate values from an array. Fastest ES5 implementation.")
    (description
      "Remove duplicate values from an array. Fastest ES5 implementation.")
    (license license:expat)))

(define-public node-fragment-cache-0.2.1
  (package
    (name "node-fragment-cache")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/fragment-cache/-/fragment-cache-0.2.1.tgz")
        (sha256
          (base32
            "114jfm5qpvz52127hf4ny4vz1qs7a597hql5lj0g2ncixn8sqg76"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-map-cache" ,node-map-cache-0.2.2)))
    (home-page
      "https://github.com/jonschlinkert/fragment-cache")
    (synopsis
      "A cache for managing namespaced sub-caches")
    (description
      "A cache for managing namespaced sub-caches")
    (license license:expat)))

(define-public node-is-windows-1.0.2
  (package
    (name "node-is-windows")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-windows/-/is-windows-1.0.2.tgz")
        (sha256
          (base32
            "18iihzz6fs6sfrgq1bpvgkfk3cza6r9wrrgn7ahcbbra1lkjh4bv"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jonschlinkert/is-windows")
    (synopsis
      "Returns true if the platform is windows. UMD module, works with node.js, commonjs, browser, AMD, electron, etc.")
    (description
      "Returns true if the platform is windows. UMD module, works with node.js, commonjs, browser, AMD, electron, etc.")
    (license license:expat)))

(define-public node-to-regex-3.0.2
  (package
    (name "node-to-regex")
    (version "3.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/to-regex/-/to-regex-3.0.2.tgz")
        (sha256
          (base32
            "039l28qygjrjy10jz9cm3j066nw5fkfimzkp5ipq47w2pl2ii0w6"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-safe-regex" ,node-safe-regex-1.1.0)
        ("node-regex-not" ,node-regex-not-1.0.2)
        ("node-extend-shallow"
         ,node-extend-shallow-3.0.2)
        ("node-define-property"
         ,node-define-property-2.0.2)))
    (home-page
      "https://github.com/jonschlinkert/to-regex")
    (synopsis
      "Generate a regex from a string or array of strings.")
    (description
      "Generate a regex from a string or array of strings.")
    (license license:expat)))

(define-public node-nanomatch-1.2.13
  (package
    (name "node-nanomatch")
    (version "1.2.13")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/nanomatch/-/nanomatch-1.2.13.tgz")
        (sha256
          (base32
            "1f4fxk7azvglyi6gfbkxmh91pd3n6i7av03y9bpizfn37xjf7g0z"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-to-regex" ,node-to-regex-3.0.2)
        ("node-snapdragon" ,node-snapdragon-0.8.2)
        ("node-regex-not" ,node-regex-not-1.0.2)
        ("node-object-pick" ,node-object-pick-1.3.0)
        ("node-kind-of" ,node-kind-of-6.0.3)
        ("node-is-windows" ,node-is-windows-1.0.2)
        ("node-fragment-cache"
         ,node-fragment-cache-0.2.1)
        ("node-extend-shallow"
         ,node-extend-shallow-3.0.2)
        ("node-define-property"
         ,node-define-property-2.0.2)
        ("node-array-unique" ,node-array-unique-0.3.2)
        ("node-arr-diff" ,node-arr-diff-4.0.0)))
    (home-page
      "https://github.com/micromatch/nanomatch")
    (synopsis
      "Fast, minimal glob matcher for node.js. Similar to micromatch, minimatch and multimatch, but complete Bash 4.3 wildcard support only (no support for exglobs, posix brackets or braces)")
    (description
      "Fast, minimal glob matcher for node.js. Similar to micromatch, minimatch and multimatch, but complete Bash 4.3 wildcard support only (no support for exglobs, posix brackets or braces)")
    (license license:expat)))

(define-public node-object-pick-1.3.0
  (package
    (name "node-object-pick")
    (version "1.3.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/object.pick/-/object.pick-1.3.0.tgz")
        (sha256
          (base32
            "02zfyg9vkizb5vanjy3d976cnbjnx4qrcjrd92z2ylyl4ih24040"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-isobject" ,node-isobject-3.0.1)))
    (home-page
      "https://github.com/jonschlinkert/object.pick")
    (synopsis
      "Returns a filtered copy of an object with only the specified keys, similar to `_.pick` from lodash / underscore.")
    (description
      "Returns a filtered copy of an object with only the specified keys, similar to `_.pick` from lodash / underscore.")
    (license license:expat)))

(define-public node-regex-not-1.0.2
  (package
    (name "node-regex-not")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/regex-not/-/regex-not-1.0.2.tgz")
        (sha256
          (base32
            "0xpjprkrk0c9fn6yhqay3bm8vw44k197smcfby89g3sfjsxlhi7s"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-safe-regex" ,node-safe-regex-1.1.0)
        ("node-extend-shallow"
         ,node-extend-shallow-3.0.2)))
    (home-page
      "https://github.com/jonschlinkert/regex-not")
    (synopsis
      "Create a javascript regular expression for matching everything except for the given string.")
    (description
      "Create a javascript regular expression for matching everything except for the given string.")
    (license license:expat)))

(define-public node-map-visit-1.0.0
  (package
    (name "node-map-visit")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/map-visit/-/map-visit-1.0.0.tgz")
        (sha256
          (base32
            "19fhyr0jmskx32s1s9b6p0jb96gmbxxnbmzx4sc42sxzwkfmqvpi"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-object-visit" ,node-object-visit-1.0.1)))
    (home-page
      "https://github.com/jonschlinkert/map-visit")
    (synopsis
      "Map `visit` over an array of objects.")
    (description
      "Map `visit` over an array of objects.")
    (license license:expat)))

(define-public node-object-visit-1.0.1
  (package
    (name "node-object-visit")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/object-visit/-/object-visit-1.0.1.tgz")
        (sha256
          (base32
            "05qpsh7jyq40dk2mqm85hbcaapb4g4hyjcb4z6b2kcziqfiynpsl"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-isobject" ,node-isobject-3.0.1)))
    (home-page
      "https://github.com/jonschlinkert/object-visit")
    (synopsis
      "Call a specified method on each value in the given object.")
    (description
      "Call a specified method on each value in the given object.")
    (license license:expat)))

(define-public node-collection-visit-1.0.0
  (package
    (name "node-collection-visit")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/collection-visit/-/collection-visit-1.0.0.tgz")
        (sha256
          (base32
            "062q78a3fjfvk4vplkay9hd4rx6f1xw4r0438sa0hbv62jf5bc46"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-object-visit" ,node-object-visit-1.0.1)
        ("node-map-visit" ,node-map-visit-1.0.0)))
    (home-page
      "https://github.com/jonschlinkert/collection-visit")
    (synopsis
      "Visit a method over the items in an object, or map visit over the objects in an array.")
    (description
      "Visit a method over the items in an object, or map visit over the objects in an array.")
    (license license:expat)))

(define-public node-is-number-3.0.0
  (package
    (name "node-is-number")
    (version "3.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-number/-/is-number-3.0.0.tgz")
        (sha256
          (base32
            "19rpbi5ryx3y28bh0pwm99az1mridh0p2sinfdxkcbpbxfx5zbf4"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-kind-of" ,node-kind-of-3.2.2)))
    (home-page
      "https://github.com/jonschlinkert/is-number")
    (synopsis
      "Returns true if the value is a number. comprehensive tests.")
    (description
      "Returns true if the value is a number. comprehensive tests.")
    (license license:expat)))

(define-public node-kind-of-4.0.0
  (package
    (name "node-kind-of")
    (version "4.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/kind-of/-/kind-of-4.0.0.tgz")
        (sha256
          (base32
            "0afwg6007r7l0c9r7jghdk8pwvfffmykhybpwrs0nlks59d2ziym"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-is-buffer" ,node-is-buffer-1.1.6)))
    (home-page
      "https://github.com/jonschlinkert/kind-of")
    (synopsis "Get the native type of a value.")
    (description "Get the native type of a value.")
    (license license:expat)))

(define-public node-has-values-1.0.0
  (package
    (name "node-has-values")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/has-values/-/has-values-1.0.0.tgz")
        (sha256
          (base32
            "19ggazh85imjwxnvvhabypdpmyl0q4mgm3frwfx2dx3ffq1g1bwc"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-kind-of" ,node-kind-of-4.0.0)
        ("node-is-number" ,node-is-number-3.0.0)))
    (home-page
      "https://github.com/jonschlinkert/has-values")
    (synopsis
      "Returns true if any values exist, false if empty. Works for booleans, functions, numbers, strings, nulls, objects and arrays. ")
    (description
      "Returns true if any values exist, false if empty. Works for booleans, functions, numbers, strings, nulls, objects and arrays. ")
    (license license:expat)))

(define-public node-has-value-1.0.0
  (package
    (name "node-has-value")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/has-value/-/has-value-1.0.0.tgz")
        (sha256
          (base32
            "1ikpbff1imw3nqqp9q9siwh7p93r1pdigiv6kgryx6l8hwiwbxr9"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-isobject" ,node-isobject-3.0.1)
        ("node-has-values" ,node-has-values-1.0.0)
        ("node-get-value" ,node-get-value-2.0.6)))
    (home-page
      "https://github.com/jonschlinkert/has-value")
    (synopsis
      "Returns true if a value exists, false if empty. Works with deeply nested values using object paths.")
    (description
      "Returns true if a value exists, false if empty. Works with deeply nested values using object paths.")
    (license license:expat)))

(define-public node-set-value-2.0.1
  (package
    (name "node-set-value")
    (version "2.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/set-value/-/set-value-2.0.1.tgz")
        (sha256
          (base32
            "0qbp2ndx2qmmn6i7y92lk8jaj79cv36gpv9lq29lgw52wr6jbrl0"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-split-string" ,node-split-string-3.1.0)
        ("node-is-plain-object"
         ,node-is-plain-object-2.0.4)
        ("node-is-extendable" ,node-is-extendable-0.1.1)
        ("node-extend-shallow"
         ,node-extend-shallow-2.0.1)))
    (home-page
      "https://github.com/jonschlinkert/set-value")
    (synopsis
      "Create nested values and any intermediaries using dot notation (`'a.b.c'`) paths.")
    (description
      "Create nested values and any intermediaries using dot notation (`'a.b.c'`) paths.")
    (license license:expat)))

(define-public node-to-object-path-0.3.0
  (package
    (name "node-to-object-path")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/to-object-path/-/to-object-path-0.3.0.tgz")
        (sha256
          (base32
            "01n40v8xlqm635rp6cyz0jpw6295wm9fkr6m85nqcf457rfbqc68"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-kind-of" ,node-kind-of-3.2.2)))
    (home-page
      "https://github.com/jonschlinkert/to-object-path")
    (synopsis
      "Create an object path from a list or array of strings.")
    (description
      "Create an object path from a list or array of strings.")
    (license license:expat)))

(define-public node-get-value-2.0.6
  (package
    (name "node-get-value")
    (version "2.0.6")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/get-value/-/get-value-2.0.6.tgz")
        (sha256
          (base32
            "057wz0ya7zlgz59m08zbaasvzfdxjzgd6bzphd2xwsydhjwnw02l"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jonschlinkert/get-value")
    (synopsis
      "Use property paths (`a.b.c`) to get a nested value from an object.")
    (description
      "Use property paths (`a.b.c`) to get a nested value from an object.")
    (license license:expat)))

(define-public node-is-extendable-0.1.1
  (package
    (name "node-is-extendable")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-extendable/-/is-extendable-0.1.1.tgz")
        (sha256
          (base32
            "12f91w1hcv9hw2jlrxf3831zhw7fb0bmzdybzsqb71h5phyjnd7b"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jonschlinkert/is-extendable")
    (synopsis
      "Returns true if a value is any of the object types: array, regexp, plain object, function or date. This is useful for determining if a value can be extended, e.g. \"can the value have keys?\"")
    (description
      "Returns true if a value is any of the object types: array, regexp, plain object, function or date. This is useful for determining if a value can be extended, e.g. \"can the value have keys?\"")
    (license license:expat)))

(define-public node-is-plain-object-2.0.4
  (package
    (name "node-is-plain-object")
    (version "2.0.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-plain-object/-/is-plain-object-2.0.4.tgz")
        (sha256
          (base32
            "1ipx9y0c1kmq6irjxix6vcxfax6ilnns9pkgjc6cq8ygnyagv4s8"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-isobject" ,node-isobject-3.0.1)))
    (home-page
      "https://github.com/jonschlinkert/is-plain-object")
    (synopsis
      "Returns true if an object was created by the `Object` constructor.")
    (description
      "Returns true if an object was created by the `Object` constructor.")
    (license license:expat)))

(define-public node-extend-shallow-3.0.2
  (package
    (name "node-extend-shallow")
    (version "3.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/extend-shallow/-/extend-shallow-3.0.2.tgz")
        (sha256
          (base32
            "02bickcbljfrxfix2rbvq5j2cdlwm0dqflxc2ky4l6jp97bcl6m0"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-is-extendable" ,node-is-extendable-1.0.1)
        ("node-assign-symbols"
         ,node-assign-symbols-1.0.0)))
    (home-page
      "https://github.com/jonschlinkert/extend-shallow")
    (synopsis
      "Extend an object with the properties of additional objects. node.js/javascript util.")
    (description
      "Extend an object with the properties of additional objects. node.js/javascript util.")
    (license license:expat)))

(define-public node-union-value-1.0.1
  (package
    (name "node-union-value")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/union-value/-/union-value-1.0.1.tgz")
        (sha256
          (base32
            "00rjw4hvxnj5vrji9qzbxn6y9rx6av1q3nv8ilyxpska3q0zpj75"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-set-value" ,node-set-value-2.0.1)
        ("node-is-extendable" ,node-is-extendable-0.1.1)
        ("node-get-value" ,node-get-value-2.0.6)
        ("node-arr-union" ,node-arr-union-3.1.0)))
    (home-page
      "https://github.com/jonschlinkert/union-value")
    (synopsis
      "Set an array of unique values as the property of an object. Supports setting deeply nested properties using using object-paths/dot notation.")
    (description
      "Set an array of unique values as the property of an object. Supports setting deeply nested properties using using object-paths/dot notation.")
    (license license:expat)))

(define-public node-has-values-0.1.4
  (package
    (name "node-has-values")
    (version "0.1.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/has-values/-/has-values-0.1.4.tgz")
        (sha256
          (base32
            "1lnp2ww5w0lxxiqrm3scyaddj7csdrd4ldsmsvapys4rdfgnx0m6"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jonschlinkert/has-values")
    (synopsis
      "Returns true if any values exist, false if empty. Works for booleans, functions, numbers, strings, nulls, objects and arrays. ")
    (description
      "Returns true if any values exist, false if empty. Works for booleans, functions, numbers, strings, nulls, objects and arrays. ")
    (license license:expat)))

(define-public node-isarray-1.0.0
  (package
    (name "node-isarray")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/isarray/-/isarray-1.0.0.tgz")
        (sha256
          (base32
            "11qcjpdzigcwcprhv7nyarlzjcwf3sv5i66q75zf08jj9zqpcg72"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/juliangruber/isarray")
    (synopsis "Array#isArray for older browsers")
    (description "Array#isArray for older browsers")
    (license license:expat)))

(define-public node-isobject-2.1.0
  (package
    (name "node-isobject")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/isobject/-/isobject-2.1.0.tgz")
        (sha256
          (base32
            "14df1spjczhml90421sq645shkxwggrbbkgp1qgal29kslc7av32"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-isarray" ,node-isarray-1.0.0)))
    (home-page
      "https://github.com/jonschlinkert/isobject")
    (synopsis
      "Returns true if the value is an object and not an array or null.")
    (description
      "Returns true if the value is an object and not an array or null.")
    (license license:expat)))

(define-public node-has-value-0.3.1
  (package
    (name "node-has-value")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/has-value/-/has-value-0.3.1.tgz")
        (sha256
          (base32
            "1h4dnzr9rszpj0a015529r1c9g8ysy4wym6ay99vc6dls02jhmny"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-isobject" ,node-isobject-2.1.0)
        ("node-has-values" ,node-has-values-0.1.4)
        ("node-get-value" ,node-get-value-2.0.6)))
    (home-page
      "https://github.com/jonschlinkert/has-value")
    (synopsis
      "Returns true if a value exists, false if empty. Works with deeply nested values using object paths.")
    (description
      "Returns true if a value exists, false if empty. Works with deeply nested values using object paths.")
    (license license:expat)))

(define-public node-unset-value-1.0.0
  (package
    (name "node-unset-value")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/unset-value/-/unset-value-1.0.0.tgz")
        (sha256
          (base32
            "11jj8ggkz8c54sf0zyqwlnwv89i5gj572ywbry7ispy6lqa84qsk"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-isobject" ,node-isobject-3.0.1)
        ("node-has-value" ,node-has-value-0.3.1)))
    (home-page
      "https://github.com/jonschlinkert/unset-value")
    (synopsis
      "Delete nested properties from an object using dot notation.")
    (description
      "Delete nested properties from an object using dot notation.")
    (license license:expat)))

(define-public node-cache-base-1.0.1
  (package
    (name "node-cache-base")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/cache-base/-/cache-base-1.0.1.tgz")
        (sha256
          (base32
            "1iny3winp8x5ac39hx52baq60g2hsp4pcx95a634c83klawdaw78"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-unset-value" ,node-unset-value-1.0.0)
        ("node-union-value" ,node-union-value-1.0.1)
        ("node-to-object-path"
         ,node-to-object-path-0.3.0)
        ("node-set-value" ,node-set-value-2.0.1)
        ("node-isobject" ,node-isobject-3.0.1)
        ("node-has-value" ,node-has-value-1.0.0)
        ("node-get-value" ,node-get-value-2.0.6)
        ("node-component-emitter"
         ,node-component-emitter-1.3.0)
        ("node-collection-visit"
         ,node-collection-visit-1.0.0)))
    (home-page
      "https://github.com/jonschlinkert/cache-base")
    (synopsis
      "Basic object cache with `get`, `set`, `del`, and `has` methods for node.js/javascript projects.")
    (description
      "Basic object cache with `get`, `set`, `del`, and `has` methods for node.js/javascript projects.")
    (license license:expat)))

(define-public node-arr-union-3.1.0
  (package
    (name "node-arr-union")
    (version "3.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/arr-union/-/arr-union-3.1.0.tgz")
        (sha256
          (base32
            "1jrcfq6xnx3lbvnpl9pzfvc2v3bcgyir1vwcc7p0slhf6gglzd3p"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jonschlinkert/arr-union")
    (synopsis
      "Combines a list of arrays, returning a single array with unique values, using strict equality for comparisons.")
    (description
      "Combines a list of arrays, returning a single array with unique values, using strict equality for comparisons.")
    (license license:expat)))

(define-public node-isobject-3.0.1
  (package
    (name "node-isobject")
    (version "3.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/isobject/-/isobject-3.0.1.tgz")
        (sha256
          (base32
            "0dvx6rhjj5b9q7fcjg24lfy2nr3a1d2ypqy9zf9lqr2s00mwkiiw"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jonschlinkert/isobject")
    (synopsis
      "Returns true if the value is an object and not an array or null.")
    (description
      "Returns true if the value is an object and not an array or null.")
    (license license:expat)))

(define-public node-copy-descriptor-0.1.1
  (package
    (name "node-copy-descriptor")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/copy-descriptor/-/copy-descriptor-0.1.1.tgz")
        (sha256
          (base32
            "1dmlg6g04hfn1kmjklw6l33va255gsml1mrlz07jfv2a4alz4470"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jonschlinkert/copy-descriptor")
    (synopsis
      "Copy a descriptor from object A to object B")
    (description
      "Copy a descriptor from object A to object B")
    (license license:expat)))

(define-public node-object-copy-0.1.0
  (package
    (name "node-object-copy")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/object-copy/-/object-copy-0.1.0.tgz")
        (sha256
          (base32
            "1b59pq32z0kdzhjkwphyk5h0m59gcc4qvmkvq7zb49yla00w5f0w"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-kind-of" ,node-kind-of-3.2.2)
        ("node-define-property"
         ,node-define-property-0.2.5)
        ("node-copy-descriptor"
         ,node-copy-descriptor-0.1.1)))
    (home-page
      "https://github.com/jonschlinkert/object-copy")
    (synopsis
      "Copy static properties, prototype properties, and descriptors from one object to another.")
    (description
      "Copy static properties, prototype properties, and descriptors from one object to another.")
    (license license:expat)))

(define-public node-static-extend-0.1.2
  (package
    (name "node-static-extend")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/static-extend/-/static-extend-0.1.2.tgz")
        (sha256
          (base32
            "1hwg7diq3kg6q7d2ymj423562yx6nfdqlym538s6ca02zxjgi7nl"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-object-copy" ,node-object-copy-0.1.0)
        ("node-define-property"
         ,node-define-property-0.2.5)))
    (home-page
      "https://github.com/jonschlinkert/static-extend")
    (synopsis
      "Adds a static `extend` method to a class, to simplify inheritance. Extends the static properties, prototype properties, and descriptors from a `Parent` constructor onto `Child` constructors.")
    (description
      "Adds a static `extend` method to a class, to simplify inheritance. Extends the static properties, prototype properties, and descriptors from a `Parent` constructor onto `Child` constructors.")
    (license license:expat)))

(define-public node-class-utils-0.3.6
  (package
    (name "node-class-utils")
    (version "0.3.6")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/class-utils/-/class-utils-0.3.6.tgz")
        (sha256
          (base32
            "0567alh9j9bl7rj91frmjdpb4q5zig9rydzkdm9pmij85h7sffjh"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-static-extend" ,node-static-extend-0.1.2)
        ("node-isobject" ,node-isobject-3.0.1)
        ("node-define-property"
         ,node-define-property-0.2.5)
        ("node-arr-union" ,node-arr-union-3.1.0)))
    (home-page
      "https://github.com/jonschlinkert/class-utils")
    (synopsis
      "Utils for working with JavaScript classes and prototype methods.")
    (description
      "Utils for working with JavaScript classes and prototype methods.")
    (license license:expat)))

(define-public node-component-emitter-1.3.0
  (package
    (name "node-component-emitter")
    (version "1.3.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/component-emitter/-/component-emitter-1.3.0.tgz")
        (sha256
          (base32
            "0qc1qx0ngvah8lmkn0d784vlxmya2kr5gpjcpfikxlpwx5v3wr0h"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/component/emitter#readme")
    (synopsis "Event emitter")
    (description "Event emitter")
    (license license:expat)))

(define-public node-is-descriptor-1.0.2
  (package
    (name "node-is-descriptor")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-descriptor/-/is-descriptor-1.0.2.tgz")
        (sha256
          (base32
            "1bbfdklsskykp1qw9h2mpxjk7hjh6685s1raq73lxbn2b6iyfppy"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-kind-of" ,node-kind-of-6.0.3)
        ("node-is-data-descriptor"
         ,node-is-data-descriptor-1.0.0)
        ("node-is-accessor-descriptor"
         ,node-is-accessor-descriptor-1.0.0)))
    (home-page
      "https://github.com/jonschlinkert/is-descriptor")
    (synopsis
      "Returns true if a value has the characteristics of a valid JavaScript descriptor. Works for data descriptors and accessor descriptors.")
    (description
      "Returns true if a value has the characteristics of a valid JavaScript descriptor. Works for data descriptors and accessor descriptors.")
    (license license:expat)))

(define-public node-define-property-1.0.0
  (package
    (name "node-define-property")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/define-property/-/define-property-1.0.0.tgz")
        (sha256
          (base32
            "1547m4v074hgd28jzv4k82ig43pl7rgfnp0y832swxmlff4ra6m6"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-is-descriptor" ,node-is-descriptor-1.0.2)))
    (home-page
      "https://github.com/jonschlinkert/define-property")
    (synopsis
      "Define a non-enumerable property on an object.")
    (description
      "Define a non-enumerable property on an object.")
    (license license:expat)))

(define-public node-mixin-deep-1.3.2
  (package
    (name "node-mixin-deep")
    (version "1.3.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/mixin-deep/-/mixin-deep-1.3.2.tgz")
        (sha256
          (base32
            "0kdw3h2r5cpfazjdfjzh9yac7c2gb1vrl3nxm0bhl7pb3yh276iq"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-is-extendable" ,node-is-extendable-1.0.1)
        ("node-for-in" ,node-for-in-1.0.2)))
    (home-page
      "https://github.com/jonschlinkert/mixin-deep")
    (synopsis
      "Deeply mix the properties of objects into the first object. Like merge-deep, but doesn't clone.")
    (description
      "Deeply mix the properties of objects into the first object. Like merge-deep, but doesn't clone.")
    (license license:expat)))

(define-public node-pascalcase-0.1.1
  (package
    (name "node-pascalcase")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/pascalcase/-/pascalcase-0.1.1.tgz")
        (sha256
          (base32
            "0hd2yjrsfhw3183dxzs5045xnas07in2ww5sl0m5jx035zcrqv2m"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jonschlinkert/pascalcase")
    (synopsis "Convert a string to pascal-case.")
    (description "Convert a string to pascal-case.")
    (license license:expat)))

(define-public node-base-0.11.2
  (package
    (name "node-base")
    (version "0.11.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/base/-/base-0.11.2.tgz")
        (sha256
          (base32
            "0wh3b37238q4x15diq4vp2vx6d6zqh9z0559p0kk6xh0v9b95ca0"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-pascalcase" ,node-pascalcase-0.1.1)
        ("node-mixin-deep" ,node-mixin-deep-1.3.2)
        ("node-isobject" ,node-isobject-3.0.1)
        ("node-define-property"
         ,node-define-property-1.0.0)
        ("node-component-emitter"
         ,node-component-emitter-1.3.0)
        ("node-class-utils" ,node-class-utils-0.3.6)
        ("node-cache-base" ,node-cache-base-1.0.1)))
    (home-page "https://github.com/node-base/base")
    (synopsis
      "base is the foundation for creating modular, unit testable and highly pluggable node.js applications, starting with a handful of common methods, like `set`, `get`, `del` and `use`.")
    (description
      "base is the foundation for creating modular, unit testable and highly pluggable node.js applications, starting with a handful of common methods, like `set`, `get`, `del` and `use`.")
    (license license:expat)))

(define-public node-is-accessor-descriptor-0.1.6
  (package
    (name "node-is-accessor-descriptor")
    (version "0.1.6")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-accessor-descriptor/-/is-accessor-descriptor-0.1.6.tgz")
        (sha256
          (base32
            "1j9kc4m771w28kdrph25q8q62yfiazg2gi6frnbfd66xfmimhmi3"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-kind-of" ,node-kind-of-3.2.2)))
    (home-page
      "https://github.com/jonschlinkert/is-accessor-descriptor")
    (synopsis
      "Returns true if a value has the characteristics of a valid JavaScript accessor descriptor.")
    (description
      "Returns true if a value has the characteristics of a valid JavaScript accessor descriptor.")
    (license license:expat)))

(define-public node-is-data-descriptor-0.1.4
  (package
    (name "node-is-data-descriptor")
    (version "0.1.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-data-descriptor/-/is-data-descriptor-0.1.4.tgz")
        (sha256
          (base32
            "0grcjmph4r8s17dd24mbq3ax09q9qgm6vrpjwkmhsc87nv42m9s3"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-kind-of" ,node-kind-of-3.2.2)))
    (home-page
      "https://github.com/jonschlinkert/is-data-descriptor")
    (synopsis
      "Returns true if a value has the characteristics of a valid JavaScript data descriptor.")
    (description
      "Returns true if a value has the characteristics of a valid JavaScript data descriptor.")
    (license license:expat)))

(define-public node-kind-of-5.1.0
  (package
    (name "node-kind-of")
    (version "5.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/kind-of/-/kind-of-5.1.0.tgz")
        (sha256
          (base32
            "0wkp161zwgg2jp5rjnkw7d7lgczbxj6bwvqvh4s8j68ghk0c8q2d"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jonschlinkert/kind-of")
    (synopsis "Get the native type of a value.")
    (description "Get the native type of a value.")
    (license license:expat)))

(define-public node-is-descriptor-0.1.6
  (package
    (name "node-is-descriptor")
    (version "0.1.6")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-descriptor/-/is-descriptor-0.1.6.tgz")
        (sha256
          (base32
            "0smh1f833y06l7m1qasjms9kv9qjr11bzyaslpz0wq9qmbkl5mdh"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-kind-of" ,node-kind-of-5.1.0)
        ("node-is-data-descriptor"
         ,node-is-data-descriptor-0.1.4)
        ("node-is-accessor-descriptor"
         ,node-is-accessor-descriptor-0.1.6)))
    (home-page
      "https://github.com/jonschlinkert/is-descriptor")
    (synopsis
      "Returns true if a value has the characteristics of a valid JavaScript descriptor. Works for data descriptors and accessor descriptors.")
    (description
      "Returns true if a value has the characteristics of a valid JavaScript descriptor. Works for data descriptors and accessor descriptors.")
    (license license:expat)))

(define-public node-define-property-0.2.5
  (package
    (name "node-define-property")
    (version "0.2.5")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/define-property/-/define-property-0.2.5.tgz")
        (sha256
          (base32
            "1r2gws87mpwv0i1rl3l79bw8psgpz44vwyd9va9cr90bvkada5yk"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-is-descriptor" ,node-is-descriptor-0.1.6)))
    (home-page
      "https://github.com/jonschlinkert/define-property")
    (synopsis
      "Define a non-enumerable property on an object.")
    (description
      "Define a non-enumerable property on an object.")
    (license license:expat)))

(define-public node-extend-shallow-2.0.1
  (package
    (name "node-extend-shallow")
    (version "2.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/extend-shallow/-/extend-shallow-2.0.1.tgz")
        (sha256
          (base32
            "09baxpl8w1rw3qmqmp7w23kyqrxks1hhbvpac0j82gbsgimrlz0v"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-is-extendable" ,node-is-extendable-0.1.1)))
    (home-page
      "https://github.com/jonschlinkert/extend-shallow")
    (synopsis
      "Extend an object with the properties of additional objects. node.js/javascript util.")
    (description
      "Extend an object with the properties of additional objects. node.js/javascript util.")
    (license license:expat)))

(define-public node-map-cache-0.2.2
  (package
    (name "node-map-cache")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/map-cache/-/map-cache-0.2.2.tgz")
        (sha256
          (base32
            "0gag01y8x17l2ffdmi5rgll24bw4cmyi3wksk45xpghirlkrkhj1"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jonschlinkert/map-cache")
    (synopsis
      "Basic cache object for storing key-value pairs.")
    (description
      "Basic cache object for storing key-value pairs.")
    (license license:expat)))

(define-public node-source-map-0.5.7
  (package
    (name "node-source-map")
    (version "0.5.7")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/source-map/-/source-map-0.5.7.tgz")
        (sha256
          (base32
            "0rvb24j4kfib26w3cjyl6yan2dxvw1iy7d0wl404y5ckqjdjipp1"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/mozilla/source-map")
    (synopsis "Generates and consumes source maps")
    (description
      "Generates and consumes source maps")
    (license license:bsd-3)))

(define-public node-atob-2.1.2
  (package
    (name "node-atob")
    (version "2.1.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/atob/-/atob-2.1.2.tgz")
        (sha256
          (base32
            "1nmrnfpzg9a99i4p88knw3470d5vcqmm3hyhavlln96wnza2lbg5"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://git.coolaj86.com/coolaj86/atob.js.git")
    (synopsis
      "atob for Node.JS and Linux / Mac / Windows CLI (it's a one-liner)")
    (description
      "atob for Node.JS and Linux / Mac / Windows CLI (it's a one-liner)")
    (license #f)))

(define-public node-decode-uri-component-0.2.0
  (package
    (name "node-decode-uri-component")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/decode-uri-component/-/decode-uri-component-0.2.0.tgz")
        (sha256
          (base32
            "1mhafchzv526marx5laflfrfz1dsz0mi61r9805rn96dhry44gha"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/samverschueren/decode-uri-component#readme")
    (synopsis "A better decodeURIComponent")
    (description "A better decodeURIComponent")
    (license license:expat)))

(define-public node-resolve-url-0.2.1
  (package
    (name "node-resolve-url")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/resolve-url/-/resolve-url-0.2.1.tgz")
        (sha256
          (base32
            "090qal7agjs8d6x98jrf0wzgx5j85ksbkb3c85f14wv3idbwpsc8"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/lydell/resolve-url")
    (synopsis
      "Like Node.jsâ\x80\x99 `path.resolve`/`url.resolve` for the browser.")
    (description
      "Like Node.jsâ\x80\x99 `path.resolve`/`url.resolve` for the browser.")
    (license license:expat)))

(define-public node-source-map-url-0.4.1
  (package
    (name "node-source-map-url")
    (version "0.4.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/source-map-url/-/source-map-url-0.4.1.tgz")
        (sha256
          (base32
            "10c201qd0ik7n9dyliaypczj4jraalg6lj3ky27y052kyngqa1cc"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/lydell/source-map-url#readme")
    (synopsis
      "Tools for working with sourceMappingURL comments.")
    (description
      "Tools for working with sourceMappingURL comments.")
    (license license:expat)))

(define-public node-urix-0.1.0
  (package
    (name "node-urix")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/urix/-/urix-0.1.0.tgz")
        (sha256
          (base32
            "19qmq8cra96cf7ji8d4ljfcgnazzrvw4lcxlf91jk1816ndx1pbm"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://github.com/lydell/urix")
    (synopsis
      "Makes Windows-style paths more unix and URI friendly.")
    (description
      "Makes Windows-style paths more unix and URI friendly.")
    (license license:expat)))

(define-public node-source-map-resolve-0.5.3
  (package
    (name "node-source-map-resolve")
    (version "0.5.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/source-map-resolve/-/source-map-resolve-0.5.3.tgz")
        (sha256
          (base32
            "1a9ykfyqnzxmna8i3f20g9pqcjzwx1f3wqwaa0bn6jwgmvwcvdj4"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-urix" ,node-urix-0.1.0)
        ("node-source-map-url"
         ,node-source-map-url-0.4.1)
        ("node-resolve-url" ,node-resolve-url-0.2.1)
        ("node-decode-uri-component"
         ,node-decode-uri-component-0.2.0)
        ("node-atob" ,node-atob-2.1.2)))
    (home-page
      "https://github.com/lydell/source-map-resolve#readme")
    (synopsis
      "Resolve the source map and/or sources for a generated file.")
    (description
      "Resolve the source map and/or sources for a generated file.")
    (license license:expat)))

(define-public node-use-3.1.1
  (package
    (name "node-use")
    (version "3.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/use/-/use-3.1.1.tgz")
        (sha256
          (base32
            "1nqrazqb927s0nma2qi2c3aambpy34krz8cgl6610n456zg5vjin"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jonschlinkert/use")
    (synopsis
      "Easily add plugin support to your node.js application.")
    (description
      "Easily add plugin support to your node.js application.")
    (license license:expat)))

(define-public node-snapdragon-0.8.2
  (package
    (name "node-snapdragon")
    (version "0.8.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/snapdragon/-/snapdragon-0.8.2.tgz")
        (sha256
          (base32
            "1anpibb0ajgw2yv400aq30bvvpcyi3yzyrp7fac2ia6r7ysxcgvq"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-use" ,node-use-3.1.1)
        ("node-source-map-resolve"
         ,node-source-map-resolve-0.5.3)
        ("node-source-map" ,node-source-map-0.5.7)
        ("node-map-cache" ,node-map-cache-0.2.2)
        ("node-extend-shallow"
         ,node-extend-shallow-2.0.1)
        ("node-define-property"
         ,node-define-property-0.2.5)
        ("node-debug" ,node-debug-2.6.9)
        ("node-base" ,node-base-0.11.2)))
    (home-page
      "https://github.com/jonschlinkert/snapdragon")
    (synopsis
      "Fast, pluggable and easy-to-use parser-renderer factory.")
    (description
      "Fast, pluggable and easy-to-use parser-renderer factory.")
    (license license:expat)))

(define-public node-is-accessor-descriptor-1.0.0
  (package
    (name "node-is-accessor-descriptor")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-accessor-descriptor/-/is-accessor-descriptor-1.0.0.tgz")
        (sha256
          (base32
            "18ybls0d0q6y7gp8mzhypyr0vbrx1vr5agm07s201byvc5dfmxql"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-kind-of" ,node-kind-of-6.0.3)))
    (home-page
      "https://github.com/jonschlinkert/is-accessor-descriptor")
    (synopsis
      "Returns true if a value has the characteristics of a valid JavaScript accessor descriptor.")
    (description
      "Returns true if a value has the characteristics of a valid JavaScript accessor descriptor.")
    (license license:expat)))

(define-public node-kind-of-6.0.3
  (package
    (name "node-kind-of")
    (version "6.0.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/kind-of/-/kind-of-6.0.3.tgz")
        (sha256
          (base32
            "1nk31q65n9hcmbp16mbn55siqnf44wn1x71rrqyjv9bcbcxl893c"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jonschlinkert/kind-of")
    (synopsis "Get the native type of a value.")
    (description "Get the native type of a value.")
    (license license:expat)))

(define-public node-is-data-descriptor-1.0.0
  (package
    (name "node-is-data-descriptor")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-data-descriptor/-/is-data-descriptor-1.0.0.tgz")
        (sha256
          (base32
            "0b51kish7r330jy625yaz424kvawrh8vxnpajmkx364pw9hm9hi8"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-kind-of" ,node-kind-of-6.0.3)))
    (home-page
      "https://github.com/jonschlinkert/is-data-descriptor")
    (synopsis
      "Returns true if a value has the characteristics of a valid JavaScript data descriptor.")
    (description
      "Returns true if a value has the characteristics of a valid JavaScript data descriptor.")
    (license license:expat)))

(define-public node-define-property-2.0.2
  (package
    (name "node-define-property")
    (version "2.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/define-property/-/define-property-2.0.2.tgz")
        (sha256
          (base32
            "0m8x3myy76d3w777c1jq94gafphxqrpj7sy3myxcvksfk0p8vpha"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-isobject" ,node-isobject-3.0.1)
        ("node-is-descriptor" ,node-is-descriptor-1.0.2)))
    (home-page
      "https://github.com/jonschlinkert/define-property")
    (synopsis
      "Define a non-enumerable property on an object. Uses Reflect.defineProperty when available, otherwise Object.defineProperty.")
    (description
      "Define a non-enumerable property on an object. Uses Reflect.defineProperty when available, otherwise Object.defineProperty.")
    (license license:expat)))

(define-public node-assign-symbols-1.0.0
  (package
    (name "node-assign-symbols")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/assign-symbols/-/assign-symbols-1.0.0.tgz")
        (sha256
          (base32
            "1lpcb6gzhdl4l9843kifsz9z48qwpk5gw6b1h6f6n7h5d8qp2xab"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jonschlinkert/assign-symbols")
    (synopsis
      "Assign the enumerable es6 Symbol properties from an object (or objects) to the first object passed on the arguments. Can be used as a supplement to other extend, assign or merge methods as a polyfill for the Symbols part of the es6 Object.assign method.")
    (description
      "Assign the enumerable es6 Symbol properties from an object (or objects) to the first object passed on the arguments. Can be used as a supplement to other extend, assign or merge methods as a polyfill for the Symbols part of the es6 Object.assign method.")
    (license license:expat)))

(define-public node-is-extendable-1.0.1
  (package
    (name "node-is-extendable")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-extendable/-/is-extendable-1.0.1.tgz")
        (sha256
          (base32
            "0n1610rd5qv9h43zkr464dvk6hns1afmfpzg1g7b7z6as3axkss2"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-is-plain-object"
         ,node-is-plain-object-2.0.4)))
    (home-page
      "https://github.com/jonschlinkert/is-extendable")
    (synopsis
      "Returns true if a value is a plain object, array or function.")
    (description
      "Returns true if a value is a plain object, array or function.")
    (license license:expat)))

(define-public node-ret-0.1.15
  (package
    (name "node-ret")
    (version "0.1.15")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/ret/-/ret-0.1.15.tgz")
        (sha256
          (base32
            "1zk9xw3jzs7di9b31sxg3fi0mljac8w09k0q6m6y311i1fsn4x2a"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/fent/ret.js#readme")
    (synopsis
      "Tokenizes a string that represents a regular expression.")
    (description
      "Tokenizes a string that represents a regular expression.")
    (license license:expat)))

(define-public node-safe-regex-1.1.0
  (package
    (name "node-safe-regex")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/safe-regex/-/safe-regex-1.1.0.tgz")
        (sha256
          (base32
            "1lkcz58mjp3nfdiydh4iynpcfgxhf5mr339swzi5k05sikx8j4k2"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-ret" ,node-ret-0.1.15)))
    (home-page
      "https://github.com/substack/safe-regex")
    (synopsis
      "detect possibly catastrophic, exponential-time regular expressions")
    (description
      "detect possibly catastrophic, exponential-time regular expressions")
    (license license:expat)))

(define-public node-micromatch-3.1.10
  (package
    (name "node-micromatch")
    (version "3.1.10")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/micromatch/-/micromatch-3.1.10.tgz")
        (sha256
          (base32
            "1j4m1y8x2sib8i5rfilml7yzpgs5njsg2nxzxwlb63997qlpb7p5"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-to-regex" ,node-to-regex-3.0.2)
        ("node-snapdragon" ,node-snapdragon-0.8.2)
        ("node-regex-not" ,node-regex-not-1.0.2)
        ("node-object-pick" ,node-object-pick-1.3.0)
        ("node-nanomatch" ,node-nanomatch-1.2.13)
        ("node-kind-of" ,node-kind-of-6.0.3)
        ("node-fragment-cache"
         ,node-fragment-cache-0.2.1)
        ("node-extglob" ,node-extglob-2.0.4)
        ("node-extend-shallow"
         ,node-extend-shallow-3.0.2)
        ("node-define-property"
         ,node-define-property-2.0.2)
        ("node-braces" ,node-braces-2.3.2)
        ("node-array-unique" ,node-array-unique-0.3.2)
        ("node-arr-diff" ,node-arr-diff-4.0.0)))
    (home-page
      "https://github.com/micromatch/micromatch")
    (synopsis
      "Glob matching for javascript/node.js. A drop-in replacement and faster alternative to minimatch and multimatch.")
    (description
      "Glob matching for javascript/node.js. A drop-in replacement and faster alternative to minimatch and multimatch.")
    (license license:expat)))

(define-public node-readdirp-2.2.1
  (package
    (name "node-readdirp")
    (version "2.2.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/readdirp/-/readdirp-2.2.1.tgz")
        (sha256
          (base32
            "18vnhdirs6khbbzwfkclgff6yqdhb55gs0bp3p9k41lpq2fk18n9"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-readable-stream"
         ,node-readable-stream-2.3.7)
        ("node-micromatch" ,node-micromatch-3.1.10)
        ("node-graceful-fs" ,node-graceful-fs-4.2.6)))
    (home-page
      "https://github.com/paulmillr/readdirp")
    (synopsis
      "Recursive version of fs.readdir with streaming api.")
    (description
      "Recursive version of fs.readdir with streaming api.")
    (license license:expat)))

(define-public node-file-uri-to-path-1.0.0
  (package
    (name "node-file-uri-to-path")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/file-uri-to-path/-/file-uri-to-path-1.0.0.tgz")
        (sha256
          (base32
            "1cvgn3xfpkzy20vjfw093mdjam1x9hfkrrkbdbrrdavmgvvcsh2l"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/TooTallNate/file-uri-to-path")
    (synopsis "Convert a file: URI to a file path")
    (description
      "Convert a file: URI to a file path")
    (license license:expat)))

(define-public node-bindings-1.5.0
  (package
    (name "node-bindings")
    (version "1.5.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/bindings/-/bindings-1.5.0.tgz")
        (sha256
          (base32
            "01q9xfgc5jl9h6ny44zb4xcin6sirmm5bignn68rmn2vihbq2xyp"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-file-uri-to-path"
         ,node-file-uri-to-path-1.0.0)))
    (home-page
      "https://github.com/TooTallNate/node-bindings")
    (synopsis
      "Helper module for loading your native module's .node file")
    (description
      "Helper module for loading your native module's .node file")
    (license license:expat)))

(define-public node-nan-2.14.2
  (package
    (name "node-nan")
    (version "2.14.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/nan/-/nan-2.14.2.tgz")
        (sha256
          (base32
            "0092x43h4ysm9zsxrxhkba1m2m1ybmnsn7dq58ppfgmd02hi98m9"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/nodejs/nan#readme")
    (synopsis
      "Native Abstractions for Node.js: C++ header for Node 0.8 -> 14 compatibility")
    (description
      "Native Abstractions for Node.js: C++ header for Node 0.8 -> 14 compatibility")
    (license license:expat)))

(define-public node-chokidar-1.7.0
  (package
    (name "node-chokidar")
    (version "1.7.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/chokidar/-/chokidar-1.7.0.tgz")
        (sha256
          (base32
            "0dxk0wm89l9smx45bgh828x0cf9mis4mahk6wgd98pfryx01y9cp"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-readdirp" ,node-readdirp-2.2.1)
        ("node-path-is-absolute"
         ,node-path-is-absolute-1.0.1)
        ("node-is-glob" ,node-is-glob-2.0.1)
        ("node-is-binary-path"
         ,node-is-binary-path-1.0.1)
        ("node-inherits" ,node-inherits-2.0.4)
        ("node-glob-parent" ,node-glob-parent-2.0.0)
        ("node-async-each" ,node-async-each-1.0.3)
        ("node-anymatch" ,node-anymatch-1.3.2)))
    (home-page
      "https://github.com/paulmillr/chokidar")
    (synopsis
      "A neat wrapper around node.js fs.watch / fs.watchFile / fsevents.")
    (description
      "A neat wrapper around node.js fs.watch / fs.watchFile / fsevents.")
    (license license:expat)))

(define-public node-source-map-0.6.1
  (package
    (name "node-source-map")
    (version "0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/source-map/-/source-map-0.6.1.tgz")
        (sha256
          (base32
            "11ib173i7xf5sd85da9jfrcbzygr48pppz5csl15hnpz2w6s3g5x"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/mozilla/source-map")
    (synopsis "Generates and consumes source maps")
    (description
      "Generates and consumes source maps")
    (license license:bsd-3)))

(define-public node-clean-css-4.2.3
  (package
    (name "node-clean-css")
    (version "4.2.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/clean-css/-/clean-css-4.2.3.tgz")
        (sha256
          (base32
            "1vqybc0yny3gzmnyypsl5hrnc0kxqd2gbp7ji1041a3llpqdml1y"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-source-map" ,node-source-map-0.6.1)))
    (home-page
      "https://github.com/jakubpawlowicz/clean-css")
    (synopsis "A well-tested CSS minifier")
    (description "A well-tested CSS minifier")
    (license license:expat)))

(define-public node-estraverse-4.3.0
  (package
    (name "node-estraverse")
    (version "4.3.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/estraverse/-/estraverse-4.3.0.tgz")
        (sha256
          (base32
            "1bpip5qvpq6f6prpn5dsxnqsaw3czx6hn8mps04v5vpqgla2n9kz"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/estools/estraverse")
    (synopsis
      "ECMAScript JS AST traversal functions")
    (description
      "ECMAScript JS AST traversal functions")
    (license #f)))

(define-public node-esutils-2.0.3
  (package
    (name "node-esutils")
    (version "2.0.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/esutils/-/esutils-2.0.3.tgz")
        (sha256
          (base32
            "03v4y32k50mbxwv70prr7ghwg59vd5gyxsdsbdikqnj919rvvbf5"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://github.com/estools/esutils")
    (synopsis
      "utility box for ECMAScript language tools")
    (description
      "utility box for ECMAScript language tools")
    (license #f)))

(define-public node-esprima-4.0.1
  (package
    (name "node-esprima")
    (version "4.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/esprima/-/esprima-4.0.1.tgz")
        (sha256
          (base32
            "0x6cjgh4452wa28yz562b4c2dad78rn3fxfzqns9bk5ykh7938fq"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "http://esprima.org")
    (synopsis
      "ECMAScript parsing infrastructure for multipurpose analysis")
    (description
      "ECMAScript parsing infrastructure for multipurpose analysis")
    (license #f)))

(define-public node-deep-is-0.1.3
  (package
    (name "node-deep-is")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/deep-is/-/deep-is-0.1.3.tgz")
        (sha256
          (base32
            "11m7mds6valw8m5c5hgjnr83s104nirkvcnmclm38g02gvxf4rcq"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://github.com/thlorenz/deep-is")
    (synopsis
      "node's assert.deepEqual algorithm except for NaN being equal to NaN")
    (description
      "node's assert.deepEqual algorithm except for NaN being equal to NaN")
    (license #f)))

(define-public node-word-wrap-1.2.3
  (package
    (name "node-word-wrap")
    (version "1.2.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/word-wrap/-/word-wrap-1.2.3.tgz")
        (sha256
          (base32
            "1ngw3nglmfh9a90b4ckay43yw96h61mbxmhm5g1qvk1j6h1dmyv4"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jonschlinkert/word-wrap")
    (synopsis "Wrap words to a specified length.")
    (description "Wrap words to a specified length.")
    (license license:expat)))

(define-public node-prelude-ls-1.1.2
  (package
    (name "node-prelude-ls")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/prelude-ls/-/prelude-ls-1.1.2.tgz")
        (sha256
          (base32
            "0msvwq9la3w6wm51s2p3j2dv6634sj7iydyx3iqw4y67vkj8zzgs"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "http://preludels.com")
    (synopsis
      "prelude.ls is a functionally oriented utility library. It is powerful and flexible. Almost all of its functions are curried. It is written in, and is the recommended base library for, LiveScript.")
    (description
      "prelude.ls is a functionally oriented utility library. It is powerful and flexible. Almost all of its functions are curried. It is written in, and is the recommended base library for, LiveScript.")
    (license #f)))

(define-public node-type-check-0.3.2
  (package
    (name "node-type-check")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/type-check/-/type-check-0.3.2.tgz")
        (sha256
          (base32
            "05iwxwj2sdbnxwp5lkxrdkbdcdvmkvq6zbq7lmsg20zbw3pyy56l"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-prelude-ls" ,node-prelude-ls-1.1.2)))
    (home-page "https://github.com/gkz/type-check")
    (synopsis
      "type-check allows you to check the types of JavaScript values at runtime with a Haskell like type syntax.")
    (description
      "type-check allows you to check the types of JavaScript values at runtime with a Haskell like type syntax.")
    (license license:expat)))

(define-public node-levn-0.3.0
  (package
    (name "node-levn")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/levn/-/levn-0.3.0.tgz")
        (sha256
          (base32
            "094nf5lc3jk3gv0hs01nzgq2vw0h2y9cxaqsbgs0xc45in2kw0ds"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-type-check" ,node-type-check-0.3.2)
        ("node-prelude-ls" ,node-prelude-ls-1.1.2)))
    (home-page "https://github.com/gkz/levn")
    (synopsis
      "Light ECMAScript (JavaScript) Value Notation - human written, concise, typed, flexible")
    (description
      "Light ECMAScript (JavaScript) Value Notation - human written, concise, typed, flexible")
    (license license:expat)))

(define-public node-fast-levenshtein-2.0.6
  (package
    (name "node-fast-levenshtein")
    (version "2.0.6")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/fast-levenshtein/-/fast-levenshtein-2.0.6.tgz")
        (sha256
          (base32
            "0g5zgdlp38dli94qbbm8vhvmj90fh48sxpggfn2083wbdcq50jxv"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/hiddentao/fast-levenshtein#readme")
    (synopsis
      "Efficient implementation of Levenshtein algorithm  with locale-specific collator support.")
    (description
      "Efficient implementation of Levenshtein algorithm  with locale-specific collator support.")
    (license license:expat)))

(define-public node-optionator-0.8.3
  (package
    (name "node-optionator")
    (version "0.8.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/optionator/-/optionator-0.8.3.tgz")
        (sha256
          (base32
            "0gdxsryh0g0vrbjqrgg8bvzjj2m98hf1rg8sksjmslspkr4fdvsi"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-fast-levenshtein"
         ,node-fast-levenshtein-2.0.6)
        ("node-levn" ,node-levn-0.3.0)
        ("node-type-check" ,node-type-check-0.3.2)
        ("node-word-wrap" ,node-word-wrap-1.2.3)
        ("node-deep-is" ,node-deep-is-0.1.3)
        ("node-prelude-ls" ,node-prelude-ls-1.1.2)))
    (home-page "https://github.com/gkz/optionator")
    (synopsis "option parsing and help generation")
    (description
      "option parsing and help generation")
    (license license:expat)))

(define-public node-escodegen-1.14.3
  (package
    (name "node-escodegen")
    (version "1.14.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/escodegen/-/escodegen-1.14.3.tgz")
        (sha256
          (base32
            "12rngj20cpcnayic21ky3rkmkldbcvrzxzsr9y6a9diaf9lgldzj"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-source-map" ,node-source-map-0.6.1)
        ("node-optionator" ,node-optionator-0.8.3)
        ("node-esprima" ,node-esprima-4.0.1)
        ("node-esutils" ,node-esutils-2.0.3)
        ("node-estraverse" ,node-estraverse-4.3.0)))
    (home-page "http://github.com/estools/escodegen")
    (synopsis "ECMAScript code generator")
    (description "ECMAScript code generator")
    (license #f)))

(define-public node-negotiator-0.6.2
  (package
    (name "node-negotiator")
    (version "0.6.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/negotiator/-/negotiator-0.6.2.tgz")
        (sha256
          (base32
            "1zpx97aamn044id45ljzalxpa9l1dm0gn260w5hwjg7998vxjz68"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jshttp/negotiator#readme")
    (synopsis "HTTP content negotiation")
    (description "HTTP content negotiation")
    (license license:expat)))

(define-public node-accepts-1.3.7
  (package
    (name "node-accepts")
    (version "1.3.7")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/accepts/-/accepts-1.3.7.tgz")
        (sha256
          (base32
            "1s550j2wkqhsgpj841fww4bdck0w67rk80qb859nwqy8x7khsycs"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-negotiator" ,node-negotiator-0.6.2)
        ("node-mime-types" ,node-mime-types-2.1.28)))
    (home-page
      "https://github.com/jshttp/accepts#readme")
    (synopsis "Higher-level content negotiation")
    (description "Higher-level content negotiation")
    (license license:expat)))

(define-public node-array-flatten-1.1.1
  (package
    (name "node-array-flatten")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/array-flatten/-/array-flatten-1.1.1.tgz")
        (sha256
          (base32
            "1v96cj6w6f7g61c7fjfkxpkbbfkxl2ksh5zm7y5mfp96xivi5jhs"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/blakeembrey/array-flatten")
    (synopsis
      "Flatten an array of nested arrays into a single flat array")
    (description
      "Flatten an array of nested arrays into a single flat array")
    (license license:expat)))

(define-public node-bytes-3.1.0
  (package
    (name "node-bytes")
    (version "3.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/bytes/-/bytes-3.1.0.tgz")
        (sha256
          (base32
            "1w0cw2wnzif217yvnldrflj14l2zkfcxdw1yhnv1jbm6ywykmngp"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/visionmedia/bytes.js#readme")
    (synopsis
      "Utility to parse a string bytes to bytes and vice-versa")
    (description
      "Utility to parse a string bytes to bytes and vice-versa")
    (license license:expat)))

(define-public node-inherits-2.0.3
  (package
    (name "node-inherits")
    (version "2.0.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/inherits/-/inherits-2.0.3.tgz")
        (sha256
          (base32
            "1pvc6l11w425i6k9zph3226gdakqw3cq8zkfg1jf51sfnplmhpvz"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/isaacs/inherits#readme")
    (synopsis
      "Browser-friendly inheritance fully compatible with standard node.js inherits()")
    (description
      "Browser-friendly inheritance fully compatible with standard node.js inherits()")
    (license license:isc)))

(define-public node-http-errors-1.7.2
  (package
    (name "node-http-errors")
    (version "1.7.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/http-errors/-/http-errors-1.7.2.tgz")
        (sha256
          (base32
            "10x0729bip3xz9p4qj8hbgp0hfbknnv8imnp6r964hn5aakd8xys"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-toidentifier" ,node-toidentifier-1.0.0)
        ("node-statuses" ,node-statuses-1.5.0)
        ("node-setprototypeof"
         ,node-setprototypeof-1.1.1)
        ("node-inherits" ,node-inherits-2.0.3)
        ("node-depd" ,node-depd-1.1.2)))
    (home-page
      "https://github.com/jshttp/http-errors#readme")
    (synopsis "Create HTTP error objects")
    (description "Create HTTP error objects")
    (license license:expat)))

(define-public node-iconv-lite-0.4.24
  (package
    (name "node-iconv-lite")
    (version "0.4.24")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/iconv-lite/-/iconv-lite-0.4.24.tgz")
        (sha256
          (base32
            "0da6ff7dlx6lfhdafsd9sv0h09sicpfakms8bqylrm4f17r68v2p"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-safer-buffer" ,node-safer-buffer-2.1.2)))
    (home-page
      "https://github.com/ashtuchkin/iconv-lite")
    (synopsis
      "Convert character encodings in pure javascript.")
    (description
      "Convert character encodings in pure javascript.")
    (license license:expat)))

(define-public node-unpipe-1.0.0
  (package
    (name "node-unpipe")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/unpipe/-/unpipe-1.0.0.tgz")
        (sha256
          (base32
            "1dnzbqfmchls4jyvkw0wnkc09pig98y66zzsy3lizgyls435xyrd"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/stream-utils/unpipe")
    (synopsis
      "Unpipe a stream from all destinations")
    (description
      "Unpipe a stream from all destinations")
    (license license:expat)))

(define-public node-raw-body-2.4.0
  (package
    (name "node-raw-body")
    (version "2.4.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/raw-body/-/raw-body-2.4.0.tgz")
        (sha256
          (base32
            "0l1v7r5mn6jk7f3h0imryzsz0cigkbq97nigdqvlna5q2v733qzh"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-unpipe" ,node-unpipe-1.0.0)
        ("node-iconv-lite" ,node-iconv-lite-0.4.24)
        ("node-http-errors" ,node-http-errors-1.7.2)
        ("node-bytes" ,node-bytes-3.1.0)))
    (home-page
      "https://github.com/stream-utils/raw-body#readme")
    (synopsis
      "Get and validate the raw body of a readable stream.")
    (description
      "Get and validate the raw body of a readable stream.")
    (license license:expat)))

(define-public node-type-is-1.6.18
  (package
    (name "node-type-is")
    (version "1.6.18")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/type-is/-/type-is-1.6.18.tgz")
        (sha256
          (base32
            "1bn3gl9vd67cq3wl2cvq686zskl2xx6lxz5kp9w47qc06f2vbnll"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-mime-types" ,node-mime-types-2.1.28)
        ("node-media-typer" ,node-media-typer-0.3.0)))
    (home-page
      "https://github.com/jshttp/type-is#readme")
    (synopsis "Infer the content-type of a request.")
    (description
      "Infer the content-type of a request.")
    (license license:expat)))

(define-public node-body-parser-1.19.0
  (package
    (name "node-body-parser")
    (version "1.19.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/body-parser/-/body-parser-1.19.0.tgz")
        (sha256
          (base32
            "18vy9ymfhmp3sisf3nxvxi0rl2xll8jp3wd84imnj8swyny5jm02"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-type-is" ,node-type-is-1.6.18)
        ("node-raw-body" ,node-raw-body-2.4.0)
        ("node-qs" ,node-qs-6.7.0)
        ("node-on-finished" ,node-on-finished-2.3.0)
        ("node-iconv-lite" ,node-iconv-lite-0.4.24)
        ("node-http-errors" ,node-http-errors-1.7.2)
        ("node-depd" ,node-depd-1.1.2)
        ("node-debug" ,node-debug-2.6.9)
        ("node-content-type" ,node-content-type-1.0.4)
        ("node-bytes" ,node-bytes-3.1.0)))
    (home-page
      "https://github.com/expressjs/body-parser#readme")
    (synopsis "Node.js body parsing middleware")
    (description "Node.js body parsing middleware")
    (license license:expat)))

(define-public node-content-disposition-0.5.3
  (package
    (name "node-content-disposition")
    (version "0.5.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/content-disposition/-/content-disposition-0.5.3.tgz")
        (sha256
          (base32
            "1gnpp7mvy8r2k8a4kx43rhg8l85n2g0rfvyfn7wai2k42zk08q4y"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-safe-buffer" ,node-safe-buffer-5.1.2)))
    (home-page
      "https://github.com/jshttp/content-disposition#readme")
    (synopsis
      "Create and parse Content-Disposition header")
    (description
      "Create and parse Content-Disposition header")
    (license license:expat)))

(define-public node-content-type-1.0.4
  (package
    (name "node-content-type")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/content-type/-/content-type-1.0.4.tgz")
        (sha256
          (base32
            "0j0rkv7yvpdyk4hfgklx95g75aksljaxraz9lhkb5chhvxqkygnv"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jshttp/content-type#readme")
    (synopsis
      "Create and parse HTTP Content-Type header")
    (description
      "Create and parse HTTP Content-Type header")
    (license license:expat)))

(define-public node-cookie-0.4.0
  (package
    (name "node-cookie")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/cookie/-/cookie-0.4.0.tgz")
        (sha256
          (base32
            "1z8xxh56qxgcz96j59aw7ik2847xk0lk91c9rdk38bkfbmncpy9f"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jshttp/cookie#readme")
    (synopsis
      "HTTP server cookie parsing and serialization")
    (description
      "HTTP server cookie parsing and serialization")
    (license license:expat)))

(define-public node-cookie-signature-1.0.6
  (package
    (name "node-cookie-signature")
    (version "1.0.6")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/cookie-signature/-/cookie-signature-1.0.6.tgz")
        (sha256
          (base32
            "04sk9ma5a8xb4jib4wmsdj8pz5bk36yzavzbj3k0drdy9bi4bww9"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/visionmedia/node-cookie-signature")
    (synopsis "Sign and unsign cookies")
    (description "Sign and unsign cookies")
    (license license:expat)))

(define-public node-finalhandler-1.1.2
  (package
    (name "node-finalhandler")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/finalhandler/-/finalhandler-1.1.2.tgz")
        (sha256
          (base32
            "1kq09av23a28ig4kd727librz6dmiwjycsr7nh5cg0vjy0bk31pj"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-unpipe" ,node-unpipe-1.0.0)
        ("node-statuses" ,node-statuses-1.5.0)
        ("node-parseurl" ,node-parseurl-1.3.3)
        ("node-on-finished" ,node-on-finished-2.3.0)
        ("node-escape-html" ,node-escape-html-1.0.3)
        ("node-encodeurl" ,node-encodeurl-1.0.2)
        ("node-debug" ,node-debug-2.6.9)))
    (home-page
      "https://github.com/pillarjs/finalhandler#readme")
    (synopsis "Node.js final http responder")
    (description "Node.js final http responder")
    (license license:expat)))

(define-public node-merge-descriptors-1.0.1
  (package
    (name "node-merge-descriptors")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/merge-descriptors/-/merge-descriptors-1.0.1.tgz")
        (sha256
          (base32
            "02d4fqgiz4cc33vbcdlah9rafj5vc2z0iy05sc9wpfpzri3kn2l8"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/component/merge-descriptors")
    (synopsis "Merge objects using descriptors")
    (description "Merge objects using descriptors")
    (license license:expat)))

(define-public node-methods-1.1.2
  (package
    (name "node-methods")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/methods/-/methods-1.1.2.tgz")
        (sha256
          (base32
            "0g50ci0gc8r8kq1i06q078gw7azkakp7j3yw5qfi6gq2qk8hdlnz"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://github.com/jshttp/methods")
    (synopsis "HTTP methods that node supports")
    (description "HTTP methods that node supports")
    (license license:expat)))

(define-public node-path-to-regexp-0.1.7
  (package
    (name "node-path-to-regexp")
    (version "0.1.7")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/path-to-regexp/-/path-to-regexp-0.1.7.tgz")
        (sha256
          (base32
            "0dlgr61ahgydnmsp1pprc1m52qnylkb3pdpvn1s5p5x8d7qn4m6y"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/component/path-to-regexp#readme")
    (synopsis "Express style path to RegExp utility")
    (description
      "Express style path to RegExp utility")
    (license license:expat)))

(define-public node-forwarded-0.1.2
  (package
    (name "node-forwarded")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/forwarded/-/forwarded-0.1.2.tgz")
        (sha256
          (base32
            "1kajx1hlidman6sircpxj5hpxw6q06p2li7hq15skff7k00prraq"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jshttp/forwarded#readme")
    (synopsis "Parse HTTP X-Forwarded-For header")
    (description "Parse HTTP X-Forwarded-For header")
    (license license:expat)))

(define-public node-ipaddr-js-1.9.1
  (package
    (name "node-ipaddr-js")
    (version "1.9.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/ipaddr.js/-/ipaddr.js-1.9.1.tgz")
        (sha256
          (base32
            "1vlg9vgdlx13dvh6h6sg3rgdbp04lkljmn6gxih43zk77xidjhbl"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/whitequark/ipaddr.js#readme")
    (synopsis
      "A library for manipulating IPv4 and IPv6 addresses in JavaScript.")
    (description
      "A library for manipulating IPv4 and IPv6 addresses in JavaScript.")
    (license license:expat)))

(define-public node-proxy-addr-2.0.6
  (package
    (name "node-proxy-addr")
    (version "2.0.6")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/proxy-addr/-/proxy-addr-2.0.6.tgz")
        (sha256
          (base32
            "1z91mzm8fiy6kb3sydbq2g7y5dvira0lgfwqsbnx8axk70x4lvv7"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-ipaddr-js" ,node-ipaddr-js-1.9.1)
        ("node-forwarded" ,node-forwarded-0.1.2)))
    (home-page
      "https://github.com/jshttp/proxy-addr#readme")
    (synopsis "Determine address of proxied request")
    (description
      "Determine address of proxied request")
    (license license:expat)))

(define-public node-qs-6.7.0
  (package
    (name "node-qs")
    (version "6.7.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/qs/-/qs-6.7.0.tgz")
        (sha256
          (base32
            "0kjlldn82gli5vwh4w84lv7xzlh5ayb91l90m08xsajins128bal"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://github.com/ljharb/qs")
    (synopsis
      "A querystring parser that supports nesting and arrays, with a depth limit")
    (description
      "A querystring parser that supports nesting and arrays, with a depth limit")
    (license license:bsd-3)))

(define-public node-safe-buffer-5.1.2
  (package
    (name "node-safe-buffer")
    (version "5.1.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/safe-buffer/-/safe-buffer-5.1.2.tgz")
        (sha256
          (base32
            "08ma0a2a9j537bxl7qd2dn6sjcdhrclpdbslr19bkbyc1z30d4p0"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/feross/safe-buffer")
    (synopsis "Safer Node.js Buffer API")
    (description "Safer Node.js Buffer API")
    (license license:expat)))

(define-public node-parseurl-1.3.3
  (package
    (name "node-parseurl")
    (version "1.3.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/parseurl/-/parseurl-1.3.3.tgz")
        (sha256
          (base32
            "06h2bx1rilkdir3v9jlg94r1q2fn895s0vxjjs0wx5z027x4pvsn"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/pillarjs/parseurl#readme")
    (synopsis "parse a url with memoization")
    (description "parse a url with memoization")
    (license license:expat)))

(define-public node-ms-2.0.0
  (package
    (name "node-ms")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/ms/-/ms-2.0.0.tgz")
        (sha256
          (base32
            "1jrysw9zx14av3jdvc3kywc3xkjqxh748g4s6p1iy634i2mm489n"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://github.com/zeit/ms#readme")
    (synopsis "Tiny milisecond conversion utility")
    (description
      "Tiny milisecond conversion utility")
    (license license:expat)))

(define-public node-destroy-1.0.4
  (package
    (name "node-destroy")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/destroy/-/destroy-1.0.4.tgz")
        (sha256
          (base32
            "0miy8a2wfc77l4j08hv4qk5v9a7566igql71zw7ds3v5yhi9cmsv"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/stream-utils/destroy")
    (synopsis "destroy a stream if possible")
    (description "destroy a stream if possible")
    (license license:expat)))

(define-public node-encodeurl-1.0.2
  (package
    (name "node-encodeurl")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/encodeurl/-/encodeurl-1.0.2.tgz")
        (sha256
          (base32
            "13afvicx42ha4k29571sg0i4b76xjggyxvmmmibm258ipf6mjinb"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/pillarjs/encodeurl#readme")
    (synopsis
      "Encode a URL to a percent-encoded form, excluding already-encoded sequences")
    (description
      "Encode a URL to a percent-encoded form, excluding already-encoded sequences")
    (license license:expat)))

(define-public node-escape-html-1.0.3
  (package
    (name "node-escape-html")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/escape-html/-/escape-html-1.0.3.tgz")
        (sha256
          (base32
            "0rh35dvab1wbp87dy1m6rynbcb9rbs5kry7jk17ixyxx7if1a0d1"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/component/escape-html")
    (synopsis "Escape string for use in HTML")
    (description "Escape string for use in HTML")
    (license license:expat)))

(define-public node-etag-1.8.1
  (package
    (name "node-etag")
    (version "1.8.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/etag/-/etag-1.8.1.tgz")
        (sha256
          (base32
            "1bqgznlsrqcmxnhmnqkhwzcrqfaalxmfxzly1ikaplkkm5w6ragn"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jshttp/etag#readme")
    (synopsis "Create simple HTTP ETags")
    (description "Create simple HTTP ETags")
    (license license:expat)))

(define-public node-fresh-0.5.2
  (package
    (name "node-fresh")
    (version "0.5.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/fresh/-/fresh-0.5.2.tgz")
        (sha256
          (base32
            "0k44badcxkwy202kz404w078l660f65jaijg473xv38ay3wpdri5"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jshttp/fresh#readme")
    (synopsis "HTTP response freshness testing")
    (description "HTTP response freshness testing")
    (license license:expat)))

(define-public node-depd-1.1.2
  (package
    (name "node-depd")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/depd/-/depd-1.1.2.tgz")
        (sha256
          (base32
            "07645ghplj1qy8z6g3vz1855xjy2j217q90bib3m44c2npk6pql3"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/dougwilson/nodejs-depd#readme")
    (synopsis "Deprecate all the things")
    (description "Deprecate all the things")
    (license license:expat)))

(define-public node-statuses-1.5.0
  (package
    (name "node-statuses")
    (version "1.5.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/statuses/-/statuses-1.5.0.tgz")
        (sha256
          (base32
            "0g6ydb53k8b5rhll1z667riba9454ipkl4hgkc5vzc62l4h10g18"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jshttp/statuses#readme")
    (synopsis "HTTP status utility")
    (description "HTTP status utility")
    (license license:expat)))

(define-public node-toidentifier-1.0.0
  (package
    (name "node-toidentifier")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/toidentifier/-/toidentifier-1.0.0.tgz")
        (sha256
          (base32
            "1i9qgk5k664mfvc7zj80mz5af7py2vh0zmr8yrvbb0fkzj1d3vba"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/component/toidentifier#readme")
    (synopsis
      "Convert a string of words to a JavaScript identifier")
    (description
      "Convert a string of words to a JavaScript identifier")
    (license license:expat)))

(define-public node-http-errors-1.7.3
  (package
    (name "node-http-errors")
    (version "1.7.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/http-errors/-/http-errors-1.7.3.tgz")
        (sha256
          (base32
            "049zp0b5kbqrvpmccy5msd8qcrslxjrq4nj5hsmbabhbbm3ys6y9"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-toidentifier" ,node-toidentifier-1.0.0)
        ("node-statuses" ,node-statuses-1.5.0)
        ("node-setprototypeof"
         ,node-setprototypeof-1.1.1)
        ("node-inherits" ,node-inherits-2.0.4)
        ("node-depd" ,node-depd-1.1.2)))
    (home-page
      "https://github.com/jshttp/http-errors#readme")
    (synopsis "Create HTTP error objects")
    (description "Create HTTP error objects")
    (license license:expat)))

(define-public node-mime-1.6.0
  (package
    (name "node-mime")
    (version "1.6.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/mime/-/mime-1.6.0.tgz")
        (sha256
          (base32
            "16iprk4h6nh780mvfv0p93k3yvj7jrq2qs92niaw6yk11qwi0li1"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/broofa/node-mime#readme")
    (synopsis
      "A comprehensive library for mime-type mapping")
    (description
      "A comprehensive library for mime-type mapping")
    (license license:expat)))

(define-public node-ms-2.1.1
  (package
    (name "node-ms")
    (version "2.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/ms/-/ms-2.1.1.tgz")
        (sha256
          (base32
            "1yhq64xslz82p9jamzh15q5ii8s8d5jy5p75qajb5q4q4k1gfmhv"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://github.com/zeit/ms#readme")
    (synopsis "Tiny millisecond conversion utility")
    (description
      "Tiny millisecond conversion utility")
    (license license:expat)))

(define-public node-ee-first-1.1.1
  (package
    (name "node-ee-first")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/ee-first/-/ee-first-1.1.1.tgz")
        (sha256
          (base32
            "175r500n567a04qmswzw5hkgdnika3dvn63n284jlar2gvmyhj2i"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jonathanong/ee-first")
    (synopsis
      "return the first event in a set of ee/event pairs")
    (description
      "return the first event in a set of ee/event pairs")
    (license license:expat)))

(define-public node-on-finished-2.3.0
  (package
    (name "node-on-finished")
    (version "2.3.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/on-finished/-/on-finished-2.3.0.tgz")
        (sha256
          (base32
            "0sv1js3fk0ag46ln3a7cwhannacqxyl694zkb1qy53fdd630sr59"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-ee-first" ,node-ee-first-1.1.1)))
    (home-page
      "https://github.com/jshttp/on-finished")
    (synopsis
      "Execute a callback when a request closes, finishes, or errors")
    (description
      "Execute a callback when a request closes, finishes, or errors")
    (license license:expat)))

(define-public node-range-parser-1.2.1
  (package
    (name "node-range-parser")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/range-parser/-/range-parser-1.2.1.tgz")
        (sha256
          (base32
            "09prs852snwqr9cfcrybm7ysl0z1wka9dh4dwc4v1415cvi6cllh"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jshttp/range-parser#readme")
    (synopsis "Range header field string parser")
    (description "Range header field string parser")
    (license license:expat)))

(define-public node-send-0.17.1
  (package
    (name "node-send")
    (version "0.17.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/send/-/send-0.17.1.tgz")
        (sha256
          (base32
            "08ds2hx476lhafl051sh03kaiilkariwa209d02gbcm6xygb8m6k"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-statuses" ,node-statuses-1.5.0)
        ("node-range-parser" ,node-range-parser-1.2.1)
        ("node-on-finished" ,node-on-finished-2.3.0)
        ("node-ms" ,node-ms-2.1.1)
        ("node-mime" ,node-mime-1.6.0)
        ("node-http-errors" ,node-http-errors-1.7.3)
        ("node-fresh" ,node-fresh-0.5.2)
        ("node-etag" ,node-etag-1.8.1)
        ("node-escape-html" ,node-escape-html-1.0.3)
        ("node-encodeurl" ,node-encodeurl-1.0.2)
        ("node-destroy" ,node-destroy-1.0.4)
        ("node-depd" ,node-depd-1.1.2)
        ("node-debug" ,node-debug-2.6.9)))
    (home-page
      "https://github.com/pillarjs/send#readme")
    (synopsis
      "Better streaming static file server with Range and conditional-GET support")
    (description
      "Better streaming static file server with Range and conditional-GET support")
    (license license:expat)))

(define-public node-serve-static-1.14.1
  (package
    (name "node-serve-static")
    (version "1.14.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/serve-static/-/serve-static-1.14.1.tgz")
        (sha256
          (base32
            "1pajpv2acavzq2bpj3rggrvik00k9wyav2fvg5yvmvgmwy3kbxh1"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-send" ,node-send-0.17.1)
        ("node-parseurl" ,node-parseurl-1.3.3)
        ("node-escape-html" ,node-escape-html-1.0.3)
        ("node-encodeurl" ,node-encodeurl-1.0.2)))
    (home-page
      "https://github.com/expressjs/serve-static#readme")
    (synopsis "Serve static files")
    (description "Serve static files")
    (license license:expat)))

(define-public node-setprototypeof-1.1.1
  (package
    (name "node-setprototypeof")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/setprototypeof/-/setprototypeof-1.1.1.tgz")
        (sha256
          (base32
            "13366ghwjzarwl9i537f1n6gkp85lggvngw81p6mpjy06hc52vx5"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/wesleytodd/setprototypeof")
    (synopsis
      "A small polyfill for Object.setprototypeof")
    (description
      "A small polyfill for Object.setprototypeof")
    (license license:isc)))

(define-public node-media-typer-0.3.0
  (package
    (name "node-media-typer")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/media-typer/-/media-typer-0.3.0.tgz")
        (sha256
          (base32
            "07vlmddn91j0bbrxr2br320dnkxw96dp7hqmvidj5ydl84adiyid"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jshttp/media-typer")
    (synopsis
      "Simple RFC 6838 media type parser and formatter")
    (description
      "Simple RFC 6838 media type parser and formatter")
    (license license:expat)))

(define-public node-mime-types-2.1.28
  (package
    (name "node-mime-types")
    (version "2.1.28")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/mime-types/-/mime-types-2.1.28.tgz")
        (sha256
          (base32
            "0c60ajxcvvxmbgvpqzihhcj2bqa4dp75snvr9nnja8ri7wkhvr8w"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-mime-db" ,node-mime-db-1.45.0)))
    (home-page
      "https://github.com/jshttp/mime-types#readme")
    (synopsis
      "The ultimate javascript content-type utility.")
    (description
      "The ultimate javascript content-type utility.")
    (license license:expat)))

(define-public node-utils-merge-1.0.1
  (package
    (name "node-utils-merge")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/utils-merge/-/utils-merge-1.0.1.tgz")
        (sha256
          (base32
            "0djhmrfzxpdhscg4pkgnsd39cddpwpkkw1w2f8mp2xfsxn7mvnfy"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jaredhanson/utils-merge#readme")
    (synopsis "merge() utility function")
    (description "merge() utility function")
    (license license:expat)))

(define-public node-vary-1.1.2
  (package
    (name "node-vary")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/vary/-/vary-1.1.2.tgz")
        (sha256
          (base32
            "0wbf4kmfyzc23dc0vjcmymkd1ks50z5gvv23lkkkayipf438cy3k"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jshttp/vary#readme")
    (synopsis "Manipulate the HTTP Vary header")
    (description "Manipulate the HTTP Vary header")
    (license license:expat)))

(define-public node-express-4.17.1
  (package
    (name "node-express")
    (version "4.17.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/express/-/express-4.17.1.tgz")
        (sha256
          (base32
            "1a82maaz62wcw1dsv863ikjp6gpyxka0b1g2sldnvxg3qi8va016"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-vary" ,node-vary-1.1.2)
        ("node-utils-merge" ,node-utils-merge-1.0.1)
        ("node-type-is" ,node-type-is-1.6.18)
        ("node-statuses" ,node-statuses-1.5.0)
        ("node-setprototypeof"
         ,node-setprototypeof-1.1.1)
        ("node-serve-static" ,node-serve-static-1.14.1)
        ("node-send" ,node-send-0.17.1)
        ("node-safe-buffer" ,node-safe-buffer-5.1.2)
        ("node-range-parser" ,node-range-parser-1.2.1)
        ("node-qs" ,node-qs-6.7.0)
        ("node-proxy-addr" ,node-proxy-addr-2.0.6)
        ("node-path-to-regexp"
         ,node-path-to-regexp-0.1.7)
        ("node-parseurl" ,node-parseurl-1.3.3)
        ("node-on-finished" ,node-on-finished-2.3.0)
        ("node-methods" ,node-methods-1.1.2)
        ("node-merge-descriptors"
         ,node-merge-descriptors-1.0.1)
        ("node-fresh" ,node-fresh-0.5.2)
        ("node-finalhandler" ,node-finalhandler-1.1.2)
        ("node-etag" ,node-etag-1.8.1)
        ("node-escape-html" ,node-escape-html-1.0.3)
        ("node-encodeurl" ,node-encodeurl-1.0.2)
        ("node-depd" ,node-depd-1.1.2)
        ("node-debug" ,node-debug-2.6.9)
        ("node-cookie-signature"
         ,node-cookie-signature-1.0.6)
        ("node-cookie" ,node-cookie-0.4.0)
        ("node-content-type" ,node-content-type-1.0.4)
        ("node-content-disposition"
         ,node-content-disposition-0.5.3)
        ("node-body-parser" ,node-body-parser-1.19.0)
        ("node-array-flatten" ,node-array-flatten-1.1.1)
        ("node-accepts" ,node-accepts-1.3.7)))
    (home-page "http://expressjs.com/")
    (synopsis
      "Fast, unopinionated, minimalist web framework")
    (description
      "Fast, unopinionated, minimalist web framework")
    (license license:expat)))

(define-public node-chain-able-1.0.1
  (package
    (name "node-chain-able")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/chain-able/-/chain-able-1.0.1.tgz")
        (sha256
          (base32
            "085qmxbrkf71bn4xslrj099yz8wb9k9ciq6vb1kk86d3zmnh67py"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/fluents/chain-able#readme")
    (synopsis "next level chaining.")
    (description "next level chaining.")
    (license license:expat)))

(define-public node-fliplog-0.3.13
  (package
    (name "node-fliplog")
    (version "0.3.13")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/fliplog/-/fliplog-0.3.13.tgz")
        (sha256
          (base32
            "16c8f09313gdm9m2jwlakkrb26zr7n8cb2vif3yv25jly1wbdi72"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-chain-able" ,node-chain-able-1.0.1)))
    (home-page "https://github.com/fliphub/fliplog")
    (synopsis
      "fluent logging with verbose insight, colors, tables, emoji, filtering, spinners, progress bars, timestamps, capturing, stack traces, clearing, & presets")
    (description
      "fluent logging with verbose insight, colors, tables, emoji, filtering, spinners, progress bars, timestamps, capturing, stack traces, clearing, & presets")
    (license license:expat)))

(define-public node-jsonfile-4.0.0
  (package
    (name "node-jsonfile")
    (version "4.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/jsonfile/-/jsonfile-4.0.0.tgz")
        (sha256
          (base32
            "1s701cy3mlbvgyhhyy2ypqcy064w5990sk8x81gv0200yybrbfaz"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-graceful-fs" ,node-graceful-fs-4.2.6)))
    (home-page
      "https://github.com/jprichardson/node-jsonfile#readme")
    (synopsis "Easily read/write JSON files.")
    (description "Easily read/write JSON files.")
    (license license:expat)))

(define-public node-universalify-0.1.2
  (package
    (name "node-universalify")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/universalify/-/universalify-0.1.2.tgz")
        (sha256
          (base32
            "0lykbpkmvjkjg0sqngrn086rxlyddgjkfnsi22r8hgixxzxb2alc"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/RyanZim/universalify#readme")
    (synopsis
      "Make a callback- or promise-based function support both promises and callbacks.")
    (description
      "Make a callback- or promise-based function support both promises and callbacks.")
    (license license:expat)))

(define-public node-fs-extra-7.0.1
  (package
    (name "node-fs-extra")
    (version "7.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/fs-extra/-/fs-extra-7.0.1.tgz")
        (sha256
          (base32
            "1f08bng4dgkdrwhd977f4xfch9419b7fbwvwwn1qpz3gy4zgja4b"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-universalify" ,node-universalify-0.1.2)
        ("node-jsonfile" ,node-jsonfile-4.0.0)
        ("node-graceful-fs" ,node-graceful-fs-4.2.6)))
    (home-page
      "https://github.com/jprichardson/node-fs-extra")
    (synopsis
      "fs-extra contains methods that aren't included in the vanilla Node.js fs package. Such as mkdir -p, cp -r, and rm -rf.")
    (description
      "fs-extra contains methods that aren't included in the vanilla Node.js fs package. Such as mkdir -p, cp -r, and rm -rf.")
    (license license:expat)))

(define-public node-fuse-concat-with-sourcemaps-1.0.5
  (package
    (name "node-fuse-concat-with-sourcemaps")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/fuse-concat-with-sourcemaps/-/fuse-concat-with-sourcemaps-1.0.5.tgz")
        (sha256
          (base32
            "1sam6dbh6zv6hgpc9h4vyv15myl28y1n5pzmcgrxzk0jb16chx3k"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-source-map" ,node-source-map-0.6.1)))
    (home-page
      "http://github.com/floridoo/concat-with-sourcemaps")
    (synopsis
      "Concatenate file contents with a custom separator and generate a source map")
    (description
      "Concatenate file contents with a custom separator and generate a source map")
    (license license:isc)))

(define-public node-getopts-2.2.5
  (package
    (name "node-getopts")
    (version "2.2.5")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/getopts/-/getopts-2.2.5.tgz")
        (sha256
          (base32
            "0w88w8pda6629sfvmfpzxigl001nb7hhx4cmw24jdsi20gfgd1n6"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jorgebucaran/getopts#readme")
    (synopsis "Parse CLI options, better.")
    (description "Parse CLI options, better.")
    (license license:expat)))

(define-public node-fs-realpath-1.0.0
  (package
    (name "node-fs-realpath")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/fs.realpath/-/fs.realpath-1.0.0.tgz")
        (sha256
          (base32
            "174g5vay9jnd7h5q8hfdw6dnmwl1gdpn4a8sz0ysanhj2f3wp04y"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/isaacs/fs.realpath#readme")
    (synopsis
      "Use node's fs.realpath, but fall back to the JS implementation if the native one fails")
    (description
      "Use node's fs.realpath, but fall back to the JS implementation if the native one fails")
    (license license:isc)))

(define-public node-inflight-1.0.6
  (package
    (name "node-inflight")
    (version "1.0.6")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/inflight/-/inflight-1.0.6.tgz")
        (sha256
          (base32
            "16w864087xsh3q7f5gm3754s7bpsb9fq3dhknk9nmbvlk3sxr7ss"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-wrappy" ,node-wrappy-1.0.2)
        ("node-once" ,node-once-1.4.0)))
    (home-page "https://github.com/isaacs/inflight")
    (synopsis
      "Add callbacks to requests in flight to avoid async duplication")
    (description
      "Add callbacks to requests in flight to avoid async duplication")
    (license license:isc)))

(define-public node-balanced-match-1.0.0
  (package
    (name "node-balanced-match")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/balanced-match/-/balanced-match-1.0.0.tgz")
        (sha256
          (base32
            "1bgzp9jp8ws0kdfgq8h6w3qz8cljyzgcrmxypxkgbknk28n615i8"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/juliangruber/balanced-match")
    (synopsis
      "Match balanced character pairs, like \"{\" and \"}\"")
    (description
      "Match balanced character pairs, like \"{\" and \"}\"")
    (license license:expat)))

(define-public node-concat-map-0.0.1
  (package
    (name "node-concat-map")
    (version "0.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/concat-map/-/concat-map-0.0.1.tgz")
        (sha256
          (base32
            "0qa2zqn9rrr2fqdki44s4s2dk2d8307i4556kv25h06g43b2v41m"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/substack/node-concat-map")
    (synopsis "concatenative mapdashery")
    (description "concatenative mapdashery")
    (license license:expat)))

(define-public node-brace-expansion-1.1.11
  (package
    (name "node-brace-expansion")
    (version "1.1.11")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/brace-expansion/-/brace-expansion-1.1.11.tgz")
        (sha256
          (base32
            "1nlmjvlwlp88knblnayns0brr7a9m2fynrlwq425lrpb4mcn9gc4"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-concat-map" ,node-concat-map-0.0.1)
        ("node-balanced-match"
         ,node-balanced-match-1.0.0)))
    (home-page
      "https://github.com/juliangruber/brace-expansion")
    (synopsis
      "Brace expansion as known from sh/bash")
    (description
      "Brace expansion as known from sh/bash")
    (license license:expat)))

(define-public node-minimatch-3.0.4
  (package
    (name "node-minimatch")
    (version "3.0.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/minimatch/-/minimatch-3.0.4.tgz")
        (sha256
          (base32
            "0wgammjc9myx0k0k3n9r9cjnv0r1j33cwqiy2fxx7w5nkgbj8sj2"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-brace-expansion"
         ,node-brace-expansion-1.1.11)))
    (home-page
      "https://github.com/isaacs/minimatch#readme")
    (synopsis "a glob matcher in javascript")
    (description "a glob matcher in javascript")
    (license license:isc)))

(define-public node-wrappy-1.0.2
  (package
    (name "node-wrappy")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/wrappy/-/wrappy-1.0.2.tgz")
        (sha256
          (base32
            "1yzx63jf27yz0bk0m78vy4y1cqzm113d2mi9h91y3cdpj46p7wxg"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://github.com/npm/wrappy")
    (synopsis "Callback wrapping utility")
    (description "Callback wrapping utility")
    (license license:isc)))

(define-public node-once-1.4.0
  (package
    (name "node-once")
    (version "1.4.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/once/-/once-1.4.0.tgz")
        (sha256
          (base32
            "1kygzk36kdcfiqz01dhql2dk75rl256m2vlpigv9iikhlc5lclfg"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-wrappy" ,node-wrappy-1.0.2)))
    (home-page
      "https://github.com/isaacs/once#readme")
    (synopsis "Run a function exactly one time")
    (description "Run a function exactly one time")
    (license license:isc)))

(define-public node-path-is-absolute-1.0.1
  (package
    (name "node-path-is-absolute")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/path-is-absolute/-/path-is-absolute-1.0.1.tgz")
        (sha256
          (base32
            "0p7p04xxd8q495qhxmxydyjgzcf762dp1hp2wha2b52n3agp0vbf"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/path-is-absolute#readme")
    (synopsis
      "Node.js 0.12 path.isAbsolute() ponyfill")
    (description
      "Node.js 0.12 path.isAbsolute() ponyfill")
    (license license:expat)))

(define-public node-glob-7.1.6
  (package
    (name "node-glob")
    (version "7.1.6")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/glob/-/glob-7.1.6.tgz")
        (sha256
          (base32
            "1hm62p225wxx15k5kw9b5byif2rdi4ivn2a595lfvv26niq53c2l"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-path-is-absolute"
         ,node-path-is-absolute-1.0.1)
        ("node-once" ,node-once-1.4.0)
        ("node-minimatch" ,node-minimatch-3.0.4)
        ("node-inherits" ,node-inherits-2.0.4)
        ("node-inflight" ,node-inflight-1.0.6)
        ("node-fs-realpath" ,node-fs-realpath-1.0.0)))
    (home-page
      "https://github.com/isaacs/node-glob#readme")
    (synopsis "a little globber")
    (description "a little globber")
    (license license:isc)))

(define-public node-ieee754-1.2.1
  (package
    (name "node-ieee754")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/ieee754/-/ieee754-1.2.1.tgz")
        (sha256
          (base32
            "1b4xiyr6fmgl05cjgc8fiyfk2jagf7xq2y5rknw9scvy76dlpwcf"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/feross/ieee754#readme")
    (synopsis
      "Read/write IEEE754 floating point numbers from/to a Buffer or array-like object")
    (description
      "Read/write IEEE754 floating point numbers from/to a Buffer or array-like object")
    (license license:bsd-3)))

(define-public node-ansi-escapes-3.2.0
  (package
    (name "node-ansi-escapes")
    (version "3.2.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/ansi-escapes/-/ansi-escapes-3.2.0.tgz")
        (sha256
          (base32
            "13ags8v3fa8ijgkhajaqf6nc5s5xhhxkkn9c44npgnwhdcx93v1f"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/ansi-escapes#readme")
    (synopsis
      "ANSI escape codes for manipulating the terminal")
    (description
      "ANSI escape codes for manipulating the terminal")
    (license license:expat)))

(define-public node-chalk-2.4.2
  (package
    (name "node-chalk")
    (version "2.4.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/chalk/-/chalk-2.4.2.tgz")
        (sha256
          (base32
            "0wf6hln5gcjb2n8p18gag6idghl6dfq4if6pxa6s1jqnwr94x26h"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-supports-color"
         ,node-supports-color-5.5.0)
        ("node-escape-string-regexp"
         ,node-escape-string-regexp-1.0.5)
        ("node-ansi-styles" ,node-ansi-styles-3.2.1)))
    (home-page
      "https://github.com/chalk/chalk#readme")
    (synopsis "Terminal string styling done right")
    (description
      "Terminal string styling done right")
    (license license:expat)))

(define-public node-mimic-fn-1.2.0
  (package
    (name "node-mimic-fn")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/mimic-fn/-/mimic-fn-1.2.0.tgz")
        (sha256
          (base32
            "0fkbqr66pl1vzbqph4m8qc0ss1wr7hhnxymdhj32klsxlvbiz4xi"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/mimic-fn#readme")
    (synopsis "Make a function mimic another one")
    (description "Make a function mimic another one")
    (license license:expat)))

(define-public node-onetime-2.0.1
  (package
    (name "node-onetime")
    (version "2.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/onetime/-/onetime-2.0.1.tgz")
        (sha256
          (base32
            "0zmzcy1cg2qdi0g0nvp5picz2gxg6c3xcx7q1rfigfdfyjwha2yr"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-mimic-fn" ,node-mimic-fn-1.2.0)))
    (home-page
      "https://github.com/sindresorhus/onetime#readme")
    (synopsis
      "Ensure a function is only called once")
    (description
      "Ensure a function is only called once")
    (license license:expat)))

(define-public node-signal-exit-3.0.3
  (package
    (name "node-signal-exit")
    (version "3.0.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/signal-exit/-/signal-exit-3.0.3.tgz")
        (sha256
          (base32
            "1iq1f7vb12fl4i2gjg9qmr9580axaypc0dvhx9xjzj0jssxckvqm"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/tapjs/signal-exit")
    (synopsis
      "when you want to fire an event no matter how a process exits.")
    (description
      "when you want to fire an event no matter how a process exits.")
    (license license:isc)))

(define-public node-restore-cursor-2.0.0
  (package
    (name "node-restore-cursor")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/restore-cursor/-/restore-cursor-2.0.0.tgz")
        (sha256
          (base32
            "18vaswnzkljbawjl88k4pr0ww4mqmmc0x8l2xrpfkvyh7jpck2cr"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-signal-exit" ,node-signal-exit-3.0.3)
        ("node-onetime" ,node-onetime-2.0.1)))
    (home-page
      "https://github.com/sindresorhus/restore-cursor#readme")
    (synopsis
      "Gracefully restore the CLI cursor on exit")
    (description
      "Gracefully restore the CLI cursor on exit")
    (license license:expat)))

(define-public node-cli-cursor-2.1.0
  (package
    (name "node-cli-cursor")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/cli-cursor/-/cli-cursor-2.1.0.tgz")
        (sha256
          (base32
            "034sp34k37dacwr16x78c86d9xvrb2z14j88mfwv054bzgkgn9pz"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-restore-cursor"
         ,node-restore-cursor-2.0.0)))
    (home-page
      "https://github.com/sindresorhus/cli-cursor#readme")
    (synopsis "Toggle the CLI cursor")
    (description "Toggle the CLI cursor")
    (license license:expat)))

(define-public node-cli-width-2.2.1
  (package
    (name "node-cli-width")
    (version "2.2.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/cli-width/-/cli-width-2.2.1.tgz")
        (sha256
          (base32
            "0vkir7lsf8psvqrb3s7zzkq7c5ng1mj2zksxcy46km03gr8ldrgg"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/knownasilya/cli-width")
    (synopsis
      "Get stdout window width, with two fallbacks, tty and then a default.")
    (description
      "Get stdout window width, with two fallbacks, tty and then a default.")
    (license license:isc)))

(define-public node-chardet-0.4.2
  (package
    (name "node-chardet")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/chardet/-/chardet-0.4.2.tgz")
        (sha256
          (base32
            "1p2mq4rv3vkwd9k8ipadambn1jw7jzf7fxcrvg16fm3d6slsk7sf"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/runk/node-chardet")
    (synopsis "Character detector")
    (description "Character detector")
    (license license:expat)))

(define-public node-safer-buffer-2.1.2
  (package
    (name "node-safer-buffer")
    (version "2.1.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/safer-buffer/-/safer-buffer-2.1.2.tgz")
        (sha256
          (base32
            "1cx383s7vchfac8jlg3mnb820hkgcvhcpfn9w4f0g61vmrjjz0bq"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/ChALkeR/safer-buffer#readme")
    (synopsis
      "Modern Buffer API polyfill without footguns")
    (description
      "Modern Buffer API polyfill without footguns")
    (license license:expat)))

(define-public node-os-tmpdir-1.0.2
  (package
    (name "node-os-tmpdir")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/os-tmpdir/-/os-tmpdir-1.0.2.tgz")
        (sha256
          (base32
            "12ddjb45wq0swr2159wiaxl2balnli8127if7sc89h3psz125rqk"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/os-tmpdir#readme")
    (synopsis "Node.js os.tmpdir() ponyfill")
    (description "Node.js os.tmpdir() ponyfill")
    (license license:expat)))

(define-public node-tmp-0.0.33
  (package
    (name "node-tmp")
    (version "0.0.33")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/tmp/-/tmp-0.0.33.tgz")
        (sha256
          (base32
            "1ifn24mp4ds8yx9i79739gsdsj5ml6vzx8v3w5vg0m74dig22fyj"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-os-tmpdir" ,node-os-tmpdir-1.0.2)))
    (home-page "http://github.com/raszi/node-tmp")
    (synopsis "Temporary file and directory creator")
    (description
      "Temporary file and directory creator")
    (license license:expat)))

(define-public node-external-editor-2.2.0
  (package
    (name "node-external-editor")
    (version "2.2.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/external-editor/-/external-editor-2.2.0.tgz")
        (sha256
          (base32
            "0l3g4am7l1li9aznhc82wfkan4qcq8lxm2y5lqfmi0vzmfj0jiii"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-tmp" ,node-tmp-0.0.33)
        ("node-iconv-lite" ,node-iconv-lite-0.4.24)
        ("node-chardet" ,node-chardet-0.4.2)))
    (home-page
      "https://github.com/mrkmg/node-external-editor#readme")
    (synopsis
      "Edit a string with the users preferred text editor using $VISUAL or $ENVIRONMENT")
    (description
      "Edit a string with the users preferred text editor using $VISUAL or $ENVIRONMENT")
    (license license:expat)))

(define-public node-figures-2.0.0
  (package
    (name "node-figures")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/figures/-/figures-2.0.0.tgz")
        (sha256
          (base32
            "1xyyajzjnv409rhscns6s5cvg519awj445x7cpx7v8g31pqirk5s"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-escape-string-regexp"
         ,node-escape-string-regexp-1.0.5)))
    (home-page
      "https://github.com/sindresorhus/figures#readme")
    (synopsis
      "Unicode symbols with Windows CMD fallbacks")
    (description
      "Unicode symbols with Windows CMD fallbacks")
    (license license:expat)))

(define-public node-lodash-4.17.20
  (package
    (name "node-lodash")
    (version "4.17.20")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/lodash/-/lodash-4.17.20.tgz")
        (sha256
          (base32
            "1qpjahj6j8l9sag75f9sxfl85r6ab8f7v8w5axv9319wzim8ranj"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://lodash.com/")
    (synopsis "Lodash modular utilities.")
    (description "Lodash modular utilities.")
    (license license:expat)))

(define-public node-mute-stream-0.0.7
  (package
    (name "node-mute-stream")
    (version "0.0.7")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/mute-stream/-/mute-stream-0.0.7.tgz")
        (sha256
          (base32
            "1mzb26ahrc1p99gqhbfdigigl473yjjpi1q9qpnjhqz24dzi0mvg"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/isaacs/mute-stream#readme")
    (synopsis
      "Bytes go in, but they don't come out (when muted).")
    (description
      "Bytes go in, but they don't come out (when muted).")
    (license license:isc)))

(define-public node-run-async-2.4.1
  (package
    (name "node-run-async")
    (version "2.4.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/run-async/-/run-async-2.4.1.tgz")
        (sha256
          (base32
            "0kwddnvq40f38jzgvz9cxs0jk730nx3gfcdb3xvm7ma05sf3mp0f"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/SBoudrias/run-async#readme")
    (synopsis
      "Utility method to run function either synchronously or asynchronously using the common `this.async()` style.")
    (description
      "Utility method to run function either synchronously or asynchronously using the common `this.async()` style.")
    (license license:expat)))

(define-public node-rx-lite-4.0.8
  (package
    (name "node-rx-lite")
    (version "4.0.8")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/rx-lite/-/rx-lite-4.0.8.tgz")
        (sha256
          (base32
            "1w1vdy83nwglrai37qsapwrrixvc5yr490rch6xzkjgcbw0cdpjp"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/Reactive-Extensions/RxJS")
    (synopsis
      "Lightweight library for composing asynchronous and event-based operations in JavaScript")
    (description
      "Lightweight library for composing asynchronous and event-based operations in JavaScript")
    (license #f)))

(define-public node-rx-lite-aggregates-4.0.8
  (package
    (name "node-rx-lite-aggregates")
    (version "4.0.8")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/rx-lite-aggregates/-/rx-lite-aggregates-4.0.8.tgz")
        (sha256
          (base32
            "05z44a4zcwfi2yby9d3ik4msjh23nmpq79py80w6wvzd73iysj4q"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-rx-lite" ,node-rx-lite-4.0.8)))
    (home-page
      "https://github.com/Reactive-Extensions/RxJS")
    (synopsis
      "Lightweight library with aggregate functions for composing asynchronous and event-based operations in JavaScript")
    (description
      "Lightweight library with aggregate functions for composing asynchronous and event-based operations in JavaScript")
    (license #f)))

(define-public node-is-fullwidth-code-point-2.0.0
  (package
    (name "node-is-fullwidth-code-point")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-fullwidth-code-point/-/is-fullwidth-code-point-2.0.0.tgz")
        (sha256
          (base32
            "0sx0mg720hlpxdcg3rpf5ck93bwzkvb5883686v2iwvbxvnx1l2c"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/is-fullwidth-code-point#readme")
    (synopsis
      "Check if the character represented by a given Unicode code point is fullwidth")
    (description
      "Check if the character represented by a given Unicode code point is fullwidth")
    (license license:expat)))

(define-public node-string-width-2.1.1
  (package
    (name "node-string-width")
    (version "2.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/string-width/-/string-width-2.1.1.tgz")
        (sha256
          (base32
            "0b3rb6pbkyg411hvnzb5v5w2vckasgxvslwwijh0p410x46dqz12"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-strip-ansi" ,node-strip-ansi-4.0.0)
        ("node-is-fullwidth-code-point"
         ,node-is-fullwidth-code-point-2.0.0)))
    (home-page
      "https://github.com/sindresorhus/string-width#readme")
    (synopsis
      "Get the visual width of a string - the number of columns required to display it")
    (description
      "Get the visual width of a string - the number of columns required to display it")
    (license license:expat)))

(define-public node-ansi-regex-3.0.0
  (package
    (name "node-ansi-regex")
    (version "3.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/ansi-regex/-/ansi-regex-3.0.0.tgz")
        (sha256
          (base32
            "0d9xwkpwak84xixi7f21bxvrbdgdpwm4pna73jkm8pfk6v8b1bdx"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/chalk/ansi-regex#readme")
    (synopsis
      "Regular expression for matching ANSI escape codes")
    (description
      "Regular expression for matching ANSI escape codes")
    (license license:expat)))

(define-public node-strip-ansi-4.0.0
  (package
    (name "node-strip-ansi")
    (version "4.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/strip-ansi/-/strip-ansi-4.0.0.tgz")
        (sha256
          (base32
            "0b90ys7pxxbavph56rhfmlymla8f8vaq7fy2pa91dq4r6r3sic5a"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-ansi-regex" ,node-ansi-regex-3.0.0)))
    (home-page
      "https://github.com/chalk/strip-ansi#readme")
    (synopsis "Strip ANSI escape codes")
    (description "Strip ANSI escape codes")
    (license license:expat)))

(define-public node-through-2.3.8
  (package
    (name "node-through")
    (version "2.3.8")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/through/-/through-2.3.8.tgz")
        (sha256
          (base32
            "0gjpaj9lwd6s356z2lljj2yj0pxwvdr8sckb6lkmfgmi1y67mchn"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/dominictarr/through")
    (synopsis "simplified stream construction")
    (description "simplified stream construction")
    (license license:expat)))

(define-public node-inquirer-3.3.0
  (package
    (name "node-inquirer")
    (version "3.3.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/inquirer/-/inquirer-3.3.0.tgz")
        (sha256
          (base32
            "0vjqv6xk5cw6p1kq563nvyb7117gyia8ry30r6a45c57kjv7s8bi"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-through" ,node-through-2.3.8)
        ("node-strip-ansi" ,node-strip-ansi-4.0.0)
        ("node-string-width" ,node-string-width-2.1.1)
        ("node-rx-lite-aggregates"
         ,node-rx-lite-aggregates-4.0.8)
        ("node-rx-lite" ,node-rx-lite-4.0.8)
        ("node-run-async" ,node-run-async-2.4.1)
        ("node-mute-stream" ,node-mute-stream-0.0.7)
        ("node-lodash" ,node-lodash-4.17.20)
        ("node-figures" ,node-figures-2.0.0)
        ("node-external-editor"
         ,node-external-editor-2.2.0)
        ("node-cli-width" ,node-cli-width-2.2.1)
        ("node-cli-cursor" ,node-cli-cursor-2.1.0)
        ("node-chalk" ,node-chalk-2.4.2)
        ("node-ansi-escapes" ,node-ansi-escapes-3.2.0)))
    (home-page
      "https://github.com/SBoudrias/Inquirer.js#readme")
    (synopsis
      "A collection of common interactive command line user interfaces.")
    (description
      "A collection of common interactive command line user interfaces.")
    (license license:expat)))

(define-public node-chain-able-3.0.0
  (package
    (name "node-chain-able")
    (version "3.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/chain-able/-/chain-able-3.0.0.tgz")
        (sha256
          (base32
            "0pk2szx36nv5lx3ndfiai36jxqwkd28cyyhycy964lqrrlmf8930"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/fluents/chain-able#readme")
    (synopsis
      "interfaces that describe their intentions.")
    (description
      "interfaces that describe their intentions.")
    (license license:expat)))

(define-public node-lego-api-1.0.8
  (package
    (name "node-lego-api")
    (version "1.0.8")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/lego-api/-/lego-api-1.0.8.tgz")
        (sha256
          (base32
            "0q1y63fyki5ss4f9y01q93bk39wli46fjvvf9wlbn8w53my05iqw"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-chain-able" ,node-chain-able-3.0.0)))
    (home-page
      "https://github.com/fuse-box/lego-api#readme")
    (synopsis "lego api")
    (description "lego api")
    (license license:expat)))

(define-public node-mustache-2.3.2
  (package
    (name "node-mustache")
    (version "2.3.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/mustache/-/mustache-2.3.2.tgz")
        (sha256
          (base32
            "0mpjnc0h2aplfgbz74j1hhghh43sbyra5x8gm7ri4syh0nfmsx9a"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://github.com/janl/mustache.js")
    (synopsis
      "Logic-less {{mustache}} templates with JavaScript")
    (description
      "Logic-less {{mustache}} templates with JavaScript")
    (license license:expat)))

(define-public node-color-name-1.1.3
  (package
    (name "node-color-name")
    (version "1.1.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/color-name/-/color-name-1.1.3.tgz")
        (sha256
          (base32
            "0kkq17s5yg6lg8ncg2nls6asih58qafn7wbrcgmwnqr5zdqk7vxh"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/dfcreative/color-name")
    (synopsis "A list of color names and its values")
    (description
      "A list of color names and its values")
    (license license:expat)))

(define-public node-color-convert-1.9.3
  (package
    (name "node-color-convert")
    (version "1.9.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/color-convert/-/color-convert-1.9.3.tgz")
        (sha256
          (base32
            "1ahbdssv1qgwlzvhv7731hpfgz8wny0619x97b7n5x9lckj17i0j"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-color-name" ,node-color-name-1.1.3)))
    (home-page
      "https://github.com/Qix-/color-convert#readme")
    (synopsis "Plain color conversion functions")
    (description "Plain color conversion functions")
    (license license:expat)))

(define-public node-ansi-styles-3.2.1
  (package
    (name "node-ansi-styles")
    (version "3.2.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/ansi-styles/-/ansi-styles-3.2.1.tgz")
        (sha256
          (base32
            "1wqd08glq159q724kvpi6nnf87biajr749a7r9c84xm639g6463k"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-color-convert" ,node-color-convert-1.9.3)))
    (home-page
      "https://github.com/chalk/ansi-styles#readme")
    (synopsis
      "ANSI escape codes for styling strings in the terminal")
    (description
      "ANSI escape codes for styling strings in the terminal")
    (license license:expat)))

(define-public node-escape-string-regexp-1.0.5
  (package
    (name "node-escape-string-regexp")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/escape-string-regexp/-/escape-string-regexp-1.0.5.tgz")
        (sha256
          (base32
            "0iy3jirnnslnfwk8wa5xkg56fnbmg7bsv5v2a1s0qgbnfqp7j375"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/escape-string-regexp")
    (synopsis "Escape RegExp special characters")
    (description "Escape RegExp special characters")
    (license license:expat)))

(define-public node-supports-color-5.5.0
  (package
    (name "node-supports-color")
    (version "5.5.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/supports-color/-/supports-color-5.5.0.tgz")
        (sha256
          (base32
            "1ap0lk4n0m3948cnkfmyz71pizqlzjdfrhs0f954pksg4jnk52h5"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-has-flag" ,node-has-flag-3.0.0)))
    (home-page
      "https://github.com/chalk/supports-color#readme")
    (synopsis
      "Detect whether a terminal supports color")
    (description
      "Detect whether a terminal supports color")
    (license license:expat)))

(define-public node-has-flag-3.0.0
  (package
    (name "node-has-flag")
    (version "3.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/has-flag/-/has-flag-3.0.0.tgz")
        (sha256
          (base32
            "1sp0m48zavms86q7vkf90mwll9z2bqi11hk3s01aw8nw40r72jzd"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/has-flag#readme")
    (synopsis "Check if argv has a specific flag")
    (description "Check if argv has a specific flag")
    (license license:expat)))

(define-public node-postcss-6.0.23
  (package
    (name "node-postcss")
    (version "6.0.23")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/postcss/-/postcss-6.0.23.tgz")
        (sha256
          (base32
            "1r6pmafj6i6qysgmkl5yjcjjxfbbylx4yf1fb82r3bsp3acx2m8k"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-supports-color"
         ,node-supports-color-5.5.0)
        ("node-source-map" ,node-source-map-0.6.1)
        ("node-chalk" ,node-chalk-2.4.2)))
    (home-page "https://postcss.org/")
    (synopsis
      "Tool for transforming styles with JS plugins")
    (description
      "Tool for transforming styles with JS plugins")
    (license license:expat)))

(define-public node-is-buffer-1.1.6
  (package
    (name "node-is-buffer")
    (version "1.1.6")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-buffer/-/is-buffer-1.1.6.tgz")
        (sha256
          (base32
            "03l8f9r41xy0lq5zjm790jg758r8wv3fcsfwsd8331w6l30dh6ix"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/feross/is-buffer#readme")
    (synopsis "Determine if an object is a Buffer")
    (description
      "Determine if an object is a Buffer")
    (license license:expat)))

(define-public node-nanoseconds-0.1.0
  (package
    (name "node-nanoseconds")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/nanoseconds/-/nanoseconds-0.1.0.tgz")
        (sha256
          (base32
            "0ap9dmnqj0k5mn7bxcj6icbw20sh15j8i1d5zfsyi2la8ic5mn2a"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jonschlinkert/nanoseconds")
    (synopsis
      "Convert the process.hrtime array to a single nanoseconds value.")
    (description
      "Convert the process.hrtime array to a single nanoseconds value.")
    (license license:expat)))

(define-public node-pretty-time-0.2.0
  (package
    (name "node-pretty-time")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/pretty-time/-/pretty-time-0.2.0.tgz")
        (sha256
          (base32
            "0i1qjipzd1dd5m1zrngx9awxyikvmfjl62bl5c1b5lbvf6k0qb46"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-nanoseconds" ,node-nanoseconds-0.1.0)
        ("node-is-number" ,node-is-number-2.1.0)))
    (home-page
      "https://github.com/jonschlinkert/pretty-time")
    (synopsis
      "Easily format the time from node.js `process.hrtime`. Works with timescales ranging from weeks to nanoseconds.")
    (description
      "Easily format the time from node.js `process.hrtime`. Works with timescales ranging from weeks to nanoseconds.")
    (license license:expat)))

(define-public node-prettysize-0.0.3
  (package
    (name "node-prettysize")
    (version "0.0.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/prettysize/-/prettysize-0.0.3.tgz")
        (sha256
          (base32
            "01kgjq1p0dr3k2gj2vlk2qba66l0bwr79z17f9ah2q939y6bwdwg"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page #f)
    (synopsis
      "Convert bytes to other sizes for prettier logging")
    (description
      "Convert bytes to other sizes for prettier logging")
    (license #f)))

(define-public node-app-root-path-1.4.0
  (package
    (name "node-app-root-path")
    (version "1.4.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/app-root-path/-/app-root-path-1.4.0.tgz")
        (sha256
          (base32
            "00ccw98bvkbrzrwf40ykq6jlckvyxbag1sl8answjqg86h2xs3lb"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/inxilpro/node-app-root-path")
    (synopsis
      "Determine an app's root path from anywhere inside the app")
    (description
      "Determine an app's root path from anywhere inside the app")
    (license license:expat)))

(define-public node-minimist-1.2.5
  (package
    (name "node-minimist")
    (version "1.2.5")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/minimist/-/minimist-1.2.5.tgz")
        (sha256
          (base32
            "0l23rq2pam1khc06kd7fv0ys2cq0mlgs82dxjxjfjmlksgj0r051"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/substack/minimist")
    (synopsis "parse argument options")
    (description "parse argument options")
    (license license:expat)))

(define-public node-mkdirp-0.5.5
  (package
    (name "node-mkdirp")
    (version "0.5.5")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/mkdirp/-/mkdirp-0.5.5.tgz")
        (sha256
          (base32
            "02mvn5hllnsxzli8yy0gkgkkxndbwd3fh302shadsag3c4db0njf"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-minimist" ,node-minimist-1.2.5)))
    (home-page
      "https://github.com/substack/node-mkdirp#readme")
    (synopsis "Recursively mkdir, like `mkdir -p`")
    (description
      "Recursively mkdir, like `mkdir -p`")
    (license license:expat)))

(define-public node-realm-utils-1.0.9
  (package
    (name "node-realm-utils")
    (version "1.0.9")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/realm-utils/-/realm-utils-1.0.9.tgz")
        (sha256
          (base32
            "06vl01rm2sxbjmdfwr4m736khcppq4nc0wn2apcg8dqgcvad09y9"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-mkdirp" ,node-mkdirp-0.5.5)
        ("node-app-root-path" ,node-app-root-path-1.4.0)))
    (home-page
      "https://github.com/realm-js/realm-utils#readme")
    (synopsis
      "Realm-js has a set of functionality that helps solving many problems or impediments related to Promises. Utilities live in this repository, apart from realm-js library. Typings included ### Install")
    (description
      "Realm-js has a set of functionality that helps solving many problems or impediments related to Promises. Utilities live in this repository, apart from realm-js library. Typings included ### Install")
    (license license:isc)))

(define-public node-regenerate-1.4.2
  (package
    (name "node-regenerate")
    (version "1.4.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/regenerate/-/regenerate-1.4.2.tgz")
        (sha256
          (base32
            "07km6in83r5da81jn43ps2gll0gpdykcy0aa94v3vlwh83ygqy33"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://mths.be/regenerate")
    (synopsis
      "Generate JavaScript-compatible regular expressions based on a given set of Unicode symbols or code points.")
    (description
      "Generate JavaScript-compatible regular expressions based on a given set of Unicode symbols or code points.")
    (license license:expat)))

(define-public node-regenerate-unicode-properties-8.2.0
  (package
    (name "node-regenerate-unicode-properties")
    (version "8.2.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/regenerate-unicode-properties/-/regenerate-unicode-properties-8.2.0.tgz")
        (sha256
          (base32
            "0piv2vsrvkwmyb5rril6j7br4xvl1lv33g89hzqnpg0wap9lsp4g"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-regenerate" ,node-regenerate-1.4.2)))
    (home-page
      "https://github.com/mathiasbynens/regenerate-unicode-properties")
    (synopsis
      "Regenerate sets for Unicode properties and values.")
    (description
      "Regenerate sets for Unicode properties and values.")
    (license license:expat)))

(define-public node-regjsgen-0.5.2
  (package
    (name "node-regjsgen")
    (version "0.5.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/regjsgen/-/regjsgen-0.5.2.tgz")
        (sha256
          (base32
            "1rcqz05jgsfq3v23aaryhfxkk2fcj4d22ah46dyg91zgjhpii74r"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/bnjmnt4n/regjsgen")
    (synopsis
      "Generate regular expressions from regjsparserâ\x80\x99s AST.")
    (description
      "Generate regular expressions from regjsparserâ\x80\x99s AST.")
    (license license:expat)))

(define-public node-jsesc-0.5.0
  (package
    (name "node-jsesc")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/jsesc/-/jsesc-0.5.0.tgz")
        (sha256
          (base32
            "0x365wjgs65s1xa7zad9rbg5izayxbh2mfgsjgqm7zl0gi3116iy"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "http://mths.be/jsesc")
    (synopsis
      "A JavaScript library for escaping JavaScript strings while generating the shortest possible valid output.")
    (description
      "A JavaScript library for escaping JavaScript strings while generating the shortest possible valid output.")
    (license #f)))

(define-public node-regjsparser-0.6.7
  (package
    (name "node-regjsparser")
    (version "0.6.7")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/regjsparser/-/regjsparser-0.6.7.tgz")
        (sha256
          (base32
            "162xnk1xj0yz3p4qg9hb7yryrijgahymkin7s461bv7gv0ikp9c4"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-jsesc" ,node-jsesc-0.5.0)))
    (home-page
      "https://github.com/jviereck/regjsparser")
    (synopsis
      "Parsing the JavaScript's RegExp in JavaScript.")
    (description
      "Parsing the JavaScript's RegExp in JavaScript.")
    (license #f)))

(define-public node-unicode-canonical-property-names-ecmascript-1.0.4
  (package
    (name "node-unicode-canonical-property-names-ecmascript")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/unicode-canonical-property-names-ecmascript/-/unicode-canonical-property-names-ecmascript-1.0.4.tgz")
        (sha256
          (base32
            "05pwacrq8a4l63zcxljrbxxm11f2rhmzspvsmdph94yy69rl49cn"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/mathiasbynens/unicode-canonical-property-names-ecmascript")
    (synopsis
      "The set of canonical Unicode property names supported in ECMAScript RegExp property escapes.")
    (description
      "The set of canonical Unicode property names supported in ECMAScript RegExp property escapes.")
    (license license:expat)))

(define-public node-unicode-property-aliases-ecmascript-1.1.0
  (package
    (name "node-unicode-property-aliases-ecmascript")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/unicode-property-aliases-ecmascript/-/unicode-property-aliases-ecmascript-1.1.0.tgz")
        (sha256
          (base32
            "0x43wbprnz7ppc5diwsbz0lry397h6jmpinmmizi9kmsxk5yh8za"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/mathiasbynens/unicode-property-aliases-ecmascript")
    (synopsis
      "Unicode property alias mappings in JavaScript format for property names that are supported in ECMAScript RegExp property escapes.")
    (description
      "Unicode property alias mappings in JavaScript format for property names that are supported in ECMAScript RegExp property escapes.")
    (license license:expat)))

(define-public node-unicode-match-property-ecmascript-1.0.4
  (package
    (name "node-unicode-match-property-ecmascript")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/unicode-match-property-ecmascript/-/unicode-match-property-ecmascript-1.0.4.tgz")
        (sha256
          (base32
            "0z4gd8jr8haxp2rdndqgxxqzp7ik83qp4phf8637zvhv2f20sdcq"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-unicode-property-aliases-ecmascript"
         ,node-unicode-property-aliases-ecmascript-1.1.0)
        ("node-unicode-canonical-property-names-ecmascript"
         ,node-unicode-canonical-property-names-ecmascript-1.0.4)))
    (home-page
      "https://github.com/mathiasbynens/unicode-match-property-ecmascript")
    (synopsis
      "Match a Unicode property or property alias to its canonical property name per the algorithm used for RegExp Unicode property escapes in ECMAScript.")
    (description
      "Match a Unicode property or property alias to its canonical property name per the algorithm used for RegExp Unicode property escapes in ECMAScript.")
    (license license:expat)))

(define-public node-unicode-match-property-value-ecmascript-1.2.0
  (package
    (name "node-unicode-match-property-value-ecmascript")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/unicode-match-property-value-ecmascript/-/unicode-match-property-value-ecmascript-1.2.0.tgz")
        (sha256
          (base32
            "13w08xs4vd5m5c9f9b3h13adar9gwr54wf5ki8a1q4z8q3g9970n"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/mathiasbynens/unicode-match-property-value-ecmascript")
    (synopsis
      "Match a Unicode property or property alias to its canonical property name per the algorithm used for RegExp Unicode property escapes in ECMAScript.")
    (description
      "Match a Unicode property or property alias to its canonical property name per the algorithm used for RegExp Unicode property escapes in ECMAScript.")
    (license license:expat)))

(define-public node-regexpu-core-4.7.1
  (package
    (name "node-regexpu-core")
    (version "4.7.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/regexpu-core/-/regexpu-core-4.7.1.tgz")
        (sha256
          (base32
            "1klc37nx92vjs48f5gq1hiqignsbcyca0l20yagcj7k850vn329k"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-unicode-match-property-value-ecmascript"
         ,node-unicode-match-property-value-ecmascript-1.2.0)
        ("node-unicode-match-property-ecmascript"
         ,node-unicode-match-property-ecmascript-1.0.4)
        ("node-regjsparser" ,node-regjsparser-0.6.7)
        ("node-regjsgen" ,node-regjsgen-0.5.2)
        ("node-regenerate-unicode-properties"
         ,node-regenerate-unicode-properties-8.2.0)
        ("node-regenerate" ,node-regenerate-1.4.2)))
    (home-page "https://mths.be/regexpu")
    (synopsis
      "regexpuâ\x80\x99s core functionality (i.e. `rewritePattern(pattern, flag)`), capable of translating ES6 Unicode regular expressions to ES5.")
    (description
      "regexpuâ\x80\x99s core functionality (i.e. `rewritePattern(pattern, flag)`), capable of translating ES6 Unicode regular expressions to ES5.")
    (license license:expat)))

(define-public node-aws-sign2-0.7.0
  (package
    (name "node-aws-sign2")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/aws-sign2/-/aws-sign2-0.7.0.tgz")
        (sha256
          (base32
            "12bjw01pgh0nfyxi7vw6lvsapyvnazrnyn4qf87lclardjaz9yd8"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/mikeal/aws-sign#readme")
    (synopsis
      "AWS signing. Originally pulled from LearnBoost/knox, maintained as vendor in request, now a standalone module.")
    (description
      "AWS signing. Originally pulled from LearnBoost/knox, maintained as vendor in request, now a standalone module.")
    (license license:asl2.0)))

(define-public node-aws4-1.11.0
  (package
    (name "node-aws4")
    (version "1.11.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/aws4/-/aws4-1.11.0.tgz")
        (sha256
          (base32
            "1a9gd1wyx3k9agcprmgpfhp63w9v6cvf2kxfnijk77k6zcqqqjnd"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/mhart/aws4#readme")
    (synopsis
      "Signs and prepares requests using AWS Signature Version 4")
    (description
      "Signs and prepares requests using AWS Signature Version 4")
    (license license:expat)))

(define-public node-caseless-0.12.0
  (package
    (name "node-caseless")
    (version "0.12.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/caseless/-/caseless-0.12.0.tgz")
        (sha256
          (base32
            "165fzm8s6qxapxk8xlb548q58xjav55k5nnychr234282irb2zjd"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/mikeal/caseless#readme")
    (synopsis
      "Caseless object set/get/has, very useful when working with HTTP headers.")
    (description
      "Caseless object set/get/has, very useful when working with HTTP headers.")
    (license license:asl2.0)))

(define-public node-combined-stream-1.0.8
  (package
    (name "node-combined-stream")
    (version "1.0.8")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/combined-stream/-/combined-stream-1.0.8.tgz")
        (sha256
          (base32
            "04hm5rrkwda2qgy1afwhrz42asmflw5hxkbpxddn741ywnmmmgmn"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-delayed-stream"
         ,node-delayed-stream-1.0.0)))
    (home-page
      "https://github.com/felixge/node-combined-stream")
    (synopsis
      "A stream that emits multiple other streams one after another.")
    (description
      "A stream that emits multiple other streams one after another.")
    (license license:expat)))

(define-public node-extend-3.0.2
  (package
    (name "node-extend")
    (version "3.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/extend/-/extend-3.0.2.tgz")
        (sha256
          (base32
            "1ckjrzapv4awrafybcvq3n5rcqm6ljswfdx97wibl355zaqd148x"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/justmoon/node-extend#readme")
    (synopsis
      "Port of jQuery.extend for node.js and the browser")
    (description
      "Port of jQuery.extend for node.js and the browser")
    (license license:expat)))

(define-public node-forever-agent-0.6.1
  (package
    (name "node-forever-agent")
    (version "0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/forever-agent/-/forever-agent-0.6.1.tgz")
        (sha256
          (base32
            "1i86r2ip6ryrnpg3v7pf0ywddhsdlr809xycd3zm9gq7zphn5a7c"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/mikeal/forever-agent")
    (synopsis
      "HTTP Agent that keeps socket connections alive between keep-alive requests. Formerly part of mikeal/request, now a standalone module.")
    (description
      "HTTP Agent that keeps socket connections alive between keep-alive requests. Formerly part of mikeal/request, now a standalone module.")
    (license license:asl2.0)))

(define-public node-asynckit-0.4.0
  (package
    (name "node-asynckit")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/asynckit/-/asynckit-0.4.0.tgz")
        (sha256
          (base32
            "1kvxnmjbjwqc8gvp4ms7d8w8x7y41rcizmz4898694h7ywq4y9cc"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/alexindigo/asynckit#readme")
    (synopsis
      "Minimal async jobs utility library, with streams support")
    (description
      "Minimal async jobs utility library, with streams support")
    (license license:expat)))

(define-public node-delayed-stream-1.0.0
  (package
    (name "node-delayed-stream")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/delayed-stream/-/delayed-stream-1.0.0.tgz")
        (sha256
          (base32
            "1lr98585rayrc5xfj599hg6mxqvks38diir74ivivyvx47jgqf5c"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/felixge/node-delayed-stream")
    (synopsis
      "Buffers events from a stream until you are ready to handle them.")
    (description
      "Buffers events from a stream until you are ready to handle them.")
    (license license:expat)))

(define-public node-form-data-2.3.3
  (package
    (name "node-form-data")
    (version "2.3.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/form-data/-/form-data-2.3.3.tgz")
        (sha256
          (base32
            "1j1ka178syqqaycr1m3vqahbb3bi7qsks0mp0iqbd6y7yj1wz7p3"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-mime-types" ,node-mime-types-2.1.28)
        ("node-combined-stream"
         ,node-combined-stream-1.0.8)
        ("node-asynckit" ,node-asynckit-0.4.0)))
    (home-page
      "https://github.com/form-data/form-data#readme")
    (synopsis
      "A library to create readable \"multipart/form-data\" streams. Can be used to submit forms and file uploads to other web applications.")
    (description
      "A library to create readable \"multipart/form-data\" streams. Can be used to submit forms and file uploads to other web applications.")
    (license license:expat)))

(define-public node-fast-deep-equal-3.1.3
  (package
    (name "node-fast-deep-equal")
    (version "3.1.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/fast-deep-equal/-/fast-deep-equal-3.1.3.tgz")
        (sha256
          (base32
            "13vvwib6za4zh7054n3fg86y127ig3jb0djqz31qsqr71yca06dh"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/epoberezkin/fast-deep-equal#readme")
    (synopsis "Fast deep equal")
    (description "Fast deep equal")
    (license license:expat)))

(define-public node-fast-json-stable-stringify-2.1.0
  (package
    (name "node-fast-json-stable-stringify")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/fast-json-stable-stringify/-/fast-json-stable-stringify-2.1.0.tgz")
        (sha256
          (base32
            "11qnzlan5yd2hg9nqi9hdv48bq6kwvw9pxsxir22n2iyqhighb8y"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/epoberezkin/fast-json-stable-stringify")
    (synopsis
      "deterministic `JSON.stringify()` - a faster version of substack's json-stable-strigify without jsonify")
    (description
      "deterministic `JSON.stringify()` - a faster version of substack's json-stable-strigify without jsonify")
    (license license:expat)))

(define-public node-json-schema-traverse-0.4.1
  (package
    (name "node-json-schema-traverse")
    (version "0.4.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/json-schema-traverse/-/json-schema-traverse-0.4.1.tgz")
        (sha256
          (base32
            "0rf0pvm62k8g81vs7n7zx080p6sfylwk52vc149jx1216vcssdgp"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/epoberezkin/json-schema-traverse#readme")
    (synopsis
      "Traverse JSON Schema passing each schema object to callback")
    (description
      "Traverse JSON Schema passing each schema object to callback")
    (license license:expat)))

(define-public node-punycode-2.1.1
  (package
    (name "node-punycode")
    (version "2.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/punycode/-/punycode-2.1.1.tgz")
        (sha256
          (base32
            "0g7z0kdxs15jrcijwbka2jajgr4b7bvpa6xmrcs0wf82pxwx1k75"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://mths.be/punycode")
    (synopsis
      "A robust Punycode converter that fully complies to RFC 3492 and RFC 5891, and works on nearly all JavaScript platforms.")
    (description
      "A robust Punycode converter that fully complies to RFC 3492 and RFC 5891, and works on nearly all JavaScript platforms.")
    (license license:expat)))

(define-public node-uri-js-4.4.1
  (package
    (name "node-uri-js")
    (version "4.4.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/uri-js/-/uri-js-4.4.1.tgz")
        (sha256
          (base32
            "0bcdxkngap84iv7hpfa4r18i3a3allxfh6dmcqzafgg8mx9dw4jn"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-punycode" ,node-punycode-2.1.1)))
    (home-page "https://github.com/garycourt/uri-js")
    (synopsis
      "An RFC 3986/3987 compliant, scheme extendable URI/IRI parsing/validating/resolving library for JavaScript.")
    (description
      "An RFC 3986/3987 compliant, scheme extendable URI/IRI parsing/validating/resolving library for JavaScript.")
    (license #f)))

(define-public node-ajv-6.12.6
  (package
    (name "node-ajv")
    (version "6.12.6")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/ajv/-/ajv-6.12.6.tgz")
        (sha256
          (base32
            "0jhk2dnzrk188p3micnkh7126lhdbkj9iip0pywhky6vh1dk8xcr"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-uri-js" ,node-uri-js-4.4.1)
        ("node-json-schema-traverse"
         ,node-json-schema-traverse-0.4.1)
        ("node-fast-json-stable-stringify"
         ,node-fast-json-stable-stringify-2.1.0)
        ("node-fast-deep-equal"
         ,node-fast-deep-equal-3.1.3)))
    (home-page
      "https://github.com/ajv-validator/ajv")
    (synopsis "Another JSON Schema Validator")
    (description "Another JSON Schema Validator")
    (license license:expat)))

(define-public node-har-schema-2.0.0
  (package
    (name "node-har-schema")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/har-schema/-/har-schema-2.0.0.tgz")
        (sha256
          (base32
            "09myh5q5225c53v39mw9n3a2kgf2pk0z9dfwbmm7rbb70npq8yrf"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/ahmadnassri/har-schema")
    (synopsis "JSON Schema for HTTP Archive (HAR)")
    (description
      "JSON Schema for HTTP Archive (HAR)")
    (license license:isc)))

(define-public node-har-validator-5.1.5
  (package
    (name "node-har-validator")
    (version "5.1.5")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/har-validator/-/har-validator-5.1.5.tgz")
        (sha256
          (base32
            "02vymdr8s3x1cbsv15m9fq6bnbiajyjy8vdz0hl9vrv8xi5ay27f"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-har-schema" ,node-har-schema-2.0.0)
        ("node-ajv" ,node-ajv-6.12.6)))
    (home-page
      "https://github.com/ahmadnassri/node-har-validator")
    (synopsis
      "Extremely fast HTTP Archive (HAR) validator using JSON Schema")
    (description
      "Extremely fast HTTP Archive (HAR) validator using JSON Schema")
    (license license:expat)))

(define-public node-assert-plus-1.0.0
  (package
    (name "node-assert-plus")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/assert-plus/-/assert-plus-1.0.0.tgz")
        (sha256
          (base32
            "1srkj0nyslz3rbfncj59sqbsllavmwik8gphd7jxwjshf52mras7"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/mcavage/node-assert-plus#readme")
    (synopsis
      "Extra assertions on top of node's assert module")
    (description
      "Extra assertions on top of node's assert module")
    (license license:expat)))

(define-public node-extsprintf-1.3.0
  (package
    (name "node-extsprintf")
    (version "1.3.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/extsprintf/-/extsprintf-1.3.0.tgz")
        (sha256
          (base32
            "0i6hmr7mkg76rgrxs7f0xny48kha2xi03wj43mfik77m0lk3k6yg"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/davepacheco/node-extsprintf")
    (synopsis "extended POSIX-style sprintf")
    (description "extended POSIX-style sprintf")
    (license license:expat)))

(define-public node-json-schema-0.2.3
  (package
    (name "node-json-schema")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/json-schema/-/json-schema-0.2.3.tgz")
        (sha256
          (base32
            "0gwkxqmwlwb5nffgxsjf1rcd1lv21br555mxr5mcnc60zd9kq5p3"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/kriszyp/json-schema#readme")
    (synopsis
      "JSON Schema validation and specifications")
    (description
      "JSON Schema validation and specifications")
    (license #f)))

(define-public node-core-util-is-1.0.2
  (package
    (name "node-core-util-is")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/core-util-is/-/core-util-is-1.0.2.tgz")
        (sha256
          (base32
            "164k94d9bdzw1335kzakj7hflhnnixpx4n6ydbhf7vbrcnmlv954"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/isaacs/core-util-is#readme")
    (synopsis
      "The `util.is*` functions introduced in Node v0.12.")
    (description
      "The `util.is*` functions introduced in Node v0.12.")
    (license license:expat)))

(define-public node-extsprintf-1.4.0
  (package
    (name "node-extsprintf")
    (version "1.4.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/extsprintf/-/extsprintf-1.4.0.tgz")
        (sha256
          (base32
            "1lphcbxrq7x1gz84f71hx7z4fkick5knxbif1cxim53ifn5cwrxm"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/davepacheco/node-extsprintf#readme")
    (synopsis "extended POSIX-style sprintf")
    (description "extended POSIX-style sprintf")
    (license license:expat)))

(define-public node-verror-1.10.0
  (package
    (name "node-verror")
    (version "1.10.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/verror/-/verror-1.10.0.tgz")
        (sha256
          (base32
            "0swyg46nvq95xlrrjjbhhmhjrdxg19yrc1aj69zipck0vi24b6q1"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-extsprintf" ,node-extsprintf-1.4.0)
        ("node-core-util-is" ,node-core-util-is-1.0.2)
        ("node-assert-plus" ,node-assert-plus-1.0.0)))
    (home-page
      "https://github.com/davepacheco/node-verror")
    (synopsis "richer JavaScript errors")
    (description "richer JavaScript errors")
    (license license:expat)))

(define-public node-jsprim-1.4.1
  (package
    (name "node-jsprim")
    (version "1.4.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/jsprim/-/jsprim-1.4.1.tgz")
        (sha256
          (base32
            "0ipc481jham9q4ayfl335zjdfmnxc1wcixx5qibfwl2ncz60gwqx"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-verror" ,node-verror-1.10.0)
        ("node-json-schema" ,node-json-schema-0.2.3)
        ("node-extsprintf" ,node-extsprintf-1.3.0)
        ("node-assert-plus" ,node-assert-plus-1.0.0)))
    (home-page
      "https://github.com/joyent/node-jsprim#readme")
    (synopsis
      "utilities for primitive JavaScript types")
    (description
      "utilities for primitive JavaScript types")
    (license license:expat)))

(define-public node-asn1-0.2.4
  (package
    (name "node-asn1")
    (version "0.2.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/asn1/-/asn1-0.2.4.tgz")
        (sha256
          (base32
            "0fymjdkmd7kg4kyga0hssiwdz9jz5nv8fyndl3l1dr9q9jbl9971"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-safer-buffer" ,node-safer-buffer-2.1.2)))
    (home-page
      "https://github.com/joyent/node-asn1#readme")
    (synopsis
      "Contains parsers and serializers for ASN.1 (currently BER only)")
    (description
      "Contains parsers and serializers for ASN.1 (currently BER only)")
    (license license:expat)))

(define-public node-dashdash-1.14.1
  (package
    (name "node-dashdash")
    (version "1.14.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/dashdash/-/dashdash-1.14.1.tgz")
        (sha256
          (base32
            "0h2kaml5wgx5x430wlbnjz3j6q1ppvndqckylfmi13xa33gfnycb"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-assert-plus" ,node-assert-plus-1.0.0)))
    (home-page
      "https://github.com/trentm/node-dashdash#readme")
    (synopsis
      "A light, featureful and explicit option parsing library.")
    (description
      "A light, featureful and explicit option parsing library.")
    (license license:expat)))

(define-public node-getpass-0.1.7
  (package
    (name "node-getpass")
    (version "0.1.7")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/getpass/-/getpass-0.1.7.tgz")
        (sha256
          (base32
            "0ifl7rdzhkbwzb2pmi6mxvv92qd2ihbfbfkipw9nqvbn22x140wg"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-assert-plus" ,node-assert-plus-1.0.0)))
    (home-page
      "https://github.com/arekinath/node-getpass#readme")
    (synopsis "getpass for node.js")
    (description "getpass for node.js")
    (license license:expat)))

(define-public node-tweetnacl-0.14.5
  (package
    (name "node-tweetnacl")
    (version "0.14.5")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/tweetnacl/-/tweetnacl-0.14.5.tgz")
        (sha256
          (base32
            "1mnzrxlww1sqwv493gn6ph9ak7n8l9w5qrahsa5kzn4vgbb37skc"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://tweetnacl.js.org")
    (synopsis
      "Port of TweetNaCl cryptographic library to JavaScript")
    (description
      "Port of TweetNaCl cryptographic library to JavaScript")
    (license license:unlicense)))

(define-public node-jsbn-0.1.1
  (package
    (name "node-jsbn")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/jsbn/-/jsbn-0.1.1.tgz")
        (sha256
          (base32
            "08r3wxx18yixax4w9rs18ya1ggw6kgzjhw5vbsj7sb8a974lpi2s"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/andyperlitch/jsbn#readme")
    (synopsis
      "The jsbn library is a fast, portable implementation of large-number math in pure JavaScript, enabling public-key crypto and other applications on desktop and mobile browsers.")
    (description
      "The jsbn library is a fast, portable implementation of large-number math in pure JavaScript, enabling public-key crypto and other applications on desktop and mobile browsers.")
    (license license:expat)))

(define-public node-ecc-jsbn-0.1.2
  (package
    (name "node-ecc-jsbn")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/ecc-jsbn/-/ecc-jsbn-0.1.2.tgz")
        (sha256
          (base32
            "0x39lihzphr0h1fvh9p65k86vx3p7z6jrxgv4b402lvdrifd56k0"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-safer-buffer" ,node-safer-buffer-2.1.2)
        ("node-jsbn" ,node-jsbn-0.1.1)))
    (home-page
      "https://github.com/quartzjer/ecc-jsbn")
    (synopsis "ECC JS code based on JSBN")
    (description "ECC JS code based on JSBN")
    (license license:expat)))

(define-public node-bcrypt-pbkdf-1.0.2
  (package
    (name "node-bcrypt-pbkdf")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/bcrypt-pbkdf/-/bcrypt-pbkdf-1.0.2.tgz")
        (sha256
          (base32
            "09kqy1rjj0b1aavdssglrjj8ayf9vxvnnvlh5ah270j3bngrwgp1"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-tweetnacl" ,node-tweetnacl-0.14.5)))
    (home-page
      "https://github.com/joyent/node-bcrypt-pbkdf#readme")
    (synopsis
      "Port of the OpenBSD bcrypt_pbkdf function to pure JS")
    (description
      "Port of the OpenBSD bcrypt_pbkdf function to pure JS")
    (license license:bsd-3)))

(define-public node-sshpk-1.16.1
  (package
    (name "node-sshpk")
    (version "1.16.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/sshpk/-/sshpk-1.16.1.tgz")
        (sha256
          (base32
            "0f885dfxv4nhpgsin60z0iflnbr9wfax9lwbcv4i9j3s7shxsjjw"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-bcrypt-pbkdf" ,node-bcrypt-pbkdf-1.0.2)
        ("node-ecc-jsbn" ,node-ecc-jsbn-0.1.2)
        ("node-tweetnacl" ,node-tweetnacl-0.14.5)
        ("node-jsbn" ,node-jsbn-0.1.1)
        ("node-safer-buffer" ,node-safer-buffer-2.1.2)
        ("node-getpass" ,node-getpass-0.1.7)
        ("node-dashdash" ,node-dashdash-1.14.1)
        ("node-assert-plus" ,node-assert-plus-1.0.0)
        ("node-asn1" ,node-asn1-0.2.4)))
    (home-page
      "https://github.com/arekinath/node-sshpk#readme")
    (synopsis
      "A library for finding and using SSH public keys")
    (description
      "A library for finding and using SSH public keys")
    (license license:expat)))

(define-public node-http-signature-1.2.0
  (package
    (name "node-http-signature")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/http-signature/-/http-signature-1.2.0.tgz")
        (sha256
          (base32
            "1y856b84kxhq6wc9yiqcfhd4187nizr7lhxi9z69mwzavmpnvgk6"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-sshpk" ,node-sshpk-1.16.1)
        ("node-jsprim" ,node-jsprim-1.4.1)
        ("node-assert-plus" ,node-assert-plus-1.0.0)))
    (home-page
      "https://github.com/joyent/node-http-signature/")
    (synopsis
      "Reference implementation of Joyent's HTTP Signature scheme.")
    (description
      "Reference implementation of Joyent's HTTP Signature scheme.")
    (license license:expat)))

(define-public node-is-typedarray-1.0.0
  (package
    (name "node-is-typedarray")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-typedarray/-/is-typedarray-1.0.0.tgz")
        (sha256
          (base32
            "0i9qr2b79d0chhvpd1fc5pcp9bvirpg37f99d40alciqffmrfp0d"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/hughsk/is-typedarray")
    (synopsis
      "Detect whether or not an object is a Typed Array")
    (description
      "Detect whether or not an object is a Typed Array")
    (license license:expat)))

(define-public node-isstream-0.1.2
  (package
    (name "node-isstream")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/isstream/-/isstream-0.1.2.tgz")
        (sha256
          (base32
            "0i0br6synccpj2ian2z5fnnna99qq4w73dbp46vnyi53l9w47bkr"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://github.com/rvagg/isstream")
    (synopsis "Determine if an object is a Stream")
    (description
      "Determine if an object is a Stream")
    (license license:expat)))

(define-public node-json-stringify-safe-5.0.1
  (package
    (name "node-json-stringify-safe")
    (version "5.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/json-stringify-safe/-/json-stringify-safe-5.0.1.tgz")
        (sha256
          (base32
            "12ljc7ipy7cprz5zxzzds20ykw6z5616763ca5zx9xmzq1jvzyxp"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/isaacs/json-stringify-safe")
    (synopsis
      "Like JSON.stringify, but doesn't blow up on circular refs.")
    (description
      "Like JSON.stringify, but doesn't blow up on circular refs.")
    (license license:isc)))

(define-public node-mime-db-1.45.0
  (package
    (name "node-mime-db")
    (version "1.45.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/mime-db/-/mime-db-1.45.0.tgz")
        (sha256
          (base32
            "0bva1q84z8cjvh1nibqz4155ylyqan4yqx9a99mriz11swngdn23"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jshttp/mime-db#readme")
    (synopsis "Media Type Database")
    (description "Media Type Database")
    (license license:expat)))

(define-public node-oauth-sign-0.9.0
  (package
    (name "node-oauth-sign")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/oauth-sign/-/oauth-sign-0.9.0.tgz")
        (sha256
          (base32
            "1g6rl2pv86pxcx4mv25qqv0w265mc5ardp3vxd2hqg80c4bsy5h0"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/mikeal/oauth-sign#readme")
    (synopsis
      "OAuth 1 signing. Formerly a vendor lib in mikeal/request, now a standalone module.")
    (description
      "OAuth 1 signing. Formerly a vendor lib in mikeal/request, now a standalone module.")
    (license license:asl2.0)))

(define-public node-performance-now-2.1.0
  (package
    (name "node-performance-now")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/performance-now/-/performance-now-2.1.0.tgz")
        (sha256
          (base32
            "0ich517fgk1nhmcjs2mfv4dp70ppqvj3xgmv3syl25zixzfrk3q6"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/braveg1rl/performance-now")
    (synopsis
      "Implements performance.now (based on process.hrtime).")
    (description
      "Implements performance.now (based on process.hrtime).")
    (license license:expat)))

(define-public node-qs-6.5.2
  (package
    (name "node-qs")
    (version "6.5.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/qs/-/qs-6.5.2.tgz")
        (sha256
          (base32
            "1w0n5rg0w76b97ds80svkhmcqzcn76c3g5z81sblvii89ww4k4sk"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://github.com/ljharb/qs")
    (synopsis
      "A querystring parser that supports nesting and arrays, with a depth limit")
    (description
      "A querystring parser that supports nesting and arrays, with a depth limit")
    (license license:bsd-3)))

(define-public node-safe-buffer-5.2.1
  (package
    (name "node-safe-buffer")
    (version "5.2.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/safe-buffer/-/safe-buffer-5.2.1.tgz")
        (sha256
          (base32
            "1s5kvjpwqsc682zcy71h9c6pxla21sysfwj270x6jjkca421h62x"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/feross/safe-buffer")
    (synopsis "Safer Node.js Buffer API")
    (description "Safer Node.js Buffer API")
    (license license:expat)))

(define-public node-psl-1.8.0
  (package
    (name "node-psl")
    (version "1.8.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/psl/-/psl-1.8.0.tgz")
        (sha256
          (base32
            "03jj0mly8g6hrjaj8h77q0w02z3awgvy6ld051ph2k38fji3zdgb"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/lupomontero/psl#readme")
    (synopsis
      "Domain name parser based on the Public Suffix List")
    (description
      "Domain name parser based on the Public Suffix List")
    (license license:expat)))

(define-public node-tough-cookie-2.5.0
  (package
    (name "node-tough-cookie")
    (version "2.5.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/tough-cookie/-/tough-cookie-2.5.0.tgz")
        (sha256
          (base32
            "0knsdm6l5mn88rh78hajzr2rrydal6wf97l2pbpqjq8ws4w8gazh"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-punycode" ,node-punycode-2.1.1)
        ("node-psl" ,node-psl-1.8.0)))
    (home-page
      "https://github.com/salesforce/tough-cookie")
    (synopsis
      "RFC6265 Cookies and Cookie Jar for node.js")
    (description
      "RFC6265 Cookies and Cookie Jar for node.js")
    (license license:bsd-3)))

(define-public node-tunnel-agent-0.6.0
  (package
    (name "node-tunnel-agent")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/tunnel-agent/-/tunnel-agent-0.6.0.tgz")
        (sha256
          (base32
            "04jhbjld99zavh1rvik2bayrgxwj2zx69xsbcm0gmlnna15c1qyk"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-safe-buffer" ,node-safe-buffer-5.2.1)))
    (home-page
      "https://github.com/mikeal/tunnel-agent#readme")
    (synopsis
      "HTTP proxy tunneling agent. Formerly part of mikeal/request, now a standalone module.")
    (description
      "HTTP proxy tunneling agent. Formerly part of mikeal/request, now a standalone module.")
    (license license:asl2.0)))

(define-public node-uuid-3.4.0
  (package
    (name "node-uuid")
    (version "3.4.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/uuid/-/uuid-3.4.0.tgz")
        (sha256
          (base32
            "1pgldpxvxyy2a9h437v0mflqxyc4w91b37iya2pcfd5wdlqcjxxs"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/uuidjs/uuid#readme")
    (synopsis "RFC4122 (v1, v4, and v5) UUIDs")
    (description "RFC4122 (v1, v4, and v5) UUIDs")
    (license license:expat)))

(define-public node-request-2.88.2
  (package
    (name "node-request")
    (version "2.88.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/request/-/request-2.88.2.tgz")
        (sha256
          (base32
            "0hj2f9qqn3hpzpvhsnbwhzjyn5f8aicjz5wn00q0mfc4824awvg8"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-uuid" ,node-uuid-3.4.0)
        ("node-tunnel-agent" ,node-tunnel-agent-0.6.0)
        ("node-tough-cookie" ,node-tough-cookie-2.5.0)
        ("node-safe-buffer" ,node-safe-buffer-5.2.1)
        ("node-qs" ,node-qs-6.5.2)
        ("node-performance-now"
         ,node-performance-now-2.1.0)
        ("node-oauth-sign" ,node-oauth-sign-0.9.0)
        ("node-mime-types" ,node-mime-types-2.1.28)
        ("node-json-stringify-safe"
         ,node-json-stringify-safe-5.0.1)
        ("node-isstream" ,node-isstream-0.1.2)
        ("node-is-typedarray" ,node-is-typedarray-1.0.0)
        ("node-http-signature"
         ,node-http-signature-1.2.0)
        ("node-har-validator" ,node-har-validator-5.1.5)
        ("node-form-data" ,node-form-data-2.3.3)
        ("node-forever-agent" ,node-forever-agent-0.6.1)
        ("node-extend" ,node-extend-3.0.2)
        ("node-combined-stream"
         ,node-combined-stream-1.0.8)
        ("node-caseless" ,node-caseless-0.12.0)
        ("node-aws4" ,node-aws4-1.11.0)
        ("node-aws-sign2" ,node-aws-sign2-0.7.0)))
    (home-page
      "https://github.com/request/request#readme")
    (synopsis "Simplified HTTP request client.")
    (description "Simplified HTTP request client.")
    (license license:asl2.0)))

(define-public node-shorthash-0.0.2
  (package
    (name "node-shorthash")
    (version "0.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/shorthash/-/shorthash-0.0.2.tgz")
        (sha256
          (base32
            "1lb884bdh1m6b7vyb3afm70vacip84i9ql4gdcl5f1wiqd5fpnfz"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page #f)
    (synopsis
      "Node.js module to get a short unique hash of a string")
    (description
      "Node.js module to get a short unique hash of a string")
    (license license:expat)))

(define-public node-source-map-0.7.3
  (package
    (name "node-source-map")
    (version "0.7.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/source-map/-/source-map-0.7.3.tgz")
        (sha256
          (base32
            "0gl32nnsbimw7m9m7m2jc93gmcgp7m021szhwk1k6ivppa3vfsdd"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/mozilla/source-map")
    (synopsis "Generates and consumes source maps")
    (description
      "Generates and consumes source maps")
    (license license:bsd-3)))

(define-public node-process-nextick-args-2.0.1
  (package
    (name "node-process-nextick-args")
    (version "2.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/process-nextick-args/-/process-nextick-args-2.0.1.tgz")
        (sha256
          (base32
            "16w8m2ycy5s4ykgdfg97qxa67gfvkh6x3vdwfsncafyj4p3zhns2"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/calvinmetcalf/process-nextick-args")
    (synopsis
      "process.nextTick but always with args")
    (description
      "process.nextTick but always with args")
    (license license:expat)))

(define-public node-string-decoder-1.1.1
  (package
    (name "node-string-decoder")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/string_decoder/-/string_decoder-1.1.1.tgz")
        (sha256
          (base32
            "0fln2r91b8gj845j7jl76fvsp7nij13fyzvz82985yh88m1n50mg"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-safe-buffer" ,node-safe-buffer-5.1.2)))
    (home-page
      "https://github.com/nodejs/string_decoder")
    (synopsis
      "The string_decoder module from Node core")
    (description
      "The string_decoder module from Node core")
    (license license:expat)))

(define-public node-util-deprecate-1.0.2
  (package
    (name "node-util-deprecate")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/util-deprecate/-/util-deprecate-1.0.2.tgz")
        (sha256
          (base32
            "1rd3qbgdrwkmcrf7vqx61sh7icma7jvxcmklqj032f8v7jcdx8br"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/TooTallNate/util-deprecate")
    (synopsis
      "The Node.js `util.deprecate()` function with browser support")
    (description
      "The Node.js `util.deprecate()` function with browser support")
    (license license:expat)))

(define-public node-readable-stream-2.3.7
  (package
    (name "node-readable-stream")
    (version "2.3.7")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/readable-stream/-/readable-stream-2.3.7.tgz")
        (sha256
          (base32
            "1ivp6i6kf0li6ak443prb3h8bjsznymjaaclmmmy5p55gb7px809"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-util-deprecate"
         ,node-util-deprecate-1.0.2)
        ("node-string-decoder"
         ,node-string-decoder-1.1.1)
        ("node-safe-buffer" ,node-safe-buffer-5.1.2)
        ("node-process-nextick-args"
         ,node-process-nextick-args-2.0.1)
        ("node-isarray" ,node-isarray-1.0.0)
        ("node-inherits" ,node-inherits-2.0.4)
        ("node-core-util-is" ,node-core-util-is-1.0.2)))
    (home-page
      "https://github.com/nodejs/readable-stream#readme")
    (synopsis
      "Streams3, a user-land copy of the stream library from Node.js")
    (description
      "Streams3, a user-land copy of the stream library from Node.js")
    (license license:expat)))

(define-public node-stream-browserify-2.0.2
  (package
    (name "node-stream-browserify")
    (version "2.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/stream-browserify/-/stream-browserify-2.0.2.tgz")
        (sha256
          (base32
            "0rqljbnx9mhx3cwm8pfl5vvvm0v8igcfk5r3c2lgg4gnxrb755qv"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-readable-stream"
         ,node-readable-stream-2.3.7)
        ("node-inherits" ,node-inherits-2.0.4)))
    (home-page
      "https://github.com/browserify/stream-browserify")
    (synopsis
      "the stream module from node core for browsers")
    (description
      "the stream module from node core for browsers")
    (license license:expat)))

(define-public node-sourcemap-blender-1.0.5
  (package
    (name "node-sourcemap-blender")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/sourcemap-blender/-/sourcemap-blender-1.0.5.tgz")
        (sha256
          (base32
            "1n80zmkdnw5gn8d51x8av4wd0dby18vp2xzjzqg5x29dlw63kpy1"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-source-map" ,node-source-map-0.7.3)))
    (home-page #f)
    (synopsis "")
    (description "")
    (license license:isc)))

(define-public node-tslib-1.14.1
  (package
    (name "node-tslib")
    (version "1.14.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/tslib/-/tslib-1.14.1.tgz")
        (sha256
          (base32
            "1ywidp09jjarr031l03jg042zsfys2y2idwdcvmqhr7g54p32fvk"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://www.typescriptlang.org/")
    (synopsis
      "Runtime library for TypeScript helper functions")
    (description
      "Runtime library for TypeScript helper functions")
    (license #f)))

(define-public node-merge-1.2.1
  (package
    (name "node-merge")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/merge/-/merge-1.2.1.tgz")
        (sha256
          (base32
            "1jkairlra9950qzmfiglyagxahccd9an27i2gjlkp6say0s04m5x"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://github.com/yeikos/js.merge")
    (synopsis
      "Merge multiple objects into one, optionally creating a new cloned object. Similar to the jQuery.extend but more flexible. Works in Node.js and the browser.")
    (description
      "Merge multiple objects into one, optionally creating a new cloned object. Similar to the jQuery.extend but more flexible. Works in Node.js and the browser.")
    (license license:expat)))

(define-public node-exec-sh-0.2.2
  (package
    (name "node-exec-sh")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/exec-sh/-/exec-sh-0.2.2.tgz")
        (sha256
          (base32
            "1493miwgm1g0f76xzgn2bxg1zaqliih2d4lmfpzxwk4xaq6d2aiq"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-merge" ,node-merge-1.2.1)))
    (home-page
      "https://github.com/tsertkov/exec-sh#readme")
    (synopsis
      "Execute shell command forwarding all stdio.")
    (description
      "Execute shell command forwarding all stdio.")
    (license #f)))

(define-public node-watch-1.0.2
  (package
    (name "node-watch")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/watch/-/watch-1.0.2.tgz")
        (sha256
          (base32
            "1kzs1vvv0r1bns3bacj2rc8k829rv60hqvd7nvdr30vsgf54irw5"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-minimist" ,node-minimist-1.2.5)
        ("node-exec-sh" ,node-exec-sh-0.2.2)))
    (home-page "https://github.com/mikeal/watch")
    (synopsis "Utilities for watching file trees.")
    (description
      "Utilities for watching file trees.")
    (license license:asl2.0)))

(define-public node-options-0.0.6
  (package
    (name "node-options")
    (version "0.0.6")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/options/-/options-0.0.6.tgz")
        (sha256
          (base32
            "1wn9bq0lbkn0md7ma7cccjfdw7z9ia8smls8ab0475rpnxqh08dy"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/einaros/options.js")
    (synopsis
      "A very light-weight in-code option parsers for node.js.")
    (description
      "A very light-weight in-code option parsers for node.js.")
    (license #f)))

(define-public node-ultron-1.0.2
  (package
    (name "node-ultron")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/ultron/-/ultron-1.0.2.tgz")
        (sha256
          (base32
            "056l6xqq23iijac63myrqinwhldixzsvmyi4lkfnawd0w4fagl67"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://github.com/unshiftio/ultron")
    (synopsis
      "Ultron is high-intelligence robot. It gathers intel so it can start improving upon his rudimentary design")
    (description
      "Ultron is high-intelligence robot. It gathers intel so it can start improving upon his rudimentary design")
    (license license:expat)))

(define-public node-ws-1.1.5
  (package
    (name "node-ws")
    (version "1.1.5")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/ws/-/ws-1.1.5.tgz")
        (sha256
          (base32
            "1vh6gflkpl9fga4kzcxjdb7zp81x9687bnihs5fyvp7xc6s2zm51"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-ultron" ,node-ultron-1.0.2)
        ("node-options" ,node-options-0.0.6)))
    (home-page "https://github.com/websockets/ws")
    (synopsis
      "Simple to use, blazing fast and thoroughly tested websocket client and server for Node.js")
    (description
      "Simple to use, blazing fast and thoroughly tested websocket client and server for Node.js")
    (license license:expat)))

(define-public node-typescript-4.1.5
  (package
    (name "node-typescript")
    (version "4.1.5")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/typescript/-/typescript-4.1.5.tgz")
        (sha256
          (base32
            "1d6vwxj0sn020vi8c6x37y0m48diwhy34rmsf33zglnjfkwj34mk"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://www.typescriptlang.org/")
    (synopsis
      "TypeScript is a language for application scale JavaScript development")
    (description
      "TypeScript is a language for application scale JavaScript development")
    (license license:asl2.0)))

(define-public node-fuse-box-3.7.1
  (package
    (name "node-fuse-box")
    (version "3.7.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/fuse-box/-/fuse-box-3.7.1.tgz")
        (sha256
          (base32
            "0rn9i8a9rxbhrzaf8w6fg2s8kjgps45vl58f2q9ih8bvag0ixj7h"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-ws" ,node-ws-1.1.5)
        ("node-watch" ,node-watch-1.0.2)
        ("node-tslib" ,node-tslib-1.14.1)
        ("node-sourcemap-blender"
         ,node-sourcemap-blender-1.0.5)
        ("node-stream-browserify"
         ,node-stream-browserify-2.0.2)
        ("node-source-map" ,node-source-map-0.7.3)
        ("node-shorthash" ,node-shorthash-0.0.2)
        ("node-request" ,node-request-2.88.2)
        ("node-regexpu-core" ,node-regexpu-core-4.7.1)
        ("node-realm-utils" ,node-realm-utils-1.0.9)
        ("node-prettysize" ,node-prettysize-0.0.3)
        ("node-pretty-time" ,node-pretty-time-0.2.0)
        ("node-postcss" ,node-postcss-6.0.23)
        ("node-mustache" ,node-mustache-2.3.2)
        ("node-lego-api" ,node-lego-api-1.0.8)
        ("node-inquirer" ,node-inquirer-3.3.0)
        ("node-ieee754" ,node-ieee754-1.2.1)
        ("node-glob" ,node-glob-7.1.6)
        ("node-getopts" ,node-getopts-2.2.5)
        ("node-fuse-concat-with-sourcemaps"
         ,node-fuse-concat-with-sourcemaps-1.0.5)
        ("node-fs-extra" ,node-fs-extra-7.0.1)
        ("node-fliplog" ,node-fliplog-0.3.13)
        ("node-express" ,node-express-4.17.1)
        ("node-escodegen" ,node-escodegen-1.14.3)
        ("node-clean-css" ,node-clean-css-4.2.3)
        ("node-chokidar" ,node-chokidar-1.7.0)
        ("node-bowser" ,node-bowser-2.11.0)
        ("node-base64-js" ,node-base64-js-1.5.1)
        ("node-base64-img" ,node-base64-img-1.0.4)
        ("node-app-root-path" ,node-app-root-path-2.2.1)
        ("node-ansi" ,node-ansi-0.3.1)
        ("node-acorn-jsx" ,node-acorn-jsx-4.1.1)
        ("node-acorn" ,node-acorn-5.7.4)))
	;; XXX: why?
	(propagated-inputs
	  `(("node-typescript" ,node-typescript-4.1.5)
		("node-terser" ,node-terser-4.8.0)
		; Using terser instead.
        ;("node-uglify-js" ,node-uglify-js-3.12.7)
		;("node-uglify-es" ,node-uglify-es-3.3.10)
		))
    (home-page
      "https://github.com/fuse-box/fuse-box#readme")
    (synopsis
      "Fuse-Box a bundler that does it right")
    (description
      "Fuse-Box a bundler that does it right")
    (license license:expat)))

;;; prosemirror-dev-tools

(define-public node-emotion-unitless-0.6.7
  (package
    (name "node-emotion-unitless")
    (version "0.6.7")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/@emotion/unitless/-/unitless-0.6.7.tgz")
        (sha256
          (base32
            "07jprfn958gmsnphyblvn81lmk3g9r0jqp612km87n0397kb56lw"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page #f)
    (synopsis
      "An object of css properties that don't accept values with units")
    (description
      "An object of css properties that don't accept values with units")
    (license license:expat)))

(define-public node-csstype-2.6.14
  (package
    (name "node-csstype")
    (version "2.6.14")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/csstype/-/csstype-2.6.14.tgz")
        (sha256
          (base32
            "1a4bf82zl4lbs696vh59k1jmwqylajbjxc1ln5jkg094fph5mwy6"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/frenic/csstype#readme")
    (synopsis
      "Strict TypeScript and Flow types for style based on MDN data")
    (description
      "Strict TypeScript and Flow types for style based on MDN data")
    (license license:expat)))

(define-public node-stylis-3.5.4
  (package
    (name "node-stylis")
    (version "3.5.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/stylis/-/stylis-3.5.4.tgz")
        (sha256
          (base32
            "0l79wch35jcrkrd2069qgmnrhrly06asrps540lda2zmgqkqwdwf"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/thysultan/stylis.js")
    (synopsis "light - weight css preprocessor")
    (description "light - weight css preprocessor")
    (license license:expat)))

(define-public node-stylis-rule-sheet-0.0.10
  (package
    (name "node-stylis-rule-sheet")
    (version "0.0.10")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/stylis-rule-sheet/-/stylis-rule-sheet-0.0.10.tgz")
        (sha256
          (base32
            "09ipsi2bzzh7xl9pg2j7gh1dqbly93yf65ifix9adr918kw6f3dx"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/thysultan/stylis.js")
    (synopsis
      "stylis plugin to extract individual rules to use with insertRule API")
    (description
      "stylis plugin to extract individual rules to use with insertRule API")
    (license license:expat)))

(define-public node-create-emotion-9.2.12
  (package
    (name "node-create-emotion")
    (version "9.2.12")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/create-emotion/-/create-emotion-9.2.12.tgz")
        (sha256
          (base32
            "0a4bsws1wv8kisjcbvv1j6zg0r0sgrb92r7gjs7h9h7yk46fdf2w"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-stylis-rule-sheet"
         ,node-stylis-rule-sheet-0.0.10)
        ("node-stylis" ,node-stylis-3.5.4)
        ("node-csstype" ,node-csstype-2.6.14)
        ("node-emotion-unitless"
         ,node-emotion-unitless-0.6.7)
        ("node-emotion-stylis"
         ,node-emotion-stylis-0.7.1)
        ("node-emotion-memoize"
         ,node-emotion-memoize-0.6.6)
        ("node-emotion-hash" ,node-emotion-hash-0.6.6)))
    (home-page "https://emotion.sh")
    (synopsis "The Next Generation of CSS-in-JS.")
    (description "The Next Generation of CSS-in-JS.")
    (license license:expat)))

(define-public node-emotion-9.2.12
  (package
    (name "node-emotion")
    (version "9.2.12")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/emotion/-/emotion-9.2.12.tgz")
        (sha256
          (base32
            "0ps3zzpnaid2mg1bwcw9dpr2vxamnmgrgal1ia3jl2g841h6adf5"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-create-emotion"
         ,node-create-emotion-9.2.12)
        ("node-babel-plugin-emotion"
         ,node-babel-plugin-emotion-9.2.11)))
    (home-page "https://emotion.sh")
    (synopsis "The Next Generation of CSS-in-JS.")
    (description "The Next Generation of CSS-in-JS.")
    (license license:expat)))

(define-public node-es6-object-assign-1.1.0
  (package
    (name "node-es6-object-assign")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/es6-object-assign/-/es6-object-assign-1.1.0.tgz")
        (sha256
          (base32
            "0qqnv6adfh52zd49jay2kiav4pjwn8v9b8g6khhpybp6sxq6m4kv"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/rubennorte/es6-object-assign")
    (synopsis
      "ECMAScript 2015 (ES6) Object.assign polyfill and ponyfill")
    (description
      "ECMAScript 2015 (ES6) Object.assign polyfill and ponyfill")
    (license license:expat)))

(define-public node-buffer-from-1.1.1
  (package
    (name "node-buffer-from")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/buffer-from/-/buffer-from-1.1.1.tgz")
        (sha256
          (base32
            "17drzww1pyrh2m1c46i02090gvwhriq00qqy66yh3bcjwz0mh3hr"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/LinusU/buffer-from#readme")
    (synopsis
      "A [ponyfill](https://ponyfill.com) for `Buffer.from`, uses native implementation if available.")
    (description
      "A [ponyfill](https://ponyfill.com) for `Buffer.from`, uses native implementation if available.")
    (license license:expat)))

(define-public node-typedarray-0.0.6
  (package
    (name "node-typedarray")
    (version "0.0.6")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/typedarray/-/typedarray-0.0.6.tgz")
        (sha256
          (base32
            "022101ap05mryhpyw33phwyamk1i139qqpn2rs2lq72qm5slnciz"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/substack/typedarray")
    (synopsis "TypedArray polyfill for old browsers")
    (description
      "TypedArray polyfill for old browsers")
    (license license:expat)))

(define-public node-concat-stream-1.6.2
  (package
    (name "node-concat-stream")
    (version "1.6.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/concat-stream/-/concat-stream-1.6.2.tgz")
        (sha256
          (base32
            "1wa3gka91z4mdwi9yz2lri8lb2b1vhimkr6zckcjdj4bjxcw2iya"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-typedarray" ,node-typedarray-0.0.6)
        ("node-readable-stream"
         ,node-readable-stream-2.3.7)
        ("node-inherits" ,node-inherits-2.0.4)
        ("node-buffer-from" ,node-buffer-from-1.1.1)))
    (home-page
      "https://github.com/maxogden/concat-stream#readme")
    (synopsis
      "writable stream that concatenates strings or binary data and calls a callback with the result")
    (description
      "writable stream that concatenates strings or binary data and calls a callback with the result")
    (license license:expat)))

(define-public node-html-1.0.0
  (package
    (name "node-html")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/html/-/html-1.0.0.tgz")
        (sha256
          (base32
            "1h0zw5lz45aaz17aq27la0pg0lkc6b8l0hvngcgq6vkm67vkij7l"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-concat-stream" ,node-concat-stream-1.6.2)))
    (home-page
      "https://github.com/maxogden/commonjs-html-prettyprinter")
    (synopsis
      "HTML pretty printer CLI utility (based on jsbeautifier)")
    (description
      "HTML pretty printer CLI utility (based on jsbeautifier)")
    (license #f)))

(define-public node-ie-array-find-polyfill-1.1.0
  (package
    (name "node-ie-array-find-polyfill")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/ie-array-find-polyfill/-/ie-array-find-polyfill-1.1.0.tgz")
        (sha256
          (base32
            "0a6hnxnv5s45fkl1y50lz84c2vm54wp4sdj5nxgsynh6jl72hyrk"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/abdalla/ie-array-find-polyfill#readme")
    (synopsis
      "Polyfill to provide array.find on IE.")
    (description
      "Polyfill to provide array.find on IE.")
    (license license:expat)))

(define-public node-diff-match-patch-1.0.5
  (package
    (name "node-diff-match-patch")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/diff-match-patch/-/diff-match-patch-1.0.5.tgz")
        (sha256
          (base32
            "1av5phlznfsvj4yr01hfh83b5fv6izs6810jq7l9q927yr5d8mpa"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/JackuB/diff-match-patch#readme")
    (synopsis
      "npm package for https://github.com/google/diff-match-patch")
    (description
      "npm package for https://github.com/google/diff-match-patch")
    (license license:asl2.0)))

(define-public node-jsondiffpatch-0.3.11
  (package
    (name "node-jsondiffpatch")
    (version "0.3.11")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/jsondiffpatch/-/jsondiffpatch-0.3.11.tgz")
        (sha256
          (base32
            "0zv6a35v83zh2fkw634vsj256xr1qxxk0mh1dy2wq3cq2wm6031p"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-diff-match-patch"
         ,node-diff-match-patch-1.0.5)
        ("node-chalk" ,node-chalk-2.4.2)))
    (home-page
      "https://github.com/benjamine/jsondiffpatch")
    (synopsis "Diff & Patch for Javascript objects")
    (description
      "Diff & Patch for Javascript objects")
    (license license:expat)))

(define-public node-prop-types-15.7.2
  (package
    (name "node-prop-types")
    (version "15.7.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/prop-types/-/prop-types-15.7.2.tgz")
        (sha256
          (base32
            "0xc3kcr9nyhavsbdh0w3p2zgdnyrirqvl5y7gxdrcm7wifg4z47p"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-react-is" ,node-react-is-16.13.1)
        ("node-object-assign" ,node-object-assign-4.1.1)
        ("node-loose-envify" ,node-loose-envify-1.4.0)))
    (home-page "https://facebook.github.io/react/")
    (synopsis
      "Runtime type checking for React props and similar objects.")
    (description
      "Runtime type checking for React props and similar objects.")
    (license license:expat)))

(define-public node-prosemirror-model-1.13.3
  (package
    (name "node-prosemirror-model")
    (version "1.13.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/prosemirror-model/-/prosemirror-model-1.13.3.tgz")
        (sha256
          (base32
            "1wpqkv651727s8iy5fa5j1h19msln1rppxnyhqnyn7wz5xbhwwq9"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-orderedmap" ,node-orderedmap-1.1.1)))
    (home-page
      "https://github.com/prosemirror/prosemirror-model#readme")
    (synopsis "ProseMirror's document model")
    (description "ProseMirror's document model")
    (license license:expat)))

(define-public node-orderedmap-1.1.1
  (package
    (name "node-orderedmap")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/orderedmap/-/orderedmap-1.1.1.tgz")
        (sha256
          (base32
            "13kzghmkqva9dfv8m7q920nwqqpab7lhrpvk3aqrcda4f8ri6lx5"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/marijnh/orderedmap#readme")
    (synopsis
      "Persistent ordered mapping from strings")
    (description
      "Persistent ordered mapping from strings")
    (license license:expat)))

(define-public node-prosemirror-transform-1.2.11
  (package
    (name "node-prosemirror-transform")
    (version "1.2.11")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/prosemirror-transform/-/prosemirror-transform-1.2.11.tgz")
        (sha256
          (base32
            "0s077cl281crfyjrlwjl5r3pwpyy4qacfvj31q2z5f7rskvvjr9j"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-prosemirror-model"
         ,node-prosemirror-model-1.13.3)))
    (home-page
      "https://github.com/prosemirror/prosemirror-transform#readme")
    (synopsis "ProseMirror document transformations")
    (description
      "ProseMirror document transformations")
    (license license:expat)))

(define-public node-prosemirror-state-1.3.4
  (package
    (name "node-prosemirror-state")
    (version "1.3.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/prosemirror-state/-/prosemirror-state-1.3.4.tgz")
        (sha256
          (base32
            "0bxj60x2d71hbv9sjmhxqf16wfp81ipa8zm809cgqcrdylk9dn4h"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-prosemirror-transform"
         ,node-prosemirror-transform-1.2.11)
        ("node-prosemirror-model"
         ,node-prosemirror-model-1.13.3)))
    (home-page
      "https://github.com/prosemirror/prosemirror-state#readme")
    (synopsis "ProseMirror editor state")
    (description "ProseMirror editor state")
    (license license:expat)))

(define-public node-lodash--getnative-3.9.1
  (package
    (name "node-lodash--getnative")
    (version "3.9.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/lodash._getnative/-/lodash._getnative-3.9.1.tgz")
        (sha256
          (base32
            "0q3rny7pjcaxp74p7y95nhq4w1gxz72d32nrcjddlb2h7w8l1wzp"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://lodash.com/")
    (synopsis
      "The modern build of lodashâ\x80\x99s internal `getNative` as a module.")
    (description
      "The modern build of lodashâ\x80\x99s internal `getNative` as a module.")
    (license license:expat)))

(define-public node-lodash-debounce-3.1.1
  (package
    (name "node-lodash-debounce")
    (version "3.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/lodash.debounce/-/lodash.debounce-3.1.1.tgz")
        (sha256
          (base32
            "0rd56z9jzp7yack2g7jb0kcsfdfd27c8da5ghkbbyfs2f3xdl9ki"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-lodash--getnative"
         ,node-lodash--getnative-3.9.1)))
    (home-page "https://lodash.com/")
    (synopsis
      "The modern build of lodashâ\x80\x99s `_.debounce` as a module.")
    (description
      "The modern build of lodashâ\x80\x99s `_.debounce` as a module.")
    (license license:expat)))

(define-public node-react-dock-0.2.4
  (package
    (name "node-react-dock")
    (version "0.2.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/react-dock/-/react-dock-0.2.4.tgz")
        (sha256
          (base32
            "0s8q2j9iy046700rsy2xa7z58pz9v2a6baxr1jwjfrfizq5ba3w6"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-prop-types" ,node-prop-types-15.7.2)
		("node-babel-runtime" ,node-babel-runtime-6.26.0)
		("node-react" ,node-react-17.0.1)
        ("node-lodash-debounce"
         ,node-lodash-debounce-3.1.1)))
    (home-page
      "https://github.com/alexkuz/react-dock")
    (synopsis "Resizable dockable react component")
    (description
      "Resizable dockable react component")
    (license license:expat)))

(define-public node-to-fast-properties-2.0.0
  (package
    (name "node-to-fast-properties")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/to-fast-properties/-/to-fast-properties-2.0.0.tgz")
        (sha256
          (base32
            "10q99rgk8nfl8k7q0aqmik4wkbm8zp4z0rpwbm8b0gr4pi4gw4y7"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/to-fast-properties#readme")
    (synopsis
      "Force V8 to use fast properties for an object")
    (description
      "Force V8 to use fast properties for an object")
    (license license:expat)))

(define-public node-babel-types-7.12.13
  (package
    (name "node-babel-types")
    (version "7.12.13")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/@babel/types/-/types-7.12.13.tgz")
        (sha256
          (base32
            "1maq4h129qpp2h58wz58zd18lrk49j0v45vgx37bmxdfbfpxwhr8"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-to-fast-properties"
         ,node-to-fast-properties-2.0.0)
        ("node-lodash" ,node-lodash-4.17.20)
        ("node-babel-helper-validator-identifier"
         ,node-babel-helper-validator-identifier-7.12.11)))
    (home-page
      "https://babel.dev/docs/en/next/babel-types")
    (synopsis
      "Babel Types is a Lodash-esque utility library for AST nodes")
    (description
      "Babel Types is a Lodash-esque utility library for AST nodes")
    (license license:expat)))

(define-public node-babel-helper-module-imports-7.12.13
  (package
    (name "node-babel-helper-module-imports")
    (version "7.12.13")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/@babel/helper-module-imports/-/helper-module-imports-7.12.13.tgz")
        (sha256
          (base32
            "1j26hw657l98yxzbb60psd9fwxjysq7dgmnq3f6c9yjiiy9kdf38"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-babel-types" ,node-babel-types-7.12.13)))
    (home-page
      "https://babel.dev/docs/en/next/babel-helper-module-imports")
    (synopsis
      "Babel helper functions for inserting module loads")
    (description
      "Babel helper functions for inserting module loads")
    (license license:expat)))

(define-public node-emotion-hash-0.6.6
  (package
    (name "node-emotion-hash")
    (version "0.6.6")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/@emotion/hash/-/hash-0.6.6.tgz")
        (sha256
          (base32
            "1cbb9m6j76xa4nvnk5kg12q18vad9r3kxgl1b4skr9zbsj6f6h5x"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page #f)
    (synopsis "A MurmurHash2 implementation")
    (description "A MurmurHash2 implementation")
    (license license:expat)))

(define-public node-emotion-utils-0.8.2
  (package
    (name "node-emotion-utils")
    (version "0.8.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/@emotion/utils/-/utils-0.8.2.tgz")
        (sha256
          (base32
            "1bpc42kvbmpyc68p519ck9bwddd3a6962wrj67awayqwrifg7pgh"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page #f)
    (synopsis "internal utils for emotion")
    (description "internal utils for emotion")
    (license license:expat)))

(define-public node-emotion-serialize-0.9.1
  (package
    (name "node-emotion-serialize")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/@emotion/serialize/-/serialize-0.9.1.tgz")
        (sha256
          (base32
            "1x60cb0nkfh93db0k0ci26466nnij0hvsx9zff8s1p6jkm2pzdls"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-emotion-utils" ,node-emotion-utils-0.8.2)
        ("node-emotion-unitless"
         ,node-emotion-unitless-0.6.7)
        ("node-emotion-memoize"
         ,node-emotion-memoize-0.6.6)
        ("node-emotion-hash" ,node-emotion-hash-0.6.6)))
    (home-page #f)
    (synopsis "serialization utils for emotion")
    (description "serialization utils for emotion")
    (license license:expat)))

(define-public node-convert-source-map-1.7.0
  (package
    (name "node-convert-source-map")
    (version "1.7.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/convert-source-map/-/convert-source-map-1.7.0.tgz")
        (sha256
          (base32
            "1n834pca0vbvx9vhh26sb18mignqhhkhzc9i5ib22bcaflw1cs9c"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-safe-buffer" ,node-safe-buffer-5.1.2)))
    (home-page
      "https://github.com/thlorenz/convert-source-map")
    (synopsis
      "Converts a source-map from/to  different formats and allows adding/changing properties.")
    (description
      "Converts a source-map from/to  different formats and allows adding/changing properties.")
    (license license:expat)))

(define-public node-emotion-babel-utils-0.6.10
  (package
    (name "node-emotion-babel-utils")
    (version "0.6.10")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/@emotion/babel-utils/-/babel-utils-0.6.10.tgz")
        (sha256
          (base32
            "02s0nks0krdzxwbn8x1k7isl4vbi9y9pyps6kkisskz3pcxry6wf"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-source-map" ,node-source-map-0.7.3)
        ("node-find-root" ,node-find-root-1.1.0)
        ("node-convert-source-map"
         ,node-convert-source-map-1.7.0)
        ("node-emotion-serialize"
         ,node-emotion-serialize-0.9.1)
        ("node-emotion-memoize"
         ,node-emotion-memoize-0.6.6)
        ("node-emotion-hash" ,node-emotion-hash-0.6.6)))
    (home-page #f)
    (synopsis
      "Internal Babel utils used in various macros")
    (description
      "Internal Babel utils used in various macros")
    (license license:expat)))

(define-public node-emotion-memoize-0.6.6
  (package
    (name "node-emotion-memoize")
    (version "0.6.6")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/@emotion/memoize/-/memoize-0.6.6.tgz")
        (sha256
          (base32
            "0fgj8kjn7ihiwbq2p5nbcv5a12zpy0x2smxlgfhk5xqj3ln1zgbn"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page #f)
    (synopsis "emotion's memoize utility")
    (description "emotion's memoize utility")
    (license license:expat)))

(define-public node-emotion-stylis-0.7.1
  (package
    (name "node-emotion-stylis")
    (version "0.7.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/@emotion/stylis/-/stylis-0.7.1.tgz")
        (sha256
          (base32
            "1g038f0pbq4l6hrddicnxxff4z79khwwfn88bxpzgn0mz96frckn"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page #f)
    (synopsis "A custom build of Stylis")
    (description "A custom build of Stylis")
    (license license:expat)))

(define-public node-regenerator-runtime-0.13.7
  (package
    (name "node-regenerator-runtime")
    (version "0.13.7")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/regenerator-runtime/-/regenerator-runtime-0.13.7.tgz")
        (sha256
          (base32
            "0km1sbvr80yhai5dwlhrsw0h7nyqy0d9n5gycj2gvyiaa3v4rkdm"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page #f)
    (synopsis
      "Runtime for Regenerator-compiled generator and async functions.")
    (description
      "Runtime for Regenerator-compiled generator and async functions.")
    (license license:expat)))

(define-public node-babel-runtime-7.12.13
  (package
    (name "node-babel-runtime")
    (version "7.12.13")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/@babel/runtime/-/runtime-7.12.13.tgz")
        (sha256
          (base32
            "089naiq510vahvgg965cl0xgwsmg3m385rnd6cgh1cljpfli289f"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-regenerator-runtime"
         ,node-regenerator-runtime-0.13.7)))
    (home-page
      "https://babel.dev/docs/en/next/babel-runtime")
    (synopsis "babel's modular runtime helpers")
    (description "babel's modular runtime helpers")
    (license license:expat)))

(define-public node-types-parse-json-4.0.0
  (package
    (name "node-types-parse-json")
    (version "4.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/@types/parse-json/-/parse-json-4.0.0.tgz")
        (sha256
          (base32
            "0cbn421abbsbvwp0zm5ykdhnqwdimn02lfbznjg3s6cw8035k7yf"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page #f)
    (synopsis
      "TypeScript definitions for parse-json")
    (description
      "TypeScript definitions for parse-json")
    (license license:expat)))

(define-public node-callsites-3.1.0
  (package
    (name "node-callsites")
    (version "3.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/callsites/-/callsites-3.1.0.tgz")
        (sha256
          (base32
            "17f8wf2bxv2s4k36ld1x4y2rbkh9a8vsmbhwab470vmz6ayl40hp"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/callsites#readme")
    (synopsis
      "Get callsites from the V8 stack trace API")
    (description
      "Get callsites from the V8 stack trace API")
    (license license:expat)))

(define-public node-parent-module-1.0.1
  (package
    (name "node-parent-module")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/parent-module/-/parent-module-1.0.1.tgz")
        (sha256
          (base32
            "01z4k8a3y21gqmpcwyf7gq9v8v4k2y5180f5g7qgswd4gq986zk0"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-callsites" ,node-callsites-3.1.0)))
    (home-page
      "https://github.com/sindresorhus/parent-module#readme")
    (synopsis "Get the path of the parent module")
    (description "Get the path of the parent module")
    (license license:expat)))

(define-public node-resolve-from-4.0.0
  (package
    (name "node-resolve-from")
    (version "4.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/resolve-from/-/resolve-from-4.0.0.tgz")
        (sha256
          (base32
            "1p11030pz8qdm9x2d9q0qi2p329447i2bb7a5j7hbsxxqbs2hhi4"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/resolve-from#readme")
    (synopsis
      "Resolve the path of a module like `require.resolve()` but from a given path")
    (description
      "Resolve the path of a module like `require.resolve()` but from a given path")
    (license license:expat)))

(define-public node-import-fresh-3.3.0
  (package
    (name "node-import-fresh")
    (version "3.3.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/import-fresh/-/import-fresh-3.3.0.tgz")
        (sha256
          (base32
            "1chk0qimpnkrd2bn072ywnlhvy69cjyndgbij59m2b9jf4rxp945"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-resolve-from" ,node-resolve-from-4.0.0)
        ("node-parent-module" ,node-parent-module-1.0.1)))
    (home-page
      "https://github.com/sindresorhus/import-fresh#readme")
    (synopsis
      "Import a module while bypassing the cache")
    (description
      "Import a module while bypassing the cache")
    (license license:expat)))

(define-public node-babel-helper-validator-identifier-7.12.11
  (package
    (name "node-babel-helper-validator-identifier")
    (version "7.12.11")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/@babel/helper-validator-identifier/-/helper-validator-identifier-7.12.11.tgz")
        (sha256
          (base32
            "02pl51v3wf8mkych0gwdfgsdh3icaxswhl89nbwzqw0x05saq2my"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page #f)
    (synopsis "Validate identifier/keywords name")
    (description "Validate identifier/keywords name")
    (license license:expat)))

(define-public node-js-tokens-4.0.0
  (package
    (name "node-js-tokens")
    (version "4.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/js-tokens/-/js-tokens-4.0.0.tgz")
        (sha256
          (base32
            "0lrw3qvcfmxrwwi7p7ng4r17yw32ki7jpnbj2a65ddddv2icg16q"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/lydell/js-tokens#readme")
    (synopsis "A regex that tokenizes JavaScript.")
    (description
      "A regex that tokenizes JavaScript.")
    (license license:expat)))

(define-public node-babel-highlight-7.12.13
  (package
    (name "node-babel-highlight")
    (version "7.12.13")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/@babel/highlight/-/highlight-7.12.13.tgz")
        (sha256
          (base32
            "0bc5x81fcyxjxjmbciwf0g8c45l1dr1h36sl497w63wr3qb3lz0b"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-js-tokens" ,node-js-tokens-4.0.0)
        ("node-chalk" ,node-chalk-2.4.2)
        ("node-babel-helper-validator-identifier"
         ,node-babel-helper-validator-identifier-7.12.11)))
    (home-page
      "https://babel.dev/docs/en/next/babel-highlight")
    (synopsis
      "Syntax highlight JavaScript strings for output in terminals.")
    (description
      "Syntax highlight JavaScript strings for output in terminals.")
    (license license:expat)))

(define-public node-babel-code-frame-7.12.13
  (package
    (name "node-babel-code-frame")
    (version "7.12.13")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/@babel/code-frame/-/code-frame-7.12.13.tgz")
        (sha256
          (base32
            "0vr3q3d4i2iik1nf01yxskjpi2l9kaq759brjhbnc8x6r6hypymm"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-babel-highlight"
         ,node-babel-highlight-7.12.13)))
    (home-page
      "https://babel.dev/docs/en/next/babel-code-frame")
    (synopsis
      "Generate errors that contain a code frame that point to source locations.")
    (description
      "Generate errors that contain a code frame that point to source locations.")
    (license license:expat)))

(define-public node-is-arrayish-0.2.1
  (package
    (name "node-is-arrayish")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-arrayish/-/is-arrayish-0.2.1.tgz")
        (sha256
          (base32
            "13734x7w9924g9pch6ywgz741hs5ir612k3578k9fy247vcib3c4"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/qix-/node-is-arrayish#readme")
    (synopsis
      "Determines if an object can be used as an array")
    (description
      "Determines if an object can be used as an array")
    (license license:expat)))

(define-public node-error-ex-1.3.2
  (package
    (name "node-error-ex")
    (version "1.3.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/error-ex/-/error-ex-1.3.2.tgz")
        (sha256
          (base32
            "12gyrmh6iqpx838bnb5iwcqm2447rnbxx1bvqn76l40fvr1aichs"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-is-arrayish" ,node-is-arrayish-0.2.1)))
    (home-page
      "https://github.com/qix-/node-error-ex#readme")
    (synopsis
      "Easy error subclassing and stack customization")
    (description
      "Easy error subclassing and stack customization")
    (license license:expat)))

(define-public node-json-parse-even-better-errors-2.3.1
  (package
    (name "node-json-parse-even-better-errors")
    (version "2.3.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/json-parse-even-better-errors/-/json-parse-even-better-errors-2.3.1.tgz")
        (sha256
          (base32
            "0s95hppvdm62vq9ax92bl7dpgs28dp97j6xlrka1q25bc797g1ra"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/npm/json-parse-even-better-errors#readme")
    (synopsis
      "JSON.parse with context information on error")
    (description
      "JSON.parse with context information on error")
    (license license:expat)))

(define-public node-lines-and-columns-1.1.6
  (package
    (name "node-lines-and-columns")
    (version "1.1.6")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/lines-and-columns/-/lines-and-columns-1.1.6.tgz")
        (sha256
          (base32
            "09p61rikwf78jz4m8n4vad0m4wpbgzaivrrbr74sxcg13v82a83m"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/eventualbuddha/lines-and-columns#readme")
    (synopsis
      "Maps lines and columns to character offsets and back.")
    (description
      "Maps lines and columns to character offsets and back.")
    (license license:expat)))

(define-public node-parse-json-5.2.0
  (package
    (name "node-parse-json")
    (version "5.2.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/parse-json/-/parse-json-5.2.0.tgz")
        (sha256
          (base32
            "1388gc6zhgygqc68vkmqxafk6r9kxgdqqwbbm96k9hpxw223ddq6"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-lines-and-columns"
         ,node-lines-and-columns-1.1.6)
        ("node-json-parse-even-better-errors"
         ,node-json-parse-even-better-errors-2.3.1)
        ("node-error-ex" ,node-error-ex-1.3.2)
        ("node-babel-code-frame"
         ,node-babel-code-frame-7.12.13)))
    (home-page
      "https://github.com/sindresorhus/parse-json#readme")
    (synopsis "Parse JSON with more helpful errors")
    (description
      "Parse JSON with more helpful errors")
    (license license:expat)))

(define-public node-path-type-4.0.0
  (package
    (name "node-path-type")
    (version "4.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/path-type/-/path-type-4.0.0.tgz")
        (sha256
          (base32
            "15wvcgwg053hr2h11ja5swvdz3vvxciqq5aad0ara9qmzgwfh9f0"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/path-type#readme")
    (synopsis
      "Check if a path is a file, directory, or symlink")
    (description
      "Check if a path is a file, directory, or symlink")
    (license license:expat)))

(define-public node-yaml-1.10.0
  (package
    (name "node-yaml")
    (version "1.10.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/yaml/-/yaml-1.10.0.tgz")
        (sha256
          (base32
            "0s5j2nrscc2aqc03q814zhvdlgqzgyw88w7r47hvika3fm0q5vzg"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://eemeli.org/yaml/")
    (synopsis
      "JavaScript parser and stringifier for YAML")
    (description
      "JavaScript parser and stringifier for YAML")
    (license license:isc)))

(define-public node-cosmiconfig-6.0.0
  (package
    (name "node-cosmiconfig")
    (version "6.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/cosmiconfig/-/cosmiconfig-6.0.0.tgz")
        (sha256
          (base32
            "1qr66zb1wgmx8rxksjcrbpvv8zmpxhrm1wbbykdxm20bmipkg9w1"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-yaml" ,node-yaml-1.10.0)
        ("node-path-type" ,node-path-type-4.0.0)
        ("node-parse-json" ,node-parse-json-5.2.0)
        ("node-import-fresh" ,node-import-fresh-3.3.0)
        ("node-types-parse-json"
         ,node-types-parse-json-4.0.0)))
    (home-page
      "https://github.com/davidtheclark/cosmiconfig#readme")
    (synopsis
      "Find and load configuration from a package.json property, rc file, or CommonJS module")
    (description
      "Find and load configuration from a package.json property, rc file, or CommonJS module")
    (license license:expat)))

(define-public node-function-bind-1.1.1
  (package
    (name "node-function-bind")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/function-bind/-/function-bind-1.1.1.tgz")
        (sha256
          (base32
            "10p0s9ypggwmazik4azdhywjnnayagnjxk10cjzsrhxlk1y2wm9d"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/Raynos/function-bind")
    (synopsis
      "Implementation of Function.prototype.bind")
    (description
      "Implementation of Function.prototype.bind")
    (license license:expat)))

(define-public node-has-1.0.3
  (package
    (name "node-has")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/has/-/has-1.0.3.tgz")
        (sha256
          (base32
            "0wsmn2vcbqb23xpbzxipjd7xcdljid2gwnwl7vn5hkp0zkpgk363"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-function-bind" ,node-function-bind-1.1.1)))
    (home-page "https://github.com/tarruda/has")
    (synopsis
      "Object.prototype.hasOwnProperty.call shortcut")
    (description
      "Object.prototype.hasOwnProperty.call shortcut")
    (license license:expat)))

(define-public node-is-core-module-2.2.0
  (package
    (name "node-is-core-module")
    (version "2.2.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-core-module/-/is-core-module-2.2.0.tgz")
        (sha256
          (base32
            "1p938n56sd0i861pghd7iw4lfmb9b2hn6hfnmkg9a4fxcslskzc5"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-has" ,node-has-1.0.3)))
    (home-page
      "https://github.com/inspect-js/is-core-module")
    (synopsis
      "Is this specifier a node.js core module?")
    (description
      "Is this specifier a node.js core module?")
    (license license:expat)))

(define-public node-path-parse-1.0.6
  (package
    (name "node-path-parse")
    (version "1.0.6")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/path-parse/-/path-parse-1.0.6.tgz")
        (sha256
          (base32
            "07x1wv7r4yky2hgcxl465a39a48hf5746840g9xkzggl3gb35ad4"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jbgutierrez/path-parse#readme")
    (synopsis "Node.js path.parse() ponyfill")
    (description "Node.js path.parse() ponyfill")
    (license license:expat)))

(define-public node-resolve-1.19.0
  (package
    (name "node-resolve")
    (version "1.19.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/resolve/-/resolve-1.19.0.tgz")
        (sha256
          (base32
            "0rpb2fl9gwc9nkgpkncklq7svxbbszk2w06ds6b50idvr4i9pjcc"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-path-parse" ,node-path-parse-1.0.6)
        ("node-is-core-module"
         ,node-is-core-module-2.2.0)))
    (home-page
      "https://github.com/browserify/resolve#readme")
    (synopsis
      "resolve like require.resolve() on behalf of files asynchronously and synchronously")
    (description
      "resolve like require.resolve() on behalf of files asynchronously and synchronously")
    (license license:expat)))

(define-public node-babel-plugin-macros-2.8.0
  (package
    (name "node-babel-plugin-macros")
    (version "2.8.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/babel-plugin-macros/-/babel-plugin-macros-2.8.0.tgz")
        (sha256
          (base32
            "03ymharkmanc9xclccgh17v7m7i2366jcraqwqzb7gc5bvpmrrb2"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-resolve" ,node-resolve-1.19.0)
        ("node-cosmiconfig" ,node-cosmiconfig-6.0.0)
        ("node-babel-runtime"
         ,node-babel-runtime-7.12.13)))
    (home-page
      "https://github.com/kentcdodds/babel-plugin-macros#readme")
    (synopsis
      "Allows you to build compile-time libraries")
    (description
      "Allows you to build compile-time libraries")
    (license license:expat)))

(define-public node-babel-plugin-syntax-jsx-6.18.0
  (package
    (name "node-babel-plugin-syntax-jsx")
    (version "6.18.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/babel-plugin-syntax-jsx/-/babel-plugin-syntax-jsx-6.18.0.tgz")
        (sha256
          (base32
            "0nlwixy37wj48m275k0w66cq8cpdnwxa5fnv4hav83nvamqz6a1g"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page #f)
    (synopsis "Allow parsing of jsx")
    (description "Allow parsing of jsx")
    (license license:expat)))

(define-public node-find-root-1.1.0
  (package
    (name "node-find-root")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/find-root/-/find-root-1.1.0.tgz")
        (sha256
          (base32
            "1m0lf8903ffmlzxhy8nss7lpkyjyhf8samyy0himcpjxknpkrrq6"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/js-n/find-root#readme")
    (synopsis "find the closest package.json")
    (description "find the closest package.json")
    (license license:expat)))

(define-public node-abbrev-1.1.1
  (package
    (name "node-abbrev")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/abbrev/-/abbrev-1.1.1.tgz")
        (sha256
          (base32
            "0vdsff38rgn0qylyj6x42n13bnxfqxb9ql34bzs4z9grlli9vh8c"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/isaacs/abbrev-js#readme")
    (synopsis "Like ruby's abbrev module, but in js")
    (description
      "Like ruby's abbrev module, but in js")
    (license license:isc)))

(define-public node-nopt-1.0.10
  (package
    (name "node-nopt")
    (version "1.0.10")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/nopt/-/nopt-1.0.10.tgz")
        (sha256
          (base32
            "09s96giczznyly80l061wl0nr35pwnnpadf8x9cw1gzv7fa64ra2"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-abbrev" ,node-abbrev-1.1.1)))
    (home-page #f)
    (synopsis
      "Option parsing for Node, supporting types, shorthands, etc. Used by npm.")
    (description
      "Option parsing for Node, supporting types, shorthands, etc. Used by npm.")
    (license #f)))

(define-public node-touch-2.0.2
  (package
    (name "node-touch")
    (version "2.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/touch/-/touch-2.0.2.tgz")
        (sha256
          (base32
            "1cf0q0cpgqchs6qhpnhzicvpsf1miamz3h8pfg7wxkav0h5qrsws"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-nopt" ,node-nopt-1.0.10)))
    (home-page
      "https://github.com/isaacs/node-touch#readme")
    (synopsis "like touch(1) in node")
    (description "like touch(1) in node")
    (license license:isc)))

(define-public node-babel-plugin-emotion-9.2.11
  (package
    (name "node-babel-plugin-emotion")
    (version "9.2.11")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/babel-plugin-emotion/-/babel-plugin-emotion-9.2.11.tgz")
        (sha256
          (base32
            "160acyl7sxcg285y63cwlji8fqa11bra8wbdlhxcqx42ikpcsj74"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-touch" ,node-touch-2.0.2)
        ("node-source-map" ,node-source-map-0.5.7)
        ("node-mkdirp" ,node-mkdirp-0.5.5)
        ("node-find-root" ,node-find-root-1.1.0)
        ("node-convert-source-map"
         ,node-convert-source-map-1.7.0)
        ("node-babel-plugin-syntax-jsx"
         ,node-babel-plugin-syntax-jsx-6.18.0)
        ("node-babel-plugin-macros"
         ,node-babel-plugin-macros-2.8.0)
        ("node-emotion-stylis"
         ,node-emotion-stylis-0.7.1)
        ("node-emotion-memoize"
         ,node-emotion-memoize-0.6.6)
        ("node-emotion-hash" ,node-emotion-hash-0.6.6)
        ("node-emotion-babel-utils"
         ,node-emotion-babel-utils-0.6.10)
        ("node-babel-helper-module-imports"
         ,node-babel-helper-module-imports-7.12.13)))
    (home-page "https://emotion.sh")
    (synopsis
      "A recommended babel preprocessing plugin for emotion, The Next Generation of CSS-in-JS.")
    (description
      "A recommended babel preprocessing plugin for emotion, The Next Generation of CSS-in-JS.")
    (license license:expat)))

(define-public node-emotion-is-prop-valid-0.6.8
  (package
    (name "node-emotion-is-prop-valid")
    (version "0.6.8")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/@emotion/is-prop-valid/-/is-prop-valid-0.6.8.tgz")
        (sha256
          (base32
            "1yw9j5jg06wp15qd61d0crm7h8icj5h7bjhaig54f1jdhyxjw239"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-emotion-memoize"
         ,node-emotion-memoize-0.6.6)))
    (home-page #f)
    (synopsis
      "A function to check whether a prop is valid for HTML and SVG elements")
    (description
      "A function to check whether a prop is valid for HTML and SVG elements")
    (license license:expat)))

(define-public node-create-emotion-styled-9.2.8
  (package
    (name "node-create-emotion-styled")
    (version "9.2.8")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/create-emotion-styled/-/create-emotion-styled-9.2.8.tgz")
        (sha256
          (base32
            "1yjh96lc5alha7vd9aqxbn1jbqrgygbrgmlyy63jycnm2mx41s0k"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-emotion-is-prop-valid"
         ,node-emotion-is-prop-valid-0.6.8)))
    (home-page "https://emotion.sh")
    (synopsis "The styled API for emotion")
    (description "The styled API for emotion")
    (license license:expat)))

(define-public node-react-emotion-9.2.12
  (package
    (name "node-react-emotion")
    (version "9.2.12")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/react-emotion/-/react-emotion-9.2.12.tgz")
        (sha256
          (base32
            "0h8ap7ppsq9zmmwvv2gx52m00il52hf28bzgr0wx7qp1gml4pq6k"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-create-emotion-styled"
         ,node-create-emotion-styled-9.2.8)
		("node-react" ,node-react-16.14.0)
        ("node-babel-plugin-emotion"
         ,node-babel-plugin-emotion-9.2.11)))
    (home-page "https://emotion.sh")
    (synopsis
      "The Next Generation of CSS-in-JS, for React projects.")
    (description
      "The Next Generation of CSS-in-JS, for React projects.")
    (license license:expat)))

(define-public node-core-js-2.6.12
  (package
    (name "node-core-js")
    (version "2.6.12")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/core-js/-/core-js-2.6.12.tgz")
        (sha256
          (base32
            "0idawjihpabdgpq1w460phfls3wrmgkl3idll6h68cy48k2z6bw7"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/zloirock/core-js#readme")
    (synopsis "Standard library")
    (description "Standard library")
    (license license:expat)))

(define-public node-regenerator-runtime-0.11.1
  (package
    (name "node-regenerator-runtime")
    (version "0.11.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/regenerator-runtime/-/regenerator-runtime-0.11.1.tgz")
        (sha256
          (base32
            "0frppih4qlrkvl1xgyd9zpgxs3fp0sarpcywwn7v1dmkhidrin6d"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page #f)
    (synopsis
      "Runtime for Regenerator-compiled generator and async functions.")
    (description
      "Runtime for Regenerator-compiled generator and async functions.")
    (license license:expat)))

(define-public node-babel-runtime-6.26.0
  (package
    (name "node-babel-runtime")
    (version "6.26.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/babel-runtime/-/babel-runtime-6.26.0.tgz")
        (sha256
          (base32
            "0swl6f8bw62qydhkq6qq53c8afxai4cqpd4rg7270jvl8s4lilhl"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-regenerator-runtime"
         ,node-regenerator-runtime-0.11.1)
        ("node-core-js" ,node-core-js-2.6.12)))
    (home-page #f)
    (synopsis "babel selfContained runtime")
    (description "babel selfContained runtime")
    (license license:expat)))

(define-public node-loose-envify-1.4.0
  (package
    (name "node-loose-envify")
    (version "1.4.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/loose-envify/-/loose-envify-1.4.0.tgz")
        (sha256
          (base32
            "1p5b3ca0b2jkxalyg7h9bss6aspa8plkh0ak1mrlz2jkjc58660j"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-js-tokens" ,node-js-tokens-4.0.0)))
    (home-page
      "https://github.com/zertosh/loose-envify")
    (synopsis
      "Fast (and loose) selective `process.env` replacer using js-tokens instead of an AST")
    (description
      "Fast (and loose) selective `process.env` replacer using js-tokens instead of an AST")
    (license license:expat)))

(define-public node-object-assign-4.1.1
  (package
    (name "node-object-assign")
    (version "4.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/object-assign/-/object-assign-4.1.1.tgz")
        (sha256
          (base32
            "1v999sycxcp74j2pikdhyinm2d80p2bsy4nnrrnb59rv4rm74bbq"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/object-assign#readme")
    (synopsis "ES2015 `Object.assign()` ponyfill")
    (description "ES2015 `Object.assign()` ponyfill")
    (license license:expat)))

(define-public node-react-is-16.13.1
  (package
    (name "node-react-is")
    (version "16.13.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/react-is/-/react-is-16.13.1.tgz")
        (sha256
          (base32
            "13zfzbq3z5cmh7fzvbzwm2ivcqg2crv97ww7162kxhfxfgabbnqb"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://reactjs.org/")
    (synopsis "Brand checking of React Elements.")
    (description "Brand checking of React Elements.")
    (license license:expat)))

(define-public node-base16-1.0.0
  (package
    (name "node-base16")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/base16/-/base16-1.0.0.tgz")
        (sha256
          (base32
            "07jqwqxly1ia87yki3kr0cv25jslkasmad55i3yxfkd9w16dxx59"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/gaearon/base16-js")
    (synopsis "Base16 themes as JS objects")
    (description "Base16 themes as JS objects")
    (license license:expat)))

(define-public node-lodash-curry-4.1.1
  (package
    (name "node-lodash-curry")
    (version "4.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/lodash.curry/-/lodash.curry-4.1.1.tgz")
        (sha256
          (base32
            "01yih63d4chjld2w5y32n15x6ql6qpglyj7cr7175acpyz6mkpz1"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://lodash.com/")
    (synopsis
      "The lodash method `_.curry` exported as a module.")
    (description
      "The lodash method `_.curry` exported as a module.")
    (license license:expat)))

(define-public node-lodash-flow-3.5.0
  (package
    (name "node-lodash-flow")
    (version "3.5.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/lodash.flow/-/lodash.flow-3.5.0.tgz")
        (sha256
          (base32
            "1p1h7s6zbs2ca7vh7xwa21qlcd5vz65hpbrhr47m39zv6jn9gqir"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://lodash.com/")
    (synopsis
      "The lodash method `_.flow` exported as a module.")
    (description
      "The lodash method `_.flow` exported as a module.")
    (license license:expat)))

(define-public node-pure-color-1.3.0
  (package
    (name "node-pure-color")
    (version "1.3.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/pure-color/-/pure-color-1.3.0.tgz")
        (sha256
          (base32
            "1z6bacqcxn1pgxzlw0sw3abpixzi05dw3xpvj0fpjh0rqq4hgdqm"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/WickyNilliams/pure-color#readme")
    (synopsis
      "Pure functions for color conversion and parsing")
    (description
      "Pure functions for color conversion and parsing")
    (license license:expat)))

(define-public node-react-base16-styling-0.5.3
  (package
    (name "node-react-base16-styling")
    (version "0.5.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/react-base16-styling/-/react-base16-styling-0.5.3.tgz")
        (sha256
          (base32
            "1w1xiymxqaa7ybyb4il9dg9nqv4bpaal3wj8ydq830501yf1n6nc"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-pure-color" ,node-pure-color-1.3.0)
        ("node-lodash-flow" ,node-lodash-flow-3.5.0)
        ("node-lodash-curry" ,node-lodash-curry-4.1.1)
        ("node-base16" ,node-base16-1.0.0)))
    (home-page
      "https://github.com/alexkuz/react-base16-styling#readme")
    (synopsis
      "React styling with base16 color scheme support")
    (description
      "React styling with base16 color scheme support")
    (license license:expat)))

(define-public node-react-json-tree-0.11.2
  (package
    (name "node-react-json-tree")
    (version "0.11.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/react-json-tree/-/react-json-tree-0.11.2.tgz")
        (sha256
          (base32
            "0bnpdp1cj1gbxp0z664jb2j6api6h6g8g4y8bkvrwvnnyynf7hb1"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-react-base16-styling"
         ,node-react-base16-styling-0.5.3)
        ("node-prop-types" ,node-prop-types-15.7.2)
        ("node-babel-runtime" ,node-babel-runtime-6.26.0)))
    (home-page
      "https://github.com/reduxjs/redux-devtools")
    (synopsis
      "React JSON Viewer Component, Extracted from redux-devtools")
    (description
      "React JSON Viewer Component, Extracted from redux-devtools")
    (license license:expat)))

(define-public node-create-react-context-0.1.6
  (package
    (name "node-create-react-context")
    (version "0.1.6")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/create-react-context/-/create-react-context-0.1.6.tgz")
        (sha256
          (base32
            "05rgnfqb99xkwh4ckk0lilxdhdl2yvhphhkz0f9j31zhaq9jdmd9"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/thejameskyle/create-react-context#readme")
    (synopsis
      "Polyfill for the proposed React context API")
    (description
      "Polyfill for the proposed React context API")
    (license license:expat)))

(define-public node-unstated-2.1.1
  (package
    (name "node-unstated")
    (version "2.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/unstated/-/unstated-2.1.1.tgz")
        (sha256
          (base32
            "1yaphbvwdgzvxp5vpsnk59lvfjznd0m8q9hbvsshg9wmpqpjybvy"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-create-react-context"
         ,node-create-react-context-0.1.6)))
    (home-page
      "https://github.com/thejameskyle/unstated#readme")
    (synopsis
      "State so simple, it goes without saying")
    (description
      "State so simple, it goes without saying")
    (license license:expat)))

(define-public node-prosemirror-dev-tools-2.1.1
  (package
    (name "node-prosemirror-dev-tools")
    (version "2.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/prosemirror-dev-tools/-/prosemirror-dev-tools-2.1.1.tgz")
        (sha256
          (base32
            "0m1cqzcwrmmgr01fpkkx17vz19wdm40w81dhbwl7nr5480c43rpk"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-unstated" ,node-unstated-2.1.1)
        ("node-react-json-tree"
         ,node-react-json-tree-0.11.2)
        ("node-react-emotion" ,node-react-emotion-9.2.12)
        ("node-react-dock" ,node-react-dock-0.2.4)
        ("node-prosemirror-state"
         ,node-prosemirror-state-1.3.4)
        ("node-prosemirror-model"
         ,node-prosemirror-model-1.13.3)
        ("node-prop-types" ,node-prop-types-15.7.2)
        ("node-jsondiffpatch" ,node-jsondiffpatch-0.3.11)
        ("node-ie-array-find-polyfill"
         ,node-ie-array-find-polyfill-1.1.0)
        ("node-html" ,node-html-1.0.0)
        ("node-es6-object-assign"
         ,node-es6-object-assign-1.1.0)
        ("node-emotion" ,node-emotion-9.2.12)))
    (home-page
      "https://github.com/d4rkr00t/prosemirror-dev-tools#readme")
    (synopsis "Dev Tools for ProseMirror")
    (description "Dev Tools for ProseMirror")
    (license license:expat)))

(define-public node-react-17.0.1
  (package
    (name "node-react")
    (version "17.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/react/-/react-17.0.1.tgz")
        (sha256
          (base32
            "05amw2p28mayzfp7h5gakn3i4iw2kqv44pxm8397l7fv98nj5ach"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-object-assign" ,node-object-assign-4.1.1)
        ("node-loose-envify" ,node-loose-envify-1.4.0)))
    (home-page "https://reactjs.org/")
    (synopsis
      "React is a JavaScript library for building user interfaces.")
    (description
      "React is a JavaScript library for building user interfaces.")
    (license license:expat)))

(define-public node-react-16.14.0
  (package
    (name "node-react")
    (version "16.14.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/react/-/react-16.14.0.tgz")
        (sha256
          (base32
            "1qx52jqad4jxavq83x08gypw2jg7cidwm2zpc5gxkjshbqvdqmzy"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-prop-types" ,node-prop-types-15.7.2)
        ("node-object-assign" ,node-object-assign-4.1.1)
        ("node-loose-envify" ,node-loose-envify-1.4.0)))
    (home-page "https://reactjs.org/")
    (synopsis
      "React is a JavaScript library for building user interfaces.")
    (description
      "React is a JavaScript library for building user interfaces.")
    (license license:expat)))

(define-public node-uglify-js-3.12.7
  (package
    (name "node-uglify-js")
    (version "3.12.7")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/uglify-js/-/uglify-js-3.12.7.tgz")
        (sha256
          (base32
            "1gik2hwzw4p1xd1d0iqvslmgk0bzyrjkplyf351n3vfbi9ka56zh"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/mishoo/UglifyJS#readme")
    (synopsis
      "JavaScript parser, mangler/compressor and beautifier toolkit")
    (description
      "JavaScript parser, mangler/compressor and beautifier toolkit")
    (license #f)))

(define-public node-commander-2.14.1
  (package
    (name "node-commander")
    (version "2.14.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/commander/-/commander-2.14.1.tgz")
        (sha256
          (base32
            "1ymikdab62hlh3dcydm4dyry18k306bpplzjp74lxhzipqzz6l98"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/tj/commander.js#readme")
    (synopsis
      "the complete solution for node.js command-line programs")
    (description
      "the complete solution for node.js command-line programs")
    (license license:expat)))

(define-public node-uglify-es-3.3.10
  (package
    (name "node-uglify-es")
    (version "3.3.10")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/uglify-es/-/uglify-es-3.3.10.tgz")
        (sha256
          (base32
            "09isws2iiqzr570gxskdj4s1l987gcl0jivi02f596i7qr4k81rd"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-source-map" ,node-source-map-0.6.1)
        ("node-commander" ,node-commander-2.14.1)))
    (home-page
      "https://github.com/mishoo/UglifyJS2/tree/harmony")
    (synopsis
      "JavaScript parser, mangler/compressor and beautifier toolkit for ES6+")
    (description
      "JavaScript parser, mangler/compressor and beautifier toolkit for ES6+")
    (license #f)))

(define-public node-commander-2.20.3
  (package
    (name "node-commander")
    (version "2.20.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/commander/-/commander-2.20.3.tgz")
        (sha256
          (base32
            "0m9bcmcwgn2zj6hdqydnyx6d1c5y014ysazyqd6qva8pw0yr00iw"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/tj/commander.js#readme")
    (synopsis
      "the complete solution for node.js command-line programs")
    (description
      "the complete solution for node.js command-line programs")
    (license license:expat)))

(define-public node-source-map-support-0.5.19
  (package
    (name "node-source-map-support")
    (version "0.5.19")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/source-map-support/-/source-map-support-0.5.19.tgz")
        (sha256
          (base32
            "116k5r9cfgjs70j7xwf49n5gzh173sjmcmpam1823szma0fac54l"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-source-map" ,node-source-map-0.6.1)
        ("node-buffer-from" ,node-buffer-from-1.1.1)))
    (home-page
      "https://github.com/evanw/node-source-map-support#readme")
    (synopsis
      "Fixes stack traces for files with source maps")
    (description
      "Fixes stack traces for files with source maps")
    (license license:expat)))

(define-public node-terser-4.8.0
  (package
    (name "node-terser")
    (version "4.8.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/terser/-/terser-4.8.0.tgz")
        (sha256
          (base32
            "0wf5labb7fv684ajg2mf2rbj2h1va7f7cz0ks91gll6i7nc0fbgs"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-source-map-support"
         ,node-source-map-support-0.5.19)
        ("node-source-map" ,node-source-map-0.6.1)
        ("node-commander" ,node-commander-2.20.3)))
    (home-page "https://terser.org")
    (synopsis
      "JavaScript parser, mangler/compressor and beautifier toolkit for ES6+")
    (description
      "JavaScript parser, mangler/compressor and beautifier toolkit for ES6+")
    (license #f)))

(define-public node-biblatex-csl-converter-1.9.5
  (package
    (name "node-biblatex-csl-converter")
    (version "1.9.5")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/biblatex-csl-converter/-/biblatex-csl-converter-1.9.5.tgz")
        (sha256
          (base32
            "1i2kwvpylc4l3azw4cjjv9nd92bvji9s5zj37mb5n11dq8x9zcxk"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/fiduswriter/biblatex-csl-converter#readme")
    (synopsis
      "a set of converters: biblatex => json, json => biblatex, json => CSL")
    (description
      "a set of converters: biblatex => json, json => biblatex, json => CSL")
    (license license:lgpl3)))


(define-public node-delegate-3.2.0
  (package
    (name "node-delegate")
    (version "3.2.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/delegate/-/delegate-3.2.0.tgz")
        (sha256
          (base32
            "1ys85812sy02lmz6r4gc5987hr5bnsiagiil8ncggaiaxk76j0vy"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/zenorocha/delegate#readme")
    (synopsis "Lightweight event delegation")
    (description "Lightweight event delegation")
    (license license:expat)))

(define-public node-good-listener-1.2.2
  (package
    (name "node-good-listener")
    (version "1.2.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/good-listener/-/good-listener-1.2.2.tgz")
        (sha256
          (base32
            "0xbzaxs3xvmh1sf3qbvd0slp82inlb0gz5fx60vkj94gn31l8s4x"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-delegate" ,node-delegate-3.2.0)))
    (home-page
      "https://github.com/zenorocha/good-listener#readme")
    (synopsis
      "A more versatile way of adding & removing event listeners")
    (description
      "A more versatile way of adding & removing event listeners")
    (license license:expat)))

(define-public node-select-1.1.2
  (package
    (name "node-select")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/select/-/select-1.1.2.tgz")
        (sha256
          (base32
            "159mk5g9gjv7qvxwk4yp3h0y3s2wawvvh0ms66fr2z79zdzgi6v6"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/zenorocha/select#readme")
    (synopsis
      "Programmatically select the text of a HTML element")
    (description
      "Programmatically select the text of a HTML element")
    (license license:expat)))

(define-public node-tiny-emitter-2.1.0
  (package
    (name "node-tiny-emitter")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/tiny-emitter/-/tiny-emitter-2.1.0.tgz")
        (sha256
          (base32
            "0fki07f4gfncqj8h3zv5rz0cnza0yc0bk6v7bkc6ih0y1jiix32k"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/scottcorgan/tiny-emitter#readme")
    (synopsis
      "A tiny (less than 1k) event emitter library")
    (description
      "A tiny (less than 1k) event emitter library")
    (license license:expat)))

(define-public node-clipboard-2.0.6
  (package
    (name "node-clipboard")
    (version "2.0.6")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/clipboard/-/clipboard-2.0.6.tgz")
        (sha256
          (base32
            "069i7xi31an2x5d6afnm24h9smp1i64wb23hxby1ffv4121m3d0j"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-tiny-emitter" ,node-tiny-emitter-2.1.0)
        ("node-select" ,node-select-1.1.2)
        ("node-good-listener" ,node-good-listener-1.2.2)))
    (home-page
      "https://github.com/zenorocha/clipboard.js#readme")
    (synopsis
      "Modern copy to clipboard. No Flash. Just 2kb")
    (description
      "Modern copy to clipboard. No Flash. Just 2kb")
    (license license:expat)))

(define-public node-fuse-js-6.4.6
  (package
    (name "node-fuse-js")
    (version "6.4.6")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/fuse.js/-/fuse.js-6.4.6.tgz")
        (sha256
          (base32
            "07c5waxc08m017xh87rw5jvsgmsv2qpw99g8z3ffk6bylivjd7c6"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "http://fusejs.io")
    (synopsis "Lightweight fuzzy-search")
    (description "Lightweight fuzzy-search")
    (license license:asl2.0)))

(define-public node-sprintf-js-1.0.3
  (package
    (name "node-sprintf-js")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/sprintf-js/-/sprintf-js-1.0.3.tgz")
        (sha256
          (base32
            "10qsmbfw9hv4hahsvq79py6v0dddhckwynji2vsr1p18qfy2dyrs"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/alexei/sprintf.js#readme")
    (synopsis "JavaScript sprintf implementation")
    (description "JavaScript sprintf implementation")
    (license license:bsd-3)))

(define-public node-argparse-1.0.10
  (package
    (name "node-argparse")
    (version "1.0.10")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/argparse/-/argparse-1.0.10.tgz")
        (sha256
          (base32
            "03dc2n1i08nwyyl1l8pq0a5fckcw9rpdqlw3zh8pw6w1syf19akz"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-sprintf-js" ,node-sprintf-js-1.0.3)))
    (home-page
      "https://github.com/nodeca/argparse#readme")
    (synopsis
      "Very powerful CLI arguments parser. Native port of argparse - python's options parsing library")
    (description
      "Very powerful CLI arguments parser. Native port of argparse - python's options parsing library")
    (license license:expat)))

(define-public node-js-yaml-3.14.1
  (package
    (name "node-js-yaml")
    (version "3.14.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/js-yaml/-/js-yaml-3.14.1.tgz")
        (sha256
          (base32
            "1jmy5pbvh80pc772pa4jhqnl6ax4p3w7npnqqk8z1md575cmynlk"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-esprima" ,node-esprima-4.0.1)
        ("node-argparse" ,node-argparse-1.0.10)))
    (home-page "https://github.com/nodeca/js-yaml")
    (synopsis "YAML 1.2 parser and serializer")
    (description "YAML 1.2 parser and serializer")
    (license license:expat)))

(define-public node-lodash-debounce-4.0.8
  (package
    (name "node-lodash-debounce")
    (version "4.0.8")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/lodash.debounce/-/lodash.debounce-4.0.8.tgz")
        (sha256
          (base32
            "180bk3h6nm2kvnn4f6phvcr0qyg5kj30ixrgflng352ndx63h655"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://lodash.com/")
    (synopsis
      "The lodash method `_.debounce` exported as a module.")
    (description
      "The lodash method `_.debounce` exported as a module.")
    (license license:expat)))

(define-public node-lodash-uniqby-4.7.0
  (package
    (name "node-lodash-uniqby")
    (version "4.7.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/lodash.uniqby/-/lodash.uniqby-4.7.0.tgz")
        (sha256
          (base32
            "1zwa038vr31hfyalgv49xrszhwdkfjq0rqi5fx5cpsjxry5pspa8"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://lodash.com/")
    (synopsis
      "The lodash method `_.uniqBy` exported as a module.")
    (description
      "The lodash method `_.uniqBy` exported as a module.")
    (license license:expat)))

(define-public node-prosemirror-changeset-2.1.2
  (package
    (name "node-prosemirror-changeset")
    (version "2.1.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/prosemirror-changeset/-/prosemirror-changeset-2.1.2.tgz")
        (sha256
          (base32
            "1lg04nxg3yawz24c2q9iijgkgyd3rf1dkxqak70b3ma9srlr8dvj"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-prosemirror-transform"
         ,node-prosemirror-transform-1.2.11)))
    (home-page
      "https://github.com/prosemirror/prosemirror-changeset#readme")
    (synopsis
      "Distills a series of editing steps into deleted and added ranges")
    (description
      "Distills a series of editing steps into deleted and added ranges")
    (license license:expat)))

(define-public node-prosemirror-commands-1.1.6
  (package
    (name "node-prosemirror-commands")
    (version "1.1.6")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/prosemirror-commands/-/prosemirror-commands-1.1.6.tgz")
        (sha256
          (base32
            "04i58ddl54700bbjizszy98nk8cw1yk02nshv2k1651m05h2jv53"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-prosemirror-state"
         ,node-prosemirror-state-1.3.4)
        ("node-prosemirror-transform"
         ,node-prosemirror-transform-1.2.11)
        ("node-prosemirror-model"
         ,node-prosemirror-model-1.13.3)))
    (home-page
      "https://github.com/prosemirror/prosemirror-commands#readme")
    (synopsis "Editing commands for ProseMirror")
    (description "Editing commands for ProseMirror")
    (license license:expat)))

(define-public node-resolve-1.20.0
  (package
    (name "node-resolve")
    (version "1.20.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/resolve/-/resolve-1.20.0.tgz")
        (sha256
          (base32
            "12x15vnr7yf5l0mr5ga28w0rsszm036832mmdp706drn8imgnhfl"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-path-parse" ,node-path-parse-1.0.6)
        ("node-is-core-module"
         ,node-is-core-module-2.2.0)))
    (home-page
      "https://github.com/browserify/resolve#readme")
    (synopsis
      "resolve like require.resolve() on behalf of files asynchronously and synchronously")
    (description
      "resolve like require.resolve() on behalf of files asynchronously and synchronously")
    (license license:expat)))

(define-public node-prosemirror-view-1.17.6
  (package
    (name "node-prosemirror-view")
    (version "1.17.6")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/prosemirror-view/-/prosemirror-view-1.17.6.tgz")
        (sha256
          (base32
            "0f0w90kkf4lpbkcynhbasrhvs5jiaazww6hgsala25gbjqsv1l5g"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-prosemirror-transform"
         ,node-prosemirror-transform-1.2.11)
        ("node-prosemirror-state"
         ,node-prosemirror-state-1.3.4)
        ("node-prosemirror-model"
         ,node-prosemirror-model-1.13.3)))
    (home-page
      "https://github.com/prosemirror/prosemirror-view#readme")
    (synopsis "ProseMirror's view component")
    (description "ProseMirror's view component")
    (license license:expat)))

(define-public node-prosemirror-dropcursor-1.3.3
  (package
    (name "node-prosemirror-dropcursor")
    (version "1.3.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/prosemirror-dropcursor/-/prosemirror-dropcursor-1.3.3.tgz")
        (sha256
          (base32
            "107waxr367s5rbg8v8lw115d718g9sq03i7vbm97rakpcl7pn54g"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-prosemirror-transform"
         ,node-prosemirror-transform-1.2.11)
        ("node-prosemirror-view"
         ,node-prosemirror-view-1.17.6)
        ("node-prosemirror-state"
         ,node-prosemirror-state-1.3.4)))
    (home-page
      "https://github.com/prosemirror/prosemirror-dropcursor#readme")
    (synopsis "Drop cursor plugin for ProseMirror")
    (description
      "Drop cursor plugin for ProseMirror")
    (license license:expat)))

(define-public node-w3c-keyname-2.2.4
  (package
    (name "node-w3c-keyname")
    (version "2.2.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/w3c-keyname/-/w3c-keyname-2.2.4.tgz")
        (sha256
          (base32
            "0chpkvgdsg3ff32hmz8xg0brd7k9x9sh4kzymqgjhjvawy3pknd0"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/marijnh/w3c-keyname#readme")
    (synopsis
      "Get a KeyboardEvent.key-style string from an event")
    (description
      "Get a KeyboardEvent.key-style string from an event")
    (license license:expat)))

(define-public node-prosemirror-keymap-1.1.4
  (package
    (name "node-prosemirror-keymap")
    (version "1.1.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/prosemirror-keymap/-/prosemirror-keymap-1.1.4.tgz")
        (sha256
          (base32
            "02ba33vpz03f7xp9khvgszy014rh8fvz3158c9bck6ridgk8xj35"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-prosemirror-state"
         ,node-prosemirror-state-1.3.4)
        ("node-w3c-keyname" ,node-w3c-keyname-2.2.4)))
    (home-page
      "https://github.com/prosemirror/prosemirror-keymap#readme")
    (synopsis "Keymap plugin for ProseMirror")
    (description "Keymap plugin for ProseMirror")
    (license license:expat)))

(define-public node-prosemirror-gapcursor-1.1.5
  (package
    (name "node-prosemirror-gapcursor")
    (version "1.1.5")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/prosemirror-gapcursor/-/prosemirror-gapcursor-1.1.5.tgz")
        (sha256
          (base32
            "0j6191i07sji1m5l5m9f6n1rxcxrp8n1wfx26l49l0gvjbc6l93i"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-prosemirror-view"
         ,node-prosemirror-view-1.17.6)
        ("node-prosemirror-state"
         ,node-prosemirror-state-1.3.4)
        ("node-prosemirror-model"
         ,node-prosemirror-model-1.13.3)
        ("node-prosemirror-keymap"
         ,node-prosemirror-keymap-1.1.4)))
    (home-page
      "https://github.com/prosemirror/prosemirror-gapcursor#readme")
    (synopsis
      "ProseMirror plugin for cursors at normally impossible-to-reach positions")
    (description
      "ProseMirror plugin for cursors at normally impossible-to-reach positions")
    (license license:expat)))

(define-public node-prosemirror-inputrules-1.1.3
  (package
    (name "node-prosemirror-inputrules")
    (version "1.1.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/prosemirror-inputrules/-/prosemirror-inputrules-1.1.3.tgz")
        (sha256
          (base32
            "1g2048iddcsrfy0bpgnpclzvirrn17raz32cqrrny1a5s3a6fnph"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-prosemirror-transform"
         ,node-prosemirror-transform-1.2.11)
        ("node-prosemirror-state"
         ,node-prosemirror-state-1.3.4)))
    (home-page
      "https://github.com/prosemirror/prosemirror-inputrules#readme")
    (synopsis
      "Automatic transforms on text input for ProseMirror")
    (description
      "Automatic transforms on text input for ProseMirror")
    (license license:expat)))

(define-public node-prosemirror-schema-list-1.1.4
  (package
    (name "node-prosemirror-schema-list")
    (version "1.1.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/prosemirror-schema-list/-/prosemirror-schema-list-1.1.4.tgz")
        (sha256
          (base32
            "1svk7c7xn20jw178mx5ln9wk0vlcsv4y7fzw57sk7633y9c876cq"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-prosemirror-transform"
         ,node-prosemirror-transform-1.2.11)
        ("node-prosemirror-model"
         ,node-prosemirror-model-1.13.3)))
    (home-page
      "https://github.com/prosemirror/prosemirror-schema-list#readme")
    (synopsis
      "List-related schema elements and commands for ProseMirror")
    (description
      "List-related schema elements and commands for ProseMirror")
    (license license:expat)))

(define-public node-prosemirror-tables-1.1.1
  (package
    (name "node-prosemirror-tables")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/prosemirror-tables/-/prosemirror-tables-1.1.1.tgz")
        (sha256
          (base32
            "0a04lvwsp3hiy2zc36d08wykf41a988rqdwz28zjmpzq0zzz1xjy"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-prosemirror-view"
         ,node-prosemirror-view-1.17.6)
        ("node-prosemirror-transform"
         ,node-prosemirror-transform-1.2.11)
        ("node-prosemirror-state"
         ,node-prosemirror-state-1.3.4)
        ("node-prosemirror-model"
         ,node-prosemirror-model-1.13.3)
        ("node-prosemirror-keymap"
         ,node-prosemirror-keymap-1.1.4)))
    (home-page
      "https://github.com/prosemirror/prosemirror-tables#readme")
    (synopsis
      "ProseMirror's rowspan/colspan tables component")
    (description
      "ProseMirror's rowspan/colspan tables component")
    (license license:expat)))

(define-public node-prosemirror-utils-0.9.6
  (package
    (name "node-prosemirror-utils")
    (version "0.9.6")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/prosemirror-utils/-/prosemirror-utils-0.9.6.tgz")
        (sha256
          (base32
            "095p3n1ak3j3lvmdymyp92sw4w3arkig4n55cnbdf5ara017cnj2"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/atlassian/prosemirror-utils#readme")
    (synopsis "Utils library for ProseMirror")
    (description "Utils library for ProseMirror")
    (license license:asl2.0)))

(define-public node-scheduler-0.19.1
  (package
    (name "node-scheduler")
    (version "0.19.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/scheduler/-/scheduler-0.19.1.tgz")
        (sha256
          (base32
            "1s9igfdyabpnmcn9pk7f5p5wlvhprnrib07d2fa0qppiq0j5jxly"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-object-assign" ,node-object-assign-4.1.1)
        ("node-loose-envify" ,node-loose-envify-1.4.0)))
    (home-page "https://reactjs.org/")
    (synopsis
      "Cooperative scheduler for the browser environment.")
    (description
      "Cooperative scheduler for the browser environment.")
    (license license:expat)))

(define-public node-react-dom-16.14.0
  (package
    (name "node-react-dom")
    (version "16.14.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/react-dom/-/react-dom-16.14.0.tgz")
        (sha256
          (base32
            "0dikgr72j7qc9gz60kvlqgmddwf2lnyzr47f1bayhk8gy638bl31"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-scheduler" ,node-scheduler-0.19.1)
        ("node-prop-types" ,node-prop-types-15.7.2)
		("node-react" ,node-react-16.14.0)
        ("node-object-assign" ,node-object-assign-4.1.1)
        ("node-loose-envify" ,node-loose-envify-1.4.0)))
    (home-page "https://reactjs.org/")
    (synopsis
      "React package for working with the DOM.")
    (description
      "React package for working with the DOM.")
    (license license:expat)))

(define-public node-memoize-one-5.1.1
  (package
    (name "node-memoize-one")
    (version "5.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/memoize-one/-/memoize-one-5.1.1.tgz")
        (sha256
          (base32
            "165gncfcwwfl2k0nkhjavsjrg3bhynrjdl55mnxpm6ax3j7q9lwx"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/alexreardon/memoize-one#readme")
    (synopsis
      "A memoization library which only remembers the latest invocation")
    (description
      "A memoization library which only remembers the latest invocation")
    (license license:expat)))

(define-public node-react-window-1.8.6
  (package
    (name "node-react-window")
    (version "1.8.6")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/react-window/-/react-window-1.8.6.tgz")
        (sha256
          (base32
            "1vq8wlf1d34hmn8jag6b41a01vwzbyw41ipylc7vj028f2zi93fq"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-memoize-one" ,node-memoize-one-5.1.1)
        ("node-babel-runtime"
         ,node-babel-runtime-7.12.13)))
    (home-page "http://react-window.now.sh/")
    (synopsis
      "React components for efficiently rendering large, scrollable lists and tabular data")
    (description
      "React components for efficiently rendering large, scrollable lists and tabular data")
    (license license:expat)))

(define-public node-textlint-ast-node-types-4.4.1
  (package
    (name "node-textlint-ast-node-types")
    (version "4.4.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/@textlint/ast-node-types/-/ast-node-types-4.4.1.tgz")
        (sha256
          (base32
            "1lqp1mpvia98n581f9qgkm1khbw2w445famivj2p0y72dxpqmw27"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/textlint/textlint#readme")
    (synopsis "textlint AST node type definition.")
    (description
      "textlint AST node type definition.")
    (license license:expat)))

(define-public node-string-decoder-1.3.0
  (package
    (name "node-string-decoder")
    (version "1.3.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/string_decoder/-/string_decoder-1.3.0.tgz")
        (sha256
          (base32
            "1bdkjw2kn1h4lrmqqfdwajsg9yivn92swwc3aciy8i83jh06j0vx"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-safe-buffer" ,node-safe-buffer-5.2.1)))
    (home-page
      "https://github.com/nodejs/string_decoder")
    (synopsis
      "The string_decoder module from Node core")
    (description
      "The string_decoder module from Node core")
    (license license:expat)))

(define-public node-readable-stream-3.6.0
  (package
    (name "node-readable-stream")
    (version "3.6.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/readable-stream/-/readable-stream-3.6.0.tgz")
        (sha256
          (base32
            "1fy6kya4g3zwjdc0xfg7gdzg9ynqnqzv9ay301ay174vvp9202ak"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-util-deprecate"
         ,node-util-deprecate-1.0.2)
        ("node-string-decoder"
         ,node-string-decoder-1.3.0)
        ("node-inherits" ,node-inherits-2.0.4)))
    (home-page
      "https://github.com/nodejs/readable-stream#readme")
    (synopsis
      "Streams3, a user-land copy of the stream library from Node.js")
    (description
      "Streams3, a user-land copy of the stream library from Node.js")
    (license license:expat)))

(define-public node-is-callable-1.2.3
  (package
    (name "node-is-callable")
    (version "1.2.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-callable/-/is-callable-1.2.3.tgz")
        (sha256
          (base32
            "1zflvsdz1gmdr4ay1nis64sh3w6x7mmgp3xjrk1cangp3y7r88yb"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/ljharb/is-callable#readme")
    (synopsis
      "Is this JS value callable? Works with Functions and GeneratorFunctions, despite ES6 @@toStringTag.")
    (description
      "Is this JS value callable? Works with Functions and GeneratorFunctions, despite ES6 @@toStringTag.")
    (license license:expat)))

(define-public node-is-date-object-1.0.2
  (package
    (name "node-is-date-object")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-date-object/-/is-date-object-1.0.2.tgz")
        (sha256
          (base32
            "10l3lp8mnafhxxfm3p1h0srcixlpx4qg6zf7wigr6qa1x5ghynrh"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/ljharb/is-date-object#readme")
    (synopsis
      "Is this value a JS Date object? This module works cross-realm/iframe, and despite ES6 @@toStringTag.")
    (description
      "Is this value a JS Date object? This module works cross-realm/iframe, and despite ES6 @@toStringTag.")
    (license license:expat)))

(define-public node-is-symbol-1.0.3
  (package
    (name "node-is-symbol")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-symbol/-/is-symbol-1.0.3.tgz")
        (sha256
          (base32
            "0kgsxf6f52fxp6ms6r04cx4wqf51yxmj0vg8azhyqp8akhj25b88"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-has-symbols" ,node-has-symbols-1.0.1)))
    (home-page
      "https://github.com/inspect-js/is-symbol#readme")
    (synopsis
      "Determine if a value is an ES6 Symbol or not.")
    (description
      "Determine if a value is an ES6 Symbol or not.")
    (license license:expat)))

(define-public node-es-to-primitive-1.2.1
  (package
    (name "node-es-to-primitive")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/es-to-primitive/-/es-to-primitive-1.2.1.tgz")
        (sha256
          (base32
            "1slbd6g1g0l9wb86h49ysflwzxzzj3cv9a7flpzrzrkp3lkmh1gk"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-is-symbol" ,node-is-symbol-1.0.3)
        ("node-is-date-object"
         ,node-is-date-object-1.0.2)
        ("node-is-callable" ,node-is-callable-1.2.3)))
    (home-page
      "https://github.com/ljharb/es-to-primitive#readme")
    (synopsis
      "ECMAScript â\x80\x9cToPrimitiveâ\x80\x9d algorithm. Provides ES5 and ES2015 versions.")
    (description
      "ECMAScript â\x80\x9cToPrimitiveâ\x80\x9d algorithm. Provides ES5 and ES2015 versions.")
    (license license:expat)))

(define-public node-is-negative-zero-2.0.1
  (package
    (name "node-is-negative-zero")
    (version "2.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-negative-zero/-/is-negative-zero-2.0.1.tgz")
        (sha256
          (base32
            "0cvc3rljj6ri87g5jkd91hffafpri9h3jygy9z9zc22wsm2p7pfn"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/inspect-js/is-negative-zero")
    (synopsis
      "Is this value negative zero? === will lie to you")
    (description
      "Is this value negative zero? === will lie to you")
    (license license:expat)))

(define-public node-call-bind-1.0.2
  (package
    (name "node-call-bind")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/call-bind/-/call-bind-1.0.2.tgz")
        (sha256
          (base32
            "06il2ki0bprw4s0a12bwnzwjc8gwwcw6f0cda0jyvj46sj56z5f3"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-get-intrinsic" ,node-get-intrinsic-1.1.1)
        ("node-function-bind" ,node-function-bind-1.1.1)))
    (home-page
      "https://github.com/ljharb/call-bind#readme")
    (synopsis "Robustly `.call.bind()` a function")
    (description
      "Robustly `.call.bind()` a function")
    (license license:expat)))

(define-public node-is-regex-1.1.2
  (package
    (name "node-is-regex")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-regex/-/is-regex-1.1.2.tgz")
        (sha256
          (base32
            "17d3mdlk9sqqmdnr0kqyvczj82fz2aaiqivmjrdcswsca1b9sps4"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-has-symbols" ,node-has-symbols-1.0.1)
        ("node-call-bind" ,node-call-bind-1.0.2)))
    (home-page
      "https://github.com/inspect-js/is-regex")
    (synopsis
      "Is this value a JS regex? Works cross-realm/iframe, and despite ES6 @@toStringTag")
    (description
      "Is this value a JS regex? Works cross-realm/iframe, and despite ES6 @@toStringTag")
    (license license:expat)))

(define-public node-object-inspect-1.9.0
  (package
    (name "node-object-inspect")
    (version "1.9.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/object-inspect/-/object-inspect-1.9.0.tgz")
        (sha256
          (base32
            "1pmfjkyl718029nxqzsld08kccmw61km61x1w0f0i718s3dmmhxc"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/inspect-js/object-inspect")
    (synopsis
      "string representations of objects in node and the browser")
    (description
      "string representations of objects in node and the browser")
    (license license:expat)))

(define-public node-object-keys-1.1.1
  (package
    (name "node-object-keys")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/object-keys/-/object-keys-1.1.1.tgz")
        (sha256
          (base32
            "04pjwxszvk8alk6z5lzk02ba3q56i2g70cs02f57qxhraq2xjy4n"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/ljharb/object-keys#readme")
    (synopsis
      "An Object.keys replacement, in case Object.keys is not available. From https://github.com/es-shims/es5-shim")
    (description
      "An Object.keys replacement, in case Object.keys is not available. From https://github.com/es-shims/es5-shim")
    (license license:expat)))

(define-public node-object-assign-4.1.2
  (package
    (name "node-object-assign")
    (version "4.1.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/object.assign/-/object.assign-4.1.2.tgz")
        (sha256
          (base32
            "0kr1m0zxblgar7ks4l5bkkfjib5s1bdfdg7gd0mx7nc8559fgmmc"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-object-keys" ,node-object-keys-1.1.1)
        ("node-has-symbols" ,node-has-symbols-1.0.1)
        ("node-define-properties"
         ,node-define-properties-1.1.3)
        ("node-call-bind" ,node-call-bind-1.0.2)))
    (home-page
      "https://github.com/ljharb/object.assign#readme")
    (synopsis
      "ES6 spec-compliant Object.assign shim. From https://github.com/es-shims/es6-shim")
    (description
      "ES6 spec-compliant Object.assign shim. From https://github.com/es-shims/es6-shim")
    (license license:expat)))

(define-public node-string-prototype-trimend-1.0.3
  (package
    (name "node-string-prototype-trimend")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/string.prototype.trimend/-/string.prototype.trimend-1.0.3.tgz")
        (sha256
          (base32
            "0jiz47xf5j5w4z5q2rkp9vp2w5ayi6wx3idlnkllsn0vrzqpqb5x"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-define-properties"
         ,node-define-properties-1.1.3)
        ("node-call-bind" ,node-call-bind-1.0.2)))
    (home-page
      "https://github.com/es-shims/String.prototype.trimEnd#readme")
    (synopsis
      "ES2019 spec-compliant String.prototype.trimEnd shim.")
    (description
      "ES2019 spec-compliant String.prototype.trimEnd shim.")
    (license license:expat)))

(define-public node-has-symbols-1.0.1
  (package
    (name "node-has-symbols")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/has-symbols/-/has-symbols-1.0.1.tgz")
        (sha256
          (base32
            "12fnwdir2mbi7bij7q86mvqdx3fxrlrmll6j7x0spg0j9angh2ix"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/ljharb/has-symbols#readme")
    (synopsis
      "Determine if the JS environment has Symbol support. Supports spec, or shams.")
    (description
      "Determine if the JS environment has Symbol support. Supports spec, or shams.")
    (license license:expat)))

(define-public node-get-intrinsic-1.1.1
  (package
    (name "node-get-intrinsic")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/get-intrinsic/-/get-intrinsic-1.1.1.tgz")
        (sha256
          (base32
            "06vvw0pyv22bv1ryb5hjwhrfxjsi2rj220z46cw2qv2smy7rc6ss"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-has-symbols" ,node-has-symbols-1.0.1)
        ("node-has" ,node-has-1.0.3)
        ("node-function-bind" ,node-function-bind-1.1.1)))
    (home-page
      "https://github.com/ljharb/get-intrinsic#readme")
    (synopsis
      "Get and robustly cache all JS language-level intrinsics at first require time")
    (description
      "Get and robustly cache all JS language-level intrinsics at first require time")
    (license license:expat)))

(define-public node-define-properties-1.1.3
  (package
    (name "node-define-properties")
    (version "1.1.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/define-properties/-/define-properties-1.1.3.tgz")
        (sha256
          (base32
            "19bhbbwl9m4jaj34gcvyw65bsx675y721fliqy9az5dv2pdz892m"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-object-keys" ,node-object-keys-1.1.1)))
    (home-page
      "https://github.com/ljharb/define-properties#readme")
    (synopsis
      "Define multiple non-enumerable properties at once. Uses `Object.defineProperty` when available; falls back to standard assignment in older engines.")
    (description
      "Define multiple non-enumerable properties at once. Uses `Object.defineProperty` when available; falls back to standard assignment in older engines.")
    (license license:expat)))

(define-public node-string-prototype-trimstart-1.0.3
  (package
    (name "node-string-prototype-trimstart")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/string.prototype.trimstart/-/string.prototype.trimstart-1.0.3.tgz")
        (sha256
          (base32
            "1802sm77yd6j8cr8w9166amr6w78wlajmlsdya832pyci51xaxsv"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-define-properties"
         ,node-define-properties-1.1.3)
        ("node-call-bind" ,node-call-bind-1.0.2)))
    (home-page
      "https://github.com/es-shims/String.prototype.trimStart#readme")
    (synopsis
      "ES2019 spec-compliant String.prototype.trimStart shim.")
    (description
      "ES2019 spec-compliant String.prototype.trimStart shim.")
    (license license:expat)))

(define-public node-es-abstract-1.18.0-next.2
  (package
    (name "node-es-abstract")
    (version "1.18.0-next.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/es-abstract/-/es-abstract-1.18.0-next.2.tgz")
        (sha256
          (base32
            "0psvxzh46x5q3g49ksyjllh23anxd4lkj9h06f0m8fvkg5l7nl9g"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-string-prototype-trimstart"
         ,node-string-prototype-trimstart-1.0.3)
        ("node-string-prototype-trimend"
         ,node-string-prototype-trimend-1.0.3)
        ("node-object-assign" ,node-object-assign-4.1.2)
        ("node-object-keys" ,node-object-keys-1.1.1)
        ("node-object-inspect"
         ,node-object-inspect-1.9.0)
        ("node-is-regex" ,node-is-regex-1.1.2)
        ("node-is-negative-zero"
         ,node-is-negative-zero-2.0.1)
        ("node-is-callable" ,node-is-callable-1.2.3)
        ("node-has-symbols" ,node-has-symbols-1.0.1)
        ("node-has" ,node-has-1.0.3)
        ("node-get-intrinsic" ,node-get-intrinsic-1.1.1)
        ("node-function-bind" ,node-function-bind-1.1.1)
        ("node-es-to-primitive"
         ,node-es-to-primitive-1.2.1)
        ("node-call-bind" ,node-call-bind-1.0.2)))
    (home-page
      "https://github.com/ljharb/es-abstract#readme")
    (synopsis "ECMAScript spec abstract operations.")
    (description
      "ECMAScript spec abstract operations.")
    (license license:expat)))

(define-public node-object-values-1.1.2
  (package
    (name "node-object-values")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/object.values/-/object.values-1.1.2.tgz")
        (sha256
          (base32
            "03dg98vbch6j6jbnnj883141s6hxmp4q4s731q39zfpnlkp26rf0"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-has" ,node-has-1.0.3)
        ("node-es-abstract"
         ,node-es-abstract-1.18.0-next.2)
        ("node-define-properties"
         ,node-define-properties-1.1.3)
        ("node-call-bind" ,node-call-bind-1.0.2)))
    (home-page
      "https://github.com/es-shims/Object.values#readme")
    (synopsis
      "ES2017 spec-compliant Object.values shim.")
    (description
      "ES2017 spec-compliant Object.values shim.")
    (license license:expat)))

(define-public node-boundary-1.0.1
  (package
    (name "node-boundary")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/boundary/-/boundary-1.0.1.tgz")
        (sha256
          (base32
            "0wv8vbfv73yywd7s4slzhbfc7bvxm5kqbw96yf3df5sv5l9yfmmc"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/Constellation/boundary")
    (synopsis
      "Provides boundary functions, (upper-bound and lower-bound).")
    (description
      "Provides boundary functions, (upper-bound and lower-bound).")
    (license #f)))

(define-public node-structured-source-3.0.2
  (package
    (name "node-structured-source")
    (version "3.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/structured-source/-/structured-source-3.0.2.tgz")
        (sha256
          (base32
            "1m6kmhbdccff9gcg355cbclbi2nhhqmh1ahacwa1wgfzawg3vplr"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-boundary" ,node-boundary-1.0.1)))
    (home-page
      "https://github.com/Constellation/structured-source")
    (synopsis
      "Provides StructuredSource and functionality for converting range and loc vice versa.")
    (description
      "Provides StructuredSource and functionality for converting range and loc vice versa.")
    (license #f)))

(define-public node-concat-stream-2.0.0
  (package
    (name "node-concat-stream")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/concat-stream/-/concat-stream-2.0.0.tgz")
        (sha256
          (base32
            "030m3v08q6mfh4vsdv5jc4w5c4q09il5lgw6mhaij8p0gix9jm6c"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-typedarray" ,node-typedarray-0.0.6)
        ("node-readable-stream"
         ,node-readable-stream-3.6.0)
        ("node-inherits" ,node-inherits-2.0.4)
        ("node-buffer-from" ,node-buffer-from-1.1.1)))
    (home-page
      "https://github.com/maxogden/concat-stream#readme")
    (synopsis
      "writable stream that concatenates strings or binary data and calls a callback with the result")
    (description
      "writable stream that concatenates strings or binary data and calls a callback with the result")
    (license license:expat)))

(define-public node-sentence-splitter-3.2.0
  (package
    (name "node-sentence-splitter")
    (version "3.2.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/sentence-splitter/-/sentence-splitter-3.2.0.tgz")
        (sha256
          (base32
            "01m627pb2pxq67j66pfm5715mpvhx1g03c6b8hfj7ij7dip2cp3b"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-structured-source"
         ,node-structured-source-3.0.2)
        ("node-object-values" ,node-object-values-1.1.2)
        ("node-concat-stream" ,node-concat-stream-2.0.0)
        ("node-textlint-ast-node-types"
         ,node-textlint-ast-node-types-4.4.1)))
    (home-page
      "https://github.com/azu/sentence-splitter")
    (synopsis
      "split {japanese, english} text into sentences.")
    (description
      "split {japanese, english} text into sentences.")
    (license license:expat)))

(define-public node-thenby-1.3.4
  (package
    (name "node-thenby")
    (version "1.3.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/thenby/-/thenby-1.3.4.tgz")
        (sha256
          (base32
            "0n4gw01z6snr70gcssazh0ll0g21wmplykdfvd5n8vinnnsp616p"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://github.com/Teun/thenBy.js")
    (synopsis
      "Micro library for sorting arrays using the firstBy().thenBy().thenBy() syntax")
    (description
      "Micro library for sorting arrays using the firstBy().thenBy().thenBy() syntax")
    (license license:asl2.0)))

(define-public node-tlite-0.1.9
  (package
    (name "node-tlite")
    (version "0.1.9")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/tlite/-/tlite-0.1.9.tgz")
        (sha256
          (base32
            "1n84qafxyf1ph6d74mc2c51pncj9rz8h7ijknqbnfsjxs77svzl3"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/chrisdavies/tlite")
    (synopsis "Tiny tooltip utility")
    (description "Tiny tooltip utility")
    (license license:expat)))

(define-public node-typescript-3.8.3
  (package
    (name "node-typescript")
    (version "3.8.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/typescript/-/typescript-3.8.3.tgz")
        (sha256
          (base32
            "11vv2h8lsja6vf9kvpm45ayswsw08zvydgk4hq97nrqaqlqhwf6p"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://www.typescriptlang.org/")
    (synopsis
      "TypeScript is a language for application scale JavaScript development")
    (description
      "TypeScript is a language for application scale JavaScript development")
    (license license:asl2.0)))

(define-public node-zenscroll-4.0.2
  (package
    (name "node-zenscroll")
    (version "4.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/zenscroll/-/zenscroll-4.0.2.tgz")
        (sha256
          (base32
            "14626z9wqrc0b2vm7f6q0irdb4zxqxs0rbg3l626kqhfzx7dy8kr"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://zengabor.github.io/zenscroll/")
    (synopsis
      "A module to smooth-scroll web pages and scrollable elements (like DIVs)")
    (description
      "A module to smooth-scroll web pages and scrollable elements (like DIVs)")
    (license license:unlicense)))

(define-public node-prosemirror-history-1.1.3
  (package
    (name "node-prosemirror-history")
    (version "1.1.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/prosemirror-history/-/prosemirror-history-1.1.3.tgz")
        (sha256
          (base32
            "15911imzm3gx6xq7fc1pzxd1ylghb4b7blpm19ryaayv2msmarzi"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-rope-sequence" ,node-rope-sequence-1.3.2)
        ("node-prosemirror-transform"
         ,node-prosemirror-transform-1.2.11)
        ("node-prosemirror-state"
         ,node-prosemirror-state-1.3.4)))
    (home-page
      "https://github.com/prosemirror/prosemirror-history#readme")
    (synopsis "Undo history for ProseMirror")
    (description "Undo history for ProseMirror")
    (license license:expat)))

(define-public node-rope-sequence-1.3.2
  (package
    (name "node-rope-sequence")
    (version "1.3.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/rope-sequence/-/rope-sequence-1.3.2.tgz")
        (sha256
          (base32
            "108il3s68x9jb8c6pv2767gkjw27b0rnzliz9610dzgclsrcms4d"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/marijnh/rope-sequence#readme")
    (synopsis "Rope-based persistent sequence type")
    (description
      "Rope-based persistent sequence type")
    (license license:expat)))

