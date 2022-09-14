;;;
;;; Copyright © 2019, 2020 Lars-Dominik Braun <ldb@leibniz-psychology.org>
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

(define-module (guix-science packages jupyter-node)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system node)
  #:use-module (srfi srfi-1))

(define-public node-commander-7.2.0
  (package
    (name "node-commander")
    (version "7.2.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/commander/-/commander-7.2.0.tgz")
        (sha256
          (base32
            "1xj8chx8pprr2mm2yrv506msr1m3psh997msf3r0379h65g8kc6b"))))
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

(define-public node-eventemitter3-4.0.7
  (package
    (name "node-eventemitter3")
    (version "4.0.7")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/eventemitter3/-/eventemitter3-4.0.7.tgz")
        (sha256
          (base32
            "1s2xjw4qgdrpg1rf759drg9ssl2pi6ban5gnb399l7cmlv7xwg3h"))))
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
      "https://github.com/primus/eventemitter3#readme")
    (synopsis
      "EventEmitter3 focuses on performance while maintaining a Node.js AND browser compatible interface.")
    (description
      "EventEmitter3 focuses on performance while maintaining a Node.js AND browser compatible interface.")
    (license license:expat)))

(define-public node-requires-port-1.0.0
  (package
    (name "node-requires-port")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/requires-port/-/requires-port-1.0.0.tgz")
        (sha256
          (base32
            "1qn60lbgy6apjdkcgk2rpda122xz5zx3ml8hlljxjq1042gpcl31"))))
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
      "https://github.com/unshiftio/requires-port")
    (synopsis
      "Check if a protocol requires a certain port number to be added to an URL.")
    (description
      "Check if a protocol requires a certain port number to be added to an URL.")
    (license license:expat)))

(define-public node-follow-redirects-1.15.2
  (package
    (name "node-follow-redirects")
    (version "1.15.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/follow-redirects/-/follow-redirects-1.15.2.tgz")
        (sha256
          (base32
            "17fw0gxns1b1nx8w5r2kwjbqpk03mqrxzw3fh3g985djlmsizfib"))))
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
      "https://github.com/follow-redirects/follow-redirects")
    (synopsis
      "HTTP and HTTPS modules that follow redirects.")
    (description
      "HTTP and HTTPS modules that follow redirects.")
    (license license:expat)))

(define-public node-http-proxy-1.18.1
  (package
    (name "node-http-proxy")
    (version "1.18.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/http-proxy/-/http-proxy-1.18.1.tgz")
        (sha256
          (base32
            "1rzjma8gsd2mq2jxldl0f3iqikxd0qzbsm4f9pb5cpspn5anjm74"))))
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
      `(("node-follow-redirects"
         ,node-follow-redirects-1.15.2)
        ("node-requires-port" ,node-requires-port-1.0.0)
        ("node-eventemitter3" ,node-eventemitter3-4.0.7)))
    (home-page
      "https://github.com/http-party/node-http-proxy#readme")
    (synopsis "HTTP proxying for the masses")
    (description "HTTP proxying for the masses")
    (license license:expat)))

(define-public node-bintrees-1.0.2
  (package
    (name "node-bintrees")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/bintrees/-/bintrees-1.0.2.tgz")
        (sha256
          (base32
            "0fncybq97vlnqz03zp9kmhwyd8szpbimqi8p2x1b34gwj6ccvbsl"))))
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
      "https://github.com/vadimg/js_bintrees#readme")
    (synopsis "Binary Search Trees")
    (description "Binary Search Trees")
    (license license:expat)))

(define-public node-tdigest-0.1.2
  (package
    (name "node-tdigest")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/tdigest/-/tdigest-0.1.2.tgz")
        (sha256
          (base32
            "1xn0rib84m4zmq85r7zcprxkjiiq1hlssxri8chbhmsi6iakzxb0"))))
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
      `(("node-bintrees" ,node-bintrees-1.0.2)))
    (home-page "https://github.com/welch/tdigest")
    (synopsis
      "javascript implementation of Dunning's T-Digest for streaming quantile approximation")
    (description
      "javascript implementation of Dunning's T-Digest for streaming quantile approximation")
    (license license:expat)))

(define-public node-prom-client-14.1.0
  (package
    (name "node-prom-client")
    (version "14.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/prom-client/-/prom-client-14.1.0.tgz")
        (sha256
          (base32
            "1wdrdkwnl8375ym7c5vr8wcyrr2ydc8bjgqbf81k8f73yd73h7yz"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-tdigest" ,node-tdigest-0.1.2)))
    (home-page
      "https://github.com/siimon/prom-client")
    (synopsis "Client for prometheus")
    (description "Client for prometheus")
    (license license:asl2.0)))

(define-public node-strftime-0.10.1
  (package
    (name "node-strftime")
    (version "0.10.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/strftime/-/strftime-0.10.1.tgz")
        (sha256
          (base32
            "1ciscmjjfghhnm1r944iqvr862g8zcyg075dl1rd0cyf8fyrzbyf"))))
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
      "https://samhuri.net/projects/strftime")
    (synopsis "strftime for JavaScript")
    (description "strftime for JavaScript")
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
      `(("node-color-name" ,node-color-name-1.1.4)))
    (home-page
      "https://github.com/Qix-/color-convert#readme")
    (synopsis "Plain color conversion functions")
    (description "Plain color conversion functions")
    (license license:expat)))

(define-public node-color-name-1.1.4
  (package
    (name "node-color-name")
    (version "1.1.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/color-name/-/color-name-1.1.4.tgz")
        (sha256
          (base32
            "020p7x7k8rlph38lhsqpqvkx0b70lzlmk6mgal9r9sz8c527qysh"))))
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
      "https://github.com/colorjs/color-name")
    (synopsis "A list of color names and its values")
    (description
      "A list of color names and its values")
    (license license:expat)))

(define-public node-is-arrayish-0.3.2
  (package
    (name "node-is-arrayish")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-arrayish/-/is-arrayish-0.3.2.tgz")
        (sha256
          (base32
            "0cpajzzj5d2f69hbmvyk1aw1ajafi4724hx33lm6a6argr4nxmh1"))))
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

(define-public node-simple-swizzle-0.2.2
  (package
    (name "node-simple-swizzle")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/simple-swizzle/-/simple-swizzle-0.2.2.tgz")
        (sha256
          (base32
            "0gv8qfgsh62r76kbdzg7r10f353n00qqh8hd9rm611xbvgrlazsw"))))
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
      `(("node-is-arrayish" ,node-is-arrayish-0.3.2)))
    (home-page
      "https://github.com/qix-/node-simple-swizzle#readme")
    (synopsis "Simply swizzle your arguments")
    (description "Simply swizzle your arguments")
    (license license:expat)))

(define-public node-color-string-1.9.1
  (package
    (name "node-color-string")
    (version "1.9.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/color-string/-/color-string-1.9.1.tgz")
        (sha256
          (base32
            "11mj2kydpsa4b53crq6s4bwrqlhx5dv62rhk23z47n6r5vahsfrj"))))
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
      `(("node-simple-swizzle"
         ,node-simple-swizzle-0.2.2)
        ("node-color-name" ,node-color-name-1.1.4)))
    (home-page
      "https://github.com/Qix-/color-string#readme")
    (synopsis
      "Parser and generator for CSS color strings")
    (description
      "Parser and generator for CSS color strings")
    (license license:expat)))

(define-public node-color-3.2.1
  (package
    (name "node-color")
    (version "3.2.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/color/-/color-3.2.1.tgz")
        (sha256
          (base32
            "0sa9k0inzwxsikhl9m14rafc43ca2hpr2x6c9s5j40i5bzcc3h51"))))
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
      `(("node-color-string" ,node-color-string-1.9.1)
        ("node-color-convert" ,node-color-convert-1.9.3)))
    (home-page
      "https://github.com/Qix-/color#readme")
    (synopsis
      "Color conversion and manipulation with CSS string support")
    (description
      "Color conversion and manipulation with CSS string support")
    (license license:expat)))

(define-public node-text-hex-1.0.0
  (package
    (name "node-text-hex")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/text-hex/-/text-hex-1.0.0.tgz")
        (sha256
          (base32
            "00zhmsmjknbn075js9wq91887fh690cymgckbjjiymv4bnmsjqaa"))))
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
      "https://github.com/3rd-Eden/text-hex")
    (synopsis
      "Generate a hex color from the given text")
    (description
      "Generate a hex color from the given text")
    (license license:expat)))

(define-public node-colorspace-1.1.4
  (package
    (name "node-colorspace")
    (version "1.1.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/colorspace/-/colorspace-1.1.4.tgz")
        (sha256
          (base32
            "0ds19v89jhifba9hzgm001zwhawcyzwjmbjla7plkk8433641crw"))))
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
      `(("node-text-hex" ,node-text-hex-1.0.0)
        ("node-color" ,node-color-3.2.1)))
    (home-page
      "https://github.com/3rd-Eden/colorspace")
    (synopsis
      "Generate HEX colors for a given namespace.")
    (description
      "Generate HEX colors for a given namespace.")
    (license license:expat)))

(define-public node-enabled-2.0.0
  (package
    (name "node-enabled")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/enabled/-/enabled-2.0.0.tgz")
        (sha256
          (base32
            "0g6dqx906cmj5m3mk6q24bkg8bf9i8cxqyzc0cx1i4n02qrz19kg"))))
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
      "https://github.com/3rd-Eden/enabled#readme")
    (synopsis
      "Check if a certain debug flag is enabled.")
    (description
      "Check if a certain debug flag is enabled.")
    (license license:expat)))

(define-public node-kuler-2.0.0
  (package
    (name "node-kuler")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/kuler/-/kuler-2.0.0.tgz")
        (sha256
          (base32
            "1x16hvz5fvdvp0mkjp5zz1270k5r38is5f6wdwcpyhzhjin934b6"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://github.com/3rd-Eden/kuler")
    (synopsis
      "Color your terminal using CSS/hex color codes")
    (description
      "Color your terminal using CSS/hex color codes")
    (license license:expat)))

(define-public node-dabh-diagnostics-2.0.3
  (package
    (name "node-dabh-diagnostics")
    (version "2.0.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/@dabh/diagnostics/-/diagnostics-2.0.3.tgz")
        (sha256
          (base32
            "01g7qcnk4dmbwkv20vaivq4qd3l4bqg1m7isc13zwcdxrlxs4isk"))))
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
      `(("node-kuler" ,node-kuler-2.0.0)
        ("node-enabled" ,node-enabled-2.0.0)
        ("node-colorspace" ,node-colorspace-1.1.4)))
    (home-page
      "https://github.com/3rd-Eden/diagnostics")
    (synopsis
      "Tools for debugging your node.js modules and event loop")
    (description
      "Tools for debugging your node.js modules and event loop")
    (license license:expat)))

(define-public node-async-3.2.4
  (package
    (name "node-async")
    (version "3.2.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/async/-/async-3.2.4.tgz")
        (sha256
          (base32
            "0vr9k1k33d1wcg3x3qq8nmg22pjjdvygznb5iz7fliihf447p3f2"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://caolan.github.io/async/")
    (synopsis
      "Higher-order functions and common patterns for asynchronous code")
    (description
      "Higher-order functions and common patterns for asynchronous code")
    (license license:expat)))

(define-public node-is-stream-2.0.1
  (package
    (name "node-is-stream")
    (version "2.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-stream/-/is-stream-2.0.1.tgz")
        (sha256
          (base32
            "1mxnc5nlh73zg34vwdd9k5mx2wcs5pc84j8a2yig2y0bl9rgy09m"))))
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
      "https://github.com/sindresorhus/is-stream#readme")
    (synopsis
      "Check if something is a Node.js stream")
    (description
      "Check if something is a Node.js stream")
    (license license:expat)))

(define-public node-logform-2.4.2
  (package
    (name "node-logform")
    (version "2.4.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/logform/-/logform-2.4.2.tgz")
        (sha256
          (base32
            "0zzsj8ymi61dqhqim3yp07abgiis89z4sdcz7c8hcgwr19pk93il"))))
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
      `(("node-triple-beam" ,node-triple-beam-1.3.0)
        ("node-safe-stable-stringify"
         ,node-safe-stable-stringify-2.3.1)
        ("node-ms" ,node-ms-2.1.3)
        ("node-fecha" ,node-fecha-4.2.3)
        ("node-colors-colors" ,node-colors-colors-1.5.0)))
    (home-page
      "https://github.com/winstonjs/logform#readme")
    (synopsis
      "An mutable object-based log format designed for chaining & objectMode streams.")
    (description
      "An mutable object-based log format designed for chaining & objectMode streams.")
    (license license:expat)))

(define-public node-fn-name-1.1.0
  (package
    (name "node-fn-name")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/fn.name/-/fn.name-1.1.0.tgz")
        (sha256
          (base32
            "1mz826ygqynqwanf0l8bgsacwmh9cqb3943gd8fyn2gl08g5f91z"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://github.com/3rd-Eden/fn.name")
    (synopsis "Extract names from functions")
    (description "Extract names from functions")
    (license license:expat)))

(define-public node-one-time-1.0.0
  (package
    (name "node-one-time")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/one-time/-/one-time-1.0.0.tgz")
        (sha256
          (base32
            "0ksxyjp47hdvggawwws6q5a29lsn3asrfpaij5p2345a8pgms06v"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-fn-name" ,node-fn-name-1.1.0)))
    (home-page
      "https://github.com/3rd-Eden/one-time#readme")
    (synopsis
      "Run the supplied function exactly one time (once)")
    (description
      "Run the supplied function exactly one time (once)")
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

(define-public node-stack-trace-0.0.10
  (package
    (name "node-stack-trace")
    (version "0.0.10")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/stack-trace/-/stack-trace-0.0.10.tgz")
        (sha256
          (base32
            "0pszfwxxiy0171s9vm53x8hmav7g4yhlqcqw91l4q8sxc8x8kq3i"))))
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
      "https://github.com/felixge/node-stack-trace")
    (synopsis
      "Get v8 stack traces as an array of CallSite objects.")
    (description
      "Get v8 stack traces as an array of CallSite objects.")
    (license license:expat)))

(define-public node-colors-colors-1.5.0
  (package
    (name "node-colors-colors")
    (version "1.5.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/@colors/colors/-/colors-1.5.0.tgz")
        (sha256
          (base32
            "1xc4j80555a3za55qr5fvshchdva0qxxh60fgqg9dc6bq8axzp1j"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://github.com/DABH/colors.js")
    (synopsis "get colors in your node.js console")
    (description
      "get colors in your node.js console")
    (license license:expat)))

(define-public node-fecha-4.2.3
  (package
    (name "node-fecha")
    (version "4.2.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/fecha/-/fecha-4.2.3.tgz")
        (sha256
          (base32
            "1a077995mln7x9kgv9bfi0mwwgpm19h5kdma4lbmmk0pzpvz67nl"))))
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
      "https://github.com/taylorhakes/fecha")
    (synopsis "Date formatting and parsing")
    (description "Date formatting and parsing")
    (license license:expat)))

(define-public node-ms-2.1.3
  (package
    (name "node-ms")
    (version "2.1.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/ms/-/ms-2.1.3.tgz")
        (sha256
          (base32
            "1ii24v83yrryzmj9p369qxmpr53337kkqbdaklpmbv9hwlanwqgn"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://github.com/vercel/ms#readme")
    (synopsis "Tiny millisecond conversion utility")
    (description
      "Tiny millisecond conversion utility")
    (license license:expat)))

(define-public node-safe-stable-stringify-2.3.1
  (package
    (name "node-safe-stable-stringify")
    (version "2.3.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/safe-stable-stringify/-/safe-stable-stringify-2.3.1.tgz")
        (sha256
          (base32
            "1hdicgksh3nwwqs12bzwa28k602mg2pp7qw47gdk59fw74xmka5c"))))
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
      "https://github.com/BridgeAR/safe-stable-stringify#readme")
    (synopsis
      "Deterministic and safely JSON.stringify to quickly serialize JavaScript objects")
    (description
      "Deterministic and safely JSON.stringify to quickly serialize JavaScript objects")
    (license license:expat)))

(define-public node-logform-2.4.2
  (package
    (name "node-logform")
    (version "2.4.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/logform/-/logform-2.4.2.tgz")
        (sha256
          (base32
            "0zzsj8ymi61dqhqim3yp07abgiis89z4sdcz7c8hcgwr19pk93il"))))
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
      `(("node-triple-beam" ,node-triple-beam-1.3.0)
        ("node-safe-stable-stringify"
         ,node-safe-stable-stringify-2.3.1)
        ("node-ms" ,node-ms-2.1.3)
        ("node-fecha" ,node-fecha-4.2.3)
        ("node-colors-colors" ,node-colors-colors-1.5.0)))
    (home-page
      "https://github.com/winstonjs/logform#readme")
    (synopsis
      "An mutable object-based log format designed for chaining & objectMode streams.")
    (description
      "An mutable object-based log format designed for chaining & objectMode streams.")
    (license license:expat)))

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

(define-public node-triple-beam-1.3.0
  (package
    (name "node-triple-beam")
    (version "1.3.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/triple-beam/-/triple-beam-1.3.0.tgz")
        (sha256
          (base32
            "1k2j7xih9yzgd0qfxwjp9cajw7rklz41rz450w3ja0dz4ckjk5a1"))))
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
      "https://github.com/winstonjs/triple-beam#readme")
    (synopsis
      "Definitions of levels for logging purposes & shareable Symbol constants.")
    (description
      "Definitions of levels for logging purposes & shareable Symbol constants.")
    (license license:expat)))

(define-public node-winston-transport-4.5.0
  (package
    (name "node-winston-transport")
    (version "4.5.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/winston-transport/-/winston-transport-4.5.0.tgz")
        (sha256
          (base32
            "0sx4saa6kf83yhpsa5jvv6lzwqmmpd126135wy6xv5ga7dcmidv7"))))
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
      `(("node-triple-beam" ,node-triple-beam-1.3.0)
        ("node-readable-stream"
         ,node-readable-stream-3.6.0)
        ("node-logform" ,node-logform-2.4.2)))
    (home-page
      "https://github.com/winstonjs/winston-transport#readme")
    (synopsis
      "Base stream implementations for winston@3 and up.")
    (description
      "Base stream implementations for winston@3 and up.")
    (license license:expat)))

(define-public node-winston-3.8.2
  (package
    (name "node-winston")
    (version "3.8.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/winston/-/winston-3.8.2.tgz")
        (sha256
          (base32
            "050070xbhbxfpphz3sl8rry9iva8b3bi22viv8404ncjnw7hf6qz"))))
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
      `(("node-winston-transport"
         ,node-winston-transport-4.5.0)
        ("node-triple-beam" ,node-triple-beam-1.3.0)
        ("node-stack-trace" ,node-stack-trace-0.0.10)
        ("node-safe-stable-stringify"
         ,node-safe-stable-stringify-2.3.1)
        ("node-readable-stream"
         ,node-readable-stream-3.6.0)
        ("node-one-time" ,node-one-time-1.0.0)
        ("node-logform" ,node-logform-2.4.2)
        ("node-is-stream" ,node-is-stream-2.0.1)
        ("node-async" ,node-async-3.2.4)
        ("node-colors-colors" ,node-colors-colors-1.5.0)
        ("node-dabh-diagnostics"
         ,node-dabh-diagnostics-2.0.3)))
    (home-page
      "https://github.com/winstonjs/winston#readme")
    (synopsis "A logger for just about everything.")
    (description
      "A logger for just about everything.")
    (license license:expat)))

(define-public configurable-http-proxy
  (package
    (name "configurable-http-proxy")
    (version "4.5.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/configurable-http-proxy/-/configurable-http-proxy-4.5.3.tgz")
        (sha256
          (base32
            "1krkn0kz91i45jk6h7r49pqpslgg4cs67k0xaacc3gsvpa7kskvp"))))
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
      `(("node-winston" ,node-winston-3.8.2)
        ("node-strftime" ,node-strftime-0.10.1)
        ("node-prom-client" ,node-prom-client-14.1.0)
        ("node-http-proxy" ,node-http-proxy-1.18.1)
        ("node-commander" ,node-commander-7.2.0)))
    (home-page
      "https://github.com/jupyterhub/configurable-http-proxy#readme")
    (synopsis "A configurable-on-the-fly HTTP Proxy")
    (description
      "A configurable-on-the-fly HTTP Proxy")
    (license license:bsd-3)))

