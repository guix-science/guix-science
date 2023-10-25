Guix science packages
=====================

.. image:: https://guix.bordeaux.inria.fr/jobset/guix-science/badge.svg?type=0
   :target: https://guix.bordeaux.inria.fr/jobset/guix-science

This GNU Guix_ channel provides recent versions of scientific software, which
cannot be included upstream.

See `Specifying Additional Channels`_ in the Guix manual for instructions how
to add it to your installation or simply add the following snippet to your
``channels.scm``:

.. code:: scheme

	(channel
	  (name 'guix-science)
	  (url "https://github.com/guix-science/guix-science.git")
	  (introduction
	   (make-channel-introduction
		"b1fe5aaff3ab48e798a4cce02f0212bc91f423dc"
		(openpgp-fingerprint
		 "CA4F 8CF4 37D7 478F DA05  5FD4 4213 7701 1A37 8446"))))

.. _Guix: https://guix.gnu.org/
.. _Specifying Additional Channels: https://guix.gnu.org/manual/en/guix.html#Specifying-Additional-Channels

Binary substitutes for ``x86_64-linux`` are available from
https://guix.bordeaux.inria.fr, see `Getting Substitutes from Other
Servers`_ in the official manual. The signing key can be imported
using:

.. code:: console

	$ guix archive --authorize <<EOF
    (public-key
     (ecc
      (curve Ed25519)
      (q #89FBA276A976A8DE2A69774771A92C8C879E0F24614AAAAE23119608707B3F06#)))
	EOF

.. _Getting Substitutes from Other Servers: https://guix.gnu.org/manual/en/guix.html#Getting-Substitutes-from-Other-Servers

Contributing
------------

We accept software fulfilling the following criteria:

- `Free and open source`_. Use guix-science-nonfree_ otherwise.
- Related to scientific research or teaching.
- Not upstreamable to Guix proper per their rules. For example: Some parts
  cannot be built from source without major efforts, excessive vendoring,
  prebuilt JavaScript, … Dependencies are exempt from this rule, although we
  prefer to have them upstreamed if possible.

.. _Free and open source: https://opensource.org/osd
.. _guix-science-nonfree: https://github.com/guix-science/guix-science-nonfree

