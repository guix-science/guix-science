Guix science packages
=====================

.. image:: https://github.com/guix-science/guix-science/workflows/build/badge.svg
   :target: https://github.com/guix-science/guix-science/actions

This GNU Guix_ channel provides recent versions of scientific software, which
cannot be included upstream. Currently the following software is packaged:

- JASP_ (``jasp``)
- Jupyter Notebook (``python-notebook``)
- JupyterLab_ (``python-jupyterlab``)
- `RStudio Desktop`_ (``rstudio``)
- RStudio Server (``rstudio-server``)

.. _JASP: https://jasp-stats.org/
.. _JupyterLab: https://jupyterlab.readthedocs.io/
.. _RStudio Desktop: https://rstudio.com/products/rstudio/

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
https://substitutes.guix.psychnotebook.org/, see `Getting Substitutes from
Other Servers`_ in the official manual. The signing key can be imported using:

.. code:: console

	$ guix archive --authorize <<EOF
	(public-key
	 (ecc
	  (curve Ed25519)
	  (q #D4E1CAFAB105581122B326E89804E3546EF905C0D9B39F161BBD8ABB4B11D14A#)
	  )
	 )
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

