Guix science packages
=====================

.. image:: https://github.com/guix-science/guix-science/workflows/build/badge.svg

This GNU Guix_ channel provides recent versions of scientific software, which
cannot be included upstream. Currently the following software is packaged:

- Jupyter Notebook (``python-notebook``)
- JupyterLab (``python-jupyterlab``)
- RStudio Desktop (``rstudio``)
- RStudio Web (``rstudio-server``)

See `Using a Custom Guix Channel`_ for instructions how to add it to your
installation or simply add the following snippet to your ``channels.scm``:

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
.. _Using a Custom Guix Channel: https://guix.gnu.org/manual/en/guix.html#Using-a-Custom-Guix-Channel


