cardinal_rlib
=============

By Rudolf Cardinal (rudolf@pobox.com).

This is an inelegant bunch of files containing miscellaneous code for use with
R and Stan.

You can source the latest version from Github. For example, to "source" the
``miscstat.R`` file from R, you can do this:

.. code-block:: R

    source("https://raw.githubusercontent.com/RudolfCardinal/rlib/master/miscstat.R")

and a copy should also be at https://egret.psychol.cam.ac.uk/rlib, as in:

.. code-block:: R

    source("https://egret.psychol.cam.ac.uk/rlib/miscstat.R")

Clearly these should be built into a proper R package at some stage.


Notes on requiring/attaching/loading packages
---------------------------------------------

As per https://r-pkgs.org/dependencies-mindset-background.html, the main
concepts for users are (a) "load", and (b) "load + attach". Attaching puts the
package on the search path, so you can call ``somefunc()`` rather than
``somepackage::somefunc()``. But library-style code should not mess with the
search path.

To load only, use ``loadNamespace()`` to throw an error upon failure, or
``requireNamespace()`` to return FALSE. (The "load + attach" equivalents are
``library()`` and ``require()``, respectively.)

User-mode code can simplify installation like this:

.. code-block:: R

    if (!require("pacman")) install.packages("pacman")
    pacman::p_load(
        data.table,
        patchwork
        # ...
    )

To show packages:

.. code-block:: R

    library()  # show install packages (system and user)
    (.packages())  # show loaded packages; see ?.packages
    loadedNamespaces()  # show loaded namespaces

Note that a package doesn't have to be loaded explicitly to be called via
``package::function()`` notation, it appears (empirically); that was a surprise
to me. It works on the command line: for example, if you have the "survival"
package installed but not loaded, and then type ``survival::<tab>``, survival
will appear in a subsequent ``loadedNamespaces()`` call. But even this works as
a script:

.. code-block:: R

    #!/usr/bin/env Rscript

    print(loadedNamespaces())
    survival::cluster(5)
    print(loadedNamespaces())

However, ``install.packages(...)`` does not add to ``loadedNamespaces()``. In
contrast, ``pacman::p_load(...)`` does.

Therefore, our quasi-library code does this for user convenience:

.. code-block:: R

    tmp_require_package_namespace <- function(...) {
        packages <- as.character(match.call(expand.dots = FALSE)[[2]])
        for (p in packages) if (!requireNamespace(p)) install.packages(p)
    }
    tmp_require_package_namespace(
        data.table,
        patchwork
        # ...
    )
    rm(tmp_require_package_namespace)

    # Could also use: packages <- sapply(substitute(...()), deparse)
    # cat("Loaded namespaces:\n"); print(loadedNamespaces())
    # cat("Attached packages:\n"); print(.packages())
