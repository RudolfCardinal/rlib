Alternative Docker Compose approach (abandoned)
===============================================

It's possible to use Docker Compose, e.g. create ``docker-compose.yml`` as
follows:

.. code-block:: none

    ---

    # =============================================================================
    # docker-compose file syntax version
    # =============================================================================

    version: "3.5"


    # =============================================================================
    # Containers (services)
    # =============================================================================

    services:

        # -------------------------------------------------------------------------
        # R container, aimed at the host's filesystem
        # -------------------------------------------------------------------------

        r_container:
            # Build a container from a Dockerfile.
            build:
                # Context for Docker to build the image (relative to this file).
                context: .

                # Filename of the Dockerfile to use, relative to the context.
                dockerfile: Dockerfile

            # Specify volumes by name, having explored the default container to
            # find out what it creates (and where it mounts it) otherwise.
            # (You can't have no volume.)
            volumes:
                # './path/on/docker/host:/path/inside/container'
                - ${HOME}:/userhome
                - ${PWD}:/userdata

This can be used by firing up the container from one prompt:

.. code-block:: bash

    docker-compose up

And from another prompt:

.. code-block:: bash

    docker container ls  # check container name
    docker exec -it rlib_r_container_1 bash

Directories within the Docker environment will include:

.. code-block:: none

    /cardinal_rlib      # from the Dockerfile
    /userhome           # mounted from docker-compose.yml
    /userdata           # mounted from docker-compose.yml

However, this is a bit convoluted. Docker Compose is all about orchestrating
multiple containers, and we really only want the one (mounted in different
ways).


Using Docker Hub
================

To push to Docker Hub:

.. code-block:: bash

    docker login -u "myusername" -p "mypassword" docker.io
    docker push IMAGE/NAME
