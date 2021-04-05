.. stan_speed.rst

.. _Ahn2017: https://pubmed.ncbi.nlm.nih.gov/29601060/
.. _Docker Swarm: https://docs.docker.com/engine/swarm/
.. _Hains2018: https://pubmed.ncbi.nlm.nih.gov/30289167/
.. _Romeu2020: https://pubmed.ncbi.nlm.nih.gov/31735532/
.. _Singularity: https://sylabs.io/singularity/
.. _SLURM: https://slurm.schedmd.com/
.. _Unison: https://www.cis.upenn.edu/~bcpierce/unison/


Making Stan run faster
======================

The compiler environment
------------------------

You want RStan itself and your own C++ code to be compiled with the ``-O3``
(highest) `level of optimization
<https://gcc.gnu.org/onlinedocs/gcc/Optimize-Options.html>`_, and without the
``-g`` ("include debugging information") flag. These are options for the
``CXXFLAGS`` environment variable, set by a ``Makeconf`` or ``Makevars`` file.

This is slightly confusing.

1.  There is a system-wide ``Makeconf``.

    Where is it?

    .. code-block:: R

        library(rstan)
        rstan::makeconf_path()  # shows path, e.g. /usr/lib/R/etc/Makeconf

2.  There may be a user-specific ``~/.R/Makevars``, which should override
    system-wide settings.

    Where is it?

    .. code-block:: R

        library(tools)
        tools::makevars_user()  # e.g. /home/rudolf/.R/Makevars

`This link <https://groups.google.com/g/stan-users/c/a96cURY9gVI?pli=1>`_
suggests that RStan automatically uses ``-O3`` for installation.

The old ``rstan::set_cppo()`` function is defunct and advises using your
``Makevars`` file.


General Stan/C++ coding
-----------------------

- Use fewer C++ statements; Stan applies an overhead to each.

- Vectorize everything that you can.

- In particular, vectorize sampling statements.

  In this context:

    .. code-block:: C++

        data {
            int N_TRIALS;
            int<lower=0, upper=1> responded_right[N_TRIALS];
        }

  this method is slow:

    .. code-block:: C++

        model {
            real p_choose_rhs;
            for (i in 1:N_TRIALS) {
                p_choose_rhs = ...
                responded_right[i] ~ bernoulli(p_choose_rhs);
            }
        }

  and this is faster, as it vectorizes the sampling statement:

    .. code-block:: C++

        model {
            vector[N_TRIALS] p_choose_rhs;
            for (i in 1:N_TRIALS) {
                p_choose_rhs[i] = ...
            }
            responded_right ~ bernoulli(p_choose_rhs);
        }

- For the ``y ~ bernoulli(theta)`` distribution, ``y`` is in {0, 1} and
  ``theta`` is a probability in the range [0, 1]. However, if you start with
  log odds, use ``y ~ bernoulli_logit(alpha)``, where alpha is a logit (log
  odds) in the range [-inf, +inf]. This is more efficient than converting the
  log odds into a probability and then using ``bernoulli()``.

- For softmax, there is no neat mapping of the softmax coefficients to to
  "logit space". Stan provides the `softmax()
  <https://mc-stan.org/docs/2_21/functions-reference/softmax.html>`_ function.
  It also provides a ``log_softmax()`` function, returning the natural log of
  the softmax. However, the reason for this function is to avoid underflow in
  some circumstances (e.g. https://stats.stackexchange.com/questions/436766/);
  "log probability" is obviously not the same as "logit" (log odds) and isn't
  useful for this purpose.

  - This library provides ``logitSoftmaxNth()`` but, when profiled, it is
    slower to use ``logitSoftmaxNth()`` and then ``bernoulli_logit()`` than
    it is to use ``softmaxNth()`` and then ``bernoulli()``. See
    ``tests/profile_stan_softmax/profile_softmax.stan``.

- If you want to fetch a particular result from a softmax operation, which is
  common, it turns out to be quicker (for a two-item softmax) to use this
  library's custom ``softmaxNth()`` function than Stan's built-in
  ``softmax()``. See ``tests/profile_stan_softmax/profile_softmax.stan``.


The model and parameterization
------------------------------

- Make the parameter space easy for Stan to explore.

- When a quantity is sampled from a Normal(mu, sigma) distribution, sample it
  from a N(0, 1) distribution and scale it:

    .. code-block:: C++

        standard_normal_X ~ Normal(0, 1);
        X = sigma * standard_normal_X + mu;

  This is referred to as "noncentred parameterization" or the "Matt trick".

- Try to use "soft constraints", i.e. avoid hard pass/fail boundaries for the
  sampling algorithm (such as truncated distributions). In particular, consider
  e.g. the Ahn2017_, Hains2018_, Romeu2020_ method in which:

  - an unconstrained parameter A is sampled like this:

    .. code-block:: C++

        real mu_A;
        real<lower=0> sigma_A;
        real A;

        mu_A ~ normal(0, 10);
        sigma_A ~ cauchy(0, 5);  // half-Cauchy because of <lower=0> limit
        A ~ normal(mu_A, sigma_A);

  - a positive parameter B is sampled like this:

    .. code-block:: C++

        real mu_B;
        real<lower=0> sigma_B;
        real normal_B;
        real B;

        mu_B ~ normal(0, 1);
        sigma_B ~ cauchy(0, 5);  // half-Cauchy because of <lower=0> limit
        normal_B ~ normal(mu_B, sigma_B);
        B = exp(normal_B);

  - a parameter C in the range [0, 1] is sampled like this:

    .. code-block:: C++

        real mu_C;
        real<lower=0> sigma_C;
        real normal_C;
        real C;

        mu_C ~ normal(0, 1);
        sigma_C ~ cauchy(0, 5);  // half-Cauchy because of <lower=0> limit
        normal_C ~ normal(mu_C, sigma_C);
        C = Phi(normal_C);  // equivalent to "inverse_probit(normal_C)"

    - The **probit** function is the quantile function (the inverse of the
      cumulative distribution function) for the standard normal
      distribution (https://en.wikipedia.org/wiki/Probit), and thus maps [0, 1]
      to [−∞, +∞]. In R, this is ``qnorm()``, as in ``q <- qnorm(p)``.

    - The **inverse probit** function is the cumulative distribution function
      (CDF) of the standard normal distribution, often written ``Φ()``. It maps
      [−∞, +∞] to [0, 1]. In R, this function is ``pnorm()``, as in ``p <-
      pnorm(q)``. In Stan, it is `Phi()
      <https://mc-stan.org/docs/2_21/stan-users-guide/logistic-probit-regression-section.html>`_.

  - a parameter D in the range [0, U], where U is an upper limit, is sampled
    like this:

    .. code-block:: C++

        real mu_D;
        real<lower=0> sigma_D;
        real normal_D;
        real D;

        mu_D ~ normal(0, 1);
        sigma_D ~ cauchy(0, 5);  // half-Cauchy because of <lower=0> limit
        normal_D ~ normal(mu_D, sigma_D);
        D = U * Phi(normal_D);


Threads and processes
---------------------

Stan has automatic support for using multiple cores, one per chain. Since 8
chains is a common number, that tends to match or exceed the number of cores
per CPU, which is helpful (not very many consumer CPUs have >8 cores). This
provides between-chain parallelization.

Stan has also introduced threading support for within-chain parallelization,
described at
https://www.r-bloggers.com/2019/08/speeding-up-bayesian-sampling-with-map_rect/,
which involves splitting your problem into "shards" and calculating each in
a separate thread (and thus core), and then using a map-reduce method to
combine the results.

I haven't gone down that route, because it's rare for me to be executing fewer
chains than I have cores.

See https://mc-stan.org/docs/2_26/stan-users-guide/parallelization-chapter.html.


Profiling
---------

Stan 2.26+ supports profiling (in a way); see
https://mc-stan.org/cmdstanr/articles/profiling.html.


Bridge sampling, generated quantities
-------------------------------------

- Bridge sampling slows things down, both in the Stan calculation and then in
  the processing of its output through the bridgesampling package. However,
  it is (unfortunately) not simple to switch the necessary calculations on/off
  easily, so they are baked in. See also my `Stan feature request
  <https://discourse.mc-stan.org/t/option-to-keep-constant-terms-in-log-probability-via-standard-sampling-syntax/20278>`_
  about this.

- "Generated quantities" (GQ) blocks can add significant time. These are not
  required for model comparison.

- If you have :math:`n` models, each with approximately a sampling time of
  :math:`t` and a GQ time of :math:`g`, then:

  - they will take :math:`(nt + ng)` to run in full;

  - it will take :math:`(nt + t + g)` to run all the models without the GQ
    blocks and then re-run the winning model with the GQ block back;

  - therefore, you should consider temporarily disabling your GQ blocks during
    model comparison if :math:`(n − 1)g > t`.


High-performance computing
==========================

Useful methods for your local cluster
-------------------------------------

Python 3
~~~~~~~~

No Python 3? Ask your administrators nicely, and if it remains unavailable,
install from source. For example:

.. code-block:: bash

    export INSTALLDIR=~/installation
    export PYTHONROOT="${INSTALLDIR}/pythonroot"
    export VENVDIR=~/python36_venv

    mkdir -p "${INSTALLDIR}"
    mkdir "${PYTHONROOT}"
    cd "${INSTALLDIR}"
    wget https://www.python.org/ftp/python/3.6.4/Python-3.6.4.tgz
    tar xvf Python-3.6.4.tgz
    cd Python-3.6.4
    ./configure --enable-optimizations --prefix="${PYTHONROOT}"
    make -j8
    make altinstall

    # Check Python works:
    "${PYTHONROOT}/bin/python3.6"

Onwards:

.. code-block:: bash

    # Now create a virtual environment:
    "${PYTHONROOT}/bin/bin/pip3.6" install venv
    "${PYTHONROOT}/bin/python3.6" -m venv "${VENVDIR}"

You could then create a file called ``~/activate_venv.sh``, like this:

.. code-block:: bash

    #!/bin/bash
    [[ $_ != $0 ]] || { echo "Script is a subshell; must be sourced"; exit 1; }
    VENVDIR=~/python36_venv
    . "${VENVDIR}/bin/activate"

and now you can activate your virtual environment simply via:

.. code-block:: bash

    . ~/activate_venv.sh

For example:

.. code-block:: bash

    . ~/activate_venv.sh
    pip install --upgrade pip
    pip install wheel
    pip install cardinal_pythonlib

You can run ``deactivate`` to exit the virtual environment.


Synchronizing your files to the cluster
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You could use a Git repository as the means of exchange, but that may be
undesirable for huge data files.

You could install Unison_ on the HPC machine, as below, and then a Unison
configuration file like this (on your local machine) will work:

.. code-block:: ini

    # MY_CLUSTER.prf

    # Place new files at the top of the list:
    sortnewfirst = true

    # Turn on ssh compression:
    rshargs = -C

    # Define local and remote directories to sync:
    root = /home/MY_LOCAL_USER/MY_LOCAL_PATH
    root = ssh://MY_CLUSTER//home/MY_CLUSTER_USER/MY_CLUSTER_PATH

    # Where should SSH find Unison on the remote (HPC cluster) machine:
    servercmd = /home/MY_CLUSTER_USER/local/bin/unison

    # Use on first run to test connection:
    # testServer = true

    # Ask no questions:
    batch = true

and if that is saved as ``~/.unison/MY_CLUSTER.prf``, you should now be able to
synchronize files with

.. code-block:: bash

    unison MY_CLUSTER


Installing Unison as a non-privileged user
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

First, use ``unison -version`` on your local machine to find out what version
you need. Here we'll aim for version 2.48.4 on an x86_64 architecture.

.. code-block:: bash

    # Debian method (assumes wget, dpkg)
    # https://askubuntu.com/questions/339/how-can-i-install-a-package-without-root-access

    export INSTALLDIR=~/installation
    export UNISONDIR="${INSTALLDIR}/unison"
    export DEBFILE=unison_2.48.4-1+b1_amd64.deb

    # Download the .deb package:
    mkdir -p "${INSTALLDIR}"
    wget "http://ftp.uk.debian.org/debian/pool/main/u/unison/${DEBFILE}" -P "${INSTALLDIR}"

    # Install:
    mkdir -p "${UNISONDIR}"
    dpkg -x "${INSTALLDIR}/${DEBFILE}" "${UNISONDIR}"

    # Test Unison:
    export UNISON="${UNISONDIR}/usr/bin/unison-2.48.4"
    "${UNISON}" -version

This is *much* easier than installing Ocaml and then Unison from source, and
worrying about which versions are required.


Help with SLURM
---------------

``myjobs.sh``:

.. code-block:: bash

    #!/bin/bash

    function join_by { local IFS="$1"; shift; echo "$*"; }
    function csv { join_by , $@; }

    USERNAME="${USER}"
    BIGSEP="=============================================================================="
    SMALLSEP="------------------------------------------------------------------------------"
    INFOSPEC="%.10i %.10P %10q %.20j %.8u %.2t %.5D %.16R %.40Z"

    # =============================================================================
    # Everyone's jobs
    # =============================================================================

    echo "${BIGSEP}"
    echo "Everyone's running jobs:"
    echo "${SMALLSEP}"

    # NJOBS=$(squeue --noheader --states=R | wc -l)
    # echo "There are ${NJOBS} jobs running."

    echo "Running jobs by QOS:"
    squeue --states=R --Format="qos" | sort | uniq -c

    echo "Pending jobs by QOS:"
    squeue --states=PD --Format="qos" | sort | uniq -c

    # echo "All running jobs:"
    # squeue --states=R --sort=+i --format="${INFOSPEC}"

    echo "${BIGSEP}"
    echo

    # =============================================================================
    # My jobs
    # =============================================================================

    mapfile -t RUNNING_JOB_IDS < <( squeue -u "${USERNAME}" --noheader --format="%i" --sort=+i --states=R )
    CSV_RUNNING_JOBS=$(csv ${RUNNING_JOB_IDS[*]})
    echo "${BIGSEP}"
    echo "Running jobs for ${USERNAME}: ${CSV_RUNNING_JOBS}"
    echo "${SMALLSEP}"
    for jobid in "${RUNNING_JOB_IDS[@]}"; do
        scontrol show job=${jobid}
    done
    # if [[ ! -z "${CSV_RUNNING_JOBS}" ]]; then
    #     sstat --jobs "${CSV_RUNNING_JOBS}" --format="JobID,NTasks,AveCPU,AveCPUFreq,AveVMSize,MaxVMSize,MaxDiskWrite"
    # fi
    echo "${BIGSEP}"
    echo

    echo "${BIGSEP}"
    echo "All jobs for user ${USERNAME}:"
    echo "${SMALLSEP}"
    squeue -u "${USERNAME}" --sort=+i --format="${INFOSPEC}"
    echo "${BIGSEP}"


Quick clusters
--------------

Or: suppose your favourite high-performance computing (HPC) environment
migrates to one with a short job length cap, and you wonder about doing it at
home, or via a commercial cloud?

The whole principle of parallel high-performance computing is to bring many
CPUs to a single problem (e.g. subdivisions of a common set of data). So the
standard design is a single central scheduling system plus multiple "compute
nodes", connected via a high-speed network. The central scheduling system, at
least, must have access to the user's data filesystem, but a common approach is
that each node can access the data filesystem (see e.g. `SLURM Overview
<https://slurm.schedmd.com/overview.html>`_). This allows user-installed
software to be run on the compute nodes. Nodes need to boot, though, so may
have cloned filesystems containing their minimal software (or might in
principle share a filesystem for this, though they are likely to need their own
filesystem for scratch space; HPC designs vary here). Typically, jobs run on a
single class of processor (e.g. "x86_64 CPU" or "GPU"), even if the cluster
offers multiple processor types.

Therefore:

- One approach is the "bare metal" one of a filesystem served by NFS, and
  nearly identical machines accessing it (such that they can run the same
  compiled code from the single filesystem -- e.g. they share a CPU class).

  - An extension to that is an orchestration system like SLURM_.
    "Bare metal + NFS + SLURM" is a fair description of lots of "proper" HPC
    setups.

- An alternative is a containerization system, like `Docker Swarm`_ or
  Singularity_. Singularity doesn't require containers to have root access,
  which can be a problem with Docker (see `Docker security
  <https://docs.docker.com/engine/security/>`_, though note also the
  `Docker rootless <https://docs.docker.com/engine/security/rootless/>`_ mode).

- As a very basic setup, you could use Docker containers to standardize your
  "analytical environment", connect them to the NFS filesystem, and start jobs
  manually in each container.

- You can even run SLURM within Docker; see

  - https://github.com/SciDAS/slurm-in-docker
  - http://www.hpcadvisorycouncil.com/events/2016/stanford-workshop/pdf/Kniep.DockerNetworkingSlurm.Gaikai.pdf
  - https://arxiv.org/abs/1509.08231

- See this 2018 article on containerization for HPC:
  https://thenewstack.io/roadmap-containers-for-high-performance-computing/.

Commercial providers include:

- Amazon AWS is one commercial cloud. A helpful guide to scientific computing
  using AWS is https://cloud-gc.readthedocs.io/, and a guide to creating HPC
  clusters is at https://jiaweizhuang.github.io/blog/aws-hpc-guide/, or there
  is Amazon's own guide at https://aws.amazon.com/hpc/getting-started/.

- Microsoft Azure is another. See
  https://docs.microsoft.com/en-us/azure/architecture/topics/high-performance-computing.

- Google Cloud is a third; see https://cloud.google.com/solutions/hpc and
  https://cloud.google.com/compute.


Docker example
~~~~~~~~~~~~~~

See the ``Dockerfile`` in this directory.
