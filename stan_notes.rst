.. stan_speed.rst

.. _Ahn2017: https://pubmed.ncbi.nlm.nih.gov/29601060/
.. _CRIU: https://criu.org/
.. _Docker Swarm: https://docs.docker.com/engine/swarm/
.. _Haines2018: https://pubmed.ncbi.nlm.nih.gov/30289167/
.. _Kanen2019: https://pubmed.ncbi.nlm.nih.gov/31324936/
.. _Romeu2020: https://pubmed.ncbi.nlm.nih.gov/31735532/
.. _Singularity: https://sylabs.io/singularity/
.. _SLURM: https://slurm.schedmd.com/
.. _Unison: https://www.cis.upenn.edu/~bcpierce/unison/
.. _Yao2018: https://arxiv.org/abs/1802.02538


Fast, accurate Stan
===================

C++ compiler optimization
-------------------------

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

You may find that RStan has done the work for you at some point, in which case
your ``Makevars`` file may look like this:

.. code-block:: ini

    # created by rstan at Fri Jun 28 14:04:38 2013
    CXXFLAGS = -O3 -pipe   $(LTO)     #set_by_rstan
    R_XTRA_CPPFLAGS =  -I$(R_INCLUDE_DIR)      #set_by_rstan

... but if not, ensure ``CXXFLAGS`` contains ``-O3``.

`This link <https://groups.google.com/g/stan-users/c/a96cURY9gVI?pli=1>`_
suggests that RStan automatically uses ``-O3`` for installation.

The old ``rstan::set_cppo()`` function is defunct and advises using your
``Makevars`` file.


General Stan/C++ coding
-----------------------

- Use fewer C++ statements; Stan applies an overhead to each.

  If you're thinking of making a cut-down model by setting parameters to zero
  in code for a more complex model, be aware that this might be much slower
  (e.g. five-fold in one example) than cutting out the unnecessary code.

- Don't check constraints twice, or apply unnecessary constraints. See
  https://mc-stan.org/docs/2_26/stan-users-guide/avoiding-validation.html.
  So, for example, if you use ``real<lower=0> some_standard_deviation`` but
  then have another method of enforcing the zero lower bound, you could cut out
  the ``<lower=0>``. (Removing this check cut execution time by 4–17% in one
  quite simple scenario.)

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


Parameterizing the model
------------------------

- Make the parameter space easy for Stan to explore.

- When a quantity is sampled from a Normal(mu, sigma) distribution, consider
  sampling it from a N(0, 1) distribution and scale it:

    .. code-block:: C++

        standard_normal_X ~ std_normal();  // = Normal(0, 1) but faster
        X = sigma * standard_normal_X + mu;

  This is referred to as "noncentred parameterization" or the "Matt trick".

  Think of it this way: if you use ``normal(mu, sigma)``, Stan is having to
  sample from a "moving target", whereas N(0, 1) is a "stationary target".

- Try to use "soft constraints", i.e. avoid hard pass/fail boundaries (such as
  truncated distributions) for the sampling algorithm.

- In particular, consider the method of sampling means from underlying
  standard normal N(0, 1) distributions, and standard deviations from similar
  (e.g. positive-half-normal, positive-half-Cauchy) distributions.
  Transformations are then applied to reach the desired parameter "space". For
  example, Ahn2017_, Haines2018_, and Romeu2020_ use a method that, when
  expressed in Stan syntax, is as follows:

  - an unconstrained parameter A is sampled like this:

    .. code-block:: C++

        parameters {
            real mu_A;
            real<lower=0> sigma_A;
            real A;
        }
        model {
            mu_A ~ normal(0, 10);
            sigma_A ~ cauchy(0, 5);  // half-Cauchy because of <lower=0> limit
            A ~ normal(mu_A, sigma_A);
        }

  - a positive parameter B is sampled like this:

    .. code-block:: C++

        parameters {
            real mu_B;
            real<lower=0> sigma_B;
            real raw_normal_B;
        }
        transformed parameters {
            real B = exp(raw_normal_B);
        }
        model {
            mu_B ~ std_normal();  // = Normal(0, 1) but faster
            sigma_B ~ cauchy(0, 5);  // half-Cauchy because of <lower=0> limit
            raw_normal_B ~ normal(mu_B, sigma_B);
        }

  - a parameter C in the range [0, 1] is sampled like this:

    .. code-block:: C++

        parameters {
            real mu_C;
            real<lower=0> sigma_C;
            real raw_normal_C;
        }
        transformed parameters {
            real C = Phi(raw_normal_C);  // equivalent to "inverse_probit(raw_normal_C)"
        }
        model {
            mu_C ~ std_normal();  // = Normal(0, 1) but faster
            sigma_C ~ cauchy(0, 5);  // half-Cauchy because of <lower=0> limit
            raw_normal_C ~ normal(mu_C, sigma_C);
        }

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

        parameters {
            real mu_D;
            real<lower=0> sigma_D;
            real raw_normal_D;
        }
        transformed parameters {
            real D = U * Phi(raw_normal_D);
        }
        model {
            mu_D ~ normal(0, 1);
            sigma_D ~ cauchy(0, 5);  // half-Cauchy because of <lower=0> limit
            raw_normal_D ~ normal(mu_D, sigma_D);
        }

Presentationally, one can show posterior values/distributions of the "unit
normal" variable, or the transformed value (e.g. Ahn2017_, pp. 31, 47;
:math:`K` or :math:`K′` in Haines2018_, pp. 2544, 2546, 2553; Romeu2020_, p.
107711). See below for cautions regarding the interpretation of transformed
values.

Unsure what a half-Cauchy distribution looks like? Try this:

.. code-block:: R

    curve(dnorm(x, mean = 0, sd = 1), 0, 5, col = "blue", ylab = "density")
    curve(dcauchy(x, location = 0, scale = 1), 0, 5, col = "red", add = TRUE)

This isn't the only way. Note that ``uniform`` is an undesirable (hard-edged)
alternative, but a ``beta`` distribution may be perfectly useful for a [0,1]
parameter. (If you use ``beta``, you might choose e.g. "normally distributed
deviations about a beta-distributed mean"; e.g. Kanen2019_. In theory such
values can go outside the range [0,1] but you can then ``reject()`` them.)


The interpretation of transformed parameters
--------------------------------------------

Be careful not to misinterpret transformed parameters.

Let's use the example of the transformed parameter B above.

Note that the mean of B in "B space" is NOT the mean of sampled values of
``exp(mu_B)``. (Though it is, of course, the mean of sampled values of B
itself, and the mean of exponentiated values of ``raw_normal_B``.) Likewise,
the standard deviation of B in "B space" is NOT ``exp(sigma_B)``! As a
demonstration in R:

.. code-block:: R

    set.seed(1)  # for reproducibility
    mu_B <- 5
    sigma_B <- 2
    raw_normal_B <- rnorm(n = 1000, mean = mu_B, sd = sigma_B)
    B <- exp(raw_normal_B)

    print(mean(raw_normal_B))  # about 5
    print(exp(mu_B))  # 148.4
    print(mean(B))  # about 1280
    print(mean(exp(raw_normal_B)))  # identical to mean(B); about 1280

    print(sd(raw_normal_B))  # about 2
    print(exp(sigma_B))  # 7.389
    print(sd(B))  # about 10100
    print(sd(exp(raw_normal_B)))  # identical to sd(B); about 10100

Why is this relevant? Because sometimes, `for efficiency
<https://mc-stan.org/docs/2_18/reference-manual/program-block-generated-quantities.html>`_,
you will not store the things you care about in the "transformed parameters"
block, and must therefore generate them in the "generated quantities" block.

Here's an example (which is highly inelegant!) in which the transformed means
are not used directly within "transformed parameters" but are calculated within
"generated quantities":

.. code-block:: R

        # Load RStan
        library(rstan)
        options(mc.cores = parallel::detectCores())
        rstan_options(auto_write = TRUE)

        # Generate some data
        set.seed(1)  # for reproducibility
        N_SUBJECTS <- 100
        N_OBSERVATIONS_PER_SUBJECT <- 100
        N_OBSERVATIONS <- N_SUBJECTS * N_OBSERVATIONS_PER_SUBJECT
        RAW_OVERALL_MEAN <- 1  # in "standard normal" space
        RAW_BETWEEN_SUBJECTS_SD <- 0.5  # in "standard normal" space
        RAW_WITHIN_SUBJECTS_SD <- 0.2  # in "standard normal" space
        EPSILON <- 0.05  # tolerance
        repeat {
            # Fake randomness so we actually end up with a mean/SD that is
            # what we want, within the tolerance of EPSILON_*.
            raw_subject_deviation_from_overall_mean <- rnorm(
                n = N_SUBJECTS, mean = 0, sd = RAW_BETWEEN_SUBJECTS_SD
            )
            if (abs(mean(raw_subject_deviation_from_overall_mean)) <=
                        EPSILON &&
                    abs(sd(raw_subject_deviation_from_overall_mean) -
                        RAW_BETWEEN_SUBJECTS_SD) <= EPSILON) {
                break
            }
        }
        subject <- rep(1:N_SUBJECTS, each = N_OBSERVATIONS_PER_SUBJECT)
        repeat {
            # Likewise, "constrained randonmess":
            error <- rnorm(
                n = N_OBSERVATIONS, mean = 0, sd = RAW_WITHIN_SUBJECTS_SD)
            if (abs(mean(error)) <= EPSILON &&
                    abs(sd(error) - RAW_WITHIN_SUBJECTS_SD) <= EPSILON) {
                break
            }
        }
        raw_y <- (
            RAW_OVERALL_MEAN +
            raw_subject_deviation_from_overall_mean[subject] +
            error
        )  # in "standard normal" space
        y <- exp(raw_y)
        standata <- list(
            N_SUBJECTS = N_SUBJECTS,
            N_OBSERVATIONS = N_OBSERVATIONS,
            subject = subject,
            y = y
        )

        # Analyse it with Stan
        model_code <- '
            // Single-group within-subjects design.
            // The prefix "raw_" means "in standard normal (Z) space".
            data {
                int<lower=1> N_SUBJECTS;
                int<lower=1> N_OBSERVATIONS;
                int<lower=1> subject[N_OBSERVATIONS];
                real y[N_OBSERVATIONS];
            }
            parameters {
                real raw_overall_mean;
                real<lower=0> raw_between_subjects_sd;
                real<lower=0> raw_within_subject_sd;

                vector[N_SUBJECTS] raw_subject_deviation_from_overall_mean;
            }
            transformed parameters {
                vector[N_SUBJECTS] raw_subject_mean = (
                    raw_overall_mean +  // real
                    raw_subject_deviation_from_overall_mean  // vector
                );
            }
            model {
                vector[N_OBSERVATIONS] raw_predicted;

                // Sample parameters
                raw_overall_mean ~ std_normal();
                raw_between_subjects_sd ~ cauchy(0, 5);
                raw_within_subject_sd ~ cauchy(0, 5);
                raw_subject_deviation_from_overall_mean ~ normal(
                    0, raw_between_subjects_sd);

                // Conceptually, raw_subject_mean is calculated at this point.

                // Calculate the per-subject mean for each observation:
                for (i in 1:N_OBSERVATIONS) {
                    raw_predicted[i] = raw_subject_mean[subject[i]];
                }

                // Fit to data:
                //      y ~ exp(normal(...)), or
                //      log(y) ~ normal(...), or
                //      y ~ lognormal(...):
                y ~ lognormal(raw_predicted, raw_within_subject_sd);
            }
            generated quantities {
                real transformed_overall_mean = exp(raw_overall_mean);
                real mean_of_transformed_subject_means = mean(
                    exp(raw_subject_mean)
                );
            }
        '
        fit <- rstan::stan(
            model_code = model_code,
            model_name = "Test model",
            data = standata
        )
        print(fit)

        # Means from Stan:
        # - raw_overall_mean = 0.98 (95% HDI 0.87-1.07), accurate
        # - raw_between_subjects_sd = 0.48 (HDI 0.42-0.56), accurate
        # - raw_within_subjects_sd = 0.20 (HDI 0.20-0.21), accurate
        # - transformed_overall_mean = 2.68 (HDI 2.38-2.90)
        #   ... relevant but NOT the same as mean(y)
        # - mean_of_transformed_subject_means = 3.00 (HDI 2.99-3.02)
        #   ... more likely to be what you want.
        #
        # Compare to values from R:
        print(mean(raw_y))  # 0.980
        print(sd(raw_subject_deviation_from_overall_mean))  # 0.479
        print(sd(error))  # 0.202
        print(mean(y))  # 3.06
        # ... noting that if all subjects don't have the same number of
        #     observations, a different calculation would be required to
        #     match mean_of_transformed_subject_means.

In this case, the point to emphasize is that "mean(exp(raw_overall_mean))" is
not the same as "mean(exp(raw_overall_mean + a normally distributed deviation
from 0))". That can be demonstrated simply again in R:

.. code-block:: R

    set.seed(1)
    deviations <- rnorm(n = 100000, mean = 0, sd = 1)
    mean(0 + deviations)  # -0.00224
    mean(exp(0 + deviations))  # 1.648
    exp(0)  # 1

Attempting to recover standard deviations in "parameter space" is unlikely to
be meaningful. If ``z ~ N(0, sigma)`` and ``y = exp(z)`` then ``y`` is not
normally distributed, so it has no "standard deviation"; the relevant SD is
that of ``z``, which will be estimated by Stan directly.


Group-level testing
-------------------

I tend to follow the "cell means" approach outlined in Kanen2019_ (see the
"Interpretation of results" section).


Homogeneity of variance
~~~~~~~~~~~~~~~~~~~~~~~

In general, it is desirable not to assume homogeneity of variance, and instead
to model (and test for) variance differences between groups. However, for "low
*n*" studies, there may be insufficient data to estimate the variances
separately. In this situation, you may find that even a very simple conceptual
model does not converge, and you may have to assume homogeneity of variance
(such models will also run faster). The assumption of homogeneity of variance
is of course the norm in traditional null-hypothesis significance testing
methods such as ANOVA.


Variational inference
---------------------

You will be tempted to use Stan's variational Bayes approximation (variational
inference), e.g. via ``rstan::vb()``, because it is much quicker. But it can be
wrong; see e.g. Yao2018_.


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
migrates to one with a short job length cap
(https://docs.hpc.cam.ac.uk/hpc/user-guide/long.html), and you wonder about
doing it at home, or via a commercial cloud?

Note that this problem might go away via checkpointing:

- In Stan:
  https://discourse.mc-stan.org/t/current-state-of-checkpointing-in-stan/12348/28.
- There are generic checkpoint tools such as CRIU_.
- SLURM supports ``scontrol checkpoint create JOB_ID`` and ``scontrol
  checkpoint restart JOB_ID``. Its support appears built-in via DMTCP and/or
  CRIU. See

  - https://slurm.schedmd.com/SLUG16/ciemat-cr.pdf.
  - https://slurm.schedmd.com/scontrol.html
  - ``man scontrol``
  - https://slurm.schedmd.com/sbatch.html
  - https://www.nersc.gov/assets/Uploads/Checkpoint-Restart-20191106.pdf
  - http://community.dur.ac.uk/ncc.admin/preemption/
  - https://hpc-aub-users-guide.readthedocs.io/en/latest/octopus/jobs.html

But otherwise...

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
