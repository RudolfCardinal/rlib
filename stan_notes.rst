.. stan_speed.rst

.. _Ahn2017: https://pubmed.ncbi.nlm.nih.gov/29601060/
.. _Hains2018: https://pubmed.ncbi.nlm.nih.gov/30289167/
.. _Romeu2020: https://pubmed.ncbi.nlm.nih.gov/31735532/


Making Stan run faster
======================

The compiler environment
------------------------

You want RStan itself and your own C++ code to be compiled with the ``-O3``
(highest) level of optimization
(https://gcc.gnu.org/onlinedocs/gcc/Optimize-Options.html), and without the
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

This link (https://groups.google.com/g/stan-users/c/a96cURY9gVI?pli=1) suggests
that RStan automatically uses ``-O3`` for installation.

The old ``rstan::set_cppo()`` function is defunct and advises using your
``Makevars`` file.


General Stan/C++ coding
-----------------------

- Use fewer C++ statements; Stan applies an overhead to each.

- Vectorize everything that you can.

- In particular, vectorize sampling statements.

  In this context:

    .. code-block:: none

        data {
            int N_TRIALS;
            int<lower=0, upper=1> responded_right[N_TRIALS];
        }

  this method is slow:

    .. code-block:: none

        model {
            real p_choose_rhs;
            for (i in 1:N_TRIALS) {
                p_choose_rhs = ...
                responded_right[i] ~ bernoulli(p_choose_rhs);
            }
        }

  and this is faster, as it vectorizes the sampling statement:

    .. code-block:: none

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
  "logit space". Stan provides the ``softmax()`` function
  (https://mc-stan.org/docs/2_21/functions-reference/softmax.html), which
  should be used in preference to a home-brew version. It also provides a
  ``log_softmax()`` function, returning the natural log of the softmax.
  However, the reason for this function is to avoid underflow in some
  circumstances (e.g. https://stats.stackexchange.com/questions/436766/); "log
  probability" is obviously not the same as "logit" (log odds) and isn't useful
  for this purpose.


The model and parameterization
------------------------------

- Make the parameter space easy for Stan to explore.

- When a quantity is sampled from a Normal(mu, sigma) distribution, sample it
  from a N(0, 1) distribution and scale it:

    .. code-block:: none

        standard_normal_X ~ Normal(0, 1);
        X = sigma * X + mu;

  This is referred to as "noncentred parameterization" or the "Matt trick".

- Try to use "soft constraints", i.e. avoid hard pass/fail boundaries for the
  sampling algorithm (such as truncated distributions). In particular, consider
  e.g. the Ahn2017_, Hains2018_, Romeu2020_ method in which:

  - an unconstrained parameter A is sampled like this:

    .. code-block:: none

        real mu_A;
        real<lower=0> sigma_A;
        real A;

        mu_A ~ normal(0, 10);
        sigma_A ~ cauchy(0, 5);  // half-Cauchy because of <lower=0> limit
        A ~ normal(mu, sigma);

  - a positive parameter B is sampled like this:

    .. code-block:: none

        real mu_B;
        real<lower=0> sigma_B;
        real normal_B;
        real B;

        mu_B ~ normal(0, 1);
        sigma_B ~ cauchy(0, 5);  // half-Cauchy because of <lower=0> limit
        normal_B ~ normal(mu, sigma);
        B = exp(normal_B);

  - a parameter C in the range [0, 1] is sampled like this:

    .. code-block:: none

        real mu_C;
        real<lower=0> sigma_C;
        real normal_C;
        real C;

        mu_C ~ normal(0, 1);
        sigma_C ~ cauchy(0, 5);  // half-Cauchy because of <lower=0> limit
        normal_C ~ normal(mu, sigma);
        C = Phi(normal_C);  // equivalent to "inverse_probit(normal_C)"

    - The **probit** function is the quantile function (the inverse of the
      cumulative distribution function) for the standard normal
      distribution (https://en.wikipedia.org/wiki/Probit), and thus maps [0, 1]
      to [−∞, +∞]. In R, this is ``qnorm()``, as in ``q <- qnorm(p)``.

    - The **inverse probit** function is the cumulative distribution function
      (CDF) of the standard normal distribution, often written ``Φ()``. It maps
      [−∞, +∞] to [0, 1]. In R, this function is ``pnorm()``, as in ``p <-
      pnorm(q)``. In Stan, it is ``Phi()``
      (https://mc-stan.org/docs/2_21/stan-users-guide/logistic-probit-regression-section.html).

  - a parameter D in the range [0, U], where U is an upper limit, is sampled
    like this:

    .. code-block:: none

        real mu_D;
        real<lower=0> sigma_D;
        real normal_D;
        real D;

        mu_D ~ normal(0, 1);
        sigma_D ~ cauchy(0, 5);  // half-Cauchy because of <lower=0> limit
        normal_D ~ normal(mu, sigma);
        D = U * Phi(normal_D);  // equivalent to "inverse_probit(normal_C)"


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
  easily, so they are baked in. See also my feature request at
  https://discourse.mc-stan.org/t/option-to-keep-constant-terms-in-log-probability-via-standard-sampling-syntax/20278.

- "Generated quantities" (GQ) blocks can add significant time. These are not
  required for model comparison.

- If you have n models, each with approximately a sampling time of t and a
  GQ time of g, then:

  - they will take (nt + ng) to run in full;

  - it will take (nt + t + g) to run all the models without the GQ blocks and
    then re-run the winning model with the GQ block back;

  - therefore, you should consider temporarily disabling your GQ blocks during
    model comparison if (n - 1)g > t.
