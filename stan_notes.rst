.. stan_speed.rst

.. _Ahn2017: https://pubmed.ncbi.nlm.nih.gov/29601060/
.. _BetancourtGirolami2013: https://arxiv.org/abs/1312.0906
.. _Bowling2009: https://www.jiem.org/index.php/jiem/article/view/60
.. _CRIU: https://criu.org/
.. _Docker Swarm: https://docs.docker.com/engine/swarm/
.. _Gelman2006: https://doi.org/10.1214/06-BA117A
.. _Haines2018: https://pubmed.ncbi.nlm.nih.gov/30289167/
.. _Howell1997: https://en.wikipedia.org/wiki/Special:BookSources?isbn=0-534-51993-8
.. _Kanen2019: https://pubmed.ncbi.nlm.nih.gov/31324936/
.. _Klein2016: https://doi.org/10.1214/15-BA983
.. _OpenCL: https://en.wikipedia.org/wiki/OpenCL
.. _Romeu2020: https://pubmed.ncbi.nlm.nih.gov/31735532/
.. _Simpson2017: https://doi.org/10.1214/16-STS576
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

  The obvious situation that I wondered about was if, for example, if you use
  ``real<lower=0> some_standard_deviation`` but then have another method of
  enforcing the zero lower bound (say, a bridgesampling-compatible sampling
  function that does ``target += negative_infinity()`` if the variable is out
  of bounds), you could cut out the ``<lower=0>``. (Removing this check cut
  execution time by 4–17% in one quite simple scenario.)

  However, see
  https://groups.google.com/forum/#!msg/stan-users/4gv3fNCqSNk/VonPUkdcZuUJ,
  which appears to suggest (in 2012) that it is important to include these
  constraints: "A common mistake in .stan files is the failure to restrict the
  support of a parameter in the parameters {} block and to then attempt to
  restrict the support of a parameter with a prior in the model {} block...
  restrict the support of parameters when appropriate; if no prior is
  specified, it is implicitly uniform over the parameter's support... if you
  want to impose an informative prior, it should put non-zero (but perhaps
  arbitrarily small) mass over the entire support of that parameter".

  But then the 2021 Stan Reference Manual says
  (https://mc-stan.org/docs/2_26/reference-manual/sampling-statements-section.html)
  that truncated distributions are now explicitly supported and the process of
  doing ``target += negative_infinity()`` if the result is out of bounds is
  exactly equivalent. (And the bad examples given by Goodrich in 2012 didn't
  do that.) (Note also that vectorized truncated distributions are not yet
  supported, as of Stan 2.26.)

  In practice, though, with rstan 2.21.2, removal of these constraints often
  leads to failure with:

  .. code-block:

    Chain 1: Rejecting initial value:
    Chain 1:   Log probability evaluates to log(0), i.e. negative infinity.
    Chain 1:   Stan can't start sampling from this initial value.

  **More on "rejecting initial value":**

  - https://discourse.mc-stan.org/t/rejecting-initial-value/6258/2
  - https://discourse.mc-stan.org/t/initial-value-rejected/5040/2
  - https://mc-stan.org/docs/2_27/reference-manual/initialization.html
  - Using ``init="0"`` initializes all variables to 0 "on the unconstrained
    support", which is typically in the middle.
  - Stan works with unconstrained values, so creates an unconstrained variable
    based on the constraints declared:
    https://mc-stan.org/docs/2_27/reference-manual/variable-transforms-chapter.html#variable-transforms.chapter

  ... so this error usually reflects the lack of constraints;
  **keep constraints** if unsure, at least for now. This may improve with
  future versions of Stan.

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

- The other useful reformulation of softmax:

  .. code-block:: none

                P[i] =
    softmax(X, β)[i] = exp(β⋅X[i]) / Σ_j{ exp(β⋅X[j]) }

  For a two-stimulus version, with X_i and X_j being the "values":

  .. code-block:: none

    softmax(X, β)[i] = exp(β⋅X_i) / [ exp(β⋅X_i) + exp(β⋅X_j) ]

    Divide top and bottom by exp(β⋅X_i):
                     = 1          / [ 1          + exp(β⋅X_j)/exp(β⋅X_i) ]
                     = 1 / [ 1 + exp(β⋅X_j - β⋅X_i) ]
                     = 1 / [ 1 + exp(β⋅[X_j - X_i]) ]
                     = 1 / [ 1 + exp(-β⋅[X_i - X_j]) ]

  But since logit(p) = log(odds) = log(p / [1 - p]), we can derive (cheat):

  .. code-block:: python

    # Octave (pkg load symbolic; syms X Y; ...)?
    # Maxima?
    # SymPy? This is clearer than most! https://www.sympy.org/

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # SymPy method
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    from sympy import *
    from sympy.abc import x

    init_printing(use_unicode=True)  # optional!
    # If you do "pip install ipython jupyterlab notebook" then you can run
    # "jupyter notebook"  from a scratch directory, create a new notebook,
    # and run this code; then expressions will be printed nicely via LaTeX.
    # Don't use print() for this; just type the expression (e.g. "pn").

    # Symbols:
    beta = Symbol("beta", real=True)
    p = Symbol("p", real=True)
    X_i, X_j, X_k = symbols("X_i, X_j, X_k", real=True)
    X = IndexedBase("X", real=True)  # a collection of reals that can be indexed
    n = Symbol("n", integer=True, positive=True)
    i = Idx("i")  # an index; do NOT specify (1, n) for range 1...n; see below
    j = Idx("j")  # an index

    # Functions:
    odds = Lambda(p, p / (1 - p))
    logit = Lambda(p, log(odds(p)))

    # The two-choice situation:
    p2 = exp(beta * X_i) / (exp(beta * X_i) + exp(beta * X_j))
    print(simplify(logit(p2)))  # beta*(X_i - X_j)

    # Some concrete numbers for the two-choice situation:
    concrete2 = {beta:1.0, X_i:0.5, X_j:0.5}
    print(p2.evalf(subs=concrete2))  # 0.5
    print(logit(p2).evalf(subs=concrete2))  # 0

    # A three-choice version:
    p3 = exp(beta * X_i) / (exp(beta * X_i) + exp(beta * X_j) + exp(beta * X_k))
    print(simplify(logit(p3)))  # X_i*beta - log(exp(X_j*beta) + exp(X_k*beta))

    # The n-choice situation:
    pn = exp(beta * Indexed(X, i)) / Sum(exp(beta * Indexed(X, j)), (j, 1, n))
    print(simplify(logit(pn)))  # no simple expression
    # ... beta*X[i] + log(1/(-exp(beta*X[i]) + Sum(exp(beta*X[j]), (j, 1, n))))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Try to reduce from the general to the specific, to learn SymPy a little:
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    p2b = pn.subs(i, 1).subs(n, 2).doit()
    # ... fails if j = Idx("j", (1, n)) rather than just j = Idx("j")
    print(simplify(logit(p2b)))  # beta*(X[1] - X[2])

    # Concrete instantiation of this derived two-choice situation:
    concrete2b = {beta:1.0, X[1]:0.5, X[2]:0.5}
    print(p2b.evalf(subs=concrete2b))  # 0.5
    print(logit(p2b).evalf(subs=concrete2b))  # 0


Parameterizing the model
------------------------

Parameterization: general
~~~~~~~~~~~~~~~~~~~~~~~~~

- Make the parameter space easy for Stan to explore.

- When a quantity is sampled from a :math:`N(\mu, \sigma)` distribution,
  consider sampling it from a :math:`N(0, 1)` distribution and scaling it:

    .. code-block:: C++

        standard_normal_X ~ std_normal();  // = Normal(0, 1) but faster
        X = sigma * standard_normal_X + mu;

  This is referred to as "noncentred parameterization" or the "Matt trick".

  Think of it this way: if you use ``normal(mu, sigma)``, Stan is having to
  sample from a "moving target", whereas N(0, 1) is a "stationary target".

- Try to use "soft constraints", i.e. avoid hard pass/fail boundaries (such
  as truncated distributions) for the sampling algorithm.

- Unsure what a half-Cauchy distribution looks like? Try this:

  .. code-block:: R

    curve(dnorm(x, mean = 0, sd = 1), 0, 5, col = "blue", ylab = "density")
    curve(dcauchy(x, location = 0, scale = 1), 0, 5, col = "red", add = TRUE)

Regarding reparameterization, see also:

- https://www.occasionaldivergences.com/post/non-centered/: explains that
  **divergent transitions (divergences)** indicate that Stan's Hamiltonian Monte
  Carlo algorithm is having trouble exploring the posterior distribution, and
  that **exceeding the maximum treedepth** is a warning about inefficiency
  rather than lack of model validity.

- https://mc-stan.org/docs/2_26/stan-users-guide/reparameterization-section.html:
  notes that the Cauchy is sometimes a tricky distribution and a candidate for
  reparameterization, and describes non-centred parameterization in general.

  - But see Gelman2006_, who recommends the half-Cauchy (p. 528) as a prior for
    standard deviations.
  - ... and even that Stan page uses ``sigma ~ cauchy(0, 5)`` in one of its
    reparameterized examples.

  - This is examined at
    https://stats.stackexchange.com/questions/346034/choosing-prior-for-sigma2-in-the-normal-polynomial-regression-model-y-i,
    which refers to Simpson et al. (2014), published as Simpson2017_. Simpson
    et al. discuss this on p. 8: the half-normal being potentialy too
    "light-tailed" but the half-Cauchy giving poor numerical behaviour. They
    argue for another, exponential, distribution.

 - Klein2016_ note that the half-normal distribution performs perfectly well as
   the prior for standard deviation (p. 1096).

- https://groups.google.com/g/stan-users/c/PkQxfc_QyGg: some 2015 discussion of
  the technique. See also BetancourtGirolami2013_.

- https://stats.stackexchange.com/questions/473386/matts-trick-reparametrization-makes-my-models-slower-not-faster:
  an example when the reparameterization makes things worse, not better.


Parameterization: Ahn method (everything is standard normal)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Consider the method of sampling means from underlying standard normal N(0, 1)
distributions, and standard deviations from similar (e.g. positive-half-normal,
positive-half-Cauchy) distributions. Transformations are then applied to reach
the desired parameter "space". For example, Ahn2017_ (for the hBayesDM
package), Haines2018_, and Romeu2020_ use a method that, when expressed in Stan
syntax, is as follows:

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
            real C = Phi_approx(raw_normal_C);
            // ... equivalent to "inverse_probit(raw_normal_C)"
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
      <https://mc-stan.org/docs/2_21/stan-users-guide/logistic-probit-regression-section.html>`_
      or ``Phi_approx()`` (as used by Ahn2017_, p. 39). ``Phi_approx`` is
      "close and much more efficient"
      (https://discourse.mc-stan.org/t/reparameterize-in-a-hierarchical-model/1833;
      see also
      https://mc-stan.org/docs/2_21/functions-reference/Phi-function.html and
      Bowling2009_).

- a parameter D in the range [0, U], where U is an upper limit, is sampledlike
  this:

    .. code-block:: C++

        parameters {
            real mu_D;
            real<lower=0> sigma_D;
            real raw_normal_D;
        }
        transformed parameters {
            real D = U * Phi_approx(raw_normal_D);
        }
        model {
            mu_D ~ normal(0, 1);
            sigma_D ~ cauchy(0, 5);  // half-Cauchy because of <lower=0> limit
            raw_normal_D ~ normal(mu_D, sigma_D);
        }

- **Beware:** the half-Cauchy(0, 5) prior for intersubject SDs may have been an
    error and they appear to have replaced it (e.g. Romeu 2020, and later
    versions of hBayesDM) with half-Normal(0, 0.2). See
    ``tests/explore_priors.R``. (But I've still had convergence problems with
    their technique and σ ~ N(0, 0.2).)


**Practicalities**

For a family of models with subsets of parameters, one option is to code the
models to use all parameters. Then, for models that don't use a given
parameter, we declare/initialize the per-subject effects as constants in
``transformed data``, rather than in ``transformed parameters``.

Finally, we must put the calculations in varying places across different types
of model. What is described above holds for between-subjects designs. Then:

-   SINGLE GROUP. Sample each parameter (per subject) from :math:`N(0, 1)`,
    which takes us directly to the result of the "transformation 2" step; then
    transform it as in the "transformation 3" step above.

-   WITHIN-SUBJECTS DESIGNS (a subject can be in several groups). This means
    you can't calculate "per-subject" final values. One could calculate within
    the ``model`` rather than the ``transformed parameters`` block. But
    extracting the transformed values is likely to be helpful. In which case,
    declare an array or matrix such as

    .. code-block:: C++

          real<lower=..., upper=...> s_g_param[N_SUBJECTS, N_GROUPS];
          matrix<lower=..., upper=...>[N_SUBJECTS, N_GROUPS] s_g_param;

    and calculate combinations there. A matrix is probably preferable
    [https://mc-stan.org/docs/2_18/stan-users-guide/indexing-efficiency-section.html].

So, for subject-within-group work:

*Sampling* in the ``parameters`` or ``model`` block:

1.  Per-group means are initially sampled in :math:`N(0, 1)` space.

2.  Per-group intersubject SDs are sampled in half-normal :math:`N(0, 0.2)^+`
    space.

3.  Per-subject effects (in between-subjects designs, each subject's deviation
    from its group mean; etc.) are initially sampled in :math:`N(0, 1)` space.

*Transformations* in the ``transformed parameters`` block:

1.  Per-subject effects  are then transformed to :math:`N(0, intersubject SD)`.

2.  Subject values are calculated in "Stan parameter space" as:

    .. code-block::

        subject_value = group_mean [S1] + subject_specific_effect [T1]

3.  We then convert from "Stan (unit normal) parameter space" to "task
    parameter space". This depends on our target parameter:

    -   Bounded parameters are inverse probit-transformed to :math:`(0, 1)`,
        then scaled; e.g. a range of :math:`(0, 7)` is given by:
        ``y = Phi_approx(x) * 7``.

    -   Unbounded positive parameters are exponentially transformed to
        :math:`(0, +\infty)` using ``y = exp(x)``.

You might want to label parameters that are in "standard normal" (raw) space,
rather than "task parameter space", e.g. with a prefix like ``raw_``.

*Priors* are therefore, approximately:

-   For everything, via temporary "raw" variable :math:`r`:

    :math:`\mu_{\mathrm group} \textasciitilde N(0, 1)`

    :math:`\sigma_{\mathrm group} \textasciitilde N(0, 0.2)^+`

    :math:`r_{\mathrm subject} \textasciitilde N(\mu_{\mathrm group}, \sigma_{\mathrm group})`

-   For bounded group means in range :math:`(L, U)`:

    :math:`x_{\mathrm subject} = L + (U - L) \cdot \phi(r_{\mathrm subject})`

-   For unbounded positive means:

    :math:`x_{\mathrm subject} = {\mathrm e}^{r_{\mathrm subject}}`

**Presentation**

One can show posterior values/distributions of the "unit normal" variable, or
the transformed value (e.g. Ahn2017_, pp. 31, 47; :math:`K` or :math:`K′` in
Haines2018_, pp. 2544, 2546, 2553; Romeu2020_, p. 107711). See below for
cautions regarding the interpretation of transformed values.

**Advantages**

A major advantage is of being able to operate in an unconstrained space
throughout, then constrain at the end if required (rather than e.g. having a
constrained parameter to which you add a deviation that might take it out of
its constraints).

**Disadvantage**

-   This obviously affects the priors a bit.

-   It's a bit fiddlier to extract the transformed parameters of interest.

-   It doesn't converge in some of my models, whereas direct sampling converged
    fine.


Parameterization: direct method
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Another way is to sampling directly from the distributions of interest. For
example, using a subjects-within-groups design:

**Sampling** in the ``parameters`` or ``model`` block:

1.  Per-group means are sampled in bounded parameter space, with sensible
    per-parameter priors.

2.  Per-group intersubject SDs are sampled in half-normal
    :math:`N(0, SD_prior)^+` space, e.g. :math:`N(0, 0.05)^+` for a parameter
    bounded [0, 1].

3.  Per-subject effects (in between-subjects designs, each subject's deviation
    from its group mean; etc.) are initially sampled in :math:`N(0, 1)` space.
    [SAME AS ROMEU.]

**Transformations** in the ``transformed parameters`` block:

1.  Per-subject effects  are then transformed to :math:`N(0, intersubject SD)`
    space. [SAME AS ROMEU.]

2.  Subject values are calculated as:

    .. code-block::

        subject_value = group_mean [S1] + subject_specific_effect [T1]

    and then bounded (clipped, but without potential for sampling
    failure) in parameter space.

**Advantages:**

-   Convergence, in one example of mine. Took maximum :math:`\^{R}` from ~160
    to ~1, where other measures hadn't helped.

    Why? Initialization parameters were at 0 (raw), meaning that bounded
    parameters start at the middle of the range, since (for bounded parameters)
    ``probit(0) = pnorm(q = 0, mean = 0, sd = 1) = 0.5``, and (for unbounded
    positive parameters) :math:`e^0 = 1`. But in diagnostic plots, a lot got
    stuck at 0.

-   Parameters are directly meaningful (no need to jump through hoops in
    ``generated quantities`` to get useful values out).

**Disadvantages:**

-   Clipping, potentially. You could ``reject()`` out-of-bounds values as
    an alternative.


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
    #   ... relevant (estimates exp(RAW_OVERALL_MEAN)), but NOT mean(y)
    # - mean_of_transformed_subject_means = 3.00 (HDI 2.99-3.02)
    #   ... potentially also of interest.
    #
    # Compare to values from R:
    print(mean(raw_y))  # 0.980
    print(sd(raw_subject_deviation_from_overall_mean))  # 0.479
    print(sd(error))  # 0.202
    print(exp(RAW_OVERALL_MEAN))  # 2.718
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

    # This is because of the intrinsic difference between mean(transform(x))
    # transform(mean(x)). It doesn't even depend on random noise:
    zero_sum_deviations <- rep(c(-1, 1), times = 100)
    mean(zero_sum_deviations)  # exactly 0
    sum(zero_sum_deviations)  # exactly 0
    mean(exp(0 + zero_sum_deviations))  # 1.543

Attempting to recover standard deviations in "parameter space" is unlikely to
be meaningful. If ``z ~ N(0, sigma)`` and ``y = exp(z)`` then ``y`` is not
normally distributed, so it has no "standard deviation"; the relevant SD is
that of ``z``, which will be estimated by Stan directly.

Which transformed parameter should you report as your posterior? For example,
in a single-group, multi-subject, within-subjects design, do you want (a) the
transformed version of the "underlying" (e.g. normally distributed) group mean,
or (b) the mean of the transformed per-subject means?

Let's illustrate this with a very basic example, using the reciprocal
transformation between speed ("underlying") and time ("transformed") for a 100m
race. Suppose five runners, some of them admittedly quite slow, race at 2, 4,
6, 8, and 10 m/s. Their mean speed is 6 m/s. Their times will be 50, 25, 16.67,
12.5, and 10 s, for a mean time of 22.83 s. But if a hypothetical person ran at
the "average speed" of 6 m/s, they would take 16.67 s — and if they ran the
"average time" of 22.83 s, they would be running at 4.38 m/s. So you could
report the mean speed (sensible in this example), but then (a) "the time taken
by a person running at the group's mean speed" (16.67 s), or the (b) "mean
time" (22.83 s).

In the context of a cognitive model of a task, therefore, do we want (a) "the
parameter used by a hypothetical subject of [group] mean underlying
normally-distributed raw parameter", or "the mean of the parameters used by our
subjects"?

Looking at the `hBayesDM <https://ccs-lab.github.io/hBayesDM/>`_ code for the
go/no-task, `gng_m1.stan
<https://github.com/CCS-Lab/hBayesDM/blob/develop/commons/stan_files/gng_m1.stan>`_,
where ``N`` is the number of subjects and ``T`` the maximum number of trials
per subject, we see that conceptually it (1) draws group means (``mu_pr``)
and standard deviations (``sigma``) from predetermined priors in N(0, 1)
space; (2) uses these to scale unit-normal variables for three parameters
(``xi_pr``, ``ep_pr``, ``rho_pr``) into "parameter space" (``xi``, ``ep``,
``rho``); (3) performs the cognitive calculations using those parameters; (4)
in the "generated quantiies" block, transforms the group-level means
(``mu_pr``) into "parameter space" and reports these (``mu_xi``, ``mu_ep``,
``mu_rho``). This is therefore approach (a).

That also accords with the Howell1997_ (p. 325) advice to analyse the
transformed thing, then report back_transform(mean(transform(raw_values)));
Howell uses the example of analysing log salary, then reporting
antilog(mean(log salary)).

So: approach (a).


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


Per-trial values
----------------

Specimen single-subject, single-parameter task:

.. code-block:: C++

    data {
        int<lower=1> N_TRIALS;
        int<lower=0, upper=1> choice[N_TRIALS];
        int<lower=0, upper=1> outcome[N_TRIALS];
    }
    transformed data {
        int N_STIMULI = 2;
    }
    parameters {
        real<lower=0, upper=1> alpha;  // learning rate
    }
    model {
        // Calculated probability
        vector[N_TRIALS] p_choose_second;

        // Prior
        alpha ~ (1.2, 1.2);

        // Reinforcement learning model
        {
            vector[N_STIMULI] stimulus_value = rep_vector(0, N_STIMULI);
            int chosen;
            real prediction_error;
            for (t in 1:N_TRIALS) {
                // Choose
                p_choose_second[t] = softmax(stimulus_value)[1];
                // ... First option has index 0; second has index 1.
                // ... Fixed inverse temp. of 1 in this very simple model.
                // Learn
                chosen = choice[t];
                prediction_error = outcome[t] - stimulus_value[chosen];
                stimulus_value[chosen] = stimulus_value[chosen] + prediction_error * alpha;
            }
        }

        // Fit to behaviour
        choice ~ bernoulli(p_choose_second);
    }

And the same thing recoded to extract a per-trial variable:

.. code-block:: C++

    data {
        int<lower=1> N_TRIALS;
        int<lower=0, upper=1> choice[N_TRIALS];
        int<lower=0, upper=1> outcome[N_TRIALS];
    }
    transformed data {
        int N_STIMULI = 2;
    }
    parameters {
        real<lower=0, upper=1> alpha;  // learning rate
    }
    transformed parameters {
        // Here we are aiming to extract prediction error, and nothing else.
        // Will get e.g. N_TRIALS * 8000 values out (for 8 chains, 1000 samples
        // per chain). Beware saving too much!

        // We want this saved:
        vector[N_TRIALS] prediction_error;

        // We don't really want this, but we have to refer to it in the model:
        // Calculated probability
        vector[N_TRIALS] p_choose_second;

        // Use braces to prevent other variables being saved.
        // Put the RL calculations in here.
        {
            // Reinforcement learning model
            vector[N_STIMULI] stimulus_value = rep_vector(0, N_STIMULI);
            int chosen;
            // Replaced with a per-trial version: // real prediction_error;
            for (t in 1:N_TRIALS) {
                // Choose
                p_choose_second[t] = softmax(stimulus_value)[1];
                // ... First option has index 0; second has index 1.
                // ... Fixed inverse temp. of 1 in this very simple model.
                // Learn
                chosen = choice[t];
                prediction_error[t] = outcome[t] - stimulus_value[chosen];
                stimulus_value[chosen] = stimulus_value[chosen] + prediction_error[t] * alpha;
            }
        }
    }
    model {
        // Prior
        alpha ~ (1.2, 1.2);

        // Fit to behaviour
        choice ~ bernoulli(p_choose_second);
    }


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


CmdStan
-------

To get started with CmdStan:

- Download cmdstan from https://github.com/stan-dev/cmdstan/releases. For
  example, ``cmdstan-2.31.0.tar.gz``.

- Unzip it. I put the zip into ``~/dev`` so this gives e.g.
  ``~/dev/cmdstan-2.31.0``.

- Change into that cmdstan home directory. You may want to use the environment
  variable CMDSTANHOME for convenience (some scripts here use that).

- Run

  .. code-block:: bash

    make  # print help
    make build  # do useful things

- Change into its home directory.

- Call your Stan program ``/MYPATH/MYPROG.stan``; it can be in any directory.

- From the $CMDSTANHOME directory, run ``make /MYPATH/MYPROG`` (without the
  ".stan" suffix).

  This will create a program called "MYPROG" in the same directory as your Stan
  source code, i.e. in ``/MYPATH``.

- Run it from the code directory with:

  .. code-block:: bash

    ./MYPROG sample

  It will write "output.csv".

- You can then run

  .. code-block:: bash

    $CMDSTANHOME/bin/stansummary output.csv

It may also write "profile.csv", if your code contains profiling statements.
Inspect this and see
https://mc-stan.org/docs/2_26/cmdstan-guide/stan-csv.html#profiling-csv-output-file


GPU support
-----------

Stan will also support GPU calculations via OpenCL_. See:

- http://mc-stan.org/math/opencl_support.html
- https://discourse.mc-stan.org/t/stan-is-not-working-on-gpu-in-linux/21331
- https://discourse.mc-stan.org/t/partial-specialization-error-when-compiling-model-with-opencl-enabled/21250
- https://discourse.mc-stan.org/t/gpu-functions-in-rstan/13722/6

Find out whether your system supports OpenCL via:

.. code-block:: bash

    clinfo  # if not installed: sudo apt install clinfo
    clinfo -l  # list platforms/devices only

For example, it may produce output like:

.. code-block:: none

    Platform #0: NVIDIA CUDA
     `-- Device #0: GeForce GTX 660 Ti

Choose your device number (e.g. 0 in the example above).

For CmdStan, edit either ``~/.config/stan/make.local`` or
``${CMDSTANHOME}/make/local`` to include these lines:

.. code-block:: bash

    STAN_OPENCL = true
    CHOSEN_OPENCL_DEVICE = 0  # choose from the output of "clinfo -l"

    $(info STAN_OPENCL is ${STAN_OPENCL})
    ifeq (${STAN_OPENCL}, true)
        $(info CHOSEN_OPENCL_DEVICE is ${CHOSEN_OPENCL_DEVICE})
        OPENCL_DEVICE_ID = ${CHOSEN_OPENCL_DEVICE}
        OPENCL_PLATFORM_ID = ${CHOSEN_OPENCL_DEVICE}
        CXXFLAGS += -fpermissive
    endif

It looks like OpenCL is supported for CmdStan but not for RStan as of July
2020:
https://discourse.mc-stan.org/t/setting-up-gpu-for-rstan-on-windows-10/16472.
Also (as per the links above) there is an overhead for using GPUs and it's not
clear to me exactly what the conditions are when enabling OpenCL will help.
Still, something for the near future.


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



Leave-one-out (LOO) cross-validation and model selection
--------------------------------------------------------

- Bridge sampling (above) is based on Bayes factors/marginal likelihoods, and
  you modify your Stan code to change ``depvar ~ dist(params)`` to ``target +=
  dist_lpdf(depvar | params)``, correcting if required for boundaries imposed.
  It runs a bit slower, but no more information is saved.

- An alternative is leave-one-out (LOO) cross-validation and the LOO
  Information Criterion (LOOIC), and related techniques. You modify your Stan
  code, most efficiently in the "generated quantities" block, to declare a
  log-likelihood variable, usually named ``log_lik``, and specify it; e.g. for
  N data points sampled from a normal distribution, you might do

  .. code-block:: C++

    generated quantities {
        vector[N] log_lik;
        for (n in 1:N) {
            log_lik[n] = normal_lpdf(y[n] | mu, sigma);
        }

        // ... though probably in this simple case you could shorten to:
        // vector[N] log_lik = normal_lpdf(y | mu, sigma);
    }

  For a reinforcement learning model, this would be e.g.

  .. code-block:: C++

    generated quantities {
        vector[N] log_lik = bernoulli_lpmf(chose_rhs | p_choose_rhs);
    }

  A downside: to do this in the "generated quantities" block, you need to save
  the calculated probability (in the "transformed parameters" block), here
  ``p_choose_rhs``, which will be of size N trials × e.g. 8000 samples per
  variable (also: as for ``log_lik`` itself!). **But** there are other methods
  for large data; see http://mc-stan.org/loo/articles/loo2-large-data.html.

  The R ``loo`` package can compare models based on LOO metrics, and the R Stan
  package has LOO methods for ``stanfit`` objects. You can compare with e.g.

  .. code-block:: R

    loo_comparison <- loo::loo_compare(
        list(
            model1 = rstan::loo(model1_fit),
            model2 = rstan::loo(model2_fit),
            model3 = rstan::loo(model3_fit)
        )
    )
    print(loo_comparison, simplify = FALSE)

- For LOO methods with Stan, see:

  - https://mc-stan.org/rstanarm/reference/loo.stanreg.html
  - http://mc-stan.org/loo/articles/
  - http://mc-stan.org/loo/articles/loo2-large-data.html

- For debate about the better way (or when each is better), see

  - https://discourse.mc-stan.org/t/model-selection-with-loo-and-bridge-sampling/20861
  - http://ritsokiguess.site/docs/2019/06/25/going-to-the-loo-using-stan-for-model-comparison/
  - Vehtari (2017) https://doi.org/10.1007/s11222-016-9696-4, on LOO
    cross-validation;

    - Gronau (2019) https://doi.org/10.1007/s42113-018-0011-7, a critique;

      - Vehtari (2019) https://doi.org/10.1007/s42113-018-0020-6, rejoinder,
        including the importance of considering the M-open situation ("the best
        model is not among those I tested") as well as the M-closed model ("the
        best model must be one of these"). Their conclusion on marginal
        likelihood-based model selection versus cross-validation methods:
        "There is no simple answer."


Troubleshooting run failures
----------------------------

- This error from ``bridgesampling``:

  .. code-block:: none

    Error in tmp$r_vals[lr - 1] * tmp$r_vals[lr] :
      non-numeric argument to binary operator

  may be this bug: https://github.com/quentingronau/bridgesampling/issues/18


Troubleshooting poor convergence (high R-hat)
---------------------------------------------

- See https://mc-stan.org/misc/warnings.html, which gives recommendations.

  ... e.g. more samples, by increasing ``iter``.

- See also
  https://mc-stan.org/users/documentation/case-studies/divergences_and_bias.html
  (also at https://betanalpha.github.io/assets/case_studies/divergences_and_bias.html).

  ... e.g. increase ``adapt_delta`` towards 1.

- https://betanalpha.github.io/assets/case_studies/rstan_workflow.html

Consider also:

- reparameterization;

- tighter priors, if scientifically reasonable;

- ``init`` at the centre of distributions if it wasn't.


Which block does my variable belong in?
---------------------------------------

See
https://mc-stan.org/docs/2_18/reference-manual/overview-of-stans-program-blocks.html.

- ``data``: when you want to provide data, which may vary, to Stan.

- ``transformed data``: when you want to use transformed versions of the data,
  or when you want to declare constants.

- ``parameters``: when you want Stan to "jiggle" the variable to find the best
  fit.

- ``transformed parameters``: when you want to use (and later inspect) values
  that are transformations of the ``parameters``.

- ``model``: for local calculations only, enabling you to fit the model.
  Variables declared in the model block are not saved. Sampling statements
  (e.g. ``y ~ normal(mu, sigma)`` or ``target += normal_lpdf(y | mu, sigma)``)
  go here.

- ``generated quantities``: when you want to calculate and extract something
  based on ``parameters`` or ``transformed parameters``, but that calculation
  isn't important for model fitting (it's just "observing" the model after it
  has been fitted).

One thing that looks like a deficiency at first glance is that you may perform
complex calculations in the ``model`` and then want to save some of these (e.g.
an important intermediate variable, like reward prediction error, or something
more basic like "proportion of trials predicted correctly"). Since that can't
be saved in ``model``, do you have to repeat the calculation logic in
``generated quantities``? And since you can't return complex objects from
user-defined functions, and you can't pass by reference (allowing a function to
modify objects referred to by its parameters), then is this significantly
limiting? My 2013 question, kindly answered by Bob Carpenter, is `here
<https://groups.google.com/g/stan-users/c/lybDQTpMWRw>`_. The **answer** is to
put them in the ``transformed parameter`` block (and hide any associated
temporary variables with a local ``{}`` block). The **downside** may be that this entails
a very large quantity of data being saved, because you will have to save
anything that you then want to refer to in the model block (i.e. for the final
step of fitting the model to the actual data).


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


A private bare-metal server
---------------------------

- ``tsp`` is a good lightweight job control system. You just need to set up
  ``postfix`` so it can e-mail you.
