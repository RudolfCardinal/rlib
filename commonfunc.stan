
    // ========================================================================
    // Common functions
    // ========================================================================
    // Reminders:
    // - Annoyingly, you can't modify arguments to Stan user-defined functions.
    //   (No pass-by-reference.)
    // - size() doesn't work on a plain "vector". Use num_elements().
    // - Array/vector indexing is 1-based.
    // - The addition-assignment (+=) operator generally doesn't work (it
    //   appears to be reserved for the one variable "target += ...").
    //   Similarly for all others you might expect.
    // - Can't define constants in a functions{} block.

    // ------------------------------------------------------------------------
    // Simple functions
    // ------------------------------------------------------------------------

    real softmaxNth(vector softmax_inputs, int index)
    {
        /*
            For softmax: see my miscstat.R; the important points for
            optimization are (1) that softmax is invariant to the addition/
            subtraction of a constant, and subtracting the mean makes the
            numbers less likely to fall over computationally; (2) we only
            need the final part of the computation for a single number
            (preference for the right), so we don't have to waste time
            vector-calculating the preference for the left as well [that is:
            we don't have to calculate s_exp_products / sum(s_exp_products)].

            Since Stan 2.0.0, the alternative is to use softmax(); see
            stan/math/fwd/mat/fun/softmax.hpp. Not sure which is faster, or
            whether it really matters.
        */
        int length = num_elements(softmax_inputs);
        vector[length] s_exp_products;
        if (index < 1 || index > length) {
            reject("softmaxNth(): index is ", index,
                   " but must be in range 1-", length);
        }
        s_exp_products = exp(softmax_inputs - mean(softmax_inputs));
        return s_exp_products[index] / sum(s_exp_products);
    }

    real softmaxNthInvTemp(vector softmax_inputs, real inverse_temp, int index)
    {
        int length = num_elements(softmax_inputs);
        vector[length] s_exp_products;
        if (index < 1 || index > length) {
            reject("softmaxNthInvTemp(): index is ", index,
                   " but must be in range 1-", length);
        }
        s_exp_products = exp(softmax_inputs * inverse_temp - mean(softmax_inputs));
        return s_exp_products[index] / sum(s_exp_products);
    }

    real logistic(real x, real x0, real k, real L)
    {
        // Notation as per https://en.wikipedia.org/wiki/Logistic_function
        // x0: centre
        // k: steepness
        // L: maximum (usually 1)

        return L / (1 + exp(-k * (x - x0)));
    }

    real bound(real x, real min_value, real max_value)
    {
        // We should simply be able to do this:
        //     return max(min_value, min(x, max_value));
        // ... but Stan doesn't have max(real, real) or
        // min(real, real) functions!

        if (x < min_value) {
            return min_value;
        } else if (x > max_value) {
            return max_value;
        } else {
            return x;
        }
    }

    real boundLower(real x, real min_value)
    {
        // a.k.a. max()

        if (x < min_value) {
            return min_value;
        } else {
            return x;
        }
    }

    real boundUpper(real x, real max_value)
    {
        // a.k.a. min()

        if (x > max_value) {
            return max_value;
        } else {
            return x;
        }
    }

    // ------------------------------------------------------------------------
    // LOG PROBABILITY FUNCTIONS FOR BRIDGE SAMPLING
    // ------------------------------------------------------------------------
    /*
    We can have functions that access the log probability accumulator
    if the function name ends in '_lp'; see Stan manual section 23.3.

    Re arguments: the Stan manual uses notation like
         real normal_lpdf(reals y | reals mu, reals sigma)
    but "reals" isn't something you can actually use in user functions.
    See p495:
        "reals" means:
                real
                real[]
                vector
                row_vector
        "ints" means
                int
                int[]

    Moreover, you can't define two copies of the same function with
    different names (23.6: no overloading of user-defined functions).
    For real arguments, the options are therefore:
         real
         real[]  // one-dimensional array
         real[,]  // two-dimensional array
         vector  // vector, similar to a one-dimensional array.
         matrix  // matrix, similar to a two-dimensional array.
    See p297 of the 2017 Stan manual, and also p319.
    Which do we use in practice?
    - Firstly, we use single numbers or one-dimensional collections,
      and generally the latter. So that means real[] or vector.
    - We use both.
    - So let's have "Real", "Arr" and "Vec" versions.
    - Then, to make things worse, we sometimes have constant parameters,
      and sometimes array/vector parameters...
    - For something with two distribution parameters, like the normal
      distribution and many others, that means that we have 3*3*3 combinations
      for each thing. Urgh. Stan should allow user overloading ;).
    - Let's do it and define "R", "A", "V" for the parameters
    - Except we won't be returning R unless it's RRR!
    - Last thing cycles fastest.
    So:
        RRR
        -- nothing else R*
        ARA
        ARV
        AAR
        AAA
        AAV
        AVR
        AVA
        AVV
        VRA
        VRV
        VAR
        VAA
        VAV
        VVR
        VVA
        VVV

    */

    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Normal distribution
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    // Normal distribution
    void sampleNormal_RRR_lp(real y, real mu, real sigma)
    {
        // Effectively samples y from normal(mean=mu, sd=sigma),
        // in a manner compatible with bridgesampling.
        target += normal_lpdf(y | mu, sigma);
        // Note that although it doesn't "return" anything, it still ends up
        // affecting y.
    }
    //
    void sampleNormal_ARR_lp(real[] y, real mu, real sigma)
    {
        target += normal_lpdf(y | mu, sigma);
    }
    void sampleNormal_ARA_lp(real[] y, real mu, real[] sigma)
    {
        target += normal_lpdf(y | mu, sigma);
    }
    void sampleNormal_ARV_lp(real[] y, real mu, vector sigma)
    {
        target += normal_lpdf(y | mu, sigma);
    }
    void sampleNormal_AAR_lp(real[] y, real[] mu, real sigma)
    {
        target += normal_lpdf(y | mu, sigma);
    }
    void sampleNormal_AAA_lp(real[] y, real[] mu, real[] sigma)
    {
        target += normal_lpdf(y | mu, sigma);
    }
    void sampleNormal_AAV_lp(real[] y, real[] mu, vector sigma)
    {
        target += normal_lpdf(y | mu, sigma);
    }
    void sampleNormal_AVR_lp(real[] y, vector mu, real sigma)
    {
        target += normal_lpdf(y | mu, sigma);
    }
    void sampleNormal_AVA_lp(real[] y, vector mu, real[] sigma)
    {
        target += normal_lpdf(y | mu, sigma);
    }
    void sampleNormal_AVV_lp(real[] y, vector mu, vector sigma)
    {
        target += normal_lpdf(y | mu, sigma);
    }
    //
    void sampleNormal_VRR_lp(vector y, real mu, real sigma)
    {
        target += normal_lpdf(y | mu, sigma);
    }
    void sampleNormal_VRA_lp(vector y, real mu, real[] sigma)
    {
        target += normal_lpdf(y | mu, sigma);
    }
    void sampleNormal_VRV_lp(vector y, real mu, vector sigma)
    {
        target += normal_lpdf(y | mu, sigma);
    }
    void sampleNormal_VAR_lp(vector y, real[] mu, real sigma)
    {
        target += normal_lpdf(y | mu, sigma);
    }
    void sampleNormal_VAA_lp(vector y, real[] mu, real[] sigma)
    {
        target += normal_lpdf(y | mu, sigma);
    }
    void sampleNormal_VAV_lp(vector y, real[] mu, vector sigma)
    {
        target += normal_lpdf(y | mu, sigma);
    }
    void sampleNormal_VVR_lp(vector y, vector mu, real sigma)
    {
        target += normal_lpdf(y | mu, sigma);
    }
    void sampleNormal_VVA_lp(vector y, vector mu, real[] sigma)
    {
        target += normal_lpdf(y | mu, sigma);
    }
    void sampleNormal_VVV_lp(vector y, vector mu, vector sigma)
    {
        target += normal_lpdf(y | mu, sigma);
    }

    // Normal distribution with lower bound
    void sampleNormalLowerBound_RRR_lp(real y, real mu, real sigma, real lower)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lccdf(lower | mu, sigma);
    }
    //
    void sampleNormalLowerBound_ARR_lp(real[] y, real mu, real sigma, real lower)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lccdf(lower | mu, sigma);
    }
    void sampleNormalLowerBound_ARA_lp(real[] y, real mu, real[] sigma, real lower)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lccdf(lower | mu, sigma);
    }
    void sampleNormalLowerBound_ARV_lp(real[] y, real mu, vector sigma, real lower)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lccdf(lower | mu, sigma);
    }
    void sampleNormalLowerBound_AAR_lp(real[] y, real[] mu, real sigma, real lower)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lccdf(lower | mu, sigma);
    }
    void sampleNormalLowerBound_AAA_lp(real[] y, real[] mu, real[] sigma, real lower)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lccdf(lower | mu, sigma);
    }
    void sampleNormalLowerBound_AAV_lp(real[] y, real[] mu, vector sigma, real lower)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lccdf(lower | mu, sigma);
    }
    void sampleNormalLowerBound_AVR_lp(real[] y, vector mu, real sigma, real lower)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lccdf(lower | mu, sigma);
    }
    void sampleNormalLowerBound_AVA_lp(real[] y, vector mu, real[] sigma, real lower)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lccdf(lower | mu, sigma);
    }
    void sampleNormalLowerBound_AVV_lp(real[] y, vector mu, vector sigma, real lower)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lccdf(lower | mu, sigma);
    }
    //
    void sampleNormalLowerBound_VRR_lp(vector y, real mu, real sigma, real lower)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lccdf(lower | mu, sigma);
    }
    void sampleNormalLowerBound_VRA_lp(vector y, real mu, real[] sigma, real lower)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lccdf(lower | mu, sigma);
    }
    void sampleNormalLowerBound_VRV_lp(vector y, real mu, vector sigma, real lower)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lccdf(lower | mu, sigma);
    }
    void sampleNormalLowerBound_VAR_lp(vector y, real[] mu, real sigma, real lower)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lccdf(lower | mu, sigma);
    }
    void sampleNormalLowerBound_VAA_lp(vector y, real[] mu, real[] sigma, real lower)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lccdf(lower | mu, sigma);
    }
    void sampleNormalLowerBound_VAV_lp(vector y, real[] mu, vector sigma, real lower)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lccdf(lower | mu, sigma);
    }
    void sampleNormalLowerBound_VVR_lp(vector y, vector mu, real sigma, real lower)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lccdf(lower | mu, sigma);
    }
    void sampleNormalLowerBound_VVA_lp(vector y, vector mu, real[] sigma, real lower)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lccdf(lower | mu, sigma);
    }
    void sampleNormalLowerBound_VVV_lp(vector y, vector mu, vector sigma, real lower)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lccdf(lower | mu, sigma);
    }

    // Normal distribution with upper bound
    void sampleNormalUpperBound_RRR_lp(real y, real mu, real sigma, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lcdf(upper | mu, sigma);
    }
    //
    void sampleNormalUpperBound_ARR_lp(real[] y, real mu, real sigma, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lcdf(upper | mu, sigma);
    }
    void sampleNormalUpperBound_ARA_lp(real[] y, real mu, real[] sigma, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lcdf(upper | mu, sigma);
    }
    void sampleNormalUpperBound_ARV_lp(real[] y, real mu, vector sigma, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lcdf(upper | mu, sigma);
    }
    void sampleNormalUpperBound_AAR_lp(real[] y, real[] mu, real sigma, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lcdf(upper | mu, sigma);
    }
    void sampleNormalUpperBound_AAA_lp(real[] y, real[] mu, real[] sigma, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lcdf(upper | mu, sigma);
    }
    void sampleNormalUpperBound_AAV_lp(real[] y, real[] mu, vector sigma, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lcdf(upper | mu, sigma);
    }
    void sampleNormalUpperBound_AVR_lp(real[] y, vector mu, real sigma, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lcdf(upper | mu, sigma);
    }
    void sampleNormalUpperBound_AVA_lp(real[] y, vector mu, real[] sigma, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lcdf(upper | mu, sigma);
    }
    void sampleNormalUpperBound_AVV_lp(real[] y, vector mu, vector sigma, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lcdf(upper | mu, sigma);
    }
    //
    void sampleNormalUpperBound_VRR_lp(vector y, real mu, real sigma, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lcdf(upper | mu, sigma);
    }
    void sampleNormalUpperBound_VRA_lp(vector y, real mu, real[] sigma, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lcdf(upper | mu, sigma);
    }
    void sampleNormalUpperBound_VRV_lp(vector y, real mu, vector sigma, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lcdf(upper | mu, sigma);
    }
    void sampleNormalUpperBound_VAR_lp(vector y, real[] mu, real sigma, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lcdf(upper | mu, sigma);
    }
    void sampleNormalUpperBound_VAA_lp(vector y, real[] mu, real[] sigma, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lcdf(upper | mu, sigma);
    }
    void sampleNormalUpperBound_VAV_lp(vector y, real[] mu, vector sigma, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lcdf(upper | mu, sigma);
    }
    void sampleNormalUpperBound_VVR_lp(vector y, vector mu, real sigma, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lcdf(upper | mu, sigma);
    }
    void sampleNormalUpperBound_VVA_lp(vector y, vector mu, real[] sigma, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lcdf(upper | mu, sigma);
    }
    void sampleNormalUpperBound_VVV_lp(vector y, vector mu, vector sigma, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  normal_lcdf(upper | mu, sigma);
    }

    // Normal distribution with lower and upper bound
    void sampleNormalRangeBound_RRR_lp(real y, real mu, real sigma, real lower, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  log_diff_exp(normal_lcdf(upper | mu, sigma),
                               normal_lcdf(lower | mu, sigma));
    }
    //
    void sampleNormalRangeBound_ARR_lp(real[] y, real mu, real sigma, real lower, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  log_diff_exp(normal_lcdf(upper | mu, sigma),
                               normal_lcdf(lower | mu, sigma));
    }
    void sampleNormalRangeBound_ARA_lp(real[] y, real mu, real[] sigma, real lower, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  log_diff_exp(normal_lcdf(upper | mu, sigma),
                               normal_lcdf(lower | mu, sigma));
    }
    void sampleNormalRangeBound_ARV_lp(real[] y, real mu, vector sigma, real lower, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  log_diff_exp(normal_lcdf(upper | mu, sigma),
                               normal_lcdf(lower | mu, sigma));
    }
    void sampleNormalRangeBound_AAR_lp(real[] y, real[] mu, real sigma, real lower, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  log_diff_exp(normal_lcdf(upper | mu, sigma),
                               normal_lcdf(lower | mu, sigma));
    }
    void sampleNormalRangeBound_AAA_lp(real[] y, real[] mu, real[] sigma, real lower, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  log_diff_exp(normal_lcdf(upper | mu, sigma),
                               normal_lcdf(lower | mu, sigma));
    }
    void sampleNormalRangeBound_AAV_lp(real[] y, real[] mu, vector sigma, real lower, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  log_diff_exp(normal_lcdf(upper | mu, sigma),
                               normal_lcdf(lower | mu, sigma));
    }
    void sampleNormalRangeBound_AVR_lp(real[] y, vector mu, real sigma, real lower, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  log_diff_exp(normal_lcdf(upper | mu, sigma),
                               normal_lcdf(lower | mu, sigma));
    }
    void sampleNormalRangeBound_AVA_lp(real[] y, vector mu, real[] sigma, real lower, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  log_diff_exp(normal_lcdf(upper | mu, sigma),
                               normal_lcdf(lower | mu, sigma));
    }
    void sampleNormalRangeBound_AVV_lp(real[] y, vector mu, vector sigma, real lower, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  log_diff_exp(normal_lcdf(upper | mu, sigma),
                               normal_lcdf(lower | mu, sigma));
    }
    //
    void sampleNormalRangeBound_VRR_lp(vector y, real mu, real sigma, real lower, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  log_diff_exp(normal_lcdf(upper | mu, sigma),
                               normal_lcdf(lower | mu, sigma));
    }
    void sampleNormalRangeBound_VRA_lp(vector y, real mu, real[] sigma, real lower, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  log_diff_exp(normal_lcdf(upper | mu, sigma),
                               normal_lcdf(lower | mu, sigma));
    }
    void sampleNormalRangeBound_VRV_lp(vector y, real mu, vector sigma, real lower, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  log_diff_exp(normal_lcdf(upper | mu, sigma),
                               normal_lcdf(lower | mu, sigma));
    }
    void sampleNormalRangeBound_VAR_lp(vector y, real[] mu, real sigma, real lower, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  log_diff_exp(normal_lcdf(upper | mu, sigma),
                               normal_lcdf(lower | mu, sigma));
    }
    void sampleNormalRangeBound_VAA_lp(vector y, real[] mu, real[] sigma, real lower, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  log_diff_exp(normal_lcdf(upper | mu, sigma),
                               normal_lcdf(lower | mu, sigma));
    }
    void sampleNormalRangeBound_VAV_lp(vector y, real[] mu, vector sigma, real lower, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  log_diff_exp(normal_lcdf(upper | mu, sigma),
                               normal_lcdf(lower | mu, sigma));
    }
    void sampleNormalRangeBound_VVR_lp(vector y, vector mu, real sigma, real lower, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  log_diff_exp(normal_lcdf(upper | mu, sigma),
                               normal_lcdf(lower | mu, sigma));
    }
    void sampleNormalRangeBound_VVA_lp(vector y, vector mu, real[] sigma, real lower, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  log_diff_exp(normal_lcdf(upper | mu, sigma),
                               normal_lcdf(lower | mu, sigma));
    }
    void sampleNormalRangeBound_VVV_lp(vector y, vector mu, vector sigma, real lower, real upper)
    {
        target += normal_lpdf(y | mu, sigma) -
                  log_diff_exp(normal_lcdf(upper | mu, sigma),
                               normal_lcdf(lower | mu, sigma));
    }

    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Cauchy distribution
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    void sampleCauchy_RRR_lp(real y, real mu, real sigma)
    {
        target += cauchy_lpdf(y | mu, sigma);
    }
    //
    void sampleCauchy_ARR_lp(real[] y, real mu, real sigma)
    {
        target += cauchy_lpdf(y | mu, sigma);
    }
    void sampleCauchy_ARA_lp(real[] y, real mu, real[] sigma)
    {
        target += cauchy_lpdf(y | mu, sigma);
    }
    void sampleCauchy_ARV_lp(real[] y, real mu, vector sigma)
    {
        target += cauchy_lpdf(y | mu, sigma);
    }
    void sampleCauchy_AAR_lp(real[] y, real[] mu, real sigma)
    {
        target += cauchy_lpdf(y | mu, sigma);
    }
    void sampleCauchy_AAA_lp(real[] y, real[] mu, real[] sigma)
    {
        target += cauchy_lpdf(y | mu, sigma);
    }
    void sampleCauchy_AAV_lp(real[] y, real[] mu, vector sigma)
    {
        target += cauchy_lpdf(y | mu, sigma);
    }
    void sampleCauchy_AVR_lp(real[] y, vector mu, real sigma)
    {
        target += cauchy_lpdf(y | mu, sigma);
    }
    void sampleCauchy_AVA_lp(real[] y, vector mu, real[] sigma)
    {
        target += cauchy_lpdf(y | mu, sigma);
    }
    void sampleCauchy_AVV_lp(real[] y, vector mu, vector sigma)
    {
        target += cauchy_lpdf(y | mu, sigma);
    }
    //
    void sampleCauchy_VRR_lp(vector y, real mu, real sigma)
    {
        target += cauchy_lpdf(y | mu, sigma);
    }
    void sampleCauchy_VRA_lp(vector y, real mu, real[] sigma)
    {
        target += cauchy_lpdf(y | mu, sigma);
    }
    void sampleCauchy_VRV_lp(vector y, real mu, vector sigma)
    {
        target += cauchy_lpdf(y | mu, sigma);
    }
    void sampleCauchy_VAR_lp(vector y, real[] mu, real sigma)
    {
        target += cauchy_lpdf(y | mu, sigma);
    }
    void sampleCauchy_VAA_lp(vector y, real[] mu, real[] sigma)
    {
        target += cauchy_lpdf(y | mu, sigma);
    }
    void sampleCauchy_VAV_lp(vector y, real[] mu, vector sigma)
    {
        target += cauchy_lpdf(y | mu, sigma);
    }
    void sampleCauchy_VVR_lp(vector y, vector mu, real sigma)
    {
        target += cauchy_lpdf(y | mu, sigma);
    }
    void sampleCauchy_VVA_lp(vector y, vector mu, real[] sigma)
    {
        target += cauchy_lpdf(y | mu, sigma);
    }
    void sampleCauchy_VVV_lp(vector y, vector mu, vector sigma)
    {
        target += cauchy_lpdf(y | mu, sigma);
    }

    // Cauchy distribution with lower bound
    void sampleCauchyLowerBound_RRR_lp(real y, real mu, real sigma, real lower)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lccdf(lower | mu, sigma);
    }
    //
    void sampleCauchyLowerBound_ARR_lp(real[] y, real mu, real sigma, real lower)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lccdf(lower | mu, sigma);
    }
    void sampleCauchyLowerBound_ARA_lp(real[] y, real mu, real[] sigma, real lower)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lccdf(lower | mu, sigma);
    }
    void sampleCauchyLowerBound_ARV_lp(real[] y, real mu, vector sigma, real lower)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lccdf(lower | mu, sigma);
    }
    void sampleCauchyLowerBound_AAR_lp(real[] y, real[] mu, real sigma, real lower)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lccdf(lower | mu, sigma);
    }
    void sampleCauchyLowerBound_AAA_lp(real[] y, real[] mu, real[] sigma, real lower)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lccdf(lower | mu, sigma);
    }
    void sampleCauchyLowerBound_AAV_lp(real[] y, real[] mu, vector sigma, real lower)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lccdf(lower | mu, sigma);
    }
    void sampleCauchyLowerBound_AVR_lp(real[] y, vector mu, real sigma, real lower)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lccdf(lower | mu, sigma);
    }
    void sampleCauchyLowerBound_AVA_lp(real[] y, vector mu, real[] sigma, real lower)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lccdf(lower | mu, sigma);
    }
    void sampleCauchyLowerBound_AVV_lp(real[] y, vector mu, vector sigma, real lower)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lccdf(lower | mu, sigma);
    }
    //
    void sampleCauchyLowerBound_VRR_lp(vector y, real mu, real sigma, real lower)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lccdf(lower | mu, sigma);
    }
    void sampleCauchyLowerBound_VRA_lp(vector y, real mu, real[] sigma, real lower)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lccdf(lower | mu, sigma);
    }
    void sampleCauchyLowerBound_VRV_lp(vector y, real mu, vector sigma, real lower)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lccdf(lower | mu, sigma);
    }
    void sampleCauchyLowerBound_VAR_lp(vector y, real[] mu, real sigma, real lower)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lccdf(lower | mu, sigma);
    }
    void sampleCauchyLowerBound_VAA_lp(vector y, real[] mu, real[] sigma, real lower)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lccdf(lower | mu, sigma);
    }
    void sampleCauchyLowerBound_VAV_lp(vector y, real[] mu, vector sigma, real lower)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lccdf(lower | mu, sigma);
    }
    void sampleCauchyLowerBound_VVR_lp(vector y, vector mu, real sigma, real lower)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lccdf(lower | mu, sigma);
    }
    void sampleCauchyLowerBound_VVA_lp(vector y, vector mu, real[] sigma, real lower)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lccdf(lower | mu, sigma);
    }
    void sampleCauchyLowerBound_VVV_lp(vector y, vector mu, vector sigma, real lower)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lccdf(lower | mu, sigma);
    }

    // Cauchy distribution with upper bound
    void sampleCauchyUpperBound_RRR_lp(real y, real mu, real sigma, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lcdf(upper | mu, sigma);
    }
    //
    void sampleCauchyUpperBound_ARR_lp(real[] y, real mu, real sigma, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lcdf(upper | mu, sigma);
    }
    void sampleCauchyUpperBound_ARA_lp(real[] y, real mu, real[] sigma, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lcdf(upper | mu, sigma);
    }
    void sampleCauchyUpperBound_ARV_lp(real[] y, real mu, vector sigma, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lcdf(upper | mu, sigma);
    }
    void sampleCauchyUpperBound_AAR_lp(real[] y, real[] mu, real sigma, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lcdf(upper | mu, sigma);
    }
    void sampleCauchyUpperBound_AAA_lp(real[] y, real[] mu, real[] sigma, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lcdf(upper | mu, sigma);
    }
    void sampleCauchyUpperBound_AAV_lp(real[] y, real[] mu, vector sigma, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lcdf(upper | mu, sigma);
    }
    void sampleCauchyUpperBound_AVR_lp(real[] y, vector mu, real sigma, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lcdf(upper | mu, sigma);
    }
    void sampleCauchyUpperBound_AVA_lp(real[] y, vector mu, real[] sigma, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lcdf(upper | mu, sigma);
    }
    void sampleCauchyUpperBound_AVV_lp(real[] y, vector mu, vector sigma, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lcdf(upper | mu, sigma);
    }
    //
    void sampleCauchyUpperBound_VRR_lp(vector y, real mu, real sigma, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lcdf(upper | mu, sigma);
    }
    void sampleCauchyUpperBound_VRA_lp(vector y, real mu, real[] sigma, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lcdf(upper | mu, sigma);
    }
    void sampleCauchyUpperBound_VRV_lp(vector y, real mu, vector sigma, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lcdf(upper | mu, sigma);
    }
    void sampleCauchyUpperBound_VAR_lp(vector y, real[] mu, real sigma, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lcdf(upper | mu, sigma);
    }
    void sampleCauchyUpperBound_VAA_lp(vector y, real[] mu, real[] sigma, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lcdf(upper | mu, sigma);
    }
    void sampleCauchyUpperBound_VAV_lp(vector y, real[] mu, vector sigma, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lcdf(upper | mu, sigma);
    }
    void sampleCauchyUpperBound_VVR_lp(vector y, vector mu, real sigma, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lcdf(upper | mu, sigma);
    }
    void sampleCauchyUpperBound_VVA_lp(vector y, vector mu, real[] sigma, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lcdf(upper | mu, sigma);
    }
    void sampleCauchyUpperBound_VVV_lp(vector y, vector mu, vector sigma, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  cauchy_lcdf(upper | mu, sigma);
    }

    // Cauchy distribution with lower and upper bound
    void sampleCauchyRangeBound_RRR_lp(real y, real mu, real sigma, real lower, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  log_diff_exp(cauchy_lcdf(upper | mu, sigma),
                               cauchy_lcdf(lower | mu, sigma));
    }
    //
    void sampleCauchyRangeBound_ARR_lp(real[] y, real mu, real sigma, real lower, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  log_diff_exp(cauchy_lcdf(upper | mu, sigma),
                               cauchy_lcdf(lower | mu, sigma));
    }
    void sampleCauchyRangeBound_ARA_lp(real[] y, real mu, real[] sigma, real lower, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  log_diff_exp(cauchy_lcdf(upper | mu, sigma),
                               cauchy_lcdf(lower | mu, sigma));
    }
    void sampleCauchyRangeBound_ARV_lp(real[] y, real mu, vector sigma, real lower, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  log_diff_exp(cauchy_lcdf(upper | mu, sigma),
                               cauchy_lcdf(lower | mu, sigma));
    }
    void sampleCauchyRangeBound_AAR_lp(real[] y, real[] mu, real sigma, real lower, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  log_diff_exp(cauchy_lcdf(upper | mu, sigma),
                               cauchy_lcdf(lower | mu, sigma));
    }
    void sampleCauchyRangeBound_AAA_lp(real[] y, real[] mu, real[] sigma, real lower, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  log_diff_exp(cauchy_lcdf(upper | mu, sigma),
                               cauchy_lcdf(lower | mu, sigma));
    }
    void sampleCauchyRangeBound_AAV_lp(real[] y, real[] mu, vector sigma, real lower, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  log_diff_exp(cauchy_lcdf(upper | mu, sigma),
                               cauchy_lcdf(lower | mu, sigma));
    }
    void sampleCauchyRangeBound_AVR_lp(real[] y, vector mu, real sigma, real lower, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  log_diff_exp(cauchy_lcdf(upper | mu, sigma),
                               cauchy_lcdf(lower | mu, sigma));
    }
    void sampleCauchyRangeBound_AVA_lp(real[] y, vector mu, real[] sigma, real lower, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  log_diff_exp(cauchy_lcdf(upper | mu, sigma),
                               cauchy_lcdf(lower | mu, sigma));
    }
    void sampleCauchyRangeBound_AVV_lp(real[] y, vector mu, vector sigma, real lower, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  log_diff_exp(cauchy_lcdf(upper | mu, sigma),
                               cauchy_lcdf(lower | mu, sigma));
    }
    //
    void sampleCauchyRangeBound_VRR_lp(vector y, real mu, real sigma, real lower, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  log_diff_exp(cauchy_lcdf(upper | mu, sigma),
                               cauchy_lcdf(lower | mu, sigma));
    }
    void sampleCauchyRangeBound_VRA_lp(vector y, real mu, real[] sigma, real lower, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  log_diff_exp(cauchy_lcdf(upper | mu, sigma),
                               cauchy_lcdf(lower | mu, sigma));
    }
    void sampleCauchyRangeBound_VRV_lp(vector y, real mu, vector sigma, real lower, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  log_diff_exp(cauchy_lcdf(upper | mu, sigma),
                               cauchy_lcdf(lower | mu, sigma));
    }
    void sampleCauchyRangeBound_VAR_lp(vector y, real[] mu, real sigma, real lower, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  log_diff_exp(cauchy_lcdf(upper | mu, sigma),
                               cauchy_lcdf(lower | mu, sigma));
    }
    void sampleCauchyRangeBound_VAA_lp(vector y, real[] mu, real[] sigma, real lower, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  log_diff_exp(cauchy_lcdf(upper | mu, sigma),
                               cauchy_lcdf(lower | mu, sigma));
    }
    void sampleCauchyRangeBound_VAV_lp(vector y, real[] mu, vector sigma, real lower, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  log_diff_exp(cauchy_lcdf(upper | mu, sigma),
                               cauchy_lcdf(lower | mu, sigma));
    }
    void sampleCauchyRangeBound_VVR_lp(vector y, vector mu, real sigma, real lower, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  log_diff_exp(cauchy_lcdf(upper | mu, sigma),
                               cauchy_lcdf(lower | mu, sigma));
    }
    void sampleCauchyRangeBound_VVA_lp(vector y, vector mu, real[] sigma, real lower, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  log_diff_exp(cauchy_lcdf(upper | mu, sigma),
                               cauchy_lcdf(lower | mu, sigma));
    }
    void sampleCauchyRangeBound_VVV_lp(vector y, vector mu, vector sigma, real lower, real upper)
    {
        target += cauchy_lpdf(y | mu, sigma) -
                  log_diff_exp(cauchy_lcdf(upper | mu, sigma),
                               cauchy_lcdf(lower | mu, sigma));
    }

    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Beta distribution
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    void sampleBeta_RRR_lp(real y, real alpha, real beta)
    {
        target += beta_lpdf(y | alpha, beta);
    }
    //
    void sampleBeta_ARR_lp(real[] y, real alpha, real beta)
    {
        target += beta_lpdf(y | alpha, beta);
    }
    void sampleBeta_ARA_lp(real[] y, real alpha, real[] beta)
    {
        target += beta_lpdf(y | alpha, beta);
    }
    void sampleBeta_ARV_lp(real[] y, real alpha, vector beta)
    {
        target += beta_lpdf(y | alpha, beta);
    }
    void sampleBeta_AAR_lp(real[] y, real[] alpha, real beta)
    {
        target += beta_lpdf(y | alpha, beta);
    }
    void sampleBeta_AAA_lp(real[] y, real[] alpha, real[] beta)
    {
        target += beta_lpdf(y | alpha, beta);
    }
    void sampleBeta_AAV_lp(real[] y, real[] alpha, vector beta)
    {
        target += beta_lpdf(y | alpha, beta);
    }
    void sampleBeta_AVR_lp(real[] y, vector alpha, real beta)
    {
        target += beta_lpdf(y | alpha, beta);
    }
    void sampleBeta_AVA_lp(real[] y, vector alpha, real[] beta)
    {
        target += beta_lpdf(y | alpha, beta);
    }
    void sampleBeta_AVV_lp(real[] y, vector alpha, vector beta)
    {
        target += beta_lpdf(y | alpha, beta);
    }
    //
    void sampleBeta_VRR_lp(vector y, real alpha, real beta)
    {
        target += beta_lpdf(y | alpha, beta);
    }
    void sampleBeta_VRA_lp(vector y, real alpha, real[] beta)
    {
        target += beta_lpdf(y | alpha, beta);
    }
    void sampleBeta_VRV_lp(vector y, real alpha, vector beta)
    {
        target += beta_lpdf(y | alpha, beta);
    }
    void sampleBeta_VAR_lp(vector y, real[] alpha, real beta)
    {
        target += beta_lpdf(y | alpha, beta);
    }
    void sampleBeta_VAA_lp(vector y, real[] alpha, real[] beta)
    {
        target += beta_lpdf(y | alpha, beta);
    }
    void sampleBeta_VAV_lp(vector y, real[] alpha, vector beta)
    {
        target += beta_lpdf(y | alpha, beta);
    }
    void sampleBeta_VVR_lp(vector y, vector alpha, real beta)
    {
        target += beta_lpdf(y | alpha, beta);
    }
    void sampleBeta_VVA_lp(vector y, vector alpha, real[] beta)
    {
        target += beta_lpdf(y | alpha, beta);
    }
    void sampleBeta_VVV_lp(vector y, vector alpha, vector beta)
    {
        target += beta_lpdf(y | alpha, beta);
    }

    // Beta distribution with lower bound
    void sampleBetaLowerBound_RRR_lp(real y, real alpha, real beta, real lower)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lccdf(lower | alpha, beta);
    }
    //
    void sampleBetaLowerBound_ARR_lp(real[] y, real alpha, real beta, real lower)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lccdf(lower | alpha, beta);
    }
    void sampleBetaLowerBound_ARA_lp(real[] y, real alpha, real[] beta, real lower)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lccdf(lower | alpha, beta);
    }
    void sampleBetaLowerBound_ARV_lp(real[] y, real alpha, vector beta, real lower)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lccdf(lower | alpha, beta);
    }
    void sampleBetaLowerBound_AAR_lp(real[] y, real[] alpha, real beta, real lower)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lccdf(lower | alpha, beta);
    }
    void sampleBetaLowerBound_AAA_lp(real[] y, real[] alpha, real[] beta, real lower)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lccdf(lower | alpha, beta);
    }
    void sampleBetaLowerBound_AAV_lp(real[] y, real[] alpha, vector beta, real lower)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lccdf(lower | alpha, beta);
    }
    void sampleBetaLowerBound_AVR_lp(real[] y, vector alpha, real beta, real lower)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lccdf(lower | alpha, beta);
    }
    void sampleBetaLowerBound_AVA_lp(real[] y, vector alpha, real[] beta, real lower)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lccdf(lower | alpha, beta);
    }
    void sampleBetaLowerBound_AVV_lp(real[] y, vector alpha, vector beta, real lower)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lccdf(lower | alpha, beta);
    }
    //
    void sampleBetaLowerBound_VRR_lp(vector y, real alpha, real beta, real lower)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lccdf(lower | alpha, beta);
    }
    void sampleBetaLowerBound_VRA_lp(vector y, real alpha, real[] beta, real lower)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lccdf(lower | alpha, beta);
    }
    void sampleBetaLowerBound_VRV_lp(vector y, real alpha, vector beta, real lower)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lccdf(lower | alpha, beta);
    }
    void sampleBetaLowerBound_VAR_lp(vector y, real[] alpha, real beta, real lower)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lccdf(lower | alpha, beta);
    }
    void sampleBetaLowerBound_VAA_lp(vector y, real[] alpha, real[] beta, real lower)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lccdf(lower | alpha, beta);
    }
    void sampleBetaLowerBound_VAV_lp(vector y, real[] alpha, vector beta, real lower)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lccdf(lower | alpha, beta);
    }
    void sampleBetaLowerBound_VVR_lp(vector y, vector alpha, real beta, real lower)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lccdf(lower | alpha, beta);
    }
    void sampleBetaLowerBound_VVA_lp(vector y, vector alpha, real[] beta, real lower)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lccdf(lower | alpha, beta);
    }
    void sampleBetaLowerBound_VVV_lp(vector y, vector alpha, vector beta, real lower)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lccdf(lower | alpha, beta);
    }

    // Beta distribution with upper bound
    void sampleBetaUpperBound_RRR_lp(real y, real alpha, real beta, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lcdf(upper | alpha, beta);
    }
    //
    void sampleBetaUpperBound_ARR_lp(real[] y, real alpha, real beta, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lcdf(upper | alpha, beta);
    }
    void sampleBetaUpperBound_ARA_lp(real[] y, real alpha, real[] beta, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lcdf(upper | alpha, beta);
    }
    void sampleBetaUpperBound_ARV_lp(real[] y, real alpha, vector beta, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lcdf(upper | alpha, beta);
    }
    void sampleBetaUpperBound_AAR_lp(real[] y, real[] alpha, real beta, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lcdf(upper | alpha, beta);
    }
    void sampleBetaUpperBound_AAA_lp(real[] y, real[] alpha, real[] beta, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lcdf(upper | alpha, beta);
    }
    void sampleBetaUpperBound_AAV_lp(real[] y, real[] alpha, vector beta, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lcdf(upper | alpha, beta);
    }
    void sampleBetaUpperBound_AVR_lp(real[] y, vector alpha, real beta, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lcdf(upper | alpha, beta);
    }
    void sampleBetaUpperBound_AVA_lp(real[] y, vector alpha, real[] beta, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lcdf(upper | alpha, beta);
    }
    void sampleBetaUpperBound_AVV_lp(real[] y, vector alpha, vector beta, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lcdf(upper | alpha, beta);
    }
    //
    void sampleBetaUpperBound_VRR_lp(vector y, real alpha, real beta, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lcdf(upper | alpha, beta);
    }
    void sampleBetaUpperBound_VRA_lp(vector y, real alpha, real[] beta, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lcdf(upper | alpha, beta);
    }
    void sampleBetaUpperBound_VRV_lp(vector y, real alpha, vector beta, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lcdf(upper | alpha, beta);
    }
    void sampleBetaUpperBound_VAR_lp(vector y, real[] alpha, real beta, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lcdf(upper | alpha, beta);
    }
    void sampleBetaUpperBound_VAA_lp(vector y, real[] alpha, real[] beta, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lcdf(upper | alpha, beta);
    }
    void sampleBetaUpperBound_VAV_lp(vector y, real[] alpha, vector beta, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lcdf(upper | alpha, beta);
    }
    void sampleBetaUpperBound_VVR_lp(vector y, vector alpha, real beta, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lcdf(upper | alpha, beta);
    }
    void sampleBetaUpperBound_VVA_lp(vector y, vector alpha, real[] beta, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lcdf(upper | alpha, beta);
    }
    void sampleBetaUpperBound_VVV_lp(vector y, vector alpha, vector beta, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  beta_lcdf(upper | alpha, beta);
    }

    // Beta distribution with lower and upper bound
    void sampleBetaRangeBound_RRR_lp(real y, real alpha, real beta, real lower, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  log_diff_exp(beta_lcdf(upper | alpha, beta),
                               beta_lcdf(lower | alpha, beta));
    }
    //
    void sampleBetaRangeBound_ARR_lp(real[] y, real alpha, real beta, real lower, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  log_diff_exp(beta_lcdf(upper | alpha, beta),
                               beta_lcdf(lower | alpha, beta));
    }
    void sampleBetaRangeBound_ARA_lp(real[] y, real alpha, real[] beta, real lower, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  log_diff_exp(beta_lcdf(upper | alpha, beta),
                               beta_lcdf(lower | alpha, beta));
    }
    void sampleBetaRangeBound_ARV_lp(real[] y, real alpha, vector beta, real lower, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  log_diff_exp(beta_lcdf(upper | alpha, beta),
                               beta_lcdf(lower | alpha, beta));
    }
    void sampleBetaRangeBound_AAR_lp(real[] y, real[] alpha, real beta, real lower, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  log_diff_exp(beta_lcdf(upper | alpha, beta),
                               beta_lcdf(lower | alpha, beta));
    }
    void sampleBetaRangeBound_AAA_lp(real[] y, real[] alpha, real[] beta, real lower, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  log_diff_exp(beta_lcdf(upper | alpha, beta),
                               beta_lcdf(lower | alpha, beta));
    }
    void sampleBetaRangeBound_AAV_lp(real[] y, real[] alpha, vector beta, real lower, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  log_diff_exp(beta_lcdf(upper | alpha, beta),
                               beta_lcdf(lower | alpha, beta));
    }
    void sampleBetaRangeBound_AVR_lp(real[] y, vector alpha, real beta, real lower, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  log_diff_exp(beta_lcdf(upper | alpha, beta),
                               beta_lcdf(lower | alpha, beta));
    }
    void sampleBetaRangeBound_AVA_lp(real[] y, vector alpha, real[] beta, real lower, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  log_diff_exp(beta_lcdf(upper | alpha, beta),
                               beta_lcdf(lower | alpha, beta));
    }
    void sampleBetaRangeBound_AVV_lp(real[] y, vector alpha, vector beta, real lower, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  log_diff_exp(beta_lcdf(upper | alpha, beta),
                               beta_lcdf(lower | alpha, beta));
    }
    //
    void sampleBetaRangeBound_VRR_lp(vector y, real alpha, real beta, real lower, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  log_diff_exp(beta_lcdf(upper | alpha, beta),
                               beta_lcdf(lower | alpha, beta));
    }
    void sampleBetaRangeBound_VRA_lp(vector y, real alpha, real[] beta, real lower, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  log_diff_exp(beta_lcdf(upper | alpha, beta),
                               beta_lcdf(lower | alpha, beta));
    }
    void sampleBetaRangeBound_VRV_lp(vector y, real alpha, vector beta, real lower, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  log_diff_exp(beta_lcdf(upper | alpha, beta),
                               beta_lcdf(lower | alpha, beta));
    }
    void sampleBetaRangeBound_VAR_lp(vector y, real[] alpha, real beta, real lower, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  log_diff_exp(beta_lcdf(upper | alpha, beta),
                               beta_lcdf(lower | alpha, beta));
    }
    void sampleBetaRangeBound_VAA_lp(vector y, real[] alpha, real[] beta, real lower, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  log_diff_exp(beta_lcdf(upper | alpha, beta),
                               beta_lcdf(lower | alpha, beta));
    }
    void sampleBetaRangeBound_VAV_lp(vector y, real[] alpha, vector beta, real lower, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  log_diff_exp(beta_lcdf(upper | alpha, beta),
                               beta_lcdf(lower | alpha, beta));
    }
    void sampleBetaRangeBound_VVR_lp(vector y, vector alpha, real beta, real lower, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  log_diff_exp(beta_lcdf(upper | alpha, beta),
                               beta_lcdf(lower | alpha, beta));
    }
    void sampleBetaRangeBound_VVA_lp(vector y, vector alpha, real[] beta, real lower, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  log_diff_exp(beta_lcdf(upper | alpha, beta),
                               beta_lcdf(lower | alpha, beta));
    }
    void sampleBetaRangeBound_VVV_lp(vector y, vector alpha, vector beta, real lower, real upper)
    {
        target += beta_lpdf(y | alpha, beta) -
                  log_diff_exp(beta_lcdf(upper | alpha, beta),
                               beta_lcdf(lower | alpha, beta));
    }

    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Gamma distribution
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Stan: gamma(alpha, beta), where alpha = shape = k;
    //                                 beta = rate = 1 / scale
    // R:    dgamma(shape, rate) OR dgamma(shape, scale), as you prefer

    void sampleGamma_RRR_lp(real y, real alpha, real beta)
    {
        target += gamma_lpdf(y | alpha, beta);
    }
    //
    void sampleGamma_ARR_lp(real[] y, real alpha, real beta)
    {
        target += gamma_lpdf(y | alpha, beta);
    }
    void sampleGamma_ARA_lp(real[] y, real alpha, real[] beta)
    {
        target += gamma_lpdf(y | alpha, beta);
    }
    void sampleGamma_ARV_lp(real[] y, real alpha, vector beta)
    {
        target += gamma_lpdf(y | alpha, beta);
    }
    void sampleGamma_AAR_lp(real[] y, real[] alpha, real beta)
    {
        target += gamma_lpdf(y | alpha, beta);
    }
    void sampleGamma_AAA_lp(real[] y, real[] alpha, real[] beta)
    {
        target += gamma_lpdf(y | alpha, beta);
    }
    void sampleGamma_AAV_lp(real[] y, real[] alpha, vector beta)
    {
        target += gamma_lpdf(y | alpha, beta);
    }
    void sampleGamma_AVR_lp(real[] y, vector alpha, real beta)
    {
        target += gamma_lpdf(y | alpha, beta);
    }
    void sampleGamma_AVA_lp(real[] y, vector alpha, real[] beta)
    {
        target += gamma_lpdf(y | alpha, beta);
    }
    void sampleGamma_AVV_lp(real[] y, vector alpha, vector beta)
    {
        target += gamma_lpdf(y | alpha, beta);
    }
    //
    void sampleGamma_VRR_lp(vector y, real alpha, real beta)
    {
        target += gamma_lpdf(y | alpha, beta);
    }
    void sampleGamma_VRA_lp(vector y, real alpha, real[] beta)
    {
        target += gamma_lpdf(y | alpha, beta);
    }
    void sampleGamma_VRV_lp(vector y, real alpha, vector beta)
    {
        target += gamma_lpdf(y | alpha, beta);
    }
    void sampleGamma_VAR_lp(vector y, real[] alpha, real beta)
    {
        target += gamma_lpdf(y | alpha, beta);
    }
    void sampleGamma_VAA_lp(vector y, real[] alpha, real[] beta)
    {
        target += gamma_lpdf(y | alpha, beta);
    }
    void sampleGamma_VAV_lp(vector y, real[] alpha, vector beta)
    {
        target += gamma_lpdf(y | alpha, beta);
    }
    void sampleGamma_VVR_lp(vector y, vector alpha, real beta)
    {
        target += gamma_lpdf(y | alpha, beta);
    }
    void sampleGamma_VVA_lp(vector y, vector alpha, real[] beta)
    {
        target += gamma_lpdf(y | alpha, beta);
    }
    void sampleGamma_VVV_lp(vector y, vector alpha, vector beta)
    {
        target += gamma_lpdf(y | alpha, beta);
    }

    // Gamma distribution with lower bound
    void sampleGammaLowerBound_RRR_lp(real y, real alpha, real beta, real lower)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lccdf(lower | alpha, beta);
    }
    //
    void sampleGammaLowerBound_ARR_lp(real[] y, real alpha, real beta, real lower)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lccdf(lower | alpha, beta);
    }
    void sampleGammaLowerBound_ARA_lp(real[] y, real alpha, real[] beta, real lower)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lccdf(lower | alpha, beta);
    }
    void sampleGammaLowerBound_ARV_lp(real[] y, real alpha, vector beta, real lower)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lccdf(lower | alpha, beta);
    }
    void sampleGammaLowerBound_AAR_lp(real[] y, real[] alpha, real beta, real lower)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lccdf(lower | alpha, beta);
    }
    void sampleGammaLowerBound_AAA_lp(real[] y, real[] alpha, real[] beta, real lower)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lccdf(lower | alpha, beta);
    }
    void sampleGammaLowerBound_AAV_lp(real[] y, real[] alpha, vector beta, real lower)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lccdf(lower | alpha, beta);
    }
    void sampleGammaLowerBound_AVR_lp(real[] y, vector alpha, real beta, real lower)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lccdf(lower | alpha, beta);
    }
    void sampleGammaLowerBound_AVA_lp(real[] y, vector alpha, real[] beta, real lower)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lccdf(lower | alpha, beta);
    }
    void sampleGammaLowerBound_AVV_lp(real[] y, vector alpha, vector beta, real lower)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lccdf(lower | alpha, beta);
    }
    //
    void sampleGammaLowerBound_VRR_lp(vector y, real alpha, real beta, real lower)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lccdf(lower | alpha, beta);
    }
    void sampleGammaLowerBound_VRA_lp(vector y, real alpha, real[] beta, real lower)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lccdf(lower | alpha, beta);
    }
    void sampleGammaLowerBound_VRV_lp(vector y, real alpha, vector beta, real lower)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lccdf(lower | alpha, beta);
    }
    void sampleGammaLowerBound_VAR_lp(vector y, real[] alpha, real beta, real lower)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lccdf(lower | alpha, beta);
    }
    void sampleGammaLowerBound_VAA_lp(vector y, real[] alpha, real[] beta, real lower)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lccdf(lower | alpha, beta);
    }
    void sampleGammaLowerBound_VAV_lp(vector y, real[] alpha, vector beta, real lower)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lccdf(lower | alpha, beta);
    }
    void sampleGammaLowerBound_VVR_lp(vector y, vector alpha, real beta, real lower)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lccdf(lower | alpha, beta);
    }
    void sampleGammaLowerBound_VVA_lp(vector y, vector alpha, real[] beta, real lower)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lccdf(lower | alpha, beta);
    }
    void sampleGammaLowerBound_VVV_lp(vector y, vector alpha, vector beta, real lower)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lccdf(lower | alpha, beta);
    }

    // Gamma distribution with upper bound
    void sampleGammaUpperBound_RRR_lp(real y, real alpha, real beta, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lcdf(upper | alpha, beta);
    }
    //
    void sampleGammaUpperBound_ARR_lp(real[] y, real alpha, real beta, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lcdf(upper | alpha, beta);
    }
    void sampleGammaUpperBound_ARA_lp(real[] y, real alpha, real[] beta, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lcdf(upper | alpha, beta);
    }
    void sampleGammaUpperBound_ARV_lp(real[] y, real alpha, vector beta, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lcdf(upper | alpha, beta);
    }
    void sampleGammaUpperBound_AAR_lp(real[] y, real[] alpha, real beta, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lcdf(upper | alpha, beta);
    }
    void sampleGammaUpperBound_AAA_lp(real[] y, real[] alpha, real[] beta, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lcdf(upper | alpha, beta);
    }
    void sampleGammaUpperBound_AAV_lp(real[] y, real[] alpha, vector beta, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lcdf(upper | alpha, beta);
    }
    void sampleGammaUpperBound_AVR_lp(real[] y, vector alpha, real beta, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lcdf(upper | alpha, beta);
    }
    void sampleGammaUpperBound_AVA_lp(real[] y, vector alpha, real[] beta, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lcdf(upper | alpha, beta);
    }
    void sampleGammaUpperBound_AVV_lp(real[] y, vector alpha, vector beta, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lcdf(upper | alpha, beta);
    }
    //
    void sampleGammaUpperBound_VRR_lp(vector y, real alpha, real beta, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lcdf(upper | alpha, beta);
    }
    void sampleGammaUpperBound_VRA_lp(vector y, real alpha, real[] beta, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lcdf(upper | alpha, beta);
    }
    void sampleGammaUpperBound_VRV_lp(vector y, real alpha, vector beta, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lcdf(upper | alpha, beta);
    }
    void sampleGammaUpperBound_VAR_lp(vector y, real[] alpha, real beta, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lcdf(upper | alpha, beta);
    }
    void sampleGammaUpperBound_VAA_lp(vector y, real[] alpha, real[] beta, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lcdf(upper | alpha, beta);
    }
    void sampleGammaUpperBound_VAV_lp(vector y, real[] alpha, vector beta, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lcdf(upper | alpha, beta);
    }
    void sampleGammaUpperBound_VVR_lp(vector y, vector alpha, real beta, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lcdf(upper | alpha, beta);
    }
    void sampleGammaUpperBound_VVA_lp(vector y, vector alpha, real[] beta, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lcdf(upper | alpha, beta);
    }
    void sampleGammaUpperBound_VVV_lp(vector y, vector alpha, vector beta, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  gamma_lcdf(upper | alpha, beta);
    }

    // Gamma distribution with lower and upper bound
    void sampleGammaRangeBound_RRR_lp(real y, real alpha, real beta, real lower, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  log_diff_exp(gamma_lcdf(upper | alpha, beta),
                               gamma_lcdf(lower | alpha, beta));
    }
    //
    void sampleGammaRangeBound_ARR_lp(real[] y, real alpha, real beta, real lower, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  log_diff_exp(gamma_lcdf(upper | alpha, beta),
                               gamma_lcdf(lower | alpha, beta));
    }
    void sampleGammaRangeBound_ARA_lp(real[] y, real alpha, real[] beta, real lower, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  log_diff_exp(gamma_lcdf(upper | alpha, beta),
                               gamma_lcdf(lower | alpha, beta));
    }
    void sampleGammaRangeBound_ARV_lp(real[] y, real alpha, vector beta, real lower, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  log_diff_exp(gamma_lcdf(upper | alpha, beta),
                               gamma_lcdf(lower | alpha, beta));
    }
    void sampleGammaRangeBound_AAR_lp(real[] y, real[] alpha, real beta, real lower, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  log_diff_exp(gamma_lcdf(upper | alpha, beta),
                               gamma_lcdf(lower | alpha, beta));
    }
    void sampleGammaRangeBound_AAA_lp(real[] y, real[] alpha, real[] beta, real lower, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  log_diff_exp(gamma_lcdf(upper | alpha, beta),
                               gamma_lcdf(lower | alpha, beta));
    }
    void sampleGammaRangeBound_AAV_lp(real[] y, real[] alpha, vector beta, real lower, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  log_diff_exp(gamma_lcdf(upper | alpha, beta),
                               gamma_lcdf(lower | alpha, beta));
    }
    void sampleGammaRangeBound_AVR_lp(real[] y, vector alpha, real beta, real lower, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  log_diff_exp(gamma_lcdf(upper | alpha, beta),
                               gamma_lcdf(lower | alpha, beta));
    }
    void sampleGammaRangeBound_AVA_lp(real[] y, vector alpha, real[] beta, real lower, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  log_diff_exp(gamma_lcdf(upper | alpha, beta),
                               gamma_lcdf(lower | alpha, beta));
    }
    void sampleGammaRangeBound_AVV_lp(real[] y, vector alpha, vector beta, real lower, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  log_diff_exp(gamma_lcdf(upper | alpha, beta),
                               gamma_lcdf(lower | alpha, beta));
    }
    //
    void sampleGammaRangeBound_VRR_lp(vector y, real alpha, real beta, real lower, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  log_diff_exp(gamma_lcdf(upper | alpha, beta),
                               gamma_lcdf(lower | alpha, beta));
    }
    void sampleGammaRangeBound_VRA_lp(vector y, real alpha, real[] beta, real lower, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  log_diff_exp(gamma_lcdf(upper | alpha, beta),
                               gamma_lcdf(lower | alpha, beta));
    }
    void sampleGammaRangeBound_VRV_lp(vector y, real alpha, vector beta, real lower, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  log_diff_exp(gamma_lcdf(upper | alpha, beta),
                               gamma_lcdf(lower | alpha, beta));
    }
    void sampleGammaRangeBound_VAR_lp(vector y, real[] alpha, real beta, real lower, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  log_diff_exp(gamma_lcdf(upper | alpha, beta),
                               gamma_lcdf(lower | alpha, beta));
    }
    void sampleGammaRangeBound_VAA_lp(vector y, real[] alpha, real[] beta, real lower, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  log_diff_exp(gamma_lcdf(upper | alpha, beta),
                               gamma_lcdf(lower | alpha, beta));
    }
    void sampleGammaRangeBound_VAV_lp(vector y, real[] alpha, vector beta, real lower, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  log_diff_exp(gamma_lcdf(upper | alpha, beta),
                               gamma_lcdf(lower | alpha, beta));
    }
    void sampleGammaRangeBound_VVR_lp(vector y, vector alpha, real beta, real lower, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  log_diff_exp(gamma_lcdf(upper | alpha, beta),
                               gamma_lcdf(lower | alpha, beta));
    }
    void sampleGammaRangeBound_VVA_lp(vector y, vector alpha, real[] beta, real lower, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  log_diff_exp(gamma_lcdf(upper | alpha, beta),
                               gamma_lcdf(lower | alpha, beta));
    }
    void sampleGammaRangeBound_VVV_lp(vector y, vector alpha, vector beta, real lower, real upper)
    {
        target += gamma_lpdf(y | alpha, beta) -
                  log_diff_exp(gamma_lcdf(upper | alpha, beta),
                               gamma_lcdf(lower | alpha, beta));
    }

    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Uniform distribution
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Always constrained with both a lower and an upper bound.
    // Simple; no extra work for the bridge sampler.

    void sampleUniform_RRR_lp(real y, real lower, real upper)
    {
        target += uniform_lpdf(y | lower, upper);
        // Probably equivalent in this case to: y ~ uniform(lower, upper);
    }
    //
    void sampleUniform_ARR_lp(real[] y, real lower, real upper)
    {
        target += uniform_lpdf(y | lower, upper);
    }
    void sampleUniform_ARA_lp(real[] y, real lower, real[] upper)
    {
        target += uniform_lpdf(y | lower, upper);
    }
    void sampleUniform_ARV_lp(real[] y, real lower, vector upper)
    {
        target += uniform_lpdf(y | lower, upper);
    }
    void sampleUniform_AAR_lp(real[] y, real[] lower, real upper)
    {
        target += uniform_lpdf(y | lower, upper);
    }
    void sampleUniform_AAA_lp(real[] y, real[] lower, real[] upper)
    {
        target += uniform_lpdf(y | lower, upper);
    }
    void sampleUniform_AAV_lp(real[] y, real[] lower, vector upper)
    {
        target += uniform_lpdf(y | lower, upper);
    }
    void sampleUniform_AVR_lp(real[] y, vector lower, real upper)
    {
        target += uniform_lpdf(y | lower, upper);
    }
    void sampleUniform_AVA_lp(real[] y, vector lower, real[] upper)
    {
        target += uniform_lpdf(y | lower, upper);
    }
    void sampleUniform_AVV_lp(real[] y, vector lower, vector upper)
    {
        target += uniform_lpdf(y | lower, upper);
    }
    //
    void sampleUniform_VRR_lp(vector y, real lower, real upper)
    {
        target += uniform_lpdf(y | lower, upper);
    }
    void sampleUniform_VRA_lp(vector y, real lower, real[] upper)
    {
        target += uniform_lpdf(y | lower, upper);
    }
    void sampleUniform_VRV_lp(vector y, real lower, vector upper)
    {
        target += uniform_lpdf(y | lower, upper);
    }
    void sampleUniform_VAR_lp(vector y, real[] lower, real upper)
    {
        target += uniform_lpdf(y | lower, upper);
    }
    void sampleUniform_VAA_lp(vector y, real[] lower, real[] upper)
    {
        target += uniform_lpdf(y | lower, upper);
    }
    void sampleUniform_VAV_lp(vector y, real[] lower, vector upper)
    {
        target += uniform_lpdf(y | lower, upper);
    }
    void sampleUniform_VVR_lp(vector y, vector lower, real upper)
    {
        target += uniform_lpdf(y | lower, upper);
    }
    void sampleUniform_VVA_lp(vector y, vector lower, real[] upper)
    {
        target += uniform_lpdf(y | lower, upper);
    }
    void sampleUniform_VVV_lp(vector y, vector lower, vector upper)
    {
        target += uniform_lpdf(y | lower, upper);
    }

    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Bernoulli distribution
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // y is in {0, 1} and theta is in the range [0, 1].

    void sampleBernoulli_IR_lp(int y, real theta)
    {
        target += bernoulli_lpmf(y | theta);
    }
    void sampleBernoulli_AA_lp(int[] y, real[] theta)
    {
        target += bernoulli_lpmf(y | theta);
    }
    void sampleBernoulli_AV_lp(int[] y, vector theta)
    {
        target += bernoulli_lpmf(y | theta);
    }

    // ------------------------------------------------------------------------
    // LOG PROBABILITY FUNCTIONS FOR BRIDGE SAMPLING WITH NON-CENTERED
    // REPARAMETERIZATION
    // ------------------------------------------------------------------------

    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Normal distribution, reparameterized to the unit normal distribution
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Compare Stan (2017) manual p299, but we use a bridgesampling version.
    // Note that we cannot do:
    //      real * real[]
    //      vector * vector

    real getReparameterizedNormal_RRR_lp(real y_unit_normal, real mu, real sigma)
    {
        sampleNormal_RRR_lp(y_unit_normal, 0, 1);
        return mu + sigma * y_unit_normal;
    }
    //
    real[] getReparameterizedNormal_ARR_lp(real[] y_unit_normal, real mu, real sigma)
    {
        int length = num_elements(y_unit_normal);
        real result[length];
        sampleNormal_ARR_lp(y_unit_normal, 0, 1);
        for (i in 1:length) {
            result[i] = mu + sigma * y_unit_normal[i];
        }
        return result;
    }
    real[] getReparameterizedNormal_ARA_lp(real[] y_unit_normal, real mu, real[] sigma)
    {
        int length = num_elements(y_unit_normal);
        real result[length];
        if (num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        sampleNormal_ARR_lp(y_unit_normal, 0, 1);
        for (i in 1:length) {
            result[i] = mu + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    real[] getReparameterizedNormal_ARV_lp(real[] y_unit_normal, real mu, vector sigma)
    {
        int length = num_elements(y_unit_normal);
        real result[length];
        if (num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        sampleNormal_ARR_lp(y_unit_normal, 0, 1);
        for (i in 1:length) {
            result[i] = mu + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    real[] getReparameterizedNormal_AAR_lp(real[] y_unit_normal, real[] mu, real sigma)
    {
        int length = num_elements(y_unit_normal);
        real result[length];
        if (num_elements(mu) != length) {
            reject("Incompatible arguments");
        }
        sampleNormal_ARR_lp(y_unit_normal, 0, 1);
        for (i in 1:length) {
            result[i] = mu[i] + sigma * y_unit_normal[i];
        }
        return result;
    }
    real[] getReparameterizedNormal_AAA_lp(real[] y_unit_normal, real[] mu, real[] sigma)
    {
        int length = num_elements(y_unit_normal);
        real result[length];
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        sampleNormal_ARR_lp(y_unit_normal, 0, 1);
        for (i in 1:length) {
            result[i] = mu[i] + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    real[] getReparameterizedNormal_AAV_lp(real[] y_unit_normal, real[] mu, vector sigma)
    {
        int length = num_elements(y_unit_normal);
        real result[length];
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        sampleNormal_ARR_lp(y_unit_normal, 0, 1);
        for (i in 1:length) {
            result[i] = mu[i] + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    real[] getReparameterizedNormal_AVR_lp(real[] y_unit_normal, vector mu, real sigma)
    {
        int length = num_elements(y_unit_normal);
        real result[length];
        if (num_elements(mu) != length) {
            reject("Incompatible arguments");
        }
        sampleNormal_ARR_lp(y_unit_normal, 0, 1);
        for (i in 1:length) {
            result[i] = mu[i] + sigma * y_unit_normal[i];
        }
        return result;
    }
    real[] getReparameterizedNormal_AVA_lp(real[] y_unit_normal, vector mu, real[] sigma)
    {
        int length = num_elements(y_unit_normal);
        real result[length];
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        sampleNormal_ARR_lp(y_unit_normal, 0, 1);
        for (i in 1:length) {
            result[i] = mu[i] + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    real[] getReparameterizedNormal_AVV_lp(real[] y_unit_normal, vector mu, vector sigma)
    {
        int length = num_elements(y_unit_normal);
        real result[length];
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        sampleNormal_ARR_lp(y_unit_normal, 0, 1);
        for (i in 1:length) {
            result[i] = mu[i] + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    //
    vector getReparameterizedNormal_VRR_lp(vector y_unit_normal, real mu, real sigma)
    {
        int length = num_elements(y_unit_normal);
        vector[length] result;
        sampleNormal_VRR_lp(y_unit_normal, 0, 1);
        result = mu + sigma * y_unit_normal;
        return result;
    }
    vector getReparameterizedNormal_VRA_lp(vector y_unit_normal, real mu, real[] sigma)
    {
        int length = num_elements(y_unit_normal);
        vector[length] result;
        if (num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        sampleNormal_VRR_lp(y_unit_normal, 0, 1);
        for (i in 1:length) {
            result[i] = mu + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    vector getReparameterizedNormal_VRV_lp(vector y_unit_normal, real mu, vector sigma)
    {
        int length = num_elements(y_unit_normal);
        vector[length] result;
        if (num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        sampleNormal_VRR_lp(y_unit_normal, 0, 1);
        for (i in 1:length) {
            result[i] = mu + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    vector getReparameterizedNormal_VAR_lp(vector y_unit_normal, real[] mu, real sigma)
    {
        int length = num_elements(y_unit_normal);
        vector[length] result;
        if (num_elements(mu) != length) {
            reject("Incompatible arguments");
        }
        sampleNormal_VRR_lp(y_unit_normal, 0, 1);
        for (i in 1:length) {
            result[i] = mu[i] + sigma * y_unit_normal[i];
        }
        return result;
    }
    vector getReparameterizedNormal_VAA_lp(vector y_unit_normal, real[] mu, real[] sigma)
    {
        int length = num_elements(y_unit_normal);
        vector[length] result;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        sampleNormal_VRR_lp(y_unit_normal, 0, 1);
        for (i in 1:length) {
            result[i] = mu[i] + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    vector getReparameterizedNormal_VAV_lp(vector y_unit_normal, real[] mu, vector sigma)
    {
        int length = num_elements(y_unit_normal);
        vector[length] result;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        sampleNormal_VRR_lp(y_unit_normal, 0, 1);
        for (i in 1:length) {
            result[i] = mu[i] + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    vector getReparameterizedNormal_VVR_lp(vector y_unit_normal, vector mu, real sigma)
    {
        int length = num_elements(y_unit_normal);
        vector[length] result;
        if (num_elements(mu) != length) {
            reject("Incompatible arguments");
        }
        sampleNormal_VRR_lp(y_unit_normal, 0, 1);
        result = mu + sigma * y_unit_normal;
        return result;
    }
    vector getReparameterizedNormal_VVA_lp(vector y_unit_normal, vector mu, real[] sigma)
    {
        int length = num_elements(y_unit_normal);
        vector[length] result;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        sampleNormal_VRR_lp(y_unit_normal, 0, 1);
        for (i in 1:length) {
            result[i] = mu[i] + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    vector getReparameterizedNormal_VVV_lp(vector y_unit_normal, vector mu, vector sigma)
    {
        int length = num_elements(y_unit_normal);
        vector[length] result;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        sampleNormal_VRR_lp(y_unit_normal, 0, 1);
        for (i in 1:length) {
            result[i] = mu[i] + sigma[i] * y_unit_normal[i];
        }
        return result;
    }

    // Lower bound
    // The user specifies the boundary in "target space". So we convert.
    // Note that for array/vector input, the boundary is specific to the mean/
    // SD being sampled, and all our boundaried sampling functions take a
    // "real" boundary, so we use the RRR sampler.

    real getReparameterizedNormalLowerBound_RRR_lp(real y_unit_normal, real mu, real sigma, real lower)
    {
        real lower_transformed = (lower - mu) / sigma;
        sampleNormalLowerBound_RRR_lp(y_unit_normal, 0, 1, lower_transformed);
        return mu + sigma * y_unit_normal;
    }
    //
    real[] getReparameterizedNormalLowerBound_ARR_lp(real[] y_unit_normal, real mu, real sigma, real lower)
    {
        int length = num_elements(y_unit_normal);
        real result[length];
        real lower_transformed = (lower - mu) / sigma;
        sampleNormalLowerBound_ARR_lp(y_unit_normal, 0, 1, lower_transformed);
        for (i in 1:length) {
            result[i] = mu + sigma * y_unit_normal[i];
        }
        return result;
    }
    real[] getReparameterizedNormalLowerBound_ARA_lp(real[] y_unit_normal, real mu, real[] sigma, real lower)
    {
        int length = num_elements(y_unit_normal);
        real result[length];
        real lower_transformed;
        if (num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = (lower - mu) / sigma[i];
            sampleNormalLowerBound_RRR_lp(y_unit_normal[i], 0, 1, lower_transformed);
            result[i] = mu + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    real[] getReparameterizedNormalLowerBound_ARV_lp(real[] y_unit_normal, real mu, vector sigma, real lower)
    {
        int length = num_elements(y_unit_normal);
        real result[length];
        real lower_transformed;
        if (num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = (lower - mu) / sigma[i];
            sampleNormalLowerBound_RRR_lp(y_unit_normal[i], 0, 1, lower_transformed);
            result[i] = mu + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    real[] getReparameterizedNormalLowerBound_AAR_lp(real[] y_unit_normal, real[] mu, real sigma, real lower)
    {
        int length = num_elements(y_unit_normal);
        real result[length];
        real lower_transformed;
        if (num_elements(mu) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = (lower - mu[i]) / sigma;
            sampleNormalLowerBound_RRR_lp(y_unit_normal[i], 0, 1, lower_transformed);
            result[i] = mu[i] + sigma * y_unit_normal[i];
        }
        return result;
    }
    real[] getReparameterizedNormalLowerBound_AAA_lp(real[] y_unit_normal, real[] mu, real[] sigma, real lower)
    {
        int length = num_elements(y_unit_normal);
        real result[length];
        real lower_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = (lower - mu[i]) / sigma[i];
            sampleNormalLowerBound_RRR_lp(y_unit_normal[i], 0, 1, lower_transformed);
            result[i] = mu[i] + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    real[] getReparameterizedNormalLowerBound_AAV_lp(real[] y_unit_normal, real[] mu, vector sigma, real lower)
    {
        int length = num_elements(y_unit_normal);
        real result[length];
        real lower_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = (lower - mu[i]) / sigma[i];
            sampleNormalLowerBound_RRR_lp(y_unit_normal[i], 0, 1, lower_transformed);
            result[i] = mu[i] + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    real[] getReparameterizedNormalLowerBound_AVR_lp(real[] y_unit_normal, vector mu, real sigma, real lower)
    {
        int length = num_elements(y_unit_normal);
        real result[length];
        real lower_transformed;
        if (num_elements(mu) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = (lower - mu[i]) / sigma;
            sampleNormalLowerBound_RRR_lp(y_unit_normal[i], 0, 1, lower_transformed);
            result[i] = mu[i] + sigma * y_unit_normal[i];
        }
        return result;
    }
    real[] getReparameterizedNormalLowerBound_AVA_lp(real[] y_unit_normal, vector mu, real[] sigma, real lower)
    {
        int length = num_elements(y_unit_normal);
        real result[length];
        real lower_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = (lower - mu[i]) / sigma[i];
            sampleNormalLowerBound_RRR_lp(y_unit_normal[i], 0, 1, lower_transformed);
            result[i] = mu[i] + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    real[] getReparameterizedNormalLowerBound_AVV_lp(real[] y_unit_normal, vector mu, vector sigma, real lower)
    {
        int length = num_elements(y_unit_normal);
        real result[length];
        real lower_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = (lower - mu[i]) / sigma[i];
            sampleNormalLowerBound_RRR_lp(y_unit_normal[i], 0, 1, lower_transformed);
            result[i] = mu[i] + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    //
    vector getReparameterizedNormalLowerBound_VRR_lp(vector y_unit_normal, real mu, real sigma, real lower)
    {
        int length = num_elements(y_unit_normal);
        vector[length] result;
        real lower_transformed = (lower - mu) / sigma;
        sampleNormalLowerBound_VRR_lp(y_unit_normal, 0, 1, lower_transformed);
        result = mu + sigma * y_unit_normal;
        return result;
    }
    vector getReparameterizedNormalLowerBound_VRA_lp(vector y_unit_normal, real mu, real[] sigma, real lower)
    {
        int length = num_elements(y_unit_normal);
        vector[length] result;
        real lower_transformed;
        if (num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = (lower - mu) / sigma[i];
            sampleNormalLowerBound_RRR_lp(y_unit_normal[i], 0, 1, lower_transformed);
            result[i] = mu + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    vector getReparameterizedNormalLowerBound_VRV_lp(vector y_unit_normal, real mu, vector sigma, real lower)
    {
        int length = num_elements(y_unit_normal);
        vector[length] result;
        real lower_transformed;
        if (num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = (lower - mu) / sigma[i];
            sampleNormalLowerBound_RRR_lp(y_unit_normal[i], 0, 1, lower_transformed);
            result[i] = mu + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    vector getReparameterizedNormalLowerBound_VAR_lp(vector y_unit_normal, real[] mu, real sigma, real lower)
    {
        int length = num_elements(y_unit_normal);
        vector[length] result;
        real lower_transformed;
        if (num_elements(mu) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = (lower - mu[i]) / sigma;
            sampleNormalLowerBound_RRR_lp(y_unit_normal[i], 0, 1, lower_transformed);
            result[i] = mu[i] + sigma * y_unit_normal[i];
        }
        return result;
    }
    vector getReparameterizedNormalLowerBound_VAA_lp(vector y_unit_normal, real[] mu, real[] sigma, real lower)
    {
        int length = num_elements(y_unit_normal);
        vector[length] result;
        real lower_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = (lower - mu[i]) / sigma[i];
            sampleNormalLowerBound_RRR_lp(y_unit_normal[i], 0, 1, lower_transformed);
            result[i] = mu[i] + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    vector getReparameterizedNormalLowerBound_VAV_lp(vector y_unit_normal, real[] mu, vector sigma, real lower)
    {
        int length = num_elements(y_unit_normal);
        vector[length] result;
        real lower_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = (lower - mu[i]) / sigma[i];
            sampleNormalLowerBound_RRR_lp(y_unit_normal[i], 0, 1, lower_transformed);
            result[i] = mu[i] + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    vector getReparameterizedNormalLowerBound_VVR_lp(vector y_unit_normal, vector mu, real sigma, real lower)
    {
        int length = num_elements(y_unit_normal);
        vector[length] result;
        real lower_transformed;
        if (num_elements(mu) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = (lower - mu[i]) / sigma;
            sampleNormalLowerBound_RRR_lp(y_unit_normal[i], 0, 1, lower_transformed);
        }
        result = mu + sigma * y_unit_normal;
        return result;
    }
    vector getReparameterizedNormalLowerBound_VVA_lp(vector y_unit_normal, vector mu, real[] sigma, real lower)
    {
        int length = num_elements(y_unit_normal);
        vector[length] result;
        real lower_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = (lower - mu[i]) / sigma[i];
            sampleNormalLowerBound_RRR_lp(y_unit_normal[i], 0, 1, lower_transformed);
            result[i] = mu[i] + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    vector getReparameterizedNormalLowerBound_VVV_lp(vector y_unit_normal, vector mu, vector sigma, real lower)
    {
        int length = num_elements(y_unit_normal);
        vector[length] result;
        real lower_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = (lower - mu[i]) / sigma[i];
            sampleNormalLowerBound_RRR_lp(y_unit_normal[i], 0, 1, lower_transformed);
            result[i] = mu[i] + sigma[i] * y_unit_normal[i];
        }
        return result;
    }

    // Upper bound

    real getReparameterizedNormalUpperBound_RRR_lp(real y_unit_normal, real mu, real sigma, real upper)
    {
        real upper_transformed = (upper - mu) / sigma;
        sampleNormalUpperBound_RRR_lp(y_unit_normal, 0, 1, upper_transformed);
        return mu + sigma * y_unit_normal;
    }
    //
    real[] getReparameterizedNormalUpperBound_ARR_lp(real[] y_unit_normal, real mu, real sigma, real upper)
    {
        int length = num_elements(y_unit_normal);
        real result[length];
        real upper_transformed = (upper - mu) / sigma;
        sampleNormalUpperBound_ARR_lp(y_unit_normal, 0, 1, upper_transformed);
        for (i in 1:length) {
            result[i] = mu + sigma * y_unit_normal[i];
        }
        return result;
    }
    real[] getReparameterizedNormalUpperBound_ARA_lp(real[] y_unit_normal, real mu, real[] sigma, real upper)
    {
        int length = num_elements(y_unit_normal);
        real result[length];
        real upper_transformed;
        if (num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            upper_transformed = (upper - mu) / sigma[i];
            sampleNormalUpperBound_RRR_lp(y_unit_normal[i], 0, 1, upper_transformed);
            result[i] = mu + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    real[] getReparameterizedNormalUpperBound_ARV_lp(real[] y_unit_normal, real mu, vector sigma, real upper)
    {
        int length = num_elements(y_unit_normal);
        real result[length];
        real upper_transformed;
        if (num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            upper_transformed = (upper - mu) / sigma[i];
            sampleNormalUpperBound_RRR_lp(y_unit_normal[i], 0, 1, upper_transformed);
            result[i] = mu + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    real[] getReparameterizedNormalUpperBound_AAR_lp(real[] y_unit_normal, real[] mu, real sigma, real upper)
    {
        int length = num_elements(y_unit_normal);
        real result[length];
        real upper_transformed;
        if (num_elements(mu) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            upper_transformed = (upper - mu[i]) / sigma;
            sampleNormalUpperBound_RRR_lp(y_unit_normal[i], 0, 1, upper_transformed);
            result[i] = mu[i] + sigma * y_unit_normal[i];
        }
        return result;
    }
    real[] getReparameterizedNormalUpperBound_AAA_lp(real[] y_unit_normal, real[] mu, real[] sigma, real upper)
    {
        int length = num_elements(y_unit_normal);
        real result[length];
        real upper_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            upper_transformed = (upper - mu[i]) / sigma[i];
            sampleNormalUpperBound_RRR_lp(y_unit_normal[i], 0, 1, upper_transformed);
            result[i] = mu[i] + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    real[] getReparameterizedNormalUpperBound_AAV_lp(real[] y_unit_normal, real[] mu, vector sigma, real upper)
    {
        int length = num_elements(y_unit_normal);
        real result[length];
        real upper_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            upper_transformed = (upper - mu[i]) / sigma[i];
            sampleNormalUpperBound_RRR_lp(y_unit_normal[i], 0, 1, upper_transformed);
            result[i] = mu[i] + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    real[] getReparameterizedNormalUpperBound_AVR_lp(real[] y_unit_normal, vector mu, real sigma, real upper)
    {
        int length = num_elements(y_unit_normal);
        real result[length];
        real upper_transformed;
        if (num_elements(mu) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            upper_transformed = (upper - mu[i]) / sigma;
            sampleNormalUpperBound_RRR_lp(y_unit_normal[i], 0, 1, upper_transformed);
            result[i] = mu[i] + sigma * y_unit_normal[i];
        }
        return result;
    }
    real[] getReparameterizedNormalUpperBound_AVA_lp(real[] y_unit_normal, vector mu, real[] sigma, real upper)
    {
        int length = num_elements(y_unit_normal);
        real result[length];
        real upper_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            upper_transformed = (upper - mu[i]) / sigma[i];
            sampleNormalUpperBound_RRR_lp(y_unit_normal[i], 0, 1, upper_transformed);
            result[i] = mu[i] + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    real[] getReparameterizedNormalUpperBound_AVV_lp(real[] y_unit_normal, vector mu, vector sigma, real upper)
    {
        int length = num_elements(y_unit_normal);
        real result[length];
        real upper_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            upper_transformed = (upper - mu[i]) / sigma[i];
            sampleNormalUpperBound_RRR_lp(y_unit_normal[i], 0, 1, upper_transformed);
            result[i] = mu[i] + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    //
    vector getReparameterizedNormalUpperBound_VRR_lp(vector y_unit_normal, real mu, real sigma, real upper)
    {
        int length = num_elements(y_unit_normal);
        vector[length] result;
        real upper_transformed = (upper - mu) / sigma;
        sampleNormalUpperBound_VRR_lp(y_unit_normal, 0, 1, upper_transformed);
        result = mu + sigma * y_unit_normal;
        return result;
    }
    vector getReparameterizedNormalUpperBound_VRA_lp(vector y_unit_normal, real mu, real[] sigma, real upper)
    {
        int length = num_elements(y_unit_normal);
        vector[length] result;
        real upper_transformed;
        if (num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            upper_transformed = (upper - mu) / sigma[i];
            sampleNormalUpperBound_RRR_lp(y_unit_normal[i], 0, 1, upper_transformed);
            result[i] = mu + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    vector getReparameterizedNormalUpperBound_VRV_lp(vector y_unit_normal, real mu, vector sigma, real upper)
    {
        int length = num_elements(y_unit_normal);
        vector[length] result;
        real upper_transformed;
        if (num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            upper_transformed = (upper - mu) / sigma[i];
            sampleNormalUpperBound_RRR_lp(y_unit_normal[i], 0, 1, upper_transformed);
            result[i] = mu + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    vector getReparameterizedNormalUpperBound_VAR_lp(vector y_unit_normal, real[] mu, real sigma, real upper)
    {
        int length = num_elements(y_unit_normal);
        vector[length] result;
        real upper_transformed;
        if (num_elements(mu) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            upper_transformed = (upper - mu[i]) / sigma;
            sampleNormalUpperBound_RRR_lp(y_unit_normal[i], 0, 1, upper_transformed);
            result[i] = mu[i] + sigma * y_unit_normal[i];
        }
        return result;
    }
    vector getReparameterizedNormalUpperBound_VAA_lp(vector y_unit_normal, real[] mu, real[] sigma, real upper)
    {
        int length = num_elements(y_unit_normal);
        vector[length] result;
        real upper_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            upper_transformed = (upper - mu[i]) / sigma[i];
            sampleNormalUpperBound_RRR_lp(y_unit_normal[i], 0, 1, upper_transformed);
            result[i] = mu[i] + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    vector getReparameterizedNormalUpperBound_VAV_lp(vector y_unit_normal, real[] mu, vector sigma, real upper)
    {
        int length = num_elements(y_unit_normal);
        vector[length] result;
        real upper_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            upper_transformed = (upper - mu[i]) / sigma[i];
            sampleNormalUpperBound_RRR_lp(y_unit_normal[i], 0, 1, upper_transformed);
            result[i] = mu[i] + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    vector getReparameterizedNormalUpperBound_VVR_lp(vector y_unit_normal, vector mu, real sigma, real upper)
    {
        int length = num_elements(y_unit_normal);
        vector[length] result;
        real upper_transformed;
        if (num_elements(mu) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            upper_transformed = (upper - mu[i]) / sigma;
            sampleNormalUpperBound_RRR_lp(y_unit_normal[i], 0, 1, upper_transformed);
        }
        result = mu + sigma * y_unit_normal;
        return result;
    }
    vector getReparameterizedNormalUpperBound_VVA_lp(vector y_unit_normal, vector mu, real[] sigma, real upper)
    {
        int length = num_elements(y_unit_normal);
        vector[length] result;
        real upper_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            upper_transformed = (upper - mu[i]) / sigma[i];
            sampleNormalUpperBound_RRR_lp(y_unit_normal[i], 0, 1, upper_transformed);
            result[i] = mu[i] + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    vector getReparameterizedNormalUpperBound_VVV_lp(vector y_unit_normal, vector mu, vector sigma, real upper)
    {
        int length = num_elements(y_unit_normal);
        vector[length] result;
        real upper_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            upper_transformed = (upper - mu[i]) / sigma[i];
            sampleNormalUpperBound_RRR_lp(y_unit_normal[i], 0, 1, upper_transformed);
            result[i] = mu[i] + sigma[i] * y_unit_normal[i];
        }
        return result;
    }

    // Range bound

    real getReparameterizedNormalRangeBound_RRR_lp(real y_unit_normal, real mu, real sigma, real lower, real upper)
    {
        real lower_transformed = (lower - mu) / sigma;
        real upper_transformed = (upper - mu) / sigma;
        sampleNormalRangeBound_RRR_lp(y_unit_normal, 0, 1, lower_transformed, upper_transformed);
        return mu + sigma * y_unit_normal;
    }
    //
    real[] getReparameterizedNormalRangeBound_ARR_lp(real[] y_unit_normal, real mu, real sigma, real lower, real upper)
    {
        int length = num_elements(y_unit_normal);
        real result[length];
        real lower_transformed = (lower - mu) / sigma;
        real upper_transformed = (upper - mu) / sigma;
        sampleNormalRangeBound_ARR_lp(y_unit_normal, 0, 1, lower_transformed, upper_transformed);
        for (i in 1:length) {
            result[i] = mu + sigma * y_unit_normal[i];
        }
        return result;
    }
    real[] getReparameterizedNormalRangeBound_ARA_lp(real[] y_unit_normal, real mu, real[] sigma, real lower, real upper)
    {
        int length = num_elements(y_unit_normal);
        real result[length];
        real lower_transformed;
        real upper_transformed;
        if (num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = (lower - mu) / sigma[i];
            upper_transformed = (upper - mu) / sigma[i];
            sampleNormalRangeBound_RRR_lp(y_unit_normal[i], 0, 1, lower_transformed, upper_transformed);
            result[i] = mu + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    real[] getReparameterizedNormalRangeBound_ARV_lp(real[] y_unit_normal, real mu, vector sigma, real lower, real upper)
    {
        int length = num_elements(y_unit_normal);
        real result[length];
        real lower_transformed;
        real upper_transformed;
        if (num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = (lower - mu) / sigma[i];
            upper_transformed = (upper - mu) / sigma[i];
            sampleNormalRangeBound_RRR_lp(y_unit_normal[i], 0, 1, lower_transformed, upper_transformed);
            result[i] = mu + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    real[] getReparameterizedNormalRangeBound_AAR_lp(real[] y_unit_normal, real[] mu, real sigma, real lower, real upper)
    {
        int length = num_elements(y_unit_normal);
        real result[length];
        real lower_transformed;
        real upper_transformed;
        if (num_elements(mu) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = (lower - mu[i]) / sigma;
            upper_transformed = (upper - mu[i]) / sigma;
            sampleNormalRangeBound_RRR_lp(y_unit_normal[i], 0, 1, lower_transformed, upper_transformed);
            result[i] = mu[i] + sigma * y_unit_normal[i];
        }
        return result;
    }
    real[] getReparameterizedNormalRangeBound_AAA_lp(real[] y_unit_normal, real[] mu, real[] sigma, real lower, real upper)
    {
        int length = num_elements(y_unit_normal);
        real result[length];
        real lower_transformed;
        real upper_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = (lower - mu[i]) / sigma[i];
            upper_transformed = (upper - mu[i]) / sigma[i];
            sampleNormalRangeBound_RRR_lp(y_unit_normal[i], 0, 1, lower_transformed, upper_transformed);
            result[i] = mu[i] + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    real[] getReparameterizedNormalRangeBound_AAV_lp(real[] y_unit_normal, real[] mu, vector sigma, real lower, real upper)
    {
        int length = num_elements(y_unit_normal);
        real result[length];
        real lower_transformed;
        real upper_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = (lower - mu[i]) / sigma[i];
            upper_transformed = (upper - mu[i]) / sigma[i];
            sampleNormalRangeBound_RRR_lp(y_unit_normal[i], 0, 1, lower_transformed, upper_transformed);
            result[i] = mu[i] + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    real[] getReparameterizedNormalRangeBound_AVR_lp(real[] y_unit_normal, vector mu, real sigma, real lower, real upper)
    {
        int length = num_elements(y_unit_normal);
        real result[length];
        real lower_transformed;
        real upper_transformed;
        if (num_elements(mu) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = (lower - mu[i]) / sigma;
            upper_transformed = (upper - mu[i]) / sigma;
            sampleNormalRangeBound_RRR_lp(y_unit_normal[i], 0, 1, lower_transformed, upper_transformed);
            result[i] = mu[i] + sigma * y_unit_normal[i];
        }
        return result;
    }
    real[] getReparameterizedNormalRangeBound_AVA_lp(real[] y_unit_normal, vector mu, real[] sigma, real lower, real upper)
    {
        int length = num_elements(y_unit_normal);
        real result[length];
        real lower_transformed;
        real upper_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = (lower - mu[i]) / sigma[i];
            upper_transformed = (upper - mu[i]) / sigma[i];
            sampleNormalRangeBound_RRR_lp(y_unit_normal[i], 0, 1, lower_transformed, upper_transformed);
            result[i] = mu[i] + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    real[] getReparameterizedNormalRangeBound_AVV_lp(real[] y_unit_normal, vector mu, vector sigma, real lower, real upper)
    {
        int length = num_elements(y_unit_normal);
        real result[length];
        real lower_transformed;
        real upper_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = (lower - mu[i]) / sigma[i];
            upper_transformed = (upper - mu[i]) / sigma[i];
            sampleNormalRangeBound_RRR_lp(y_unit_normal[i], 0, 1, lower_transformed, upper_transformed);
            result[i] = mu[i] + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    //
    vector getReparameterizedNormalRangeBound_VRR_lp(vector y_unit_normal, real mu, real sigma, real lower, real upper)
    {
        int length = num_elements(y_unit_normal);
        vector[length] result;
        real lower_transformed = (lower - mu) / sigma;
        real upper_transformed = (upper - mu) / sigma;
        sampleNormalRangeBound_VRR_lp(y_unit_normal, 0, 1, lower_transformed, upper_transformed);
        result = mu + sigma * y_unit_normal;
        return result;
    }
    vector getReparameterizedNormalRangeBound_VRA_lp(vector y_unit_normal, real mu, real[] sigma, real lower, real upper)
    {
        int length = num_elements(y_unit_normal);
        vector[length] result;
        real lower_transformed;
        real upper_transformed;
        if (num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = (lower - mu) / sigma[i];
            upper_transformed = (upper - mu) / sigma[i];
            sampleNormalRangeBound_RRR_lp(y_unit_normal[i], 0, 1, lower_transformed, upper_transformed);
            result[i] = mu + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    vector getReparameterizedNormalRangeBound_VRV_lp(vector y_unit_normal, real mu, vector sigma, real lower, real upper)
    {
        int length = num_elements(y_unit_normal);
        vector[length] result;
        real lower_transformed;
        real upper_transformed;
        if (num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = (lower - mu) / sigma[i];
            upper_transformed = (upper - mu) / sigma[i];
            sampleNormalRangeBound_RRR_lp(y_unit_normal[i], 0, 1, lower_transformed, upper_transformed);
            result[i] = mu + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    vector getReparameterizedNormalRangeBound_VAR_lp(vector y_unit_normal, real[] mu, real sigma, real lower, real upper)
    {
        int length = num_elements(y_unit_normal);
        vector[length] result;
        real lower_transformed;
        real upper_transformed;
        if (num_elements(mu) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = (lower - mu[i]) / sigma;
            upper_transformed = (upper - mu[i]) / sigma;
            sampleNormalRangeBound_RRR_lp(y_unit_normal[i], 0, 1, lower_transformed, upper_transformed);
            result[i] = mu[i] + sigma * y_unit_normal[i];
        }
        return result;
    }
    vector getReparameterizedNormalRangeBound_VAA_lp(vector y_unit_normal, real[] mu, real[] sigma, real lower, real upper)
    {
        int length = num_elements(y_unit_normal);
        vector[length] result;
        real lower_transformed;
        real upper_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = (lower - mu[i]) / sigma[i];
            upper_transformed = (upper - mu[i]) / sigma[i];
            sampleNormalRangeBound_RRR_lp(y_unit_normal[i], 0, 1, lower_transformed, upper_transformed);
            result[i] = mu[i] + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    vector getReparameterizedNormalRangeBound_VAV_lp(vector y_unit_normal, real[] mu, vector sigma, real lower, real upper)
    {
        int length = num_elements(y_unit_normal);
        vector[length] result;
        real lower_transformed;
        real upper_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = (lower - mu[i]) / sigma[i];
            upper_transformed = (upper - mu[i]) / sigma[i];
            sampleNormalRangeBound_RRR_lp(y_unit_normal[i], 0, 1, lower_transformed, upper_transformed);
            result[i] = mu[i] + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    vector getReparameterizedNormalRangeBound_VVR_lp(vector y_unit_normal, vector mu, real sigma, real lower, real upper)
    {
        int length = num_elements(y_unit_normal);
        vector[length] result;
        real lower_transformed;
        real upper_transformed;
        if (num_elements(mu) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = (lower - mu[i]) / sigma;
            upper_transformed = (upper - mu[i]) / sigma;
            sampleNormalRangeBound_RRR_lp(y_unit_normal[i], 0, 1, lower_transformed, upper_transformed);
        }
        result = mu + sigma * y_unit_normal;
        return result;
    }
    vector getReparameterizedNormalRangeBound_VVA_lp(vector y_unit_normal, vector mu, real[] sigma, real lower, real upper)
    {
        int length = num_elements(y_unit_normal);
        vector[length] result;
        real lower_transformed;
        real upper_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = (lower - mu[i]) / sigma[i];
            upper_transformed = (upper - mu[i]) / sigma[i];
            sampleNormalRangeBound_RRR_lp(y_unit_normal[i], 0, 1, lower_transformed, upper_transformed);
            result[i] = mu[i] + sigma[i] * y_unit_normal[i];
        }
        return result;
    }
    vector getReparameterizedNormalRangeBound_VVV_lp(vector y_unit_normal, vector mu, vector sigma, real lower, real upper)
    {
        int length = num_elements(y_unit_normal);
        vector[length] result;
        real lower_transformed;
        real upper_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = (lower - mu[i]) / sigma[i];
            upper_transformed = (upper - mu[i]) / sigma[i];
            sampleNormalRangeBound_RRR_lp(y_unit_normal[i], 0, 1, lower_transformed, upper_transformed);
            result[i] = mu[i] + sigma[i] * y_unit_normal[i];
        }
        return result;
    }


    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Cauchy distribution, reparameterized to the uniform distribution
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    /*
    See p339 of the Stan (2017) manual.
    The transformation is

        y_cauchy(mu, sigma) = mu + sigma * y_uniform(-pi/2, pi/2)

    so the boundary transformation is the reverse, namely

        uniform_boundary = arctan((boundary - mu) / sigma)

    Note that
        arctan(-infinity) is -pi/2; arctan(0) is 0; arctan(infinity) is pi/2
        ... so for lower == 0, lower_transformed == 0

    We can do the range constraints like this:
         real<lower=-pi()/2, upper=pi()/2> y_uniform;  // Cauchy
         real<lower=0, upper=pi()/2> y_uniform;  // positive half-Cauchy
    and the sampling statement would be unnecessary, but we're going to
    to the sampling using "target +=" so that bridgesampling works.

    You might think that because of that, the range constraint is unnecessary,
    but it IS STILL NECESSARY or Stan will explore invalid ranges.

    */

    real reparameterizedCauchyBoundary(real boundary, real mu, real sigma)
    {
        return atan((boundary - mu) / sigma);
    }

    real getReparameterizedCauchy_RRR_lp(real y_uniform, real mu, real sigma)
    {
        sampleUniform_RRR_lp(y_uniform, -pi()/2, pi()/2);
        return mu + sigma * tan(y_uniform);
    }
    //
    real[] getReparameterizedCauchy_ARR_lp(real[] y_uniform, real mu, real sigma)
    {
        int length = num_elements(y_uniform);
        real result[length];
        sampleUniform_ARR_lp(y_uniform, -pi()/2, pi()/2);
        for (i in 1:length) {
            result[i] = mu + sigma * tan(y_uniform[i]);
        }
        return result;
    }
    real[] getReparameterizedCauchy_ARA_lp(real[] y_uniform, real mu, real[] sigma)
    {
        int length = num_elements(y_uniform);
        real result[length];
        if (num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        sampleUniform_ARR_lp(y_uniform, -pi()/2, pi()/2);
        for (i in 1:length) {
            result[i] = mu + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    real[] getReparameterizedCauchy_ARV_lp(real[] y_uniform, real mu, vector sigma)
    {
        int length = num_elements(y_uniform);
        real result[length];
        if (num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        sampleUniform_ARR_lp(y_uniform, -pi()/2, pi()/2);
        for (i in 1:length) {
            result[i] = mu + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    real[] getReparameterizedCauchy_AAR_lp(real[] y_uniform, real[] mu, real sigma)
    {
        int length = num_elements(y_uniform);
        real result[length];
        if (num_elements(mu) != length) {
            reject("Incompatible arguments");
        }
        sampleUniform_ARR_lp(y_uniform, -pi()/2, pi()/2);
        for (i in 1:length) {
            result[i] = mu[i] + sigma * tan(y_uniform[i]);
        }
        return result;
    }
    real[] getReparameterizedCauchy_AAA_lp(real[] y_uniform, real[] mu, real[] sigma)
    {
        int length = num_elements(y_uniform);
        real result[length];
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        sampleUniform_ARR_lp(y_uniform, -pi()/2, pi()/2);
        for (i in 1:length) {
            result[i] = mu[i] + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    real[] getReparameterizedCauchy_AAV_lp(real[] y_uniform, real[] mu, vector sigma)
    {
        int length = num_elements(y_uniform);
        real result[length];
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        sampleUniform_ARR_lp(y_uniform, -pi()/2, pi()/2);
        for (i in 1:length) {
            result[i] = mu[i] + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    real[] getReparameterizedCauchy_AVR_lp(real[] y_uniform, vector mu, real sigma)
    {
        int length = num_elements(y_uniform);
        real result[length];
        if (num_elements(mu) != length) {
            reject("Incompatible arguments");
        }
        sampleUniform_ARR_lp(y_uniform, -pi()/2, pi()/2);
        for (i in 1:length) {
            result[i] = mu[i] + sigma * tan(y_uniform[i]);
        }
        return result;
    }
    real[] getReparameterizedCauchy_AVA_lp(real[] y_uniform, vector mu, real[] sigma)
    {
        int length = num_elements(y_uniform);
        real result[length];
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        sampleUniform_ARR_lp(y_uniform, -pi()/2, pi()/2);
        for (i in 1:length) {
            result[i] = mu[i] + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    real[] getReparameterizedCauchy_AVV_lp(real[] y_uniform, vector mu, vector sigma)
    {
        int length = num_elements(y_uniform);
        real result[length];
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        sampleUniform_ARR_lp(y_uniform, -pi()/2, pi()/2);
        for (i in 1:length) {
            result[i] = mu[i] + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    //
    vector getReparameterizedCauchy_VRR_lp(vector y_uniform, real mu, real sigma)
    {
        int length = num_elements(y_uniform);
        vector[length] result;
        sampleUniform_VRR_lp(y_uniform, -pi()/2, pi()/2);
        result = mu + sigma * tan(y_uniform);
        return result;
    }
    vector getReparameterizedCauchy_VRA_lp(vector y_uniform, real mu, real[] sigma)
    {
        int length = num_elements(y_uniform);
        vector[length] result;
        if (num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        sampleUniform_VRR_lp(y_uniform, -pi()/2, pi()/2);
        for (i in 1:length) {
            result[i] = mu + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    vector getReparameterizedCauchy_VRV_lp(vector y_uniform, real mu, vector sigma)
    {
        int length = num_elements(y_uniform);
        vector[length] result;
        if (num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        sampleUniform_VRR_lp(y_uniform, -pi()/2, pi()/2);
        for (i in 1:length) {
            result[i] = mu + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    vector getReparameterizedCauchy_VAR_lp(vector y_uniform, real[] mu, real sigma)
    {
        int length = num_elements(y_uniform);
        vector[length] result;
        if (num_elements(mu) != length) {
            reject("Incompatible arguments");
        }
        sampleUniform_VRR_lp(y_uniform, -pi()/2, pi()/2);
        for (i in 1:length) {
            result[i] = mu[i] + sigma * tan(y_uniform[i]);
        }
        return result;
    }
    vector getReparameterizedCauchy_VAA_lp(vector y_uniform, real[] mu, real[] sigma)
    {
        int length = num_elements(y_uniform);
        vector[length] result;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        sampleUniform_VRR_lp(y_uniform, -pi()/2, pi()/2);
        for (i in 1:length) {
            result[i] = mu[i] + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    vector getReparameterizedCauchy_VAV_lp(vector y_uniform, real[] mu, vector sigma)
    {
        int length = num_elements(y_uniform);
        vector[length] result;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        sampleUniform_VRR_lp(y_uniform, -pi()/2, pi()/2);
        for (i in 1:length) {
            result[i] = mu[i] + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    vector getReparameterizedCauchy_VVR_lp(vector y_uniform, vector mu, real sigma)
    {
        int length = num_elements(y_uniform);
        vector[length] result;
        if (num_elements(mu) != length) {
            reject("Incompatible arguments");
        }
        sampleUniform_VRR_lp(y_uniform, -pi()/2, pi()/2);
        result = mu + sigma * tan(y_uniform);
        return result;
    }
    vector getReparameterizedCauchy_VVA_lp(vector y_uniform, vector mu, real[] sigma)
    {
        int length = num_elements(y_uniform);
        vector[length] result;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        sampleUniform_VRR_lp(y_uniform, -pi()/2, pi()/2);
        for (i in 1:length) {
            result[i] = mu[i] + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    vector getReparameterizedCauchy_VVV_lp(vector y_uniform, vector mu, vector sigma)
    {
        int length = num_elements(y_uniform);
        vector[length] result;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        sampleUniform_VRR_lp(y_uniform, -pi()/2, pi()/2);
        for (i in 1:length) {
            result[i] = mu[i] + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }

    // Lower bound
    // The user specifies the boundary in "target space". So we convert.
    // Note that for array/vector input, the boundary is specific to the mean/
    // SD being sampled, and all our boundaried sampling functions take a
    // "real" boundary, so we use the RRR sampler.

    real getReparameterizedCauchyLowerBound_RRR_lp(real y_uniform, real mu, real sigma, real lower)
    {
        real lower_transformed = atan((lower - mu) / sigma);
        sampleUniform_RRR_lp(y_uniform, lower_transformed, pi()/2);
        if (mu + sigma * tan(y_uniform) < lower) {
            reject("Bug. mu ", mu, "; sigma ", sigma, "; y_uniform ", y_uniform,
                   "; tan(y_uniform) ", tan(y_uniform), "; lower ", lower);
        }
        return mu + sigma * tan(y_uniform);
    }
    //
    real[] getReparameterizedCauchyLowerBound_ARR_lp(real[] y_uniform, real mu, real sigma, real lower)
    {
        int length = num_elements(y_uniform);
        real result[length];
        real lower_transformed = atan((lower - mu) / sigma);
        sampleUniform_ARR_lp(y_uniform, lower_transformed, pi()/2);
        for (i in 1:length) {
            result[i] = mu + sigma * tan(y_uniform[i]);
        }
        return result;
    }
    real[] getReparameterizedCauchyLowerBound_ARA_lp(real[] y_uniform, real mu, real[] sigma, real lower)
    {
        int length = num_elements(y_uniform);
        real result[length];
        real lower_transformed;
        if (num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = atan((lower - mu) / sigma[i]);
            sampleUniform_RRR_lp(y_uniform[i], lower_transformed, pi()/2);
            result[i] = mu + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    real[] getReparameterizedCauchyLowerBound_ARV_lp(real[] y_uniform, real mu, vector sigma, real lower)
    {
        int length = num_elements(y_uniform);
        real result[length];
        real lower_transformed;
        if (num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = atan((lower - mu) / sigma[i]);
            sampleUniform_RRR_lp(y_uniform[i], lower_transformed, pi()/2);
            result[i] = mu + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    real[] getReparameterizedCauchyLowerBound_AAR_lp(real[] y_uniform, real[] mu, real sigma, real lower)
    {
        int length = num_elements(y_uniform);
        real result[length];
        real lower_transformed;
        if (num_elements(mu) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = atan((lower - mu[i]) / sigma);
            sampleUniform_RRR_lp(y_uniform[i], lower_transformed, pi()/2);
            result[i] = mu[i] + sigma * tan(y_uniform[i]);
        }
        return result;
    }
    real[] getReparameterizedCauchyLowerBound_AAA_lp(real[] y_uniform, real[] mu, real[] sigma, real lower)
    {
        int length = num_elements(y_uniform);
        real result[length];
        real lower_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = atan((lower - mu[i]) / sigma[i]);
            sampleUniform_RRR_lp(y_uniform[i], lower_transformed, pi()/2);
            result[i] = mu[i] + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    real[] getReparameterizedCauchyLowerBound_AAV_lp(real[] y_uniform, real[] mu, vector sigma, real lower)
    {
        int length = num_elements(y_uniform);
        real result[length];
        real lower_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = atan((lower - mu[i]) / sigma[i]);
            sampleUniform_RRR_lp(y_uniform[i], lower_transformed, pi()/2);
            result[i] = mu[i] + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    real[] getReparameterizedCauchyLowerBound_AVR_lp(real[] y_uniform, vector mu, real sigma, real lower)
    {
        int length = num_elements(y_uniform);
        real result[length];
        real lower_transformed;
        if (num_elements(mu) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = atan((lower - mu[i]) / sigma);
            sampleUniform_RRR_lp(y_uniform[i], lower_transformed, pi()/2);
            result[i] = mu[i] + sigma * tan(y_uniform[i]);
        }
        return result;
    }
    real[] getReparameterizedCauchyLowerBound_AVA_lp(real[] y_uniform, vector mu, real[] sigma, real lower)
    {
        int length = num_elements(y_uniform);
        real result[length];
        real lower_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = atan((lower - mu[i]) / sigma[i]);
            sampleUniform_RRR_lp(y_uniform[i], lower_transformed, pi()/2);
            result[i] = mu[i] + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    real[] getReparameterizedCauchyLowerBound_AVV_lp(real[] y_uniform, vector mu, vector sigma, real lower)
    {
        int length = num_elements(y_uniform);
        real result[length];
        real lower_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = atan((lower - mu[i]) / sigma[i]);
            sampleUniform_RRR_lp(y_uniform[i], lower_transformed, pi()/2);
            result[i] = mu[i] + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    //
    vector getReparameterizedCauchyLowerBound_VRR_lp(vector y_uniform, real mu, real sigma, real lower)
    {
        int length = num_elements(y_uniform);
        vector[length] result;
        real lower_transformed = atan((lower - mu) / sigma);
        sampleUniform_VRR_lp(y_uniform, lower_transformed, pi()/2);
        result = mu + sigma * tan(y_uniform);
        return result;
    }
    vector getReparameterizedCauchyLowerBound_VRA_lp(vector y_uniform, real mu, real[] sigma, real lower)
    {
        int length = num_elements(y_uniform);
        vector[length] result;
        real lower_transformed;
        if (num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = atan((lower - mu) / sigma[i]);
            sampleUniform_RRR_lp(y_uniform[i], lower_transformed, pi()/2);
            result[i] = mu + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    vector getReparameterizedCauchyLowerBound_VRV_lp(vector y_uniform, real mu, vector sigma, real lower)
    {
        int length = num_elements(y_uniform);
        vector[length] result;
        real lower_transformed;
        if (num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = atan((lower - mu) / sigma[i]);
            sampleUniform_RRR_lp(y_uniform[i], lower_transformed, pi()/2);
            result[i] = mu + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    vector getReparameterizedCauchyLowerBound_VAR_lp(vector y_uniform, real[] mu, real sigma, real lower)
    {
        int length = num_elements(y_uniform);
        vector[length] result;
        real lower_transformed;
        if (num_elements(mu) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = atan((lower - mu[i]) / sigma);
            sampleUniform_RRR_lp(y_uniform[i], lower_transformed, pi()/2);
            result[i] = mu[i] + sigma * tan(y_uniform[i]);
        }
        return result;
    }
    vector getReparameterizedCauchyLowerBound_VAA_lp(vector y_uniform, real[] mu, real[] sigma, real lower)
    {
        int length = num_elements(y_uniform);
        vector[length] result;
        real lower_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = atan((lower - mu[i]) / sigma[i]);
            sampleUniform_RRR_lp(y_uniform[i], lower_transformed, pi()/2);
            result[i] = mu[i] + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    vector getReparameterizedCauchyLowerBound_VAV_lp(vector y_uniform, real[] mu, vector sigma, real lower)
    {
        int length = num_elements(y_uniform);
        vector[length] result;
        real lower_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = atan((lower - mu[i]) / sigma[i]);
            sampleUniform_RRR_lp(y_uniform[i], lower_transformed, pi()/2);
            result[i] = mu[i] + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    vector getReparameterizedCauchyLowerBound_VVR_lp(vector y_uniform, vector mu, real sigma, real lower)
    {
        int length = num_elements(y_uniform);
        vector[length] result;
        real lower_transformed;
        if (num_elements(mu) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = atan((lower - mu[i]) / sigma);
            sampleUniform_RRR_lp(y_uniform[i], lower_transformed, pi()/2);
        }
        result = mu + sigma * tan(y_uniform);
        return result;
    }
    vector getReparameterizedCauchyLowerBound_VVA_lp(vector y_uniform, vector mu, real[] sigma, real lower)
    {
        int length = num_elements(y_uniform);
        vector[length] result;
        real lower_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = atan((lower - mu[i]) / sigma[i]);
            sampleUniform_RRR_lp(y_uniform[i], lower_transformed, pi()/2);
            result[i] = mu[i] + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    vector getReparameterizedCauchyLowerBound_VVV_lp(vector y_uniform, vector mu, vector sigma, real lower)
    {
        int length = num_elements(y_uniform);
        vector[length] result;
        real lower_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = atan((lower - mu[i]) / sigma[i]);
            sampleUniform_RRR_lp(y_uniform[i], lower_transformed, pi()/2);
            result[i] = mu[i] + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }

    // Upper bound

    real getReparameterizedCauchyUpperBound_RRR_lp(real y_uniform, real mu, real sigma, real upper)
    {
        real upper_transformed = atan((upper - mu) / sigma);
        sampleUniform_RRR_lp(y_uniform, -pi()/2, upper_transformed);
        return mu + sigma * tan(y_uniform);
    }
    //
    real[] getReparameterizedCauchyUpperBound_ARR_lp(real[] y_uniform, real mu, real sigma, real upper)
    {
        int length = num_elements(y_uniform);
        real result[length];
        real upper_transformed = atan((upper - mu) / sigma);
        sampleUniform_ARR_lp(y_uniform, -pi()/2, upper_transformed);
        for (i in 1:length) {
            result[i] = mu + sigma * tan(y_uniform[i]);
        }
        return result;
    }
    real[] getReparameterizedCauchyUpperBound_ARA_lp(real[] y_uniform, real mu, real[] sigma, real upper)
    {
        int length = num_elements(y_uniform);
        real result[length];
        real upper_transformed;
        if (num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            upper_transformed = atan((upper - mu) / sigma[i]);
            sampleUniform_RRR_lp(y_uniform[i], -pi()/2, upper_transformed);
            result[i] = mu + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    real[] getReparameterizedCauchyUpperBound_ARV_lp(real[] y_uniform, real mu, vector sigma, real upper)
    {
        int length = num_elements(y_uniform);
        real result[length];
        real upper_transformed;
        if (num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            upper_transformed = atan((upper - mu) / sigma[i]);
            sampleUniform_RRR_lp(y_uniform[i], -pi()/2, upper_transformed);
            result[i] = mu + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    real[] getReparameterizedCauchyUpperBound_AAR_lp(real[] y_uniform, real[] mu, real sigma, real upper)
    {
        int length = num_elements(y_uniform);
        real result[length];
        real upper_transformed;
        if (num_elements(mu) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            upper_transformed = atan((upper - mu[i]) / sigma);
            sampleUniform_RRR_lp(y_uniform[i], -pi()/2, upper_transformed);
            result[i] = mu[i] + sigma * tan(y_uniform[i]);
        }
        return result;
    }
    real[] getReparameterizedCauchyUpperBound_AAA_lp(real[] y_uniform, real[] mu, real[] sigma, real upper)
    {
        int length = num_elements(y_uniform);
        real result[length];
        real upper_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            upper_transformed = atan((upper - mu[i]) / sigma[i]);
            sampleUniform_RRR_lp(y_uniform[i], -pi()/2, upper_transformed);
            result[i] = mu[i] + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    real[] getReparameterizedCauchyUpperBound_AAV_lp(real[] y_uniform, real[] mu, vector sigma, real upper)
    {
        int length = num_elements(y_uniform);
        real result[length];
        real upper_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            upper_transformed = atan((upper - mu[i]) / sigma[i]);
            sampleUniform_RRR_lp(y_uniform[i], -pi()/2, upper_transformed);
            result[i] = mu[i] + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    real[] getReparameterizedCauchyUpperBound_AVR_lp(real[] y_uniform, vector mu, real sigma, real upper)
    {
        int length = num_elements(y_uniform);
        real result[length];
        real upper_transformed;
        if (num_elements(mu) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            upper_transformed = atan((upper - mu[i]) / sigma);
            sampleUniform_RRR_lp(y_uniform[i], -pi()/2, upper_transformed);
            result[i] = mu[i] + sigma * tan(y_uniform[i]);
        }
        return result;
    }
    real[] getReparameterizedCauchyUpperBound_AVA_lp(real[] y_uniform, vector mu, real[] sigma, real upper)
    {
        int length = num_elements(y_uniform);
        real result[length];
        real upper_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            upper_transformed = atan((upper - mu[i]) / sigma[i]);
            sampleUniform_RRR_lp(y_uniform[i], -pi()/2, upper_transformed);
            result[i] = mu[i] + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    real[] getReparameterizedCauchyUpperBound_AVV_lp(real[] y_uniform, vector mu, vector sigma, real upper)
    {
        int length = num_elements(y_uniform);
        real result[length];
        real upper_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            upper_transformed = atan((upper - mu[i]) / sigma[i]);
            sampleUniform_RRR_lp(y_uniform[i], -pi()/2, upper_transformed);
            result[i] = mu[i] + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    //
    vector getReparameterizedCauchyUpperBound_VRR_lp(vector y_uniform, real mu, real sigma, real upper)
    {
        int length = num_elements(y_uniform);
        vector[length] result;
        real upper_transformed = atan((upper - mu) / sigma);
        sampleUniform_VRR_lp(y_uniform, -pi()/2, upper_transformed);
        result = mu + sigma * tan(y_uniform);
        return result;
    }
    vector getReparameterizedCauchyUpperBound_VRA_lp(vector y_uniform, real mu, real[] sigma, real upper)
    {
        int length = num_elements(y_uniform);
        vector[length] result;
        real upper_transformed;
        if (num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            upper_transformed = atan((upper - mu) / sigma[i]);
            sampleUniform_RRR_lp(y_uniform[i], -pi()/2, upper_transformed);
            result[i] = mu + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    vector getReparameterizedCauchyUpperBound_VRV_lp(vector y_uniform, real mu, vector sigma, real upper)
    {
        int length = num_elements(y_uniform);
        vector[length] result;
        real upper_transformed;
        if (num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            upper_transformed = atan((upper - mu) / sigma[i]);
            sampleUniform_RRR_lp(y_uniform[i], -pi()/2, upper_transformed);
            result[i] = mu + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    vector getReparameterizedCauchyUpperBound_VAR_lp(vector y_uniform, real[] mu, real sigma, real upper)
    {
        int length = num_elements(y_uniform);
        vector[length] result;
        real upper_transformed;
        if (num_elements(mu) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            upper_transformed = atan((upper - mu[i]) / sigma);
            sampleUniform_RRR_lp(y_uniform[i], -pi()/2, upper_transformed);
            result[i] = mu[i] + sigma * tan(y_uniform[i]);
        }
        return result;
    }
    vector getReparameterizedCauchyUpperBound_VAA_lp(vector y_uniform, real[] mu, real[] sigma, real upper)
    {
        int length = num_elements(y_uniform);
        vector[length] result;
        real upper_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            upper_transformed = atan((upper - mu[i]) / sigma[i]);
            sampleUniform_RRR_lp(y_uniform[i], -pi()/2, upper_transformed);
            result[i] = mu[i] + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    vector getReparameterizedCauchyUpperBound_VAV_lp(vector y_uniform, real[] mu, vector sigma, real upper)
    {
        int length = num_elements(y_uniform);
        vector[length] result;
        real upper_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            upper_transformed = atan((upper - mu[i]) / sigma[i]);
            sampleUniform_RRR_lp(y_uniform[i], -pi()/2, upper_transformed);
            result[i] = mu[i] + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    vector getReparameterizedCauchyUpperBound_VVR_lp(vector y_uniform, vector mu, real sigma, real upper)
    {
        int length = num_elements(y_uniform);
        vector[length] result;
        real upper_transformed;
        if (num_elements(mu) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            upper_transformed = atan((upper - mu[i]) / sigma);
            sampleUniform_RRR_lp(y_uniform[i], -pi()/2, upper_transformed);
        }
        result = mu + sigma * tan(y_uniform);
        return result;
    }
    vector getReparameterizedCauchyUpperBound_VVA_lp(vector y_uniform, vector mu, real[] sigma, real upper)
    {
        int length = num_elements(y_uniform);
        vector[length] result;
        real upper_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            upper_transformed = atan((upper - mu[i]) / sigma[i]);
            sampleUniform_RRR_lp(y_uniform[i], -pi()/2, upper_transformed);
            result[i] = mu[i] + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    vector getReparameterizedCauchyUpperBound_VVV_lp(vector y_uniform, vector mu, vector sigma, real upper)
    {
        int length = num_elements(y_uniform);
        vector[length] result;
        real upper_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            upper_transformed = atan((upper - mu[i]) / sigma[i]);
            sampleUniform_RRR_lp(y_uniform[i], -pi()/2, upper_transformed);
            result[i] = mu[i] + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }

    // Range bound

    real getReparameterizedCauchyRangeBound_RRR_lp(real y_uniform, real mu, real sigma, real lower, real upper)
    {
        real lower_transformed = atan((lower - mu) / sigma);
        real upper_transformed = atan((upper - mu) / sigma);
        sampleUniform_RRR_lp(y_uniform, lower_transformed, upper_transformed);
        return mu + sigma * tan(y_uniform);
    }
    //
    real[] getReparameterizedCauchyRangeBound_ARR_lp(real[] y_uniform, real mu, real sigma, real lower, real upper)
    {
        int length = num_elements(y_uniform);
        real result[length];
        real lower_transformed = atan((lower - mu) / sigma);
        real upper_transformed = atan((upper - mu) / sigma);
        sampleUniform_ARR_lp(y_uniform, lower_transformed, upper_transformed);
        for (i in 1:length) {
            result[i] = mu + sigma * tan(y_uniform[i]);
        }
        return result;
    }
    real[] getReparameterizedCauchyRangeBound_ARA_lp(real[] y_uniform, real mu, real[] sigma, real lower, real upper)
    {
        int length = num_elements(y_uniform);
        real result[length];
        real lower_transformed;
        real upper_transformed;
        if (num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = atan((lower - mu) / sigma[i]);
            upper_transformed = atan((upper - mu) / sigma[i]);
            sampleUniform_RRR_lp(y_uniform[i], lower_transformed, upper_transformed);
            result[i] = mu + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    real[] getReparameterizedCauchyRangeBound_ARV_lp(real[] y_uniform, real mu, vector sigma, real lower, real upper)
    {
        int length = num_elements(y_uniform);
        real result[length];
        real lower_transformed;
        real upper_transformed;
        if (num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = atan((lower - mu) / sigma[i]);
            upper_transformed = atan((upper - mu) / sigma[i]);
            sampleUniform_RRR_lp(y_uniform[i], lower_transformed, upper_transformed);
            result[i] = mu + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    real[] getReparameterizedCauchyRangeBound_AAR_lp(real[] y_uniform, real[] mu, real sigma, real lower, real upper)
    {
        int length = num_elements(y_uniform);
        real result[length];
        real lower_transformed;
        real upper_transformed;
        if (num_elements(mu) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = atan((lower - mu[i]) / sigma);
            upper_transformed = atan((upper - mu[i]) / sigma);
            sampleUniform_RRR_lp(y_uniform[i], lower_transformed, upper_transformed);
            result[i] = mu[i] + sigma * tan(y_uniform[i]);
        }
        return result;
    }
    real[] getReparameterizedCauchyRangeBound_AAA_lp(real[] y_uniform, real[] mu, real[] sigma, real lower, real upper)
    {
        int length = num_elements(y_uniform);
        real result[length];
        real lower_transformed;
        real upper_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = atan((lower - mu[i]) / sigma[i]);
            upper_transformed = atan((upper - mu[i]) / sigma[i]);
            sampleUniform_RRR_lp(y_uniform[i], lower_transformed, upper_transformed);
            result[i] = mu[i] + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    real[] getReparameterizedCauchyRangeBound_AAV_lp(real[] y_uniform, real[] mu, vector sigma, real lower, real upper)
    {
        int length = num_elements(y_uniform);
        real result[length];
        real lower_transformed;
        real upper_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = atan((lower - mu[i]) / sigma[i]);
            upper_transformed = atan((upper - mu[i]) / sigma[i]);
            sampleUniform_RRR_lp(y_uniform[i], lower_transformed, upper_transformed);
            result[i] = mu[i] + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    real[] getReparameterizedCauchyRangeBound_AVR_lp(real[] y_uniform, vector mu, real sigma, real lower, real upper)
    {
        int length = num_elements(y_uniform);
        real result[length];
        real lower_transformed;
        real upper_transformed;
        if (num_elements(mu) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = atan((lower - mu[i]) / sigma);
            upper_transformed = atan((upper - mu[i]) / sigma);
            sampleUniform_RRR_lp(y_uniform[i], lower_transformed, upper_transformed);
            result[i] = mu[i] + sigma * tan(y_uniform[i]);
        }
        return result;
    }
    real[] getReparameterizedCauchyRangeBound_AVA_lp(real[] y_uniform, vector mu, real[] sigma, real lower, real upper)
    {
        int length = num_elements(y_uniform);
        real result[length];
        real lower_transformed;
        real upper_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = atan((lower - mu[i]) / sigma[i]);
            upper_transformed = atan((upper - mu[i]) / sigma[i]);
            sampleUniform_RRR_lp(y_uniform[i], lower_transformed, upper_transformed);
            result[i] = mu[i] + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    real[] getReparameterizedCauchyRangeBound_AVV_lp(real[] y_uniform, vector mu, vector sigma, real lower, real upper)
    {
        int length = num_elements(y_uniform);
        real result[length];
        real lower_transformed;
        real upper_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = atan((lower - mu[i]) / sigma[i]);
            upper_transformed = atan((upper - mu[i]) / sigma[i]);
            sampleUniform_RRR_lp(y_uniform[i], lower_transformed, upper_transformed);
            result[i] = mu[i] + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    //
    vector getReparameterizedCauchyRangeBound_VRR_lp(vector y_uniform, real mu, real sigma, real lower, real upper)
    {
        int length = num_elements(y_uniform);
        vector[length] result;
        real lower_transformed = atan((lower - mu) / sigma);
        real upper_transformed = atan((upper - mu) / sigma);
        sampleUniform_VRR_lp(y_uniform, lower_transformed, upper_transformed);
        result = mu + sigma * tan(y_uniform);
        return result;
    }
    vector getReparameterizedCauchyRangeBound_VRA_lp(vector y_uniform, real mu, real[] sigma, real lower, real upper)
    {
        int length = num_elements(y_uniform);
        vector[length] result;
        real lower_transformed;
        real upper_transformed;
        if (num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = atan((lower - mu) / sigma[i]);
            upper_transformed = atan((upper - mu) / sigma[i]);
            sampleUniform_RRR_lp(y_uniform[i], lower_transformed, upper_transformed);
            result[i] = mu + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    vector getReparameterizedCauchyRangeBound_VRV_lp(vector y_uniform, real mu, vector sigma, real lower, real upper)
    {
        int length = num_elements(y_uniform);
        vector[length] result;
        real lower_transformed;
        real upper_transformed;
        if (num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = atan((lower - mu) / sigma[i]);
            upper_transformed = atan((upper - mu) / sigma[i]);
            sampleUniform_RRR_lp(y_uniform[i], lower_transformed, upper_transformed);
            result[i] = mu + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    vector getReparameterizedCauchyRangeBound_VAR_lp(vector y_uniform, real[] mu, real sigma, real lower, real upper)
    {
        int length = num_elements(y_uniform);
        vector[length] result;
        real lower_transformed;
        real upper_transformed;
        if (num_elements(mu) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = atan((lower - mu[i]) / sigma);
            upper_transformed = atan((upper - mu[i]) / sigma);
            sampleUniform_RRR_lp(y_uniform[i], lower_transformed, upper_transformed);
            result[i] = mu[i] + sigma * tan(y_uniform[i]);
        }
        return result;
    }
    vector getReparameterizedCauchyRangeBound_VAA_lp(vector y_uniform, real[] mu, real[] sigma, real lower, real upper)
    {
        int length = num_elements(y_uniform);
        vector[length] result;
        real lower_transformed;
        real upper_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = atan((lower - mu[i]) / sigma[i]);
            upper_transformed = atan((upper - mu[i]) / sigma[i]);
            sampleUniform_RRR_lp(y_uniform[i], lower_transformed, upper_transformed);
            result[i] = mu[i] + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    vector getReparameterizedCauchyRangeBound_VAV_lp(vector y_uniform, real[] mu, vector sigma, real lower, real upper)
    {
        int length = num_elements(y_uniform);
        vector[length] result;
        real lower_transformed;
        real upper_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = atan((lower - mu[i]) / sigma[i]);
            upper_transformed = atan((upper - mu[i]) / sigma[i]);
            sampleUniform_RRR_lp(y_uniform[i], lower_transformed, upper_transformed);
            result[i] = mu[i] + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    vector getReparameterizedCauchyRangeBound_VVR_lp(vector y_uniform, vector mu, real sigma, real lower, real upper)
    {
        int length = num_elements(y_uniform);
        vector[length] result;
        real lower_transformed;
        real upper_transformed;
        if (num_elements(mu) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = atan((lower - mu[i]) / sigma);
            upper_transformed = atan((upper - mu[i]) / sigma);
            sampleUniform_RRR_lp(y_uniform[i], lower_transformed, upper_transformed);
        }
        result = mu + sigma * tan(y_uniform);
        return result;
    }
    vector getReparameterizedCauchyRangeBound_VVA_lp(vector y_uniform, vector mu, real[] sigma, real lower, real upper)
    {
        int length = num_elements(y_uniform);
        vector[length] result;
        real lower_transformed;
        real upper_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = atan((lower - mu[i]) / sigma[i]);
            upper_transformed = atan((upper - mu[i]) / sigma[i]);
            sampleUniform_RRR_lp(y_uniform[i], lower_transformed, upper_transformed);
            result[i] = mu[i] + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }
    vector getReparameterizedCauchyRangeBound_VVV_lp(vector y_uniform, vector mu, vector sigma, real lower, real upper)
    {
        int length = num_elements(y_uniform);
        vector[length] result;
        real lower_transformed;
        real upper_transformed;
        if (num_elements(mu) != length || num_elements(sigma) != length) {
            reject("Incompatible arguments");
        }
        for (i in 1:length) {
            lower_transformed = atan((lower - mu[i]) / sigma[i]);
            upper_transformed = atan((upper - mu[i]) / sigma[i]);
            sampleUniform_RRR_lp(y_uniform[i], lower_transformed, upper_transformed);
            result[i] = mu[i] + sigma[i] * tan(y_uniform[i]);
        }
        return result;
    }

    // ------------------------------------------------------------------------
    // ANOVA-type designs: DEPRECATED APPROACH
    // ------------------------------------------------------------------------
    // ... rather than coding intercept + main effects + interactions (etc.),
    // as here, it's probably best to code individual cells. That makes
    // distributions more sensible (and predictable/easily calculable).

    int interactionIndex2Way(int first_index, int first_max,
                             int second_index, int second_max)
    {
        /*
            Because Stan doesn't support sampling into matrix, we need to
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            convert matrix-like concepts to vectors. Specifically, it doesn't
            support either
                matrix[A, B] m;
                m ~ normal(0, 0.5);  // error: "no matches for matrix ~ normal(int, real)"
            or
                real a[A, B];
                a ~ normal(0, 0.5);  // error: "no matches for real[,] ~ normal(int, real)"

            And note that a vectorized sampling statement is strongly preferred
            (for performance reasons) over iterating through a matrix:
                https://groups.google.com/forum/#!topic/stan-users/4gv3fNCqSNk
                    "Do not loop over sampling statements when a vectorized
                    sampling statement is possible"

            So we use a vector of size A*B, and this index lookup function.
            Parameters:
            - first_index is from 1 to first_max
            - second_index is from 1 to second_max
            - We want a consecutive index from 1 to (first_max * second_max)

            In the output, the FIRST will cycle LEAST rapidly, and the
            LAST will cycle MOST rapidly.
        */
        return (
            (first_index - 1) * first_max +     // slow cycling
            second_index                        // fast cycling
        );
    }

    vector setLastForZeroSum(vector parameters)
    {
        /*
            Makes a vector of parameters sum to zero, by setting the last
            element to the negative sum of the others.
            Used for ANOVA-style effects; e.g. if you have a grand mean, you
            might specify the effects of a three-level factor A as A1, A2, A3;
            then A1 + A2 + A3 must be zero, so A1 and A2 are free parameters
            that are drawn from an appropriate distribution, and then A3 is
            fully constrainted to be -(A1 + A2).

            Because we can't modify the input parameters, we make a new copy.

            Returns a vector of the SAME LENGTH as the original.
            (The last element of the incoming vector is ignored.)
        */
        int length = num_elements(parameters);
        vector[length] newparams;
        real total = 0.0;
        for (i in 1:length - 1) {
            real value = parameters[i];
            newparams[i] = value;
            total = total + value;
        }
        newparams[length] = -total;
        return newparams;
    }

    vector appendElementForZeroSum(vector parameters)
    {
        /*
            As for setLastForZeroSum(), but uses all the information in the
            incoming vector, and returns a vector that's one element longer.
        */
        int initial_length = num_elements(parameters);
        int new_length = initial_length + 1;
        vector[new_length] newparams;
        real total = 0.0;
        for (i in 1:initial_length) {
            real value = parameters[i];
            newparams[i] = value;
            total = total + value;
        }
        newparams[new_length] = -total;
        return newparams;
    }
