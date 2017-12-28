
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

    // ------------------------------------------------------------------------
    // ANOVA-type designs
    // ------------------------------------------------------------------------

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
