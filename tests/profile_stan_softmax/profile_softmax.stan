/*
    Test various versions of softmax for speed.
    The content is just garbage.

    To get started with cmdstan: see stan_notes.rst

*/

functions {

    real softmaxNth(vector softmax_inputs, int index)
    {
        /*
            Returns the nth value (at "index") of the softmax of the inputs.
            Assumes an inverse temperature of 1.
        */

        int length = num_elements(softmax_inputs);
        real constant = max(softmax_inputs);
        vector[length] s_exp_products = exp(softmax_inputs - constant);
        return s_exp_products[index] / sum(s_exp_products);
    }

    real logitSoftmaxNth_method1(vector inputs, int index)
    {
        // Returns logit(softmax(inputs))[index].

        // METHOD 1 (fewer calculations involved):
        real log_p = inputs[index] - log_sum_exp(inputs);

        // EITHER WAY:
        real log_1mp = log1m_exp(log_p);  // = log(1 - exp(log_p)) = log(1 - p)
        return log_p - log_1mp;  // logit = log(p) - log(1 - p)
    }

    real logitSoftmaxNth_method1b(vector inputs, int index)
    {
        // Returns logit(softmax(inputs))[index].

        // METHOD 1 (fewer calculations involved):
        real log_p = inputs[index] - log_sum_exp(inputs);

        // Combining statements from the previous method:
        return log_p - log1m_exp(log_p);
    }

    real logitSoftmaxNth_method2(vector inputs, int index)
    {
        // Returns logit(softmax(inputs))[index].

        // METHOD 2:
        real log_p = log_softmax(inputs)[index];

        // EITHER WAY:
        real log_1mp = log1m_exp(log_p);  // = log(1 - exp(log_p)) = log(1 - p)
        return log_p - log_1mp;  // logit = log(p) - log(1 - p)
    }

}

transformed data {
    int N = 1000;
    int OPTION_OF_INTEREST = 1;
    int OTHER_OPTION = 2;
    real EPSILON = 1e-6;

    int<lower=0, upper=1> y[N] = {
        1, 0, 0, 0, 0, 0, 0, 1, 1, 1,
        1, 1, 1, 0, 1, 1, 0, 1, 1, 1,
        0, 1, 0, 1, 1, 1, 1, 1, 0, 0,
        0, 1, 0, 0, 0, 1, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 1, 1, 1,
        1, 0, 1, 0, 1, 1, 0, 1, 0, 0,
        0, 1, 1, 0, 1, 0, 1, 1, 1, 1,
        0, 1, 1, 0, 0, 0, 1, 0, 0, 1,
        0, 1, 1, 1, 1, 0, 0, 0, 1, 0,
        1, 1, 0, 0, 1, 1, 0, 1, 0, 1,

        1, 0, 0, 0, 0, 0, 0, 1, 1, 1,
        1, 1, 1, 0, 1, 1, 0, 1, 1, 1,
        0, 1, 0, 1, 1, 1, 1, 1, 0, 0,
        0, 1, 0, 0, 0, 1, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 1, 1, 1,
        1, 0, 1, 0, 1, 1, 0, 1, 0, 0,
        0, 1, 1, 0, 1, 0, 1, 1, 1, 1,
        0, 1, 1, 0, 0, 0, 1, 0, 0, 1,
        0, 1, 1, 1, 1, 0, 0, 0, 1, 0,
        1, 1, 0, 0, 1, 1, 0, 1, 0, 1,

        1, 0, 0, 0, 0, 0, 0, 1, 1, 1,
        1, 1, 1, 0, 1, 1, 0, 1, 1, 1,
        0, 1, 0, 1, 1, 1, 1, 1, 0, 0,
        0, 1, 0, 0, 0, 1, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 1, 1, 1,
        1, 0, 1, 0, 1, 1, 0, 1, 0, 0,
        0, 1, 1, 0, 1, 0, 1, 1, 1, 1,
        0, 1, 1, 0, 0, 0, 1, 0, 0, 1,
        0, 1, 1, 1, 1, 0, 0, 0, 1, 0,
        1, 1, 0, 0, 1, 1, 0, 1, 0, 1,

        1, 0, 0, 0, 0, 0, 0, 1, 1, 1,
        1, 1, 1, 0, 1, 1, 0, 1, 1, 1,
        0, 1, 0, 1, 1, 1, 1, 1, 0, 0,
        0, 1, 0, 0, 0, 1, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 1, 1, 1,
        1, 0, 1, 0, 1, 1, 0, 1, 0, 0,
        0, 1, 1, 0, 1, 0, 1, 1, 1, 1,
        0, 1, 1, 0, 0, 0, 1, 0, 0, 1,
        0, 1, 1, 1, 1, 0, 0, 0, 1, 0,
        1, 1, 0, 0, 1, 1, 0, 1, 0, 1,

        1, 0, 0, 0, 0, 0, 0, 1, 1, 1,
        1, 1, 1, 0, 1, 1, 0, 1, 1, 1,
        0, 1, 0, 1, 1, 1, 1, 1, 0, 0,
        0, 1, 0, 0, 0, 1, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 1, 1, 1,
        1, 0, 1, 0, 1, 1, 0, 1, 0, 0,
        0, 1, 1, 0, 1, 0, 1, 1, 1, 1,
        0, 1, 1, 0, 0, 0, 1, 0, 0, 1,
        0, 1, 1, 1, 1, 0, 0, 0, 1, 0,
        1, 1, 0, 0, 1, 1, 0, 1, 0, 1,

        1, 0, 0, 0, 0, 0, 0, 1, 1, 1,
        1, 1, 1, 0, 1, 1, 0, 1, 1, 1,
        0, 1, 0, 1, 1, 1, 1, 1, 0, 0,
        0, 1, 0, 0, 0, 1, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 1, 1, 1,
        1, 0, 1, 0, 1, 1, 0, 1, 0, 0,
        0, 1, 1, 0, 1, 0, 1, 1, 1, 1,
        0, 1, 1, 0, 0, 0, 1, 0, 0, 1,
        0, 1, 1, 1, 1, 0, 0, 0, 1, 0,
        1, 1, 0, 0, 1, 1, 0, 1, 0, 1,

        1, 0, 0, 0, 0, 0, 0, 1, 1, 1,
        1, 1, 1, 0, 1, 1, 0, 1, 1, 1,
        0, 1, 0, 1, 1, 1, 1, 1, 0, 0,
        0, 1, 0, 0, 0, 1, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 1, 1, 1,
        1, 0, 1, 0, 1, 1, 0, 1, 0, 0,
        0, 1, 1, 0, 1, 0, 1, 1, 1, 1,
        0, 1, 1, 0, 0, 0, 1, 0, 0, 1,
        0, 1, 1, 1, 1, 0, 0, 0, 1, 0,
        1, 1, 0, 0, 1, 1, 0, 1, 0, 1,

        1, 0, 0, 0, 0, 0, 0, 1, 1, 1,
        1, 1, 1, 0, 1, 1, 0, 1, 1, 1,
        0, 1, 0, 1, 1, 1, 1, 1, 0, 0,
        0, 1, 0, 0, 0, 1, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 1, 1, 1,
        1, 0, 1, 0, 1, 1, 0, 1, 0, 0,
        0, 1, 1, 0, 1, 0, 1, 1, 1, 1,
        0, 1, 1, 0, 0, 0, 1, 0, 0, 1,
        0, 1, 1, 1, 1, 0, 0, 0, 1, 0,
        1, 1, 0, 0, 1, 1, 0, 1, 0, 1,

        1, 0, 0, 0, 0, 0, 0, 1, 1, 1,
        1, 1, 1, 0, 1, 1, 0, 1, 1, 1,
        0, 1, 0, 1, 1, 1, 1, 1, 0, 0,
        0, 1, 0, 0, 0, 1, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 1, 1, 1,
        1, 0, 1, 0, 1, 1, 0, 1, 0, 0,
        0, 1, 1, 0, 1, 0, 1, 1, 1, 1,
        0, 1, 1, 0, 0, 0, 1, 0, 0, 1,
        0, 1, 1, 1, 1, 0, 0, 0, 1, 0,
        1, 1, 0, 0, 1, 1, 0, 1, 0, 1,

        1, 0, 0, 0, 0, 0, 0, 1, 1, 1,
        1, 1, 1, 0, 1, 1, 0, 1, 1, 1,
        0, 1, 0, 1, 1, 1, 1, 1, 0, 0,
        0, 1, 0, 0, 0, 1, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 1, 1, 1,
        1, 0, 1, 0, 1, 1, 0, 1, 0, 0,
        0, 1, 1, 0, 1, 0, 1, 1, 1, 1,
        0, 1, 1, 0, 0, 0, 1, 0, 0, 1,
        0, 1, 1, 1, 1, 0, 0, 0, 1, 0,
        1, 1, 0, 0, 1, 1, 0, 1, 0, 1
    };

    print("- Ignore the obvious bug that we will sample y many times; this ",
          "just allows us to compare speeds.");

}

parameters {
    real p;
}

model {
    // ========================================================================
    // Declarations
    // ========================================================================

    vector[2] options;
    vector[N] calc_p;
    vector[N] calc_logit;
    real calc_p_daft;
    real check_builtin;
    real check_homebrew;

    // ========================================================================
    // Sampling and some basic calculations
    // ========================================================================

    p ~ uniform(0, 1);

    options[OPTION_OF_INTEREST] = p;
    options[OTHER_OPTION] = 1 - p;

    // ========================================================================
    // Diagnostics
    // ========================================================================

    check_builtin = softmax(options)[OPTION_OF_INTEREST];
    check_homebrew = softmaxNth(options, OPTION_OF_INTEREST);
    if (fabs(check_homebrew - check_builtin) > EPSILON) {
        reject(
            "- Bug in softmaxNth(): built-in softmax() gives ", check_builtin,
            " but homebrew version gives ", check_homebrew
        );
    }

    // ========================================================================
    // Compare methods
    // ========================================================================

    profile("builtin_softmax_inefficient_sample") {

        for (i in 1:N) {
            calc_p_daft = softmax(options)[OPTION_OF_INTEREST];
            y[i] ~ bernoulli(calc_p_daft);
        }

    }

    profile("builtin_softmax_vectorized_sample") {

        // Faster than builtin_softmax_inefficient_sample; takes 90.4% of its
        // total_time (it improves forward_time and marginally improves
        // reverse_time).

        for (i in 1:N) {
            calc_p[i] = softmax(options)[OPTION_OF_INTEREST];
        }
        y ~ bernoulli(calc_p);

    }

    profile("homebrew_softmax_vectorized_sample") {

        // Faster than builtin_softmax_vectorized_sample; takes 79% of its
        // total_time (taking 70.4% of its forward_time and 208% of its
        // reverse_time).
        // THE BEST OF THESE.

        for (i in 1:N) {
            calc_p[i] = softmaxNth(options, OPTION_OF_INTEREST);
        }
        y ~ bernoulli(calc_p);

    }

    profile("logit_method_1") {

        // SLOWER than homebrew_softmax_vectorized_sample; takes 133% of its
        // total_time.

        for (i in 1:N) {
            calc_logit[i] = logitSoftmaxNth_method1(options, OPTION_OF_INTEREST);
        }
        y ~ bernoulli_logit(calc_logit);

    }

    profile("logit_method_1b") {

        // Fractionally faster than logit_method_1 at 97.5% of its total_time,
        // but still slower than homebrew_softmax_vectorized_sample at 144% of
        // its total_time.

        for (i in 1:N) {
            calc_logit[i] = logitSoftmaxNth_method1b(options, OPTION_OF_INTEREST);
        }
        y ~ bernoulli_logit(calc_logit);

    }

    profile("logit_method_2") {

        // SLOWER than logit_method_1; takes 111% of its total_time.

        for (i in 1:N) {
            calc_logit[i] = logitSoftmaxNth_method2(options, OPTION_OF_INTEREST);
        }
        y ~ bernoulli_logit(calc_logit);

    }

}


// ============================================================================
// Test the impact of a silly "generated quantities" block
// ============================================================================
// - With this block, run time is 4.384 (warmup), 279.089 (sampling), 283.473
//   (total), plus a 2.2 Gb "output.csv".
// - Without it: 4.314 (warmup), 4.631 (sampling), 8.945 (total), and a 49 kb
//   output file.

/*

generated quantities {
    int differences[N, N];
    for (i in 1:N) {
        for (j in 1:N) {
            differences[i, j] = y[i] - y[j];
        }
    }
}

*/
