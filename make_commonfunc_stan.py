#! /usr/bin/env python3
# rlib/make_commonfunc_stan.py

"""
===============================================================================
    Copyright (C) 2009-2018 Rudolf Cardinal (rudolf@pobox.com).

    This file is part of rlib.

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
===============================================================================

Stan doesn't allow templating of its user-defined functions.
As a result, we end up repeating boilerplate code.
This is probably preferable - a script to make the .stan file.

2018-04-23:
    - updated for 3D arrays
    - bugfix for bridgesampling normalization. The bridgesampling manual
      uses the example

        target += normal_lpdf(y | mu, sigma) - normal_lcdf(upper | mu, sigma);

      but note that Stan allows this sampling notation for vectors and one-
      dimensional arrays. In this situation the left-hand term is the sum
      of log probabilities for many values, whereas the right-hand term is
      a correction for a single value. Need, therefore, to multiply the
      correction by the number of terms being sampled.

      Confirmed by Quentin Gronau, personal communication, 2018-04-23; he
      notes also that this is done on the example on p23 of their more
      R-focused paper on bridgesampling, https://arxiv.org/pdf/1710.08162.pdf

    - Quick R confirmation that the _lpdf functions cope automatically, using
      normal_lpdf() as an example:

        library(rstan)

        CODE <- '
        data {
            int<lower=1> N;
            vector[N] y;
        }
        parameters {
            real mu;
            real<lower=0> sigma;
        }
        model {
            // Priors; we do not care.
            mu ~ normal(0, 1);
            sigma ~ cauchy(0, 5);

            // This bit we do care about:
            {
                int n = 10;
                vector[n] x;
                real a = 0.0;
                real b = 0.0;
                for (i in 1:n) {
                    x[i] = 0.1;  // value unimportant
                    a = a + normal_lpdf(x[i] | 0, 1);
                }
                b = normal_lpdf(x | 0, 1);
                print("Piecewise, a = ", a, "; vector-wise, b = ", b);
                if (a != b) {
                    reject("a != b");
                }
            }

            // Fit; we do not care.
            y ~ normal(mu, sigma);
        }
        '

        N <- 20
        DATA <- list(N=N, y=rnorm(n=N, mean=5, sd=2))
        fit <- rstan::stan(model_code=CODE, data=DATA)

2019-05-17:

- renamed some parameters called "parameters" to "params", as in Stan 2.18.2
  that has become a keyword that you can't use. The error looked like:

  .. code-block:: none

    SYNTAX ERROR, MESSAGE(S) FROM PARSER:

    Unknown variable: enforceLowerBound_R_lp
    Unknown variable: enforceLowerBound_A_lp
    ... lots more like that...
    Unknown variable: sampleUniform_RRR_lp

    Unexpected open block, missing close block "}" before keyword "parameters".
      error in 'model45cd2c23be_Kanen_M2_param_recovery' at line 4194, column 44
      -------------------------------------------------
      4192:             (The last element of the incoming vector is ignored.)
      4193:         */
      4194:         int length = num_elements(parameters);
                                                       ^
      4195:         vector[length] newparams;
      -------------------------------------------------

    Error in stanc(file = file, model_code = model_code, model_name = model_name,  :
      failed to parse Stan model 'Kanen_M2_param_recovery' due to the above error.

2019-05-21:

- Functions related to ``categorical`` and ``categorical_logit`` distributions.

"""

import argparse
from enum import Enum
from typing import List, Tuple


# =============================================================================
# Stan variable types
# =============================================================================

class VarDescriptor(object):
    def __init__(self,
                 abbreviation: str,
                 typedef: str,
                 singleton: bool,
                 dimensions: int,
                 vector: bool,
                 array: bool,
                 name: str = None) -> None:
        self.abbreviation = abbreviation
        self.typedef = typedef
        self.singleton = singleton
        self.dimensions = dimensions
        self.vector = vector
        self.array = array
        self.name = name

    def __str__(self) -> str:
        return self.typedef

    def __repr__(self) -> str:
        return "VarDescriptor<{} {}>".format(self.typedef, self.name)

    def __eq__(self, other: "VarDescriptor") -> bool:
        return self.typedef == other.typedef

    def clone(self) -> "VarDescriptor":
        return VarDescriptor(
            abbreviation=self.abbreviation,
            typedef=self.typedef,
            singleton=self.singleton,
            dimensions=self.dimensions,
            vector=self.vector,
            array=self.array,
            name=self.name
        )

    @property
    def polydim_array(self) -> bool:
        return self.array and self.dimensions > 1


REAL = VarDescriptor(
    abbreviation="R",
    typedef="real",
    singleton=True,
    dimensions=0,
    vector=False,
    array=False
)
ARRAY = VarDescriptor(
    abbreviation="A",
    typedef="real[]",
    singleton=False,
    dimensions=1,
    vector=False,
    array=True
)
ARRAY_2D = VarDescriptor(
    abbreviation="2",
    typedef="real[,]",
    singleton=False,
    dimensions=2,
    vector=False,
    array=True
)
ARRAY_3D = VarDescriptor(
    abbreviation="3",
    typedef="real[,,]",
    singleton=False,
    dimensions=3,
    vector=False,
    array=True
)
VECTOR = VarDescriptor(
    abbreviation="V",
    typedef="vector",
    singleton=False,
    dimensions=1,
    vector=True,
    array=False
)

ALL_TYPES = [REAL, ARRAY, ARRAY_2D, ARRAY_3D, VECTOR]


class SampleMethod(Enum):
    PLAIN = 1
    LOWER = 2
    UPPER = 3
    RANGE = 4


# =============================================================================
# Helper functions
# =============================================================================

def comment(x: str) -> str:
    return """
    // {}
    """.format(x)


def remove_blank_lines(x: str) -> str:
    lines = x.splitlines()
    return "\n".join(line for line in lines if line.strip())


# =============================================================================
# Common stuff
# =============================================================================

HEADER = """
    // DO NOT EDIT THIS FILE DIRECTLY. It is created by make_commonfunc_stan.py.

    // ========================================================================
    // Common functions
    // ========================================================================
    /*
        Reminders:
        - Annoyingly, you can't modify arguments to Stan user-defined
          functions. (No pass-by-reference.)
        - size() doesn't work on a plain "vector". Use num_elements().
        - Array/vector indexing is 1-based.
        - The addition-assignment (+=) operator generally doesn't work (it
          appears to be reserved for the one variable "target += ...").
          Similarly for all others you might expect.
          - Aha! By Stan 2.19, this has changed. Can use "x += 1"
            (p19 of Stan 2.19 Reference Manual).
        - Can't define constants in a functions{} block.
    */
"""

_ = """
    // ------------------------------------------------------------------------
    // Simple functions: softmax
    // ------------------------------------------------------------------------
    
    /*
        What about the logit domain?
        
        For a probability p:
            odds = p / (1 - p)
            x = logit(p) = log(odds) = log(p) - log(1 - p) = -log((1/p) - 1)
        The logistic function is the inverse of the logit function:
            p = logistic(x) = 1 / (1 + exp(-x)) = exp(x) / (exp(x) + 1)
        Now, the softmax of a bunch of values can be used as a probability,
        since softmax outputs sum to 1.

        For three events, of which exactly one must happen, probabilities 
        p + q + r = 1. There is no simple additive or multiplicative 
        relationship for log odds. (For two events, p + q = 1 and x = -y.)
        A simple example in R:
       
            logit <- function(p) { -log((1 / p) - 1) }
            logistic <- function(x) { 1 / (1 + exp(-x)) }
            logit(c(0.01, 0.01, 0.98))  # -4.59512 -4.59512  3.89182

        More generally, re combining multiple value scales:
        
        We've been doing this:
        ----------------------
        Probability method 1:
            inputvec = k1 * values1 + k2 * values2 + ...;  // arbitrary range
            p = softmaxNth(inputvec, index);  // 0-1
            choices ~ bernoulli(p);
            
        Logit method 1:
            inputvec = k1 * values1 + k2 * values2 + ...; // arbitrary range
            p = softmaxNth(inputvec, index);  // 0-1
            x = logit(p);   // arbitrary range
            choices ~ bernoulli_logit(x);
            // logically equivalent to probability method 1
            
        OK as long as input values are positive:
        ----------------------------------------
        Probability method 2:
            inputvec = k1 * values1 + k2 * values2 + ...; // must constrain >0
            proportion_of_total = inputvec[index] / sum(inputvec);  // 0-1 as long as input values > 0
            choices ~ bernoulli(proportion_of_total);
            
        Logit method 2:
            inputvec = k1 * values1 + k2 * values2 + ...; // must constrain >0
            proportion_of_total = inputvec[index] / sum(inputvec);  // 0-1 as long as input values > 0
            x = logit(proportion_of_total);
            choices ~ bernoulli_logit(x);
            // logically equivalent to probability method 2
            
        Arbitrary:
        ----------
        
            inputvec = k1 * values1 + k2 * values2 + ...;  // arbitrary range
            x = someFunction(inputvec, index);  // arbitrary range
            // ... the sum of logistic(x) across all indices should be 1
            choices ~ bernoulli_logit(x);

    */

    real logitSoftmaxNth(vector inputs, int index)
    {
        // Returns a number suitable for use as a logit, by rescaling the 
        // inputs via a softmax function.
        /*
            NOTES:
            - Across all inputs, the sum of the logistic of the output from 
              this function will be 1 (the inputs represent all possible 
              events, and are mutually exclusive).  
            - The output range is arbitrary.
            - Note some things that don't work:
            - If "inputs" is a set of proportions (all values in range 0-1 and
              sum of 1) and there are two of them, then mean=0.5 and sum=1 and
              then
                    proportions = x / sum(x)
                    // ... for positive x, this means that sum(proportions) = 1
                    // ... and if length(x) == 2, then mean(proportions) = 0.5
                    // ... so this function is equivalent, in that special 
                    //     case, to -0.5 + proportions[i]
              but what's below generalizes to arbitrary numbers.
            - We cannot use (x - mean(x)) / sd(x), i.e. standardization, 
              because it is entirely possible that all x values are equal, in
              which case the SD is zero.
            - We cannot use (x - mean(x)) / sum(x), since it's also possible
              that all values sum to zero (e.g. -1, +1).
                - For two positive inputs, these last two methods are 
                  equivalent to:
                        proportions = x / sum(x)
                        // ... proportions sum to 1
                        // ... mean of the two proportions is 0.5
                        return -0.5 + proportions[i];
            - However, we cannot use (x - mean(x)) since then logits can 
              transform to nonsensical probabilities (e.g. two probabilities
              >0.5 in a set of three).
            - So we return to the softmax concept.
            - Note:
                softmax(y) = exp(y) / sum(exp(y))
                log_softmax(y) = log(softmax(y)) = y - log_sum_exp(y)
                
            - R version:
            
                library(matrixStats)  # for logSumExp
                logitSoftmaxNth <- function(x) {
                    lse <- logSumExp(x)
                    log_p <- x - lse
                    log_1mp = log(1 - exp(log_p))
                    logit <- log_p - log_1mp
                    cat("lse:\n"); print(lse)
                    cat("log_p:\n"); print(log_p)
                    cat("p:\n"); print(exp(log_p))
                    cat("log_1mp:\n"); print(log_1mp)
                    cat("logit:\n"); print(logit)
                    return(logit)
                }
                logitSoftmaxNth(x)
        */
        real target_input = inputs[index];        
        real lse = log_sum_exp(inputs);
        real log_p = target_input - lse;  // = log_softmax(inputs)[index]
        real log_1mp = log1m_exp(log_p_target);  // = log(1 - exp(log_p));
        real logit = log_p - log_1mp;
        return logit;
    }
"""

SIMPLE_FUNCTIONS = """
    // ------------------------------------------------------------------------
    // Simple functions: softmax
    // ------------------------------------------------------------------------
    
    real softmaxNth(vector softmax_inputs, int index)
    {
        /*
            Returns the nth value (at "index") of the softmax of the inputs.
            Assumes an inverse temperature of 1.

        NOTES:
            A softmax function takes several inputs and normalizes them so 
            that:
                - the outputs are in the same relative order as the inputs
                - the outputs sum to 1.
            
            For softmax: see my miscstat.R; the important points for
            optimization are (1) that softmax is invariant to the addition/
            subtraction of a constant, and subtracting the mean makes the
            numbers less likely to fall over computationally; (2) we only
            need the final part of the computation for a single number
            (preference for the right), so we don't have to waste time
            vector-calculating the preference for the left as well [that is:
            we don't have to calculate s_exp_products / sum(s_exp_products)].
            
            The constant can be the mean, or the max; Stan uses the max, which
            is probably a little more efficient.

            Since Stan 2.0.0, the alternative is to use softmax(); see
            https://github.com/stan-dev/math/blob/develop/stan/math/prim/mat/fun/softmax.hpp
            The exact syntactic equivalence is:
            
                real result = softmaxNth(inputs, index);
                real result = softmax(inputs)[index];
                
            Stan's version is in stan/math/prim/mat/fun/softmax.hpp; it uses
            Eigen.
        */
        int length = num_elements(softmax_inputs);
        real constant = max(softmax_inputs);
        vector[length] s_exp_products = exp(softmax_inputs - constant);
        return s_exp_products[index] / sum(s_exp_products);
    }

    real softmaxNthInvTemp(vector softmax_inputs, real inverse_temp, int index)
    {
        /*
            Version of softmaxNth allowing you to specify the inverse temp.
            
            The direct Stan equivalent is:
            
                real result = softmaxNthInvTemp(inputs, invtemp, index);
                real result = softmax(inputs * invtemp)[index];
        */
        
        return softmaxNth(softmax_inputs * inverse_temp, index);
    }

    real logitSoftmaxNth(vector inputs, int index)
    {
        // Returns logit(softmax(inputs))[index].

        // METHOD 1 (fewer calculations involved):
        real log_p = inputs[index] - log_sum_exp(inputs);
        
        // METHOD 2:
        // real log_p = log_softmax(inputs)[index];

        // EITHER WAY:
        real log_1mp = log1m_exp(log_p);  // = log(1 - exp(log_p)) = log(1 - p)
        return log_p - log_1mp;  // logit = log(p) - log(1 - p)
    }
    
    // ------------------------------------------------------------------------
    // Simple functions: logistic
    // ------------------------------------------------------------------------

    // For the logit function, use Stan's built-in logit().
    
    real logistic(real x, real x0, real k, real L)
    {
        // Returns x transformed through a logistic function.
        // Notation as per https://en.wikipedia.org/wiki/Logistic_function
        // x0: centre
        // k: steepness
        // L: maximum (usually 1)

        return L / (1 + exp(-k * (x - x0)));
        
        // If you were to transform x so as to be a logit giving the same
        // result via the standard logistic function, 1 / (1 + exp(-x)), for
        // L = 1, you want this logit:
        //      k * (x - x0) 
    }
    
    // For the standard logistic (with x0 = 0, k = 1, L = 1), use Stan's
    // inv_logit(). 

    // ------------------------------------------------------------------------
    // Simple functions: boundaries (min, max)
    // ------------------------------------------------------------------------

    real bound(real x, real min_value, real max_value)
    {
        // Returns x with minimum/maximum boundaries applied.
        // We should simply be able to do this:
        //     return max(min_value, min(x, max_value));
        // ... but Stan doesn't have max(real, real) or min(real, real) 
        // functions!

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
    // Simple functions: data manipulation
    // ------------------------------------------------------------------------

    vector vector_from_real_array_row(real[,] x, int row)
    {
        // Given an array
        //      real x[nrows, ncols];
        // you can slice the array with
        //      real a[ncols] = x[row];
        // but not with
        //      vector[ncols] y = x[row];
        // so this function does that.
        
        int x_dimensions[2] = dims(x);
        int ncols = x_dimensions[2];
        vector[ncols] v;
        for (i in 1:ncols) {
            v[i] = x[row, i];
        }
        return v;
    }
    
    vector vector_from_int_array_row(int[,] x, int row)
    {
        // As above, but for an int array.
        
        int x_dimensions[2] = dims(x);
        int ncols = x_dimensions[2];
        vector[ncols] v;
        for (i in 1:ncols) {
            v[i] = x[row, i];
        }
        return v;
    }
    
    vector except_V_V(vector v, int except)
    {
        // Returns a vector that is the original without the element at index 
        // "except".
        
        int n = num_elements(v);
        vector[n - 1] result;
        int r = 1;  // indexes result
        for (i in 1:n) {
            if (i == except) {
                continue;
            }
            result[r] = v[i];
            r += 1;
        }
        return result;
    }
    
    int except_I_I(int x, int except)
    {
        // The argument is an index to a vector v; the result is the equivalent
        // index to a vector except_V_V(v, except).
        
        if (x < 1) {
            reject("Argument x is a Stan index so must be >= 1");
        }
        if (except == x) {
            reject("Cannot remove 'except' where except == x");
        }
        if (except < 1 || except > x) {
            return x;
        }
        return x - 1;
    }
    

    // ------------------------------------------------------------------------
    // Simple functions: matrix calculations
    // ------------------------------------------------------------------------
    // Note that Stan only provides the following versions of dot_product():
    //      dot_product(vector, vector)
    //      dot_product(row vector, row vector)
    //      dot_product(vector, row vector)
    //      dot_product(row vector, vector)
    //      dot_product(real[], real[])
    
    vector dot_product_MV_V(matrix x, vector y)
    {
        // Dot product between a matrix (2 dimensions) and a vector (1
        // dimension):
        //
        //      (p, q) matrix ⋅ (q, 1) vector = (p, 1) vector
        //
        // For example:
        //
        //              [a, b]   [g]      [ag + bh]
        //      x ⋅ y = [c, d] ⋅ [h]    = [cg + dh]
        //              [e, f]            [eg + fh]
        //
        //              (3, 2) ⋅ (2, 1) = (3, 1)

        int x_dimensions[2] = dims(x);
        int p = x_dimensions[1];
        int q = x_dimensions[2];
        vector[p] z;

        if (q != num_elements(y)) {
            reject("Incompatible arguments");
        }
        for (i in 1:p) {  // rows of x
            real cell = 0.0;
            for (j in 1:q) {  // columns of x
                cell += x[i, j] * y[j];
            }
            z[i] = cell;
        }
        return z;
    }

    vector dot_product_2A_V(real[,] x, real[] y)
    {
        // As dot_product_MV_V, but for array inputs.

        int x_dimensions[2] = dims(x);
        int p = x_dimensions[1];
        int q = x_dimensions[2];
        vector[p] z;

        if (q != num_elements(y)) {
            reject("Incompatible arguments");
        }
        for (i in 1:p) {  // rows of x
            real cell = 0.0;
            for (j in 1:q) {  // columns of x
                cell += x[i, j] * y[j];
            }
            z[i] = cell;
        }
        return z;
    }

    vector dot_product_VM_V(vector x, matrix y)
    {
        // Dot product between a vector (1 dimension) and a matrix (2
        // dimensions):
        //
        //      (1, p) vector ⋅ (p, q) matrix = (1, q) vector
        //
        // For example:
        //
        //                       [a, c, e]
        //      x ⋅ y = [g, h] ⋅ [b, d, f] = [ag + bh, cg + dh, eg + fh]
        //                                 = y' ⋅ x'
        //
        //              (1, 2) ⋅ (2, 3)    = (1, 3) 

        int y_dimensions[2] = dims(y);
        int p = y_dimensions[1];
        int q = y_dimensions[2];
        vector[q] z;

        if (p != num_elements(x)) {
            reject("Incompatible arguments");
        }
        for (j in 1:q) {  // columns of y
            real cell = 0.0;
            for (i in 1:p) {  // rows of y
                cell += x[j] * y[i, j];
            }
            z[j] = cell;
        }
        return z;
    }

    vector dot_product_A2_V(real[] x, real[,] y)
    {
        // As dot_product_VM_V(), but for array inputs.

        int y_dimensions[2] = dims(y);
        int p = y_dimensions[1];
        int q = y_dimensions[2];
        vector[q] z;

        if (p != num_elements(x)) {
            reject("Incompatible arguments");
        }
        for (j in 1:q) {  // columns of y
            real cell = 0.0;
            for (i in 1:p) {  // rows of y
                cell += x[j] * y[i, j];
            }
            z[j] = cell;
        }
        return z;
    }
    
    real dot_product_AA_R(real[] x, real[] y)
    {
        // Dot product of two arrays.

        int n = num_elements(x);
        real z = 0.0;
        if (n != num_elements(y)) {
            reject("Incompatible arguments");
        }
        for (i in 1:n) {
            z += x[i] * y[i];
        }
        return z;
    }
    
    real dot_product_iAV_R(int[] x, vector y)
    {
        int n = num_elements(x);
        real z = 0.0;
        if (n != num_elements(y)) {
            reject("Incompatible arguments");
        }
        for (i in 1:n) {
            z += x[i] * y[i];
        }
        return z;
    }
    
    matrix tensordot_A3_M(real[] x, real[,,] y)
    {
        // Equivalent to Numpy's tensordot(x, y, axes=1), for:
        //
        //      (1, p) ⋅ (p, q, r) = (q, r)
        //
        // For example:
        //
        //      [a, b] ⋅ [ [c, d, e, f]       = [ac + bc', ad + bd', ...]
        //                 [g, h, i, j]         [ag + bg', ag + bg', ...]
        //                 [k, l, m, n],        [ak + bk', ak + bk', ...]
        //
        //                 [c', d', e', f']
        //                 [g', h', i', j']
        //                 [k', l', m', n'] ]
        //         
        //      (1, 2) ⋅ (2, 3, 4)            = (3, 4)
        
        int dimensions[3] = dims(y);
        int p = dimensions[1];
        int q = dimensions[2];
        int r = dimensions[3];
        matrix[q, r] z;

        if (p != num_elements(x)) {
            reject("Incompatible arguments");
        }
        for (j in 1:q) {
            for (k in 1:r) {
                real cell = 0.0;
                for (i in 1:p) {
                    cell += x[i] * y[i, j, k];
                }
                z[j, k] = cell;
            }
        }
        return z;
    }

    real[,] tensordot_A3_2(real[] x, real[,,] y)
    {
        // As for tensordot_A3_M(), but returning an array.
        
        int dimensions[3] = dims(y);
        int p = dimensions[1];
        int q = dimensions[2];
        int r = dimensions[3];
        real z[q, r];

        if (p != num_elements(x)) {
            reject("Incompatible arguments");
        }
        for (j in 1:q) {
            for (k in 1:r) {
                real cell = 0.0;
                for (i in 1:p) {
                    cell += x[i] * y[i, j, k];
                }
                z[j, k] = cell;
            }
        }
        return z;
    }

"""

DUFF_ANOVA_FUNCTIONS = """
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

    vector setLastForZeroSum(vector params)
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
        int length = num_elements(params);
        vector[length] newparams;
        real total = 0.0;
        for (i in 1:length - 1) {
            real value = params[i];
            newparams[i] = value;
            total = total + value;
        }
        newparams[length] = -total;
        return newparams;
    }

    vector appendElementForZeroSum(vector params)
    {
        /*
            As for setLastForZeroSum(), but uses all the information in the
            incoming vector, and returns a vector that's one element longer.
        */
        int initial_length = num_elements(params);
        int new_length = initial_length + 1;
        vector[new_length] newparams;
        real total = 0.0;
        for (i in 1:initial_length) {
            real value = params[i];
            newparams[i] = value;
            total = total + value;
        }
        newparams[new_length] = -total;
        return newparams;
    }
"""  # noqa

LOG_PROB_HEADER = """
    // ------------------------------------------------------------------------
    // LOG PROBABILITY FUNCTIONS FOR BRIDGE SAMPLING
    // ------------------------------------------------------------------------
    /*
    We can have functions that access the log probability accumulator
    if the function name ends in '_lp'; see Stan manual section 23.3.

    RE ARGUMENTS:

    The Stan manual uses notation like
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
         real[,,]  // three-dimensional array (... etc.)
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
    - Let's do it and define "R", "A", "2", "3", "V" for the parameters.
      (Also "M" for matrix.)
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

        2RR
            ...

        3RR
            ...

        VRA
        VRV
        VAR
        VAA
        VAV
        VVR
        VVA
        VVV

    RE SAMPLING TWO-DIMENSIONAL ARRAYS:

    You can't sample an entire matrix or 2D array; you have do to it row-wise.
    - This isn't very clear in the manual, as far as I can see.
    - The definition of e.g. beta_lpdf() is in terms of "reals", which
      probably means a vector or array of real.
    - Section 9.6 ("Multi-logit regression") of the Stan manual v2.16.0
      gives an example where one would use a matrix sampling statement but
      they don't.
    - But it is explicit in the sense that they define what they mean by
      "reals", as above, and that doesn't include 2D arrays.
    - Better to move the boilerplate code here than in user land, though.

    RE TWO-DIMENSIONAL ARRAYS:

        real thing[N_A, N_B];

        // One way to iterate through all elements:
        for (a in 1:N_A) {
            for (b in 1:N_B) {
                do_something(thing[a, b]);
            }
        }

        // NOT another way to iterate through all elements:
        for (i in 1:num_elements(thing)) {
            do_something(thing[i]);  // thing[i] is a real[], not a real
            // ... and thing[num_elements(thing)] will be an index overflow
        }

    So for some functions we want real[,]... let's give this the one-character
    notation "2" (for 2D array).

    Now:
        num_elements() gives the total, in this case N_A * N_B;
            ... but when *accessing* a 2D array, my_array[1] gives the first
                row, not the first element; see Stan 2017 manual p323.
        size() gives the size of first dimension, in this case N_A;
        dims() gives all dimensions, in this case an int[] containing {N_A, N_B}.

    RE ARITHMETIC:

    Note that we cannot do:
            real * real[]
            vector * vector

    */
"""

LOG_PROB_HELPERS = """
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Helper functions for boundary checking
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // See Stan (2017) manual p82.
    // These are internal functions that ASSUME size match.
    // We can't use a leading "_" prefix on function names (Stan syntax error).

    // Lower

    void enforceLowerBound_R_lp(real y, real lower)
    {
        if (y < lower) {
            target += negative_infinity();
        }
    }
    void enforceLowerBound_A_lp(real[] y, real lower)
    {
        int length = num_elements(y);
        for (i in 1:length) {
            if (y[i] < lower) {
                target += negative_infinity();
            }
        }
    }
    void enforceLowerBound_2_lp(real[,] y, real lower)
    {
        int dimensions[2] = dims(y);
        for (i in 1:dimensions[1]) {
            for (j in 1:dimensions[2]) {
                if (y[i, j] < lower) {
                    target += negative_infinity();
                }
            }
        }
    }
    void enforceLowerBound_3_lp(real[,,] y, real lower)
    {
        int dimensions[3] = dims(y);
        for (i in 1:dimensions[1]) {
            for (j in 1:dimensions[2]) {
                for (k in 1:dimensions[3]) {
                    if (y[i, j, k] < lower) {
                        target += negative_infinity();
                    }
                }
            }
        }
    }
    void enforceLowerBound_V_lp(vector y, real lower)
    {
        int length = num_elements(y);
        for (i in 1:length) {
            if (y[i] < lower) {
                target += negative_infinity();
            }
        }
    }

    // Upper

    void enforceUpperBound_R_lp(real y, real upper)
    {
        if (y > upper) {
            target += negative_infinity();
        }
    }
    void enforceUpperBound_A_lp(real[] y, real upper)
    {
        int length = num_elements(y);
        for (i in 1:length) {
            if (y[i] > upper) {
                target += negative_infinity();
            }
        }
    }
    void enforceUpperBound_2_lp(real[,] y, real upper)
    {
        int dimensions[2] = dims(y);
        for (i in 1:dimensions[1]) {
            for (j in 1:dimensions[2]) {
                if (y[i, j] > upper) {
                    target += negative_infinity();
                }
            }
        }
    }
    void enforceUpperBound_3_lp(real[,,] y, real upper)
    {
        int dimensions[3] = dims(y);
        for (i in 1:dimensions[1]) {
            for (j in 1:dimensions[2]) {
                for (k in 1:dimensions[3]) {
                    if (y[i, j, k] > upper) {
                        target += negative_infinity();
                    }
                }
            }
        }
    }
    void enforceUpperBound_V_lp(vector y, real upper)
    {
        int length = num_elements(y);
        for (i in 1:length) {
            if (y[i] > upper) {
                target += negative_infinity();
            }
        }
    }

    // Range

    void enforceRangeBounds_R_lp(real y, real lower, real upper)
    {
        if (y < lower || y > upper) {
            target += negative_infinity();
        }
    }
    void enforceRangeBounds_A_lp(real[] y, real lower, real upper)
    {
        int length = num_elements(y);
        for (i in 1:length) {
            real value = y[i];  // lookup only once
            if (value < lower || value > upper) {
                target += negative_infinity();
            }
        }
    }
    void enforceRangeBounds_2_lp(real[,] y, real lower, real upper)
    {
        int dimensions[2] = dims(y);
        for (i in 1:dimensions[1]) {
            for (j in 1:dimensions[2]) {
                real value = y[i, j];  // lookup only once
                if (value < lower || value > upper) {
                    target += negative_infinity();
                }
            }
        }
    }
    void enforceRangeBounds_3_lp(real[,,] y, real lower, real upper)
    {
        int dimensions[3] = dims(y);
        for (i in 1:dimensions[1]) {
            for (j in 1:dimensions[2]) {
                for (k in 1:dimensions[3]) {
                    real value = y[i, j, k];  // lookup only once
                    if (value < lower || value > upper) {
                        target += negative_infinity();
                    }
                }
            }
        }
    }
    void enforceRangeBounds_V_lp(vector y, real lower, real upper)
    {
        int length = num_elements(y);
        for (i in 1:length) {
            real value = y[i];  // lookup only once
            if (value < lower || value > upper) {
                target += negative_infinity();
            }
        }
    }
"""

REPARAM_HEADER = """
    // ------------------------------------------------------------------------
    // LOG PROBABILITY FUNCTIONS FOR BRIDGE SAMPLING WITH NON-CENTERED
    // REPARAMETERIZATION
    // ------------------------------------------------------------------------
"""


# =============================================================================
# Generic distribution
# =============================================================================

def sample_generic(name_caps: str,
                   name_lower: str,
                   y: VarDescriptor,
                   distribution_params: List[VarDescriptor],
                   method: SampleMethod) -> str:
    """
    Writes functions to sample from arbitrary Stan distributions, with
    - correction of the "target" special log-probability variable for
      bridgesampling;
    - upper/lower boundary checking if required.

    NOT YET (RE)IMPLEMENTED: multiple values for the distribution parameters.
    """
    if any(vd.dimensions > 0 for vd in distribution_params):
        raise NotImplementedError("y={}, distribution_params={}".format(
            y, distribution_params))
    y.name = "y"
    call_params = [y] + distribution_params
    lower = REAL.clone()
    lower.name = "lower"
    upper = REAL.clone()
    upper.name = "upper"
    lpdf_func = "{}_lpdf".format(name_lower)
    lcdf_func = "{}_lcdf".format(name_lower)
    lccdf_func = "{}_lccdf".format(name_lower)
    pdf_call_params = ", ".join(vd.name for vd in distribution_params)
    funcname_extra = ""

    if method == SampleMethod.PLAIN:
        if y.dimensions == 3:
            code = """
        int dimensions[3] = dims(y);
        for (i in 1:dimensions[1]) {{
            for (j in 1:dimensions[2]) {{
                for (k in 1:dimensions[3]) {{
                    target += {lpdf_func}(y[i, j, k] | {pdf_call_params});
                }}
            }}
        }}
            """.format(lpdf_func=lpdf_func,
                       pdf_call_params=pdf_call_params)
        elif y.dimensions == 2:
            code = """
        int nrows = size(y);
        for (i in 1:nrows) {{
            target += {lpdf_func}(y[i] | {pdf_call_params});
            // ... y[i] is a one-dimensional array
        }}
           """.format(lpdf_func=lpdf_func,
                      pdf_call_params=pdf_call_params)
        else:  # vector, 1D array, real
            code = """
        target += {lpdf_func}(y | {pdf_call_params});
            """.format(lpdf_func=lpdf_func,
                       pdf_call_params=pdf_call_params)

    elif method in [SampleMethod.LOWER,
                    SampleMethod.UPPER,
                    SampleMethod.RANGE]:
        # Some sort of bridgesampling correction.

        # Define the correction PER SAMPLED VALUE.
        if method == SampleMethod.LOWER:
            code = """
        real correction_per_value = {lccdf_func}(lower | {pdf_call_params});
            """.format(lccdf_func=lccdf_func,
                       pdf_call_params=pdf_call_params)
        elif method == SampleMethod.UPPER:
            code = """
        real correction_per_value = {lcdf_func}(upper | {pdf_call_params});
            """.format(lcdf_func=lcdf_func,
                       pdf_call_params=pdf_call_params)
        elif method == SampleMethod.RANGE:
            code = """
        real correction_per_value = log_diff_exp(
            {lcdf_func}(upper | {pdf_call_params}),
            {lcdf_func}(lower | {pdf_call_params}));
            """.format(lcdf_func=lcdf_func,
                       pdf_call_params=pdf_call_params)
        else:
            raise AssertionError("bug")
        code = code.rstrip()

        # Sample, and apply the correction to the "target" special log-prob
        # variable.
        if y.dimensions == 3:
            code += """
        int dimensions[3] = dims(y);
        for (i in 1:dimensions[1]) {{
            for (j in 1:dimensions[2]) {{
                for (k in 1:dimensions[3]) {{
                    target += {lpdf_func}(y[i, j, k] | {pdf_call_params}) -
                              correction_per_value;
                }}
            }}
        }}
                """.format(lpdf_func=lpdf_func,
                           pdf_call_params=pdf_call_params)
        elif y.dimensions == 2:
            code += """
        int dimensions[2] = dims(y);
        real correction_per_row = correction_per_value * dimensions[2];
        for (i in 1:dimensions[1]) {{
            target += {lpdf_func}(y[i] | {pdf_call_params}) -
                      correction_per_row;
            // ... y[i] is a one-dimensional array
        }}
                """.format(lpdf_func=lpdf_func,
                           pdf_call_params=pdf_call_params)
        elif y.dimensions == 1:  # vector or 1D array
            code += """
        target += {lpdf_func}(y | {pdf_call_params}) -
                  correction_per_value * num_elements(y);
               """.format(lpdf_func=lpdf_func,
                          pdf_call_params=pdf_call_params)
        elif y.singleton:
            code += """
        target += {lpdf_func}(y | {pdf_call_params}) -
                  correction_per_value;
            """.format(lpdf_func=lpdf_func,
                       pdf_call_params=pdf_call_params)
        else:
            raise AssertionError("bug")
        code = code.rstrip()

        # Apply bounds checking
        if method == SampleMethod.LOWER:
            code += """
        enforceLowerBound_{ya}_lp(y, lower);
            """.format(ya=y.abbreviation)
            funcname_extra = "LowerBound"
            call_params += [lower]

        elif method == SampleMethod.UPPER:
            code += """
        enforceUpperBound_{ya}_lp(y, upper);
            """.format(ya=y.abbreviation)
            funcname_extra = "UpperBound"
            call_params += [upper]

        elif method == SampleMethod.RANGE:
            code += """
        enforceRangeBounds_{ya}_lp(y, lower, upper);
            """.format(ya=y.abbreviation)
            funcname_extra = "RangeBound"
            call_params += [lower, upper]
        else:
            raise AssertionError("bug")

    else:
        raise AssertionError("bug")

    funcname = "sample{name_caps}{funcname_extra}_{types}_lp".format(
        name_caps=name_caps,
        funcname_extra=funcname_extra,
        types="".join(vd.abbreviation for vd in [y] + distribution_params)
    )
    param_defs = ", ".join(
        "{} {}".format(vd.typedef, vd.name)
        for vd in call_params
    )
    return """
    void {funcname}({param_defs})
    {{
        {code}
    }}
    """.format(
        funcname=funcname,
        param_defs=param_defs,
        code=code.strip(),
    )


def sample_uniform(y: VarDescriptor, lower: VarDescriptor,
                   upper: VarDescriptor) -> str:
    """
    This one gets its own function because boundaries are an intrinsic part
    of uniform distributions (and so, also, no additional boundary corrections
    are required for bridgesampling).
    """
    distribution_params = [lower, upper]
    if (y.dimensions > 1 and
            any(vd.dimensions > 1 for vd in distribution_params)):
        raise NotImplementedError("y={}, distribution_params={}".format(
            y, distribution_params))
    y.name = "y"
    lower.name = "lower"
    upper.name = "upper"

    if y.dimensions == 3:
        code = """
        int dimensions[3] = dims(y);
        for (i in 1:dimensions[1]) {
            for (j in 1:dimensions[2]) {
                for (k in 1:dimensions[3]) {
                    target += uniform_lpdf(y[i, j, k] | lower, upper);
                }
            }
        }
        """
    elif y.dimensions == 2:
        code = """
        int nrows = size(y);
        for (i in 1:nrows) {
            target += uniform_lpdf(y[i] | lower, upper);
            // ... y[i] is a one-dimensional array
        }
        """
    else:  # vector, 1D array, real
        code = """
        target += uniform_lpdf(y | lower, upper);
        """

    call_params = [y, lower, upper]
    funcname = "sampleUniform_{types}_lp".format(
        types="".join(vd.abbreviation for vd in call_params)
    )
    param_defs = ", ".join(
        "{} {}".format(vd.typedef, vd.name)
        for vd in call_params
    )

    return """
    void {funcname}({param_defs})
    {{
        {code}
    }}
    """.format(
        funcname=funcname,
        param_defs=param_defs,
        code=code.strip(),
    )


# =============================================================================
# Normal distribution
# =============================================================================

def get_normal_distribution() -> str:
    code = """
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Normal distribution
    // - mu is the mean; sigma is the standard deviation
    // - See Stan 2.16.0 manual p512;
    //   http://mathworld.wolfram.com/NormalDistribution.html
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    """

    supported_combinations = []  # type: List[Tuple[VarDescriptor, VarDescriptor, VarDescriptor]]  # noqa
    for y in ALL_TYPES:
        for mu in ALL_TYPES:
            for sigma in ALL_TYPES:
                if mu != REAL or sigma != REAL:
                    continue
                supported_combinations.append((y, mu, sigma))

    def do_call(y_: VarDescriptor,
                mu_: VarDescriptor,
                sigma_: VarDescriptor,
                method: SampleMethod):
        nonlocal code
        # Cloning necessary to prevent name overwriting:
        mu_ = mu_.clone()
        sigma_ = sigma_.clone()
        y_ = y_.clone()
        mu_.name = "mu"
        sigma_.name = "sigma"
        code += sample_generic(
            name_caps="Normal",
            name_lower="normal",
            y=y_,
            distribution_params=[mu_, sigma_],
            method=method
        )

    code += comment("Sampling")
    for y, mu, sigma in supported_combinations:
        do_call(y, mu, sigma, SampleMethod.PLAIN)
    code += comment("Sampling with lower bound")
    for y, mu, sigma in supported_combinations:
        do_call(y, mu, sigma, SampleMethod.LOWER)
    code += comment("Sampling with upper bound")
    for y, mu, sigma in supported_combinations:
        do_call(y, mu, sigma, SampleMethod.UPPER)
    code += comment("Sampling with range (lower and upper) bounds")
    for y, mu, sigma in supported_combinations:
        do_call(y, mu, sigma, SampleMethod.RANGE)
    return code


# =============================================================================
# Cauchy distribution
# =============================================================================

def get_cauchy_distribution() -> str:
    code = """
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Cauchy distribution
    // - mu is location parameter; sigma is scale parameter
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    """

    supported_combinations = []  # type: List[Tuple[VarDescriptor, VarDescriptor, VarDescriptor]]  # noqa
    for y in ALL_TYPES:
        for mu in ALL_TYPES:
            for sigma in ALL_TYPES:
                if mu != REAL or sigma != REAL:
                    continue
                supported_combinations.append((y, mu, sigma))

    def do_call(y_: VarDescriptor,
                mu_: VarDescriptor,
                sigma_: VarDescriptor,
                method: SampleMethod):
        nonlocal code
        # Cloning necessary to prevent name overwriting:
        mu_ = mu_.clone()
        sigma_ = sigma_.clone()
        y_ = y_.clone()
        mu_.name = "mu"
        sigma_.name = "sigma"
        code += sample_generic(
            name_caps="Cauchy",
            name_lower="cauchy",
            y=y_,
            distribution_params=[mu_, sigma_],
            method=method
        )

    code += comment("Sampling")
    for y, mu, sigma in supported_combinations:
        do_call(y, mu, sigma, SampleMethod.PLAIN)
    code += comment("Sampling with lower bound")
    for y, mu, sigma in supported_combinations:
        do_call(y, mu, sigma, SampleMethod.LOWER)
    code += comment("Sampling with upper bound")
    for y, mu, sigma in supported_combinations:
        do_call(y, mu, sigma, SampleMethod.UPPER)
    code += comment("Sampling with range (lower and upper) bounds")
    for y, mu, sigma in supported_combinations:
        do_call(y, mu, sigma, SampleMethod.RANGE)
    return code


# =============================================================================
# Beta distribution
# =============================================================================

def get_beta_distribution() -> str:
    code = """
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Beta distribution
    // - in R, alpha is called scale1, and beta is called scale2
    // - Stan 2.16.0 manual p532; R ?dbeta;
    //   https://www.rdocumentation.org/packages/visualize/versions/4.3.0/topics/visualize.beta
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    """

    supported_combinations = []  # type: List[Tuple[VarDescriptor, VarDescriptor, VarDescriptor]]  # noqa
    for y in ALL_TYPES:
        for alpha in ALL_TYPES:
            for beta in ALL_TYPES:
                if alpha != REAL or beta != REAL:
                    continue
                supported_combinations.append((y, alpha, beta))

    def do_call(y_: VarDescriptor,
                alpha_: VarDescriptor,
                beta_: VarDescriptor,
                method: SampleMethod):
        nonlocal code
        # Cloning necessary to prevent name overwriting:
        alpha_ = alpha_.clone()
        beta_ = beta_.clone()
        y_ = y_.clone()
        alpha_.name = "alpha"
        beta_.name = "beta"
        code += sample_generic(
            name_caps="Beta",
            name_lower="beta",
            y=y_,
            distribution_params=[alpha_, beta_],
            method=method
        )

    code += comment("Sampling")
    for y, alpha, beta in supported_combinations:
        do_call(y, alpha, beta, SampleMethod.PLAIN)
    code += comment("Sampling with lower bound")
    for y, alpha, beta in supported_combinations:
        do_call(y, alpha, beta, SampleMethod.LOWER)
    code += comment("Sampling with upper bound")
    for y, alpha, beta in supported_combinations:
        do_call(y, alpha, beta, SampleMethod.UPPER)
    code += comment("Sampling with range (lower and upper) bounds")
    for y, alpha, beta in supported_combinations:
        do_call(y, alpha, beta, SampleMethod.RANGE)
    return code


# =============================================================================
# Gamma distribution
# =============================================================================

def get_gamma_distribution() -> str:
    code = """
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Gamma distribution
    // - Stan's alpha is R's shape; Stan's beta is R's rate.
    //   (R also offers scale = 1/rate.)
    // - https://en.wikipedia.org/wiki/Gamma_distribution
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    """

    supported_combinations = []  # type: List[Tuple[VarDescriptor, VarDescriptor, VarDescriptor]]  # noqa
    for y in ALL_TYPES:
        for alpha in ALL_TYPES:
            for beta in ALL_TYPES:
                if alpha != REAL or beta != REAL:
                    continue
                supported_combinations.append((y, alpha, beta))

    def do_call(y_: VarDescriptor,
                alpha_: VarDescriptor,
                beta_: VarDescriptor,
                method: SampleMethod):
        nonlocal code
        # Cloning necessary to prevent name overwriting:
        alpha_ = alpha_.clone()
        beta_ = beta_.clone()
        y_ = y_.clone()
        alpha_.name = "alpha"
        beta_.name = "beta"
        code += sample_generic(
            name_caps="Gamma",
            name_lower="gamma",
            y=y_,
            distribution_params=[alpha_, beta_],
            method=method
        )

    code += comment("Sampling")
    for y, alpha, beta in supported_combinations:
        do_call(y, alpha, beta, SampleMethod.PLAIN)
    code += comment("Sampling with lower bound")
    for y, alpha, beta in supported_combinations:
        do_call(y, alpha, beta, SampleMethod.LOWER)
    code += comment("Sampling with upper bound")
    for y, alpha, beta in supported_combinations:
        do_call(y, alpha, beta, SampleMethod.UPPER)
    code += comment("Sampling with range (lower and upper) bounds")
    for y, alpha, beta in supported_combinations:
        do_call(y, alpha, beta, SampleMethod.RANGE)
    return code


# =============================================================================
# Uniform distribution
# =============================================================================

def get_uniform_distribution() -> str:
    code = """
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Uniform distribution
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Always constrained with both a lower and an upper bound.
    // Simple; no extra work for the bridge sampler.
    """

    supported_combinations = []  # type: List[Tuple[VarDescriptor, VarDescriptor, VarDescriptor]]  # noqa
    for y in ALL_TYPES:
        for lower in ALL_TYPES:
            for upper in ALL_TYPES:
                if y == REAL and (lower != REAL or upper != REAL):
                    continue
                if (y.array and y.dimensions > 1 and
                        (lower != REAL or upper != REAL)):
                    continue
                if lower.dimensions > 1 or upper.dimensions > 1:
                    continue
                supported_combinations.append((y, lower, upper))

    def do_call(y_: VarDescriptor,
                lower_: VarDescriptor,
                upper_: VarDescriptor):
        nonlocal code
        # Cloning necessary to prevent name overwriting:
        lower_ = lower_.clone()
        upper_ = upper_.clone()
        y_ = y_.clone()
        lower_.name = "lower"
        upper_.name = "upper"
        code += sample_uniform(y=y_, lower=lower_, upper=upper_)

    code += comment("Sampling")
    for y, lower, upper in supported_combinations:
        do_call(y, lower, upper)
    return code


# =============================================================================
# Bernoulli distribution
# =============================================================================
# So specialized that we just write the code manually.

SAMPLE_BERNOULLI = """
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Bernoulli distribution
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // y is in {0, 1} and theta is a probability in the range [0, 1].

    void sampleBernoulli_IR_lp(int y, real theta)
    {
        target += bernoulli_lpmf(y | theta);
    }
    void sampleBernoulli_AR_lp(int[] y, real theta)
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

    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Bernoulli logit distribution
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // y is in {0, 1} and alpha is a logit (log odds) in the range [-inf, +inf].

    void sampleBernoulliLogit_IR_lp(int y, real alpha)
    {
        target += bernoulli_logit_lpmf(y | alpha);
    }
    void sampleBernoulliLogit_AR_lp(int[] y, real alpha)
    {
        target += bernoulli_logit_lpmf(y | alpha);
    }
    void sampleBernoulliLogit_AA_lp(int[] y, real[] alpha)
    {
        target += bernoulli_logit_lpmf(y | alpha);
    }
    void sampleBernoulliLogit_AV_lp(int[] y, vector alpha)
    {
        target += bernoulli_logit_lpmf(y | alpha);
    }

"""


# =============================================================================
# Categorical and categorical-logit distributions
# =============================================================================
# So specialized that we just write the code manually.

SAMPLE_CATEGORICAL = """
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Categorical distribution
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // y is in {1, ..., K} and theta is a K-simplex (of the probabilities of
    // each of the K outcomes). An N-simplex is a vector (etc.) of non-negative
    // numbers that sum to 1.
    // Note that theta must be vector, not "reals".
    // The logit version is such that categorical_logit_lpmf(beta) is the same
    // as categorical_logit(softmax(beta)), i.e. theta = softmax(beta).

    void sampleCategorical_IV_lp(int y, vector theta)
    {
        target += categorical_lpmf(y | theta);
    }
    void sampleCategorical_AV_lp(int[] y, vector theta)
    {
        target += categorical_lpmf(y | theta);
    }

    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Categorical logit distribution
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // y is in {1, ..., K} and beta is a K-vector of (log odds) in the range 
    // [-inf, +inf]; theta = softmax(beta) as above.

    void sampleCategoricalLogit_IV_lp(int y, vector beta)
    {
        target += categorical_logit_lpmf(y | beta);
    }
    void sampleCategoricalLogit_AV_lp(int[] y, vector beta)
    {
        target += categorical_logit_lpmf(y | beta);
    }

"""


# =============================================================================
# Reparameterized normal distribution
# =============================================================================

def make_reparam_normal(y: VarDescriptor,
                        mu: VarDescriptor,
                        sigma: VarDescriptor,
                        method: SampleMethod) -> str:
    """
    The reparameterization is to a standard (unit) normal distribution,
    N(0, 1). See get_reparamaterized_normal().
    """
    if ((y.dimensions > 1  or y.singleton) and
            (not mu.singleton or not sigma.singleton)):
        raise NotImplementedError("y={}, mu={}, sigma={}".format(
            y, mu, sigma))
    y.name = "y_unit_normal"
    mu.name = "mu"
    sigma.name = "sigma"
    call_params = [y, mu, sigma]
    original_call_params = call_params.copy()
    lower = REAL.clone()
    lower.name = "lower"
    upper = REAL.clone()
    upper.name = "upper"

    using_lower = False
    using_upper = False
    funcname_extra = ""
    constraints = ""
    mu_i = "[i]" if not mu.singleton else ""
    sigma_i = "[i]" if not sigma.singleton else ""
    calc_transformed_1 = ""
    calc_transformed_2 = ""
    if method == SampleMethod.PLAIN:
        pass  # as above
    elif method == SampleMethod.LOWER:
        using_lower = True
        using_upper = False
        funcname_extra = "LowerBound"
    elif method == SampleMethod.UPPER:
        using_lower = False
        using_upper = True
        funcname_extra = "UpperBound"
    elif method == SampleMethod.RANGE:
        using_lower = True
        using_upper = True
        funcname_extra = "RangeBound"

    if using_lower:
        call_params += [lower]
        constraints += ", lower_transformed"
        calc_transformed_1 = (
            "lower_transformed = (lower - mu{mu_i}) / sigma{sigma_i};".format(
                mu_i=mu_i, sigma_i=sigma_i)
        )
    if using_upper:
        call_params += [upper]
        constraints += ", upper_transformed"
        calc_transformed_2 = (
            "upper_transformed = (upper - mu{mu_i}) / sigma{sigma_i};".format(
                mu_i=mu_i, sigma_i=sigma_i)
        )

    # Variable declarations
    code = ""
    if y.singleton:
        code += """
        real result;
        """
    elif y.vector:
        code += """
        int length = num_elements(y_unit_normal);
        vector[length] result;
        """
    elif y.dimensions == 1:  # 1D array
        code += """
        int length = num_elements(y_unit_normal);
        real result[length];
        """
    elif y.dimensions == 2:
        code += """
        int dimensions[2] = dims(y_unit_normal);
        real result[dimensions[1], dimensions[2]];
        """
    elif y.dimensions == 3:
        code += """
        int dimensions[3] = dims(y_unit_normal);
        real result[dimensions[1], dimensions[2], dimensions[3]];
        """
    else:
        raise AssertionError("bug")
    if using_lower:
        code += """
        real lower_transformed;
        """
    if using_upper:
        code += """
        real upper_transformed;
        """

    # Size checks
    if not y.singleton:
        sized_dist_params = [x for x in [mu, sigma] if not x.singleton]
        if sized_dist_params:
            code += """
        if ({conditions}) {{
            reject("Incompatible arguments");
        }}
            """.format(conditions=" || ".join(
                "num_elements({}) != length".format(x.name)
                for x in sized_dist_params
            ))

    # Sample, calculate result, etc.
    if y.singleton:
        code += """
        {calc_transformed_1}
        {calc_transformed_2}
        sampleNormal{fe}_{ya}RR_lp(y_unit_normal, 0, 1{constraints});
        result = mu + sigma * y_unit_normal;
        """.format(
            calc_transformed_1=calc_transformed_1,
            calc_transformed_2=calc_transformed_2,
            fe=funcname_extra,
            ya=y.abbreviation,
            constraints=constraints,
        )
    elif y.dimensions == 1:  # vector, 1D array
        code += """
        for (i in 1:length) {{
            {calc_transformed_1}
            {calc_transformed_2}
            sampleNormal{fe}_RRR_lp(y_unit_normal[i], 0, 1{constraints});
            result[i] = mu{mu_i} + sigma{sigma_i} * y_unit_normal[i];
        }}
        """.format(
            calc_transformed_1=calc_transformed_1,
            calc_transformed_2=calc_transformed_2,
            fe=funcname_extra,
            ya=y.abbreviation,
            constraints=constraints,
            mu_i=mu_i,
            sigma_i=sigma_i
        )
    elif y.dimensions == 2:
        code += """
        {calc_transformed_1}
        {calc_transformed_2}
        for (i in 1:dimensions[1]) {{
            for (j in 1:dimensions[2]) {{
                sampleNormal{fe}_RRR_lp(y_unit_normal[i, j], 0, 1{constraints});
                result[i, j] = mu + sigma * y_unit_normal[i, j];
            }}
        }}
        """.format(
            calc_transformed_1=calc_transformed_1,
            calc_transformed_2=calc_transformed_2,
            fe=funcname_extra,
            ya=y.abbreviation,
            constraints=constraints,
        )
    elif y.dimensions == 3:
        code += """
        {calc_transformed_1}
        {calc_transformed_2}
        for (i in 1:dimensions[1]) {{
            for (j in 1:dimensions[2]) {{
                for (k in 1:dimensions[3]) {{
                    sampleNormal{fe}_RRR_lp(y_unit_normal[i, j, k], 0, 1{constraints});
                    result[i, j, k] = mu + sigma * y_unit_normal[i, j, k];
                }}
            }}
        }}
        """.format(
            calc_transformed_1=calc_transformed_1,
            calc_transformed_2=calc_transformed_2,
            fe=funcname_extra,
            ya=y.abbreviation,
            constraints=constraints,
        )
    else:
        raise AssertionError("bug")

    # Return value
    code += """
        return result;
    """

    funcname = "getReparameterizedNormal{funcname_extra}_{types}_lp".format(
        funcname_extra=funcname_extra,
        types="".join(vd.abbreviation for vd in original_call_params)
    )
    param_defs = ", ".join("{} {}".format(vd.typedef, vd.name)
                           for vd in call_params)

    return """
    {rettype} {funcname}({param_defs})
    {{
        {code}
    }}
    """.format(
        rettype=y.typedef,
        funcname=funcname,
        param_defs=param_defs,
        code=remove_blank_lines(code.strip()),
    )


def get_reparamaterized_normal() -> str:
    code = """
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Normal distribution, reparameterized to the unit normal distribution
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Compare Stan (2017) manual p299, but we use a bridgesampling version.

    real reparameterizedNormalBoundary(real boundary, real mu, real sigma)
    {
        // boundary: in real-world N(mu, sigma) space
        // return value: equivalent in the reparameterized N(0, 1) space
        return (boundary - mu) / sigma;
    }
    """

    supported_combinations = []  # type: List[Tuple[VarDescriptor, VarDescriptor, VarDescriptor]]  # noqa
    for y in ALL_TYPES:
        for lower in ALL_TYPES:
            for upper in ALL_TYPES:
                if ((y == REAL or y.polydim_array) and
                        (lower != REAL or upper != REAL)):
                    continue
                if lower.dimensions > 1 or upper.dimensions > 1:
                    continue
                supported_combinations.append((y, lower, upper))

    def do_call(y_: VarDescriptor,
                mu_: VarDescriptor,
                sigma_: VarDescriptor,
                method: SampleMethod):
        nonlocal code
        # Cloning necessary to prevent name overwriting:
        mu_ = mu_.clone()
        sigma_ = sigma_.clone()
        y_ = y_.clone()
        code += make_reparam_normal(y_, mu_, sigma_, method)

    code += comment("Plain")
    for y, mu, sigma in supported_combinations:
        do_call(y, mu, sigma, SampleMethod.PLAIN)
    code += comment("With lower bound")
    for y, mu, sigma in supported_combinations:
        do_call(y, mu, sigma, SampleMethod.LOWER)
    code += comment("With upper bound")
    for y, mu, sigma in supported_combinations:
        do_call(y, mu, sigma, SampleMethod.UPPER)
    code += comment("With range (lower and upper) bounds")
    for y, mu, sigma in supported_combinations:
        do_call(y, mu, sigma, SampleMethod.RANGE)
    return code


# =============================================================================
# Reparameterized Cauchy distribution
# =============================================================================

def make_reparam_cauchy(y: VarDescriptor,
                        mu: VarDescriptor,
                        sigma: VarDescriptor,
                        method: SampleMethod) -> str:
    """
    The reparameterization is to a uniform distribution.
    See get_reparameterized_cauchy() for docs.
    """
    if ((y.dimensions > 1  or y.singleton) and
            (not mu.singleton or not sigma.singleton)):
        raise NotImplementedError("y={}, mu={}, sigma={}".format(
            y, mu, sigma))
    y.name = "y_uniform"
    mu.name = "mu"
    sigma.name = "sigma"
    call_params = [y, mu, sigma]
    original_call_params = call_params.copy()
    lower = REAL.clone()
    lower.name = "lower"
    upper = REAL.clone()
    upper.name = "upper"

    using_lower = False
    using_upper = False
    funcname_extra = ""
    constraints = ""
    mu_i = "[i]" if not mu.singleton else ""
    sigma_i = "[i]" if not sigma.singleton else ""
    calc_transformed_1 = ""
    calc_transformed_2 = ""
    if method == SampleMethod.PLAIN:
        pass  # as above
    elif method == SampleMethod.LOWER:
        using_lower = True
        using_upper = False
        funcname_extra = "LowerBound"
    elif method == SampleMethod.UPPER:
        using_lower = False
        using_upper = True
        funcname_extra = "UpperBound"
    elif method == SampleMethod.RANGE:
        using_lower = True
        using_upper = True
        funcname_extra = "RangeBound"

    if using_lower:
        call_params += [lower]
        constraints += ", lower_transformed"
        calc_transformed_1 = (
            "lower_transformed = atan((lower - mu{mu_i}) / sigma{sigma_i});".format(  # noqa
                mu_i=mu_i, sigma_i=sigma_i)
        )
    if using_upper:
        call_params += [upper]
        constraints += ", upper_transformed"
        calc_transformed_2 = (
            "upper_transformed = atan((upper - mu{mu_i}) / sigma{sigma_i});".format(  # noqa
                mu_i=mu_i, sigma_i=sigma_i)
        )

    # Variable declarations
    code = ""
    if y.singleton:
        code += """
        real result;
        """
    elif y.vector:
        code += """
        int length = num_elements(y_uniform);
        vector[length] result;
        """
    elif y.dimensions == 1:  # 1D array
        code += """
        int length = num_elements(y_uniform);
        real result[length];
        """
    elif y.dimensions == 2:
        code += """
        int dimensions[2] = dims(y_uniform);
        real result[dimensions[1], dimensions[2]];
        """
    elif y.dimensions == 3:
        code += """
        int dimensions[3] = dims(y_uniform);
        real result[dimensions[1], dimensions[2], dimensions[3]];
        """
    else:
        raise AssertionError("bug")
    if using_lower:
        code += """
        real lower_transformed;
        """
    if using_upper:
        code += """
        real upper_transformed;
        """

    # Size checks
    if not y.singleton:
        sized_dist_params = [x for x in [mu, sigma] if not x.singleton]
        if sized_dist_params:
            code += """
        if ({conditions}) {{
            reject("Incompatible arguments");
        }}
            """.format(conditions=" || ".join(
                "num_elements({}) != length".format(x.name)
                for x in sized_dist_params
            ))

    lower_param = "lower_transformed" if using_lower else "-pi()/2"
    upper_param = "upper_transformed" if using_upper else "pi()/2"

    # Sample, calculate result, etc.
    if y.singleton:
        code += """
        {calc_transformed_1}
        {calc_transformed_2}
        sampleUniform_{ya}RR_lp(y_uniform, {lp}, {up});
        result = mu + sigma * tan(y_uniform);
        """.format(
            calc_transformed_1=calc_transformed_1,
            calc_transformed_2=calc_transformed_2,
            ya=y.abbreviation,
            lp=lower_param,
            up=upper_param,
        )
    elif y.dimensions == 1:  # vector, 1D array
        code += """
        for (i in 1:length) {{
            {calc_transformed_1}
            {calc_transformed_2}
            sampleUniform_RRR_lp(y_uniform[i], {lp}, {up});
            result[i] = mu{mu_i} + sigma{sigma_i} * tan(y_uniform[i]);
        }}
        """.format(
            calc_transformed_1=calc_transformed_1,
            calc_transformed_2=calc_transformed_2,
            ya=y.abbreviation,
            lp=lower_param,
            up=upper_param,
            mu_i=mu_i,
            sigma_i=sigma_i
        )
    elif y.dimensions == 2:
        code += """
        {calc_transformed_1}
        {calc_transformed_2}
        for (i in 1:dimensions[1]) {{
            for (j in 1:dimensions[2]) {{
                sampleUniform_RRR_lp(y_uniform[i, j], {lp}, {up});
                result[i, j] = mu + sigma * tan(y_uniform[i, j]);
            }}
        }}
        """.format(
            calc_transformed_1=calc_transformed_1,
            calc_transformed_2=calc_transformed_2,
            ya=y.abbreviation,
            lp=lower_param,
            up=upper_param,
        )
    elif y.dimensions == 3:
        code += """
        {calc_transformed_1}
        {calc_transformed_2}
        for (i in 1:dimensions[1]) {{
            for (j in 1:dimensions[2]) {{
                for (k in 1:dimensions[3]) {{
                    sampleUniform_RRR_lp(y_uniform[i, j, k], {lp}, {up});
                    result[i, j, k] = mu + sigma * tan(y_uniform[i, j, k]);
                }}
            }}
        }}
        """.format(
            calc_transformed_1=calc_transformed_1,
            calc_transformed_2=calc_transformed_2,
            ya=y.abbreviation,
            lp=lower_param,
            up=upper_param,
        )
    else:
        raise AssertionError("bug")

    # Return value
    code += """
        return result;
    """

    funcname = "getReparameterizedCauchy{funcname_extra}_{types}_lp".format(
        funcname_extra=funcname_extra,
        types="".join(vd.abbreviation for vd in original_call_params)
    )
    param_defs = ", ".join("{} {}".format(vd.typedef, vd.name)
                           for vd in call_params)

    return """
    {rettype} {funcname}({param_defs})
    {{
        {code}
    }}
    """.format(
        rettype=y.typedef,
        funcname=funcname,
        param_defs=param_defs,
        code=remove_blank_lines(code.strip()),
    )


def get_reparamaterized_cauchy() -> str:
    code = """
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
        // boundary: in real-world Cauchy(mu, sigma) space
        // return value: equivalent in the reparameterized uniform [-pi/2, +pi/2] space
        return atan((boundary - mu) / sigma);
    }
    """  # noqa

    supported_combinations = []  # type: List[Tuple[VarDescriptor, VarDescriptor, VarDescriptor]]  # noqa
    for y in ALL_TYPES:
        for lower in ALL_TYPES:
            for upper in ALL_TYPES:
                if ((y == REAL or y.polydim_array) and
                        (lower != REAL or upper != REAL)):
                    continue
                if lower.dimensions > 1 or upper.dimensions > 1:
                    continue
                supported_combinations.append((y, lower, upper))

    def do_call(y_: VarDescriptor,
                mu_: VarDescriptor,
                sigma_: VarDescriptor,
                method: SampleMethod):
        nonlocal code
        # Cloning necessary to prevent name overwriting:
        mu_ = mu_.clone()
        sigma_ = sigma_.clone()
        y_ = y_.clone()
        code += make_reparam_cauchy(y_, mu_, sigma_, method)

    code += comment("Plain")
    for y, mu, sigma in supported_combinations:
        do_call(y, mu, sigma, SampleMethod.PLAIN)
    code += comment("With lower bound")
    for y, mu, sigma in supported_combinations:
        do_call(y, mu, sigma, SampleMethod.LOWER)
    code += comment("With upper bound")
    for y, mu, sigma in supported_combinations:
        do_call(y, mu, sigma, SampleMethod.UPPER)
    code += comment("With range (lower and upper) bounds")
    for y, mu, sigma in supported_combinations:
        do_call(y, mu, sigma, SampleMethod.RANGE)
    return code


# =============================================================================
# Main
# =============================================================================

def get_code() -> str:
    return (
        HEADER +
        SIMPLE_FUNCTIONS +
        LOG_PROB_HEADER +
        LOG_PROB_HELPERS +
        get_normal_distribution() +
        get_cauchy_distribution() +
        get_beta_distribution() +
        get_gamma_distribution() +
        get_uniform_distribution() +
        SAMPLE_BERNOULLI +
        SAMPLE_CATEGORICAL +
        REPARAM_HEADER +
        get_reparamaterized_normal() +
        get_reparamaterized_cauchy() +
        DUFF_ANOVA_FUNCTIONS
    )


def main() -> None:
    parser = argparse.ArgumentParser(
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
        description="""
Make a set of common functions for Stan programs.
By Rudolf Cardinal. Created 2018-02-09.
        """)
    parser.add_argument(
        "--filename", type=str, default="commonfunc.stan",
        help="Output filename"
    )
    args = parser.parse_args()

    code = get_code()
    with open(args.filename, "w") as f:
        f.write(code)
    print("Written to {}".format(args.filename))


if __name__ == '__main__':
    main()
