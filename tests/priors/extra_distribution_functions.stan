/*

Additional distribution functions.
Rudolf Cardinal, 2022-12-21.

Original copyright/license below. This code also licensed under GPL v3+.

*/

/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2006 The R Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

functions {

    // START_OF_EXCERPT_FOR_MAKE_COMMONFUNC

    // ========================================================================
    // Probability distribution functions not provided by Stan
    // ========================================================================
    // See extra_distribution_functions.stan, which also implements tests.

    // ------------------------------------------------------------------------
    // qbeta()
    // ------------------------------------------------------------------------

    real qbeta(real p, real alpha, real beta)
    {
        // Quantile, or inverse cumulative distribution function (inverse CDF),
        // for the beta distribution. Equivalent to qbeta() in R, or at least a
        // less capable version of it. Implements the missing Stan function
        // beta_qf().
        //
        // - The first parameter is a cumulative probability.
        // - Distribution parameter shape1 (R) = alpha (Stan).
        // - Distribution parameter shape2 (R) = beta (Stan).
        // - The result is a value from the beta distribution.
        //
        // From
        // https://github.com/SurajGupta/r-source/blob/master/src/nmath/qnbeta.c,
        // modified as per qbeta_notes.txt. We are just implementing the
        // version with lower_tail = false and log_p = false.

        real DBL_EPSILON_X = machine_precision();
        real ONE_M_DBL_EPSILON = 1 - DBL_EPSILON_X;  // precalculate
        real DBL_MIN_ = 1e-323;
        real accu = 1e-15;
        real Eps = 1e-14;  // must be > accu

        real ux, lx, nx, pp;

        if (p < 0.0 || p > 1.0) {
            reject("qbeta: bad parameter: p < 0 or p > 1");
        }
        if (alpha <= 0.0 || beta <= 0.0) {
            reject("qbeta: bad parameter: alpha <= 0 or beta <= 0");
        }

        // p = R_DT_qIv(p);
        // ... reduces to p for log_p = false and lower_tail = false.

        // Invert pnbeta(.):
        // 1. finding an upper and lower bound
        if (p > ONE_M_DBL_EPSILON) {
            return 1.0;
        }

        // pp = fmin2(ONE_M_DBL_EPSILON, p * (1 + Eps));
        pp = fmin(ONE_M_DBL_EPSILON, p * (1 + Eps));

        // Start ux at 0.5 and work it up (in big steps) while it's too low.
        ux = 0.5;
        while (ux < ONE_M_DBL_EPSILON && beta_cdf(ux | alpha, beta) < pp) {
            ux = 0.5 * (1 + ux);
        }
        // ux is now 0.5 or higher

        pp = p * (1 - Eps);

        // Start lx at 0.5 and work it down (in big steps) while it's too high.
        lx = 0.5;
        while (lx > DBL_MIN_ && beta_cdf(lx | alpha, beta) > pp) {
            lx *= 0.5;
        }
        // lx is now 0.5 or lower

        // 2. interval (lx,ux) halving:
        // Narrow down the gap to find the answer.
        while (1) {
            nx = 0.5 * (lx + ux);  // nx is the mean of lx and ux
            if (beta_cdf(nx | alpha, beta) > p) {
                ux = nx;  // nx too high; move down (shift the upper boundary down)
            } else {
                lx = nx;  // nx too low; move up (shift the lower boundary up)
            }
            if ((ux - lx) / nx <= accu) {
                // Sufficiently accurate!
                break;
            }
        }

        return 0.5 * (ux + lx);
    }

    // ------------------------------------------------------------------------
    // qcauchy()
    // ------------------------------------------------------------------------

    real tanpi(real x)
    {
        // Computes tan(pi * x).
        // R's nmath/nmath.h defines __STDC_WANT_IEC_60559_FUNCS_EXT__ to
        // ensure that tanpi() is defined. There is a declaration in
        // https://github.com/SurajGupta/r-source/blob/master/src/include/Rmath.h0.in
        // Stan has a change request to add tanpi() as of Apr 2021 at
        // https://github.com/stan-dev/math/issues/2376
        //
        // Even Boost's Cauchy distribution uses a simple implementation:
        // https://www.boost.org/doc/libs/1_81_0/boost/random/cauchy_distribution.hpp
        return tan(pi() * x);
    }

    real qcauchy(real p, real location, real scale)
    {
        // location (R) = mu (Stan)
        // scale (R) = sigma (Stan)
        //
        // Implements "lower.tail = TRUE, log.p = FALSE" version.
        // See https://github.com/SurajGupta/r-source/blob/master/src/nmath/qcauchy.c

        int lower_tail = 1;  // no "bool" data type
        real p_working = p;  // can't rewrite argument

        if (p < 0.0 || p > 1.0) {
            reject("qcauchy: bad parameter: p < 0 or p > 1");
        }
        if (scale <= 0) {
            if (scale == 0) {
                // [RNC] Point mass.
                return location;
            }
            reject("qcauchy: bad parameter: sigma (scale) < 0");
        }

        if (p > 0.5) {
            if (p == 1.0) {
                return location
                    + (lower_tail ? scale : -scale) * positive_infinity();
                // RNC: curious! Was the my_INF macro.
            }
            p_working = 1 - p;
            lower_tail = !lower_tail;
        }
        // Use p_working, not p, below here.
        // Stan doesn't let us rewrite our argument.

        if (p_working == 0.5) {
            return location;  // avoid 1/Inf below
        }
        if (p_working == 0.0) {
            return location
                + (lower_tail ? scale : -scale) * negative_infinity();
            // p = 1 is handled above
        }
        return location + (lower_tail ? -scale : scale) / tanpi(p_working);
        /*	-1/tan(pi * p) = -cot(pi * p) = tan(pi * (p - 1/2))  */
    }

    // ------------------------------------------------------------------------
    // qgamma(), and its support functions
    // ------------------------------------------------------------------------

    real logcf(real x, real i, real d, real eps)
    {
        // eps: relative tolerance
        // See https://github.com/SurajGupta/r-source/blob/master/src/nmath/pgamma.c
        real c1 = 2 * d;
        real c2 = i + d;
        real c4 = c2 + d;
        real a1 = c2;
        real b1 = i * (c2 - i * x);
        real b2 = d * d * x;
        real a2 = c4 * c2 - b2;
        real scalefactor = pow(2.0, 256.0);
        // = ((4294967296.0^2)^2)^2 = (2^32)^8 = 2^256 = 1.157921e+77
        real c3;

        b2 = c4 * b1 - i * b2;

        while (abs(a2 * b1 - a1 * b2) > abs(eps * b1 * b2)) {
            c3 = c2*c2*x;
            c2 += d;
            c4 += d;
            a1 = c4 * a2 - c3 * a1;
            b1 = c4 * b2 - c3 * b1;

            c3 = c1 * c1 * x;
            c1 += d;
            c4 += d;
            a2 = c4 * a1 - c3 * a2;
            b2 = c4 * b1 - c3 * b2;

            if (abs (b2) > scalefactor) {
                a1 /= scalefactor;
                b1 /= scalefactor;
                a2 /= scalefactor;
                b2 /= scalefactor;
            } else if (abs (b2) < 1 / scalefactor) {
                a1 *= scalefactor;
                b1 *= scalefactor;
                a2 *= scalefactor;
                b2 *= scalefactor;
            }
        }

        return a2 / b2;
    }

    real log1pmx(real x)
    {
        // Accurate calculation of log(1+x)-x, particularly for small x.
        // See https://github.com/SurajGupta/r-source/blob/master/src/nmath/pgamma.c
        real minLog1Value = -0.79149064;

        if (x > 1 || x < minLog1Value) {
            return log1p(x) - x;
        } else {
            real r = x / (2 + x), y = r * r;
            if (abs(x) < 1e-2) {
                real two = 2;
                return r * ((((two / 9 * y + two / 7) * y + two / 5) * y +
                        two / 3) * y - x);
            } else {
                real tol_logcf = 1e-14;
                return r * (2 * y * logcf (y, 3, 2, tol_logcf) - x);
            }
        }
    }

    real lgamma1p(real a)
    {
        // Compute  log(gamma(a+1))  accurately also for small a (0 < a < 0.5).
        // See https://github.com/SurajGupta/r-source/blob/master/src/nmath/pgamma.c
        real eulers_const =	 0.5772156649015328606065120900824024;

        // coeffs[i] holds (zeta(i+2)-1)/(i+2) , i = 0:(N-1), N = 40 :
        int N = 40;
        array[N] real coeffs = {
            0.3224670334241132182362075833230126e-0,  // = (zeta(2)-1)/2
            0.6735230105319809513324605383715000e-1,  // = (zeta(3)-1)/3
            0.2058080842778454787900092413529198e-1,
            0.7385551028673985266273097291406834e-2,
            0.2890510330741523285752988298486755e-2,
            0.1192753911703260977113935692828109e-2,
            0.5096695247430424223356548135815582e-3,
            0.2231547584535793797614188036013401e-3,
            0.9945751278180853371459589003190170e-4,
            0.4492623673813314170020750240635786e-4,
            0.2050721277567069155316650397830591e-4,
            0.9439488275268395903987425104415055e-5,
            0.4374866789907487804181793223952411e-5,
            0.2039215753801366236781900709670839e-5,
            0.9551412130407419832857179772951265e-6,
            0.4492469198764566043294290331193655e-6,
            0.2120718480555466586923135901077628e-6,
            0.1004322482396809960872083050053344e-6,
            0.4769810169363980565760193417246730e-7,
            0.2271109460894316491031998116062124e-7,
            0.1083865921489695409107491757968159e-7,
            0.5183475041970046655121248647057669e-8,
            0.2483674543802478317185008663991718e-8,
            0.1192140140586091207442548202774640e-8,
            0.5731367241678862013330194857961011e-9,
            0.2759522885124233145178149692816341e-9,
            0.1330476437424448948149715720858008e-9,
            0.6422964563838100022082448087644648e-10,
            0.3104424774732227276239215783404066e-10,
            0.1502138408075414217093301048780668e-10,
            0.7275974480239079662504549924814047e-11,
            0.3527742476575915083615072228655483e-11,
            0.1711991790559617908601084114443031e-11,
            0.8315385841420284819798357793954418e-12,
            0.4042200525289440065536008957032895e-12,
            0.1966475631096616490411045679010286e-12,
            0.9573630387838555763782200936508615e-13,
            0.4664076026428374224576492565974577e-13,
            0.2273736960065972320633279596737272e-13,
            0.1109139947083452201658320007192334e-13  // = (zeta(40+1)-1)/(40+1)
        };

        real c = 0.2273736845824652515226821577978691e-12;  // zeta(N+2)-1
        real tol_logcf = 1e-14;
        real lgam;
        int i;

        if (abs(a) >= 0.5) {
            // - R C code: lgammafn(x): computes log|gamma(x)|
            //   https://github.com/SurajGupta/r-source/blob/master/src/nmath/lgamma.c
            // - Stan: lgamma(x): natural log of the gamma function applied to x
            //   https://github.com/stan-dev/math/blob/master/stan/math/prim/fun/lgamma.hpp
            return lgamma(a + 1);
        }

        lgam = c * logcf(-a / 2, N + 2, 1, tol_logcf);
        i = N - 1;
        while (i >= 0) {
            lgam = coeffs[i] - a * lgam;
            i -= 1;
        }

        return (a * lgam - eulers_const) * a - log1pmx(a);
    }

    real qchisq_appr(real p, real nu, real g, real tol)
    {
        // An approximation to R's qchisq(p, nu)?
        // See https://github.com/SurajGupta/r-source/blob/master/src/nmath/qgamma.c
        // nu = 'df'
        // g = log Gamma(nu/2) = lgamma(nu/2)
        real C7 = 4.67;
        real C8 = 6.66;
        real C9 = 6.73;
        real C10 = 13.32;
        real M_LN2_ = 0.693147180559945309417232121458;  // ln(2)

        real alpha, a, c, ch, p1;
        real p2, q, t, x;

        // test arguments and initialise

        if (p < 0.0 || p > 1.0) {
            reject("qchisq_appr: bad parameter: p < 0 or p > 1");
        }
        if (nu <= 0.0) {
            reject("qchisq_appr: bad parameter: nu <= 0");
        }

        alpha = 0.5 * nu;  // = [pq]gamma() shape
        c = alpha - 1;

        // p1 = R_DT_log(p);
        // ... R_DT_log(p) reduces to R_D_log(p) for lower_tail = true
        // ... which reduces to log(p) for log_p = false
        p1 = log(p);
        if (nu < -1.24 * p1) {  // for small chi-squared
            // log(alpha) + g = log(alpha) + log(gamma(alpha)) =
            //        = log(alpha*gamma(alpha)) = lgamma(alpha+1) suffers from
            //  catastrophic cancellation when alpha << 1
            real lgam1pa = (alpha < 0.5) ? lgamma1p(alpha) : (log(alpha) + g);
            ch = exp((lgam1pa + p1) / alpha + M_LN2_);

        } else if (nu > 0.32) {  // using Wilson and Hilferty estimate
            // x = qnorm(p, 0, 1, lower_tail, log_p);
            x = std_normal_qf(p);
            if (is_inf(x)) {
                // RNC alteration; if p = 1 then x = inf, and then we end up
                // with ch = inf in the next steps, and then via the (ch > 2.2
                // * ...) condition, we get ch = -nan instead of inf. So:
                ch = x;
                // This makes it work.
            } else {

                p1 = 2. / (9 * nu);
                ch = nu * pow(x * sqrt(p1) + 1 - p1, 3);

                // approximation for p tending to 1:
                if (ch > 2.2 * nu + 6) {
                    ch = -2 * (log1p(-p) - c * log(0.5 * ch) + g);
                    // ... R_DT_Clog(p) reduces to R_D_LExp(p) for lower_tail = true
                    // ... R_D_LExp(p) reduces to log1p(-p) for log_p = false
                }
            }

        } else {  // "small nu" : 1.24*(-log(p)) <= nu <= 0.32
            ch = 0.4;
            a = log1p(-p) + g + c * M_LN2_;
            // R_DT_Clog(p) -> log1p(-p), as above
            while (1) {
                q = ch;
                p1 = 1. / (1 + ch * (C7 + ch));
                p2 = ch * (C9 + ch * (C8 + ch));
                t = -0.5 + (C7 + 2 * ch) * p1 - (C9 + ch * (C10 + 3 * ch)) / p2;
                ch -= (1 - exp(a + 0.5 * ch) * p2 * p1) / t;
                if (abs(q - ch) <= tol * abs(ch)) {
                    // converged
                    break;
                }
            }
        }

        return ch;
    }

    real qgamma(real p, real alpha, real beta) {
        // Quantile, or inverse cumulative distribution function (inverse CDF),
        // for the gamma distribution. Equivalent to qgamma() in R, or at least
        // a less capable version of it. Implements the missing Stan function
        // gamma_qf().
        //
        // - The first parameter is a cumulative probability.
        // - Distribution parameter shape (R) = alpha (Stan).
        // - Distribution parameter rate (R) = beta (Stan).
        //   (R also offers scale = 1/rate.)
        // - The result is a value from the gamma distribution.
        //
        // From
        // https://github.com/SurajGupta/r-source/blob/master/src/nmath/qgamma.c,
        // modified as per qbeta_notes.txt. We are just implementing the
        // version with lower_tail = false and log_p = false.

        real scale = 1. / beta;
        real EPS1 = 1e-2;
        real EPS2 = 5e-7;  // final precision of AS 91
        real EPS_N = 1e-15;  // precision of Newton step / iterations
        real LN_EPS = -36.043653389117156;  // = log(.Machine$double.eps) iff IEEE_754
        int MAXIT = 1000;  // was 20
        real pMIN = 1e-100;  // was 0.000002 = 2e-6
        real pMAX = (1 - 1e-14);  // was (1-1e-12) and 0.999998 = 1 - 2e-6
        real i420 = 1. / 420., i2520 = 1. / 2520., i5040 = 1. / 5040;
        real p_, a, b, c, g, ch, ch0, p1;
        real p2, q, s1, s2, s3, s4, s5, s6, t, x;
        int max_it_Newton = 1;
        int iterate = 1;  // no boolean type in Stan

        if (p < 0.0 || p > 1.0) {
            reject("qgamma: bad parameter: p < 0 or p > 1");
        }
        if (alpha < 0.0 || scale <= 0.0) {
            reject("qbeta: bad parameter: alpha < 0 or scale (1/rate) <= 0");
        }

        if (alpha == 0.0) {
            // all mass at 0:
            return 0;
        }

        if (alpha < 1e-10) {
            print(
                "qgamma: alpha (", alpha,
                ") is extremely small: results may be unreliable"
            );
            max_it_Newton = 7;  // may still be increased below
        }

        // p_ = R_DT_qIv(p);  // lower_tail prob (in any case)
        p_ = p;

        // g = lgammafn(alpha);  // log Gamma(v/2)
        g = lgamma(alpha);  // log Gamma(v/2)

        // ----- Phase I : Starting Approximation
        ch = qchisq_appr(
            p,
            2 * alpha,  // nu = 'df'
            g,  // = lgamma(nu/2)
            EPS1  // tol
        );
        // if (!R_FINITE(ch)) {
        if (is_inf(ch)) {
            // forget about all iterations!
            max_it_Newton = 0;
            iterate = 0;
        } else if (ch < EPS2) {
            // Corrected according to AS 91; MM, May 25, 1999
            max_it_Newton = 20;
            iterate = 0;  // and do Newton steps
        } else if (p_ > pMAX || p_ < pMIN) {
            max_it_Newton = 20;
            iterate = 0;  // and do Newton steps
        }

        if (iterate) {
            // ----- Phase II: Iteration
            // Call pgamma() [AS 239]	and calculate seven term taylor series
            real M_LN2_ = 0.693147180559945309417232121458;  // ln(2)
            c = alpha - 1;
            s6 = (120 + c * (346 + 127 * c)) * i5040;  // used below, is "const"

            ch0 = ch;  // save initial approx.
            for (i in 1:MAXIT) {
                q = ch;
                p1 = 0.5 * ch;
                p2 = p_ - gamma_cdf(p1 | alpha, 1.0);
                // pgamma_raw(p1, alpha, lower_tail=TRUE, log_p=FALSE)
                // is equivalent to pgamma(p1, alpha, scale=1) in R
                // and thus to gamma_cdf(p1, alpha, 1.0).
                // See https://github.com/SurajGupta/r-source/blob/master/src/nmath/pgamma.c

                if (is_inf(p2) || ch <= 0) {
                    ch = ch0;
                    max_it_Newton = 27;
                    break;
                }  // was  return ML_NAN;

                t = p2 * exp(alpha * M_LN2_ + g + p1 - c * log(ch));
                b = t / ch;
                a = 0.5 * t - b * c;
                s1 = (210 + a * (140 + a * (105 + a * (84 + a * (70 + 60 * a)))))
                    * i420;
                s2 = (420 + a * (735 + a * (966 + a * (1141 + 1278 * a)))) * i2520;
                s3 = (210 + a * (462 + a * (707 + 932 * a))) * i2520;
                s4 = (252 + a * (672 + 1182 * a) + c * (294 + a * (889 + 1740 * a)))
                    * i5040;
                s5 = (84 + 2264 * a + c * (1175 + 606 * a)) * i2520;

                ch += t
                    * (1 + 0.5 * t * s1
                       - b * c
                           * (s1 - b * (s2 - b * (s3 - b * (s4 - b * (s5 - b * s6)))))
                    );
                if (abs(q - ch) < EPS2 * ch) {
                    break;
                }
                if (abs(q - ch) > 0.1 * ch) {
                    // diverging? -- also forces ch > 0
                    if (ch < q) {
                        ch = 0.9 * q;
                    } else {
                        ch = 1.1 * q;
                    }
                }
            }
            // no convergence in MAXIT iterations -- but we add Newton now...
        }

        x = 0.5 * scale * ch;
        if (max_it_Newton) {
            // always use log scale
            real NEG_INF = negative_infinity();
            real DBL_MIN_ = 1e-323;  // as above
            real logp = log(p);  // can't reassign to p
            if (x == 0) {
                real one_p = 1. + 1e-7;
                real one_m = 1. - 1e-7;
                x = DBL_MIN_;
                p_ = gamma_lcdf(x | alpha, beta);
                if (p_ > logp * one_p) {
                    return 0;
                }
                // else:  continue, using x = DBL_MIN instead of 0
            } else {
                p_ = gamma_lcdf(x | alpha, beta);
            }
            if (p_ == NEG_INF) {
                return 0.0;  // PR#14710
            }
            for (i in 1:max_it_Newton) {
                p1 = p_ - logp;
                if (abs(p1) < abs(EPS_N * logp)) {
                    break;
                }
                g = gamma_lpdf(x | alpha, beta);
                if (g == NEG_INF) {
                    break;
                }
                t = p1 * exp(p_ - g);  // = "delta x"
                t = x - t;
                p_ = gamma_cdf(t | alpha, beta);
                if (abs(p_ - logp) > abs(p1)
                        || (i > 1 && abs(p_ - logp) == abs(p1))) {
                    // no improvement
                    break;
                }
                x = t;
            }
        }

        return x;
    }

    // ------------------------------------------------------------------------
    // qwiener(), as in R's RWiener package
    // ------------------------------------------------------------------------
    // Notes:
    // - Stan's ceil() does not return an integer. One might think to_int()
    //   would solve the problem, but it needs an input qualified as "data".
    //   If you mark all function arguments "data real" rather than real,
    //   values calculated from them are not automatically considered "data";
    //   nor can you declare a local variable as "data real". This looks like
    //   nannying on the part of Stan and is unhelpful.
    //   We need a to_int_stupid() function.
    // - pnorm(q, mean = 0, sd = 1, lower_tail = 1, log.p = 0) becomes
    //   std_normal_cdf(q).
    // - qnorm(p, 0, 1, 1, 0) becomes std_normal_qf(q).
    // - sign() requires implementing

    int to_positive_int(real x) {
        // https://discourse.mc-stan.org/t/real-to-integer-conversion/5622/5
        // This assumes that min_val >= 0 is the minimum integer in range,
        // max_val > min_val, and that x has already been rounded. It should
        // find the integer equivalent to x.
        // RNC: By TRUNCATION.
        int min_val = 0;
        int max_val = 2147483646;  // 2 ^ 31 - 2;
        // ... https://mc-stan.org/docs/2_18/reference-manual/numerical-data-types.html
        int range;
        int mid_pt;
        int out;
        int n_for_simple_version = 20;

        if (x < min_val || x > max_val) {
            reject(
                "to_positive_int: ", x,
                " out of range [", min_val, ", ", max_val, "]"
            );
        }

        // The "halving range" method below takes up to about 30-31 interations
        // for our possible range, so is pretty efficient. But for some small
        // number, this is more efficient:
        if (x - min_val < n_for_simple_version) {
            for (q in min_val:min_val + n_for_simple_version) {
                if (q > x) {  // e.g. 6 > 5.1
                    return q - 1;  // e.g. return 5
                }
            }
        }

        range = (max_val - min_val + 1) %/% 2;
        // We add 1 to make sure that truncation doesn't exclude a number.
        // ... and %/% for integer division in Stan, to avoid a warning.
        mid_pt = min_val + range;
        while (range > 0) {
            if (x >= mid_pt && x < mid_pt + 1) {
                out = mid_pt;
                range = 0;
            } else {
                // figure out if range == 1
                range = (range + 1) %/% 2;  // basically, halve the range
                mid_pt = x > mid_pt ? mid_pt + range : mid_pt - range;
            }
        }
        return out;
    }

    int sign(real x) {
        // https://discourse.mc-stan.org/t/signum-function/9156
        // https://github.com/stan-dev/math/issues/1686
        return x < 0 ? -1 : x > 0;
    }

    real prob_upperbound(real v, real a, real w)
    {
        // Used by pwiener. See
        // https://github.com/cran/RWiener/blob/master/src/pwiener.c
        real e = exp(-2.0 * v * a * (1.0 - w));
        if (is_inf(e)) {
            return 1;
        }
        if (v == 0 || w == 1) {
            return 1 - w;
        }
        return ((1 - e) / (exp(2.0 * v * a * w) - e));
    }

    real exp_pnorm(real a, real b)
    {
        // Used by pwiener. See
        // https://github.com/cran/RWiener/blob/master/src/pwiener.c
        // But the original had some daft tests for impossible conditions.
        return exp(a) * std_normal_cdf(b);
    }

    int K_large(real q, real v, real a, real w)
    {
        // Used by pwiener. See
        // https://github.com/cran/RWiener/blob/master/src/pwiener.c
        real err = 1e-10;
        real PI = pi();
        real sqrtL1 = sqrt(1 / q) * a / PI;
        real sqrtL2 = sqrt(fmax(
            1.0,
            -2 / q * a * a / PI / PI * (
                log(err * PI * q / 2 * (v * v + PI * PI / a / a))
                + v * a * w + v * v * q / 2
            )
        ));
        return to_positive_int(ceil(fmax(sqrtL1, sqrtL2)));
    }

    int K_small(real q, real v, real a, real w, real epsilon)
    {
        // Used by pwiener. See
        // https://github.com/cran/RWiener/blob/master/src/pwiener.c
        real S2, S3, S4;
        int K;
        if (v == 0) {
            return to_positive_int(ceil(
                fmax(
                    0.0,
                    w / 2 - sqrt(q) / 2 / a * std_normal_qf(
                        fmax(0.0, fmin(1.0, epsilon / (2 - 2 * w)))
                    )
                )
            ));
        }
        if (v > 0) {
            return K_small(q, -v, a, w, exp(-2 * a * w * v) * epsilon);
        }
        S2 = w - 1 + 0.5 / v / a * log(epsilon / 2 * (1 - exp(2 * v * a)));
        S3 = (0.535 * sqrt(2 * q) + v * q + a * w) / 2 / a;
        S4 = w / 2 - sqrt(q) / 2 / a * std_normal_qf(
            fmax(
                0.0,
                fmin(
                    1.0,
                    epsilon * a / 0.3 / sqrt(2 * pi() * q)
                        * exp(v * v * q / 2 + v * a * w)
                )
            )
        );
        return to_positive_int(ceil(fmax(fmax(fmax(S2, S3), S4), 0.0)));
    }

    real Fl_lower(real q, real v, real a, real w, int K)
    {
        // Used by pwiener. See
        // https://github.com/cran/RWiener/blob/master/src/pwiener.c
        real F = 0;
        real PI = pi();
        int k = K;
        while (k >= 1) {
            F = F - k
                / (v * v * 1.0 + k * k * PI * PI / (a * 1.0) / a)
                * exp(
                    -v * a * w * 1.0
                    - 0.5 * v * v * q
                    - 0.5 * k * k * PI * PI / (a * 1.0) / a * q
                )
                * sin(PI * k * w);
            // RNC: What's with all the "* 1.0"? Pointless, or something fancy
            // about floating-point handling?
            k -= 1;
        }
        return prob_upperbound(v, a, w) + 2.0 * PI / (a * 1.0) / a * F;
    }

    real Fs0_lower(real q, real a, real w, int K)
    {
        // Used by pwiener. See
        // https://github.com/cran/RWiener/blob/master/src/pwiener.c
        real F = 0;
        int k = K;
        while (k >= 0) {
            F = F
                - std_normal_cdf((-2 * k - 2 + w) * a / sqrt(q))
                + std_normal_cdf((-2 * k - w) * a / sqrt(q));
            k -= 1;
        }
        return 2 * F;
    }

    real Fs_lower(real q, real v, real a, real w, int K)
    {
        // Used by pwiener. See
        // https://github.com/cran/RWiener/blob/master/src/pwiener.c
        real S1 = 0, S2 = 0;
        real sqt = sqrt(q);
        int k = K;
        if (v == 0) {
            return Fs0_lower(q, a, w, K);
        }
        while (k >= 1) {
            S1 = S1
                + exp_pnorm(
                    2 * v * a * k,
                    -sign(v) * (2 * a * k + a * w + v * q) / sqt
                )
                - exp_pnorm(
                    -2 * v * a * k - 2 * v * a * w,
                    sign(v) * (2 * a * k + a * w - v * q) / sqt
                );
            S2 = S2
                + exp_pnorm(
                    -2 * v * a * k,
                    sign(v) * (2 * a * k - a * w - v * q) / sqt
                )
                - exp_pnorm(
                    2 * v * a * k - 2 * v * a * w,
                    -sign(v) * (2 * a * k - a * w + v * q) / sqt
                );
            k -= 1;
        }
        return prob_upperbound(v, a, w)
            + sign(v) * (
                (
                    std_normal_cdf(-sign(v) * (a * w + v * q) / sqt)
                    - exp_pnorm(
                        -2 * v * a * w,
                        sign(v) * (a * w - v * q) / sqt
                    )
                )
                + S1 + S2
            );
    }

    real F_lower(real q, real v, real a, real w)
    {
        // Used by pwiener. See
        // https://github.com/cran/RWiener/blob/master/src/pwiener.c
        real err = 1e-10;
        int K_l = K_large(q, v, a, w);
        int K_s = K_small(q, v, a, w, err);
        if (K_l < 10 * K_s) {
            return Fl_lower(q, v, a, w, K_l);
        } else {
            return Fs_lower(q, v, a, w, K_s);
        }
    }

    real pwiener(real q, real alpha, real tau, real beta, real delta)
    {
        // https://github.com/cran/RWiener/blob/master/src/pwiener.c
        if (is_inf(q)) {
            return positive_infinity();
        }
        if (is_nan(q)) {
            return not_a_number();
        }
        if (abs(q) <= tau) {
            return 0;
        }

        if (q < 0) { // lower boundary 0
            return F_lower(abs(q) - tau, delta, alpha, beta);
        } else {  // upper boundary a
            return F_lower(q - tau, (-delta), alpha, (1 - beta));
        }
    }

    real qwiener(real p, real alpha, real tau, real beta, real delta)
    {
        // Based on https://github.com/cran/RWiener/blob/master/src/qwiener.c
        // Its license: GPL v2
        //
        // R's qwiener() handles upper, lower, or both boundaries.
        // Stan's only does the upper boundary, so we can just implement that.
        // https://mc-stan.org/docs/functions-reference/wiener-first-passage-time-distribution.html
        //
        // R's qwiener() calls C function qwiener_c, which is likely qwiener()
        // in qwiener.c. That is boilerplate code around qwiener_d().
        // We'll restrict it to p in [0, 1]; the original dealt with negative
        // probabilities too. And pmin/pmax not used.
        //
        // For pwiener(), Stan does not provide wiener_cdf, so we have to
        // implement that too.

        real pmid = 0;  // value of p for current q guess
        real qmin = 0;  // lower bound of q
        real qmax = positive_infinity();  // upper bound of q
        real q = 1;  // current guess for q
        int c = 0;  // loop counter

        if (p < 0.0 || p > 1.0) {
            reject("qwiener: bad parameter: p < 0 or p > 1");
        }

        while (1) {
            c += 1;
            pmid = pwiener(q, alpha, tau, beta, delta);
            if (p <= pmid) {  // near lower point
                // RNC: pmid correct or too high; move q down
                qmax = q;
                q = qmin + (qmax - qmin) / 2;
            } else {  // near upper point
                // RNC: pmid too low; move q up
                qmin = q;
                if (!is_inf(qmax)) {
                    q = qmin + (qmax - qmin) / 2;
                } else {
                    q = q * 10;
                }
            }
            if (is_nan(pmid)) {
                return not_a_number();
            }
            if (q >= 1e+10) {
                return positive_infinity();
            }
            if (abs(p - pmid) <= 1e-10 || c >= 1000) {
                // accurate enough, or enough iterations
                break;
            }
        }
        return q;
    }

    // ------------------------------------------------------------------------
    // Upper-half quantile functions
    // ------------------------------------------------------------------------
    // Return the quantile of a random variate from a HALF-distribution, given
    // the corresponding cumulative probability. The transformation is p/2 +
    // 0.5, which compresses p values from the range [0, 1] to [0.5, 1].

    real qupperhalfnormal(real p, real mu, real sigma)
    {
        return std_normal_qf(p / 2.0 + 0.5) * sigma + mu;
    }

    real qupperhalfcauchy(real p, real mu, real sigma)
    {
        return qcauchy(p / 2.0 + 0.5, mu, sigma);
    }

    // END_OF_EXCERPT_FOR_MAKE_COMMONFUNC

}

transformed data {
    // ========================================================================
    // Constants
    // ========================================================================

    real INF = positive_infinity();

    // In R: p <- seq(0, 1, 0.05)
    int N_P = 21;  // length(p)
    array[N_P] real<lower=0, upper=1> test_p = {
        // paste(p, collapse = ", ")
        0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45,
        0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1
    };

    int N_BETA_TESTS = 4;
    array[N_BETA_TESTS, 2] real beta_test_params = {
        // shape1 (alpha), shape2 (beta)
        {1, 1},
        {1.2, 1.2},
        {0.7, 2.5},
        {2, 5}
    };
    array[N_BETA_TESTS, N_P] real beta_test_expected_q = {
        // Test values from R
        {
            // paste(qbeta(p, 1, 1), collapse = ", ")
            0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55,
            0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1
        },
        {
            // paste(qbeta(p, 1.2, 1.2), collapse = ", ")
            0, 0.0698775235334272, 0.125178760545994, 0.176409641160966,
            0.22535691476734, 0.27282261026866, 0.319269060696914,
            0.365003823536004, 0.410253539394432, 0.45519988227064, 0.5,
            0.54480011772936, 0.589746460605568, 0.634996176463996,
            0.680730939303086, 0.72717738973134, 0.77464308523266,
            0.823590358839034, 0.874821239454006, 0.930122476466573, 1
        },
        {
            // paste(qbeta(p, 0.7, 2.5), collapse = ", ")
            0, 0.00514300150792603, 0.0139522739704543, 0.0251489566861088,
            0.0383816465302874, 0.0535112633576878, 0.070500907953247,
            0.0893808014046576, 0.110236540250833, 0.133208312264786,
            0.158497821158516, 0.18638320918205, 0.217244937079059,
            0.251609355705056, 0.290224108785119, 0.334196617097765,
            0.385272264241011, 0.446471393165703, 0.523877365306375,
            0.63392720576047, 1
        },
        {
            // paste(qbeta(p, 2, 5), collapse = ", ")
            0, 0.0628498917083544, 0.0925952589131288, 0.117379501492188,
            0.139880688269958, 0.161162916790327, 0.181803471318949,
            0.20218104339977, 0.222583533615424, 0.243259634976479,
            0.26444998329566, 0.286411749759018, 0.309444427545315,
            0.333924134989511, 0.36035769038002, 0.389479485200725,
            0.422447524846272, 0.46130359954471, 0.510316306551492,
            0.581803409252026, 1
        }
    };
    real EPSILON_BETA = 1e-14;

    int N_CAUCHY_TESTS = 4;
    array[N_CAUCHY_TESTS, 2] real cauchy_test_params = {
        // location (mu), scale (sigma)
        {0, 1},
        {0, 5},
        {1, 3},
        {-2, 2.5}
    };
    array[N_CAUCHY_TESTS, N_P] real cauchy_test_expected_q = {
        // Test values from R
        {
            // paste(qcauchy(p, 0, 1), collapse = ", ")
            -INF, -6.31375151467504, -3.07768353717525, -1.96261050550515,
            -1.37638192047117, -1, -0.726542528005361, -0.509525449494429,
            -0.324919696232906, -0.158384440324536, 0, 0.158384440324537,
            0.324919696232907, 0.509525449494429, 0.726542528005361, 1,
            1.37638192047117, 1.96261050550515, 3.07768353717525,
            6.31375151467505, INF
        },
        {
            // paste(qcauchy(p, 0, 5), collapse = ", ")
            -INF, -31.5687575733752, -15.3884176858763, -9.81305252752575,
            -6.88190960235587, -5, -3.6327126400268, -2.54762724747214,
            -1.62459848116453, -0.791922201622682, 0, 0.791922201622683,
            1.62459848116453, 2.54762724747214, 3.63271264002681, 5,
            6.88190960235587, 9.81305252752576, 15.3884176858763,
            31.5687575733753, INF
        },
        {
            // paste(qcauchy(p, 1, 3), collapse = ", ")
            -INF, -17.9412545440251, -8.23305061152576, -4.88783151651545,
            -3.12914576141352, -2, -1.17962758401608, -0.528576348483287,
            0.0252409113012808, 0.524846679026391, 1, 1.47515332097361,
            1.97475908869872, 2.52857634848329, 3.17962758401608, 4,
            5.12914576141352, 6.88783151651546, 10.2330506115258,
            19.9412545440252, INF
        },
        {
            // paste(qcauchy(p, -2, 2.5), collapse = ", ")
            -INF, -17.7843787866876, -9.69420884293814, -6.90652626376288,
            -5.44095480117793, -4.5, -3.8163563200134, -3.27381362373607,
            -2.81229924058227, -2.39596110081134, -2, -1.60403889918866,
            -1.18770075941773, -0.726186376263928, -0.183643679986597, 0.5,
            1.44095480117793, 2.90652626376288, 5.69420884293814,
            13.7843787866876, INF
        }
    };
    real EPSILON_CAUCHY = 1e-12;  // 1e-13 doesn't work

    int N_GAMMA_TESTS = 7;
    array[N_GAMMA_TESTS, 2] real gamma_test_params = {
        // shape (alpha), rate (beta)
        {1, 1},
        {1, 2},
        {3, 2},
        {5, 1},
        {7.5, 1},
        {9, 0.5},
        {4.82, 0.88}  // our Gershman (2016) favourites
    };
    array[N_GAMMA_TESTS, N_P] real gamma_test_expected_q = {
        // Test values from R
        {
            // paste(qgamma(p, 1, 1), collapse = ", ")
            0, 0.0512932943875505, 0.105360515657826, 0.162518929497775,
            0.22314355131421, 0.287682072451781, 0.356674943938732,
            0.430782916092454, 0.510825623765991, 0.59783700075562,
            0.693147180559945, 0.798507696217772, 0.916290731874155,
            1.04982212449868, 1.20397280432594, 1.38629436111989,
            1.6094379124341, 1.89711998488588, 2.30258509299405,
            2.99573227355399, INF
        },
        {
            // paste(qgamma(p, 1, 2), collapse = ", ")
            0, 0.0256466471937753, 0.0526802578289132, 0.0812594647488875,
            0.111571775657105, 0.14384103622589, 0.178337471969366,
            0.215391458046227, 0.255412811882995, 0.29891850037781,
            0.346573590279973, 0.399253848108886, 0.458145365937078,
            0.524911062249339, 0.601986402162968, 0.693147180559945,
            0.80471895621705, 0.948559992442941, 1.15129254649702,
            1.497866136777, INF
        },
        {
            // paste(qgamma(p, 3, 2), collapse = ", ")
            0, 0.408845723581977, 0.551032664124661, 0.665318294036726,
            0.767522101322322, 0.86364970893026, 0.956887897063531,
            1.04931738191883, 1.14253845200169, 1.23796915158474,
            1.33703015686178, 1.44129983519196, 1.55268929863168,
            1.6736902190136, 1.807783832933, 1.96020103014628,
            2.13951493006267, 2.36152578169733, 2.66116016891711,
            3.147896810936, INF
        },
        {
            // paste(qgamma(p, 5, 1), collapse = ", ")
            0, 1.97014956805953, 2.43259102596266, 2.78502972210798,
            3.0895396280197, 3.36860038597732, 3.6336090829638,
            3.89162148414803, 4.14773588047054, 4.40617589931198,
            4.67090888279598, 4.94610786289654, 5.23661811569773,
            5.54857114096589, 5.89036131369701, 6.27443069844469,
            6.72097878748656, 7.2669679976155, 7.99358958605263,
            9.15351902663757, INF
        },
        {
            // paste(qgamma(p, 7.5, 1), collapse = ", ")
            0, 3.63047196383501, 4.27337812085227, 4.74964079827436,
            5.15347950331264, 5.51826882954551, 5.86058448647248,
            6.19044387784394, 6.51487479968726, 6.83950354389229,
            7.16942975547832, 7.50984381821166, 7.86661147579392,
            8.24700676558967, 8.66084724924961, 9.12254280120757,
            9.65532855529545, 10.301503908206, 11.1535647907893,
            12.4978950698643, INF
        },
        {
            // paste(qgamma(p, 9, 0.5), collapse = ", ")
            0, 9.39045508068898, 10.8649361165089, 11.9462515398972,
            12.8569530964119, 13.6752903503983, 14.4398623422606,
            15.1738110389047, 15.8932117219243, 16.6107821925416,
            17.3379023687407, 18.0860121405924, 18.8679041212485,
            19.6993135959085, 20.601354114108, 21.6048897957282,
            22.7595458211044, 24.1554709846535, 25.9894230826372,
            28.8692994303926, INF
        },
        {
            // paste(qgamma(p, 4.82, 0.88), collapse = ", ")
            0, 2.11161808363452, 2.62064088009752, 3.0097714382654,
            3.34665966471938, 3.65586592516826, 3.94987167916868,
            4.23642540648445, 4.52114465122086, 4.80870078577801,
            5.10349845392268, 5.41018640614245, 5.73417689796538,
            6.08233250109886, 6.46406138447198, 6.89332127448846,
            7.39278702812332, 8.00397094433884, 8.81809776782466,
            10.1192071633214, INF
        }
    };
    real EPSILON_GAMMA = 1e-13;  // doesn't quite manage 1e-14

    int N_TO_POSITIVE_INT_TESTS = 5;
    array[N_TO_POSITIVE_INT_TESTS] real to_positive_int_params = {
        0.0, 1.0, 5.1, 100.99, 2147483646.0
    };
    array[N_TO_POSITIVE_INT_TESTS] int to_positive_int_expected = {
        0,   1,   5,   100,    2147483646
    };
    int N_WIENER_TESTS = 4;
    array[N_WIENER_TESTS, 4] real wiener_test_params = {
        // alpha (boundary sep., >0),
        // tau (nondecision time, >0),
        // beta (start, 0-1),
        // delta (rate)
        {2, 0.3, 0.5, 0},  // demo values from ?qwiener
        {1, 1e-5, 0.5, 0.7},
        {1, 99, 0.5, 0.3},
        {4, 0.1, 0.2, -0.3}
    };
    array[N_WIENER_TESTS, N_P] real wiener_test_expected_q = {
        // Test values from R
        {
            // paste(qwiener(p, 2, 0.3, 0.5, 0, resp=rep("upper", length(p))), collapse = ", ")
            0.125, 0.560317782685161, 0.669613259378821, 0.782604275271297,
            0.909202825278044, 1.05749567685416, 1.23849643592257,
            1.47170581773389, 1.80036591459066, 2.36220996780321, 55, INF, INF,
            INF, INF, INF, INF, INF, INF, INF, INF
        },
        {
            // paste(qwiener(p, 1, 1e-5, 0.5, 0.7, resp=rep("upper", length(p))), collapse = ", ")
            0.005859375, 0.056625128199812, 0.0769340710830875,
            0.0964674862625543, 0.116885954921599, 0.139079156622756,
            0.163835697923787, 0.192082859517541, 0.225111804145854,
            0.264952990342863, 0.315202706609853, 0.383314897655509,
            0.489498388953507, 0.744627000764012, INF, INF, INF, INF, INF, INF,
            INF
        },
        {
            // paste(qwiener(p, 1, 99, 0.5, 0.3, resp=rep("upper", length(p))), collapse = ", ")
            0.5, 99.0611168317355, 99.0849751511132, 99.1087843149671,
            99.1345429452576, 99.1635524735284, 99.1972394864388,
            99.237649412371, 99.2882636351897, 99.3560886072373,
            99.4592687927798, 99.6829128492391, INF, INF, INF, INF, INF, INF,
            INF, INF, INF
        },
        {
            // paste(qwiener(p, 4, 0.1, 0.2, -0.3, resp=rep("upper", length(p))), collapse = ", ")
            0.375, 6.85549499467015, INF, INF, INF, INF, INF, INF, INF, INF,
            INF, INF, INF, INF, INF, INF, INF, INF, INF, INF, INF
        }
    };
    real EPSILON_WIENER = 1e-13;  // not quite 1e-14

    int N_HALF_NORMAL_TESTS = 4;
    array[N_HALF_NORMAL_TESTS, 3] real upper_half_normal_params = {
        // p, mu, sigma
        // Very basic tests only.
        {0.0, 0, 1},
        {0.0, 0, 2},
        {0.0, 5, 1},
        {1.0, 0, 1}
    };
    array[N_HALF_NORMAL_TESTS] real upper_half_normal_expected = {
        // q
        0.0,
        0.0,
        5.0,
        INF
    };
    real EPSILON_HALF_NORMAL = 1e-14;

    int N_HALF_CAUCHY_TESTS = 4;
    array[N_HALF_CAUCHY_TESTS, 3] real upper_half_cauchy_params = {
        // p, mu, sigma
        // Very basic tests only.
        {0.0, 0, 1},
        {0.0, 0, 2},
        {0.0, 5, 1},
        {1.0, 0, 1}
    };
    array[N_HALF_CAUCHY_TESTS] real upper_half_cauchy_expected = {
        // q
        0.0,
        0.0,
        5.0,
        INF
    };
    real EPSILON_HALF_CAUCHY = 1e-14;

    // ========================================================================
    // Testing
    // ========================================================================

    // ------------------------------------------------------------------------
    // qbeta()
    // ------------------------------------------------------------------------

    print("--- Testing qbeta() with tolerance ", EPSILON_BETA);
    for (t in 1:N_BETA_TESTS) {
        real alpha = beta_test_params[t, 1];
        real beta = beta_test_params[t, 2];
        for (i in 1:N_P) {
            real p = test_p[i];
            real q = beta_test_expected_q[t, i];  // expected, from R
            real x = qbeta(p, alpha, beta);  // obtained
            if (abs(x - q) > EPSILON_BETA) {
                reject(
                    "Bad result for qbeta(p = ", p,
                    ", alpha = ", alpha,
                    ", beta = ", beta,
                    "); expected q = ", q,
                    " but obtained ", x
                );
            } else {
                print(
                    "Good result for qbeta(p = ", p,
                    ", alpha = ", alpha,
                    ", beta = ", beta,
                    "); expected q = ", q,
                    ", obtained ", x
                );
            }
        }
    }

    // ------------------------------------------------------------------------
    // qcauchy()
    // ------------------------------------------------------------------------

    print("--- Testing qcauchy() with tolerance ", EPSILON_CAUCHY);
    for (t in 1:N_CAUCHY_TESTS) {
        real mu = cauchy_test_params[t, 1];
        real sigma = cauchy_test_params[t, 2];
        for (i in 1:N_P) {
            real p = test_p[i];
            real q = cauchy_test_expected_q[t, i];  // expected, from R
            real x = qcauchy(p, mu, sigma);  // obtained
            if (abs(x - q) > EPSILON_CAUCHY) {
                reject(
                    "Bad result for qcauchy(p = ", p,
                    ", mu = ", mu,
                    ", sigma = ", sigma,
                    "); expected q = ", q,
                    " but obtained ", x
                );
            } else {
                print(
                    "Good result for qcauchy(p = ", p,
                    ", mu = ", mu,
                    ", sigma = ", sigma,
                    "); expected q = ", q,
                    ", obtained ", x
                );
            }
        }
    }

    // ------------------------------------------------------------------------
    // qgamma()
    // ------------------------------------------------------------------------

    print("--- Testing qgamma() with tolerance ", EPSILON_GAMMA);
    for (t in 1:N_GAMMA_TESTS) {
        real alpha = gamma_test_params[t, 1];
        real beta = gamma_test_params[t, 2];
        for (i in 1:N_P) {
            real p = test_p[i];
            real q = gamma_test_expected_q[t, i];  // expected, from R
            real x = qgamma(p, alpha, beta);  // obtained
            if (abs(x - q) > EPSILON_GAMMA) {
                reject(
                    "Bad result for qgamma(p = ", p,
                    ", alpha = ", alpha,
                    ", beta = ", beta,
                    "); expected q = ", q,
                    " but obtained ", x
                );
            } else {
                print(
                    "Good result for qgamma(p = ", p,
                    ", alpha = ", alpha,
                    ", beta = ", beta,
                    "); expected q = ", q,
                    ", obtained ", x
                );
            }
        }
    }

    // ------------------------------------------------------------------------
    // qwiener()
    // ------------------------------------------------------------------------

    print("--- Testing to_positive_int()");
    for (t in 1:N_TO_POSITIVE_INT_TESTS) {
        real input = to_positive_int_params[t];
        int expected = to_positive_int_expected[t];
        int obtained = to_positive_int(input);
        if (expected != obtained) {
            reject(
                "Bad result for to_positive_int(", input,
                "); expected ", expected,
                " but obtained ", obtained
            );
        } else {
            print(
                "Good result for to_positive_int(", input,
                ") -> ", obtained
            );
        }
    }
    print("--- Testing qwiener() with tolerance ", EPSILON_WIENER);
    for (t in 1:N_WIENER_TESTS) {
        real alpha = wiener_test_params[t, 1];
        real tau = wiener_test_params[t, 2];
        real beta = wiener_test_params[t, 3];
        real delta = wiener_test_params[t, 4];
        for (i in 1:N_P) {
            real p = test_p[i];
            real q = wiener_test_expected_q[t, i];  // expected, from R
            real x = qwiener(p, alpha, tau, beta, delta);  // obtained
            if (abs(x - q) > EPSILON_WIENER) {
                reject(
                    "Bad result for qwiener(p = ", p,
                    ", alpha = ", alpha,
                    ", tau = ", tau,
                    ", beta = ", beta,
                    ", delta = ", delta,
                    "); expected q = ", q,
                    " but obtained ", x
                );
            } else {
                print(
                    "Good result for qwiener(p = ", p,
                    ", alpha = ", alpha,
                    ", tau = ", tau,
                    ", beta = ", beta,
                    ", delta = ", delta,
                    "); expected q = ", q,
                    ", obtained ", x
                );
            }
        }
    }

    // ------------------------------------------------------------------------
    // Half distributions
    // ------------------------------------------------------------------------

    print(
        "--- Testing qupperhalfnormal() with tolerance ", EPSILON_HALF_NORMAL
    );
    for (t in 1:N_HALF_NORMAL_TESTS) {
        real p = upper_half_normal_params[t, 1];
        real mu = upper_half_normal_params[t, 2];
        real sigma = upper_half_normal_params[t, 3];
        real q = upper_half_normal_expected[t];
        real x = qupperhalfnormal(p, mu, sigma);
        if (abs(x - q) > EPSILON_HALF_NORMAL) {
            reject(
                "Bad result for qupperhalfnormal(", p,
                ", ", mu,
                ", ", sigma,
                "); expected ", q,
                " but obtained ", x
            );
        } else {
            print(
                "Good result for qupperhalfnormal(", p,
                ", ", mu,
                ", ", sigma,
                ") -> ", x
            );
        }
    }
    print(
        "--- Testing qupperhalfcauchy() with tolerance ", EPSILON_HALF_CAUCHY
    );
    for (t in 1:N_HALF_CAUCHY_TESTS) {
        real p = upper_half_cauchy_params[t, 1];
        real mu = upper_half_cauchy_params[t, 2];
        real sigma = upper_half_cauchy_params[t, 3];
        real q = upper_half_cauchy_expected[t];
        real x = qupperhalfcauchy(p, mu, sigma);
        if (abs(x - q) > EPSILON_HALF_CAUCHY) {
            reject(
                "Bad result for qupperhalfnormal(", p,
                ", ", mu,
                ", ", sigma,
                "); expected ", q,
                " but obtained ", x
            );
        } else {
            print(
                "Good result for qupperhalfnormal(", p,
                ", ", mu,
                ", ", sigma,
                ") -> ", x
            );
        }
    }

    // ------------------------------------------------------------------------
    // Done.
    // ------------------------------------------------------------------------

    print("--- All tests successful.");

}
