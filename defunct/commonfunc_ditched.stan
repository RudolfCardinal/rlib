
    // ------------------------------------------------------------------------
    // Randomized probability matching (RPM)
    // ------------------------------------------------------------------------
    // Disappointingly, in real use this fails with bad_alloc errors, meaning,
    // I think, out of memory (on a 16 Gb machine).
    // So possibly we need to preprocess it in R.

    /*

    NOTES ON NUMERIC INTEGRATION IN STAN.
    See Chapters 20.3 and 45 of the Stan 2017 manual.
    Generic signature for an ODE in Stan:

        real[] rpmOde(real t,  // time
                      real[] y,  // system state
                      real[] theta,  // parameters
                      real[] x_r,  // real data
                      int[] x_i)  // integer data
        {
            // to pass empty parameters use e.g. "real someparam[0]"
            // ...

            return dy_dt;
            // ... derivatives with respect to time of the state
            // ... length must match that of y
        }

    Call it with e.g.

        real solution[,];
        // ... 2D array; solutions at the specified times
        // ... number of COLUMNS is the length of "initial_state" and "y"
        // ... number of ROWS is the length of "times"

        solution = integrate_ode_rk45(
            ode,  // function
            initial_state,  // real[], initial state; same length as "y" (state)
            initial_time,  // real or int, initial time
            times,  // real[], solution times
            theta,  // real[], parameters
            x_r,  // real[], real data
            x_i,  // int[], integer data
            rel_tol,  // OPTIONAL; real; relative tolerance for the ODE solver
            abs_tol,  // OPTIONAL; real; relative tolerance for the ODE solver
            max_num_steps  // OPTIONAL; int; max #steps to take in ODE solver
        )

    Looking at the source, I suspect that all of
        y0, theta, x (= x_r), x_int (= x_i)
    are constant, and "t" varies.

    For comparison, the integrate() function in R:

        integrate(f, lower, upper, ...)
        ... f takes a numeric first argument and returns a numeric vector of
            the same length [e.g. f(x) returns dy/xy, and integrate(f, x0, x1)
            gives us y, specifically the definite integral of f(x) = dy/dx from
            x0 to x1.
        ... returns list x where x$value is the final estimate.

    FAILURE: if we try to pack information into the "state" variable it gets
    altered, and if we try to use theta, x_r, or x_i, Stan moans that we must
    pass arguments that are "data only and [must] not reference parameters",
    by which it means defined in the "data" block (not as local variables).
    This is a limitation of the integrate_ode_*() functions.

    More on numerical integration in Stan:
        http://mc-stan.org/events/stancon2017-notebooks/stancon2017-margossian-gillespie-ode.html
        https://github.com/stan-dev/stan/blob/develop/src/test/test-models/good/integrate_ode_rk45.stan
        https://github.com/stan-dev/math/blob/develop/stan/math/prim/arr/functor/integrate_ode_rk45.hpp
    Pretty difficult to get solution_times sorted!
        "fourth argument to integrate_ode_rk45 (solution times) must be
            data only and not reference parameters"
        - https://groups.google.com/forum/#!topic/stan-users/5wZmNcdjzb4
        - http://discourse.mc-stan.org/t/ode-integrator-calls-from-user-defined-functions/2043/4

    We could, of course, build our own integrator...

    */

    /*
    real betaFunction(real alpha, real beta)
    {
        // Not actually used.
        // Stan 2017 (v2.16.0) manual p445; see also pp 532, 601.
        return exp(lbeta(alpha, beta));
    }
    */

    real dbeta(real x, real a, real b)
    {
        // Implementation of R's dbeta function.
        //      shape1 == a
        //      shape2 == b
        // R's "dDIST" functions are PDFs; e.g. dnorm(0) = 0.3989.
        //
        // Tried this:
        //
        //      d = exp(beta_lpdf(x | shape1, shape2));  // funny notation!
        //
        // ... but it fails under a number of circumstances.
        //
        // So: https://github.com/wch/r-source/blob/trunk/src/nmath/dbeta.c

        // real d;
        // real lval;

        if (a < 0 || b < 0) {
            reject("dbeta: Bad parameters");
        }
        if (x < 0 || x > 1) {
            return 0;  // R_D__0 means 0
        }

        // limit cases for (a,b), leading to point masses
        if (a == 0 || b == 0 || is_inf(a) || is_inf(b)) {
            if (a == 0 && b == 0) {  // point mass 1/2 at each of {0,1} :
                if (x == 0 || x == 1) {
                    return positive_infinity();
                } else {
                    return 0;
                }
            }
            if (a == 0 || a/b == 0) {  // point mass 1 at 0
                if (x == 0) {
                    return positive_infinity();
                } else {
                    return 0;
                }
            }
            if (b == 0 || b/a == 0) {  // point mass 1 at 1
                if (x == 1) {
                    return positive_infinity();
                } else {
                    return 0;
                }
            }
            // else, remaining case:  a = b = Inf : point mass 1 at 1/2
            if (x == 0.5) {
                return positive_infinity();
            } else {
                return 0;
            }
        }

        if (x == 0) {
            if (a > 1) {
                return 0;
            }
            if (a < 1) {
                return positive_infinity();
            }
            /* a == 1 : */
            // return(R_D_val(b));
            // ... see below: empirically, dbeta(0, 1, b) -> b
            return b;
        }
        if (x == 1) {
            if (b > 1) {
                return 0;
            }
            if (b < 1) {
                return positive_infinity();
            }
            /* b == 1 : */
            // return(R_D_val(a));
            // ... defined in dpq.h as:
            // #define R_D_val(x)	(log_p	? log(x) : (x))
            // ... and empirically, dbeta(1, a, 1) -> a
            return a;
        }

        // Possibly now we can go back to Stan's?

        // if (a <= 2 || b <= 2) {
        //     lval = (a - 1) * log(x) + (b - 1) * log1p(-x) - lbeta(a, b);
        // } else {
        //     lval = log(a+b-1) + dbinom_raw(a-1, a+b-2, x, 1-x, TRUE);
        // }
        // d = exp(lval);

        return exp(beta_lpdf(x | a, b));  // funny notation!
        /*
        d = exp(beta_lpdf(x | a, b));  // funny notation!
        if (is_nan(d)) {
            reject(
                "dbeta failing:",
                " x=", x,
                ", a=", a,
                ", b=", b,
                ", beta_lpdf(x | a, b)=", beta_lpdf(x | a, b),
                ", d=", d);
        }
        return d;
        */
    }

    real pbeta(real q, real shape1, real shape2)
    {
        // Implementation of R's pbeta function.
        // q is a quantile.
        // Regardless of the way R's help phrases is, "pDIST" functions are
        // CUMULATIVE (PROBABILITY) DENSITY functions; e.g.
        // pnorm(0) == 0.5; pnorm(-999) ~= 0; pnorm(999) ~= 1.
        // The qnorm() and pnorm() functions reverse each other.
        //
        // shape1 == alpha
        // shape2 == beta

        // However, beta_cdf complains about values of 1 (probably meaning 1
        // plus a tiny bit), whereas R's is more forgiving. We need that.
        if (q >= 1) {
            return 1;
        }
        if (q <= 0) {
            return 0;
        }
        return beta_cdf(q, shape1, shape2);

        // if (is_nan(p)) {
        //     reject("pbeta failing:",
        //            " q=", q,
        //            ", shape1=", shape1,
        //            ", shape2=", shape2,
        //            ", p=", p);
        // }
        // return p;
    }

    vector rpmDifferential(real x, vector wins, vector losses, int k)
    {
        // Internal function used by rpmComputeProbopt
        vector[k] answer;
        real r;
        for (i in 1:k) {
            r = dbeta(x, wins[i] + 1, losses[i] + 1);
            for (j in 1:k) {
                if (j != i) {
                    r = r * pbeta(x, wins[j] + 1, losses[j] + 1);
                }
            }
            answer[i] = r;
            /*
            if (is_nan(r)) {
                reject("rpmDifferential failing:",
                       " x=", x,
                       ", wins=", wins,
                       ", losses=", losses,
                       ", r=", r);
            }
            */
        }
        // print("rpmDifferential(",
        //       "x=", x,
        //       ", wins=", wins,
        //       ", losses=", losses,
        //       ") -> ", answer);
        return answer;
    }

    vector rpmComputeProbopt(vector wins,  // 'y' in Scott 2010; length 'k'
                             vector totals)  // 'n' in Scott 2010; length 'k'
    {
        // Matches output from the R version compute.probopt in Scott 2010.
        // This version uses the trapezium rule for integration.
        int k = num_elements(wins);
        vector[k] losses;
        vector[k] optimum_probabilities;
        // Constants used in the integration:
        real x_start = 0.0;  // lower limit of integral
        real x_end = 1.0;  // upper limit of integral
        real x_span = x_end - x_start;
        int N_int = 1000;  // number of trapezoids
        real N_real = N_int;
        real dx = x_span / N_real;

        // Working variables:
        vector[k] f_x_previous;
        vector[k] f_x;
        real x;

        if (num_elements(totals) != k) {
            reject("rpmComputeProbopt num_elements(totals) != ",
                   "num_elements(wins) == k == ", k);
        }
        for (i in 1:k) {
            losses[i] = totals[i] - wins[i];
            if (wins[i] < 0) {
                reject("rpmComputeProbopt: invalid data with wins < 0 at ",
                       "index ", i);
            }
            if (losses[i] < 0) {
                reject("rpmComputeProbopt: invalid data with wins > total at ",
                       "index ", i);
            }
            optimum_probabilities[i] = 0.0;
        }

        f_x_previous = rpmDifferential(x_start, wins, losses, k);  // value at x=0
        x = x_start;
        for (i in 1:N_int) {
            // x = x_start + i * dx;
            x = x + dx;
            f_x = rpmDifferential(x, wins, losses, k);
            optimum_probabilities = optimum_probabilities +
                (f_x_previous + f_x);  // trapezium area * 2 / dx
            f_x_previous = f_x;
        }
        optimum_probabilities = optimum_probabilities * dx / 2.0;
            // ... finally arriving at the trapezium rule
            // ... more efficient to do the multiplication/division once only
        // print("rpmComputeProbopt(",
        //       "wins=", wins,
        //       ", totals=", totals,
        //       ") -> ", optimum_probabilities);
        return optimum_probabilities;
    }

    /*
    // Test our RPM implementation:
    vector[2] wins;
    vector[2] totals;
    vector[2] rpmprob;

    wins[1] = 19;
    wins[2] = 19;
    totals[1] = 48;
    totals[2] = 28;

    // wins[1] = 5;
    // wins[2] = 10;
    // totals[1] = 12;
    // totals[2] = 20;

    rpmprob = rpmComputeProbopt(wins, totals);
    print(rpmprob);
    reject("done");
    */
