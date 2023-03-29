#!/usr/bin/env Rscript
#
# test_auroc_algorithm.R
#
# Tests an implemtation of AUROC (area under the receiver operating
# characteristic curve) for Stan.


if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    data.table,
    ggplot2,
    pROC  # reference implementations
)


auroc <- function(binary_outcome, probability)
{
    # R version of Stan code, for testing.
    # There are probably more efficient ways in R, but that's not the point.

    # After:
    # - https://stephanosterburg.gitbook.io/scrapbook/data-science/ds-cheatsheets/machine-learning/fast-computation-of-auc-roc-score
    # - https://github.com/jfpuget/metrics/blob/master/auc.ipynb
    #
    # "Let's first define some entities.
    #
    # - pos is the set of examples with target 1. These are the positive
    #   examples.
    # - neg is the set of examples with target 0. These are the negative
    #   examples.
    # - p(i) is the prediction for example i. p(i) is a number between 0
    #   and 1.
    # - A pair of examples (i, j) is labelled the right way if i is a
    #   positive example, j is a negative example, and the prediction for
    #   i is higher than the prediction for j.
    # - | s | is the number of elements in set s.
    #
    # Then AUC-ROC is the count of pairs labelled the right way divided
    # by the number of pairs:
    #
    #     AUC-ROC = | {(i,j), i in pos, j in neg, p(i) > p(j)} | / (| pos | * | neg |)
    #
    # A naive code to compute this would be to consider each possible
    # pair and count those labelled the right way. A much better way is
    # to sort the predictions first, then visit the examples in
    # increasing order of predictions. Each time we see a positive
    # example we add the number of negative examples we've seen so far."

    n <- length(binary_outcome)
    # Sort the binary outcome by ascending probability:
    y <- binary_outcome[order(probability)]
    n_false <- 0
    total <- 0.0;
    for (i in 1:n) {
        current_y <- y[i]
        n_false <- n_false + (1 - current_y)  # add 1 if false; unchanged if true
        total <- total + current_y * n_false
        # ... if we are seeing a positive example, add the number of
        # negative examples so far.
    }
    return(total / (n_false * (n - n_false)))
}

test_auroc <- function(seed = 1234,
                       test_n_values = c(5, 10, 25),
                       n_tests = 200,
                       test_p = 0.5)
{
    set.seed(seed)
    prespecified_test_n <- 25
    prespecified_tests <- list(
        list(
            # The example used in ?pROC::roc
            outcome = as.integer(as.logical(aSAH$outcome == "Poor")),
            probability = aSAH$s100b
        ),
        list(
            # Perfect (AUROC = 1):
            outcome = c(0, 0, 1, 1),
            probability = c(0.1, 0.2, 0.3, 0.4)
        ),
        list(
            # Random
            outcome = rbinom(n = prespecified_test_n, size = 1, prob = 0.5),
            probability = runif(n = prespecified_test_n, min = 0, max = 1)
        )
    )
    # print(tests)
    line <- "===============================================================================\n"
    for (i in 1:length(prespecified_tests)) {
        testdata <- prespecified_tests[[i]]
        outcome <- testdata$outcome
        probability <- testdata$probability
        cat(line)
        cat("TESTING: outcome =\n")
        print(outcome)
        cat("... probability =\n")
        print(probability)
        cat("Correlation:\n")
        print(cor(outcome, probability))
        cat("Via pROC::roc():\n")
        a_via_roc <- pROC::roc(
            response = outcome,
            predictor = probability,
            levels = c(0, 1),
            direction = "<"
            # see below
        )
        print(a_via_roc)
        cat("Via auroc():\n")
        a_via_auroc <- auroc(outcome, probability)
        print(a_via_auroc)
    }
    random_test_results <- NULL
    for (test_n in test_n_values) {
        for (i in 1:n_tests) {
            outcome <- rbinom(n = test_n, size = 1, prob = test_p)
            sum_outcome <- sum(outcome)
            if (sum_outcome == 0 || sum_outcome == test_n) {
                # all ones or all zeros
                next
            }
            probability <- runif(n = test_n, min = 0, max = 1)
            correl <- cor(outcome, probability)
            print(outcome)
            a_via_proc <- pROC::roc(
                response = outcome,
                predictor = probability,
                levels = c(0, 1),
                direction = "<"
            )$auc
            # By default it does direction autodetection ("auto").
            # It will correctly autodetect levels: controls = 0, case = 1
            # ... but specifying that makes it shut up.
            # Specifying "<" enforces "controls < cases".
            a_via_auroc <- auroc(outcome, probability)
            random_test_results <- rbind(
                random_test_results,
                data.table(test_n, correl, a_via_proc, a_via_auroc)
            )
        }
    }
    random_test_results[, discrepancy := a_via_proc - a_via_auroc]
    random_test_results[, abs_discrepancy := abs(discrepancy)]
    print(random_test_results)
    cat("Maximum absolute disrepancy:\n")
    print(max(random_test_results$discrepancy))
    return(random_test_results)
}


random_test_results <- test_auroc()
p <- (
    ggplot(
        random_test_results,
        aes(x = a_via_proc, y = a_via_auroc, colour = test_n)
    ) +
    geom_point()
)
print(p)

# ... yup, that is accurate enough for me.
