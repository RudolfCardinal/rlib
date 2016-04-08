# miscvisualize.R

requireNamespace("arm")
library("ggplot2")

#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

miscvisualize = new.env()

#==============================================================================
# Visualize regression output from an lme4 logistic regression
#==============================================================================

# SEE ALSO https://rpubs.com/mcgill_linguistics/63173, for visualization
# of logistic regression with multiple factors.

miscvisualize$visualize_regression_predictions_1cov_1factor <- function(
        model,  # lme4 model
        factorname,
        xvarname,
        constants,
        num_x_values = 200,
        inverse_link = arm::invlogit,
        ylab_link = "logit",
        ylab_response = "p"
) {
    fulldata <- model@frame

    # ?predict.merMod
    # methods(class="merMod")

    # We're going to make a grid of values for the prediction, using
    # expand.grid. That works like:
    #       expand.grid(a=c(1, 2), b=c(3, 4))
    #       expand.grid(list(a=c(1, 2), b=c(3, 4)))
    # We'll use the latter form.
    # But to make a named list, arbitrarily...

    # Start with the constants.
    expandables <- as.list(constants)
    expandable_names <- names(constants)
    # Add in the factors
    expandables[[length(expandables) + 1]] <- unique(fulldata[, factorname])
    expandable_names <- c(expandable_names, factorname)
    # Add in the variable
    xvar <- fulldata[, xvarname]
    expandables[[length(expandables) + 1]] <- seq(min(xvar), max(xvar),
                                                  length.out = num_x_values)
    expandable_names <- c(expandable_names, xvarname)
    # Name our expandables
    names(expandables) <- expandable_names
    # We don't need to bother with random effects, since we will turn off
    # predictions relating to those.

    # Now expand!
    newdata <- expand.grid(expandables)
    # Now predict!
    newdata <- within(newdata, {
        predicted_link <- predict(model, newdata, re.form = NA)
        predicted_response <- inverse_link(predicted_link)
    })
    # Now plot!
    link_plot <- (
        ggplot(newdata, aes_string(x = xvarname,
                                   y = "predicted_link",
                                   colour = factorname))
        + geom_line()
        + ylab(ylab_link)
    )
    response_plot <- (
        ggplot(newdata, aes_string(x = xvarname,
                                   y = "predicted_response",
                                   colour = factorname))
        + geom_line()
        + ylab(ylab_response)
    )
    return(list(
        link_plot = link_plot,
        response_plot = response_plot
    ))
}

#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

if ("miscvisualize" %in% search()) detach("miscvisualize")
attach(miscvisualize)  # subsequent additions not found, so attach at the end
