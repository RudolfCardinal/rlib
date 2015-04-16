# miscneuron.R

#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

miscneuron = new.env()

#===============================================================================
# Exponentially weighted moving average (EWMA)
#===============================================================================

miscneuron$ewma = function(previoushistory, newvalue, alpha)
{
    # Exponentially weighted moving average; e.g. http://en.wikipedia.org/wiki/Moving_average_%28technical_analysis%29#Exponential_moving_average
    # Nice for several reasons, inc. it's got a very simple (single-value) memory.
    
    alpha * newvalue + (1 - alpha) * previoushistory
    
    # After an impulse, the half life n (timesteps) is given by 0.5 = (1-alpha)^n, i.e. n = log(0.5)/log(1-alpha); alternatively, alpha = 1 - 0.5^(1/n)
    # e.g. for half-life of 20 timesteps, alpha=0.034
}

#===============================================================================
# Sigmoid functions
#===============================================================================

miscneuron$logistic_sigmoid = function(x)
{
    1 / (1 + exp(-x))
}

miscneuron$inverse_logistic_sigmoid = function(y)
{
    -log( (1/y) - 1 )
}

miscneuron$width_of_standard_logistic_sigmoid_encompassing = function(fraction)
{
    threshold = 1 - (1 - fraction)/2 # e.g. for fraction 0.99, this will give 0.995
    # we want twice the value of x where 1/(1+exp(-x)) = threshold
    x = inverse_logistic_sigmoid(threshold)
    return(2 * x)
}

miscneuron$WIDTH_99PCT_STANDARD_LOGISTIC_SIGMOID = width_of_standard_logistic_sigmoid_encompassing(0.99) # cache; it's 10.58661

sigmoid_specifying_width <- function(xval, xcentre, xwidth99percent, ymin = 0, ymax = 1) {
    # based on 1/(1+exp(-x)), the standard logistic sigmoid function;
    # this has an output range of 0-1, a centre (y=0.5) of x=0, and 99% of its output (meaning 0.005 in each tail) comes,
    # approximately, from x=-5.3 to x=+5.3 (i.e. a "width" of 10.6)
    return( logistic_sigmoid( (xval - xcentre) / (xwidth99percent / miscneuron$WIDTH_99PCT_STANDARD_LOGISTIC_SIGMOID) ) * (ymax - ymin) + ymin )
}

#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

if ("miscneuron" %in% search()) detach("miscneuron")
attach(miscneuron)  # subsequent additions not found, so attach at the end
