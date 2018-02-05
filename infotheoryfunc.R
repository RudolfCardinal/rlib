#!/usr/bin/env Rscript
# infotheory.R

# =============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
# =============================================================================

infotheoryfunc = new.env()

# =============================================================================
# Normalized mutual information
# =============================================================================
# https://stackoverflow.com/questions/21831953/r-package-available-for-adjusted-mutual-information

infotheoryfunc$f_nij <- function(v1, v2, l1, l2)  # contingency table n(i,j)=t(i,j)
{
    m <- matrix(0, l1, l2)
    for (i in 1:length(v1)) {
        m[v1[i],v2[i]] <- m[v1[i], v2[i]] + 1
    }
    m
}


infotheoryfunc$f_emi <- function(s1, s2, l1, l2, n)  # expected mutual information
{
    s_emi <- 0
    for(i in 1:l1) {
        for (j in 1:l2) {
            min_nij <- max(1, s1[i] + s2[j] - n)
            max_nij <- min(s1[i], s2[j])
            n.ij <- seq(min_nij, max_nij)  # sequence of consecutive numbers
            t1 <- (n.ij / n) * log((n.ij * n) / (s1[i] * s2[j]))
            t2 <- exp(
                lfactorial(s1[i]) + lfactorial(s2[j]) +
                lfactorial(n - s1[i]) + lfactorial(n - s2[j]) -
                lfactorial(n) -
                lfactorial(n.ij) -
                lfactorial(s1[i] - n.ij) -
                lfactorial(s2[j] - n.ij) -
                lfactorial(n - s1[i] - s2[j] + n.ij)
            )
            emi <- sum(t1 * t2)
            s_emi <- s_emi + emi
        }
    }
    return(s_emi)
}

infotheoryfunc$f_nmi_ami <- function(v1, v2)
{
    library(infotheo)
    s1 <- tabulate(v1);
    s2 <- tabulate(v2);
    l1 <- length(s1)
    l2 <- length(s2)
    N <- length(v1)
    tij <- infotheoryfunc$f_nij(v1, v2, l1, l2)  # contingency table n(i,j)=t(i,j). this would be equivalent with table(v1,v2)
    mi <- infotheo::mutinformation(v1, v2) # function for Mutual Information from package infotheo
    h1 <- -sum(s1 * log(s1/N)) / N
    h2 <- -sum(s2 * log(s2/N)) / N
    nmi <- mi / max(h1, h2)        # NMI Normalized MI
    emi <- infotheoryfunc$f_emi(s1, s2, l1, l2, N)  # EMI Expected MI
    ami <- (mi - emi)/max(h1, h2)  #AMI Adjusted MI
    return(c(normalized_mutual_information=nmi,
             adjusted_mutual_information=ami))
}


# =============================================================================
# Randomized dependence coefficient
# =============================================================================
# http://papers.nips.cc/paper/5138-the-randomized-dependence-coefficient.pdf
# https://github.com/lopezpaz/randomized_dependence_coefficient/blob/master/code/algorithms.r

# library(acepack)
# library(energy)
# library(kernlab)
# library(minerva)

infotheoryfunc$computeKernelMatrix <- function(sample) {
    library(kernlab)
	n        <- nrow(sample)
	Q        <- matrix(apply(sample^2, 1, sum), n, n)
	distance <- Q + t(Q) - 2 * sample %*% t(sample)
	exp(-kernlab::sigest(sample,scale=NULL)[2]*distance)
}

infotheoryfunc$hsic <- function(sampleX, sampleY) {
	N  <- nrow(as.matrix(sampleX))
	K  <- infotheoryfunc$computeKernelMatrix(as.matrix(sampleX))
	L  <- infotheoryfunc$computeKernelMatrix(as.matrix(sampleY))
	KH <- K - 1 / N * matrix(apply(K, 2, sum), N, N)
	LH <- L - 1 / N * matrix(apply(L, 2, sum), N, N)
	1 / N * sum(sum(KH * t(LH)))
}

hsiccop <- function(sampleX, sampleY) {
    sampleX <- apply(as.matrix(sampleX), 2, function(u) ecdf(u)(u))
    sampleY <- apply(as.matrix(sampleY), 2, function(u) ecdf(u)(u))
    infotheoryfunc$hsic(sampleX, sampleY)
}

infotheoryfunc$rdc <- function(x, y, k=20, s=1/6, f=sin) {
    # from https://github.com/lopezpaz/randomized_dependence_coefficient/blob/master/code/algorithms.r
    x <- cbind(apply(as.matrix(x), 2, function(u) rank(u)/length(u)), 1)
    y <- cbind(apply(as.matrix(y), 2, function(u) rank(u)/length(u)), 1)
    x <- s / ncol(x) * x %*% matrix(rnorm(ncol(x) * k), ncol(x))
    y <- s / ncol(y) * y %*% matrix(rnorm(ncol(y) * k), ncol(y))
    cancor(cbind(f(x), 1), cbind(f(y), 1))$cor[1]
}

#infotheoryfunc$rdc_detail <- function(x, y, k=20, s=1/6, f=sin) {
#    # RNC, based on the above.
#    # *** Not finished. Intent is to try to extract *which* function is best.
#    xm <- cbind(apply(as.matrix(x), 2, function(u) rank(u)/length(u)), 1)
#    ym <- cbind(apply(as.matrix(y), 2, function(u) rank(u)/length(u)), 1)
#    wx <- matrix(rnorm(ncol(xm) * k, 0, s), ncol(xm), k)
#    wy <- matrix(rnorm(ncol(ym) * k, 0, s), ncol(ym), k)
#    tx <- cbind(f(xm %*% wx), 1)
#    ty <- cbind(f(ym %*% wy), 1)
#    cc <- cancor(tx, ty)
#    list(
#        rdc=cc$cor[1],
#        f=f,
#        xcoef=cc$xcoef[1],
#        ycoef=cc$ycoef[1],
#        xcenter=cc$xcenter[1],
#        ycenter=cc$ycenter[1]
#    )
#}
