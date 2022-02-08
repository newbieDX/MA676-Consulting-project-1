# [ MA589: Bootstrap intro ]

# Law school data (Efron)
lsat <- read.csv("D:/Boston University/Courses/MA675-Statistical Practicum/consulting/10.06- environmental health/code/lsat.csv", comment.char="#")
(lsat_cor <- cor(lsat$lsat, lsat$gpa))
sd_cor <- function (r, n) (1 - r ^ 2) / sqrt(n - 3)
sd_cor(lsat_cor, nrow(lsat))

# Bootstrap sample from data frame `x` with statistics `g` for `B` samples
bootstrap <- function (x, g, B = 100) {
  n <- nrow(x)
  theta.star <- numeric(B)
  for (i in 1:B) { # for each bootstrap sample
    x.star <- x[sample.int(n, replace = TRUE), ]
    theta.star[i] <- g(x.star)
  }
  theta.star
}
 
bootstrap_nonparam <- function (x, g, B = 100)
  replicate(B, g(x[sample.int(nrow(x), replace = TRUE), ]))
 
cor_lsat <- function (x) cor(x$lsat, x$gpa)
lsat_boot <- bootstrap_nonparam(lsat, cor_lsat, B = 1000)
sd(lsat_boot)
hist(lsat_boot); abline(v = lsat_cor, col = "red", lwd = 2)


# Parametric bootstrap: sample replications with `sample_x`
bootstrap_param <- function (x, g, sample_x, B = 100)
  replicate(B, g(sample_x()))

# parametric estimate based on bivariate normal
sample_binormal <- function (x) {
  n <- nrow(x); p <- ncol(x)
  mu <- colMeans(x)
  C <- chol(cov(x))
  function ()
    as.data.frame(t(drop(replicate(n, mu + crossprod(C, rnorm(p))))))
}
lsat_pboot <- bootstrap_param(lsat, cor_lsat,
                              sample_binormal(lsat), B = 1000)
sd(lsat_pboot)
hist(lsat_pboot); abline(v = lsat_cor, col = "red", lwd = 2)


# Bayesian bootstrap (Rubin)
bayesian_boot_sample <- function (n)
  sample.int(n, prob = diff(c(0, sort(runif(n - 1)), 1)), replace = TRUE)
bootstrap_bayesian <- function (x, g, B = 100)
  replicate(B, g(x[bayesian_boot_sample(nrow(x)), ]))
 
lsat_bboot <- bootstrap_bayesian(lsat, cor_lsat, B = 1000)
sd(lsat_bboot)
hist(lsat_bboot); abline(v = lsat_cor, col = "red", lwd = 2)


# Fisher transformation
fisher_cor <- function (r) .5 * log((1 + r) / (1 - r))
hist(fisher_cor(lsat_boot))
hist(fisher_cor(lsat_pboot))
hist(fisher_cor(lsat_bboot))
