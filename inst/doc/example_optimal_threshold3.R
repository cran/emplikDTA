## ----check pandoc, eval=TRUE, echo=FALSE--------------------------------------
if (!requireNamespace("rmarkdown", quietly = TRUE) ||
      !rmarkdown::pandoc_available("1.14")) {
    warning(call. = FALSE, "These vignettes assume rmarkdown and pandoc version 1.14. These were not found. Older versions will not work.")
    knitr::knit_exit()
  }

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
options(rgl.useNULL = TRUE)
library(emplikDTA)
library(rgl)
set.seed(34)

## -----------------------------------------------------------------------------
GYI_normal <- function(par, mu, sigma){
  ff <- function(par, mu, sigma){
    tcf1 <- pnorm(par[1], mean = mu[1], sd = sigma[1])
    tcf2 <- pnorm(par[2], mean = mu[2], sd = sigma[2]) - 
      pnorm(par[1], mean = mu[2], sd = sigma[2])
    tcf3 <- 1 - pnorm(par[2], mean = mu[3], sd = sigma[3])
    return(tcf1 + tcf2 + tcf3 + par[3]*(par[1] - par[2]))
  }
  out_optim <- optim(par = par, fn = ff, method = "L-BFGS-B",
                     lower = c(-Inf, -Inf, 0), upper = c(Inf, Inf, Inf),
                     mu = mu, sigma = sigma, control = list(fnscale = -1))
  return(out_optim)
}


## -----------------------------------------------------------------------------
mu_true <- c(0, 2.5, 3.69)
sigma_true <- c(1, 1.1, 1.2)

## -----------------------------------------------------------------------------
true_GYI_normal <- GYI_normal(c(1.25, 3.1, 0.1), mu = mu_true, 
                              sigma = sigma_true)
true_GYI_normal

## -----------------------------------------------------------------------------
n1 <- 30
n2 <- 30
n3 <- 30

## -----------------------------------------------------------------------------
X1 <- rnorm(n1, mu_true[1], sigma_true[1])
X2 <- rnorm(n2, mu_true[2], sigma_true[2])
X3 <- rnorm(n3, mu_true[3], sigma_true[3])

## -----------------------------------------------------------------------------
out_GYI_normal <- threshold3(x = X1, y = X2, z = X3, method = "GYI",
                             kernel_type = "gauss", bwd_type = "AZZ", B = 1000)

## -----------------------------------------------------------------------------
out_GYI_normal$opt_thresh

## -----------------------------------------------------------------------------
quantile(out_GYI_normal$out_bts[1,], probs = (1 + c(-1, 1)*0.95)/2, 
         type = 8, names = FALSE)

## -----------------------------------------------------------------------------
quantile(out_GYI_normal$out_bts[2,], probs = (1 + c(-1, 1)*0.95)/2, 
         type = 8, names = FALSE)

## -----------------------------------------------------------------------------
out_GYI_normal$ci_tau

## -----------------------------------------------------------------------------
out_GYI_normal$c_alp

## -----------------------------------------------------------------------------
data_test <- data.frame(biom = c(X1, X2, X3), 
                        diag = rep(c("1", "2", "3"), c(n1, n2, n3)))

## -----------------------------------------------------------------------------
out_GYI_normal_2 <- threshold3(formula = biom ~ diag, data = data_test, 
                               method = "GYI", kernel_type = "gauss", 
                               bwd_type = "AZZ", B = 1000)

## -----------------------------------------------------------------------------
out_GYI_normal_2$opt_thresh

## -----------------------------------------------------------------------------
out_GYI_normal_2$ci_tau

## -----------------------------------------------------------------------------
out_GYI_normal_2$c_alp

## ----eval=FALSE---------------------------------------------------------------
# out_roc_surface <- ROC_emp_surface(X1, X2, X3, ncp = 35)
# CR_tcfs(out_roc_surface, cpts = c(0.5, 3.4), fill = TRUE)
# rgl::rglwidget(width = 800, height = 800)
# rgl::close3d()

## ----eval=FALSE---------------------------------------------------------------
# out_roc_surface_2 <- ROC_emp_surface(formula = biom ~ diag, data = data_test, ncp = 35,
#                                      color = "steelblue", alpha = 0.5)
# rgl::rglwidget(
#     width = 800,
#     height = 800
# )

## ----eval=FALSE---------------------------------------------------------------
# CR_tcfs(out_roc_surface_2, cpts = c(0.5, 3.4), fill = TRUE)
# rgl::rglwidget(width = 800, height = 800)
# rgl::close3d()

