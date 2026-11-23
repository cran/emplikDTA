## ----check pandoc, eval=TRUE, echo=FALSE--------------------------------------
if (!requireNamespace("rmarkdown", quietly = TRUE) ||
      !rmarkdown::pandoc_available("1.14")) {
    warning(call. = FALSE, "These vignettes assume rmarkdown and pandoc version 1.14. These were not found. Older versions will not work.")
    knitr::knit_exit()
  }

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(emplikDTA)
set.seed(34)

## -----------------------------------------------------------------------------
mu_true <- c(0, 0, 0)
sigma_true <- c(1, 1, 1)

## -----------------------------------------------------------------------------
n1 <- 30
n2 <- 30
n3 <- 30

## -----------------------------------------------------------------------------
X1 <- rnorm(n1, mu_true[1], sigma_true[1])
X2 <- rnorm(n2, mu_true[2], sigma_true[2])
X3 <- rnorm(n3, mu_true[3], sigma_true[3])

## -----------------------------------------------------------------------------
out_vus <- vus(x = X1, y = X2, z = X3)

## -----------------------------------------------------------------------------
out_vus

## -----------------------------------------------------------------------------
out_vus$ll.value/mean(out_vus$r.bts)

## -----------------------------------------------------------------------------
out_vus$ll.value * out_vus$r.adj

## ----eval=FALSE---------------------------------------------------------------
# out_vus_MC <- lapply(1:5000, FUN = function(i, n1, n2, n3, mu, sigma){
#   x1 <- rnorm(n1, mu[1], sigma[1])
#   x2 <- rnorm(n2, mu[2], sigma[2])
#   x3 <- rnorm(n3, mu[3], sigma[3])
#   out_vus_temp <- vus(x = x1, y = x2, z = x3, seed = NULL)
#   r_mean <- 1/mean(out_vus_temp$r.bts)
#   ll_val_mean <- r_mean * out_vus_temp$ll.value
#   ll_val_med <- out_vus_temp$ll.value * out_vus_temp$r.adj
#   signed_root_mean <- sign(out_vus_temp$estimate - 1/6) * sqrt(ll_val_mean)
#   p_val_mean <- pnorm(signed_root_mean, lower.tail = FALSE)
#   p_val_med <- out_vus_temp$p.value
#   return(list(ll_val_mean = ll_val_mean, ll_val_med = ll_val_med,
#               p_val_mean = p_val_mean, p_val_med = p_val_med))
# }, n1 = 30, n2 = 30, n3 = 30, mu = mu_true, sigma = sigma_true)

## ----eval=FALSE---------------------------------------------------------------
# p_val_mean_MC <- sapply(1:5000, function(i) out_vus_MC[[i]]$p_val_mean)
# hist(p_val_mean_MC)

## ----eval=FALSE---------------------------------------------------------------
# p_val_med_MC <- sapply(1:5000, function(i) out_vus_MC[[i]]$p_val_med)
# hist(p_val_med_MC)

## ----eval=FALSE---------------------------------------------------------------
# mean(p_val_mean_MC <= 0.05)

## ----eval=FALSE---------------------------------------------------------------
# mean(p_val_med_MC <= 0.05)

## ----eval=FALSE, warning=FALSE------------------------------------------------
# out_vus_form_MC <- lapply(1:5000, FUN = function(i, n1, n2, n3, mu, sigma){
#   x1 <- rnorm(n1, mu[1], sigma[1])
#   x2 <- rnorm(n2, mu[2], sigma[2])
#   x3 <- rnorm(n3, mu[3], sigma[3])
#   data_test <- data.frame(biom = c(x1, x2, x3),
#                           diag = rep(c("1", "2", "3"), c(n1, n2, n3)))
#   out_vus_temp <- vus(formula = biom ~ diag, data = data_test, seed = i)
#   r_mean <- 1/mean(out_vus_temp$r.bts)
#   ll_val_mean <- r_mean * out_vus_temp$ll.value
#   ll_val_med <- out_vus_temp$ll.value * out_vus_temp$r.adj
#   signed_root_mean <- sign(out_vus_temp$estimate - 1/6) * sqrt(ll_val_mean)
#   p_val_mean <- pnorm(signed_root_mean, lower.tail = FALSE)
#   p_val_med <- out_vus_temp$p.value
#   return(list(ll_val_mean = ll_val_mean, ll_val_med = ll_val_med,
#               p_val_mean = p_val_mean, p_val_med = p_val_med))
# }, n1 = 30, n2 = 30, n3 = 30, mu = mu_true, sigma = sigma_true)

## ----eval=FALSE---------------------------------------------------------------
# p_val_mean_form_MC <- sapply(1:5000, function(i) out_vus_form_MC[[i]]$p_val_mean)
# hist(p_val_mean_form_MC)

## ----eval=FALSE---------------------------------------------------------------
# sapply(c(0.1, 0.05, 0.01), FUN = function(x) mean(p_val_mean_form_MC <= x))

## ----eval=FALSE---------------------------------------------------------------
# p_val_med_form_MC <- sapply(1:5000, function(i) out_vus_form_MC[[i]]$p_val_med)
# hist(p_val_med_form_MC)

## ----eval=FALSE---------------------------------------------------------------
# sapply(c(0.1, 0.05, 0.01), FUN = function(x) mean(p_val_med_form_MC <= x))

