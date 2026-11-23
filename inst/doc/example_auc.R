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
mu_true <- c(0, 1)
sigma_true <- c(1, 1)

## -----------------------------------------------------------------------------
n1 <- 250
n2 <- 250

## -----------------------------------------------------------------------------
X1 <- rnorm(n1, mu_true[1], sigma_true[1])
X2 <- rnorm(n2, mu_true[2], sigma_true[2])

## -----------------------------------------------------------------------------
auc(x = X1, y = X2)

## -----------------------------------------------------------------------------
data_test <- data.frame(biom = c(X1, X2), diag = rep(c("1", "2"), c(n1, n2)))

## -----------------------------------------------------------------------------
auc(formula = biom ~ diag, data = data_test)

## -----------------------------------------------------------------------------
auc(formula = biom ~ diag, data = data_test, ci_level = 0.9)

## -----------------------------------------------------------------------------
auc(formula = biom ~ diag, data = data_test, diag_levels = c("2", "1"))

## -----------------------------------------------------------------------------
out_test <- auc(formula = biom ~ diag, data = data_test)
print(out_test)

## -----------------------------------------------------------------------------
auc(formula = biom ~ diag, data = data_test, plot = TRUE)

## -----------------------------------------------------------------------------
out_roc <- ROC_emp_curve(x = X1, y = X2, plot = FALSE)

## -----------------------------------------------------------------------------
plot(out_roc) +
    geom_confidence_region(out_roc, cpt = 0.5) +
    geom_operating_point(out_roc, cpt = 0.5)

## -----------------------------------------------------------------------------
out_roc_2 <- ROC_emp_curve(formula = biom ~ diag, data = data_test, 
                           plot = FALSE)
p1 <- plot(out_roc_2)
p1

## -----------------------------------------------------------------------------
p1 + geom_confidence_region(out_roc_2, cpt = 0.5) +
  geom_operating_point(out_roc_2, cpt = 0.5) 

## -----------------------------------------------------------------------------
out_thresh <- threshold2(x = X1, y = X2, method = "YI", kernel_type = "gauss", 
                         bwd_type = "AZZ", plot = FALSE)


## -----------------------------------------------------------------------------
p1 + geom_confidence_region(out_thresh, cpt_type = "optimal") +
  geom_operating_point(out_thresh, cpt_type = "optimal") 

## ----eval=FALSE---------------------------------------------------------------
# out_auc_MC <- lapply(1:5000, FUN = function(i, n1, n2, mu, sigma){
#   x1 <- rnorm(n1, mu[1], sigma[1])
#   x2 <- rnorm(n2, mu[2], sigma[2])
#   out_auc_temp <- auc(x = x1, y = x2, seed = i)
#   r_mean <- 1/mean(out_auc_temp$r.bts)
#   ll_val_mean <- r_mean * out_auc_temp$ll.value
#   ll_val_med <- out_auc_temp$ll.value * out_auc_temp$r.adj
#   signed_root_mean <- sign(out_auc_temp$estimate - 1/2) * sqrt(ll_val_mean)
#   p_val_mean <- pnorm(signed_root_mean, lower.tail = FALSE)
#   p_val_med <- out_auc_temp$p.value
#   return(list(ll_val_mean = ll_val_mean, ll_val_med = ll_val_med,
#               p_val_mean = p_val_mean, p_val_med = p_val_med))
# }, n1 = 30, n2 = 30, mu = mu_true, sigma = sigma_true)

## ----eval=FALSE---------------------------------------------------------------
# p_val_auc_mean_MC <- sapply(1:5000, function(i) out_auc_MC[[i]]$p_val_mean)
# hist(p_val_auc_mean_MC)

## ----eval=FALSE---------------------------------------------------------------
# p_val_auc_med_MC <- sapply(1:5000, function(i) out_auc_MC[[i]]$p_val_med)
# hist(p_val_auc_med_MC)

## ----eval=FALSE---------------------------------------------------------------
# sapply(c(0.1, 0.05, 0.01), FUN = function(x) mean(p_val_auc_mean_MC <= x))

## ----eval=FALSE---------------------------------------------------------------
# sapply(c(0.1, 0.05, 0.01), FUN = function(x) mean(p_val_auc_med_MC <= x))

