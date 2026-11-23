## ----check pandoc, eval=TRUE, echo=FALSE--------------------------------------
if (!requireNamespace("rmarkdown", quietly = TRUE) ||
      !rmarkdown::pandoc_available("1.14")) {
    warning(call. = FALSE, "These vignettes assume rmarkdown and pandoc version 1.14. These were not found. Older versions will not work.")
    knitr::knit_exit()
  }

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(emplikDTA)

## -----------------------------------------------------------------------------
mu_true <- c(0, 2.5, 3.69)
sigma_true <- c(1, 1.1, 1.2)

n1 <- 15
n2 <- 15
n3 <- 15

set.seed(345)
X1 <- rnorm(n1, mu_true[1], sigma_true[1])
X2 <- rnorm(n2, mu_true[2], sigma_true[2])
X3 <- rnorm(n3, mu_true[3], sigma_true[3])

## -----------------------------------------------------------------------------
vus(x = X1, y = X2, z = X3)

## -----------------------------------------------------------------------------
data_test <- data.frame(biom = c(X1, X2, X3), 
                        diag = rep(c("1", "2", "3"), c(n1, n2, n3)))

## -----------------------------------------------------------------------------
vus(formula = biom ~ diag, data = data_test)

## -----------------------------------------------------------------------------
vus(formula = biom ~ diag, data = data_test, ci_level = 0.90)

## -----------------------------------------------------------------------------
vus(formula = biom ~ diag, data = data_test, diag_levels = c("1", "2", "3"))

## -----------------------------------------------------------------------------
vus(formula = biom ~ diag, data = data_test, diag_levels = c("2", "3", "1"))

## -----------------------------------------------------------------------------
out_test <- vus(formula = biom ~ diag, data = data_test)
print(out_test)

## -----------------------------------------------------------------------------
vus(formula = biom ~ diag, data = data_test, plot = TRUE)

