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
mu_true <- c(0, 2.5, 3.69)
sigma_true <- c(1, 1.1, 1.2)

n1 <- 30
n2 <- 30
n3 <- 30

X1 <- rnorm(n1, mu_true[1], sigma_true[1])
X2 <- rnorm(n2, mu_true[2], sigma_true[2])
X3 <- rnorm(n3, mu_true[3], sigma_true[3])

## -----------------------------------------------------------------------------
out_roc_surface <- ROC_emp_surface(X1, X2, X3, ncp = 35,
                                   color = "steelblue", alpha = 0.5)
rgl::rglwidget(
    width = 800,
    height = 800
)

## -----------------------------------------------------------------------------
CR_tcfs(out_roc_surface, cpts = c(0.5, 3.4), fill = TRUE)
rgl::rglwidget(width = 800, height = 800)
rgl::close3d()

## -----------------------------------------------------------------------------
data_test <- data.frame(biom = c(X1, X2, X3), 
                          diag = rep(c("1", "2", "3"), c(n1, n2, n3)))

## -----------------------------------------------------------------------------
out_roc_surface_2 <- ROC_emp_surface(formula = biom ~ diag, data = data_test, ncp = 35,
                                     color = "steelblue", alpha = 0.5)
rgl::rglwidget(
    width = 800,
    height = 800
)

## ----eval=FALSE---------------------------------------------------------------
# plot(out_roc_surface_2)
# CR_tcfs(out_roc_surface_2, cpts = c(0.5, 3.4), fill = TRUE)
# rgl::rglwidget(width = 800, height = 800)
# rgl::close3d()

## ----include=FALSE------------------------------------------------------------
rgl::close3d()

