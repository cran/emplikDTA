## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
options(rgl.useNULL = TRUE)
library(emplikDTA)

## ----data---------------------------------------------------------------------
data("ADNeuro", package = "emplikDTA")
group_order <- c("D-", "D0", "D+")
marker_formulas <- list(
  FACTOR1 = I(-FACTOR1) ~ group,
  ktemp = I(-ktemp) ~ group,
  zpsy004 = I(-zpsy004) ~ group
)
columns <- c("group", names(marker_formulas))
ad <- ADNeuro[complete.cases(ADNeuro[columns]), columns]

rbind(Available = table(ADNeuro$group), Analysed = table(ad$group))
aggregate(ad[names(marker_formulas)], list(group = ad$group), mean)

## ----roc-surface--------------------------------------------------------------
roc_fits <- lapply(marker_formulas, function(f) {
  ROC_emp_surface(f, data = ad, diag_levels = group_order,
                  ncp = 35, plot = FALSE)
})

marker <- "FACTOR1"
roc <- roc_fits[[marker]]
print(roc)
knitr::kable(
  head(data.frame(tau1 = roc$cpoint[, 1], tau2 = roc$cpoint[, 2], roc$vals)),
  row.names = FALSE, digits = 3
)

plot(roc, color = "steelblue", alpha = 0.35)
roc_widget <- rgl::rglwidget(width = 640, height = 500)
rgl::close3d()
roc_widget

## ----vus----------------------------------------------------------------------
vus_null <- 1/6
vus_fits <- lapply(marker_formulas, function(f) {
  vus(f, data = ad, diag_levels = group_order, vus0 = vus_null,
      ci_level = 0.95, B = 500, seed = 34, alternative = "greater")
})

vus_table <- do.call(rbind, lapply(vus_fits, function(fit) {
  c(VUS = fit$estimate, Lower = fit$conf.int[1], Upper = fit$conf.int[2])
}))
knitr::kable(vus_table, digits = 3, caption = "VUS and 95% EL confidence intervals.")

## ----vus-tests----------------------------------------------------------------
p_greater <- vapply(vus_fits, function(fit) {
  if (is.null(fit$p.value)) NA_real_ else fit$p.value
}, numeric(1))

n_tests <- length(vus_fits)
p_table <- cbind(
  "One-sided p" = p_greater,
  "Bonferroni" = stats::p.adjust(p_greater, method = "bonferroni", n = n_tests),
  "Holm" = stats::p.adjust(p_greater, method = "holm", n = n_tests)
)
knitr::kable(
  formatC(p_table, format = "e", digits = 3), align = "r",
  caption = "One-sided VUS p-values before and after adjustment across the three markers."
)

## ----thresholds---------------------------------------------------------------
opt_fits <- lapply(marker_formulas, function(f) {
  threshold3(f, data = ad, diag_levels = group_order, method = "all",
             kernel_type = "gauss", bwd_type = "RT",
             ci = TRUE, ci_level = 0.95, B = 1000, seed = 34, plot = FALSE)
})

thresholds <- do.call(rbind, lapply(names(opt_fits), function(m) {
  data.frame(Marker = m, opt_fits[[m]]$summary)
}))
stopifnot(all(thresholds$tau1 < thresholds$tau2))

knitr::kable(
  thresholds[c("Marker", "method", "tau1", "tau2",
               "tcf1_opt", "tcf2_opt", "tcf3_opt")],
  digits = 3, col.names = c("Marker", "Criterion", "t1", "t2", "TCF1", "TCF2", "TCF3"),
  caption = "Optimal thresholds on the sign-reversed scale and kernel estimates of the TCFs."
)

## ----threshold-intervals------------------------------------------------------
opt <- opt_fits[[marker]]
gyi_intervals <- opt$results$GYI$ci_tau
rownames(gyi_intervals) <- c("t1", "t2")
knitr::kable(gyi_intervals, digits = 3, col.names = c("Lower", "Upper"),
             caption = "95% bootstrap percentile intervals for the FACTOR1 GYI thresholds.")

## ----region-helper------------------------------------------------------------
plot_opt_region <- function(roc, fit) {
  tau <- fit$opt_thresh
  stopifnot(tau[1] < tau[2], is.finite(fit$c_alp))
  el <- emplikDTA:::empi_llike3_prepare(
    x = roc$x, y = roc$y, z = roc$z, tau = tau, type_F = "empi"
  )
  point <- emplikDTA:::tcfs_emp(roc$x, roc$y, roc$z, tau)
  stopifnot(!is.null(el), all(point > 0 & point < 1))

  grid <- seq(0.001, 0.999, length.out = 41)
  plot(roc, color = "steelblue", alpha = 0.25)
  on.exit(rgl::close3d())
  misc3d::contour3d(
    f = function(x, y, z) {
      emplikDTA:::empi_llike_3C(el, tcf1 = x, tcf2 = z, tcf3 = y)
    },
    level = fit$c_alp, x = grid, y = grid, z = grid,
    add = TRUE, color = "firebrick", alpha = 0.45
  )
  rgl::points3d(point[1], point[3], point[2], color = "firebrick", size = 9)
  rgl::rglwidget(width = 640, height = 500)
}

## ----region-gyi---------------------------------------------------------------
plot_opt_region(roc, opt$results$GYI)

## ----region-ctp---------------------------------------------------------------
plot_opt_region(roc, opt$results$CtP)

## ----region-mv----------------------------------------------------------------
plot_opt_region(roc, opt$results$MV)

## ----tcf2-fixed---------------------------------------------------------------
tcf2_fixed <- lapply(marker_formulas, function(f) {
  tcf2(f, data = ad, diag_levels = group_order,
       tcf10 = 0.8, tcf30 = 0.8,
       ci_level = 0.95, B = 500, seed = 34, plot = FALSE)
})

tcf2_fixed_table <- do.call(rbind, lapply(names(tcf2_fixed), function(m) {
  fit <- tcf2_fixed[[m]]
  stopifnot(fit$tau_est[1] < fit$tau_est[2])
  data.frame(
    Marker = m, TCF1 = fit$tcf1, TCF3 = fit$tcf3,
    t1 = fit$tau_est[1], t2 = fit$tau_est[2], TCF2 = fit$estimate,
    Lower = fit$conf.int[1], Upper = fit$conf.int[2]
  )
}))
knitr::kable(
  tcf2_fixed_table, row.names = FALSE, digits = 3,
  caption = "TCF2 and 95% EL confidence intervals at target TCF1 = TCF3 = 0.8."
)

## ----tcf2-grid----------------------------------------------------------------
tcf_targets <- expand.grid(
  TCF1 = c(0.6, 0.7, 0.8, 0.9),
  TCF3 = c(0.6, 0.7, 0.8, 0.9)
)

tcf2_grid <- do.call(rbind, lapply(names(marker_formulas), function(m) {
  do.call(rbind, lapply(seq_len(nrow(tcf_targets)), function(i) {
    fit <- tcf2(
      marker_formulas[[m]], data = ad, diag_levels = group_order,
      tcf10 = tcf_targets$TCF1[i], tcf30 = tcf_targets$TCF3[i],
      ci_level = 0.95, B = 500, seed = 34, plot = FALSE
    )
    stopifnot(fit$tau_est[1] < fit$tau_est[2])
    data.frame(
      Marker = m, TCF1 = fit$tcf1, TCF3 = fit$tcf3,
      t1 = fit$tau_est[1], t2 = fit$tau_est[2], TCF2 = fit$estimate,
      Lower = fit$conf.int[1], Upper = fit$conf.int[2]
    )
  }))
}))

## ----tcf2-grid-plot, fig.width=8, fig.height=3.5, fig.cap="Estimated TCF2 for each pair of target TCF1 and TCF3 values. All panels use the same colour scale."----
ggplot2::ggplot(tcf2_grid, ggplot2::aes(x = TCF1, y = TCF3, fill = TCF2)) +
  ggplot2::geom_tile() +
  ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", TCF2)), size = 3) +
  ggplot2::facet_wrap(~ Marker, nrow = 1) +
  ggplot2::scale_x_continuous(breaks = sort(unique(tcf_targets$TCF1))) +
  ggplot2::scale_y_continuous(breaks = sort(unique(tcf_targets$TCF3))) +
  ggplot2::scale_fill_gradient(low = "white", high = "steelblue", limits = c(0, 1)) +
  ggplot2::coord_equal() +
  ggplot2::labs(x = "Target TCF1 (D-)", y = "Target TCF3 (D+)", fill = "TCF2") +
  ggplot2::theme_bw()

## ----tcf2-grid-ci-plot, fig.width=8, fig.height=6, fig.cap="TCF2 estimates and pointwise 95% EL confidence intervals for all 16 target pairs, shown as (TCF1, TCF3)."----
pair_labels <- with(tcf_targets, sprintf("(%.1f, %.1f)", TCF1, TCF3))
tcf2_ci_data <- transform(
  tcf2_grid,
  Target_pair = factor(
    sprintf("(%.1f, %.1f)", TCF1, TCF3), levels = rev(pair_labels)
  )
)

tcf2_ci_plot <- ggplot2::ggplot(
  tcf2_ci_data, ggplot2::aes(x = TCF2, y = Target_pair)
) +
  ggplot2::geom_errorbar(
    ggplot2::aes(xmin = Lower, xmax = Upper),
    orientation = "y", width = 0.25, colour = "steelblue"
  ) +
  ggplot2::geom_point(size = 1.8, colour = "steelblue") +
  ggplot2::facet_wrap(~ Marker, nrow = 1) +
  ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  ggplot2::labs(x = "TCF2 with 95% EL confidence interval",
                y = "Target pair (TCF1, TCF3)") +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank())
tcf2_ci_plot

