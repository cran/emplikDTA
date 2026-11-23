## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 6,
                      fig.height = 5, fig.align = "center")

## ----data---------------------------------------------------------------------
library(emplikDTA)
data("antib2", package = "emplikDTA")

table(data_antib2$status)
group_order <- c("0", "1")

# Use thousands of AU/mL to improve numerical scaling for threshold optimization.
antib <- transform(data_antib2, titer_kAU = antib2 / 1000)

## ----roc----------------------------------------------------------------------
roc_fit <- ROC_emp_curve(
  titer_kAU ~ status, data = antib, diag_levels = group_order,
  ncp = 1000, plot = FALSE
)

roc_plot <- plot(roc_fit) +
  ggplot2::labs(title = "Empirical ROC curve for IgG titers")
roc_plot

## ----auc----------------------------------------------------------------------
auc_fit <- auc(
  titer_kAU ~ status, data = antib, diag_levels = group_order,
  auc0 = 0.5, ci_level = 0.95, B = 500, seed = 34, alternative = "greater"
)
print(auc_fit)

## ----thresholds---------------------------------------------------------------
opt_fit <- threshold2(
  titer_kAU ~ status, data = antib, diag_levels = group_order,
  method = "all", kernel_type = "gauss", bwd_type = "CV",
  ci = TRUE, ci_level = 0.95, B = 1000, seed = 34
)

thresholds <- opt_fit$summary
knitr::kable(
  data.frame(
    Criterion = thresholds$method,
    Threshold = 1000 * thresholds$opt_thresh,
    Lower = 1000 * thresholds$ci_tau_lower,
    Upper = 1000 * thresholds$ci_tau_upper,
    Specificity = thresholds$spe_opt,
    Sensitivity = thresholds$sen_opt
  ),
  digits = c(0, 1, 1, 1, 3, 3),
  caption = "Thresholds and 95% bootstrap intervals in AU/mL, with kernel estimates of specificity and sensitivity."
)

## ----operating-points---------------------------------------------------------
operating_points <- do.call(rbind, lapply(opt_fit$results, function(fit) {
  data.frame(
    method = fit$method,
    FPR = 1 - unname(fit$spe_sen_emp["sp"]),
    TPR = unname(fit$spe_sen_emp["se"]),
    EL_cutoff = fit$c_alp
  )
}))

knitr::kable(
  operating_points, row.names = FALSE, digits = 3,
  col.names = c("Criterion", "Empirical FPR", "Empirical sensitivity", "EL cutoff")
)

## ----roc-regions, fig.width=9, fig.height=3.8---------------------------------
criteria <- names(opt_fit$results)
operating_points$method <- factor(operating_points$method, levels = criteria)

el_grid <- do.call(rbind, lapply(opt_fit$results, function(fit) {
  grid <- subset(fit$grid_sen_spe, is.finite(z))
  transform(grid, method = factor(fit$method, levels = criteria),
            centered_el = z - fit$c_alp)
}))

roc_plot +
  ggplot2::geom_contour(
    data = el_grid,
    mapping = ggplot2::aes(x = 1 - Spe, y = Sen, z = centered_el),
    breaks = 0, inherit.aes = FALSE, colour = "firebrick", linewidth = 0.9
  ) +
  ggplot2::geom_point(
    data = operating_points, mapping = ggplot2::aes(x = FPR, y = TPR),
    inherit.aes = FALSE, colour = "firebrick", size = 2.5
  ) +
  ggplot2::facet_wrap(ggplot2::vars(method), nrow = 1) +
  ggplot2::theme(panel.spacing.x = grid::unit(1.5, "lines")) +
  ggplot2::labs(subtitle = "Empirical operating points and 95% joint EL regions")

