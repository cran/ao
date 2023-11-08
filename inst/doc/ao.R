## ---- setup, include = FALSE--------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "ao-"
)

## ---- ao demo-----------------------------------------------------------------
library("ao")
himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
ao(
  f = himmelblau, p = c(0, 0), partition = list(1, 2),
  base_optimizer = optimizeR::Optimizer$new(
    which = "stats::optim", lower = -5, upper = 5, method = "L-BFGS-B"
  )
)

