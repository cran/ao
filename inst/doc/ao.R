## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----himmelblau---------------------------------------------------------------
himmelblau <- function(x) (x[1]^2+x[2]-11)^2+(x[1]+x[2]^2-7)^2

## ----set_f--------------------------------------------------------------------
f <- ao::set_f(f = himmelblau, npar = 2, lower = -5, upper = 5, check = TRUE)

## ----ao-----------------------------------------------------------------------
ao::ao(f = f, partition = list(1, 2), initial = 0, iterations = 10, 
       tolerance = 1e-6, minimize = TRUE, progress = FALSE, plot = FALSE)

