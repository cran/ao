## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "ao-"
)
library("ao")
set.seed(2)

## ----ao call, eval = FALSE----------------------------------------------------
#  ao(
#    f,
#    initial,
#    target = NULL,
#    npar = NULL,
#    gradient = NULL,
#    ...,
#    partition = "sequential",
#    new_block_probability = 0.5,
#    minimum_block_number = 2,
#    minimize = TRUE,
#    lower = -Inf,
#    upper = Inf,
#    iteration_limit = Inf,
#    seconds_limit = Inf,
#    tolerance_value = 1e-6,
#    tolerance_parameter = 1e-6,
#    tolerance_parameter_norm = function(x, y) sqrt(sum((x - y)^2)),
#    base_optimizer = Optimizer$new("stats::optim", method = "L-BFGS-B"),
#    verbose = FALSE
#  )

## ----himmelblau---------------------------------------------------------------
himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2

## ----himmelblau evaluate------------------------------------------------------
himmelblau(c(3, 2))

## ----visualize_himmelblau, echo = FALSE, message = FALSE, warning = FALSE, fig.align = 'center', out.width = "80%", fig.dim = c(8, 6)----
library("ggplot2")
x <- y <- seq(-5, 5, 0.1)
grid <- expand.grid(x, y)
grid$z <- apply(grid, 1, himmelblau)
ggplot(grid, aes(x = Var1, y = Var2, z = z)) +
  geom_raster(aes(fill = z)) +
  geom_contour(colour = "white", bins = 40) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_linedraw() +
  labs(
    x = "x",
    y = "y",
    fill = "value",
    title = "Himmelblau function",
    subtitle = "the four local minima are marked in green"
  ) +
  coord_fixed() +
  annotate(
    "Text",
    x = c(3, -2.8, -3.78, 3.58),
    y = c(2, 3.13, -3.28, -1.85),
    label = "X", size = 6, color = "green"
  )

## ----ao himmelblau------------------------------------------------------------
ao(f = himmelblau, initial = c(0, 0))

## ----himmelblau gradient------------------------------------------------------
gradient <- function(x) {
  c(
    4 * x[1] * (x[1]^2 + x[2] - 11) + 2 * (x[1] + x[2]^2 - 7),
    2 * (x[1]^2 + x[2] - 11) + 4 * x[2] * (x[1] + x[2]^2 - 7)
  )
}

## ----ao himmelblau with gradient, eval = FALSE--------------------------------
#  ao(f = himmelblau, initial = c(0, 0), gradient = gradient)

## ----partition demo-----------------------------------------------------------
procedure <- ao:::Procedure$new(
  npar = 10,
  partition = "random",
  new_block_probability = 0.5,
  minimum_block_number = 2
)
procedure$get_partition()
procedure$get_partition()

## ----visualize_faithful, echo = FALSE, message = FALSE, warning = FALSE, fig.align = 'center', out.width = "80%", fig.dim = c(8, 6)----
library("ggplot2")
ggplot(datasets::faithful, aes(x = eruptions)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30) +
  xlab("eruption time (min)")

## ----normal mixture-----------------------------------------------------------
normal_mixture_llk <- function(theta, data) {
  mu <- theta[1:2]
  sd <- exp(theta[3:4])
  lambda <- plogis(theta[5])
  c1 <- lambda * dnorm(data, mu[1], sd[1])
  c2 <- (1 - lambda) * dnorm(data, mu[2], sd[2])
  sum(log(c1 + c2))
}

## ----ao normal mixture--------------------------------------------------------
out <- ao(
  f = normal_mixture_llk,
  initial = runif(5),
  data = datasets::faithful$eruptions,
  partition = "random",
  minimize = FALSE
)
round(out$details)

## ----normal mixture type 2----------------------------------------------------
normal_mixture_llk <- function(data, mu, sd, lambda) {
  sd <- exp(sd)
  lambda <- plogis(lambda)
  c1 <- lambda * dnorm(data, mu[1], sd[1])
  c2 <- (1 - lambda) * dnorm(data, mu[2], sd[2])
  sum(log(c1 + c2))
}

## ----ao normal mixture type 2, results = "hide"-------------------------------
ao(
  f = normal_mixture_llk,
  initial = runif(5),
  target = c("mu", "sd", "lambda"),
  npar = c(2, 2, 1),
  data = datasets::faithful$eruptions,
  partition = "random",
  minimize = FALSE
)

## ----normal mixture type 3, results = "hide"----------------------------------
normal_mixture_llk <- function(mu, sd, lambda, data) {
  c1 <- lambda * dnorm(data, mu[1], sd[1])
  c2 <- (1 - lambda) * dnorm(data, mu[2], sd[2])
  sum(log(c1 + c2))
}
ao(
  f = normal_mixture_llk,
  initial = runif(5),
  target = c("mu", "sd", "lambda"),
  npar = c(2, 2, 1),
  data = datasets::faithful$eruptions,
  partition = "random",
  minimize = FALSE,
  lower = c(-Inf, -Inf, 0, 0, 0),
  upper = c(Inf, Inf, Inf, Inf, 1)
)

