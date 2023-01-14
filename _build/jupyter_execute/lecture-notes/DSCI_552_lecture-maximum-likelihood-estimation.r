options(repr.plot.height = 8, repr.plot.width = 20)
options(repr.matrix.max.rows = 6)
source("scripts/support_functions.R")
library(tidyverse)
library(gridExtra)

sample_n30 <- tibble(values = c(
  24.9458614574341, 7.23174970992907, 4.16136401519179, 5.60304128237143,
  5.37929488345981, 1.40547217217847, 7.0701988485075, 2.84055356831115,
  0.894746121019125, 2.9016381111011, 3.19011222943664, 11.0930137682099,
  3.49700326472521, 46.2914818498428, 2.00653892990149, 2.87363994969391,
  11.4050390862658, 11.6616687767937, 12.8855835341646, 3.88483320176601,
  0.406148910522461, 25.7642258988289, 8.4743227359272, 4.17410666868091,
  1.84968510270119, 2.15972620035141, 10.5289600339151, 6.44162824716339,
  10.6035323139645, 66.6861112673485
))

hist_sample_n30 <- sample_n30 %>%
    ggplot() +
    geom_histogram(aes(values, ..density..), fill = "grey", color = "black", bins = 50) +
    theme(
      plot.title = element_text(size = 24, face = "bold"),
      axis.text = element_text(size = 18),
      axis.title = element_text(size = 24)
    ) +
    ggtitle("Histogram of a Continuous Random Sample of n = 30") +
    scale_colour_discrete(name = expression(paste("Rate (", lambda, ")"))) +
    labs(x = "Observed Value", y = "Density")

hist_sample_n30

(2 * exp(-2 * 0.8)) * (2 * exp(-2 * 2.1)) * (2 * exp(-2 * 2.4))

data <- c(0.8, 2.1, 2.4)
prod(dexp(data, 2))

prod(dexp(data, 0.5))
prod(dexp(data, 0.05))

sample_n30

likelihood_0.05 <- prod(dexp(sample_n30$values, 0.05))
likelihood_0.05

density_0.05 <- tibble(
  x = seq(0, 70, 0.05),
  density = dexp(x, 0.05)
)
density_0.05

plot_lambda_0.05 <- hist_sample_n30 +
  geom_line(
    data = density_0.05,
    aes(x = x, y = density), colour = "red"
  ) +
  annotate("text", x = 45, y = 0.1, label = paste0("l = ", likelihood_0.05), size = 10) +
  ggtitle(expression(paste(lambda, "= 0.05")))

plot_lambda_0.05

likelihood_0.125 <- prod(dexp(sample_n30$values, 0.125))
likelihood_0.125

density_0.125 <- tibble(
  x = seq(0, 70, 0.125),
  density = dexp(x, 0.125)
)

likelihood_0.5 <- prod(dexp(sample_n30$values, 0.5))
likelihood_0.5

density_0.5 <- tibble(
  x = seq(0, 70, 0.5),
  density = dexp(x, 0.5)
)

plot_lambda_0.125 <- hist_sample_n30 +
  geom_line(
    data = density_0.125,
    aes(x = x, y = density), colour = "red"
  ) +
  annotate("text", x = 45, y = 0.1, label = paste0("l = ", likelihood_0.125), size = 10) +
  ggtitle(expression(paste(lambda, "= 0.125"))) + ylim(c(0, 0.5))

plot_lambda_0.5 <- hist_sample_n30 +
  geom_line(
    data = density_0.5,
    aes(x = x, y = density), colour = "red"
  ) +
  annotate("text", x = 45, y = 0.1, label = paste0("l = ", likelihood_0.5), size = 10) +
  ggtitle(expression(paste(lambda, "= 0.5")))

options(repr.plot.width = 16, repr.plot.height = 14)
grid.arrange(plot_lambda_0.05 + ylim(c(0, 0.5)), plot_lambda_0.125, plot_lambda_0.5, nrow = 3)

likelihood_0.05
likelihood_0.125
likelihood_0.5

round(log(likelihood_0.05), 4)
round(log(likelihood_0.125), 4)
round(log(likelihood_0.5), 4)

exp_values <- tibble(
  possible_lambdas = seq(0.01, 0.2, 0.001),
  likelihood = map_dbl(possible_lambdas, ~ prod(dexp(sample_n30$values, .))),
  log_likelihood = map_dbl(possible_lambdas, ~ log(prod(dexp(sample_n30$values, .))))
)
exp_values

options(repr.plot.height = 8, repr.plot.width = 20)

(exp_like_plot <- ggplot(exp_values, aes(x = possible_lambdas, y = likelihood)) +
  geom_line() +
  theme(
    plot.title = element_text(size = 26, face = "bold"),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 16)
  ) +
  ggtitle("Likelihood Values") +
  labs(x = expression(lambda), y = "Likelihood"))

(exp_log_like_plot <- ggplot(exp_values, aes(x = possible_lambdas, y = log_likelihood)) +
  geom_line() +
  theme(
    plot.title = element_text(size = 26, face = "bold"),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 26)
  ) +
  ggtitle("Likelihood Values") +
  labs(x = expression(lambda), y = "Log-likelihood"))

empirical_MLE <- exp_values %>%
  arrange(desc(likelihood)) %>%
  slice(1)
empirical_MLE

analytical_MLE <- 1 / sample_n30 %>%
  pull(values) %>%
  mean()
round(analytical_MLE, 4)

exp_like_plot +
  geom_vline(xintercept = analytical_MLE, colour = "red")

exp_log_like_plot +
  geom_vline(xintercept = analytical_MLE, colour = "red")

empirical_MLE

round(analytical_MLE, 4)

suppressWarnings(suppressMessages(print(prob_vs_likelihood())))

LL <- function(l) log(prod(dexp(sample_n30$values, l)))
optimize(LL, c(0.01, 0.2), maximum = TRUE)
