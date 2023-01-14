# MLE Example

example_mle <- function() {
  set.seed(123)
  exp_sample <- tibble(observation = rexp(n = 300, rate = 1))
  plot <- exp_sample %>%
    ggplot() +
    geom_histogram(aes(observation, ..density..), fill = "grey", color = "black", bins = 50) +
    stat_function(fun = dexp, args = (rate <- 1), aes(colour = "1")) +
    stat_function(fun = dexp, args = (rate <- 1.5), aes(colour = "1.5")) +
    stat_function(fun = dexp, args = (rate <- 2), aes(colour = "2")) +
    theme(
      plot.title = element_text(size = 21, face = "bold"),
      axis.text = element_text(size = 15),
      axis.title = element_text(size = 21),
      legend.text = element_text(size = 21),
      legend.title = element_text(size = 18, face = "bold")
    ) +
    ggtitle("Histogram and Densities of an Exponential Distribution") +
    scale_colour_discrete(name = expression(paste("Rate (", lambda, ")"))) +
    labs(x = "Observed Value", y = "Density")
  return(plot)
}

# Probability versus likelihood

prob_vs_likelihood <- function() {
  data <- tibble(
    quantile = seq(-3, 3, 0.01), density = dnorm(quantile)
  )
  plot <- data %>%
    ggplot() +
    geom_line(aes(x = quantile, y = density), colour = "red") +
    geom_vline(xintercept = qnorm(0.9), linetype = "dashed", colour = "blue") +
    geom_hline(yintercept = 0) +
    geom_segment(aes(x = quantile, y = 0, xend = quantile, yend = dnorm(quantile)),
      color = "blue",
      data = data %>% filter(quantile >= qnorm(0.9)), alpha = 0.3
    ) +
    geom_hline(yintercept = dnorm(qnorm(0.9)), linetype = "dashed", colour = "red") +
    annotate("text",
      x = 2.6, y = 0.08, label = paste("Pr(X > 1.28)", "=", 0.10),
      color = "darkblue", size = 10
    ) +
    annotate("text",
      x = -2.1, y = 0.2, label = expression(paste(f[X], "(x = 1.28 |", mu, ", ", sigma^2, ") = 0.18")),
      color = "red", size = 10
    ) +
    theme(
      axis.text = element_text(size = 23),
      axis.title = element_text(size = 29),
    ) +
    scale_x_continuous(breaks = seq(-3, 3, 0.5)) +
    xlab("x") +
    ylab(expression(paste(f[X], "(x |", mu, ", ", sigma^2, ")")))
  return(plot)
}

# Three initial examples of linear models

example_1 <- function() {
  set.seed(123)
  data <- tibble(x = rnorm(50, 10, 3), y = 2 * x + rnorm(length(x)))
  plot <- data %>% ggplot() +
    geom_point(aes(x, y)) +
    ggtitle("Example 1") +
    theme(
      plot.title = element_text(size = 30, face = "bold"),
      axis.text = element_text(size = 21),
      axis.title = element_text(size = 27)
    )
  return(plot)
}

example_2 <- function() {
  set.seed(456)
  data <- tibble(x = rnorm(50, 10, 3), y = 5 * (x > 10) + rnorm(length(x), 0, 0.24))
  plot <- data %>% ggplot() +
    geom_point(aes(x, y)) +
    ggtitle("Example 2") +
    theme(
      plot.title = element_text(size = 30, face = "bold"),
      axis.text = element_text(size = 21),
      axis.title = element_text(size = 27)
    )
  return(plot)
}

example_3 <- function() {
  set.seed(789)
  data <- tibble(x = rnorm(100, 10, 3), y = 2 * sin(x) + rnorm(length(x), 0, 0.4))
  plot <- data %>% ggplot() +
    geom_point(aes(x, y)) +
    ggtitle("Example 3") +
    theme(
      plot.title = element_text(size = 30, face = "bold"),
      axis.text = element_text(size = 21),
      axis.title = element_text(size = 27)
    )
  return(plot)
}

# Example of violation of normality on residuals

example_non_normality <- function() {
  set.seed(123)
  data <- tibble(x = rchisq(100, 10), y = 3 * x + rchisq(length(x), 10))
  model <- lm(y ~ x, data)
  par(mfrow = c(1, 2))
  plot(model, 2)
  hist(residuals(object = model), breaks = 10,
    main = "Histogram of Residuals",
    xlab = "Residuals"
  )
}

# Example of heteroscedasticity

example_heteroscedasticity <- function() {
  set.seed(123)
  data <- tibble(
    x = rnorm(500), residual = rnorm(500, 0, x^2),
    y = 3 * x + residual
  )
  model <- lm(y ~ x, data)
  plot(model, 1)
}

# Example of deterministic relation

example_deterministic_relation <- function() {
  data <- tibble(x = seq(0, 5, 0.1), y = 2 * x)
  plot <- data %>% ggplot() +
    geom_line(aes(x, y)) +
    ggtitle("Deterministic Relation of Y = 2X") +
    xlab("X") +
    ylab("Y") +
    theme(
      plot.title = element_text(size = 30, face = "bold"),
      axis.text = element_text(size = 21),
      axis.title = element_text(size = 27)
    )
  return(plot)
}

# Example of stochastic relation

example_stochastic_relation <- function() {
  prob <- tibble(
    x = seq(0, 4, 0.01),
    y = 2 * x,
    y1 = 2 + dnorm(x, mean = 1, sd = 0.25),
    y2 = 6 + dnorm(x, mean = 3, sd = 0.25),
    y3 = 4 + dnorm(x, mean = 2, sd = 0.25)
  )
  plot <- prob %>%
    ggplot() +
    geom_line(aes(x, y)) +
    ggtitle("Stochastic Relation") +
    geom_line(aes(x, y1), color = "red") +
    geom_line(aes(x, y2), color = "red") +
    geom_line(aes(x, y3), color = "red") +
    geom_segment(aes(x = 1, y = 2, xend = 1, yend = 3.6), color = "red", linetype = "dotted") +
    geom_segment(aes(x = 2, y = 4, xend = 2, yend = 5.6), color = "red", linetype = "dotted") +
    geom_segment(aes(x = 3, y = 6, xend = 3, yend = 7.6), color = "red", linetype = "dotted") +
    coord_flip() +
    scale_y_continuous(
      name = "X",
      breaks = c(2, 4, 6),
      labels = c("1", "2", "3")
    ) +
    scale_x_continuous(
      name = "Y",
      breaks = c(1, 2, 3),
      labels = c("2", "4", "6")
    ) +
    theme(
      plot.title = element_text(size = 30, face = "bold"),
      axis.text = element_text(size = 21),
      axis.title = element_text(size = 27)
    )
  return(plot)
}

# Q-Q plot

qqplot_dev_residuals <- function(data, title) {
  suppressWarnings(print(ggplot(binary_log_model_dev_residuals,
    mapping = aes(sample = dev_residuals)
  ) +
    stat_qq_band() +
    stat_qq_line() +
    stat_qq_point() +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
    ggtitle(title) +
    theme(
      plot.title = element_text(size = 30, face = "bold"),
      axis.text = element_text(size = 21),
      axis.title = element_text(size = 27),
      legend.text = element_text(size = 21),
      legend.title = element_text(size = 21, face = "bold")
    )))
}