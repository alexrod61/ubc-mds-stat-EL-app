options(repr.matrix.max.rows = 6)
source("scripts/support_functions.R")
library(tidyverse)
library(mlbench)
library(AER)
library(cowplot)
library(broom)
library(performance)
library(qqplotr)

options(repr.plot.height = 9, repr.plot.width = 20)
plot_grid(example_1(), example_2(), example_3(), nrow = 1)

options(repr.plot.height = 9, repr.plot.width = 15)
example_3()

example_non_normality()

example_heteroscedasticity()

data(BostonHousing)
str(BostonHousing)

data(BreastCancer)
str(BreastCancer)

data(NMES1988)
str(NMES1988)

options(repr.plot.height = 9, repr.plot.width = 20)
plot_grid(example_deterministic_relation(), example_stochastic_relation(), nrow = 1)

set.seed(123)
sin_data <- tibble(X = seq(2, 13.65, 0.05), Y = 5 + 10 * sin(X) + rnorm(length(seq(2, 13.65, 0.05)), 0, 1.5))
suppressMessages(print(sin_data %>% ggplot() +
  geom_point(aes(X, Y)) +
  geom_smooth(aes(X, Y), method = "lm", se = FALSE, color = "red") +
  ggtitle(bquote("Linear regression of " ~ Y[i] == beta[0] + beta[1] * X[i] + epsilon[i])) +
  theme(
      plot.title = element_text(size = 30),
      axis.text = element_text(size = 21),
      axis.title = element_text(size = 27)
    )))

sin_function_model <- lm(Y ~ X, data = sin_data)
tidy(sin_function_model) %>% mutate_if(is.numeric, round, 3)
glance(sin_function_model) %>% mutate_if(is.numeric, round, 3)

breast_cancer <- suppressWarnings(suppressMessages(read_csv("datasets/breast_cancer.csv")))

breast_cancer_binary <- breast_cancer %>%
  dplyr::select(mean_radius, target)
breast_cancer_binary

breast_cancer_binary <- breast_cancer_binary %>% 
  mutate(target = if_else(target == "malignant", 1, 0))
breast_cancer_binary

breast_cancer_plot <- breast_cancer_binary %>%
  ggplot() +
  geom_point(aes(mean_radius, target)) +
  geom_smooth(aes(mean_radius, target),
    method = "lm", formula = y ~ x, se = FALSE
  ) +
  labs(y = "Prob. of a Malignant Tumour", x = "Mean Radius") +
  ggtitle("OLS Fitted Regression Line") +
  theme(
    plot.title = element_text(size = 31, face = "bold"),
    axis.text = element_text(size = 21),
    axis.title = element_text(size = 27)
  ) 
breast_cancer_plot

breast_cancer_plot <- breast_cancer_plot +
  geom_smooth(aes(mean_radius, target),
    method = "glm", formula = y ~ x,
    method.args = c(family = binomial), se = FALSE, color = "red"
  ) +
  ggtitle("OLS (Blue) and Binary Logistic (Red) Fitted Regression Lines")
breast_cancer_plot

binary_log_model <- glm(as.factor(target) ~ mean_radius,
  data = breast_cancer_binary, family = binomial
)

tidy(binary_log_model, conf.int = TRUE) %>% mutate_if(is.numeric, round, 3)

tidy(binary_log_model, conf.int = TRUE, exponentiate = TRUE) %>% mutate_if(is.numeric, round, 2)

breast_cancer_binary_2 <- breast_cancer %>%
  dplyr::select(mean_radius, mean_texture, target)
breast_cancer_binary_2

binary_log_model_2 <- glm(as.factor(target) ~ mean_radius + mean_texture,
  data = breast_cancer_binary_2, family = binomial)
tidy(binary_log_model_2, conf.int = TRUE) %>% mutate_if(is.numeric, round, 3)

tidy(binary_log_model_2, conf.int = TRUE, exponentiate = TRUE) %>% mutate_if(is.numeric, round, 2)

round(exp(predict(binary_log_model_2,
  tibble(mean_radius = 16, mean_texture = 20),
  type = "link"
)), 2)

round(predict(binary_log_model_2,
  tibble(mean_radius = 16, mean_texture = 20),
  type = "response"
), 2)

glance(binary_log_model_2) %>% mutate_if(is.numeric, round, 3)

binary_log_model_dev_residuals <- data.frame(dev_residuals = residuals(binary_log_model, type = "deviance"))
binary_log_model_dev_residuals

binary_log_model_2_dev_residuals <- data.frame(dev_residuals = residuals(binary_log_model_2, type = "deviance"))
binary_log_model_2_dev_residuals

qqplot_dev_residuals(
  data = binary_log_model_dev_residuals,
  title = "Q-Q Plot for Binary Logistic Model with Mean Radius"
)

qqplot_dev_residuals(
  data = binary_log_model_2_dev_residuals,
  title = "Q-Q Plot for Binary Logistic Model with Mean Radius and Mean Texture"
)

plot(binary_log_model_2, 1, cex.lab = 1.5, cex.axis = 1.5)

diagnostic_bins <- binned_residuals(binary_log_model_2)
diagnostic_bins

plot(diagnostic_bins) +
  theme(
    plot.title = element_text(size = 30, face = "bold"),
    plot.subtitle = element_text(size = 23), 
    axis.text = element_text(size = 21),
    axis.title = element_text(size = 27),
    legend.text = element_text(size = 21),
    legend.title = element_text(size = 21, face = "bold")
  )
