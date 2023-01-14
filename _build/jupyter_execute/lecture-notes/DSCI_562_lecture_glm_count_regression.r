options(repr.matrix.max.rows = 6)
source("scripts/support_functions.R")
library(tidyverse)
library(broom)
library(glmbb)
library(AER)

breast_cancer <- suppressWarnings(suppressMessages(read_csv("datasets/breast_cancer.csv")))

breast_cancer <- breast_cancer %>%
  dplyr::select(mean_radius, mean_texture, target)
breast_cancer

breast_cancer <- breast_cancer %>%
  mutate(target = as.factor(target))

levels(breast_cancer$target)

binary_log_model_1 <- glm(as.factor(target) ~ mean_radius,
  data = breast_cancer, family = binomial
)

binary_log_model_2 <- glm(as.factor(target) ~ mean_radius + mean_texture,
  data = breast_cancer, family = binomial
)

round(anova(binary_log_model_1,
  binary_log_model_2,
  test = "Chi"
), 4)

glance(binary_log_model_1) %>% mutate_if(is.numeric, round, 3)

glance(binary_log_model_2) %>% mutate_if(is.numeric, round, 3)

data(crabs)
crabs <- crabs %>%
  rename(n_males = satell) %>%
  dplyr::select(-y)
crabs

options(repr.plot.height = 9, repr.plot.width = 20)
plot_crabs_vs_width <- crabs %>%
  ggplot() +
  geom_point(aes(width, n_males)) +
  labs(y = "Number of Male Crabs", x = "Carapace Width (mm)") +
  ggtitle("Scatterplot of Number of Male Crabs Versus Carapace Width") +
  theme(
    plot.title = element_text(size = 31, face = "bold"),
    axis.text = element_text(size = 21),
    axis.title = element_text(size = 27)
  ) +
  scale_x_continuous(limits = c(20, 35), breaks = seq(0, 35, by = 2.5))
plot_crabs_vs_width

group_avg_width <- crabs %>% 
  mutate(intervals = cut(crabs$width, breaks=10)) %>% 
  group_by(intervals) %>% 
  summarise(mean = mean(n_males), n = n()) 
group_avg_width

crabs_avg_width_plot <- group_avg_width %>%
  ggplot() +
  geom_point(aes(intervals, mean), colour = "red", size = 4) +
  labs(y = "Mean Number of Male Crabs", x = "Carapace Width Interval (mm)") +
  ggtitle("Mean Number of Male Crabs Versus Carapace Width by Interval") +
  theme(
    plot.title = element_text(size = 31, face = "bold"),
    axis.text = element_text(size = 21),
    axis.title = element_text(size = 27)
  )
crabs_avg_width_plot

crabs_avg_width_bar_chart <- crabs %>%
  ggplot() +
  geom_bar(aes(as.factor(n_males)), fill = "grey", color = "black") +
  theme(
    plot.title = element_text(size = 31, face = "bold"),
    axis.text = element_text(size = 21),
    axis.title = element_text(size = 27)
  ) +
  ggtitle("Bar Chart of Counts by Numbers of Male Crabs") +
  labs(x = "Number of Male Crabs", y = "Count")
crabs_avg_width_bar_chart

poisson_model_1 <- glm(n_males ~ width, family = poisson, data = crabs)

tidy(poisson_model_1, conf.int = TRUE) %>% mutate_if(is.numeric, round, 3)

plot_crabs_vs_width <- plot_crabs_vs_width +
  geom_smooth(
    data = crabs, aes(width, n_males),
    method = "glm", formula = y ~ x,
    method.args = list(family = poisson), se = FALSE
  ) +
  ggtitle("Poisson Regression")
plot_crabs_vs_width

levels(crabs$color)

poisson_model_2 <- glm(n_males ~ width + color, family = poisson, data = crabs)
tidy(poisson_model_2, conf.int = TRUE) %>% mutate_if(is.numeric, round, 3)

tidy(poisson_model_2, exponentiate = TRUE, conf.int = TRUE) %>% mutate_if(is.numeric, round, 2)

tidy(poisson_model_2, exponentiate = TRUE, conf.int = TRUE) %>% mutate_if(is.numeric, round, 2)

round(predict(poisson_model_2, tibble(width = 27.5, color = "light"),
  type = "response"
), 2)

summary_poisson_model_1 <- glance(poisson_model_1)
summary_poisson_model_1

pchisq(summary_poisson_model_1$deviance,
  df = summary_poisson_model_1$df.residual,
  lower.tail = FALSE
)

round(anova(poisson_model_1,
  poisson_model_2,
  test = "Chi"
), 4)

glance(poisson_model_1) %>% mutate_if(is.numeric, round, 3)

glance(poisson_model_2) %>% mutate_if(is.numeric, round, 3)

set.seed(562)

poisson_samples <- tibble(x = -1, lambda = 0)

for (lambda in seq(1, 91, 10)) {
  sample <- rpois(1000, lambda)
  poisson_samples <- poisson_samples %>% bind_rows(tibble(
    x = sample,
    lambda = lambda
  ))
}

poisson_samples <- poisson_samples %>% filter(x != -1)

poisson_jitter_plots <- poisson_samples %>%
  ggplot(aes(lambda, x)) +
  geom_jitter(alpha = .2, width = 2.5) +
  theme(
    plot.title = element_text(size = 31, face = "bold"),
    axis.text = element_text(size = 21),
    axis.title = element_text(size = 27)
  ) +
  coord_cartesian(ylim = c(0, 150)) +
  labs(y = "Observed Value") +
  ggtitle("Side-by-Side Jitter Plots") +
  scale_x_continuous(breaks = seq(1, 91, 10)) + 
  labs(x = expression(lambda))
poisson_jitter_plots

poisson_boxplots <- poisson_samples %>%
  ggplot(aes(x = as_factor(lambda), y = x)) + 
  geom_boxplot() +
  theme(
    plot.title = element_text(size = 31, face = "bold"),
    axis.text = element_text(size = 21),
    axis.title = element_text(size = 27)
  ) +
  coord_cartesian(ylim = c(0, 150)) +
  labs(y = "Observed Value", x = "lambda") +
  ggtitle("Side-by-Side Boxplots") + 
  labs(x = expression(lambda))
poisson_boxplots

suppressWarnings(suppressMessages(print(poisson_samples %>% group_by(lambda) %>%
  summarise(sample_variance = var(x)) %>%
  ggplot() +
  geom_line(aes(lambda, sample_variance)) +
  geom_point(aes(lambda, sample_variance), size = 4) +
  theme(
    plot.title = element_text(size = 31, face = "bold"),
    axis.text = element_text(size = 21),
    axis.title = element_text(size = 27)
  ) +
  labs(y = "Sample Variance", x = expression(lambda)) +
  ggtitle("Sample Variance versus Lambda") +
  scale_x_continuous(breaks = seq(1, 91, 10)))))

summary(poisson_model_1)

dispersiontest(poisson_model_1)

quasi_poisson_model <- glm(n_males ~ width, family = quasipoisson, data = crabs)
summary(quasi_poisson_model)

summary(poisson_model_1)

library(MASS)

negative_binomial_model <- glm.nb(n_males ~ width, data = crabs)
summary(negative_binomial_model)
