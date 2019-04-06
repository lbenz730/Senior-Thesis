### Figure 2.1
library(dplyr)
library(ggplot2)
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

z <- read.csv("model_coefficients/model_0_coeffs.csv", as.is = T) 
ggplot(z, aes(x = 2400 - max_time, y = estimate, group = coefficient)) +
  facet_wrap(~coefficient, ncol = 2, scales = "free") +
  geom_point(aes(col = (p_value < 0.05))) +
  geom_smooth(data = filter(z, coefficient == "score_diff"), 
              method = "loess", formula = "y~x", span = 0.1) +
  geom_smooth(data = filter(z, coefficient != "score_diff"), 
              method = "loess", formula = "y~x", span = 0.4) +
  geom_hline(yintercept = 0, lty = 2) +
  theme_bw() +
  theme(legend.position  = "bottom",
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14)) +
  labs(x = "Seconds Remaining",
       y = "Coefficient Estimate",
       title = "Win Probability Model Coefficients Over Time",
       color = "Statistically Significant",
       subtitle = "Model 0") +
  scale_x_continuous(breaks = seq(0, 2400, 400), limits = c(0, 2400)) +
  scale_color_manual(values = gg_color_hue(2)[2])

### Figure 2.2
z <- read.csv("model_coefficients/model_1_coeffs.csv", as.is = T) 
ggplot(z, aes(x = 2400 - max_time, y = estimate, group = coefficient)) +
  facet_wrap(~coefficient, ncol = 2, scales = "free") +
  geom_point(aes(col = (p_value < 0.05))) +
  geom_smooth(data = filter(z, coefficient == "favored_by"), 
              method = "loess", formula = "y~x", span = 0.4) +
  geom_smooth(data = filter(z, coefficient != "favored_by"), 
              method = "loess", formula = "y~x", span = 0.1) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(yintercept = 0, lty = 2) +
  theme_bw() +
  theme(legend.position  = "bottom",
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14)) +
  labs(x = "Seconds Elapsed",
       y = "Coefficient Estimate",
       title = "Win Probability Model Coefficients Over Time",
       color = "Statistically Significant",
       subtitle = "Model 1") +
  scale_x_continuous(breaks = seq(0, 2400, 400), limits = c(0, 2400))

### Figure 2.3
results <- read.csv("testcv_results/results.csv", as.is = T)
ggplot(results, aes(x = model, y = log_loss)) +
  geom_point(aes(color = span), size = 2) +
  theme_bw() + 
  labs(x = "Model",
       y = "Log Loss",
       title = "Model Performance on Test Set",
       subtitle = "Log Loss",
       color = "LOESS Span") +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        legend.position = "bottom")


### Figure 2.4
time_results <-read.csv("testcv_results/time_results.csv", as.is = T)
mutate(time_results, "model_id" = paste(model, span)) %>%
  filter(model_id %in% c("0 0.5",  "4 0.5"),
         !is.na(log_loss)) %>%
  group_by(max_time) %>%
  summarise("mse" = min(mse)) %>%
  inner_join(time_results, by = c("max_time", "mse")) %>%
  ggplot(aes(x = 2400 - max_time + 100, y = log_loss)) +
  geom_point(aes(col = as.character(model))) +
  theme_bw() + 
  labs(x = "Seconds Elapsed",
       y = "Minimum Log Loss",
       title = "Model Performance on Test Set Over Time",
       subtitle = "Best Models Only",
       color = "Model w/ Better Log Loss") +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        legend.position = "bottom") + 
  scale_color_manual(values = gg_color_hue(2), 
                     labels = c("No Timeout Indicator (Model 0)",
                                "Timeout Indicator (Model 4)")) +
  scale_x_continuous(breaks = seq(0, 2400, 400), limits = c(0, 2400))

### Figure 2.5
z <- read.csv("model_coefficients/model_1_coeffs.csv", as.is = T) %>%
  filter(coefficient == "timeout_ind")
p1 <-ggplot(z, aes(x = 2400 - max_time, y = estimate, group = coefficient)) +
  geom_point(aes(col = (p_value < 0.05))) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(yintercept = 0, lty = 2) +
  theme_bw() +
  theme(legend.position  = "bottom",
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 10)) +
  labs(x = "Seconds Elapsed",
       y = "Coefficient Estimate",
       title = "Timeout Indicator Coefficient Estimate Over Time",
       color = "Statistically Significant") +
  scale_x_continuous(breaks = seq(0, 2400, 400), limits = c(0, 2400))
p2 <- mutate(time_results, "model_id" = paste(model, span)) %>%
  filter(model_id %in% c("0 0.5",  "4 0.5"),
         !is.na(log_loss)) %>%
  group_by(max_time) %>%
  summarise("mse" = min(mse)) %>%
  inner_join(time_results, by = c("max_time", "mse")) %>%
  ggplot(aes(x = 2400 - max_time + 100, y = log_loss)) +
  geom_point(aes(col = as.character(model))) +
  theme_bw() + 
  labs(x = "Seconds Elapsed",
       y = "Minimum Log Loss",
       title = "Model Performance on Test Set Over Time",
       subtitle = "Best Models Only",
       color = "Model w/ Better Log Loss") +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 10),
        legend.position = "bottom") + 
  scale_color_manual(values = gg_color_hue(2), 
                     labels = c("No Timeout Indicator (Model 0)",
                                "Timeout Indicator (Model 4)")) +
  scale_x_continuous(breaks = seq(0, 2400, 400), limits = c(0, 2400))
gridExtra::grid.arrange(p1, p2)