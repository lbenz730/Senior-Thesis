### Figure 4.1
load("train_runs.rda")
library(forcats)
library(dplyr)
library(ggplot2)
### Data Cleaning
timeouts <- filter(train_runs, grepl("Timeout", description))
timeouts$run_interval.f <- as.factor(as.character(timeouts$run_interval))
timeouts$run_interval.f <- fct_relevel(timeouts$run_interval.f,
                                       "-60", "-120", "-180", "-240", "-300",
                                       "60", "120", "180", "240", "300")
# Stripwhite function 
stripwhite <- function(x) gsub("\\s*$", "", gsub("^\\s*", "", x))
timeouts$timeout_taker <- stripwhite(gsub(" Timeout", "", timeouts$description))
timeouts <- mutate(timeouts, score_run = ifelse(opponent == timeout_taker, -score_run, score_run),
                   favored_by = ifelse(opponent == timeout_taker, -favored_by, favored_by))

non_tv <- filter(timeouts, !grepl("Official TV Timeout", description))
tv <- filter(timeouts, grepl("Official TV Timeout", description))
# ggcolors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


ggplot(non_tv, aes(x = as.factor(abs(run_interval)), y = score_run)) +
  geom_boxplot(aes(fill = run_interval > 0))  +
  theme_bw() +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        legend.position = "bottom") +
  labs(x = "Interval Length (Seconds)",
       y = "Net Score Differential During Interval\n(Perspective of Team Taking Timeout)",
       fill = "",
       title = "Net Score Differential Before and After Timeouts",
       subtitle = "Non-Media Timeouts") +
  scale_fill_manual(values = gg_color_hue(2), labels = c("Before Timeout", "After Timeout"))


### Table 4.1
library(knitr)
library(kableExtra)
y <- group_by(non_tv, run_interval) %>%
  summarise("mean" = mean(score_run),
            "var" = var(score_run)) %>%
  ungroup()
y <- rbind(
  y[1:5,] %>%
    mutate("run_interval" = abs(run_interval),
           "to_relativity" = "Before Timeout"),
  y[6:10,] %>%
    mutate("to_relativity" = "After Timeout"))

cbind(round(y[1:5, 1:3], 2), round(y[10:6, 2:3],2))[5:1,c(1,2,4,3,5)] %>%
  rename("Interval (Seconds)" = run_interval,
         "Pre-Timeout Average Net Score Differential" = mean,
         "Post-Timeout Average Net Score Differential" = mean.1,
         "Pre-Timeout Net Score Differential Variance" = var,
         "Post-Timeout Net Score Differentual Variance" = var.1) %>%
  kable(caption = "\\label{tab:4.1} Pre and Post Non-Media Timeout Summary Statistics", 
        "latex",
        booktabs = T,
        row.names = F, align = "ccccc") %>%
  column_spec(2:5, width = "3cm")

### Figure 4.2
ggplot(tv, aes(x = as.factor(abs(run_interval)), y = score_run)) +
  geom_boxplot(aes(fill = run_interval > 0))  +
  theme_bw() +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        legend.position = "bottom") +
  labs(x = "Interval Length (Seconds)",
       y = "Net Score Differential During Interval\n(Perspective of Home Team)",
       fill = "",
       title = "Net Score Differential Before and After Timeouts",
       subtitle = "Media Timeouts") +
  scale_fill_manual(values = gg_color_hue(2), labels = c("Before Timeout", "After Timeout"))

### Table 4.2
y <- group_by(tv, run_interval) %>%
  summarise("mean" = mean(score_run),
            "var" = var(score_run)) %>%
  ungroup()
y <- rbind(
  y[1:5,] %>%
    mutate("run_interval" = abs(run_interval),
           "to_relativity" = "Before Timeout"),
  y[6:10,] %>%
    mutate("to_relativity" = "After Timeout"))

cbind(round(y[1:5, 1:3], 2), round(y[10:6, 2:3],2))[5:1,c(1,2,4,3,5)] %>%
  rename("Interval (Seconds)" = run_interval,
         "Pre-Timeout Average Net Score Differential" = mean,
         "Post-Timeout Average Net Score Differential" = mean.1,
         "Pre-Timeout Net Score Differential Variance" = var,
         "Post-Timeout Net Score Differentual Variance" = var.1) %>%
  kable(caption = "\\label{tab:4.2} Pre and Post Media Timeout Summary Statistics (Home Team Perspective)", 
        "latex",
        booktabs = T,
        row.names = F, align = "ccccc") %>%
  column_spec(2:5, width = "3cm")

### Figure 4.3
pre_post_pairs <- inner_join(non_tv, 
                             select(non_tv, game_id, play_id, score_run, run_interval),
                             by = c("game_id", "play_id"),
                             suffix = c("_pre", "_post")) %>%
  filter(run_interval_pre < 0, run_interval_post > 0)

ggplot(pre_post_pairs, aes(x = score_run_post - score_run_pre, y = ..density..)) +
  facet_grid(run_interval_post ~ run_interval_pre) +
  geom_histogram(bins = 20, fill = "orange") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        legend.position = "bottom") +
  labs(x = expression(
    paste("Improvement in Net Score Differential Following Timeout (", Delta[tij], ")",
          sep = "")
  ),
  y = "Density",
  color = "Post Timeout - Pre Timeout Delta",
  title = "Difference in Before and After Timeout Net Score Differential",
  subtitle = "Non-Media Timeouts") +
  geom_vline(xintercept = 0, lty = 2, size = 0.6, alpha = 0.5) +
  scale_x_continuous(limits = c(-35, 35), breaks = seq(-20, 20, 20)) +
  scale_y_continuous(limits = c(0, 0.12), breaks = seq(0, 0.12, 0.03))

### Figure 4.4
pre_post_pairs_tv <- inner_join(tv, 
                                select(tv, game_id, play_id, score_run, run_interval),
                                by = c("game_id", "play_id"),
                                suffix = c("_pre", "_post")) %>%
  filter(run_interval_pre < 0, run_interval_post > 0)

ggplot(pre_post_pairs_tv, aes(x = score_run_post - score_run_pre, y = ..density..)) +
  facet_grid(run_interval_post ~ run_interval_pre) +
  geom_histogram(bins = 20, fill = "red") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        legend.position = "bottom") +
  labs(x = expression(
    paste("Improvement in Net Score Differential Following Timeout (", Delta[hij], ")",
          sep = "")
  ),
  y = "Density",
  color = "Post Timeout - Pre Timeout Delta",
  title = "Difference in Before and After Timeout Net Score Differential",
  subtitle = "Non-Media Timeouts") +
  geom_vline(xintercept = 0, lty = 2, size = 0.6, alpha = 0.5) +
  scale_x_continuous(limits = c(-35, 35), breaks = seq(-20, 20, 20)) +
  scale_y_continuous(limits = c(0, 0.12), breaks = seq(0, 0.12, 0.03))

### Table 4.3
cbind(group_by(pre_post_pairs, run_interval_pre, run_interval_post) %>%
        summarise("non_media_delta" = round(mean(score_run_post - score_run_pre),2)) %>%
        ungroup() %>%
        mutate(run_interval_pre = -run_interval_pre) %>%
        arrange(run_interval_pre),
      group_by(pre_post_pairs_tv, run_interval_pre, run_interval_post) %>%
        summarise("media_delta" = round(mean(score_run_post - score_run_pre),2)) %>%
        ungroup() %>%
        mutate(run_interval_pre = -run_interval_pre) %>%
        arrange(run_interval_pre) %>%
        select(media_delta)) %>%
  rename("Pre-Timeout Interval" = run_interval_pre,
         "Post-Timeout Interval" = run_interval_post,
         "Mean Improvement (Non-Media Timeouts)" = non_media_delta,
         "Mean Improvement (Media Timeouts)" = media_delta) %>%
  kable(caption = "\\label{tab:4.3} Summary Table of Post Timeout Improvement", 
        "latex",
        booktabs = T,
        row.names = F, align = "ccccc") %>%
  column_spec(2:4, width = "3cm")

### Figure 4.5
ggplot(filter(pre_post_pairs, -run_interval_pre == run_interval_post),
       aes(x = secs_elapsed, y = score_run_post - score_run_pre)) +
  facet_wrap(~run_interval_post) +
  geom_smooth(col = gg_color_hue(2)[2]) +
  geom_vline(xintercept = 1200, lty = 2, size = 1.2) +
  theme_bw() +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14)) +
  labs(x = "Seconds Elapsed",
       y = "Improvement in Net Score Differential",
       fill = "",
       title = "Improvement in Net Score Differential in Interval Following Timeout \nOver Course of Game",
       subtitle = "Non-Media Timeouts") +
  scale_x_continuous(breaks = seq(0, 2400, 600), limits = c(0, 2400))

### Figure 4.6
ggplot(filter(pre_post_pairs_tv, -run_interval_pre == run_interval_post),
       aes(x = secs_elapsed, y = score_run_post - score_run_pre)) +
  facet_wrap(~run_interval_post) +
  geom_smooth(col = gg_color_hue(2)[1]) +
  geom_vline(xintercept = 1200, lty = 2, size = 1.2) +
  theme_bw() +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14)) +
  labs(x = "Seconds Elapsed",
       y = "Improvement in Net Score Differential",
       fill = "",
       title = "Improvement in Net Score Differential in Interval Following Timeout \nOver Course of Game",
       subtitle = "Media Timeouts") +
  scale_x_continuous(breaks = seq(0, 2400, 600), limits = c(0, 2400))


### Figure 4.7
library(sjPlot)
tab_model(lmer.60, lmer.120, lmer.180, lmer.240, lmer.300,
          dv.labels = c("60 Sec", "120 Sec", "180 Sec", "240 Sec", "300 Sec"),
          show.ci = F)

### Figure 4.8
### Points above expectation computed
### in pts_above_exp.R
timeout_pairs <- mutate(timeout_pairs, 
                        "pts_above_exp" = score_run_post - pred_score_run_post,
                        "timeout_type" = ifelse(timeout_taker == "Official TV", 
                                                "Media Timeout", "Non-Media Timeout"))

ggplot(timeout_pairs,
       aes(x = as.factor(run_interval_post), y = pts_above_exp)) +
  geom_hline(yintercept = 0, lty = 2, col = "red", size = 1.1) +
  geom_boxplot(aes(fill = timeout_type), alpha = 0.5) +
  theme_bw() +
  labs(x = "Interval Length (Seconds)",
       y = "Points Above Expectation",
       fill = "Timeout Type",
       title = "Points Above Expectation in Post-Timeout Intervals of Varying Length") +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        legend.position = "bottom")

### Table 4.4
library(kableExtra)
group_by(filter(timeout_pairs, timeout_taker != "Official TV"), run_interval_post) %>%
  summarise("mean_pae_ntv" = round(mean(pts_above_exp, na.rm = T), 3),
            "median_pae_ntv" = round(median(pts_above_exp, na.rm = T), 3)) %>%
  inner_join(group_by(filter(timeout_pairs, timeout_taker == "Official TV"), run_interval_post) %>%
               summarise("mean_pae_tv" = round(mean(pts_above_exp, na.rm = T), 3),
                         "median_pae_tv" = round(median(pts_above_exp, na.rm = T), 3))) %>%
  rename("Post-Timeout Interval Length" = run_interval_post,
         "Mean PAE (Non-Media Timeouts)" = mean_pae_ntv,
         "Median PAE (Non-Media Timeouts)" = median_pae_ntv,
         "Mean PAE (Media Timeouts)" = mean_pae_tv,
         "Median PAE (Media Timeouts)" = median_pae_tv) %>%
  kable(caption = "\\label{tab:4.4} Summary Table of Points Above Expectation", 
        "latex",
        booktabs = T,
        row.names = F, align = "ccccc") %>%
  column_spec(1:5, width = "3cm")


### Figure 4.9
ggplot(filter(timeout_pairs, timeout_taker != "Official TV"), 
       aes(x = secs_elapsed, y = pts_above_exp)) +
  geom_smooth(col = gg_color_hue(2)[2]) +
  facet_wrap(~run_interval_post) +
  theme_bw() +
  labs(x = "Seconds Elapsed",
       y = "Points Above Expectation",
       title = "Points Above Expectation in Post-Timeout Intervals Over Course of Game",
       subtitle = "Non-Media Timeouts") +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        legend.position = "bottom") +
  scale_x_continuous(breaks = seq(0, 2400, 600), limits = c(0, 2400))

### Figure 4.10
ggplot(filter(timeout_pairs, timeout_taker == "Official TV"), 
       aes(x = secs_elapsed, y = pts_above_exp)) +
  geom_smooth(col = gg_color_hue(2)[1]) +
  facet_wrap(~run_interval_post) +
  theme_bw() +
  labs(x = "Seconds Elapsed",
       y = "Points Above Expectation",
       title = "Points Above Expectation in Post-Timeout Intervals Over Course of Game",
       subtitle = "Media Timeouts") +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        legend.position = "bottom") +
  scale_x_continuous(breaks = seq(0, 2400, 600), limits = c(0, 2400))

### Figure 4.11
coeffs <- read.csv("model_coefficients/model_0_coeffs.csv", as.is = T) 
score_diff_smooth <- 
  loess(estimate ~ max_time, 
        data = filter(coeffs, coefficient == "score_diff"),
        span = 0.5)

s.300 <- loess(pts_above_exp ~ secs_elapsed, 
               data = filter(timeout_pairs, timeout_taker != "Official TV",
                             run_interval_post == 300),
               span = 0.175)
sec <- seq(0, 2400, 1)

sd_smooth <- predict(score_diff_smooth, newdata = 2400 - sec)
smooth300 <- predict(s.300, newdata = sec)
value300 <- (sd_smooth * smooth300)/max(sd_smooth * smooth300, na.rm = T)
plot(value300)

df <- data.frame("time" = sec,
                 "value" = value300)

ggplot(df, aes(x = time, y = value)) +
  geom_line(col = gg_color_hue(2)[2], size = 2) +
  scale_x_continuous(breaks = seq(0, 2400, 400), 
                     limits = c(0, 2400)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2),
                     limits = c(0,1)) +
  theme_bw() +
  labs(x = "Seconds Elapsed",
       y = "Relative Value",
       title = "Relative Value of Taking a Timeout") +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14))
