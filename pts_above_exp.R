### pts_above_exp.R
### Computes after timeout 
### Points above expectation

library(dplyr)
library(lme4)
load("train_runs.rda")

# Stripwhite function 
stripwhite <- function(x) gsub("\\s*$", "", gsub("^\\s*", "", x))

### All Pre/Post Pairs
x <- filter(train_runs, run_interval < 0) %>% 
  mutate("abs_interval" = abs(run_interval))
y <- filter(train_runs, run_interval > 0) %>%
  select(game_id, play_id, score_run, run_interval)

### All Pairs
pre_post_pairs_all <- inner_join(x, y,
                                 by = c("game_id", "play_id", "abs_interval" = "run_interval"),
                                 suffix = c("_pre", "_post"))

pre_post_pairs_all <- rename(pre_post_pairs_all, "run_interval_pre" = run_interval,
                             "run_interval_post" = abs_interval)

no_timeout_pairs <- filter(pre_post_pairs_all, !grepl("Timeout", description))
timeout_pairs <- filter(pre_post_pairs_all, grepl("Timeout", description))

### Data Cleaning
timeout_pairs$timeout_taker <- stripwhite(gsub(" Timeout", "", timeout_pairs$description))
timeout_pairs <- mutate(timeout_pairs, 
                        "score_run_pre" = ifelse(opponent == timeout_taker, -score_run_pre, score_run_pre),
                        "score_run_post" = ifelse(opponent == timeout_taker, -score_run_post, score_run_post),
                        "favored_by" = ifelse(opponent == timeout_taker, -favored_by, favored_by))

### Predicted Post Interval Run
lm.60 <- lm(score_run_post ~ favored_by + score_diff + secs_elapsed + score_run_pre,
            data = filter(no_timeout_pairs, run_interval_post == 60))
lm.120 <- lm(score_run_post ~ favored_by + score_diff + secs_elapsed + score_run_pre,
             data = filter(no_timeout_pairs, run_interval_post == 120))
lm.180 <- lm(score_run_post ~ favored_by + score_diff + secs_elapsed + score_run_pre,
             data = filter(no_timeout_pairs, run_interval_post == 180))
lm.240 <- lm(score_run_post ~ favored_by + score_diff + secs_elapsed + score_run_pre,
             data = filter(no_timeout_pairs, run_interval_post == 240))
lm.300 <- lm(score_run_post ~ favored_by + score_diff + secs_elapsed + score_run_pre,
             data = filter(no_timeout_pairs, run_interval_post == 300))

lmer.60 <- 
  lmer(score_run_post ~ favored_by + score_diff + secs_elapsed + score_run_pre +
         (1|game_id),
       data = filter(no_timeout_pairs, run_interval_post == 60))
lmer.120 <- 
  lmer(score_run_post ~ favored_by + score_diff + secs_elapsed + score_run_pre +
         (1|game_id),
       data = filter(no_timeout_pairs, run_interval_post == 120))
lmer.180 <- 
  lmer(score_run_post ~ favored_by + score_diff + secs_elapsed + score_run_pre +
         (1|game_id),
       data = filter(no_timeout_pairs, run_interval_post == 180))
lmer.240 <- 
  lmer(score_run_post ~ favored_by + score_diff + secs_elapsed + score_run_pre +
         (1|game_id),
       data = filter(no_timeout_pairs, run_interval_post == 240))
lmer.300 <- 
  lmer(score_run_post ~ favored_by + score_diff + secs_elapsed + score_run_pre +
         (1|game_id),
       data = filter(no_timeout_pairs, run_interval_post == 300))


### Predict Post Time Out Runs
timeout_pairs$pred_score_run_post <- NA
timeout_pairs$pred_score_run_post[timeout_pairs$run_interval_post == 60] <- 
  predict(lmer.60, newdata = timeout_pairs[timeout_pairs$run_interval_post == 60,],
          allow.new.levels = T)
timeout_pairs$pred_score_run_post[timeout_pairs$run_interval_post == 120] <- 
  predict(lmer.120, newdata = timeout_pairs[timeout_pairs$run_interval_post == 120,],
          allow.new.levels = T)
timeout_pairs$pred_score_run_post[timeout_pairs$run_interval_post == 180] <- 
  predict(lmer.180, newdata = timeout_pairs[timeout_pairs$run_interval_post == 180,],
          allow.new.levels = T)
timeout_pairs$pred_score_run_post[timeout_pairs$run_interval_post == 240] <- 
  predict(lmer.240, newdata = timeout_pairs[timeout_pairs$run_interval_post == 240,],
          allow.new.levels = T)
timeout_pairs$pred_score_run_post[timeout_pairs$run_interval_post == 300] <- 
  predict(lmer.300, newdata = timeout_pairs[timeout_pairs$run_interval_post == 300,],
          allow.new.levels = T)

### Pts Above Expectation
timeout_pairs <- mutate(timeout_pairs, 
                        "pts_above_exp" = score_run_post - pred_score_run_post,
                        "timeout_type" = ifelse(timeout_taker == "Official TV", 
                                                "Media Timeout", "Non-Media Timeout"))
