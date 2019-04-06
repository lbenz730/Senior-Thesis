### score_runs.R
### Computes net score differential 
### Into/out of every point of all games
### For intervals of varying length

library(dplyr)

load("cleaned_training_data.rda")
train <- mutate(train, secs_elapsed = 2400 - secs_remaining)
game_ids <- unique(train$game_id)

### Score Run Function
score_run <- function(j, interval) {
  if(interval > 0) {
    if(x$secs_remaining[j] < interval) {
      return(NA)
    }
    scores <- x$score_diff[x$secs_elapsed >= x$secs_elapsed[j] & 
                             x$secs_elapsed <= (x$secs_elapsed[j] + interval)]
  }else{
    if(x$secs_elapsed[j] < abs(interval)) {
      return(NA)
    }
    scores <- x$score_diff[x$secs_elapsed <= x$secs_elapsed[j] & 
                             x$secs_elapsed >= (x$secs_elapsed[j] + interval)]
  }
  return(scores[length(scores)] - scores[1])
}


intervals <- c(seq(-300, -60, 60), seq(60, 300, 60))
n <- length(game_ids)
for(i in 1:n) {
  if(i %% 100 == 0) {
    cat(i, "of", n, "\n") 
  }
  ### Remove Duplicate Time Events
  x <- filter(train, game_id == game_ids[i]) %>%
    filter(!duplicated(secs_elapsed))
  nx <- nrow(x)
  if(nx > 0) {
    for(k in 1:length(intervals)) {
      if(k == 1) {
        df <- mutate(x, 
                     "score_run" = sapply(1:nx, score_run, interval = intervals[k]),
                     "run_interval" = intervals[k])
      }else{
        df <- df %>% bind_rows(mutate(x, 
                                      "score_run" = sapply(1:nx, score_run, interval = intervals[k]),
                                      "run_interval" = intervals[k]))
      }
    }
    if(i == 1) {
      train_runs <- df 
    }else{
      train_runs <- bind_rows(train_runs, df)
    }
  }
}

train_runs <- filter(train_runs, !is.na(score_run))
save("train_runs", file = "train_runs.rda")