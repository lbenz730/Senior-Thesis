### model_cv.R
### Predicts fitted win probability models on test set data
### Cross Validates over different values of LOESS span parameters

library(readr)
library(dplyr)

### Fits Test win probability on 2018-19 season games
files <- dir("test_pbp_data", full.names = T)
n <- length(files)

### Define Logit Function
logit <- function(x) {
  tmp <- exp(x)
  case_when(
    tmp == Inf ~ 1,
    tmp == -Inf ~ 0,
    T ~ tmp/(1 + tmp)
  )
}

### Set Model Number
model_n <- 0
coeffs <- read_csv(paste0("model_coefficients/model_", model_n, "_coeffs.csv"), 
                   col_types = cols())

### Set Coefficients to achieve deterministic relationship at max_time = 0
coeffs$estimate[coeffs$max_time <= 2 & coeffs$coefficient == "favored_by"] <- 0

### Cross Validate over span 
spans <- seq(0.1, 0.5, 0.1)
for(j in 1:length(spans)) {
  ### Fit Loess Models to get smooth functions of coefficient estimate over time
  score_diff_smooth <- 
    loess(estimate ~ max_time, 
          data = filter(coeffs, coefficient == "score_diff"),
          span = spans[j])
  
  favored_by_smooth <- 
    loess(estimate ~ max_time, 
          data = filter(coeffs, coefficient == "favored_by"),
          span = spans[j])
  
  timeout_diff_smooth <-
    loess(estimate ~ max_time,
          data = filter(coeffs, coefficient == "timeout_diff"),
          span = spans[j])
  
  timeout_ind_smooth <- 
    loess(estimate ~ max_time, 
          data = filter(coeffs, coefficient == "timeout_ind"),
          span = spans[j])
  
  
  
  for(i in 1:n) {
    if(i %% 100 == 0) {
      cat("Computing Model # ", model_n, " Game ", i, " of ", n, " (span = ", spans[j], ")\n", 
          sep = "")
    }
    ### Read in Test Game
    x <- read_csv(files[i], col_types = cols()) %>%
      select(-win_prob)
    
    y <- filter(x, home_timeout_ind == 1, description == paste(x$away[1], " Timeout"))
    switch <- ifelse(nrow(y) > 0, -1, 1)
    
    
    if(!is.na(x$home_favored_by[1])) {
      ### Get Coefficient Values for Current Game
      sc_diff <- predict(score_diff_smooth, newdata = x$secs_remaining)
      fb <- predict(favored_by_smooth, newdata = x$secs_remaining)
      to_diff <- predict(timeout_diff_smooth, newdata = x$secs_remaining)
      to_ind <- predict(timeout_ind_smooth, newdata = x$secs_remaining)
      
      ### Capture Game Determinism
      index <- x$secs_remaining == 0
      sc_diff[index] <- 20
      fb[index] <- predict(favored_by_smooth, newdata = 1)
      to_diff[index] <- predict(timeout_diff_smooth, newdata = 1)
      to_ind[index] <- predict(timeout_ind_smooth, newdata = 1)
      
      ### Compute log odds of winning
      log_odds <- 
        sc_diff * x$score_diff  + 
        fb * x$home_favored_by + 
        to_ind * (x$home_timeout_ind - x$away_timeout_ind) * switch +
        to_diff * (x$home_time_out_remaining - x$away_time_out_remaining) * switch
      
      x <- mutate(x, 
                  "win_prob" = logit(log_odds),
                  "win" = ifelse(score_diff[nrow(x)] > 0, 1, 0),
                  "favored_by" = home_favored_by,
                  "timeout_ind" = home_timeout_ind - away_timeout_ind,
                  "timeout_diff" = home_time_out_remaining - away_time_out_remaining,
                  "model" = model_n,
                  "span" = "mixed") %>%
        select(game_id, date, win, win_prob, secs_remaining, 
               score_diff, favored_by, timeout_ind, timeout_diff, model, span)
      
      if(any(is.na(x$win_prob))) {
        break
      }
      
      ### Save Results
      if(i == 1) {
        test <- x
      }else{
        test <- bind_rows(test, x)
      }
    }
  }
  write.csv(test, paste0("testcv_results/model_", model_n, "span_", spans[j], ".csv"), 
            row.names = F)
}
