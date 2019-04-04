### fit_model.R
### Obtains coefficient estimates for smoothed win probability model
library(dplyr)
library(ggplot2)
load("cleaned_training_data.rda")

mnum <- 1 ### Set to 0, 1, 2, or 3

### Set Rolling Intervals
### Model 1 (90% Overlap): 
###   - 600 to 2400 seconds remaining, 100 second intervals
###   - 100 to 600 seconds remaining, 50 second intervals
###   - 10 to 100 seconds remaining, 5 second intervals
###   - 0 to 10 second remainings, 2 second intervals
###   - 0 to 1 second remaining
###   Model 0 and 4 includes same time/stamp breakdown

if(mnum == 0 | mnum == 1) {
  max_sec <- c(seq(2400, 600, -10), seq(600, 100, -5), seq(100, 10, -1), seq(10, 2, -1), 1)
  min_sec <- c(seq(2300, 500, -10), seq(550, 50, -5), seq(95, 5, -1), seq(8, 0, -1), 0)
  n <- length(max_sec)
}

### Model 2 (90% Overlap): 
###   - 600 to 2400 seconds remaining, 60 second intervals
###   - 100 to 600 seconds remaining, 30 second intervals
###   - 20 to 100 seconds remaining, 10 second intervals
###   - 0 to 20 second remainings, 2 second intervals
###   - 0 to 1 second remaining

if(mnum == 2) {
  max_sec <- c(seq(2400, 600, -6), seq(600, 100, -3), seq(100, 20, -1), seq(20, 2, -1), 1)
  min_sec <- c(seq(2340, 540, -6), seq(570, 70, -3), seq(90, 10, -1), seq(18, 0, -1), 0)
  n <- length(max_sec)
}

### Model 3 (90 % Overlap): 
###   - 300 to 2400 seconds remaining, 30 second intervals
###   - 60 to 300 seconds remaining, 15 second intervals
###   - 0 to 60 second remainings, 3 second intervals
###   - 0 to 1 second remaining

if(mnum == 3) {
  max_sec <- c(seq(2400, 300, -3), seq(300, 60, -1.5), seq(60, 3, -1), 1)
  min_sec <- c(seq(2370, 270, -3), seq(285, 45, -1.5), seq(57, 0, -1), 0)
  n <- length(max_sec)
}

### Fit Model
for(i in 1:n) {
  cat("Fitting Window", i, "of", n, "\n")
  ### Filter Training set based on time slice
  x <- filter(train, secs_remaining >= min_sec[i], secs_remaining <= max_sec[i])
  model <- 
    glm(win ~ score_diff + favored_by + timeout_diff + timeout_ind -1, data = x, 
        family = binomial(link = "logit"))
  
  ### Save Coefficient Estimates
  df <- as.data.frame(summary(model)$coefficients)
  names(df) <- c("estimate", "std_error", "z_value", "p_value")
  df$coefficient <- row.names(df)
  row.names(df) <- c()
  df <- select(df, coefficient, everything()) %>% 
    mutate("min_time" = min_sec[i], "max_time" = max_sec[i])
  
  if(i == 1) {
    model_coeffs <- df
  }else{
    model_coeffs <- bind_rows(model_coeffs, df)
  }
}

### Save results
write.csv(model_coeffs, paste0("model_coefficients/model_", mnum, "_coeffs.csv"), row.names = F)