### eval.R
### Computes MSE and Log-Loss for different models

library(dplyr)
library(readr)
library(ggplot2)

### Loss Functions
log_loss <- function(x, y) {
  eps <- 1e-16
  x[x < eps] <- eps
  x[x > (1 - eps)] <- 1 - eps
  return(-mean(y * log(x) + (1-y) * log(1-x)))
}

mse <- function(x, y) {
  return(mean((x-y)^2))
}

### Recreate ggoplot2 colors
### Copied from https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

files <- dir("testcv_results", full.names = T)

### Entire Game Evaluation
for(i in 1:length(files)) {
  cat(i, "of", length(files), "\n")
  test <- read_csv(files[i], col_types = cols())
  model <- substr(files[i], 22, 22)
  span <- ifelse(model == "5", "multi-span", substr(files[i], 28, 30))
  model <- ifelse(model == "5", "4", model)
  df <- tibble(
    "model" = model,
    "span" = span,
    "log_loss" = log_loss(test$win_prob, test$win),
    "mse" = mse(test$win_prob, test$win)
  )
  
  if(i == 1) {
    results <- df 
  }else{
    results <- bind_rows(results, df)
  }
}

max_times <- seq(100, 2400, 10)
min_times <- seq(0, 2300, 10)
### Entire Game Evaluation
for(i in 1:length(files)) {
  cat(i, "of", length(files), "\n")
  test <- read_csv(files[i], col_types = cols())
  
  model <- substr(files[i], 22, 22)
  span <- ifelse(model == "5", "multi-span", substr(files[i], 28, 30))
  model <- ifelse(model == "5", "4", model)
  
  for(j in 1:(length(min_times))) {
    x <- filter(test, secs_remaining >= min_times[j], secs_remaining <= max_times[j+1])
    df <- tibble(
      "model" = model,
      "span" = span,
      "log_loss" = log_loss(x$win_prob, x$win),
      "mse" = mse(x$win_prob, x$win),
      "min_time" = min_times[j],
      "max_time" = max_times[j]
    )
    
    if(i == 1 & j == 1) {
      time_results <- df 
    }else{
      time_results <- bind_rows(time_results, df)
    }
  }
}

write.csv(results, "testcv_results/results.csv", row.names = F)
write.csv(time_results, "testcv_results/time_results.csv", row.names = F)