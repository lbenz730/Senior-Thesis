### inputation_evaluation.R
### Allows for evaluation of imputed 
### point spreads on 2018-19 test set

library(ncaahoopR)
x <- read.csv("2019_Final.csv", as.is = T)

files <- unlist(sapply(dir("test_pbp_data", full.names = T), dir, full.names = T))
n <- length(files)
vegas_lines <- c()
imputed_lines <- c()

for(i in 1:n) {
  if(i %% 100 == 0) {
    print(i)
  }
  y <- read.csv(files[i], as.is = T)
  home <- dict$NCAA[dict$ESPN == y$home[1] | dict$ESPN_PBP == y$home[1]]
  away <- dict$NCAA[dict$ESPN == y$away[1] | dict$ESPN_PBP == y$away[1]]
  if(length(home) == 0) {
    home <- NA
  } 
  if(length(away) == 0) {
    away <- NA
  }
  date_line <- y$date[1]
  pred_line <- filter(x, team == home, opponent == away, date == date_line) %>% 
    pull(pred_score_diff)
  true_line <- y$home_favored_by[1]
  
  if(length(c(pred_line, true_line)) == 2) {
    if(!is.na(true_line) & !is.na(pred_line)) {
      if(true_line != pred_line ) {
        vegas_lines <- c(vegas_lines, true_line)
        imputed_lines <- c(imputed_lines, pred_line)
      }
    }
  }
}

df <- data.frame("vegas_line" = vegas_lines,
           "imputed_line" = imputed_lines)
write.csv(df, "line_imputation.csv", row.names = F)
