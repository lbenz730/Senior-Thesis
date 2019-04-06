### Clean.R
### Prepares data for modeling purposes
### Notes:
###   In creating the training set, I get cleaned versions from both team's 
###   perspectives. The purpose of this is two-fold. Win probability should
###   should be symmetrical--that is, if we switched which team's perspective
###   we are predicting the model from, all that should change is estimates
###   of win probability (p) go to (1-p). Moreover, the default rendering
###   of ncaahoopR and the underlying ESPN play by play game logs is to 
###   examine everything from the perspective of the home team. While 
###   home court advantage is implict in the favored_by column, we need to be
###   careful to ensure that we are not conflating important effects with 
###   simply being the home team.

library(dplyr)
library(readr)

### Names of all ESPN files
file_names <- dir("pbp_data", full.names = T)
n <- length(file_names)

for(i in 1:n) {
  if(i %% 100 == 0) {
    cat("Cleaning File #:", i, "of", n, "\n")
  }
  x <- read_csv(file_names[i], col_types = cols())
  
  ### Check to make sure pbp log is valid
  bad_file <- 
    any(abs(x$score_diff[2:nrow(x)] - x$score_diff[1:(nrow(x) -1)]) > 3) 
  
  if(!bad_file) {
    ### Clean data set from home team's perspective
    team <- 
      mutate(x, "win" = ifelse(score_diff[nrow(x)] > 0, 1, 0),
             "timeout_ind" = home_timeout_ind - away_timeout_ind,
             "timeout_diff" = home_time_out_remaining - away_time_out_remaining,
             "timeout_to_use" = home_time_out_remaining > 0) %>%
      select(game_id, date, home, away, play_id, half, secs_remaining, 
             secs_remaining_absolute, home_score, away_score, score_diff, 
             timeout_ind, timeout_diff, timeout_to_use, home_favored_by, 
             win, description) %>%
      rename("team" = home,
             "opponent" = away,
             "team_score" = home_score,
             "opp_score" = away_score,
             "favored_by" = home_favored_by)
    
    if(!exists("train")) {
      train <- team
    }else{
      train <- bind_rows(train, team)
    }
  }
}

### Remove Games with Bad Timeout Parsing
bad_ids <- c() 
for(i in 1:n) {
  if(i %% 100 == 0) {
    cat("Cleaning File #:", i, "of", n, "\n")
  }
  x <- read_csv(file_names[i], col_types = cols())
  
  if(any(x$home_time_out_remaining < 0 | x$away_time_out_remaining < 0 |
         x$home_time_out_remaining > 6 | x$away_time_out_remaining > 6)) {
   bad_ids <- c(bad_ids, x$game_id[1]) 
  }
}

train <- filter(train, !game_id %in% bad_ids)

### Fix Issue with Timeout Indicator being flipped
game_ids <- unique(train$game_id)
j <- 0
for(i in 1:length(game_ids)) {
  print(i)
  x <- filter(train, game_id == game_ids[i])
  y <- filter(x, timeout_ind == 1, description == paste(x$opponent[1], " Timeout"))
  if(nrow(y) > 0) {
    x$timeout_diff <- x$timeout_diff * -1
    x$timeout_ind <- x$timeout_ind * -1
    train$timeout_diff[train$game_id == game_ids[i]] <- x$timeout_diff
    train$timeout_ind[train$game_id == game_ids[i]] <- x$timeout_ind
    j <- j + 1
  }
}

### Save Training Data
save("train", file = "cleaned_training_data.rda")
