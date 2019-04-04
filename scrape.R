### Scrape.R
### Scrapes Data for 2016-17 and 2017-18 Seasons
### Also Scrapes 2018-19 season data as test set (Run on 2/3/2019)

library(ncaahoopR)

### Load Games IDs to Scrape
load("espn_game_ids.rda")
espn_game_ids <- 
  espn_game_ids[!espn_game_ids %in% gsub("[^0-9]", "", dir("pbp_data/"))]
n <- length(espn_game_ids)

for(i in 1:n) {
  cat("Game", i, "of", n, "\n")
  fails <- 0
  while(fails >= 0 & fails <= 5) {
    x <- try(get_pbp_game(espn_game_ids[i]))
    if(class(x) != "try-error") {
      write.csv(x, paste0("pbp_data/game_id_", espn_game_ids[i], ".csv"), row.names = F) 
      fails <- -1
    }else{
      fails <- fails + 1
    }
  }
}

### Scrape 2018-19 Data
game_ids <- c()
i <- 0
for(team in ids$team) {
  i <- i + 1
  cat("Scraping Team", i, "of", "353\n")
  game_ids <- c(
    game_ids,
    get_schedule(team) %>%
      filter(date < Sys.Date()) %>%
      pull(game_id))
}

game_ids <- unique(game_ids)
n <- length(game_ids)
for(i in 1:n) {
  cat("Scraping Game", i, "of", n, "\n")
  x <- get_pbp_game(game_ids[i])
  write.csv(x, paste0("test_pbp_data/game_id_", game_ids[i], ".csv"), row.names = F)
}
