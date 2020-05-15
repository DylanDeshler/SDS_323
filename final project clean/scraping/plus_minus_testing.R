devtools::install_github("ryurko/fcscrapR")
devtools::install_github("fmatano/PlusMinusData")
library(magrittr)
library(fcscrapR)
library(PlusMinusData)
library(dplyr)
setwd("~/Desktop/testing")

devtools::load_all()

league_selected <- "english premier league"

source("scrape_lineups.R")
source("plus_minus.R")

start <- as.Date("2019-8-8")
end <- as.Date("2020-3-15")
day <- start
X <- data.frame()
while(day <= end) {
  espn_games <- tryCatch(fcscrapR::scrape_scoreboard_ids(scoreboard_name=league_selected, game_date=day), error = function(e) e)
  if(!is.na(espn_games)) {
    for(game_id in espn_games$game_id) {
      lineup <- scrape_lineup(game_id = game_id)
      
      game_commentary <- fcscrapR::scrape_commentary(game_id = game_id)
      
      lineup <- add_red_card_info(game_commentary = game_commentary, game_lineup = lineup)
      
      segmentation_mat <- create_segmentation(game_lineup = lineup)
      segmentation_mat <- data.frame(segmentation_mat)
      colnames(segmentation_mat) <- c("start", "stop")
      segmentation_mat$home_goal <- 0
      segmentation_mat$away_goal <- 0
      
      # hand make segmentation matrix
      game_commentary <- game_commentary[game_commentary$match_time!="-",]
      game_commentary$team_one_score <- as.integer(game_commentary$team_one_score)
      game_commentary$team_two_score <- as.integer(game_commentary$team_two_score)
      time <- game_commentary$match_time
      time <- gsub("'","",time)
      time <- as.integer(lapply(time, function(x) ifelse(nchar(x) > 2, x <- substr(x,1,2), x <- x)))
      game_commentary$match_time <- time
      home_score <- 0
      away_score <- 0
      goal_times <- data.frame(time = integer(),
                               home = logical(),
                               count = integer())
      j <- 1
      for(i in 1:nrow(game_commentary)) {
        if(game_commentary$team_one_score[i] > home_score) {
          team_one_score <- game_commentary$team_one_score[i]
          goal_times[j,] <- c(game_commentary$match_time[i], TRUE, team_one_score - home_score)
          home_score <- team_one_score
          j <- j + 1
        }
        if(game_commentary$team_two_score[i] > away_score) {
          team_two_score <- game_commentary$team_two_score[i]
          goal_times[j,] <- c(game_commentary$match_time[i], FALSE, team_two_score - away_score)
          away_score <- team_two_score
          j <- j + 1
        }
      }
      
      if(nrow(goal_times) > 0) {
        for(i in 1:nrow(segmentation_mat)) {
          for(j in 1:nrow(goal_times)) {
            if(goal_times$time[j] >= segmentation_mat$start[i] && goal_times$time[j] < segmentation_mat$stop[i]) {
              if(goal_times$home[j]) {
                segmentation_mat$home_goal[i] <- segmentation_mat$home_goal[i] + goal_times$count[j]
              } else if(!goal_times$home[j]) {
                segmentation_mat$away_goal[i] <- segmentation_mat$away_goal[i] + goal_times$count[j]
              }
            }
          }
        }
      }
      
      for(i in 1:nrow(lineup)) {
        player_in <- lineup$time_of_sub_in[i]
        player_out <- lineup$time_of_sub_out[i]
        for(j in 1:nrow(segmentation_mat)) {
          if(player_in <= segmentation_mat$start[j] && player_out >= segmentation_mat$stop[j]) {
            segmentation_mat[j, i + 4] <- ifelse(lineup$team[i] == "home", 1, -1)
          } else {
            segmentation_mat[j, i + 4] <- 0
          }
        }
        colnames(segmentation_mat)[i + 4] <- lineup$lineup[i]
      }
    }
    
    X <- bind_rows(X, segmentation_mat)
  }
  
  day <- day + 1
}
write.csv(X, "new_espn2019.csv")


# plus minus testing
head(order(colSums(X), decreasing = TRUE))
#write.csv(X, "new_espn2016.csv")
X[is.na(X)] <- 0
pm <- X$Ederson*(X$home_goal-X$away_goal)
sum(pm)
players <- dplyr::select(X, -c(start, stop, home_goal, away_goal))
pm <- apply(players, 2, function(x) x*(X$home_goal-X$away_goal))
head(sort(colSums(pm), decreasing = TRUE), 20)


