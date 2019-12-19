##############################
# Load Libraries
##############################
library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)

source("helper_functions/plot_field.R")
source("helper_functions/read_train_data.R")


##############################
# Define your seasons
##############################
first <- 2017 # first season to grab. min available=2009
last <- 2018  # most recent season

# define an empty list
datalist = list()

# read kaggle data
data <- read_train_data()

data <-
  data %>%
  mutate(temp = x,
         x = 160/3 - y,
         y = temp,
         x_end = x + velocity * cos((180 - dir_std_2) * pi / 180),
         y_end = y + velocity * sin((180 - dir_std_2) * pi / 180))

# create a unique play id: gameplay_id
data <-
  data %>%
  mutate(play_id = substr(play_id, 11, 14)) %>%
  mutate(play_id = sub("^[0]+", "", play_id)) %>%
  mutate(
    gameplay_id = paste(game_id,play_id, sep="")
    
  )

# remove player info and get one row per play
play_level_data <-
  data %>% 
  select(gameplay_id, game_id, play_id, season:yards, home_team_abbr:wind_direction) %>%
  distinct()

# create a unique play id: gameplay_id
play_level_data <-
  play_level_data %>%
  mutate(
    gameplay_id = paste(game_id,play_id, sep="")
  )

# read nflscrapR game and play by play data for 2017-2018
# join them together
for (yr in first:last) {
  pbp   <- read_csv(url(paste0("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_", yr, ".csv")))
  games <- read_csv(url(paste0("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/regular_season/reg_games_", yr, ".csv")))
  pbp   <- 
    pbp %>% 
    inner_join(games %>% 
                 distinct(game_id, week, season)) %>% 
    select(-fumble_recovery_2_yards, -blocked_player_id, -fumble_recovery_2_player_id)
  datalist[[yr]] <- pbp # add it to your list
}

# get all the years in one data frame
pbp_all <- dplyr::bind_rows(datalist)


# replace team names for teams that have moved cities
# or just changed their abbreviations
pbp_all <- 
  pbp_all %>%
  mutate_at(
    vars(home_team, away_team, posteam, defteam), 
    funs(
      case_when(
        . %in% "JAX" ~ "JAC",
        . %in% "STL" ~ "LA",
        . %in% "SD" ~ "LAC",
        TRUE ~ .
      )))


# determine if a play is a run or a pass
pbp_all <- 
  pbp_all %>%
  mutate(
    pass = if_else(str_detect(desc, "( pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa < 0, 1 , 0)
  )

# merge kaggle and play by play data
playmerge <- 
  merge(play_level_data, pbp_all, by=(intersect(names(play_level_data), names(pbp_all))), all.x="TRUE")


playmerge <- playmerge %>%
  select(gameplay_id, game_id, play_id, posteam, defteam, desc, run_location, run_gap, ep, epa, wp, wpa,
         third_down_converted:fourth_down_failed, fumble_forced:safety, tackled_for_loss, fumble_lost, fumble,
         assist_tackle, tackle_for_loss_1_player_id:tackle_for_loss_2_player_name,
         forced_fumble_player_1_player_id:assist_tackle_4_team, fumbled_1_team:return_yards) %>%
  distinct()


one_play <-
  data %>%
  group_by(game_id, play_id) %>%
  filter(gameplay_id == 20170910001102)


one_play <-
  merge(one_play, playmerge, by="gameplay_id", all.x="TRUE")

plot_field(y_min = 70, y_max = 110) +
  geom_point(data = one_play, aes(x = x, y = y, colour = team), size = 7) +
  geom_segment(data = one_play, aes(x = x, y = y, xend = x_end, yend = y_end),
               arrow = arrow(length = unit(0.4, "cm")),
               size = 0.75, color = "black") +
  geom_text(data = one_play, aes(x = x, y = y, group = nfl_id, label = jersey_number), color = 'black', size = 4) +
  scale_color_manual(values = c("lightblue", "red"))

