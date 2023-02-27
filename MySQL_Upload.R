library(RMySQL)
library(bbd)
library(baseballr)
library(tidyverse)

conn <- dbConnect(MySQL(), dbname = "Statcast",
                  user = "root", password = ":)")

dbWriteTable(conn, name = "full_events", value = all_events,
              row.names = FALSE)


all_2017 = bbd::statcast(start = "2017-04-02",
                         end = "2017-10-01",
                         process = TRUE,
                         names = TRUE,
                         verbose = TRUE)

all_2018 = bbd::statcast(start = "2018-03-29",
                         end = "2018-10-01",
                         process = TRUE,
                         names = TRUE,
                         verbose = TRUE)

all_2019 = bbd::statcast(start = "2019-03-20",
                         end = "2019-09-29",
                         process = TRUE,
                         names = TRUE,
                         verbose = TRUE)

all_2020 = bbd::statcast(start = "2020-07-23",
                      end = "2020-09-27",
                      process = TRUE,
                      names = TRUE,
                      verbose = TRUE)

all_2021 = bbd::statcast(start = "2021-04-01",
                         end = "2021-10-03",
                         process = TRUE,
                         names = TRUE,
                         verbose = TRUE)

all_2022 = bbd::statcast(start = "2022-04-07",
                         end = "2022-10-05",
                         process = TRUE,
                         names = TRUE,
                         verbose = TRUE)


all_dat <- rbind(all_2017, all_2018, all_2019, all_2020, all_2021, all_2022)


dbWriteTable(conn, name = "pitches", value = all_dat,
             append = TRUE, row.names = FALSE)

height_2017 = mlb_sports_players(sport_id = 1, season = 2017) %>%
  filter(primary_position_abbreviation == "P") %>%
  select(full_name, height, primary_position_abbreviation, pitch_hand_code)

height_2018 = mlb_sports_players(sport_id = 1, season = 2018) %>%
  filter(primary_position_abbreviation == "P") %>%
  select(full_name, height, primary_position_abbreviation, pitch_hand_code)

height_2019 = mlb_sports_players(sport_id = 1, season = 2019) %>%
  filter(primary_position_abbreviation == "P") %>%
  select(full_name, height, primary_position_abbreviation, pitch_hand_code)

height_2020 = mlb_sports_players(sport_id = 1, season = 2020) %>%
  filter(primary_position_abbreviation == "P") %>%
  select(full_name, height, primary_position_abbreviation, pitch_hand_code)

height_2021 = mlb_sports_players(sport_id = 1, season = 2021) %>%
  filter(primary_position_abbreviation == "P") %>%
  select(full_name, height, primary_position_abbreviation, pitch_hand_code) 

height_2022 = mlb_sports_players(sport_id = 1, season = 2022) %>%
  filter(primary_position_abbreviation == "P") %>%
  select(full_name, height, primary_position_abbreviation, pitch_hand_code)

all_height <- rbind(height_2017, height_2018, height_2019, height_2020, height_2021, height_2022) %>%
  unique()

dbWriteTable(conn, name = "height", value = all_height,
            overwrite = TRUE, row.names = FALSE)


