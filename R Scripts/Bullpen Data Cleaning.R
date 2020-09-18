## Data Cleaning


# Loading Packages --------------------------------------------------------

library(tidyverse)
library(skimr)
library(janitor)
library(lubridate)
library(broom)

bullpen_dat <- read_csv("Yankee Bullpen Data.csv")
bullpen_dat %>% 
  skim()


# Adding a Dummy Variable For Each Pitch ----------------------------------

bullpen_dat <- bullpen_dat %>% 
  mutate(four_seam = ifelse(pitch_name == "4-Seam Fastball", 1, 0),
       curve_ball = ifelse(pitch_name == "4-Seam Fastball", 1, 0),
       cutter    = ifelse(pitch_name == "Cutter", 1, 0),
       unnamed_pitch = ifelse(pitch_name == "", 1, 0),
       sinker = ifelse(pitch_name == "Sinker", 1, 0),
       slider = ifelse(pitch_name == "4-Seam Fastball", 1, 0),
       changeup = ifelse(pitch_name == "Changeup", 1, 0),
       two_seam = ifelse(pitch_name == "2-Seam Fastball", 1, 0),
       split_finger = ifelse(pitch_name == "Split Finger", 1, 0),
       knuckle_curve = ifelse(pitch_name == "Knuckle Curve", 1, 0),
       forkball = ifelse(pitch_name == "Forkball", 1, 0),
       eephus = ifelse(pitch_name == "Eephus", 1, 0),
       pitch_out = ifelse(pitch_name == "Pitch Out", 1, 0), 
       screwball = ifelse(pitch_name == "Screwball", 1, 0),
       knuckle_ball = ifelse(pitch_name == "Knuckle Ball", 1, 0),
       strike = ifelse(type == "S", 1, 0),
       up_zone = ifelse((zone == 1)|(zone == 2)|(zone == 3)|(zone == 11)|(zone == 12), 1, 0),
       mid_zone = ifelse((zone == 4)|(zone == 5)|(zone == 6), 1, 0),
       down_zone = ifelse((zone == 7)|(zone == 8)|(zone == 9)|(zone == 13)|(zone == 14), 1, 0)) 
bullpen_dat %>% 
  select(player_name) %>% 
  unique()

# Adding Strike, Swinging Strike & Hit Indicator --------------------------

bullpen_dat <- bullpen_dat %>% 
  mutate(hit = ifelse((events == "single" | events == "double" | events == "triple" | events == "home_run"), 1, 0),
         strike = ifelse(type == "S", 1, 0),
         swinging_strike = ifelse((description == "swinging_strike" | description == "swinging_strike_blocked" | description == "missed_bunt"), 1, 0),
         chapman = ifelse(player_name == "Aroldis Chapman", 1, 0),
         betances = ifelse(player_name == "Dellin Betances", 1, 0),
         britton = ifelse(player_name == "Zach Britton", 1, 0),
         green = ifelse(player_name == "Chad Green", 1, 0),
         ottavino = ifelse(player_name == "Adam Ottavino", 1, 0))

bullpen_dat %>% 
  summarize(prop_ss = mean(swinging_strike),
            prop_str = mean(strike),
            prop_swingvcall = prop_ss / prop_str)


# Adding Pitch Count Variable ---------------------------------------------

bullpen_dat <- bullpen_dat %>% 
  arrange(game_date) %>%
  mutate(id = row_number()) %>% 
  arrange(game_pk, id) %>% 
  group_by(game_pk, player_name) %>% 
  mutate(pitch_count = row_number())


# Removing Variables Without Data Inputted -------------------------------

bullpen_dat <- bullpen_dat %>% 
  select(-c(break_angle_deprecated, break_length_deprecated, spin_dir, spin_rate_deprecated, tfs_deprecated, tfs_zulu_deprecated, umpire))

# Making CSV --------------------------------------------------------------

write_csv(bullpen_dat, "Yankees Bullpen Data.csv")


