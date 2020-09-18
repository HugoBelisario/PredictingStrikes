## Exploratory Data Analysis for Strikes


# Loading Packages --------------------------------------------------------

library(tidyverse)
library(skimr)
library(broom)
library(GGally)

## Importing EDA Data
eda_dat <- read_csv(file = "Data/Processed/EDA Data.csv") %>% 
  mutate(release_spin_rate = as.numeric(release_spin_rate)) %>% 
  na.omit()
eda_dat %>%
  skim()


# Overall Strike Data -----------------------------------------------------

eda_dat %>%
  select(strike) %>% 
  summarize(prop = mean(strike))


# By Pitcher --------------------------------------------------------------

eda_dat %>%
  select(strike, player_name) %>% 
  group_by(player_name) %>% 
  summarize(prop = mean(strike))

# Finding out what proportion of the time certain pitches are strikes -----

##Table
eda_dat %>% 
  select(pitch_name, strike) %>% 
  group_by(pitch_name) %>% 
  summarize(prop = mean(strike)) %>% 
  arrange(-prop)

## Plotting Data
eda_dat %>% 
  select(strike, pitch_name) %>% 
  group_by(pitch_name) %>% 
  summarize(prop = mean(strike)) %>% 
  arrange(prop) %>% 
  ggplot(aes(x = reorder(pitch_name, prop), y = prop)) +
  geom_bar(stat = "identity") + 
  coord_flip() +
  xlab("Pitch") +
  ylab("Proportion Strikes")

#Clearly chances of a hit are different depending on what pitch is thrown, pitch type worth looking into

# Spin Rate ---------------------------------------------------------------

## Spin Rate by pitch - breaking balls spin more
eda_dat %>% 
  select(release_spin_rate, pitch_name) %>% 
  mutate(release_spin_rate = as.numeric(release_spin_rate, na.rm = TRUE)) %>% 
  group_by(pitch_name) %>% 
  summarize(prop = mean(release_spin_rate)) %>% 
  arrange(-prop)

## How it affects strikes

eda_dat %>% 
  select(release_spin_rate, strike) %>%
  mutate(strikeYN = ifelse(strike == 1, "Y", "N")) %>% 
  ggplot(aes(x = release_spin_rate)) +
  geom_freqpoly(mapping = aes(color = strikeYN), bins = 50) +
  ylab("Pitches")

## Higher spin rates are more likely to be strikes

# Speed -------------------------------------------------------------------

## Speed by pitch
eda_dat %>% 
  select(effective_speed, release_speed, pitch_name) %>% 
  group_by(pitch_name) %>% 
  summarize(prop_eff = mean(effective_speed, na.rm = TRUE),
            prop_rel = mean(release_speed, na.rm = TRUE)) %>% 
  arrange()

### Effective and release pretty similar, gonna use effective b/c from batters perspective

eda_dat %>% 
  select(effective_speed, strike) %>%
  mutate(strikeYN = ifelse(strike == 1, "Y", "N")) %>% 
  ggplot(aes(x = effective_speed)) +
  geom_freqpoly(mapping = aes(color = strikeYN), bins = 50) +
  ylab("Pitches")


## Higher speeds are more likely to be strikes


# Location ----------------------------------------------------------------

eda_dat %>% 
  select(zone, strike) %>% 
  group_by(zone) %>% 
  summarize(prop = mean(strike)) %>%
  arrange(-prop)

eda_dat %>% 
  select(zone, strike) %>% 
  group_by(zone) %>% 
  summarize(prop = mean(strike)) %>%
  ggplot(aes(x = zone, y = prop)) +
  geom_bar(stat = "identity")

eda_dat %>%
  select(up_zone, mid_zone, down_zone, strike) %>% 
  gather(key = location, value = pitches, -strike) %>% 
  filter(pitches == 1) %>% 
  group_by(location) %>% 
  summarize(prop = mean(strike)) %>% 
  arrange(-prop)

eda_dat %>%
  select(up_zone, mid_zone, down_zone, strike) %>% 
  gather(key = location, value = pitches, -strike) %>% 
  filter(pitches == 1) %>% 
  group_by(location) %>% 
  summarize(prop = mean(strike)) %>% 
  ggplot(aes(x = location, y = prop)) +
  geom_bar(stat = "identity")

## Pitches down the middle are much more likely to be strikes than any other zone


# Game Conditions ---------------------------------------------------------

## Home vs. Away
eda_dat %>% 
  select(inning_topbot, strike) %>% 
  group_by(inning_topbot) %>% 
  summarize(prop = mean(strike)) %>%
  arrange(-prop)

### More strikes by home team, does not seem super significant though

## Count
eda_dat %>% 
  select(balls, strikes, strike) %>% 
  group_by(balls, strikes) %>% 
  summarize(prop = mean(strike)) %>% 
  filter(balls < 4) %>% 
  arrange(-prop)

### Pitchers are most likely to throw a strike if the count is full

## Count with outs
eda_dat %>% 
  select(balls, strikes, outs_when_up, strike) %>% 
  group_by(balls, strikes, outs_when_up) %>% 
  summarize(prop = mean(strike)) %>% 
  filter(balls < 4) %>% 
  arrange(-prop)

## Outs
eda_dat %>% 
  select(outs_when_up, strike) %>% 
  group_by(outs_when_up) %>% 
  summarize(prop = mean(strike)) %>% 
  arrange(-prop)

### Pitchers throw best with one out recorded, perhaps because they are warmed up, but not pressured to end the inning

## Inning
eda_dat %>% 
  select(inning, strike) %>% 
  group_by(inning) %>% 
  summarize(prop = mean(strike)) %>% 
  arrange(-prop)

eda_dat %>% 
  mutate(extras = ifelse(inning > 9, 1, 0)) %>% 
  select(inning, strike, extras) %>% 
  group_by(extras) %>% 
  summarize(prop = mean(strike)) %>% 
  arrange(-prop)

### There seem to be way more strikes in extras than in regulation, inning wise there is no clear trend

## At bat number
eda_dat %>% 
  select(at_bat_number, strike) %>%
  filter(at_bat_number < 100) %>% 
  group_by(at_bat_number) %>% 
  summarize(prop = mean(strike)) %>% 
  ggplot(aes(x = at_bat_number, y = prop)) +
  geom_line(stat = "identity")

## When do they pitch?
eda_dat %>%
  group_by(inning) %>% 
  ggplot(aes(x = inning)) +
  geom_bar()
  

## Most variance earlier and later in the game, but they also dont pitch as much then

## Score
eda_dat %>% 
  mutate(field_lead = fld_score - bat_score) %>% 
  select(field_lead, strike) %>%
  group_by(field_lead) %>% 
  summarize(prop = mean(strike)) %>%  
  ggplot(aes(x = field_lead, y = prop)) +
  geom_bar(stat = "identity")

eda_dat %>% 
  mutate(field_lead = fld_score - bat_score) %>% 
  select(field_lead, strike) %>%
  filter(strike == 1) %>% 
  ggplot(aes(x = field_lead)) +
  geom_histogram()


### These guys pitch best when the score is close, and they also pitch the most then

### Pitcher throw vs. hitter stance
eda_dat %>% 
  select(p_throws, stand, strike) %>% 
  group_by(p_throws, stand) %>% 
  summarize(prop = mean(strike)) %>% 
  arrange(-prop)

##Pitching against a batter with a dominant stace opposite of the pitchers throwing arm are better combos

# Pitch Count/Fatigue -----------------------------------------------------

eda_dat %>% 
  select(strike, pitch_count) %>% 
  group_by(pitch_count) %>% 
  summarize(prop = mean(strike)) %>% 
  arrange(-prop) %>% 
  ggplot(aes(x = pitch_count, y = prop)) +
  geom_line(stat = "identity")

eda_dat %>% 
  ggplot(aes(x = pitch_count)) +
  geom_bar(bins = 30)
## Pretty consistent till about 20ish pitches, but also dont usually pitch more than about 15 

## Important factors seem to be pitch type, spin rate, speed, zone, count, pitcher throws vs. hitter stance

