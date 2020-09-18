## Data Division

# Loading Packages --------------------------------------------------------

library(tidyverse)
library(skimr)
library(janitor)
library(broom)
set.seed(42)


# Initial Data Division ---------------------------------------------------

bullpen_dat <- read_csv(file = "Data/Processed/Yankees Bullpen Data.csv")

bullpen_eda <- bullpen_dat %>% sample_frac(0.3)
bullpen_bldg <- bullpen_dat %>% setdiff(bullpen_eda)


# Creating CSVs for Split Up Data -----------------------------------------

#Strike Data
write.csv(bullpen_eda, file = "EDA Data.csv")
write.csv(bullpen_bldg, file = "Building Data.csv")

