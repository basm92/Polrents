#regression_tables_suffrage_and_turnout
library(readxl); library(tidyverse); library(hrbrthemes); library(rdrobust); library(modelsummary)
library(ggtext); library(lubridate)
# calculate att etc. 
source("./Code/Analyses/function_calculate_itt_and_att.R")
source("./Code/Analyses/functions_for_tables.R")
source("./Code/Analyses/code_for_tables/new_data_analysis.R")



# before and after suffrage extensions
before <- firstrents_pooled %>%
  filter(verkiezingdatum < dmy("27-03-1888"))

after <- firstrents_pooled %>%
  filter(verkiezingdatum > dmy("26-03-1888"))

rdrobust(before$defw, before$margin) %>% summary()
rdrobust(after$defw, after$margin) %>% summary()

before2 <- firstrents_pooled %>%
  filter(verkiezingdatum < dmy("15-06-1897"))
after2 <- firstrents_pooled %>%
  filter(verkiezingdatum > "14-06-1897")

rdrobust(before2$defw, before2$margin) %>% summary()
rdrobust(after2$defw, after2$margin) %>% summary()

# table

# figure moving average


# high versus low turnout

dataset <- dataset %>%
  mutate(perc_diff_turn = diff_turn/district_pop_1889)

dataset_high <- dataset %>% filter(perc_diff_turn > 0)
dataset_low <- dataset %>% filter(perc_diff_turn < 0)

