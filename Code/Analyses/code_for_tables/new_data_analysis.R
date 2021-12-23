library(readxl); library(tidyverse); library(hrbrthemes); library(rdrobust); library(modelsummary)
library(ggtext); library(RATest); library(lubridate); library(extraDistr)

# inverse hyperbolic sine transform
ihs <- function(x) {log(x + sqrt(x^2 + 1))}
## Few mutations with the data set
dataset <- read_delim("./Data/analysis/full_sample_analysis_novars.csv", delim=",") %>%
  select(-1) %>%
  janitor::clean_names() %>%
  mutate(defw = log(1+deflated_wealth), defw2 = ihs(deflated_wealth)) %>%
  mutate(across(starts_with("verk_"), ~ if_else(.x > 1, 1, .x))) %>% 
  filter(consequential_election == 1)

firstrents_firsttry <- dataset %>%
  filter(hoeveelste_keer_prob == 1 & hoevaak_gewonnen_verleden == 0)
firstrents_secondtry <- dataset %>%
  filter(hoeveelste_keer_prob == 2 & hoevaak_gewonnen_verleden == 0)
firstrents_pooled <- dataset %>%
  filter(hoevaak_gewonnen_verleden == 0)

secondrents <- dataset %>%
  filter(hoevaak_gewonnen_verleden == 1)
thirdrents <- dataset %>%
  filter(hoevaak_gewonnen_verleden == 2)
fourthrents <- dataset %>%
  filter(hoevaak_gewonnen_verleden == 3)
fifthrents <- dataset %>%
  filter(hoevaak_gewonnen_verleden > 3)

## Incumbency advantages
rdrobust::rdrobust(y = dataset$verk_2_gewonnen, x = dataset$margin) %>% summary()
rdrobust::rdrobust(y = dataset$verk_3_gewonnen, x = dataset$margin) %>% summary()
rdrobust::rdrobust(y = dataset$verk_4_gewonnen, x = dataset$margin) %>% summary()
rdrobust::rdrobust(y = dataset$verk_5_gewonnen, x = dataset$margin) %>% summary()
rdrobust::rdrobust(y = dataset$verk_6_gewonnen, x = dataset$margin) %>% summary()

rdrobust::rdrobust(y = firstrents_firsttry$defw, x = firstrents_firsttry$margin) %>% summary()
rdrobust::rdrobust(y = firstrents_secondtry$defw, x = firstrents_secondtry$margin) %>% summary()

rdrobust::rdrobust(y = secondrents$defw, x = secondrents$margin) %>% summary()
rdrobust::rdrobust(y = thirdrents$defw, x = thirdrents$margin) %>% summary()
rdrobust::rdrobust(y = fourthrents$defw, x = fourthrents$margin) %>% summary()
rdrobust::rdrobust(y = fifthrents$defw, x = fifthrents$margin) %>% summary()
