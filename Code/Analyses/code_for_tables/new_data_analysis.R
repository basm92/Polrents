library(readxl); library(tidyverse); library(hrbrthemes); library(rdrobust); library(modelsummary)
library(ggtext); library(lubridate)
# calculate att etc. 
source("./Code/Analyses/function_calculate_itt_and_att.R")
source("./Code/Analyses/functions_for_tables.R")

# inverse hyperbolic sine transform
ihs <- function(x) {log(x + sqrt(x^2 + 1))}
## Few mutations with the data set
dataset <- read_delim("./Data/analysis/full_sample_analysis_allvars.csv", delim=",") %>%
  select(-1) %>%
  janitor::clean_names() %>%
  mutate(defw = log(1+deflated_wealth), defw2 = asinh(deflated_wealth)) %>%
  mutate(across(starts_with("verk_"), ~ if_else(.x > 1, 1, .x))) %>% 
  filter(consequential_election == 1)

dataset <- dataset %>%
  mutate(distrverk = str_c(district, "-", verkiezingdatum),
         lifespan = lifespan/365,
         politician_dummy = if_else(!is.na(b1_nummer), 1, 0),
         politician_indic = if_else(!is.na(b1_nummer), "Politician", "Non-Politician"),
         aantal_stemmen_geldig = as.numeric(aantal_stemmen_geldig),
         #taxespercap_1859 = if_else(is.na(taxespercap_1859), 0.5, taxespercap_1859),
         #taxespercap_1889 = if_else(is.na(taxespercap_1889), 0.5, taxespercap_1889),
         district_share_prot = district_prot / (district_prot + district_cath + district_ov),
         district_share_cath = district_cath / (district_prot + district_cath + district_ov),
         rec_ar = case_when(stringr::str_detect(aanbevolen_door, "AR|VA|NC|NH") ~ 1, 
                            is.na(aanbevolen_door) ~ 0,
                            TRUE~  0),
         rec_kath = case_when(stringr::str_detect(aanbevolen_door, "Ka|KD|DT") ~ 1,
                              is.na(aanbevolen_door) ~ 0,
                              TRUE~ 0),
         rec_lib = case_when(stringr::str_detect(aanbevolen_door, "Lib|VL|AH") ~ 1,
                             is.na(aanbevolen_door) ~ 0,
                             TRUE~ 0),
         rec_soc = case_when(stringr::str_detect(aanbevolen_door, "Rad|Soc|SDAP|SDP") ~ 1,
                             is.na(aanbevolen_door) ~ 0,
                             TRUE~ 0),
         elec_type_alg = if_else(type == "algemeen", 1, 0),
         elec_type_else = if_else(type != "algemeen", 1, 0),
         yod = as.numeric(stringr::str_extract(stefdatum,"\\d{4}")),
         yoe = as.numeric(stringr::str_extract(verkiezingdatum, "\\d{4}"))
  ) 

## Create different datasets

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

