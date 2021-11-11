library(readxl); library(tidyverse); library(hrbrthemes); 
library(rdrobust); library(modelsummary)

# Parameters

ihs <- function(x) {log(x + sqrt(x^2 + 1))}
## Few mutations with the data set
dataset <- read_delim("./Data/analysis/unmatched_sample_with_vars.csv", delim=",") %>% # %>% #"./Data/analysis/unmatched_sample_with_vars.csv") %>%
  select(-1) %>%
  mutate(defw = log(1+Vermogen_deflated),
         defw2 = ihs(Vermogen_deflated),
         distrverk = str_c(District, "-", Verkiezingdatum),
         lifespan = lifespan/365,
         politician_dummy = if_else(!is.na(`b1-nummer`), 1, 0),
         politician_indic = if_else(!is.na(`b1-nummer`), "Politician", "Non-Politician"),
         taxespercap_1859 = if_else(is.na(taxespercap_1859), 0.5, taxespercap_1859),
         taxespercap_1889 = if_else(is.na(taxespercap_1889), 0.5, taxespercap_1889),
         district_share_prot = district_prot / (district_prot + district_cath + district_ov),
         district_share_cath = district_cath / (district_prot + district_cath + district_ov),
         rec_ar = case_when(stringr::str_detect(party_election, "AR|VA|NC|NH") ~ 1, 
                            is.na(party_election) ~ 0,
                            TRUE~  0),
         rec_kath = case_when(stringr::str_detect(party_election, "Ka|KD|DT") ~ 1,
                              is.na(party_election) ~ 0,
                              TRUE~ 0),
         rec_lib = case_when(stringr::str_detect(party_election, "Lib|VL|AH") ~ 1,
                             is.na(party_election) ~ 0,
                             TRUE~ 0),
         rec_soc = case_when(stringr::str_detect(party_election, "Rad|Soc|SDAP|SDP") ~ 1,
                             is.na(party_election) ~ 0,
                             TRUE~ 0),
         elec_type_alg = if_else(election_type == "algemeen", 1, 0),
         elec_type_else = if_else(election_type != "algemeen", 1, 0),
         yod = as.numeric(stringr::str_extract(Sterfdatum,"\\d{4}$")),
         yoe = as.numeric(stringr::str_extract(Verkiezingdatum, "\\d{4}$"))
  ) %>%
  filter(!is.na(defw2)) #%>%
  #filter(yoe < 1905)


# show the rd coefficient of variable on margin
get_coef_and_se2 <- function(variable){
  #var <- deparse(substitute(variable))
  regression_output <- rdrobust(y = variable, x = dataset[['margin']])
  
  t_stat <- as.numeric(regression_output['coef'][[1]][1])/as.numeric(regression_output['se'][[1]][3]) %>%
    abs()
  
  coef <- regression_output['coef'][[1]][1] %>%
    round(3) %>%
    format(nsmall=3)
  se <- regression_output['se'][[1]][3] %>%
    round(3) %>%
    format(nsmall=3)
  
  if (t_stat > 1.7) {
  paste(coef, " ", "(", se, ")","\\*", sep = "") }
  else { paste(coef, " ", "(", se, ")", sep = "")}
}

far <- 0.2
close <- 0.05 

mean_treatment_far <- function(x){ x[dataset$politician_dummy == 1 & abs(dataset$margin) < far] %>%
    mean(na.rm= TRUE)}
sd_treatment_far <- function(x){ x[dataset$politician_dummy == 1 & abs(dataset$margin) < far] %>%
    sd(na.rm= TRUE)}
mean_control_far <- function(x){ x[dataset$politician_dummy == 0 & abs(dataset$margin) < far] %>%
    mean(na.rm= TRUE)}
sd_control_far <- function(x){ x[dataset$politician_dummy == 1 & abs(dataset$margin) < far] %>%
    sd(na.rm= TRUE)}
mean_treatment_close <- function(x){ x[dataset$politician_dummy == 1 & abs(dataset$margin) < close] %>%
    mean(na.rm= TRUE)}
sd_treatment_close <- function(x){ x[dataset$politician_dummy == 1 & abs(dataset$margin) < close] %>%
    sd(na.rm= TRUE)}
mean_control_close <- function(x){ x[dataset$politician_dummy == 0 & abs(dataset$margin) < close] %>%
    mean(na.rm= TRUE)}
sd_control_close <- function(x){ x[dataset$politician_dummy == 1 & abs(dataset$margin) < close] %>%
    sd(na.rm= TRUE)}

# todo: create a new column with p-values
## Find the element to extract the p-value
p_val_close <- function(x) {t.test(x[abs(dataset$margin) < close] ~ dataset$politician_dummy[abs(dataset$margin) < close])}
p_val_far <- function(x) {t.test(x[abs(dataset$margin) < far] ~ dataset$politician_dummy[abs(dataset$margin) < far])}
# see ?datasummary for new columns to find out how to specify where the new column should be
datasummary(data = dataset,
            formula = 
              rec_ar + 
              rec_lib + 
              rec_soc + 
              rec_kath +
              lifespan +
              age_at_election +
              yod +
              age_of_death + 
              yoe + 
              howmany_before_alg +
              log(turnout) +
              log(turnout_previous_el) +
              log(1+birthplace_pop_1859) +
              birthplace_share_cath +
              birthplace_share_prot +
              birthplace_agri +
              birthplace_indus +
              taxespercap_1859 +
              taxespercap_1889 +
              district_share_prot +
              district_share_cath +
              district_agri +
              district_indus ~ mean_treatment_far  + mean_control_far + p_value_far + mean_treatment_close + mean_control_close + get_coef_and_se2, 
            out = "kableExtra") %>%
  kableExtra::group_rows("Panel A: Newspaper Recommendations", 1,4) %>%
  kableExtra::group_rows("Panel B: Pre-Election Demographic Characteristics", 5, 8) %>%
  kableExtra::group_rows("Panel C: Election Characteristics", 9, 12) %>%
  kableExtra::group_rows("Panel D: Birthplace Characteristics", 13, 19)  %>%
  kableExtra::group_rows("Panel E: District Characteristics", 20, 23) %>% 
  kableExtra::add_header_above(c(" " = 1, "Margin < 0.2" = 2, "Margin < 0.05" = 2, " " = 1)) %>%
  kableExtra::footnote(general = "hoisdfjaosdfjaosidfjaiosjfadsjfaso", threeparttable = TRUE)

rdrobust::rdrobust(y= dataset$rec_kath, x = dataset$margin) -> test

rdrobust::rdrobust(y= dataset$defw2, x = dataset$margin) %>%
  summary()

rdrobust::rdbwselect(y = dataset$defw, x = dataset$margin, bwselect = 'msetwo') %>%
  summary()
