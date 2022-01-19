#covariate balance tables
library(readxl); library(tidyverse); library(hrbrthemes); library(rdrobust); library(modelsummary)
library(ggtext); library(lubridate)
# calculate att etc. 
source("./Code/Analyses/function_calculate_itt_and_att.R")
source("./Code/Analyses/functions_for_tables.R")
source("./Code/Analyses/code_for_tables/new_data_analysis.R")

# parameters for the table
far <- 0.2
close <- 0.05 

# first rents, first try table
notes <- c("The table contains means for various sets of variables conditioned on the absolute margin being < 0.2 (left panel) and <0.05 (right panel). The first two columns represent the means for subsequent politicians and non-politicians respectively, and the third column shows the p-value of a Welch two-sample t-test. The last column shows the local non-parametric RD estimate, estimated by the procedure in \\\\cite{cattaneo2019practical}. HC-Robust standard errors are shown between brackets. Significance is indicated by *: p < 0.1, **: p < 0.05, ***: p < 0.01.")
knitr::opts_current$set(label = "covbal_firstrents_firsttry")
datasummary(data = firstrents_firsttry,
            align = c("llllllll"),
            formula = 
              (`Rec.: Protestant`=rec_ar) + 
              (`Rec.: Liberal`=rec_lib) + 
              (`Rec.: Socialist`=rec_soc) + 
              (`Rec: Catholic` = rec_kath)+
              (`Lifespan`=lifespan) +
              (`Age at Election`=age_at_election) +
              (`Year of Death`=yod) +
              (`Year of Election`=yoe) + 
              (`Log Turnout`=log(aantal_stemmen_geldig) )+
              (`Log Turnout Previous`=log(turnout_previous_el)) +
              (`Log Population 1859`=log(1+birthplace_pop_1859)) +
              (`Share Protestant`=birthplace_share_prot) +
              (`Share Catholic`=birthplace_share_cath) +
              (`Labor Force Share Agricul.`=birthplace_agri) +
              (`Labor Force Share Industry`=birthplace_indus )+
              (`Taxes Per Capita 1859`=taxespercap_1859) +
              (`Taxes Per Capita 1889`=taxespercap_1889) +
              (`Distance to the Hague`=distance_bp_hag) +
              (`Share Protestant`=district_share_prot) +
              (`Share Catholic`=district_share_cath) +
              (`Labor Force Share Agricul.`=district_agri) +
              (`Labor Force Share Industry`=district_indus)~ (`Politicians`=mean_treatment_far) + 
              (`Non-Politicians`=mean_control_far) + 
              (`p-val.`=p_val_far) + 
              (`Politicians`=mean_treatment_close) + (`Non-Politicians`=mean_control_close) +
              (`p-val.` = p_val_close) + (`RD Estimate (SD)`=get_coef_and_se3), 
            out = "kableExtra",
            output = "latex",
            title = "Covariate Balance - First Attempts - First Stint") %>%
  kableExtra::group_rows("Panel A: Newspaper Recommendations", 1,4) %>%
  kableExtra::group_rows("Panel B: Demographic Characteristics", 5, 8) %>%
  kableExtra::group_rows("Panel C: Election Characteristics", 9, 10) %>%
  kableExtra::group_rows("Panel D: Birthplace Characteristics", 11, 18)  %>%
  kableExtra::group_rows("Panel E: District Characteristics", 19, 22) %>% 
  kableExtra::add_header_above(c(" " = 1, "Margin < 0.2" = 3, "Margin < 0.05" = 3, " " = 1)) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  kableExtra::footnote(general = notes, footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  kableExtra::save_kable("./Tables/covariate_balance_firsttry.tex")

# second rents table
notes <- c("The table contains means for various sets of variables conditioned on the absolute margin being < 0.2 (left panel) and <0.05 (right panel). The first two columns represent the means for subsequent politicians and non-politicians respectively, and the third column shows the p-value of a Welch two-sample t-test. The last column shows the local non-parametric RD estimate, estimated by the procedure in \\\\cite{cattaneo2019practical}. HC-Robust standard errors are shown between brackets. Significance is indicated by *: p < 0.1, **: p < 0.05, ***: p < 0.01.")
knitr::opts_current$set(label = "covbal_secondrents")
datasummary(data = secondrents,
            #align = c("llllllll"),
            formula = 
              (`Rec.: Protestant`=rec_ar) + 
              (`Rec.: Liberal`=rec_lib) + 
              (`Rec.: Socialist`=rec_soc) + 
              (`Rec: Catholic` = rec_kath)+
              (`Lifespan`=lifespan) +
              (`Age at Election`=age_at_election) +
              (`Year of Death`=yod) +
              (`Year of Election`=yoe) + 
              (`Log Turnout`=log(aantal_stemmen_geldig) )+
              (`Log Turnout Previous`=log(turnout_previous_el)) +
              (`Log Population 1859`=log(1+birthplace_pop_1859)) +
              (`Share Protestant`=birthplace_share_prot) +
              (`Share Catholic`=birthplace_share_cath) +
              (`Labor Force Share Agricul.`=birthplace_agri) +
              (`Labor Force Share Industry`=birthplace_indus )+
              (`Taxes Per Capita 1859`=taxespercap_1859) +
              (`Taxes Per Capita 1889`=taxespercap_1889) +
              (`Distance to the Hague`=distance_bp_hag) +
              (`Share Protestant`=district_share_prot) +
              (`Share Catholic`=district_share_cath) +
              (`Labor Force Share Agricul.`=district_agri) +
              (`Labor Force Share Industry`=district_indus)~ (`Politicians`=mean_treatment_far) + 
              (`Non-Politicians`=mean_control_far) + 
              (`p-val.`=p_val_far) + 
              (`Politicians`=mean_treatment_close) + (`Non-Politicians`=mean_control_close) +
              (`p-val.` = p_val_close) + (`RD Estimate (SD)`=get_coef_and_se4), 
            out = "kableExtra",
            output = "latex",
            title = "Covariate Balance - Second Stint") %>%
  kableExtra::group_rows("Panel A: Newspaper Recommendations", 1,4) %>%
  kableExtra::group_rows("Panel B: Demographic Characteristics", 5, 8) %>%
  kableExtra::group_rows("Panel C: Election Characteristics", 9, 10) %>%
  kableExtra::group_rows("Panel D: Birthplace Characteristics", 11, 18)  %>%
  kableExtra::group_rows("Panel E: District Characteristics", 19, 22) %>% 
  kableExtra::add_header_above(c(" " = 1, "Margin < 0.2" = 3, "Margin < 0.05" = 3, " " = 1)) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  kableExtra::footnote(general = notes, footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  kableExtra::save_kable("./Tables/covariate_balance_secondrents.tex")
  
  
# raw descriptive statistics table

knitr::opts_current$set(label = "descriptivestats")
notes <- "This table shows descriptive statistics for all observations. In panel A, I show newspaper recommendations for each major political faction. Panel B discusses demographic characteristics, and panel C discusses characteristics related to elections. Panels D and E contain birthplace and district characteristics. Panel F contains ex-post variables and Panel G and H contain several variables related to party and career characteristics."
datasummary(data = dataset %>%
              mutate(lib = if_else(party_category=="liberal", 1, 0),
                     prot = if_else(party_category=="protestant", 1, 0),
                     cath = if_else(party_category=="catholic", 1, 0)),
            formula = 
              (`Rec.: Protestant`=rec_ar) + 
              (`Rec.: Liberal`=rec_lib) + 
              (`Rec.: Socialist`=rec_soc) + 
              (`Rec: Catholic` = rec_kath)+
              (`Lifespan`=lifespan) +
              (`Age at Election`=age_at_election) +
              (`Year of Death`=yod) +
              (`Year of Election`=yoe) + 
              (`Log Turnout`=log(aantal_stemmen_geldig) )+
              (`Log Turnout Previous`=log(turnout_previous_el)) +
              (`Log Population 1859`=log(1+birthplace_pop_1859)) +
              (`Share Protestant`=birthplace_share_prot) +
              (`Share Catholic`=birthplace_share_cath) +
              (`Labor Force Share Agricul.`=birthplace_agri) +
              (`Labor Force Share Industry`=birthplace_indus )+
              (`Taxes Per Capita 1859`=taxespercap_1859) +
              (`Taxes Per Capita 1889`=taxespercap_1889) +
              (`Distance to the Hague`=distance_bp_hag) +
              (`Share Protestant`=district_share_prot) +
              (`Share Catholic`=district_share_cath) +
              (`Labor Force Share Agricul.`=district_agri) +
              (`Labor Force Share Industry`=district_indus) +
              (`Log Deflated Wealth`=defw) +
              (`Age of Death`=age_of_death) + 
              (`Elected After ARP`=election_after_arp) +
              (`Elected After RK`=election_after_rk ) +
              (`Elected After Lib`=election_after_lib) + 
              (`Liberal`=lib) + 
              (`Protestant`=prot) +
              (`Catholic`=cath) +
              (`Profession: Business`=prof_business) +
              (`Profession: Mayor`=prof_politics) +
              (`Profession: Colonial`=prof_colonial) ~ Mean + SD + Min + Max + N,
            out = "kableExtra",
            output="latex",
            title = "Descriptive Statistics") %>%
  kableExtra::group_rows("Panel A: Newspaper Recommendations", 1,4) %>%
  kableExtra::group_rows("Panel B: Demographic Characteristics Politicians", 5, 8) %>%
  kableExtra::group_rows("Panel C: Election Characteristics", 9, 11) %>%
  kableExtra::group_rows("Panel D: Birthplace Characteristics", 12, 18)  %>%
  kableExtra::group_rows("Panel E: District Characteristics", 19, 22) %>% 
  kableExtra::group_rows("Panel F: Ex-Post Characteristics", 23, 24) %>%
  kableExtra::group_rows("Panel G: Party and Career Characteristics", 25, 30) %>%
  kableExtra::group_rows("Panel H: Career Paths", 31, 33) %>% 
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  kableExtra::footnote(general = notes, footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  kableExtra::save_kable("./Tables/descriptivestats_trad.tex")
  