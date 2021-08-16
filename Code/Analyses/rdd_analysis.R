library(readxl); library(tidyverse); library(rddtools); library(hrbrthemes)

dataset <- read_csv("./Data/analysis/unmatched_sample_with_vars.csv") %>%
  select(-1) %>%
  mutate(defw = log(1+Vermogen_deflated),
         distrverk = str_c(District, "-", Verkiezingdatum)) %>%
  filter(!is.na(defw)) %>%
  filter(margin > -0.15) # Changing this helps the density asymmetry

test <- dataset %>%
  filter(is.na(`b1-nummer`)) %>%
  select(distrverk) %>%
  pull()

dataset <- dataset %>%
  filter(is.element(distrverk, test))

# [Plot of E[Y|X] to also see the discontinuity in outcomes]
p1 <- dataset %>%
  ggplot(aes(x = margin, y = defw)) + 
  geom_point() +
  xlab("Margin") + 
  ylab("Wealth") +
  theme_minimal()

# [Plot of E[W|X] to not see a discontinuity in covariates]
# Still to do


# [Density of X (check for manipulation, McGreary test]
dataset_analysis <- rdd_data(
        y = defw,
         x = margin,
         data = dataset,
         cutpoint = 0)

bw_ik <- rdd_bw_ik(dataset_analysis)

model1 <- rdd_reg_np(
  rdd_object=dataset_analysis, bw=bw_ik)

model2<- rdd_reg_lm(rdd_object = dataset_analysis, order = 2)

dens_test(model1)
dens_test(model2)

plot(dataset_analysis, h = c(0.005, 0.05, 0.1), nplot = 3)



