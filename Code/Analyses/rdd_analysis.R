library(readxl); library(tidyverse); library(rddtools); library(hrbrthemes); library(rdrobust)

## Few mutations with the data set
dataset <- read_csv("./Data/analysis/unmatched_sample_with_vars.csv") %>%
  select(-1) %>%
  mutate(defw = log(1+Vermogen_deflated),
         distrverk = str_c(District, "-", Verkiezingdatum),
         politician_dummy = if_else(!is.na(`b1-nummer`), 1, 0)) %>%
  filter(!is.na(defw)) %>%
  filter(margin > -0.20) %>% 
  filter(lubridate::dmy(Sterfdatum) < lubridate::dmy('01-01-1928'))
  ## Changing the last two lines helps alleviate the density asymmetry
  ## Making the max. margin equal
  ## Making the circumstances equal (last line)

## Match on election
elections_for_ctrl_group <- dataset %>%
  filter(is.na(`b1-nummer`)) %>%
  select(distrverk) %>%
  pull()

dataset <- dataset %>%
  filter(is.element(distrverk, elections_for_ctrl_group))


test <- rdplot(y=dataset$defw, x=dataset$margin, binselect="espr", ci=95, 
       title="RD Plot: U.S. Senate Election Data", 
       y.label="Vote Share in Election at time t+2",
       x.label="Vote Share in Election at time t") 

summary(rdrobust(y=dataset$defw, x=dataset$margin, covs = cbind(dataset$lifespan,
                                                                dataset$before,
                                                                dataset$after,
                                                                dataset$amount_votes),
        all = TRUE))



# [Plot of E[Y|X] to also see the discontinuity in outcomes]
p1 <- dataset %>%
  ggplot(aes(x = margin, y = defw)) + 
  geom_point() +
  xlab("Margin") + 
  ylab("Log(Wealth)") +
  theme_minimal()

# [Plot of E[W|X] to not see a discontinuity in covariates]
# Still to do
p2 <- dataset %>%
  ggplot(aes(x = margin, y = politician_dummy)) +
  geom_point() +
  xlab("Margin") +
  ylab("Prob(Politician)") +
  theme_minimal()

# [Density of X (check for manipulation, McGreary test]
dataset_analysis <- rdd_data(
        y = defw,
         x = margin,
         data = dataset,
         cutpoint = 0, covar = data.frame(lifespan, before, after))

bw_ik <- rdd_bw_ik(dataset_analysis)

model1 <- rdd_reg_np(
  rdd_object=dataset_analysis, bw=bw_ik)

model2<- rdd_reg_lm(rdd_object = dataset_analysis)

dens_test(model1)
dens_test(model2)

plot(dataset_analysis, h = c(0.005, 0.05, 0.1), nplot = 3)

plotSensi(model1, from = 0, to = 0.25, by = 0.02)

