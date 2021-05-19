library(readxl); library(tidyverse); library(rddtools); library(hrbrthemes)

dataset <- read_csv("./Data/analysis/unmatched_sample_analysis.csv") %>%
  select(-1) %>%
  mutate(defw = log(1+Vermogen_deflated)) %>%
  filter(!is.na(defw))

# [Plot of E[Y|X] to also see the discontinuity in outcomes]
p1 <- dataset %>%
  ggplot(aes(x = margin, y = log(1+Vermogen_deflated))) + 
  geom_point() +
  xlab("Margin") + 
  ylab("Wealth") +
  theme_minimal()

# [Plot of E[W|X] to not see a discontinuity in covariates]
# Still to do


# [Density of X (check for manipulation, McGreary test]
dataset <- rdd_data(
        y = defw,
         x = margin,
         data = dataset,
         cutpoint = 0)

bw_ik <- rdd_bw_ik(dataset)
reg_nonpara <- rdd_reg_np(
  rdd_object=dataset, bw=bw_ik)

reg_nonpara
dens_test(reg_nonpara)

hist(dataset$margin)
plot(dataset, h = c(0.01, 0.05, 0.1), nplot = 3)

hoi <- rdd_reg_np(dataset, bw = bw_ik)

plot(hoi, binwidth = 0.01)
summary(hoi)

dens_test(hoi)
