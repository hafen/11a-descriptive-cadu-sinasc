library(seriesclust)
library(dplyr)

library(haven)

sisvan <- read_dta("data/discovered/_raw/sisvanweb/Sisvan_NutritionStatus_0-5yrs_BolsaFamilia_2008-2017.dta")


# scale the monthly median close price so that we are clustering on general shape
d <- nasd16 %>%
  group_by(symbol) %>%
  mutate(close_scl = as.numeric(scale(med_close))) %>%
  select(-company, -med_close)

set.seed(1234)
# k-means clustering with 2, 5, and 9 clusters
km <- get_kmeans(d, x = "month", y = "close_scl", k = c(2, 5, 9))
plot_scree(km)
plot_clust(km, 9)
plot_heat(km, 9, col = "sector")
plot_heat(km, 9, col = "industry", cutoff = 10)