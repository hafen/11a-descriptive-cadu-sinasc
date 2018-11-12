library(seriesclust)
library(dplyr)

library(haven)

load("data/discovered/_raw/sisvanweb/geo_data.Rdata")


d <- muni_data %>%
  select(muni_code, year, stunting, state_code, region_code) %>%
  arrange(muni_code, year)

set.seed(1234)
# k-means clustering with 2, 5, and 9 clusters
km <- get_kmeans(d, x = "year", y = "stunting", k = c(2, 5, 9, 16))
plot_scree(km)
plot_clust(km, 9)
plot_clust(km, 16)
plot_heat(km, 16, col = "region_code")
plot_heat(km, 16, col = "state_code")
plot_heat(km, 16, col = "state_code", cutoff = 10)


heat <- plot_heat(km, 16, col = "region_code", interactive = FALSE,
  display_numbers = TRUE, cutree_cols = 4, cutree_rows = 3,
  annotation_labs = c("A", "B", "C", "D"))
heat

cents <- get_centroid_data(km, 16)
plot_centroid_groups(cents, heat)

get_centroid_data(x, k, use_median = FALSE)

# scale the monthly median close price so that we are clustering on general shape
d <- nasd16 %>%
  group_by(symbol) %>%
  mutate(close_scl = as.numeric(scale(med_close))) %>%
  select(-company, -med_close)

km <- get_kmeans(d, x = "month", y = "close_scl", k = c(2, 5, 9))
