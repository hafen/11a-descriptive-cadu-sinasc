library(seriesclust)
library(dplyr)

library(haven)

load("data/discovered/_raw/sisvanweb/geo_data.Rdata")

mc <- brazilgeo::br_muni_codes
mc$muni_code <- substr(mc$muni_code, 1, 6)

muni_data <- left_join(select(muni_data, -state_code), mc)

muni_data2 <- filter(muni_data, n_haz >= 100 & n_whz >= 100 & n_bmi >= 100)

muni_data2 <- filter(muni_data2, !is.infinite(wh_stunting_ratio))

muni_data2 <- muni_data2 %>%
  group_by(muni_code) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n == 10) %>%
  select(-n)


get_clusts <- function(var, k = 2:10) {
  dd <- muni_data2 %>%
    select(muni_code, year, !! sym(var), state_name, region_code) %>%
    arrange(muni_code, year)

  if (var == "wh_stunting_ratio")
    dd$wh_stunting_ratio <- log10(dd$wh_stunting_ratio)

  set.seed(1234)
  get_kmeans(dd, x = "year", y = var, k = k)
}


vrs <- c(
  "stunting",
  "overweight_wh",
  "wh_stunting_ratio",
  "wasting")
ylbs <- c(
  "Percentage of stunted children 0-5y",
  "Percentage of overweight (based on WHZ) childen 0-5y",
  "log base 10 pverweight (WHZ) / stunting ratio",
  "Percentage of wasted children 0-5y")

km <- list()
for (vr in vrs)
  km[[vr]] <- get_clusts(vr)

for (ii in seq_along(vrs)) {
  vr <- vrs[ii]
  dir.create(file.path("results/sisvan_cluster/clusters", vr))
  message(vr)
  for (k in 2:10) {
    message(k)
    png(file.path("results/sisvan_cluster/clusters", vr,
    paste0(k, ".png")), width = 10 * 150, height = 7 * 150, res = 150)
    print(plot_clust(km[[vr]], k, layout = c(k, 1), ylab = ylbs[ii]))
    dev.off()
  }
}


plot_clust(km$stunting, 5, layout = c(5, 1),
  ylab = "asdf")
plot_clust(km$stunting, 9, layout = c(9, 1))

plot_scree(km$wasting)
plot_clust(km$wasting, 5, layout = c(5, 1))
plot_clust(km$wasting, 9, layout = c(9, 1))

plot_clust(km$overweight_wh, 5, layout = c(5, 1))
plot_clust(km$overweight_wh, 9, layout = c(9, 1))

plot_clust(km$wh_stunting_ratio, 3, layout = c(3, 1))
plot_clust(km$wh_stunting_ratio, 5, layout = c(5, 1))
plot_clust(km$wh_stunting_ratio, 9, layout = c(9, 1))

plot_clust(km, 5, layout = c(5, 1))
plot_clust(km, 9, layout = c(9, 1))
plot_clust(km, 16, layout = c(16, 1))
plot_clust(km, 25)
plot_heat(km, 9, col = "region_code")
plot_heat(km, 9, col = "state_name")
plot_heat(km, 9, col = "state_name", cutoff = 10)


heat <- plot_heat(km, 9, col = "state_name", interactive = FALSE,
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
