library(dplyr)
library(haven)

sisvan <- read_dta("data/discovered/_raw/sisvanweb/Sisvan_NutritionStatus_0-5yrs_BolsaFamilia_2008-2017.dta")

library(geovis)

geo <- get_geo_data("brazil", get_munis = TRUE, scale = 10)
geo$muni$state_code <- geo$muni$state_code2
geo$muni$state_code2 <- NULL
geo$muni$muni_code <- substr(geo$muni$muni_code, 1, 6)

brmuni <- brazilgeo::br_muni_codes
brmuni$muni_code <- substr(brmuni$muni_code, 1, 6)

for (i in seq_along(sisvan))
  attributes(sisvan[[i]]) <- NULL

vars <- c(
  "ha01_0a5_n", # HAZ <-3
  "ha02_0a5_n", # >=-3 HAZ <-2
  "ha_0a5_total", # Total number of 0-5y children evaluated for HAZ
  "wh01_0a5_n", # WHZ <-3
  "wh02_0a5_n", # >=-3 WHZ <-2
  "wh05_0a5_n", # >+2 WHZ <=+3
  "wh06_0a5_n", # WHZ >+3
  "wh_0a5_total", # Total number of 0-5y children evaluated for WHZ
  "bmi05_0a5_n", # (>+2 BMI <=+3)
  "bmi06_0a5_n", # (BMI >+3)
  "bmi_0a5_total" # Total number of 0-5y children evaluated for BMI
)

sisvan2 <- sisvan %>%
  select(one_of(c("ibge_code", "year", vars))) %>%
  rename(muni_code = ibge_code) %>%
  mutate(muni_code = as.character(muni_code)) %>%
  left_join(brmuni) %>%
  mutate(state_code = state_code2) %>%
  select(-state_code2)

sisvan3 <- sisvan2[complete.cases(sisvan2), ]

sisvan4 <- sisvan3 %>%
  mutate(
    stunting = 100 * (ha01_0a5_n + ha02_0a5_n) / ha_0a5_total,
    wasting = 100 * (wh01_0a5_n + wh02_0a5_n) / wh_0a5_total,
    overweight_wh = 100 * (wh05_0a5_n + wh06_0a5_n) / wh_0a5_total,
    overweight_bmi = 100 * (bmi05_0a5_n + bmi06_0a5_n) / bmi_0a5_total,
    overweight_stunting_ratio1 = 100 * (overweight_wh / stunting),
    overweight_stunting_ratio2 = 100 * (overweight_bmi / stunting),
    n_haz = ha_0a5_total,
    n_whz = wh_0a5_total,
    n_bmi = bmi_0a5_total
  ) %>%
  filter(n_haz > 30 & n_whz > 30 & n_bmi > 30)

length(which(is.infinite(sisvan4$overweight_stunting_ratio1)))
length(which(is.infinite(sisvan4$overweight_stunting_ratio2)))

sisvan4$overweight_stunting_ratio1[is.infinite(sisvan4$overweight_stunting_ratio1)] <- NA
sisvan4$overweight_stunting_ratio2[is.infinite(sisvan4$overweight_stunting_ratio2)] <- NA

sisvan4 <- sisvan4[complete.cases(sisvan4), ]

muni_data <- sisvan4 %>%
  select(one_of(setdiff(names(sisvan4), vars))) %>%
  group_by(muni_code) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n == 10) %>%
  select(-n)

sisvan5 <- sisvan4 %>%
  group_by(year, state_code) %>%
  summarise(
    stunting = 100 * (sum(ha01_0a5_n) + sum(ha02_0a5_n)) / sum(ha_0a5_total),
    wasting = 100 * (sum(wh01_0a5_n) + sum(wh02_0a5_n)) / sum(wh_0a5_total),
    overweight_wh = 100 * (sum(wh05_0a5_n) + sum(wh06_0a5_n)) / sum(wh_0a5_total),
    overweight_bmi = 100 * (sum(bmi05_0a5_n) + sum(bmi06_0a5_n)) / sum(bmi_0a5_total),
    overweight_stunting_ratio1 = 100 * (overweight_wh / stunting),
    overweight_stunting_ratio2 = 100 * (overweight_bmi / stunting),
    n_haz = sum(n_haz),
    n_whz = sum(n_whz),
    n_bmi = sum(n_bmi)
  )

state_data <- sisvan5 %>%
  select(one_of(setdiff(names(sisvan5), vars)))

save(muni_data, state_data, file = "data/discovered/_raw/sisvanweb/geo_data.Rdata")
