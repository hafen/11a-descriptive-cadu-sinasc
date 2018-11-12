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
  "la01_0a2_n", # HAZ <-3
  "la02_0a2_n", # >=-3 HAZ <-2
  "la_0a2_total", # Total number of 0-5y children evaluated 

  "ha01_2a5_n", # HAZ <-3
  "ha02_2a5_n", # >=-3 HAZ <-2
  "ha_2a5_total", # Total number of 0-5y children evaluated for HAZ

  "ha01_0a5_n", # HAZ <-3
  "ha02_0a5_n", # >=-3 HAZ <-2
  "ha_0a5_total", # Total number of 0-5y children evaluated for HAZ

  "wl01_0a2_n", # WHZ <-3
  "wl02_0a2_n", # >=-3 WHZ <-2

  "wl05_0a2_n", # >+2 WHZ <=+3
  "wl06_0a2_n", # WHZ >+3
  "wl_0a2_total", # Total number of 0-5y children evaluated for WHZ

  "wh01_2a5_n", # WHZ <-3
  "wh02_2a5_n", # >=-3 WHZ <-2

  "wh05_2a5_n", # >+2 WHZ <=+3
  "wh06_2a5_n", # WHZ >+3
  "wh_2a5_total", # Total number of 0-5y children evaluated for WHZ

  "wh01_0a5_n", # WHZ <-3
  "wh02_0a5_n", # >=-3 WHZ <-2

  "wh05_0a5_n", # >+2 WHZ <=+3
  "wh06_0a5_n", # WHZ >+3
  "wh_0a5_total", # Total number of 0-5y children evaluated for WHZ

  "bmi05_0a2_n", # (>+2 BMI <=+3)
  "bmi06_0a2_n", # (BMI >+3)
  "bmi_0a2_total", # Total number of 0-5y children evaluated for BMI

  "bmi05_2a5_n", # (>+2 BMI <=+3)
  "bmi06_2a5_n", # (BMI >+3)
  "bmi_2a5_total", # Total number of 0-5y children evaluated for BMI

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
    stunting_wh_ratio = (stunting / overweight_wh),
    stunting_bmi_ratio = (stunting / overweight_bmi),
    wh_stunting_ratio = (overweight_wh / stunting),
    bmi_stunting_ratio = (overweight_bmi / stunting),

    stunting02 = 100 * (la01_0a2_n + la02_0a2_n) / la_0a2_total,
    wasting02 = 100 * (wl01_0a2_n + wl02_0a2_n) / wl_0a2_total,
    overweight_wl02 = 100 * (wl05_0a2_n + wl06_0a2_n) / wl_0a2_total,
    overweight_bmi02 = 100 * (bmi05_0a2_n + bmi06_0a2_n) / bmi_0a2_total,
    stunting_wl_ratio02 = (stunting02 / overweight_wl02),
    stunting_bmi_ratio02 = (stunting02 / overweight_bmi02),
    wl_stunting_ratio02 = (overweight_wl02 / stunting02),
    bmi_stunting_ratio02 = (overweight_bmi02 / stunting02),

    stunting25 = 100 * (ha01_2a5_n + ha02_2a5_n) / ha_2a5_total,
    wasting25 = 100 * (wh01_2a5_n + wh02_2a5_n) / wh_2a5_total,
    overweight_wh25 = 100 * (wh05_2a5_n + wh06_2a5_n) / wh_2a5_total,
    overweight_bmi25 = 100 * (bmi05_2a5_n + bmi06_2a5_n) / bmi_2a5_total,
    stunting_wh_ratio25 = (stunting25 / overweight_wh25),
    stunting_bmi_ratio25 = (stunting25 / overweight_bmi25),
    wh_stunting_ratio25 = (overweight_wh25 / stunting25),
    bmi_stunting_ratio25 = (overweight_bmi25 / stunting25),

    n_haz = ha_0a5_total,
    n_whz = wh_0a5_total,
    n_bmi = bmi_0a5_total
  ) %>%
  filter(n_haz > 30 & n_whz > 30 & n_bmi > 30)

length(which(is.infinite(sisvan4$stunting_wh_ratio)))
length(which(is.infinite(sisvan4$stunting_bmi_ratio)))

sisvan4$stunting_wh_ratio[is.infinite(sisvan4$stunting_wh_ratio)] <- NA
sisvan4$stunting_bmi_ratio[is.infinite(sisvan4$stunting_bmi_ratio)] <- NA

sisvan4$stunting_wl_ratio02[is.infinite(sisvan4$stunting_wl_ratio02)] <- NA
sisvan4$stunting_bmi_ratio02[is.infinite(sisvan4$stunting_bmi_ratio02)] <- NA

sisvan4$stunting_wh_ratio25[is.infinite(sisvan4$stunting_wh_ratio25)] <- NA
sisvan4$stunting_bmi_ratio25[is.infinite(sisvan4$stunting_bmi_ratio25)] <- NA

sisvan4 <- sisvan4[complete.cases(sisvan4), ]

muni_data <- sisvan4 %>%
  select(one_of(setdiff(names(sisvan4), vars))) %>%
  group_by(muni_code) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n == 10) %>%
  select(-n)

muni_data <- muni_data %>%
  select(-muni_name, -micro_name, -micro_code, -meso_name,
    -meso_code, -state_name, -region_name, -region_code)

sisvan5 <- sisvan4 %>%
  group_by(year, state_code) %>%
  summarise(
    stunting = 100 * (sum(ha01_0a5_n) + sum(ha02_0a5_n)) / sum(ha_0a5_total),
    wasting = 100 * (sum(wh01_0a5_n) + sum(wh02_0a5_n)) / sum(wh_0a5_total),
    overweight_wh = 100 * (sum(wh05_0a5_n) + sum(wh06_0a5_n)) / sum(wh_0a5_total),
    overweight_bmi = 100 * (sum(bmi05_0a5_n) + sum(bmi06_0a5_n)) / sum(bmi_0a5_total),
    stunting_wh_ratio = (stunting / overweight_wh),
    stunting_bmi_ratio = (stunting / overweight_bmi),
    wh_stunting_ratio = (overweight_wh / stunting),
    bmi_stunting_ratio = (overweight_bmi / stunting),

    stunting02 = 100 * (sum(la01_0a2_n) + sum(la02_0a2_n)) / sum(la_0a2_total),
    wasting02 = 100 * (sum(wl01_0a2_n) + sum(wl02_0a2_n)) / sum(wl_0a2_total),
    overweight_wl02 = 100 * (sum(wl05_0a2_n) + sum(wl06_0a2_n)) / sum(wl_0a2_total),
    overweight_bmi02 = 100 * (sum(bmi05_0a2_n) + sum(bmi06_0a2_n)) / sum(bmi_0a2_total),
    stunting_wl_ratio02 = (stunting02 / overweight_wl02),
    stunting_bmi_ratio02 = (stunting02 / overweight_bmi02),
    wl_stunting_ratio02 = (overweight_wl02 / stunting02),
    bmi_stunting_ratio02 = (overweight_bmi02 / stunting02),

    stunting25 = 100 * (sum(ha01_2a5_n) + sum(ha02_2a5_n)) / sum(ha_2a5_total),
    wasting25 = 100 * (sum(wh01_2a5_n) + sum(wh02_2a5_n)) / sum(wh_2a5_total),
    overweight_wh25 = 100 * (sum(wh05_2a5_n) + sum(wh06_2a5_n)) / sum(wh_2a5_total),
    overweight_bmi25 = 100 * (sum(bmi05_2a5_n) + sum(bmi06_2a5_n)) / sum(bmi_2a5_total),
    stunting_wh_ratio25 = (stunting25 / overweight_wh25),
    stunting_bmi_ratio25 = (stunting25 / overweight_bmi25),
    wh_stunting_ratio25 = (overweight_wh25 / stunting25),
    bmi_stunting_ratio25 = (overweight_bmi25 / stunting25),

    n_haz = sum(n_haz),
    n_whz = sum(n_whz),
    n_bmi = sum(n_bmi)
  )

state_data <- sisvan5 %>%
  select(one_of(setdiff(names(sisvan5), vars)))

save(muni_data, state_data, file = "data/discovered/_raw/sisvanweb/geo_data.Rdata")
