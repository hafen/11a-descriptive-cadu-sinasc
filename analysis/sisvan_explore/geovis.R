library(dplyr)
library(haven)

sisvan <- read_dta("data/discovered/_raw/SISVANWeb/Sisvan_NutritionStatus_0-5yrs_Todos_2008-2017.dta")

library(geovis)

geo <- get_geo_data("brazil", get_munis = TRUE, scale = 10)
geo$muni$state_code <- geo$muni$state_code2
geo$muni$state_code2 <- NULL
geo$muni$muni_code <- substr(geo$muni$muni_code, 1, 6)

vars <- data_frame(
  name = c(
    "ha01_2a5_n",
    "ha01_2a5_p",
    "ha02_2a5_n",
    "ha02_2a5_p",
    "ha03_2a5_n",
    "ha03_2a5_p",
    "ha_2a5_total",
    "wa01_2a5_n",
    "wa01_2a5_p",
    "wa02_2a5_n",
    "wa02_2a5_p",
    "wa03_2a5_n",
    "wa03_2a5_p",
    "wa04_2a5_n",
    "wa04_2a5_p",
    "wa_2a5_total"
  ),
  desc = c(
    "Number of 2-5y severely underweight children (WAZ <-3)",
    "Percentage of 2-5y severely underweight children (WAZ <-3)",
    "Number of 2-5y moderately underweight children (>=-3 WAZ <-2)",
    "Percentage of 2-5y moderately underweight children (>=-3 WAZ <-2)",
    "Number of 2-5y adequate-WAZ children (>=-2 WAZ <=+2)",
    "Percentage of 2-5y adequate-WAZ children (>=-2 WAZ <=+2)",
    "Number of 2-5y overweight children (WAZ >+2 )**",
    "Number of 2-5y overweight children (WAZ >+2 )**",
    "Total number of 2-5y children evaluated for WAZ",
    "Number of 2-5y severely stunted children (HAZ <-3)",
    "Percentage of 2-5y severely stunted children (HAZ <-3)",
    "Number of 2-5y moderately stunted children (>=-3 HAZ <-2)",
    "Percentage of 2-5y moderately stunted children (>=-3 HAZ <-2)",
    "Number of 2-5y adequate-HAZ children (HAZ >=-2)",
    "Percentage of 2-5y adequate-HAZ children (HAZ >=-2)",
    "Total number of 2-5y children evaluated for HAZ"
  )
)

brmuni <- brazilgeo::br_muni_codes
brmuni$muni_code <- substr(brmuni$muni_code, 1, 6)

sisvan2 <- sisvan %>%
  select(one_of(c("ibge_code", "year", vars$name))) %>%
  rename(muni_code = ibge_code) %>%
  mutate(muni_code = as.character(muni_code)) %>%
  left_join(brmuni) %>%
  mutate(state_code = state_code2) %>%
  select(-state_code2)

unique(sisvan2$year)

country_data <- data.frame(
  year = 2008:2017,
  country_code = "BRA",
  dat = runif(10),
  stringsAsFactors = FALSE
)

state_data <- sisvan2 %>%
  group_by(year, state_code) %>%
  summarise(
    ha01_2a5_p = sum(ha01_2a5_n, na.rm = TRUE) / sum(ha_2a5_total, na.rm = TRUE),
    ha02_2a5_p = sum(ha02_2a5_n, na.rm = TRUE) / sum(ha_2a5_total, na.rm = TRUE),
    ha03_2a5_p = sum(ha03_2a5_n, na.rm = TRUE) / sum(ha_2a5_total, na.rm = TRUE),
    wa01_2a5_p = sum(wa01_2a5_n, na.rm = TRUE) / sum(wa_2a5_total, na.rm = TRUE),
    wa02_2a5_p = sum(wa02_2a5_n, na.rm = TRUE) / sum(wa_2a5_total, na.rm = TRUE),
    wa03_2a5_p = sum(wa03_2a5_n, na.rm = TRUE) / sum(wa_2a5_total, na.rm = TRUE),
    wa04_2a5_p = sum(wa04_2a5_n, na.rm = TRUE) / sum(wa_2a5_total, na.rm = TRUE)
  ) %>%
  mutate(country_code = "BRA")

muni_data <- sisvan2 %>%
  group_by(year, muni_code, state_code) %>%
  summarise(
    ha01_2a5_p = sum(ha01_2a5_n, na.rm = TRUE) / sum(ha_2a5_total, na.rm = TRUE),
    ha02_2a5_p = sum(ha02_2a5_n, na.rm = TRUE) / sum(ha_2a5_total, na.rm = TRUE),
    ha03_2a5_p = sum(ha03_2a5_n, na.rm = TRUE) / sum(ha_2a5_total, na.rm = TRUE),
    wa01_2a5_p = sum(wa01_2a5_n, na.rm = TRUE) / sum(wa_2a5_total, na.rm = TRUE),
    wa02_2a5_p = sum(wa02_2a5_n, na.rm = TRUE) / sum(wa_2a5_total, na.rm = TRUE),
    wa03_2a5_p = sum(wa03_2a5_n, na.rm = TRUE) / sum(wa_2a5_total, na.rm = TRUE),
    wa04_2a5_p = sum(wa04_2a5_n, na.rm = TRUE) / sum(wa_2a5_total, na.rm = TRUE)
  ) %>%
  mutate(country_code = "BRA") %>%
  filter(muni_code %in% geo$muni$muni_code)

fixdat <- function(dat) {
  dat$ha01_2a5_p[is.nan(dat$ha01_2a5_p)] <- 0
  dat$ha02_2a5_p[is.nan(dat$ha02_2a5_p)] <- 0
  dat$ha03_2a5_p[is.nan(dat$ha03_2a5_p)] <- 0
  dat$wa01_2a5_p[is.nan(dat$wa01_2a5_p)] <- 0
  dat$wa02_2a5_p[is.nan(dat$wa02_2a5_p)] <- 0
  dat$wa03_2a5_p[is.nan(dat$wa03_2a5_p)] <- 0
  dat$wa04_2a5_p[is.nan(dat$wa04_2a5_p)] <- 0
  dat
}

state_data <- fixdat(state_data)
muni_data <- fixdat(muni_data)

geovis(geo, path = "~/Desktop/geowidget",
  name = "Brazil SISVAN-Web Explorer",
  view_level = "country",
  view_country_code = "BRA",
  default_var = "dat",
  country_data = country_data,
  state_data = state_data,
  muni_data = muni_data
)
