library(dplyr)
library(kitools)
library(geovis)
library(brazilgeo)

# get data from Synapse
d <- kitools::data_use("snsc_2011-2015")

get_pct <- function(x, val) {
  100 * length(which(x == val)) / length(which(!is.na(x)))
}

get_pct_in <- function(x, vals) {
  100 * length(which(x %in% vals)) / length(which(!is.na(x)))
}

get_summaries <- function(dat, geo = "m_muni_code") {
  dat %>%
    group_by(!! sym(geo), birth_year) %>%
    summarise(
      n_births_log10 = log10(n()),
      brthwt_g = mean(brthwt_g, na.rm = TRUE),
      brthwt_z = mean(brthwt_z, na.rm = TRUE),
      brthwt_z2 = mean(brthwt_z2, na.rm = TRUE),
      sga_pct = get_pct(brthwt_centile_cat, "Percentile < 10 (SGA)"),
      lga_pct = get_pct(brthwt_centile_cat, "Percentile > 90 (LGA)"),
      sga_pct2 = get_pct(brthwt_centile_cat2, "Percentile < 10 (SGA)"),
      lga_pct2 = get_pct(brthwt_centile_cat2, "Percentile > 90 (LGA)"),
      preterm_pct = get_pct_in(preterm,
        c("Extremely Preterm", "Very Preterm")),
      preterm_pct2 = get_pct_in(preterm2,
        c("Extremely Preterm", "Very Preterm")),
      ces_prelabor_pct = get_pct(ces_pre_labor, "Yes"),
      ces_pct = get_pct(deliv_type, "Cesarean"),
      educ12_pct = get_pct(m_educ, "12 and over"),
      induced_pct = get_pct(labor_induced, "Yes"),
      teen_pct = 100 * length(which(m_age_yrs < 20)) /
        length(which(!is.na(m_age_yrs))),
      race_white_pct = get_pct(m_race, "White"),
      single_mom_pct = get_pct(marital_status, "Single"),
      visits7_pct = get_pct(n_prenat_visit_cat, "7 and over"),
      n_prev_preg = mean(n_prev_preg, na.rm = TRUE),
      apgar1 = mean(apgar1, na.rm = TRUE),
      gest_month_precare = mean(gest_month_precare, na.rm = TRUE)
    )
}

# # which education variable is best to use?
# length(which(is.na(d$m_educ)))
# length(which(is.na(d$m_educ_grade)))
# length(which(is.na(d$m_educ_2010)))
# length(which(is.na(d$m_educ_2010agg)))
# table(d$m_educ_2010agg)
# table(d$m_educ_2010)
# table(d$m_educ)
# # m_educ is least missing

# get muni and state summaries
muni_summ <- get_summaries(d, "m_muni_code")
state_summ <- get_summaries(d, "m_state_code")

# how many missings do we have
lapply(muni_summ, function(x)
  length(which(is.na(x) | is.infinite(x) | is.nan(x))))

# only munis/year combinations with at least 30 observations 
muni_summ <- muni_summ %>%
  filter(n_births_log10 > log10(30))

# no NAs (will relax in the future with geovis update)
muni_summ <- muni_summ[complete.cases(muni_summ), ]

# only munis that have complete time series...
# (will also relax this with geovis update)
muni_summ <- muni_summ %>%
  group_by(m_muni_code) %>%
  mutate(nyrs = n()) %>%
  filter(nyrs == 5) %>%
  ungroup() %>%
  select(-nyrs) %>%
  arrange(m_muni_code, birth_year) %>%
  rename(muni_code = m_muni_code, year = birth_year) %>%
  mutate(country_code = "BRA")

state_summ <- state_summ %>%
  filter(!is.na(m_state_code)) %>%
  arrange(m_state_code, birth_year) %>%
  rename(state_code = m_state_code, year = birth_year) %>%
  mutate(country_code = "BRA")

# put in the correct state code (geovis wants codes like BRA-xxx)
state_codes <- brazilgeo::br_state_codes %>% ungroup()
# TODO: need to ungroup data in brazilgeo package

state_summ <- state_summ %>%
  ungroup() %>%
  left_join(select(state_codes, state_code, state_code2)) %>%
  select(-state_code) %>%
  rename(state_code = state_code2)

# need to add state code to muni data
muni_codes <- brazilgeo::br_muni_codes
muni_codes$muni_code <- substr(muni_codes$muni_code, 1, 6)

muni_summ <- muni_summ %>%
  left_join(select(muni_codes, muni_code, state_code2)) %>%
  rename(state_code = state_code2) %>%
  filter(muni_code %in% geo$muni$muni_code)

# get geographic boundary data ready
geo <- get_geo_data("brazil", get_munis = TRUE, scale = 10)
# fix state and muni codes in geo data (to match the time series data)
geo$muni$state_code <- geo$muni$state_code2
geo$muni$state_code2 <- NULL
geo$muni$muni_code <- substr(geo$muni$muni_code, 1, 6)

# country-level data is not used but geofacet wants it...
# fill with dummy date within the range of the other data
country_summ <- data.frame(
  year = 2011:2015,
  country_code = "BRA",
  n_births_log10 = mean(state_summ$n_births_log10),
  brthwt_g = mean(state_summ$brthwt_g),
  sga_pct = mean(state_summ$sga_pct),
  lga_pct = mean(state_summ$lga_pct),
  sga_pct2 = mean(state_summ$sga_pct2),
  lga_pct2 = mean(state_summ$lga_pct2),
  preterm_pct = mean(state_summ$preterm_pct),
  preterm_pct2 = mean(state_summ$preterm_pct2),
  ces_prelabor_pct = mean(state_summ$ces_prelabor_pct),
  ces_pct = mean(state_summ$ces_pct),
  educ12_pct = mean(state_summ$educ12_pct),
  induced_pct = mean(state_summ$induced_pct),
  teen_pct = mean(state_summ$teen_pct),
  race_white_pct = mean(state_summ$race_white_pct),
  single_mom_pct = mean(state_summ$single_mom_pct),
  visits7_pct = mean(state_summ$visits7_pct),
  n_prev_preg = mean(state_summ$n_prev_preg),
  apgar1 = mean(state_summ$apgar1),
  gest_month_precare = mean(state_summ$gest_month_precare)
)

# data frame of variable names and descriptions
vars <- data_frame(
  name = c(
    "n_births_log10",
    "brthwt_g",
    "brthwt_z",
    "brthwt_z2",
    "sga_pct",
    "lga_pct",
    "sga_pct2",
    "lga_pct2",
    "preterm_pct",
    "preterm_pct2",
    "ces_prelabor_pct",
    "ces_pct",
    "educ12_pct",
    "induced_pct",
    "teen_pct",
    "race_white_pct",
    "single_mom_pct",
    "visits7_pct",
    "n_prev_preg",
    "apgar1",
    "gest_month_precare"),
  desc = c(
    "Log base 10 number of births",
    "Mean birthweight (g)",
    "Mean birthweight for GA z-score",
    "Mean birthweight for GA z-score (based on LMP)",
    "% of children SGA",
    "% of children LGA",
    "% of children born at <32 weeks",
    "% of children SGA (based on LMP)",
    "% of children LGA (based on LMP)",
    "% of children born at <32 weeks (based on LMP)",
    "% of cesarean births initiated before labor",
    "% of cesarean births",
    "% of mothers with 12 or more years education",
    "% of induced births",
    "% of teenager mothers",
    "% of white mothers",
    "% of single mothers",
    "% of mothers with 7 or more prenatal visits",
    "Mean number of previous pregnancies",
    "Mean apgar1 score",
    "Mean gestational month of first prenatal visit"
  )
)

# var_info is optional but allows us to give variables custom descriptions and ranges
# a lot of our variables have long tails so we cut color scale off at 99th percentile
var_info <- list()
for (i in seq_len(nrow(vars))) {
  nm <- vars$name[i]
  desc <- vars$desc[i]
  cur <- list(name = desc)

  curdat <- c(
    country_summ[[nm]],
    state_summ[[nm]],
    muni_summ[[nm]]
  )
  # plot(sort(curdat))

  if (nm %in% c("brthwt_g", "brthwt_z", "brthwt_z2")) {
    qs <- unname(quantile(curdat, c(0.01, 0.99),
      na.rm = TRUE))
    curdat <- curdat[curdat > qs[1] & curdat < qs[2]]
    cur$breaks <- pretty(curdat, n = 6)
    cur$range <- range(curdat, na.rm = TRUE)
  } else if (nm != "n_births_log10") {
    q99 <- unname(quantile(curdat, 0.99, na.rm = TRUE))
    curdat <- curdat[curdat < q99]
    cur$breaks <- pretty(curdat, n = 6)
    cur$range <- range(curdat, na.rm = TRUE)
  }

  var_info[[nm]] <- cur
}

geovis(geo, path = "~/Desktop/geovis_sinasc",
  name = "Brazil SINASC Explorer",
  view_level = "country",
  view_country_code = "BRA",
  default_var = "brthwt_g",
  var_info = var_info,
  country_data = country_summ,
  state_data = state_summ,
  muni_data = muni_summ
)

plot(sort(muni_summ$brthwt_g))
qs <- quantile(muni_summ$brthwt_g, c(0.01, 0.99))
abline(h = qs)

filter(muni_summ, preterm_pct > 0.5)

max(muni_summ$preterm_pct)
quantile(muni_summ$preterm_pct, 0.99)

tmp <- filter(d, m_muni_code == "352670")
