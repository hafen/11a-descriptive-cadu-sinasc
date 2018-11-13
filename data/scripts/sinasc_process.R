## script to process the SINASC data
##---------------------------------------------------------

Sys.setlocale("LC_ALL", "C")
# Sys.setenv(R_MAX_VSIZE = 64e9)
# Sys.getenv("R_MAX_VSIZE")

library(cidacsdict)
library(tidyverse)

snsc_dict <- subset(cdcs_dict, db == "SINASC")

newvar <- data_frame(
  id = 1,
  db_name = "SINASC",
  db_name_en = "SINASC",
  name = "codestab",
  name_en = "health_estbl_code",
  label = "Código de estabelecimento de saúde",
  label_en = "Health establishment code",
  label_google_en = "Health establishment code",
  map = NA,
  map_en = NA,
  type = "character",
  presence = "",
  presence_en = "",
  comments_en = "",
  db = "SINSAC",
  map_en_orig = NA,
  map_orig = NA
)

snsc_dict <- bind_rows(snsc_dict, newvar)

ff <- list.files("data/discovered/_raw/datasus/SINASC/DNRES", full.names = TRUE)
# restrict to 2006+
# ff <- ff[!grepl("2001|2002|2003|2004|2005", ff)]

res <- vector(length = length(ff), mode = "list")

a <- read_datasus(ff[405]) %>%
  transform_data(dict = snsc_dict, quiet = TRUE)

for (ii in seq_along(ff)) {
  message(ii)
  res[[ii]] <- read_datasus(ff[ii]) %>%
    transform_data(dict = snsc_dict, quiet = TRUE) %>%
    clean_data()
}

table(sapply(res, ncol))

snsc <- bind_rows(res)

ncol(snsc)
nrow(snsc_dict)

# look at variables in the dictionary that aren't in the public SINASC data
snsc_dict %>%
  filter(! name_en %in% names(snsc)) %>%
  select(name, name_en, label_en)
#   name       name_en           label_en
# --------------------------------------------------------------
# cepnasc    birth_postal_code Zip code of the birth place
# codestocor birth_fu_code     Federation Unit Code of the birth
# cepres     res_postal_code   Zip code of the residence

# snsc <- snsc %>% tibble::as.tibble()
# save(snsc, file = "data/snsc_all2.Rdata")
# load("data/discovered/snsc_all.Rdata")

# look at health establishment and see if we can use C-section rate to
# assign it as private vs. public

# snsc <- kitools::data_use("snsc_2001-2015")

hosp_deliv <- snsc %>%
  select(health_estbl_code, deliv_type) %>%
  filter(!is.na(deliv_type)) %>%
  group_by(health_estbl_code) %>%
  summarise(
    n = n(),
    pct_ces = length(which(deliv_type == "Cesarean")) / n
  )

plot(sort(log10(hosp_deliv$n)))
abline(h = log10(100))
length(which(hosp_deliv$n < 100)) / nrow(hosp_deliv)
length(which(hosp_deliv$n < 10)) / nrow(hosp_deliv)

15 * 365 / 100
# about 1 birth per 55 days

plot(sort(hosp_deliv$pct_ces))
tmp <- filter(hosp_deliv, n >= 100)$pct_ces
qt <- quantile(tmp, c(0.25, 0.5, 0.75))
plot(sort(tmp))
abline(h = qt)

# new variable: hosp_deliv_type
# - if hospital has less than 100 births, call it "small"
# - if <25% cesarean, call it "most_vaginal"
# - if >75% cesarean, call it "most_cesarean"
# - otherwise, call it "mixed"

hosp_deliv$hosp_deliv_type <- "mixed"
hosp_deliv$hosp_deliv_type[hosp_deliv$pct_ces <= 0.25] <- "most_vaginal"
hosp_deliv$hosp_deliv_type[hosp_deliv$pct_ces >= 0.75] <- "most_cesarean"
hosp_deliv$hosp_deliv_type[hosp_deliv$n < 100] <- "small_hospital"

mdat <- select(hosp_deliv, health_estbl_code, pct_ces, hosp_deliv_type)
names(mdat)[2] <- "hosp_pct_ces"
snsc <- left_join(snsc, mdat)

# merge in mean muni income
income <- kitools::data_use("census_income")
muni_income <- income %>%
  filter(year == 2010) %>%
  group_by(muni_code) %>%
  summarise(
    m_muni_mean_inc = sum(house_inc) / sum(pop),
    m_muni_prop_2mw = sum(pop_2mw) / sum(pop),
    m_muni_pop = sum(pop)
  )
names(muni_income)[1] <- "m_muni_code"
muni_income$m_muni_code <- as.character(muni_income$m_muni_code)

snsc <- left_join(snsc, muni_income)

length(which(is.na(snsc$m_muni_mean_inc)))

# add birth season...
m <- as.integer(format(snsc$birth_date, "%m"))
snsc$birth_qtr <- factor(floor((m - 0.5) / 3))
levels(snsc$birth_qtr) <- c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec")

# further cleaning...
snsc$sex[snsc$sex == "Null"] <- NA
snsc$m_educ[snsc$m_educ == "Null"] <- NA
snsc$m_educ_2010[snsc$m_educ_2010 == "Null"] <- NA
snsc$ces_pre_labor[snsc$ces_pre_labor == "Not applicable"] <- NA

snsc$n_prev_preg[snsc$n_prev_preg == 99] <- NA
snsc$n_vag_deliv[snsc$n_vag_deliv == 99] <- NA
snsc$n_ces_deliv[snsc$n_ces_deliv == 99] <- NA
snsc$n_prenat_visit[snsc$n_prenat_visit == 99] <- NA
snsc$gest_month_precare[snsc$gest_month_precare == 99] <- NA

# get rid of period in levels for m_race
levels(snsc$m_race) <- gsub("\\.", "", levels(snsc$m_race))

kitools::data_publish(snsc, name = "snsc_2001-2015",
  desc = "Public individual-level SINASC data from 2001-2015, with variables that intersect with the CIDACS GCE data dictionary.")

snsc2 <- filter(snsc, birth_year >= 2011)

# table(snsc2$n_prenat_visit, useNA = "always") %>% data.frame()

tmp <- head(snsc2, 100)

# add weight for gestational age z-score
library(growthstandards)

snsc2 <- filter(snsc2, !is.na(sex))

snsc2$brthwt_z <- igb_wtkg2zscore(snsc2$gest_weeks * 7, snsc2$brthwt_g / 1000, sex = as.character(snsc2$sex))

hist(snsc2$brthwt_z)

# data_frame(name_en = names(snsc2)) %>%
#   left_join(
#     snsc_dict %>%
#       filter(name_en %in% names(snsc2)) %>%
#       select(name_en, label_en)
#   ) %>%
#   write_csv("~/Desktop/snsc_dictionary.csv")


# make Ignored, Null, etc. NA
isfct <- which(unlist(lapply(d, is.factor)))
lvls <- sort(unique(unlist(lapply(d[isfct], levels))))
ign <- c("Ignored", "Inconsistency", "Null",
  "Not applicable")
for (nm in names(isfct)) {
  idx <- which(d[[nm]] %in% ign)
  message(nm, " ", length(idx))
  d[[nm]][idx] <- NA
  d[[nm]] <- droplevels(d[[nm]])
}

# only look at single births
d <- filter(d, preg_type == "Singleton")

# add birth weight centile
d$brthwt_centile <- NA
idx <- which(!is.na(d$sex))

d$brthwt_centile[idx] <- igb_wtkg2centile(d$gest_weeks[idx] * 7, d$brthwt_g[idx] / 1000, sex = as.character(d$sex[idx]))

hist(d$brthwt_centile)

## Preterm 
d <- d %>% 
  mutate(preterm = if_else(gest_weeks < 28, true = 1, false = 
      if_else(gest_weeks >= 28 & gest_weeks < 32, true = 2, false = 
          if_else(gest_weeks >= 32 & gest_weeks < 37, true = 3, false = 
            if_else(gest_weeks == 37 | gest_weeks == 38,
              true = 4, false = 5)))))

## Preterm Intervention
d <- d %>% 
  mutate(preterm_interv = if_else(gest_weeks < 37 & labor_induced == "No", true = 1, false = 
      if_else(gest_weeks < 37 & labor_induced == "Yes", true = 2, false = 
          if_else(gest_weeks >= 37, true = 3, false = 4))))

## Adding Labels
d$preterm <- recode(d$preterm, "1" = "Extremely Preterm", "2" = "Very Preterm", "3" = "Moderate or Late Premature", "4" = "Early Term", "5" = "Normal")
d$preterm_interv <- recode(d$preterm_interv, "1" = "Not induced Preterm", "2" = "Induced Preterm", "3" = "Not Preterm")

###  Percentile  ###   AIG = 0 (10 =< Percentile <= 90)   GIG = 1 (Percentile > 90)     PIG = 2 (Percentile < 10)
d$brthwt_centile_cat[d$brthwt_centile >= 10 & d$brthwt_centile <= 90] <- 0   
d$brthwt_centile_cat[d$brthwt_centile > 90] <- 1 
d$brthwt_centile_cat[d$brthwt_centile < 10] <- 2

d$brthwt_centile_cat <- recode(d$brthwt_centile_cat,
  "0" = "10 =< Percentile <= 90 (Normal)",
  "1" = "Percentile > 90 (LGA)",
  "2" = "Percentile < 10 (SGA)"
)

d$gest_weeks2 <- as.numeric(difftime(d$birth_date, 
  d$menstrual_date_last, units = "weeks"))

length(which(d$gest_weeks2 > 50))

d$gest_weeks2[d$gest_weeks2 > 50] <- NA
d$gest_weeks2[d$gest_weeks2 < 20] <- NA

hist(d$gest_weeks2, breaks = 30)

d$brthwt_z2 <- NA
idx <- which(!is.na(d$sex))
library(growthstandards)
d$brthwt_z2[idx] <- igb_wtkg2zscore(d$gest_weeks2[idx] * 7, d$brthwt_g[idx] / 1000, sex = as.character(d$sex[idx]))

d$brthwt_z2[is.infinite(d$brthwt_z2)] <- NA
d$brthwt_z2[d$brthwt_z2 < -8] <- NA
d$brthwt_z2[d$brthwt_z2 > 8] <- NA

hist(d$brthwt_z2)

# add birth weight centile
d$brthwt_centile2 <- NA
idx <- which(!is.na(d$sex))

d$brthwt_centile2[idx] <- igb_wtkg2centile(d$gest_weeks2[idx] * 7, d$brthwt_g[idx] / 1000, sex = as.character(d$sex[idx]))

hist(d$brthwt_centile2)

## Preterm 
d <- d %>% 
  mutate(preterm2 = if_else(gest_weeks2 < 28, true = 1, false = 
      if_else(gest_weeks2 >= 28 & gest_weeks2 < 32, true = 2, false = 
          if_else(gest_weeks2 >= 32 & gest_weeks2 < 37, true = 3, false = 
            if_else(gest_weeks2 == 37 | gest_weeks2 == 38,
              true = 4, false = 5)))))

## Preterm Intervention
d <- d %>% 
  mutate(preterm_interv2 = if_else(gest_weeks2 < 37 & labor_induced == "No", true = 1, false = 
      if_else(gest_weeks2 < 37 & labor_induced == "Yes", true = 2, false = 
          if_else(gest_weeks2 >= 37, true = 3, false = 4))))

## Adding Labels
d$preterm2 <- recode(d$preterm2, "1" = "Extremely Preterm", "2" = "Very Preterm", "3" = "Moderate or Late Premature", "4" = "Early Term", "5" = "Normal")
d$preterm_interv2 <- recode(d$preterm_interv2, "1" = "Not induced Preterm", "2" = "Induced Preterm", "3" = "Not Preterm")

###  Percentile  ###   AIG = 0 (10 =< Percentile <= 90)   GIG = 1 (Percentile > 90)     PIG = 2 (Percentile < 10)
d$brthwt_centile_cat2[d$brthwt_centile2 >= 10 & d$brthwt_centile2 <= 90] <- 0   
d$brthwt_centile_cat2[d$brthwt_centile2 > 90] <- 1 
d$brthwt_centile_cat2[d$brthwt_centile2 < 10] <- 2

d$brthwt_centile_cat2 <- recode(d$brthwt_centile_cat2,
  "0" = "10 =< Percentile <= 90 (Normal)",
  "1" = "Percentile > 90 (LGA)",
  "2" = "Percentile < 10 (SGA)"
)

kitools::data_publish(d, name = "snsc_2011-2015",
  desc = "Public individual-level SINASC data from 2011-2015, with variables that intersect with the CIDACS GCE data dictionary and additional derived variables.")











ignore <- c(
  "m_muni_code",
  "birth_muni_code",
  "birth_place",
  "health_estbl_code",
  "occ_code",
  "gest_weeks_cat",
  "birth_date",
  "cong_icd10",
  "birth_micro_code",
  "birth_meso_code",
  "m_micro_code",
  "m_meso_code",
  "birth_nbhd_code",
  "res_nbhd_code",
  "birth_time",
  "m_country_code",
  "m_birth_country_code",
  "m_birth_date",
  "menstrual_date_last",
  "m_fu_code"
)

data.table::fwrite(snsc2[, setdiff(names(snsc2), ignore)], "~/Desktop/snsc_ml.csv")
