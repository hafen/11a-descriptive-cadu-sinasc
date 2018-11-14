# add new gestational age variable


library(dplyr)

load("snsccdu_dict.Rdata")

d <- data.table::fread("~/Desktop/Rally/CoorteKI_Rally/KI_Rally_Sprint11A/Shared_Rally/CINASC_CADU_mapped_11-08.csv", na.strings = "")

length(which(is.na(d$menstrual_date_last))) / nrow(d)

d$birth_date <- as.Date(substr(d$birth_date, 1, 10))
d$menstrual_date_last <- as.Date(d$menstrual_date_last)

d$gest_weeks2 <- as.numeric(difftime(d$birth_date, d$menstrual_date_last, units = "weeks"))

length(which(d$gest_weeks2 > 50))

d$gest_weeks2[d$gest_weeks2 > 50] <- NA
d$gest_weeks2[d$gest_weeks2 < 20] <- NA

hist(d$gest_weeks2, breaks = 30)


d$brthwt_z2 <- NA
idx <- which(!is.na(d$sex))

library(growthstandards)
d$brthwt_z2[idx] <- igb_wtkg2zscore(d$gest_weeks2[idx] * 7, d$brthwt_g[idx] / 1000, sex = as.character(d$sex[idx]))

hist(d$brthwt_z2)
# looks pretty good

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
              if_else(gest_weeks2 == 37 | gest_weeks2 == 38, true = 4, false = 5)))))

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

data.table::fwrite(d, file = "~/Desktop/Rally/CoorteKI_Rally/KI_Rally_Sprint11A/Shared_Rally/CINASC_CADU_mapped_11-12.csv")
