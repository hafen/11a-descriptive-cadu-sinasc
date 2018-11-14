library(dplyr)

d <- data.table::fread("~/Desktop/Rally/CoorteKI_Rally/KI_Rally_Sprint11A/Shared_Rally/CINASC_CADU_mapped_11-06.csv", na.strings = "")

fcts <- c("sex", "cong_anom", "birth_place", "marital_status", "m_edu_level", 
  "m_edu_level_aggregated", "m_race", "preg_type", "deliv_type", 
  "n_prenat_visit_cat", "gest_method", "presentation", "labor_induced", 
  "ces_pre_labor", "birth_assist", "household_type", "household_material", 
  "household_water", "household_sanitary", "household_lighting", 
  "household_waste", "bf", "child_death", "n_children", "m_age_cat", "education_recl",
  "hour_dat", "major_degree")

d$birth_year <- as.integer(substr(d$birth_date, 1, 4))

d$gest_month_precare[d$gest_month_precare == 99] <- NA

d$m_race[d$m_race == "multiracial"] <- "Multiracial"
d$marital_status[d$marital_status == "Wudiw"] <- "Widow"
d$birth_place[d$birth_place == "Others health units"] <- "Others health facility"

d$n_children[d$n_children == "Sem perdas"] <- "0"
# d$child_death[d$child_death == "Sem perdas"] <- "No losses"
# d$child_death[d$child_death == ""] <- "No losses"

## Creating Preterm Variable ## 

## Preterm 
d <- d %>% 
  mutate(preterm = if_else(gest_weeks < 28, true = 1, false = 
      if_else(gest_weeks >= 28 & gest_weeks < 32, true = 2, false = 
          if_else(gest_weeks >= 32 & gest_weeks < 37, true = 3, false = 
              if_else(gest_weeks == 37 | gest_weeks == 38, true = 4, false = 5)))))

## Preterm Intervention

d <- d %>% 
  mutate(preterm_interv = if_else(gest_weeks < 37 & labor_induced == "No", true = 1, false = 
      if_else(gest_weeks < 37 & labor_induced == "Yes", true = 2, false = 
          if_else(gest_weeks >= 37, true = 3, false = 4))))

## Adding Labels

d$preterm <- recode(d$preterm, "1" = "Extremely Preterm", "2" = "Very Preterm", "3" = "Moderate or Late Premature", "4" = "Early Term", "5" = "Normal")
d$preterm_interv <- recode(d$preterm_interv, "1" = "Not induced Preterm", "2" = "Induced Preterm", "3" = "Not Preterm")

### peso ### baixo peso brthwt_g < 2500, normal brthwt_g >= 2500
d$brthwt_cat[d$brthwt_g < 2500] <- 0
d$brthwt_cat[d$brthwt_g >= 2500] <- 1

d$brthwt_cat <- recode(d$brthwt_cat, "0" = "<2500g", "1" = ">=2500g")

###  Percentile  ###   AIG = 0 (10 =< Percentile <= 90)   GIG = 1 (Percentile > 90)     PIG = 2 (Percentile < 10)
d$brthwt_centile_cat[d$brthwt_centile >= 10 & d$brthwt_centile <= 90] <- 0   
d$brthwt_centile_cat[d$brthwt_centile > 90] <- 1 
d$brthwt_centile_cat[d$brthwt_centile < 10] <- 2

d$brthwt_centile_cat <- recode(d$brthwt_centile_cat,
  "0" = "10 =< Percentile <= 90 (Normal)",
  "1" = "Percentile > 90 (LGA)",
  "2" = "Percentile < 10 (SGA)"
)

data.table::fwrite(d, file = "~/Desktop/Rally/CoorteKI_Rally/KI_Rally_Sprint11A/Shared_Rally/CINASC_CADU_mapped_11-08.csv")



