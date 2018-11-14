library(dplyr)

load("snsccdu_dict.Rdata")

d <- data.table::fread("~/Desktop/Rally/CoorteKI_Rally/KI_Rally_Sprint11A/Shared_Rally/CINASC_CADU_mapped_11-12.csv", na.strings = "")

d <- d %>%
  mutate(preterm = if_else(gest_weeks < 28, true = 1, false = 
    if_else(gest_weeks >= 28 & gest_weeks < 32, true = 2, false = 
      if_else(gest_weeks >= 32 & gest_weeks < 34, true = 3, false = 
        if_else(gest_weeks >= 34 & gest_weeks < 37, true = 4, false = 
          if_else(gest_weeks == 37 | gest_weeks == 38, true = 5, false = 
            if_else(gest_weeks == 39 | gest_weeks == 40, true = 6, false = 
              if_else(gest_weeks == 41, true = 7, false = 
                if_else(gest_weeks == 42, true = 8, false = 
                  if_else(gest_weeks == 43 | gest_weeks == 44, true = 9, false = 10))))))))))

d$preterm <- recode(d$preterm,
  "1" = "Extremely Preterm",
  "2" = "Very Preterm",
  "3" = "Premature", 
  "4" = "Late Premature",
  "5" = "Early Term",
  "6" = "Term",
  "7" = "Late Term",
  "8" = "42 Weeks",
  "9" = "43 or 44 Weeks",
  "10" = "NA")

d$preterm[d$preterm == "NA"] <- NA



d <- d %>%
  mutate(preterm2 = if_else(gest_weeks2 < 28, true = 1, false = 
      if_else(gest_weeks2 >= 28 & gest_weeks2 < 32, true = 2, false = 
          if_else(gest_weeks2 >= 32 & gest_weeks2 < 34, true = 3, false = 
              if_else(gest_weeks2 >= 34 & gest_weeks2 < 37, true = 4, false = 
                  if_else(gest_weeks2 == 37 | gest_weeks2 == 38, true = 5, false = 
                      if_else(gest_weeks2 == 39 | gest_weeks2 == 40, true = 6, false = 
                          if_else(gest_weeks2 == 41, true = 7, false = 
                              if_else(gest_weeks2 == 42, true = 8, false = 
                                  if_else(gest_weeks2 == 43 | gest_weeks2 == 44, true = 9, false = 10))))))))))

d$preterm2 <- recode(d$preterm2,
  "1" = "Extremely Preterm",
  "2" = "Very Preterm",
  "3" = "Premature", 
  "4" = "Late Premature",
  "5" = "Early Term",
  "6" = "Term",
  "7" = "Late Term",
  "8" = "42 Weeks",
  "9" = "43 or 44 Weeks",
  "10" = "NA")

d$preterm2[d$preterm2 == "NA"] <- NA



#Exclude values <14 years and aggregate >= 45 years of mothers age

d$m_age_cat <- cut(d$m_age_yrs, breaks = c(0,14,19,24,29,34,39,44,Inf), right = TRUE, 
  labels = c(
    "Under 14",
    "14 - 19 years",
    "20 - 24 years",
    "25 - 29 years",
    "30 - 34 years",
    "35 - 39 years",
    "40 - 44 years",
    ">= 45 years"))

d$m_age_cat <- ifelse(d$m_age_cat == "Under 14", yes = NA, no = d$m_age_cat)
d$m_age_cat <- recode(d$m_age_cat,
  "2" = "14 - 19 years",
  "3" = "20 - 24 years",
  "4" = "25 - 29 years",
  "5" = "30 - 34 years",
  "6" = "35 - 39 years",
  "7" = "40 - 44 years",
  "8" = "45 or more years")

#Cesarian Deliveries

d$n_ces_deliv_cat <- ifelse(d$n_ces_deliv >= 5, yes = "5 or more", no = d$n_ces_deliv)


#Number of live children

d$n_live_child_cat <- ifelse(d$n_live_child >= 10, yes = "10 or more", no = d$n_live_child)


#Number of dead children

d$n_dead_child_cat <- ifelse(d$n_dead_child >= 5, yes = "5 or more", no = d$n_dead_child)


#Number of vaginal deliveries

d$n_vag_deliv_cat <- ifelse(d$n_vag_deliv >= 10, yes = "10 or more", no = d$n_vag_deliv)


#Number of prenatal visits

d$n_prenat_visit_cat <- ifelse(d$n_prenat_visit >= 15, yes = "15 or more", no = d$n_prenat_visit)


#Gestational month precare start

d$gest_month_precare_cat <- ifelse(d$gest_month_precare == 10, yes = NA, no = d$gest_month_precare)
d$gest_month_precare_cat <- as.character(d$gest_month_precare_cat)

#Recategorize marital status

d$marital_status2 <- ifelse(d$marital_status %in% c("Divorced", "Single", "Widow"), yes = "Without Partner", no = 
  ifelse(d$marital_status %in% c("Married", "Stable Union"), yes = "With Partner", no = NA))

data.table::fwrite(d, file = "~/Desktop/Rally/CoorteKI_Rally/KI_Rally_Sprint11A/Shared_Rally/CINASC_CADU_mapped_11-13.csv")


#### update dictionary

a <- readr::read_csv("/home/ryan.hafen/Desktop/Rally/CoorteKI_Rally/KI_Rally_Sprint11A/Shared_Rally/2018_11_13_Dictionary_SINASC.csv",
  locale = readr::locale(encoding = "latin1"))

a$variavel <- tolower(a$variavel)

# compare dictionary to the dictionary published for GCE
library(cidacsdict)
snsc_dict <- dplyr::filter(cdcs_dict, db == "SINASC")

# take the rally data dictionary and make it compatible with cidacsdict R package
snsccdu <- a %>%
  rename(
    name = variavel,
    label = descricao,
    label_en = description,
    map = classes,
    map_en = labels
  )

tmp <- cdcs_dict$name_en
names(tmp) <- cdcs_dict$name
snsccdu$name_en <- unname(tmp[snsccdu$name])

snsccdu %>%
  filter(is.na(name_en)) %>%
  select(name, label_en) %>% data.frame()

snsccdu$name_en[snsccdu$label_en == "Code of household waste"] <- "household_waste"
snsccdu$name_en[snsccdu$label_en == "Household sanitary disposal code"] <- "household_sanitary"
snsccdu$name_en[snsccdu$label_en == "Code of household water supply"] <- "household_water"
snsccdu$name_en[snsccdu$name == "bf"] <- "bf"
snsccdu$name_en[snsccdu$name == "childs_deaths"] <- "child_death"
snsccdu$name_en[snsccdu$name == "childs"] <- "n_children"
snsccdu$name_en[snsccdu$name == "age_cat"] <- "m_age_cat"
snsccdu$name_en[snsccdu$name == "codestab"] <- "hosp_code"
snsccdu$name_en[snsccdu$name == "hour_cat"] <- "hour_cat"
snsccdu$name_en[snsccdu$name == "major_degree"] <- "major_degree"
snsccdu$name_en[snsccdu$name == "education_recl"] <- "education_recl"

map_to_list <- function(x, en = FALSE) {
  if (is.na(x))
    return(NA)
  
  # message("mapping")
  # # deal with malformed json
  # x <- snsccdu$map[1]
  # x <- gsub("\\{", "", x)
  # x <- gsub("\\}", "", x)
  # x <- gsub("\"", "", x)
  # x <- gsub(" \\:", ":", x)
  # x <- gsub("\\: ", ":", x)
  # xx <- strsplit(x, ",")[[1]]
  # xx <- strsplit(xx, ":")
  # nms <- unlist(lapply(xx, "[[", 1))
  # vals <- lapply(xx, "[[", 2)
  # names(vals) <- nms
  
  res <- jsonlite::fromJSON(x)
  res$`99` <- ifelse(en, "Inconsistency", "Inconsistência")
  res$`88` <- ifelse(en, "Ignored", "Ignorado")
  res
}

snsccdu$map <- lapply(snsccdu$map, map_to_list)
snsccdu$map_en <- lapply(snsccdu$map_en, function(x) map_to_list(x, TRUE))

snsccdu$type[snsccdu$type == "Integer"] <- "integer"
snsccdu$type[snsccdu$type == "Byte"] <- "factor"
snsccdu$type[snsccdu$type == "Date"] <- "date"
snsccdu$type[snsccdu$type == "Boolean"] <- "boolean"

# these variables have already been mapped
# af <- c("bf", "childs_deaths", "childs", "age_cat", "education_recl", "dtnasc", "hour_cat", "major_degree")
# for (nm in af) {
#   snsccdu$map[snsccdu$name == nm] <- NA
#   snsccdu$map_en[snsccdu$name == nm] <- NA
#   snsccdu$type[snsccdu$name == nm] <- "alreadyfactor"
# }

# fix an incorrect encoding (5 should be 4)
idx <- which(snsccdu$name == "cod_destino_lixo_domic_fam_eq")
names(snsccdu$map[idx][[1]])[5] <- "4"
names(snsccdu$map_en[idx][[1]])[5] <- "4"

idx <- which(is.na(snsccdu$name_en))
snsccdu$name_en[idx] <- snsccdu$name[idx]

snsccdu$label_en
snsccdu$map_en

save(snsccdu, file = "snsccdu_dict_11-13.Rdata")


