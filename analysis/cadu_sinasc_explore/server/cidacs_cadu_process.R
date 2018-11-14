library(dplyr)

# read dictionary
# a <- readr::read_csv("/home/ryan.hafen/Desktop/Rally/CoorteKI_Rally/KI_Rally_Sprint11A/DB_ORIGINAL/Dictionary/2018_10_22_Dictionary_SINASC.csv",
#   locale = readr::locale(encoding = "latin1"))
# a <- readr::read_csv("/home/ryan.hafen/Desktop/Rally/CoorteKI_Rally/KI_Rally_Sprint11A/Shared_Rally/2018_11_06_Dictionary_SINASC.csv",
#   locale = readr::locale(encoding = "latin1"))
a <- readr::read_csv("/home/ryan.hafen/Desktop/Rally/CoorteKI_Rally/KI_Rally_Sprint11A/Shared_Rally/2018_11_08_Dictionary_SINASC.csv",
  locale = readr::locale(encoding = "latin1"))

a$variavel <- tolower(a$variavel)

# compare dictionary to the dictionary published for GCE
library(cidacsdict)
snsc_dict <- dplyr::filter(cdcs_dict, db == "SINASC")

length(intersect(snsc_dict$name, a$variavel))
# 32 variables in common

xtra <- setdiff(snsc_dict$name, a$variavel)
xtradf <- dplyr::filter(snsc_dict, name %in% xtra)
cat(paste(paste0("- ", xtradf$name, ": ", xtradf$label_en), collapse = "\n"))

mss <- setdiff(a$variavel, snsc_dict$name)
tmp <- dplyr::filter(cdcs_dict, name %in% mss)
tmp$db
# these are all CADU
setdiff(setdiff(a$variavel, snsc_dict$name), intersect(cdcs_dict$name, mss))
# cod_abaste_agua_domic_fam_eq
# cod_escoa_sanitario_domic_fam_eq
# cod_destino_lixo_domic_fam_eq

dplyr::select(snsc_dict, name, label_en) %>% data.frame()

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
  select(name, label_en)

snsccdu$name_en[snsccdu$label_en == "Code of household waste"] <- "household_waste"
snsccdu$name_en[snsccdu$label_en == "Household sanitary disposal code"] <- "household_sanitary"
snsccdu$name_en[snsccdu$label_en == "Code of household water supply"] <- "household_water"
snsccdu$name_en[snsccdu$name == "bf"] <- "bf"
snsccdu$name_en[snsccdu$name == "childs_deaths"] <- "child_death"
snsccdu$name_en[snsccdu$name == "childs"] <- "n_children"
snsccdu$name_en[snsccdu$name == "age_cat"] <- "m_age_cat"
snsccdu$name_en[snsccdu$name == "codestab"] <- "hosp_code"
snsccdu$name_en[snsccdu$name == "hour_cat"] <- "hour_dat"
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

save(snsccdu, file = "snsccdu_dict.Rdata")

# now read in Spark data frame
d <- readr::read_csv("Desktop/rally_data/part-00000-10eb73e5-6213-4042-af60-57325b6a436e-c000.csv") # , n_max = 100000)

glimpse(d)

# see if there are any discrepancies between the data and the mapping
for (nm in snsccdu$name) {
  mp <- filter(snsccdu, name == nm)$map[[1]]
  if (is.list(mp)) {
    # message(nm)
    kys <- as.character(unique(d[[nm]]))
    kys <- kys[!is.na(kys)]
    dff <- setdiff(kys, names(mp))
    if (length(dff) > 0)
      message(nm, ": values in data that are not in dictionary: ",
        paste(dff, collapse = ", "))
  }
}

d2 <- transform_data(d, snsccdu)

head(d2) %>% data.frame()

select(d2, one_of(snsccdu$name_en[snsccdu$type == "integer"])) %>% glimpse()

# set '99' values to NA
fix <- c("apgar1", "apgar5", "n_live_child", "n_dead_child", "n_prev_preg", "gest_weeks",
  "n_vag_deliv", "n_ces_deliv", "n_prenat_visit", "n_household", "m_age_yrs",
  "gest_month_precare")
for (nm in fix)
  d2[[nm]][d2[[nm]] == 99] <- NA

# set categories to NA that should be NA
fix <- snsccdu$name_en[snsccdu$type == "factor"]
for (nm in fix) {
  if (is.factor(d2[[nm]])) {
    d2[[nm]][d2[[nm]] == "Null"] <- NA
    d2[[nm]][d2[[nm]] == "Inconsistency"] <- NA
    d2[[nm]][d2[[nm]] == "Ignored"] <- NA
    d2[[nm]][d2[[nm]] == "Not apllicabe"] <- NA
    d2[[nm]][d2[[nm]] == "Not applicable"] <- NA
    d2[[nm]] <- droplevels(d2[[nm]])
    # levels(d2[[nm]]) <- setdiff(levels(d2[[nm]]), c("Null", "Inconsistency",
    #   "Ignored", "Not apllicabe", "Not applicable"))
  }
}

sort(unique(unlist(lapply(d2[which(sapply(d2, is.factor))], levels))))

# add birth weight z-score
d2$brthwt_z <- NA
idx <- which(!is.na(d2$sex))

library(growthstandards)
d2$brthwt_z[idx] <- igb_wtkg2zscore(d2$gest_weeks[idx] * 7, d2$brthwt_g[idx] / 1000, sex = as.character(d2$sex[idx]))

hist(d2$brthwt_z)
# looks pretty good

# add birth weight centile
d2$brthwt_centile <- NA
idx <- which(!is.na(d2$sex))

d2$brthwt_centile[idx] <- igb_wtkg2centile(d2$gest_weeks[idx] * 7, d2$brthwt_g[idx] / 1000, sex = as.character(d2$sex[idx]))

hist(d2$brthwt_centile)

length(which(is.na(d2$brthwt_z))) / nrow(d2)
# we don't have brthwt_z for 14% of the data

d2$bf <- gsub("_", "-", d2$bf)

head(d2) %>% data.frame()

# add birth year
d2$birth_year <- as.integer(substr(d2$birth_date, 1, 4))

# save the data
data.table::fwrite(d2, file = "~/Desktop/Rally/CoorteKI_Rally/KI_Rally_Sprint11A/Shared_Rally/CINASC_CADU_mapped_11-06.csv")

length(which(!is.na(d2$resi_muni_code)))

# optional - merge in state, meso, micro codes and names
library(brazilgeo)
mc <- br_muni_codes
mc$resi_muni_code <- substr(mc$muni_code, 1, 6)
mc$muni_code <- NULL
d2$resi_muni_code <- as.character(d2$resi_muni_code)
d2 <- left_join(d2, select(mc, resi_muni_code, micro_code, meso_code, state_code, region_code))


# code to read the data back into memory
d <- data.table::fread("~/Desktop/Rally/CoorteKI_Rally/KI_Rally_Sprint11A/Shared_Rally/CINASC_CADU_mapped_11-06.csv")

fcts <- c("sex", "cong_anom", "birth_place", "marital_status", "m_edu_level", 
  "m_edu_level_aggregated", "m_race", "preg_type", "deliv_type", 
  "n_prenat_visit_cat", "gest_method", "presentation", "labor_induced", 
  "ces_pre_labor", "birth_assist", "household_type", "household_material", 
  "household_water", "household_sanitary", "household_lighting", 
  "household_waste", "bf", "child_death", "m_age_cat", "education_recl",
  "hour_dat", "major_degree")

for (fct in fcts)
  d[[fct]] <- factor(d[[fct]])

