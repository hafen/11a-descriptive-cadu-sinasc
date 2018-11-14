library(dplyr)

load("snsccdu_dict_11-13.Rdata")

d2 <- data.table::fread("~/Desktop/Rally/CoorteKI_Rally/KI_Rally_Sprint11A/Shared_Rally/CINASC_CADU_mapped_11-13.csv", na.strings = "") %>% as_data_frame()

d <- d2

names(which(sapply(d2, is.character))) %>% dput

fcts <- c("sex", "cong_anom", "birth_place", "marital_status", "m_edu_level", 
  "m_edu_level_aggregated", "m_race", "preg_type", "deliv_type",
  "n_prenat_visit_cat", "gest_method", "presentation", "labor_induced",
  "ces_pre_labor", "birth_assist", "household_type", "household_material",
  "household_water", "household_sanitary", "household_lighting",
  "household_waste", "bf", "child_death", "n_children", "m_age_cat", "education_recl",
  "hour_dat", "major_degree", "preterm", "preterm_interv", "brthwt_cat", "brthwt_centile_cat", 
  "preterm2", "preterm_interv2", "brthwt_centile_cat2", "n_ces_deliv_cat", 
  "n_live_child_cat", "n_dead_child_cat", "n_vag_deliv_cat", "marital_status2"
)

d$child_death <- recode(d$child_death, "Sem perdas" = "No", "Perdas" = "Yes")
d$n_children <- recode(d$n_children, ">= 4" = "more than 4", "Sem perdas" = "No children")

d$education_recl <- recode(d$education_recl,
  "Ensino Medio Completo" = "Basic level incomplete",
  "Fundamental Completo" = "Basic level complete",
  "Fundamental Incompleto" = "High School Complete",
  "Superior Completo" = "Major Degree")

# filter(snsccdu, name_en == "birth_place")$map_en

d$major_degree <- tolower(as.character(d$major_degree))

for (fct in fcts) {
  mp <- filter(snsccdu, name_en == fct)$map_en
  if (!is.null(mp) && !is.na(mp)) {
    lvls <- unname(unlist(mp))
    lvls <- setdiff(lvls, c("Null", "Ignored", "Inconsistency", "Not apllicabe", "Not applicable"))
    d[[fct]] <- factor(d[[fct]], levels = lvls)
  } else {
    d[[fct]] <- factor(d[[fct]])
  }
}

# make sure they all still line up after reordering factor levels
r1 <- sapply(d[fcts], function(x) length(which(is.na(x))))
r2 <- sapply(d2[fcts], function(x) length(which(is.na(x))))
which(r1 != r2)

table(d$marital_status2)
table(d2$marital_status2)

which(sapply(d, function(x) all(is.na(x))))

lapply(d[fcts], levels)

d$hour_dat <- forcats::fct_relevel(d$hour_dat,
  c("Dawn", "Morning", "Evening", "Night"))

# d$gest_month_precare[d$gest_month_precare == 99] <- NA

length(which(abs(d$brthwt_z) > 5))
length(which(is.na(d$brthwt_z)))
length(which(d$gest_weeks <= 20))

d <- filter(d, abs(brthwt_z) < 5 | is.na(brthwt_z))
d <- filter(d, gest_weeks > 20 | is.na(gest_weeks))

levels(d$birth_place)
d$birth_place[d$birth_place %in% c("Others health facility", "Others")] <- NA
d$birth_place <- droplevels(d$birth_place)

levels(d$birth_assist)[2] <- "Nurse"

library(brazilgeo)
mc <- br_muni_codes
mc$resi_muni_code <- substr(mc$muni_code, 1, 6)
mc$muni_code <- NULL
d$resi_muni_code <- as.character(d$resi_muni_code)
d <- left_join(d, select(mc, resi_muni_code, micro_code, meso_code, state_code, region_code))

which(sapply(d, function(x) all(is.na(x))))

source("_fns.R")

length(which(d$gest_weeks > 45))
d$gest_weeks[d$gest_weeks > 45] <- NA

addl <- c("birth_year")
fcts <- setdiff(fcts, "preg_type")

brthwt_z_summ <- lapply(c(fcts, addl), function(x) {
  message(x)
  summarize_var_by(d, brthwt_z, !! sym(x))
})
names(brthwt_z_summ) <- c(fcts, addl)

pth <- "results/cadu_sinasc2/brthwt_z_summ"
dir.create(pth, recursive = TRUE)

for (nm in names(brthwt_z_summ)) {
  lab <- filter(snsccdu, name_en == nm)$label_en
  if (length(lab) == 0) {
    lab <- nm
  } else {
    lab <- paste0(lab, " (", nm, ")")
  }
  dd <- filter(brthwt_z_summ[[nm]], n > 50 & !is.nan(mean))
  rng <- range(c(dd$mean - 2 * dd$se, dd$mean + 2 * dd$se))
  if (diff(rng) < 1)
    rng <- mean(rng) + c(-1, 1) * 0.5
  
  p <- plot_var_by(filter(brthwt_z_summ[[nm]], n > 50),
    "Birth Weight for GA Z-Score",
    nm,
    xlab = lab,
    se = TRUE)
  p <- p + geom_abline(slope = 0, intercept = 0, alpha = 0.3) + ylim(rng)

  ggsave(file.path(pth, paste0(nm, ".png")), width = 6, height = 6)
}

brthwt_z_summ_dat <- bind_rows(lapply(names(brthwt_z_summ), function(x) {
  cur <- filter(brthwt_z_summ[[x]], n > 50)
  data_frame(nm = x, sd = sd(cur$mean), nrow = nrow(cur))
})) %>%
  arrange(-sd)

readr::write_csv(brthwt_z_summ_dat, file.path(pth, "summ_dat.csv"))



gest_weeks_summ <- lapply(c(fcts, addl), function(x) {
  message(x)
  summarize_var_by(d, gest_weeks, !! sym(x))
})
names(gest_weeks_summ) <- c(fcts, addl)

pth2 <- "results/cadu_sinasc2/gest_weeks_summ"
dir.create(pth2, recursive = TRUE)

for (nm in names(gest_weeks_summ)) {
  lab <- filter(snsccdu, name_en == nm)$label_en
  if (length(lab) == 0) {
    lab <- nm
  } else {
    lab <- paste0(lab, " (", nm, ")")
  }
  dd <- filter(gest_weeks_summ[[nm]], n > 50 & !is.nan(mean))
  rng <- range(c(dd$mean - 2 * dd$se, dd$mean + 2 * dd$se))
  if (diff(rng) < 1)
    rng <- mean(rng) + c(-1, 1) * 0.5
  
  p <- plot_var_by(dd,
    "Gestational Age at Birth (weeks)",
    nm,
    xlab = lab,
    se = TRUE)
  p <- p + geom_abline(slope = 0, intercept = 0, alpha = 0.3) + ylim(rng)
  ggsave(file.path(pth2, paste0(nm, ".png")), width = 6, height = 6)
}

gest_weeks_summ_dat <- bind_rows(lapply(names(gest_weeks_summ), function(x) {
  cur <- filter(gest_weeks_summ[[x]], n > 50)
  data_frame(nm = x, sd = sd(cur$mean), nrow = nrow(cur))
})) %>%
  arrange(-sd)

readr::write_csv(gest_weeks_summ_dat, file.path(pth2, "summ_dat.csv"))


deliv_type_summ <- lapply(c(fcts, addl), function(x) {
  message(x)
  summarize_bvar_by(d, deliv_type, !! sym(x), "Cesarean")
})
names(deliv_type_summ) <- c(fcts, addl)

pth3 <- "results/cadu_sinasc2/deliv_type_summ"
dir.create(pth3, recursive = TRUE)

for (nm in names(deliv_type_summ)) {
  lab <- filter(snsccdu, name_en == nm)$label_en
  if (length(lab) == 0) {
    lab <- nm
  } else {
    lab <- paste0(lab, " (", nm, ")")
  }
  
  p <- plot_bvar_by(
    filter(deliv_type_summ[[nm]], n > 50),
    "Proportion of Cesarean Deliveries",
    nm,
    xlab = lab) +
    ylim(c(0, 1))
  
  ggsave(file.path(pth3, paste0(nm, ".png")), width = 6, height = 6)
}

deliv_type_summ_dat <- bind_rows(lapply(names(deliv_type_summ), function(x) {
  cur <- filter(deliv_type_summ[[x]], n > 50)
  data_frame(nm = x, sd = sd(cur$p), nrow = nrow(cur))
})) %>%
  arrange(-sd) %>%
  filter(nm != "deliv_type")

readr::write_csv(deliv_type_summ_dat, file.path(pth3, "summ_dat.csv"))




brthwt_cat_summ <- lapply(c(fcts, addl), function(x) {
  message(x)
  summarize_bvar_by(d, brthwt_cat, !! sym(x), "<2500g")
})
names(brthwt_cat_summ) <- c(fcts, addl)

pth4 <- "results/cadu_sinasc2/brthwt_cat_summ"
dir.create(pth4, recursive = TRUE)

for (nm in names(brthwt_cat_summ)) {
  lab <- filter(snsccdu, name_en == nm)$label_en
  if (length(lab) == 0) {
    lab <- nm
  } else {
    lab <- paste0(lab, " (", nm, ")")
  }
  dd <- filter(brthwt_cat_summ[[nm]], n > 50 & !is.nan(p))
  dd <- dd[complete.cases(dd),]
  rng <- range(c(dd$p - 2 * dd$se, dd$p + 2 * dd$se))
  if (diff(rng) < 0.25 & all(dd$p < 0.25))
    rng <- c(0, 0.25)
  
  p <- plot_bvar_by(
    filter(brthwt_cat_summ[[nm]], n > 50),
    "Proportion of Low Birth Weight",
    nm,
    xlab = lab) +
    ylim(rng)
  
  ggsave(file.path(pth4, paste0(nm, ".png")), width = 6, height = 6)
}

# note: sex=NA have high proportion of low birth weight (very premature?)
# sex          n      p        se
# <fct>    <int>  <dbl>     <dbl>
#   1 Male   7354402 0.0668 0.0000921
#   2 Female 6995136 0.0801 0.000103 
#   3 NA        2446 0.434  0.0100

brthwt_cat_summ_dat <- bind_rows(lapply(names(brthwt_cat_summ), function(x) {
  cur <- filter(brthwt_cat_summ[[x]], n > 50)
  data_frame(nm = x, sd = sd(cur$p), nrow = nrow(cur))
})) %>%
  arrange(-sd) %>%
  filter(nm != "brthwt_cat")

readr::write_csv(brthwt_cat_summ_dat, file.path(pth4, "summ_dat.csv"))



sga_summ <- lapply(c(fcts, addl), function(x) {
  message(x)
  summarize_bvar_by(d, brthwt_centile_cat, !! sym(x), "Percentile < 10 (SGA)")
})
names(sga_summ) <- c(fcts, addl)

pth5 <- "results/cadu_sinasc2/sga_summ"
dir.create(pth5, recursive = TRUE)

for (nm in names(sga_summ)) {
  lab <- filter(snsccdu, name_en == nm)$label_en
  if (length(lab) == 0) {
    lab <- nm
  } else {
    lab <- paste0(lab, " (", nm, ")")
  }
  dd <- filter(sga_summ[[nm]], n > 50 & !is.nan(p))
  dd <- dd[complete.cases(dd),]
  rng <- range(c(dd$p - 2 * dd$se, dd$p + 2 * dd$se))
  if (diff(rng) < 0.25 & all(dd$p < 0.25))
    rng <- c(0, 0.25)
  
  p <- plot_bvar_by(
    filter(sga_summ[[nm]], n > 50),
    "Proportion of SGA",
    nm,
    xlab = lab) +
    ylim(rng)
  
  ggsave(file.path(pth5, paste0(nm, ".png")), width = 6, height = 6)
}

sga_summ_dat <- bind_rows(lapply(names(sga_summ), function(x) {
  cur <- filter(sga_summ[[x]], n > 50)
  data_frame(nm = x, sd = sd(cur$p), nrow = nrow(cur))
})) %>%
  arrange(-sd) %>%
  filter(nm != "brthwt_centile_cat")

readr::write_csv(sga_summ_dat, file.path(pth5, "summ_dat.csv"))



lga_summ <- lapply(c(fcts, addl), function(x) {
  message(x)
  summarize_bvar_by(d, brthwt_centile_cat, !! sym(x), "Percentile > 90 (LGA)")
})
names(lga_summ) <- c(fcts, addl)

pth7 <- "results/cadu_sinasc2/lga_summ"
dir.create(pth7, recursive = TRUE)

for (nm in names(lga_summ)) {
  lab <- filter(snsccdu, name_en == nm)$label_en
  if (length(lab) == 0) {
    lab <- nm
  } else {
    lab <- paste0(lab, " (", nm, ")")
  }
  dd <- filter(lga_summ[[nm]], n > 50 & !is.nan(p))
  dd <- dd[complete.cases(dd),]
  rng <- range(c(dd$p - 2 * dd$se, dd$p + 2 * dd$se))
  if (diff(rng) < 0.25 & all(dd$p < 0.25))
    rng <- c(0, 0.25)
  
  p <- plot_bvar_by(
    filter(lga_summ[[nm]], n > 50),
    "Proportion of LGA",
    nm,
    xlab = lab) +
    ylim(rng)
  
  ggsave(file.path(pth7, paste0(nm, ".png")), width = 6, height = 6)
}

lga_summ_dat <- bind_rows(lapply(names(lga_summ), function(x) {
  cur <- filter(lga_summ[[x]], n > 50)
  data_frame(nm = x, sd = sd(cur$p), nrow = nrow(cur))
})) %>%
  arrange(-sd) %>%
  filter(nm != "brthwt_centile_cat")

readr::write_csv(lga_summ_dat, file.path(pth7, "summ_dat.csv"))



# look at categorical outcomes vs. categorical risk factors over time and space

nm <- "brthwt_centile_cat"
rf <- "n_prenat_visit_cat"
val <- "Percentile > 90 (LGA)"

summarize_outcome_st <- function(d, nm, rf, val) {
  summ <- d %>%
    group_by(!! sym(rf), state_code) %>%
    summarise(
      n = length(which(!is.na(!! sym(nm)))),
      p = length(which(!! sym(nm) %in% val)) / n,
      se = sqrt(p * (1 - p) / n)
    )
  summ <- summ[complete.cases(summ),]
  
  rng <- range(c(summ$p - 2 * summ$se, summ$p + 2 * summ$se))
  names(summ)[1] <- "yvar"
  summ$yvar2 <- as.numeric(summ$yvar)
  attr(summ, "rf") <- rf
  attr(summ, "nm") <- nm
  attr(summ, "val") <- val
  attr(summ, "rng") <- rng
  summ
}

plot_outcome_st <- function(summ, ylab) {
  nm <- attr(summ, "rf")
  lab <- filter(snsccdu, name_en == nm)$label_en
  if (length(lab) == 0) {
    lab <- nm
  } else {
    lab <- paste0(lab, " (", nm, ")")
  }
  
  rng <- attr(summ, "rng")
  
  ggplot(summ, aes(yvar, p, xmin = yvar2 - 0.5, xmax = yvar2 + 0.5)) +
    geom_rect(aes(ymin = rng[1], ymax = p), fill = tableau10[1], alpha = 0.6) +
    geom_rect(aes(ymin = p, ymax = rng[2]), fill = tableau10[1], alpha = 0.4) +
    theme_bw() +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = p - 2 * se, ymax = p + 2 * se), width = 0.2) +
    facet_geo(~ state_code, grid = "br_states_grid2", label = "name") +
    theme(
      strip.text.x = element_text(margin = margin(0.1, 0, 0.1, 0, "cm"), size = 7),
      axis.text.x = element_text(angle = 60, hjust = 1)) +
    labs(x = lab, y = ylab)
}

lga_rf_geo_summ <- lapply(fcts, function(x) {
  message(x)
  summarize_outcome_st(d, nm = "brthwt_centile_cat", rf = x, val = "Percentile > 90 (LGA)")
})
names(lga_rf_geo_summ) <- fcts

pth8 <- "results/cadu_sinasc2/lga_rf_geo_summ"
dir.create(pth8, recursive = TRUE)

for (nm in names(lga_rf_geo_summ)) {
  message(nm)
  plot_outcome_st(lga_rf_geo_summ[[nm]],
    ylab = "Proportion of LGA births (birth weight for GA percentile > 90")
  ggsave(file.path(pth8, paste0(nm, ".png")), width = 10, height = 12)
}


preterm_rf_geo_summ <- lapply(fcts, function(x) {
  message(x)
  summarize_outcome_st(d, nm = "preterm", rf = x, val = c("Extremely Preterm", "Very Preterm"))
})
names(preterm_rf_geo_summ) <- fcts

pth10 <- "results/cadu_sinasc2/preterm_rf_geo_summ"
dir.create(pth10, recursive = TRUE)

for (nm in names(preterm_rf_geo_summ)) {
  message(nm)
  plot_outcome_st(preterm_rf_geo_summ[[nm]],
    ylab = "Proportion of Preterm births (birth before 32 weeks)")
  ggsave(file.path(pth10, paste0(nm, ".png")), width = 10, height = 12)
}




cesarean_rf_geo_summ <- lapply(fcts, function(x) {
  message(x)
  summarize_outcome_st(d, nm = "deliv_type", rf = x, val = "Cesarean")
})
names(cesarean_rf_geo_summ) <- fcts

pth11 <- "results/cadu_sinasc2/cesarean_rf_geo_summ"
dir.create(pth11, recursive = TRUE)

for (nm in names(cesarean_rf_geo_summ)) {
  message(nm)
  plot_outcome_st(cesarean_rf_geo_summ[[nm]],
    ylab = "Proportion of cesarean births")
  ggsave(file.path(pth11, paste0(nm, ".png")), width = 10, height = 12)
}




lapply(fcts, function(x) length(levels(d[[x]])))

geo_time_summ <- lapply(fcts, function(x) {
  message(x)
  summarize_st_yr(d, !! sym(x))  
})
names(geo_time_summ) <- fcts

pth6 <- "results/cadu_sinasc2/geo_time_summ"
dir.create(pth6, recursive = TRUE)

geo_time_summ$n_prenat_visit_cat <- NULL
geo_time_summ$n_live_child_cat <- NULL
geo_time_summ$n_vag_deliv_cat <- NULL

for (nm in names(geo_time_summ)) {
  plot_st_yr(geo_time_summ[[nm]], nm, llab = nm)
  ggsave(file.path(pth6, paste0(nm, ".png")), width = 11, height = 12)
}

# now with keeping NAs
geo_time_summ2 <- lapply(fcts, function(x) {
  message(x)
  summarize_st_yr(d, !! sym(x), keep_na = TRUE)
})
names(geo_time_summ2) <- fcts

geo_time_summ2$n_prenat_visit_cat <- NULL
geo_time_summ2$n_live_child_cat <- NULL
geo_time_summ2$n_vag_deliv_cat <- NULL

for (nm in names(geo_time_summ2)) {
  plot_st_yr(geo_time_summ2[[nm]], nm, llab = nm)
  ggsave(file.path(pth6, paste0(nm, "_na.png")), width = 11, height = 12)
}





hist(d$gest_weeks)
hist(d$brthwt_z)
mean(d$brthwt_z, na.rm = TRUE)
# 0.27



# Study of the impact of the type of hospital and type of delivery on the gestational age at birth in the Municipality of São Paulo, Brazil, in the city of São Paulo, stratified by schooling 2013-2014 ")

filter(brazilgeo::br_muni_codes, muni_name == "São Paulo")

sp <- filter(d, resi_muni_code == "355030") # & birth_year %in% c(2013, 2014))

# major_degree

library(tidyr)
library(rlang)

get_tables <- function(sp, edu_var) {
  summ <- sp %>%
    group_by(!! sym(edu_var), deliv_type, preterm) %>%
    summarise(n = n())
  
  sum(summ$n)
  
  summ <- summ[complete.cases(summ), ]
  
  sum(summ$n)
  
  summspl <- split(summ, summ[[edu_var]])
  
  lapply(summspl, function(x) {
    x %>%
      ungroup() %>%
      select(- !! sym(edu_var)) %>%
      spread(deliv_type, n) %>%
      mutate(
        `Total Births` = Cesarean + Normal,
        `Percent Cesarean` = round(100 * Cesarean / (Cesarean + Normal), 1))
  })
}

tb1 <- get_tables(sp, "m_edu_level")

tb2 <- get_tables(sp, "major_degree")
names(tb2) <- c("No Major Degree", "Major Degree")

tb1
tb2


summm <- sp %>%
  group_by(major_degree, deliv_type, preterm, birth_year) %>%
  summarise(n = n())

summm <- summm[complete.cases(summm), ]

a <- summm %>%
  group_by(major_degree, preterm, birth_year) %>%
  mutate(pct = n / sum(n)) %>%
  filter(deliv_type == "Cesarean")

ggplot(a, aes(birth_year, pct, color = major_degree)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ preterm, nrow = 1) +
  scale_x_continuous(breaks = c(2013, 2014, 2015), labels = c("'13", "'14", "'15"))




summm2 <- d %>%
  group_by(major_degree, deliv_type, preterm, birth_year) %>%
  summarise(n = n())

summm2 <- summm2[complete.cases(summm2), ]

a2 <- summm2 %>%
  group_by(major_degree, preterm, birth_year) %>%
  mutate(pct = n / sum(n)) %>%
  filter(deliv_type == "Cesarean")

ggplot(a2, aes(birth_year, pct, color = major_degree)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ preterm, nrow = 1) +
  scale_x_continuous(breaks = c(2013, 2014, 2015), labels = c("'13", "'14", "'15")) +
  ylim(c(0, 1))



