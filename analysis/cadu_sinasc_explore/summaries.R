fcts <- c("sex", "cong_anom", "birth_place", "marital_status", "m_edu_level", 
  "m_edu_level_aggregated", "m_race", "preg_type", "deliv_type", 
  "n_prenat_visit_cat", "gest_method", "presentation", "labor_induced", 
  "ces_pre_labor", "birth_assist", "household_type", "household_material", 
  "household_water", "household_sanitary", "household_lighting", 
  "household_waste", "bf", "child_death", "n_children", "m_age_cat", "education_recl",
  "hour_dat", "major_degree")

d <- filter(d, abs(brthwt_z) < 5 | is.na(brthwt_z))
d <- filter(d, gest_weeks > 20 | is.na(gest_weeks))

library(brazilgeo)
mc <- br_muni_codes
mc$resi_muni_code <- substr(mc$muni_code, 1, 6)
mc$muni_code <- NULL
d$resi_muni_code <- as.character(d$resi_muni_code)
d <- left_join(d, select(mc, resi_muni_code, micro_code, meso_code, state_code, region_code))

source("_fns.R")

addl <- c("m_age_yrs", "n_live_child", "n_dead_child", "n_prev_preg", "n_vag_deliv", "n_ces_deliv", "gest_weeks", "n_prenat_visit", "gest_month_precare", "n_household", "n_children", "birth_year")

brthwt_z_summ <- lapply(c(fcts, addl), function(x) {
  message(x)
  summarize_var_by(d, brthwt_z, !! sym(x))
})
names(brthwt_z_summ) <- c(fcts, addl)

pth <- "results/cadu_sinasc/brthwt_z_summ"
dir.create(pth, recursive = TRUE)

for (nm in names(brthwt_z_summ)) {
  lab <- filter(snsccdu, name_en == nm)$label_en
  if (length(lab) == 0) {
    lab <- nm
  } else {
    lab <- paste0(lab, " (", nm, ")")
  }
  dd <- filter(brthwt_z_summ[[nm]], n > 50)
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

pth2 <- "results/cadu_sinasc/gest_weeks_summ"
dir.create(pth2, recursive = TRUE)

for (nm in names(gest_weeks_summ)) {
  lab <- filter(snsccdu, name_en == nm)$label_en
  if (length(lab) == 0) {
    lab <- nm
  } else {
    lab <- paste0(lab, " (", nm, ")")
  }
  dd <- filter(gest_weeks_summ[[nm]], n > 50)
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


fcts <- names(which(sapply(d, is.factor)))

deliv_type_summ <- lapply(c(fcts, addl), function(x) {
  message(x)
  summarize_bvar_by(d, deliv_type, !! sym(x), "Cesarean")
})
names(deliv_type_summ) <- c(fcts, addl)

pth3 <- "results/cadu_sinasc/deliv_type_summ"
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




lapply(fcts, function(x) length(levels(d[[x]])))

geo_time_summ <- lapply(fcts, function(x) {
  message(x)
  summarize_st_yr(d, !! sym(x))  
})
names(geo_time_summ) <- fcts

pth3 <- "results/cadu_sinasc/geo_time_summ"
dir.create(pth3, recursive = TRUE)

for (nm in names(geo_time_summ)) {
  plot_st_yr(geo_time_summ[[nm]], nm, llab = nm)
  ggsave(file.path(pth3, paste0(nm, ".png")), width = 11, height = 12)
}

# now with keepnig NAs
geo_time_summ2 <- lapply(fcts, function(x) {
  message(x)
  summarize_st_yr(d, !! sym(x), keep_na = TRUE)
})
names(geo_time_summ2) <- fcts

for (nm in names(geo_time_summ2)) {
  plot_st_yr(geo_time_summ2[[nm]], nm, llab = nm)
  ggsave(file.path(pth3, paste0(nm, "_na.png")), width = 11, height = 12)
}





hist(d$gest_weeks)
hist(d$brthwt_z)
mean(d$brthwt_z, na.rm = TRUE)
# 0.27
