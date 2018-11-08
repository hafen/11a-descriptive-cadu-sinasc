
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

pth2 <- "results/cadu_sinasc/gest_weeks_summ"
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




brthwt_cat_summ <- lapply(c(fcts, addl), function(x) {
  message(x)
  summarize_bvar_by(d, brthwt_cat, !! sym(x), "<2500g")
})
names(brthwt_cat_summ) <- c(fcts, addl)

pth4 <- "results/cadu_sinasc/brthwt_cat_summ"
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

pth5 <- "results/cadu_sinasc/sga_summ"
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

pth7 <- "results/cadu_sinasc/lga_summ"
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
      p = length(which(!! sym(nm) == val)) / n,
      se = sqrt(p * (1 - p) / n)
    )
  summ <- summ[complete.cases(summ),]
  
  rng <- range(c(summ$p - 2 * summ$se, summ$p + 2 * summ$se))
  names(summ)[1] <- "yvar"
  summ$yvar2 <- as.numeric(summ$yvar)
  attribute(summ, "rf") <- rf
  attribute(summ, "nm") <- nm
  summ
}

plot_outcome_st <- function(summ) {
  atrs <- attributes(summ)
  ggplot(summ, aes(yvar, p, xmin = yvar2 - 0.5, xmax = yvar2 + 0.5)) +
    geom_rect(aes(ymin = rng[1], ymax = p), fill = tableau10[1], alpha = 0.6) +
    geom_rect(aes(ymin = p, ymax = rng[2]), fill = tableau10[1], alpha = 0.3) +
    theme_bw() +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = p - 2 * se, ymax = p + 2 * se), width = 0.2) +
    facet_geo(~ state_code, grid = "br_states_grid2", label = "name") +
    theme(
      strip.text.x = element_text(margin = margin(0.1, 0, 0.1, 0, "cm"), size = 7),
      axis.text.x = element_text(angle = 60, hjust = 1)) +
    labs(x = atr$rf, y = atr$nm)
}

lga_rf_geo_summ <- lapply(fcts, function(x) {
  message(x)
  summarize_outcome_st(d, nm = "brthwt_centile_cat", rf = x, val = "Percentile > 90 (LGA)")
})
names(lga_rf_geo_summ) <- fcts

pth8 <- "results/cadu_sinasc/lga_rf_geo_summ"
dir.create(pth8, recursive = TRUE)








lapply(fcts, function(x) length(levels(d[[x]])))

geo_time_summ <- lapply(fcts, function(x) {
  message(x)
  summarize_st_yr(d, !! sym(x))  
})
names(geo_time_summ) <- fcts

pth6 <- "results/cadu_sinasc/geo_time_summ"
dir.create(pth6, recursive = TRUE)

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

for (nm in names(geo_time_summ2)) {
  plot_st_yr(geo_time_summ2[[nm]], nm, llab = nm)
  ggsave(file.path(pth6, paste0(nm, "_na.png")), width = 11, height = 12)
}





hist(d$gest_weeks)
hist(d$brthwt_z)
mean(d$brthwt_z, na.rm = TRUE)
# 0.27





