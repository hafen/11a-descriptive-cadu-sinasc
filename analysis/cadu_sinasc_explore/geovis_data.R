library(dplyr)

d <- data.table::fread("~/Desktop/Rally/CoorteKI_Rally/KI_Rally_Sprint11A/Shared_Rally/CINASC_CADU_mapped_11-12.csv", na.strings = "")

length(which(is.na(d$resi_muni_code)))

library(brazilgeo)
mc <- br_muni_codes
mc$resi_muni_code <- substr(mc$muni_code, 1, 6)
mc$muni_code <- NULL
d$resi_muni_code <- as.character(d$resi_muni_code)
d <- left_join(d, select(mc, resi_muni_code, state_code))

get_pct <- function(x, val) {
  length(which(x == val)) / length(which(!is.na(x)))
}

get_pct_in <- function(x, vals) {
  length(which(x %in% vals)) / length(which(!is.na(x)))
}

get_summaries <- function(dat, geo = "resi_muni_code", bf = c("all", "yes", "no")) {
  
  bf <- match.arg(bf)
  if (bf == "yes") {
    dat <- filter(dat, bf == "Recipient")
  } else if (bf == "no") {
    dat <- filter(dat, bf == "Non-Recipient")
  }

  dat %>%
    group_by(!! sym(geo), birth_year) %>%
    summarise(
      n_births_log10 = log10(n()),
      brthwt_g = mean(brthwt_g, na.rm = TRUE),
      sga_pct = get_pct(brthwt_centile_cat, "Percentile < 10 (SGA)"),
      lga_pct = get_pct(brthwt_centile_cat, "Percentile > 90 (LGA)"),
      sga_pct2 = get_pct(brthwt_centile_cat2, "Percentile < 10 (SGA)"),
      lga_pct2 = get_pct(brthwt_centile_cat2, "Percentile > 90 (LGA)"),
      preterm_pct = get_pct_in(preterm, c("Extremely Preterm", "Very Preterm")),
      preterm_pct2 = get_pct_in(preterm2, c("Extremely Preterm", "Very Preterm")),
      bf_pct = get_pct(bf, "Recipient"),
      ces_prelabor_pct = get_pct(ces_pre_labor, "Yes"),
      ces_pct = get_pct(deliv_type, "Cesarean"),
      major_degree_pct = get_pct(major_degree, TRUE),
      induced_pct = get_pct(labor_induced, "Yes"),
      teen_pct = get_pct_in(m_age_cat, c("< 15", "[15,20)")),
      race_white_pct = get_pct(m_race, "White"),
      single_mom_pct = get_pct(marital_status, "Single"),
      visits7_pct = get_pct(n_prenat_visit_cat, "7 or more"),
      n_prev_preg = mean(n_prev_preg, na.rm = TRUE),
      apgar1 = mean(apgar1, na.rm = TRUE),
      gest_month_precare = mean(gest_month_precare, na.rm = TRUE)
    )
}

pars <- expand.grid(
  bf = c("all", "yes", "no"),
  geo = c("resi_muni_code", "state_code"),
  stringsAsFactors = FALSE
)

summs <- lapply(seq_len(nrow(pars)), function(ii) {
  cp <- pars[ii, ]
  message(paste(cp, collapse = " "))
  get_summaries(d, geo = cp$geo, bf = cp$bf)
})


readr::write_csv(summs[[1]], "muni_bf_all.csv")
readr::write_csv(summs[[2]], "muni_bf_yes.csv")
readr::write_csv(summs[[3]], "muni_bf_no.csv")
readr::write_csv(summs[[4]], "state_bf_all.csv")
readr::write_csv(summs[[5]], "state_bf_yes.csv")
readr::write_csv(summs[[6]], "state_bf_no.csv")
