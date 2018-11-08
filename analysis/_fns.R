## some universal utility functions and packages to load
##---------------------------------------------------------

library(tidyverse)
library(geofacet)
# library(ggthemes)

tableau10 <- c('#1F77B4', '#FF7F0E', '#2CA02C', '#D62728', '#9467BD', '#8C564B', '#CFECF9', '#7F7F7F', '#BCBD22', '#17BECF')

theme_set(theme_bw())
scale_colour_discrete <- function(...)
  scale_colour_manual(..., values = tableau10)
scale_fill_discrete <- function(...)
  scale_fill_manual(..., values = tableau10)

## functions to summarize a variable by another and plot
##---------------------------------------------------------

summarize_var_by <- function(dat, x, by) {
  x <- enquo(x)
  by <- enquo(by)
  
  dat %>%
    filter(!is.na(!!by)) %>%
    group_by(!!by) %>%
    summarise(
      n = n(),
      med  = median(!!x, na.rm = TRUE),
      q1 = quantile(!!x, 0.25, na.rm = TRUE),
      q3 = quantile(!!x, 0.75, na.rm = TRUE),
      mad = mad(!!x, na.rm = TRUE),
      mean = mean(!!x, na.rm = TRUE),
      sd = sd(!!x, na.rm = TRUE),
      se = sd / sqrt(n()))
}

plot_var_by <- function(dat, x, by, se = FALSE, xlab = NULL, ylab = NULL) {
  dat <- dat[!is.na(dat[[by]]), ]
  
  dat$by <- dat[[by]]
  if (is.null(xlab))
    xlab <- by
  if (is.null(ylab))
    ylab <- x
  
  if (se) {
    p <- ggplot(dat, aes(by, mean)) +
      geom_point(size = 2) +
      theme_bw() +
      labs(x = xlab, y = ylab) +
      geom_errorbar(aes(ymin = mean - 2 * se, ymax = mean + 2 * se), width = 0.2)
  } else {
    p <- ggplot(dat, aes(by, med)) +
      geom_point(size = 2) +
      theme_bw() +
      labs(x = xlab, y = ylab) +
      geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.2)
  }
  
  p
}

# for a bivariate outcome
summarize_bvar_by <- function(dat, x, by, val) {
  x <- enquo(x)
  by <- enquo(by)

  dat %>%
    group_by(!!by) %>%
    summarise(
      n = length(which(!is.na(!!x))),
      p = length(which(!!x == val)) / n,
      se = sqrt(p * (1 - p) / n)
    )
}

plot_bvar_by <- function(dat, x, by, xlab = NULL, ylab = NULL) {
  dat <- dat[!is.na(dat[[by]]), ]

  dat$by <- dat[[by]]
  if (is.null(xlab))
    xlab <- by
  if (is.null(ylab))
    ylab <- x

  p <- ggplot(dat, aes(by, p)) +
    geom_point(size = 2) +
    theme_bw() +
    labs(x = xlab, y = ylab)

  p + geom_errorbar(aes(ymin = p - 2 * se, ymax = p + 2 * se), width = 0.2)
}


## functions to summarize and plot by state and year
##---------------------------------------------------------

summarize_st_yr <- function(dat, var, keep_na = FALSE) {
  var <- enquo(var)
  
  res <- dat %>%
    group_by(state_code, birth_year, !!var) %>%
    tally()
  
  if (!keep_na)
    res <- filter(res, !is.na(!!var))
  
  res %>%
    filter(!is.na(state_code)) %>%
    group_by(birth_year, state_code) %>%
    mutate(
      n_yst = sum(n),
      pct = n / n_yst * 100)
}

plot_st_yr <- function(dat, var, ylab = NULL, llab = NULL) {
  if (is.null(ylab))
    ylab <- paste0("Percentage of Births")
  
  if (is.null(llab))
    llab <- var
  
  dat$fill_var <- dat[[var]]
  
  ggplot(dat, aes(birth_year, pct, fill = fill_var)) +
    geom_col(position = position_stack(), width = 1, alpha = 0.7) +
    geom_abline(slope = 0, intercept = 50, alpha = 0.25) +
    theme_bw() +
    scale_fill_manual(name = llab, values = tableau10) +
    scale_x_continuous(expand = c(0, 0),
      labels = function(x) paste0("'", substr(x, 3, 4))) +
    scale_y_continuous(expand = c(0, 0)) +
    facet_geo(~ state_code, grid = "br_states_grid2", label = "name") +
    theme(strip.text.x = element_text(margin = margin(0.1, 0, 0.1, 0, "cm"),
      size = 7)) +
    labs(x = "Year", y = ylab)
}
