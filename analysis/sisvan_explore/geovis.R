library(geovis)

geo <- get_geo_data("brazil", get_munis = TRUE, scale = 10)
geo$muni$state_code <- geo$muni$state_code2
geo$muni$state_code2 <- NULL
geo$muni$muni_code <- substr(geo$muni$muni_code, 1, 6)

brmuni <- brazilgeo::br_muni_codes
brmuni$muni_code <- substr(brmuni$muni_code, 1, 6)

load("data/discovered/_raw/sisvanweb/geo_data.Rdata")

country_data <- data.frame(
  year = 2008:2017,
  country_code = "BRA",
  stunting = mean(state_data$stunting),
  wasting = mean(state_data$wasting),
  overweight_wh = mean(state_data$overweight_wh),
  overweight_bmi = mean(state_data$overweight_bmi),
  stunting_wh_ratio = mean(state_data$stunting_wh_ratio),
  stunting_bmi_ratio = mean(state_data$stunting_bmi_ratio),
  wh_stunting_ratio = mean(state_data$wh_stunting_ratio),
  bmi_stunting_ratio = mean(state_data$bmi_stunting_ratio),

  stunting02 = mean(state_data$stunting02),
  wasting02 = mean(state_data$wasting02),
  overweight_wl02 = mean(state_data$overweight_wl02),
  overweight_bmi02 = mean(state_data$overweight_bmi02),
  stunting_wl_ratio02 = mean(state_data$stunting_wl_ratio02),
  stunting_bmi_ratio02 = mean(state_data$stunting_bmi_ratio02),
  wl_stunting_ratio02 = mean(state_data$wl_stunting_ratio02),
  bmi_stunting_ratio02 = mean(state_data$bmi_stunting_ratio02),

  stunting25 = mean(state_data$stunting25),
  wasting25 = mean(state_data$wasting25),
  overweight_wh25 = mean(state_data$overweight_wh25),
  overweight_bmi25 = mean(state_data$overweight_bmi25),
  stunting_wh_ratio25 = mean(state_data$stunting_wh_ratio25),
  stunting_bmi_ratio25 = mean(state_data$stunting_bmi_ratio25),
  wh_stunting_ratio25 = mean(state_data$wh_stunting_ratio25),
  bmi_stunting_ratio25 = mean(state_data$bmi_stunting_ratio25),

  n_haz = mean(state_data$n_haz),
  n_whz = mean(state_data$n_whz),
  n_bmi = mean(state_data$n_bmi),
  stringsAsFactors = FALSE
)

state_data$country_code <- "BRA"
muni_data$country_code <- "BRA"

hist(muni_data$stunting_wl_ratio02)
hist(log10(muni_data$stunting_wl_ratio02))
abline(h = 4)
plot(sort((muni_data$stunting_wl_ratio02)))
quantile(muni_data$stunting_wl_ratio02, 0.99, na.rm = TRUE)

plot(sort(log10(muni_data$stunting_wh_ratio25)))
quantile(muni_data$stunting_wh_ratio25, 0.99)

vars <- data_frame(
  name = c(
    "stunting", "wasting", "overweight_wh",
"overweight_bmi", "stunting_wh_ratio", "stunting_bmi_ratio", "wh_stunting_ratio", "bmi_stunting_ratio",
    "stunting02", "wasting02", "overweight_wl02",
"overweight_bmi02", "stunting_wl_ratio02", "stunting_bmi_ratio02", "wl_stunting_ratio02", "bmi_stunting_ratio02",
    "stunting25", "wasting25", "overweight_wh25",
"overweight_bmi25", "stunting_wh_ratio25", "stunting_bmi_ratio25", "wh_stunting_ratio25", "bmi_stunting_ratio25",
    "n_haz", "n_whz", "n_bmi"),
  desc = c(
    "% 0-5y stunted children",
    "% 0-5y wasted children",
    "% 0-5y overweight children (WHZ)",
    "% 0-5y overweight children (BMI)",
    "0-5y stunting / overweight (WHZ)",
    "0-5y stunting / overweight (BMI)",
    "0-5y overweight (WHZ) / stunting",
    "0-5y overweight (BMI) / stunting",

    "% 0-2y stunted children",
    "% 0-2y wasted children",
    "% 0-2y overweight children (WLZ)",
    "% 0-2y overweight children (BMI)",
    "0-2y stunting / overweight (WLZ)",
    "0-2y stunting / overweight (BMI)",
    "0-2y overweight (WLZ) / stunting",
    "0-2y overweight (BMI) / stunting",

    "% 2-5y stunted children",
    "% 2-5y wasted children",
    "% 2-5 y overweight children (WHZ)",
    "% 2-5y overweight children (BMI)",
    "2-5y stunting / overweight (WHZ)",
    "2-5y stunting / overweight (BMI)",
    "2-5y overweight (WHZ) / stunting",
    "2-5y overweight (BMI) / stunting",

    "# 0-5y child evaluated for HAZ",
    "# 0-5y child evaluated for WHZ",
    "# 0-5y child evaluated for BMI"
  )
)

muni_data <- muni_data %>%
  filter(muni_code %in% geo$muni$muni_code) %>%
  arrange(year, muni_code)

state_data <- state_data %>%
  arrange(year, state_code)

var_info <- list()
for (i in seq_len(nrow(vars))) {
  nm <- vars$name[i]
  desc <- vars$desc[i]
  cur <- list(name = desc)

  curdat <- c(
    country_data[[nm]],
    state_data[[nm]],
    muni_data[[nm]]
  )
  # plot(sort(curdat))
  q99 <- unname(quantile(curdat, 0.99, na.rm = TRUE))
  curdat <- curdat[curdat < q99]
  cur$breaks <- pretty(curdat)
  cur$range <- range(curdat, na.rm = TRUE)

  var_info[[nm]] <- cur
}

geovis(geo, path = "~/Desktop/geowidget",
  name = "Brazil SISVAN-Web Explorer",
  view_level = "country",
  view_country_code = "BRA",
  default_var = "stunting02",
  var_info = var_info,
  country_data = country_data,
  state_data = state_data,
  muni_data = muni_data
)

# for both 0-2 and 2-5
# stunted (use haz) < -2
# wasted (use whz) < -2
# underweight (use waz) < -2
# overweight (use whz) > 2

rsconnect::deployApp(
  appDir = "~/Desktop/geowidget/",
  appName = "geovis_example",
  appTitle = "Example of geovis with SISVAN-Web data",
  server = "rstudio.studyexplorer.io",
  contentCategory = "application")
