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
  overweight_stunting_ratio1 = mean(state_data$overweight_stunting_ratio1),
  overweight_stunting_ratio2 = mean(state_data$overweight_stunting_ratio2),
  n_haz = mean(state_data$n_haz),
  n_whz = mean(state_data$n_whz),
  n_bmi = mean(state_data$n_bmi),
  stringsAsFactors = FALSE
)

state_data$country_code <- "BRA"
muni_data$country_code <- "BRA"

plot(sort(log10(muni_data$overweight_stunting_ratio1)))
quantile(muni_data$overweight_stunting_ratio1, 0.99)

vars <- data_frame(
  name = c("stunting", "wasting", "overweight_wh",
"overweight_bmi", "overweight_stunting_ratio1", "overweight_stunting_ratio2", "n_haz", "n_whz", "n_bmi"),
  desc = c(
    "Percent 0-5y stunted children",
    "Percent 0-5y wasted children",
    "Percent 0-5 y overweight children (WHZ)",
    "Percent 0-5y overweight children (BMI)",
    "Prevalence of overweight (WHZ) / stunting",
    "Prevalence of overweight (BMI) / stunting",
    "Number of children evaluated for HAZ",
    "Number of children evaluated for WHZ",
    "Number of children evaluated for BMI"
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

muni_data <- muni_data %>%
  select(-muni_name, -micro_name, -micro_code, -meso_name,
    -meso_code, -state_name, -region_name, -region_code)

geovis(geo, path = "~/Desktop/geowidget",
  name = "Brazil SISVAN-Web Explorer",
  view_level = "country",
  view_country_code = "BRA",
  default_var = "stunting",
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
