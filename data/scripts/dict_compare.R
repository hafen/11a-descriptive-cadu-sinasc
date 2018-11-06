library(dplyr)

a <- readr::read_csv("data/discovered/_raw/2018_10_22_Dictionary_SINASC.csv",
  locale = readr::locale(encoding = "latin1"))

a$X1 <- NULL
a$variavel <- tolower(a$variavel)

library(cidacsdict)
snsc_dict <- dplyr::filter(cdcs_dict, db == "SINASC")

length(intersect(snsc_dict$name, a$variavel))
# 31 variables in common (out of 42)
# the rest are CADU

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


name
name_en
label
label_en
map
map_en
type

snsccdu <- a %>%
  select(-type) %>%
  rename(
    name = variavel,
    label = descricao,
    label_en = description,
    map = classes,
    map_en = labels,
    type = tipo
  )

map_to_list <- function(x) {
  if (is.na(x)) {
    # message("hi")
    return(list(NA))
  }

  # message(x)
  # x <- snsccdu$map[1]
  x <- gsub("\\{", "", x)
  x <- gsub("\\}", "", x)
  x <- gsub("\"", "", x)
  x <- gsub(" \\:", ":", x)
  x <- gsub("\\: ", ":", x)
  xx <- strsplit(x, ",")[[1]]
  xx <- strsplit(xx, ":")
  nms <- unlist(lapply(xx, "[[", 1))
  vals <- lapply(xx, "[[", 2)
  names(vals) <- nms

  vals
}

snsccdu$map <- lapply(snsccdu$map, map_to_list)
snsccdu$map_en <- lapply(snsccdu$map_en, map_to_list)

snsccdu$type[snsccdu$type == "Inteiro"] <- "integer"
snsccdu$type[snsccdu$type == "Categórica"] <- "factor"
snsccdu$type[snsccdu$type == "Data"] <- "date"

tmp <- cdcs_dict$name_en
names(tmp) <- cdcs_dict$name

snsccdu$name_en <- unname(tmp[snsccdu$name])

snsccdu %>%
  filter(is.na(name_en)) %>%
  select(name, label_en)

snsccdu$name_en[snsccdu$label_en == "Code of household waste"] <- "house_waste"
snsccdu$name_en[snsccdu$label_en == "Household sanitary disposal code"] <- "house_sanitary"
snsccdu$name_en[snsccdu$label_en == "Code of household water supply"] <- "house_water"

