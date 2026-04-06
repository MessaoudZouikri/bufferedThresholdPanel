## ============================================================
## data-raw/prepare_panel_data.R
##
## Reproducible script for creating the panel_data dataset.
## Run once with: source("data-raw/prepare_panel_data.R")
## Requires: readxl, usethis
## ============================================================

library(readxl)
library(usethis)

# ---- Read the raw Excel file -------------------------------------------- #
raw <- read_excel("data-raw/dataset.xlsx", sheet = "data")

# ---- Build numeric country ID (alphabetical) ---------------------------- #
country_levels <- sort(unique(raw$id))
panel_data <- as.data.frame(raw)
panel_data <- cbind(
  countryId = match(panel_data$id, country_levels),
  country   = panel_data$id,
  panel_data[, setdiff(names(panel_data), "id")],
  stringsAsFactors = FALSE
)

# ---- Enforce types ------------------------------------------------------- #
panel_data$countryId <- as.integer(panel_data$countryId)
panel_data$year      <- as.integer(panel_data$year)

# ---- Sort ---------------------------------------------------------------- #
panel_data <- panel_data[order(panel_data$countryId, panel_data$year), ]
rownames(panel_data) <- NULL

# ---- Verify -------------------------------------------------------------- #
stopifnot(
  nrow(panel_data)              == 1380L,
  length(unique(panel_data$country)) == 92L,
  length(unique(panel_data$year))    == 15L,
  !anyNA(panel_data)
)

cat("panel_data: N =", length(unique(panel_data$country)),
    ", T =", length(unique(panel_data$year)),
    ", NT =", nrow(panel_data), "\n")

# ---- Save as package data ----------------------------------------------- #
usethis::use_data(panel_data, overwrite = TRUE, compress = "bzip2")
