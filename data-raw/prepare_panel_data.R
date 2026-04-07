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

# Fix types
panel_data$countryId <- as.integer(panel_data$countryId)
panel_data$year      <- as.integer(panel_data$year)

# Enforce documented column order (14 columns with rle at position 6)
panel_data <- panel_data[, c("countryId", "country", "year",
                             "growthRate", "oilRentGDP", "rle",
                             "initialGDP", "eci", "fdiGDP",
                             "capFormGDP", "inflation", "popGrowth",
                             "indVAGDP", "tradeOpenness")]

# Verify
stopifnot(
  nrow(panel_data)                    == 1380L,
  ncol(panel_data)                    == 14L,
  names(panel_data)[6]                == "rle",
  !anyNA(panel_data),
  is.integer(panel_data$countryId),
  is.integer(panel_data$year)
)

cat("panel_data: N =", length(unique(panel_data$country)),
    ", T =", length(unique(panel_data$year)),
    ", NT =", nrow(panel_data),
    ", cols =", ncol(panel_data), "\n")

# ---- Save as package data ----------------------------------------------- #
usethis::use_data(panel_data, overwrite = TRUE, compress = "bzip2")








