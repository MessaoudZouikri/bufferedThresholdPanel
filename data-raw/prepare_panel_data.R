## ============================================================
## data-raw/prepare_panel_data.R
##
## Reproducible script for creating the panel_data dataset.
## Run once with: source("data-raw/prepare_panel_data.R")
##
## Source data: dataset.xlsx (sheet "data")
## Variables: countryId, country, year, growthRate, oilRentGDP, rle,
##            initialGDP, eci, fdiGDP, capFormGDP, inflation,
##            popGrowth, indVAGDP, tradeOpenness
##
## Column roles in the two empirical models:
##   Model I  — oilRentGDP: threshold variable  |  rle: predictor
##   Model II — rle: threshold variable          |  oilRentGDP: predictor
## ============================================================

library(readxl)
library(usethis)

# ---- Read the raw Excel file -------------------------------------------- #
raw <- read_excel("data-raw/dataset.xlsx", sheet = "data")

# Confirm rle is present in the raw data
if (!"rle" %in% names(raw))
  stop("Column 'rle' not found in dataset.xlsx. Add Rule of Law data first.")

# ---- Build numeric country ID (alphabetical) ---------------------------- #
country_levels <- sort(unique(raw$id))
panel_data <- as.data.frame(raw)
panel_data <- cbind(
  countryId = match(panel_data$id, country_levels),
  country   = panel_data$id,
  panel_data[, setdiff(names(panel_data), "id")],
  stringsAsFactors = FALSE
)

# ---- Enforce documented column order (14 columns) ----------------------- #
panel_data <- panel_data[, c(
  "countryId", "country", "year",
  "growthRate",                      # dependent variable
  "oilRentGDP",                      # threshold var. Model I / predictor Model II
  "rle",                             # threshold var. Model II / predictor Model I
  "initialGDP", "eci", "fdiGDP",
  "capFormGDP", "inflation", "popGrowth",
  "indVAGDP", "tradeOpenness"
)]

# ---- Enforce correct types ---------------------------------------------- #
panel_data$countryId <- as.integer(panel_data$countryId)
panel_data$year      <- as.integer(panel_data$year)

# ---- Sort by country then year ------------------------------------------ #
panel_data <- panel_data[order(panel_data$countryId, panel_data$year), ]
rownames(panel_data) <- NULL

# ---- Verify -------------------------------------------------------------- #
stopifnot(
  nrow(panel_data)                       == 1380L,
  ncol(panel_data)                       == 14L,
  names(panel_data)[5]                   == "oilRentGDP",
  names(panel_data)[6]                   == "rle",
  !anyNA(panel_data),
  is.integer(panel_data$countryId),
  is.integer(panel_data$year),
  length(unique(panel_data$country))     == 92L,
  length(unique(panel_data$year))        == 15L
)

cat(sprintf(
  "panel_data: N = %d countries, T = %d years, NT = %d obs, cols = %d\n",
  length(unique(panel_data$country)),
  length(unique(panel_data$year)),
  nrow(panel_data),
  ncol(panel_data)
))

# ---- Quick summary of both threshold variables -------------------------- #
cat("\nThreshold variables:\n")
cat(sprintf("  oilRentGDP : mean = %.4f  sd = %.4f  min = %.4f  max = %.4f\n",
    mean(panel_data$oilRentGDP), sd(panel_data$oilRentGDP),
    min(panel_data$oilRentGDP),  max(panel_data$oilRentGDP)))
cat(sprintf("  rle        : mean = %.4f  sd = %.4f  min = %.4f  max = %.4f\n",
    mean(panel_data$rle), sd(panel_data$rle),
    min(panel_data$rle),  max(panel_data$rle)))

# ---- Save as package data ----------------------------------------------- #
usethis::use_data(panel_data, overwrite = TRUE, compress = "bzip2")
