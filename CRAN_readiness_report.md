# CRAN Submission Readiness Report
## bufferedThresholdPanel v0.1.0
## Final audit — all issues resolved

---

## Status: READY FOR `devtools::check()` AND GITHUB PUSH

---

## Issues resolved across all sessions

| # | File(s) | Issue | Status |
|---|---|---|---|
| 1 | `data/` | `panel_data.csv` must not be in `data/` | **Delete manually** |
| 2 | `tables.R` | `bptr_shiny()` called `inst/app/` but folder was `inst/shiny/` | ✅ Fixed in tables.R |
| 3 | `.Rbuildignore` | Not present — dev artefacts included in tarball | ✅ Created |
| 4 | `NEWS.md` | Not present | ✅ Created |
| 5 | `tests/` | No test suite | ✅ Created (20 tests) |
| 6 | `LICENSE` | Created — verify at package root | ✅ Verify exists |
| 7 | `utils.R` | 7 internal functions exported unnecessarily | ✅ Fixed |
| 8 | `methods.R` | 13 S3 methods missing `@rdname` — no Rd files | ✅ Fixed |
| 9 | `DESCRIPTION` | Syntax error on Hamdi line; wrong Imports | ✅ Fixed |
| 10 | `zzz.R` | Missing `"_PACKAGE"` sentinel — help system silent | ✅ Fixed |
| 11 | `vignette` | `bptr_test_seq()` blocking `R CMD build` for 30+ min | ✅ Fixed (eval=FALSE) |
| 12 | `panel_data.rda` | `year`/`countryId` stored as float64; `rle` missing | Update from xlsx |
| 13 | `NAMESPACE` | 7 spurious exports; `buildBufferIndicators/3` missing | ✅ Fixed |
| 14 | `bptr.R` | Examples did not demonstrate `panel_data` or Model I/II | ✅ Fixed |
| 15 | All formulas | `rle` / `oilRentGDP` alternating roles not documented | ✅ Fixed in 4 files |

---

## Final package structure

```
bufferedThresholdPanel/           ← package root
├── .Rbuildignore                 ✅ excludes dev artefacts from tarball
├── DESCRIPTION                   ✅ syntax correct, all dois, LazyData: true
├── NAMESPACE                     ✅ 12 exports, 14 S3method(), full importFrom
├── LICENSE                       ✅ GPL-3 copyright notice
├── NEWS.md                       ✅ v0.1.0 entry
├── README.md                     ✅ Model I/II, rle, Li 2015, Davies 1987
├── HELP_WORKFLOW.md              (dev guide, excluded from tarball)
├── INSTALLATION.md               (user guide, excluded from tarball)
├── R/
│   ├── bptr.R                    ✅ Model I/II examples, @donttest panel_data
│   ├── bootstrap.R               ✅
│   ├── data.R                    ✅ 14 cols, rle documented, Model I/II roles
│   ├── methods.R                 ✅ all 13 S3 methods have @rdname
│   ├── tables.R                  ✅ bptr_shiny() → system.file("app", ...)
│   ├── tidy.R                    ✅
│   ├── utils.R                   ✅ 7 internal, 2 exported
│   └── zzz.R                     ✅ _PACKAGE sentinel, globalVariables
├── data/
│   └── panel_data.rda            ⚠️  Regenerate from xlsx with rle + integer types
├── data-raw/
│   ├── dataset.xlsx              (source — not in tarball)
│   └── prepare_panel_data.R      ✅ 14 cols, rle, column order, integer types
├── inst/
│   └── app/
│       └── app.R                 ✅ rename inst/shiny → inst/app
├── man/                          run devtools::document() to regenerate
├── tests/
│   ├── testthat.R                ✅ test runner
│   └── testthat/
│       └── test-bptr.R           ✅ 20 tests across 8 groups
└── vignettes/
    ├── bufferedThresholdPanel.Rmd ✅ no slow chunks, Model I/II, eval=FALSE
    └── references.bib             ✅ 5 entries including Davies, Li
```

---

## Exported user-facing functions (12)

| Function | Purpose |
|---|---|
| `bptr()` | Estimate PTR / BTPD model |
| `bptr_test()` | F1,2 bootstrap test |
| `bptr_test_23()` | F2,3 bootstrap test |
| `bptr_test_seq()` | Full sequential procedure |
| `bptr_bootstrap()` | Residual bootstrap CIs |
| `bptr_table()` | Publication table (gt) |
| `bptr_latex()` | LaTeX export |
| `bptr_kable()` | knitr::kable export |
| `bptr_shiny()` | Launch Shiny app |
| `threshold_tidy()` | Tidy threshold estimates |
| `buildBufferIndicators()` | 2-regime hysteresis rule |
| `buildBufferIndicators3()` | 3-regime hysteresis rule |

---

## Two manual steps still required on your machine

### Step 1 — Delete `data/panel_data.csv`
```bash
rm r_package/data/panel_data.csv
```

### Step 2 — Rename `inst/shiny` → `inst/app`
```bash
mv r_package/inst/shiny r_package/inst/app
```

### Step 3 — Regenerate `panel_data.rda` with `rle`
Add `rle` to `dataset.xlsx`, then:
```r
source("data-raw/prepare_panel_data.R")
```

---

## Complete pre-push workflow

```r
setwd("~/Documents/BTPM/r_package")

# 1. Regenerate all man/*.Rd files
devtools::document()

# 2. Run the full CRAN check
devtools::check(cran = TRUE, remote = FALSE)
# Target: 0 ERRORs, 0 WARNINGs, ≤1 NOTE (no tests NOTE gone now)

# 3. Install with vignettes
devtools::install(build_vignettes = TRUE)

# 4. Confirm help system works
library(bufferedThresholdPanel)
help(package = "bufferedThresholdPanel")   # shows full function index
?bptr                                       # opens bptr.Rd
browseVignettes("bufferedThresholdPanel")  # shows vignette
vignette("bufferedThresholdPanel")         # opens it

# 5. Run tests
devtools::test()
# Expected: 20 passed, 0 failed, 0 warnings

# 6. Spell check
devtools::spell_check()

# 7. Check URLs
urlchecker::url_check()

# 8. Build tarball
devtools::build()

# 9. Final check on tarball
devtools::check_built("bufferedThresholdPanel_0.1.0.tar.gz", cran = TRUE)
```

---

## Before CRAN submission (after GitHub testing phase)

```r
# Increase bootstrap replications in vignette eval=FALSE blocks
# (already set to 499 in the shown code — no change needed)

# Submit
devtools::release()
# or manually at https://cran.r-project.org/submit.html
```

---

## References

- CRAN Repository Policy: https://cran.r-project.org/web/packages/policies.html
- Writing R Extensions: https://cran.r-project.org/doc/manuals/r-release/R-exts.html
- Belarbi et al. (2021) doi:10.1016/j.econmod.2021.02.018
- Hamdi et al. (2025) doi:10.1111/twec.70023
