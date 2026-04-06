# Building, Installing, and Accessing Help
## bufferedThresholdPanel — Developer Workflow Guide

---

## Why the help commands returned nothing

Three separate issues, each with a different cause.

---

### Issue 1 — `help(package = "bufferedThresholdPanel")` showed only the DESCRIPTION

**Cause:** The package was installed but `man/*.Rd` files were missing or not
installed alongside it.  This happens when:

- `devtools::document()` was never run (man pages not generated yet), **or**
- The package was installed with `install.packages(..., type = "source")` from
  a folder where `man/` was empty, **or**
- `devtools::install()` was used without first calling `devtools::document()`

**Fix:** Run in this exact order:

```r
# Step 1: generate all man/*.Rd files from roxygen2 @-tags
devtools::document()

# Step 2: install the package (includes the newly generated man/ folder)
devtools::install()

# Step 3: restart R, then test
help(package = "bufferedThresholdPanel")   # should now list all functions
help(bptr)                                  # should open the bptr help page
?bptr                                       # shorthand for help(bptr)
??bufferedThresholdPanel                    # searches the help index
```

After the fix, `help(package = "bufferedThresholdPanel")` will show:
- The package overview page (generated from the `"_PACKAGE"` sentinel in `zzz.R`)
- A clickable index of every exported function

---

### Issue 2 — `browseVignettes("bufferedThresholdPanel")` returned nothing

**Cause:** `devtools::install()` skips vignette compilation by default (it sets
`build_vignettes = FALSE` internally for speed).  The vignette `.Rmd` file is
present in `vignettes/` but has not been compiled into an `.html` file and
registered in the package's help index.

**Fix:** Install the package with vignettes explicitly built:

```r
# Option A — devtools (easiest during development)
devtools::install(build_vignettes = TRUE)

# Option B — command line (closest to CRAN behaviour)
# Run from the directory that CONTAINS the package folder
R CMD build bufferedThresholdPanel
R CMD INSTALL bufferedThresholdPanel_0.1.0.tar.gz
```

After either option, restart R, then:

```r
browseVignettes("bufferedThresholdPanel")
# Opens a browser page listing the vignette with links to HTML and R source

vignette("bufferedThresholdPanel", package = "bufferedThresholdPanel")
# Opens the vignette directly
```

**Important:** Every time you edit the vignette `.Rmd`, you must re-run
`devtools::install(build_vignettes = TRUE)` or `R CMD build` + `R CMD INSTALL`
to see the updated version.

---

### Issue 3 — `help.search("bufferedThresholdPanel")` returned nothing

**Cause:** Two sub-causes:

1. The package had no package-level help page.  `help.search()` indexes the
   `Title` and `Description` fields of every `.Rd` file.  Without a
   `man/bufferedThresholdPanel-package.Rd` file there was nothing to index.
   The `"_PACKAGE"` sentinel added to `zzz.R` creates this file when
   `devtools::document()` is run.

2. The search database may be stale.  After installing or updating a package,
   R sometimes needs its help index rebuilt:

```r
# Rebuild the help search database
tools::makeRdDb()            # rebuilds for all installed packages
# or target just this package:
utils::help.search("bptr")                  # search by function name
utils::help.search("threshold")             # search by keyword
utils::help.search("BTPD")                  # search by acronym in Description
??bptr                                      # shorthand
```

---

### Issue 4 — `man bufferedThresholdPanel` in the terminal returned nothing

**This is expected behaviour and is not a bug.**

`man` is the Unix/Linux system manual command.  It reads `.1`, `.2`, … `.8`
manual pages stored in `/usr/share/man/`.  R packages do **not** create Unix
man pages.  R's own help system is entirely separate.

| Command | Where it runs | What it searches |
|---------|---------------|-----------------|
| `man bufferedThresholdPanel` | Bash/Zsh terminal | Unix system manuals |
| `help(bptr)` | R console | R package help pages (`man/*.Rd`) |
| `?bptr` | R console | Same as above |
| `??bufferedThresholdPanel` | R console | Full-text search of installed R help |
| `help(package = "bufferedThresholdPanel")` | R console | Package overview + function index |
| `browseVignettes("bufferedThresholdPanel")` | R console | Compiled vignettes |

The `man` terminal command will always fail for any R package.  It is not the
right tool.  Use `help()` or `?` inside an R session.

---

## Complete development workflow

Run these commands **in this order** every time you edit the package:

```r
# 0. Set working directory to the package root
setwd("~/Documents/BTPM/r_package")

# 1. Load all R source files without installing (fast iterative testing)
devtools::load_all()

# 2. Generate / update all man/*.Rd help files from @-tags
devtools::document()

# 3. Run R CMD check (catches all CRAN-level issues)
devtools::check()

# 4. Install the package with vignettes
devtools::install(build_vignettes = TRUE)

# 5. Restart R, then verify everything works
library(bufferedThresholdPanel)
help(package = "bufferedThresholdPanel")        # package index
?bptr                                            # function help
browseVignettes("bufferedThresholdPanel")        # vignette
vignette("bufferedThresholdPanel",
         package = "bufferedThresholdPanel")     # open vignette directly
```

---

## Verifying man/ files were created

After `devtools::document()`, the `man/` directory should contain one `.Rd`
file for every exported function plus the package overview:

```
man/
  bufferedThresholdPanel-package.Rd   ← package overview (from zzz.R)
  bptr.Rd
  bptr_bootstrap.Rd
  bptr_kable.Rd
  bptr_latex.Rd
  bptr_shiny.Rd
  bptr_table.Rd
  bptr_test.Rd
  bptr_test_23.Rd
  bptr_test_seq.Rd
  threshold_tidy.Rd
  panel_data.Rd                        ← dataset documentation (from data.R)
  coef.bptr.Rd
  fitted.bptr.Rd
  nobs.bptr.Rd
  predict.bptr.Rd
  plot.bptr.Rd
  print.bptr.Rd
  print.bptr_bootstrap.Rd
  print.bptr_test.Rd
  print.bptr_test23.Rd
  print.bptr_test_seq.Rd
  print.summary.bptr.Rd
  residuals.bptr.Rd
  summary.bptr.Rd
  tidy.bptr.Rd
  glance.bptr.Rd
  augment.bptr.Rd
  vcov.bptr.Rd
```

Verify with:

```r
list.files("man")
```

If `man/` is empty after `devtools::document()`, check that:
- All `@export` tags are present in the R source files
- The `NAMESPACE` file is being generated (should start with
  `# Generated by roxygen2`)
- `RoxygenNote:` in DESCRIPTION matches your installed roxygen2 version:
  ```r
  packageVersion("roxygen2")
  ```

---

## Quick test after correct installation

```r
library(bufferedThresholdPanel)

# 1. Package overview
help(package = "bufferedThresholdPanel")

# 2. Function help pages
?bptr
?bptr_test_seq
?panel_data

# 3. Vignette
browseVignettes("bufferedThresholdPanel")

# 4. Confirm the dataset loads
data(panel_data)
nrow(panel_data)   # should be 1380
```
