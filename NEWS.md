# NEWS for ALFAM2 R package

# ALFAM2 v4.2.13 (5 Dec 2025)
## Bug fixes
From v4.2 `alfam2()` could be run without TAN application rate, but this did not work when the `conf.int` argument was used. That problem was fixed.

## Documentation
Minor change to description in DESCRIPTION and correction of year for most recent paper (2025).

# ALFAM2 v4.2 (8 Nov 2024)

## Parameter correction
The latest default parameter set was revised slightly to reflect new results after making corrections to the parameter estimation procedure.
Resulting predictions will be slightly different than in v4.1.11 and earlier.
In some cases the change may be around 1% of applied TAN, but is typically smaller.

## New feature
The `alfam2()` function can now be run without a column for TAN applcation rate (`app.name` argument). If omitted (or if default argument name is not present) only relative emission will be returned with a warning.

## Documentation
Available default parameter sets are now documented in a separate help file.
View it with `?alfam2pars` or `?alfam2pars03` etc.

A new paper describing recent developments in the package has been published and is now listed in the help files and vignette.

# ALFAM2 v4.1.6 (31 July 2024)

## Other changes
`alfam2()` function now checks that all `pass.col` are in the `dat` data frame (see issue [#92](https://github.com/AU-BCE-EE/ALFAM2/issues/92)).

# ALFAM2 v4.1.5 (23 June 2024)

## Bug fixes
Fix a phantom incorporation effect on the r3 parameter that could show up with inconsistent user inputs (see issue [#87](https://github.com/AU-BCE-EE/ALFAM2/issues/87)).

## New content
A NEWS.md file (this file) has been added.

# ALFAM2 v4.1.3 (17 June 2024)

## Bug fixes
* Use of `conf.int` in the `alfam2()` function no longer returns incorrect message about user-defined parameters

## New features
* With `conf.int = 'all'` the `alfam2()` function will now return a parameter set index column in the output, which is convenient for further processing (e.g., for relative effects)

## Documentation
* The latest parameter set is now correctly identified as 3 in the `alfam2()` help (documentation) file, where additional details were added

## Other changes
* A problem with macOS checking of the package caused by deliberate errors in the vignette (for demonstration) was fixed using `purl=FALSE` (thanks to [Ben Bolker](https://stat.ethz.ch/pipermail/r-package-devel/2024q2/010888.html) and [Ivan Krylov](https://stat.ethz.ch/pipermail/r-package-devel/2024q2/010889.html))
* Some implausible wind speed values were corrected in the vignette

# ALFAM2 v4.0 (4 June 2024)
First version on CRAN.
For differences from earlier versions see the notes given with previous [releases](https://github.com/AU-BCE-EE/ALFAM2/releases).
