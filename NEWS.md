# NEWS for ALFAM2 R package

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
