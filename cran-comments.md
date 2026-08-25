## Submission

This is a new release, and the first submission of GRAFS to CRAN.

## Test environments

* Local: Windows 11 x64, R 4.5.0
* win-builder (R-devel), via `devtools::check_win_devel()`
* R-hub (via GitHub Actions): Ubuntu 24.04 (R-devel), macOS 15 arm64 (R-devel)

## R CMD check results

* Local (Windows): 0 errors | 0 warnings | 1 note
  - checking for future file timestamps ... NOTE
    "unable to verify current time" -- caused by no network access to an
    NTP time server from the local machine used for checking; not
    expected to occur on CRAN's own build infrastructure.
* win-builder (R-devel): 0 errors | 0 warnings | 1 note
  - checking CRAN incoming feasibility ... NOTE (new submission; a few
    possibly-misspelled words in DESCRIPTION are domain terminology and
    author surnames from the cited references)
* R-hub Ubuntu (R-devel): 0 errors | 0 warnings | 0 notes
* R-hub macOS arm64 (R-devel): 0 errors | 0 warnings | 0 notes

## Downstream dependencies

There are currently no downstream dependencies for this package.
