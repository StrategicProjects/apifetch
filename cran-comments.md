## R CMD check results

0 errors | 0 warnings | 1 note

* The one NOTE is the standard "New submission" note; this is a new package.

## Test environments

* local macOS, R 4.6.0
* GitHub Actions: macOS, Windows, and Ubuntu (R release)

## Notes for CRAN

* This package generalises the data-retrieval engine of the author's existing
  CRAN package 'BigDataPE'; 'BigDataPE' will, in a future update, depend on
  'apifetch'.
* Tokens are stored only in process environment variables (via `Sys.setenv()`),
  never written to disk and never using the system keychain.
* All examples that perform network requests are wrapped in `\dontrun{}`, and
  the vignette is not evaluated, so the check does not contact any external API.
