## Update

This is an update of 'apifetch' (0.1.0 -> 0.2.0). It fixes several bugs
(an infinite loop in `af_fetch_all()` with non-paginated APIs, malformed URLs
when the endpoint already has a query string, and edge cases in argument
validation) and adds small improvements; see NEWS.md. It also corrects the
spelling of two co-authors' surnames ("Amorim", "Wasiliew").

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local macOS, R 4.6.0
* win-builder, R-devel
* GitHub Actions: macOS, Windows, and Ubuntu (R release)

## Reverse dependencies

There are currently no reverse dependencies.

## Notes for CRAN

* Tokens are stored only in process environment variables (via `Sys.setenv()`),
  never written to disk and never using the system keychain.
* All examples that perform network requests are wrapped in `\dontrun{}`, the
  vignette is not evaluated, and the tests mock HTTP responses, so the check
  does not contact any external API.
