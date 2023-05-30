## Comments
- Re-submit (SORRY!) with fixed URLS
- Changed API ERRORS to messages and return NA or NULL.  Would bomb out runs when this would happen only occasionally. Also this should meet CRAN policy on failing gracefully.
- Switched long lat check to st::sf_is_longlat
- Fixed EPQS API URL that was poitned out by a user and emails from Uwe Ligges and Brian Ripley on 2023-05-26.

## Test Environments
- Github Actions, Ubuntu 20.04.6 LTS, R version 4.3.0
- Github Actions, Ubuntu 20.04.6 LTS, R version 4.2.3
- Github Actions, Ubuntu 22.04.2 LTS, R development
- Github Actions, Microsoft Windows Server 2022, R Version 4.3.0
- Github Actions, Microsoft Windows Server 2019, R Version 4.2.3
- Github Actions, Mac OS 12.6.5, R Version 4.3.0
- Local, Windows 10 x64 (build 19042), R version 4.2.2

## R CMD check results
- No ERRORS or WARNINGS

## revdepcheck results

We checked 9 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 1 packages

Issues with CRAN packages are summarised below.

### Failed to check

* shoredate (NA)
