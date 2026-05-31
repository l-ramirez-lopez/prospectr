## version 0.2.9
## Test environments

- ubuntu-latest, R release (GitHub Actions) — OK
- ubuntu-latest, R devel (GitHub Actions) — OK
- windows-latest, R release (GitHub Actions) — OK
- macos-latest, R release (GitHub Actions) — OK
- win-builder, R release — OK
- win-builder, R devel — OK

## R CMD check results

No ERRORs, no WARNINGs.

1 NOTE (pre-existing, not introduced in this release):

* checking installed package size ... NOTE
  installed size is ~6.5Mb
  sub-directories of 1Mb or more:
    data   1.9Mb
    libs   3.1-4.0Mb

This NOTE has been present since version 0.2.1 and is due to compiled
code (Rcpp) and the bundled example datasets. It is not feasible to
reduce further without removing functionality.

## Reverse dependencies

Reverse dependencies have been checked and are unaffected by this release.
