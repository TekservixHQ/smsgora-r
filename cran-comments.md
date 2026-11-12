## Submission

This is a new release (first submission).

## Test environments

* GitHub Actions: ubuntu-latest (R devel, release, oldrel-1), windows-latest (release), macos-latest (release)

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission.

## Notes for reviewers

* The package is a client for a commercial web API. Every function except the
  client constructor and the webhook verification helper calls the API and
  needs a valid API key, so those examples are wrapped in \dontrun{}. The
  webhook verification and client examples run.
* Tests do not use the network on CRAN. One test calls the live API with an
  invalid key and is skipped unless an environment variable is set.
