
test_that("spelling", {
  skip_on_cran()
  skip_if_not_installed("spelling")

  # Skip when running coverage (set in CI step)
  if (identical(Sys.getenv("COVR"), "true")) {
    skip("Skip spelling under coverage")
  }

  spelling::spell_check_test(
    vignettes = TRUE,
    error = FALSE,
    skip_on_cran = TRUE
  )
})