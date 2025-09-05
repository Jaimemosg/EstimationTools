# tests/spelling.R
if (Sys.getenv("R_COVR") == "true") {
  cat("Skipping spelling::spell_check_test() under covr.\n")
} else if (requireNamespace("spelling", quietly = TRUE)) {
  spelling::spell_check_test(vignettes = TRUE, error = FALSE, skip_on_cran = TRUE)
}
