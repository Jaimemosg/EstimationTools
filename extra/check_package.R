tools::compactPDF(
  paths = "./vignettes/maxlogL.pdf",
  gs_quality = "ebook"
)

rhub::check_for_cran(
  env_vars = c(
    `_R_CHECK_CRAN_INCOMING_` = "true",
    `_R_CHECK_CRAN_INCOMING_REMOTE_` = "true",
    `_R_CHECK_VC_DIRS_` = "true",
    `_R_CHECK_TIMINGS_` = 10,
    `_R_CHECK_INSTALL_DEPENDS_` = "true",
    `_R_CHECK_SUGGESTS_ONLY_` = "true",
    `_R_CHECK_NO_RECOMMENDED_` = "true",
    `_R_CHECK_EXECUTABLES_EXCLUSIONS_` = "true",
    `_R_CHECK_DOC_SIZES2_` = "true",
    `_R_CHECK_CODE_ASSIGN_TO_GLOBALENV_` = "true",
    `_R_CHECK_CODE_ATTACH_` = "true",
    `_R_CHECK_CODE_DATA_INTO_GLOBALENV_` = "true",
    `_R_CHECK_CODE_USAGE_VIA_NAMESPACES_` = "true",
    `_R_CHECK_DOT_FIRSTLIB_` = "true",
    `_R_CHECK_DEPRECATED_DEFUNCT_` = "true",
    `_R_CHECK_REPLACING_IMPORTS_` = "true",
    `_R_CHECK_SCREEN_DEVICE_` = "stop",
    `_R_CHECK_TOPLEVEL_FILES_` = "true",
    `_R_CHECK_S3_METHODS_NOT_REGISTERED_` = "true",
    `_R_CHECK_OVERWRITE_REGISTERED_S3_METHODS_` = "true",
    `_R_CHECK_PRAGMAS_` = "true",
    `_R_CHECK_COMPILATION_FLAGS_` = "true",
    `_R_CHECK_R_DEPENDS_` = "warn",
    `_R_CHECK_SERIALIZATION_` = "true",
    `_R_CHECK_R_ON_PATH_` = "true",
    `_R_CHECK_PACKAGES_USED_IN_TESTS_USE_SUBDIRS_` = "true",
    `_R_CHECK_SHLIB_OPENMP_FLAGS_` = "true",
    `_R_CHECK_CONNECTIONS_LEFT_OPEN_` = "true",
    `_R_CHECK_FUTURE_FILE_TIMESTAMPS_` = "true",
    `_R_CHECK_LENGTH_1_CONDITION_` = "package:_R_CHECK_PACKAGE_NAME_,abort,verbose",
    `_R_CHECK_LENGTH_1_LOGIC2_` = "package:_R_CHECK_PACKAGE_NAME_,abort,verbose",
    `_R_CHECK_AUTOCONF_` = "true",
    `_R_CHECK_DATALIST_` = "true",
    `_R_CHECK_THINGS_IN_CHECK_DIR_` = "true",
    `_R_CHECK_THINGS_IN_TEMP_DIR_` = "true",
    `_R_CHECK_BASHISMS_` = "true",
    `_R_CLASS_MATRIX_ARRARY_` = "true",
    `_R_CHECK_ORPHANED_` = "true",
    `_R_CHECK_BOGUS_RETURN_` = "true",
    `_R_CHECK_MATRIX_DATA_` = "true",
    `_R_CHECK_CODE_CLASS_IS_STRING_` = "true",
    `_R_CHECK_RD_VALIDATE_RD2HTML_` = "true",
    `_R_CHECK_RD_MATH_RENDERING_` = "true",
    `_R_CHECK_NEWS_IN_PLAIN_TEXT_` = "true",
    `_R_CHECK_BROWSER_NONINTERACTIVE_` = "true"
  )
)

devtools::check(
  manual = TRUE,
  remote = TRUE,
  incoming = TRUE,
  args = c("--as-cran"),
  build_args = c("--resave-data=best", "--compact-vignettes=both")
)
