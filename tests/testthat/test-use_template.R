test_that("all templates copy successfully", {
  templates <- list.dirs(
    system.file("templates", package = "bittoolbox"),
    full.name = FALSE,
    recursive = FALSE
  )
  for (template in templates) {
    template_state <- .check_template_state(template)

    expect_equal(
      template_state$copied_files,
      template_state$expected_files,
      info = paste("template:", template)
    )
  }
})

test_that("successfully copies predictiv_cint_cleaning template", {
  template <- "predictiv_cint_cleaning"
  template_state <- .check_template_state(template)

  expect_equal(template_state$copied_files, template_state$expected_files)
})

test_that("successfully copies apac_quarto template", {
  template <- "apac_quarto"
  template_state <- .check_template_state(template)

  expect_equal(template_state$copied_files, template_state$expected_files)
})

test_that("overwrites existing file when overwrite is TRUE", {
  tmp_target_path <- withr::local_tempdir()
  template <- "predictiv_cint_cleaning"
  suppressMessages(
    use_template(name = template, target_path = tmp_target_path)
  )

  expect_message(
    use_template(name = template, target_path = tmp_target_path, overwrite = TRUE),
    "replaced existing file"
  )
})

test_that("copying fails when file exists and overwrite is FALSE", {
  tmp_target_path <- withr::local_tempdir()
  template <- "predictiv_cint_cleaning"
  overwrite <- FALSE

  suppressMessages(
    use_template(name = template, target_path = tmp_target_path, overwrite = overwrite)
  )

  expect_error(
    use_template(name = template, target_path = tmp_target_path, overwrite = overwrite),
    class = "error_file_exists"
  )
})

test_that("copying fails with non-character template name", {
  expect_error(
    use_template(name = 42),
    class = "error_bad_argument"
  )
})

test_that("copying fails with empty template name", {
  expect_error(
    use_template(name = ""),
    class = "error_bad_argument"
  )
})

test_that("copying fails with whitespace only template name", {
  expect_error(
    use_template(name = " "),
    class = "error_bad_argument"
  )
})

test_that("copying fails with non-existing template name", {
  expect_error(
    use_template(name = "i_do_not_exist.R"),
    class = "error_missing_template"
  )
})

test_that("error when copying fails for another reason", {
  tmp_path <- withr::local_tempdir()
  local_mocked_bindings(file.copy = function(...) FALSE, .package = "base")

  expect_error(
    use_template(name = "predictiv_cint_cleaning", target_path = tmp_path),
    class = "error_copying_failed"
  )
})
