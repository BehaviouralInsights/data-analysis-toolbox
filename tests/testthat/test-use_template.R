test_that("sucessfully copies predictiv_cint_cleaning template", {
  tmp_path <- withr::local_tempdir()
  tmp_file <- "predictiv_cint_cleaning.R"

  suppressMessages(
    use_template(
      type = "predictiv_cint_cleaning",
      destination = tmp_path
    )
  )

  file_exists <- file.exists(
    file.path(tmp_path, tmp_file)
  )
  expect_true(file_exists)
})

test_that("overwrites existing file when overwrite is TRUE", {
  tmp_path <- withr::local_tempdir()
  tmp_file <- "predictiv_cint_cleaning.R"
  suppressMessages(
    use_template(destination = tmp_path, filename = tmp_file, overwrite = FALSE)
  )

  expect_message(
    use_template(destination = tmp_path, filename = tmp_file, overwrite = TRUE),
    "replaced existing file"
  )
})

test_that("copying fails when directory does not exist", {
  expect_error(
    use_template(destination = "./i_do_not_exist"),
    class = "error_missing_dir"
  )
})

test_that("copying fails when file exists and overwrite is FALSE", {
  tmp_path <- withr::local_tempdir()
  tmp_file <- "predictiv_cint_cleaning.R"
  overwrite <- FALSE

  suppressMessages(
    use_template(
      destination = tmp_path,
      filename = tmp_file,
      overwrite = overwrite
    )
  )

  expect_error(
    use_template(destination = tmp_path, filename = tmp_file, overwrite = overwrite),
    class = "error_file_exists"
  )
})

test_that("copying fails with non-character template name", {
  expect_error(
    use_template(type = 42),
    class = "error_bad_argument"
  )
})

test_that("copying fails with empty template name", {
  expect_error(
    use_template(type = ""),
    class = "error_bad_argument"
  )
})

test_that("copying fails with whitespace only template name", {
  expect_error(
    use_template(type = " "),
    class = "error_bad_argument"
  )
})

test_that("copying fails with non-existing template name", {
  expect_error(
    use_template(type = "i_do_not_exist.R"),
    class = "error_missing_template"
  )
})

test_that("error when copying fails for another reason", {
  tmp_path <- withr::local_tempdir()
  local_mocked_bindings(file.copy = function(...) FALSE, .package = "base")

  expect_error(
    use_template(destination = tmp_path),
    class = "error_copying_failed"
  )
})
