test_that("reads a CSV file into a tibble", {
  path <- write_temp_file("csv")
  demo_data <- bittoolbox::ai_can_take_your_job_not_mine
  readr::write_csv(demo_data, path)

  result <- import_from_file(path)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), nrow(demo_data))
  expect_equal(colnames(result), colnames(demo_data))
})

test_that("aborts on unsupported file extensions", {
  path <- write_temp_file("parquet")
  writeLines('{"a":1}', path)

  expect_error(import_from_file(path), "Unsupported file type: '.parquet'. Must be one of: .csv")
})

test_that("aborts on non-string input", {
  path <- 1
  expect_error(import_from_file(path), "`path` must be a string.")
})

test_that("aborts on empty input", {
  path <- " "
  expect_error(import_from_file(path), "`path` can't be an empty string")
})

test_that("aborts on non-existing file", {
  path <- "./i_dont_exist.csv"
  expect_error(import_from_file(path), paste0("File not found: ", path, ". Please check `path`."))
})
