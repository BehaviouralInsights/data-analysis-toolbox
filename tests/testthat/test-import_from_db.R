test_that("returns tibble when call to DuckLake calls succeeds", {
  mockery::stub(load_from_ducklake, "DBI::dbConnect", list())
  mockery::stub(load_from_ducklake, "DBI::dbDisconnect", NULL)
  mockery::stub(load_from_ducklake, "DBI::dbExecute", 0L)
  mockery::stub(
    load_from_ducklake,
    "DBI::dbGetQuery",
    data.frame(id = 1:3, value = c(10, 20, 30))
  )

  result <- load_from_ducklake(test_query, test_credentials)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_named(result, c("id", "value"))
})

test_that("returns empty tibble when query yields no rows", {
  mockery::stub(load_from_ducklake, "DBI::dbConnect", list())
  mockery::stub(load_from_ducklake, "DBI::dbDisconnect", NULL)
  mockery::stub(load_from_ducklake, "DBI::dbExecute", 0L)
  mockery::stub(
    load_from_ducklake,
    "DBI::dbGetQuery",
    data.frame(id = integer(), value = numeric())
  )

  result <- load_from_ducklake(test_query, test_credentials)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_named(result, c("id", "value"))
})

test_that("disconnects even when query errors", {
  disconnected <- FALSE

  mockery::stub(load_from_ducklake, "DBI::dbConnect", list())
  mockery::stub(load_from_ducklake, "DBI::dbDisconnect", function(...) { disconnected <<- TRUE })
  mockery::stub(load_from_ducklake, "DBI::dbExecute", 0L)
  mockery::stub(load_from_ducklake, "DBI::dbGetQuery", function(...) stop("query failed"))

  expect_error(load_from_ducklake(test_query, test_credentials))
  expect_true(disconnected)
})

test_that("fails on invalid db", {
  db <- "unsupported_db"
  expect_error(
    import_from_db(db, test_query, test_credentials),
    "Invalid value for `db`. Check function documentation for supported database types"
  )
})

test_that("CREATE SECRET SQL contains S3 credentials", {
  executed_sql <- character()

  mockery::stub(load_from_ducklake, "DBI::dbConnect", list())
  mockery::stub(load_from_ducklake, "DBI::dbDisconnect", NULL)
  mockery::stub(load_from_ducklake, "DBI::dbExecute", function(conn, statement, ...) {
    executed_sql <<- c(executed_sql, statement)
  })
  mockery::stub(load_from_ducklake, "DBI::dbGetQuery", data.frame())

  load_from_ducklake(test_query, test_credentials)

  secret_sql <- executed_sql[grepl("CREATE SECRET", executed_sql)]
  expect_match(secret_sql, test_credentials$s3_key_id)
  expect_match(secret_sql, test_credentials$s3_secret)
  expect_match(secret_sql, test_credentials$s3_endpoint)
  expect_match(secret_sql, test_credentials$s3_region)
})

test_that("fails on empty query string", {
  test_query <- " "
  expect_error(
    import_from_db("ducklake", test_query, test_credentials),
    "`query` can't be an empty"
  )
})

test_that("fails on non-string query", {
  test_query <- 1
  expect_error(
    import_from_db("ducklake", test_query, test_credentials),
    "`query` must be a string."
  )
})

test_that("fails on missing ducklake credential", {
  test_credentials <- list(
    s3_key_id = "test-key",
    s3_secret = "test-secret",
    s3_endpoint = "localhost:4566",
    s3_region = "us-east-1",
    pg_user = "postgres",
    pg_pw = "postgres",
    pg_host = "localhost",
    pg_port = "5434",
    pg_schema = "ducklake",
    lake_data_path = "s3://test/"
  )
  expect_error(
    import_from_db("ducklake", test_query, test_credentials),
    "Missing credentials fields: lake_metadata_schema"
  )
})

test_that("fails on missing ducklake credentials", {
  test_credentials <- list(
    s3_key_id = "test-key",
    s3_secret = "test-secret",
    s3_endpoint = "localhost:4566",
    s3_region = "us-east-1",
    pg_user = "postgres",
    pg_pw = "postgres",
    pg_host = "localhost",
    pg_port = "5434"
  )
  expect_error(
    import_from_db("ducklake", test_query, test_credentials),
    "Missing credentials fields: pg_schema, lake_metadata_schema, lake_data_path"
  )
})

test_that("returns empty vector when all fields have values", {
  result <- check_blank_list_fields(
    list(a = "value", b = "other")
  )
  expect_equal(result, character(0))
})

test_that("returns field name when value is empty string", {
  result <- check_blank_list_fields(
    list(a = "", b = "ok")
  )
  expect_equal(result, "a")
})

test_that("returns field name when value is NULL", {
  result <- check_blank_list_fields(
    list(a = NULL, b = "ok")
  )
  expect_equal(result, "a")
})

test_that("returns field name when value is whitespace only", {
  result <- check_blank_list_fields(
    list(a = " ", b = "ok")
  )
  expect_equal(result, "a")
})

test_that("returns multiple field names when several are blank", {
  result <- check_blank_list_fields(
    list(a = NULL, b = " ", c = "ok")
  )
  expect_equal(result, c("a", "b"))
})
