#' Check for blank or null field values in a list
#'
#' @param list A named list
#'
#' @returns Character vector of field names that have null/blank values
#' @noRd
check_blank_list_fields <- function(list) {
  fields <- names(list)
  .is_empty <- function(x) {
    is.null(x) || (is.character(x) && nchar(trimws(x)) == 0)
  }

  fields[
    vapply(fields, function(f) .is_empty(list[[f]]), logical(1))
  ]
}

#' Query data from a DuckLake table
#'
#' @param query String with SQL query
#' @param credentials Named list with database credentials (see `import_from_db`)
#'
#' @returns tibble with query result
#' @noRd
load_from_ducklake <- function(query, credentials) {
  required_fields <- c(
    "s3_key_id", "s3_secret", "s3_endpoint", "s3_region",
    "pg_user", "pg_pw", "pg_host", "pg_port", "pg_schema",
    "lake_metadata_schema", "lake_data_path"
  )
  actual_fields <- names(credentials)
  missing_fields <- setdiff(required_fields, actual_fields)
  if (length(missing_fields) > 0) {
    rlang::abort(
      paste("Missing credentials fields:", paste(missing_fields, collapse = ", ")),
      class = "error_missing_credentials"
    )
  }

  s3_key_id <- credentials$s3_key_id
  s3_secret <- credentials$s3_secret
  s3_endpoint <- credentials$s3_endpoint
  s3_region <- credentials$s3_region
  pg_user <- credentials$pg_user
  pg_pw <- credentials$pg_pw
  pg_host <- credentials$pg_host
  pg_port <- as.integer(credentials$pg_port)
  pg_schema <- credentials$pg_schema
  lake_metadata_schema <- credentials$lake_metadata_schema
  lake_data_path <- credentials$lake_data_path

  # Spin up a blank, in-memory DuckDB instance, and inject S3 credentials
  conn <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  DBI::dbExecute(conn, "INSTALL ducklake")
  DBI::dbExecute(conn, "INSTALL httpfs")
  DBI::dbExecute(conn, "LOAD ducklake")
  DBI::dbExecute(conn, "LOAD httpfs")

  DBI::dbExecute(
    conn,
    as.character(
      glue::glue(
        "CREATE SECRET ducklake (
            TYPE S3,
            KEY_ID '{s3_key_id}',
            SECRET '{s3_secret}',
            ENDPOINT '{s3_endpoint}',
            URL_STYLE 'path',
            USE_SSL false,
            REGION '{s3_region}'
          )"
      )
    )
  )

  # Connect to DuckLake metadata catalog
  DBI::dbExecute(
    conn,
    as.character(
      glue::glue(
        "ATTACH 'ducklake:postgres:postgresql://{pg_user}:{pg_pw}@{pg_host}:{pg_port}/{pg_schema}'
         AS {lake_metadata_schema}
         (DATA_PATH '{lake_data_path}', METADATA_SCHEMA '{lake_metadata_schema}')"
      )
    )
  )

  # Fetch data
  result <- DBI::dbGetQuery(conn, query)
  tibble::as_tibble(result)
}

#' Import data from a local CSV file
#'
#' @param path A string with the local path to the data file. Must include file ending.
#'
#' @returns tibble with the imported data
#' @export
#'
#' @examples
#' \dontrun{
#' path <- "./this_is/the_relative_path/to_my/data.csv"
#' data <- import_from_file(here::here(path))
#' }
import_from_file <- function(path) {
  ext_supported = c("csv")
  ext_actual <- tolower(tools::file_ext(path))

  if (!is.character(path)) {
    rlang::abort("`path` must be a string.", class = "error_bad_argument")
  }
  if (nchar(trimws(path)) == 0) {
    rlang::abort("`path` can't be an empty string", class = "error_bad_argument")
  }
  if (!file.exists(path)) {
    rlang::abort(paste0("File not found: ", path, ". Please check `path`."), class = "error_file_not_found")
  }
  if (!ext_actual %in% ext_supported) {
    rlang::abort(
      paste0(
        "Unsupported file type: '.", ext_actual, "'. Must be one of: ",
        paste0(".", ext_supported, collapse = ", ")
      ),
      class = "error_unsupported_type"
    )
  }

  switch(
    ext_actual,
    csv = readr::read_csv(path, show_col_types = FALSE),
  )
}


#' Import data from a database
#'
#' Currently supported values for `db`: `"ducklake"`.
#' If you don't know what to enter as database credentials, please reach out to
#' the Predictiv team, so they can assign you an account.
#'
#' @param db A string with the database from which to fetch results
#' @param query String with SQL query
#' @param credentials List with database credentials. See the **Credentials**
#' section for required fields per database.
#'
#' @section Credentials:
#'
#' **`"ducklake"`**
#'
#' | Field | Description |
#' | --- | --- |
#' | `s3_key_id` | S3 access key ID |
#' | `s3_secret` | S3 secret access key |
#' | `s3_endpoint` | S3 endpoint |
#' | `s3_region` | S3 region|
#' | `pg_user` | Postgres username |
#' | `pg_pw` | Postgres password |
#' | `pg_host` | Postgres host |
#' | `pg_port` | Postgres port|
#' | `pg_schema` | Postgres schema containing DuckLake metadata |
#' | `lake_data_path` | S3 path to Parquet data files|
#' | `lake_metadata_schema` | DuckLake catalog name used in queries |
#'
#' @returns tibble with requested data
#' @export
#'
#' @examples
#' \dontrun{
#' db <- "ducklake"
#' query <- "SELECT * FROM lake_catalog.predictiv.results"
#' credentials <- list(
#'   s3_key_id= "test",
#'   s3_secret = "test",
#'   s3_endpoint = "localhost:4566",
#'   s3_region = "us-east-1",
#'   pg_user = "postgres",
#'   pg_pw = "postgres",
#'   pg_host = "localhost",
#'   pg_port = "5434",
#'   pg_schema = "ducklake",
#'   lake_data_path = "s3://test/",
#'   lake_metadata_schema = "lake_catalog"
#' )
#' results <- import_from_db(db, query, credentials)
#' }
import_from_db <- function(db = "ducklake", query, credentials) {
  blank_credential_fields <- check_blank_list_fields(credentials)
  if (!is.character(query)) {
    rlang::abort("`query` must be a string.", class = "error_bad_argument")
  }
  if (nchar(query) == 0 || query == " ") {
    rlang::abort("`query` can't be an empty", class = "error_bad_argument")
  }
  if (length(blank_credential_fields) > 0) {
    rlang::abort(
      paste("Blank credential fields:", paste(blank_credential_fields, collapse = ", ")),
      class = "error_blank_credentials"
    )
  }

  msg = "Invalid value for `db`. Check function documentation for supported database types"
  switch(
    db,
    ducklake = load_from_ducklake(query, credentials),
    rlang::abort(msg, class = "error_bad_argument")
  )
}
