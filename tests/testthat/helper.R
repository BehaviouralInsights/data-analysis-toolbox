# Test credentials and queries
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
  lake_data_path = "s3://test/",
  lake_metadata_schema = "lake_catalog"
)

test_query <- "SELECT * FROM lake_catalog.predictiv.results"

# Helper: generate a path to a temporary file with a fileending `ext`
write_temp_file <- function(ext) {
  tempfile(fileext = paste0(".", ext))
}
