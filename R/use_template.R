#' Copy templates, bundled with the `bittoolbox` package
#'
#' Copies a template script, bundled with the `bittoolbox` package, to a
#' specific destination. The following templates are currently included:
#' - `predictiv_cint_cleaning`: cleaning routines for survey data, collected on
#'  Predictiv via Cint
#'
#' @param type Character. Name of the template that should be copied.Defaults to
#'  `predictiv_cint_cleaning`
#' @param destination Character. Path to the directory where the template should
#'  be copied. Defaults to the current working directory (`"."`).
#' @param filename Character. Name of the copied template file. Defaults to
#'  `"predictiv_cint_cleaning.R"`.
#' @param overwrite Logical. Whether to overwrite an existing file. Defaults to
#'  FALSE.
#'
#' @returns Invisibly returns the path to the copied file.
#'
#' @details
#' The function looks for a template file bundled with the package and copies
#' it to the specified location. If a file already exists at the destination
#' and `overwrite = FALSE`, the function aborts with an error message.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Copies the default template to the default location
#' use_template()
#'
#' # Copies `my_template` to `./my/destination/my_filename.R`, and overwrites
#' # existing files with the same name
#' use_template(
#'   "my_template",
#'   "./my/destination/",
#'   "my_filename.R",
#'   TRUE
#' )
#' }
use_template <- function(
    type = "predictiv_cint_cleaning",
    destination = ".",
    filename = "predictiv_cint_cleaning.R",
    overwrite = FALSE
) {
  if (!is.character(type) || length(type) != 1 || nchar(trimws(type)) == 0) {
    rlang::abort(
      "`type` must be a non-empty string.",
      class = "error_bad_argument"
    )
  }

  target_path <- file.path(destination, filename)
  file_exists <- file.exists(target_path)
  template_path <- system.file(
    paste0("templates/", type, ".R"),
    package = "bittoolbox"
  )

  if (!dir.exists(destination)) {
    rlang::abort(
      paste0("Destination directory does not exist: ", destination),
      class = "error_missing_dir"
    )
  }
  if (file_exists && !overwrite) {
    rlang::abort(
      paste0(
        "File already exists at destination: '",
        target_path,
        "'. Use `overwrite = TRUE` to replace."
      ),
      class = "error_file_exists"
    )
  }
  if (template_path == "") {
    rlang::abort(
      paste0(
        "Template does not exist in package: ",
        type,
        ". Please check template name and try again."
      ),
      class = "error_missing_template"
    )
  }

  success <- file.copy(template_path, target_path, overwrite = overwrite)
  if (!success) {
    msg <- paste0(
      "Failed to copy template to: ",
      target_path
    )
    rlang::abort(msg, class = "error_copying_failed")
  } else {
    msg <- paste0(
      "Template ",
      type,
      " successfully copied to ",
      target_path,
      if (file_exists && overwrite) " (replaced existing file)" else ""
    )
    message(msg)
  }

  invisible(target_path)
}
