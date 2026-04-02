#' Check whether input is a non-empty string
#'
#' @param x Value to check
#' @param arg Character. Name of the argument to use in error message.
#'  Defaults to the name of `x` as supplied by the caller.
#'
#' @returns Invisibly returns `x` if the check passes. Aborts with class
#'  `error_bad_argument` otherwise.
#'
#' @noRd
.check_string <- function(x, arg = rlang::caller_arg(x)) {
  if (!is.character(x) || length(x) != 1 || nchar(trimws(x)) == 0) {
    rlang::abort(
      paste0("`", arg, "` must be a non-empty string."),
      class = "error_bad_argument"
    )
  }

  invisible(x)
}

#' Resolve template path
#'
#' @param name Character. Template name.
#'
#' @returns Character. Path to template.
#'
#' @noRd
.resolve_template <- function(name) {
  .check_string(name)

  path <- system.file(
    file.path("templates", name),
    package = "bittoolbox"
  )
  if (path == "") {
    rlang::abort(
      paste0("Template does not exist in package: ", name, "."),
      class = "error_missing_template"
    )
  }

  path
}

#' Copy templates, bundled with the `bittoolbox` package
#'
#' Copies a template, bundled with the `bittoolbox` package, to the current
#' working directory. The following templates are currently included:
#' - `predictiv_cint_cleaning`: cleaning routines for survey data, collected on
#'  Predictiv via Cint
#' - `apac_quarto`: BIT APAC's Quarto template for "producing technical
#'  documents, formatted to resemble other BIT templates for Google docs, and
#'  advice in the BIT brand bible on Norm."
#'
#' @param name Character. Name of the template that should be copied.
#' @param target_path Character. Target path where the template will be copied to.
#' @param overwrite Logical. Whether to overwrite an existing file. Defaults to
#'  FALSE.
#'
#' @returns Invisibly returns the path to the target directory.
#'
#' @details
#' The function looks for a template file bundled with the package and copies
#' it to the specified location. If a file already exists at the target
#' and `overwrite = FALSE`, the function aborts with an error message.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' use_template(
#'   "my_template",
#'   overwrite = TRUE
#' )
#' }
use_template <- function(
    name,
    target_path = ".",
    overwrite = FALSE
) {
  .check_string(target_path)

  template_path <- .resolve_template(name)
  template_files <- list.files(template_path, recursive = TRUE)
  target_basepath <- file.path(target_path, name)
  target_filepaths <- file.path(target_basepath, template_files)

  # Check whether template files exist at target
  existing <- target_filepaths[file.exists(target_filepaths)]
  if (length(existing) > 0 && !overwrite) {
    rlang::abort(
      paste0(
        "Files already exist at target:\n",
        paste0("- ", existing, collapse = "\n"),
        "\nUse `overwrite = TRUE` to overwrite existing files."
      ),
      class = "error_file_exists"
    )
  }

  # Create missing folders in target
  target_dirs <- unique(dirname(target_filepaths))
  for (d in target_dirs) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }

  # Copy files to target
  sources <- file.path(template_path, template_files)
  success <- file.copy(sources, target_filepaths, overwrite = overwrite)
  if (any(!success)) {
    rlang::abort(
      paste0("Failed to copy:\n", paste0("- ", template_files[!success], collapse = "\n")),
      class = "error_copying_failed")
  }

  n <- length(template_files)
  suffix <- if (n > 1) "s" else ""
  message(
    paste0(
      "Template `", name, "`: ", n, " file", suffix, " copied to ",
      "'", target_basepath, "'",
      if (length(existing) > 0) " (replaced existing files)"
    )
  )

  invisible(target_basepath)
}
