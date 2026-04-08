# TODO replace with an actual, representative data set for more robust testing
demo_data <- tibble::tibble(
  id = 1:5,
  name = c("Alice", "Bob", "Carol", "Dave", "Eve"),
  score = 91:95,
  active = c(TRUE, FALSE, TRUE, FALSE, TRUE)
)

usethis::use_data(demo_data, overwrite = TRUE)
