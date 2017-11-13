# This script enables you to lint your R scripts, to check on styling errors.
library(lintr)
library(config)
config <- config::get(file = "config.yml")

# Custom lint rules
linters <- with_defaults(
  camel_case_linter = NULL,
  line_length_linter(120),
  multiple_dots_linter = NULL,
  object_length_linter(40),
  spaces_left_parentheses_linter = NULL,
  trailing_whitespace_linter = NULL
)

lintr::clear_cache(file = NULL)
lintr::lint(filename = config$fileToLint, linters = linters)
