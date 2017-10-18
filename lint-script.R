# This script enables you to lint your R scripts, to check on styling errors.
library(lintr)

# File to lint
fileToLint <- "src/.."

# Custom lint rules
linters <- with_defaults(
  camel_case_linter = NULL,
  line_length_linter(120),
  multiple_dots_linter = NULL,
  object_length_linter(40)
)

lintr::clear_cache(file = NULL)
lintr::lint(filename = fileToLint, linters = linters)
