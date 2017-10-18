# File to lint
fileToLint <- "map/map_renderer.R"
library(lintr)
# Custom lint rules
linters <- with_defaults(
  camel_case_linter = NULL,
  line_length_linter(120),
  multiple_dots_linter = NULL,
  object_length_linter(40),
  spaces_left_parentheses_linter = NULL
)


lintr::clear_cache(file = NULL)
lintr::lint(filename = fileToLint, linters = linters)
