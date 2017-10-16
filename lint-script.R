# File to lint
fileToLint <- "src/smart_charging_vs_kwh.R"

# Custom lint rules
linters <- with_defaults(
  camel_case_linter = NULL,
  line_length_linter(120),
  multiple_dots_linter = NULL,
  object_length_linter(40)
)

library(lintr)
lintr::clear_cache(file = NULL)
lintr::lint(filename = fileToLint, linters = linters)