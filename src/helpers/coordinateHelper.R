# Format a coordinate with dots as delimiters
formatCoordinate <- function(coordinate, type) {
  index = NA
  if (type == "longitude") {
    index = 2
  }
  if (type == "latitude") {
    index = 3
  }
  if (is.na(index)) {
    warning("Wrong type given. Choose between 'longitude' or 'latitude'.")
  }
  insert <- "."[order(index)]
  index <- sort(index)
  paste(interleave(split_str_by_index(coordinate, index), "."), collapse = "")
}

split_str_by_index <- function(target, index) {
  index <- sort(index)
  substr(rep(target, length(index) + 1),
         start = c(1, index),
         stop = c(index - 1, nchar(target)))
}

interleave <- function(v1, v2) {
  ord1 <- 2 * (1:length(v1)) - 1
  ord2 <- 2 * (1:length(v2))
  c(v1, v2)[order(c(ord1, ord2))]
}