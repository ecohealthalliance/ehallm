#' Extract textract blocks as a tibble
#'
#' @param doc
#'
#' @return
#' @export
#'
#' @examples
get_textract_blocks <- function(doc) {

  blocks <- doc$Blocks |> map(~as_tibble(rbind(.x))) |> bind_rows()

  # Check if each list in a column has length 0 or 1
  zero_or_one_length_columns <- sapply(blocks, function(x) all(lengths(x) %in% c(0, 1)))

  # Unnest columns with lists of length 0 or 1, replacing length 0 with NA
  blocks <- blocks %>%
    mutate(across(where(~is.list(.)), ~if (all(lengths(.) %in% c(0, 1))) {
      ifelse(lengths(.) == 0, NA, unlist(.))
    } else .))

  # Unnest Relationships column to make it easier to work with
  blocks <- blocks |> mutate(Relationships = map(Relationships, bind_rows),
                          library(tidyverse)
library(targets)   Geometry = map(Geometry, bind_rows))

  |> mutate(Relationships = map(Relationships, ~.x |> bind_rows()))

  # Unnest Geometry column to make it easier to work with
  blocks <- blocks |> unnest(Geometry) |> mutate(Geometry = map(Geometry, ~.x |> bind_rows()))
}
