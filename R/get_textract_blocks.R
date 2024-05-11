#' Extract textract blocks as a tibble
#'
#' @param doc
#'
#' @return
#' @export
#'
#' @examples
get_textract_blocks <- function(document) {

  blocks <- document$Blocks |> map(~as_tibble(rbind(.x))) |> bind_rows()

  ## This is very opaque but it essentially de-lists list columns where each cell contains only one element or less
  blocks <- blocks |> mutate(across(where(~max(sapply(.x, length)) <= 1), ~map_vec(.x, ~if(length(.x) > 0) .x[[1]] else NA)))


  # Unnest Relationships column to make it easier to work with
  blocks <- blocks |> mutate(Relationships = map(Relationships, bind_rows),
                             Geometry = map(Geometry, bind_rows))

  blocks
}
