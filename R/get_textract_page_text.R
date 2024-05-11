#' Extract all text LINES and WORDS, excluding tables, from a block
#'
#' @param page
#' @param blocks
#'
#' @return
#' @export
#'
#' @examples
get_page_text <- function(page, blocks) {

  # Get all WORD and LINE children. Order them by their Y axis first and X axis second.
  lines <- get_block_children(page, blocks) |> filter(BlockType == "LINE")
  page_text <- lines |> pull(Text) |> paste(collapse = "\n")

  page_text
}

get_tables <- function(blocks) {

  # Get all WORD and LINE children. Order them by their Y axis first and X axis second.
  tables <- blocks |> filter(BlockType == "TABLE")

  map_dfr(1:nrow(tables), ~get_block_table(tables[.x,], blocks))
}


get_block_children <- function(block, blocks) {

  children <- block |> unnest(Relationships)
  if(nrow(children) == 0) children <- tibble(Ids = NA)
  blocks |> filter(Id %in% children$Ids)

}

get_block_table <- function(block, blocks) {

  cells <- get_block_children(block, blocks) |>
    unnest(Relationships) |>
    left_join(blocks |>
                select(Text, Id), by = c("Ids" = "Id")) |>
    mutate(Text = coalesce(Text.x, Text.y))

  # 1. Go through a table's cells one-by-one
  table <- matrix(nrow = max(cells$RowIndex, na.rm=T), ncol = max(cells$ColumnIndex, na.rm=T))
  for (i in 1:nrow(cells)) table[cells[i,]$RowIndex, cells[i,]$ColumnIndex] <- paste(cells[i,]$Text, collapse = " ")

  tibble(table = list(table),
       table.name = cells |> filter(BlockType == "TABLE_TITLE") |> pull(Text) |> paste(collapse = " "),
       table.footer = cells |> filter(BlockType == "TABLE_FOOTER") |> pull(Text) |> paste(collapse = " "))
}




