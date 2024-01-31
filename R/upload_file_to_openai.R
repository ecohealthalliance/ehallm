#' Upload a JSONL training file to the openai servers for use in fine-tuning a model
#'
#' @param filepath
#' @param url
#'
#' @return
#' @export
#'
#' @examples
upload_file_to_openai <- function(filepath,
                                  url = "https://api.openai.com/v1/files") {

  # https://platform.openai.com/docs/api-reference/files

  stopifnot(grepl("\\.jsonl", filepath))

  headers <- c(Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
               `Content-Type` = "multipart/form-data")

  body <- list()
  body[["purpose"]] <- "fine-tune"
  body[["file"]] <- httr::upload_file(filepath)

  response <- httr::POST(
    url = url,
    httr::add_headers(.headers = headers), # Reflects the -H option in POST
    body = body
  )

  openai:::verify_mime_type(response)
  parsed <- response %>% httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE)
  if (httr::http_error(response)) {
    paste0("OpenAI API request failed [", httr::status_code(response),
           "]:\n\n", parsed$error$message) %>% stop(call. = FALSE)
  }
  parsed

}
