#' Upload a training file and submit a request to train a custom openai model on the provided data
#'
#' @param training_data_file
#' @param model
#' @param url
#'
#' @return
#' @export
#'
#' @examples
create_fine_tuning_job <- function(training_data_file="data/test.jsonl",
                                   model="gpt-3.5-turbo",
                                   url="https://api.openai.com/v1/fine_tuning/jobs") {
  # https://platform.openai.com/docs/api-reference/fine-tuning/create

  # Upload training data file
  upload_response <- upload_file_to_openai(training_data_file)

  # Add some error handling to this
  file_id <- upload_response$id

  headers <- c(Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
               `Content-Type` = "application/jsonl")

  body <- list()
  body[["training_file"]] <- file_id
  body[["model"]] <- model

  response <- httr::POST(
    url = url,
    httr::add_headers(.headers = headers), # Reflects the -H option in POST
    body = body,
    encode = "jsonl"
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
