#' Retrive a list of all openai fine-tuning jobs or information about a specific job
#'
#' @param url
#' @param job_id
#'
#' @return
#' @export
#'
#' @examples
get_fine_tuning_job <- function(url = "https://api.openai.com/v1/fine_tuning/jobs", job_id = NA) {

  if(!is.na(job_id)) paste0(url, "/", job_id)

  headers <- c(Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
               `Content-Type` = "application/jsonl")

  response <- httr::GET(
    url = url,
    httr::add_headers(.headers = headers), # Reflects the -H option in POST
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
