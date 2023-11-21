# Note: the required option is very buggy. Chat will call the function before finding the parameter in the text and try to pass in
# null values which invalidates the schema.


#' Construct a function call for to enforce a JSON schema formatted reply from openAI's chatGPT
#'
#' @return An appropriately formatted function call
#' @export
#'
#' @examples
get_openai_function_call <- function(parameters,
                                     function_name = "exract_outbreak_data",
                                     function_description = "Extract outbreak details from provided abstract",
                                     ) {

  function_call <-
    list(
      list("name" = function_name,
           "description" = function_description,
           "parameters" =
            list(
                "type" = "object",
                "properties" =
                 list(
                   "outbreak_detected" =
                      list("type" = "string",
                           "enum" = c("TRUE", "FALSE"),
                           "description" = "Whether any outbreak or disease events were detected in the abstract"),
                   "further_outbreak_details" =
                     list("type" = "string",
                          "enum" = c("TRUE", "FALSE"),
                          "description" = "Whether further outbreak or disease event details beyond what is available in the abstract are likely to be found elsewhere in the article."),
                   "events" = list("type" = "object",
                                   "properties" =
                                     list("disease" =
                                            list("type" = c("string", "null"),
                                                 "description" = "The outbreak event disease name"),
                                          "country" =
                                            list("type" = c("string", "null"),
                                                 "description" = "The country where the disease event occurred"),
                                          "year" =
                                            list("type" = c("string", "null"),
                                                 "description" = "The years the outbreak event occurred"),
                                          "event_type" =
                                            list("type" = "string",
                                                 "enum" = c("human", "animal", "both"),
                                                 "description" = "Whether the event occured in human or animal populations or both"),
                                          "species" =
                                            list("type" = c("string", "null"),
                                                 "description" = "The species or common names of the animals effected by the outbreak event"
                                                 )
                                          )
                                   )
                   )
                )
           )
      )

  function_call

  function_call_2 <- get_function_call_parameter


}

get_function_call_parameter <- function(parameter_name,
                                        description,
                                        type = c("string", "null"),
                                        enum = NULL) {

  parameter <- purrr::compact(list("type" = type,
                                   "enum" = enum,
                                   "description" = description))

  return(list(parameter) |> setNames(parameter_name))

}

get_test_param_df <- function() {

  data.frame(parameter_name = c("p1", "p2", "p3", "p4"),
         description = c("d1", "d2", "d3", "d4"),
         type = list(list("t1.1", "t1.2"), list("t2.1", "t2.2"), list("t3.1", "t3.2"), list("t4.1", "t4.2")),
         enum = c(NULL, c("e2.1", "e2.2"), NULL, NULL))


}


openai_chat <- function(specific_context,
                        common_context,
                        hint = NA,
                        model = 'gpt-3.5-turbo',
                        system_content = "You act as a function to extract outbreak infromation from a provided abstract.") {

  messages <-
    list(
        list(
            "role" = "system",
            "content" = system_content
        ),
        list(
            "role" = "user",
            "content" = common_context
        ),
        list(
          "role" = "user",
          "content" = specific_context
        )
    )

  if(!is.na(hint)) {
    messages = append(messages, list(list("role" = "user", "content" = paste("This may relate to", hint))))
  }

  functions = get_openai_function_call()

  outbreak_details <- openai::create_chat_completion(
    model = model,
    messages = messages,
    functions = functions
  ) |> bind_cols()

  outbreak_details
}


format_openai_results <- function(model_response) {

  format_JSON <- function(json) {
    tryCatch({
      json |> jsonlite::fromJSON() |> map(~.x |> compact())
    }, error=function(cond) {
      return(NA)
      }
    )
  }

  format_tb <- function(arguments) {
    if(all(is.na(arguments))) return(tibble(outbreak_detected = NA, JSON_parsing_error = T))
    if(!"events" %in% names(arguments)) {
      if(!"event" %in% names(arguments)) { # Darned chat!
        arguments$events <- NA
      } else {
        arguments$events <- arguments$event
        arguments <- within(arguments, rm(event))
      }
    }
    arguments$events <- list(as_tibble(arguments$events) |> mutate(across(everything(), as.character)))
    arguments |> as_tibble()
  }

  model_response |> mutate(response = map_dfr(message.function_call.arguments, ~format_JSON(.x) |> format_tb()))
  }
