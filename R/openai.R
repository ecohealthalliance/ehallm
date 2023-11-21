# Note: this requires using a fork of the openai package which can be installed as following
# if (!require(remotes))
#   install.packages("remotes")
# remotes::install_github("nhardtskim/openai")

#'  context aware openai function call.
#'
#' @param country Passed to get_function_call_params if parameter descriptions need to include context
#' @param disease Passed to get_function_call_params if parameter descriptions need to include context
#' @param function_name The name of the imaginary function openAI will be formatting it's response for. Should be descriptive of the desired task it will perform.
#' @param function_description # A description of the what the function would accomplish
#'
#' @return Returns a list containing a formatted function call as an R list, as a compact JSON, a pretty JSON, and a tibble of the parameters used in constructing the call.
#' @export
#'
#' @examples
#' function_call <- get_openai_function_call()
get_openai_function_call <- function(country = NA,
                                     disease = NA,
                                     function_name = "exract_outbreak_data",
                                     function_description = "Extract outbreak details from provided abstract") {

  function_call_df <- get_function_call_params(country, disease)

  function_call <- list(
    list("name" = function_name,
         "description" = function_description,
         "parameters" =
           list(
             "type" = "object",
             "properties" = format_function_call_df(function_call_df)$parameter))) # Formatting is necessary

  list(function_call = function_call,
       JSON_compact = jsonlite::toJSON(function_call),
       JSON_pretty = jsonlite::toJSON(function_call, pretty = T),
       parameters = function_call_df)
}

#' Construct a tibble of parameters that openAI will return and specify constraints such as type and enumeration
#'
#' @param country Country context can be added to parameter descriptions by modifying the tribble to include glue or paste statements
#' @param disease Disease context can be added to parameter descriptions by modifying the tribble to include glue or paste statements
#'
#' @return A tibble outlining the parameters that openAI should use in it's reply. Will be further processed by format_function_call_df
#' @export
#'
#' @examples
#' function_call_params <- get_function_call_params()
get_function_call_params <- function(country = NA,
                                     disease = NA) {

  # Note: the required option is very buggy. I suggest you don't use it.
  # Chat will call the function before finding the parameter in the text and try to pass in
  # null values which invalidates the schema.

  # Note: Chat will sometimes return a logical FALSE and sometimes a character "FALSE". Use type = "string" and enum = c("TRUE", "FALSE") to enforce consistent return type

  # Note: Using type = c("string", "null") and enum = c("option1", "option2", "option3") could also return "null" which ISN'T in the enumeration list.

  # Modify any of these items with paste() or glue() to give it's description context.
  function_call_df <- tribble(~parameter_name, ~description, ~type, ~enum, ~group, ~group_description,
          "outbreak_detected", "Whether any outbreak or disease events were detected in the abstract", "string", c("TRUE", "FALSE"), NA, NA,
          "further_outbreak_details", "Whether further outbreak or disease event details beyond what is available in the abstract are likely to be found elsewhere in the article.", "string", c("TRUE", "FALSE"), NA, NA,
          "disease", "The outbreak event disease name", c("string", "null"), NA, "events", "outbreak event details if an event is detected",
          "country", "The country where the disease event occurred", c("string", "null"), NA, "events", "outbreak event details if an event is detected",
          "year", "The years the outbreak event occurred", c("string", "null"), NA, "events", "outbreak event details if an event is detected",
          "event_type", "Whether the event occured in human or animal populations or both", "string", c("human", "animal", "both"), "events", "outbreak event details if an event is detected",
          "species", "The species or common names of the animals effected by the outbreak event", c("string", "null"), NA, "events", "outbreak event details if an event is detected")

  function_call_df
}

#' Format the function call dataframe. Nest parameters based on the group column and format the list structure for conversion to JSON
#'
#' @param function_call_df
#'
#' @return A formatted tibble
#' @export
#'
#' @examples
#' function_call_df <- get_function_call_params() |> format_function_call_df()
format_function_call_df <- function(function_call_df) {

  function_call_df <- function_call_df |>
  drop_na(parameter_name) |>
  group_by(group) |>
  group_split() |>
  map_dfr(function(grp) {
    grp <- grp |> rowwise() |> mutate(parameter = get_function_call_parameter(parameter_name, description, type, enum, group))
    if(!is.na(grp$group[[1]])) {
      grp <- tibble(parameter_name = NA,
                    description = NA,
                    type = NA,
                    enum = NA,
                    group = grp$group[[1]],
                    group_description = grp$group_description[[1]],
                    parameter = list(list(
                      "type" = "object",
                      "description" = grp$group_description[[1]],
                      "properties" = grp$parameter |> setNames(grp$parameter_name)
                      )) |> setNames(grp$group[[1]])
      )
    }
    grp
  })

  function_call_df
}

#' Arrange parameter details in JSON schema format
#'
#' @param parameter_name The name of the parameter openAI should return
#' @param description A description of the parameter. OpenAI will use this description to construct it's response
#' @param type What type or types are allowed in OpenAI's response. i.e. c("string", "null") constrains openAI to returning a character string or NULL.
#' @param enum A list of the return values that OpenAI should adhere to when choosing a response.
#'
#' @return A formatted and named parameter list
#' @export
#'
#' @examples
#' function_call_parameter <- get_function_call_parameter("disease", "The outbreak event disease name")
get_function_call_parameter <- function(parameter_name,
                                        description,
                                        type = c("string", "null"),
                                        enum = NULL) {

  parameter <- purrr::compact(list("description" = description,
                                   "type" = type,
                                   "enum" = enum))

  list(parameter[!is.na(parameter)]) |> setNames(parameter_name)

}

#' Initiate a conversation with openAI using the provided model and system, common, and specific context. Note: to use these functions you need to provide an openai key in the .env file as in:
# OPENAI_API_KEY=
#'
#' @param specific_context This is the text specific to each query you would like OpenAI to consider
#' @param common_context This is the text OpenAI should consider in every query
#' @param hint An optional hint to get OpenAI on the right track.
#' @param model The identity of the model you would like to submit the context to.
#' @param system_context Text explaining to the model what role you would like it to play. In example, "Respond as if you an 18th century naturalist."
#'
#' @return A tibble containing the responses from openAI for each query
#' @export
#'
#' @examples
#' specific_context <- "Chronic wasting disease (CWD) is a fatal, prion disease of cervids that was first detected in Alberta in 2005"
#' common_context <- "Please use the extract_outbreak_data function to extract outbreak details from the given abstract and return only the results"
#' chat_results <- openai_chat(specific_context, common_context)
openai_chat <- function(specific_context,
                        common_context,
                        hint = NA,
                        model = 'gpt-3.5-turbo',
                        system_context = "You act as a function to extract outbreak infromation from a provided abstract.") {

  messages <-
    list(
        list(
            "role" = "system",
            "content" = system_context
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

  function_call = get_openai_function_call()

  outbreak_details <- openai::create_chat_completion(
    model = model,
    messages = messages,
    functions = function_call$function_call
  ) |> bind_cols()

  outbreak_details
}


#' Convert the JSON formatted openAI responses into a more human readable nested tibble.
#'
#' @param model_response
#'
#' @return A nested tibble
#' @export
#'
#' @examples
#' #' specific_context <- "Chronic wasting disease (CWD) is a fatal, prion disease of cervids that was first detected in Alberta in 2005"
#' common_context <- "Please use the extract_outbreak_data function to extract outbreak details from the given abstract and return only the results"
#' chat_results <- openai_chat(specific_context, common_context)
#' formatted_chat_results <- format_openai_results(chat_results)
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
