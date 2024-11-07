# Imagine you're passing post it notes to someone in another room instructing them to build something for you out of legos. And they're slightly drunk. You have three main challenges.

# 1) Being as precise as possible given you're using post it notes. To many notes can be confusing, not enough detail and they won't know what to make.
# B) How do you quantify success at building with legos?
# III) Data harmonization.

library(paws)
library(tidyverse)

# AWS S3 bucket name and pdf file location within the bucket
bucket <- "project-dtra-ml-main"
file <- "papers/test.pdf"

# Load some required functions
for (r_file in list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)) try(source(r_file))

# Get a PDF document's tables using Amazon Textract.
document <- textract_document(bucket, file)

# Extract the blocks as a nested tibble
blocks <- get_textract_blocks(document)

# Extract the tables as a nested tibble
tables <- get_tables(blocks)

# Extract the document text
text <- get_block_children(blocks, blocks) |> filter(BlockType == "LINE") |> pull(Text) |> paste(collapse = "\n")

cat(text)

tables
walk(1:nrow(tables), ~print(tables[.x,]$table))

library(openai)

# Define ChatGPT's role
system_message <- list("role" = "system", "content" = "You use serialized text and JSON formatted tables to extract details of specific epizootic risk mitigation measure or strategies from scientific articles to identify which specific mitigation measures were used.")

# Define the specific query
task_message <- list("role" = "user", "content" = "For each specific mitigation strategy, please return the following information formated as a JSON object. The article DOI, the authors as a list, the disease pathogen (such as rabies), the host species name (such as Canis lupus), the specific mitigation stategy employed, a brief summary of the specific mitigation strategy employed, the date the specific mitigation measure was implemented, the country where the specific mitigation measure was implemented, whether the specific mitigation measure was proposed tested or evaluated, the efficacy of the measure if reported, how efficacy was measured if it was reported, and the sentence detailing what kind of specific mitigation strategy was employed reproduced in full.")

# Provide clarification or examples
example_message <- list("role" = "user", "content" = "For the specific mitigation strategy do not respond with 'risk mitigation strategy' or 'risk mitigation measures' instead identify which specific mitigation strategy employed. For example, specific mitigation strategies could include things like active environmental surveillance, importation bans, host monitoring and control, vector monitoring and control, serology and sequencing, establishing seasonal infection-free zones, sanitation and hygiene measures, or controlling animal transport and housing conditions. If multiple specific strategies were employed, return all information for each such as DOI, author, pathogen and host as a separate row.")

# Provide the pdf text
text_message <- list("role" = "user", "content" = text)

# Provide serialized tables as JSON
tables_message <- list("role" = "user", "content" = jsonlite::toJSON(tables))

# Submit the request
completion <- openai::create_chat_completion(
  model = "gpt-4",
  messages = list(system_message,
                  task_message,
                  example_message,
                  text_message,
                  tables_message))

# ERROR handling
# Resubmit if chatGPT returns a malformed JSON
response <- tryCatch({
    completion$choices$message.content |> jsonlite::fromJSON()
  },
  error = function(cond) {
    system_message = list("role" = "system", "content" = "You repair malformed JSON objects and return only the corrected JSON")
    user_message = list("role" = "user", "content" = completion$choices$message.content)
    repair_completion <- openai::create_chat_completion(
      model = "gpt-3.5-turbo",
      messages = list(system_message,
                      user_message))

    repair_completion$choices$message.content |> jsonlite::fromJSON()
  })
# Resubmit if chatGPT tries to talk back instead of returning a JSON object
# Resubmit if....

# RESULTS
# Before we see the response a review:
# It's EASY to get the model to talk back to you.
# It's HARD to 1) measure performance 2) harmonize replies

# Here's the response as a list object
response

# The goal is tabular data - which depends on how the model formats it's reply
response |> as_tibble()

response |> as_tibble() |> pull(data) |> pull(pathogen)

response |> as_tibble() |> pull(data) |> pull(host)

response |> as_tibble() |> pull(data) |> pull(summary)

response |> as_tibble() |> pull(data) |> pull('mitigation strategy')


# But even when responses can be converted to tables there is no guarantee
# that the way the data is structured will be the same next time without further
# work!

# The solution to that is more query engineering and something called 'function calling'
# https://www.promptingguide.ai/applications/function_calling
