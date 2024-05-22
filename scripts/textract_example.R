library(paws)
library(tidyverse)

# AWS S3 bucket name and pdf file location within the bucket
bucket <- "project-dtra-ml-main"
file <- "papers/fpd.2016.2182.pdf"

# Load some required functions
for (r_file in list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)) try(source(r_file))

# Get a PDF document's tables using Amazon Textract.
document <- textract_document(bucket, file)

# Extract the blocks as a nested tibble
blocks <- get_textract_blocks(document)

# Extracat the tables as a nested tibble
tables <- get_tables(blocks)

# Extract the document text
text <- get_block_children(blocks, blocks) |> filter(BlockType == "LINE") |> pull(Text) |> paste(collapse = "\n")

cat(text)

tables
walk(1:nrow(tables), ~print(tables[.x,]$table))

library(openai)

system_message <- list("role" = "system", "content" = "You use serialized text and JSON formatted tables to extract AMR outbreak data from scientific articles.")
text_message <- list("role" = "user", "content" = text)
tables_message <- list("role" = "user", "content" = jsonlite::toJSON(tables))
task_message <- list("role" = "user", "content" = "Please return the following information formated as a JSON object. DOI,Author,PubDate,ISO3,YCoord,XCoord,StartDate,EndDate,Species,SampleType,SampleOrigin,Method,Pathogens,Strain,Nsamples,Prev,NIsolates,Compound,ATC.Code,Rescom,Concg,Guidelines,Breakpoint,Remark,Class,WHO_MedImp")

# This could be improved by using function calling to better describe what we want and how we want it returned.
completion <- openai::create_chat_completion(
  model = "gpt-3.5-turbo",
  messages = list(system_message,
                  text_message,
                  tables_message,
                  task_message))

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
