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
