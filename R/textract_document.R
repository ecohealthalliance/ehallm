#' Analyze a document store in an S3 bucket. Requires having AWS access id and key in .env
#'
#' @param bucket # The AWS S3 bucket name
#' @param file # The file name including path to analyze
#'
#' @return
#' @export
#'
#' @examples
textract_document <- function(bucket, file, timeout = 30) {

  # Start analyzing the PDF.
  textract <- paws::textract()

  resp <- textract$start_document_analysis(
    DocumentLocation = list(
      S3Object = list(Bucket = bucket, Name = file)
    ),
    FeatureTypes = "TABLES"
  )

  # Check that the analysis is done and get the result.
  count <- 0
  while (count < timeout && (!exists("result") || result$JobStatus == "IN_PROGRESS")) {
    Sys.sleep(1)
    result <- textract$get_document_analysis(
      JobId = resp$JobId
    )
    # If the result has multiple parts, get the remaining parts.
    next_token <- result$NextToken
    while (length(next_token) > 0) {
      next_result <- textract$get_document_analysis(
        JobId = resp$JobId,
        NextToken = next_token
      )
      result$Blocks <- c(result$Blocks, next_result$Blocks)
      next_token <- next_result$NextToken
    }
    count <- count + 1
  }

  return(result)
}
