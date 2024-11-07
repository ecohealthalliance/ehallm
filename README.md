# EHA Large Language Model (LLM) resources
EHALLM is a collection of R functions and scripts for interacting with OpenAI and other Large Language Models (LLM's). Right now the focus is on OpenAI's ChatGPT. The pipeline will eventually include:
1. Automated academic literature search
2. OCR and text extraction to serialize PDF text, tables, and figures
3. Function calling to enforce a structured response from an LLM
4. Prompts designed to extract information from literature corpora
5. A pipeline to submit requests and format, clean, and structure

## Installation

### Dependencies

`ehallm` depends on a [fork](https://github.com/nhardtskim/openai) of the `openai` r package to enable function calling. As of January 2024 function calling is still [not available](https://github.com/irudnyts/openai/pull/47) in the main `openai` package. The appropriate fork can be installed using the following command:

```
if (!require(remotes)) install.packages("remotes")
remotes::install_github("nhardtskim/openai")
```

### `ehallm` package

## Resources
[Prompt Engineering](https://www.promptingguide.ai/introduction/settings)
