# EHA Large Language Model (LLM) resources

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
