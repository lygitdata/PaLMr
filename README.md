# Gemini is on the horizon. Kindly explore the latest R package, "GenAI" available at https://github.com/GitData-GA/GenAI. This package seamlessly integrates with both Google's and OpenAI's models.

<img src="https://genai.gd.edu.kg/assets/img/logo.jpg" width="300px" alt="Logo">

<HR>

# PaLMr - Interface for Google Pathways Language Model (PaLM)

<img src="https://palmr.ly.gd.edu.kg/img/PaLMr_logo.svg" width="100px">

## 1. Basic Information

Version: 0.1.4

Author: Li Yuan

Maintainer: Li Yuan <lyuan@gd.edu.kg>

Description: ‘Google’s ’PaLM’ https://developers.generativeai.google/ as a coding and writing assistant designed for ‘R’ and ‘RStudio.’ With a range of functions, including natural language processing and coding optimization, to assist R developers in simplifying tedious coding tasks and content searching.

URL: https://palmr.r.ly.gd.edu.kg/

Imports: `httr`, `jsonlite`

[![CRAN status](https://www.r-pkg.org/badges/version/PaLMr)](https://CRAN.R-project.org/package=PaLMr)
[![R-CMD-check](https://github.com/lygitdata/PaLMr/workflows/R-CMD-check/badge.svg)](https://github.com/lygitdata/PaLMr/actions)
[![CRAN](https://cranlogs.r-pkg.org/badges/grand-total/PaLMr)](https://cran.r-project.org/package=PaLMr)
[![License: CC BY 4.0](https://img.shields.io/badge/License-CC_BY_4.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)
[![build pass](https://img.shields.io/circleci/build/github/badges/shields/master.svg)](https://github.com/lygitdata/PaLMr/releases)
[![github](https://img.shields.io/badge/GitHub-100000)](https://github.com/lygitdata/PaLMr)

## 2. Quickstart

## Step 1: 

Login with your personal Google account (Google Workspace accounts will not work). Get Google Generative AI API key from https://makersuite.google.com/. On the left side navigation bar, click on "Get API Key" (or click on the small key image). Finally, click on "Create API in new project".

<p><img src="https://palmr.r.ly.gd.edu.kg/img/api_step1.png" width="500px"/><img src="https://palmr.r.ly.gd.edu.kg/img/api_step2.png" width="500px"/><img src="https://palmr.r.ly.gd.edu.kg/img/api_step3.png" width="500px"/></p>

## Step 2

Open your R IDE, install the package, and include the library.

### Installation

**Option 1**. Install the package from [The Comprehensive R Archive Network (CRAN)](https://cran.r-project.org/package=PaLMr):

```{r, eval=FALSE, warning=FALSE, results='hide', message=FALSE}
install.packages("PaLMr")
```

**Option 2**. Install the package from the [official website of PaLMr](https://palmr.r.ly.gd.edu.kg/):

```{r, eval=FALSE, warning=FALSE, results='hide', message=FALSE}
install.packages("https://palmr.r.ly.gd.edu.kg/PaLMr_latest.tar.gz", repos=NULL, method="libcurl")
```

### Import the Package

```{r}
library(PaLMr)
```

## Step 3: 

Setup your Google Generative AI parameters in your R IDE:

```
# replace your_api_key_here with the API key you just got from https://makersuite.google.com/
apiKey = "your_api_key_here"
# choose the model version, either "v1beta2" or "v1beta3"
modelVersion = "v1beta3"
# choose the model type, there is only "text-bison-001" for now
modelType = "text-bison-001"
# setup your connection to the Google PaLM API
PaLM = setupPALM(apiKey, modelVersion, modelType)
```

## Step 4: 

Enjoy using Google Generative AI with our library - PaLMr! The following is a summary table, including current available functions calling the Google PaLM API.

| Function                                     | Description                                               | Detail & Example                                                |
| -------------- | -------------------- | ----------------------|
| `setupPALM(...)`| Set up and connect to the Google PaLM model.| <a href="https://palmr.r.ly.gd.edu.kg/#setupPALM()">Go to doc</a> |
| `getTextPALM(...)` | Generate text using the Google PaLM API. | <a href="https://palmr.r.ly.gd.edu.kg/#getTextPALM()">Go to doc</a> |
| `fixGrammarPALM(...)` | Fix Grammar and Rewrite Text Using the Google PaLM API. | <a href="https://palmr.r.ly.gd.edu.kg/#fixGrammarPALM()">Go to doc</a> |
| `getReferencePALM(...)` | Get References Using the Google PaLM API. | <a href="https://palmr.r.ly.gd.edu.kg/#getReferencePALM()">Go to doc</a> |
| `explainCodePALM(...)` | Explain Code Using the Google PaLM API. | <a href="https://palmr.r.ly.gd.edu.kg/#explainCodePALM()">Go to doc</a> |
| `optimizeCodePALM(...)` | Optimize Code Using the Google PaLM API. | <a href="https://palmr.r.ly.gd.edu.kg/#optimizeCodePALM()">Go to doc</a> |


## 3. Issues and Suggestions

If you find any issues or have any suggestions, please report them through GitHub repository issues section: https://github.com/lygitdata/PaLMr/issues
