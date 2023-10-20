#' Explain code based on a query using the Google PaLM model.
#'
#' This function sends a query with a code snippet to the Google PaLM model and generates a detailed explanation of the code. It supports
#' various programming languages and allows you to customize the explanation. The explanation includes a step-by-step breakdown of how
#' the code works.
#'
#' @param palmParameter A character vector containing the API key, model version, and model type, as provided by Google.
#' The API key should be a 39-character string. Model version and type are specified by Google. See function `setupPALM()` for details.
#' @param inquery A character string representing the code snippet for explanation. The length of the code snippet should be between
#' 1 and 8196 characters, inclusive.
#' @param language A character string specifying the programming language used in the code (default: "R").
#' @param temperature A numeric value between 0.0 and 1.0, inclusive (default: 0.7). Controls the randomness of the generated explanation.
#' A higher value (e.g., 0.9) results in more creative responses, while a lower value (e.g., 0.3) produces more straightforward explanations.
#' @param maxOutputTokens An integer value between 1 and 1024, inclusive (default: 1024). Specifies the maximum number of tokens to include
#' in the generated explanation.
#' @param topP A numeric value between 0.0 and 1.0, inclusive (default: 0.95). Defines the maximum cumulative probability of tokens considered
#' when sampling. It controls the diversity of the explanation generated.
#' @param topK An integer value between 1 and 1,000,000, inclusive (default: 40). Sets the maximum number of tokens to consider when sampling.
#' @param htUnspecified Safety setting threshold for unspecified harm. The default threshold is "meda." Refer to \href{https://developers.generativeai.google/api/rest/generativelanguage/HarmCategory}{HarmCategory - Google PaLMr}.
#' Valid options include:
#'
#' - "unsp" (HARM_BLOCK_THRESHOLD_UNSPECIFIED)
#'
#' - "lowa" (BLOCK_LOW_AND_ABOVE)
#'
#' - "meda" (BLOCK_MEDIUM_AND_ABOVE)
#'
#' - "high" (BLOCK_ONLY_HIGH)
#'
#' - "none" (BLOCK_NONE)
#'
#' @param htDerogatory Safety setting threshold for derogatory harm. The default threshold is "meda." Refer to \href{https://developers.generativeai.google/api/rest/generativelanguage/HarmCategory}{HarmCategory - Google PaLMr}.
#' Valid options include:
#'
#' - "unsp" (HARM_BLOCK_THRESHOLD_UNSPECIFIED)
#'
#' - "lowa" (BLOCK_LOW_AND_ABOVE)
#'
#' - "meda" (BLOCK_MEDIUM_AND_ABOVE)
#'
#' - "high" (BLOCK_ONLY_HIGH)
#'
#' - "none" (BLOCK_NONE)
#'
#' @param htToxicity Safety setting threshold for toxicity harm. The default threshold is "meda." Refer to \href{https://developers.generativeai.google/api/rest/generativelanguage/HarmCategory}{HarmCategory - Google PaLMr}.
#' Valid options include:
#'
#' - "unsp" (HARM_BLOCK_THRESHOLD_UNSPECIFIED)
#'
#' - "lowa" (BLOCK_LOW_AND_ABOVE)
#'
#' - "meda" (BLOCK_MEDIUM_AND_ABOVE)
#'
#' - "high" (BLOCK_ONLY_HIGH)
#'
#' - "none" (BLOCK_NONE)
#'
#' @param htViolence Safety setting threshold for violence harm. The default threshold is "meda." Refer to \href{https://developers.generativeai.google/api/rest/generativelanguage/HarmCategory}{HarmCategory - Google PaLMr}.
#' Valid options include:
#'
#' - "unsp" (HARM_BLOCK_THRESHOLD_UNSPECIFIED)
#'
#' - "lowa" (BLOCK_LOW_AND_ABOVE)
#'
#' - "meda" (BLOCK_MEDIUM_AND_ABOVE)
#'
#' - "high" (BLOCK_ONLY_HIGH)
#'
#' - "none" (BLOCK_NONE)
#'
#' @param htSexual Safety setting threshold for sexual harm. The default threshold is "meda." Refer to \href{https://developers.generativeai.google/api/rest/generativelanguage/HarmCategory}{HarmCategory - Google PaLMr}.
#' Valid options include:
#'
#' - "unsp" (HARM_BLOCK_THRESHOLD_UNSPECIFIED)
#'
#' - "lowa" (BLOCK_LOW_AND_ABOVE)
#'
#' - "meda" (BLOCK_MEDIUM_AND_ABOVE)
#'
#' - "high" (BLOCK_ONLY_HIGH)
#'
#' - "none" (BLOCK_NONE)
#'
#' @param htMedical Safety setting threshold for medical harm. The default threshold is "meda." Refer to \href{https://developers.generativeai.google/api/rest/generativelanguage/HarmCategory}{HarmCategory - Google PaLMr}.
#' Valid options include:
#'
#' - "unsp" (HARM_BLOCK_THRESHOLD_UNSPECIFIED)
#'
#' - "lowa" (BLOCK_LOW_AND_ABOVE)
#'
#' - "meda" (BLOCK_MEDIUM_AND_ABOVE)
#'
#' - "high" (BLOCK_ONLY_HIGH)
#'
#' - "none" (BLOCK_NONE)
#'
#' @param htDangerous Safety setting threshold for dangerous harm. The default threshold is "meda." Refer to \href{https://developers.generativeai.google/api/rest/generativelanguage/HarmCategory}{HarmCategory - Google PaLMr}.
#' Valid options include:
#'
#' - "unsp" (HARM_BLOCK_THRESHOLD_UNSPECIFIED)
#'
#' - "lowa" (BLOCK_LOW_AND_ABOVE)
#'
#' - "meda" (BLOCK_MEDIUM_AND_ABOVE)
#'
#' - "high" (BLOCK_ONLY_HIGH)
#'
#' - "none" (BLOCK_NONE)
#'
#' @return A character string containing the detailed explanation of the provided code snippet based on the query and parameters.
#'
#' @details
#' This function interacts with the Google PaLM model by sending a code query to explain code. It allows you to customize the generated
#' code explanations by specifying the programming language, and additional parameters like temperature, token limits, and safety settings.
#'
#' If the function is successful, it returns a detailed explanation of the provided code as a character string. If an error occurs during the
#' API request, it will stop execution and provide an error message.
#'
#' The `palmParameter` argument should be a character vector with the API key, model version, and model type provided by Google. You can
#' obtain this information by following the instructions provided by Google for using the PaLM API.
#'
#' The safety settings control the content's safety level based on different harm categories. Harm thresholds are specified as per Google's
#' guidelines and can be customized to control the content generated.
#'
#' For more information on safety settings, harm categories, and harm thresholds, refer to the official Google PaLM API documentation:
#' \href{https://developers.generativeai.google/api/rest/generativelanguage/SafetySetting}{Safety Setting - Google PaLMr}
#'
#' @examples
#' \dontrun{
#' # Set up the PaLM parameters
#' # Replace your_api_key_here with the API key you get from Google
#' palmParameter <- c("your_api_key_here", "v1beta3", "text-bison-001")
#'
#' # Explain code based on a query
#'
#' # Example 1
#' inquery <- "
#'   foo <- function(n) {
#'     if (n == 0) {
#'       return(1)
#'     } else {
#'       return(n * foo(n - 1))
#'     }
#'   }
#' "
#' print(explainCodePALM(palmParameter, inquery,
#'                       temperature = 0.7, language = "R"))
#'
#' # Example output:
#' ## The function `foo` takes a number `n` as input and returns the value of `n!`.
#' ## The function uses a recursive approach, which means that it calls itself to
#' ## calculate the factorial of smaller numbers. The base case is when `n == 0`,
#' ## in which case the function simply returns `1`. Otherwise, the function multiplies
#' ## `n` by the factorial of `n - 1`.
#' ## ## Here is a step-by-step breakdown of how the function works:
#' ## ## 1. When `n == 0`, the function returns `1`.
#' ## 2. When `n > 0`, the function first multiplies `n` by the factorial of `n - 1`.
#' ## 3. The function then calls itself to calculate the factorial of `n - 1`.
#' ## 4. This process continues until the base case is reached, at which point the
#' ## function returns `1`.
#' ##
#' ## The following table shows how the function would calculate the factorial of 5:
#' ##
#' ## | n | `foo(n)` |
#' ## |---|---|
#' ## | 0 | 1 |
#' ## | 1 | 1 |
#' ## | 2 | 2 |
#' ## | 3 | 6 |
#' ## | 4 | 24 |
#' ## | 5 | 120 |
#'
#' # Example 2
#' inquery <- "
#'   llm_data %>%
#'    ggplot(aes(x=Training_Data,y=Params, label=Model))+
#'    geom_label()+
#'    labs(
#'    x= 'Training Data (billion tokens)',
#'    y= 'Parameters (billions)'
#'    )+
#'    theme_bw()
#' "
#' print(explainCodePALM(palmParameter, inquery, language = "Python"))
#'
#' # Example output:
#' ## The code above is using the `ggplot2` library to create a scatter plot of
#' ## the `llm_data` dataset. The `x`-axis of the plot shows the amount of training
#' ## data (in billions of tokens), and the `y`-axis shows the number of parameters
#' ## (in billions). The `geom_label()` function is used to add labels to each point
#' ## on the plot, which show the name of the model. The `labs()` function is used
#' ## to add titles and axis labels to the plot. Finally, the `theme_bw()` function
#' ## is used to apply a black and white theme to the plot.
#' ## ## Here is a breakdown of the code:
#' ##
#' ## * `llm_data %>%`: This loads the `llm_data` dataset into the `ggplot2` environment.
#' ## * `ggplot(aes(x=Training_Data,y=Params, label=Model))`: This creates a scatter
#' ## plot with the `x`-axis being `Training_Data`, the `y`-axis being `Params`, and
#' ## the labels being `Model`.
#' ## * `geom_label()`: This adds labels to each point on the plot.
#' ## * `labs(x= 'Training Data (billion tokens)', y= 'Parameters (billions)')`: This adds
#' ## titles and axis labels to the plot.
#' ## * `theme_bw()`: This applies a black and white theme to the plot.
#' }
#'
#' @seealso
#' \href{https://developers.generativeai.google/api/rest/generativelanguage/SafetySetting}{Safety Setting - Google PaLMr}
#'
#' \href{https://developers.generativeai.google/api/rest/generativelanguage/HarmCategory}{HarmCategory - Google PaLMr}
#'
#' @export
#'
#' @importFrom PaLMr checkModelSelection
#' @importFrom PaLMr generateSafetySettings
#' @importFrom PaLMr generateOutput
explainCodePALM = function(palmParameter, inquery, language = "R",
                           temperature = 0.7,
                           maxOutputTokens = 1024, topP = 0.95, topK = 40,
                           htUnspecified = "meda",
                           htDerogatory = "meda",
                           htToxicity = "meda",
                           htViolence = "meda",
                           htSexual = "meda",
                           htMedical = "meda",
                           htDangerous = "meda") {
  checkModelSelection(palmParameter[2], palmParameter[3])

  # Define the API URL
  apiURL = paste0("https://generativelanguage.googleapis.com/",
                  palmParameter[2],
                  "/models/", palmParameter[3],
                  ":generateText?key=", palmParameter[1])

  # Create the request body as a JSON object
  inquery = gsub('"', '\\"', inquery)
  requestBody = list(
    prompt = list(
      text = paste("Explain the following", language, "code:\n",
                   "# Code starts #\n",
                   inquery, "\n",
                   "# Code ends #\n")
    ),
    safetySettings = generateSafetySettings(htUnspecified, htDerogatory, htToxicity,
                                            htViolence, htSexual, htMedical, htDangerous),
    temperature = temperature,
    maxOutputTokens = as.integer(maxOutputTokens),
    topP = topP,
    topK = as.integer(topK)
  )

  # Convert the request body to JSON
  requestBodyJSON = jsonlite::toJSON(requestBody, auto_unbox = TRUE)

  # Get response from the API
  response = httr::POST(url = apiURL,
                        body = requestBodyJSON,
                        httr::add_headers("Content-Type" = "application/json"))
  responseJSON = httr::content(response, "parsed")

  # Generate output
  generateOutput(responseJSON)
}
