#' Explain code based on a query using the Google PaLM 2 text model
#'
#' This function sends a query with a code snippet to the Google PaLM 2 text model and generates a detailed explanation of the code. It supports
#' various programming languages and allows you to customize the explanation.
#'
#' @param model.parameter A character vector containing the API key, model version, and proxy status. Model version and type
#' are specified by Google. See function \code{\link{palm.connect}} for detail.
#' @param prompt A character string representing the code snippet for explanation. The length of the code snippet should be between
#' 1 and 8196 characters, inclusive.
#' @param language A character string specifying the programming language used in the code (default: "R").
#' @param temperature A numeric value between 0.0 and 1.0, inclusive (default: 0.7). Controls the randomness of the generated explanation.
#' A higher value (e.g., 0.9) results in more creative responses, while a lower value (e.g., 0.3) produces more straightforward explanations.
#' @param maxOutputTokens An integer value (default: 1024). Specifies the maximum number of tokens to include
#' in the generated explanation.
#' @param topP A numeric value (default: 0.95). Defines the maximum cumulative probability of tokens considered
#' when sampling. It controls the diversity of the explanation generated.
#' @param topK An integer value (default: 40). Sets the maximum number of tokens to consider when sampling.
#' @param htUnspecified Safety setting threshold for unspecified harm. The default threshold is "meda". Valid options are as follows.
#'
#' \itemize{
#'    \item{"unsp"}{HARM\_BLOCK\_THRESHOLD\_UNSPECIFIED}
#'    \item{"lowa"}{BLOCK\_LOW\_AND\_ABOVE}
#'    \item{"meda"}{BLOCK\_MEDIUM\_AND\_ABOVE}
#'    \item{"high"}{BLOCK\_ONLY\_HIGH}
#'    \item{"none"}{BLOCK\_NONE}
#' }
#'
#' @param htDerogatory Safety setting threshold for derogatory harm. The default threshold is "meda". Valid options are as follows.
#'
#' \itemize{
#'    \item{"unsp"}{HARM\_BLOCK\_THRESHOLD\_UNSPECIFIED}
#'    \item{"lowa"}{BLOCK\_LOW\_AND\_ABOVE}
#'    \item{"meda"}{BLOCK\_MEDIUM\_AND\_ABOVE}
#'    \item{"high"}{BLOCK\_ONLY\_HIGH}
#'    \item{"none"}{BLOCK\_NONE}
#' }
#'
#' @param htToxicity Safety setting threshold for toxicity harm. The default threshold is "meda". Valid options are as follows.
#'
#' \itemize{
#'    \item{"unsp"}{HARM\_BLOCK\_THRESHOLD\_UNSPECIFIED}
#'    \item{"lowa"}{BLOCK\_LOW\_AND\_ABOVE}
#'    \item{"meda"}{BLOCK\_MEDIUM\_AND\_ABOVE}
#'    \item{"high"}{BLOCK\_ONLY\_HIGH}
#'    \item{"none"}{BLOCK\_NONE}
#' }
#'
#' @param htViolence Safety setting threshold for violence harm. The default threshold is "meda". Valid options are as follows.
#'
#' \itemize{
#'    \item{"unsp"}{HARM\_BLOCK\_THRESHOLD\_UNSPECIFIED}
#'    \item{"lowa"}{BLOCK\_LOW\_AND\_ABOVE}
#'    \item{"meda"}{BLOCK\_MEDIUM\_AND\_ABOVE}
#'    \item{"high"}{BLOCK\_ONLY\_HIGH}
#'    \item{"none"}{BLOCK\_NONE}
#' }
#'
#' @param htSexual Safety setting threshold for sexual harm. The default threshold is "meda". Valid options are as follows.
#'
#' \itemize{
#'    \item{"unsp"}{HARM\_BLOCK\_THRESHOLD\_UNSPECIFIED}
#'    \item{"lowa"}{BLOCK\_LOW\_AND\_ABOVE}
#'    \item{"meda"}{BLOCK\_MEDIUM\_AND\_ABOVE}
#'    \item{"high"}{BLOCK\_ONLY\_HIGH}
#'    \item{"none"}{BLOCK\_NONE}
#' }
#'
#' @param htMedical Safety setting threshold for medical harm. The default threshold is "meda". Valid options are as follows.
#'
#' \itemize{
#'    \item{"unsp"}{HARM\_BLOCK\_THRESHOLD\_UNSPECIFIED}
#'    \item{"lowa"}{BLOCK\_LOW\_AND\_ABOVE}
#'    \item{"meda"}{BLOCK\_MEDIUM\_AND\_ABOVE}
#'    \item{"high"}{BLOCK\_ONLY\_HIGH}
#'    \item{"none"}{BLOCK\_NONE}
#' }
#'
#' @param htDangerous Safety setting threshold for dangerous harm. The default threshold is "meda". Valid options are as follows.
#'
#' \itemize{
#'    \item{"unsp"}{HARM\_BLOCK\_THRESHOLD\_UNSPECIFIED}
#'    \item{"lowa"}{BLOCK\_LOW\_AND\_ABOVE}
#'    \item{"meda"}{BLOCK\_MEDIUM\_AND\_ABOVE}
#'    \item{"high"}{BLOCK\_ONLY\_HIGH}
#'    \item{"none"}{BLOCK\_NONE}
#' }
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
#' The `model.parameter` argument should be a character vector with the API key, model version, and model type provided by
#' Google. You can obtain this information by following the instructions provided by Google for using the PaLM API.
#'
#' The safety settings control the content's safety level based on different harm categories. Harm thresholds are
#' specified as per Google's guidelines and can be customized to control the content generated.
#'
#' @examples
#' \dontrun{
#' # Connect to the model, replace API_KEY with your api key
#' palm.model = palm.connect("v1beta2",
#'                           "API_KEY",
#'                           FALSE)
#'
#' prompt = "foo <- function(n) {
#'             if (n <= 0) {
#'               return(0)
#'             } else if (n == 1) {
#'               return(1)
#'             } else {
#'               return(foo(n - 1) + foo(n - 2))
#'             }
#'           }"
#' code.explanation = palm.txt.explain.code(palm.model,
#'                                          prompt)
#' cat(code.explanation)
#' }
#'
#' @seealso
#' \href{https://palmr.ly.gd.edu.kg/documentation/}{PaLMr - Documentation}
#'
#' \href{https://ai.google.dev/api/rest/v1beta/SafetySetting}{Safety Setting - Google AI for Developers}
#'
#' \href{https://ai.google.dev/api/rest/v1beta/HarmCategory}{HarmCategory - Google AI for Developers}
#'
#' @export
#'
#' @importFrom jsonlite toJSON
#' @importFrom httr GET POST add_headers content
palm.txt.explain.code = function(model.parameter,
                                 prompt,
                                 language = "R",
                                 temperature = 0.7,
                                 maxOutputTokens = 1024,
                                 topP = 0.95,
                                 topK = 40,
                                 htUnspecified = "meda",
                                 htDerogatory = "meda",
                                 htToxicity = "meda",
                                 htViolence = "meda",
                                 htSexual = "meda",
                                 htMedical = "meda",
                                 htDangerous = "meda") {
  model.parameter["version"] = match.arg(model.parameter["version"],
                                         c("v1beta2", "v1beta3"),
                                         several.ok = FALSE)

  apiURL = ifelse(
    model.parameter["proxy"],
    paste0(
      "https://api.genai.gd.edu.kg/google/",
      model.parameter["version"],
      "/models/text-bison-001",
      ":generateText?key=",
      model.parameter["api"]
    ),
    paste0(
      "https://generativelanguage.googleapis.com/",
      model.parameter["version"],
      "/models/text-bison-001",
      ":generateText?key=",
      model.parameter["api"]
    )
  )

  requestBody = list(
    prompt = list(
      text = paste0("Explain the following", language, "code:\n",
                    "# Code starts #\n",
                    prompt, "\n",
                    "# Code ends #\n")
    ),
    safetySettings = generateSafetySettings(htUnspecified,
                                            htDerogatory,
                                            htToxicity,
                                            htViolence,
                                            htSexual,
                                            htMedical,
                                            htDangerous),
    temperature = temperature,
    maxOutputTokens = as.integer(maxOutputTokens),
    topP = topP,
    topK = as.integer(topK)
  )

  requestBodyJSON = jsonlite::toJSON(requestBody, auto_unbox = TRUE)

  response = httr::POST(url = apiURL,
                        body = requestBodyJSON,
                        httr::add_headers("Content-Type" = "application/json"))
  responseJSON = httr::content(response, "parsed")

  generateOutput(responseJSON)
}
