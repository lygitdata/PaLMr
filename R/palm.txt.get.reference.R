#' Get references based on a query using the Google PaLM 2 text model
#'
#' This function sends a query to the Google PaLM 2 text model and generates a list of references based on the query. It allows customization of the
#' generated references and supports various citation styles and source types.
#'
#' @param model.parameter A character vector containing the API key, model version, and proxy status. Model version and type
#' are specified by Google. See function \code{\link{palm.connect}} for detail.
#' @param prompt A character string representing the code snippet for explanation. The length of the code snippet should be between
#' 1 and 8196 characters, inclusive.
#' @param source.type A character string specifying the type of sources to search for (default: "articles").
#' @param source.date A character string specifying the date range for the sources (default: "most recent").
#' @param n.source An integer value specifying the number of sources to retrieve (default: 5).
#' @param citation.style A character string specifying the citation style for the references (default: "APA7").
#' @param temperature A numeric value between 0.0 and 1.0, inclusive (default: 0.7). Controls the randomness of the generated references.
#' A higher value (e.g., 0.9) results in more creative responses, while a lower value (e.g., 0.3) produces more straightforward references.
#' @param maxOutputTokens An integer value (default: 1024). Specifies the maximum number of tokens to include in the generated references.
#' @param topP A numeric value between (default: 0.95). Defines the maximum cumulative probability of tokens considered when sampling.
#' It controls the diversity of the references generated.
#' @param topK An integer value between (default: 40). Sets the maximum number of tokens to consider when sampling.
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
#' @return A character string containing the generated references based on the provided query and parameters.
#'
#' @details
#' This function interacts with the Google PaLM model by sending a query to find references. It allows you to customize the generated
#' references by specifying the number of sources, citation style, source type, date range, and safety settings.
#'
#' If the function is successful, it returns a character vector containing the generated references. If an error occurs during the
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
#' correct.text = palm.txt.get.reference(palm.model,
#'                                       "H5N1 in the United States")
#' cat(correct.text)
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
palm.txt.get.reference = function(model.parameter,
                                  prompt,
                                  source.type = "articles",
                                  source.date = "most recent",
                                  n.source = 5,
                                  citation.style = "APA7",
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
      text = paste("Find", n.source ,
                   source.date ,"source(s) from real and accessible", source.type,
                   ". The source(s) must be related to", prompt,
                   ". Present the reference list in the citation style of", citation.style)
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
