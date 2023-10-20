#' Set up and connect to the Google PaLM model.
#'
#' This function establishes a connection to the Google PaLM model by specifying the API key, model version, and model type.
#'
#' @param apiKey A character string representing the API key for accessing the Google PaLM model.
#'   The API key should be 39 characters long and must be of the "character" class.
#' @param modelVersion The model version to use ("v1beta2" or "v1beta3"). Must be one of the available model versions.
#' @param modelType The model type ("text-bison-001"). This is the only model type available for now.
#'
#' @return If successful, the function returns a character vector containing the API key, model version, and model type.
#'   If the API response indicates an error, the function stops execution and provides an error message.
#'
#' @details
#' This function performs the necessary setup to connect to the Google PaLM model. It validates the provided API key and
#' checks the correctness of the model version and type. If the input is valid, it constructs the API request and sends
#' it to the PaLM API endpoint.
#'
#' If an error occurs during the API request, such as an invalid API key or input parameters, an error message is displayed.
#' If the API request is successful, the function prints the model details to the console and returns a character vector
#' with the API key, model version, and model type.
#'
#' Obtaining Google PaLMr API key may not be available in some regions. Please refer to \href{https://developers.generativeai.google/available_regions#available_regions}{Available Regions - Google PaLMr}
#' and \href{https://developers.generativeai.google/tutorials/setup}{Get An API Key - Google PaLMr}.
#'
#' @examples
#' \dontrun{
#' # Replace your_api_key_here with the API key you get from Google
#' apiKey <- "your_api_key_here"
#' modelVersion <- "v1beta3"
#' modelType <- "text-bison-001"
#' result <- setupPALM(apiKey, modelVersion, modelType)
#'
#' # The 'result' character vector may look like this if successful:
#' # [1] "your_api_key_here" "v1beta3"           "text-bison-001"
#' }
#'
#' @seealso
#' \href{https://developers.generativeai.google/available_regions#available_regions}{Available Regions - Google PaLMr}
#'
#' \href{https://developers.generativeai.google/tutorials/setup}{Get An API Key - Google PaLMr}
#'
#' @importFrom PaLMr checkModelSelection
#'
#' @export
setupPALM = function(apiKey, modelVersion, modelType) {
  # Check whether modelVersion and modelType are vaild
  # apiKey vaildilty will be checked by PaLM API
  checkModelSelection(modelVersion, modelType)

  # Create the API URL
  apiURL = paste0("https://generativelanguage.googleapis.com/",
                  modelVersion,
                  "/models/", modelType,
                  "?key=", apiKey)

  # Get response from the API
  response = httr::GET(url = apiURL,
                       httr::add_headers("Content-Type" = "application/json"))
  responseJSON = httr::content(response, "parsed")

  # Check if the response is an error
  if (!is.null(responseJSON$error)) {
    stop(responseJSON$error$message)
  }

  # Return checked parameters
  c(apiKey, modelVersion, modelType)
}
