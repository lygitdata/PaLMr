#' Set up and connect to the Google PaLM 2 text model.
#'
#' This function establishes a connection to the Google PaLM text model by specifying the API key, and model version.
#'
#' @param api A character string representing the API key for accessing the Google PaLM 2 text model.
#'   The API key should be 39 characters long and must be of the "character" class.
#' @param version A character string representing the model version to use, either \code{"v1beta2"} or \code{"v1beta3"}.
#' @param proxy A boolean value indicating whether to use a proxy for accessing the API
#' URL (default is \code{FALSE}). If your local internet cannot access the APIs, set this
#' parameter to \code{TRUE}.
#'
#' @return If successful, the function returns a character vector containing the API key, model version, and proxy status.
#'   If the API response indicates an error, the function stops execution and provides an error message.
#'
#' @details
#' This function performs the necessary setup to connect to the Google PaLM 2 text model. It validates the provided API key and
#' checks the correctness of the model version. If the input is valid, it constructs the API request and sends
#' it to the PaLM 2 API endpoint.
#'
#' If an error occurs during the API request, such as an invalid API key or input parameters, an error message is displayed.
#' If the API request is successful, the function prints the model details to the console and returns a character vector
#' with the API key, model version, and model type.
#'
#' @examples
#' \dontrun{
#' # Connect to the model, replace API_KEY with your api key
#' palm.model = palm.connect("v1beta2",
#'                           "API_KEY",
#'                           FALSE)
#' palm.model
#' }
#'
#' @seealso
#' \href{https://palmr.ly.gd.edu.kg/documentation/}{PaLMr - Documentation}
#'
#' @export
#'
#' @importFrom jsonlite toJSON
#' @importFrom httr GET POST add_headers content
palm.connect = function(version,
                        api,
                        proxy = FALSE) {
  version = match.arg(version, c("v1beta2", "v1beta3"), several.ok = FALSE)

  apiURL = ifelse(
    proxy,
    paste0(
      "https://api.genai.gd.edu.kg/google/",
      version,
      "/models/text-bison-001",
      "?key=",
      api
    ),
    paste0(
      "https://generativelanguage.googleapis.com/",
      version,
      "/models/text-bison-001",
      "?key=",
      api
    )
  )

  response = httr::GET(url = apiURL,
                       httr::add_headers("Content-Type" = "application/json"))
  responseJSON = httr::content(response, "parsed")

  if (!is.null(responseJSON$error)) {
    stop(responseJSON$error$message)
  }

  return(c(
    "version" = version,
    "api" = api,
    "proxy" = proxy
  ))
}
