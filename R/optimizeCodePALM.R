#' Optimize code based on a query using the Google PaLM model.
#'
#' This function sends a query with a code snippet to the Google PaLM model and generates an optimized version of the code. You can specify the programming language and the aspect you want to optimize (e.g., "runtime" or "memory"). The optimized code is provided along with the original code for comparison.
#'
#' @param palmParameter A character vector containing the API key, model version, and model type, as provided by Google.
#' The API key should be a 39-character string. Model version and type are specified by Google. See function `setupPALM()` for details.
#' @param inquery A character string representing the code snippet to be optimized. The length of the code snippet should be between
#' 1 and 8196 characters, inclusive.
#' @param language A character string specifying the programming language used in the code (default: "R").
#' @param aspect A character string specifying the aspect you want to optimize, such as "runtime", "memory", "runtime&memory", and "general" (default: "general").
#' @param temperature A numeric value between 0.0 and 1.0, inclusive (default: 0.7). Controls the randomness of the generated optimization.
#' A higher value (e.g., 0.9) results in more creative optimizations, while a lower value (e.g., 0.3) produces more straightforward optimizations.
#' @param maxOutputTokens An integer value between 1 and 1024, inclusive (default: 1024). Specifies the maximum number of tokens to include
#' in the generated optimization.
#' @param topP A numeric value between 0.0 and 1.0, inclusive (default: 0.95). Defines the maximum cumulative probability of tokens considered
#' when sampling. It controls the diversity of the optimization generated.
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
#'
#' @return A character string containing the optimized version of the provided code snippet based on the query and parameters.
#'
#' @details
#' This function interacts with the Google PaLM model by sending a code query for code optimization. It allows you to customize the generated
#' code optimizations by specifying the programming language, optimization aspect, and additional parameters like temperature, token limits,
#' and safety settings.
#'
#' If the function is successful, it returns an optimized version of the provided code as a character string. If an error occurs during the
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
#' # Optimize code based on a query
#'
#' # Example 1
#' inquery <- "
#'   foo <- function(n) {
#'     if (n <= 0) {
#'       return(0)
#'     } else if (n == 1) {
#'       return(1)
#'     } else {
#'       return(foo(n - 1) + foo(n - 2))
#'     }
#'   }
#' "
#' print(optimizeCodePALM(palmParameter, inquery, temperature = 0.7,
#'                        language = "R", aspect = "runtime"))
#'
#' # Example output
#' ## ```
#' ## foo <- function(n) {
#' ##   if (n <= 0) {
#' ##     return(0)
#' ##   } else if (n == 1) {
#' ##     return(1)
#' ##   } else {
#' ##     return(foo(n - 1) + foo(n - 2))
#' ##   }
#' ## }
#' ##
#' ## # Optimized version
#' ##
#' ## foo <- function(n) {
#' ##   if (n <= 0) {
#' ##     return(0)
#' ##   } else if (n == 1) {
#' ##     return(1)
#' ##   } else {
#' ##     a <- foo(n - 1)
#' ##     b <- foo(n - 2)
#' ##     return(a + b)
#' ##   }
#' ## }
#' ## ```
#'
#' # Example 2
#' inquery <- "
#'   #include <iostream>
#'   #include <vector>
#'
#'   unsigned long long factorial_recursive(int n) {
#'       if (n <= 1) {
#'           return 1;
#'       } else {
#'           return n * factorial_recursive(n - 1);
#'       }
#'   }
#'
#'   int main() {
#'       int n = 10;
#'       unsigned long long result = factorial_recursive(n);
#'       std::cout << 'Factorial of ' << n << ' is ' << result << std::endl;
#'       return 0;
#'   }
#' "
#' print(optimizeCodePALM(palmParameter, inquery, temperature = 1,
#'                        language = "C++", aspect = "runtime&memory"))
#'
#' # Example output
#' ## ```c++
#' ## #include <iostream>
#' ## #include <vector>
#' ##
#' ## unsigned long long factorial_iterative(int n) {
#' ##   unsigned long long product = 1;
#' ##   for (int i = 2; i <= n; i++) {
#' ##     product *= i;
#' ##   }
#' ##   return product;
#' ## }
#' ##
#' ## int main() {
#' ##   int n = 10;
#' ##   unsigned long long result = factorial_iterative(n);
#' ##   std::cout << "Factorial of " << n << " is " << result << std::endl;
#' ##   return 0;
#' ## }
#' ## ```
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
optimizeCodePALM = function(palmParameter, inquery, language = "R", aspect = "general",
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
  aspect = match.arg(aspect, c("general", "runtime", "memory", "runtime&memory"), several.ok = FALSE)

  # Define the API URL
  apiURL = paste0("https://generativelanguage.googleapis.com/",
                  palmParameter[2],
                  "/models/", palmParameter[3],
                  ":generateText?key=", palmParameter[1])

  # Create the request body as a JSON object
  requestBody = list(
    prompt = list(
      text = paste("Optimize the following", language,
                   "code from", aspect, "aspect:\n",
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
