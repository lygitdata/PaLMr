#' @noRd
checkModelSelection = function(modelVersion, modelType) {
  # Check whether parameters are vaild apiKey vaildilty will be checked by PaLM API
  modelVersion = match.arg(modelVersion, c("v1beta2", "v1beta3"), several.ok = FALSE)
  modelType = match.arg(modelType, "text-bison-001", several.ok = FALSE)
}

#' @noRd
generateOutput = function(responseJSON) {
  # Check if a valid output is present in the response
  if (!is.null(responseJSON$candidates[[1]]$output)) {
    # Output the generated text
    return(as.character(responseJSON$candidates[[1]]$output))
  }
  # Check if there's an error in the response
  else if (!is.null(responseJSON$error)) {
    # Display a warning with the error message
    warning(responseJSON$error$message)
  }
  # Check if there's safety feedback in the response
  else if (!is.null(responseJSON$safetyFeedback)) {
    # Filter safety feedback with "MEDIUM" or "HIGH" probability
    riskyFeedback = responseJSON$safetyFeedback[sapply(responseJSON$safetyFeedback, function(f) {
      f$rating$probability %in% c("MEDIUM", "HIGH")
    })]

    # If there is risky feedback, display a warning
    if (length(riskyFeedback) > 0) {
      # Extract and display the categories with risky feedback
      categories = sapply(riskyFeedback, function(f) f$rating$category)
      warning('Unsafe inquery. Try to change harm thresholds. Your inquery violates safety setting(s):\n', paste(categories, collapse = ", "))
    }
    # If there's safety feedback but not risky, inform the user
    else {
      warning('Safe inquery, but there is safety feedback. Consider reviewing them.')
    }
  }
  # If there are no candidates, error, or safety feedback, provide general guidance
  else {
    warning('Unknown error. Cannot generate output. Please refer to:\n1) https://developers.generativeai.google/api\n2) https://palmr.r.ly.gd.edu.kg/')
  }
}

#' @noRd
generateSafetySettings = function(htUnspecified, htDerogatory, htToxicity,
                                  htViolence, htSexual, htMedical, htDangerous) {
  # Map the threshold values to category names
  thresholdMap = c("unsp" = "HARM_BLOCK_THRESHOLD_UNSPECIFIED",
                   "lowa" = "BLOCK_LOW_AND_ABOVE",
                   "meda" = "BLOCK_MEDIUM_AND_ABOVE",
                   "high" = "BLOCK_ONLY_HIGH",
                   "none" = "BLOCK_NONE")

  # Check validity of the threshold values
  stopifnot(all(htUnspecified %in% names(thresholdMap)),
            all(htDerogatory %in% names(thresholdMap)),
            all(htToxicity %in% names(thresholdMap)),
            all(htViolence %in% names(thresholdMap)),
            all(htSexual %in% names(thresholdMap)),
            all(htMedical %in% names(thresholdMap)),
            all(htDangerous %in% names(thresholdMap)))

  # Create safety settings list in one go
  safetySettings = list(
    list(category = "HARM_CATEGORY_UNSPECIFIED", threshold = thresholdMap[htUnspecified]),
    list(category = "HARM_CATEGORY_DEROGATORY", threshold = thresholdMap[htDerogatory]),
    list(category = "HARM_CATEGORY_TOXICITY", threshold = thresholdMap[htToxicity]),
    list(category = "HARM_CATEGORY_VIOLENCE", threshold = thresholdMap[htViolence]),
    list(category = "HARM_CATEGORY_SEXUAL", threshold = thresholdMap[htSexual]),
    list(category = "HARM_CATEGORY_MEDICAL", threshold = thresholdMap[htMedical]),
    list(category = "HARM_CATEGORY_DANGEROUS", threshold = thresholdMap[htDangerous])
  )

  return(safetySettings)
}
