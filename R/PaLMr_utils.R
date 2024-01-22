#' @noRd
checkModelSelection = function(modelVersion, modelType) {
  modelVersion = match.arg(modelVersion, c("v1beta2", "v1beta3"), several.ok = FALSE)
  modelType = match.arg(modelType, "text-bison-001", several.ok = FALSE)
}

#' @noRd
generateOutput = function(responseJSON) {
  if (!is.null(responseJSON$candidates[[1]]$output)) {
    return(as.character(responseJSON$candidates[[1]]$output))
  }
  else if (!is.null(responseJSON$error)) {
    warning(responseJSON$error$message)
  }
  else if (!is.null(responseJSON$safetyFeedback)) {
    riskyFeedback = responseJSON$safetyFeedback[sapply(responseJSON$safetyFeedback, function(f) {
      f$rating$probability %in% c("MEDIUM", "HIGH")
    })]

    if (length(riskyFeedback) > 0) {
      categories = sapply(riskyFeedback, function(f) f$rating$category)
      warning('Your prompt violates safety setting(s):\n', paste(categories, collapse = ", "))
    }
    else {
      warning('Safe inquery, but there is safety feedback. Consider reviewing them.')
    }
  }
}

#' @noRd
generateSafetySettings = function(htUnspecified, htDerogatory, htToxicity,
                                  htViolence, htSexual, htMedical, htDangerous) {
  thresholdMap = c("unsp" = "HARM_BLOCK_THRESHOLD_UNSPECIFIED",
                   "lowa" = "BLOCK_LOW_AND_ABOVE",
                   "meda" = "BLOCK_MEDIUM_AND_ABOVE",
                   "high" = "BLOCK_ONLY_HIGH",
                   "none" = "BLOCK_NONE")

  stopifnot(all(htUnspecified %in% names(thresholdMap)),
            all(htDerogatory %in% names(thresholdMap)),
            all(htToxicity %in% names(thresholdMap)),
            all(htViolence %in% names(thresholdMap)),
            all(htSexual %in% names(thresholdMap)),
            all(htMedical %in% names(thresholdMap)),
            all(htDangerous %in% names(thresholdMap)))

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
