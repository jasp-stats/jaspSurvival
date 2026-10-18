# Native inference only; saved fits from earlier information policies must refit.
.sapInformationPolicyVersion <- "native-design-standardization-3"

.sapInformationRecord <- function(fit, method, outcome, provenance) {

  attr(fit, "informationPolicyVersion") <- .sapInformationPolicyVersion
  attr(fit, "informationMethod") <- method
  attr(fit, "informationOutcome") <- outcome
  attr(fit, "informationProvenance") <- provenance
  return(fit)
}

.sapInformationNote <- function(fit) {

  messages <- attr(fit, "informationProvenance", exact = TRUE)[["nativeWarnings"]]
  return(unique(c(attr(fit, "inferenceUnavailable", exact = TRUE), messages)))
}

.sapAssessInformation <- function(fit, censoringType) {

  if (identical(attr(fit, "informationPolicyVersion", exact = TRUE), .sapInformationPolicyVersion))
    return(fit)
  return(.sapInformationRecord(fit, "native", if (inherits(fit, "try-error")) "retainedError" else
    if (fit[["npars"]] == 0L) "fullyFixed" else "nativeAvailable", NULL))
}
