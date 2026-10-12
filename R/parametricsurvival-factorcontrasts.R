# Factor coding belongs to an equation; raw category order is unchanged.
.sapFactorContrastSpecification <- function(row) {
  coding <- row[["factorContrast"]]
  reference <- if (coding == "treatment" || coding == "SAS") row[["factorReference"]] else NULL
  return(list(contrast = coding, referenceLevel = reference))
}
.sapFactorReference <- function(value, specification) {
  levels <- levels(value)
  if (is.null(specification)) return(levels[1L])
  coding <- specification[["contrast"]]
  if (!coding %in% c("treatment", "SAS")) return(levels[1L])
  return(specification[["referenceLevel"]])
}
.sapFactorContrastCall <- function(value, specification) {
  levels <- levels(value)
  # deparse substitutes <U+...> in non-UTF-8 locales. Embed Unicode codepoints
  # through a deterministic base call; deparse still escapes all ASCII labels.
  literal <- vapply(levels, function(level) {
    points <- utf8ToInt(enc2utf8(level))
    if (any(points > 127L))
      return(paste0("base::intToUtf8(", paste(deparse(as.numeric(points), width.cutoff = 500L), collapse = ""), ")"))
    return(paste(deparse(level, width.cutoff = 500L), collapse = ""))
  }, character(1))
  literal <- paste0("c(", paste(literal, collapse = ", "), ")")
  # Numeric contrast-column names avoid locale-dependent model.matrix label
  # conversion and collisions; raw levels stay embedded and in native xlev.
  count <- paste0("length(", literal, ")")
  coding <- specification[["contrast"]]
  expression <- switch(coding,
    treatment =, SAS = paste0("stats::contr.treatment(", count, ", base = ", match(.sapFactorReference(value, specification), levels), ")"),
    sum = paste0("stats::contr.sum(", count, ")"),
    helmert = paste0("stats::contr.helmert(", count, ")"),
    poly = paste0("stats::contr.poly(", count, ")"))
  return(expression)
}
.sapPredictorExpression <- function(variable, equation, dataset, weights) {
  coding <- equation[["factorContrasts"]][[variable]]
  if (is.null(coding)) return(.sapVariableExpression(variable, equation[["transformations"]][[variable]], dataset, weights))
  .sapCheckFactorData(dataset, equation)
  expression <- paste0("stats::C(", variable, ", contr = ", .sapFactorContrastCall(dataset[[variable]], coding), ")")
  return(paste(deparse(str2lang(expression), width.cutoff = 500L), collapse = ""))
}
.sapCheckFactorData <- function(dataset, equation) {
  for (variable in equation[["factorVariables"]]) {
    value <- dataset[[variable]]
    label <- jaspBase::decodeColNames(variable)
    if (nlevels(value) < 2L)
      .quitAnalysis(gettextf("Factor '%1$s' needs at least two observed levels in the fitting data.", label))
    reference <- .sapFactorReference(value, equation[["factorContrasts"]][[variable]])
    if (!reference %in% levels(value))
      .quitAnalysis(gettextf("The selected reference level '%1$s' for factor '%2$s' is absent from the fitting data. Choose a level present in this subgroup or change the subgroup selection.", reference, label))
  }
}
.sapFittedFactorReference <- function(fit, variable) {
  training <- attr(fit, "dataset")[[variable]]
  equations <- attr(fit, "equations")[["equations"]]
  for (detail in equations) {
    equation <- detail[["equation"]]
    if (variable %in% unlist(equation[["components"]], use.names = FALSE))
      return(.sapFactorReference(training, equation[["factorContrasts"]][[variable]]))
  }
  return(levels(training)[1L])
}
.sapFactorCoefficientTokens <- function(fit, equation) {
  dataset <- attr(fit, "dataset")
  replacements <- character(0)
  for (variable in names(equation[["factorContrasts"]])) {
    specification <- equation[["factorContrasts"]][[variable]]
    expression <- .sapPredictorExpression(variable, equation, dataset, rep(1, nrow(dataset)))
    coding <- specification[["contrast"]]
    matrix <- eval(str2lang(.sapFactorContrastCall(dataset[[variable]], specification)))
    columns <- colnames(matrix)
    if (is.null(columns)) columns <- as.character(seq_len(ncol(matrix)))
    label <- jaspBase::decodeColNames(variable)
    labels <- if (coding %in% c("treatment", "SAS"))
      vapply(columns, function(column) gettextf("%1$s = %2$s", label, levels(dataset[[variable]])[as.integer(column)]), character(1)) else
      vapply(columns, function(column) gettextf("%1$s contrast %2$s (%3$s)", label, column,
        switch(coding, sum = gettext("Sum"), helmert = gettext("Helmert"), poly = gettext("Orthogonal polynomial"))), character(1))
    replacements <- c(replacements, stats::setNames(labels, paste0(expression, columns)))
  }
  return(replacements)
}
.sapFactorDesignColumns <- function(fit, equation, columns) {
  formula <- stats::delete.response(stats::terms(equation[["formula"]]))
  labels <- attr(formula, "term.labels")
  factors <- names(equation[["equation"]][["factorContrasts"]])
  dataset <- attr(fit, "dataset")
  terms <- vapply(factors, function(variable) .sapPredictorExpression(variable, equation[["equation"]], dataset, rep(1, nrow(dataset))), character(1))
  selected <- which(equation[["slopeTerms"]] %in% match(terms, labels))
  return(intersect(columns, colnames(equation[["matrix"]])[-1L][selected]))
}
.sapFactorCodingNotes <- function(fit) {
  if (jaspBase::isTryError(fit)) return(character(0))
  notes <- character(0)
  variables <- character(0)
  for (detail in attr(fit, "equations")[["equations"]]) {
    for (variable in names(detail[["equation"]][["factorContrasts"]])) {
      specification <- detail[["equation"]][["factorContrasts"]][[variable]]
      coding <- specification[["contrast"]]
      label <- switch(coding, treatment = gettext("Treatment"), SAS = gettext("SAS"), sum = gettext("Sum"),
        helmert = gettext("Helmert"), poly = gettext("Orthogonal polynomial"))
      notes <- c(notes, if (coding %in% c("treatment", "SAS"))
        gettextf("Factor %1$s in %2$s, parameter %3$s: %4$s contrasts with reference level %5$s.",
          jaspBase::decodeColNames(variable), attr(fit, "modelTitle"), detail[["parameter"]], label,
          .sapFactorReference(attr(fit, "dataset")[[variable]], specification)) else
        gettextf("Factor %1$s in %2$s, parameter %3$s: %4$s contrasts in the observed category order.",
          jaspBase::decodeColNames(variable), attr(fit, "modelTitle"), detail[["parameter"]], label))
      variables <- union(variables, variable)
    }
  }
  for (variable in variables)
    notes <- c(notes, gettextf("Default prediction profile for %1$s in %2$s: %3$s.",
      jaspBase::decodeColNames(variable), attr(fit, "modelTitle"), .sapFittedFactorReference(fit, variable)))
  return(unique(notes))
}
.sapRetainedFactorContrastWarning <- function(warning, fit) {
  # model.frame drops C() attributes when applying xlev; native flexsurv then
  # rebuilds every equation with these exact stored contrasts. Keep all other
  # warnings, including warnings for ordinary factor terms.
  expressions <- as.character(unique(unlist(lapply(fit[["all.contrasts"]], names), use.names = FALSE)))
  expressions <- expressions[startsWith(expressions, "stats::C(")]
  if (!length(expressions)) return(FALSE)
  # Match the upstream stats translation key exactly, including its domain.
  return(conditionMessage(warning) %in% gettextf("contrasts dropped from factor %s", expressions, domain = "R-stats"))
}
