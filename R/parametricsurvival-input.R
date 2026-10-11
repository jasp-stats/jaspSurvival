# Formula fields accept scalar expressions or lists of expressions. Keep parsing
# separate from the contextual range checks and messages at each call site.
.sapNumericExpression <- function(expression, multiple = FALSE, allowSpaces = FALSE) {

  if (!is.character(expression) || length(expression) != 1L)
    return(expression)

  text <- trimws(expression)
  if (multiple)
    text <- paste0("c(", gsub("[;|\r\n\t]+", ",", text), ")")
  evaluate <- function(text) {
    functions <- c("+", "-", "*", "/", "^", "%%", "%/%", "(", ":", "c", "seq", "seq_len", "seq_along", "rep",
      "abs", "sign", "sqrt", "exp", "expm1", "log", "log1p", "log2", "log10", "round", "signif", "floor", "ceiling", "trunc",
      "sin", "cos", "tan", "asin", "acos", "atan", "sinh", "cosh", "tanh", "min", "max", "sum", "prod", "mean", "median",
      "qnorm", "pnorm", "qchisq", "pchisq", "qt", "pt", "qf", "pf", "qgamma", "pgamma", "qbeta", "pbeta", "qweibull", "pweibull")
    valid <- function(node) {
      if (is.numeric(node) || is.logical(node) || is.null(node)) return(TRUE)
      if (is.name(node)) return(as.character(node) %in% c("pi", "Inf", "NaN", "NA"))
      if (!is.call(node)) return(FALSE)
      head <- node[[1L]]
      allowed <- if (is.name(head)) as.character(head) %in% functions else
        is.call(head) && identical(head[[1L]], as.name("::")) && is.name(head[[2L]]) && is.name(head[[3L]]) &&
        as.character(head[[2L]]) %in% c("base", "stats") && as.character(head[[3L]]) %in% functions
      return(allowed && all(vapply(as.list(node)[-1L], valid, logical(1))))
    }
    parsed <- parse(text = text)
    if (length(parsed) != 1L || !valid(parsed[[1L]]))
      stop(gettext("Expression inputs allow numeric arithmetic and mathematical functions only."))
    # Resolve only the allowed numeric functions; global/user functions cannot
    # shadow them, and common stats expressions need no namespace prefix.
    numericFunctions <- stats::setNames(lapply(functions, function(name) {
      if (exists(name, envir = baseenv(), inherits = FALSE)) get(name, envir = baseenv()) else
        getExportedValue("stats", name)
    }), functions)
    return(eval(parsed, envir = list2env(numericFunctions, parent = baseenv())))
  }
  value <- try(evaluate(text), silent = TRUE)

  # Coefficient vectors also accept whitespace separators; try them only after
  # normal parsing, so spaces inside an expression keep their usual meaning.
  if (multiple && allowSpaces && inherits(value, "try-error")) {
    text  <- gsub("[;|\r\n\t]+", ",", trimws(expression))
    text  <- paste(strsplit(text, "[[:space:]]+")[[1L]], collapse = ",")
    value <- try(evaluate(paste0("c(", text, ")")), silent = TRUE)
  }

  return(value)
}

.sapFiniteNumeric <- function(value, expectedLength = NULL) {

  valid <- is.numeric(value) && !is.complex(value) && length(value) > 0L && all(is.finite(value))
  if (!is.null(expectedLength))
    valid <- valid && length(value) == expectedLength

  return(valid)
}
