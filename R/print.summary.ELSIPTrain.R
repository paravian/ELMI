#' Summarize ELSIPTrain object
#'
#' @importFrom caret confusionMatrix extractPrediction
#' @param x An \code{\link{ELSIPTrain}} object.
#' @param ... Optional arguments (currently none.)
#' @method print summary.ELSIPTrain
#' @export
print.summary.ELSIPTrain <- function (x, ...) {
  cat("ELSIP Train:\n\n")
  v <- sapply(x, function (o) round(o$overall, 4))
  v <- t(v[,order(colnames(v), decreasing = TRUE)])
  print(v)
}
