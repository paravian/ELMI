#' Summarize ELSIPTrain object
#'
#' @importFrom caret confusionMatrix extractPrediction
#' @param object An \code{\link{ELSIPTrain}} object.
#' @param ... Optional arguments (currently none.)
#' @method summary ELSIPTrain
#' @export
summary.ELSIPTrain <- function (object, ...) {
  dt <- levels(object$data$data_type)

  pred_args <- list(models = list(object$model))
  if ("Test" %in% dt) {
    pred_args$testX <- object$data$x[object$data$data_type == "Test",]
    pred_args$testY <- object$data$y[object$data$data_type == "Test"]
  }
  if ("Unknown" %in% dt) {
    pred_args$unkX <- object$data$x[object$data$data_type == "Unknown",]
    dt <- dt[!dt == "Unknown"]
  }

  preds <- do.call(extractPrediction, pred_args)

  mtxs <- lapply(dt, function (d)
    confusionMatrix(data = preds$pred[preds$dataType == d],
                    reference = preds$obs[preds$dataType == d]))

  names(mtxs) <- tolower(dt)
  class(mtxs) <- "summary.ELSIPTrain"
  mtxs
}
