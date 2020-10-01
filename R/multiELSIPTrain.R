#' multiELSIPTrain class
#'
#' @name multiELSIPTrain
#' @importFrom R6 R6Class
multiELSIPTrain <- R6Class("multiELSIPTrain",
  private = list(
    .train = NULL
  ),
  active = list(
    #' @field train A list of two or more objects of type
    #'   \code{\link{ELSIPTrain}}. This is a read only value.
    train = function (value) {
      if (missing(value)) {
        private$.train
      } else {
        stop("`$train` is read only", call. = FALSE)
      }
    }
  ),
  public = list(
    #' @description
    #' Create a new \code{multiELSIPTrain} object.
    #' @param ... One or more \code{\link{ELSIPTrain}} objects. The training
    #'   data of each object must contain the same number of rows and identical
    #'   outcome variables.
    #' @importFrom checkmate assertList assertNames assertTRUE
    #' @return A \code{multiELSIPTrain} object.
    initialize = function (...) {
      train <- list(...)
      assertList(train, min.len = 2, types = "ELSIPTrain")

      # Pairwise-check the training data of each ELSIPTrain object
      if (length(train) > 1) {
        pairs <- combinations(length(train), 2)
        cell_pattern <- NULL
        for (pair_row in nrow(pairs)) {
          pair <- pairs[pair_row,]
          assertTRUE(nrow(train[[pair[1]]]$model$trainingData) ==
                     nrow(train[[pair[2]]]$model$trainingData))
          assertNames(levels(train[[pair[1]]]$model$trainingData$.outcome),
                      permutation.of = levels(train[[pair[2]]]$model$trainingData$.outcome))
          assertTRUE(all(train[[pair[1]]]$model$trainingData$.outcome ==
                         train[[pair[2]]]$model$trainingData$.outcome))
        }
      }

      private$.train <- train
    },
    #' @description
    #' Print a \code{multiELSIPTrain} object.
    #' @param ... Ignored.
    print = function (...) {
      cat("Multi ELSIP train:\n\n")

      cat(length(private$.model), "trained models;")
      # cat(private$.model$modelInfo$label, " (\"", private$.model$method, "\")", sep = "")
    }
  )
)
