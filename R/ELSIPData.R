#' ELSIPData class
#'
#' @name ELSIPData
#' @importFrom R6 R6Class
#' @importFrom checkmate assert assertDataFrame assertFactor checkFactor
#'   checkNull
#' @importFrom magrittr %>%
#' @field x a data frame object where samples are in rows and features are in
#'   columns.
#' @field y a factor vector indicating the observed class for each row. All
#'   samples intended for model training and testing purposes should have an
#'   assigned class. Any sample that does not have an assigned class
#'   (i.e., \code{NA}) will be interpreted as an unknown sample and classified
#'   after model training and testing.
#' @field data_type an optional factor vector indicating the model class for
#'   each observation. The allowed options are \code{"Training"},
#'   \code{"Test"}, and \code{"Unknown"}.
NULL

ELSIPData <- R6Class("ELSIPData",
  private = list(
    .x = NULL,
    .y = NULL,
    .data_type = NULL
  ),
  active = list(
    x = function (value) {
      if (missing(value)) {
        private$.x
      } else {
        stop("`$x` is read only", call. = FALSE)
      }
    },
    y = function (value) {
      if (missing(value)) {
        private$.y
      } else {
        stop("`$y` is read only", call. = FALSE)
      }
    },
    data_type = function (value) {
      if (missing(value)) {
        private$.data_type
      } else {
        assert(
          checkNull(value),
          checkFactor(value, len = nrow(private$.x),
                      levels = c("Training", "Test", "Unknown"))
        )
        private$.data_type <- value
        self
      }
    }
  ),
  public = list(
    initialize = function(x, y, data_type = NULL) {
      assertDataFrame(x, min.rows = 1, min.cols = 1, all.missing = FALSE,
                      col.names = "named")
      assertFactor(y, len = nrow(x), min.levels = 2)
      assert(
        checkNull(data_type),
        checkFactor(data_type, len = nrow(x), levels = c("Training", "Test", "Unknown"))
      )

      private$.x <- x
      private$.y <- y
      self$data_type <- data_type
    },
    print = function (...) {
      cat("ELSIP data:\n\n")
      cat(nrow(private$.x), "observations of", ncol(private$.x), "variables\n")
      if (!is.null(self$data_type)) {
        types <- tapply(private$.data_type, private$.data_type, length) %>%
          sort(decreasing = T)
        for (n in names(types)) {
          cat(types[n], casefold(n), sep = " ")
          idx <- which(n == names(types))
          if (idx < length(types)) {
            cat(ifelse(idx < length(types) - 1, ", ", " and "))
          }
        }
        cat(" samples\n")
      }
      invisible(self)
    }
  )
)
