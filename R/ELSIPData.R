#' ELSIPData class
#'
#' @description
#' An object containing continuous/discrete numerical and/or categorical
#' multivariate data with associated classes and model training partitioning
#' information.
#' @importFrom R6 R6Class
ELSIPData <- R6Class("ELSIPData",
  private = list(
    .x = NULL,
    .y = NULL,
    .data_type = NULL
  ),
  active = list(
    #' @field x A data frame object where samples are in rows and features are
    #'   in columns. This is a read only value.
    x = function (value) {
      if (missing(value)) {
        private$.x
      } else {
        stop("`$x` is read only", call. = FALSE)
      }
    },
    #' @field y A factor vector indicating the observed class for each row.
    #'   This is a read only value.
    y = function (value) {
      if (missing(value)) {
        private$.y
      } else {
        stop("`$y` is read only", call. = FALSE)
      }
    },
    #' @importFrom checkmate assert assertDataFrame assertFactor checkFactor
    #'   checkNames checkNull
    #' @importFrom magrittr %>%
    #' @field data_type A factor vector indicating the model class for each
    #'   observation, if defined, or \code{NULL}.
    data_type = function (value) {
      if (missing(value)) {
        private$.data_type
      } else {
        assert(
          checkNull(value),
          assert(
            checkFactor(value, len = nrow(private$.x)),
            checkNames(names(value),
                       subset.of = c("Training", "Test", "Unknown"),
                       must.include = c("Training", "Test"))
          )
        )
        private$.data_type <- value
        self
      }
    }
  ),
  public = list(
    #' @description
    #' Create a new \code{ELSIPData} object.
    #' @param x A data frame object where samples are in rows and features are
    #'   in columns.
    #' @param y A factor vector indicating the observed class for each row. All
    #'   samples intended for model training and testing purposes should have
    #'   an assigned class. Any sample that does not have an assigned class
    #'   (i.e., \code{NA}) will be interpreted as an unknown sample and
    #'   classified after model training and testing.
    #' @param data_type An optional factor vector indicating the model class
    #'   for each observation. The allowed options are \code{"Training"},
    #'   \code{"Test"}, and \code{"Unknown"}.
    #' @importFrom checkmate assert assertDataFrame assertFactor checkFactor
    #'   checkNull
    #' @return A new \code{ELSIPData} object.
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
    #' @description
    #' Print an \code{ELSIPData} object.
    #' @param ... Ignored.
    #' @importFrom magrittr %>%
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
