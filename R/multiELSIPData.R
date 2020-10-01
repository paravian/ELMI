#' multiELSIPData class
#'
#' @description
#' An object containing multiple \code{\link{ELSIPData}} objects derived from
#' the a single data source.
#' @importFrom R6 R6Class
multiELSIPData <- R6Class("multiELSIPData",
  private = list(
    .data = NULL
  ),
  active = list(
    #' @field data A list of two or more objects of type
    #'   \code{\link{ELSIPData}}. This is a read only value.
    data = function (value) {
      if (missing(value)) {
        private$.data
      } else {
        stop("`$data` is read only", call. = FALSE)
      }
    }
  ),
  public = list(
    #' @description
    #' Create a new \code{multiELSIPData} object.
    #' @param ... One or more \code{\link{ELSIPData}} objects. The \code{x}
    #'   value of each object must have equal dimensions and contain the same
    #'   missing or imputed data pattern. The \code{y} and \code{data_type}
    #'   values of each object must also be identical.
    #' @importFrom checkmate assertList assertTRUE
    #' @importFrom gtools combinations
    #' @return A new \code{multiELSIPData} object.
    initialize = function(...) {
      data <- list(...)
      assertList(data, min.len = 2, types = "ELSIPData")

      # Pairwise-check the objects of each ELSIPData object
      if (length(data) > 1) {
        pairs <- combinations(length(data), 2)
        cell_pattern <- NULL

        for (pair_row in nrow(pairs)) {
          pair <- pairs[pair_row,]
          assertTRUE(all(dim(data[[pair[1]]]$x == dim(data[[pair[2]]]$x))))
          assertTRUE(all(data[[pair[1]]]$y == dim(data[[pair[2]]]$y)))
          assertTRUE(all(data[[pair[1]]]$data_type == dim(data[[pair[2]]]$data_type)))
          equal_cells <- data[[pair[1]]]$x == data[[pair[2]]]$x
          if (is.null(cell_pattern)) {
            cell_pattern <- equal_cells
          } else {
            assertTRUE(all(equal_cells == cell_pattern))
          }
        }
      }

      private$.data <- data
    },
    #' @description
    #' Print a \code{multiELSIPData} object.
    #' @param ... Ignored.
    #' @importFrom magrittr %>%
    print = function (...) {
      cat("Multiple ELSIP data:\n\n")
      cat(length(private$.data), "datasets;",
          nrow(private$.data[[1]]$x), "observations of",
          ncol(private$.data[[1]]$x), "variables\n")
      if (!is.null(private$.data[[1]]$data_type)) {
        types <- tapply(private$.data[[1]]$data_type,
                        private$.data[[1]]$data_type, length) %>%
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
