#' multiELSIPData class
#'
#' @name multiELSIPData
#' @importFrom R6 R6Class
#' @importFrom checkmate assertList assertTRUE
#' @importFrom gtools combinations
#' @field data Two or more objects of type \code{\link{ELSIPData}}.
NULL

multiELSIPData <- R6Class("multiELSIPData",
  private = list(
    .data = NULL
  ),
  active = list(
    data = function (value) {
      if (missing(value)) {
        private$.data
      } else {
        stop("`$data` is read only", call. = FALSE)
      }
    }
  ),
  public = list(
    # initialize = function(x, y, data_type = NULL, imputations = NULL) {
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
    }
  )
)
