#' Partition multivariate data
#'
#' Thus function allocates samples of a dataset into training and testing
#' samples depending on class. Samples with a \code{NA} class are assigned
#' to a separate "Unknown" partition.
#' @importFrom caret createDataPartition
#' @importFrom checkmate assertClass qassert
#' @param data an \code{\link{ELSIPData}} object, such as the output of
#'   \code{\link{classifyPrepare}}.
#' @param train_prop a numeric indicating what proportion of observations will
#'   constitute the training dataset. Must be a number between 0 and 1, not
#'   inclusive.
#' @param seed a random seed for initialising the creation of partitions.
#' @return an \code{\link{ELSIPData}} object.
#' @export
partition <- function (data, train_prop, seed = NULL) {
  assertClass(data, "ELSIPData")
  qassert(train_prop, "N1(0,1)")

  data_type <- rep("Training", nrow(data$x))
  data_type[is.na(data$y)] <- "Unknown"

  set.seed(seed)
  training_idx <- createDataPartition(data$y[data_type != "Unknown"],
                                      p = train_prop, list = FALSE)

  data_type[data_type != "Unknown"][-training_idx] <- "Test"
  data_type <- factor(data_type)

  data$data_type <- data_type
  data
}
