#' Compare ELSIP parameter combinations
#'
#' @importFrom checkmate asInteger assertNames checkDataFrame checkCharacter checkChoice checkInteger checkSubset
#' @importFrom caret confusionMatrix
#' @param data a data frame containing observations
#' @param grid a data frame containing parameter combinations for \code{ELSIP}.
#' @param label the column name in \code{data} to use as the classification
#'   outcome.
#' @param classifiers a named list of classifiers, with parameters for each
#'   specified according to the scheme outlined in \code{\link{ELSIP}}.
#' @param replications the number of times to run the analysis.
#' @export
multi_ELSIP <- function (grid, data, label, classifiers, replications = 1) {
  checkDataFrame(data)
  checkDataFrame(grid)
  checkCharacter(label, len = 1)
  checkChoice(label, names(data))
  assertNames(names(grid), subset.of = c("subsample", "imp_n",
    "imp_method", "train_prop", "classifier", "pairwise_stack",
    "meta_classifier", "ensemble_single", "seed", "verbose"))
  assertNames(unique(c(grid$classifier, grid$meta_classifier)),
              subset.of = names(classifiers))
  replications <- asInteger(replications)
  checkInteger(replications, len = 1, lower = 1)

  cl <- match.call()
  label_col <- which(names(data) == label)

  # Remove any conflicting entries
  if (all(c("imp_method", "imp_n") %in% names(grid))) {
    conflict_mean <- grid$imp_method == "mean" & grid$imp_n > 1
    if (any(conflict_mean)) {
      message("Warning: removing parameter combinations where imp_method is 'mean' and imp_n > 1")
      grid <- grid[!conflict_mean,]
    }
  }
  if (all(c("pairwise_stack", "imp_n") %in% names(grid))) {
    conflict_stack <- grid$pairwise_stack & grid$imp_n == 1
    if (any(conflict_stack)) {
      message("Warning: removing parameter combinations where pairwise stack is TRUE and imp_n is 1")
      grid <- grid[!conflict_stack,]
    }
  }

  # Extract variable parameters from grid
  res_grid_train <- grid[,apply(grid, 2, function (s) length(unique(s)) > 1)]
  res_grid_test <- res_grid_train

  ELSIP_rep <- function (grid, data, classifiers) {
    cn <- c("classifier", "meta_classifier")
    cm_names <- c("Accuracy", "Kappa")
    grid_res <- list(
      Training = data.frame(
        matrix(ncol = 2, nrow = 0, dimnames = list(NULL, cm_names))),
      Test = data.frame(
        matrix(ncol = 2, nrow = 0, dimnames = list(NULL, cm_names))))
    for (row in 1:nrow(grid)) {
      # Convert grid row to a list of arguments for ELSIP; copy data and
      # classification objects over
      args <- as.list(grid[row,])
      args$x <- data[,-label_col]
      args$y <- data[,label_col]
      args[cn] <- lapply(cn, function (c) classifiers[[args[[c]]]])
      # Call ELSIP
      rep_res <- do.call(ELSIP, args)
      # Extract prediction accuracies
      if ("pred" %in% names(rep_res)) {
        rep_res <- split(rep_res$pred, rep_res$pred$dataType)
      } else {
        rep_res <- split(rep_res$ens_pred, rep_res$ens_pred$dataType)
      }
      for (n in c("Training", "Test")) {
        obj <- confusionMatrix(rep_res[[n]]$pred, rep_res[[n]]$obs)
        grid_res[[n]][row,] <- obj$overall[cm_names]
      }
    }
    grid_res
  }

  res <- replicate(replications, ELSIP_rep(grid, data, classifiers))

  list(call = cl, grid = grid, runs = res)
}
