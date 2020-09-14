#' Prepare classification data
#'
#' This function processes datasets containing numerical and/or discrete value
#' datasets into a format suitable for analysis using \code{\link{ELSIP}} and
#' \code{\link{multi_ELSIP}}, and using transformations specifically suited to
#' each type of data. Presently, three types of data types are supported by
#' \code{classifyPrepare}: continuous, ratio, and discrete. The variables
#' desired are specified according to the named columns in \code{data} for
#' any of the three aforementioned data types. Named columns in
#' \code{discrete}, \code{continuous} and \code{ratios} will appear in the
#' returned data frame, with the following transformations applied:
#' \itemize{
#'   \item{\code{discrete} variables are included as is, but converted to
#'   \code{factor}s;}
#'   \item{\code{continuous} variables are log-10 transformed; and}
#'   \item{\code{ratios} variables are logit transformed. Note that the upper
#'   and lower bounds of the logit transformation are set to minus and plus 1
#'   of the lowest and highest value of the ratio variable, respectively, to
#'   avoid returning \code{Inf}s.}
#' }
#' @importFrom checkmate assert checkChoice checkDataFrame checkCharacter checkSubset
#' @importFrom gtools logit
#' @param data a \code{data.frame} object containing observations.
#' @param label a character indicating the column in \code{data} to use as the
#'   classification outcome.
#' @param discrete a character vector of columns representing discrete
#'   variables.
#' @param continuous a character vector of columns representing continuous
#'   variables.
#' @param ratios a character vector of columns representing ratio variables.
#' @return a \code{data.frame} object containing the outcome variable as a
#'   factor and the transformed variables from \code{data}.
#' @export
classifyPrepare <- function(data, label = character(), discrete = list(),
                     continuous = list(), ratios = list()) {
  checkDataFrame(data, min.rows = 1, min.cols = 1)
  checkChoice(label, names(data))

  assert(
    checkCharacter(discrete, min.len = 1),
    checkCharacter(continuous, min.len = 1),
    checkCharacter(ratios, min.len = 1)
  )
  checkSubset(discrete, names(data))
  checkSubset(continuous, names(data))
  checkSubset(ratios, names(data))

  for (d in discrete) {
    data[,d] <- as.factor(data[,d])
  }
  for (c in c(continuous, ratios)) {
    data[,c] <- as.numeric(data[,c])
  }
  data[,label] <- as.factor(data[,label])

  usable_cols <- c(which(names(data) == label))

  if (length(discrete)) {
    if (any(!discrete %in% names(data))) {
      stop("Could not find the following discrete variables: ",
           paste(discrete[!discrete %in% names(data)]), collapse = ", ")
    }
    discrete_cols <- which(names(data) %in% discrete)

    # Check for any variables of factor or character type and convert
    var_classes <- sapply(data, class)[discrete_cols]
    var_chars <- names(var_classes[var_classes %in% c("character", "numeric")])

    for (var_char in var_chars) {
      data[,var_char] <- as.factor(data[,var_char])
    }
    var_classes[which(var_chars == names(var_classes))] <- "factor"

    var_factors <- names(var_classes[var_classes == "factor"])

    for (var_fact in var_factors) {
      data[,var_fact] <- as.factor(data[,var_fact])
    }
    usable_cols <- c(usable_cols, discrete_cols)
  }

  # Transform continuous variables
  if (length(continuous)) {
    if (any(!continuous %in% names(data))) {
      stop("Could not find the following continuous variables: ",
           paste(continuous[!continuous %in% names(data)]), collapse = ", ")
    }
    cont_cols <- which(names(data) %in% continuous)

    data[cont_cols] <- lapply(data[cont_cols], log10)
    usable_cols <- c(usable_cols, cont_cols)
  }

  # Transform ratio variables
  if (length(ratios)) {
    if (any(!ratios %in% names(data))) {
      stop("Could not find the following ratio variables: ",
           paste(ratios[!ratios %in% names(data)]), collapse = ", ")
    }
    ratio_cols <- which(names(data) %in% ratios)

    data[ratio_cols] <- lapply(data[ratio_cols],
                               function (r) logit(r, min(r, na.rm = T) - 1, max(r, na.rm = T) + 1))
    usable_cols <- c(usable_cols, ratio_cols)
  }

  return(data[,usable_cols])
}
