#' Impute multivariate data
#'
#' @importFrom caret createDataPartition
#' @importFrom checkmate asInt assertClass assertChoice assertInt assertLogical
#'   assertSubset checkNull checkIntegerish qassert
#' @importFrom Hmisc impute
#' @importFrom magrittr %>%
#' @importFrom mice mice complete
#' @importFrom missMDA estim_ncpPCA MIPCA
#' @param x object that inherits \code{\link{ELSIPData}}, such as the output
#'   of \code{\link{classifyPrepare}}.
#' @param method a single length character indicating the imputation method to
#'   use. Options are "\code{mean}" (default), "\code{median}",
#'   "\code{mice_pmm}", "\code{mice_rf}" and "\code{mipca}".
#' @param n a single length numeric or integer greater than 0 indicating the
#'   number of imputations to generate. Note that multiple imputations (i.e.,
#'   values of \code{n} greater than 1, cannot be generated when \code{method}
#'   is "\code{mean}" or "\code{median}".
#' @param verbose a logical indicating wether to output information to the
#'   console during multiple imputation.
#' @param ... ignored
#' @param seed an integer value to initialise the multiple imputation.
#' @method impute ELSIPData
#' @export
impute.ELSIPData <- function (x, method = "mean", n = 1, seed = NULL,
                              verbose = FALSE, ...) {
  assertClass(data, "ELSIPData")
  assertChoice(method, c("mean", "median", "mice_pmm", "mice_rf", "mipca"))
  n <- asInt(n)
  assertInt(n, lower = 1)
  if (n > 1 & method %in% c("mean", "median")) {
    stop("Cannot generate ", n, " imputations using ", method, " method")
  }
  assert(
    checkNull(seed),
    checkIntegerish(seed)
  )
  assertLogical(verbose)

  if (length(grep("mice", method))) {
    method <- gsub("mice_", "", method)
    imp <- mice(data$x, m = n, method = method,
                seed = ifelse(is.null(seed), NA, seed),
                printFlag = verbose) %>%
      complete(action = "all")
    names(imp) <- NULL
  } else {
    assertSubset(sapply(data$x, class), c("numeric"))
    if (method == "mipca") {
      nb <- estim_ncpPCA(data$x)
      imp <- MIPCA(data$x, nb$ncp, method.mi = "Bayes", nboot = n)$res.MI
    } else if (method %in% c("mean", "median")) {
      imp <- sapply(data$x, function (col) Hmisc::impute(col, get(method))) %>%
        as.data.frame() %>%
        list()
    }
  }

  data <- lapply(imp, function (i) {
    ELSIPData$new(x = i, y = data$y, data_type = data$data_type)
  })
  if (length(data) > 1) {
    res <- do.call(multiELSIPData$new, data)
  } else {
    res <- data[[1]]
  }
  res
}
