#' Subsample multivariate datasets
#'
#' @importFrom caret downSample upSample
#' @importFrom checkmate assertCharacter assertClass checkIntegerish checkNull
#' @param data an object that inherits \code{\link{ELSIPData}}, such as the output
#'   of \code{\link{classifyPrepare}}.
#' @param type a single length character indicating the type of subsampling to
#'   perform.
#' @param seed a random seed for initializing the subsampling.
#' @return An \code{\link{ELSIPData}} object.
#' @export
subsample <- function (data, type = c("up", "down"), seed = NULL) {
  assertClass(data, "ELSIPData")
  assertCharacter(type, len = 1, any.missing = FALSE)
  assert(
    checkNull(seed),
    checkIntegerish(seed)
  )

  unknowns <- is.na(data$y)
  ss_args <- list(x = data$x[!unknowns,],
                  y = data$y[!unknowns])
  type <- match.arg(type)
  if (type == "up") {
    ss_fn <- upSample
  } else if (type == "down") {
    ss_fn <- downSample
  }
  set.seed(seed = seed)
  ss <- do.call(ss_fn, ss_args)
  ss <- merge(ss, data$x[unknowns,], all = TRUE, sort = FALSE)

  ELSIPData$new(ss[,names(ss) != "Class"], ss$Class, data$data_type)
}
