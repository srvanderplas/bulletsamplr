#' Slice signature into cycles
#'
#' @param x a numeric vector with signature information
#' @param cycle_type one of 'half' or 'full'. Half-cyles start at the default
#'          value, reach a local max or min, and then return to the default
#'          value. Full cycles start at the default value, reach a local max or
#'          min, return to the default, reach a local min or max, and then
#'          return to the default value.
#' @param ... additional parameters passed to bulletxtrctr::sig_get_peaks
#' @return a list of vectors with signature cycles
#' @importFrom assertthat assert_that has_name
#' @importFrom dplyr '%>%'
#' @importFrom bulletxtrctr sig_get_peaks
#' @examples
#' data("sig")
crosscut_slice <- function(x, cycle_type = 'full', ...) {
  if (!is.numeric(x)) {
    assert_that(has_name(x, "sig"),
                msg = paste0("sig must either be numeric or a list or ",
                             "data.frame containing a column named 'sig'"))
    sig <- x$sig
  } else {
    sig <- x
  }

  assert_that(cycle_type %in% c("half", "full"),
              msg = "cycle_type must be one of 'half' or 'full'")
  assert_that(is.numeric(sig))

  # Need to figure out how to deal with small peaks and hugely lopsided peaks
  # that may not reach the critical value before the next extrema.

  # Get residual structure in one object, peak height in another; then combine
  peaks <- bulletxtrctr::sig_get_peaks(sig, ...)
  med_val <- stats::median(sig)
}
