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
#' @import dplyr
#' @importFrom purrr pmap
#' @importFrom bulletxtrctr sig_get_peaks
#' @export
#' @examples
#' data("sig")
#' crosscut_slice(sig) # Slice whole data frame
#' crosscut_slice(sig$sig) # Slice signature only
crosscut_slice <- function(x, cycle_type = 'full', ...) {
  if (!is.numeric(x)) {
    assertthat::assert_that(
      assertthat::has_name(x, "sig"),
      msg = paste0("sig must either be numeric or a list or ",
                   "data.frame containing a column named 'sig'"))
    sig <- x$sig
  } else {
    sig <- x
  }

  assertthat::assert_that(
    cycle_type %in% c("half", "full"),
    msg = "cycle_type must be one of 'half' or 'full'")
  assertthat::assert_that(is.numeric(sig))

  # Get residual structure in one object, peak height in another; then combine
  # peaks <- bulletxtrctr::sig_get_peaks(sig, ...)
  med_val <- stats::median(sig, na.rm = T)

  # Convert to std scale - median
  sigmed <- sig - med_val
  signs <- sign(sigmed)
  neg <- which(signs == -1)
  pos <- which(signs == 1 | signs == 0)
  n2p <- intersect(neg + 1, pos) # get neg-to-pos transition
  p2n <- intersect(pos + 1, neg) # get pos-to-neg transition

  # Index
  cycle_type <- ifelse(n2p[1] > p2n[1], "pn", "np")
  idx <- if (cycle_type == "pn") {
    n2p
  } else {
    p2n
  }

  sig_idx <- data_frame(
    chunk = 0:length(idx),
    start = c(1, idx),
    end = c(idx - 1, length(sig)),
    type = c("start", rep(cycle_type, length(idx) - 1), "end")
  )

  chunks <- purrr::pmap(sig_idx, function(chunk, start, end, type) {
    z <- if (is.data.frame(x)) {
      x[start:end,]
    } else {
      data_frame(
        x = start:end,
        sig = sig[start:end]
      )
    }

    if (type == "end") {
      z$x <- -z$x
      z$sig <- rev(z$sig)
      z <- arrange(z, x)
    }

    z %>%
      mutate(
        sig = sig - med_val,
        chunk = chunk,
        type = type
      ) %>%
      mutate(sig = ifelse(type == "np", -sig, sig),
             type = ifelse(type == "np", "pn", type),
             type = ifelse(type == "pn", "pn", "boundary"))
  })

  chunks
}
