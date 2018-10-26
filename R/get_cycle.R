#' Slice signature into chunks consisting of N cycles
#'
#' @param x a numeric vector with signature information
#' @param ncycle number of cycles per chunk
#' @return a list of vectors with signature cycles
#' @importFrom assertthat assert_that has_name
#' @import dplyr
#' @importFrom purrr pmap
#' @importFrom bulletxtrctr sig_get_peaks
#' @importFrom stats median
#' @export
#' @examples
#' data("sig")
#' crosscut_slice(sig) # Slice whole data frame
#' crosscut_slice(sig$sig, ncycle = 3) # Slice signature only, using 3 cycles/chunk
crosscut_slice <- function(x, ncycle = 1) {
  .type <- NULL

  if (!is.numeric(x)) {
    assertthat::assert_that(
      assertthat::has_name(x, "sig"),
      msg = paste0("sig must either be numeric or a list or ",
                   "data.frame containing a column named 'sig'"))
    sig <- x$sig
    sig.n <- x
  } else {
    sig <- x
    sig.n <- x
  }

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
  cycle <- ifelse(n2p[1] > p2n[1], "pn", "np")
  idx <- if (cycle == "pn") {
    n2p
  } else {
    p2n
  }

  sig_idx <- data_frame(
    map_cycle = 0:length(idx),
    map_start = c(1, idx),
    map_end = c(idx - 1, length(sig)),
    map_type = c("start", rep(cycle, length(idx) - 1), "end"),
    map_chunk = ceiling(map_cycle/ncycle)
  ) %>%
    group_by(map_chunk) %>%
    summarize(map_start = min(map_start),
              map_end = max(map_end),
              map_type = ifelse(n_distinct(map_type) == 1, map_type[1],
                                ifelse("end" %in% map_type, "end", "start")))

  chunks <- purrr::pmap(sig_idx, chunk_slice, sig = sig.n)

  chunks
}

chunk_slice <- function(sig, map_chunk, map_start, map_end, map_type) {
  z <- if (is.data.frame(sig)) {
    sig[map_start:map_end,]
  } else {
    data_frame(
      x = map_start:map_end,
      sig = sig[map_start:map_end]
    )
  }
  z$.type <- map_type

  med_val <- stats::median(z$sig, na.rm = T)

  if (map_type == "end") {
    # z$x <- -z$x + max(z$x)
    z$sig <- rev(z$sig)
  }

  z <- z %>%
    mutate(
      x = x - min(x, na.rm = T),
      sig = sig - med_val,
      .chunk = map_chunk
    )

  # ensure chunks end on a negative note
  last <- max(which(!is.na(z$sig)))
  sgn <- z$sig[(last - 1):last] %>% sign() %>% mean(na.rm = T) %>% sign()
  if ((map_type == "start" | map_type == "end" | map_type == "boundary") &
      sgn > 0) {
    z$sig <- -z$sig
  }

  z %>%
    mutate(sig = ifelse(.type == "np", -sig, sig),
           .type = ifelse(.type == "np", "pn", .type),
           .type = ifelse(.type == "pn", "pn", "boundary")) %>%
    mutate(x = ifelse(.type == 'boundary', x - max(x, na.rm = T), x))
}
