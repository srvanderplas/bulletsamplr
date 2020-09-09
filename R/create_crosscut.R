slice_df_check <- function(df) {
  if (is.data.frame(df)) {
    assertthat::assert_that(
      assertthat::has_name(df, "id"),
      assertthat::has_name(df, "sig"),
      assertthat::has_name(df, "type"))
  }

  if ("tbl_sql" %in% class(df) | "tbl_lazy" %in% class(df)) {
    assertthat::assert_that("id" %in% df$ops$vars,
                            "type" %in% df$ops$vars,
                            "sig" %in% df$ops$vars)
  }
}

slice_sum_df_check <- function(df) {
  if (is.data.frame(df)) {
    assertthat::assert_that(
      assertthat::has_name(df, "id"),
      assertthat::has_name(df, "type"),
      assertthat::has_name(df, "n"),
      assertthat::has_name(df, "not_na"))
  }

  if ("tbl_sql" %in% class(df) | "tbl_lazy" %in% class(df)) {
    assertthat::assert_that("id" %in% df$ops$vars,
                            "type" %in% df$ops$vars,
                            "n" %in% df$ops$vars,
                            "not_na" %in% df$ops$vars)
  }
}

#' Pad or trim the data frame to the specified number of rows
#'
#' @param full_df full data frame to pad/trim to the correct length
#' @param len desired length of the data frame
#' @return a data frame with len rows
#' @import dplyr
#' @importFrom assertthat assert_that has_name
df_fix_length <- function(full_df, len) {
  assertthat::assert_that(
    assertthat::has_name(full_df, "id"),
    assertthat::has_name(full_df, "type"),
    assertthat::has_name(full_df, "sig")
  )
  assertthat::assert_that(is.numeric(len), len %% 1 == 0)

  if (nrow(full_df) == len) {
    return(full_df)
  } else if (nrow(full_df) > len) {
    trim_front <- sample(1:(nrow(full_df) - len), 1)
    trim_back <- trim_front + len - 1
    return(
      dplyr::filter(full_df, dplyr::row_number() >= trim_front,
                    dplyr::row_number() <= trim_back)
    )
  } else {
    # full_df is too short
    padna <- len - nrow(full_df)
    pad_front <- sample(1:padna, 1)
    pad_back <- padna - pad_front
    blank_row <- tibble::tibble(type = "fill")
    return(
      dplyr::bind_rows(lapply(1:pad_front, function(i) blank_row),
                       full_df, lapply(1:pad_back, function(i) blank_row))
    )
  }
}

#' Draw cycles for a resampled bullet sequence
#'
#' This function assembles cycles to form a sequence of approximately length
#' `len` using a summary table with the following information:
#' \enumerate{
#' \item id, a variable uniquely identifying the cycle,
#' \item type, a character or factor variable with values `boundary_indicator`
#'         and some other value indicating a complete cycle
#' \item n, the length of the sequence,
#' \item not_na, the number of non NA values in the sequence
#' }
#'
#' @param len target sequence length
#' @param tab summary table
#' @param boundary_indicator character indicating an incomplete cycle
#' @return a data frame of sampled responses, in order
#' @importFrom assertthat assert_that has_name
#' @import dplyr
cycle_draw <- function(len, tab, boundary_indicator = "boundary") {
  # Cran check fix warnings - issues with pipe identification of variables
  . <- .idx <- .type <- type <- cum_length <- NULL

  # Useful for debugging purposes - shouldn't ever be called
  if (!exists("boundary_indicator")) { boundary_indicator <- "boundary" }
  if (!exists("len")) { len <- 3000 }

  assertthat::assert_that(is.numeric(len))
  slice_sum_df_check(tab)

  if (!"not_na" %in% names(tab)) {
    tab$not_na <- tab$n
  }

  # Choose initial sign
  init_sign <- sample(c(-1, 1), 1)

  boundaries <- tab %>%
    dplyr::filter(type %in% boundary_indicator) %>%
    collect()

  boundary_sample <- boundaries %>%
    dplyr::sample_n(size = 2, replace = T)

  # Choose a starting chunk
  start_chunk <- boundary_sample[1,] %>%
    mutate(.idx = 0, rev = F, sign = init_sign)

  # Choose an ending chunk
  end_chunk <- boundary_sample[2,] %>%
    mutate(.idx = Inf, rev = T, sign = -init_sign)

  na_max_len <- pmax(
    (start_chunk$n - start_chunk$not_na) + (end_chunk$n - end_chunk$not_na),
    min(boundaries$n),
    100
  )
  remaining_len <- len - start_chunk$not_na - end_chunk$not_na

  cycles <- dplyr::filter(tab, !type %in% boundary_indicator) %>%
    collect() %>%
    sample_n(size = nrow(.), replace = T) %>%
    mutate(cum_length = cumsum(n)) %>%
    filter(cum_length <= remaining_len | row_number() == 1) %>%
    mutate(.idx = 1:n(), sign = init_sign, rev = F)

  cycles <- bind_rows(start_chunk, cycles, end_chunk) %>%
    mutate(.idx = 1:n() - 1)

  cycles
}

#' Assemble a "resampled" bullet sequence
#'
#' This function assembles a sequence of length `len` from a table (or database)
#' of cycles created by `crosscut_slice()` in the following way:
#' \enumerate{
#' \item Starting and ending "chunks" are chosen
#' \item The remaining cycles in the sequence are sampled from the table until the full sequence has reached an appropriate length
#' \item The full data is assembled from the cycles selected in the first two steps
#' \item The sequence is trimmed (if too long) or padded with NA values (if too short) until it is the correct length
#' }
#'
#' @param len target sequence length
#' @param df data frame with columns id, sig, and type (boundary or otherwise).
#'          Can also be a dplyr tbl from a database. This data frame should
#'          have all sequence pieces which are to be resampled, and id should
#'          uniquely identify a sequence piece.
#' @param df_summary (optional) summary of df, will be generated if it is not
#'          provided. Can be a dplyr tbl from a database. The summary table
#'          should have columns id, type, n (the length of the chunk), and
#'          not_na (the number of non-na pieces).
#' @param output_res resolution of output crosscut in microns. Defaults to 0.645
#' @param show_plot useful for debugging; requires ggplot2
#' @param ... additional arguments passed to `cycle_draw()` function. May
#'          include `boundary_indicator` if type = 'boundary' is not used to
#'          denote a boundary chunk in df.
#' @param fill when the sequence has nearly len values, should it be padded?
#'          Otherwise, the returned sequence may differ slightly from the
#'          specified length.
#' @export
#' @importFrom assertthat assert_that has_name
#' @import dplyr
#' @importFrom stats median
crosscut_assemble <- function(len, df, df_summary = NULL,
                              output_res = 0.645,
                              show_plot = F, fill = T) {
  # Cran check fix warnings - issues with pipe identification of variables
  type <- . <- sig <- x <- .idx <- cum_length <- NULL

  # Useful for debugging purposes - shouldn't ever be called
  if (!exists("output_res")) { output_res <- 0.645 }
  if (!exists("show_plot")) { show_plot <- T }
  if (!exists("fill")) { fill <- T }

  # Check df
  slice_df_check(df)

  if (is.null(df_summary)) {
    df_summary <- df %>%
      group_by(id, type) %>%
      summarize(n = n(),
                not_na = sum(!is.na(sig), na.rm = T)) %>%
      ungroup() %>%
      collect()
  }

  # slice_sum_df_check(df_summary)

  cycle_idx <- cycle_draw(len, df_summary)

  cycles_df <- dplyr::filter(df, id %in% cycle_idx$id) %>%
    collect()

  cycles_df <- cycles_df %>%
    left_join(dplyr::select(cycle_idx, id, .idx, rev, sign), by = "id") %>%
    mutate(sig = sign*sig) %>%
    group_by(.idx) %>%
    mutate(x = ifelse(rev, -x, x)) %>%
    ungroup() %>%
    arrange(.idx, x) %>%
    mutate(x = seq(0, by = output_res, length.out = nrow(.))) %>%
    select(-rev, -sign) %>%
    mutate(sig = sig - median(sig, na.rm = T))

  if (show_plot) {
    cycles_df %>%
    ggplot2::ggplot(ggplot2::aes(x, sig, color = factor(.idx))) +
      ggplot2::geom_line()
  }

  if (fill) {
    df_fix_length(cycles_df, len)
  } else {
    cycles_df
  }
}
