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
    blank_row <- dplyr::data_frame(type = "fill")
    return(
      dplyr::bind_rows(lapply(1:pad_front, function(i) blank_row),
                       full_df, lapply(1:pad_back, function(i) blank_row))
    )
  }
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
#' @param tab if a db connection is passed in, this should be a character vector
#'     of length two, giving the name of the full table and the summary table.
#'     The full table should have (at least) columns id, sig, and type
#'     (boundary or otherwise). The summary table should have columns id, type,
#'     n (the length of the chunk), and not_na (the number of non-na pieces).
#'     The full table should have all sequence pieces which are to be resampled.
#'     If a scalar is supplied, the summary table will be generated within the
#'     function. If a db connection is not available, this should be a data
#'     frame with the relevant information.
#' @param con database connection
#' @export
#' @importFrom assertthat assert_that has_name
#' @import dplyr
crosscut_assemble <- function(len, tab = c('bullet.slice', 'bullet.slice.idx'),
                              con = NULL) {
  type <- sig <- x <- df_summary <- .idx <- NULL

  if (is.null(con)) {
    assertthat::assert_that(is.data.frame(tab))
    assertthat::assert_that(assertthat::has_name(tab, "sig"))
    df <- tab
    slice_df_check(df)
    df_summary <- df %>% group_by(id, type) %>%
      summarize(n = n(),
                not_na = sum(!is.na(sig))) %>%
      ungroup()

    slice_sum_df_check(df_summary)
  } else if (length(tab) == 2) {
    assertthat::assert_that(is.character(tab))
    df <- dplyr::tbl(con, tab[1])
    slice_df_check(df)
    df_summary <- dplyr::tbl(con, tab[2]) %>%
      dplyr::collect()
    slice_sum_df_check(df_summary)
  } else if (length(tab) == 1) {
    assertthat::assert_that(is.character(tab))
    df <- dplyr::tbl(con, tab)
    slice_df_check(df)
    df_summary <- df %>%
      group_by(id, type) %>%
      collect() %>%
      summarize(n = n(),
                not_na = sum(!is.na(sig))) %>%
      ungroup()
    slice_sum_df_check(df_summary)
  } else {
    stop("tab must be either a data frame or a character vector containing the table name(s)")
  }

  stopifnot(exists("df_summary"))

  # Choose a starting chunk
  start_chunk <- dplyr::filter(df_summary, type == "boundary") %>%
    dplyr::filter(dplyr::row_number() == sample(1:dplyr::n(), 1))

  # Choose an ending chunk
  end_chunk <- dplyr::filter(df_summary, type == "boundary") %>%
    dplyr::filter(row_number() == sample(1:dplyr::n(), 1))

  na_max_len <- pmax(
    (start_chunk$n - start_chunk$not_na) + (end_chunk$n - end_chunk$not_na),
    min(df_summary$n[df_summary$type != 'boundary'])
  )
  remaining_len <- len - start_chunk$not_na - end_chunk$not_na

  # Get empty cycles data frame with columns as in df_summary
  cycles <- dplyr::filter(df_summary, 0 == 1) %>%
    dplyr::mutate(.idx = as.numeric(id))
  iterations <- 0

  while (remaining_len > na_max_len & iterations < 500) {
    iterations <- iterations + 1
    new_cycle <- dplyr::filter(df_summary, type != "boundary") %>%
      dplyr::filter(n < remaining_len) %>%
      dplyr::filter(dplyr::row_number() == sample(1:dplyr::n(), 1)) %>%
      mutate(.idx = iterations)

    if (nrow(new_cycle) == 0) {
      break
    }

    cycles <- dplyr::bind_rows(cycles, new_cycle)

    remaining_len <- remaining_len - new_cycle$n
  }

  # Choose initial sign
  init_sign <- sample(c(-1, 1), 1)

  start_df <- dplyr::filter(df, id == start_chunk$id) %>% dplyr::collect()
  if (sign(start_df$sig[!is.na(start_df$sig)][1]) != init_sign) {
    start_df$sig <- start_df$sig * -1
  }

  cycles_df <- dplyr::filter(df, id %in% cycles$id) %>% dplyr::collect() %>%
    dplyr::left_join(dplyr::select(cycles, id, .idx), by = "id") %>%
    dplyr::arrange(.idx, x) %>%
    dplyr::select(-.idx)

  end_df <- dplyr::filter(df, id == end_chunk$id) %>% dplyr::collect() %>%
    dplyr::mutate(.idx = 1:dplyr::n()) %>%
    dplyr::arrange(dplyr::desc(.idx)) %>%
    dplyr::select(-.idx)
  if (sign(end_df$sig[!is.na(end_df$sig)][1]) < 0) {
    end_df$sig <- end_df$sig * -1
  }

  full_df <- dplyr::bind_rows(start_df, cycles_df, end_df)

  df_fix_length(full_df, len)
}
