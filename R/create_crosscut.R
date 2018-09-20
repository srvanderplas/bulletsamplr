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
      assertthat::has_name(df_summary, "id"),
      assertthat::has_name(df_summary, "type"),
      assertthat::has_name(df_summary, "n"),
      assertthat::has_name(df_summary, "not_na"))
  }

  if ("tbl_sql" %in% class(df) | "tbl_lazy" %in% class(df)) {
    assertthat::assert_that("id" %in% df_summary$ops$vars,
                            "type" %in% df_summary$ops$vars,
                            "n" %in% df_summary$ops$vars,
                            "not_na" %in% df_summary$ops$vars)
  }
}

#' Assemble a "resampled" bullet sequence
#'
#' @param len target sequence length
#' @param tab if a db connection is passed in, this should be a character vector
#'     of length two, giving the name of the full table and the summary table.
#'     The full table should have (at least) columns id, sig, and type. The
#'     summary table should have columns id, type, n (the length of the chunk),
#'     and not_na (the number of non-na pieces). The full table should have all
#'     sequence pieces which are to be resampled. If a scalar is supplied, the
#'     summary table will be generated within the function.
#'     If a db connection is not available, this should be a data frame with the
#'     relevant information.
#' @param con database connection
#' @export
#' @importFrom assertthat assert_that has_name
crosscut_assemble <- function(len, tab = c('bullet.slice', "bullet.slice.idx"),
                              con = NULL) {
  if (is.null(con)) {
    assertthat::assert_that(is.data.frame(tab))
    assertthat::assert_that(assertthat::has_name(tab, "sig"))
    df <- tab
    slice_df_check(df)
    df_summary <- df %>% group_by(id, type) %>%
      summarize(n = n(),
                not_na = sum(!is.na(sig)))
    slice_sum_df_check(df_summary)
  } else if (length(tab) == 2) {
    assertthat::assert_that(is.character(tab))
    df <- tbl(con, tab[1])
    slice_df_check(df)
    df_summary <- tbl(con, tab[2]) %>% collect()
    slice_sum_df_check(df_summary)
  } else if (length(tab) == 1) {
    assertthat::assert_that(is.character(tab))
    df <- tbl(con, tab[1])
    slice_df_check(df)
    df_summary <- df %>% group_by(id, type) %>%
      summarize(n = n(), not_na = sum(!is.na(sig))) %>% collect()
    slice_sum_df_check(df_summary)
  } else {
    stop("tab must be either a data frame or a character vector containing the table name(s)")
  }

  # Choose a starting chunk
  start_chunk <- filter(df_summary, type == "boundary") %>%
    filter(row_number() == sample(1:n(), 1))

  # Choose an ending chunk
  end_chunk <- filter(df_summary, type == "boundary") %>%
    filter(row_number() == sample(1:n(), 1))

  na_max_len <- (start_chunk$n - start_chunk$not_na) + (end_chunk$n - end_chunk$not_na)
  remaining_len <- len - start_chunk$not_na - end_chunk$not_na

  # Get empty cycles data frame with columns as in df_summary
  cycles <- filter(df_summary, 0 == 1)
  iterations <- 0

  while (remaining_len > na_max_len & iterations < 500) {
    new_cycle <- filter(df_summary, type != "boundary") %>%
      filter(n < remaining_len) %>%
      filter(row_number() == sample(1:n(), 1))

    cycles <- bind_rows(cycles, new_cycle)

    remaining_len <- remaining_len - new_cycle$n
  }

  # Choose initial sign
  init_sign <- sample(c(-1, 1), 1)

  start_df <- filter(df, id == start_chunk$id) %>% collect()
  if (sign(start_df$sig[!is.na(start_df$sig)][1]) != init_sign) {
    start_df$sig <- start_df$sig * -1
  }

  cycle_df <- filter(df, id %in% cycles$id) %>% collect() %>%
    mutate(id = factor(id, levels = cycles$id)) %>%
    arrange(id, x) %>%
    mutate(id = as.character(id))

  end_df <- filter(df, id == end_chunk$id) %>% collect()
  if (sign(end_df$sig[!is.na(end_df$sig)][1]) < 0) {
    end_df$sig <- end_df$sig * -1
  }

  bind_rows(start_df, cycle_df, end_df)
}
