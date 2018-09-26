# Set up test environment
library(DBI)
library(odbc)
library(RSQLite)
library(dplyr)
library(purrr)
library(tidyr)

# Generate test slices
set.seed(40920793)
test_slice_sig <- bind_rows(
  data_frame(id = "cos", type = "boundary", x = seq(0, 1.5*pi, by = .01),
             sig = cos(x), len = length(x)),
  data_frame(id = "sin", type = "pn", x = seq(0, 2*pi, by = .01),
             sig = sin(x), len = length(x)),
  data_frame(id = "sin2", type = "pn", x = seq(0, pi, by = .01),
             sig = sin(2*x), len = length(x)),
  data_frame(id = "sharp", type = "pn", x = seq(0, 2*pi, by = .01),
             sig = asin(sin(x)), len = length(x)),
  data_frame(id = "sharp2", type = "pn", x = seq(0, pi, by = .01),
             sig = asin(sin(2*x)), len = length(x))
)

split_idx <- map_df(
  unique(test_slice_sig$id),
  function(.) {
    start_idx <- floor(sum(test_slice_sig$id == .)/2)
    end_idx <- sum(test_slice_sig$id == .)
    zzz <- sample(start_idx:end_idx, 1)
    data_frame(split_idx = zzz, id = .)
  })

test_slice_table <- test_slice_sig %>%
  left_join(split_idx, by = "id") %>%
  nest(-split_idx) %>%
  mutate(
    tab = map2(split_idx, data, function(idx, data) {
      if (unique(data$type) ==  "pn") {
        bind_rows(
          data,
          data[1:idx,] %>% mutate(id = paste0(id, "_start"), type = "boundary"),
          data[rev((idx + 1):nrow(data)),] %>%
            mutate(id = paste0(id, "_end"), type = "boundary")
        )
      } else {
        data
      }
    })
  ) %>%
  select(-split_idx, -data) %>%
  unnest()
