# Set up test environment
library(DBI)
library(odbc)
library(RSQLite)
library(dplyr)

# Generate test slices
set.seed(40920793)
test_slice_sig <- bind_rows(
  data_frame(id = "cos", type = "boundary", x = seq(runif(1), 1.5*pi, by = .01),
             xdel = diff(c(0, x)),
             sig = cos(x), len = length(x)),
  data_frame(id = "sin", type = "pn", x = seq(0, 2*pi, by = .01),
             xdel = diff(c(0, x)),
             sig = sin(x), len = length(x)),
  data_frame(id = "sin2", type = "pn", x = seq(0, pi, by = .01),
             xdel = diff(c(0, x)),
             sig = sin(2*x), len = length(x)),
  data_frame(id = "sharp", type = "pn", x = seq(0, 2*pi, by = .01),
             xdel = diff(c(0, x)),
             sig = asin(sin(x)), len = length(x)),
  data_frame(id = "sharp2", type = "pn", x = seq(0, pi, by = .01),
             xdel = diff(c(0, x)),
             sig = asin(sin(2*x)), len = length(x)),
  data_frame(id = "sin3", type = "boundary", x = seq(0, runif(1)*pi, by = .01),
             xdel = diff(c(0, x)),
             sig = sin(x*2), len = length(x))
) %>%
  mutate(xold = x,
         x = cumsum(xdel))

test_slice_table <- crosscut_slice(test_slice_sig$sig) %>% bind_rows() %>%
  rename(id = .chunk, type = .type)
