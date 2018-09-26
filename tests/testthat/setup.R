# Set up test environment
library(DBI)
library(odbc)
library(dplyr)
library(RSQLite)

# Generate test slices
set.seed(40920793)
test_slice_table <- data_frame(id = "sin", type = "pn",
                               x = seq(0, 2*pi, by = .005), sig = sin(x))
split_idx <- sample(floor(nrow(test_slice_table)/2):nrow(test_slice_table), 1)
test_slice_table <- bind_rows(
  test_slice_table,
  test_slice_table[1:split_idx,] %>%
    mutate(id = "sin_start", type = "boundary"),
  test_slice_table[(split_idx + 1):nrow(test_slice_table),] %>%
    mutate(id = "sin_end", type = "boundary")
)

sql_tests <- FALSE
bigfoot <- system('hostname', intern = T) == 'bigfoot'

# If bigfoot, use sql connection
if (bigfoot) {
  bigfoot_db_con <- dbConnect(odbc::odbc(), "bullets", timeout = 10)
}

if ("RSQLite" %in% installed.packages) {
  db_con <- dbConnect(RSQLite::SQLite(), ":memory:")
  dbWriteTable(db_con, "bullet.slice", test_slice_table)
} else {
  sql_tests <- TRUE
}
