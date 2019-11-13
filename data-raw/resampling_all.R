library(tidyverse)
library(bulletsamplr)
library(bulletxtrctr)
library(ggplot2)
library(odbc)
library(furrr)
plan(multicore, workers = 20)

con <- dbConnect(odbc::odbc(), "bullets", timeout = 10)

cs_lims <- tbl(con, "bullet.crosssection") %>%
  group_by(Study, Barrel, Bullet, Land, id, source) %>%
  mutate(isna = is.na(sig)) %>%
  summarize(seq_xavg = mean(x, na.rm = T), seq_len = n()) %>%
  collect() %>%
  ungroup()

peaks <- tbl(con, "bullet.peaks") %>%
  collect() %>%
  left_join(cs_lims) %>%
  mutate(xavg = (xmin + xmax)/2)
dbDisconnect(con)

ggplot(peaks) +
  geom_bin2d(aes(x = xavg,
               y = heights), bins = 100) +
  xlab("Striae Center") + ylab("Striae Height")


peak_lens <- peaks %>% group_by(Study, Barrel, Bullet, Land, id) %>%
  summarize(seq_len = unique(seq_len))

crosscut_assemble(len = 3431, df = tbl(con, "bullet.slice"), df_summary = tbl(con, "bullet.slice.idx"), fill = F)

resampled_sigs <- furrr::future_map_dfr(.x = as.numeric(peak_lens$seq_len),
                                        .f = crosscut_assemble,
                                        .id = "sample",
                                        df = tbl(con, "bullet.slice"),
                                        df_summary = tbl(con, "bullet.slice.idx"))

resampled_peaks <- resampled_sigs  %>%
  tidyr::nest(type, id, x, y, value, sig) %>%
  mutate(peaks = furrr::future_map(data, ~sig_get_peaks(.x$sig)$lines)) %>%
  select(-data) %>%
  tidyr::unnest()
resampled_peaks <- resampled_peaks  %>%
  mutate(xavg = (xmin + xmax)/2)

saveRDS(resampled_sigs, file = "data-raw/resampled_sigs.rda")
saveRDS(resampled_peaks, file = "data-raw/resampled_peaks.rda")

ggplot(resampled_peaks) +
  geom_bin2d(aes(x = xavg,
                 y = heights), bins = 100) +
  xlab("Striae Center") + ylab("Striae Height")

bind_rows(
  peaks %>% select(xavg, heights) %>% mutate(type = "Original Data"),
  resampled_peaks %>% select(xavg, heights) %>% mutate(type = "Resampled Data")
) %>%
ggplot() +
  stat_density_2d(aes(x = xavg, y = heights, color = type, fill = stat(density)),
                  contour = F, geom = "raster") +
  facet_wrap(~type)




