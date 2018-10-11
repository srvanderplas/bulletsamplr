library(bulletxtrctr)
library(x3ptools)

example_data <- bullet_pipeline(
  location = list(Bullet1 = c(hamby252demo$bullet1[3])),
  x3p_clean = function(x) x %>%
    x3pheader_to_microns() %>%
    rotate_x3p(angle = -90) %>%
    y_flip_x3p()
)

sig <- bulletxtrctr::cc_get_signature(example_data$ccdata[[1]], example_data$grooves[[1]])
usethis::use_data(sig)



library(bulletxtrctr)
library(x3ptools)
if (!dir.exists("README_files/data")) {
  dir.create("README_files/data")
}
if (!file.exists("README_files/data/Bullet1/Hamby252_Barrel1_Bullet1_Land1.x3p")) {
  NRBTDsample_download("README_files/data")
}
b1 <- read_bullet("README_files/data/Bullet1")
b2 <- read_bullet("README_files/data/Bullet2")
b1$bullet <- 1
b2$bullet <- 2
b1$land <- 1:6
b2$land <- 1:6
bullets <- rbind(b1, b2)
bullets <- bullets %>% mutate(
  x3p = x3p %>% purrr::map(.f = x3p_m_to_mum)
)
bullets <- bullets %>% mutate(
  x3p = x3p %>% purrr::map(.f = function(x) x %>%
                             rotate_x3p(angle = -90) %>%
                             y_flip_x3p())
)
bullets <- bullets %>% mutate(
  crosscut = x3p %>% purrr::map_dbl(.f = x3p_crosscut_optimize)
)
# now extract the crosscuts
bullets <- bullets %>% mutate(
  ccdata = purrr::map2(.x = x3p, .y = crosscut,
                       .f = x3p_crosscut)
)
bullets <- bullets %>% mutate(
  grooves = ccdata %>%
    purrr::map(.f = cc_locate_grooves, method = "middle",
               adjust = 30, return_plot = TRUE)
)
bullets <- bullets %>% mutate(
  sigs = purrr::map2(
    .x = ccdata, .y = grooves,
    .f = function(x, y) {
      cc_get_signature(
        ccdata = x, grooves = y, span1 = 0.75, span2 = 0.03)
    })
)
signatures <- bullets %>% select(source, sigs) %>% tidyr::unnest()

library(bulletsamplr)
library(stringr)

# Clean up signatures
signatures <- signatures %>%
  mutate(source = gsub("README_files/data/", "", source))

signature_slices <- signatures %>%
  select(-raw_sig, -se) %>%
  tidyr::nest(x, y, value, sig) %>%
  mutate(newsig = purrr::map(.$data, bulletsamplr::crosscut_slice)) %>%
  select(-data) %>%
  mutate(
    Study = "Hamby252",
    barrel = str_extract(source, "(Barrel\\d{1,})|(Unknown)") %>%
      str_remove("Barrel") %>%
      str_replace("Unknown", "??") %>%
      str_pad(width = 2, side = "left", pad = "0"),
    bullet = str_extract(source, "(Bullet[A-Z\\d])") %>%
      str_remove("Bullet"),
    land = str_extract(source, "(Land\\d)") %>% str_remove("Land"),
    id = sprintf("%02s-%s-%s", barrel, bullet, land)) %>%
  tidyr::unnest(newsig) %>%
  tidyr::unnest(newsig) %>%
  mutate(id = sprintf("%s_%s_%02d", Study, id, .chunk)) %>%
  select(id, type = .type, x, y, value, sig) %>%
  ungroup()

usethis::use_data(signature_slices)
