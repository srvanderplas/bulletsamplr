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
