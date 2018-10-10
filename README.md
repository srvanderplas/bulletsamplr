
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bulletsamplr

[![license](https://img.shields.io/badge/license-MIT%20+%20file%20LICENSE-lightgrey.svg)](https://choosealicense.com/)
[![CRAN
status](https://www.r-pkg.org/badges/version/bulletsamplr)](https://cran.r-project.org/package=bulletsamplr)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Last-changedate](https://img.shields.io/badge/last%20change-2018--10--10-yellowgreen.svg)](/commits/master)
[![Travis build
status](https://travis-ci.org/srvanderplas/bulletsamplr.svg?branch=master)](https://travis-ci.org/srvanderplas/bulletsamplr)
[![Coverage
status](https://codecov.io/gh/srvanderplas/bulletsamplr/branch/master/graph/badge.svg)](https://codecov.io/github/srvanderplas/bulletsamplr?branch=master)

The goal of bulletsamplr is to create bullet signatures which can be
used to examine the behavior of match statistics under truly random
conditions.

## Installation

You can install the released version of bulletsamplr from
[github](https://github.com/srvanderplas/bulletsamplr) with:

``` r
devtools::install_github("srvanderplas/bulletsamplr")
```

## Example

``` r
library(ggplot2)
library(bulletsamplr)
library(dplyr)

data(sig)
```

We start with a signature from a scanned bullet land:

``` r
ggplot(aes(x = x, y = sig), data = sig) + 
  geom_line() + 
  ggtitle("Original Signature") + 
  theme(axis.title = element_blank(), axis.text.x = element_blank())
```

<img src="man/figures/README-unnamed-chunk-1-1.png" title="Land Signature" alt="Land Signature" width="100%" />

The threshold bootstrap creates new sequences by resampling “cycles”
from actual data. These cycles are created as follows:

First, the median of the signature is used to divide the signature into
**cycles** of one positive and one negative region of the signature
(relative to the median value).

``` r
threshold <- median(sig$sig, na.rm = T)
ggplot(aes(x = x, y = sig), data = sig) + 
  geom_line() + 
  geom_hline(aes(yintercept = threshold), color = "red") + 
  ggtitle("Threshold used to create cycles for future sample signatures") + 
  theme(axis.title = element_blank(), axis.text.x = element_blank())
#> Warning: Removed 265 rows containing missing values (geom_path).
```

<img src="man/figures/README-unnamed-chunk-2-1.png" title="Land Signature. Cycles are created when the sequence crosses the median, and include one positive region and one negative region. This ensures continuity between adjacent segments." alt="Land Signature. Cycles are created when the sequence crosses the median, and include one positive region and one negative region. This ensures continuity between adjacent segments." width="100%" />

These divisions are shown below, with each segment in a different color;
adjacent segments are disjoint.

``` r
sig_slices <- crosscut_slice(sig) %>% bind_rows() %>%
  group_by(.chunk) %>%
  # mutate(x = x - mean(x, na.rm = T)) %>%
  ungroup()

sig_slices %>%
  group_by(.chunk) %>%
  mutate(x = ifelse(.chunk == max(sig_slices$.chunk), rev(x), x)) %>%
  mutate(sig = ifelse(.chunk == max(sig_slices$.chunk), -sig, sig)) %>%
  ungroup() %>%
  arrange(.chunk, x) %>%
  mutate(xnew = 1:nrow(.)) %>%
ggplot(data = ., aes(x = xnew*1.5625, y = sig, color = factor(.chunk))) + geom_line() + 
  scale_color_discrete("Chunk") + 
  ggtitle("Cycles (and boundary regions)") + 
  theme(axis.title = element_blank(), axis.text.x = element_blank())
#> Warning: Removed 265 rows containing missing values (geom_path).
```

<img src="man/figures/README-chunk-slices-1.png" width="100%" />

The cycles are then post-processed so that they all share the same
orientation (positive portion of the cycle first); this requires that
the boundary chunks be oriented so that the final portion of the chunk
is negative. End chunks are thus reversed (in x) and may be flipped in y
depending on the original orientation.

``` r

sig_slices$chunk_label <- factor(
  sig_slices$.chunk, 
  labels = c("boundary_start", 
             paste("cycle", unique(sig_slices$.chunk)[-c(1, max(sig_slices$.chunk))]), 
             "boundary_end"))

ggplot(aes(x = x, y = sig, group = .chunk), data = sig_slices) + 
  facet_wrap(~chunk_label, scales = "free") +
  geom_line() + theme(axis.title = element_blank()) + 
  ggtitle("Sliced Land Signature")
#> Warning: Removed 265 rows containing missing values (geom_path).
```

<img src="man/figures/README-unnamed-chunk-3-1.png" title="Slices of land signature. Note that the boundary_end slice has been reversed (in x) and flipped (in y) so that it is 'in phase' with the positive-to-negative cycles in the main portion of the signature." alt="Slices of land signature. Note that the boundary_end slice has been reversed (in x) and flipped (in y) so that it is 'in phase' with the positive-to-negative cycles in the main portion of the signature." width="100%" />

The cycles can then be saved to a database, or the data frame of cycles
can be reassembled directly into new sequences. Ideally, of course, new
sequences would be assembled from a library composed of cycles from many
different signatures.
