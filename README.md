
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Houston-Persistence

<!-- badges: start -->
<!-- badges: end -->

This repository contains the scans acquired from bullets 11 to 50 from a
set of eleven barrels Ruger LCP.

[Website for Barrel
A](https://heike.github.io/Houston-Persistence/docs/matrix-comparisons/matrix_A.html)

[Website for Barrel
D](https://heike.github.io/Houston-Persistence/docs/matrix-comparisons/matrix_D.html)

## Structure of the repo

    -- scans
    -- apps 
    ... 

### Folder `scans`

The folder `scans` contains x3p files of the land engraved areas of each
fired bullet. Names of the x3p files are encoded in the form:
study-barrel-bullet-land, and conform to the regular expression:
`HFSCP-B[123A-J]-B[1-5][0-9]-L[1-6].x3p`

    -- scans
    |   |-- Barrel A
    |   |   |-- Bullet 11
    |   |   |   |-- HFSCP-BA-B11-L1.x3p    
    |   |   |   |-- HFSCP-BA-B11-L2.x3p    
    |   |   |   |-- HFSCP-BA-B11-L3.x3p    
    |   |   |   |-- HFSCP-BA-B11-L4.x3p    
    |   |   |   |-- HFSCP-BA-B11-L5.x3p    
    |   |   |   |-- HFSCP-BA-B11-L6.x3p    
    |   |   |-- Bullet 12
    |   |   |   |-- HFSCP-BA-B12-L1.x3p    
    |   |   |   |-- HFSCP-BA-B12-L2.x3p    
    |   |   |   |-- HFSCP-BA-B12-L3.x3p    
    |   |   |   |-- HFSCP-BA-B12-L4.x3p    
    |   |   |   |-- HFSCP-BA-B12-L5.x3p    
    |   |   |   |-- HFSCP-BA-B12-L6.x3p    
    ... 

### Folder `apps`

The folder `apps` contains two shiny apps. Each app can be run with the
command

`shiny::runApp("<app>.R")`

`check-crosscut.R` expects an object `bullets` in the work space (see
`create-features.R`)

``` r
library(tidyverse)
library(x3ptools)
library(bulletxtrctr)

bullets <- bulletxtrctr::read_bullet("scans/Barrel A/Bullet 11/") # just to try out with one bullet
bullets <- bulletxtrctr::read_bullet("scans/Barrel A/") # full barrel
bullets <- bullets %>% mutate(
  land_id = gsub(".x3p", "",basename(source)),
  cc = x3p %>% purrr::map_dbl(.f = bulletxtrctr::x3p_crosscut_optimize)
)
shiny::runApp("apps/check-crosscut.R")
```

![](images/check-crosscut-screenshot.png)

`check-grooves.R` expects an object `grooves` in the work space (see
`create-features.R`)

``` r
resolution <- x3p %>% x3p_get_scale()

# extract 10 lines from the scan at height cc
bullets <- bullets %>% mutate(
  ccdata = purrr::map2(.f = bulletxtrctr::x3p_crosscut, .x=x3p, .y = cc, 
                       range = 10*resolution) 
)

bullets <- bullets %>% mutate(
  grooves_pred = ccdata %>% purrr::map(.f = bulletxtrctr::cc_locate_grooves, 
                                       return_plot = TRUE)
)

grooves <- bullets$grooves_pred
shiny::runApp("apps/check-grooves.R")

bullets$grooves <- grooves
```

![](images/check-grooves-screenshot.png)

Extract the signal for each land:

``` r
meta <- read.csv("meta.csv")
meta$source <- gsub("//", "/", meta$source)
bullets <- bulletxtrctr::read_bullet("scans/Barrel A/Bullet 37/") # just to try out with one bullet
bullets$source <- gsub("//", "/", bullets$source)

bullets <- bullets %>% left_join(meta %>% select(land_id, damaged, source, cc, groove_left, groove_right), by="source")

resolution <- bullets$x3p[[1]] %>% x3p_get_scale()

bullets <- bullets %>% mutate(
  ccdata = purrr::map2(.x = x3p, .y = cc, .f = x3p_crosscut, range = 10*resolution)
)

# summarise ccdata
bullets <- bullets %>% mutate(
  ccdata = ccdata %>% purrr::map(.f = function(df) {
    df <- df %>% mutate(x_bin = round(x/resolution))
    df %>% group_by(x_bin) %>% 
      summarize(
        value = mean(value, na.rm=TRUE),
        y = mean(y, na.rm=TRUE)
        ) %>% ungroup() %>%
      mutate(
        x = x_bin*resolution
      ) %>% select(-x_bin)
  })
)



bullets <- bullets %>% mutate(
  grooves = purrr::map2(.x = groove_left, .y=groove_right, 
                        function(x,y) list(groove=c(x,y))),
  signal = purrr::map2(.x = ccdata, .y = grooves, .f = function(ccdata, gg) {
    cc_get_signature(ccdata, gg, span1 = 0.75, span2=0.03)
  })
)

bullets %>% unnest(cols = signal) %>%
  ggplot(aes(x = x, y = sig)) + geom_line()+
  facet_grid(~land_id) + ylim(-10,10) + 
  ggtitle("Bullet 37")
```

![](signals-b11.png) ![](signals-b37.png)

After staring at these signals long enough, we see that land 5 in bullet
11 matches land 6 in bullet 37. Therefore land 4 in bullet 11 should
match land 5, land 3 should match land 4, and so on.
