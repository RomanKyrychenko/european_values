suppressPackageStartupMessages({
  require(rgeos)
  require(sp)
  require(maptools)
  require(magrittr)
  require(dplyr)
  require(purrr)
  require(spdplyr)
  require(spdep)
  require(ggplot2)
  require(hrbrthemes)
  require(lightgbm)
  require(patchwork)
})


es2002 <- c("Austria", "Belgium", "Germany", "Denmark", "Spain", "Finland", "France", "United Kingdom", 
            "Greece", "Ireland", "Italy", "Luxembourg", "Netherlands", "Norway", "Portugal", "Sweden")

es2004 <- c("Austria", "Belgium", "Germany", "Denmark", "Spain", "Finland", "France", "United Kingdom", 
            "Greece", "Ireland", "Italy", "Luxembourg", "Netherlands", "Norway", "Portugal", "Sweden",
            "Czech Republic", "Hungary", "Poland", "Slovenia", "Estonia", "Slovakia", "Latvia", "Lithuania")

es2007 <- c("Austria", "Belgium", "Germany", "Denmark", "Spain", "Finland", "France", "United Kingdom", 
            "Greece", "Ireland", "Italy", "Luxembourg", "Netherlands", "Norway", "Portugal", "Sweden",
            "Czech Republic", "Hungary", "Poland", "Slovenia", "Estonia", "Slovakia", "Latvia", "Lithuania",
            "Bulgaria", "Romania")

es2013 <- c("Austria", "Belgium", "Germany", "Denmark", "Spain", "Finland", "France", "United Kingdom", 
            "Greece", "Ireland", "Italy", "Luxembourg", "Netherlands", "Norway", "Portugal", "Sweden",
            "Czech Republic", "Hungary", "Poland", "Slovenia", "Estonia", "Slovakia", "Latvia", "Lithuania",
            "Bulgaria", "Romania", "Croatia")

all_year = c("Slovenia", "United Kingdom", "Switzerland", "Sweden", "Portugal", "Poland", "Norway", "Netherlands",
             "Ireland", "Hungary", "Germany", "France", "Finland", "Belgium")

clusters_fill <- list(
  cluster_1 = c(
    "#9ecae1",
    "#6baed6",
    "#3182bd"),
  cluster_2 = c(
    "#74c476",
    "#006d2c")
) %>% unlist()
