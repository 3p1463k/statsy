library(rvest)
library(tidyverse)
library(reshape2)
library(lubridate)

xtb_web <- read_html("https://www.xtb.com/cz/nabidka/informace-o-uctech/specifikace-instrumentu")

tbls <- html_nodes(xtb_web, "table")
tbls

tbls_ls <- xtb_web %>%
  html_nodes("table") %>%
  .[2] %>%
  html_table(fill = TRUE)

xtb <- as.data.frame(tbls_ls)


