library(rvest)
library(tidyverse)
library(reshape2)
library(lubridate)


webpage <- read_html("/cloud/project/data/csv/nama_10_gdp.html")
tbls <- html_nodes(webpage, "table")
tbls

tbls_ls1 <- webpage %>%
  html_nodes("table") %>%
  .[0] %>%
  html_table(fill = TRUE)
gdp1 <- as.data.frame(tbls_ls1)


gdp <- read_csv("/cloud/project/data/csv/GDP-and-main-components (output, expenditure and income).csv")
gdp$Year <-  ymd(gdp$Year, truncated = 2L)

d <- melt(gdp, id.vars="Year")
d$value <- as.numeric(d$value)
ggplot(d, aes(Year,value/1000, col=variable)) + 
  theme_wsj()+
  geom_line()+
  theme(legend.position = "none",)


ggplot(gdp,aes(Year,Czechia))+geom_line()

gdp1 <-  gdp %>% gather(key = "variable", value = "value", -Year)
gdp1$value <- as.numeric(gdp1$value)

ggplot(gdp1, aes(Year, value))+
  geom_point()


