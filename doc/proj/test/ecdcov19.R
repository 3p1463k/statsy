library(rvest)
library(tidyverse)
library(scales)
library(grid)
library(extrafont)
library(ggrepel)

ncov19ecd <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
ecdc <- read_csv(ncov19ecd)
ecdc$popData2018[ecdc$countriesAndTerritories == 'Czechia'] <- 10000000


ecdc$day <- as.numeric(ecdc$day)
ecdc$month <- as.numeric(ecdc$month)
ecdc$deaths <- as.numeric(ecdc$deaths)
ecdc$year <- as.numeric(ecdc$year)
ecdc$popData2018 <- as.numeric(ecdc$popData2018)
ecdc$cases <- as.numeric(ecdc$cases)
ecdc$dateRep <- as.Date(ecdc$dateRep)

ecd <- ecdc %>% filter(deaths>0 & cases>0, popData2018>35000) %>%  mutate(casespm = cases/popData2018*1e+06, deathspm=deaths/popData2018*1e+06)


ecdsum <- NULL

for (country in unique(ecd$countriesAndTerritories)) {
  
  ecd1 <- ecd %>% filter(countriesAndTerritories==country) %>% 
    summarize(cpm= sum(casespm), dpm=sum(deathspm)) %>% 
    mutate(Country=country)
  
  ecdsum <- rbind(ecdsum, ecd1)
  
}


#DAILY INCREASE
ggplot(ecd %>% filter(dateRep=="05-04-2020") , aes(cases, deaths, label=countriesAndTerritories)) + 
  geom_point(aes(col=countryterritoryCode),shape = 21,fill = "red", size = 5, stroke = 5, alpha = 1/2)+
  scale_x_log10(breaks = c(100, 1000, 5000, 25000))+
  scale_y_log10(breaks= c(10,100,1000))+
  geom_text_repel(size=4)+
  xlab("Tested Positive")+
  ylab("Deaths")+
  theme_minimal()+
  theme(panel.background = element_rect(fill ="#fffefc", color = "black"),
        plot.background = element_rect(fill = "#fffefc", color="black"),
        text= element_text(family="Times New Roman"),
        legend.position = "none",
        plot.title = element_text(color="#17202A"),
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major = element_line(
          size = 1,
          linetype = "dotted",
          lineend = NULL,
          color = "grey",
          arrow = NULL,
          inherit.blank = FALSE
        ), 
        axis.text = element_text(size=15, colour = "black"),
        #axis.title = element_text(size = 15),
        title = element_text(size=20),
        plot.caption=element_text(color = "black",  hjust=1.01, size=10),
        plot.margin = unit(c(15, 25, 5, 5), "pt"),
        plot.subtitle = element_text(size = 15)
  )+
  labs(title = "Daily increase nCov19", subtitle = date(), caption="Source : European Centre for Disease Prevention and Control")+
  
  annotate("text", x = 50000  , y = 1, label = "Datastak",
           hjust=0.8, vjust=0.2, col="black", cex=6, alpha = 0.1)




#PER MILLION PEOPLE INFECTED TOTAL

ggplot(ecdsum %>% filter(dpm>0.05) , aes(cpm, dpm, label=Country)) + 
  geom_point(aes(col=Country),shape = 21,fill = "red", size = 5, stroke = 5, alpha = 1/2)+
  scale_x_log10(breaks = c(1, 5, 10, 20, 50,100, 500, 1000, 2000,5000))+
  scale_y_log10(breaks= c(0.1, 1, 5, 10, 20, 50, 100, 350))+
  geom_text_repel(size=4)+
  xlab("Tested Positive per million")+
  ylab("Deaths per million")+
  theme_minimal()+
  theme(panel.background = element_rect(fill ="#fffefc", color = "black"),
        plot.background = element_rect(fill = "#fffefc", color="black"),
        text= element_text(family="Times New Roman"),
        legend.position = "none",
        plot.title = element_text(color="#17202A"),
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major = element_line(
          colour = "grey",
          size = 1,
          linetype = "dotted",
          lineend = NULL,
          arrow = NULL,
          inherit.blank = FALSE
        ), 
        axis.text = element_text(size=15, colour = "black"),
        #axis.title = element_text(size = 15),
        title = element_text(size=20),
        plot.caption=element_text(color = "black",  hjust=1.01, size=10),
        plot.margin = unit(c(15, 25, 5, 5), "pt"),
        plot.subtitle = element_text(size = 15)
  )+
  labs(title = "Daily nCov19 World Total", subtitle = date(), caption="Source : European Centre for Disease Prevention and Control")+
  annotate("text", x = 2500  , y = 0.01, label = "Datastak",
           hjust=0.8, vjust=0.2, col="black", cex=6, alpha = 0.05)


#EUROPE

# Member States of the European Union
europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czechia","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden","United_Kingdom")

EU <- ecd %>%filter(countriesAndTerritories%in%europeanUnion)

EU$day <- as.numeric(EU$day)
EU$month <- as.numeric(EU$month)
EU$deaths <- as.numeric(EU$deaths)
EU$year <- as.numeric(EU$year)
EU$popData2018 <- as.numeric(EU$popData2018)
EU$cases <- as.numeric(EU$cases)
EU$dateRep <- as.Date(EU$dateRep)

EU1 <- EU %>% mutate(casespm = cases/popData2018*1e+06, deathspm=deaths/popData2018*1e+06)


eusum <- NULL

for (c in unique(EU1$countriesAndTerritories)) {
  
  EU2 <- EU1 %>% filter(countriesAndTerritories==c) %>% 
    summarize(cpmeu= sum(casespm), dpmeu=sum(deathspm)) %>% 
    mutate(Country=c)
  
  eusum <- rbind(EU2, eusum)
  
}












ggplot(eusum , aes(cpmeu, dpmeu, label=Country)) + 
  geom_point(aes(col=Country),shape = 21,fill = "red", size = 6, stroke = 6, alpha = 1/2)+
  scale_x_log10(breaks = c(1, 5, 10, 20, 50,100, 500, 1000, 2000,4000))+
  scale_y_log10(breaks= c(1, 5, 10, 20, 50, 100, 200, 400))+
  geom_text_repel(size=5)+
  xlab("Tested Positive per million")+
  ylab("Deaths per million")+
  theme_minimal()+
  theme(panel.background = element_rect(fill ="#fffefc", color = "black"),
        plot.background = element_rect(fill = "#fffefc", color="black"),
        text= element_text(family="Times New Roman"),
        legend.position = "none",
        plot.title = element_text(color="#17202A"),
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major = element_line(
          colour = "grey",
          size = 1,
          linetype = "dotted",
          lineend = NULL,
          arrow = NULL,
          inherit.blank = FALSE
        ), 
        axis.text = element_text(size=15, colour = "black"),
        #axis.title = element_text(size = 15),
        title = element_text(size=20),
        plot.caption=element_text(color = "black",  hjust=1.01, size=10),
        plot.margin = unit(c(15, 25, 5, 5), "pt"),
        plot.subtitle = element_text(size = 15)
  )+
  labs(title = "Daily nCov19 Europe Total", subtitle = date(), caption="Source : European Centre for Disease Prevention and Control")+
  annotate("text", x = 4000  , y = 0.01, label = "Datastak",
           hjust=0.8, vjust=0.2, col="black", cex=6, alpha = 0.05)

